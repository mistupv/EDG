package edg.edge;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.function.Function;
import java.util.function.Predicate;

import edg.constraint.AddNodeConstraint;
import edg.constraint.Constraints;
import edg.constraint.EdgeConstraint;
import edg.constraint.GlobalVariableConstraint;
import edg.constraint.GrammarConstraint;
import edg.constraint.IgnoreEdgeConstraint;
import edg.constraint.NodeConstraint;
import edg.constraint.PhaseConstraint;
import edg.constraint.SeekingConstraint;
import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.graph.VariableInfo.Context;
import edg.slicing.Phase;
import edg.traverser.ControlFlowTraverser;
import edg.traverser.EDGTraverser;
import edg.traverser.ControlFlowTraverser.NodeWork;

public class FlowEdgeGenerator extends EdgeGenerator
{
	private static class State
	{
		private final Node node;
		private final Set<VariableId> uses;
		private final Set<VariableId> definitions;

		private State(Node node)
		{
			this(node, new HashSet<VariableId>(), new HashSet<VariableId>());
		}
		private State(Node node, Set<VariableId> uses, Set<VariableId> definitions)
		{
			this.node = node;
			this.uses = uses;
			this.definitions = definitions;
		}

		public String toString()
		{
			return "{" + this.node.getData().getId() + " = U" + this.uses.toString() + ", D" + this.definitions.toString() + "}";
		}
		public boolean equals(Object o)
		{
			if (o == this)
				return true;
			if (!(o instanceof State))
				return false;

			final State state = (State) o;

			if (this.node != state.node)
				return false;
			if (!this.uses.equals(state.uses))
				return false;
			if (!this.definitions.equals(state.definitions))
				return false;
			return true;
		}
		public int hashCode()
		{
			return this.node.getData().getId() + this.uses.size() + this.definitions.size();
		}
	}
	private static class Variable
	{
		private final VariableId variableId;
		private final Node node;

		private Variable(VariableId variableId, Node node)
		{
			this.variableId = variableId;
			this.node = node;
		}
	}
	private static class VariableId
	{
		private final String variableId;

		private VariableId(String variableId)
		{
			this.variableId = variableId;
		}

		public String toString()
		{
			return this.variableId;
		}
		public boolean equals(Object o)
		{
			if (o == this)
				return true;
			if (!(o instanceof VariableId))
				return false;

			final VariableId variableId = (VariableId) o;

			if (!this.variableId.equals(variableId.variableId))
				return false;
			return true;
		}
		public int hashCode()
		{
			return this.variableId.hashCode();
		}

		public String getVariableName()
		{
			return this.split()[0];
		}
		public boolean greater(VariableId variableId)
		{
			final String[] split = variableId.split();
			if (split.length < 2)
				return false;
			final String variable = split[0];
			return this.variableId.equals(variable);
		}
		private String[] split()
		{
			final StringTokenizer st = new StringTokenizer(this.variableId, "[]");
			final List<String> split = new LinkedList<String>();
			while (st.hasMoreTokens())
				split.add(st.nextToken());
			return split.toArray(new String[0]);
		}
	}

	private final Map<Node, Set<State>> clauseMap = new HashMap<Node, Set<State>>();
	private final Map<Node, Set<State>> callMap = new HashMap<Node, Set<State>>();

	public FlowEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		this.collectInfo();
		this.addEdges();
	}

	private Set<State> getNodeStates(Node node, boolean global)
	{
		if (this.callMap.containsKey(node))
			return this.callMap.get(node);
		return this.getVariableStates(node, global);
	}
	private Set<State> getVariableStates(Node node, boolean global)
	{
		final Set<State> states = new HashSet<State>();
		final Set<VariableId> uses = new HashSet<VariableId>();
		final Set<VariableId> definitions = new HashSet<VariableId>();
		final NodeInfo nodeInfo = node.getData();

		if (nodeInfo instanceof VariableInfo)
		{
			final VariableInfo variableInfo = (VariableInfo) nodeInfo;

			if (!global || variableInfo.isGlobal())
			{
				final Context context = variableInfo.getContext();
				final VariableId variableId = this.getVariableId(node);

				switch (context)
				{
					case Use:
						uses.add(variableId);
						break;
					case Definition:
						definitions.add(variableId);
						break;
					case Def_Use:			// ADDED FOR UnaryOperations (++/--) that both define and use a variable 
						uses.add(variableId);
						definitions.add(variableId);
						break;
				}
			}
		}
		states.add(new State(node, uses, definitions));

		return states;
	}
	private VariableId getVariableId(Node variable)
	{
		if (!(variable.getData() instanceof VariableInfo))
			throw new RuntimeException("The node is not a variable");

		final VariableInfo variableInfo = (VariableInfo) variable.getData();
		final String variableName = variableInfo.getName();
		return new VariableId(variableName);
	}

	// Collect info
	private void collectInfo()
	{
		final List<Node> workList = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Clause);
		final Set<Node> worksDone = new HashSet<Node>();

		while (!workList.isEmpty())
		{
			final Node clause = workList.remove(0);
			final Node parametersNode = EDGTraverser.getChild(clause, 0);
			final Node clauseResult = EDGTraverser.getChild(clause, 3);
			final List<Node> callResults = EDGTraverser.getOutputs(clauseResult, EDGTraverser.Direction.Forwards);

			final Function<NodeWork<State>, Set<NodeWork<State>>> newStates = new Function<NodeWork<State>, Set<NodeWork<State>>>() {
				public Set<NodeWork<State>> apply(NodeWork<State> nodeWork)
				{
					final Node newNode = nodeWork.getNode();
					final State prevState = nodeWork.getState();
					final Set<State> states = FlowEdgeGenerator.this.getNodeStates(newNode, true);
					final Set<NodeWork<State>> newStates = new HashSet<NodeWork<State>>();

					for (State state : states)
					{
						final Set<VariableId> newUses = new HashSet<VariableId>(state.uses);
						final Set<VariableId> uses = new HashSet<VariableId>(prevState.uses);
						final Set<VariableId> definitions = new HashSet<VariableId>(prevState.definitions);

						newUses.removeIf(variable -> definitions.contains(variable));
						uses.addAll(newUses);
						definitions.addAll(state.definitions);
						newStates.add(new NodeWork<State>(newNode, new State(newNode, uses, definitions)));
					}

					return newStates;
				}
			};
			final Predicate<ControlFlowTraverser.NodeWork<State>> collectAndStop = nodeWork -> nodeWork.getNode() == clauseResult;
			final ControlFlowTraverser.NodeWork<State> nodeWork = new ControlFlowTraverser.NodeWork<State>(parametersNode, new State(parametersNode));
			final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(ControlFlowTraverser.Direction.Forwards, true, true, true, true, true);
			final Set<ControlFlowTraverser.NodeWork<State>> newWorks = ControlFlowTraverser.traverse(nodeWork, configuration, newStates, collectAndStop);
			final Set<State> states = new HashSet<State>();

			newWorks.forEach(newWork -> states.add(newWork.getState()));
			this.clauseMap.put(parametersNode, states);

			for (Node callResult : callResults)
			{
				final Node outputClause = EDGTraverser.getAncestor(callResult, NodeInfo.Type.Clause);
				final Set<State> newInfo = new HashSet<State>();
				for (State state : states)
					newInfo.add(new State(callResult, state.uses, state.definitions));

				final Set<State> prevInfo = this.callMap.get(callResult);
				// TODO Hacer la union y compararla con el prev. Si no ha cambiado es que no hay nada nuevo
				if (newInfo.equals(prevInfo))
					continue;

				this.callMap.put(callResult, newInfo);
				this.addWork(workList, worksDone, outputClause);
			}
		}
	}

	private <S> void addWork(List<S> workList, Set<S> worksDone, S element)
	{
		if (!workList.contains(element) && !worksDone.contains(element))
			workList.add(element);
	}

	// Add edges
	private void addEdges()
	{
		final List<Variable> definitionNodes = this.getDefinitions();
		final NodeConstraint nodeConstraint = new IgnoreEdgeConstraint(EdgeInfo.Type.Value);
		final EdgeConstraint ignoreConstraint = new AddNodeConstraint(nodeConstraint);

		for (Variable definitionVariable : definitionNodes)
		{
			final VariableId variableId = definitionVariable.variableId;
			final Node definitionNode = definitionVariable.node;
			final Node definitionResultNode = EDGTraverser.getResult(definitionNode);
			
// TODO POCO ELEGANTE, SUSTITUIR DE ALGUNA MANERA MAS COHERENTE. Esto se utiliza para aquellas Variables usadas (e.g. System.out) que se consideran
//		parametros de entrada pero no tienen resultnode en "parameters"
if (definitionResultNode == null) 
	continue;

			final boolean isDefinitionParameters = definitionNode.getData().getType() == NodeInfo.Type.Parameters;
			final boolean isDefinitionCall = !isDefinitionParameters && EDGTraverser.getSibling(definitionNode, 0).getData().getType() == NodeInfo.Type.Call;
			final boolean isDefinitionVariable = !isDefinitionParameters && !isDefinitionCall;
			final GlobalVariableConstraint addConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Add, variableId.toString());
			final GlobalVariableConstraint letThroughConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.LetThrough, variableId.toString());
			final GlobalVariableConstraint removeConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Remove, variableId.toString());

			// Definitions
			final Node declarationNode = this.getDeclaration(variableId, definitionNode);
			final boolean isGlobalDeclaration = declarationNode != null && declarationNode.getData() instanceof VariableInfo && ((VariableInfo) declarationNode.getData()).isGlobal();
			if (declarationNode != null && declarationNode != definitionNode)
				if (isDefinitionParameters)
				{
					final GrammarConstraint grammarConstraint = new GrammarConstraint(this.edg.getGrammar(), declarationNode);
					final Constraints production = new Constraints();
					production.pushEdgeConstraint(new PhaseConstraint(Phase.Input));
					production.pushEdgeConstraint(removeConstraint);
					this.edg.addProduction(grammarConstraint, production);
					this.edg.addEdge(declarationNode, definitionResultNode, 0, new EdgeInfo(EdgeInfo.Type.Input, grammarConstraint));
				}
				else
					this.edg.addEdge(declarationNode, definitionResultNode, 0, new EdgeInfo(EdgeInfo.Type.Flow, ignoreConstraint));

			// Uses
			final Node lastNode = isGlobalDeclaration ? this.getLastNode(definitionNode) : null;
			final Set<Node> usesNodes = this.getUses(variableId, definitionNode, lastNode);
			for (Node useNode : usesNodes)
			{
				final boolean isUseLastNode = useNode == lastNode;
				final boolean isUseCall = !isUseLastNode && EDGTraverser.getSibling(useNode, 0).getData().getType() == NodeInfo.Type.Call;
				final boolean isUseVariable = !isUseLastNode && !isUseCall;
				final Node useNode0 = !isUseCall ? useNode : this.getArgumentsNode(useNode);
				final Node useNode1 = !isUseCall ? EDGTraverser.getResult(useNode0) : useNode0;

				if (isDefinitionVariable && isUseVariable)
					this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow));
				else if (isDefinitionVariable)
					this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, removeConstraint));
//					this.edg.addEdge(definitionNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Data, this.getConstraint(definitionNode, useNode0, removeConstraint)));
				else if (isUseVariable)
					this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
//					this.edg.addEdge(definitionNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Data, this.getConstraint(definitionNode, useNode0, addConstraint)));
				else if (isDefinitionCall || isUseCall)
					this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, letThroughConstraint));
			}
		}
	}
	private Node getDeclaration(VariableId variableId, Node definitionNode)
	{
		final Predicate<Node> collectAndStop = new Predicate<Node>() {
			public boolean test(Node node)
			{
				if (!(node.getData() instanceof VariableInfo))
					return false;
				final VariableInfo variableInfo = (VariableInfo) node.getData();
				if (!variableInfo.isDeclaration())
					return false;
				final VariableId variableId0 = FlowEdgeGenerator.this.getVariableId(node);
				if (variableId.equals(variableId0))
					return true;
				return false;
			}
		};
		final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(ControlFlowTraverser.Direction.Backwards, true, false, true, false, true);
		final Set<Node> declaration = ControlFlowTraverser.traverse(definitionNode, configuration, collectAndStop);
		if (!declaration.isEmpty())
			return declaration.iterator().next();

		final String variableName = variableId.getVariableName();
		final List<Node> variables = this.getVariables(variableName, true, null, true);
		return variables.isEmpty() ? null : variables.get(0);
	}
	private List<Variable> getDefinitions()
	{
		final List<Variable> definitions = new LinkedList<Variable>();

		// Definition nodes
		final List<Node> definitionNodes = this.getVariables(null, null, Context.Definition, null);

// ADDED FOR DEF_USE CONTEXT VARIABLES
final List<Node> definitionUseNodes= this.getVariables(null, null, Context.Def_Use, null);
definitionNodes.addAll(definitionUseNodes);
		
		for (Node definitionNode : definitionNodes)
		{
			final VariableId variableId = this.getVariableId(definitionNode);
			definitions.add(new Variable(variableId, definitionNode));
		}

		// Call nodes
		for (Entry<Node, Set<State>> entry : this.callMap.entrySet())
		{
			final Node callNode = entry.getKey();
			final Set<State> states = entry.getValue();
			final Set<VariableId> variableIds = new HashSet<VariableId>();

			for (State state : states)
				variableIds.addAll(state.definitions);
			for (VariableId variableId : variableIds)
				definitions.add(new Variable(variableId, callNode));
		}

		// Parameters nodes
		for (Entry<Node, Set<State>> entry : this.clauseMap.entrySet())
		{
			final Node parametersNode = entry.getKey();
			final Set<State> states = entry.getValue();
			final Set<VariableId> variableIds = new HashSet<VariableId>();

			for (State state : states)
				variableIds.addAll(state.uses);
			for (VariableId variableId : variableIds)
				definitions.add(new Variable(variableId, parametersNode));
		}

		return definitions;
	}
	private Set<Node> getUses(VariableId variableId, Node definitionNode, Node lastNode)
	{
		final Predicate<Node> collect = new Predicate<Node>() {
			public boolean test(Node node)
			{
				if (node == definitionNode)
					return false;
				if (node == lastNode)
					return true;
				final Set<State> states = FlowEdgeGenerator.this.getNodeStates(node, false);
				for (State state : states)
					for (VariableId use : state.uses)
						if (variableId.equals(use) || variableId.greater(use))
							return true;
				return false;
			}
		};
		final Predicate<Node> stop = new Predicate<Node>() {
			public boolean test(Node node)
			{
				if (node == definitionNode)
					return false;
				final Set<State> states = FlowEdgeGenerator.this.getNodeStates(node, false);
				if (states.isEmpty())
					return false;
				for (State state : states)
					if (!state.definitions.contains(variableId))
						return false;
				return true;
			}
		};
		final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(ControlFlowTraverser.Direction.Forwards, true, true, true, true, true);

		return ControlFlowTraverser.traverse(definitionNode, configuration, collect, stop);
	}

	private Node getLastNode(Node node)
	{
		final Node clauseNode = EDGTraverser.getAncestor(node, NodeInfo.Type.Clause);
		if (clauseNode != null)
			return EDGTraverser.getChild(clauseNode, 3);
		return null;
	}
	private Node getArgumentsNode(Node node)
	{
		final Node callNode = EDGTraverser.getSibling(node, 0);
		return EDGTraverser.getChild(callNode, 1);
	}
}