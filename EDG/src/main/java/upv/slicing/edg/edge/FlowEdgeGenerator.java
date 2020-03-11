package upv.slicing.edg.edge;

import upv.slicing.edg.LASTBuilder.ClassInfo;
import upv.slicing.edg.constraint.*;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.graph.Variable.Context;
import upv.slicing.edg.traverser.ControlFlowTraverser;
import upv.slicing.edg.traverser.EDGTraverser;
import upv.slicing.edg.traverser.LASTTraverser;

import java.util.*;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.function.Predicate;

public class FlowEdgeGenerator extends EdgeGenerator
{	
	private static class State
	{
		private final Node node;
		private final Set<VariableId> uses;
		private final Set<VariableId> definitions;

		private State(Node node)
		{
			this(node, new HashSet<>(), new HashSet<>());
		}
		private State(Node node, Set<VariableId> uses, Set<VariableId> definitions)
		{
			this.node = node;
			this.uses = uses;
			this.definitions = definitions;
		}

		public String toString()
		{
			return "{" + this.node.getId() + " = U" + this.uses.toString() + ", D" + this.definitions.toString() + "}";
		}
		public boolean equals(Object o)
		{
			if (o == this)
				return true;
			if (!(o instanceof State))
				return false;

			final State state = (State) o;
			return Objects.equals(node, state.node) &&
					Objects.equals(uses, state.uses) &&
					Objects.equals(definitions, state.definitions);
		}
		public int hashCode()
		{
			return Objects.hash(node.getId(), uses, definitions);
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
		private final Node index;
		
		private VariableId(String variableId)
		{
			this(variableId, null);
		}
		private VariableId(String variableId, Node index)
		{
			this.variableId = variableId;
			this.index = index;
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

			return this.variableId.equals(variableId.variableId);
		}
		public int hashCode()
		{
			return this.variableId.hashCode();
		}

		public String getVariableName()
		{
			return this.split()[0];
		}
		public String getFullName()
		{
			if (this.index != null)
				return this.getVariableName() + this.index.getName();
			return this.getVariableName();
		}
		public Node getVariableIndex()
		{
			return this.index;
		}
		public Node.Type getVariableIndexType()
		{
			if (index == null)
				return null;
			return this.index.getType();
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
			final List<String> split = new LinkedList<>();
			while (st.hasMoreTokens())
				split.add(st.nextToken());
			return split.toArray(new String[0]);
		}
	}

	private final Map<Node, Set<State>> clauseMap = new HashMap<>();
	private final Map<Node, Set<State>> callMap = new HashMap<>();

	public FlowEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		this.collectInfo(); // Se encarga de atravesar el CFG y definir usos y definiciones en calls y parameters
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
		final Set<State> states = new HashSet<>();
		final Set<VariableId> uses = new HashSet<>();
		final Set<VariableId> definitions = new HashSet<>();

		if (node instanceof upv.slicing.edg.graph.Variable)
		{
			final upv.slicing.edg.graph.Variable variable = (upv.slicing.edg.graph.Variable) node;

			if (!global || variable.isGlobal())
			{
				final Context context = variable.getContext();
				
				final Node grandParent = EDGTraverser.getParent(edg, EDGTraverser.getParent(edg, node));
				final VariableId variableId;
				if (grandParent.getType() == Node.Type.DataConstructorAccess)
				{
					final Node index = EDGTraverser.getChild(edg, EDGTraverser.getChild(edg, grandParent, 1),0);
					variableId = new VariableId(node.getName(), index);
				}
				else 
					variableId = this.getVariableId(node);
				
				switch (context)
				{
					case Use:
						uses.add(variableId);
						break;
					case Definition:
						definitions.add(variableId);
						break;
					case Def_Use:			// ADDED FOR UnaryOperations (++/--) that both define and use a variable, Scope Variables too (v.addElement(elem)) 
						uses.add(variableId);
						definitions.add(variableId);
						break;
					case Declaration:
						break;
				}
			}
		}
		states.add(new State(node, uses, definitions));

		return states;
	}
	private VariableId getVariableId(Node variable)
	{
		if (!(variable instanceof upv.slicing.edg.graph.Variable))
			throw new RuntimeException("The node is not a variable");

		final upv.slicing.edg.graph.Variable variableInfo = (upv.slicing.edg.graph.Variable) variable;
		final String variableName = variableInfo.getName();
		return new VariableId(variableName);
	}

	// Collect info
	private void collectInfo()
	{
		final List<Node> workList = EDGTraverser.getNodes(this.edg, Node.Type.Clause);
		final Set<Node> worksDone = new HashSet<>();

		while (!workList.isEmpty())
		{
			final Node clause = workList.remove(0);
			final Node parametersNode = EDGTraverser.getChild(edg, clause, Node.Type.ParameterIn);
			final Node clauseResult = EDGTraverser.getResFromNode(edg, clause);
			final List<Node> callResults = EDGTraverser.getOutputs(edg, clauseResult, EDGTraverser.Direction.Forwards);

			final Function<ControlFlowTraverser.NodeWork<State>, Set<ControlFlowTraverser.NodeWork<State>>> newStates = nodeWork -> {
				final Node newNode = nodeWork.getNode();
				final State prevState = nodeWork.getState();
				final Set<State> states = FlowEdgeGenerator.this.getNodeStates(newNode, true);
				final Set<ControlFlowTraverser.NodeWork<State>> newStates1 = new HashSet<>();

				for (State state : states)
				{
					final Set<VariableId> newUses = new HashSet<>(state.uses);
					final Set<VariableId> uses = new HashSet<>(prevState.uses);
					final Set<VariableId> definitions = new HashSet<>(prevState.definitions);

					newUses.removeIf(definitions::contains);
					uses.addAll(newUses);
					definitions.addAll(state.definitions);
					newStates1.add(new ControlFlowTraverser.NodeWork<>(newNode, new State(newNode, uses, definitions)));
				}

				return newStates1;
			};
			final Predicate<ControlFlowTraverser.NodeWork<State>> collectAndStop = nodeWork -> nodeWork.getNode() == clauseResult;
			final ControlFlowTraverser.NodeWork<State> nodeWork = new ControlFlowTraverser.NodeWork<>(parametersNode, new State(parametersNode));
			final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(ControlFlowTraverser.Direction.Forwards, true, true, true, true, true);
			final Set<ControlFlowTraverser.NodeWork<State>> newWorks = ControlFlowTraverser.traverse(edg, nodeWork, configuration, newStates, collectAndStop);
			final Set<State> states = new HashSet<>();

			newWorks.forEach(newWork -> states.add(newWork.getState()));
			this.clauseMap.put(parametersNode, states);

			for (Node callResult : callResults)
			{
				final Node outputClause = EDGTraverser.getAncestor(edg, callResult, Node.Type.Clause);
				if (outputClause != null)
				{
					final Set<State> newInfo = new HashSet<>();
					for (State state : states)
						newInfo.add(new State(callResult, state.uses, state.definitions));
	
					final Set<State> prevInfo = this.callMap.get(callResult);
					
					if (prevInfo != null)
					{
						final Set<State> union = new HashSet<>();
						for (State state : prevInfo)
							union.add(new State(callResult, state.uses, state.definitions));
						union.addAll(newInfo);
						if (union.equals(prevInfo))
							continue;
						
						// TODO Hacer la union y compararla con el prev. Si no ha cambiado es que no hay nada nuevo
						//if (newInfo.equals(prevInfo))
						//	continue;
					}
	
					this.callMap.put(callResult, newInfo);
					this.addWork(workList, worksDone, outputClause);
				}
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
		final NodeConstraint nodeConstraint = new IgnoreEdgeConstraint(Edge.Type.Value, Edge.Type.Structural);
		final EdgeConstraint ignoreConstraint = new AddNodeConstraint(
				nodeConstraint); // Constraint usada para las declaraciones

		for (Variable definitionVariable : definitionNodes)
		{
			final VariableId variableId = definitionVariable.variableId;
			final Node definitionNode = definitionVariable.node;

			final boolean isDefinitionParameters = definitionNode.getType() == Node.Type.ParameterIn;
			final boolean isDefinitionCall = !isDefinitionParameters &&
					EDGTraverser.getSibling(edg, definitionNode, 0).getType() == Node.Type.Call;
			final boolean isDefinitionVariable = !isDefinitionParameters && !isDefinitionCall;

			final Node definitionResultNode;
			if (isDefinitionCall)
			{
				definitionResultNode = getArgumentOutNode(definitionNode);

// TODO ESTE TRATAMIENTO NO DEBE HACERSE AQUI, YA QUE ESTA CLASE ES COMUN A TODOS LOS SLICERS E INDEPENDIENTE DEL LENGUAJE
				final Node callNode = EDGTraverser.getSibling(edg, definitionNode, 0);
				final Node calleeNode = EDGTraverser.getChild(edg, callNode,0);
				final Node nameNode = EDGTraverser.getChild(edg, calleeNode, 1);
				final Node nameChild = EDGTraverser.getChild(edg, nameNode, 0);
				final String Name = nameChild.getType() == Node.Type.Expression ? EDGTraverser.getChild(edg, nameChild,0).getName() : "";
				
				if (Name.equals("<constructor>")) 
				{
					final GlobalVariableConstraint addGVAsteriskConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Add, "*");
					this.edg.addEdge(definitionResultNode, definitionNode, new Edge(Edge.Type.Summary, addGVAsteriskConstraint));
				}
			} else
			{
				final Node definitionResultNode0 = EDGTraverser.getResult(edg, definitionNode);
				final Node definitionResultNode1 =
						definitionResultNode0 == null ? definitionNode : definitionResultNode0;
				definitionResultNode =
						definitionResultNode1.getType() == Node.Type.Expression ? EDGTraverser
								.getResult(edg, definitionResultNode1) : definitionResultNode1;
			}

			final GlobalVariableConstraint addConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Add,
																						variableId.toString());
			final GlobalVariableConstraint letThroughConstraint = new GlobalVariableConstraint(
					SeekingConstraint.Operation.LetThrough, variableId.toString());
			final GlobalVariableConstraint removeConstraint = new GlobalVariableConstraint(
					SeekingConstraint.Operation.Remove, variableId.toString());

			final String className0 = isDefinitionCall ?
					this.getScopeClass(EDGTraverser.getParent(edg, definitionResultNode)) :
					definitionNode.getInfo().getClassName();
			final String className = className0.equals("super") ? definitionNode.getInfo().getClassName() : className0;
			final Node declarationNode = this.getDeclaration(variableId, className, definitionNode);
			if (declarationNode == null) // Do not link variables not explicitly defined, they must be class names or class attributes (STATIC CLASSES/ATTRIBUTES)
				continue;
			final boolean isGlobalVariable = declarationNode instanceof upv.slicing.edg.graph.Variable &&
					((upv.slicing.edg.graph.Variable) declarationNode).isGlobal();

			// Declaration edges
			if (declarationNode != definitionNode)
				if (isDefinitionParameters)
					this.edg.addEdge(declarationNode, definitionNode,
							new Edge(Edge.Type.Flow, letThroughConstraint));
				else if (isDefinitionVariable)
				{
					final Node definitionName = EDGTraverser.getSibling(edg, definitionResultNode, 0);
					this.edg.addEdge(declarationNode, definitionName,
							new Edge(Edge.Type.Flow, ignoreConstraint));
				}
			
/* 
 * CODIGO DAVID, NO SE PARA QUE SIRVE PERO TIENE QUE VER CON LAS DEFINICIONES EN PARAMETERS Y LAS GRAMATICAS (SLICING CON PILA)
 * 
if (declarationNode != null && declarationNode != definitionNode)
	if (isDefinitionParameters)
	{
		final GrammarConstraint grammarConstraint = new GrammarConstraint(this.edg.getGrammar(), declarationNode);
		final Constraints production = new Constraints();
		production.pushEdgeConstraint(new PhaseConstraint(Phase.Input));
		production.pushEdgeConstraint(removeConstraint);
		this.edg.addProduction(grammarConstraint, production);
		this.edg.addEdge(declarationNode, definitionResultNode, 0, new Edge(Edge.Type.Input, grammarConstraint));
	}
	else
		this.edg.addEdge(declarationNode, definitionResultNode, 0, new Edge(Edge.Type.Flow, ignoreConstraint));
 * 
 */		
			// Definitions
			final Node lastNode = this.getLastNode(definitionNode);
			final Set<Node> usesNodes = this.getUses(variableId, definitionNode, lastNode);
			for (Node useNode : usesNodes)
			{
				final boolean isUseLastNode = useNode == lastNode;
				final boolean isUseCall = !isUseLastNode && EDGTraverser.getSibling(edg, useNode, 0).getType() == Node.Type.Call;
				final boolean isUseVariable = !isUseLastNode && !isUseCall;
				final Node useNode1 = !isUseCall ? EDGTraverser.getResult(edg, useNode) : getArgumentInNode(useNode);
				
				// INSIDE FUNCTIONS, GLOBALS & LOCALS
				if(isDefinitionVariable && isUseVariable)
				{
					final Node parent = EDGTraverser.getParent(edg, useNode1);
					if (parent.getType() == Node.Type.DataConstructorAccess)
					{
						final Node variableIndex = variableId.getVariableIndex();
						final Node index = EDGTraverser.getChild(edg, parent, Node.Type.Index);
						if (index.getType() == Node.Type.Literal && variableIndex == null)
						{
							final String indexValue = index.getName();
							final EdgeConstraint dataConstructorConstraint = new DataConstructorConstraint(
									AccessConstraint.Operation.Add, indexValue);
							this.edg.addEdge(definitionResultNode, useNode1,
									new Edge(Edge.Type.Flow, dataConstructorConstraint));
						} else
							this.edg.addEdge(definitionResultNode, useNode1, Edge.Type.Flow);
					}
					else
						this.edg.addEdge(definitionResultNode, useNode1, Edge.Type.Flow);
				}
				
				// ONLY GLOBALS
				if (isGlobalVariable && definitionResultNode != useNode)
				{
					if (isUseCall)
					{    // useNode1 is node argumentIn
						this.edg.addEdge(useNode1, EDGTraverser.getResult(edg, useNode),
								new Edge(Edge.Type.Flow, addConstraint));
						if (isDefinitionVariable)
							this.edg.addEdge(definitionResultNode, useNode1,
									new Edge(Edge.Type.Flow, removeConstraint));
						else if (isDefinitionParameters)
							this.edg.addEdge(definitionNode, useNode1,
									new Edge(Edge.Type.Flow, letThroughConstraint));
						else
							this.edg.addEdge(definitionResultNode, useNode1,
									new Edge(Edge.Type.Flow, letThroughConstraint));
					} else if (isDefinitionParameters && isUseVariable)
					{
						final Set<Edge> calleeResults = EDGTraverser.getEdges(edg, EDGTraverser.getParent(edg, definitionNode), LASTTraverser.Direction.Backwards, Edge.Type.Input);
						for (Edge calleeResult : calleeResults)
						{
							final Node clauseArgsInNode = EDGTraverser.getSibling(edg,
									EDGTraverser.getParent(edg, edg.getEdgeSource(calleeResult)),
									Node.Type.ArgumentIn);
							this.edg.addEdge(clauseArgsInNode, definitionResultNode,
									new Edge(Edge.Type.Input, letThroughConstraint));
						}
						this.edg.addEdge(definitionNode, useNode1, new Edge(Edge.Type.Flow, addConstraint));
					}
					else if (isDefinitionCall && isUseVariable) 
						this.edg.addEdge(definitionResultNode, useNode1, new Edge(Edge.Type.Flow, addConstraint));
					else if (isDefinitionVariable && isUseLastNode)
					{
						final Node clauseNode = EDGTraverser.getNodeFromRes(edg, useNode);
						final Node parameterOutNode = EDGTraverser.getChild(edg, clauseNode, Node.Type.ParameterOut);
						this.edg.addEdge(definitionResultNode, parameterOutNode,
								new Edge(Edge.Type.Flow, removeConstraint));
						final Node routineNode = EDGTraverser.getAncestor(edg, definitionNode, Node.Type.Routine);
						final String routineName = routineNode.getName();
						if (routineName.equals("<constructor>"))
							this.edg.addEdge(definitionResultNode, useNode,
									new Edge(Edge.Type.Flow, removeConstraint));
					}
					else if (isDefinitionCall && isUseLastNode)
					{
						final Node clauseNode = EDGTraverser.getNodeFromRes(edg, useNode);
						final Node parametersNode = EDGTraverser.getChild(edg, clauseNode, Node.Type.ParameterOut);
						this.edg.addEdge(definitionResultNode, parametersNode,
								new Edge(Edge.Type.Flow, letThroughConstraint));
					}
					
					// ARCOS ESPECIALES PARA COMUNICAR FUNCIONES Y COGER DEFINICION DE LA DECLARACION SI PROCEDE
					if (isDefinitionCall)
					{
						// definitionResultNode here is Definition call argsOut Node
						final Node calleeDefCallResult = EDGTraverser
								.getResult(edg, EDGTraverser.getSibling(edg, definitionResultNode, 0));
						this.edg.addEdge(calleeDefCallResult, definitionResultNode,
								new Edge(Edge.Type.Flow, removeConstraint));

						final Set<Edge> calledFunctionClauseEdges = EDGTraverser
								.getEdges(edg, calleeDefCallResult, LASTTraverser.Direction.Forwards, Edge.Type.Input);
						for (Edge clauseEdge : calledFunctionClauseEdges)
						{
							final Node clauseParameterOut = EDGTraverser
									.getChild(edg, edg.getEdgeTarget(clauseEdge), Node.Type.ParameterOut);
							this.edg.addEdge(clauseParameterOut, definitionResultNode,
									new Edge(Edge.Type.Output, letThroughConstraint));
						}
					}
					
					// Arcos para coger la definicion en la declaracion de la funcion si la hubiera 
// TODO Discutir si habria que coger esta definición cuando haya llamadas a esta funcion
					if (isDefinitionParameters)
					{
						final Node declarationResultNode = EDGTraverser.getResult(edg, declarationNode);
						if (declarationResultNode != null)
							this.edg.addEdge(declarationResultNode, definitionNode,
									new Edge(Edge.Type.Input, removeConstraint));
					}
				}
			}
		}
	}	
//	private Node getDeclaration(VariableId variableId, Node definitionNode)
//	{
//		final Predicate<Node> collectAndStop = new Predicate<Node>() {
//			public boolean test(Node node)
//			{
//				if (!(node.getInfo() instanceof VariableInfo))
//					return false;
//				final VariableInfo variableInfo = (VariableInfo) node.getInfo();
//				if (!variableInfo.isDeclaration())
//					return false;
//				final VariableId variableId0 = FlowEdgeGenerator.this.getVariableId(node);
//				if (variableId.equals(variableId0))
//					return true;
//				return false;
//			}
//		};
//		final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(ControlFlowTraverser.Direction.Backwards, true, false, true, false, true);
//		final Set<Node> declaration = ControlFlowTraverser.traverse(definitionNode, configuration, collectAndStop);
//		if (!declaration.isEmpty())
//			return declaration.iterator().next();
//
//		final String variableName = variableId.getVariableName();
//		// final List<Node> variables = this.getVariables(variableName, true, null, true);
//		final List<Node> variables = this.getVariables(variableName, Context.Declaration, true);
//		
//		return variables.isEmpty() ? null : variables.get(0);
//	}
// 	GET DECLARATION WITH CLASS. Necessary to differentiate global variables with the same name in different classes
	private Node getDeclaration(VariableId variableId, String clazz, Node definitionNode)
	{
		final Predicate<Node> collectAndStop = node -> {
			if (!(node instanceof upv.slicing.edg.graph.Variable))
				return false;
			final upv.slicing.edg.graph.Variable variable = (upv.slicing.edg.graph.Variable) node;
			if (!variable.isDeclaration())
				return false;
			final VariableId variableId0 = FlowEdgeGenerator.this.getVariableId(node);
			if (!variableId.equals(variableId0))
				return false;
			final String clazz0 = variable.getInfo().getClassName();
			return clazz0.equals(clazz);
		};
		final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(
				ControlFlowTraverser.Direction.Backwards, false, false, true, false, true);
		final Set<Node> declaration = ControlFlowTraverser.traverse(edg, definitionNode, configuration, collectAndStop);
		if (!declaration.isEmpty())
			return declaration.iterator().next();


		// Global Variable Declaration
		// AQUI USAR LA INFO DEL NODO CLASE		
		final String variableName = variableId.getVariableName();
		//final Node moduleNode = EDGTraverserNew.getAncestor(definitionNode, Node.Info.Type.Module);

		final Node moduleNode = EDGTraverser.getModuleByName(edg, clazz);
		final ClassInfo info = (ClassInfo) moduleNode.getInfo().getInfo()[2];
		final Node variableDeclaration = info.getVariables().get(variableName);

		return variableDeclaration;
//		OLD CODE
//		final List<Node> variables = this.getVariables(variableName, Context.Declaration, clazz, true);
//		return variables.isEmpty() ? null : variables.get(0);
	}
	
	private List<Variable> getDefinitions()
	{
		final List<Variable> definitions = new LinkedList<>();

		// Definition nodes
		final List<Node> definitionNodes = this.getVariables(null, Context.Definition, null);

		// ADDED FOR DEF_USE CONTEXT VARIABLES
		final List<Node> definitionUseNodes= this.getVariables(null, Context.Def_Use, null);
		definitionNodes.addAll(definitionUseNodes);
		
		for (Node definitionNode : definitionNodes)
		{
			final Node parent = EDGTraverser.getParent(edg, definitionNode);
			if (parent.getType() == Node.Type.DataConstructorAccess ||
				parent.getType() == Node.Type.FieldAccess)
			{
				final Node dataAccessResultNode = EDGTraverser.getResFromNode(edg, parent);
				final Node index = EDGTraverser.getChild(edg, parent, Node.Type.Index);
				final VariableId variableId = new VariableId(definitionNode.getName(), index);
				definitions.add(new Variable(variableId, dataAccessResultNode));
			} else
			{
				final VariableId variableId = this.getVariableId(definitionNode);
				definitions.add(new Variable(variableId, definitionNode));
			}
		}

//		final Node grandParent = EDGTraverserNew.getParent(EDGTraverserNew.getParent(definitionNode));
//		final boolean isEqualityContext = grandParent.getInfo().getType() == Node.Info.Type.Equality;
//		if(isEqualityContext)
//		{
//			final Node rightHandNode = EDGTraverserNew.getChild(grandParent,1);
//			final Node child = EDGTraverserNew.getChild(rightHandNode,1);
//			if (child.getInfo().getType() == Node.Info.Type.DataConstructor)
//			{
//				
//			}
//		}

		// Call nodes
		for (Entry<Node, Set<State>> entry : this.callMap.entrySet())
		{
			final Node callNode = entry.getKey();
			final Set<State> states = entry.getValue();
			final Set<VariableId> variableIds = new HashSet<>();

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
			final Set<VariableId> variableIds = new HashSet<>();

			for (State state : states)
				variableIds.addAll(state.uses);
			for (VariableId variableId : variableIds)
				definitions.add(new Variable(variableId, parametersNode));
		}

		return definitions;
	}
	private Set<Node> getUses(VariableId variableId, Node definitionNode, Node lastNode)
	{
		final Predicate<Node> collect = node -> {
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
		};
		final Predicate<Node> stop = node -> {
			if (node == definitionNode)
				return false;
			final Set<State> states = FlowEdgeGenerator.this.getNodeStates(node, false);
			if (states.isEmpty())
				return false;
			for (State state : states)
				if (!state.definitions.contains(variableId))
					return false;
			return true;
		};
		final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(ControlFlowTraverser.Direction.Forwards, true, true, true, true, true);

		return ControlFlowTraverser.traverse(edg, definitionNode, configuration, collect, stop);
	}

//	private List<Variable> getExplicitDefinitions()
//	{
//		final List<Variable> definitions = new LinkedList<Variable>();
//
//		// Definition nodes
//		final List<Node> definitionNodes = this.getVariables(null, Context.Definition, null);
//
//		// ADDED FOR DEF_USE CONTEXT VARIABLES
//		final List<Node> definitionUseNodes= this.getVariables(null, Context.Def_Use, null);
//		definitionNodes.addAll(definitionUseNodes);
//		
//		for (Node definitionNode : definitionNodes)
//		{
//			final VariableId variableId = this.getVariableId(definitionNode);
//			definitions.add(new Variable(variableId, definitionNode));
//		}
//		return definitions;
//	} 
	
	private Node getLastNode(Node node)
	{
		final Node clauseNode = EDGTraverser.getAncestor(edg, node, Node.Type.Clause);
		if (clauseNode != null)
			return EDGTraverser.getResFromNode(edg, clauseNode);
		return EDGTraverser.getResult(edg, node);
	}
	private Node getArgumentsNode(Node node)
	{
		final Node callNode = EDGTraverser.getSibling(edg, node, 0);
		return EDGTraverser.getChild(edg, callNode, Node.Type.Arguments);
	}
	private Node getArgumentInNode(Node node)
	{
		final Node callNode = EDGTraverser.getSibling(edg, node, 0);
		return EDGTraverser.getChild(edg, callNode, Node.Type.ArgumentIn);
	}
	private Node getArgumentOutNode(Node node)
	{
		final Node callNode = EDGTraverser.getSibling(edg, node, 0);
		return EDGTraverser.getChild(edg, callNode, Node.Type.ArgumentOut);
	}
	
//	private Node getArgumentInNode(Node node)
//	{
//		final Node argumentsNode = getArgumentsNode(node);
//		return EDGTraverserNew.getChild(argumentsNode, EDGTraverserNew.getChildren(argumentsNode).size() - 2);
//	}
//	private Node getArgumentOutNode(Node node)
//	{
//		final Node argumentsNode = getArgumentsNode(node);
//		return EDGTraverserNew.getChild(argumentsNode, EDGTraverserNew.getChildren(argumentsNode).size() - 1);
//	}
	
	
	private String getScopeClass(Node callResult) // EN CASO DE QUE LA VARIABLE GLOBAL QUE SE MODIFICA PERTENEZCA A OTRA CLASE, HAY QUE SABER A QUE CLASE PERTENECE
	{
		final Node callNode = EDGTraverser.getSibling(edg, callResult, 0);
		final Node scopeNode = EDGTraverser.getChild(edg, EDGTraverser.getChild(edg, callNode, 0),0);
		final List<Node> scopeChildren = EDGTraverser.getChildren(edg, scopeNode);
		if (scopeChildren.isEmpty())
			return callResult.getInfo().getClassName();
		else
		{
			final Node moduleRef0 = scopeChildren.get(0);
			final Node moduleRef1 = moduleRef0.getType() != Node.Type.Expression ? moduleRef0 : EDGTraverser.getChild(edg, moduleRef0, 0);
			final Node moduleRef = moduleRef1.getType() == Node.Type.TypeTransformation ? EDGTraverser.getChild(edg, EDGTraverser.getChild(edg, moduleRef1, 0),0) :
				moduleRef1;
			final Node.Type moduleRefType = moduleRef.getType();
			
			final String moduleName0 = moduleRefType == Node.Type.Variable ? moduleRef.getInfo().getInfo()[1].toString() : null;
			final String moduleName1 = moduleRefType == Node.Type.Literal || moduleRefType == Node.Type.Type ? moduleRef.getName() : moduleName0;
			// Esto solo es cierto cuando la call es a un constructor. Habria que analizar el tipo que devuelve el método para asegurarse que es el indicado
			final String moduleName = moduleRefType == Node.Type.Call ? getScopeClass(moduleRef) : moduleName1;
			 
			return moduleName;
		}
	}

		
}