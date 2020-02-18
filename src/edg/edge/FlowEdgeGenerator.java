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

import edg.constraint.AccessConstraint;
import edg.constraint.AddNodeConstraint;
import edg.constraint.Constraints;
import edg.constraint.DataConstructorConstraint;
import edg.constraint.EdgeConstraint;
import edg.constraint.GlobalVariableConstraint;
import edg.constraint.GrammarConstraint;
import edg.constraint.IgnoreEdgeConstraint;
import edg.constraint.NodeConstraint;
import edg.constraint.PhaseConstraint;
import edg.constraint.SeekingConstraint;
import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.graph.VariableInfo.Context;
import edg.slicing.Phase;
import edg.traverser.ControlFlowTraverser;
import edg.traverser.EDGTraverser;
import edg.traverser.EDGTraverser.Direction;
import edg.traverser.ControlFlowTraverser.NodeWork;
import edg.ASTBuilder.ClassInfo;

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
		public String getFullName()
		{
			if (this.index != null)
				return this.getVariableName() + this.index.getData().getName();
			return this.getVariableName();
		}
		public Node getVariableIndex()
		{
			return this.index;
		}
		public NodeInfo.Type getVariableIndexType()
		{
			if (index == null)
				return null;
			return this.index.getData().getType();
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
				
				final Node grandParent = EDGTraverser.getParent(EDGTraverser.getParent(node));
				final VariableId variableId;
				if (grandParent.getData().getType() == NodeInfo.Type.DataConstructorAccess)
				{
					final Node index = EDGTraverser.getChild(EDGTraverser.getChild(grandParent, 1),0);
					variableId = new VariableId(nodeInfo.getName(), index);
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
				if (outputClause != null)
				{
					final Set<State> newInfo = new HashSet<State>();
					for (State state : states)
						newInfo.add(new State(callResult, state.uses, state.definitions));
	
					final Set<State> prevInfo = this.callMap.get(callResult);
					
					if (prevInfo != null)
					{
						final Set<State> union = new HashSet<State>();
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
		final NodeConstraint nodeConstraint = new IgnoreEdgeConstraint(EdgeInfo.Type.Value);
		final EdgeConstraint ignoreConstraint = new AddNodeConstraint(nodeConstraint); // Constraint usada para las declaraciones
		
		for (Variable definitionVariable : definitionNodes)
		{	
			final VariableId variableId = definitionVariable.variableId;
			final Node definitionNode = definitionVariable.node;			
			
			final boolean isDefinitionParameters = definitionNode.getData().getType() == NodeInfo.Type.Parameters;
			final boolean isDefinitionCall = !isDefinitionParameters && EDGTraverser.getSibling(definitionNode, 0).getData().getType() == NodeInfo.Type.Call;
			final boolean isDefinitionVariable = !isDefinitionParameters && !isDefinitionCall;
		
			final Node definitionResultNode;
			if (isDefinitionCall)
			{
				definitionResultNode = getArgumentOutNode(definitionNode);
				
// TODO ESTE TRATAMIENTO NO DEBE HACERSE AQUI, YA QUE ESTA CLASE ES COMUN A TODOS LOS SLICERS E INDEPENDIENTE DEL LENGUAJE
				final Node callNode = EDGTraverser.getSibling(definitionNode,0);
				final Node calleeNode = EDGTraverser.getChild(callNode,0);
				final Node nameNode = EDGTraverser.getChild(calleeNode, 1);
				final Node nameChild = EDGTraverser.getChild(nameNode, 0);
				final String Name = nameChild.getData().getType() == NodeInfo.Type.Expression ? EDGTraverser.getChild(nameChild,0).getData().getName() : "";
				
				if (Name.equals("<constructor>")) 
				{
					final GlobalVariableConstraint addGVAsteriskConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Add, "*");
					this.edg.addEdge(definitionResultNode, definitionNode, 0, new EdgeInfo(EdgeInfo.Type.Summary, addGVAsteriskConstraint));
				}
			}
			else
			{
				final Node definitionResultNode0 = EDGTraverser.getResult(definitionNode); 
				final Node definitionResultNode1 = definitionResultNode0 == null ? definitionNode : definitionResultNode0;
				definitionResultNode = definitionResultNode1.getData().getType() == NodeInfo.Type.Expression ? EDGTraverser.getResult(definitionResultNode1) : definitionResultNode1;
			}
			
			final GlobalVariableConstraint addConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Add, variableId.toString());
			final GlobalVariableConstraint letThroughConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.LetThrough, variableId.toString());
			final GlobalVariableConstraint removeConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Remove, variableId.toString());
			
			final String className0 = isDefinitionCall ? this.getScopeClass(EDGTraverser.getParent(definitionResultNode)) : definitionNode.getData().getInfo().getClassName();
			final String className = className0.equals("super") ? definitionNode.getData().getInfo().getClassName() : className0;
			final Node declarationNode = this.getDeclaration(variableId, className, definitionNode);
if (declarationNode == null) // Do not link variables not explicitly defined, they must be class names or class attributes
	continue;
			final boolean isGlobalVariable = declarationNode != null && declarationNode.getData() instanceof VariableInfo && ((VariableInfo) declarationNode.getData()).isGlobal();
			
			// Declaration edges
			if (declarationNode != null && declarationNode != definitionNode) 
				if (isDefinitionParameters)
					this.edg.addEdge(declarationNode, definitionNode, 0, new EdgeInfo(EdgeInfo.Type.Input, letThroughConstraint));
				else if (isDefinitionVariable)
				{
					final Node definitionName = EDGTraverser.getSibling(definitionResultNode, 0);
					this.edg.addEdge(declarationNode, definitionName, 0, new EdgeInfo(EdgeInfo.Type.Flow, ignoreConstraint));
				}
			
/* 
 * CODIGO DAVID, NO SE PARA QUE SIRVE PERO TIENE QUE VER CON LAS DEFINICIONES EN PARAMETERS
 * 
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
 * 
 */		
			// Definitions
			final Node lastNode = this.getLastNode(definitionNode);
			final Set<Node> usesNodes = this.getUses(variableId, definitionNode, lastNode);
			for (Node useNode : usesNodes)
			{
				final boolean isUseLastNode = useNode == lastNode;
				final boolean isUseCall = !isUseLastNode && EDGTraverser.getSibling(useNode, 0).getData().getType() == NodeInfo.Type.Call;
				final boolean isUseVariable = !isUseLastNode && !isUseCall;
				final Node useNode1 = !isUseCall ? EDGTraverser.getResult(useNode) : getArgumentInNode(useNode);
				
				// INSIDE FUNCTIONS, GLOBALS & LOCALS
				if(isDefinitionVariable && isUseVariable)
				{
					final Node sibling = EDGTraverser.getSibling(useNode1, 0);
					if (sibling.getData().getType() == NodeInfo.Type.DataConstructorAccess) 
					{
						final Node variableIndex = variableId.getVariableIndex();
						final Node index = EDGTraverser.getChild(EDGTraverser.getChild(sibling,1),0);
						if (index.getData().getType() == NodeInfo.Type.Literal && variableIndex == null)
						{
							final String indexValue = index.getData().getName();
							final EdgeConstraint dataConstructorConstraint = new DataConstructorConstraint(AccessConstraint.Operation.Add, indexValue);
							this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, dataConstructorConstraint));
						}
						else
							this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow));	
					}
					else
						this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow));
				}
				
				
				// ONLY GLOBALS
				if (isGlobalVariable && definitionResultNode != useNode)
				{
// 					EDGES DEFINED CASE BY CASE:
//					
//					// EXPLICIT DEFINITION
//					if (isDefinitionVariable && isUseCall) // La definición global escapa a la funcion (sale sin ser usada (lastNode) o se usa en otra llamada (useCall))
//					{	
//						this.edg.addEdge(useNode1, EDGTraverser.getResult(useNode), 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
//						this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, removeConstraint));
//					}
//					else if (isDefinitionVariable && isUseLastNode)
//						this.edg.addEdge(definitionResultNode, parametersNode, 0, new EdgeInfo(EdgeInfo.Type.Flow, removeConstraint));
//					
//					// PARAMETERS DEFINITION
//					else if (isDefinitionParameters && isUseVariable) // Usos explicitos +GVname en los arcos 
//						this.edg.addEdge(definitionNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
//					else if (isDefinitionParameters && isUseCall) 
//					{	
//						this.edg.addEdge(useNode1, EDGTraverser.getResult(useNode), 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
//						this.edg.addEdge(definitionNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, letThroughConstraint));
//					}
//					
//					// CALL DEFINITION
//					else if (isDefinitionCall && isUseVariable) 
//						this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
//					else if (isDefinitionCall && isUseCall) 
//					{	
//						this.edg.addEdge(useNode1, EDGTraverser.getResult(useNode), 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
//						this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, letThroughConstraint));
//					}
//					else if (isDefinitionCall && isUseLastNode)
//						this.edg.addEdge(definitionResultNode, parametersNode, 0, new EdgeInfo(EdgeInfo.Type.Flow, letThroughConstraint));
//					
					// EDGES GROUPED IN COMMON PERFORMANCE:
					if (isUseCall)
					{	
						this.edg.addEdge(useNode1, EDGTraverser.getResult(useNode), 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
						if (isDefinitionVariable)
							this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, removeConstraint));
						else if (isDefinitionParameters)
							this.edg.addEdge(definitionNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, letThroughConstraint));
						else
							this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, letThroughConstraint));
					}
					else if (isDefinitionParameters && isUseVariable)
					{
						final List<Edge> calleeResults = EDGTraverser.getEdges(EDGTraverser.getParent(definitionNode), Direction.Backwards, EdgeInfo.Type.Input);
						for (Edge calleeResult : calleeResults)
						{
							final Node clauseArgsOutNode = EDGTraverser.getSibling(EDGTraverser.getParent(calleeResult.getFrom()),3);
							this.edg.addEdge(clauseArgsOutNode, definitionResultNode, 0, new EdgeInfo(EdgeInfo.Type.Output, letThroughConstraint));
						}
						this.edg.addEdge(definitionNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
					}
					else if (isDefinitionCall && isUseVariable) 
						this.edg.addEdge(definitionResultNode, useNode1, 0, new EdgeInfo(EdgeInfo.Type.Flow, addConstraint));
					else if (isDefinitionVariable && isUseLastNode)
					{
						final Node parametersNode = isUseLastNode ? EDGTraverser.getSibling(useNode,0) : null; 
						this.edg.addEdge(definitionResultNode, parametersNode, 0, new EdgeInfo(EdgeInfo.Type.Flow, removeConstraint));
						final Node routineNode = EDGTraverser.getAncestor(definitionNode, NodeInfo.Type.Routine);
						final String routineName = routineNode.getData().getName();
						if (routineName.equals("<constructor>"))
							this.edg.addEdge(definitionResultNode, useNode, 0, new EdgeInfo(EdgeInfo.Type.Flow, removeConstraint));
					}
					else if (isDefinitionCall && isUseLastNode)
					{
						final Node parametersNode = isUseLastNode ? EDGTraverser.getSibling(useNode,0) : null; 
						this.edg.addEdge(definitionResultNode, parametersNode, 0, new EdgeInfo(EdgeInfo.Type.Flow, letThroughConstraint));
					}
					
					// ARCOS ESPECIALES PARA COMUNICAR FUNCIONES Y COGER DEFINICION DE LA DECLARACION SI PROCEDE
					if (isDefinitionCall)
						this.edg.addEdge(EDGTraverser.getResult(EDGTraverser.getSibling(definitionResultNode, 0)), definitionResultNode, 0, new EdgeInfo(EdgeInfo.Type.Flow, removeConstraint));
					
					// Arcos para coger la definicion en la declaracion de la funcion si la hubiera 
// TODO Discutir si habria que coger esta definición cuando haya llamadas a esta funcion
					if (isDefinitionParameters)
					{
						final Node declarationResultNode = EDGTraverser.getResult(declarationNode);
						if (declarationResultNode != null)
							this.edg.addEdge(declarationResultNode, definitionNode, 0, new EdgeInfo(EdgeInfo.Type.Output, removeConstraint));
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
//				if (!(node.getData() instanceof VariableInfo))
//					return false;
//				final VariableInfo variableInfo = (VariableInfo) node.getData();
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
		final Predicate<Node> collectAndStop = new Predicate<Node>() {
			public boolean test(Node node)
			{
				if (!(node.getData() instanceof VariableInfo))
					return false;
				final VariableInfo variableInfo = (VariableInfo) node.getData();
				if (!variableInfo.isDeclaration())
					return false;
				final VariableId variableId0 = FlowEdgeGenerator.this.getVariableId(node);
				if (!variableId.equals(variableId0))
					return false;
				final String clazz0 = variableInfo.getInfo().getClassName();
				if (clazz0.equals(clazz))
					return true;
				return false;
			}
		};
		final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(ControlFlowTraverser.Direction.Backwards, false, false, true, false, true);
		final Set<Node> declaration = ControlFlowTraverser.traverse(definitionNode, configuration, collectAndStop);
		if (!declaration.isEmpty())
			return declaration.iterator().next();

		
		
		// Global Variable Declaration
		// AQUI USAR LA INFO DEL NODO CLASE		
		final String variableName = variableId.getVariableName();
		//final Node moduleNode = EDGTraverser.getAncestor(definitionNode, NodeInfo.Type.Module);
		
		final Node moduleNode = EDGTraverser.getModuleByName(edg, clazz);
		final ClassInfo info = (ClassInfo) moduleNode.getData().getInfo().getInfo()[2];
		final Node variableDeclaration = info.getVariables().get(variableName);

		return variableDeclaration;
//		OLD CODE
//		final List<Node> variables = this.getVariables(variableName, Context.Declaration, clazz, true);
//		return variables.isEmpty() ? null : variables.get(0);
	}
	
	private List<Variable> getDefinitions()
	{
		final List<Variable> definitions = new LinkedList<Variable>();

		// Definition nodes
		final List<Node> definitionNodes = this.getVariables(null, Context.Definition, null);

		// ADDED FOR DEF_USE CONTEXT VARIABLES
		final List<Node> definitionUseNodes= this.getVariables(null, Context.Def_Use, null);
		definitionNodes.addAll(definitionUseNodes);
		
		for (Node definitionNode : definitionNodes)
		{
			final Node grandParent = EDGTraverser.getParent(EDGTraverser.getParent(definitionNode));
			if (grandParent.getData().getType() == NodeInfo.Type.DataConstructorAccess || grandParent.getData().getType() == NodeInfo.Type.FieldAccess)
			{
				final Node dataAccessResultNode = EDGTraverser.getSibling(grandParent,1);
				final Node index = EDGTraverser.getChild(EDGTraverser.getChild(grandParent,1),0);
				final VariableId variableId = new VariableId(definitionNode.getData().getName(), index);
				definitions.add(new Variable(variableId, dataAccessResultNode));
			}
			else
			{
				final VariableId variableId = this.getVariableId(definitionNode);
				definitions.add(new Variable(variableId, definitionNode));
			}
		}

//		final Node grandParent = EDGTraverser.getParent(EDGTraverser.getParent(definitionNode));
//		final boolean isEqualityContext = grandParent.getData().getType() == NodeInfo.Type.Equality;
//		if(isEqualityContext)
//		{
//			final Node rightHandNode = EDGTraverser.getChild(grandParent,1);
//			final Node child = EDGTraverser.getChild(rightHandNode,1);
//			if (child.getData().getType() == NodeInfo.Type.DataConstructor)
//			{
//				
//			}
//		}
		
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
		final Node clauseNode = EDGTraverser.getAncestor(node, NodeInfo.Type.Clause);
		if (clauseNode != null)
			return EDGTraverser.getChild(clauseNode, 3);
		return EDGTraverser.getResult(node);
	}
	private Node getArgumentsNode(Node node)
	{
		final Node callNode = EDGTraverser.getSibling(node, 0);
		return EDGTraverser.getChild(callNode, 1);
	}
	private Node getArgumentInNode(Node node)
	{
		final Node callNode = EDGTraverser.getSibling(node, 0);
		return EDGTraverser.getChild(callNode, 2);
	}
	private Node getArgumentOutNode(Node node)
	{
		final Node callNode = EDGTraverser.getSibling(node, 0);
		return EDGTraverser.getChild(callNode, 3);
	}
	
//	private Node getArgumentInNode(Node node)
//	{
//		final Node argumentsNode = getArgumentsNode(node);
//		return EDGTraverser.getChild(argumentsNode, EDGTraverser.getChildren(argumentsNode).size() - 2);
//	}
//	private Node getArgumentOutNode(Node node)
//	{
//		final Node argumentsNode = getArgumentsNode(node);
//		return EDGTraverser.getChild(argumentsNode, EDGTraverser.getChildren(argumentsNode).size() - 1);
//	}
	
	
	private String getScopeClass(Node callResult) // EN CASO DE QUE LA VARIABLE GLOBAL QUE SE MODIFICA PERTENEZCA A OTRA CLASE, HAY QUE SABER A QUE CLASE PERTENECE
	{
		final Node callNode = EDGTraverser.getSibling(callResult, 0);
		final Node scopeNode = EDGTraverser.getChild(EDGTraverser.getChild(callNode, 0),0);
		final List<Node> scopeChildren = EDGTraverser.getChildren(scopeNode);
		if (scopeChildren.isEmpty())
			return callResult.getData().getInfo().getClassName();
		else
		{
			final Node moduleRef0 = scopeChildren.get(0);
			final Node moduleRef1 = moduleRef0.getData().getType() != NodeInfo.Type.Expression ? moduleRef0 : EDGTraverser.getChild(moduleRef0, 0);
			final Node moduleRef = moduleRef1.getData().getType() == NodeInfo.Type.TypeTransformation ? EDGTraverser.getChild(EDGTraverser.getChild(moduleRef1, 0),0) :
				moduleRef1;
			final NodeInfo.Type moduleRefType = moduleRef.getData().getType();
			
			final String moduleName0 = moduleRefType == NodeInfo.Type.Variable ? moduleRef.getData().getInfo().getInfo()[1].toString() : null;
			final String moduleName1 = moduleRefType == NodeInfo.Type.Literal || moduleRefType == NodeInfo.Type.Type ? moduleRef.getData().getName() : moduleName0;
			// Esto solo es cierto cuando la call es a un constructor. Habria que analizar el tipo que devuelve el método para asegurarse que es el indicado
			final String moduleName = moduleRefType == NodeInfo.Type.Call ? getScopeClass(moduleRef) : moduleName1;
			 
			return moduleName;
		}
	}

		
}