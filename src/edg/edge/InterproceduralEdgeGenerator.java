package edg.edge;

import java.util.LinkedList;
import java.util.List;

import edg.constraint.PhaseConstraint;
import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.slicing.Phase;
import edg.traverser.EDGTraverser;

public class InterproceduralEdgeGenerator extends EdgeGenerator
{
	public InterproceduralEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		final List<Node> calls = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Call);

		for (Node call : calls)
		{
			this.generateInputEdges(call);
			this.generateOutputEdges(call);
		}
	}

	/************************************/
	/************ Input edges ***********/
	/************************************/
	private void generateInputEdges(Node call)
	{
		final List<Node> possibleClauses = this.getPossibleClauses(call);
		final List<Node> matchingClauses = this.getMatchingClauses(possibleClauses, call);
		final Node callee = EDGTraverser.getChild(call, 0);
		final Node calleeResultNode = EDGTraverser.getChild(callee, 2);
		final Node arguments = EDGTraverser.getChild(call, 1);
		final Node argumentsIn = EDGTraverser.getChild(call, 2);
		final List<Node> argumentNodes = EDGTraverser.getChildren(arguments);

		for (Node matchingClause : matchingClauses)
		{
			final Node parameters = EDGTraverser.getChild(matchingClause, 0);
			final List<Node> parameterNodes = EDGTraverser.getChildren(parameters);

			this.edg.addEdge(calleeResultNode, matchingClause, 0, new EdgeInfo(EdgeInfo.Type.Input, new PhaseConstraint(Phase.Input)));
			for (int argumentIndex = 0; argumentIndex < argumentNodes.size(); argumentIndex++)
			{
				final Node argument = argumentNodes.get(argumentIndex);
				final Node parameter = parameterNodes.get(argumentIndex);
				final Node argumentResult = EDGTraverser.getResult(argument);
				final Node parameterResult = EDGTraverser.getResult(parameter);

				if (argumentResult != null && parameterResult != null)
					this.edg.addEdge(argumentResult, parameterResult, 0, new EdgeInfo(EdgeInfo.Type.Input, new PhaseConstraint(Phase.Input)));
			}
			//this.edg.addEdge(arguments, parameters, 0, new EdgeInfo(EdgeInfo.Type.Call, new PhaseConstraint(Phase.Input)));
			this.edg.addEdge(argumentsIn, parameters, 0, new EdgeInfo(EdgeInfo.Type.Call, new PhaseConstraint(Phase.Input)));
		}
	}
	private List<Node> getPossibleClauses(Node call)
	{
		final Node callee = EDGTraverser.getChild(call, 0);
		final Node scopeNode = EDGTraverser.getChild(callee, 0);
		final Node nameNode = EDGTraverser.getChild(callee, 1);
		final List<Node> scopeChildren = EDGTraverser.getChildren(scopeNode);
		final Node routineArguments = EDGTraverser.getChild(call, 1);
		final List<Node> arguments = EDGTraverser.getChildren(routineArguments);

		// Module
		final Node moduleRef0 = scopeChildren.isEmpty() ? EDGTraverser.getAncestor(call, NodeInfo.Type.Module) : scopeChildren.get(0);
		final Node moduleRef = moduleRef0.getData().getType() != NodeInfo.Type.Expression ? moduleRef0 : EDGTraverser.getChild(moduleRef0, 0);
		final NodeInfo.Type moduleRefType = moduleRef.getData().getType();
		
		//String moduleName = moduleRefType == NodeInfo.Type.Literal ? moduleRef.getData().getName() : null;//moduleRef.getData().getInfo().getClassName();
		final String moduleName0 = moduleRefType == NodeInfo.Type.Variable ? moduleRef.getData().getInfo().getInfo()[1].toString() : null;
		final String moduleName1 = moduleRefType == NodeInfo.Type.Literal ? moduleRef.getData().getName() : moduleName0;
		String moduleName = scopeChildren.size() == 0 ? moduleRef.getData().getName() : moduleName1;
		
		// Function
		final Node name0 = EDGTraverser.getChild(nameNode, 0);
		final Node name = name0.getData().getType() != NodeInfo.Type.Expression ? name0 : EDGTraverser.getChild(name0, 0);
		final NodeInfo.Type nameType = name.getData().getType();
		String routineName = name.getData().getName();

		// - M:R()	all routines of all modules				=> _:_
		// - m:R()	all routines of module m				=> m:_
		// - M:r()	routine r of all modules				=> _:r
		// - m:r()	routine f of module m					=> m:r
		// - r()	routine f of current module				=> m:r
		// - ar()	this anonymous routine					=> null:_
		// - X()	all routines (including anonymous ones)	=> null:null
		if (moduleRefType != NodeInfo.Type.Variable && moduleRefType != NodeInfo.Type.Literal && moduleRefType != NodeInfo.Type.Module)
			moduleName = "_";
		if (nameType != NodeInfo.Type.Literal)
			routineName = "_";
		if (nameType != NodeInfo.Type.Literal && scopeChildren.isEmpty())
			moduleName = null;
		if (moduleName == null && nameType != NodeInfo.Type.Routine)
			routineName = null;

		final List<Node> possibleClauses = new LinkedList<Node>();
		final List<Node> clauses = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Clause);
		final boolean thisAnonymousRoutine = moduleName == null && routineName != null;
		final boolean allRoutines = moduleName == null && routineName == null;

		for (Node clause : clauses)
		{
			final Node routine = EDGTraverser.getParent(clause);
			if (thisAnonymousRoutine && routine != name)
				continue;
			
//			final String clauseModule = clause.getData().getInfo().getClassName();
//			if (TypeName != null && !TypeName.equals(clauseModule))
//				continue;
			
			if (!thisAnonymousRoutine && !allRoutines)
			{
				final Node module = EDGTraverser.getParent(routine);
				final NodeInfo.Type moduleType = module.getData().getType();
				if (moduleType != NodeInfo.Type.Module)
					continue;
				final String moduleText = module.getData().getName();
				if (!moduleName.equals(moduleText) && !moduleName.equals("_"))
					continue;
				final String routineText = routine.getData().getName();
				if (!routineName.equals(routineText) && !routineName.equals("_"))
					continue;
			}

			final Node parameters = EDGTraverser.getChild(clause, 0);
			final List<Node> parameterNodes = EDGTraverser.getChildren(parameters);
			if (arguments.size() != parameterNodes.size())
				continue;

			possibleClauses.add(clause);
		}

		return possibleClauses;
	}
	private List<Node> getMatchingClauses(List<Node> possibleClauses, Node call)
	{
		final List<Node> matchingClauses = new LinkedList<Node>();

		for (Node possibleClause : possibleClauses)
			if (this.matchClause(possibleClause, call))
				matchingClauses.add(possibleClause);

		return matchingClauses;
	}
	private boolean matchClause(Node possibleClause, Node call)
	{
		final Node parameters = EDGTraverser.getChild(possibleClause, 0);
		final List<Node> parameterNodes = EDGTraverser.getChildren(parameters);
		final Node arguments = EDGTraverser.getChild(call, 1);
		final List<Node> argumentNodes = EDGTraverser.getChildren(arguments);
		if (argumentNodes.size() != parameterNodes.size())
			return false;

		for (int parameterIndex = 0; parameterIndex < parameterNodes.size(); parameterIndex++)
		{
			final Node parameter = parameterNodes.get(parameterIndex);
			final Node argument = argumentNodes.get(parameterIndex);
			final List<Node[]> matches = this.getMatches(parameter, argument);

			if (matches.isEmpty())
				return false;
		}
		return true;
	}

	/************************************/
	/*********** Output edges ***********/
	/************************************/
	private void generateOutputEdges(Node call)
	{
		final Node callResult = EDGTraverser.getSibling(call, 1);
		final Node callee = EDGTraverser.getChild(call, 0);
		final Node calleeResult = EDGTraverser.getChild(callee, 2);
		final List<Node> callingFunctions = EDGTraverser.getInputs(calleeResult, EDGTraverser.Direction.Forwards);

		for (Node callingFunction : callingFunctions)
		{
			final Node result = EDGTraverser.getResult(callingFunction);

			if (result != null)
				this.edg.addEdge(result, callResult, 0, new EdgeInfo(EdgeInfo.Type.Output, new PhaseConstraint(Phase.Output)));
		}
	}
	
	
	
	/*****************************************/
	/** Sacar tipos buscando la declaracion **/
	/*****************************************/
	
//	private String getVarTypeName(Node node)
//	{
//		final String varName = node.getData().getName();
//		final String clazz = node.getData().getInfo().getClassName();
//		
//		final Node declaration = getDeclaration(varName, clazz, node);
//		if (declaration == null)
//			return "_";
//		return declaration.getData().getInfo().getInfo()[1].toString();
//	}
//	
//	private Node getDeclaration(String variableName, String clazz, Node definitionNode)
//	{
//		final Predicate<Node> collectAndStop = new Predicate<Node>() {
//			public boolean test(Node node)
//			{
//				if (!(node.getData() instanceof VariableInfo))
//					return false;
//				final VariableInfo variableInfo = (VariableInfo) node.getData();
//				if (!variableInfo.isDeclaration())
//					return false;
//				final String variableName0 =node.getName();
//				if (!variableName0.equals(variableName))
//					return false;
//				final String clazz0 = variableInfo.getInfo().getClassName();
//				if (clazz0.equals(clazz))
//					return true;
//				return false;
//			}
//		};
//		final ControlFlowTraverser.Configuration configuration = new ControlFlowTraverser.Configuration(ControlFlowTraverser.Direction.Backwards, true, false, true, false, true);
//		final Set<Node> declaration = ControlFlowTraverser.traverse(definitionNode, configuration, collectAndStop);
//		if (!declaration.isEmpty())
//			return declaration.iterator().next();
//
//		final List<Node> variables = this.getVariables(variableName, Context.Declaration, clazz, true);
//		return variables.isEmpty() ? null : variables.get(0);
//	}
}