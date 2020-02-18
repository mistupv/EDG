package edg.edge;

import java.util.LinkedList;
import java.util.List;

import edg.LASTBuilder.ClassInfo;
import edg.constraint.GlobalVariableConstraint;
import edg.constraint.PhaseConstraint;
import edg.constraint.SeekingConstraint;
import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.slicing.Phase;
import edg.traverser.EDGTraverserNew;

public class InterproceduralEdgeGeneratorNew extends EdgeGenerator
{
	public InterproceduralEdgeGeneratorNew(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		final List<Node> calls = EDGTraverserNew.getNodes(this.edg, NodeInfo.Type.Call);

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
		final Node callee = EDGTraverserNew.getChild(call, NodeInfo.Type.Callee);
		final Node calleeResultNode = EDGTraverserNew.getChild(callee, NodeInfo.Type.Result);
		final Node arguments = EDGTraverserNew.getChild(call, NodeInfo.Type.Arguments);
		final Node argumentsIn = EDGTraverserNew.getChild(call, NodeInfo.Type.ArgumentIn);
		final List<Node> argumentNodes = EDGTraverserNew.getChildren(arguments);

		for (Node matchingClause : matchingClauses)
		{
			final Node parameters = EDGTraverserNew.getChild(matchingClause, NodeInfo.Type.Parameters);
			final List<Node> parameterNodes = EDGTraverserNew.getChildren(parameters);

			this.edg.addEdge(calleeResultNode, matchingClause, 0, new EdgeInfo(EdgeInfo.Type.Input, new PhaseConstraint(Phase.Input)));
			for (int argumentIndex = 0; argumentIndex < argumentNodes.size(); argumentIndex++)
			{
				final Node argument = argumentNodes.get(argumentIndex);
				final Node parameter = parameterNodes.get(argumentIndex);
				final Node argumentResult = EDGTraverserNew.getResult(argument);
				final Node parameterResult = EDGTraverserNew.getResult(parameter);

				if (argumentResult != null && parameterResult != null)
					this.edg.addEdge(argumentResult, parameterResult, 0, new EdgeInfo(EdgeInfo.Type.Input, new PhaseConstraint(Phase.Input)));
			}
			final Node parameterIn = EDGTraverserNew.getChild(matchingClause, NodeInfo.Type.ParameterIn);
			//this.edg.addEdge(arguments, parameters, 0, new EdgeInfo(EdgeInfo.Type.Call, new PhaseConstraint(Phase.Input)));
			this.edg.addEdge(argumentsIn, parameterIn, 0, new EdgeInfo(EdgeInfo.Type.Call, new PhaseConstraint(Phase.Input)));
		}
	}
	private List<Node> getPossibleClauses(Node call)
	{
		final Node callee = EDGTraverserNew.getChild(call, NodeInfo.Type.Callee);
		final Node scopeNode = EDGTraverserNew.getChild(callee, NodeInfo.Type.Scope);
		final Node nameNode = EDGTraverserNew.getChild(callee, NodeInfo.Type.Name);
		final List<Node> scopeChildren = EDGTraverserNew.getChildren(scopeNode);
		final Node routineArguments = EDGTraverserNew.getChild(call, NodeInfo.Type.Arguments);
		final List<Node> arguments = EDGTraverserNew.getChildren(routineArguments);

		// Module
		final Node moduleRef0 = scopeChildren.isEmpty() ? EDGTraverserNew.getAncestor(call, NodeInfo.Type.Module) : scopeChildren.get(0);
		final Node moduleRef1 = moduleRef0.getData().getType() != NodeInfo.Type.Expression ? moduleRef0 : EDGTraverserNew.getChild(moduleRef0, NodeInfo.Type.Value);
		final Node moduleRef = moduleRef1.getData().getType() != NodeInfo.Type.TypeTransformation ? moduleRef1 : EDGTraverserNew.getChild(EDGTraverserNew.getChild(moduleRef1, NodeInfo.Type.Variable),NodeInfo.Type.Value);
		final NodeInfo.Type moduleRefType = moduleRef.getData().getType();
		
		//String moduleName = moduleRefType == NodeInfo.Type.Literal ? moduleRef.getData().getName() : null;//moduleRef.getData().getInfo().getClassName();
		final String moduleName0 = moduleRefType == NodeInfo.Type.Variable ? moduleRef.getData().getInfo().getInfo()[1].toString() : null;
		final String moduleName1 = moduleRefType == NodeInfo.Type.Literal ? moduleRef.getData().getName() : moduleName0;
		String moduleName = scopeChildren.size() == 0 ? moduleRef.getData().getName() : moduleName1;
		
		// Function
		final Node name0 = EDGTraverserNew.getChild(nameNode, 0);
		final Node name = name0.getData().getType() != NodeInfo.Type.Expression ? name0 : EDGTraverserNew.getChild(name0, NodeInfo.Type.Value);
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
		
//		if (moduleName.equals("_"))
//		{
//			final Node callModule = EDGTraverserNew.getAncestor(call, NodeInfo.Type.Module);
//			final ClassInfo moduleInfo = (ClassInfo) callModule.getData().getInfo().getInfo()[2];
//			final List<Node> classClauses = this.getAllClauses(moduleInfo, routineName);
//			System.out.println("STOP");
//			return classClauses;
//		}
//		else 
		if (moduleName != null)
		{
			final String moduleScopeName = moduleName;
			if (moduleScopeName.equals("super"))
			{
				final Node module = EDGTraverserNew.getAncestor(call, NodeInfo.Type.Module);
				moduleName = module.getData().getName();
			}
			final List<Node> modules = EDGTraverserNew.getNodes(this.edg, NodeInfo.Type.Module);
			for (Node module : modules)
			{
				final String moduleText = module.getData().getName();
				if (moduleName.equals(moduleText))
				{
					final ClassInfo moduleInfo = (ClassInfo) module.getData().getInfo().getInfo()[2];
					final List<Node> classClauses;
					if (!scopeChildren.isEmpty())
					{
						if (moduleScopeName.equals("super") && routineName.equals("<constructor>"))
							routineName = moduleScopeName+routineName;
						classClauses = this.getAllClauses(moduleInfo, routineName);
					}
					else
					{	
						final Node parentRoutineNode = EDGTraverserNew.getAncestor(call, NodeInfo.Type.Routine);
						if (parentRoutineNode != null)
						{
							final String parentRoutineName = EDGTraverserNew.getAncestor(call, NodeInfo.Type.Routine).getData().getName();
							classClauses = this.getClassClauses(moduleInfo, routineName, parentRoutineName);
						}
						else 
							classClauses = this.getAllClauses(moduleInfo, routineName);
					}
					return classClauses;
				}
			}
		}
		
		final List<Node> possibleClauses = new LinkedList<Node>();
		final List<Node> clauses = EDGTraverserNew.getNodes(this.edg, NodeInfo.Type.Clause); // ONLY WHEN THERE IS ONLY A DEFINITION OF A FUNCTION, NOT FOR POLIMORPHIC CALLS
		final boolean thisAnonymousRoutine = moduleName == null && routineName != null;
		final boolean allRoutines = moduleName == null && routineName == null;

		for (Node clause : clauses)
		{
			final Node routine = EDGTraverserNew.getParent(clause);
			if (thisAnonymousRoutine && routine != name)
				continue;
			
			if (!thisAnonymousRoutine && !allRoutines)
			{
				final Node module = EDGTraverserNew.getParent(routine);
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

			final Node parameters = EDGTraverserNew.getChild(clause, 0);
			final List<Node> parameterNodes = EDGTraverserNew.getChildren(parameters);
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
		final Node parameters = EDGTraverserNew.getChild(possibleClause, 0);
		final List<Node> parameterNodes = EDGTraverserNew.getChildren(parameters);
		final Node arguments = EDGTraverserNew.getChild(call, 1);
		final List<Node> argumentNodes = EDGTraverserNew.getChildren(arguments);
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
		final Node callResult = EDGTraverserNew.getResult(call);
		final Node callee = EDGTraverserNew.getChild(call, NodeInfo.Type.Callee);
		final Node calleeResult = EDGTraverserNew.getChild(callee, NodeInfo.Type.Result);
		final List<Node> callingFunctions = EDGTraverserNew.getInputs(calleeResult, EDGTraverserNew.Direction.Forwards);

		for (Node callingFunction : callingFunctions)
		{
			final Node result = EDGTraverserNew.getResult(callingFunction);

			if (result != null)
			{
				final String routineName = EDGTraverserNew.getAncestor(callingFunction,NodeInfo.Type.Routine).getData().getName();
				if (routineName.equals("<constructor>"))
				{
					final GlobalVariableConstraint addConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Add, "*");
					this.edg.addEdge(EDGTraverserNew.getChild(callingFunction, NodeInfo.Type.ParameterOut), callResult, 0, new EdgeInfo(EdgeInfo.Type.Output, addConstraint));
				}
				this.edg.addEdge(result, callResult, 0, new EdgeInfo(EdgeInfo.Type.Output, new PhaseConstraint(Phase.Output)));
			}
		}
	}
	
	private List<Node> getAllClauses(ClassInfo moduleInfo, String routineName)
	{
		final List<Node> methodClauses = new LinkedList<Node>();
		final List<Node> classClauses = moduleInfo.getMethods().get(routineName);
		if (classClauses != null)
			methodClauses.addAll(classClauses);
		
		final List<ClassInfo> childrenClassInfo = moduleInfo.getChildrenClasses();
		if (childrenClassInfo.isEmpty())
			return methodClauses;
		else
		{
			final List<Node> childrenMethodClauses = new LinkedList<Node>();
			for (ClassInfo childClassInfo : childrenClassInfo)
				childrenMethodClauses.addAll(this.getAllClauses(childClassInfo, routineName));
			
			for (Node childrenMethodClause : childrenMethodClauses)
				if (!methodClauses.contains(childrenMethodClause))
					methodClauses.add(childrenMethodClause);
		}
		return methodClauses;
	}
	private List<Node> getClassClauses(ClassInfo moduleInfo, String routineName, String parentRoutineName)
	{
		final List<Node> methodClauses = new LinkedList<Node>();
		final List<Node> classClauses = moduleInfo.getMethods().get(routineName);
		
		if (classClauses != null)
			methodClauses.addAll(classClauses);
		
		final List<ClassInfo> childrenClassInfo = moduleInfo.getChildrenClasses();
		if (childrenClassInfo.isEmpty())
			return methodClauses;
		else
		{	
			List<Node> parentMethodClauses = moduleInfo.getMethods().get(parentRoutineName); 
			
			final List<Node> childrenMethodClauses = new LinkedList<Node>();
			for (ClassInfo childClassInfo : childrenClassInfo)
			{
				List<Node> childParentMethodClauses = childClassInfo.getMethods().get(parentRoutineName);
				if (childParentMethodClauses == parentMethodClauses)
					childrenMethodClauses.addAll(this.getClassClauses(childClassInfo, routineName, parentRoutineName));
			}
			
			for (Node childrenMethodClause : childrenMethodClauses)
				if (!methodClauses.contains(childrenMethodClause))
					methodClauses.add(childrenMethodClause);
		}
		return methodClauses;
	}

	public void generateNoInheritance() {
		// TODO IMPLEMENT IGNORING MODULES
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