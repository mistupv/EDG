package upv.slicing.edg.edge;

import upv.slicing.edg.LASTBuilder.ClassInfo;
import upv.slicing.edg.constraint.GlobalVariableConstraint;
import upv.slicing.edg.constraint.PhaseConstraint;
import upv.slicing.edg.constraint.SeekingConstraint;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.slicing.Phase;

import java.util.LinkedList;
import java.util.List;

public class InterproceduralEdgeGenerator extends EdgeGenerator {
	public InterproceduralEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		final List<Node> calls = this.edg.getNodes(Node.Type.Call);

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
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node calleeResultNode = edg.getResFromNode(callee);
		final Node argumentsNode = edg.getChild(call, Node.Type.Arguments);
		final Node argumentsIn = edg.getChild(call, Node.Type.ArgumentIn);
		final List<Node> arguments = edg.getChildren(argumentsNode);
		arguments.removeIf(n -> n.getType() == Node.Type.Result);

		for (Node matchingClause : matchingClauses)
		{
			final Node parametersNode = edg.getChild(matchingClause, Node.Type.Parameters);
			final List<Node> parameters = edg.getChildren(parametersNode);
			parameters.removeIf(n -> n.getType() == Node.Type.Result);

			this.edg.addEdge(calleeResultNode, matchingClause, new Edge(Edge.Type.Input, new PhaseConstraint(Phase.Input)));
			for (int argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
			{
				final Node argument = arguments.get(argumentIndex);
				final Node parameter = parameters.get(argumentIndex);

				final Node argumentResult = edg.getResFromNode(argument);
				final Node parameterResult = edg.getResFromNode(parameter);

				this.edg.addEdge(argumentResult, parameterResult, new Edge(Edge.Type.Input, new PhaseConstraint(Phase.Input)));
			}
			final Node parameterIn = edg.getChild(matchingClause, Node.Type.ParameterIn);
			this.edg.addEdge(argumentsIn, parameterIn, new Edge(Edge.Type.Call, new PhaseConstraint(Phase.Input)));
		}
	}
	private List<Node> getPossibleClauses(Node call)
	{
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node scopeNode = edg.getChild(callee, Node.Type.Scope);
		final Node nameNode = edg.getChild(callee, Node.Type.Name);
		final List<Node> scopeChildren = edg.getChildren(scopeNode);
		final Node routineArguments = edg.getChild(call, Node.Type.Arguments);
		final List<Node> arguments = edg.getChildren(routineArguments);

		// Module
		final Node moduleRef0 = scopeChildren.isEmpty() ? edg.getAncestor(call, Node.Type.Module) : scopeChildren.get(0);
		final Node moduleRef1 = moduleRef0.getType() != Node.Type.Expression ? moduleRef0 : edg.getChild(moduleRef0, Node.Type.Value);
		final Node moduleRef =
				moduleRef1.getType() != Node.Type.TypeTransformation ? moduleRef1 : edg.getChild(edg.getChild(moduleRef1, Node.Type.Variable), Node.Type.Value);
		final Node.Type moduleRefType = moduleRef.getType();

		//String moduleName = moduleRefType == Node.Info.Type.Literal ? moduleRef.getName() : null;//moduleRef.getInfo().getClassName();
		final String moduleName0 =
				moduleRefType == Node.Type.Variable ? moduleRef.getInfo().getInfo()[1].toString() : null;
		final String moduleName1 = moduleRefType == Node.Type.Literal ? moduleRef.getName() : moduleName0;
		String moduleName = scopeChildren.size() == 0 ? moduleRef.getName() : moduleName1;

		// Function
		final Node name0 = edg.getChild(nameNode, 0);
		final Node name = name0.getType() != Node.Type.Expression ? name0 : edg.getChild(name0, Node.Type.Value);
		final Node.Type nameType = name.getType();
		String routineName = name.getName();

		// - M:R()	all routines of all modules				=> _:_
		// - m:R()	all routines of module m				=> m:_
		// - M:r()	routine r of all modules				=> _:r
		// - m:r()	routine f of module m					=> m:r
		// - r()	routine f of current module				=> m:r
		// - ar()	this anonymous routine					=> null:_
		// - X()	all routines (including anonymous ones)	=> null:null
		if (moduleRefType != Node.Type.Variable && moduleRefType != Node.Type.Literal && moduleRefType != Node.Type.Module)
			moduleName = "_";
		if (nameType != Node.Type.Literal)
			routineName = "_";
		if (nameType != Node.Type.Literal && scopeChildren.isEmpty())
			moduleName = null;
		if (moduleName == null && nameType != Node.Type.Routine)
			routineName = null;
		
//		if (moduleName.equals("_"))
//		{
//			final Node callModule = EDGTraverserNew.getAncestor(call, Node.Info.Type.Module);
//			final ClassInfo moduleInfo = (ClassInfo) callModule.getInfo().getInfo()[2];
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
				final Node module = edg.getAncestor(call, Node.Type.Module);
				moduleName = module.getName();
			}
			final List<Node> modules = this.edg.getNodes(Node.Type.Module);
			for (Node module : modules)
			{
				final String moduleText = module.getName();
				if (moduleName.equals(moduleText))
				{
					final ClassInfo moduleInfo = (ClassInfo) module.getInfo().getInfo()[2];
					final List<Node> classClauses;
					if (!scopeChildren.isEmpty())
					{
						if (moduleScopeName.equals("super") && routineName.equals("<constructor>"))
							routineName = moduleScopeName+routineName;
						classClauses = this.getAllClauses(moduleInfo, routineName);
					}
					else
					{	
						final Node parentRoutineNode = edg.getAncestor(call, Node.Type.Routine);
						if (parentRoutineNode != null)
						{
							final String parentRoutineName = edg.getAncestor(call, Node.Type.Routine).getName();
							classClauses = this.getClassClauses(moduleInfo, routineName, parentRoutineName);
						}
						else 
							classClauses = this.getAllClauses(moduleInfo, routineName);
					}
					return classClauses;
				}
			}
		}
		
		final List<Node> possibleClauses = new LinkedList<>();
		final List<Node> clauses = this.edg.getNodes(Node.Type.Clause); // ONLY WHEN THERE IS ONLY A DEFINITION OF A FUNCTION, NOT FOR POLIMORPHIC CALLS
		final boolean thisAnonymousRoutine = moduleName == null && routineName != null;
		final boolean allRoutines = moduleName == null && routineName == null;

		for (Node clause : clauses)
		{
			final Node routine = edg.getParent(clause);
			if (thisAnonymousRoutine && routine != name)
				continue;
			
			if (!thisAnonymousRoutine && !allRoutines)
			{
				final Node module = edg.getParent(routine);
				final Node.Type moduleType = module.getType();
				if (moduleType != Node.Type.Module)
					continue;
				final String moduleText = module.getName();
				if (!moduleName.equals(moduleText) && !moduleName.equals("_"))
					continue;
				final String routineText = routine.getName();
				if (!routineName.equals(routineText) && !routineName.equals("_"))
					continue;
			}

			final Node parameters = edg.getChild(clause, 0);
			final List<Node> parameterNodes = edg.getChildren(parameters);
			if (arguments.size() != parameterNodes.size())
				continue;

			possibleClauses.add(clause);
		}

		return possibleClauses;
	}
	private List<Node> getMatchingClauses(List<Node> possibleClauses, Node call)
	{
		final List<Node> matchingClauses = new LinkedList<>();

		for (Node possibleClause : possibleClauses)
			if (this.matchClause(possibleClause, call))
				matchingClauses.add(possibleClause);

		return matchingClauses;
	}
	private boolean matchClause(Node possibleClause, Node call)
	{
		final Node parameters = edg.getChild(possibleClause, 0);
		final List<Node> parameterNodes = edg.getChildren(parameters);
		final Node arguments = edg.getChild(call, 1);
		final List<Node> argumentNodes = edg.getChildren(arguments);
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
		final Node callResult = edg.getResFromNode(call);
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node calleeResult = edg.getResFromNode(callee);
		final List<Node> callingFunctions = edg.getInputs(calleeResult, LAST.Direction.Forwards);

		for (Node callingFunction : callingFunctions)
		{
			final Node result = edg.getResFromNode(callingFunction);

			if (result != null)
			{
				final String routineName = edg.getAncestor(callingFunction, Node.Type.Routine).getName();
				if (routineName.equals("<constructor>"))
				{
					final GlobalVariableConstraint addConstraint = new GlobalVariableConstraint(
							SeekingConstraint.Operation.Add, "*");
					this.edg.addEdge(edg.getChild(callingFunction, Node.Type.ParameterOut), callResult,
							new Edge(Edge.Type.Output, addConstraint));
				}
				this.edg.addEdge(result, callResult, new Edge(Edge.Type.Output, new PhaseConstraint(Phase.Output)));
			}
		}
	}
	
	private List<Node> getAllClauses(ClassInfo moduleInfo, String routineName)
	{
		final List<Node> methodClauses = new LinkedList<>();
		final List<Node> classClauses = moduleInfo.getMethods().get(routineName);
		if (classClauses != null)
			methodClauses.addAll(classClauses);
		
		final List<ClassInfo> childrenClassInfo = moduleInfo.getChildrenClasses();
		if (childrenClassInfo.isEmpty())
			return methodClauses;
		else
		{
			final List<Node> childrenMethodClauses = new LinkedList<>();
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
		final List<Node> methodClauses = new LinkedList<>();
		final List<Node> classClauses = moduleInfo.getMethods().get(routineName);
		
		if (classClauses != null)
			methodClauses.addAll(classClauses);
		
		final List<ClassInfo> childrenClassInfo = moduleInfo.getChildrenClasses();
		if (childrenClassInfo.isEmpty())
			return methodClauses;
		else
		{	
			List<Node> parentMethodClauses = moduleInfo.getMethods().get(parentRoutineName); 
			
			final List<Node> childrenMethodClauses = new LinkedList<>();
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

	public void generateNoInheritance()
	{
		// TODO IMPLEMENT IGNORING MODULES
	}


	/*****************************************/
	/** Sacar tipos buscando la declaracion **/
	/*****************************************/

//	private String getVarTypeName(Node node)
//	{
//		final String varName = node.getName();
//		final String clazz = node.getInfo().getClassName();
//		
//		final Node declaration = getDeclaration(varName, clazz, node);
//		if (declaration == null)
//			return "_";
//		return declaration.getInfo().getInfo()[1].toString();
//	}
//	
//	private Node getDeclaration(String variableName, String clazz, Node definitionNode)
//	{
//		final Predicate<Node> collectAndStop = new Predicate<Node>() {
//			public boolean test(Node node)
//			{
//				if (!(node.getInfo() instanceof VariableInfo))
//					return false;
//				final VariableInfo variableInfo = (VariableInfo) node.getInfo();
//				if (!variableInfo.isDeclaration())
//					return false;
//				final String variableName0 =node.getName();
//				if (!variableName0.equals(variableName))
//					return false;
//				final String clazz0 = variableInfo.getClassName();
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
