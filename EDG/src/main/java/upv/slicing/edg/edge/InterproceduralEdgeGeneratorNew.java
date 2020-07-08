package upv.slicing.edg.edge;

import upv.slicing.edg.LASTBuilder.ClassInfo;
import upv.slicing.edg.LDASTNodeInfo;
import upv.slicing.edg.constraint.GlobalVariableConstraint;
import upv.slicing.edg.constraint.PhaseConstraint;
import upv.slicing.edg.constraint.SeekingConstraint;
import upv.slicing.edg.graph.*;
import upv.slicing.edg.slicing.Phase;

import java.util.*;

public class InterproceduralEdgeGeneratorNew extends EdgeGenerator {
	public InterproceduralEdgeGeneratorNew(EDG edg) { super(edg); }

	public void generateCallEdges()
	{
		final List<Node> calls = this.edg.getNodes(Node.Type.Call);

		for (Node call : calls) {
			final List<Node> possibleClauses = this.getPossibleClauses(call);
			final List<Node> matchingClauses = this.getMatchingClauses(possibleClauses, call);
			final Node callee = edg.getChild(call, Node.Type.Callee);
			final Node calleeResultNode = edg.getResFromNode(callee);

			for( Node matchingClause : matchingClauses)
				this.edg.addEdge(calleeResultNode, matchingClause, new Edge(Edge.Type.Call, new PhaseConstraint(Phase.Input)));
		}
	}

	private List<Node> getPossibleClauses(Node call)
	{
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node scopeNode = edg.getChild(callee, Node.Type.Scope);

		// Module of the caller
		// TODO: Can be a list of modules in a polymorphic scenario
		final List<Node> callerModules = this.getCallerModules(scopeNode);
		if (callerModules.isEmpty())
			return callerModules;

		final Node nameNode = edg.getChild(callee, Node.Type.Name);
		final List<Node> nameChildren = edg.getChildren(nameNode);
		nameChildren.removeIf(node -> node.getType() == Node.Type.Result);

		// Called routine
		final Node routineNameNode = nameChildren.get(0);;
		final String routineName = routineNameNode.getName();

		final List<Node> possibleClauses = new LinkedList<>();

		for (Node callerModule : callerModules) {
			final ClassInfo moduleInfo = (ClassInfo) callerModule.getInfo().getInfo()[2];
			possibleClauses.addAll(moduleInfo.getMethods().get(routineName));
		}

		return possibleClauses;

		// TODO: ERLANG behaviour
		 /*- M:R()	all routines of all modules				=> _:_
		 - m:R()	all routines of module m				=> m:_
		 - M:r()	routine r of all modules				=> _:r
		 - m:r()	routine f of module m					=> m:r
		 - r()	routine f of current module				=> m:r
		 - ar()	this anonymous routine					=> null:_
		 - X()	all routines (including anonymous ones)	=> null:null
		if (moduleRefType != Node.Type.Variable && moduleRefType != Node.Type.Literal && moduleRefType != Node.Type.Module)
			moduleName = "_";
		if (nameType != Node.Type.Literal)
			routineName = "_";
		if (nameType != Node.Type.Literal && scopeChildren.isEmpty())
			moduleName = null;
		if (moduleName == null && nameType != Node.Type.Routine)
			routineName = null;
		if (moduleName.equals("_"))
		{
			final Node callModule = EDGTraverserNew.getAncestor(call, Node.Info.Type.Module);
			final ClassInfo moduleInfo = (ClassInfo) callModule.getInfo().getInfo()[2];
			final List<Node> classClauses = this.getAllClauses(moduleInfo, routineName);
			System.out.println("STOP");
			return classClauses;
		}
		else*/
	}

	private List<Node> getCallerModules(Node scopeNode)
	{
		final List<Node> scopeChildren = edg.getChildren(scopeNode);
		scopeChildren.removeIf(node -> node.getType() == Node.Type.Result);

		// TODO: SURE? TEST IN JAVA
		if (scopeChildren.isEmpty())
			return List.of(edg.getAncestor(scopeNode, Node.Type.Module));

		assert(scopeChildren.size() == 1);
		final Node scopeExprNode = scopeChildren.get(0);
		final List<Node> modules = this.edg.getNodes(Node.Type.Module);

		switch (scopeExprNode.getType())
		{
			case TypeTransformation:
			case Variable:
				final Variable variable;
				if (scopeExprNode.getType() == Node.Type.TypeTransformation)
					variable = (Variable) edg.getChild(scopeExprNode, Node.Type.Variable);
				else
					variable = (Variable) scopeExprNode;
				final List<String> dynTypes = variable.getDynamicTypes();
				final List<Node> moduleNodes = new LinkedList<>();

				for (String dynType : dynTypes) {
					if (!dynType.equals("StaticType"))
						moduleNodes.add(this.getModuleByName(dynType, modules));
					else {
						final Node module = this.getModuleByName(variable.getStaticType(), modules);
						moduleNodes.add(module);
						final ClassInfo moduleInfo = (ClassInfo) module.getInfo().getInfo()[2];
						List<ClassInfo> classInfo = moduleInfo.getChildrenClasses();
						for (ClassInfo ci : classInfo)
							if (!moduleNodes.contains(ci.getClassNode()))
								moduleNodes.add(ci.getClassNode());
					}
				}

				return moduleNodes;
			case Type:
				return List.of(this.getModuleByName(scopeExprNode.getName(), modules));
			// TODO:
			// case FieldAccess:
			// case DataConstructorAccess:
			case Reference: // TODO: Static Calls
			case Literal:
				final String className = scopeExprNode.getName();
				if (className.equals("this"))
					return List.of(edg.getAncestor(scopeNode, Node.Type.Module));
				if (className.equals("super")) {
					final ClassInfo ci = (ClassInfo) edg.getAncestor(scopeNode, Node.Type.Module).getInfo().getInfo()[2];
					final List<Node> clauses = ci.getMethods().get("super<constructor>");

					if (clauses.isEmpty())
						throw new RuntimeException("There is no super constructor to be called.");

					return List.of(edg.getAncestor(clauses.get(0),Node.Type.Module));
				}
			default:
				throw new RuntimeException("The caller module cannot be found: " + scopeExprNode.getType());
		}
	}

	private Node getModuleByName(String moduleName, List<Node> modules)
	{
		for (Node module : modules)
			if (module.getName().equals(moduleName))
				return module;
		return null;
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
		final Node parameters = edg.getChild(possibleClause, Node.Type.Parameters);
		final List<Node> parameterNodes = edg.getChildren(parameters);
		parameterNodes.removeIf(node -> node.getType() == Node.Type.Result);

		final Node arguments = edg.getChild(call, Node.Type.Arguments);
		final List<Node> argumentNodes = edg.getChildren(arguments);
		argumentNodes.removeIf(node -> node.getType() == Node.Type.Result);

		// TODO: Tratar el caso de el ultimo argumento con "...". Ahora args.size() == parameters.size
		if (argumentNodes.size() != parameterNodes.size())
			return false;
		// if (argumentNodes.size() > 0 && parameterNodes.size() == 0)
		// 	return false;


		for (int argIndex = 0; argIndex < argumentNodes.size(); argIndex++)
		{
			final Node argument = argumentNodes.get(argIndex);
			final Node parameter = parameterNodes.get(argIndex);
			if (!isMatch(parameter, argument))
				return false;
		}
		return true;
	}

	// TODO: This function is language dependent. In Java is about types,
	//  in Erlang about matching patterns and expressions
	private boolean isMatch(Node param, Node arg)
	{
		Variable parameter = (Variable) param;
		String paramType = parameter.getStaticType();

		switch(arg.getType()){
			case Literal:	// Only for primitive types
				final LDASTNodeInfo ldNodeInfo = arg.getInfo();
				final String construction = ldNodeInfo.getConstruction();
				if (paramType.equals(construction))
					return true;
				return false;

			case Variable: // May be object or primitive type
				Variable argument = (Variable) arg;
				if (argument.getStaticType().equals(paramType))
					return true;

				List<String> childrenTypes = edg.getChildrenClasses(paramType);
				return childrenTypes.contains(argument.getStaticType());

			case Call:
				String returnType = (String) arg.getInfo().getInfo()[0];

				if (returnType.equals(paramType))
					return true;

				List<String> childrenTypes0 = edg.getChildrenClasses(paramType);
				return childrenTypes0.contains(returnType);

			default: // TODO: the rest of types are non contemplated
				throw new RuntimeException("This argument type is not considered");
		}
	}

	public void generateIO()
	{
		final List<Node> calls = this.edg.getNodes(Node.Type.Call);
		for (Node call : calls)
			this.generateInputOutput(call);
	}

	/* **************************************** */
	/* *********** Input-Output edges ********* */
	/* **************************************** */
	private void generateInputOutput(Node call)
	{
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node calleeResultNode = edg.getResFromNode(callee);
		final Set<Edge> callEdges = edg.getEdges(calleeResultNode, LAST.Direction.Forwards, Edge.Type.Call);

		final Set<Node> clauses = new HashSet<>();
		callEdges.stream().forEach(edge -> clauses.add(edg.getEdgeTarget(edge)));

		for (Node clause : clauses)
		{
			// EXPLICIT ARGUMENTS
			final Node argumentsNode = edg.getChild(call, Node.Type.Arguments);
			final Node parametersNode = edg.getChild(clause, Node.Type.Parameters);
			this.generateInputArcs(argumentsNode, parametersNode);

			// INPUT GLOBAL VARIABLES
			final Node argInNode = edg.getPolymorphicNode(call, clause.getInfo().getClassName(),Edge.Type.Input);
			final Node paramInNode = edg.getChild(clause, Node.Type.ParameterIn);
			this.generateInputArcs(argInNode, paramInNode);

			// OUTPUT GLOBAL VARIABLES
			final Node argOutNode = edg.getPolymorphicNode(call, clause.getInfo().getClassName(),Edge.Type.Output);
			final Node paramOutNode = edg.getChild(clause, Node.Type.ParameterOut);
			this.generateOutputArcs(argOutNode, paramOutNode);

			// CALL RESULT ARC
			this.generateCallResultArcs(call, clause);
		}
	}

	private void generateInputArcs(Node argumentsNode, Node parametersNode)
	{
		final List<Node> arguments = edg.getChildren(argumentsNode);
		arguments.removeIf(n -> n.getType() == Node.Type.Result);

		final List<Node> parameters = edg.getChildren(parametersNode);
		parameters.removeIf(n -> n.getType() == Node.Type.Result);

		for (int argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
		{
			final Node argument = arguments.get(argumentIndex);
			final Node parameter = parameters.get(argumentIndex);

			final Node argumentResult = edg.getResFromNode(argument);
			final Node parameterResult = edg.getResFromNode(parameter);

			this.edg.addEdge(argumentResult, parameterResult,
					new Edge(Edge.Type.Input, new PhaseConstraint(Phase.Input)));
		}
	}

	private void generateOutputArcs(Node argumentsNode, Node parametersNode)
	{
		final List<Node> arguments = edg.getChildren(argumentsNode);
		arguments.removeIf(n -> n.getType() == Node.Type.Result);

		final List<Node> parameters = edg.getChildren(parametersNode);
		parameters.removeIf(n -> n.getType() == Node.Type.Result);

		for (int argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
		{
			final Node argument = arguments.get(argumentIndex);
			final Node parameter = parameters.get(argumentIndex);

			final Node argumentResult = edg.getResFromNode(argument);
			final Node parameterResult = edg.getResFromNode(parameter);

			this.edg.addEdge(parameterResult, argumentResult,
					new Edge(Edge.Type.Output, new PhaseConstraint(Phase.Output)));
		}
	}

	private void generateCallResultArcs(Node call, Node clause)
	{
		final String routineName = edg.getParent(clause).getName();
		final Node clauseRes = edg.getResFromNode(clause);
		final Node callRes = edg.getResFromNode(call);
		if (!routineName.equals("<constructor>"))
			this.edg.addEdge(clauseRes, callRes, new Edge(Edge.Type.Output, new PhaseConstraint(Phase.Output)));

		final List<Node> clauseResChildren = edg.getChildren(clauseRes);
		if (!clauseResChildren.isEmpty())
		{
			final List<Node> callResChildren = edg.getChildren(callRes);

			clauseResChildren.removeIf(child -> child.getType() == Node.Type.Result);
			callResChildren.removeIf(child -> child.getType() == Node.Type.Result);
			for (int index = 0; index < clauseResChildren.size(); index++)
				this.edg.addEdge(edg.getResFromNode(clauseResChildren.get(index)),
						edg.getResFromNode(callResChildren.get(index)),
						new Edge(Edge.Type.Output, new PhaseConstraint(Phase.Output)));
		}
	}



	/** DAVID CODE **/

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

			this.edg.addEdge(calleeResultNode, matchingClause, new Edge(Edge.Type.Call, new PhaseConstraint(Phase.Input)));
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
