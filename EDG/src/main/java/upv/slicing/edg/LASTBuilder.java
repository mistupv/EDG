package upv.slicing.edg;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.graph.Variable;

import java.util.*;

// TODO Caracteristicas de los lenguajes: single assignment, orden metodos y atributos, bloque aisla contextos
// TODO Hay que crear las siguientes expresiones: compound pattern
public class LASTBuilder {

    public enum Where {
        ParameterIn, Parameters, ParameterOut, ArgumentIn, Arguments, ArgumentOut, Guard, Scope, Name, Body, Condition, Then, Else, Selector, Cases, Selectable, Restrictions, Value,
        Init, Update, Try, Catch, Finally, Throw, Reference, Iterator
    }

    // LAST
    public static LAST createLAST(LDASTNodeInfo info)
    {
        final LAST last = new LAST();
        final Node rootNode = new Node("LAST", last.getNextId(), Node.Type.Root, "LAST", info);

		last.setRootNode(rootNode);

		return last;
	}
	public static int addModule(LAST last, String name, LDASTNodeInfo info)
	{
		final Node parent = last.getRootNode();
		final Node module = LASTBuilder.addNode(last, parent, Node.Type.Module, name, "module" + "\\n" + name, info);

		return module.getId();
	}

	// Add expression
	public static int addRoutine(LAST last, int parentId, Where where, String name, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final String routineName = name == null ? "routine" : "routine" + "\\n" + name;
		final Node routine;
		final LDASTNodeInfo expInfo = new LDASTNodeInfo(info, true);

		routine = LASTBuilder.addNode(last, parent, Node.Type.Routine, name, routineName, expInfo);

		return routine.getId();
	}
	public static int addClause(LAST last, int functionId, LDASTNodeInfo info)
	{
		final Node parent = last.getNode(functionId);
		final Node.Type parentType = parent.getType();
		if (parentType != Node.Type.Routine)
			throw new RuntimeException("A " + parentType + " cannot contain a clause");

		final LDASTNodeInfo expInfo = new LDASTNodeInfo(info, true);
		final Node clause = LASTBuilder.addNode(last, parent, Node.Type.Clause, "clause", expInfo);
		LASTBuilder.addNode(last, clause, Node.Type.ParameterIn, "paramIn", info);
		LASTBuilder.addNode(last, clause, Node.Type.Parameters, "parameters", info); // ADDED info to parameters to obtain the name of the class
		LASTBuilder.addNode(last, clause, Node.Type.Guard, "guard", null);
		LASTBuilder.addNode(last, clause, Node.Type.Body, "body", null);
		LASTBuilder.addNode(last, clause, Node.Type.ParameterOut, "paramOut", info);

		return clause.getId();
	}

	public static int addVariable(LAST last, int parentId, Where where, String name, String varType, boolean declaration, boolean definition, boolean use, boolean global, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node variable = LASTBuilder.addVariableNode(last, parent, Node.Type.Variable, name, varType, "variable" + "\\n" + name, info);

		final Variable variableInfo = (Variable) variable;
		final Variable.Context context;
		
		if (definition && use)
			context = Variable.Context.Def_Use;
		else if (definition)
			context = Variable.Context.Definition;
		else if (use)
			context = Variable.Context.Use;
		else
			context = Variable.Context.Declaration;

		variableInfo.setDeclaration(declaration);
		variableInfo.setContext(context);
		variableInfo.setGlobal(global);

		return variableInfo.getId();
	}
	public static int addLiteral(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node literal = LASTBuilder.addNode(last, parent, Node.Type.Literal, value, "literal" + "\\n" + value, info);

		return literal.getId();
	}
	public static int addEquality(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node equality = LASTBuilder.addNode(last, parent, Node.Type.Equality, "equality", info);

		return equality.getId();
	}
	public static int addEquality(LAST last, int parentId, Where where, String sign, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node equality = LASTBuilder.addNode(last, parent, Node.Type.Equality, "equality" + "\\n"+ sign, info);

		return equality.getId();
	}
	public static int addOperation(LAST last, int parentId, Where where, String sign, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node operation = LASTBuilder.addNode(last, parent, Node.Type.Operation, sign, "operation" + "\\n" + sign, info);

		return operation.getId();
	}
	public static int addList(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node list = LASTBuilder.addNode(last, parent, Node.Type.List, "[]", "list" + "\\n" + "[]", info);

		return list.getId();
	}
	public static int addDataConstructor(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node dataConstructor = LASTBuilder.addNode(last, parent, Node.Type.DataConstructor, "{}", "data constructor" + "\\n" + "{}", info);

		return dataConstructor.getId();
	}
	public static int addDataConstructorAccess(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node dataConstructorAccess = LASTBuilder.addNode(last, parent, Node.Type.DataConstructorAccess, "data constructor access", "data constructor" + "\\n" + "access", info);

		return dataConstructorAccess.getId();
	}
	public static int addFieldAccess(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node fieldAccess = LASTBuilder.addNode(last, parent, Node.Type.FieldAccess, "field access", "field access", info);
		
		return fieldAccess.getId();
	}
	
	public static int addBlock(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node block = LASTBuilder.addNode(last, parent, Node.Type.Block, "block", info);

		return block.getId();
	}
	public static int addIf(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node _if = LASTBuilder.addNode(last, parent, Node.Type.If, "if", info);
		LASTBuilder.addNode(last, _if, Node.Type.Condition, "condition", null);
		LASTBuilder.addNode(last, _if, Node.Type.Body, "then", null);
		LASTBuilder.addNode(last, _if, Node.Type.Body, "else", null);

		return _if.getId();
	}
	public static int addSwitch(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node _switch = LASTBuilder.addNode(last, parent, Node.Type.Switch, "switch", info);
		LASTBuilder.addNode(last, _switch, Node.Type.Selector, "selector", null);
		LASTBuilder.addNode(last, _switch, Node.Type.Cases, "cases", null);

		return _switch.getId();
	}
	public static int addCase(LAST last, int parentId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, Where.Cases);
		final Node _case = LASTBuilder.addNode(last, parent, Node.Type.Case, "case", info);
		LASTBuilder.addNode(last, _case, Node.Type.Selectable, "selectable", null);
		LASTBuilder.addNode(last, _case, Node.Type.Guard, "guard", null);
		LASTBuilder.addNode(last, _case, Node.Type.Body, "body", null);

		return _case.getId();
	}
	public static int addDefaultCase(LAST last, int parentId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, Where.Cases);
		final Node defaultCase = LASTBuilder.addNode(last, parent, Node.Type.DefaultCase, "default", info);
		LASTBuilder.addNode(last, defaultCase, Node.Type.Body, "body", null);

		return defaultCase.getId();
	}
	public static int addBreak(LAST last, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node breakNode = LASTBuilder.addNode(last, parent, Node.Type.Break, "break " + dstId, info);

		return breakNode.getId();
	}
	public static int addContinue(LAST last, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node continueNode = LASTBuilder.addNode(last, parent, Node.Type.Continue, "continue " + dstId, info);

		return continueNode.getId();
	}
	public static int addCall(LAST last, int parentId, Where where, LDASTNodeInfo info) // TODO Añadir info al nodo result para encontrar declaraciones (necesita la clase)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node call = LASTBuilder.addNode(last, parent, Node.Type.Call, "call", info);
		final Node callee = LASTBuilder.addNode(last, call, Node.Type.Callee, "callee", info);
		LASTBuilder.addNode(last, callee, Node.Type.Scope, "scope", null);
		LASTBuilder.addNode(last, callee, Node.Type.Name, "name", null);
//		LASTBuilder.addNode(last, callee, Node.Type.Result, "result", null);
		LASTBuilder.addNode(last, call, Node.Type.ArgumentIn, "argsIn", null);
		LASTBuilder.addNode(last, call, Node.Type.Arguments, "arguments", null);
		LASTBuilder.addNode(last, call, Node.Type.ArgumentOut, "argsOut", null);
		return call.getId();
	}

	public static int addLabel(LAST last, int parentId, Where where, String labelText, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node label = LASTBuilder.addNode(last, parent, Node.Type.Label, "labeledExpr" + "\\n" + labelText, info);
		
		return label.getId();
	}

	public static int addExHandler(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node exHandler = LASTBuilder.addNode(last, parent, Node.Type.ExHandler, "exHandler", info);
		LASTBuilder.addNode(last, exHandler, Node.Type.Body, "try", null);
		LASTBuilder.addNode(last, exHandler, Node.Type.Body, "catch", null);
		LASTBuilder.addNode(last, exHandler, Node.Type.Body, "finally", null);

		return exHandler.getId();
	}
	public static int addCatchClause(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final LDASTNodeInfo expInfo = new LDASTNodeInfo(info, true);
		final Node catchClause = LASTBuilder.addNode(last, parent, Node.Type.CatchClause, "clause", expInfo);

		LASTBuilder.addNode(last, catchClause, Node.Type.Parameters, "parameters", info); // ADDED info to parameters to obtain the name of the class
		LASTBuilder.addNode(last, catchClause, Node.Type.Guard, "guard", null);
		LASTBuilder.addNode(last, catchClause, Node.Type.Body, "body", null);

		return catchClause.getId();
	}
	public static int addThrow(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node _throw = LASTBuilder.addNode(last, parent, Node.Type.Throw, "throw", info);
		
		return _throw.getId();
	}
	
	public static int addListComprehension(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node lc = LASTBuilder.addNode(last, parent, Node.Type.ListComprehension, "lc", info);
		LASTBuilder.addNode(last, lc, Node.Type.Restrictions, "restrictions", null);
		LASTBuilder.addNode(last, lc, Node.Type.Value, "value", null);

		return lc.getId();
	}
	public static int addGenerator(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node generator = LASTBuilder.addNode(last, parent, Node.Type.Generator, "generator", info);

		return generator.getId();
	}
	public static int addFilter(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node filter = LASTBuilder.addNode(last, parent, Node.Type.Filter, "filter", info);

		return filter.getId();
	}
	public static int addForLoop(LAST last, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node loop = LASTBuilder.addNode(last, parent, Node.Type.FLoop, "loop", info);
		
		LASTBuilder.addNode(last, loop, Node.Type.Init, "init", null);
		LASTBuilder.addNode(last, loop, Node.Type.Condition, "condition", null);
		LASTBuilder.addNode(last, loop, Node.Type.Body, "body", null);
		LASTBuilder.addNode(last, loop, Node.Type.Update, "update", null);
		
		return loop.getId();
	}
	public static int addCondLoop(LAST last, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node loop = LASTBuilder.addNode(last, parent, Node.Type.CLoop, "loop", info);
		
		LASTBuilder.addNode(last, loop, Node.Type.Condition, "condition", null);
		LASTBuilder.addNode(last, loop, Node.Type.Body, "body", null);
		
		return loop.getId();
	}
	public static int addRepeatLoop(LAST last, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node loop = LASTBuilder.addNode(last, parent, Node.Type.RLoop, "loop", info);
		
		// EN ORDEN DE APARICION EN EL CODIGO, NECESARIO A LA HORA DE MIRAR DEFINICIONES PREVIAS
		LASTBuilder.addNode(last, loop, Node.Type.Body, "body", null);
		LASTBuilder.addNode(last, loop, Node.Type.Condition, "condition", null);
		
		return loop.getId();
	}
	public static int addForeach(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node foreach = LASTBuilder.addNode(last, parent, Node.Type.Foreach, "foreach", info);

		// TODO: Delete iterator in foreach structure
		LASTBuilder.addNode(last, foreach, Node.Type.Iterator, "iterator", null);
		LASTBuilder.addNode(last, foreach, Node.Type.Body, "body", null);
		
		return foreach.getId();
	}

	public static int addEnclosed(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node enclosed = LASTBuilder.addNode(last, parent, Node.Type.Enclosed, "()", info);

		return enclosed.getId();
	}


	public static int addReturn(LAST last, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node _return = LASTBuilder.addNode(last, parent, Node.Type.Return, "return " + dstId, info);

		return _return.getId();
	}

	private static Node addNode(LAST last, Node parent, Node.Type type, String name, LDASTNodeInfo info)
	{
		return LASTBuilder.addNode(last, parent, type, name, name, info);
	}

	private static Node addVariableNode(LAST last, Node parent, Node.Type type, String name, String varType, String text, LDASTNodeInfo info)
	{
		copyFileAndClass(last, parent, info);
		final Node node = new Variable(last.getNextId(), type, name, varType, info);
		node.setLabel(text);
		addNode(last, node, parent, Edge.Type.Structural);
		return node;
	}

	private static Node addNode(LAST last, Node parent, Node.Type type, String name, String text, LDASTNodeInfo info)
	{
		copyFileAndClass(last, parent, info);
		final Node node = LASTBuilder.getNode(last, type, parent, name, info);
		node.setLabel(text);
		addNode(last, node, parent, Edge.Type.Structural);
		return node;
	}

	private static void copyFileAndClass(LAST last, Node parent, LDASTNodeInfo info) {
		if (info != null && info.getFile() == null)
			info.setFile(LASTBuilder.getArchive(last, parent));
		if (info != null && info.getClassName() == null)
			info.setClassName(LASTBuilder.getClassName(last, parent));
	}

	public static void addNode(LAST last, Node node, Node parent, Edge.Type type)
	{
		last.addVertex(node);
		last.addEdge(parent, node, type);
	}

	private static String getArchive(LAST last, Node node)
	{
		Node ancestor = node;

		while (ancestor != null)
		{
			final LDASTNodeInfo nodeInfo = ancestor.getInfo();
			if (nodeInfo != null)
			{
				final String file = nodeInfo.getFile();
				if (file != null)
					return file;
			}
			ancestor = last.getParent(ancestor);
		}

		return null;
	}

	private static String getClassName(LAST last, Node node)
	{
		Node ancestor = node;

		while (ancestor != null)
		{
			final LDASTNodeInfo nodeInfo = ancestor.getInfo();
			if (nodeInfo != null)
			{
				final String className = nodeInfo.getClassName();
				if (className != null)
					return className;
			}
			ancestor = last.getParent(ancestor);
		}

		return null;
	}

	// Typed languages
	public static int addTypeCheck(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node typeCheck = LASTBuilder.addNode(last, parent, Node.Type.TypeCheck, "typecheck", info);

		return typeCheck.getId();
	}
	public static int addTypeTransformation(LAST last, int parentId, Where where, LDASTNodeInfo info, boolean isEnclosedExpr)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
//		final Node expression = LASTBuilder.addNode(last, parent, Node.Info.Type.Expression, "expression", new LDASTNodeInfo(-1, "", isEnclosedExpr));
		info.addInfo(isEnclosedExpr);
		final Node typeTrans = LASTBuilder.addNode(last, parent, Node.Type.TypeTransformation, "typeTransformation", info);
		
		return typeTrans.getId();
	}
	public static int addTypeTransformation(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node typeTrans = LASTBuilder.addNode(last, parent, Node.Type.TypeTransformation, "typeTransformation", info);
		
		return typeTrans.getId();
	}
	public static int addSuperReference(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		
		// TODO asignar tipo a super
//		final Node classNode = LASTTraverser.getAncestor(parent, Node.Info.Type.Module);
//		final String extendedClassName = classNode.getInfo().getInfo()[0].toString();

		final Node reference = LASTBuilder.addNode(last, parent, Node.Type.Reference, value, "reference" + "\\n" + value, info);

		return reference.getId();

	}
	public static int addThisReference(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		
		// TODO asignar tipo a super
//		final Node classNode = LASTTraverser.getAncestor(parent, Node.Info.Type.Module);
//		final String extendedClassName = classNode.getInfo().getInfo()[0].toString();

		final Node reference = LASTBuilder.addNode(last, parent, Node.Type.Reference, value, "reference" + "\\n" + value, info);

		return reference.getId();

	}

	public static int addReference(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node reference = LASTBuilder.addNode(last, parent, Node.Type.Reference, value, "reference" + "\\n" + value, info);
		return reference.getId();
	}

	public static int addType(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final String label = info.getInfo().length == 0 ? "type" + "\\n" + value : "type" + "\\n" + value +"[]" ;
		final Node type = LASTBuilder.addNode(last, parent, Node.Type.Type, value, label, info);

		return type.getId();
	}
	
	// Complete LAST
	public static void completeLAST(LAST last)
	{
		final Node root = last.getRootNode();
		Node currentNode = root;
		int childIndexToVisit = 0;
		final LinkedList<Integer> bifurcations = new LinkedList<>();

		while (true)
		{
			final List<Node> children = last.getChildren(currentNode);

			if (childIndexToVisit == children.size())
			{ // It is a leaf or all its children have been already processed
				LASTBuilder.completeNode(last, currentNode);
				if (currentNode == root)
					break;
				// Go back to parent
				currentNode = last.getParent(currentNode);
				childIndexToVisit = bifurcations.removeLast();
			}
			else
			{ // A child has not been processed yet
				// Go to that child
				currentNode = children.get(childIndexToVisit);
				bifurcations.add(childIndexToVisit + 1);
				childIndexToVisit = 0;
			}
		}
	}
	private static void completeNode(LAST last, Node node)
	{
		final Node.Type type = node.getType();

		switch (type)
		{
			case Expression:
				LASTBuilder.completeExpression(last, node);
				break;
			default:
				break;
		}
	}
	private static void completeExpression(LAST last, Node node)
	{
		final Node child = last.getChild(node, 0);
		final Node.Type childType = child.getType();
	}

	// Add class inheritance information
	public static void addInheritanceInformation(LAST last) {
		final Node root = last.getRootNode();
		final List<Node> children = last.getChildren(root);
		
		final List<String> definedClasses = new LinkedList<>();
		for (Node child : children)
			definedClasses.add(child.getInfo().getClassName());

		final Map<String, Node> treatedClasses = new HashMap<>();
		int index = 0;
		while (!children.isEmpty())
		{	
			final Node child = children.get(index);
			final String className = child.getInfo().getClassName();
			final String extendedClassName = child.getInfo().getInfo()[0].toString();
			if (extendedClassName.equals("") || !definedClasses.contains(extendedClassName))
			{
				LASTBuilder.addNonExtendedClassInfo(last, child);
				treatedClasses.put(className,children.remove(index));
				index = 0;
			}
			else if(treatedClasses.containsKey(extendedClassName))
			{
				// TRATAR LA INFORMACION LINKEANDO SUS METODOS
				final Node parent = treatedClasses.get(extendedClassName);
				LASTBuilder.addExtendedClassInfo(last, child, parent);
				treatedClasses.put(className,children.remove(index));
				index = 0;
			}
			else
				// TRATAR LA SIGUIENTE
				index++;
		}
	}
	public static void addNonExtendedClassInfo(LAST last, Node node)
	{
		final List<Node> children = last.getChildren(node);
		children.removeIf(n -> n.getType() == Node.Type.Result);

		final Map<String, List<Node>> methods = new HashMap<>();
		final Map<String, Node> variables = new HashMap<>();
		
		for (Node child : children)
		{
			if (child.getType() == Node.Type.Variable)
				variables.put(child.getName(), child);
			else if (child.getType() == Node.Type.Equality) {
				Node dataMemberNode =last.getChild(child, Node.Type.Pattern);
				variables.put(dataMemberNode.getName(), dataMemberNode);
			}
			else
			{
				final String methodName = child.getName();
				final List<Node> methodClauses = last.getChildren(child);
				methodClauses.removeIf(n -> n.getType() != Node.Type.Clause);

				if (methods.containsKey(methodName))
					methodClauses.addAll(methods.get(methodName));
				methods.put(methodName, methodClauses);
			}
		}
		LDASTNodeInfo nodeInfo = node.getInfo();
		ClassInfo classInfo = new ClassInfo(node, methods, variables);
		nodeInfo.addInfo(classInfo);
	}
	public static void addExtendedClassInfo(LAST last, Node node, Node parent)
	{
		ClassInfo parentClassInfo = (ClassInfo) parent.getInfo().getInfo()[2];
		ClassInfo nodeClassInfo = new ClassInfo(node, parentClassInfo);
		
		final List<String> methodNames = new LinkedList<>();
		for (String name : nodeClassInfo.methods.keySet())
			methodNames.add(name);

		// Copy the parentMethods to maintain a reference for super calls
		for (String methodName : methodNames) {
			if (methodName.equals("<constructor>"))
				nodeClassInfo.methods.put("super<constructor>", nodeClassInfo.methods.get(methodName));
			else
				nodeClassInfo.methods.put("super." + methodName, nodeClassInfo.methods.get(methodName));
		}

		final List<Node> children = last.getChildren(node);
		children.removeIf(child -> child.getType() == Node.Type.Result);

		for (Node child : children)
		{
			if (child.getType() == Node.Type.Variable)
				nodeClassInfo.variables.put(child.getName(), child);
			else if (child.getType() == Node.Type.Equality) {
				Node dataMemberNode = last.getChild(child, Node.Type.Pattern);
				nodeClassInfo.variables.put(dataMemberNode.getName(), dataMemberNode);
			} else {
				final String methodName = child.getName();
				final List<Node> methodClauses = last.getChildren(child);
				methodClauses.removeIf(n -> n.getType() == Node.Type.Result);

				if (!nodeClassInfo.methods.containsKey(methodName))
					nodeClassInfo.methods.put(methodName, methodClauses);
				else
				{
					List<Node> parentClauses = nodeClassInfo.methods.get(methodName);
					boolean found;
					for (Node clause : methodClauses) {
						found = false;
						final List<Node> finalClauses = new LinkedList<>();
						for (Node parentClause : parentClauses) {
							if (!found && areMatchingClauses(last, clause, parentClause)) {
								finalClauses.add(clause);
								found = true;
							}
							else
								finalClauses.add(parentClause);
						}
						if (!found)
							finalClauses.add(clause);
						parentClauses = finalClauses;
					}
					nodeClassInfo.methods.put(methodName, parentClauses);
				}
			}
		}
		LDASTNodeInfo nodeInfo = node.getInfo();
		nodeInfo.addInfo(nodeClassInfo);
		
		parentClassInfo.addChildClass(nodeClassInfo);
	}

	public static boolean areMatchingClauses(LAST last, Node clause1, Node clause2){
    	final Node params1 = last.getChild(clause1, Node.Type.Parameters);
		final Node params2 = last.getChild(clause2, Node.Type.Parameters);

		final List<Node> paramList1 = last.getChildren(params1);
		final List<Node> paramList2 = last.getChildren(params2);

		if (paramList1.size() != paramList2.size())
			return false;

		paramList1.removeIf(param -> param.getType() == Node.Type.Result);
		paramList2.removeIf(param -> param.getType() == Node.Type.Result);

		for (int index = 0; index < paramList1.size(); index++)
		{
			Variable param1 = (Variable) paramList1.get(index);
			Variable param2 = (Variable) paramList2.get(index);
			if (param1.getStaticType() != param2.getStaticType())
				return false;
		}
		return true;
	}

	public static void completeFieldAccessTypes(LAST last)
	{
		List<Node> fieldAccesses = last.getNodes(Node.Type.FieldAccess);
		List<Node> modules = last.getNodes(Node.Type.Module);
		for (Node fieldAccess : fieldAccesses)
		{
			final Node index = last.getChild(fieldAccess, Node.Type.Index);
			if (index instanceof Variable)
			{
				final Variable indexVar = (Variable) index;
				final String classType = indexVar.getStaticType();
				final String varName = indexVar.getName();
				final String dataMemberName = varName.substring(varName.lastIndexOf(".") + 1);
				boolean dataMemberFound = false;
				for (Node module : modules)
				{
					if (module.getName().equals(classType))
					{
						ClassInfo ci = (ClassInfo) module.getInfo().getInfo()[2];
						String dataMemberStaticType = ((Variable) ci.getVariables().get(dataMemberName)).getStaticType();
						indexVar.setStaticType(dataMemberStaticType);
						dataMemberFound = true;
						break;
					}
				}
				if (!dataMemberFound)
					indexVar.setStaticType("unknown");
			}
		}
	}

	// Child node
	private static Node getClauseChildNode(LAST last, Node clause, Where where)
	{
		if (clause.getType() == Node.Type.Clause)
			switch (where)
			{
				case ParameterIn:
					return last.getChild(clause, 0);
				case Parameters:
					return last.getChild(clause, 1);
				case Guard:
					return last.getChild(clause, 2);
				case Body:
					return last.getChild(clause, 3);
				case ParameterOut:
					return last.getChild(clause, 4);
				default:
					throw new RuntimeException("A clause cannot contain " + where);
			}
		// If it's not a CLAUSE it is a CATCHCLAUSE, which have less children
		switch (where) // TODO: CHANGE STRUCTURE OF CATCHCLAUSE TO THE ONE IN CLAUSE IF NECESSARY
		{
			case Parameters:
				return last.getChild(clause, 0);
			case Guard:
				return last.getChild(clause, 1);
			case Body:
				return last.getChild(clause, 2);
			default:
				throw new RuntimeException("A clause cannot contain " + where);
		}
	}
	private static Node getIfChildNode(LAST last, Node _if, Where where)
	{
		switch (where)
		{
			case Condition:
				return last.getChild(_if, 0);
			case Then:
				return last.getChild(_if, 1);
			case Else:
				return last.getChild(_if, 2);
			default:
				throw new RuntimeException("An if cannot contain " + where);
		}
	}
	private static Node getSwitchChildNode(LAST last, Node _switch, Where where)
	{
		switch (where)
		{
			case Selector:
				return last.getChild(_switch, 0);
			case Cases:
				return last.getChild(_switch, 1);
			default:
				throw new RuntimeException("A switch cannot contain " + where);
		}
	}
	private static Node getCaseChildNode(LAST last, Node _case, Where where)
	{
		switch (where)
		{
			case Selectable:
				return last.getChild(_case, 0);
			case Guard:
				return last.getChild(_case, 1);
			case Body:
				return last.getChild(_case, 2);
			default:
				throw new RuntimeException("A case cannot contain " + where);
		}
	}
	private static Node getDefaultCaseChildNode(LAST last, Node defaultCase, Where where)
	{
		switch (where)
		{
			case Body:
				return last.getChild(defaultCase, 0);
			default:
				throw new RuntimeException("A default case cannot contain " + where);
		}
	}
	private static Node getCallChildNode(LAST last, Node call, Where where)
	{
		switch (where)
		{
			case Scope:
				final Node callee = last.getChild(call, 0);
				return last.getChild(callee, 0);
			case Name:
				final Node callee0 = last.getChild(call, 0);
				return last.getChild(callee0, 1);
			case ArgumentIn:
				return last.getChild(call, 1);
			case Arguments:
				return last.getChild(call, 2);
			case ArgumentOut:
				return last.getChild(call, 3);
			default:
				throw new RuntimeException("A call cannot contain " + where);
		}
	}
	private static Node getListComprehensionChildNode(LAST last, Node listComprehension, Where where)
	{
		switch (where)
		{
			case Restrictions:
				return last.getChild(listComprehension, 0);
			case Value:
				return last.getChild(listComprehension, 1);
			default:
				throw new RuntimeException("A list comprehension cannot contain " + where);
		}
	}

	private static Node getLoopChildNode(LAST last, Node loop, Where where)
	{
		return last.getChild(loop, getLoopChildNodeType(where));
	}

	private static Node.Type getLoopChildNodeType(Where where)
	{
		switch (where)
		{
			case Init:      return Node.Type.Init;
			case Condition: return Node.Type.Condition;
			case Body:      return Node.Type.Body;
			case Update:    return Node.Type.Update;
			default:
				throw new RuntimeException("A loop cannot contain " + where);
		}
	}

	private static Node getForeachChildNode(LAST last, Node foreach, Where where)
	{
		return last.getChild(foreach, getForeachChildNodeType(where));
	}

	private static Node.Type getForeachChildNodeType(Where where)
	{
		switch (where)
		{
			case Iterator: return Node.Type.Iterator;
			case Body:     return Node.Type.Body;
			default:
				throw new RuntimeException("A loop cannot contain " + where);
		}
	}

	private static Node getExhandlerChildNode(LAST last, Node exHandler, Where where)
	{
		return last.getChild(exHandler, getExhandlerChildNodeType(where));
	}

	private static Node.Type getExhandlerChildNodeType(Where where)
	{
		switch (where)
		{
			case Try:     return Node.Type.Try;
			case Catch:   return Node.Type.Catch;
			case Finally: return Node.Type.Finally;
			default:
				throw new RuntimeException("An exception handler cannot contain " + where);
		}
	}
	
	// Common
	public static List<Where> getWheres(Node.Type type)
	{
		final List<Where> wheres = new LinkedList<>();

		switch (type)
		{
			case Clause:
				return Arrays.asList(Where.Parameters, Where.Guard, Where.Body);
			case If:
				return Arrays.asList(Where.Condition, Where.Then, Where.Else);
			case Switch:
				return Arrays.asList(Where.Selector, Where.Cases);
			case Case:
				return Arrays.asList(Where.Selectable, Where.Guard, Where.Body);
			case DefaultCase:
				return Collections.singletonList(Where.Body);
			case Call:
				return Arrays.asList(Where.Scope, Where.Name, Where.Arguments);
			case ListComprehension:
				return Arrays.asList(Where.Restrictions, Where.Value);
			case FLoop:
				return Arrays.asList(Where.Init, Where.Condition, Where.Body, Where.Update);
			case CLoop:
			case RLoop:
				return Arrays.asList(Where.Condition, Where.Body);
			case Module:
			case Routine:
			case Variable:
			case Literal:
			case Equality:
			case Operation:
			case DataConstructor:
			case List:
			case Block:
				return Collections.emptyList();
			default:
				throw new RuntimeException("Type not contemplated: " + type);
		}
	}
	private static Node getParentNode(LAST last, int parentId, Where where)
	{
		final Node parentNode = last.getNode(parentId);
		final Node.Type type = parentNode.getType();

		switch (type)
		{
			case Clause:
			case CatchClause:
				return LASTBuilder.getClauseChildNode(last, parentNode, where);
			case Call:
				return LASTBuilder.getCallChildNode(last, parentNode, where);
			case If:
				return LASTBuilder.getIfChildNode(last, parentNode, where);
			case Switch:
				return LASTBuilder.getSwitchChildNode(last, parentNode, where);
			case Case:
				return LASTBuilder.getCaseChildNode(last, parentNode, where);
			case DefaultCase:
				return LASTBuilder.getDefaultCaseChildNode(last, parentNode, where);
			case ListComprehension:
				return LASTBuilder.getListComprehensionChildNode(last, parentNode, where);
			case FLoop:
			case CLoop:
			case RLoop:
				return LASTBuilder.getLoopChildNode(last, parentNode, where);
			case Foreach:
				return LASTBuilder.getForeachChildNode(last, parentNode, where);
//ADDED
//case ForLoop:
//	return ASTBuilder.getLoopChildNode(parentNode, where);
//
			case ExHandler:
				return LASTBuilder.getExhandlerChildNode(last, parentNode, where);
				
			case TypeCheck: // ADDED
			case TypeTransformation: // ADDED
			case Throw:
			case Module:
			case Routine:
			case Block:
			case DataConstructorAccess:
			case List:
			case DataConstructor:
			case Equality:
			case Operation:
			case Generator:
			case Filter:
			case Return:
			case Enclosed:
			case Label:
			case FieldAccess:
			case ParameterIn:
			case ParameterOut:
			case ArgumentIn:
			case ArgumentOut:
			case PolymorphicCall:
			case Variable:
			case Result:
				if (where == null)
					return parentNode;
			default:
				throw new RuntimeException(type + " does not contain " + where);
		}
	}
//	public static Node getNode(Node.Type type, boolean isVariable, String name, LDASTNodeInfo info)
//	{
//		if (isVariable)
//			return new Variable(last.getNextId(), type, name, info);
//		return new Node(last.getNextId(), type, name, info);
//	}
	static Node getNode(LAST last, Node.Type type, Node parent, String name, LDASTNodeInfo info)
	{
		switch(type)
		{
			case Variable:
				return new Variable(last.getNextId(), type, name, info);
			case Result:
				final Node firstSibling = last.getChild(parent, 0);
				final Node.Type siblingType = firstSibling.getType();
				switch(siblingType)
                {
                    case Variable:
						return new Variable(last.getNextId(), type, name, info);
                    case Literal:
                    case DataConstructorAccess:
                    case DataConstructor:
                    case Operation:
                    default:
                        return new Node(last.getNextId(), type, name, info);
                }
			case DefaultCase:

                final List<Node> children = last.getChildren(parent);
                if (children.size() == 1)
                    return new Node(last.getNextId(), type, name, info);

                final Node lastChild = children.get(children.size() - 1);
                if (lastChild.getType() != Node.Type.DefaultCase)
                    return new Node(last.getNextId(), type, name, info);

                return new Node(last.getNextId(), type, name, info);

			case Routine:
			case Clause:
			case Module:
			case Return:
			case Continue:
			case Break:
			case Case:
			case CatchClause:
			case Callee:
			case ArgumentIn:
			case ArgumentOut:
            default:
                return new Node(last.getNextId(), type, name, info);
        }
	}

	public static class ClassInfo
	{
		private Node classNode;
		private Map<String, List<Node>> methods;
		private Map<String, Node> variables;
		private List<ClassInfo> childrenClasses = new LinkedList<>();
		
		public ClassInfo(Node clazz, ClassInfo ci)
		{
			classNode = clazz;
			this.methods = new HashMap<>(ci.methods);
			this.variables = new HashMap<>(ci.variables);
		}
		
		public ClassInfo(Node clazz, Map<String, List<Node>> methodMap, Map<String, Node> variableMap)
		{
			classNode = clazz;
			methods = methodMap;
			variables = variableMap;
		}
		
		public void addChildClass(ClassInfo child)
		{
			childrenClasses.add(child);
		}
		public List<ClassInfo> getChildrenClasses()
		{
			return this.childrenClasses;
		}
		public Map<String,List<Node>> getMethods()
		{
			return this.methods;
		}
		public Map<String,Node> getVariables()
		{
			return this.variables;
		}
		public Node getClassNode() { return this.classNode; }
	}
}
