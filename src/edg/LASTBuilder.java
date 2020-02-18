package edg;

import edg.graph.*;
import edg.traverser.LASTTraverser;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

// TODO Caracteristicas de los lenguajes: single assignment, orden metodos y atributos, bloque aisla contextos
// TODO Hay que crear las siguientes expresiones: compound pattern
public class LASTBuilder {
    public static int nextId = 0;

    public enum Where {
        ParameterIn, Parameters, ParameterOut, ArgumentIn, Arguments, ArgumentOut, Guard, Scope, Name, Body, Condition, Then, Else, Selector, Cases, Selectable, Restrictions, Value,
        Init, Update, Try, Catch, Finally, Throw, Reference, Iterator
    }

    // LAST
    public static LAST createLAST(LDASTNodeInfo info)
    {
        LASTBuilder.nextId = 0;

        final LAST last = new LAST();
        final NodeInfo rootNodeInfo = new NodeInfo(LASTBuilder.nextId++, NodeInfo.Type.Root, "LAST", info);
		final Node rootNode = new Node("LAST", rootNodeInfo);

		last.setRootNode(rootNode);

		return last;
	}
	public static int addModule(LAST last, String name, LDASTNodeInfo info)
	{
		final Node parent = last.getRootNode();
		final Node module = LASTBuilder.addNode(last, parent, NodeInfo.Type.Module, name, "module" + "\\n" + name, info);

		return module.getData().getId();
	}

	// Add expression
	public static int addRoutine(LAST last, int parentId, Where where, String name, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final String routineName = name == null ? "routine" : "routine" + "\\n" + name;
		final Node routine;

		if (parent.getData().getType() == NodeInfo.Type.Module)
			routine = LASTBuilder.addNode(last, parent, NodeInfo.Type.Routine, name, routineName, info);
		else
		{
			final Node expression = LASTBuilder.addNode(last, parent, NodeInfo.Type.Expression, "expression", null);
			routine = LASTBuilder.addNode(last, expression, NodeInfo.Type.Routine, name, routineName, info);
			LASTBuilder.addNode(last, expression, NodeInfo.Type.Result, "result", null);
		}

		return routine.getData().getId();
	}
	public static int addClause(LAST last, int functionId, LDASTNodeInfo info)
	{
		final Node parent = LASTTraverser.getNode(last, functionId);
		final NodeInfo.Type parentType = parent.getData().getType();
		if (parentType != NodeInfo.Type.Routine)
			throw new RuntimeException("A " + parentType + " cannot contain a clause");

		final Node clause = LASTBuilder.addNode(last, parent, NodeInfo.Type.Clause, "clause", info);
		//ASTBuilder.addNode(last, clause, NodeInfo.Type.Parameters, "parameters", null);
		LASTBuilder.addNode(last, clause, NodeInfo.Type.ParameterIn, "paramIn", info);
		LASTBuilder.addNode(last, clause, NodeInfo.Type.Parameters, "parameters", info); // ADDED info to parameters to obtain the name of the class
		LASTBuilder.addNode(last, clause, NodeInfo.Type.ParameterOut, "paramOut", info);
		LASTBuilder.addNode(last, clause, NodeInfo.Type.Guard, "guard", null);
		LASTBuilder.addNode(last, clause, NodeInfo.Type.Body, "body", null);
		LASTBuilder.addNode(last, clause, NodeInfo.Type.Result, "result", null);

		return clause.getData().getId();
	}
	public static int addVariable(LAST last, int parentId, Where where, String name, boolean declaration, boolean definition, boolean use, boolean global, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node variable = LASTBuilder.addVariableNode(last, parent, NodeInfo.Type.Variable, name, "variable" + "\\n" + name, info);

		final VariableInfo variableInfo = (VariableInfo) variable.getData();
		// final VariableInfo.Context context = definition ? VariableInfo.Context.Definition : VariableInfo.Context.Use; //ORIGINAL

// ADDED UnaryOperations Variable are both Definition and Uses, consider this case
		final VariableInfo.Context context;	
		//final LDASTNodeInfo parentInfo = parent.getData().getInfo(); //NOSE PARA QUE ES parentInfo
		
		//if (parentInfo != null)
			if(definition && use)
				context = VariableInfo.Context.Def_Use;
			else if (definition)
				context = VariableInfo.Context.Definition;
			else if (use)
				context = VariableInfo.Context.Use;
			else
				context = VariableInfo.Context.Declaration; // null context implica que no es ni definicion ni uso, solo declaraciones sueltas.
				//throw new RuntimeException("The variable has not been defined, neither used...");

		
		variableInfo.setDeclaration(declaration);
		variableInfo.setContext(context);
		variableInfo.setGlobal(global);

		return variableInfo.getId();
	}
	public static int addLiteral(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node literal = LASTBuilder.addNode(last, parent, NodeInfo.Type.Literal, value, "literal" + "\\n" + value, info);

		return literal.getData().getId();
	}
	public static int addEquality(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node equality = LASTBuilder.addNode(last, parent, NodeInfo.Type.Equality, "equality", info);

		return equality.getData().getId();
	}
	public static int addEquality(LAST last, int parentId, Where where, String sign, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node equality = LASTBuilder.addNode(last, parent, NodeInfo.Type.Equality, "equality" + "\\n"+ sign, info);

		return equality.getData().getId();
	}
	public static int addOperation(LAST last, int parentId, Where where, String sign, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node operation = LASTBuilder.addNode(last, parent, NodeInfo.Type.Operation, sign, "operation" + "\\n" + sign, info);

		return operation.getData().getId();
	}
	public static int addList(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node list = LASTBuilder.addNode(last, parent, NodeInfo.Type.List, "[]", "list" + "\\n" + "[]", info);

		return list.getData().getId();
	}
	public static int addDataConstructor(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node dataConstructor = LASTBuilder.addNode(last, parent, NodeInfo.Type.DataConstructor, "{}", "data constructor" + "\\n" + "{}", info);

		return dataConstructor.getData().getId();
	}
	public static int addDataConstructorAccess(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node dataConstructorAccess = LASTBuilder.addNode(last, parent, NodeInfo.Type.DataConstructorAccess, "data constructor access", "data constructor" + "\\n" + "access", info);

		return dataConstructorAccess.getData().getId();
	}
	public static int addFieldAccess(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node fieldAccess = LASTBuilder.addNode(last, parent, NodeInfo.Type.FieldAccess, "field access", "field access", info);
		
		return fieldAccess.getData().getId();
	}
	
	public static int addBlock(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node block = LASTBuilder.addNode(last, parent, NodeInfo.Type.Block, "block", info);

		return block.getData().getId();
	}
	public static int addIf(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node _if = LASTBuilder.addNode(last, parent, NodeInfo.Type.If, "if", info);
		LASTBuilder.addNode(last, _if, NodeInfo.Type.Condition, "condition", null);
		LASTBuilder.addNode(last, _if, NodeInfo.Type.Body, "then", null);
		LASTBuilder.addNode(last, _if, NodeInfo.Type.Body, "else", null);

		return _if.getData().getId();
	}
	public static int addSwitch(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node _switch = LASTBuilder.addNode(last, parent, NodeInfo.Type.Switch, "switch", info);
		LASTBuilder.addNode(last, _switch, NodeInfo.Type.Selector, "selector", null);
		LASTBuilder.addNode(last, _switch, NodeInfo.Type.Cases, "cases", null);

		return _switch.getData().getId();
	}
	public static int addCase(LAST last, int parentId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, Where.Cases);
		final Node _case = LASTBuilder.addNode(last, parent, NodeInfo.Type.Case, "case", info);
		LASTBuilder.addNode(last, _case, NodeInfo.Type.Selectable, "selectable", null);
		LASTBuilder.addNode(last, _case, NodeInfo.Type.Guard, "guard", null);
		LASTBuilder.addNode(last, _case, NodeInfo.Type.Body, "body", null);

		return _case.getData().getId();
	}
	public static int addDefaultCase(LAST last, int parentId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, Where.Cases);
		final Node defaultCase = LASTBuilder.addNode(last, parent, NodeInfo.Type.DefaultCase, "default", info);
		LASTBuilder.addNode(last, defaultCase, NodeInfo.Type.Body, "body", null);

		return defaultCase.getData().getId();
	}
	public static int addBreak(LAST last, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node breakNode = LASTBuilder.addNode(last, parent, NodeInfo.Type.Break, "break " + dstId, info);

		return breakNode.getData().getId();
	}
	public static int addContinue(LAST last, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node continueNode = LASTBuilder.addNode(last, parent, NodeInfo.Type.Continue, "continue " + dstId, info);

		return continueNode.getData().getId();
	}
	public static int addCall(LAST last, int parentId, Where where, LDASTNodeInfo info) // TODO Añadir info al nodo result para encontrar declaraciones (necesita la clase)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node call = LASTBuilder.addNode(last, parent, NodeInfo.Type.Call, "call", info);  
		final Node callee = LASTBuilder.addNode(last, call, NodeInfo.Type.Callee, "callee", null);
		LASTBuilder.addNode(last, callee, NodeInfo.Type.Scope, "scope", null);
		LASTBuilder.addNode(last, callee, NodeInfo.Type.Name, "name", null);
		LASTBuilder.addNode(last, callee, NodeInfo.Type.Result, "result", null);
		LASTBuilder.addNode(last, call, NodeInfo.Type.ArgumentIn, "argsIn", null);
		LASTBuilder.addNode(last, call, NodeInfo.Type.Arguments, "arguments", null);
		LASTBuilder.addNode(last, call, NodeInfo.Type.ArgumentOut, "argsOut", null);
		return call.getData().getId();
	}

	public static int addLabel(LAST last, int parentId, Where where, String labelText, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node label = LASTBuilder.addNode(last, parent, NodeInfo.Type.Label, "labeledExpr" + "\\n" + labelText, info);
		
		return label.getData().getId();
	}
//	public static void addArgumentsInOut(LAST last, int callId, Where where, LDASTNodeInfo info)
//	{
//		final Node callNode = LASTTraverser.getNode(last, callId);
//		final Node argumentsNode = ASTBuilder.getCallChildNode(callNode, where);
//		ASTBuilder.addNode(last, argumentsNode, NodeInfo.Type.ArgumentIn, "in", null);
//		ASTBuilder.addNode(last, argumentsNode, NodeInfo.Type.ArgumentOut, "out", null);
//	}

	public static int addExHandler(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node exHandler = LASTBuilder.addNode(last, parent, NodeInfo.Type.ExHandler, "exHandler", info);
		LASTBuilder.addNode(last, exHandler, NodeInfo.Type.Try, "try", null);
		LASTBuilder.addNode(last, exHandler, NodeInfo.Type.Catch, "catch", null);
		LASTBuilder.addNode(last, exHandler, NodeInfo.Type.Finally, "finally", null);

		return exHandler.getData().getId();
	}
	public static int addCatchClause(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node catchClause = LASTBuilder.addNode(last, parent, NodeInfo.Type.CatchClause, "clause", info);
		
		LASTBuilder.addNode(last, catchClause, NodeInfo.Type.Parameters, "parameters", info); // ADDED info to parameters to obtain the name of the class
		LASTBuilder.addNode(last, catchClause, NodeInfo.Type.Guard, "guard", null);
		LASTBuilder.addNode(last, catchClause, NodeInfo.Type.Body, "body", null);
		LASTBuilder.addNode(last, catchClause, NodeInfo.Type.Result, "result", null);

		return catchClause.getData().getId();
	}
	public static int addThrow(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node _throw = LASTBuilder.addNode(last, parent, NodeInfo.Type.Throw, "throw", info);
		
		return _throw.getData().getId();
	}
	
	public static int addListComprehension(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node lc = LASTBuilder.addNode(last, parent, NodeInfo.Type.ListComprehension, "lc", info);
		LASTBuilder.addNode(last, lc, NodeInfo.Type.Restrictions, "restrictions", null);
		LASTBuilder.addNode(last, lc, NodeInfo.Type.Value, "value", null);

		return lc.getData().getId();
	}
	public static int addGenerator(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node generator = LASTBuilder.addNode(last, parent, NodeInfo.Type.Generator, "generator", info);

		return generator.getData().getId();
	}
	public static int addFilter(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node filter = LASTBuilder.addNode(last, parent, NodeInfo.Type.Filter, "filter", info);

		return filter.getData().getId();
	}
	public static int addForLoop(LAST last, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node loop = LASTBuilder.addNode(last, parent, NodeInfo.Type.FLoop, "loop", info);
		
		LASTBuilder.addNode(last, loop, NodeInfo.Type.Init, "init", null);
		LASTBuilder.addNode(last, loop, NodeInfo.Type.Condition, "condition", null);
		LASTBuilder.addNode(last, loop, NodeInfo.Type.Body, "body", null);
		LASTBuilder.addNode(last, loop, NodeInfo.Type.Update, "update", null);
		
		return loop.getData().getId();
	}
	public static int addCondLoop(LAST last, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node loop = LASTBuilder.addNode(last, parent, NodeInfo.Type.CLoop, "loop", info);
		
		LASTBuilder.addNode(last, loop, NodeInfo.Type.Condition, "condition", null);
		LASTBuilder.addNode(last, loop, NodeInfo.Type.Body, "body", null);
		
		return loop.getData().getId();
	}
	public static int addRepeatLoop(LAST last, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node loop = LASTBuilder.addNode(last, parent, NodeInfo.Type.RLoop, "loop", info);
		
		// EN ORDEN DE APARICION EN EL CODIGO, NECESARIO A LA HORA DE MIRAR DEFINICIONES PREVIAS
		LASTBuilder.addNode(last, loop, NodeInfo.Type.Body, "body", null);
		LASTBuilder.addNode(last, loop, NodeInfo.Type.Condition, "condition", null);
		
// THE ORDER COND-BODY EASE THE TRAVESAL TO GENERATE CONTROL DEPENDENCIES GUARD/COND -> BODY, FINISHING THE TRAVERSAL AT THE BODY NODE.
// IT IS NOT VALID BECAUSE THE ORDER OF APPEAREANCE RUINS THE SCOPE USED IN DEF-USE FLOW DEPENDENCIES
//		ASTBuilder.addNode(last, loop, NodeInfo.Type.Condition, "condition", null);
//		ASTBuilder.addNode(last, loop, NodeInfo.Type.Body, "body", null);
		
		return loop.getData().getId();
	}
	public static int addForeach(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node foreach = LASTBuilder.addNode(last, parent, NodeInfo.Type.Foreach, "foreach", info);
		
		LASTBuilder.addNode(last, foreach, NodeInfo.Type.Iterator, "iterator", null);
		LASTBuilder.addNode(last, foreach, NodeInfo.Type.Body, "body", null);
		
		return foreach.getData().getId();
	}
	public static int addReturn(LAST last, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node _return = LASTBuilder.addNode(last, parent, NodeInfo.Type.Return, "return " + dstId, info);

		return _return.getData().getId();
	}

	private static Node addNode(LAST last, Node parent, NodeInfo.Type type, String name, LDASTNodeInfo info)
	{
		return LASTBuilder.addNode(last, parent, type, name, name, info);
	}
	private static Node addNode(LAST last, Node parent, NodeInfo.Type type, String name, String text, LDASTNodeInfo info)
	{
		return LASTBuilder.addNode(last, parent, type, false, name, text, info);
	}
	private static Node addVariableNode(LAST last, Node parent, NodeInfo.Type type, String name, LDASTNodeInfo info)
	{
		return LASTBuilder.addVariableNode(last, parent, type, name, name, info);
	}
	private static Node addVariableNode(LAST last, Node parent, NodeInfo.Type type, String name, String text, LDASTNodeInfo info)
	{
		return LASTBuilder.addNode(last, parent, type, true, name, text, info);
	}
	private static Node addNode(LAST last, Node parent, NodeInfo.Type type, boolean isVariable, String name, String text, LDASTNodeInfo info)
	{
		if (info != null && info.getArchive() == null)
			info.setArchive(LASTBuilder.getArchive(parent));
		if (info != null && info.getClassName() == null)
			info.setClassName(LASTBuilder.getClassName(parent));

		
//final NodeInfo nodeInfo0 = ASTBuilder.getNodeInfo(type, isVariable, name, info);
// ADDED SDG NODE ID
		//final NodeInfo.Type parentType = parent.getData().getType();
		final NodeInfo nodeInfo = LASTBuilder.getNodeInfo(type, parent, isVariable, name, info);
		
		final EdgeInfo edgeInfo = new EdgeInfo(EdgeInfo.Type.Structural);
		final Node node = new Node(text, nodeInfo);
		
		last.addNode(node);
		last.addEdge(parent, node, 0, edgeInfo);

		return node;
	}
	public static void addNode(LAST last, Node node, Node parent, EdgeInfo info)
	{
		last.addNode(node);
		last.addEdge(parent, node, 0, info);
	}
	private static String getArchive(Node node)
	{
		Node ancestor = node;

		while (ancestor != null)
		{
			final LDASTNodeInfo nodeInfo = ancestor.getData().getInfo();
			if (nodeInfo != null)
			{
				final String archive = nodeInfo.getArchive();
				if (archive != null)
					return archive;
			}
			ancestor = LASTTraverser.getParent(ancestor);
		}

		return null;
	}
	private static String getClassName(Node node)
	{
		Node ancestor = node;

		while (ancestor != null)
		{
			final LDASTNodeInfo nodeInfo = ancestor.getData().getInfo();
			if (nodeInfo != null)
			{
				final String className = nodeInfo.getClassName();
				if (className != null)
					return className;
			}
			ancestor = LASTTraverser.getParent(ancestor);
		}

		return null;
	}

	// Typed languages
	public static int addTypeCheck(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node typeCheck = LASTBuilder.addNode(last, parent, NodeInfo.Type.TypeCheck, "typecheck", info);

		return typeCheck.getData().getId();
	}
	public static int addTypeTransformation(LAST last, int parentId, Where where, LDASTNodeInfo info, boolean isEnclosedExpr)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
//		final Node expression = LASTBuilder.addNode(last, parent, NodeInfo.Type.Expression, "expression", new LDASTNodeInfo(-1, "", isEnclosedExpr));
		info.addInfo(isEnclosedExpr);
		final Node typeTrans = LASTBuilder.addNode(last, parent, NodeInfo.Type.TypeTransformation, "typeTransformation", info);
		
		return typeTrans.getData().getId();
	}
	public static int addTypeTransformation(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node typeTrans = LASTBuilder.addNode(last, parent, NodeInfo.Type.TypeTransformation, "typeTransformation", info);
		
		return typeTrans.getData().getId();
	}
	public static int addSuperReference(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		
		// TODO asignar tipo a super
//		final Node classNode = LASTTraverser.getAncestor(parent, NodeInfo.Type.Module);
//		final String extendedClassName = classNode.getData().getInfo().getInfo()[0].toString();

		final Node reference = LASTBuilder.addNode(last, parent, NodeInfo.Type.Reference, value, "reference" + "\\n" + value, info);

		return reference.getData().getId();

	}
	public static int addThisReference(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		
		// TODO asignar tipo a super
//		final Node classNode = LASTTraverser.getAncestor(parent, NodeInfo.Type.Module);
//		final String extendedClassName = classNode.getData().getInfo().getInfo()[0].toString();

		final Node reference = LASTBuilder.addNode(last, parent, NodeInfo.Type.Reference, value, "reference" + "\\n" + value, info);

		return reference.getData().getId();

	}
	public static int addType(LAST last, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final String label = info.getInfo().length == 0 ? "type" + "\\n" + value : "type" + "\\n" + value +"[]" ;
		final Node type = LASTBuilder.addNode(last, parent, NodeInfo.Type.Type, value, label, info);

		return type.getData().getId();
	}
	
	// Complete LAST
	public static void completeLAST(LAST last)
	{
		final Node root = last.getRootNode();
		Node currentNode = root;
		int childIndexToVisit = 0;
		final LinkedList<Integer> bifurcations = new LinkedList<Integer>();

		while (true)
		{
			final List<Node> children = LASTTraverser.getChildren(currentNode);

			if (childIndexToVisit == children.size())
			{ // It is a leaf or all its children have been already processed
				LASTBuilder.completeNode(currentNode);
				if (currentNode == root)
					break;
				// Go back to parent
				currentNode = LASTTraverser.getParent(currentNode);
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
	private static void completeNode(Node node)
	{
		final NodeInfo.Type type = node.getData().getType();

		switch (type)
		{
			case Expression:
				LASTBuilder.completeExpression(node);
				break;
			default:
				break;
		}
	}
	private static void completeExpression(Node node)
	{
		final Node child = LASTTraverser.getChild(node, 0);
		final NodeInfo.Type childType = child.getData().getType();
	}

	// Add class inheritance information
	public static void addInheritanceInfomation(LAST last) {
		final Node root = last.getRootNode();
		final List<Node> children = LASTTraverser.getChildren(root);
		
		final List<String> definedClasses = new LinkedList<String>();
		for (Node child : children)
			definedClasses.add(child.getData().getInfo().getClassName());
		
		final Map<String, Node> treatedClasses = new HashMap<String, Node>();
		int index = 0;
		while (!children.isEmpty())
		{	
			final Node child = children.get(index);
			final String className = child.getData().getInfo().getClassName();
			final String extendedClassName = child.getData().getInfo().getInfo()[0].toString();
			if (extendedClassName.equals("") || !definedClasses.contains(extendedClassName))
			{
				LASTBuilder.addNonExtendedClassInfo(child);
				treatedClasses.put(className,children.remove(index));
				index = 0;
			}
			else if(treatedClasses.containsKey(extendedClassName))
			{
				// TRATAR LA INFORMACION LINKEANDO SUS METODOS
				final Node parent = treatedClasses.get(extendedClassName);
				LASTBuilder.addExtendedClassInfo(child, parent);
				treatedClasses.put(className,children.remove(index));
				index = 0;
			}
			else
				// TRATAR LA SIGUIENTE
				index++;
		}
	}
	public static void addNonExtendedClassInfo(Node node)
	{
		final List<Node> children = LASTTraverser.getChildren(node);
		
		final Map<String, List<Node>> methods = new HashMap<String, List<Node>>();
		final Map<String, Node> variables = new HashMap<String, Node>();
		
		for (Node child : children)
		{
			if (child.getData().getType() == NodeInfo.Type.Expression || child.getData().getType() == NodeInfo.Type.Variable)
			{
				final Node expressionInstanceNode = child.getData().getType() == NodeInfo.Type.Expression ? LASTTraverser.getChild(child,0) : child;
				final Node variableNode = expressionInstanceNode.getData().getType() == NodeInfo.Type.Variable ? 
							expressionInstanceNode : LASTTraverser.getChild(LASTTraverser.getChild(expressionInstanceNode,0),0);
				final String name = variableNode.getData().getName();
				
				variables.put(name, variableNode);
			}
			else
			{ 
				final String methodName = child.getData().getName();
				final List<Node> methodClauses = LASTTraverser.getChildren(child);
				List<Node> previousClauses = methods.get(methodName);
				if (previousClauses != null)
					methodClauses.addAll(previousClauses);
				methods.put(methodName, methodClauses);
			}
		}
		LDASTNodeInfo nodeInfo = node.getData().getInfo();
		ClassInfo classInfo = new ClassInfo(methods, variables);
		nodeInfo.addInfo(classInfo);
	}
	public static void addExtendedClassInfo(Node node, Node parent)
	{
		ClassInfo parentClassInfo = (ClassInfo) parent.getData().getInfo().getInfo()[2];
		ClassInfo nodeClassInfo = new ClassInfo(parentClassInfo);
		
		final List<Node> children = LASTTraverser.getChildren(node);
		
		for (Node child : children)
		{
			if (child.getData().getType() == NodeInfo.Type.Expression || child.getData().getType() == NodeInfo.Type.Variable)
			{
				final Node expressionInstanceNode = child.getData().getType() == NodeInfo.Type.Expression ? LASTTraverser.getChild(child,0) : child;
				final Node variableNode = expressionInstanceNode.getData().getType() == NodeInfo.Type.Variable ? 
							expressionInstanceNode : LASTTraverser.getChild(LASTTraverser.getChild(expressionInstanceNode,0),0);
				final String name = variableNode.getData().getName();
				
				nodeClassInfo.variables.put(name, variableNode);
			}
			else
			{ 
				final String methodName = child.getData().getName();
				final List<Node> methodClauses = LASTTraverser.getChildren(child);
				final List<Integer> resultArities = new LinkedList<Integer>();
				for (Node clause : methodClauses)
				{	
					final int arity = LASTTraverser.getChildren(LASTTraverser.getChild(clause, 0)).size();
					if (!resultArities.contains(arity))
						resultArities.add(arity);
				}
				
				if (nodeClassInfo.methods.containsKey(methodName) && methodName.equals("<constructor>"))
					nodeClassInfo.methods.put("super<constructor>", nodeClassInfo.methods.get("<constructor>"));
				
				if (nodeClassInfo.methods.containsKey(methodName))
				{
					final List<Node> parentMethodClauses = nodeClassInfo.methods.get(methodName);
					for (Node parentMethodClause : parentMethodClauses)
					{
						final int existentArity = LASTTraverser.getChildren(LASTTraverser.getChild(parentMethodClause, 0)).size();
						if (!resultArities.contains(existentArity))
							methodClauses.add(parentMethodClause);
					}
				}
				nodeClassInfo.methods.put(methodName, methodClauses);
			}
		}
		LDASTNodeInfo nodeInfo = node.getData().getInfo();
		nodeInfo.addInfo(nodeClassInfo);
		
		parentClassInfo.addChildClass(nodeClassInfo);
	}
	
	// Child node
	private static Node getClauseChildNode(Node clause, Where where)
	{
		switch (where)
		{
			case ParameterIn:
				return LASTTraverser.getChild(clause, 0);
			case Parameters:
				return LASTTraverser.getChild(clause, 1);
			case ParameterOut:
				return LASTTraverser.getChild(clause, 2);
			case Guard:
				return LASTTraverser.getChild(clause, 3);
			case Body:
				return LASTTraverser.getChild(clause, 4);
			default:
				throw new RuntimeException("A clause cannot contain " + where);
		}
	}
	private static Node getIfChildNode(Node _if, Where where)
	{
		switch (where)
		{
			case Condition:
				return LASTTraverser.getChild(_if, 0);
			case Then:
				return LASTTraverser.getChild(_if, 1);
			case Else:
				return LASTTraverser.getChild(_if, 2);
			default:
				throw new RuntimeException("An if cannot contain " + where);
		}
	}
	private static Node getSwitchChildNode(Node _switch, Where where)
	{
		switch (where)
		{
			case Selector:
				return LASTTraverser.getChild(_switch, 0);
			case Cases:
				return LASTTraverser.getChild(_switch, 1);
			default:
				throw new RuntimeException("A switch cannot contain " + where);
		}
	}
	private static Node getCaseChildNode(Node _case, Where where)
	{
		switch (where)
		{
			case Selectable:
				return LASTTraverser.getChild(_case, 0);
			case Guard:
				return LASTTraverser.getChild(_case, 1);
			case Body:
				return LASTTraverser.getChild(_case, 2);
			default:
				throw new RuntimeException("A case cannot contain " + where);
		}
	}
	private static Node getDefaultCaseChildNode(Node defaultCase, Where where)
	{
		switch (where)
		{
			case Body:
				return LASTTraverser.getChild(defaultCase, 0);
			default:
				throw new RuntimeException("A default case cannot contain " + where);
		}
	}
	private static Node getCallChildNode(Node call, Where where)
	{
		switch (where)
		{
			case Scope:
				final Node callee = LASTTraverser.getChild(call, 0);
				return LASTTraverser.getChild(callee, 0);
			case Name:
				final Node callee0 = LASTTraverser.getChild(call, 0);
				return LASTTraverser.getChild(callee0, 1);
			case ArgumentIn:
				return LASTTraverser.getChild(call, 1);
			case Arguments:
				return LASTTraverser.getChild(call, 2);
			case ArgumentOut:
				return LASTTraverser.getChild(call, 3);
			default:
				throw new RuntimeException("A call cannot contain " + where);
		}
	}
	private static Node getListComprehensionChildNode(Node listComprehension, Where where)
	{
		switch (where)
		{
			case Restrictions:
				return LASTTraverser.getChild(listComprehension, 0);
			case Value:
				return LASTTraverser.getChild(listComprehension, 1);
			default:
				throw new RuntimeException("A list comprehension cannot contain " + where);
		}
	}
	private static Node getLoopChildNode(Node loop, Where where)
	{
		switch (where)
		{
			case Init:
				return LASTTraverser.getChild(loop, NodeInfo.Type.Init);
			case Condition:
				return LASTTraverser.getChild(loop, NodeInfo.Type.Condition);
			case Body:
				return LASTTraverser.getChild(loop, NodeInfo.Type.Body);
			case Update:
				return LASTTraverser.getChild(loop, NodeInfo.Type.Update);
			default:
				throw new RuntimeException("A loop cannot contain " + where);
		}
	}
	private static Node getForeachChildNode(Node foreach, Where where)
	{
		switch (where)
		{
			case Iterator:
				return LASTTraverser.getChild(foreach, NodeInfo.Type.Iterator);
			case Body:
				return LASTTraverser.getChild(foreach, NodeInfo.Type.Body);
			default:
				throw new RuntimeException("A loop cannot contain " + where);
		}
	}
	private static Node getExhandlerChildNode(Node exHandler, Where where)
	{
		switch (where)
		{
			case Try:
				return LASTTraverser.getChild(exHandler, NodeInfo.Type.Try);
			case Catch:
				return LASTTraverser.getChild(exHandler, NodeInfo.Type.Catch);
			case Finally:
				return LASTTraverser.getChild(exHandler, NodeInfo.Type.Finally);
			default:
				throw new RuntimeException("An exception handler cannot contain " + where);
		}
	}
	
	// Common
	public static List<Where> getWheres(NodeInfo.Type type)
	{
		final List<Where> wheres = new LinkedList<Where>();

		switch (type)
		{
			case Clause:
				wheres.add(Where.Parameters);
				wheres.add(Where.Guard);
				wheres.add(Where.Body);
				break;
			case If:
				wheres.add(Where.Condition);
				wheres.add(Where.Then);
				wheres.add(Where.Else);
				break;
			case Switch:
				wheres.add(Where.Selector);
				wheres.add(Where.Cases);
				break;
			case Case:
				wheres.add(Where.Selectable);
				wheres.add(Where.Guard);
				wheres.add(Where.Body);
				break;
			case DefaultCase:
				wheres.add(Where.Body);
				break;
			case Call:
				wheres.add(Where.Scope);
				wheres.add(Where.Name);
				wheres.add(Where.Arguments);
				break;
			case ListComprehension:
				wheres.add(Where.Restrictions);
				wheres.add(Where.Value);
				break;
			case FLoop: //ADDED INIT & UPDATE
				wheres.add(Where.Init);
				wheres.add(Where.Condition);
				wheres.add(Where.Body);
				wheres.add(Where.Update);
				break;
			case CLoop: 
			case RLoop: 
				wheres.add(Where.Condition);
				wheres.add(Where.Body);
				break;
			case Module:
			case Routine:
			case Variable:
			case Literal:
			case Equality:
			case Operation:
			case DataConstructor:
			case List:
			case Block:
				break;
			default:
				throw new RuntimeException("Type not contemplated: " + type);
		}

		return wheres;
	}
	private static Node getParentNode(LAST last, int parentId, Where where)
	{
		final Node parentNode = LASTTraverser.getNode(last, parentId);
		final NodeInfo.Type type = parentNode.getData().getType();

		switch (type)
		{
			case Clause:
			case CatchClause:
				return LASTBuilder.getClauseChildNode(parentNode, where);
			case Call:
				return LASTBuilder.getCallChildNode(parentNode, where);
			case If:
				return LASTBuilder.getIfChildNode(parentNode, where);
			case Switch:
				return LASTBuilder.getSwitchChildNode(parentNode, where);
			case Case:
				return LASTBuilder.getCaseChildNode(parentNode, where);
			case DefaultCase:
				return LASTBuilder.getDefaultCaseChildNode(parentNode, where);
			case ListComprehension:
				return LASTBuilder.getListComprehensionChildNode(parentNode, where);
			case FLoop:
			case CLoop:
			case RLoop:
				return LASTBuilder.getLoopChildNode(parentNode, where);
			case Foreach:
				return LASTBuilder.getForeachChildNode(parentNode, where);
//ADDED
//case ForLoop:
//	return ASTBuilder.getLoopChildNode(parentNode, where);
//
			case ExHandler:
				return LASTBuilder.getExhandlerChildNode(parentNode, where); 
				
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
			case Label:
			case FieldAccess:
				if (where == null)
					return parentNode;
			default:
				throw new RuntimeException(type + " does not contain " + where);
		}
	}
	public static NodeInfo getNodeInfo(NodeInfo.Type type, boolean isVariable, String name, LDASTNodeInfo info)
	{
		if (isVariable)
			return new VariableInfo(LASTBuilder.nextId++, type, name, info);
		return new NodeInfo(LASTBuilder.nextId++, type, name, info);
	}
	static NodeInfo getNodeInfo(NodeInfo.Type type, Node parent, boolean isVariable, String name, LDASTNodeInfo info)
	{
		NodeInfo.Type parentType = parent.getData().getType();
		switch(type)
		{
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
				return new NodeInfo(LASTBuilder.nextId++, type, name, info);
			case Result:
				final Node firstSibling = LASTTraverser.getChild(parent, 0);
				final NodeInfo.Type siblingType = firstSibling.getData().getType();
				switch(siblingType)
                {
                    case Variable:
                    case Literal:
                    case DataConstructorAccess:
                    case DataConstructor:
                    case Operation:
                        if (isVariable)
                            return new VariableInfo(LASTBuilder.nextId++, type, name, info);
                        return new NodeInfo(LASTBuilder.nextId++, type, name, info);
                    default:
                        return new NodeInfo(LASTBuilder.nextId++, type, name, info);
                }
			case DefaultCase:

                final List<Node> children = LASTTraverser.getChildren(parent);
                if (children.size() == 1)
                    return new NodeInfo(LASTBuilder.nextId++, type, name, info);

                final Node lastChild = children.get(children.size() - 1);
                if (lastChild.getData().getType() != NodeInfo.Type.DefaultCase)
                    return new NodeInfo(LASTBuilder.nextId++, type, name, info);

                final Node penultimateChild = children.get(children.size() - 2);
                return new NodeInfo(LASTBuilder.nextId++, type, name, info);

            case Expression:
                switch (parentType)
                {
                    case Parameters:
                        final Node grandParent = LASTTraverser.getParent(parent);
                        if (grandParent.getData().getType() == NodeInfo.Type.CatchClause)
                            return new NodeInfo(LASTBuilder.nextId++, type, name, info);
                    case Arguments:
                    case Body:
					case Block:
					case Module:
                    case Init:
                    case Update:
                    case Try:
                    case Finally:
                        return new NodeInfo(LASTBuilder.nextId++, type, name, info);
                    default:
                        break;
                }
            default:
                if (isVariable)
                    return new VariableInfo(LASTBuilder.nextId++, type, name, info);
                return new NodeInfo(LASTBuilder.nextId++, type, name, info);
        }
	}

	public static class ClassInfo
	{
		private Map<String, List<Node>> methods;
		private Map<String, Node> variables;
		private List<ClassInfo> childrenClasses = new LinkedList<ClassInfo>();
		
		public ClassInfo(ClassInfo ci)
		{
			this.methods = new HashMap<String, List<Node>>();
			this.variables = new HashMap<String, Node>();
			methods.putAll(ci.methods);
			variables.putAll(ci.variables);
		}
		
		public ClassInfo(Map<String, List<Node>> methodMap, Map<String, Node> variableMap)
		{
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
		
	}
}