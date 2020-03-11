package upv.slicing.edg;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.graph.Variable;
import upv.slicing.edg.traverser.LASTTraverser;

import java.util.*;

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
        final Node rootNode = new Node("LAST", LASTBuilder.nextId++, Node.Type.Root, "LAST", info);

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
		final Node parent = LASTTraverser.getNode(last, functionId);
		final Node.Type parentType = parent.getType();
		if (parentType != Node.Type.Routine)
			throw new RuntimeException("A " + parentType + " cannot contain a clause");

		final LDASTNodeInfo expInfo = new LDASTNodeInfo(info, true);
		final Node clause = LASTBuilder.addNode(last, parent, Node.Type.Clause, "clause", expInfo);
		//ASTBuilder.addNode(last, clause, Node.Info.Type.Parameters, "parameters", null);
		LASTBuilder.addNode(last, clause, Node.Type.ParameterIn, "paramIn", info);
		LASTBuilder.addNode(last, clause, Node.Type.Parameters, "parameters", info); // ADDED info to parameters to obtain the name of the class
		LASTBuilder.addNode(last, clause, Node.Type.ParameterOut, "paramOut", info);
		LASTBuilder.addNode(last, clause, Node.Type.Guard, "guard", null);
		LASTBuilder.addNode(last, clause, Node.Type.Body, "body", null);

		return clause.getId();
	}
	public static int addVariable(LAST last, int parentId, Where where, String name, boolean declaration, boolean definition, boolean use, boolean global, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node variable = LASTBuilder.addVariableNode(last, parent, Node.Type.Variable, name, "variable" + "\\n" + name, info);

		final Variable variableInfo = (Variable) variable;
		// final VariableInfo.Context context = definition ? VariableInfo.Context.Definition : VariableInfo.Context.Use; //ORIGINAL

// ADDED UnaryOperations Variable are both Definition and Uses, consider this case
		final Variable.Context context;
		//final LDASTNodeInfo parentInfo = parent.getInfo(); //NOSE PARA QUE ES parentInfo
		
		//if (parentInfo != null)
			if(definition && use)
				context = Variable.Context.Def_Use;
			else if (definition)
				context = Variable.Context.Definition;
			else if (use)
				context = Variable.Context.Use;
			else
				context = Variable.Context.Declaration; // null context implica que no es ni definicion ni uso, solo declaraciones sueltas.
				//throw new RuntimeException("The variable has not been defined, neither used...");

		
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
//	public static void addArgumentsInOut(LAST last, int callId, Where where, LDASTNodeInfo info)
//	{
//		final Node callNode = LASTTraverser.getNode(last, callId);
//		final Node argumentsNode = ASTBuilder.getCallChildNode(callNode, where);
//		ASTBuilder.addNode(last, argumentsNode, Node.Info.Type.ArgumentIn, "in", null);
//		ASTBuilder.addNode(last, argumentsNode, Node.Info.Type.ArgumentOut, "out", null);
//	}

	public static int addExHandler(LAST last, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = LASTBuilder.getParentNode(last, parentId, where);
		final Node exHandler = LASTBuilder.addNode(last, parent, Node.Type.ExHandler, "exHandler", info);
		LASTBuilder.addNode(last, exHandler, Node.Type.Try, "try", null);
		LASTBuilder.addNode(last, exHandler, Node.Type.Catch, "catch", null);
		LASTBuilder.addNode(last, exHandler, Node.Type.Finally, "finally", null);

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
		
// THE ORDER COND-BODY EASE THE TRAVESAL TO GENERATE CONTROL DEPENDENCIES GUARD/COND -> BODY, FINISHING THE TRAVERSAL AT THE BODY NODE.
// IT IS NOT VALID BECAUSE THE ORDER OF APPEAREANCE RUINS THE SCOPE USED IN DEF-USE FLOW DEPENDENCIES
//		ASTBuilder.addNode(last, loop, Node.Info.Type.Condition, "condition", null);
//		ASTBuilder.addNode(last, loop, Node.Info.Type.Body, "body", null);
		
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
	private static Node addNode(LAST last, Node parent, Node.Type type, String name, String text, LDASTNodeInfo info)
	{
		return LASTBuilder.addNode(last, parent, type, false, name, text, info);
	}
	private static Node addVariableNode(LAST last, Node parent, Node.Type type, String name, LDASTNodeInfo info)
	{
		return LASTBuilder.addVariableNode(last, parent, type, name, name, info);
	}
	private static Node addVariableNode(LAST last, Node parent, Node.Type type, String name, String text, LDASTNodeInfo info)
	{
		return LASTBuilder.addNode(last, parent, type, true, name, text, info);
	}
	private static Node addNode(LAST last, Node parent, Node.Type type, boolean isVariable, String name, String text, LDASTNodeInfo info)
	{
		if (info != null && info.getFile() == null)
			info.setFile(LASTBuilder.getArchive(last, parent));
		if (info != null && info.getClassName() == null)
			info.setClassName(LASTBuilder.getClassName(last, parent));


//final Node.Info nodeInfo0 = ASTBuilder.getNode(type, isVariable, name, info);
// ADDED SDG NODE ID
		//final Node.Info.Type parentType = parent.getType();
		final Node node = LASTBuilder.getNode(last, type, parent, isVariable, name, info);
		node.setLabel(text);

		last.addNode(node);
		last.addEdge(parent, node, Edge.Type.Structural);

		return node;
	}
	public static void addNode(LAST last, Node node, Node parent, Edge.Type type)
	{
		last.addNode(node);
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
			ancestor = LASTTraverser.getParent(last, ancestor);
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
			ancestor = LASTTraverser.getParent(last, ancestor);
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
			final List<Node> children = LASTTraverser.getChildren(last, currentNode);

			if (childIndexToVisit == children.size())
			{ // It is a leaf or all its children have been already processed
				LASTBuilder.completeNode(last, currentNode);
				if (currentNode == root)
					break;
				// Go back to parent
				currentNode = LASTTraverser.getParent(last, currentNode);
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
		final Node child = LASTTraverser.getChild(last, node, 0);
		final Node.Type childType = child.getType();
	}

	// Add class inheritance information
	public static void addInheritanceInfomation(LAST last) {
		final Node root = last.getRootNode();
		final List<Node> children = LASTTraverser.getChildren(last, root);
		
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
		final List<Node> children = LASTTraverser.getChildren(last, node);
		
		final Map<String, List<Node>> methods = new HashMap<>();
		final Map<String, Node> variables = new HashMap<>();
		
		for (Node child : children)
		{
			if (child.getType() == Node.Type.Expression || child.getType() == Node.Type.Variable)
			{
				final Node expressionInstanceNode = child.getType() == Node.Type.Expression ? LASTTraverser.getChild(last, child,0) : child;
				final Node variableNode = expressionInstanceNode.getType() == Node.Type.Variable ?
							expressionInstanceNode : LASTTraverser.getChild(last, LASTTraverser.getChild(last, expressionInstanceNode,0),0);
				final String name = variableNode.getName();
				
				variables.put(name, variableNode);
			}
			else
			{ 
				final String methodName = child.getName();
				final List<Node> methodClauses = LASTTraverser.getChildren(last, child);
				methodClauses.removeIf(n -> n.getType() != Node.Type.Clause);
				List<Node> previousClauses = methods.get(methodName);
				if (previousClauses != null)
					methodClauses.addAll(previousClauses);
				methods.put(methodName, methodClauses);
			}
		}
		LDASTNodeInfo nodeInfo = node.getInfo();
		ClassInfo classInfo = new ClassInfo(methods, variables);
		nodeInfo.addInfo(classInfo);
	}
	public static void addExtendedClassInfo(LAST last, Node node, Node parent)
	{
		ClassInfo parentClassInfo = (ClassInfo) parent.getInfo().getInfo()[2];
		ClassInfo nodeClassInfo = new ClassInfo(parentClassInfo);
		
		final List<Node> children = LASTTraverser.getChildren(last, node);
		
		for (Node child : children)
		{
			if (child.getType() == Node.Type.Expression || child.getType() == Node.Type.Variable)
			{
				final Node expressionInstanceNode = child.getType() == Node.Type.Expression ? LASTTraverser.getChild(last, child,0) : child;
				final Node variableNode = expressionInstanceNode.getType() == Node.Type.Variable ?
							expressionInstanceNode : LASTTraverser.getChild(last, LASTTraverser.getChild(last, expressionInstanceNode,0),0);
				final String name = variableNode.getName();
				
				nodeClassInfo.variables.put(name, variableNode);
			}
			else
			{ 
				final String methodName = child.getName();
				final List<Node> methodClauses = LASTTraverser.getChildren(last, child);
				final List<Integer> resultArities = new LinkedList<>();
				for (Node clause : methodClauses)
				{	
					final int arity = LASTTraverser.getChildren(last, LASTTraverser.getChild(last, clause, 0)).size();
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
						final int existentArity = LASTTraverser.getChildren(last, LASTTraverser.getChild(last, parentMethodClause, 0)).size();
						if (!resultArities.contains(existentArity))
							methodClauses.add(parentMethodClause);
					}
				}
				nodeClassInfo.methods.put(methodName, methodClauses);
			}
		}
		LDASTNodeInfo nodeInfo = node.getInfo();
		nodeInfo.addInfo(nodeClassInfo);
		
		parentClassInfo.addChildClass(nodeClassInfo);
	}
	
	// Child node
	private static Node getClauseChildNode(LAST last, Node clause, Where where)
	{
		switch (where)
		{
			case ParameterIn:
				return LASTTraverser.getChild(last, clause, 0);
			case Parameters:
				return LASTTraverser.getChild(last, clause, 1);
			case ParameterOut:
				return LASTTraverser.getChild(last, clause, 2);
			case Guard:
				return LASTTraverser.getChild(last, clause, 3);
			case Body:
				return LASTTraverser.getChild(last, clause, 4);
			default:
				throw new RuntimeException("A clause cannot contain " + where);
		}
	}
	private static Node getIfChildNode(LAST last, Node _if, Where where)
	{
		switch (where)
		{
			case Condition:
				return LASTTraverser.getChild(last, _if, 0);
			case Then:
				return LASTTraverser.getChild(last, _if, 1);
			case Else:
				return LASTTraverser.getChild(last, _if, 2);
			default:
				throw new RuntimeException("An if cannot contain " + where);
		}
	}
	private static Node getSwitchChildNode(LAST last, Node _switch, Where where)
	{
		switch (where)
		{
			case Selector:
				return LASTTraverser.getChild(last, _switch, 0);
			case Cases:
				return LASTTraverser.getChild(last, _switch, 1);
			default:
				throw new RuntimeException("A switch cannot contain " + where);
		}
	}
	private static Node getCaseChildNode(LAST last, Node _case, Where where)
	{
		switch (where)
		{
			case Selectable:
				return LASTTraverser.getChild(last, _case, 0);
			case Guard:
				return LASTTraverser.getChild(last, _case, 1);
			case Body:
				return LASTTraverser.getChild(last, _case, 2);
			default:
				throw new RuntimeException("A case cannot contain " + where);
		}
	}
	private static Node getDefaultCaseChildNode(LAST last, Node defaultCase, Where where)
	{
		switch (where)
		{
			case Body:
				return LASTTraverser.getChild(last, defaultCase, 0);
			default:
				throw new RuntimeException("A default case cannot contain " + where);
		}
	}
	private static Node getCallChildNode(LAST last, Node call, Where where)
	{
		switch (where)
		{
			case Scope:
				final Node callee = LASTTraverser.getChild(last, call, 0);
				return LASTTraverser.getChild(last, callee, 0);
			case Name:
				final Node callee0 = LASTTraverser.getChild(last, call, 0);
				return LASTTraverser.getChild(last, callee0, 1);
			case ArgumentIn:
				return LASTTraverser.getChild(last, call, 1);
			case Arguments:
				return LASTTraverser.getChild(last, call, 2);
			case ArgumentOut:
				return LASTTraverser.getChild(last, call, 3);
			default:
				throw new RuntimeException("A call cannot contain " + where);
		}
	}
	private static Node getListComprehensionChildNode(LAST last, Node listComprehension, Where where)
	{
		switch (where)
		{
			case Restrictions:
				return LASTTraverser.getChild(last, listComprehension, 0);
			case Value:
				return LASTTraverser.getChild(last, listComprehension, 1);
			default:
				throw new RuntimeException("A list comprehension cannot contain " + where);
		}
	}

	private static Node getLoopChildNode(LAST last, Node loop, Where where)
	{
		return LASTTraverser.getChild(last, loop, getLoopChildNodeType(where));
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
		return LASTTraverser.getChild(last, foreach, getForeachChildNodeType(where));
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
		return LASTTraverser.getChild(last, exHandler, getExhandlerChildNodeType(where));
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
		final Node parentNode = LASTTraverser.getNode(last, parentId);
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
			case Label:
			case FieldAccess:
				if (where == null)
					return parentNode;
			default:
				throw new RuntimeException(type + " does not contain " + where);
		}
	}
	public static Node getNode(Node.Type type, boolean isVariable, String name, LDASTNodeInfo info)
	{
		if (isVariable)
			return new Variable(LASTBuilder.nextId++, type, name, info);
		return new Node(LASTBuilder.nextId++, type, name, info);
	}
	static Node getNode(LAST last, Node.Type type, Node parent, boolean isVariable, String name, LDASTNodeInfo info)
	{
		Node.Type parentType = parent.getType();
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
				return new Node(LASTBuilder.nextId++, type, name, info);
			case Result:
				final Node firstSibling = LASTTraverser.getChild(last, parent, 0);
				final Node.Type siblingType = firstSibling.getType();
				switch(siblingType)
                {
                    case Variable:
                    case Literal:
                    case DataConstructorAccess:
                    case DataConstructor:
                    case Operation:
                        if (isVariable)
                            return new Variable(LASTBuilder.nextId++, type, name, info);
                        return new Node(LASTBuilder.nextId++, type, name, info);
                    default:
                        return new Node(LASTBuilder.nextId++, type, name, info);
                }
			case DefaultCase:

                final List<Node> children = LASTTraverser.getChildren(last, parent);
                if (children.size() == 1)
                    return new Node(LASTBuilder.nextId++, type, name, info);

                final Node lastChild = children.get(children.size() - 1);
                if (lastChild.getType() != Node.Type.DefaultCase)
                    return new Node(LASTBuilder.nextId++, type, name, info);

                final Node penultimateChild = children.get(children.size() - 2);
                return new Node(LASTBuilder.nextId++, type, name, info);

            case Expression:
                switch (parentType)
                {
                    case Parameters:
                        final Node grandParent = LASTTraverser.getParent(last, parent);
                        if (grandParent.getType() == Node.Type.CatchClause)
                            return new Node(LASTBuilder.nextId++, type, name, info);
                    case Arguments:
                    case Body:
					case Block:
					case Module:
                    case Init:
                    case Update:
                    case Try:
                    case Finally:
                        return new Node(LASTBuilder.nextId++, type, name, info);
                    default:
                        break;
                }
            default:
                if (isVariable)
                    return new Variable(LASTBuilder.nextId++, type, name, info);
                return new Node(LASTBuilder.nextId++, type, name, info);
        }
	}

	public static class ClassInfo
	{
		private Map<String, List<Node>> methods;
		private Map<String, Node> variables;
		private List<ClassInfo> childrenClasses = new LinkedList<>();
		
		public ClassInfo(ClassInfo ci)
		{
			this.methods = new HashMap<>(ci.methods);
			this.variables = new HashMap<>(ci.variables);
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
