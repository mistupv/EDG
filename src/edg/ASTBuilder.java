package edg;

import java.util.LinkedList;
import java.util.List;

import edg.edge.EdgeGenerator;
import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.traverser.EDGTraverser;

// TODO Caracteristicas de los lenguajes: single assignment, orden metodos y atributos, bloque aisla contextos
// TODO Hay que crear las siguientes expresiones: compound pattern
public class ASTBuilder
{
	public static int nextId = 0;
	public static enum Where { Parameters, Arguments, Guard, Scope, Name, Body, Condition, Then, Else, Selector, Cases, Selectable, Restrictions, Value,
		Init, Update // ADDED SERGIO NEW LOOP
		}

	// EDG
	public static EDG createEDG(LDASTNodeInfo info)
	{
		ASTBuilder.nextId = 0;

		final EDG edg = new EDG();
		final NodeInfo rootNodeInfo = new NodeInfo(ASTBuilder.nextId++, NodeInfo.Type.Root, "EDG", info);
		final Node rootNode = new Node("EDG", rootNodeInfo);

		edg.setRootNode(rootNode);

		return edg;
	}
	public static int addModule(EDG edg, String name, LDASTNodeInfo info)
	{
		final Node parent = edg.getRootNode();
		final Node module = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Module, name, "module" + "\\n" + name, info);

		return module.getData().getId();
	}

	// Add expression
	public static int addRoutine(EDG edg, int parentId, Where where, String name, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final String routineName = name == null ? "routine" : "routine" + "\\n" + name;
		final Node routine;

		if (parent.getData().getType() == NodeInfo.Type.Module)
			routine = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Routine, name, routineName, info);
		else
		{
			final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
			routine = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Routine, name, routineName, info);
			ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		}

		return routine.getData().getId();
	}
	public static int addClause(EDG edg, int functionId, LDASTNodeInfo info)
	{
		final Node parent = EDGTraverser.getNode(edg, functionId);
		final NodeInfo.Type parentType = parent.getData().getType();
		if (parentType != NodeInfo.Type.Routine)
			throw new RuntimeException("A " + parentType + " cannot contain a clause");

		final Node clause = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Clause, "clause", info);
		ASTBuilder.addNode(edg, clause, NodeInfo.Type.Parameters, "parameters", null);
		ASTBuilder.addNode(edg, clause, NodeInfo.Type.Guard, "guard", null);
		ASTBuilder.addNode(edg, clause, NodeInfo.Type.Body, "body", null);
		ASTBuilder.addNode(edg, clause, NodeInfo.Type.Result, "result", null);

		return clause.getData().getId();
	}
	public static int addVariable(EDG edg, int parentId, Where where, String name, boolean declaration, boolean definition, boolean global, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node variable = ASTBuilder.addVariableNode(edg, expression, NodeInfo.Type.Variable, name, "variable" + "\\n" + name, info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		final VariableInfo variableInfo = (VariableInfo) variable.getData();

		final VariableInfo.Context context;
		final LDASTNodeInfo parentInfo = parent.getData().getInfo();
		if(parentInfo != null && parentInfo.getConstruction().equals("unary"))
			context = VariableInfo.Context.Def_Use;
		else
			context = definition ? VariableInfo.Context.Definition : VariableInfo.Context.Use;
		
		variableInfo.setDeclaration(declaration);
		variableInfo.setContext(context);
		variableInfo.setGlobal(global);

		return variableInfo.getId();
	}
	public static int addLiteral(EDG edg, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node literal = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Literal, value, "literal" + "\\n" + value, info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return literal.getData().getId();
	}
	public static int addEquality(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node equality = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Equality, "equality", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return equality.getData().getId();
	}
	public static int addEquality(EDG edg, int parentId, Where where, String sign, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node equality = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Equality, "equality" + "\\n"+ sign, info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return equality.getData().getId();
	}
	public static int addOperation(EDG edg, int parentId, Where where, String sign, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node operation = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Operation, sign, "operation" + "\\n" + sign, info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return operation.getData().getId();
	}
	public static int addList(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node list = ASTBuilder.addNode(edg, expression, NodeInfo.Type.List, "[]", "list" + "\\n" + "[]", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return list.getData().getId();
	}
	public static int addDataConstructor(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node dataConstructor = ASTBuilder.addNode(edg, expression, NodeInfo.Type.DataConstructor, "{}", "data constructor" + "\\n" + "{}", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return dataConstructor.getData().getId();
	}
	public static int addDataConstructorAccess(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node dataConstructorAccess = ASTBuilder.addNode(edg, expression, NodeInfo.Type.DataConstructorAccess, "data constructor access", "data constructor" + "\\n" + "access", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return dataConstructorAccess.getData().getId();
	}
	public static int addBlock(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node block = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Block, "block", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return block.getData().getId();
	}
	public static int addIf(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node _if = ASTBuilder.addNode(edg, expression, NodeInfo.Type.If, "if", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		ASTBuilder.addNode(edg, _if, NodeInfo.Type.Condition, "condition", null);
		ASTBuilder.addNode(edg, _if, NodeInfo.Type.Body, "then", null);
		ASTBuilder.addNode(edg, _if, NodeInfo.Type.Body, "else", null);

		return _if.getData().getId();
	}
	public static int addSwitch(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node _switch = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Switch, "switch", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		ASTBuilder.addNode(edg, _switch, NodeInfo.Type.Selector, "selector", null);
		ASTBuilder.addNode(edg, _switch, NodeInfo.Type.Cases, "cases", null);

		return _switch.getData().getId();
	}
	public static int addCase(EDG edg, int parentId, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, Where.Cases);
		final Node _case = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Case, "case", info);
		ASTBuilder.addNode(edg, _case, NodeInfo.Type.Selectable, "selectable", null);
		ASTBuilder.addNode(edg, _case, NodeInfo.Type.Guard, "guard", null);
		ASTBuilder.addNode(edg, _case, NodeInfo.Type.Body, "body", null);

		return _case.getData().getId();
	}
	public static int addDefaultCase(EDG edg, int parentId, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, Where.Cases);
		final Node defaultCase = ASTBuilder.addNode(edg, parent, NodeInfo.Type.DefaultCase, "default", info);
		ASTBuilder.addNode(edg, defaultCase, NodeInfo.Type.Body, "body", null);

		return defaultCase.getData().getId();
	}
	public static int addBreak(EDG edg, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node breakNode = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Break, "break " + dstId, info);

		return breakNode.getData().getId();
	}
	public static int addContinue(EDG edg, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node continueNode = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Continue, "continue " + dstId, info);

		return continueNode.getData().getId();
	}
	public static int addCall(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node call = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Call, "call", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		final Node callee = ASTBuilder.addNode(edg, call, NodeInfo.Type.Callee, "callee", null);
		ASTBuilder.addNode(edg, callee, NodeInfo.Type.Scope, "scope", null);
		ASTBuilder.addNode(edg, callee, NodeInfo.Type.Name, "name", null);
		ASTBuilder.addNode(edg, callee, NodeInfo.Type.Result, "result", null);
		ASTBuilder.addNode(edg, call, NodeInfo.Type.Arguments, "arguments", null);

		return call.getData().getId();
	}
	public static int addListComprehension(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node lc = ASTBuilder.addNode(edg, expression, NodeInfo.Type.ListComprehension, "lc", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		ASTBuilder.addNode(edg, lc, NodeInfo.Type.Restrictions, "restrictions", null);
		ASTBuilder.addNode(edg, lc, NodeInfo.Type.Value, "value", null);

		return lc.getData().getId();
	}
	public static int addGenerator(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node generator = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Generator, "generator", info);

		return generator.getData().getId();
	}
	public static int addFilter(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node filter = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Filter, "filter", info);

		return filter.getData().getId();
	}
	public static int addForLoop(EDG edg, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node loop = ASTBuilder.addNode(edg, expression, NodeInfo.Type.FLoop, "loop", info);
		
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Init, "init", null);
		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Condition, "condition", null);
		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Body, "body", null);
		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Update, "update", null);
		
		return loop.getData().getId();
	}
	public static int addCondLoop(EDG edg, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node loop = ASTBuilder.addNode(edg, expression, NodeInfo.Type.CLoop, "loop", info);
		
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Condition, "condition", null);
		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Body, "body", null);
		
		return loop.getData().getId();
	}
	public static int addRepeatLoop(EDG edg, int parentId, Where where, LDASTNodeInfo info, boolean general)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node loop = ASTBuilder.addNode(edg, expression, NodeInfo.Type.RLoop, "loop", info);
		
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		// EN ORDEN DE APARICION EN EL CODIGO, NECESARIO A LA HORA DE MIRAR DEFINICIONES PREVIAS
		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Body, "body", null);
		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Condition, "condition", null);
		
// THE ORDER COND-BODY EASE THE TRAVESAL TO GENERATE CONTROL DEPENDENCIES GUARD/COND -> BODY, FINISHING THE TRAVERSAL AT THE BODY NODE.
// IT IS NOT VALID BECAUSE THE ORDER OF APPEAREANCE RUINS THE SCOPE USED IN DEF-USE FLOW DEPENDENCIES
//		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Condition, "condition", null);
//		ASTBuilder.addNode(edg, loop, NodeInfo.Type.Body, "body", null);
		
		return loop.getData().getId();
	}
	public static int addReturn(EDG edg, int parentId, Where where, int dstId, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node _return = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Return, "return " + dstId, info);

		return _return.getData().getId();
	}

	private static Node addNode(EDG edg, Node parent, NodeInfo.Type type, String name, LDASTNodeInfo info)
	{
		return ASTBuilder.addNode(edg, parent, type, name, name, info);
	}
	private static Node addNode(EDG edg, Node parent, NodeInfo.Type type, String name, String text, LDASTNodeInfo info)
	{
		return ASTBuilder.addNode(edg, parent, type, false, name, text, info);
	}
	private static Node addVariableNode(EDG edg, Node parent, NodeInfo.Type type, String name, LDASTNodeInfo info)
	{
		return ASTBuilder.addVariableNode(edg, parent, type, name, name, info);
	}
	private static Node addVariableNode(EDG edg, Node parent, NodeInfo.Type type, String name, String text, LDASTNodeInfo info)
	{
		return ASTBuilder.addNode(edg, parent, type, true, name, text, info);
	}
	private static Node addNode(EDG edg, Node parent, NodeInfo.Type type, boolean isVariable, String name, String text, LDASTNodeInfo info)
	{
		if (info != null && info.getArchive() == null)
			info.setArchive(ASTBuilder.getArchive(parent));

		final NodeInfo nodeInfo = ASTBuilder.getNodeInfo(type, isVariable, name, info);
		final EdgeInfo edgeInfo = new EdgeInfo(EdgeInfo.Type.Structural);
		final Node node = new Node(text, nodeInfo);

		edg.addNode(node);
		edg.addEdge(parent, node, 0, edgeInfo);

		return node;
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
			ancestor = EDGTraverser.getParent(ancestor);
		}

		return null;
	}

	// Typed languages
	public static int addTypeCheck(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node typeCheck = ASTBuilder.addNode(edg, expression, NodeInfo.Type.TypeCheck, "typecheck", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return typeCheck.getData().getId();
	}
	public static int addTypeTransformation(EDG edg, int parentId, Where where, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node typeTrans = ASTBuilder.addNode(edg, expression, NodeInfo.Type.TypeTransformation, "typetransform", info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);
		
		return typeTrans.getData().getId();
	}
	public static int addType(EDG edg, int parentId, Where where, String value, LDASTNodeInfo info)
	{
		final Node parent = ASTBuilder.getParentNode(edg, parentId, where);
		final Node expression = ASTBuilder.addNode(edg, parent, NodeInfo.Type.Expression, "expression", null);
		final Node type = ASTBuilder.addNode(edg, expression, NodeInfo.Type.Type, value, "type" + "\\n" + value, info);
		ASTBuilder.addNode(edg, expression, NodeInfo.Type.Result, "result", null);

		return type.getData().getId();
	}

	
	
	// Complete EDG
	public static void completeEDG(EDG edg)
	{
		final Node root = edg.getRootNode();
		Node currentNode = root;
		int childIndexToVisit = 0;
		final LinkedList<Integer> bifurcations = new LinkedList<Integer>();

		while (true)
		{
			final List<Node> children = EDGTraverser.getChildren(currentNode);

			if (childIndexToVisit == children.size())
			{ // It is a leaf or all its children have been already processed
				ASTBuilder.completeNode(currentNode);
				if (currentNode == root)
					break;
				// Go back to parent
				currentNode = EDGTraverser.getParent(currentNode);
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
				ASTBuilder.completeExpression(node);
				break;
			default:
				break;
		}
	}
	private static void completeExpression(Node node)
	{
		final Node child = EDGTraverser.getChild(node, 0);
		final NodeInfo.Type childType = child.getData().getType();

		switch (childType)
		{
			default:
				break;
		}
	}

	// Child node
	private static Node getClauseChildNode(Node clause, Where where)
	{
		switch (where)
		{
			case Parameters:
				return EDGTraverser.getChild(clause, 0);
			case Guard:
				return EDGTraverser.getChild(clause, 1);
			case Body:
				return EDGTraverser.getChild(clause, 2);
			default:
				throw new RuntimeException("A clause cannot contain " + where);
		}
	}
	private static Node getIfChildNode(Node _if, Where where)
	{
		switch (where)
		{
			case Condition:
				return EDGTraverser.getChild(_if, 0);
			case Then:
				return EDGTraverser.getChild(_if, 1);
			case Else:
				return EDGTraverser.getChild(_if, 2);
			default:
				throw new RuntimeException("An if cannot contain " + where);
		}
	}
	private static Node getSwitchChildNode(Node _switch, Where where)
	{
		switch (where)
		{
			case Selector:
				return EDGTraverser.getChild(_switch, 0);
			case Cases:
				return EDGTraverser.getChild(_switch, 1);
			default:
				throw new RuntimeException("A switch cannot contain " + where);
		}
	}
	private static Node getCaseChildNode(Node _case, Where where)
	{
		switch (where)
		{
			case Selectable:
				return EDGTraverser.getChild(_case, 0);
			case Guard:
				return EDGTraverser.getChild(_case, 1);
			case Body:
				return EDGTraverser.getChild(_case, 2);
			default:
				throw new RuntimeException("A case cannot contain " + where);
		}
	}
	private static Node getDefaultCaseChildNode(Node defaultCase, Where where)
	{
		switch (where)
		{
			case Body:
				return EDGTraverser.getChild(defaultCase, 0);
			default:
				throw new RuntimeException("A default case cannot contain " + where);
		}
	}
	private static Node getCallChildNode(Node call, Where where)
	{
		switch (where)
		{
			case Scope:
				final Node callee = EDGTraverser.getChild(call, 0);
				return EDGTraverser.getChild(callee, 0);
			case Name:
				final Node callee0 = EDGTraverser.getChild(call, 0);
				return EDGTraverser.getChild(callee0, 1);
			case Arguments:
				return EDGTraverser.getChild(call, 1);
			default:
				throw new RuntimeException("A call cannot contain " + where);
		}
	}
	private static Node getListComprehensionChildNode(Node listComprehension, Where where)
	{
		switch (where)
		{
			case Restrictions:
				return EDGTraverser.getChild(listComprehension, 0);
			case Value:
				return EDGTraverser.getChild(listComprehension, 1);
			default:
				throw new RuntimeException("A list comprehension cannot contain " + where);
		}
	}
	private static Node getLoopChildNode(Node loop, Where where)
	{
		switch (where)
		{
			case Init:
				return EDGTraverser.getChild(loop, NodeInfo.Type.Init);
			case Condition:
				return EDGTraverser.getChild(loop, NodeInfo.Type.Condition);
			case Body:
				return EDGTraverser.getChild(loop, NodeInfo.Type.Body);
			case Update:
				return EDGTraverser.getChild(loop, NodeInfo.Type.Update);
			default:
				throw new RuntimeException("A loop cannot contain " + where);
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
			case FLoop:
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
	private static Node getParentNode(EDG edg, int parentId, Where where)
	{
		final Node parentNode = EDGTraverser.getNode(edg, parentId);
		final NodeInfo.Type type = parentNode.getData().getType();

		switch (type)
		{
			case Clause:
				return ASTBuilder.getClauseChildNode(parentNode, where);
			case Call:
				return ASTBuilder.getCallChildNode(parentNode, where);
			case If:
				return ASTBuilder.getIfChildNode(parentNode, where);
			case Switch:
				return ASTBuilder.getSwitchChildNode(parentNode, where);
			case Case:
				return ASTBuilder.getCaseChildNode(parentNode, where);
			case DefaultCase:
				return ASTBuilder.getDefaultCaseChildNode(parentNode, where);
			case ListComprehension:
				return ASTBuilder.getListComprehensionChildNode(parentNode, where);
			case FLoop:
			case CLoop:
			case RLoop:
				return ASTBuilder.getLoopChildNode(parentNode, where);
//ADDED
//case ForLoop:
//	return ASTBuilder.getLoopChildNode(parentNode, where);
//
				
			case TypeCheck:
			case TypeTransformation:
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
				if (where == null)
					return parentNode;
			default:
				throw new RuntimeException(type + " does not contain " + where);
		}
	}
	private static NodeInfo getNodeInfo(NodeInfo.Type type, boolean isVariable, String name, LDASTNodeInfo info)
	{
		if (isVariable)
			return new VariableInfo(ASTBuilder.nextId++, type, name, info);
		return new NodeInfo(ASTBuilder.nextId++, type, name, info);
	}

	// Dependencies
	public static void generateDependencies(EDG edg)
	{
		EdgeGenerator.generateEdges(edg);
	}
}