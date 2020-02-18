package eknife.java;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ast.*;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.comments.BlockComment;
import com.github.javaparser.ast.comments.JavadocComment;
import com.github.javaparser.ast.comments.LineComment;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.modules.*;
import com.github.javaparser.ast.stmt.*;
import com.github.javaparser.ast.type.*;
import com.github.javaparser.ast.visitor.VoidVisitorAdapter;
import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.NodeInfo.Type;

import java.io.File;
import java.util.Stack;

public class EDGFactory
{

	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static EDG createEDG(String programPath)
	{
		return EDGFactory.createEDG(programPath, true, true);
	}
	public static EDG createEDG(String programPath, boolean createDependencies)
	{
		return EDGFactory.createEDG(programPath, createDependencies, true);
	}
	public static EDG createEDG(String programPath, boolean createDependencies, boolean constraintsActivated)
	{
		try
		{
			final CompilationUnit cu = JavaParser.parse(new File(programPath));
			final EDGFactory edgFactory = new EDGFactory();
			final EDG edg = edgFactory.generate(cu);

// TODO Descomentar
//			if (createDependencies)
//				new DependenceGenerator().generateEdges(edg, constraintsActivated);

			return edg;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return null;
		}
	}

	/********************************************************************************************************************************/
	/************************************************************ Util ************************************************************/
	/********************************************************************************************************************************/
	private EDGFactory()
	{
		
	}

	private EDG generate(CompilationUnit cu)
	{
		final EDG edg = new EDG();
		final Util util = new Util(edg);
		final JavaVisitor visitor = new JavaVisitor();

		visitor.visit(cu, util);

		return edg;
	}

	private static class JavaVisitor extends VoidVisitorAdapter<Util>
	{
		public void visit(NodeList n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(AnnotationDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(AnnotationMemberDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ArrayAccessExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ArrayCreationExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ArrayCreationLevel n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ArrayInitializerExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ArrayType n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(AssertStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(AssignExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(BinaryExpr n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int line = n.getBegin().get().line;
			final String text = n.getOperator().asString();
			final String name = "(op)\n" + text;
			final NodeInfo nodeInfo = new NodeInfo(Type.Operation, line, text);
			final Node node = new Node(name, nodeInfo);
			final EdgeInfo edgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			edg.addNode(node);
			edg.addEdge(parentNode, node, 0, edgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Operation, node));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(BlockComment n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(BlockStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(BooleanLiteralExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(BreakStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(CastExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(CatchClause n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(CharLiteralExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ClassExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ClassOrInterfaceDeclaration n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int line = n.getBegin().get().line;
			final String text = n.getNameAsString();
			final String name = "(class)\n" + text;
			final NodeInfo nodeInfo = new NodeInfo(Type.Clause, line, text);
			final Node node = new Node(name, nodeInfo);
			final EdgeInfo edgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			edg.addNode(node);
			edg.addEdge(parentNode, node, 0, edgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Clause, node));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(ClassOrInterfaceType n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(CompilationUnit n, Util arg)
		{
			final EDG edg = arg.getEDG();

			// Create EDG node
			final int line = n.getBegin().get().line;
			final String name = "Root";
			final NodeInfo nodeInfo = new NodeInfo(Type.Root, line, name);
			final Node node = new Node(name, nodeInfo);
			edg.setRootNode(node);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Root, node));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(ConditionalExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ConstructorDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ContinueStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(DoStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(DoubleLiteralExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(EmptyMemberDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(EmptyStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(EnclosedExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(EnumConstantDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(EnumDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ExplicitConstructorInvocationStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ExpressionStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(FieldAccessExpr n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int varLine = n.getBegin().get().line;
			final String varText = n.getScope().get().toString() + "." + n.getNameAsString();
			final String varName = "(field)\n" + varText;
			final NodeInfo varNodeInfo = new NodeInfo(Type.Variable, varLine, varText);
			final EdgeInfo varEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node varNode = new Node(varName, varNodeInfo);
			edg.addNode(varNode);
			edg.addEdge(parentNode, varNode, 0, varEdgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Variable, varNode));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(FieldDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ForStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ForeachStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(IfStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ImportDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(InitializerDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(InstanceOfExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(IntegerLiteralExpr n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int integerLine = n.getBegin().get().line;
			final String integerText = n.getValue();
			final String integerName = "(int)\n" + integerText;
			final NodeInfo integerNodeInfo = new NodeInfo(Type.Integer, integerLine, integerText);
			final EdgeInfo integerEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node integerNode = new Node(integerName, integerNodeInfo);
			edg.addNode(integerNode);
			edg.addEdge(parentNode, integerNode, 0, integerEdgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Integer, integerNode));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(IntersectionType n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(JavadocComment n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(LabeledStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(LambdaExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(LineComment n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(LocalClassDeclarationStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(LongLiteralExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(MarkerAnnotationExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(MemberValuePair n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(MethodCallExpr n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
				// Call
			final int callLine = n.getBegin().get().line;
			final String callName = "(call)";
			final NodeInfo callNodeInfo = new NodeInfo(Type.FunctionCall, callLine, callName);
			final EdgeInfo callEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node callNode = new Node(callName, callNodeInfo);
			edg.addNode(callNode);
			edg.addEdge(parentNode, callNode, 0, callEdgeInfo);
				// Caller
			final int callerLine = n.getBegin().get().line;
			final String callerName = "(caller)";
			final NodeInfo callerNodeInfo = new NodeInfo(Type.Atom, callerLine, callerName);
			final EdgeInfo callerEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node callerNode = new Node(callerName, callerNodeInfo);
			edg.addNode(callerNode);
			edg.addEdge(callNode, callerNode, 0, callerEdgeInfo);
				// Caller
			final int returnLine = n.getBegin().get().line;
			final String returnName = "(return)";
			final NodeInfo returnNodeInfo = new NodeInfo(Type.Return, returnLine, returnName);
			final EdgeInfo returnEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node returnNode = new Node(returnName, returnNodeInfo);
			edg.addNode(returnNode);
			edg.addEdge(callNode, returnNode, 0, returnEdgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.FunctionCall, callNode, callerNode, returnNode));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(MethodDeclaration n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
				// Function
			final int functionLine = n.getBegin().get().line;
			final String functionText = n.getNameAsString();
			final int functionArity = n.getParameters().size();
			final String functionName = "(function)\n" + functionText + "/" + functionArity;
			final NodeInfo functionNodeInfo = new NodeInfo(Type.Function, functionLine, functionText, functionArity);
			final EdgeInfo functionEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node functionNode = new Node(functionName, functionNodeInfo);
			edg.addNode(functionNode);
			edg.addEdge(parentNode, functionNode, 0, functionEdgeInfo);
				// Clause
			final int clauseLine = n.getBegin().get().line;
			final String clauseName = "(clause)";
			final NodeInfo clauseNodeInfo = new NodeInfo(Type.Clause, clauseLine, clauseName);
			final EdgeInfo clauseEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node clauseNode = new Node(clauseName, clauseNodeInfo);
			edg.addNode(clauseNode);
			edg.addEdge(functionNode, clauseNode, 0, clauseEdgeInfo);
				// Guard
			final int guardLine = n.getBegin().get().line;
			final String guardName = "(guard)";
			final NodeInfo guardNodeInfo = new NodeInfo(Type.Guard, guardLine, guardName);
			final EdgeInfo guardEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node guardNode = new Node(guardName, guardNodeInfo);
			edg.addNode(guardNode);
			edg.addEdge(clauseNode, guardNode, 0, guardEdgeInfo);
				// Body
			final int bodyLine = n.getBegin().get().line;
			final String bodyText = "(body)";
			final NodeInfo bodyNodeInfo = new NodeInfo(Type.Body, bodyLine, bodyText);
			final EdgeInfo bodyEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node bodyNode = new Node(bodyText, bodyNodeInfo);
			edg.addNode(bodyNode);
			edg.addEdge(guardNode, bodyNode, 0, bodyEdgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Function, functionNode, clauseNode, guardNode, bodyNode));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(MethodReferenceExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(NameExpr n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int nameLine = n.getBegin().get().line;
			final String nameText = n.getNameAsString();
			final String nameName = "(name)\n" + nameText;
			final NodeInfo nameNodeInfo = new NodeInfo(Type.Variable, nameLine, nameText);
			final EdgeInfo nameEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node nameNode = new Node(nameName, nameNodeInfo);
			edg.addNode(nameNode);
			edg.addEdge(parentNode, nameNode, 0, nameEdgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Variable, nameNode));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(Name n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(NormalAnnotationExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(NullLiteralExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ObjectCreationExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(PackageDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(Parameter n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int line = n.getBegin().get().line;
			final String text = n.getNameAsString();
			final String name = "(param)\n" + text;
			final NodeInfo nodeInfo = new NodeInfo(Type.Variable, line, text);
			final EdgeInfo edgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node node = new Node(name, nodeInfo);
			edg.addNode(node);
			edg.addEdge(parentNode, node, 0, edgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Variable, node));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(PrimitiveType n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ReturnStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(SimpleName n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int nameLine = n.getBegin().get().line;
			final String nameText = n.getId();
			final String nameName = "(name)\n" + nameText;
			final NodeInfo nameNodeInfo = new NodeInfo(Type.Variable, nameLine, nameText);
			final EdgeInfo nameEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node nameNode = new Node(nameName, nameNodeInfo);
			edg.addNode(nameNode);
			edg.addEdge(parentNode, nameNode, 0, nameEdgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.Variable, nameNode));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(SingleMemberAnnotationExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(StringLiteralExpr n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int stringLine = n.getBegin().get().line;
			final String stringText = n.getValue();
			final String stringName = "(string)\n" + stringText;
			final NodeInfo stringNodeInfo = new NodeInfo(Type.String, stringLine, stringText);
			final EdgeInfo stringEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node stringNode = new Node(stringName, stringNodeInfo);
			edg.addNode(stringNode);
			edg.addEdge(parentNode, stringNode, 0, stringEdgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.String, stringNode));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(SuperExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(SwitchEntryStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(SwitchStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(SynchronizedStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ThisExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ThrowStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(TryStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(TypeExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(TypeParameter n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(UnaryExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(UnionType n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(UnknownType n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(VariableDeclarationExpr n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(VariableDeclarator n, Util arg)
		{
			final EDG edg = arg.getEDG();
			final Util.Structure parentStructure = arg.peekStructure();
			final Node parentNode = this.getParentNode(n, parentStructure);
			if (parentNode == null)
				return;

			// Create EDG node
			final int variableDeclaratorLine = n.getBegin().get().line;
			final String variableDeclaratorText = "(pm)";
			final NodeInfo variableDeclaratorNodeInfo = new NodeInfo(Type.PatternMatching, variableDeclaratorLine, variableDeclaratorText);
			final EdgeInfo variableDeclaratorEdgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);
			final Node variableDeclaratorNode = new Node(variableDeclaratorText, variableDeclaratorNodeInfo);
			edg.addNode(variableDeclaratorNode);
			edg.addEdge(parentNode, variableDeclaratorNode, 0, variableDeclaratorEdgeInfo);

			// Keep visiting the code
			arg.pushStructure(arg.new Structure(Type.PatternMatching, variableDeclaratorNode));
			super.visit(n, arg);
			arg.popStructure();
		}
		public void visit(VoidType n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(WhileStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(WildcardType n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ModuleDeclaration n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ModuleRequiresStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ModuleExportsStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ModuleProvidesStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ModuleUsesStmt n, Util arg)
		{
			super.visit(n, arg);
		}
		public void visit(ModuleOpensStmt n, Util arg)
		{
			super.visit(n, arg);
		}

		private Node getParentNode(com.github.javaparser.ast.Node n, Util.Structure structure)
		{
			final com.github.javaparser.ast.Node parentNode = n.getParentNode().get();
			final Type type = structure.getType();
			final Node[] nodes = structure.getNodes();

			switch (type)
			{
				case Root:
					return nodes[0];
				case Clause: // Clase
					if (n instanceof SimpleName)
						return null;
					return nodes[0];
				case Function:
					if (parentNode instanceof MethodDeclaration && n instanceof SimpleName)
						return null;
					if (n instanceof Parameter)
						return nodes[1];
					return nodes[3];
				case Variable:
					return null;
				case PatternMatching:
					if (parentNode instanceof BinaryExpr)
						return nodes[0];
					if (parentNode instanceof VariableDeclarator)
						return nodes[0];
					return null;
				case Operation:
					return nodes[0];
				case FunctionCall:
 					if (parentNode instanceof MethodCallExpr && ((MethodCallExpr) parentNode).getArguments().contains(n))
						return nodes[0];
					if (parentNode instanceof MethodCallExpr && n instanceof SimpleName)
						return nodes[1];
					if (parentNode instanceof MethodCallExpr && n instanceof Expression)
						return nodes[1];
					return null;
				default:
					throw new RuntimeException("Type not contemplated: " + type);
			}
		}
	}
	private static class Util
	{
		private final EDG edg;
		private final Stack<Structure> structures = new Stack<Structure>();

		public Util(EDG edg)
		{
			this.edg = edg;
		}

		public EDG getEDG()
		{
			return this.edg;
		}

		public void pushStructure(Structure structure)
		{
			this.structures.push(structure);
		}
		public Structure popStructure()
		{
			return this.structures.pop();
		}
		public Structure peekStructure()
		{
			return this.structures.peek();
		}

		private class Structure
		{
			private final Type type;
			private final Node[] nodes;

			public Structure(Type type, Node... nodes)
			{
				this.type = type;
				this.nodes = nodes;
			}

			public Type getType()
			{
				return this.type;
			}
			public Node[] getNodes()
			{
				return this.nodes;
			}
		}
	}
}