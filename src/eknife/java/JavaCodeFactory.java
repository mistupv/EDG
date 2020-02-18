package eknife.java;

import java.io.File;
import java.util.EnumSet;
import java.util.LinkedList;
import java.util.List;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Modifier;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.CallableDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.ArrayAccessExpr;
import com.github.javaparser.ast.expr.ArrayInitializerExpr;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.BinaryExpr;
import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.CastExpr;
import com.github.javaparser.ast.expr.CharLiteralExpr;
import com.github.javaparser.ast.expr.ConditionalExpr;
import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.InstanceOfExpr;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.LongLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.NullLiteralExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.SimpleName;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.expr.UnaryExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.nodeTypes.NodeWithAnnotations;
import com.github.javaparser.ast.nodeTypes.NodeWithBlockStmt;
import com.github.javaparser.ast.nodeTypes.NodeWithOptionalBlockStmt;
import com.github.javaparser.ast.nodeTypes.NodeWithParameters;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.DoStmt;
import com.github.javaparser.ast.stmt.EmptyStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.IfStmt;
import com.github.javaparser.ast.stmt.ReturnStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntryStmt;
import com.github.javaparser.ast.stmt.SwitchStmt;
import com.github.javaparser.ast.stmt.TryStmt;
import com.github.javaparser.ast.stmt.WhileStmt;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.type.PrimitiveType;
import com.github.javaparser.ast.type.PrimitiveType.Primitive;
import com.github.javaparser.ast.type.Type;
import com.github.javaparser.ast.type.VoidType;
import com.github.javaparser.printer.Printable;

import edg.LDASTNodeInfo;
import edg.graph.EDG;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.traverser.EDGTraverser;
import misc.Misc;

import eknife.java.JavaEDGFactory.*;

public class JavaCodeFactory
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static void createJavaFile(File outputFile, EDG edg)
	{
		JavaCodeFactory.createJavaFile(outputFile, edg, null);
	}
	public static void createJavaFile(File outputFile, EDG edg, List<Node> slice)
	{
		final JavaCodeFactory javaFactory = new JavaCodeFactory(edg, slice);
		final CompilationUnit cu = javaFactory.generate();
		final String text = cu.toString();

// SHOW CODE IN TERMINAL
System.out.println("\n"+text);

		Misc.write(outputFile, text, false);
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private final EDG edg;
	private final List<Node> slice;
	private boolean funundef = false;

	private JavaCodeFactory(EDG edg, List<Node> slice)
	{
		this.edg = edg;
		this.slice = slice;
	}

	private CompilationUnit generate()
	{
		final CompilationUnit cu = new CompilationUnit();
		final Node root = this.edg.getRootNode();

		// Modules
		final List<Node> modules = EDGTraverser.getChildren(root);
		for (Node module : modules)
		{
			if (this.slice != null && !this.slice.contains(module))
				continue;

			this.parseModule(cu, module);
		}

		return cu;
	}

	// Structure
	private void parseModule(CompilationUnit cu, Node module)
	{
		final String moduleName = module.getData().getName();
		final ClassOrInterfaceDeclaration clazz = cu.addClass(moduleName);
		
		NodeList<ClassOrInterfaceType> extendedTypes = (NodeList<ClassOrInterfaceType>) module.getData().getInfo().getInfo()[0];
		NodeList<ClassOrInterfaceType> implementedTypes = (NodeList<ClassOrInterfaceType>) module.getData().getInfo().getInfo()[1];
		
		clazz.setExtendedTypes(extendedTypes);
		clazz.setImplementedTypes(implementedTypes);
		
		this.parseMembers(clazz, module);
	}
	private void parseMembers(ClassOrInterfaceDeclaration clazz, Node module)
	{
		final List<Node> members = EDGTraverser.getChildren(module);

		this.funundef = false;
		for (Node member : members)
		{
			if (this.slice != null && !this.slice.contains(member))
				continue;

			// TODO No pasar el clazz en las llamadas
			if (member.getData().getType() != NodeInfo.Type.Routine)
				this.parseGlobalVariable(clazz, member);
			else
				this.parseRoutine(clazz, member);
		}

		if (this.funundef)
			clazz.addMember(this.createFunundef());
	}
	private void parseGlobalVariable(ClassOrInterfaceDeclaration clazz, Node globalVariable)
	{
		final NodeList<VariableDeclarator> variableDeclarators = new NodeList<VariableDeclarator>();
		final NodeInfo.Type nodeType = globalVariable.getData().getType();
		final Node expression = nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(globalVariable, 0) : globalVariable;
		final VariableDeclarator variableDeclarator = this.getVariableDeclarator(expression);
		final Node variable = (expression.getData().getType() == NodeInfo.Type.Equality) ? 
				EDGTraverser.getChild(EDGTraverser.getChild(expression,0), 0) : expression;
		final LDASTNodeInfo ldNodeInfo = variable.getData().getInfo();
		@SuppressWarnings("unchecked")
		final EnumSet<Modifier> modifiers = (EnumSet<Modifier>) ldNodeInfo.getInfo()[0];

		variableDeclarators.add(variableDeclarator);
		clazz.addMember(new FieldDeclaration(modifiers, variableDeclarators));
	}
	private void parseRoutine(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final Node clause = EDGTraverser.getChild(routine, 0);
		final Node parametersNode = EDGTraverser.getChild(clause, 0);
		final Node body = EDGTraverser.getChild(clause, 2);

		// Callable declaration
		final String name = routine.getData().getName();
		final CallableDeclaration<?> callableDeclaration = name.equals("<constructor>") ? this.parseConstructor(clazz, routine) : this.parseMethod(clazz, routine);

		// Parameters
		final NodeWithParameters<?> nodeWithParameters = (NodeWithParameters<?>) callableDeclaration;
		final List<Node> parametersChildren = EDGTraverser.getChildren(parametersNode);
		for (Node parameter : parametersChildren)
			nodeWithParameters.addParameter(this.parseParameter(parameter));

		// Statements
		final BlockStmt blockStmt = callableDeclaration instanceof NodeWithBlockStmt ? ((NodeWithBlockStmt<?>) callableDeclaration).getBody() : ((NodeWithOptionalBlockStmt<?>) callableDeclaration).getBody().get();
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final List<Statement> statements = this.parseStatements(bodyChildren, false);
		for (Statement statement : statements)
			blockStmt.addStatement(statement);
	}
	private CallableDeclaration<?> parseMethod(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final NodeInfo nodeInfo = routine.getData();
		final LDASTNodeInfo ldNodeInfo = nodeInfo.getInfo();
		final String name = nodeInfo.getName();
		@SuppressWarnings({ "unchecked", "rawtypes" })
		final Modifier[] modifiers = (Modifier[]) ((EnumSet) ldNodeInfo.getInfo()[0]).toArray(new Modifier[0]);
		final Type type = (Type) ldNodeInfo.getInfo()[1];
		final MethodDeclaration methodDeclaration = clazz.addMethod(name, modifiers);

		methodDeclaration.setType(type);

		return methodDeclaration;
	}
	private CallableDeclaration<?> parseConstructor(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final LDASTNodeInfo ldNodeInfo = routine.getData().getInfo();
		@SuppressWarnings({ "unchecked", "rawtypes" })
		final Modifier[] modifiers = (Modifier[]) ((EnumSet) ldNodeInfo.getInfo()[0]).toArray(new Modifier[0]);
		final ConstructorDeclaration constructorDeclaration = clazz.addConstructor(modifiers);

		return constructorDeclaration;
	}
	private Parameter parseParameter(Node parameter)
	{
		final Node variable = EDGTraverser.getChild(parameter, 0);
		final NodeInfo nodeInfo = variable.getData();
		final LDASTNodeInfo ldNodeInfo = nodeInfo.getInfo();
		final Type type = (Type) ldNodeInfo.getInfo()[0];
		final String name = this.slice != null && !this.slice.contains(variable) ? "fresh" : nodeInfo.getName();

		return new Parameter(type, name);
	}

	// Statement
	private List<Statement> parseStatements(List<Node> nodes, boolean transformUnused)
	{
		final List<Statement> statementsList = new LinkedList<Statement>();

		for (Node node : nodes)
		{
			if (!transformUnused && this.slice != null && !this.slice.contains(node))
				continue;

			statementsList.add(this.parseStatement(node));
		}
		if (!nodes.isEmpty() && statementsList.isEmpty())
			statementsList.add(new EmptyStmt());

		return statementsList;
	}
	private Statement parseStatement(Node node)
	{
		if (this.slice != null && !this.slice.contains(node))
			return new EmptyStmt();

		final NodeInfo.Type nodeType = node.getData().getType();
		final Node statement = nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(node, 0) : node;
		final NodeInfo.Type statementType = statement.getData().getType();

		switch (statementType)
		{
			case If:
				return this.parseIf(statement);
			case Switch:
				return this.parseSwitch(statement);
			case Case:
				return this.parseCase(statement);
			case CLoop:
				return this.parseCLoop(statement);
			case RLoop:
				return this.parseRLoop(statement);
			case FLoop:
				return this.parseFLoop(statement);
			case Return:
				return this.parseReturn(statement);
			case ExHandler:
				return this.parseExHandler(statement);
			default:
				return new ExpressionStmt(this.parseExpression(statement));
		}
	}
	private Statement parseIf(Node _if)
	{
		// Condition
		final Node conditionNode = EDGTraverser.getChild(_if, 0);
		final Node condition = EDGTraverser.getChild(conditionNode, 0);
		final Expression conditionExpression = this.parseExpression(condition);

		// Then
		final Node then = EDGTraverser.getChild(_if, 1);
		final List<Node> thenChildren = EDGTraverser.getChildren(then);
		final List<Statement> thenStatements = this.parseStatements(thenChildren, false);
		final BlockStmt thenBlock = new BlockStmt();
		for (Statement thenStatement : thenStatements)
			thenBlock.addStatement(thenStatement);

		// Else
		final Node _else = EDGTraverser.getChild(_if, 2);
		final List<Node> elseChildren = EDGTraverser.getChildren(_else);
		final List<Statement> elseStatements = this.parseStatements(elseChildren, false);

		if (elseStatements.isEmpty()) // Avoid generating an empty Else block
			return	new IfStmt(conditionExpression, thenBlock, null);
		
		final BlockStmt elseBlock = new BlockStmt();
		for (Statement elseStatement : elseStatements)
			elseBlock.addStatement(elseStatement);
		
		return new IfStmt(conditionExpression, thenBlock, elseBlock);
	}
	private Statement parseSwitch(Node _switch)
	{
		// Selector
		final Node selectors = EDGTraverser.getChild(_switch, 0);
		final Node selectorNode = EDGTraverser.getChild(selectors, 0);
		final Expression selector = this.parseExpression(selectorNode);

		// Cases
		final Node cases = EDGTraverser.getChild(_switch, 1);
		final List<Node> casesChildren = EDGTraverser.getChildren(cases);
		final NodeList<SwitchEntryStmt> entries = new NodeList<SwitchEntryStmt>();
		final List<Statement> statements = this.parseStatements(casesChildren, false);
		for (Statement statement : statements)
			entries.add((SwitchEntryStmt) statement);

		return new SwitchStmt(selector, entries);
	}
	private Statement parseCase(Node _case)
	{
		// Label
		final Node selectables = EDGTraverser.getChild(_case, 0);
		final List<Node> selectablesChildren = EDGTraverser.getChildren(selectables);
		final Node selectableNode = selectablesChildren.get(0);
		final Expression label = this.parseExpression(selectableNode);

		// Statements
		final Node body = EDGTraverser.getChild(_case, 2);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final NodeList<Statement> statements = new NodeList<Statement>();
		final List<Statement> statements0 = this.parseStatements(bodyChildren, false);
		for (Statement statement : statements0)
			statements.add((Statement) statement);

		return new SwitchEntryStmt(label, statements);
	}
	
	// Different Loop Structures
	private Statement parseCLoop(Node loop)
	{
		// Condition
		final Node condition = EDGTraverser.getChild(loop, 0);
		final List<Node> conditionChildren = EDGTraverser.getChildren(condition);
		final Node conditionNode = conditionChildren.get(0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		// Body
		final Node body = EDGTraverser.getChild(loop, 1);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);

		return new WhileStmt(conditionExpression, bodyBlock);
	}
	private Statement parseRLoop(Node loop)
	{
		// Body
		final Node body = EDGTraverser.getChild(loop, 0);
//final Node body = EDGTraverser.getChild(loop, 1);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);
		
		// Condition
		final Node condition = EDGTraverser.getChild(loop, 1);
//final Node condition = EDGTraverser.getChild(loop, 0);
		final List<Node> conditionChildren = EDGTraverser.getChildren(condition);
		final Node conditionNode = conditionChildren.get(0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		return new DoStmt(bodyBlock, conditionExpression);
	}
	private Statement parseFLoop(Node loop)
	{
		// Initialization
		final Node initialization = EDGTraverser.getChild(loop, 0);
		final List<Node> initializationChildren = EDGTraverser.getChildren(initialization);
		final NodeList<Expression> initBlock = new NodeList<Expression>();
		initBlock.addAll(this.parseExpressions(initializationChildren, false));
		
		// Condition
		final Node condition = EDGTraverser.getChild(loop, 1);
		final List<Node> conditionChildren = EDGTraverser.getChildren(condition);
		final Node conditionNode = conditionChildren.get(0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		// Update
		final Node update = EDGTraverser.getChild(loop, 3);
		final List<Node> updateChildren = EDGTraverser.getChildren(update);
		final NodeList<Expression> updateBlock = new NodeList<Expression>();
		updateBlock.addAll(this.parseExpressions(updateChildren, false));
		
		// Body
		final Node body = EDGTraverser.getChild(loop, 2);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);

		return new ForStmt(initBlock, conditionExpression, updateBlock, bodyBlock);
	}
	
	// Exception Statements
	private Statement parseExHandler(Node exHandler)
	{
		// Try
		final Node tryNode = EDGTraverser.getChild(exHandler, 0);
		final BlockStmt tryBlock0 = this.parseBlockStatements(tryNode);
		final BlockStmt tryBlock = tryBlock0 == null ? new BlockStmt() : tryBlock0;

		// Catch
		final Node catchNode = EDGTraverser.getChild(exHandler, 1);
		final NodeList<CatchClause> catchClauses = (NodeList<CatchClause>) this.parseCatch(catchNode);

		// Finally
		final Node finallyNode = EDGTraverser.getChild(exHandler, 2);
		final BlockStmt finallyBlock = this.parseBlockStatements(finallyNode);

		return new TryStmt(tryBlock, catchClauses, finallyBlock);
	}
	private List<CatchClause> parseCatch(Node _catch)
	{
		final List<Node> clauses = EDGTraverser.getChildren(_catch);
		final List<CatchClause> catchClauses = new NodeList<CatchClause>();
		for (Node clause : clauses)
		{
			if (this.slice != null && !this.slice.contains(clause))
				continue;
			
			final Node parameters = EDGTraverser.getChild(clause, 0);
			final Node parameter = EDGTraverser.getChild(parameters,0);
			final Parameter parameter0 = this.parseParameter(parameter);
			
			final Node body = EDGTraverser.getChild(clause, 2);
			final BlockStmt bodyBlock = this.parseBlockStatements(body);
			
			catchClauses.add(new CatchClause(parameter0, bodyBlock));
		}
		return catchClauses;
	}
	
	private Statement parseReturn(Node _return)
	{
		final List<Node> returnChildren = EDGTraverser.getChildren(_return);
		if (returnChildren.isEmpty())
			return new ReturnStmt();

		final Node returnChild = returnChildren.get(0);
		final Expression returnExpression = this.parseExpression(returnChild);

		return new ReturnStmt(returnExpression);
	}
	private BlockStmt parseBlockStatements(Node blockRoot)
	{
		if (this.slice != null && !this.slice.contains(blockRoot))
			return null;
		
		final List<Node> bodyChildren = EDGTraverser.getChildren(blockRoot);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);
		return bodyBlock;
	}
	
	// Expressions
	private List<Expression> parseExpressions(List<Node> nodes, boolean transformUnused)
	{
		final List<Expression> expressionsList = new LinkedList<Expression>();

		for (Node node : nodes)
		{
			if (!transformUnused && this.slice != null && !this.slice.contains(node))
				continue;

			expressionsList.add(this.parseExpression(node));
		}
		if (!nodes.isEmpty() && expressionsList.isEmpty())
			expressionsList.add(new NullLiteralExpr());

		return expressionsList;
	}
	private Expression parseExpression(Node node)
	{
		if (this.slice != null && !this.slice.contains(node))
			return new NullLiteralExpr();

		final NodeInfo.Type nodeType = node.getData().getType();
		final Node expression = nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(node, 0) : node;
		final NodeInfo.Type expressionType = expression.getData().getType();

		switch (expressionType)
		{
			case Equality:
				return this.parseEquality(expression);
			case Call:
				return this.parseCall(expression);
			case DataConstructor:
				return this.parseDataConstructor(expression);
			case Operation:
				return this.parseOperation(expression);
			case DataConstructorAccess:
				return this.parseDataConstructorAccess(expression);
			case If:
				return this.parseTernary(expression);
			case Variable:
				return this.parseVariable(expression);
			case Literal:
				return this.parseLiteral(expression);
			// ADDED
			case TypeCheck:
				return this.parseInstanceOf(expression);
			case TypeTransformation:
				return this.parseCastExpr(expression);
			default:
				throw new RuntimeException("Expression type not contemplated: " + expressionType);
		}
	}
	private Expression parseEquality(Node equality)
	{
		final Node targetExpression = EDGTraverser.getChild(equality, 0);
		final Node target = EDGTraverser.getChild(targetExpression, 0);
		final LDASTNodeInfo ldNodeInfo = target.getData().getInfo();
		final Object[] info = ldNodeInfo.getInfo();
		
		if(target.getData().getType() == NodeInfo.Type.Variable)
		{
			VariableInfo vi = (VariableInfo) target.getData();
			if (vi.isDeclaration())
				return this.parseDeclaration(equality);
			return this.parseDefinition(equality);
		}
		else if (info == null || info.length == 0)
			return this.parseDefinition(equality);
		return this.parseDeclaration(equality);
	}
	private Expression parseDeclaration(Node declaration)
	{
		final NodeList<VariableDeclarator> variableDeclarators = new NodeList<VariableDeclarator>();
		final VariableDeclarator variableDeclarator = this.getVariableDeclarator(declaration);
		final Node variableExpression = (declaration.getData().getType() == NodeInfo.Type.Equality) ? EDGTraverser.getChild(declaration, 0) : declaration;
		final Node variable = (variableExpression.getData().getType() == NodeInfo.Type.Variable) ? variableExpression : EDGTraverser.getChild(variableExpression, 0);
		//final Node variable = EDGTraverser.getChild(variableExpression, 0);
		final LDASTNodeInfo ldNodeInfo = variable.getData().getInfo();
		@SuppressWarnings("unchecked")
		final EnumSet<Modifier> modifiers = (EnumSet<Modifier>) ldNodeInfo.getInfo()[0];

		variableDeclarators.add(variableDeclarator);

		return new VariableDeclarationExpr(modifiers, variableDeclarators);
	}
	private Expression parseDefinition(Node definition)
	{
		final Node target = EDGTraverser.getChild(definition, 0);
		final Node value = EDGTraverser.getChild(definition, 1);

		final Expression targetExpr = this.parseExpression(target);
		final Expression valueExpr = this.parseExpression(value);

		if (targetExpr instanceof NullLiteralExpr)
			return valueExpr;
		return new AssignExpr(targetExpr, valueExpr, AssignExpr.Operator.ASSIGN);
	}
	private Expression parseDataConstructor(Node dataConstructor)
	{
		final List<Node> elements = EDGTraverser.getChildren(dataConstructor);
		final List<Expression> expressions = this.parseExpressions(elements, true);

		return new ArrayInitializerExpr(NodeList.nodeList(expressions));
	}
	
	// OLD VERSION (Errors when extracting the name of the class and the name of the function)
	// EMPTY SCOPE not considered
//	private Expression parseCall2(Node call) 
//	{
//		final Node callee = EDGTraverser.getChild(call, 0);
//		final Node arguments = EDGTraverser.getChild(call, 1);
//
//		final Node scopeNode = EDGTraverser.getChild(callee, 0);
//		final Node scope = EDGTraverser.getChild(scopeNode, 0);
//		final Node nameNode = EDGTraverser.getChild(callee, 1);
//		final Node name = EDGTraverser.getChild(nameNode, 0);
//		final Expression scopeExpression = this.parseExpression(scope);
//		final Expression nameExpression = this.parseExpression(name);
//		final String scopeText = scope.getData().getName();
//		final String nameText = name.getData().getName();
//
//		final NodeList<Expression> argumentsList = new NodeList<Expression>();
//		final List<Node> argumentsChildren = EDGTraverser.getChildren(arguments);
//		argumentsList.addAll(this.parseExpressions(argumentsChildren, true));
//
//		if (scopeExpression instanceof NullLiteralExpr || nameExpression instanceof NullLiteralExpr)
//		{
//			this.funundef = true;
//			return new MethodCallExpr(null, new SimpleName("funundef"), argumentsList);
//		}
//		else if (nameText.equals("<constructor>"))
//		{
//			final ClassOrInterfaceType type = new ClassOrInterfaceType(scopeText);
//			return new ObjectCreationExpr(null, type, argumentsList);
//		}
//		else
//			return new MethodCallExpr(scopeExpression, new SimpleName(nameText), argumentsList);
	
	
//	}
	private Expression parseCall(Node call)
	{
		final Node callee = EDGTraverser.getChild(call, 0);
		final Node arguments = EDGTraverser.getChild(call, 1);

		// Function name
		final Node nameNode = EDGTraverser.getChild(callee, 1);
		final Node name = EDGTraverser.getChild(nameNode, 0);
		final Expression nameExpression = this.parseExpression(name);
		final String nameText = EDGTraverser.getChild(name, 0).getData().getName();
		
		// Arguments List
		final NodeList<Expression> argumentsList = new NodeList<Expression>();
		final List<Node> argumentsChildren = EDGTraverser.getChildren(arguments);
		argumentsList.addAll(this.parseExpressions(argumentsChildren, true));
		
		// Scope
		final Node scopeNode = EDGTraverser.getChild(callee, 0);
		if (EDGTraverser.getChildren(scopeNode).isEmpty())
		{
			if (nameExpression instanceof NullLiteralExpr)
			{
				this.funundef = true;
				return new MethodCallExpr(null, new SimpleName("funundef"), argumentsList);
			}
			else
				return new MethodCallExpr(null, new SimpleName(nameText), argumentsList);			
		}
		
		final Node scope = EDGTraverser.getChild(scopeNode, 0);
		final Expression scopeExpression = this.parseExpression(scope);
		final String scopeText = EDGTraverser.getChild(scope,0).getData().getName();
		
		if (scopeExpression instanceof NullLiteralExpr)
		{
			this.funundef = true;
			return new MethodCallExpr(null, new SimpleName("funundef"), argumentsList);
		}
		else if (nameText.equals("<constructor>"))
		{
			final ClassOrInterfaceType type = new ClassOrInterfaceType(scopeText);
			return new ObjectCreationExpr(null, type, argumentsList);
		}
		else
			return new MethodCallExpr(scopeExpression, new SimpleName(nameText), argumentsList);
	}
	private Expression parseOperation(Node operation)
	{
		final String sign = operation.getData().getName();
		final List<Node> operands = EDGTraverser.getChildren(operation);
		final Node expression = operands.get(0);
		final Expression firstExpression = this.parseExpression(expression);
		switch (operands.size())
		{
			case 1:
				final UnaryExpr.Operator[] unaryOperators = UnaryExpr.Operator.values();
				final boolean isPostfix = (boolean) operation.getData().getInfo().getInfo()[0];
				final Printable unaryOperator = this.getOperator(unaryOperators, sign, isPostfix);
				return new UnaryExpr(firstExpression, (UnaryExpr.Operator) unaryOperator);
			case 2:
				final Expression secondExpression = this.parseExpression(operands.get(1));
				final Printable[] binaryOperators = BinaryExpr.Operator.values();
				final Printable binaryOperator = this.getOperator(binaryOperators, sign);
				return new BinaryExpr(firstExpression, secondExpression, (BinaryExpr.Operator) binaryOperator);
			default:
				throw new RuntimeException("Operation arity not contemplated: " + operands.size());
		}
	}
	
	// ADDED TYPE EXPRESSIONS
	private Expression parseInstanceOf(Node instanceOf) {
		final Node expression = EDGTraverser.getChild(instanceOf, 0);
		final Node typeExpr = EDGTraverser.getChild(instanceOf, 1);
		final Node type = EDGTraverser.getChild(typeExpr, 0);
		
		final Expression instanceExpr = this.parseExpression(expression);
		final ClassOrInterfaceType instanceType = (ClassOrInterfaceType) this.parseType(type);
		return new InstanceOfExpr(instanceExpr, instanceType); // TODO Aqui pueden haber PRIMITIVE TYPES?
	}
	private Expression parseCastExpr(Node cast)
	{
		final Node typeExpr = EDGTraverser.getChild(cast, 0);
		final Node type = EDGTraverser.getChild(typeExpr, 0);
		final Node expression = EDGTraverser.getChild(cast, 1);
		
		final Expression castExpr = this.parseExpression(expression);
		final Type castType = this.parseType(type);
		return new CastExpr(castType, castExpr); 
	}
	private Type parseType(Node type)
	{
		
		if (type.getData().getInfo().getInfo().length != 0)
		{
			if (this.slice != null && !this.slice.contains(type))
				return new PrimitiveType();
			else
			{
				Object typeNode = type.getData().getInfo().getInfo()[0];
				if (typeNode instanceof Primitive)
			        return new PrimitiveType((Primitive) typeNode);
				else 
					return new PrimitiveType();
			}
				 
		}
		else 
			if (this.slice != null && !this.slice.contains(type))
				return new ClassOrInterfaceType("Object");
			else
				return new ClassOrInterfaceType(type.getData().getName());
	}
	
	private Expression parseDataConstructorAccess(Node dataConstructorAccess)
	{
		final Node dataConstructor = EDGTraverser.getChild(dataConstructorAccess, 0);
		final Node access = EDGTraverser.getChild(dataConstructorAccess, 1);

		final Expression dataConstructorExpr = this.parseExpression(dataConstructor);
		final Expression accessExpr = this.parseExpression(access);

		return new ArrayAccessExpr(dataConstructorExpr, accessExpr);
	}
	private Expression parseTernary(Node _if)
	{
		// Condition
		final Node condition = EDGTraverser.getChild(_if, 0);
		final Node conditionNode = EDGTraverser.getChild(condition, 0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		// Then
		final Node thenNode = EDGTraverser.getChild(_if, 1);
		final Node thenReturn = EDGTraverser.getChild(thenNode, 0);
		final Node then = EDGTraverser.getChild(thenReturn, 0);
		final Expression thenExpression = this.parseExpression(then);

		// Else
		final Node elseNode = EDGTraverser.getChild(_if, 2);
		final Node elseReturn = EDGTraverser.getChild(elseNode, 0);
		final Node _else = EDGTraverser.getChild(elseReturn, 0);
		final Expression elseExpression = this.parseExpression(_else);

		return new ConditionalExpr(conditionExpression, thenExpression, elseExpression);
	}

	// Variables & literals
	private Expression parseVariable(Node variable)
	{
		final VariableInfo info = (VariableInfo) variable.getData();
		if (info.isDeclaration())
			return this.parseDeclaration(variable);

		final String value = info.getName();
		return new NameExpr(value);
	}
	private Expression parseLiteral(Node literal)
	{
		final NodeInfo nodeInfo = literal.getData();
		final LDASTNodeInfo ldNodeInfo = nodeInfo.getInfo();
		final String value = nodeInfo.getName();		
		final String construction = ldNodeInfo.getConstruction();

		switch (construction)
		{
			case "boolean":
				return new BooleanLiteralExpr(Boolean.parseBoolean(value));
			case "char":
				return new CharLiteralExpr(value);
			case "double":
				return new DoubleLiteralExpr(value);
			case "int":
				return new IntegerLiteralExpr(value);
			case "long":
				return new LongLiteralExpr(value);
			case "String":
				return new StringLiteralExpr(value);
			case "null":
				return new NullLiteralExpr();
			case "name":
				return new NameExpr(value);
			case "object creation":
				return null;
			default:
				return new NullLiteralExpr();
		}
	}

	// Auxiliaries
	private VariableDeclarator getVariableDeclarator(Node declaration)
	{
		final Node variableExpression = (declaration.getData().getType() == NodeInfo.Type.Equality) ? EDGTraverser.getChild(declaration, 0) : declaration;
		final Node initializerNode = (declaration.getData().getType() == NodeInfo.Type.Equality) ? EDGTraverser.getChild(declaration, 1) : null;
		final Expression initializer = this.slice != null && !this.slice.contains(initializerNode) ? null : this.parseExpression(initializerNode);
		final Node variable = (variableExpression.getData().getType() == NodeInfo.Type.Variable) ? variableExpression : EDGTraverser.getChild(variableExpression, 0);
		//final Node variable = EDGTraverser.getChild(variableExpression, 0);
		final NodeInfo nodeInfo = variable.getData();
		final LDASTNodeInfo ldNodeInfo = nodeInfo.getInfo();
		final Type type = (Type) ldNodeInfo.getInfo()[1];
		final String name = nodeInfo.getName();

		return new VariableDeclarator(type, name, initializer);
	}
	private MethodDeclaration createFunundef()
	{
		final MethodDeclaration funundef;
		final NodeList<Parameter> parameters = new NodeList<Parameter>();

		parameters.add(new Parameter(new ClassOrInterfaceType("Object..."), "params"));
		funundef = new MethodDeclaration(EnumSet.of(Modifier.PRIVATE, Modifier.STATIC), "funundef", new VoidType(), parameters);

		return funundef;
	}
	private Printable getOperator(Printable[] operators, String sign)
	{
		for (Printable operator : operators)
			if (operator.asString().equals(sign))
				return operator;
		return null;
	}
	private Printable getOperator(UnaryExpr.Operator[] operators, String sign, Boolean isPostfix){
		for (UnaryExpr.Operator operator : operators)
			if (operator.asString().equals(sign) && operator.isPostfix() == isPostfix)
				return operator;
		return null;
	}
	
}