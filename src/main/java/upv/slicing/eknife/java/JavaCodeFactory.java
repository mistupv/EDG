package upv.slicing.eknife.java;

import com.github.javaparser.ast.ArrayCreationLevel;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Modifier;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.nodeTypes.NodeWithBlockStmt;
import com.github.javaparser.ast.nodeTypes.NodeWithOptionalBlockStmt;
import com.github.javaparser.ast.nodeTypes.NodeWithParameters;
import com.github.javaparser.ast.stmt.*;
import com.github.javaparser.ast.type.*;
import com.github.javaparser.ast.type.PrimitiveType.Primitive;
import com.github.javaparser.printer.Printable;
import upv.slicing.edg.LDASTNodeInfo;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.graph.Variable;
import upv.slicing.edg.traverser.EDGTraverser;
import upv.slicing.misc.Misc;

import java.io.File;
import java.util.*;

public class JavaCodeFactory {
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static void createJavaFile(File outputFile, EDG edg)
	{
		JavaCodeFactory.createJavaFile(outputFile, edg, null);
	}
	public static void createJavaFile(File outputFile, EDG edg, Set<Node> slice)
	{
		final JavaCodeFactory javaFactory = new JavaCodeFactory(edg, slice);
		final CompilationUnit cu = javaFactory.generate();
		final String text = cu.toString();

// SHOW CODE IN TERMINAL
		System.out.println("\n" + text);

		Misc.write(outputFile, text, false);
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private final EDG edg;
	private final Set<Node> slice;
	private boolean funundef = false;
	//	private boolean returnRequired = false;
	private final Stack<Context> context = new Stack<>();

	private JavaCodeFactory(EDG edg, Set<Node> slice)
	{
		this.edg = edg;
		this.slice = slice;
	}

	private CompilationUnit generate()
	{
		final CompilationUnit cu = new CompilationUnit();
		final Node root = this.edg.getRootNode();

		// Modules
		final List<Node> modules = EDGTraverser.getChildren(edg, root);
		for (Node module : modules)
		{
			if (this.slice != null && !this.slice.contains(module))
				continue;

			this.parseModule(cu, module);
		}

		return cu;
	}

	// Structure
	@SuppressWarnings("unchecked")
	private void parseModule(CompilationUnit cu, Node module)
	{
		final String moduleName = module.getName();
		final ClassOrInterfaceDeclaration clazz = cu.addClass(moduleName);

		//NodeList<ClassOrInterfaceType> extendedTypes = (NodeList<ClassOrInterfaceType>) module.getInfo().getInfo()[0];
		// TODO ENCONTRAR UNA SOLUCION MEJOR
		String extendedTypes0 = (String) module.getInfo().getInfo()[0]; // Si hay mas de un extends no funcionara, solo vale para extends de 1 clase
		NodeList<ClassOrInterfaceType> extendedTypes = new NodeList<>();
		if (!extendedTypes0.equals(""))
			extendedTypes.add(new ClassOrInterfaceType(extendedTypes0));

		NodeList<ClassOrInterfaceType> implementedTypes = (NodeList<ClassOrInterfaceType>) module.getInfo().getInfo()[1];

		clazz.setExtendedTypes(extendedTypes);
		clazz.setImplementedTypes(implementedTypes);

		this.parseMembers(clazz, module);
	}
	private void parseMembers(ClassOrInterfaceDeclaration clazz, Node module)
	{
		final List<Node> members = EDGTraverser.getChildren(edg, module);

		this.funundef = false;
		for (Node member : members)
		{
			if (this.slice != null && !this.slice.contains(member))
				continue;

			// TODO No pasar el clazz en las llamadas
			if (member.getType() != Node.Type.Routine)
				this.parseGlobalVariable(clazz, member);
			else
				this.parseRoutine(clazz, member);
		}

		if (this.funundef)
			clazz.addMember(this.createFunundef());
	}
	private void parseGlobalVariable(ClassOrInterfaceDeclaration clazz, Node globalVariable)
	{
		final NodeList<VariableDeclarator> variableDeclarators = new NodeList<>();
		final Node.Type nodeType = globalVariable.getType();
		final Node expression =
				nodeType == Node.Type.Expression ? EDGTraverser.getChild(edg, globalVariable, 0) : globalVariable;
		final VariableDeclarator variableDeclarator = this.getVariableDeclarator(expression);
		final Node variable = (expression.getType() == Node.Type.Equality) ?
				EDGTraverser.getChild(edg, EDGTraverser.getChild(edg, expression, 0), 0) : expression;
		final LDASTNodeInfo ldNodeInfo = variable.getInfo();
		@SuppressWarnings("unchecked") final EnumSet<Modifier> modifiers = (EnumSet<Modifier>) ldNodeInfo.getInfo()[0];

		variableDeclarators.add(variableDeclarator);
		clazz.addMember(new FieldDeclaration(modifiers, variableDeclarators));
	}

	private void parseRoutine(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final Node clause = EDGTraverser
				.getChild(edg, routine, 0); // If there are multiple clauses (Erlang) this is not correct
		final Node parametersNode = EDGTraverser.getChild(edg, clause, Node.Type.Parameters);
		final Node body = EDGTraverser.getChild(edg, clause, Node.Type.Body);
		final Type returnType = (Type) routine.getInfo().getInfo()[1];
		final boolean returnRequired = !(returnType instanceof VoidType);

		context.push(new Context(routine.getType(), returnType, returnRequired));

		// Callable declaration
		final String name = routine.getName();
		final CallableDeclaration<?> callableDeclaration = name.equals("<constructor>") ? this
				.parseConstructor(clazz, routine) : this.parseMethod(clazz, routine);

		// Parameters
		final NodeWithParameters<?> nodeWithParameters = (NodeWithParameters<?>) callableDeclaration;
		final List<Node> parametersChildren = EDGTraverser.getChildren(edg, parametersNode);
		for (Node parameter : parametersChildren)
			nodeWithParameters.addParameter(this.parseParameter(parameter));

		// Statements
		final BlockStmt blockStmt = callableDeclaration instanceof NodeWithBlockStmt ?
				((NodeWithBlockStmt<?>) callableDeclaration).getBody() :
				((NodeWithOptionalBlockStmt<?>) callableDeclaration).getBody().get();
		final List<Node> bodyChildren = EDGTraverser.getChildren(edg, body);
		final List<Statement> statements = this.parseStatements(bodyChildren, false);

		if (context.peek().returnReq)
		{
			final Expression returnExpr = generateReturnExpr(returnType);
			statements.add(new ReturnStmt(returnExpr));
		}

		for (Statement statement : statements)
			blockStmt.addStatement(statement);

		context.pop();
	}
	private CallableDeclaration<?> parseMethod(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final LDASTNodeInfo ldNodeInfo = routine.getInfo();
		final String name = routine.getName();
		@SuppressWarnings("rawtypes")
		final Modifier[] modifiers = (Modifier[]) ((EnumSet) ldNodeInfo.getInfo()[0]).toArray(new Modifier[0]);
		final Type type = (Type) ldNodeInfo.getInfo()[1];
		final MethodDeclaration methodDeclaration = clazz.addMethod(name, modifiers);

		methodDeclaration.setType(type);

		return methodDeclaration;
	}
	private CallableDeclaration<?> parseConstructor(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final LDASTNodeInfo ldNodeInfo = routine.getInfo();
		@SuppressWarnings("rawtypes")
		final Modifier[] modifiers = (Modifier[]) ((EnumSet) ldNodeInfo.getInfo()[0]).toArray(new Modifier[0]);
		final ConstructorDeclaration constructorDeclaration = clazz.addConstructor(modifiers);

		return constructorDeclaration;
	}
	private Parameter parseParameter(Node parameter)
	{
		final Node variable = EDGTraverser.getChild(edg, parameter, Node.Type.Value);
		final LDASTNodeInfo ldNodeInfo = variable.getInfo();
		final Type type = this.slice != null && !this.slice.contains(variable) ? new ClassOrInterfaceType(
				"Object") : (Type) ldNodeInfo.getInfo()[0];
		final String name = this.slice != null && !this.slice.contains(variable) ? "fresh" : variable.getName();

		return new Parameter(type, name);
	}

	// Statement
	private List<Statement> parseStatements(List<Node> nodes, boolean transformUnused)
	{
		final List<Statement> statementsList = new LinkedList<>();

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

	private List<Statement> parseStatements(List<Node> nodes, boolean transformUnused, boolean returnReq)
	{
		final List<Statement> statementsList = new LinkedList<>();

		for (Node node : nodes)
		{
			if (!transformUnused && this.slice != null && !this.slice.contains(node) && !returnReq)
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
			// Hay que poner returns en las calls a funciones
			if (node.getType() != Node.Type.Return)
				return new EmptyStmt();

		final Node.Type nodeType = node.getType();
		final Node statement = nodeType == Node.Type.Expression ? EDGTraverser.getChild(edg, node, 0) : node;
		final Node.Type statementType = statement.getType();

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
			case Throw:
				return this.parseThrow(statement);
			case Foreach:
				return this.parseForeach(statement);
			default:
				return new ExpressionStmt(this.parseExpression(statement));
		}
	}
	private Statement parseIf(Node _if)
	{
		// Condition
		final Node conditionNode = EDGTraverser.getChild(edg, _if, Node.Type.Condition);
		final Node condition = EDGTraverser.getChild(edg, conditionNode, 0);
		final Expression conditionExpression = this.parseExpression(condition);

		// Then
		final Node then = EDGTraverser.getChild(edg, _if, Node.Type.Then);
		final List<Node> thenChildren = EDGTraverser.getChildren(edg, then);
		final List<Statement> thenStatements = this.parseStatements(thenChildren, false);
		final BlockStmt thenBlock = new BlockStmt();
		for (Statement thenStatement : thenStatements)
			thenBlock.addStatement(thenStatement);

		// Else
		final Node _else = EDGTraverser.getChild(edg, _if, Node.Type.Else);
		final List<Node> elseChildren = EDGTraverser.getChildren(edg, _else);
		final List<Statement> elseStatements = this.parseStatements(elseChildren, false);

		if (elseStatements.isEmpty()) // Avoid generating an empty Else block
			return new IfStmt(conditionExpression, thenBlock, null);

		final BlockStmt elseBlock = new BlockStmt();
		for (Statement elseStatement : elseStatements)
			elseBlock.addStatement(elseStatement);

		return new IfStmt(conditionExpression, thenBlock, elseBlock);
	}
	private Statement parseSwitch(Node _switch)
	{
		// Selector
		final Node selectors = EDGTraverser.getChild(edg, _switch, Node.Type.Selector);
		final Node selectorNode = EDGTraverser.getChild(edg, selectors, 0);
		final Expression selector = this.parseExpression(selectorNode);

		// Cases
		final Node cases = EDGTraverser.getChild(edg, _switch, Node.Type.Cases);
		final List<Node> casesChildren = EDGTraverser.getChildren(edg, cases);
		final NodeList<SwitchEntryStmt> entries = new NodeList<>();
		final List<Statement> statements = this.parseStatements(casesChildren, false);
		for (Statement statement : statements)
			entries.add((SwitchEntryStmt) statement);

		return new SwitchStmt(selector, entries);
	}
	private Statement parseCase(Node _case)
	{
		// Label
		final Node selectable = EDGTraverser.getChild(edg, _case, Node.Type.Selectable);
		final List<Node> selectablesChildren = EDGTraverser.getChildren(edg, selectable);
		final Node selectableNode = selectablesChildren.get(0);
		final Expression label = this.parseExpression(selectableNode);

		// Statements
		final Node body = EDGTraverser.getChild(edg, _case, Node.Type.Body);
		final List<Node> bodyChildren = EDGTraverser.getChildren(edg, body);
		final NodeList<Statement> statements = new NodeList<>();
		final List<Statement> statements0 = this.parseStatements(bodyChildren, false);
		statements.addAll(statements0);

		return new SwitchEntryStmt(label, statements);
	}

	// Different Loop Structures
	private Statement parseCLoop(Node loop)
	{
		// Condition
		final Node condition = EDGTraverser.getChild(edg, loop, 0);
		final List<Node> conditionChildren = EDGTraverser.getChildren(edg, condition);
		final Node conditionNode = conditionChildren.get(0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		// Body
		final Node body = EDGTraverser.getChild(edg, loop, 1);
		final List<Node> bodyChildren = EDGTraverser.getChildren(edg, body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);

		return new WhileStmt(conditionExpression, bodyBlock);
	}

	private Statement parseRLoop(Node loop)
	{
		// Body
		final Node body = EDGTraverser.getChild(edg, loop, 0);
//final Node body = EDGTraverserNew.getChild(loop, 1);
		final List<Node> bodyChildren = EDGTraverser.getChildren(edg, body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);

		// Condition
		final Node condition = EDGTraverser.getChild(edg, loop, 1);
//final Node condition = EDGTraverserNew.getChild(loop, 0);
		final List<Node> conditionChildren = EDGTraverser.getChildren(edg, condition);
		final Node conditionNode = conditionChildren.get(0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		return new DoStmt(bodyBlock, conditionExpression);
	}

	private Statement parseFLoop(Node loop)
	{
		// Initialization
		final Node initialization = EDGTraverser.getChild(edg, loop, 0);
		final List<Node> initializationChildren = EDGTraverser.getChildren(edg, initialization);
		final NodeList<Expression> initBlock = new NodeList<>();
		initBlock.addAll(this.parseExpressions(initializationChildren, false));

		// Condition
		final Node condition = EDGTraverser.getChild(edg, loop, 1);
		final List<Node> conditionChildren = EDGTraverser.getChildren(edg, condition);
		final Node conditionNode = conditionChildren.get(0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		// Update
		final Node update = EDGTraverser.getChild(edg, loop, 3);
		final List<Node> updateChildren = EDGTraverser.getChildren(edg, update);
		final NodeList<Expression> updateBlock = new NodeList<>();
		updateBlock.addAll(this.parseExpressions(updateChildren, false));

		// Body
		final Node body = EDGTraverser.getChild(edg, loop, 2);
		final List<Node> bodyChildren = EDGTraverser.getChildren(edg, body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);

		return new ForStmt(initBlock, conditionExpression, updateBlock, bodyBlock);
	}

	private Statement parseForeach(Node foreach)
	{
		final Node iterator = EDGTraverser.getChild(edg, foreach, 0);
		final Node generator = EDGTraverser.getChild(edg, iterator, 0);
		final Node variableDeclaration = EDGTraverser.getChild(edg, generator, 0);
		final Node iterable = EDGTraverser.getChild(edg, generator, 1);
		final Expression variableDeclarationExpr = this.parseExpression(variableDeclaration);
		final Expression iterableExpr = this.parseExpression(iterable);

		final Node body = EDGTraverser.getChild(edg, foreach, 1);
		final List<Node> bodyChildren = EDGTraverser.getChildren(edg, body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);
		if (variableDeclarationExpr instanceof VariableDeclarationExpr)
			return new ForeachStmt((VariableDeclarationExpr) variableDeclarationExpr, iterableExpr, bodyBlock);

		throw new RuntimeException("There must be a variable declaration in the left-hand-side of an iterator");
	}

	// Exception Statements
	private Statement parseExHandler(Node exHandler)
	{
		// Try
		final Node tryNode = EDGTraverser.getChild(edg, exHandler, 0);
		final BlockStmt tryBlock0 = this.parseBlockStatements(tryNode);
		final BlockStmt tryBlock = tryBlock0 == null ? new BlockStmt() : tryBlock0;

		// Catch
		final Node catchNode = EDGTraverser.getChild(edg, exHandler, 1);
		final NodeList<CatchClause> catchClauses = (NodeList<CatchClause>) this.parseCatch(catchNode);
		if (catchClauses.isEmpty())
			catchClauses
					.add(new CatchClause(new Parameter(new ClassOrInterfaceType("Exception"), new SimpleName("fresh")),
										 new BlockStmt()));

		// Finally
		final Node finallyNode = EDGTraverser.getChild(edg, exHandler, 2);
		final BlockStmt finallyBlock = this.parseBlockStatements(finallyNode);

		return new TryStmt(tryBlock, catchClauses, finallyBlock);
	}

	private List<CatchClause> parseCatch(Node _catch)
	{
		final List<Node> clauses = EDGTraverser.getChildren(edg, _catch);
		final List<CatchClause> catchClauses = new NodeList<>();
		for (Node clause : clauses)
		{
			if (this.slice != null && !this.slice.contains(clause))
				continue;

			final Node parameters = EDGTraverser.getChild(edg, clause, 0);
			final Node parameter = EDGTraverser.getChild(edg, parameters, 0);
			final Parameter parameter0 = this.parseParameter(parameter);

			final Node body = EDGTraverser.getChild(edg, clause, 2);
			final BlockStmt bodyBlock0 = this.parseBlockStatements(body);
			final BlockStmt bodyBlock = bodyBlock0 == null ? new BlockStmt() : bodyBlock0;

			catchClauses.add(new CatchClause(parameter0, bodyBlock));
		}
		return catchClauses;
	}

	private Statement parseThrow(Node _throw)
	{
		final Node throwExpressionNode = EDGTraverser.getChild(edg, _throw, 0);
		final Expression throwExpression = this.parseExpression(throwExpressionNode);
		return new ThrowStmt(throwExpression);
	}

	private Statement parseReturn(Node _return)
	{
		if (this.slice != null && !this.slice.contains(_return))
		{
			Context ctx = context.peek();
			if (ctx.returnReq)
			{
				updateReturnContext();

				final Type type = ctx.returnType;
				final Expression returnExpr = generateReturnExpr(type);
				return new ReturnStmt(returnExpr);
			}
		}

		final List<Node> returnChildren = EDGTraverser.getChildren(edg, _return);
		if (returnChildren.isEmpty())
			return new ReturnStmt();

		final Node returnChild = returnChildren.get(0);
		final Expression returnExpression = this.parseExpression(returnChild);

		updateReturnContext();

		return new ReturnStmt(returnExpression);
	}

	private BlockStmt parseBlockStatements(Node blockRoot)
	{
		if (this.slice != null && !this.slice.contains(blockRoot))
			return null;

		final List<Node> bodyChildren = EDGTraverser.getChildren(edg, blockRoot);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);
		return bodyBlock;
	}

	// Expressions
	private List<Expression> parseExpressions(List<Node> nodes, boolean transformUnused)
	{
		final List<Expression> expressionsList = new LinkedList<>();

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

		final Node.Type nodeType = node.getType();
		final Node expression = nodeType == Node.Type.Expression ? EDGTraverser.getChild(edg, node, 0) : node;
		final Node.Type expressionType = expression.getType();

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
			case FieldAccess:
				return this.parseFieldAccess(expression);
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
			case Reference:
				return this.parseReference(expression);
			default:
				throw new RuntimeException("Expression type not contemplated: " + expressionType);
		}
	}

	private Expression parseEquality(Node equality)
	{
		final Node targetExpression = EDGTraverser.getChild(edg, equality, 0);
		final Node target = EDGTraverser.getChild(edg, targetExpression, Node.Type.Value);
		final LDASTNodeInfo ldNodeInfo = target.getInfo();
		final Object[] info = ldNodeInfo.getInfo();

		if (target.getType() == Node.Type.Variable)
		{
			Variable vi = (Variable) target;
			if (vi.isDeclaration())
				return this.parseDeclaration(equality);
			return this.parseDefinition(equality);
		} else if (info == null || info.length == 0)
			return this.parseDefinition(equality);
		return this.parseDeclaration(equality);
	}
	private Expression parseDeclaration(Node declaration)
	{
		final NodeList<VariableDeclarator> variableDeclarators = new NodeList<>();
		final VariableDeclarator variableDeclarator = this.getVariableDeclarator(declaration);
		final Node variableExpression = (declaration.getType() == Node.Type.Equality) ? EDGTraverser
				.getChild(edg, declaration, 0) : declaration;
		final Node variable = (variableExpression.getType() == Node.Type.Variable) ? variableExpression : EDGTraverser
				.getChild(edg, variableExpression, 0);
		//final Node variable = EDGTraverserNew.getChild(variableExpression, 0);
		final LDASTNodeInfo ldNodeInfo = variable.getInfo();
		@SuppressWarnings("unchecked") final EnumSet<Modifier> modifiers = (EnumSet<Modifier>) ldNodeInfo.getInfo()[0];

		variableDeclarators.add(variableDeclarator);

		return new VariableDeclarationExpr(modifiers, variableDeclarators);
	}
	private Expression parseDefinition(Node definition)
	{
		final Node target = EDGTraverser.getChild(edg, definition, 0);
		final Node value = EDGTraverser.getChild(edg, definition, 1);

		final Expression targetExpr = this.parseExpression(target);
		final Expression valueExpr = this.parseExpression(value);

		if (targetExpr instanceof NullLiteralExpr)
		{
			final String name = "fresh";
			final Type type = new ClassOrInterfaceType("Object");
			return new VariableDeclarationExpr(new VariableDeclarator(type, name, valueExpr));
		}
		return new AssignExpr(targetExpr, valueExpr, AssignExpr.Operator.ASSIGN);
	}

	private Expression parseDataConstructor(Node dataConstructor)
	{
		final List<Node> elements = EDGTraverser.getChildren(edg, dataConstructor);
		final List<Expression> expressions = this.parseExpressions(elements, true);

		return new ArrayInitializerExpr(NodeList.nodeList(expressions));
	}

	// OLD VERSION (Errors when extracting the name of the class and the name of the function)
	// EMPTY SCOPE not considered
//	private Expression parseCall2(Node call) 
//	{
//		final Node callee = EDGTraverserNew.getChild(call, 0);
//		final Node arguments = EDGTraverserNew.getChild(call, 1);
//
//		final Node scopeNode = EDGTraverserNew.getChild(callee, 0);
//		final Node scope = EDGTraverserNew.getChild(scopeNode, 0);
//		final Node nameNode = EDGTraverserNew.getChild(callee, 1);
//		final Node name = EDGTraverserNew.getChild(nameNode, 0);
//		final Expression scopeExpression = this.parseExpression(scope);
//		final Expression nameExpression = this.parseExpression(name);
//		final String scopeText = scope.getName();
//		final String nameText = name.getName();
//
//		final NodeList<Expression> argumentsList = new NodeList<Expression>();
//		final List<Node> argumentsChildren = EDGTraverserNew.getChildren(arguments);
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
		final Node callee = EDGTraverser.getChild(edg, call, Node.Type.Callee);
		final Node arguments = EDGTraverser.getChild(edg, call, Node.Type.Arguments);

		// Function name
		final Node nameNode = EDGTraverser.getChild(edg, callee, Node.Type.Name);
		final Node name = EDGTraverser.getChild(edg, nameNode, 0);
		final Expression nameExpression = this.parseExpression(name);
		final String nameText;
		if (this.slice != null && !this.slice.contains(nameNode))
		{
			this.funundef = true;
			nameText = "funundef";
		} else
			nameText = EDGTraverser.getChild(edg, name, 0).getName();

		// Arguments List
		final NodeList<Expression> argumentsList = new NodeList<>();
		final List<Node> argumentsChildren = EDGTraverser.getChildren(edg, arguments);
		argumentsList.addAll(this.parseExpressions(argumentsChildren, true));

		// ONLY CALL -> SCOPE REGARDS ABOUT LAS ENCLOSED_EXPR TYPE
		// Scope
		final Node scopeNode = EDGTraverser.getChild(edg, callee, Node.Type.Scope);
		if (EDGTraverser.getChildren(edg, scopeNode).isEmpty())
		{
			if (nameExpression instanceof NullLiteralExpr)
			{
				this.funundef = true;
				return new MethodCallExpr(null, new SimpleName("funundef"), argumentsList);
			} else
				return new MethodCallExpr(null, new SimpleName(nameText), argumentsList);
		}

		final Node scope = EDGTraverser.getChild(edg, scopeNode, 0);
//		final boolean scopeEnclosedExpr = scope.getInfo() == null || scope.getInfo().length == 0 ? false : (boolean) scope.getInfo().getInfo()[0];
		final Expression scopeExpression = this.parseExpression(scope);
		final String scopeText = EDGTraverser.getChild(edg, scope, 0).getName();

		if (scopeExpression instanceof NullLiteralExpr)
		{
			this.funundef = true;
			return new MethodCallExpr(null, new SimpleName("funundef"), argumentsList);
		} else if (nameText.equals("<constructor>"))
		{
			final ClassOrInterfaceType type = new ClassOrInterfaceType(scopeText);

//			if (scopeEnclosedExpr)
//				return new EnclosedExpr(new ObjectCreationExpr(null, type, argumentsList));
			return new ObjectCreationExpr(null, type, argumentsList);
		} else if (nameText.equals("<arrayConstructor>"))
		{
			final ClassOrInterfaceType type = new ClassOrInterfaceType(scopeText);
			@SuppressWarnings("unchecked")
			final Expression returned = new ArrayCreationExpr(type, (NodeList<ArrayCreationLevel>) EDGTraverser
					.getChild(edg, name, 0).getInfo().getInfo()[0], null);
//			if (scopeEnclosedExpr)
//				return new EnclosedExpr(returned);
			return returned;
		} else
			return new MethodCallExpr(scopeExpression, new SimpleName(nameText), argumentsList);
	}
	private Expression parseOperation(Node operation)
	{
		final String sign = operation.getName();
		final List<Node> operands = EDGTraverser.getChildren(edg, operation);
		final Node expression = operands.get(0);
		final Expression firstExpression = this.parseExpression(expression);
		switch (operands.size())
		{
			case 1:
				final UnaryExpr.Operator[] unaryOperators = UnaryExpr.Operator.values();
				final boolean isPostfix = (boolean) operation.getInfo().getInfo()[0];
				final Printable unaryOperator = this.getOperator(unaryOperators, sign, isPostfix);
				return new UnaryExpr(firstExpression, (UnaryExpr.Operator) unaryOperator);
			case 2:
				final Expression secondExpression = this.parseExpression(operands.get(1));
				final Printable[] binaryOperators = BinaryExpr.Operator.values();
				final Printable binaryOperator0 = this.getOperator(binaryOperators, sign);
				final BinaryExpr.Operator binaryOperator = (BinaryExpr.Operator) binaryOperator0;
				final Expression firstExpression0 = this
						.isEnclosedExpr(firstExpression, binaryOperator, true) ? new EnclosedExpr(
						firstExpression) : firstExpression;
				final Expression secondExpression0 = this
						.isEnclosedExpr(secondExpression, binaryOperator, false) ? new EnclosedExpr(
						secondExpression) : secondExpression;
				return new BinaryExpr(firstExpression0, secondExpression0, binaryOperator);
			default:
				throw new RuntimeException("Operation arity not contemplated: " + operands.size());
		}
	}

	private Expression parseReference(Node reference)
	{
		final String value = reference.getName();

		switch (value)
		{
			case "super":
				return new SuperExpr();
			case "this":
				return new ThisExpr();
			default:
				return new NullLiteralExpr();
		}
	}

	// ADDED TYPE EXPRESSIONS
	private Expression parseInstanceOf(Node instanceOf)
	{
		final Node expression = EDGTraverser.getChild(edg, instanceOf, 0);
		final Node type = EDGTraverser.getChild(edg, instanceOf, 1);
//		final Node type = EDGTraverserNew.getChild(typeExpr, 0);

		final Expression instanceExpr = this.parseExpression(expression);
		final Type instanceType = this.parseType(
				type); // InstanceOf only accepts object types and array types, it is not applicable to Primitive Types
		if (instanceType instanceof ClassOrInterfaceType)
			return new InstanceOfExpr(instanceExpr, (ClassOrInterfaceType) instanceType);
		return new InstanceOfExpr(instanceExpr, (ArrayType) instanceType);
	}

	private Expression parseCastExpr(Node cast)
	{
		final Node type = EDGTraverser.getChild(edg, cast, 0);
//		final Node type = EDGTraverserNew.getChild(typeExpr, 0);
		final Node expression = EDGTraverser.getChild(edg, cast, 1);

		final Expression castExpr = this.parseExpression(expression);
		final Type castType = this.parseType(type);
		final CastExpr finalCast = new CastExpr(castType, castExpr);

		final boolean isEnclosedExpr = (boolean) cast.getInfo().getInfo()[0];
		return isEnclosedExpr ? new EnclosedExpr(finalCast) : finalCast;
	}

	private Type parseType(Node type)
	{
		if (type.getInfo().getInfo().length != 0)
		{
			if (this.slice != null && !this.slice.contains(type))
				return new PrimitiveType();
			else
			{    // DIRIA QUE ESTO NUNCA SE EJECUTA
				Object info = type.getInfo().getInfo()[0];
				if (info.equals("arrayType"))

					return new ArrayType(new ClassOrInterfaceType(type.getName()));
				else if (info instanceof PrimitiveType.Primitive)
					return new PrimitiveType((Primitive) info);
				else
					return new PrimitiveType();
			}

		} else if (this.slice != null && !this.slice.contains(type))
			return new ClassOrInterfaceType("Object");
		else
			return new ClassOrInterfaceType(type.getName());
	}

	private Expression parseDataConstructorAccess(Node dataConstructorAccess)
	{
		final Node dataConstructor = EDGTraverser.getChild(edg, dataConstructorAccess, Node.Type.Variable);
		final Node access = EDGTraverser.getChild(edg, dataConstructorAccess, Node.Type.Index);

		final Expression dataConstructorExpr = this.parseExpression(dataConstructor);
		final Expression accessExpr = this.parseExpression(access);

		return new ArrayAccessExpr(dataConstructorExpr, accessExpr);
	}

	private Expression parseFieldAccess(Node fieldAccess)
	{
		final Node scope = EDGTraverser.getChild(edg, fieldAccess, 0);
		final Node name = EDGTraverser.getChild(edg, fieldAccess, 1);

		final Expression scopeExpr = this.parseExpression(scope);
		final Expression nameExpr = this.parseExpression(name);

		return new FieldAccessExpr(scopeExpr, nameExpr.toString());

	}

	private Expression parseTernary(Node _if)
	{
		// Condition
		final Node condition = EDGTraverser.getChild(edg, _if, Node.Type.Condition);
		final Node conditionNode = EDGTraverser.getChild(edg, condition, 0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		// Then
		final Node thenNode = EDGTraverser.getChild(edg, _if, Node.Type.Then);
		final Node thenReturn = EDGTraverser.getChild(edg, thenNode, 0);
		final Node then = EDGTraverser.getChild(edg, thenReturn, 0);
		final Expression thenExpression = this.parseExpression(then);

		// Else
		final Node elseNode = EDGTraverser.getChild(edg, _if, Node.Type.Else);
		final Node elseReturn = EDGTraverser.getChild(edg, elseNode, 0);
		final Node _else = EDGTraverser.getChild(edg, elseReturn, 0);
		final Expression elseExpression = this.parseExpression(_else);

		return new ConditionalExpr(conditionExpression, thenExpression, elseExpression);
	}

	// Variables & literals
	private Expression parseVariable(Node variable)
	{
		final Variable info = (Variable) variable;
		if (info.isDeclaration())
			return this.parseDeclaration(variable);

		final String value = info.getName();
		return new NameExpr(value);
	}
	private Expression parseLiteral(Node literal)
	{
		final LDASTNodeInfo ldNodeInfo = literal.getInfo();
		final String value = literal.getName();
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
			case "array creation":
				return null;
			default:
				return new NullLiteralExpr();
		}
	}

	// Auxiliaries
	private VariableDeclarator getVariableDeclarator(Node declaration)
	{

		final Node variableExpression = (declaration.getType() == Node.Type.Equality) ? EDGTraverser
				.getChild(edg, declaration, 0) : declaration;
		final Node initializerNode = (declaration.getType() == Node.Type.Equality) ? EDGTraverser
				.getChild(edg, declaration, 1) : null;
		Expression initializer = this.slice != null && !this.slice.contains(initializerNode) ? null : this
				.parseExpression(initializerNode);

		if (initializer == null)
		{
			final Node variable = (variableExpression.getType() == Node.Type.Variable) ? variableExpression : EDGTraverser
					.getChild(edg, variableExpression, 0);
			final LDASTNodeInfo ldNodeInfo = variable.getInfo();
			final Type varType = (Type) ldNodeInfo.getInfo()[1];
			if (varType instanceof PrimitiveType || initializerNode == null)
				initializer = null;
			else
				initializer = new ObjectCreationExpr(null, (ClassOrInterfaceType) varType, new NodeList<>());
		}

		final Node variable = (variableExpression.getType() == Node.Type.Variable) ? variableExpression : EDGTraverser
				.getChild(edg, variableExpression, 0);
		//final Node variable = EDGTraverserNew.getChild(variableExpression, 0);

		if (this.slice != null && !this.slice.contains(variable))
		{
			final String name = "fresh";
			final Type type = new ClassOrInterfaceType("Object");
			return new VariableDeclarator(type, name, initializer);
		}

		final LDASTNodeInfo ldNodeInfo = variable.getInfo();
		final Type type = (Type) ldNodeInfo.getInfo()[1];
		final String name = variable.getName();

		return new VariableDeclarator(type, name, initializer);
	}
	private MethodDeclaration createFunundef()
	{
		final MethodDeclaration funundef;
		final NodeList<Parameter> parameters = new NodeList<>();

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

	private Printable getOperator(UnaryExpr.Operator[] operators, String sign, Boolean isPostfix)
	{
		for (UnaryExpr.Operator operator : operators)
			if (operator.asString().equals(sign) && operator.isPostfix() == isPostfix)
				return operator;
		return null;
	}

	private boolean isEnclosedExpr(Expression expr, BinaryExpr.Operator operator, boolean isLeftExpr)
	{
		if (expr instanceof BinaryExpr)
		{
			BinaryExpr.Operator opChild = ((BinaryExpr) expr).getOperator();
			switch (operator)
			{
				case PLUS:
				case MINUS:
					switch (opChild)
					{
						case PLUS:
						case MINUS:
							return !isLeftExpr;
						case MULTIPLY:
						case DIVIDE:
						case REMAINDER:
							return false;
						default:
							break;
					}
					break;
				case MULTIPLY:
				case DIVIDE:
				case REMAINDER:
					switch (opChild)
					{
						case PLUS:
						case MINUS:
							return true;
						case MULTIPLY:
						case DIVIDE:
						case REMAINDER:
							return !isLeftExpr;
						default:
							break;
					}
					break;
				default:
					break;
			}
		}
		return false;
	}

	private Expression generateReturnExpr(Type type)
	{
		if (type.equals(PrimitiveType.intType()))
			return new IntegerLiteralExpr(0);
		if (type.equals(PrimitiveType.shortType()))
			return new IntegerLiteralExpr(0);
		if (type.equals(PrimitiveType.longType()))
			return new LongLiteralExpr(0);
		if (type.equals(PrimitiveType.floatType()))
			return new DoubleLiteralExpr(0);
		if (type.equals(PrimitiveType.doubleType()))
			return new DoubleLiteralExpr(0);
		if (type.equals(PrimitiveType.byteType()))
			return new IntegerLiteralExpr(0);
		if (type.equals(PrimitiveType.charType()))
			return new CharLiteralExpr(' ');
		if (type.equals(PrimitiveType.booleanType()))
			return new BooleanLiteralExpr(true);
		return new NullLiteralExpr();
	}

	private void updateReturnContext()
	{
		Context ctx = context.peek();
		if (ctx.returnReq)
		{
			ctx.trueBranches++;
			if (ctx.nodeType == Node.Type.If && ctx.trueBranches == 2)
			{
				ctx.returned();
				final Context ifContext = context.pop();
				final Context previousContext = context.peek();

				if (previousContext.nodeType != Node.Type.If)
					previousContext.returned();
				else if (previousContext.trueBranches == 1)
					previousContext.returned();
				else
					previousContext.trueBranches++;
				context.push(ifContext);
			} else
				ctx.returned();
		}
	}

	private static class Context {
		private final Node.Type nodeType;
		private final Type returnType;
		private boolean returnReq;
		private int trueBranches = 0;

		public Context(Node.Type nodeType, Type returnType, boolean returnReq)
		{
			this.nodeType = nodeType;
			this.returnType = returnType;
			this.returnReq = returnReq;
		}

		public void returned()
		{
			this.returnReq = false;
		}
	}

}