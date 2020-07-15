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
import upv.slicing.edg.graph.*;
import upv.slicing.eknife.Util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static upv.slicing.edg.graph.Node.Type.CLoop;
import static upv.slicing.edg.graph.Node.Type.Value;

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

		outputFile.delete();
		try (PrintWriter writer = new PrintWriter(outputFile)) {
			writer.print(text);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
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
		final List<Node> modules = edg.getChildren(root);
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
		final List<Node> members = edg.getChildren(module);
		members.removeIf(node -> node.getType() == Node.Type.Result);

		this.funundef = false;
		for (Node member : members)
		{

			if (member.getType() == Node.Type.Routine) {
				if (this.slice != null && !this.slice.contains(member))
					continue;
				this.parseRoutine(clazz, member);
			}

			if (member.getType() != Node.Type.Routine)
			{
				List<FieldDeclaration> globalVars = parseGlobalVariable(member);
				for (FieldDeclaration globalVar : globalVars)
					clazz.addMember(globalVar);
			}

		}

		if (this.funundef)
			clazz.addMember(this.createFunundef());
	}

	@SuppressWarnings("unchecked")
	private List<FieldDeclaration> parseGlobalVariable(Node globalVariable)
	{
		final NodeList<VariableDeclarator> variableDeclarators = new NodeList<>();
		final List<Expression> variableDeclaratorExprs;
		if (globalVariable.getType() == Node.Type.Variable)
			variableDeclaratorExprs = parseDeclarationVariable(globalVariable);
		else
			// TODO: This can be a list of Expressions without any FieldDeclaration
			variableDeclaratorExprs = parseDeclaration(globalVariable);

		if (variableDeclaratorExprs.isEmpty())
			return List.of();

		assert variableDeclaratorExprs.size() == 1;
		VariableDeclarationExpr varDecExpr = (VariableDeclarationExpr) variableDeclaratorExprs.get(0);
		return List.of(new FieldDeclaration(varDecExpr.getModifiers(), varDecExpr.getVariables()));
	}

	private void parseRoutine(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final Node clause = edg.getChild(routine, 0); // If there are multiple clauses (Erlang) this is not correct
		final Node parametersNode = edg.getChild(clause, Node.Type.Parameters);
		final Node body = edg.getChild(clause, Node.Type.Body);
		final Type returnType = (Type) routine.getInfo().getInfo()[1];
		final String name = routine.getName();
		final boolean returnRequired = !(returnType instanceof VoidType) && !name.equals("<constructor>");

		context.push(new Context(routine.getType(), returnType, returnRequired));

		// Callable declaration
		final CallableDeclaration<?> callableDeclaration = name.equals("<constructor>") ? this
				.parseConstructor(clazz, routine) : this.parseMethod(clazz, routine);

		// Parameters
		final NodeWithParameters<?> nodeWithParameters = (NodeWithParameters<?>) callableDeclaration;
		final List<Node> parametersChildren = edg.getChildren(parametersNode);
		for (Node parameter : parametersChildren)
			// This would not be necessary if Result nodes were not linked to parent with marked edges in order to generate a readable .dot file
			if (parameter.getType() != Node.Type.Result)
				nodeWithParameters.addParameter(this.parseParameter(parameter));

		// Statements
		final BlockStmt blockStmt = callableDeclaration instanceof NodeWithBlockStmt ?
				((NodeWithBlockStmt<?>) callableDeclaration).getBody() :
				((NodeWithOptionalBlockStmt<?>) callableDeclaration).getBody().get();
		final List<Node> bodyChildren = edg.getChildren(body);
		final List<Statement> statements = this.parseStatements(bodyChildren, false);

		if (context.peek().returnReq)
		{
			final Expression returnExpr = generateSlicedValueExpr(returnType);
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
		@SuppressWarnings("unchecked")
		final NodeList<Modifier> modifiers = (NodeList<Modifier>) ldNodeInfo.getInfo()[0];
		final Type type = (Type) ldNodeInfo.getInfo()[1];
		final Set<Modifier.Keyword> keywords = modifiers.stream().map(Modifier::getKeyword).collect(Collectors.toSet());
		final MethodDeclaration methodDeclaration = clazz.addMethod(name, keywords.toArray(Modifier.Keyword[]::new));

		methodDeclaration.setType(type);

		return methodDeclaration;
	}
	private CallableDeclaration<?> parseConstructor(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final LDASTNodeInfo ldNodeInfo = routine.getInfo();
		@SuppressWarnings("unchecked")
		final NodeList<Modifier> modifiers = (NodeList<Modifier>) ldNodeInfo.getInfo()[0];
		final Set<Modifier.Keyword> keywords = modifiers.stream().map(Modifier::getKeyword).collect(Collectors.toSet());
		return clazz.addConstructor(keywords.toArray(Modifier.Keyword[]::new));
	}
	private Parameter parseParameter(Node parameter)
	{
		final LDASTNodeInfo ldNodeInfo = parameter.getInfo();
		final Type type = this.slice != null && !this.slice.contains(parameter) ? new ClassOrInterfaceType(
				"Object") : (Type) ldNodeInfo.getInfo()[0];
		final String name = this.slice != null && !this.slice.contains(parameter) ? "sliced" : parameter.getName();

		return new Parameter(type, name);
	}

	/**
	 * Parse to javaparser a list of statements of the EDG (only those contained in the slice).
	 * @param nodes List of statement nodes to be parsed.
	 * @param blockRequired Flag to transform some sliced nodes to code for compilation purposes.
	 * @return A list of javaparser statements.
	 */
	private List<Statement> parseStatements(List<Node> nodes, boolean blockRequired)
	{
		final List<Statement> statementsList = new LinkedList<>();

		for (Node node : nodes)
		{
			if (node.getType() == Node.Type.Result)
				continue;
			if (!blockRequired && this.slice != null && !this.slice.contains(node) &&
				!mayContainInternalSliceCode(node))
				continue;

			List<Statement> parsedList = this.parseStatement(node);
			if (!parsedList.isEmpty())
				statementsList.addAll(parsedList);
		}
		if (!nodes.isEmpty() && statementsList.isEmpty() && blockRequired)
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

			List<Statement> stmtList = this.parseStatement(node);
			if (stmtList.isEmpty())
				continue;
			statementsList.addAll(stmtList);
		}
		if (!nodes.isEmpty() && statementsList.isEmpty())
			statementsList.add(new EmptyStmt());

		return statementsList;
	}
	/**
	 * Parse to javaparser a single statement of the EDG (only the parts contained in the slice).
	 * @apiNote This method is also used to parse expressions. These expressions may result in a
	 *  list of ExpressionStmt objects when parsed.
	 * @param statement Statement node to be parsed.
	 * @return A list of javaparser statements.
	 */
	private List<Statement> parseStatement(Node statement)
	{

		if (this.slice != null && !this.slice.contains(statement) && !mayContainInternalSliceCode(statement))
			// Hay que poner returns en las calls a funciones
			// if (statement.getType() != Node.Type.Return)
				return List.of();

		final Node.Type statementType = statement.getType();
		final Function<Node, List<Statement>> parserFunc;

		switch (statementType)
		{
			case If:
				parserFunc = this::parseIf;
				break;
			case Switch:
				parserFunc = this::parseSwitch;
				break;
			case CLoop:
			case RLoop:
				parserFunc = this::parseWhile_DoWhileLoop;
				break;
			case FLoop:
				parserFunc = this::parseForLoop;
				break;
			case Return:
				parserFunc = this::parseReturn;
				break;
			case ExHandler:
				parserFunc = this::parseExHandler;
				break;
			case Throw:
				parserFunc = this::parseThrow;
				break;
			case Foreach:
				parserFunc = this::parseForeach;
				break;
			case Break:
				parserFunc = this::parseBreak;
				break;
			case Continue:
				parserFunc = this::parseContinue;
				break;
			case Call:
				boolean cond = this.isExplicitConstructor(statement);
				if (cond) {
					parserFunc = this::parseExplicitConstructorCall;
					break;
				}
			default:
				parserFunc = this::parseExpressionStmt;
				break;
		}

		return parserFunc.apply(statement);
	}

	private List<Statement> parseExpressionStmt(Node statement)
	{
		final List<Expression> parsedExprs = this.parseExpression(statement);
		return parsedExprs.stream().map(ExpressionStmt::new).collect(Collectors.toList());
	}

	/**
	 * Parse to javaparser an if-then or if-then-else statement of the EDG (only the parts contained in the slice).
	 * @param _if if-then / if-then-else statement node to be parsed.
	 * @return A list of javaparser statements containing an if-then/if-then-else statement or
	 * 		   a list of statements of the condition node, which are part of the slice.
	 */
	private List<Statement> parseIf(Node _if)
	{
		Context ctx = context.peek();
		context.push(new Context(_if.getType(), ctx.returnType, ctx.returnReq));

		final Node conditionNode = edg.getChild(_if, Node.Type.Condition);
		final Node conditionExprNode = edg.getChild(conditionNode, Node.Type.Value);
		final List<Expression> conditionExpression = this.parseExpression(conditionExprNode);

		final Node thenNode = edg.getChild(_if, Node.Type.Then);
		final List<Node> thenChildren = edg.getChildren(thenNode);
		final List<Statement> thenStatements = this.parseStatements(thenChildren, false);

		final Node _else = edg.getChild(_if, Node.Type.Else);
		final List<Node> elseChildren = edg.getChildren(_else);
		final List<Statement> elseStatements = this.parseStatements(elseChildren, false);

		if (thenStatements.isEmpty() && elseStatements.isEmpty()) {
			context.pop();
			return conditionExpression.stream().map(ExpressionStmt::new).collect(Collectors.toList());
		}

		final BlockStmt thenBlock = new BlockStmt();
		for (Statement thenStatement : thenStatements)
			thenBlock.addStatement(thenStatement);

		if (elseStatements.isEmpty()) {
			context.pop();
			return List.of(new IfStmt(conditionExpression.get(0), thenBlock, null));
		}

		final BlockStmt elseBlock = new BlockStmt();
		for (Statement elseStatement : elseStatements)
			elseBlock.addStatement(elseStatement);

		context.pop();
		assert conditionExpression.size() == 1;
		return List.of(new IfStmt(conditionExpression.get(0), thenBlock, elseBlock));
	}

	/**
	 * Parse to javaparser a switch statement of the EDG (only the parts contained in the slice).
	 * @param _switch switch statement node to be parsed.
	 * @return A list of javaparser statements containing a switch statement or
	 * 		   a list of statements of the selector node, which are part of the slice.
	 */
	private List<Statement> parseSwitch(Node _switch) // TODO: Control return required environment
	{
		// Selector
		final Node selectorNode = edg.getChild(_switch, Node.Type.Selector);
		final Node selectorExprNode = edg.getChild(selectorNode, Value);
		final List<Expression> selectorExprs = this.parseExpression(selectorExprNode);

		if (slice != null && selectorExprs.isEmpty())
			return List.of();

		// Cases
		final Node cases = edg.getChild(_switch, Node.Type.Cases);
		final List<Node> casesChildren = edg.getChildren(cases);
		final List<SwitchEntry> resultNodes = new LinkedList<>();
		for (Node child : casesChildren)
			resultNodes.addAll(this.parseCase(child));

		if (slice != null && resultNodes.isEmpty())
			return selectorExprs.stream().map(ExpressionStmt::new).collect(Collectors.toList());

		final NodeList<SwitchEntry> entries = new NodeList<>(resultNodes);

		assert selectorExprs.size() == 1;
		return List.of(new SwitchStmt(selectorExprs.get(0), entries));
	}

	/**
	 * Parse to javaparser a case statement (SwitchEntryStmt) inside a switch of the EDG (only the parts contained in the slice).
	 * @param _case case statement node to be parsed.
	 * @return A list of javaparser statements containing the case statement or
	 * 		   a list of statements with only the selectable, which are part of the slice.
	 */
	private List<SwitchEntry> parseCase(Node _case)
	{
		if (edg.getChildren(_case).size() == 1)
			return parseDefaultCase(_case);

		if (slice != null && !slice.contains(_case))
			return List.of();

		// Label
		final Node selectableNode = edg.getChild(_case, Node.Type.Selectable);
		final Node selectableExpr = edg.getChild(selectableNode, Node.Type.Value);
		final List<Expression> label = this.parseExpression(selectableExpr);

		// Statements
		final Node body = edg.getChild(_case, Node.Type.Body);
		final List<Node> bodyChildren = edg.getChildren(body);
		final List<Statement> statements = this.parseStatements(bodyChildren, false);

		final NodeList<Statement> bodyStatements = new NodeList<>(statements);

		assert label.size() == 1; // Java: this assumption breaks under Java >= 12
		return List.of(new SwitchEntry(new NodeList<>(label.get(0)), SwitchEntry.Type.STATEMENT_GROUP, bodyStatements));
	}

	private List<SwitchEntry> parseDefaultCase(Node defaultCase)
	{
		if (slice != null && !slice.contains(defaultCase))
			return List.of();

		// Statements
		final Node body = edg.getChild(defaultCase, Node.Type.Body);
		final List<Node> bodyChildren = edg.getChildren(body);
		final List<Statement> statements = this.parseStatements(bodyChildren, false);

		final NodeList<Statement> bodyStatements = new NodeList<>();
		bodyStatements.addAll(statements);

		return List.of(new SwitchEntry(new NodeList<>(), SwitchEntry.Type.STATEMENT_GROUP, bodyStatements));
	}

	/**
	 * Parse to javaparser a while/do_while statement of the EDG (only the parts contained in the slice).
	 * @param loop while/do_while statement node to be parsed.
	 * @return A list of javaparser statements containing a while/do_while statement or
	 * 		   a list of statements of the condition node of the loop, which are part of the slice.
	 */
	private List<Statement> parseWhile_DoWhileLoop(Node loop)
	{
		if (loop.getType() == CLoop) {
			Context ctx = context.peek();
			context.push(new Context(loop.getType(), ctx.returnType, ctx.returnReq));
		}
		// Condition
		final Node conditionNode = edg.getChild(loop, Node.Type.Condition);
		if (slice != null && !slice.contains(conditionNode)) {
			if (loop.getType() == CLoop)
				context.pop();
			return List.of();
		}

		final Node conditionExprNode = edg.getChild(conditionNode, Node.Type.Value);
		final List<Expression> conditionExpression = this.parseExpression(conditionExprNode);

		// Body
		final Node body = edg.getChild(loop, Node.Type.Body);
		final List<Node> bodyChildren = edg.getChildren(body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
//		if (bodyStatements.isEmpty())
//			return conditionExpression.stream().map(ExpressionStmt::new).collect(Collectors.toList());

		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);

		assert conditionExpression.size() == 1;
		if (loop.getType() == Node.Type.CLoop) {
			context.pop();
			return List.of(new WhileStmt(conditionExpression.get(0), bodyBlock));
		}
		return List.of(new DoStmt(bodyBlock, conditionExpression.get(0)));
	}

	/**
	 * Parse to javaparser a for statement of the EDG (only the parts contained in the slice).
	 * @param loop for statement node to be parsed.
	 * @return A list of javaparser statements containing a for statement or
	 * 		   a list of statements of the condition, init or both nodes of the loop that are part of the slice.
	 */
	private List<Statement> parseForLoop(Node loop)
	{
		Context ctx = context.peek();
		context.push(new Context(loop.getType(), ctx.returnType, ctx.returnReq));

		final Node initNode = edg.getChild(loop, Node.Type.Init);
		final Node conditionNode = edg.getChild(loop, Node.Type.Condition);
		final Node bodyNode = edg.getChild(loop, Node.Type.Body);
		final Node updateNode = edg.getChild(loop, Node.Type.Update);

		if (slice != null && !slice.contains(initNode) && !slice.contains(conditionNode)) {
			context.pop();
			return List.of();
		}

		// init: N expressions or 1 variable declaration with initialization (can be multiple)
		final List<Node> initChildren = edg.getChildren(initNode);
		initChildren.removeIf(node -> node.getType() == Node.Type.Result);
		final List<Expression> initExprs = this.parseExpressions(initChildren, false);

		if (slice != null && slice.contains(initNode) && !slice.contains(conditionNode)) {
			context.pop();
			return initExprs.stream().map(ExpressionStmt::new).collect(Collectors.toList());
		}

		// Condition slice part
		final Node conditionExprNode = edg.getChild(conditionNode, Node.Type.Value);
		final List<Expression> conditionExpressions = this.parseExpression(conditionExprNode);

		// Body slice part
		final List<Node> bodyChildren = edg.getChildren(bodyNode);
		bodyChildren.removeIf(node -> node.getType() == Node.Type.Result);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);

		// Update slice part
		final List<Node> updateChildren = edg.getChildren(updateNode);
		updateChildren.removeIf(node -> node.getType() == Node.Type.Result);
		final List<Expression> updateExpressions = this.parseExpressions(updateChildren, false);

//		if (slice != null && slice.contains(conditionNode) && bodyStatements.isEmpty() && updateExpressions.isEmpty())
//			return Util.join(initExprs,conditionExpressions).stream().map(ExpressionStmt::new).collect(Collectors.toList());

		// ForStmt structure is only required if any statement of the body or update is necessary in the slice
		final NodeList<Expression> initBlock = new NodeList<>();
		initBlock.addAll(initExprs);

		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);
		if (bodyBlock.isEmpty())
			bodyBlock.addStatement(new EmptyStmt());

		final NodeList<Expression> updateBlock = new NodeList<>();
		updateBlock.addAll(updateExpressions);

		context.pop();
		assert conditionExpressions.size() == 1;
		return List.of(new ForStmt(initBlock, conditionExpressions.get(0), updateBlock, bodyBlock));
	}

	/**
	 * Parse to javaparser a foreach statement of the EDG (only the parts contained in the slice).
	 * @param foreach foreach statement node to be parsed.
	 * @return A list of javaparser statements containing a foreach statement or
	 * 		   a list of statements of the iterable part that are part of the slice.
	 */
	private List<Statement> parseForeach(Node foreach)
	{
		Context ctx = context.peek();
		context.push(new Context(foreach.getType(), ctx.returnType, ctx.returnReq));

		// TODO: UPDATE AFTER DELETING ITERATOR NODE
		final Node iterator = edg.getChild(foreach, Node.Type.Iterator);
		final Node generator = edg.getChild(iterator, Node.Type.Iterator);
		final Node variableDeclaration = edg.getChild(generator, Node.Type.Variable);
		final Node iterable = edg.getChild(generator, Node.Type.Iterator);

		if (slice != null && !slice.contains(iterable)) {
			context.pop();
			return List.of();
		}

		final List<Expression> variableDeclarationExpr = this.parseExpression(variableDeclaration);
		final List<Expression> iterableExpr = this.parseExpression(iterable);

		if (slice != null && !slice.contains(variableDeclaration) && slice.contains(iterable)) {
			context.pop();
			return iterableExpr.stream().map(ExpressionStmt::new).collect(Collectors.toList());
		}

		final Node body = edg.getChild(foreach, Node.Type.Body);
		final List<Node> bodyChildren = edg.getChildren(body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);

		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);

		if(bodyBlock.isEmpty())
			bodyBlock.addStatement(new EmptyStmt());

		context.pop();
		assert variableDeclarationExpr.size() == 1 && iterableExpr.size() == 1;
		return List.of(new ForEachStmt((VariableDeclarationExpr) variableDeclarationExpr.get(0), iterableExpr.get(0), bodyBlock));
	}

	/* *** PENDING *** */
	private List<Statement> parseExHandler(Node exHandler)
	{
		assert false;	// TODO: @serperu generate code for try-catch-finally when properly implemented
		// Try
		final Node tryNode = edg.getChild(exHandler, Node.Type.Try);

		if (slice != null && !slice.contains(tryNode))
			return List.of();

		final BlockStmt tryBlock = this.parseBlockStatements(tryNode);

		// Catch
		final Node catchNode = edg.getChild(exHandler, Node.Type.Catch);
		final NodeList<CatchClause> catchClauses = (NodeList<CatchClause>) this.parseCatch(catchNode);

		// Finally
		final Node finallyNode = edg.getChild(exHandler, Node.Type.Finally);
		final BlockStmt finallyBlock = this.parseBlockStatements(finallyNode);

		if (slice != null && catchClauses.isEmpty() && finallyBlock.isEmpty())
			return tryBlock.getStatements();

		if (catchClauses.isEmpty())
			return List.of(new TryStmt(tryBlock, new NodeList<>(), finallyBlock));
		return List.of(new TryStmt(tryBlock, catchClauses, new BlockStmt()));
	}

	private List<CatchClause> parseCatch(Node _catch)
	{
		final List<Node> clauses = edg.getChildren(_catch);
		final List<CatchClause> catchClauses = new NodeList<>();
		for (Node clause : clauses)
		{
			if (this.slice != null && !this.slice.contains(clause))
				continue;

			final Node parameters = edg.getChild(clause, 0);
			final Node parameter = edg.getChild(parameters, 0);
			final Parameter parameter0 = this.parseParameter(parameter);

			final Node body = edg.getChild(clause, 2);
			final BlockStmt bodyBlock0 = this.parseBlockStatements(body);
			final BlockStmt bodyBlock = bodyBlock0 == null ? new BlockStmt() : bodyBlock0;

			catchClauses.add(new CatchClause(parameter0, bodyBlock));
		}
		return catchClauses;
	}

	private List<Statement> parseThrow(Node _throw)
	{
		final Node throwExpressionNode = edg.getChild(_throw, 0);
		final List<Expression> throwExpression = this.parseExpression(throwExpressionNode);

		return List.of(new ThrowStmt(throwExpression.get(0)));

	}

	private List<Statement> parseBreak(Node _break)
	{
		if (this.slice != null && !this.slice.contains(_break))
			return List.of();
		return List.of(new BreakStmt(null, null));
	}

	private List<Statement> parseContinue(Node _continue)
	{
		if (this.slice != null && !this.slice.contains(_continue))
			return List.of();
		return List.of(new ContinueStmt());
	}

	private List<Statement> parseReturn(Node _return)
	{
		if (this.slice != null && !this.slice.contains(_return))
		{
			Context ctx = context.peek();
			if (ctx.returnReq)
			{
				updateReturnContext();

				final Type type = ctx.returnType;
				final Expression returnExpr = generateSlicedValueExpr(type);
				return List.of(new ReturnStmt(returnExpr));
			}
		}

		final List<Node> returnChildren = edg.getChildren(_return);
		if (returnChildren.isEmpty())
			return List.of(new ReturnStmt());

		final Node returnChild = returnChildren.get(0);
		final List<Expression> returnExpression = this.parseExpression(returnChild);

		Context ctx = context.peek();
		updateReturnContext();

		if (returnExpression.isEmpty())
		{
			final Type type = ctx.returnType;
			final Expression returnExpr = generateSlicedValueExpr(type);
			return List.of(new ReturnStmt(returnExpr));
		}

		return List.of(new ReturnStmt(returnExpression.get(0)));
	}

	private BlockStmt parseBlockStatements(Node blockRoot)
	{
		final BlockStmt bodyBlock = new BlockStmt();

		if (this.slice != null && !this.slice.contains(blockRoot))
			return bodyBlock;

		final List<Node> bodyChildren = edg.getChildren(blockRoot);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);

		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);
		return bodyBlock;
	}

	/**
	 * Parse to javaparser a list of expressions of the EDG (only those contained in the slice).
	 * @param nodes List of statement nodes to be parsed.
	 * @param transformUnused Flag to transform some sliced nodes to code for compilation purposes.
	 * @return A list of javaparser expressions.
	 */
	private List<Expression> parseExpressions(List<Node> nodes, boolean transformUnused)
	{
		final List<Expression> expressionsList = new LinkedList<>();

		for (Node node : nodes)
		{
			if (node.getType() == Node.Type.Result)
				continue;
			if (!transformUnused && this.slice != null && !this.slice.contains(node) &&
				!mayContainInternalSliceCode(node))
				continue;

			List<Expression> parsedList = this.parseExpression(node);
			if (!parsedList.isEmpty())
				expressionsList.addAll(parsedList);
			if (parsedList.isEmpty() && transformUnused)
				expressionsList.add(new NullLiteralExpr()); // Null for each expression not used
		}

		return expressionsList;
	}

	/**
	 * Parse to javaparser a single expressions of the EDG (only the parts contained in the slice).
	 * @param expression Statement node to be parsed.
	 * @return A list of javaparser expressions.
	 * @throws IllegalArgumentException if the expression type is not contemplated
	 */
	private List<Expression> parseExpression(Node expression)
	{
		if (this.slice != null && !this.slice.contains(expression) && !mayContainInternalSliceCode(expression))
			return Collections.emptyList();

		final Node.Type expressionType = expression.getType();
		final Function<Node, List<Expression>> parserFunc;

		switch (expressionType)
		{
			case Equality:
				parserFunc = this::parseEquality;
				break;
			case Call:
				parserFunc = this::parseCall;
				break;
			case DataConstructor:
				parserFunc = this::parseDataConstructor;
				break;
			case Operation:
				parserFunc = this::parseOperation;
				break;
			case DataConstructorAccess:
				parserFunc = this::parseDataConstructorAccess;
				break;
			case FieldAccess:
				parserFunc = this::parseFieldAccess;
				break;
			case Enclosed:
				parserFunc = this::parseEnclosed;
				break;
			case If:
				parserFunc = this::parseTernary;
				break;
			case Variable:
				parserFunc = this::parseVariable;
				break;
			case Literal:
				parserFunc = this::parseLiteral;
				break;
			// ADDED
			case TypeCheck:
				parserFunc = this::parseInstanceOf;
				break;
			case TypeTransformation:
				parserFunc = this::parseCastExpr;
				break;
			case Reference:
				parserFunc = this::parseReference;
				break;
			default:
				throw new IllegalArgumentException("Unexpected expression type found: " + expressionType);
		}
		return parserFunc.apply(expression);
	}


	/**
	 * Parse to javaparser an Equality node of the EDG (only the parts contained in the slice).
	 * @param equality Equality node to be parsed.
	 * @return A list of javaparser expressions contained in the slice.
	 */
	private List<Expression> parseEquality(Node equality)
	{
		final Node target = edg.getChild(equality, Node.Type.Pattern);
		final LDASTNodeInfo ldNodeInfo = target.getInfo();
		final Object[] info = ldNodeInfo.getInfo();

		if (target.getType() == Node.Type.Variable)
		{
			Variable vi = (Variable) target;
			if (vi.isDeclaration())
				return this.parseDeclaration(equality);
			return this.parseDefinition(equality);
		}
		if (info == null || info.length == 0)
			return this.parseDefinition(equality);

		return this.parseDeclaration(equality);
	}

	/**
	 * Parse to javaparser an Equality declaration node of the EDG (only the parts contained in the slice).
	 * @param declaration Declaration node to be parsed.
	 * @return A list of javaparser expressions inside the given node contained in the slice.
	 */
	@SuppressWarnings("unchecked")
	private List<Expression> parseDeclaration(Node declaration)
	{
		final Node variableNode = edg.getChild(declaration, Node.Type.Pattern);
		final Node initializerNode = edg.getChild(declaration, Value);

		final List<Expression> initializerExprs = this.parseExpression(initializerNode);
		if (slice != null && initializerExprs.isEmpty())
		{
			if (slice.contains(variableNode))
				return parseDeclarationNonEmpty(variableNode);
			return List.of();
		}

		if (slice != null && !slice.contains(variableNode))
			return initializerExprs;

		if (slice != null && !slice.contains(declaration))
			return Util.join(initializerExprs,parseDeclarationNonEmpty(variableNode));

		final LDASTNodeInfo ldNodeInfo = variableNode.getInfo();
		final Type type = (Type) ldNodeInfo.getInfo()[1];
		final String name = variableNode.getName();
		final NodeList<Modifier> modifiers = (NodeList<Modifier>) ldNodeInfo.getInfo()[0];

		assert initializerExprs.size() == 1;
		VariableDeclarator variableDeclarator = new VariableDeclarator(type, name, initializerExprs.get(0));

		final NodeList<VariableDeclarator> variableDeclarators = new NodeList<>();
		variableDeclarators.add(variableDeclarator);

		return List.of(new VariableDeclarationExpr(new NodeList<>(modifiers), variableDeclarators));
	}
	/**
	 * Parse to javaparser a Variable declaration node of the EDG (only the parts contained in the slice).
	 * @param declarationVariable Declaration node to be parsed.
	 * @return A list of javaparser expressions inside the given node contained in the slice.
	 */
	private List<Expression> parseDeclarationVariable(Node declarationVariable)
	{
		final LDASTNodeInfo ldNodeInfo = declarationVariable.getInfo();
		final Type type = (Type) ldNodeInfo.getInfo()[1];
		final String name = declarationVariable.getName();
		@SuppressWarnings("unchecked")
		final NodeList<Modifier> modifiers = (NodeList<Modifier>) ldNodeInfo.getInfo()[0];

		final NodeList<VariableDeclarator> variableDeclarator = new NodeList<>();
		if (slice != null && !slice.contains(declarationVariable))
			return List.of();

		variableDeclarator.add(new VariableDeclarator(type, name));
		return List.of(new VariableDeclarationExpr(new NodeList<>(modifiers), variableDeclarator));
	}
	/**
	 * Parse to javaparser a Variable declaration node of the EDG (only the parts contained in the slice).
	 * @param declaration Declaration node to be parsed.
	 * @return A list of javaparser expressions inside the given node contained in the slice.
	 * @apiNote This method parses variable declaration nodes and returns the variable declaration if the variable
	 * 			type is Primitive Type or an initialization with the empty constructor if the variable type is a
	 * 			ClassOrInterfaceType.
	 * 			The produced code may not compile if the ClassOrInterfaceType has no empty constructor.
	 */
	private List<Expression> parseDeclarationNonEmpty(Node declaration)
	{
		final LDASTNodeInfo ldNodeInfo = declaration.getInfo();
		final Type type = (Type) ldNodeInfo.getInfo()[1];
		final String name = declaration.getName();
		final NodeList<Modifier> modifiers = (NodeList<Modifier>) ldNodeInfo.getInfo()[0];

		final NodeList<VariableDeclarator> variableDeclarator = new NodeList<>();
		// TODO: This behaviour will be no longer needed after object flow dependence and total definition dependence
		if (type instanceof PrimitiveType)
			variableDeclarator.add(new VariableDeclarator(type, name));
		else
		{
			final Expression initializer = new ObjectCreationExpr(null, (ClassOrInterfaceType) type, new NodeList<>());
			variableDeclarator.add(new VariableDeclarator(type, name, initializer));
		}
		return List.of(new VariableDeclarationExpr(new NodeList<>(modifiers), variableDeclarator));
	}


	/**
	 * Parse to javaparser a definition node of the EDG (only the parts contained in the slice).
	 * @param definition Definition node to be parsed.
	 * @return A list of javaparser expressions inside the given node contained in the slice.
	 */
	private List<Expression> parseDefinition(Node definition)
	{
		if (slice != null && !slice.contains(definition))
			return this.parseExpression(edg.getChild(definition, Value));

		final Node target = edg.getChild(definition, Node.Type.Pattern);
		final Node value = edg.getChild(definition, Value);

		final List<Expression> targetExprs = this.parseExpression(target);
		final List<Expression> valueExprs = this.parseExpression(value);

		if (!targetExprs.isEmpty() && !valueExprs.isEmpty())
			return List.of(new AssignExpr(targetExprs.get(0), valueExprs.get(0), AssignExpr.Operator.ASSIGN));
		if (!targetExprs.isEmpty())
			return targetExprs;
		return valueExprs;
	}

	/**
	 * Parse to javaparser a dataConstructor node of the EDG (only the parts contained in the slice).
	 * Also translate to "null" all the nodes in the initialization part not needed in the slice.
	 * Example:
	 * 		dataContructor code  => int[2] = {1,2};
	 * 		dataContructor slice => int[2] = {1,null};
	 * @param dataConstructor DataConstructor node to be parsed.
	 * @return A list of javaparser expressions inside the given node contained in the slice.
	 */
	private List<Expression> parseDataConstructor(Node dataConstructor)
	{
		final List<Node> elements = edg.getChildren(dataConstructor);
		elements.removeIf(node -> node.getType() == Node.Type.Result);

		if (slice != null && !slice.contains(dataConstructor))
			return this.parseExpressions(elements, false);

		final List<Expression> expressions = this.parseExpressions(elements, true);
		return List.of(new ArrayInitializerExpr(NodeList.nodeList(expressions)));
	}

	/**
	 * Parse to javaparser a call node of the EDG (only the parts contained in the slice).
	 * @param call Call node to be parsed.
	 * @return A list of javaparser expressions of the given node contained in the slice.
	 */
	private List<Expression> parseCall(Node call) {
		/*
		 * Possibilities:
		 * 	1) One/Some args needed
		 * 	2) Only scope needed
		 * 	3) 1) + 2)
		 * 	4) Full call needed (may exclude some arguments)
		 * 	5) Object Creation Call
		 * 	6) Array Creation Call
		*/
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node scope = edg.getChild(callee, Node.Type.Scope);
		final Node args = edg.getChild(call, Node.Type.Arguments);

		List<Expression> scopeExpr = new LinkedList<>();
		ClassOrInterfaceType scopeType = null;
		// Case 1
		if (this.slice != null && !this.slice.contains(callee) && !this.slice.contains(scope))
			return this.parseArguments(args, false);

		final Node name = edg.getChild(callee, Node.Type.Name);

		if (this.slice != null && this.slice.contains(scope))
		{
			final Node scopeValue = edg.getChild(scope, Value);
			if (scopeValue.getType() == Node.Type.Type)
				scopeType = (ClassOrInterfaceType) this.parseType(scopeValue);
			else
				scopeExpr = this.parseExpression(scopeValue);

			// Case 2
			if (!this.slice.contains(name) && !this.slice.contains(args))
				return scopeExpr;
		}

		final NodeList<Expression> argumentsList = new NodeList<>();
		argumentsList.addAll(this.parseArguments(args, true));

		// Case 3
		if (this.slice == null || !this.slice.contains(name))
			return Util.join(scopeExpr, argumentsList);

		final Node nameValue = edg.getChild(name, Value);
		final String nameText = nameValue.getName();

		switch(nameText)
		{
			case "<constructor>": // Case 5
				return List.of(
						new ObjectCreationExpr(
								scopeExpr.isEmpty() ? null : scopeExpr.get(0),
								scopeType,
								argumentsList));

			case "<arrayConstructor>": // Case 6
				return List.of(
						new ArrayCreationExpr(scopeType,
								(NodeList<ArrayCreationLevel>) edg.getChild(name, 0).getInfo().getInfo()[0], // TODO: MMMM... (¬.¬)
								null));
			default: // Case 4
				return List.of(
						new MethodCallExpr(scopeExpr.isEmpty() ? null : scopeExpr.get(0),
						new SimpleName(nameText), argumentsList));
		}
	}

	private List<Statement> parseExplicitConstructorCall(Node call){
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node scope = edg.getChild(callee, Node.Type.Scope);
		final Node args = edg.getChild(call, Node.Type.Arguments);

		final NodeList<Expression> argumentsList = new NodeList<>();
		argumentsList.addAll(this.parseArguments(args, true));

		final Node scopeValue = edg.getChild(scope, Value);
		if (scopeValue.getName().equals("super"))
			return List.of(new ExplicitConstructorInvocationStmt(false, null, argumentsList));
		return List.of(new ExplicitConstructorInvocationStmt(true, null, argumentsList));
	}


	/**
	 * Parse to javaparser the arguments node of a call in the EDG (only the parts contained in the slice).
	 * @param argsNode Arguments node to be parsed.
	 * @param transformUnused Flag that decides if the arguments not used in the slice
	 *                        need to be converted to null for compilation purposes.
	 * @return A list of javaparser expressions with only the arguments contained in the slice.
	 */
	private List<Expression> parseArguments(Node argsNode, boolean transformUnused)
	{
		if (this.slice != null && !this.slice.contains(argsNode) && !transformUnused)
			return List.of();

		final List<Node> argumentsChildren = edg.getChildren(argsNode);
		argumentsChildren.removeIf(node -> node.getType() == Node.Type.Result);

		final List<Expression> argExpressions = new LinkedList<>();
		if (transformUnused) {
			for (Node argument : argumentsChildren){
				if (slice.contains(argument))
					argExpressions.addAll(parseExpression(argument));
				else {
					Set<Edge> inputEdges = edg.getEdges(edg.getResFromNode(argument),
							LAST.Direction.Forwards, Edge.Type.Input);
					if (inputEdges.isEmpty()) // TODO: generar un argumento del mismo tipo que lo que haya
						return this.parseExpressions(argumentsChildren, true);
					final Node target = edg.getNodeFromRes(edg.getEdgeTarget(inputEdges.iterator().next()));
					String type = ((Variable) target).getStaticType();
					argExpressions.add(this.createDefaultTypeByString(type));
				}
			}
			return argExpressions;
		}

		return this.parseExpressions(argumentsChildren, false); // Discard sliced arguments
	}

	/**
	 * Parse to javaparser an operation node of the EDG (only the parts contained in the slice).
	 * @param operation Operation node to be parsed.
	 * @return A list of javaparser expressions of the given node contained in the slice.
	 * @throws IllegalStateException The arity of the operation is greater that 2
	 */
	private List<Expression> parseOperation(Node operation)
	{
		final String sign = operation.getName();
		final List<Node> operands = edg.getChildren(operation);
		operands.removeIf(node -> node.getType() == Node.Type.Result);
		final Node leftNode = operands.get(0);
		final List<Expression> leftExpression = this.parseExpression(leftNode);
		switch (operands.size())
		{
			case 1:
				final UnaryExpr.Operator[] unaryOperators = UnaryExpr.Operator.values();
				final boolean isPostfix = (boolean) operation.getInfo().getInfo()[0];
				final Printable unaryOperator = this.getOperator(unaryOperators, sign, isPostfix);
				if (!leftExpression.isEmpty())
					return List.of(
							new UnaryExpr(leftExpression.get(0), (UnaryExpr.Operator) unaryOperator));
				return Collections.emptyList();
			case 2:
				final Printable[] binaryOperators = BinaryExpr.Operator.values();
				final Printable binaryOperator0 = this.getOperator(binaryOperators, sign);
				final BinaryExpr.Operator binaryOperator = (BinaryExpr.Operator) binaryOperator0;

				final Node rightNode = operands.get(1);
				final List<Expression> rightExpression = this.parseExpression(rightNode);

				if (slice != null && slice.contains(operation))
					return List.of(new BinaryExpr(leftExpression.get(0), rightExpression.get(0), binaryOperator));

				if (slice != null && !leftExpression.isEmpty())
				{
					if (!rightExpression.isEmpty())
						return Util.join(leftExpression,rightExpression);
					return leftExpression;
				}
				return rightExpression;

			default:
				throw new IllegalStateException("Operation arity not contemplated: " + operands.size());
		}
	}

	/**
	 * Parse to javaparser a parenthesis node of the EDG (only the parts contained in the slice).
	 * @param enclosed Parenthesis node containing the expression to be parsed.
	 * @return A list of javaparser expressions of the given node contained in the slice.
	 */
	private List<Expression> parseEnclosed(Node enclosed)
	{
		final Node exprNode = edg.getChild(enclosed, Node.Type.Value);
		List<Expression> expressions = parseExpression(exprNode);

		if (slice != null && !slice.contains(enclosed))
			return expressions;

		assert expressions.size() == 1;
		return List.of(new EnclosedExpr(expressions.get(0)));
	}

	/**
	 * Parse to javaparser a reference node of the EDG (this or super).
	 * @param reference Reference node to be parsed.
	 * @return A list of javaparser expressions with the Expression representing the reference or an empty list.
	 * @throws IllegalArgumentException Unexpected reference name found
	 */
	private List<Expression> parseReference(Node reference)
	{
		if (slice != null && !slice.contains(reference))
			return List.of();

		final String value = reference.getName();

		switch (value)
		{
			case "super":
				return List.of(new SuperExpr());
			case "this":
				return List.of(new ThisExpr());
			default:
				throw new IllegalArgumentException("Unexpected reference name found " + value);
		}
	}

	/**
	 * Parse to javaparser a TypeCheck node of the EDG (only the parts contained in the slice).
	 * @param instanceOf TypeCheck node to be parsed.
	 * @return A list of javaparser expressions of the given node contained in the slice.
	 */
	private List<Expression> parseInstanceOf(Node instanceOf)
	{
		final Node expressionNode = edg.getChild(instanceOf, Node.Type.Variable);
		final List<Expression> instanceExpr = this.parseExpression(expressionNode);

		if (instanceExpr.isEmpty())
			return List.of();

		final Node typeNode = edg.getChild(instanceOf, Node.Type.Type);
		if (slice != null && !slice.contains(typeNode))
			return instanceExpr;

		final Type instanceType = this.parseType(typeNode);
		if (instanceType instanceof ClassOrInterfaceType)
			return List.of(new InstanceOfExpr(instanceExpr.get(0), (ClassOrInterfaceType) instanceType));

		return List.of(new InstanceOfExpr(instanceExpr.get(0), (ArrayType) instanceType));
	}
	/**
	 * Parse to javaparser a Cast Expression node of the EDG (only the parts contained in the slice).
	 * @param cast Cast Expression node to be parsed.
	 * @return A list of javaparser expressions of the given node contained in the slice.
	 */
	private List<Expression> parseCastExpr(Node cast)
	{
		final Node typeNode = edg.getChild(cast, Node.Type.Type);
		final Node expressionNode = edg.getChild(cast, Node.Type.Variable);

		final List<Expression> castExpr = this.parseExpression(expressionNode);
		if (castExpr.isEmpty())
			return List.of();

		if (slice != null && !slice.contains(typeNode))
			return castExpr;

		final Type castType = this.parseType(typeNode);
		final CastExpr castResult = new CastExpr(castType, castExpr.get(0));

		if ((boolean) cast.getInfo().getInfo()[0])
			return List.of(new EnclosedExpr(castResult));
		return List.of(castResult);
	}

	/**
	 * Extracts the represented type from a Type node or if the Type node is in the slice
	 * @param type Cast Expression node to be parsed.
	 * @return The javaparser Type of the given node or "Object" if not in the slice.
	 */
	private Type parseType(Node type)
	{
		if (type.getInfo().getInfo().length != 0)
		{
			if (this.slice != null && !this.slice.contains(type))
				return new PrimitiveType();

			// TODO: Comprobar esta afirmacion tan gratuita
			// DIRIA QUE ESTO NUNCA SE EJECUTA
			Object info = type.getInfo().getInfo()[0];
			if (info.equals("arrayType"))
				return new ArrayType(new ClassOrInterfaceType(type.getName()));
			if (info instanceof PrimitiveType.Primitive)
				return new PrimitiveType((Primitive) info);
			return new PrimitiveType();

		}
		if (this.slice != null && !this.slice.contains(type))
			return new ClassOrInterfaceType("Object");
		return new ClassOrInterfaceType(type.getName());
	}

	/**
	 * Parse to javaparser a DataConstructorAccess node of the EDG (only the parts contained in the slice).
	 * @param dataConstructorAccess DataConstructorAccess node to be parsed.
	 * @return A list of javaparser expressions inside the given node contained in the slice.
	 */
	private List<Expression> parseDataConstructorAccess(Node dataConstructorAccess)
	{
		final Node dataConstructorNode = edg.getChild(dataConstructorAccess, Node.Type.Variable);
		final Node accessNode = edg.getChild(dataConstructorAccess, Node.Type.Index);

		final List<Expression> dataConstructorExpr = this.parseExpression(dataConstructorNode);
		final List<Expression> accessExpr = this.parseExpression(accessNode);

		if (dataConstructorExpr.isEmpty())
			return accessExpr;
		if (accessExpr.isEmpty())
			return dataConstructorExpr;

		if (this.slice != null && !slice.contains(dataConstructorAccess))
			return Util.join(accessExpr, dataConstructorExpr);
		return List.of(new ArrayAccessExpr(dataConstructorExpr.get(0), accessExpr.get(0)));

	}

	/**
	 * Parse to javaparser a FieldAccess node of the EDG (only the parts contained in the slice).
	 * @param fieldAccess FieldAccess node to be parsed.
	 * @return A list of javaparser expressions inside the given node contained in the slice.
	 */
	private List<Expression> parseFieldAccess(Node fieldAccess)
	{
		final Node scope = edg.getChild(fieldAccess, Node.Type.Variable);
		final Node name = edg.getChild(fieldAccess, Node.Type.Index);

		final List<Expression> scopeExpr = this.parseExpression(scope);
		final List<Expression> nameExpr = this.parseExpression(name);

		if (scopeExpr.isEmpty())
			return nameExpr;
		if (nameExpr.isEmpty())
			return scopeExpr;
		if (this.slice != null && !slice.contains(fieldAccess))
			return Util.join(scopeExpr, nameExpr);
		return List.of(new FieldAccessExpr(scopeExpr.get(0), nameExpr.get(0).toString()));

	}

	/**
	 * Parse to javaparser a Ternary Expression node of the EDG (only the parts contained in the slice).
	 * @param _if Ternary Expression node to be parsed.
	 * @return A list of javaparser expressions inside the given node contained in the slice
	 * 		  (the whole Ternary Expression accordingly parsed or a list of expressions in the condition part).
	 */
	private List<Expression> parseTernary(Node _if)
	{
		final Node condition = edg.getChild(_if, Node.Type.Condition);
		final Node conditionExprNode = edg.getChild(condition, Value);

		// TODO: Any expression in the slice inside the ternaryExpr must include the Condition node in the slice
		if (slice != null && !slice.contains(conditionExprNode) && !mayContainInternalSliceCode(conditionExprNode))
			return List.of();

		final Node thenNode = edg.getChild(_if, Node.Type.Then);
		final List<Node> thenExprNodes = edg.getChildren(thenNode);
		thenExprNodes.removeIf(n -> n.getType() == Node.Type.Result);

		final Node elseNode = edg.getChild(_if, Node.Type.Else);
		final List<Node> elseExprNodes = edg.getChildren(elseNode);
		elseExprNodes.removeIf(n -> n.getType() == Node.Type.Result);

		final List<Expression>  conditionExpressions = this.parseExpression(conditionExprNode);
		// TODO: These two calls MUST only return ZERO or ONE expressions
		final List<Expression> thenExpressions = this.parseExpressions(thenExprNodes, false);
		final List<Expression> elseExpressions = this.parseExpressions(elseExprNodes, false);

		if (thenExpressions.isEmpty() && elseExpressions.isEmpty())
			return conditionExpressions;

		if (thenExpressions.isEmpty())
			return List.of(new ConditionalExpr(
							conditionExpressions.get(0),
							new NullLiteralExpr(),
							elseExpressions.get(0)));
		if (elseExpressions.isEmpty())
			return List.of(new ConditionalExpr(
							conditionExpressions.get(0),
							thenExpressions.get(0),
							new NullLiteralExpr()));

		return List.of(new ConditionalExpr(
						conditionExpressions.get(0),
						thenExpressions.get(0),
						elseExpressions.get(0)));
	}

	/**
	 * Parse to javaparser a variable node of the EDG (if contained in the slice).
	 * @param variable Variable node to be parsed.
	 * @return A list of javaparser expressions with the variable Expression or empty.
	 */
	private List<Expression> parseVariable(Node variable)
	{
		if (slice != null && !slice.contains(variable))
			return List.of();

		final Variable info = (Variable) variable;
		if (info.isDeclaration())
			return this.parseDeclarationVariable(variable);

		final String value = info.getName();
		return List.of(new NameExpr(value));
	}
	/**
	 * Parse to javaparser a literal node of the EDG (if contained in the slice).
	 * @param literal Literal node to be parsed.
	 * @return A list of javaparser literal with the correspondent Literal expression or empty.
	 */
	private List<Expression> parseLiteral(Node literal)
	{
		if (slice != null && !slice.contains(literal))
			return List.of();

		final LDASTNodeInfo ldNodeInfo = literal.getInfo();
		final String value = literal.getName();
		final String construction = ldNodeInfo.getConstruction();

		return List.of(createTypeByString(construction, value));
	}

	private Expression createTypeByString(String construction, String value)
	{
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
			case "name":
				return new NameExpr(value);
			case "object creation":
			case "array creation":
				return null; // TODO: CAMBIAR ESTE NULL QUE PETA TODO
			default:
				return new NullLiteralExpr();
		}
	}

	private Expression createDefaultTypeByString(String type)
	{
		switch (type)
		{
			case "boolean":
				return new BooleanLiteralExpr(Boolean.parseBoolean("true"));
			case "char":
				return new CharLiteralExpr(' ');
			case "double":
				return new DoubleLiteralExpr(0);
			case "int":
				return new IntegerLiteralExpr(0);
			case "long":
				return new LongLiteralExpr(0);
			case "String":
				return new StringLiteralExpr("sliced");
			case "object creation":
			case "array creation": // TODO: TREAT THESE TYPES ACCORDINGLY
				throw new RuntimeException("Returning a null also returns in a broken slicer :/");
			default:
				return new ObjectCreationExpr(null, new ClassOrInterfaceType(type), new NodeList<>());
		}
	}

//	/**
//	 * Parse to javaparser a variable declaration node of the EDG (if contained in the slice).
//	 * @param declaration Declaration node to be parsed.
//	 * @return A javaparser VariableDeclarator item considering which parts of it are in the slice.
//	 */
//	private List<VariableDeclarator> getVariableDeclarator(Node declaration)
//	{
//		final Node variableNode = EDGTraverser.getChild(edg, declaration, Node.Type.Pattern);
//		final Node initializerNode = EDGTraverser.getChild(edg, declaration, Value);
//
//		final LDASTNodeInfo ldNodeInfo = variableNode.getInfo();
//		final Type type = (Type) ldNodeInfo.getInfo()[1];
//		final String name = variableNode.getName();
//
//		if (this.slice != null && !this.slice.contains(initializerNode) && !mayContentInternalSliceCode(initializerNode))
//		{
//			if (type instanceof PrimitiveType)
//				return new VariableDeclarator(type, name);
//
//			final Expression initializer = new ObjectCreationExpr(null, (ClassOrInterfaceType) type, new NodeList<>());
//			return new VariableDeclarator(type, name, initializer);
//		}
//
//		final Expression initializerExpr = this.parseExpression(initializerNode).get(0);
//		return new VariableDeclarator(type, name, initializerExpr);
//	}


	private MethodDeclaration createFunundef()
	{
		final NodeList<Modifier> modifiers = new NodeList<>(Modifier.privateModifier(), Modifier.staticModifier());
		final MethodDeclaration funundef;
		final NodeList<Parameter> parameters = new NodeList<>();

		parameters.add(new Parameter(new ClassOrInterfaceType("Object..."), "params"));
		funundef = new MethodDeclaration(modifiers, "funundef", new VoidType(), parameters);

		return funundef;
	}

	private Printable getOperator(Printable[] operators, String sign)
	{
		for (Printable operator : operators)
			if (operator.asString().equals(sign))
				return operator;
		throw new IllegalArgumentException("Invalid operator: " + sign);
	}

	private Printable getOperator(UnaryExpr.Operator[] operators, String sign, Boolean isPostfix)
	{
		for (UnaryExpr.Operator operator : operators)
			if (operator.asString().equals(sign) && operator.isPostfix() == isPostfix)
				return operator;
		throw new IllegalArgumentException("Invalid operator: " + sign);
	}

	private Expression generateSlicedValueExpr(Type type)
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

	private boolean mayContainInternalSliceCode(Node node)
	{
		switch (node.getType())
		{
			case Operation:
			case Equality:
			case Call:
			case Condition:
			case If:
			case CLoop:
			case RLoop:
			case Foreach:
			case FLoop:
			case Switch:
			case Selector:
			case Enclosed:
			case TypeTransformation:
			case TypeCheck:
			case Generator:
			case Iterator:
			case DataConstructor:
			case DataConstructorAccess:
				return true;
			default:
				return false;
		}
	}
	private boolean isExplicitConstructor(Node call){
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node scope = edg.getChild(callee, Node.Type.Scope);
		final Node name = edg.getChild(callee, Node.Type.Name);

		final Node scopeValue = edg.getChild(scope, Value);
		final Node nameValue = edg.getChild(name, Value);

		return nameValue.getName().equals("<constructor>") &&
				(scopeValue.getName().equals("super")|| scopeValue.getName().equals("this"));
	}
}
