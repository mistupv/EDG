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
import upv.slicing.edg.graph.NodeInfo;
import upv.slicing.edg.graph.VariableInfo;
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
	public static void createJavaFile(File outputFile, EDG edg, List<Node> slice)
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
	private final List<Node> slice;
	private boolean funundef = false;
	//	private boolean returnRequired = false;
	private final Stack<Context> context = new Stack<Context>();

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
	@SuppressWarnings("unchecked")
	private void parseModule(CompilationUnit cu, Node module)
	{
		final String moduleName = module.getData().getName();
		final ClassOrInterfaceDeclaration clazz = cu.addClass(moduleName);

		//NodeList<ClassOrInterfaceType> extendedTypes = (NodeList<ClassOrInterfaceType>) module.getData().getInfo().getInfo()[0];
		// TODO ENCONTRAR UNA SOLUCION MEJOR
		String extendedTypes0 = (String) module.getData().getInfo()
											   .getInfo()[0]; // Si hay mas de un extends no funcionara, solo vale para extends de 1 clase
		NodeList<ClassOrInterfaceType> extendedTypes = new NodeList<ClassOrInterfaceType>();
		if (!extendedTypes0.equals(""))
			extendedTypes.add(new ClassOrInterfaceType(extendedTypes0));

		NodeList<ClassOrInterfaceType> implementedTypes = (NodeList<ClassOrInterfaceType>) module.getData().getInfo()
																								 .getInfo()[1];

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

	@SuppressWarnings("unchecked")
	private void parseGlobalVariable(ClassOrInterfaceDeclaration clazz, Node globalVariable)
	{
		final NodeList<VariableDeclarator> variableDeclarators = new NodeList<VariableDeclarator>();
		final NodeInfo.Type nodeType = globalVariable.getData().getType();
		final Node expression =
				nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(globalVariable, 0) : globalVariable;
		final VariableDeclarator variableDeclarator = this.getVariableDeclarator(expression);
		final Node variable = (expression.getData().getType() == NodeInfo.Type.Equality) ?
				EDGTraverser.getChild(EDGTraverser.getChild(expression, 0), 0) : expression;
		final LDASTNodeInfo ldNodeInfo = variable.getData().getInfo();
		final EnumSet<Modifier> modifiers = (EnumSet<Modifier>) ldNodeInfo.getInfo()[0];

		variableDeclarators.add(variableDeclarator);
		clazz.addMember(new FieldDeclaration(modifiers, variableDeclarators));
	}

	private void parseRoutine(ClassOrInterfaceDeclaration clazz, Node routine)
	{
		final Node clause = EDGTraverser
				.getChild(routine, 0); // If there are multiple clauses (Erlang) this is not correct
		final Node parametersNode = EDGTraverser.getChild(clause, NodeInfo.Type.Parameters);
		final Node body = EDGTraverser.getChild(clause, NodeInfo.Type.Body);
		final Type returnType = (Type) routine.getData().getInfo().getInfo()[1];
		final boolean returnRequired = !(returnType instanceof VoidType);

		context.push(new Context(routine.getData().getType(), returnType, returnRequired));

		// Callable declaration
		final String name = routine.getData().getName();
		final CallableDeclaration<?> callableDeclaration = name.equals("<constructor>") ? this
				.parseConstructor(clazz, routine) : this.parseMethod(clazz, routine);

		// Parameters
		final NodeWithParameters<?> nodeWithParameters = (NodeWithParameters<?>) callableDeclaration;
		final List<Node> parametersChildren = EDGTraverser.getChildren(parametersNode);
		for (Node parameter : parametersChildren)
			// This would not be necessary if Result nodes were not linked to parent with marked edges in order to generate a readable .dot file
			if (parameter.getData().getType() != NodeInfo.Type.Result)
				nodeWithParameters.addParameter(this.parseParameter(parameter));

		// Statements
		final BlockStmt blockStmt = callableDeclaration instanceof NodeWithBlockStmt ? ((NodeWithBlockStmt<?>) callableDeclaration)
				.getBody() : ((NodeWithOptionalBlockStmt<?>) callableDeclaration).getBody().get();
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
//		final List<Statement> statements = this.parseStatements(bodyChildren, false);
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
		final NodeInfo nodeInfo = parameter.getData();
		final LDASTNodeInfo ldNodeInfo = nodeInfo.getInfo();
		final Type type = this.slice != null && !this.slice.contains(parameter) ? new ClassOrInterfaceType(
				"Object") : (Type) ldNodeInfo.getInfo()[0];
		final String name = this.slice != null && !this.slice.contains(parameter) ? "sliced" : nodeInfo.getName();

		return new Parameter(type, name);
	}

	// Statement
	private List<Statement> parseStatements(List<Node> nodes, boolean transformUnused)
	{
		final List<Statement> statementsList = new LinkedList<Statement>();

		for (Node node : nodes)
		{
			if (node.getData().getType() == NodeInfo.Type.Result)
				continue;
			if (!transformUnused && this.slice != null && !this.slice.contains(node) &&
				!mayContentInternalSliceCode(node))
				continue;

			Statement stmt = this.parseStatement(node);
			if (stmt == null)
				continue;
			statementsList.add(stmt);
		}
		if (!nodes.isEmpty() && statementsList.isEmpty())
			statementsList.add(new EmptyStmt());

		return statementsList;
	}

	private List<Statement> parseStatements(List<Node> nodes, boolean transformUnused, boolean returnReq)
	{
		final List<Statement> statementsList = new LinkedList<Statement>();

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

	private Statement parseStatement(Node statement)
	{

		if (this.slice != null && !this.slice.contains(statement) && !mayContentInternalSliceCode(statement))
			// Hay que poner returns en las calls a funciones
			if (statement.getData().getType() != NodeInfo.Type.Return)
				return new EmptyStmt();

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
			case Throw:
				return this.parseThrow(statement);
			case Foreach:
				return this.parseForeach(statement);
			default:
				Expression e = this.parseExpression(statement);
				if (e == null)
					return null;
				return new ExpressionStmt(e);
		}
	}
	private Statement parseIf(Node _if)
	{
		// Condition
		final Node conditionNode = EDGTraverser.getChild(_if, NodeInfo.Type.Condition);
		final Node condition = EDGTraverser.getChild(conditionNode, 0);
		final Expression conditionExpression = this.parseExpression(condition);

		// Then
		final Node then = EDGTraverser.getChild(_if, NodeInfo.Type.Then);
		final List<Node> thenChildren = EDGTraverser.getChildren(then);
		final List<Statement> thenStatements = this.parseStatements(thenChildren, false);
		final BlockStmt thenBlock = new BlockStmt();
		for (Statement thenStatement : thenStatements)
			thenBlock.addStatement(thenStatement);

		// Else
		final Node _else = EDGTraverser.getChild(_if, NodeInfo.Type.Else);
		final List<Node> elseChildren = EDGTraverser.getChildren(_else);
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
		final Node selectors = EDGTraverser.getChild(_switch, NodeInfo.Type.Selector);
		final Node selectorNode = EDGTraverser.getChild(selectors, 0);
		final Expression selector = this.parseExpression(selectorNode);

		// Cases
		final Node cases = EDGTraverser.getChild(_switch, NodeInfo.Type.Cases);
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
		final Node selectable = EDGTraverser.getChild(_case, NodeInfo.Type.Selectable);
		final List<Node> selectablesChildren = EDGTraverser.getChildren(selectable);
		final Node selectableNode = selectablesChildren.get(0);
		final Expression label = this.parseExpression(selectableNode);

		// Statements
		final Node body = EDGTraverser.getChild(_case, NodeInfo.Type.Body);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final NodeList<Statement> statements = new NodeList<Statement>();
		final List<Statement> statements0 = this.parseStatements(bodyChildren, false);
		for (Statement statement : statements0)
			statements.add(statement);

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
//final Node body = EDGTraverserNew.getChild(loop, 1);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final List<Statement> bodyStatements = this.parseStatements(bodyChildren, false);
		final BlockStmt bodyBlock = new BlockStmt();
		for (Statement bodyStatement : bodyStatements)
			bodyBlock.addStatement(bodyStatement);

		// Condition
		final Node condition = EDGTraverser.getChild(loop, 1);
//final Node condition = EDGTraverserNew.getChild(loop, 0);
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

	private Statement parseForeach(Node foreach)
	{
		final Node iterator = EDGTraverser.getChild(foreach, 0);
		final Node generator = EDGTraverser.getChild(iterator, 0);
		final Node variableDeclaration = EDGTraverser.getChild(generator, 0);
		final Node iterable = EDGTraverser.getChild(generator, 1);
		final Expression variableDeclarationExpr = this.parseExpression(variableDeclaration);
		final Expression iterableExpr = this.parseExpression(iterable);

		final Node body = EDGTraverser.getChild(foreach, 1);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
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
		final Node tryNode = EDGTraverser.getChild(exHandler, 0);
		final BlockStmt tryBlock0 = this.parseBlockStatements(tryNode);
		final BlockStmt tryBlock = tryBlock0 == null ? new BlockStmt() : tryBlock0;

		// Catch
		final Node catchNode = EDGTraverser.getChild(exHandler, 1);
		final NodeList<CatchClause> catchClauses = (NodeList<CatchClause>) this.parseCatch(catchNode);
		if (catchClauses.isEmpty())
			catchClauses
					.add(new CatchClause(new Parameter(new ClassOrInterfaceType("Exception"), new SimpleName("sliced")),
										 new BlockStmt()));

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
			final Node parameter = EDGTraverser.getChild(parameters, 0);
			final Parameter parameter0 = this.parseParameter(parameter);

			final Node body = EDGTraverser.getChild(clause, 2);
			final BlockStmt bodyBlock0 = this.parseBlockStatements(body);
			final BlockStmt bodyBlock = bodyBlock0 == null ? new BlockStmt() : bodyBlock0;

			catchClauses.add(new CatchClause(parameter0, bodyBlock));
		}
		return catchClauses;
	}

	private Statement parseThrow(Node _throw)
	{
		final Node throwExpressionNode = EDGTraverser.getChild(_throw, 0);
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

		final List<Node> returnChildren = EDGTraverser.getChildren(_return);
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
			if (node.getData().getType() == NodeInfo.Type.Result)
				continue;
			if (!transformUnused && this.slice != null && !this.slice.contains(node) &&
				!mayContentInternalSliceCode(node))
				continue;

			Expression parsed = this.parseExpression(node);
			if (parsed != null)
				expressionsList.add(parsed);
		}
		if (!nodes.isEmpty() && expressionsList.isEmpty())
			expressionsList.add(new NullLiteralExpr());

		return expressionsList;
	}

	private Expression parseExpression(Node expression)
	{
		if (this.slice != null && !this.slice.contains(expression) && !mayContentInternalSliceCode(expression))
			return null; // TEMPORAL SOLUTION

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
		final Node target = EDGTraverser.getChild(equality, NodeInfo.Type.Pattern);
		final LDASTNodeInfo ldNodeInfo = target.getData().getInfo();
		final Object[] info = ldNodeInfo.getInfo();

		if (target.getData().getType() == NodeInfo.Type.Variable)
		{
			VariableInfo vi = (VariableInfo) target.getData();
			if (vi.isDeclaration())
				return this.parseDeclaration(equality);
			return this.parseDefinition(equality);
		} else if (info == null || info.length == 0)
			return this.parseDefinition(equality); // TODO: RE-IMPLEMENT
		return this.parseDeclaration(equality);
	}

	@SuppressWarnings("unchecked")
	private Expression parseDeclaration(Node declaration)
	{
		if (slice != null && !slice.contains(declaration))
			return this.parseExpression(EDGTraverser.getChild(declaration, NodeInfo.Type.Value));

		final NodeList<VariableDeclarator> variableDeclarators = new NodeList<VariableDeclarator>();
		final VariableDeclarator variableDeclarator = this.getVariableDeclarator(declaration);
		final Node variableExpression = (declaration.getData().getType() == NodeInfo.Type.Equality) ? EDGTraverser
				.getChild(declaration, NodeInfo.Type.Pattern) : declaration;
		//final Node variable = EDGTraverserNew.getChild(variableExpression, 0);
		final LDASTNodeInfo ldNodeInfo = variableExpression.getData().getInfo();
		final EnumSet<Modifier> modifiers = (EnumSet<Modifier>) ldNodeInfo.getInfo()[0];

		if (variableDeclarator != null) ;
		variableDeclarators.add(variableDeclarator);

		return new VariableDeclarationExpr(modifiers, variableDeclarators);
	}
	private Expression parseDefinition(Node definition)
	{
		if (slice != null && !slice.contains(definition))
			return this.parseExpression(EDGTraverser.getChild(definition, NodeInfo.Type.Value));

		final Node target = EDGTraverser.getChild(definition, NodeInfo.Type.Pattern);
		final Node value = EDGTraverser.getChild(definition, NodeInfo.Type.Value);

		final Expression targetExpr = this.parseExpression(target);
		final Expression valueExpr = this.parseExpression(value);

//		if (targetExpr instanceof NullLiteralExpr)
//		{
//			final String name = "sliced";
//			final Type type = new ClassOrInterfaceType("Object");
//			return new VariableDeclarationExpr(new VariableDeclarator(type, name, valueExpr));
//		}
		return new AssignExpr(targetExpr, valueExpr, AssignExpr.Operator.ASSIGN);
	}

	private Expression parseDataConstructor(Node dataConstructor)
	{
		final List<Node> elements = EDGTraverser.getChildren(dataConstructor);
		final List<Expression> expressions = this.parseExpressions(elements, true);

		return new ArrayInitializerExpr(NodeList.nodeList(expressions));
	}

	private Expression parseCall(Node call) // TODO: This should return List<Expression>
	{
		/*
		 * Possibilities:
		 * 	1) One/Some args needed
		 * 	2) Only scope needed
		 * 	3) 1) + 2)
		 * 	4) Full call needed (may exclude some arguments)
		 * 	5) Object Creation Call
		 * 	6) Array Creation Call
		*/
		final Node callee = EDGTraverser.getChild(call, NodeInfo.Type.Callee);
		final Node scope = EDGTraverser.getChild(callee, NodeInfo.Type.Scope);
		final Node args = EDGTraverser.getChild(call, NodeInfo.Type.Arguments);

		Expression scopeExpr = null;

		// Case 1
		if (this.slice != null && !this.slice.contains(callee) && !this.slice.contains(scope))
			return this.parseArguments(args);

		if (this.slice != null && this.slice.contains(scope))
		{
			final Node scopeValue = EDGTraverser.getChild(scope, NodeInfo.Type.Value);
			scopeExpr = this.parseExpression(scopeValue);

			// Case 2
			if (!this.slice.contains(args))
				return scopeExpr;
		}

		final Node name = EDGTraverser.getChild(callee, NodeInfo.Type.Name);
		// Case 3 (slice == null NOT CONSIDERED) TODO: Broke all the parts in the slice in different statements
		if (this.slice == null || !this.slice.contains(name))
			return scopeExpr; // ERROR

		final Node nameValue = EDGTraverser.getChild(name, NodeInfo.Type.Value);
		final String nameText = nameValue.getData().getName();

		final NodeList<Expression> argumentsList = new NodeList<>();
		final List<Node> argumentsChildren = EDGTraverser.getChildren(args);
		argumentsList.addAll(this.parseExpressions(argumentsChildren, true)); // Fill the sliced arguments with "null"

		switch(nameText)
		{
			case "<constructor>": // Case 5
				final String scopeText = EDGTraverser.getChild(scope, NodeInfo.Type.Value).getData().getName();
				return new ObjectCreationExpr(scopeExpr, new ClassOrInterfaceType(scopeText), argumentsList);

			case "<arrayConstructor>": // Case 6
				final String scopeText0 = EDGTraverser.getChild(scope, NodeInfo.Type.Value).getData().getName();
				final ClassOrInterfaceType type = new ClassOrInterfaceType(scopeText0);
				return new ArrayCreationExpr(type, (NodeList<ArrayCreationLevel>) EDGTraverser
						.getChild(name, 0).getData().getInfo().getInfo()[0], null); // TODO: MMMM... (¬.¬)
			default: // Case 4
				return new MethodCallExpr(scopeExpr, new SimpleName(nameText), argumentsList);
		}
	}


	private Expression parseArguments(Node argsNode) // TODO: This should return List<Expression>
	{
		if (this.slice != null && !this.slice.contains(argsNode))
			return null;

		final NodeList<Expression> argumentsList = new NodeList<Expression>();
		final List<Node> argumentsChildren = EDGTraverser.getChildren(argsNode);
		argumentsList.addAll(this.parseExpressions(argumentsChildren, false)); // Discard sliced arguments

		if (argumentsList.size() != 0)
			return argumentsList.get(0); //return argumentsList;
		return null;
	}

	private Expression parseOperation(Node operation) // TODO: This should return List<Expression>
	{
		final String sign = operation.getData().getName();
		final List<Node> operands = EDGTraverser.getChildren(operation);
		operands.removeIf(node -> node.getData().getType() == NodeInfo.Type.Result);
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
				final Printable binaryOperator0 = this.getOperator(binaryOperators, sign);
				final BinaryExpr.Operator binaryOperator = (BinaryExpr.Operator) binaryOperator0;
				final Expression firstExpression0 = this
						.isEnclosedExpr(firstExpression, binaryOperator, true) ? new EnclosedExpr(
						firstExpression) : firstExpression;
				final Expression secondExpression0 = this
						.isEnclosedExpr(secondExpression, binaryOperator, false) ? new EnclosedExpr(
						secondExpression) : secondExpression;
				if (firstExpression == null && secondExpression == null)
					return null;
				if (firstExpression != null && secondExpression != null)
					return new BinaryExpr(firstExpression0, secondExpression0, binaryOperator);
				if (firstExpression != null)
					return firstExpression0;
				return secondExpression0;
			default:
				throw new RuntimeException("Operation arity not contemplated: " + operands.size());
		}
	}

	private Expression parseReference(Node reference)
	{
		final NodeInfo nodeInfo = reference.getData();
		final String value = nodeInfo.getName();

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
		final Node expression = EDGTraverser.getChild(instanceOf, 0);
		final Node type = EDGTraverser.getChild(instanceOf, 1);
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
		final Node type = EDGTraverser.getChild(cast, 0);
//		final Node type = EDGTraverserNew.getChild(typeExpr, 0);
		final Node expression = EDGTraverser.getChild(cast, 1);

		final Expression castExpr = this.parseExpression(expression);
		final Type castType = this.parseType(type);
		final CastExpr finalCast = new CastExpr(castType, castExpr);

		final boolean isEnclosedExpr = (boolean) cast.getData().getInfo().getInfo()[0];
		return isEnclosedExpr ? new EnclosedExpr(finalCast) : finalCast;
	}

	private Type parseType(Node type)
	{
		if (type.getData().getInfo().getInfo().length != 0)
		{
			if (this.slice != null && !this.slice.contains(type))
				return new PrimitiveType();
			else
			{    // DIRIA QUE ESTO NUNCA SE EJECUTA
				Object info = type.getData().getInfo().getInfo()[0];
				if (info.equals("arrayType"))

					return new ArrayType(new ClassOrInterfaceType(type.getData().getName()));
				else if (info instanceof PrimitiveType.Primitive)
					return new PrimitiveType((Primitive) info);
				else
					return new PrimitiveType();
			}

		} else if (this.slice != null && !this.slice.contains(type))
			return new ClassOrInterfaceType("Object");
		else
			return new ClassOrInterfaceType(type.getData().getName());
	}

	private Expression parseDataConstructorAccess(Node dataConstructorAccess)
	{
		final Node dataConstructor = EDGTraverser.getChild(dataConstructorAccess, NodeInfo.Type.Variable);
		final Node access = EDGTraverser.getChild(dataConstructorAccess, NodeInfo.Type.Index);

		final Expression dataConstructorExpr = this.parseExpression(dataConstructor);
		final Expression accessExpr = this.parseExpression(access);

		return new ArrayAccessExpr(dataConstructorExpr, accessExpr);
	}

	private Expression parseFieldAccess(Node fieldAccess)
	{
		final Node scope = EDGTraverser.getChild(fieldAccess, 0);
		final Node name = EDGTraverser.getChild(fieldAccess, 1);

		final Expression scopeExpr = this.parseExpression(scope);
		final Expression nameExpr = this.parseExpression(name);

		return new FieldAccessExpr(scopeExpr, nameExpr.toString());

	}

	private Expression parseTernary(Node _if)
	{
		// Condition
		final Node condition = EDGTraverser.getChild(_if, NodeInfo.Type.Condition);
		final Node conditionNode = EDGTraverser.getChild(condition, 0);
		final Expression conditionExpression = this.parseExpression(conditionNode);

		// Then
		final Node thenNode = EDGTraverser.getChild(_if, NodeInfo.Type.Then);
		final Node thenReturn = EDGTraverser.getChild(thenNode, 0);
		final Node then = EDGTraverser.getChild(thenReturn, 0);
		final Expression thenExpression = this.parseExpression(then);

		// Else
		final Node elseNode = EDGTraverser.getChild(_if, NodeInfo.Type.Else);
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
			case "array creation":
				return null;
			default:
				return new NullLiteralExpr();
		}
	}

	// Auxiliaries
	private VariableDeclarator getVariableDeclarator(Node declaration)
	{
		final Node variableExpression = (declaration.getData().getType() == NodeInfo.Type.Equality) ? EDGTraverser
				.getChild(declaration, 0) : declaration;
		final Node initializerNode = (declaration.getData().getType() == NodeInfo.Type.Equality) ? EDGTraverser
				.getChild(declaration, 1) : null;
		Expression initializer = this.slice != null && !this.slice.contains(initializerNode) ? null : this
				.parseExpression(initializerNode);

		if (initializer == null)
		{
			final Node variable = (variableExpression.getData().getType() ==
								   NodeInfo.Type.Variable) ? variableExpression : EDGTraverser
					.getChild(variableExpression, 0);
			final LDASTNodeInfo ldNodeInfo = variable.getData().getInfo();
			final Type varType = (Type) ldNodeInfo.getInfo()[1];
			if (varType instanceof PrimitiveType || initializerNode == null)
				initializer = null;
			else
				initializer = new ObjectCreationExpr(null, (ClassOrInterfaceType) varType, new NodeList<Expression>());
		}

		final Node variable = (variableExpression.getData().getType() ==
							   NodeInfo.Type.Variable) ? variableExpression : EDGTraverser
				.getChild(variableExpression, 0);
		//final Node variable = EDGTraverserNew.getChild(variableExpression, 0);

		final NodeInfo nodeInfo = variable.getData();
		if (this.slice != null && !this.slice.contains(variable))
		{
			final String name = "sliced";
			final Type type = new ClassOrInterfaceType("Object");
			return new VariableDeclarator(type, name, initializer);
		}

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
			if (ctx.nodeType == NodeInfo.Type.If && ctx.trueBranches == 2)
			{
				ctx.returned();
				final Context ifContext = context.pop();
				final Context previousContext = context.peek();

				if (previousContext.nodeType != NodeInfo.Type.If)
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
		private final NodeInfo.Type nodeType;
		private final Type returnType;
		private boolean returnReq;
		private int trueBranches = 0;

		public Context(NodeInfo.Type nodeType, Type returnType, boolean returnReq)
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

	private boolean mayContentInternalSliceCode(Node node)
	{
		switch (node.getData().getType())
		{
			case Operation:
			case Equality:
			case Call:
				return true;
			default:
				return false;
		}
	}

}