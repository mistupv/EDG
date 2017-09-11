package eknife.java;

import java.io.File;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Modifier;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.BodyDeclaration;
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
import com.github.javaparser.ast.expr.EnclosedExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.LongLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.NullLiteralExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.SimpleName;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.BreakStmt;
import com.github.javaparser.ast.stmt.ContinueStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForeachStmt;
import com.github.javaparser.ast.stmt.IfStmt;
import com.github.javaparser.ast.stmt.ReturnStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntryStmt;
import com.github.javaparser.ast.stmt.SwitchStmt;
import com.github.javaparser.ast.stmt.TryStmt;
import com.github.javaparser.ast.stmt.WhileStmt;
import com.github.javaparser.ast.type.Type;

import edg.EDGFactory;
import edg.LDASTNodeInfo;
import edg.ASTBuilder.Where;
import edg.graph.EDG;
import edg.graph.NodeInfo;
import misc.Misc;

public class JavaEDGFactory extends EDGFactory
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static EDG createEDG(String sourcePath)
	{
		return JavaEDGFactory.createEDG(sourcePath, true);
	}
	public static EDG createEDG(String sourcePath, boolean generateArcs)
	{
		final File programFile = new File(sourcePath);
		final List<File> files = programFile.isFile() ? Arrays.asList(programFile) : Misc.getFiles(programFile, new String[] { ".java" }, true);
		final JavaEDGFactory edgFactory = new JavaEDGFactory(sourcePath);

		return edgFactory.createEDG(generateArcs, files);
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private final String codebase;

	private JavaEDGFactory(String codebase)
	{
		this.codebase = codebase;
	}

	private EDG createEDG(boolean generateArcs, List<File> files)
	{
		try
		{
			final List<ClassOrInterfaceDeclaration> classes = new LinkedList<ClassOrInterfaceDeclaration>();
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(this.codebase, 0, "EDG");

			for (File file : files)
			{
				final String path = file.getAbsolutePath();
				final CompilationUnit cu = JavaParser.parse(file);
				final NodeList<TypeDeclaration<?>> types = cu.getTypes();

				for (TypeDeclaration<?> type : types)
					if (type instanceof ClassOrInterfaceDeclaration)
						classes.add((ClassOrInterfaceDeclaration) type);
			}

			return super.createEDG(generateArcs, classes, ldNodeInfo);
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return null;
		}
	}
	protected void processElement(Object element, Map<String, Object> info)
	{
		if (this.needsReturn(info))
			this.processReturn(element, info);
		else if (element instanceof ClassOrInterfaceDeclaration)
			this.process((ClassOrInterfaceDeclaration) element);
		else if (element instanceof FieldDeclaration)
			this.process((FieldDeclaration) element, info);
		else if (element instanceof CallableDeclaration)
			this.process((CallableDeclaration<?>) element, info);
		else if (element instanceof Parameter)
			this.process((Parameter) element);
		else if (element instanceof ExpressionStmt)
			this.process((ExpressionStmt) element, info);
		else if (element instanceof BlockStmt)
			this.process((BlockStmt) element);
		else if (element instanceof EnclosedExpr)
			this.process((EnclosedExpr) element, info);
		else if (element instanceof VariableDeclarationExpr)
			this.process((VariableDeclarationExpr) element);
		else if (element instanceof AssignExpr)
			this.process((AssignExpr) element);
		else if (element instanceof ArrayAccessExpr)
			this.process((ArrayAccessExpr) element);
		else if (element instanceof ArrayInitializerExpr)
			this.process((ArrayInitializerExpr) element);
		else if (element instanceof MethodCallExpr)
			this.process((MethodCallExpr) element);
		else if (element instanceof SimpleName)
			this.process((SimpleName) element);
		else if (element instanceof NameExpr)
			this.process((NameExpr) element);
		else if (element instanceof Variable)
			this.process((Variable) element);
		else if (element instanceof Literal)
			this.process((Literal) element);
		else if (element instanceof BinaryExpr)
			this.process((BinaryExpr) element);
		else if (element instanceof FieldAccessExpr)
			this.process((FieldAccessExpr) element);
		else if (element instanceof TryStmt)
			this.process((TryStmt) element);
		else if (element instanceof IfStmt)
			this.process((IfStmt) element);
		else if (element instanceof SwitchStmt)
			this.process((SwitchStmt) element);
		else if (element instanceof SwitchEntryStmt)
			this.process((SwitchEntryStmt) element);
		else if (element instanceof ObjectCreationExpr)
			this.process((ObjectCreationExpr) element);
		else if (element instanceof ForeachStmt)
			this.process((ForeachStmt) element);
		else if (element instanceof ReturnStmt)
			this.process((ReturnStmt) element, info);
		else if (element instanceof BreakStmt)
			this.process((BreakStmt) element, info);
		else if (element instanceof ContinueStmt)
			this.process((ContinueStmt) element, info);
		else if (element instanceof CastExpr)
			this.process((CastExpr) element);
		else if (element instanceof WhileStmt)
			this.process((WhileStmt) element);
		else if (element instanceof ConditionalExpr)
			this.process((ConditionalExpr) element);
		else if (element instanceof BooleanLiteralExpr)
			this.process((BooleanLiteralExpr) element);
		else if (element instanceof CharLiteralExpr)
			this.process((CharLiteralExpr) element);
		else if (element instanceof DoubleLiteralExpr)
			this.process((DoubleLiteralExpr) element);
		else if (element instanceof IntegerLiteralExpr)
			this.process((IntegerLiteralExpr) element);
		else if (element instanceof LongLiteralExpr)
			this.process((LongLiteralExpr) element);
		else if (element instanceof StringLiteralExpr)
			this.process((StringLiteralExpr) element);
		else if (element instanceof NullLiteralExpr)
			this.process((NullLiteralExpr) element);
		else
			throw new RuntimeException("Element not contemplated: " + element);
	}

	// Return
	private boolean needsReturn(Map<String, Object> info)
	{
		final EDGFactory.Branch parent = (EDGFactory.Branch) info.get("parent");
		if (parent == null)
			return false;

		final NodeInfo.Type parentType = parent.getNodeType();
		final String construction = parent.getLdASTNodeInfo().getConstruction();
		final Where where = parent.getWhere();
		final int index = parent.getIndex();
		final int length = parent.getLength();

		if (index != length)
			return false;
		switch (parentType)
		{
			case If:
				return construction.equals("ternary") && (where == Where.Then || where == Where.Else);
			default:
				return false;
		}
	}
	private void processReturn(Object expression, Map<String, Object> info)
	{
		@SuppressWarnings("unchecked")
		final List<EDGFactory.Branch> ancestors = (List<EDGFactory.Branch>) info.get("ancestors");

		for (int ancestorIndex = ancestors.size() - 1; ancestorIndex >= 0; ancestorIndex--)
		{
			final EDGFactory.Branch ancestor = ancestors.get(ancestorIndex);
			final NodeInfo.Type type = ancestor.getNodeType();
			if (type != NodeInfo.Type.If)
				continue;

			final int id = ancestor.getNodeId();
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "return");
			super.addReturn(expression, id, ldNodeInfo);
			break;
		}
	}

	// Structure
	private void process(ClassOrInterfaceDeclaration _class)
	{
		final long line = _class.getRange().get().begin.line;
		final String className = _class.getNameAsString();
		final List<BodyDeclaration<?>> members = new LinkedList<BodyDeclaration<?>>();
		final NodeList<BodyDeclaration<?>> members0 = _class.getMembers();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(className + ".java", line, "class");

		for (BodyDeclaration<?> member : members0)
			if (member instanceof FieldDeclaration || member instanceof CallableDeclaration)
				members.add((BodyDeclaration<?>) member);

		this.createContext();
		super.addModule(className, members, ldNodeInfo);
		this.destroyContext();
	}
	private void process(FieldDeclaration field, Map<String, Object> info)
	{
		final NodeList<VariableDeclarator> variables = field.getVariables();
		final EnumSet<Modifier> modifiers = field.getModifiers();

		for (VariableDeclarator variable : variables)
		{
			final long line = variable.getRange().get().begin.line;
			final Type type = variable.getType();
			final Expression initializer = variable.getInitializer().isPresent() ? variable.getInitializer().get() : null;

			if (initializer == null)
			{
				final String name = variable.getName().getIdentifier();
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "field", modifiers, type);
				super.addVariable(name, true, true, true, ldNodeInfo);
			}
			else
			{
				final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, "name", modifiers, type);
				final Variable name = new Variable(variable.getName(), true, true, ldNodeInfo0);
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "field");
				super.addEquality(name, initializer, ldNodeInfo);
			}
		}
	}
	private void process(CallableDeclaration<?> callable, Map<String, Object> info)
	{
		final EDGFactory.Branch parent = (EDGFactory.Branch) info.get("parent");
		final NodeInfo.Type parentType = parent.getNodeType();
		final long line = callable.getRange().get().begin.line;

		switch (parentType)
		{
			case Module:
				final String name = callable instanceof MethodDeclaration ? callable.getName().getIdentifier() : "<constructor>";
				final List<CallableDeclaration<?>> clauses = new LinkedList<CallableDeclaration<?>>();
				final EnumSet<Modifier> modifiers = callable.getModifiers();
				final Type type = callable instanceof MethodDeclaration ? ((MethodDeclaration) callable).getType() : null;
				final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, "callable", modifiers, type);
				clauses.add(callable);
				super.addRoutine(name, clauses, ldNodeInfo0);
				break;
			case Routine:
				final List<Parameter> parameters = callable.getParameters();
				final BlockStmt body = callable instanceof MethodDeclaration ? ((MethodDeclaration) callable).getBody().get() : ((ConstructorDeclaration) callable).getBody();
				final List<Statement> statements = body.getStatements();
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "callable");
				this.createNewContext();
				super.addClause(parameters, null, statements, ldNodeInfo);
				this.destroyContext();
				break;
			default:
				throw new RuntimeException("Node type not contemplated: " + parentType);
		}
	}

	// Statements
	private void process(ExpressionStmt expressionStatement, Map<String, Object> info)
	{
		final Expression expression = expressionStatement.getExpression();
		this.processElement(expression, info);
	}
	private void process(BlockStmt blockStatement)
	{
		final long line = blockStatement.getRange().get().begin.line;
		final List<Statement> statements = blockStatement.getStatements();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "block");

		this.createContext();
		super.addBlock(statements, ldNodeInfo);
		this.destroyContext();
	}
	private void process(ArrayInitializerExpr arrayInitializer)
	{
		final long line = arrayInitializer.getRange().get().begin.line;
		final NodeList<Expression> values = arrayInitializer.getValues();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "array initializer");

		super.addDataConstructor(values, ldNodeInfo);
	}
	private void process(ArrayAccessExpr arrayAccess)
	{
		final long line = arrayAccess.getRange().get().begin.line;
		final Object name = this.treatExpression(arrayAccess.getName(), false, false, line);
		final Expression index = arrayAccess.getIndex();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "array access");

		super.addDataConstructorAccess(name, index, ldNodeInfo);
	}
	private void process(TryStmt _try)
	{
		// TODO
	}
	private void process(ForeachStmt foreach)
	{
		// TODO
	}
	private void process(ReturnStmt _return, Map<String, Object> info)
	{
		final long line = _return.getRange().get().begin.line;
		final Object expression = !_return.getExpression().isPresent() ? null : this.treatExpression(_return.getExpression().get(), false, false, line);
		final int id = this.getJumpDestiny(info, NodeInfo.Type.Clause);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "return");

		super.addReturn(expression, id, ldNodeInfo);
	}
	private void process(BreakStmt _break, Map<String, Object> info)
	{
		final long line = _break.getRange().get().begin.line;
		final int id = this.getJumpDestiny(info, NodeInfo.Type.Switch);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "break");

		super.addBreak(id, ldNodeInfo);
	}
	private void process(ContinueStmt _continue, Map<String, Object> info)
	{
		final long line = _continue.getRange().get().begin.line;
		final int id = this.getJumpDestiny(info);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "continue");

		super.addContinue(id, ldNodeInfo);
	}
	private void process(IfStmt _if)
	{
		final long line = _if.getRange().get().begin.line;
		final Expression condition = _if.getCondition();
		final Statement then = _if.getThenStmt();
		final Statement _else = _if.getElseStmt().isPresent() ? _if.getElseStmt().get() : null;
		final List<Statement> thenStatements = then instanceof BlockStmt ? ((BlockStmt) then).getStatements() : new LinkedList<Statement>();
		final List<Statement> elseStatements = _else instanceof BlockStmt ? ((BlockStmt) _else).getStatements() : new LinkedList<Statement>();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "if");

		if (!(then instanceof BlockStmt))
			thenStatements.add(then);
		if (_else != null && !(_else instanceof BlockStmt))
			elseStatements.add(_else);
		super.addIf(condition, thenStatements, elseStatements, ldNodeInfo);
	}
	private void process(SwitchStmt _switch)
	{
		final long line = _switch.getRange().get().begin.line;
		final Expression selector = _switch.getSelector();
		final NodeList<SwitchEntryStmt> entries = _switch.getEntries();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "switch");

		super.addSwitch(selector, entries, ldNodeInfo);
	}
	private void process(SwitchEntryStmt _case)
	{
		final long line = _case.getRange().get().begin.line;
		final NodeList<Statement> statements = _case.getStatements();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "case");

		if (_case.getLabel().isPresent())
		{
			final List<Expression> selectables = new LinkedList<Expression>();
			final Expression label = _case.getLabel().get();
			selectables.add(label);
			super.addCase(selectables, null, statements, ldNodeInfo);
		}
		else
			super.addDefaultCase(statements, ldNodeInfo);
	}
	private void process(WhileStmt _while)
	{
		final long line = _while.getRange().get().begin.line;
		final Expression condition = _while.getCondition();
		final Statement body = _while.getBody();
		final List<Statement> bodyStatements = body instanceof BlockStmt ? ((BlockStmt) body).getStatements() : new LinkedList<Statement>();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "while");

		if (!(body instanceof BlockStmt))
			bodyStatements.add(body);
		super.addLoop(condition, bodyStatements, ldNodeInfo);
	}

	// Expressions
	private void process(EnclosedExpr enclosedExpr, Map<String, Object> info)
	{
		final Expression inner = enclosedExpr.getInner().get();
		this.processElement(inner, info);
	}
	private void process(VariableDeclarationExpr variableDeclaration)
	{
		final NodeList<VariableDeclarator> variables = variableDeclaration.getVariables();
		final EnumSet<Modifier> modifiers = variableDeclaration.getModifiers();

		for (VariableDeclarator variable : variables)
		{
			final long line = variable.getRange().get().begin.line;
			final Type type = variable.getType();
			final Expression initializer = variable.getInitializer().isPresent() ? variable.getInitializer().get() : null;

			if (initializer == null)
			{
				final String name = variable.getName().getIdentifier();
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "var", modifiers, type);
				super.addVariable(name, true, true, false, ldNodeInfo);
			}
			else
			{
				final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, "name", modifiers, type);
				final Variable name = new Variable(variable.getName(), true, true, ldNodeInfo0);
				final Object initializer0 = this.treatExpression(initializer, false, false, line);
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "var");
				super.addEquality(name, initializer0, ldNodeInfo);
			}
		}
	}
	private void process(AssignExpr assignation)
	{
		final long line = assignation.getRange().get().begin.line;
		final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, "var");
		final Variable target = new Variable(assignation.getTarget(), false, true, ldNodeInfo0);
		final Object value = this.treatExpression(assignation.getValue(), false, false, line);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "assign");

		super.addEquality(target, value, ldNodeInfo);
	}
	private void process(CastExpr cast)
	{
		// TODO
	}
	private void process(MethodCallExpr methodCall)
	{
		final long line = methodCall.getRange().get().begin.line;
		final Expression scope = methodCall.getScope().isPresent() ? methodCall.getScope().get() : null;
		final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, "name", "name");
		final Literal function = new Literal(methodCall.getName(), ldNodeInfo0);
		final List<Object> arguments = this.treatExpressions(methodCall.getArguments(), false, false, line);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "method");

		super.addCall(scope, function, arguments, ldNodeInfo);
	}
	private void process(BinaryExpr binaryExpression)
	{
		final long line = binaryExpression.getRange().get().begin.line;
		final String operation = binaryExpression.getOperator().asString();
		final List<Object> operands = new LinkedList<Object>();
		final Object left = this.treatExpression(binaryExpression.getLeft(), false, false, line);
		final Object right = this.treatExpression(binaryExpression.getRight(), false, false, line);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "binary");

		operands.add(left);
		operands.add(right);
		super.addOperation(operation, operands, ldNodeInfo);
	}
	private void process(ObjectCreationExpr objectCreationExpression)
	{
		final long line = objectCreationExpression.getRange().get().begin.line;
		final Literal scope = new Literal(objectCreationExpression.getType().getName(), null);
		final Literal function = new Literal(objectCreationExpression, "<constructor>", null);
		final List<Expression> arguments = objectCreationExpression.getArguments();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "object creation");

		super.addCall(scope, function, arguments, ldNodeInfo);
	}
	private void process(ConditionalExpr ternaryExpr)
	{
		final long line = ternaryExpr.getRange().get().begin.line;
		final Expression condition = ternaryExpr.getCondition();
		final Expression then = ternaryExpr.getThenExpr();
		final Expression _else = ternaryExpr.getElseExpr();
		final List<Expression> thenStatements = new LinkedList<Expression>();
		final List<Expression> elseStatements = new LinkedList<Expression>();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "ternary");

		thenStatements.add(then);
		elseStatements.add(_else);
		super.addIf(condition, thenStatements, elseStatements, ldNodeInfo);
	}

	// Variables
	private void process(Parameter parameter)
	{
		final long line = parameter.getRange().get().begin.line;
		final Type type = parameter.getType();
		final String name = parameter.getNameAsString();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "var", type);

		this.addVariableToContext(name);
		super.addVariable(name, true, true, false, ldNodeInfo);
	}
	private void process(FieldAccessExpr fieldAccessExpression)
	{
		final long line = fieldAccessExpression.getRange().get().begin.line;
		final String value = fieldAccessExpression.toString();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "field");

		super.addVariable(value, false, false, true, ldNodeInfo);
	}
	private void process(NameExpr nameExpression)
	{
		final long line = nameExpression.getRange().get().begin.line;
		final String name = nameExpression.getNameAsString();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "var");

		super.addVariable(name, false, false, false, ldNodeInfo);
	}
	private void process(SimpleName simpleName)
	{
		final long line = simpleName.getRange().get().begin.line;
		final String name = simpleName.getId();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "var");

		super.addVariable(name, false, false, false, ldNodeInfo);
	}
	private void process(Variable variable)
	{
		final String name = variable.getName();
		final boolean declaration = variable.declaration;
		final boolean definition = variable.definition;
		final boolean global = (declaration && this.variableContexts.size() == 1) || (!declaration && !this.variableContexts.peek().contains(name));
		final LDASTNodeInfo ldNodeInfo = variable.ldNodeInfo;

		if (declaration)
			this.addVariableToContext(name);
		super.addVariable(name, declaration, definition, global, ldNodeInfo);
	}

	// Literals
	private void process(BooleanLiteralExpr _boolean)
	{
		final long line = _boolean.getRange().get().begin.line;
		final boolean value = _boolean.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "boolean");

		super.addLiteral(value + "", ldNodeInfo);
	}
	private void process(CharLiteralExpr _char)
	{
		final long line = _char.getRange().get().begin.line;
		final String value = _char.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "char");

		super.addLiteral(value + "", ldNodeInfo);
	}
	private void process(DoubleLiteralExpr _double)
	{
		final long line = _double.getRange().get().begin.line;
		final String value = _double.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "double");

		super.addLiteral(value + "", ldNodeInfo);
	}
	private void process(IntegerLiteralExpr integer)
	{
		final long line = integer.getRange().get().begin.line;
		final String value = integer.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "int");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(LongLiteralExpr _long)
	{
		final long line = _long.getRange().get().begin.line;
		final String value = _long.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "long");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(StringLiteralExpr string)
	{
		final long line = string.getRange().get().begin.line;
		final String value = string.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "String");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(NullLiteralExpr _null)
	{
		final long line = _null.getRange().get().begin.line;
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "null");

		super.addLiteral("null", ldNodeInfo);
	}
	private void process(Literal literal)
	{
		final String name = literal.getName();
		final LDASTNodeInfo ldNodeInfo = literal.ldNodeInfo;

		super.addLiteral(name, ldNodeInfo);
	}

	// Auxiliary
	private List<Object> treatExpressions(List<Expression> expressions, boolean declaration, boolean definition, long line)
	{
		final List<Object> elements = new LinkedList<Object>();

		for (Expression expression : expressions)
			elements.add(this.treatExpression(expression, declaration, definition, line));

		return elements;
	}
	private Object treatExpression(Expression expression, boolean declaration, boolean definition, long line)
	{
		if (expression instanceof NameExpr)
			return new Variable(expression, declaration, definition, new LDASTNodeInfo(line, "var"));
//		if (expression instanceof ArrayAccessExpr)
//			return new Variable(expression, declaration, definition, new LDASTNodeInfo(line, "array access"));
		return expression;
	}
	private int getJumpDestiny(Map<String, Object> info, NodeInfo.Type... seekingTypes)
	{
		@SuppressWarnings("unchecked")
		final List<EDGFactory.Branch> ancestors = (List<EDGFactory.Branch>) info.get("ancestors");

		for (int ancestorIndex = ancestors.size() - 1; ancestorIndex >= 0; ancestorIndex--)
		{
			final EDGFactory.Branch ancestor = ancestors.get(ancestorIndex);
			final NodeInfo.Type type = (NodeInfo.Type) ancestor.getNodeType();

			for (NodeInfo.Type seekingType : seekingTypes)
				if (seekingType == type)
					return ancestor.getNodeId();
		}

		throw new RuntimeException("Wrong jump instruction");
	}

	private static class Variable
	{
		private final Node node;
		private final String name;
		private final boolean declaration;
		private final boolean definition;
		private final LDASTNodeInfo ldNodeInfo;

		public Variable(Node node, boolean declaration, boolean definition, LDASTNodeInfo ldNodeInfo)
		{
			this(node, null, declaration, definition, ldNodeInfo);
		}
		public Variable(Node node, String name, boolean declaration, boolean definition, LDASTNodeInfo ldNodeInfo)
		{
			this.node = node;
			this.name = name;
			this.declaration = declaration;
			this.definition = definition;
			this.ldNodeInfo = ldNodeInfo;
		}

		public int getLine()
		{
			return this.node.getRange().get().begin.line;
		}
		public String getName()
		{
			if (this.name != null)
				return this.name;
			if (this.node instanceof ArrayAccessExpr)
				return (((ArrayAccessExpr) this.node).getName()).toString();
			return this.node.toString();
		}
	}
	private static class Literal
	{
		private final Node node;
		private final String name;
		private final LDASTNodeInfo ldNodeInfo;

		public Literal(Node node, LDASTNodeInfo ldNodeInfo)
		{
			this(node, null, ldNodeInfo);
		}
		public Literal(Node node, String name, LDASTNodeInfo ldNodeInfo)
		{
			this.node = node;
			this.name = name;
			this.ldNodeInfo = ldNodeInfo;
		}

		public int getLine()
		{
			return this.node.getRange().get().begin.line;
		}
		public String getName()
		{
			if (this.name != null)
				return this.name;
			return this.node.toString();
		}
	}

	// Variable global/local
	private Stack<List<String>> variableContexts = new Stack<List<String>>();

	private void createContext()
	{
		final List<String> variableContext = new LinkedList<String>();
		final List<String> lastVariableContext = this.variableContexts.size() == 0 ? new LinkedList<String>() : this.variableContexts.peek();

		variableContext.addAll(lastVariableContext);
		this.variableContexts.add(variableContext);
	}
	private void createNewContext()
	{
		this.variableContexts.add(new LinkedList<String>());
	}
	private void destroyContext()
	{
		this.variableContexts.pop();
	}
	private void addVariableToContext(String variableName)
	{
		if (variableName.equals("_"))
			return;

		final List<String> variableContext = this.variableContexts.peek();
		if (!variableContext.contains(variableName))
			variableContext.add(variableName);
	}
}