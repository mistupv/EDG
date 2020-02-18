package eknife.java;

import java.io.File;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Stack;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.ArrayCreationLevel;
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
import com.github.javaparser.ast.expr.ArrayCreationExpr;
import com.github.javaparser.ast.expr.ArrayInitializerExpr;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.BinaryExpr;
import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.CastExpr;
import com.github.javaparser.ast.expr.CharLiteralExpr;
import com.github.javaparser.ast.expr.ClassExpr;
import com.github.javaparser.ast.expr.ConditionalExpr;
import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.EnclosedExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.InstanceOfExpr;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.LongLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.NullLiteralExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.SimpleName;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.expr.SuperExpr;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.expr.UnaryExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.AssertStmt;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.BreakStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.ContinueStmt;
import com.github.javaparser.ast.stmt.DoStmt;
import com.github.javaparser.ast.stmt.EmptyStmt;
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.ForeachStmt;
import com.github.javaparser.ast.stmt.IfStmt;
import com.github.javaparser.ast.stmt.LabeledStmt;
import com.github.javaparser.ast.stmt.ReturnStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntryStmt;
import com.github.javaparser.ast.stmt.SwitchStmt;
import com.github.javaparser.ast.stmt.ThrowStmt;
import com.github.javaparser.ast.stmt.TryStmt;
import com.github.javaparser.ast.stmt.WhileStmt;
import com.github.javaparser.ast.type.ArrayType;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.type.PrimitiveType;
import com.github.javaparser.ast.type.Type;

import edg.LASTFactory;
import edg.LDASTNodeInfo;
import edg.LASTBuilder.Where;
import edg.graph.LAST;
import edg.graph.NodeInfo;
import misc.Misc;

public class JavaLASTFactory extends LASTFactory
{
	
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static LAST createLAST(String sourcePath)
	{
		return JavaLASTFactory.createLAST(sourcePath, true);
	}
	public static LAST createLAST(String sourcePath, boolean generateArcs)
	{
		final File programFile = new File(sourcePath);
		final List<File> files = programFile.isFile() ? Arrays.asList(programFile) : Misc.getFiles(programFile, new String[] { ".java" }, true);
		final JavaLASTFactory lastFactory = new JavaLASTFactory(sourcePath);

		return lastFactory.createLAST(generateArcs, files);
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private final String codebase;

	private JavaLASTFactory(String codebase)
	{
		this.codebase = codebase;
	}

	private LAST createLAST(boolean generateArcs, List<File> files)
	{
		try
		{
			final List<ClassOrInterfaceDeclaration> classes = new LinkedList<ClassOrInterfaceDeclaration>();
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(this.codebase, 0, "LAST");

			for (File file : files)
			{
				final String path = file.getAbsolutePath();
				final CompilationUnit cu = JavaParser.parse(file);
				final NodeList<TypeDeclaration<?>> types = cu.getTypes();

				for (TypeDeclaration<?> type : types)
					if (type instanceof ClassOrInterfaceDeclaration)
						classes.add((ClassOrInterfaceDeclaration) type);
			}
		 
			final LAST last = super.createLAST(generateArcs, classes, ldNodeInfo);
			if (generateArcs)
			{
				new ControlFlowEdgeGenerator(last).generate();
				new ValueEdgeGenerator(last).generate();
			}
			return last;
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
		else if (element instanceof ArrayCreationExpr)
			this.process((ArrayCreationExpr) element);
		else if (element instanceof ArrayAccessExpr)
			this.process((ArrayAccessExpr) element);
		else if (element instanceof ArrayInitializerExpr)
			this.process((ArrayInitializerExpr) element);
		else if (element instanceof ArrayType)
			this.process((ArrayType) element);
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
			this.process((FieldAccessExpr) element, info);
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
			this.process((CastExpr) element, info);
		else if (element instanceof WhileStmt)
			this.process((WhileStmt) element);
		else if (element instanceof ForStmt)
			this.process((ForStmt) element);
		else if (element instanceof DoStmt)
			this.process((DoStmt) element);
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
		//ADDED
		else if (element instanceof LabeledStmt)
			this.process((LabeledStmt) element);
		else if (element instanceof UnaryExpr)
			this.process((UnaryExpr) element);
		else if (element instanceof InstanceOfExpr)
			this.process((InstanceOfExpr) element);
		else if (element instanceof ClassOrInterfaceType)
			this.process((ClassOrInterfaceType) element);
		else if (element instanceof PrimitiveType)
			this.process((PrimitiveType) element);
		else if (element instanceof CatchClause)
			this.process((CatchClause) element);
		else if (element instanceof ThrowStmt)
			this.process((ThrowStmt) element);
		// SUPER & THIS
		else if (element instanceof SuperExpr)
			this.process((SuperExpr) element);
		else if (element instanceof ThisExpr)
			this.process((ThisExpr) element);
		else if (element instanceof ExplicitConstructorInvocationStmt)
			this.process((ExplicitConstructorInvocationStmt) element);
		else if (element instanceof EmptyStmt)
			this.process((EmptyStmt) element);
		else if (element instanceof AssertStmt)
			this.process((AssertStmt) element);
		
		else if (element instanceof ClassExpr)
			this.process((ClassExpr) element);
		else
			throw new RuntimeException("Element not contemplated: " + element);
	}

	// Return
	private boolean needsReturn(Map<String, Object> info)
	{
		final LASTFactory.Branch parent = (LASTFactory.Branch) info.get("parent");
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
		final List<LASTFactory.Branch> ancestors = (List<LASTFactory.Branch>) info.get("ancestors");

		for (int ancestorIndex = ancestors.size() - 1; ancestorIndex >= 0; ancestorIndex--)
		{
			final LASTFactory.Branch ancestor = ancestors.get(ancestorIndex);
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
		final String archive = new File(this.codebase).getName();
		final long line = _class.getRange().get().begin.line;  
		final String className = _class.getNameAsString();
		final List<BodyDeclaration<?>> members = new LinkedList<BodyDeclaration<?>>();
		final NodeList<BodyDeclaration<?>> members0 = _class.getMembers();
		
		final NodeList<ClassOrInterfaceType> extended = _class.getExtendedTypes();
		
		final String extended0;
		if (extended.isEmpty())
			extended0 = "";
		else
		{
			final ClassOrInterfaceType extendedClassName = extended.get(0);
			extended0 = extendedClassName.toString();
		}
		
		final NodeList<ClassOrInterfaceType> implemented = _class.getImplementedTypes();
		
		//final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(archive, className + ".java", line, "class", extended, implemented);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(archive, className, line, "class", extended0, implemented);
		
		for (BodyDeclaration<?> member : members0)
			if (member instanceof FieldDeclaration || member instanceof CallableDeclaration || member instanceof ClassOrInterfaceDeclaration)
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
				// TODO Incorrecto, este tipo de fields ("private int v;") no son definiciones, solo declaraciones... TABUM
				//super.addVariable(name, true, true, true, ldNodeInfo); 
this.addVariableToContext(new VariableRecord(name, modifiers, type)); // Añadir Global Vars al contexto
				super.addVariable(name, true, false, false, true, ldNodeInfo); // Son Globales y no son ni definicion ni uso, solo declaracion
			}
			else
			{
				final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, true,"name", modifiers, type);
				final Variable name = new Variable(variable.getName(), true, true, false, ldNodeInfo0);
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "field");
this.addVariableToContext(new VariableRecord(name.getName(), modifiers, type));
				super.addEquality(name, initializer, ldNodeInfo);
			}
		}
	}
	private void process(CallableDeclaration<?> callable, Map<String, Object> info)
	{
		final LASTFactory.Branch parent = (LASTFactory.Branch) info.get("parent");
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
				currentLabels.clear();
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
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "array initializer");

		super.addDataConstructor(values, ldNodeInfo);
	}
	private void process(ArrayAccessExpr arrayAccess)
	{
		final long line = arrayAccess.getRange().get().begin.line;
		final Object name = this.treatExpression(arrayAccess.getName(), false, false, true, line); // ESTO ESTA MAL, un DATA ACCESS no siempre es un uso 
		final Object index = this.treatExpression(arrayAccess.getIndex(), false, false, true, line);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "array access");

		super.addDataConstructorAccess(name, index, ldNodeInfo);
	}
	private void process(ArrayCreationExpr arrayCreation)
	{
		final long line = arrayCreation.getRange().get().begin.line;
		final List<ArrayCreationLevel> levels = arrayCreation.getLevels();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "array creation"); 
		final Literal scope = new Literal(arrayCreation.getElementType(), ldNodeInfo);
		
		final LDASTNodeInfo functionNodeInfo = new LDASTNodeInfo(line, true, "array creation", levels); // info[0] = levels
		final Literal function = new Literal(arrayCreation, "<arrayConstructor>", functionNodeInfo);
		final List<Expression> arguments = new LinkedList<Expression>();
		
		super.addCall(scope, function, arguments, ldNodeInfo);
	}
	private void process(LabeledStmt labeledStmt)
	{
		final long line = labeledStmt.getRange().get().begin.line;
		final String labelText = labeledStmt.getLabel().getIdentifier();
		final Statement statement = labeledStmt.getStatement();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "labeled statement"); 
		
		super.addLabel(labelText, statement, ldNodeInfo);
	}
	
	// Exceptions
	private void process(TryStmt _try)
	{
		final long line = _try.getRange().get().begin.line;
		
		final Optional<BlockStmt> tryBlock = _try.getTryBlock();
		final List<CatchClause> catchClauses = _try.getCatchClauses();
		final Optional<BlockStmt>	finallyBlock = _try.getFinallyBlock();
		
		final List<Statement> tryStatements = tryBlock.isPresent() ? tryBlock.get().getStatements() : new LinkedList<Statement>();
		// SOMETHING TODO WITH catchClauses
		
		final List<Statement> finallyStatements = finallyBlock.isPresent() ? finallyBlock.get().getStatements() : new LinkedList<Statement>();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "exHandler");

		super.addExHandler(tryStatements, catchClauses, finallyStatements, ldNodeInfo);
	}
	private void process(CatchClause catchClause)
	{
		final long line = catchClause.getRange().get().begin.line;
		final Parameter parameter = catchClause.getParameter();
		final List<Statement> statements = catchClause.getBody().getStatements();
		
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "catchClause");
		super.addCatchClause(parameter, null, statements, ldNodeInfo);
	}
	private void process(ThrowStmt _throw)
	{
		final long line = _throw.getRange().get().begin.line;
		final Expression expression = _throw.getExpression();
		
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "throw");
		super.addThrow(expression, ldNodeInfo);
	}
	
	private void process(ForeachStmt foreach)
	{
		final long line = foreach.getRange().get().begin.line;
		final VariableDeclarationExpr variableDeclaration = foreach.getVariable();
		final Expression iterableExpr = foreach.getIterable();
		final Statement body = foreach.getBody();
		final List<Statement> bodyStatements = body instanceof BlockStmt ? ((BlockStmt) body).getStatements() : new LinkedList<Statement>();
		if (bodyStatements.isEmpty() && body instanceof ExpressionStmt)
			bodyStatements.add(body);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "foreach");
		
		super.addForeach(variableDeclaration, iterableExpr, bodyStatements, ldNodeInfo);
	}
	private void process(ReturnStmt _return, Map<String, Object> info)
	{
		final long line = _return.getRange().get().begin.line;
		final Object expression = !_return.getExpression().isPresent() ? null : this.treatExpression(_return.getExpression().get(), false, false, true, line);
		final int id = this.getJumpDestiny(info, NodeInfo.Type.Clause);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "return");

		super.addReturn(expression, id, ldNodeInfo);
	}
	private void process(BreakStmt _break, Map<String, Object> info)
	{
		final long line = _break.getRange().get().begin.line;
		final int id = this.getJumpDestiny(info, NodeInfo.Type.Switch,  NodeInfo.Type.CLoop, NodeInfo.Type.RLoop, NodeInfo.Type.FLoop ); // Y un break en un loop?
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "break");

		super.addBreak(id, ldNodeInfo);
	}
	private void process(ContinueStmt _continue, Map<String, Object> info)
	{
		final long line = _continue.getRange().get().begin.line;
		final Optional<SimpleName> label = _continue.getLabel();
		final int jumpId;
		final String labelText = label.isPresent() ? label.get().getIdentifier() : "";
		if (!labelText.equals(""))
			if (currentLabels.containsKey(labelText))
				jumpId = currentLabels.get(labelText);
			else
				jumpId = -1; // When the label is not found, return label -1
		else
			jumpId = this.getJumpDestiny(info, NodeInfo.Type.CLoop, NodeInfo.Type.RLoop, NodeInfo.Type.FLoop);
		
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "continue");
		
		super.addContinue(jumpId, labelText, ldNodeInfo);
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
			//final List<Expression> selectables = new LinkedList<Expression>();
			final Expression label = _case.getLabel().get();
			//selectables.add(label);
			super.addCase(label, null, statements, ldNodeInfo);
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
		super.addCondLoop(condition, bodyStatements, ldNodeInfo);
	}
	private void process(ForStmt _for)
	{
		final long line = _for.getRange().get().begin.line;
		final List<Expression> initialization = _for.getInitialization();
		final Optional<Expression> compare = _for.getCompare();
		final Expression condition = compare.isPresent() ? compare.get(): null;
		final Statement body = _for.getBody();
		final List<Expression> update = _for.getUpdate();
		final List<Statement> bodyStatements = body instanceof BlockStmt ? ((BlockStmt) body).getStatements() : new LinkedList<Statement>();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "for");
		
		if (!(body instanceof BlockStmt))
			bodyStatements.add(body);
		super.addForLoop(initialization, condition, bodyStatements, update, ldNodeInfo);
	}
	private void process(DoStmt _do)
	{
		final long line = _do.getRange().get().begin.line;
		final Expression condition = _do.getCondition();
		final Statement body = _do.getBody();

		final List<Statement> bodyStatements = body instanceof BlockStmt ? ((BlockStmt) body).getStatements() : new LinkedList<Statement>();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "do_while");
		
		if (!(body instanceof BlockStmt))
			bodyStatements.add(body);
		super.addRepeatLoop(condition, bodyStatements, ldNodeInfo);
	}
	
	// Expressions
	private void process(EnclosedExpr enclosedExpr, Map<String, Object> info)
	{
		final Expression inner = enclosedExpr.getInner().get();
		info.put("enclosed", true);
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
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "var", modifiers, type);
this.addVariableToContext(new VariableRecord(name, modifiers, type));
				super.addVariable(name, true, true, false, false, ldNodeInfo);
			}
			else
			{
				final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, true, "name", modifiers, type);
				final Variable name = new Variable(variable.getName(), true, true, false, ldNodeInfo0); // TODO THIS IS NOT ALWAYS A VARIABLE
				final Object initializer0 = this.treatExpression(initializer, false, false, true, line);
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "var");
this.addVariableToContext(new VariableRecord(name.getName(), modifiers, type));
				super.addEquality(name, initializer0, ldNodeInfo);
			}
		}
	}
	private void process(AssignExpr assignation)
	{
		final long line = assignation.getRange().get().begin.line;
		//final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, "var");
		//final Variable target = new Variable(assignation.getTarget(), false, true, false, ldNodeInfo0); // TODO This is not always a variable
		final Object target = this.treatExpression(assignation.getTarget(), false, true, false, line);
		final Object value = this.treatExpression(assignation.getValue(), false, false, true, line);
		final String operator = assignation.getOperator().asString();
		final LDASTNodeInfo ldNodeInfo;

		if (operator.equals("="))
		{
			ldNodeInfo = new LDASTNodeInfo(line, true, "assign");
			super.addEquality(target, value, ldNodeInfo);
		}
		else
		{	
			ldNodeInfo = new LDASTNodeInfo(line, true, "assign", operator);
			super.addEquality(operator, target, value, ldNodeInfo);
		}
	}
	private void process(CastExpr cast, Map<String, Object> info)
	{
		boolean isEnclosedExpr = info.containsKey("enclosed");
		final long line = cast.getRange().get().begin.line;
		final Object type = cast.getType();
		final Object expression = this.treatExpression(cast.getExpression(), false, false, true, line);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "typeTransformation");

		if (expression instanceof Variable)
		{
			final Variable var = (Variable) expression;
			final Boolean declaration = var.declaration;
			final String varName = var.getName();
			
			final VariableRecord newContextVariable = this.getVarByName(this.variableContexts.peek(), varName);
			final boolean global = (declaration && this.variableContexts.size() == 1) || (!declaration && newContextVariable == null);
			
			final VariableRecord newContextVariable0 = global ? this.getVarByName(this.variableContexts.firstElement(), varName) : newContextVariable;
			EnumSet<Modifier> modifiers = newContextVariable0.varModifiers;
			
			this.createNewContext();
			this.addVariableToContext(new VariableRecord(varName, modifiers, (Type) type));
			if (isEnclosedExpr)
				super.addTypeTransformation(type, expression, ldNodeInfo, isEnclosedExpr);
			else
				super.addTypeTransformation(type, expression, ldNodeInfo);
			this.destroyContext();
		}

		else
			super.addTypeTransformation(type, expression, ldNodeInfo, isEnclosedExpr);
	}
	private void process(MethodCallExpr methodCall)
	{
		final long line = methodCall.getRange().get().begin.line;
		final Expression scope = methodCall.getScope().isPresent() ? methodCall.getScope().get() : null;
		final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(line, true, "name", "name");
		final Literal function = new Literal(methodCall.getName(), ldNodeInfo0);
		final List<Object> arguments = this.treatExpressions(methodCall.getArguments(), false, false, true, line); // TODO Paso de argumentos por referencia, OJO!!
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "method");

		super.addCall(scope, function, arguments, ldNodeInfo);
	}
	private void process(BinaryExpr binaryExpression)
	{
		final long line = binaryExpression.getRange().get().begin.line;
		final String operation = binaryExpression.getOperator().asString();
		final List<Object> operands = new LinkedList<Object>();
		final Object left = this.treatExpression(binaryExpression.getLeft(), false, false, true, line);
		final Object right = this.treatExpression(binaryExpression.getRight(), false, false, true, line);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "binary");

		operands.add(left);
		operands.add(right);
		super.addOperation(operation, operands, ldNodeInfo);
	}
	private void process(UnaryExpr unaryExpression)
	{
		final long line = unaryExpression.getRange().get().begin.line;
		final String operation = unaryExpression.getOperator().asString();
		final boolean isPostfix = unaryExpression.isPostfix(); // DECIDIR SI ES ++i o i++ 
		//final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "unary", postfix);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "unary", isPostfix);
		
		final Object operand = this.treatExpression(unaryExpression.getExpression(), false, true, true, line);
		super.addUnaryOperation(operation, operand, ldNodeInfo);
	}
	private void process(ObjectCreationExpr objectCreationExpression)
	{
		final long line = objectCreationExpression.getRange().get().begin.line;
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "object creation");
		final Literal scope = new Literal(objectCreationExpression.getType().getName(), ldNodeInfo);
		final Literal function = new Literal(objectCreationExpression, "<constructor>", ldNodeInfo);
		final List<Expression> arguments = objectCreationExpression.getArguments();

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
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "ternary");

		thenStatements.add(then);
		elseStatements.add(_else);
		super.addIf(condition, thenStatements, elseStatements, ldNodeInfo);
	}
	private void process(InstanceOfExpr instanceOfExpr)
	{
		final long line = instanceOfExpr.getRange().get().begin.line;
		final Object expression = this.treatExpression(instanceOfExpr.getExpression(), false, false, true, line);
		final Object type = instanceOfExpr.getType();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "typecheck");
		
		super.addTypeCheck(expression, type, ldNodeInfo);
	}
	private void process(SuperExpr superExpr) // TODO: Treat super & this properly
	{
		final long line = superExpr.getRange().get().begin.line;
		final Optional<Expression> classExpr = superExpr.getClassExpr(); // UNUSED
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "reference");

		super.addSuperReference("super", ldNodeInfo);
	}
	private void process(ThisExpr thisExpr)
	{
		final long line = thisExpr.getRange().get().begin.line;
		final Optional<Expression> classExpr = thisExpr.getClassExpr(); // UNUSED
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "reference");

		super.addSuperReference("this", ldNodeInfo);
	}
	private void process(ExplicitConstructorInvocationStmt eciExpr) // super(x)
	{
		final long line = eciExpr.getRange().get().begin.line;
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "explicit constructor invocation");
		final boolean isThis = eciExpr.isThis();
		final Literal scope = isThis ? new Literal(new SimpleName("this"), ldNodeInfo) : new Literal(new SimpleName("super"), ldNodeInfo);
		final Literal function = new Literal(eciExpr, "<constructor>", ldNodeInfo);
		final List<Expression> arguments = eciExpr.getArguments();
		
		super.addCall(scope, function, arguments, ldNodeInfo);
	}
	
	// Types
	private void process(ClassExpr _clazz)
	{
		final long line = _clazz.getRange().get().begin.line;
		final Type type = _clazz.getType();
		final String value;
		if (type instanceof ClassOrInterfaceType)
			value = ((ClassOrInterfaceType) type).getNameAsString();
		else if (type instanceof PrimitiveType)
			value = ((PrimitiveType) type).asString();
		else
			value = "";
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "class expression");
		
		super.addType(value, ldNodeInfo);
	}
	private void process(ClassOrInterfaceType type)
	{
		final long line = type.getRange().get().begin.line;
		final String value = type.getNameAsString();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "type");
		
		super.addType(value, ldNodeInfo);
	}
	private void process(ArrayType type)
	{
		final long line = type.getRange().get().begin.line;
		final String value = type.getComponentType().toString();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "arrayType", "arrayType");
		
		super.addType(value, ldNodeInfo);
	}
	private void process(PrimitiveType type)
	{
		final long line = type.getRange().get().begin.line;
		final String value = type.asString();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "type", type.getType());
		
		super.addType(value, ldNodeInfo);
	}
	
	// Variables
	private void process(Parameter parameter)
	{
		final long line = parameter.getRange().get().begin.line;
		final Type type = parameter.getType();
		final String name = parameter.getNameAsString();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "var", type);

//		this.addVariableToContext(name); TYPE VARIABLE 
this.addVariableToContext(new VariableRecord(name, type));
		
		super.addVariable(name, true, true, false, false, ldNodeInfo);
	}
	private void process(FieldAccessExpr fieldAccessExpression, Map<String,Object> info)
	{
		final long line = fieldAccessExpression.getRange().get().begin.line;
		//final String value = fieldAccessExpression.toString();
		
		final Optional<Expression> scopeExpr = fieldAccessExpression.getScope();
		final SimpleName nameExpr = fieldAccessExpression.getName();
		
		//final Variable scopeVar = scopeExpr.isPresent() ? new Variable(scopeExpr.get(), false, false, true, new LDASTNodeInfo(line, "var")) : null; // Correct Logic
		final Variable scopeVar = scopeExpr.isPresent() ? new Variable(scopeExpr.get(), false, true, true, new LDASTNodeInfo(line, true, "var")) : null; // Patch to pick up the object declaration in a Field Access
		final Variable nameVar;
		
		if((boolean)info.get("patternZone"))
			nameVar = new Variable(nameExpr, false, true, false, new LDASTNodeInfo(line, true, "var"));
		else
			nameVar = new Variable(nameExpr, false, false, true, new LDASTNodeInfo(line, true, "var"));
		
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "field");
		super.addFieldAccess(scopeVar, nameVar, ldNodeInfo);
		
//		final VariableRecord newContextVariable = this.getVarByName(this.variableContexts.peek(), scopeVar.name);
//		final boolean global = (this.variableContexts.size() == 1 || newContextVariable == null);
//		
//		final VariableRecord newContextVariable0 = global ? this.getVarByName(this.variableContexts.firstElement(), value) : newContextVariable;	
//		final boolean isStaticClassname = newContextVariable0 == null;
//		
//		final Object[] varInfo;
//		if (!isStaticClassname)
//		{
//			varInfo = new Object[2];
//			varInfo[0] = newContextVariable0.varModifiers; 
//			varInfo[1] = newContextVariable0.varType;
//		}
//		else
//		{
//			varInfo = new Object[2];
//			varInfo[0] = null; // TODO must be an empty list of modifiers
//			varInfo[1] = value;
//		}
//		
//		
//		super.addVariable(value, false, false, true, true, ldNodeInfo);
	}
	private void process(NameExpr nameExpression)
	{
		final long line = nameExpression.getRange().get().begin.line;
		final String name = nameExpression.getNameAsString();
		
		final VariableRecord newContextVariable = this.getVarByName(this.variableContexts.peek(), name);
		final boolean global = (this.variableContexts.size() == 1 || newContextVariable == null);
		
		final VariableRecord newContextVariable0 = global ? this.getVarByName(this.variableContexts.firstElement(), name) : newContextVariable;	
		final boolean isStaticClassname = newContextVariable0 == null;
		
		final Object[] info;
		if (!isStaticClassname)
		{
			info = new Object[2];
			info[0] = newContextVariable0.varModifiers; 
			info[1] = newContextVariable0.varType;
		}
		else
		{
			info = new Object[2];
			info[0] = null; // TODO must be an empty list of modifiers
			info[1] = name;
		}
		
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "var", info);
		super.addVariable(name, false, true, true, global, ldNodeInfo); 
	}
	private void process(SimpleName simpleName)
	{
		final long line = simpleName.getRange().get().begin.line;
		final String name = simpleName.getId();
		
		final VariableRecord newContextVariable = this.getVarByName(this.variableContexts.peek(), name);
		final boolean global = (this.variableContexts.size() == 1 || newContextVariable == null);
		
		final VariableRecord newContextVariable0 = global ? this.getVarByName(this.variableContexts.firstElement(), name) : newContextVariable;	
		final Object[] info = {newContextVariable0.varModifiers, newContextVariable0.varType};
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "var", info);

		super.addVariable(name, false, false, true, false, ldNodeInfo);  // TODO REVISAR SimpleName
	}
	private void process(Variable variable)
	{
		if(variable.node instanceof ArrayAccessExpr)
			processArrayAccess(variable);
		else	
			processVar(variable);
	}

	private void processVar(Variable variable)
	{
		final String name = variable.getName();
		final boolean declaration = variable.declaration;
		final boolean definition = variable.definition;
		final boolean use = variable.use;
		final VariableRecord newContextVariable = this.getVarByName(this.variableContexts.peek(), name);
		final boolean global = (declaration && this.variableContexts.size() == 1) || (!declaration && newContextVariable == null);
		
		final VariableRecord newContextVariable0 = global ? this.getVarByName(this.variableContexts.firstElement(), name) : newContextVariable;
		
//final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "var", modifiers, type);
		
		final LDASTNodeInfo ldNodeInfo;
		if(!declaration && newContextVariable0 != null)
		{
			final Object[] info = {newContextVariable0.varModifiers, newContextVariable0.varType};
			ldNodeInfo = new LDASTNodeInfo(variable.ldNodeInfo.getArchive(),variable.ldNodeInfo.getClassName(),
											variable.ldNodeInfo.getLine(), variable.ldNodeInfo.isExpression(),
											variable.ldNodeInfo.getConstruction(), info);
		}
		else
			ldNodeInfo = variable.ldNodeInfo;
		super.addVariable(name, declaration, definition, use, global, ldNodeInfo);
	}
	private void processArrayAccess(Variable variable)
	{
		final ArrayAccessExpr expression = (ArrayAccessExpr) variable.node;
		final long line = expression.getRange().get().begin.line;
//		final String varName = expression.getName().toString();
//		final String varIndex = expression.getIndex().toString();
//		final String name = varName+"["+varIndex+"]";
		final LDASTNodeInfo ldNodeInfo = variable.ldNodeInfo;
		
		final Object name0 = this.treatExpression(expression.getName(), variable.declaration, variable.definition, variable.use, line);
		final Object index0 = this.treatExpression(expression.getIndex(), false, false, true, line);
		
		super.addDataConstructorAccess(name0, index0, ldNodeInfo);
		
//		final VariableRecord newContextVariable = this.getVarByName(this.variableContexts.peek(), varName);
//		final boolean global = (declaration && this.variableContexts.size() == 1) || (!declaration && newContextVariable == null);
		
//		final VariableRecord newContextVariable0 = global ? this.getVarByName(this.variableContexts.firstElement(), name) : newContextVariable;
		
//final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "var", modifiers, type);
		
//		final LDASTNodeInfo ldNodeInfo;
//		if(!declaration && newContextVariable0 != null)
//		{
//			final Object[] info = {newContextVariable0.varModifiers, newContextVariable0.varType};
//			ldNodeInfo = new LDASTNodeInfo(variable.ldNodeInfo.getArchive(),variable.ldNodeInfo.getClassName(),
//											variable.ldNodeInfo.getLine(),variable.ldNodeInfo.getConstruction(), info);
//		}
//		else
//			ldNodeInfo = variable.ldNodeInfo;
//		super.addVariable(name, declaration, definition, use, global, ldNodeInfo);
	}

	// Literals
	private void process(BooleanLiteralExpr _boolean)
	{
		final long line = _boolean.getRange().get().begin.line;
		final boolean value = _boolean.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "boolean");

		super.addLiteral(value + "", ldNodeInfo);
	}
	private void process(CharLiteralExpr _char)
	{
		final long line = _char.getRange().get().begin.line;
		final String value = _char.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "char");

		super.addLiteral(value + "", ldNodeInfo);
	}
	private void process(DoubleLiteralExpr _double)
	{
		final long line = _double.getRange().get().begin.line;
		final String value = _double.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "double");

		super.addLiteral(value + "", ldNodeInfo);
	}
	private void process(IntegerLiteralExpr integer)
	{
		final long line = integer.getRange().get().begin.line;
		final String value = integer.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "int");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(LongLiteralExpr _long)
	{
		final long line = _long.getRange().get().begin.line;
		final String value = _long.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "long");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(StringLiteralExpr string)
	{
		final long line = string.getRange().get().begin.line;
		final String value = string.getValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "String");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(NullLiteralExpr _null)
	{
		final long line = _null.getRange().get().begin.line;
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, true, "null");

		super.addLiteral("null", ldNodeInfo);
	}
	private void process(Literal literal)
	{
		final String name = literal.getName();
		final LDASTNodeInfo ldNodeInfo = literal.ldNodeInfo;

		super.addLiteral(name, ldNodeInfo);
	}

	// Auxiliary
	private void process(EmptyStmt stmt)
	{
		// No representation TODO: (Analyze the representation for these statements in JavaCodeFactory)
	}
	private void process(AssertStmt stmt)
	{
		// No representation TODO: (Analyze the representation for these statements in JavaCodeFactory)
	}
	
	private List<Object> treatExpressions(List<Expression> expressions, boolean declaration, boolean definition, boolean use, long line)
	{
		final List<Object> elements = new LinkedList<Object>();

		for (Expression expression : expressions)
			elements.add(this.treatExpression(expression, declaration, definition, use, line));

		return elements;
	}
	private Object treatExpression(Expression expression, boolean declaration, boolean definition, boolean use, long line)
	{
		if (expression instanceof NameExpr)
			return new Variable(expression, declaration, definition, use, new LDASTNodeInfo(line, true, "var"));
//		if (expression instanceof ArrayAccessExpr)
//			return new Variable(expression, declaration, definition, new LDASTNodeInfo(line, "array access"));
		return expression;
	}

	private int getJumpDestiny(Map<String, Object> info, NodeInfo.Type... seekingTypes)
	{
		@SuppressWarnings("unchecked")
		final List<LASTFactory.Branch> ancestors = (List<LASTFactory.Branch>) info.get("ancestors");

		for (int ancestorIndex = ancestors.size() - 1; ancestorIndex >= 0; ancestorIndex--)
		{
			final LASTFactory.Branch ancestor = ancestors.get(ancestorIndex);
			final NodeInfo.Type type = (NodeInfo.Type) ancestor.getNodeType();

			for (NodeInfo.Type seekingType : seekingTypes)
				if (seekingType == type)
					switch(seekingType){
						case Switch:
							return ancestor.getNodeId() + 2; // Node Cases
						case CLoop:
							return ancestor.getNodeId() + 2; // Node Condition after creating the CLoop node
						case RLoop:
							return ancestor.getNodeId() + 3; // Node Condition after creating the RLoop node
						case FLoop:
							return ancestor.getNodeId() + 5; // Node Update after creating the FLoop node
						case Clause:
							return ancestor.getNodeId() + 6; // Node Result after creating the Clause node
						default:
							break;
					}
		}

		throw new RuntimeException("Wrong jump instruction");
	}

	static class Variable
	{
		private final Node node;
		private final String name;
		private final boolean declaration;
		private final boolean definition;
		private final boolean use;
		private final LDASTNodeInfo ldNodeInfo;

		public Variable(Node node, boolean declaration, boolean definition, boolean use, LDASTNodeInfo ldNodeInfo)
		{
			this(node, null, declaration, definition, use, ldNodeInfo);
		}
		public Variable(Node node, String name, boolean declaration, boolean definition, boolean use, LDASTNodeInfo ldNodeInfo)
		{
			this.node = node;
			this.name = name;
			this.declaration = declaration;
			this.definition = definition;
			this.use = use;
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
	private static class VariableRecord
	{
		private final String varName;
		private final EnumSet<Modifier> varModifiers;
		private final Type varType;
		
		public VariableRecord(String name, Type type)
		{
			this(name, null, type);
		}
		public VariableRecord(String name, EnumSet<Modifier> modifiers, Type type)
		{
			this.varName = name;
			this.varModifiers = modifiers;
			this.varType = type;
		}
		public Boolean isVarName(String name)
		{
			return name.equals(this.varName);
		}
		public Boolean equals(VariableRecord vr)
		{
			return this.varName.equals(vr.varName) && this.varType == vr.varType;
		}
		public String toString()
		{
			return "("+varName+","+varType+")";
		}
	}
	
	// Variable global/local
	private Stack<List<VariableRecord>> variableContexts = new Stack<List<VariableRecord>>();

	
	private void createContext()
	{
		final List<VariableRecord> variableContext = new LinkedList<VariableRecord>();
		final List<VariableRecord> lastVariableContext = this.variableContexts.size() == 0 ? new LinkedList<VariableRecord>() : this.variableContexts.peek();

		variableContext.addAll(lastVariableContext);
		this.variableContexts.add(variableContext);
	}
	private void createNewContext()
	{
		this.variableContexts.add(new LinkedList<VariableRecord>());
	}
	private void addVariableToContext(VariableRecord record)
	{
		final String variableName = record.varName;
		if (variableName.equals("_"))
			return;

		final List<VariableRecord> variableContext = this.variableContexts.peek();
		if (!variableContext.contains(record))
			variableContext.add(record);
	}
	private void destroyContext()
	{
		this.variableContexts.pop();
	}	

	private VariableRecord getVarByName(List<VariableRecord> context, String varName)
	{
		for (VariableRecord vr : context)
			if (vr.isVarName(varName))
				return vr;
		return null;
	}
	
}