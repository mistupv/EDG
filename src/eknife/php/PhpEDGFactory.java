package eknife.php;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.Charset;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;

import edg.EDGFactory;
import edg.LDASTNodeInfo;
import edg.graph.EDG;
import edg.graph.NodeInfo;
import eknife.php.PHPParser.*;
import misc.Misc;

public class PhpEDGFactory extends EDGFactory
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static EDG createEDG(String sourcePath)
	{
		return PhpEDGFactory.createEDG(sourcePath, true);
	}
	public static EDG createEDG(String sourcePath, boolean generateArcs)
	{
		final File programFile = new File(sourcePath);
		final List<File> files = programFile.isFile() ? Arrays.asList(programFile) : Misc.getFiles(programFile, new String[] { ".java" }, true);
		final PhpEDGFactory edgFactory = new PhpEDGFactory(sourcePath);

		return edgFactory.createEDG(generateArcs, files);
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private final String codebase;

	private PhpEDGFactory(String codebase)
	{
		this.codebase = codebase;
	}

	private EDG createEDG(boolean generateArcs, List<File> files)
	{
		try
		{
			final Charset charset = Charset.forName("UTF-8");
			final CharStream charStream = CharStreams.fromPath(Paths.get(this.codebase), charset);
			final PHPLexer lexer = new PHPLexer(charStream);
			final CommonTokenStream tokens = new CommonTokenStream(lexer);
			final PHPParser parser = new PHPParser(tokens);
			final RuleContext tree = parser.htmlDocument();
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(this.codebase, 0, "EDG");

			return super.createEDG(generateArcs, new RuleContext[] { tree }, ldNodeInfo);
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return null;
		}
	}
	protected void processElement(Object element, Map<String, Object> info)
	{
		if (element instanceof ImportStatementContext)
			this.process((ImportStatementContext) element, info);
		else if (element instanceof ClassDeclarationContext)
			this.process((ClassDeclarationContext) element, info);
		else if (element instanceof ClassStatementContext)
			this.process((ClassStatementContext) element, info);
		else if (element instanceof FormalParameterContext)
			this.process((FormalParameterContext) element, info);
		else if (element instanceof FunctionDeclarationContext)
			this.process((FunctionDeclarationContext) element, info);
		else if (element instanceof NewExprContext)
			this.process((NewExprContext) element, info);
		else if (element instanceof IfStatementContext)
			this.process((IfStatementContext) element, info);
		else if (element instanceof ComparisonExpressionContext)
			this.process((ComparisonExpressionContext) element, info);
		else if (element instanceof AssignmentExpressionContext)
			this.process((AssignmentExpressionContext) element, info);
		else if (element instanceof ChainContext)
			this.process((ChainContext) element, info);
		else if (element instanceof ReturnStatementContext)
			this.process((ReturnStatementContext) element, info);
		else if (element instanceof ConstantInititalizerContext)
			this.process((ConstantInititalizerContext) element, info);
		else if (element instanceof TypeRefContext)
			this.process((TypeRefContext) element, info);
		else if (element instanceof KeyedVariableContext)
			this.process((KeyedVariableContext) element, info);
		else if (element instanceof KeyedFieldNameContext)
			this.process((KeyedFieldNameContext) element, info);
		else if (element instanceof ConstantContext)
			this.process((ConstantContext) element, info);
		else if (element instanceof TerminalNode)
			this.process((TerminalNode) element, info);
		else if (element instanceof Variable)
			this.process((Variable) element, info);
		else if (element instanceof Literal)
			this.process((Literal) element, info);
		else
			this.processElements((ParserRuleContext) element, info);
	}

	private void process(ImportStatementContext context, Map<String, Object> info)
	{
		final TerminalNode _import = context.Import();
		final TerminalNode namespace = context.Namespace();

		System.out.println("Import) " + _import.getText() + " => " + namespace.getText());
	}
	private void process(ClassDeclarationContext context, Map<String, Object> info)
	{
		final String name = context.identifier().getText();
		final List<ClassStatementContext> classStatements = context.classStatement();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(this.codebase, 0, "class");

		this.createContext();
		super.addModule(name, classStatements, ldNodeInfo);
		this.destroyContext();
	}
	private void process(ClassStatementContext context, Map<String, Object> info)
	{
		final List<VariableInitializerContext> variableInitializers = context.variableInitializer();
		final IdentifierContext identifier = context.identifier();
		final FormalParameterListContext parameterList = context.formalParameterList();
		final MethodBodyContext methodBody = context.methodBody();

		if (!variableInitializers.isEmpty())
		{
			final VariableInitializerContext variable = variableInitializers.get(0);
			final TerminalNode varName = variable.VarName();
			final String name = varName.getText();
			final ConstantInititalizerContext initializer = variable.constantInititalizer();

			if (initializer == null)
			{
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "var");
				super.addVariable(name, true, false, true, ldNodeInfo);
			}
			else
			{
				final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(0, "var");
				final Variable variable0 = new Variable(varName, true, true, ldNodeInfo0);
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "assignment");
				super.addEquality(variable0, initializer, ldNodeInfo);
			}
		}
		else if (identifier != null)
		{
			final EDGFactory.Branch parent = (EDGFactory.Branch) info.get("parent");
			final NodeInfo.Type parentType = parent.getNodeType();
			final String name = identifier.getText();

			switch (parentType)
			{
				case Module:
					final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(0, "routine");
					super.addRoutine(name, new Object[] { context }, ldNodeInfo0);
					break;
				case Routine:
					final List<FormalParameterContext> parameters = parameterList.formalParameter();
					final BlockStatementContext block = methodBody.blockStatement();
					final InnerStatementListContext statementList = block.innerStatementList();
					final List<InnerStatementContext> statements = statementList.innerStatement();
					final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "clause");
					super.addClause(parameters, null, statements, ldNodeInfo);
					break;
				default:
					throw new RuntimeException("Node type not contemplated: " + parentType);
			}
		}
		else
			System.out.println("ClassStatement) " + context.getText());
	}
	private void process(FormalParameterContext context, Map<String, Object> info)
	{
		final VariableInitializerContext initializer = context.variableInitializer();
		final String name = initializer.VarName().getText();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "parameter");

		super.addVariable(name, true, false, false, ldNodeInfo);
	}
	private void process(FunctionDeclarationContext context, Map<String, Object> info)
	{
		System.out.println("FunctionDeclaration) " + context.getText());
	}
	private void process(NewExprContext context, Map<String, Object> info)
	{
		final TypeRefContext scope = context.typeRef();
		final List<ActualArgumentContext> arguments = context.arguments().actualArgument();
		final LDASTNodeInfo ldNodeInfo0 = new LDASTNodeInfo(0, "name");
		final Literal function = new Literal(null, "__constructor", ldNodeInfo0);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "call");

		super.addCall(scope, function, arguments, ldNodeInfo);
	}
	private void process(IfStatementContext context, Map<String, Object> info)
	{
		final ExpressionContext condition = context.parenthesis().expression();
		final StatementContext thenStatement = context.statement();
		final List<StatementContext> thenStatements = Arrays.asList(thenStatement);
		final List<StatementContext> elseStatements = Arrays.asList();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "if");

		super.addIf(condition, thenStatements, elseStatements, ldNodeInfo);
	}
	private void process(ComparisonExpressionContext context, Map<String, Object> info)
	{
		final ComparisonExpressionContext comparison = context.comparisonExpression();
		final AdditionExpressionContext additionExpression = context.additionExpression();
		final List<ParserRuleContext> operands = Arrays.asList(comparison, additionExpression);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "comparison");

		if (comparison == null)
			this.processElement(additionExpression, info);
		else
			super.addOperation("==", operands, ldNodeInfo);
	}
	private void process(AssignmentExpressionContext context, Map<String, Object> info)
	{
		final ChainContext chain = context.chain(0);
		final ExpressionContext expression = context.expression();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "assignment");

		super.addEquality(chain, expression, ldNodeInfo);
	}
	private void process(ChainContext context, Map<String, Object> info)
	{
		final ChainBaseContext chainBase = context.chainBase();
		final FunctionCallContext functionCall = context.functionCall();
		final NewExprContext newExpr = context.newExpr();
		final MemberAccessContext memberAccess = context.memberAccess(0);

		if (chainBase != null && memberAccess != null)
		{
			final KeyedFieldNameContext method = memberAccess.keyedFieldName();
			final ActualArgumentsContext actualArguments = memberAccess.actualArguments();

			if (actualArguments != null)
			{
				final List<ActualArgumentContext> arguments = actualArguments.arguments().actualArgument();
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "call");
				super.addCall(chainBase, method, arguments, ldNodeInfo);
			}
			else
			{
				final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "var");
				super.addVariable(context.getText(), false, false, true, ldNodeInfo);
			}
		}
		else if (chainBase != null)
		{
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "var");
			super.addVariable(chainBase.getText(), false, false, false, ldNodeInfo);
		}
	}
	private void process(ReturnStatementContext context, Map<String, Object> info)
	{
		final ExpressionContext expression = context.expression();
		final int id = this.getJumpDestiny(info, NodeInfo.Type.Clause);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "return");

		super.addReturn(expression, id, ldNodeInfo);
	}
	private void process(ConstantInititalizerContext context, Map<String, Object> info)
	{
		final String value = context.getText();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "constant initializer");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(TypeRefContext context, Map<String, Object> info)
	{
		final String value = context.getText();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "type ref");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(KeyedVariableContext context, Map<String, Object> info)
	{
		final String value = context.getText();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "keyed variable");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(KeyedFieldNameContext context, Map<String, Object> info)
	{
		final String value = context.getText();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "key field name");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(ConstantContext context, Map<String, Object> info)
	{
		final String value = context.getText();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "constant");

		super.addLiteral(value, ldNodeInfo);
	}
	private void process(TerminalNode terminalNode, Map<String, Object> info)
	{
		final String value = terminalNode.getText();

		System.out.println(value);
	}
	private void process(Variable variable, Map<String, Object> info)
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
	private void process(Literal literal, Map<String, Object> info)
	{
		final String name = literal.getName();
		final LDASTNodeInfo ldNodeInfo = literal.ldNodeInfo;

		super.addLiteral(name, ldNodeInfo);
	}

	// Auxiliary
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

	private void processElements(ParserRuleContext context, Map<String, Object> info)
	{
		final List<Object> elements = this.getElements(context);

		for (Object element : elements)
		{
			if (element instanceof ParserRuleContext)
				this.processElement((ParserRuleContext) element, info);
			else if (element instanceof TerminalNode)
				this.processElement((TerminalNode) element, info);
			else
				throw new RuntimeException("Error!");
		}
	}
	@SuppressWarnings("unchecked")
	private List<Object> getElements(ParserRuleContext context)
	{
		final List<Object> contexts = new LinkedList<Object>();
		final Method[] methods = context.getClass().getDeclaredMethods();

		for (Method method : methods)
		{
			final String methodName = method.getName();
			final int parameterCount = method.getParameterCount();
			if (methodName.equals("getRuleIndex") || methodName.equals("enterRule") || methodName.equals("exitRule"))
				continue;
			if (parameterCount != 0)
				continue;

			try
			{
				final Object result = method.invoke(context);

				if (result instanceof List)
				{
					final List<Object> results = (List<Object>) result;
					for (Object result0 : results)
						contexts.add(result0);
				}
				else if (result instanceof ParserRuleContext || result instanceof TerminalNode)
				{
					contexts.add(result);
				}
				else if (result == null)
					;
				else
					throw new RuntimeException("Error");
			}
			catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e)
			{
				e.printStackTrace();
			}
		}

		return contexts;
	}

	private static class Variable
	{
		private final TerminalNode node;
		private final String name;
		private final boolean declaration;
		private final boolean definition;
		private final LDASTNodeInfo ldNodeInfo;

		public Variable(TerminalNode node, boolean declaration, boolean definition, LDASTNodeInfo ldNodeInfo)
		{
			this(node, null, declaration, definition, ldNodeInfo);
		}
		public Variable(TerminalNode node, String name, boolean declaration, boolean definition, LDASTNodeInfo ldNodeInfo)
		{
			this.node = node;
			this.name = name;
			this.declaration = declaration;
			this.definition = definition;
			this.ldNodeInfo = ldNodeInfo;
		}

		public int getLine()
		{
			return 0;
		}
		public String getName()
		{
			if (this.name != null)
				return this.name;
			return this.node.getText();
		}
	}
	private static class Literal
	{
		private final TerminalNode node;
		private final String name;
		private final LDASTNodeInfo ldNodeInfo;

		public Literal(TerminalNode node, LDASTNodeInfo ldNodeInfo)
		{
			this(node, null, ldNodeInfo);
		}
		public Literal(TerminalNode node, String name, LDASTNodeInfo ldNodeInfo)
		{
			this.node = node;
			this.name = name;
			this.ldNodeInfo = ldNodeInfo;
		}

		public int getLine()
		{
			return 0;
		}
		public String getName()
		{
			if (this.name != null)
				return this.name;
			return this.node.getText();
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