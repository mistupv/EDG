package eknife.erlang;

import java.io.File;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import edg.LDASTNodeInfo;
import edg.graph.EDG;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.traverser.EDGTraverser;
import eknife.config.Config;
import misc.Misc;

public class ErlangCodeFactory
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	private static final Config config = Config.getConfig();

	public static void createErlangFile(File outputFile, EDG edg)
	{
		ErlangCodeFactory.createErlangFile(outputFile, edg, null);
	}
	public static void createErlangFile(File outputFile, EDG edg, List<Node> slice)
	{
		final File scriptsFile = ErlangCodeFactory.config.getScriptsFile();
		final List<File> prevFiles = Misc.getFiles(scriptsFile, null, true);

		final ErlangCodeFactory erlangFactory = new ErlangCodeFactory(edg, slice);
		final OtpErlangList asts = erlangFactory.generate();
		final Launcher launcher = Launcher.getLauncher();
		launcher.launch("saver", "save", asts);

		final List<File> postFiles = Misc.getFiles(scriptsFile, null, true);
		final List<File> newFiles = Misc.disjunt(postFiles, prevFiles);

		if (outputFile.isDirectory())
			Misc.moveToFolder(newFiles, outputFile);
		else if (newFiles.size() == 1)
			Misc.moveFile(newFiles.get(0), outputFile);
		else
		{
			Misc.delete(newFiles);
			throw new RuntimeException(newFiles.size() + " output files cannot be saved at " + outputFile.getAbsolutePath());
		}
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private final EDG edg;
	private final List<Node> slice;
	private final List<String> functions = new LinkedList<String>();
	private final Map<Integer, Boolean> funundef = new Hashtable<Integer, Boolean>();

	private ErlangCodeFactory(EDG edg, List<Node> slice)
	{
		this.edg = edg;
		this.slice = slice;
	}

	private OtpErlangList generate()
	{
		final Node root = this.edg.getRootNode();
		final List<Node> modules = EDGTraverser.getChildren(root);
		final List<OtpErlangObject> modules0 = new LinkedList<OtpErlangObject>();

		for (Node module : modules)
		{
			if (this.slice != null && !this.slice.contains(module))
				continue;

			final LDASTNodeInfo ldNodeInfo = module.getData().getInfo();
			final String archive = ldNodeInfo.getArchive();
			final OtpErlangTuple moduleTuple = this.parseModule("./" + archive, module);
			modules0.add(moduleTuple);
		}

		final OtpErlangObject[] modulesElements = this.toArray(modules0);
		return new OtpErlangList(modulesElements);
	}

	// Modules
	private OtpErlangTuple parseModule(String archive, Node module)
	{
		final OtpErlangObject[] moduleElements = new OtpErlangObject[2];

		this.functions.clear();
		this.funundef.clear();
		moduleElements[0] = new OtpErlangAtom(archive);
		moduleElements[1] = this.parseModule(module);

		return new OtpErlangTuple(moduleElements);
	}
	private OtpErlangList parseModule(Node module)
	{
		final List<OtpErlangObject> bodyList = new LinkedList<OtpErlangObject>();

		// Others
		final LDASTNodeInfo ldNodeInfo = module.getData().getInfo();
		final OtpErlangList others = (OtpErlangList) ldNodeInfo.getInfo()[0];
		for (OtpErlangObject other : others)
			bodyList.add(other);

		// Functions
		final List<Node> routines = EDGTraverser.getChildren(module);
		for (Node routine : routines)
		{
			if (this.slice != null && !this.slice.contains(routine))
				continue;

			final OtpErlangTuple functionTuple = this.parseRoutine(routine);
			bodyList.add(functionTuple);
		}

		// Funundef functions
		final OtpErlangTuple[] funundefs = this.createFunundefs();
		for (OtpErlangTuple funundef : funundefs)
			bodyList.add(bodyList.size(), funundef);

		// Eof
		final OtpErlangObject[] eofElements = new OtpErlangObject[2];
		eofElements[0] = new OtpErlangAtom("eof");
		eofElements[1] = new OtpErlangLong(1);
		final OtpErlangTuple eofTuple = new OtpErlangTuple(eofElements);
		bodyList.add(eofTuple);

		// Clear ast
		this.removeFile(bodyList);
		this.removeExports(bodyList);

		return new OtpErlangList(this.toArray(bodyList));
	}
	private OtpErlangTuple parseRoutine(Node routine)
	{
		final String routineName = routine.getData().getName();
		final LDASTNodeInfo ldNodeInfo = routine.getData().getInfo();
		final long routineArity = (int) ldNodeInfo.getInfo()[0];
		final List<Node> clauses = EDGTraverser.getChildren(routine);
		final OtpErlangObject[] functionElements = new OtpErlangObject[5];

		functionElements[0] = new OtpErlangAtom("function");
		functionElements[1] = new OtpErlangLong(1);
		functionElements[2] = new OtpErlangAtom(routineName);
		functionElements[3] = new OtpErlangLong(routineArity);
		functionElements[4] = this.parseClauses(clauses);

		this.functions.add(routineName + "/" + routineArity);

		return new OtpErlangTuple(functionElements);
	}
	private OtpErlangList parseClauses(List<Node> clauses)
	{
		final List<OtpErlangObject> clausesList = new LinkedList<OtpErlangObject>();

		for (Node clause : clauses)
		{
			if (this.slice != null && !this.slice.contains(clause))
				continue;

			final OtpErlangTuple clauseTuple = this.parseClause(clause);
			clausesList.add(clauseTuple);
		}
		if (clausesList.isEmpty())
			clausesList.add(this.createEmptyClause());

		final OtpErlangObject[] clausesElements = this.toArray(clausesList);
		return new OtpErlangList(clausesElements);
	}
	private OtpErlangTuple parseClause(Node clause)
	{
		final Node parameters = EDGTraverser.getChild(clause, 0);
		final List<Node> parameterChildren = EDGTraverser.getChildren(parameters);
		final Node guard = EDGTraverser.getChild(clause, 1);
		final Node body = EDGTraverser.getChild(clause, 2);
		final List<Node> expressions = EDGTraverser.getChildren(body);
		final OtpErlangObject[] clauseElements = new OtpErlangObject[5];

		clauseElements[0] = new OtpErlangAtom("clause");
		clauseElements[1] = new OtpErlangLong(1);
		clauseElements[2] = this.parse(parameterChildren, true);
		clauseElements[3] = this.parseGuard(guard);
		clauseElements[4] = this.parse(expressions, false);

		return new OtpErlangTuple(clauseElements);
	}
	private OtpErlangList parseGuard(Node guard)
	{
		final List<Node> guardChildren = EDGTraverser.getChildren(guard);
		if (guardChildren.isEmpty())
			return new OtpErlangList();

		final List<OtpErlangObject> orGuardsList = new LinkedList<OtpErlangObject>();
		final Node orGuardExpression = guardChildren.get(0);
		final Node orGuard = EDGTraverser.getChild(orGuardExpression, 0);
		final List<Node> andGuardExpressions = EDGTraverser.getChildren(orGuard);

		for (Node andGuardExpression : andGuardExpressions)
		{
			if (this.slice != null && !this.slice.contains(andGuardExpression))
				continue;

			final List<OtpErlangObject> andGuardList = new LinkedList<OtpErlangObject>();
			final Node andGuard = EDGTraverser.getChild(andGuardExpression, 0);
			final List<Node> andGuardChildren = EDGTraverser.getChildren(andGuard);

			for (Node guardNode : andGuardChildren)
			{
				if (this.slice != null && !this.slice.contains(guardNode))
					continue;

				andGuardList.add(this.parse(guardNode));
			}

			final OtpErlangObject[] andGuardElements = this.toArray(andGuardList);
			final OtpErlangList andGuardsList = new OtpErlangList(andGuardElements);
			orGuardsList.add(andGuardsList);
		}

		final OtpErlangObject[] orGuardsElements = this.toArray(orGuardsList);
		return new OtpErlangList(orGuardsElements);
	}

	// Expressions
	private OtpErlangList parse(List<Node> nodes, boolean transformUnused)
	{
		final List<OtpErlangObject> expressionsList = new LinkedList<OtpErlangObject>();

		for (Node node : nodes)
		{
			if (!transformUnused && this.slice != null && !this.slice.contains(node))
				continue;

			final OtpErlangTuple expressionTuple = this.parse(node);
			expressionsList.add(expressionTuple);
		}
		if (!nodes.isEmpty() && expressionsList.isEmpty())
			expressionsList.add(this.parseEmptyLiteral());

		final OtpErlangObject[] expressionsElements = this.toArray(expressionsList);
		return new OtpErlangList(expressionsElements);
	}
	private OtpErlangTuple parse(Node node)
	{
		if (this.slice != null && !this.slice.contains(node))
			return this.parseEmpty(node);

		final NodeInfo.Type nodeType = node.getData().getType();
		final Node expression = nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(node, 0) : node;
		final NodeInfo.Type expressionType = expression.getData().getType();

		switch (expressionType)
		{
			case Block:
				return this.parseBlock(expression);
			case Switch:
				return this.parseSwitch(expression);
			case Call:
				return this.parseCall(expression);
			case Variable:
				return this.parseVariable(expression);
			case Literal:
				return this.parseLiteral(expression);
			case Operation:
				return this.parseOperation(expression);
			case Equality:
				return this.parseEquality(expression);
			case DataConstructor:
				return this.parseDataConstructor(expression);
			case List:
				return this.parseList(expression);
			case ListComprehension:
				return this.parseListComprehension(expression);
			case Generator:
				return this.parseGenerator(expression);
			case Filter:
				return this.parseFilter(expression);
			case Return:
				return this.parseReturn(expression);
			case Routine:
				return this.parseAnonymousRoutine(expression);
			default:
				throw new RuntimeException("Expression type not contemplated: " + expressionType);
		}
	}
	private OtpErlangTuple parseBlock(Node block)
	{
		final List<Node> blockExpressions = EDGTraverser.getChildren(block);
		final OtpErlangObject[] blockElements = new OtpErlangObject[3];

		blockElements[0] = new OtpErlangAtom("block");
		blockElements[1] = new OtpErlangLong(1);
		blockElements[2] = this.parse(blockExpressions, false);

		return new OtpErlangTuple(blockElements);
	}
	private OtpErlangTuple parseSwitch(Node _switch)
	{
		final LDASTNodeInfo ldNodeInfo = _switch.getData().getInfo();
		final String construction = ldNodeInfo.getConstruction();

		switch (construction)
		{
			case "if":
				return this.parseIf(_switch);
			case "case":
				return this.parseCase(_switch);
			default:
				throw new RuntimeException("Switch type not contemplated: " + construction);
		}
	}
	private OtpErlangTuple parseIf(Node _switch)
	{
		final Node cases = EDGTraverser.getChild(_switch, 1);
		final List<Node> casesChildren = EDGTraverser.getChildren(cases);
		final OtpErlangObject[] ifElements = new OtpErlangObject[3];

		ifElements[0] = new OtpErlangAtom("if");
		ifElements[1] = new OtpErlangLong(1);
		ifElements[2] = this.parseClauses(casesChildren);

		return new OtpErlangTuple(ifElements);
	}
	private OtpErlangTuple parseCase(Node _switch)
	{
		final Node selectorNode = EDGTraverser.getChild(_switch, 0);
		final Node selector = EDGTraverser.getChild(selectorNode, 0);
		final Node cases = EDGTraverser.getChild(_switch, 1);
		final List<Node> casesChildren = EDGTraverser.getChildren(cases);
		final OtpErlangObject[] caseElements = new OtpErlangObject[4];

		caseElements[0] = new OtpErlangAtom("case");
		caseElements[1] = new OtpErlangLong(1);
		caseElements[2] = this.parse(selector);
		caseElements[3] = this.parseClauses(casesChildren);

		return new OtpErlangTuple(caseElements);
	}
	private OtpErlangTuple parseCall(Node call)
	{
		final Node callee = EDGTraverser.getChild(call, 0);
		final Node scopeNode = EDGTraverser.getChild(callee, 0);
		final Node nameNode = EDGTraverser.getChild(callee, 1);
		final List<Node> scopeChildren = EDGTraverser.getChildren(scopeNode);
		final Node argumentsNode = EDGTraverser.getChild(call, 1);
		final List<Node> arguments = EDGTraverser.getChildren(argumentsNode);
		final OtpErlangObject[] callElements = new OtpErlangObject[4];

		callElements[0] = new OtpErlangAtom("call");
		callElements[1] = new OtpErlangLong(1);
		if (!scopeChildren.isEmpty() && (this.slice == null || this.slice.contains(callee)))
		{
			final Node scope = EDGTraverser.getChild(scopeNode, 0);
			final Node name = EDGTraverser.getChild(nameNode, 0);
			final OtpErlangObject[] remoteElements = new OtpErlangObject[4];

			remoteElements[0] = new OtpErlangAtom("remote");
			remoteElements[1] = new OtpErlangLong(1);
			remoteElements[2] = this.parse(scope);
			remoteElements[3] = this.parse(name);
			callElements[2] = new OtpErlangTuple(remoteElements);
		}
		else
		{
			final Node name = EDGTraverser.getChild(nameNode, 0);
			callElements[2] = this.parse(name);
		}
		callElements[3] = this.parse(arguments, true);

		// Replace undef with funundef
		final OtpErlangObject[] funName = ((OtpErlangTuple) callElements[2]).elements();
		if (funName[2].toString().equals("undef"))
		{
			funName[2] = new OtpErlangAtom("funundef");
			callElements[2] = new OtpErlangTuple(funName);
			this.funundef.put(arguments.size(), true);
		}

		return new OtpErlangTuple(callElements);
	}
	private OtpErlangTuple parseOperation(Node operation)
	{
		final List<Node> operationExpressions = EDGTraverser.getChildren(operation);

		switch (operationExpressions.size())
		{
			case 1:
				return this.parseUnaryOperation(operation);
			case 2:
				return this.parseBinaryOperation(operation);
			default:
				throw new RuntimeException("Operation size not contemplated: " + operationExpressions.size());
		}
	}
	private OtpErlangTuple parseUnaryOperation(Node operation)
	{
		final String nodeText = operation.getData().getName();
		final Node operationExpression = EDGTraverser.getChild(operation, 0);
		final OtpErlangObject[] operationElements = new OtpErlangObject[4];

		operationElements[0] = new OtpErlangAtom("op");
		operationElements[1] = new OtpErlangLong(1);
		operationElements[2] = new OtpErlangAtom(nodeText);
		operationElements[3] = this.parse(operationExpression);

		return new OtpErlangTuple(operationElements);
	}
	private OtpErlangTuple parseBinaryOperation(Node operation)
	{
		final String nodeText = operation.getData().getName();
		final Node operationExpression1 = EDGTraverser.getChild(operation, 0);
		final Node operationExpression2 = EDGTraverser.getChild(operation, 1);
		final OtpErlangObject[] operationElements = new OtpErlangObject[5];

		operationElements[0] = new OtpErlangAtom("op");
		operationElements[1] = new OtpErlangLong(1);
		operationElements[2] = new OtpErlangAtom(nodeText);
		operationElements[3] = this.parse(operationExpression1);
		operationElements[4] = this.parse(operationExpression2);

		return new OtpErlangTuple(operationElements);
	}
	private OtpErlangTuple parseDataConstructor(Node dataConstructor)
	{
		final List<Node> dataConstructorExpressions = EDGTraverser.getChildren(dataConstructor);
		final OtpErlangObject[] tupleElements = new OtpErlangObject[3];

		tupleElements[0] = new OtpErlangAtom("tuple");
		tupleElements[1] = new OtpErlangLong(1);
		tupleElements[2] = this.parse(dataConstructorExpressions, true);

		return new OtpErlangTuple(tupleElements);
	}
	private OtpErlangTuple parseList(Node list)
	{
		final List<Node> listExpressions = EDGTraverser.getChildren(list);
		if (listExpressions.isEmpty())
		{
			final OtpErlangObject[] nilElements = new OtpErlangObject[2];
			nilElements[0] = new OtpErlangAtom("nil");
			nilElements[1] = new OtpErlangLong(1);
			return new OtpErlangTuple(nilElements);
		}

		final Node listHead = listExpressions.get(0);
		final Node listTail = listExpressions.get(1);
		final OtpErlangObject[] listElements = new OtpErlangObject[4];

		listElements[0] = new OtpErlangAtom("cons");
		listElements[1] = new OtpErlangLong(1);
		listElements[2] = this.parse(listHead);
		listElements[3] = this.parse(listTail);

		return new OtpErlangTuple(listElements);
	}
	private OtpErlangTuple parseEquality(Node equality)
	{
		final Node equalityVariable = EDGTraverser.getChild(equality, 0);
		final Node equalityValue = EDGTraverser.getChild(equality, 1);
		final OtpErlangObject[] patternMatchingElements = new OtpErlangObject[4];

		patternMatchingElements[0] = new OtpErlangAtom("match");
		patternMatchingElements[1] = new OtpErlangLong(1);
		patternMatchingElements[2] = this.parse(equalityVariable);
		patternMatchingElements[3] = this.parse(equalityValue);

		return new OtpErlangTuple(patternMatchingElements);
	}
	private OtpErlangTuple parseListComprehension(Node listComprehension)
	{
		final Node generators = EDGTraverser.getChild(listComprehension, 0);
		final List<Node> generatorNodes = EDGTraverser.getChildren(generators);
		final Node value = EDGTraverser.getChild(listComprehension, 1);
		final Node valueNode = EDGTraverser.getChild(value, 0);
		final OtpErlangObject[] listComprehensionElements = new OtpErlangObject[4];

		listComprehensionElements[0] = new OtpErlangAtom("lc");
		listComprehensionElements[1] = new OtpErlangLong(1);
		listComprehensionElements[2] = this.parse(valueNode);
		listComprehensionElements[3] = this.parse(generatorNodes, false);

		return new OtpErlangTuple(listComprehensionElements);
	}
	private OtpErlangTuple parseGenerator(Node generator)
	{
		final Node pattern = EDGTraverser.getChild(generator, 0);
		final Node expression = EDGTraverser.getChild(generator, 1);
		final OtpErlangObject[] generatorElements = new OtpErlangObject[4];

		generatorElements[0] = new OtpErlangAtom("generate");
		generatorElements[1] = new OtpErlangLong(1);
		generatorElements[2] = this.parse(pattern);
		generatorElements[3] = this.parse(expression);

		return new OtpErlangTuple(generatorElements);
	}
	private OtpErlangTuple parseFilter(Node filter)
	{
		final List<Node> filterChildren = EDGTraverser.getChildren(filter);
		final Node expression = filterChildren.get(0);

		return this.parse(expression);
	}
	private OtpErlangTuple parseReturn(Node returnNode)
	{
		final Node returnExpression = EDGTraverser.getChild(returnNode, 0);

		return this.parse(returnExpression);
	}
	private OtpErlangTuple parseAnonymousRoutine(Node anonymousRoutine)
	{
		final String routineName = anonymousRoutine.getData().getName();

		if (routineName == null)
			return this.parseNormalAnonymousRoutine(anonymousRoutine);
		return this.parseNamedAnonymousRoutine(anonymousRoutine);
	}
	private OtpErlangTuple parseNormalAnonymousRoutine(Node anonymousRoutine)
	{
		final LDASTNodeInfo ldNodeInfo = anonymousRoutine.getData().getInfo();
		final boolean sugar = (boolean) ldNodeInfo.getInfo()[0];

		if (!sugar)
		{
			final List<Node> clauses = EDGTraverser.getChildren(anonymousRoutine);
			final OtpErlangObject[] funElements = new OtpErlangObject[3];
			final OtpErlangObject[] clausesElements = new OtpErlangObject[2];

			clausesElements[0] = new OtpErlangAtom("clauses");
			clausesElements[1] = this.parseClauses(clauses);
			funElements[0] = new OtpErlangAtom("fun");
			funElements[1] = new OtpErlangLong(1);
			funElements[2] = new OtpErlangTuple(clausesElements);

			return new OtpErlangTuple(funElements);
		}
		else
		{
			final Node clause = EDGTraverser.getChild(anonymousRoutine, 0);
			final Node body = EDGTraverser.getChild(clause, 2);
			final Node returnNode = EDGTraverser.getChild(body, 0);
			final Node expression = EDGTraverser.getChild(returnNode, 0);
			final Node call = EDGTraverser.getChild(expression, 0);
			final Node callee = EDGTraverser.getChild(call, 0);
			final Node scopeNode = EDGTraverser.getChild(callee, 0);
			final List<Node> scopeChildren = EDGTraverser.getChildren(scopeNode);
			final Node scope0 = scopeChildren.isEmpty() ? null : scopeChildren.get(0);
			final Node scope = scope0 == null ? null : EDGTraverser.getChild(scope0, 0);
			final Node nameNode = EDGTraverser.getChild(callee, 1);
			final Node name0 = EDGTraverser.getChild(nameNode, 0);
			final Node name = name0 == null ? null : EDGTraverser.getChild(name0, 0);
			final Node argumentsNode = EDGTraverser.getChild(call, 1);
			final List<Node> arguments = EDGTraverser.getChildren(argumentsNode);
			final int arity = arguments.size();

			final OtpErlangObject[] funElements = new OtpErlangObject[3];
			final OtpErlangObject[] functionElements;

			if (scope == null)
			{
				functionElements = new OtpErlangObject[3];
				functionElements[0] = new OtpErlangAtom("function");
				functionElements[1] = new OtpErlangAtom(name.getData().getName());
				functionElements[2] = new OtpErlangLong(arity);
			}
			else
			{
				final OtpErlangObject[] moduleElements = new OtpErlangObject[3];
				final OtpErlangObject[] nameElements = new OtpErlangObject[3];
				final OtpErlangObject[] arityElements = new OtpErlangObject[3];
				final NodeInfo.Type scopeType = scope.getData().getType();
				final NodeInfo.Type nameType = name.getData().getType();

				moduleElements[0] = new OtpErlangAtom(scopeType == NodeInfo.Type.Literal ? "atom" : "var");
				moduleElements[1] = new OtpErlangLong(1);
				moduleElements[2] = new OtpErlangAtom(scope.getData().getName());
				nameElements[0] = new OtpErlangAtom(nameType == NodeInfo.Type.Literal ? "atom" : "var");
				nameElements[1] = new OtpErlangLong(1);
				nameElements[2] = new OtpErlangAtom(name.getData().getName());
				arityElements[0] = new OtpErlangAtom("integer");
				arityElements[1] = new OtpErlangLong(1);
				arityElements[2] = new OtpErlangLong(arity);
				functionElements = new OtpErlangObject[4];
				functionElements[0] = new OtpErlangAtom("function");
				functionElements[1] = new OtpErlangTuple(moduleElements);
				functionElements[2] = new OtpErlangTuple(nameElements);
				functionElements[3] = new OtpErlangTuple(arityElements);
			}
			funElements[0] = new OtpErlangAtom("fun");
			funElements[1] = new OtpErlangLong(1);
			funElements[2] = new OtpErlangTuple(functionElements);

			return new OtpErlangTuple(funElements);
		}
	}
	private OtpErlangTuple parseNamedAnonymousRoutine(Node anonymousRoutine)
	{
		final String routineName = anonymousRoutine.getData().getName();
		final List<Node> clauses = EDGTraverser.getChildren(anonymousRoutine);
		final OtpErlangObject[] functionElements = new OtpErlangObject[4];
		final OtpErlangObject[] clausesElements = new OtpErlangObject[2];

		clausesElements[0] = new OtpErlangAtom("clauses");
		clausesElements[1] = this.parseClauses(clauses);
		functionElements[0] = new OtpErlangAtom("named_fun");
		functionElements[1] = new OtpErlangLong(1);
		functionElements[2] = new OtpErlangAtom(routineName);
		functionElements[3] = this.parseClauses(clauses);

		return new OtpErlangTuple(functionElements);
	}

	// Variables & literals
	private OtpErlangTuple parseVariable(Node variable)
	{
		final String variableName = variable.getData().getName();
		final OtpErlangObject[] variableElements = new OtpErlangObject[3];

		variableElements[0] = new OtpErlangAtom("var");
		variableElements[1] = new OtpErlangLong(1);
		variableElements[2] = new OtpErlangAtom(variableName);

		return new OtpErlangTuple(variableElements);
	}
	private OtpErlangTuple parseLiteral(Node literal)
	{
		final LDASTNodeInfo ldNodeInfo = literal.getData().getInfo();
		final String construction = ldNodeInfo.getConstruction();

		switch (construction)
		{
			case "atom":
				return this.parseAtom(literal);
			case "string":
				return this.parseString(literal);
			case "integer":
				return this.parseInteger(literal);
			case "char":
				return this.parseChar(literal);
			default:
				throw new RuntimeException("Literal type not contemplated: " + construction);
		}
	}
	private OtpErlangTuple parseAtom(Node atom)
	{
		final String atomText = atom.getData().getName();
		final OtpErlangObject[] atomElements = new OtpErlangObject[3];

		atomElements[0] = new OtpErlangAtom("atom");
		atomElements[1] = new OtpErlangLong(1);
		atomElements[2] = new OtpErlangAtom(atomText);

		return new OtpErlangTuple(atomElements);
	}
	private OtpErlangTuple parseString(Node string)
	{
		final String stringText = string.getData().getName();
		final OtpErlangObject[] stringElements = new OtpErlangObject[3];

		stringElements[0] = new OtpErlangAtom("string");
		stringElements[1] = new OtpErlangLong(1);
		stringElements[2] = new OtpErlangString(stringText);

		return new OtpErlangTuple(stringElements);
	}
	private OtpErlangTuple parseInteger(Node integer)
	{
		final String nodeText = integer.getData().getName();
		final long value = Long.parseLong(nodeText);
		final OtpErlangObject[] integerElements = new OtpErlangObject[3];

		integerElements[0] = new OtpErlangAtom("integer");
		integerElements[1] = new OtpErlangLong(1);
		integerElements[2] = new OtpErlangLong(value);

		return new OtpErlangTuple(integerElements);
	}
	private OtpErlangTuple parseChar(Node _char)
	{
		final String nodeText = _char.getData().getName();
		final long value = Long.parseLong(nodeText);
		final OtpErlangObject[] charElements = new OtpErlangObject[3];

		charElements[0] = new OtpErlangAtom("char");
		charElements[1] = new OtpErlangLong(1);
		charElements[2] = new OtpErlangLong(value);

		return new OtpErlangTuple(charElements);
	}

	// Auxiliaries
	private OtpErlangTuple parseEmpty(Node node)
	{
		if (EDGTraverser.isPatternZone(node))
			return this.parseEmptyVar();
		return this.parseEmptyLiteral();
	}
	private OtpErlangTuple parseEmptyVar()
	{
		final OtpErlangObject[] varElements = new OtpErlangObject[3];

		varElements[0] = new OtpErlangAtom("var");
		varElements[1] = new OtpErlangLong(1);
		varElements[2] = new OtpErlangAtom("_");

		return new OtpErlangTuple(varElements);
	}
	private OtpErlangTuple parseEmptyLiteral()
	{
		final OtpErlangObject[] varElements = new OtpErlangObject[3];

		varElements[0] = new OtpErlangAtom("atom");
		varElements[1] = new OtpErlangLong(1);
		varElements[2] = new OtpErlangAtom("undef");

		return new OtpErlangTuple(varElements);
	}
	private OtpErlangTuple createEmptyClause()
	{
		final OtpErlangObject[] varElements = new OtpErlangObject[5];

		varElements[0] = new OtpErlangAtom("clause");
		varElements[1] = new OtpErlangLong(1);
		varElements[2] = new OtpErlangList(this.parseEmptyVar());
		varElements[3] = new OtpErlangList(new OtpErlangObject[0]);
		varElements[4] = new OtpErlangList(this.parseEmptyLiteral());

		return new OtpErlangTuple(varElements);
	}
	private OtpErlangTuple[] createFunundefs()
	{
		final Set<Integer> keys = this.funundef.keySet();
		final OtpErlangTuple[] funundefs = new OtpErlangTuple[keys.size()];
		int index = 0;

		for (Integer key : keys)
		{
			final OtpErlangObject[] functionElements = new OtpErlangObject[5];

			functionElements[0] = new OtpErlangAtom("function");
			functionElements[1] = new OtpErlangLong(1);
			functionElements[2] = new OtpErlangAtom("funundef");
			functionElements[3] = new OtpErlangLong(key.longValue());

			final OtpErlangObject[] clauseElements = new OtpErlangObject[5];

			clauseElements[0] = new OtpErlangAtom("clause");
			clauseElements[1] = new OtpErlangLong(1);
			clauseElements[2] = null;
			clauseElements[3] = new OtpErlangList();
			clauseElements[4] = null;

			final OtpErlangObject[] params = new OtpErlangObject[key];
			for (int paramIndex = 0; paramIndex < key.longValue(); paramIndex++)
			{
				final OtpErlangObject[] elems = new OtpErlangObject[3];

				elems[0] = new OtpErlangAtom("var");
				elems[1] = new OtpErlangLong(1);
				elems[2] = new OtpErlangAtom("_");
				params[paramIndex] = new OtpErlangTuple(elems);
			}
			clauseElements[2] = new OtpErlangList(params);

			final OtpErlangObject[] instructions = new OtpErlangObject[1];
			final OtpErlangObject[] instructionElem = new OtpErlangObject[3];

			instructionElem[0] = new OtpErlangAtom("atom");
			instructionElem[1] = new OtpErlangLong(1);
			instructionElem[2] = new OtpErlangAtom("undef");
			instructions[0] = new OtpErlangTuple(instructionElem);
			clauseElements[4] = new OtpErlangList(instructions);

			final OtpErlangTuple clauses = new OtpErlangTuple(clauseElements);

			functionElements[4] = new OtpErlangList(clauses);
			funundefs[index++] = new OtpErlangTuple(functionElements);
		}

		return funundefs;
	}
	private void removeFile(List<OtpErlangObject> bodyList)
	{
		for (int bodyElementIndex = 0; bodyElementIndex < bodyList.size(); bodyElementIndex++)
		{
			final OtpErlangTuple element = (OtpErlangTuple) bodyList.get(bodyElementIndex);
			final OtpErlangAtom type = (OtpErlangAtom) element.elementAt(2);
			if (type == null || !type.toString().equals("file"))
				continue;

			bodyList.remove(bodyElementIndex);
			break;
		}
	}
	private void removeExports(List<OtpErlangObject> bodyList)
	{
		for (int bodyElementIndex = 0; bodyElementIndex < bodyList.size(); bodyElementIndex++)
		{
			final OtpErlangTuple element = (OtpErlangTuple) bodyList.get(bodyElementIndex);
			final OtpErlangAtom type = (OtpErlangAtom) element.elementAt(2);
			if (type == null || !type.toString().equals("export"))
				continue;

			final OtpErlangList exports = (OtpErlangList) element.elementAt(3);
			final List<OtpErlangObject> newExports = new LinkedList<OtpErlangObject>();

			for (OtpErlangObject export : exports)
			{
				final OtpErlangTuple export0 = (OtpErlangTuple) export;
				final String name = export0.elementAt(0).toString();
				final String arity = export0.elementAt(1).toString();

				if (this.functions.contains(name + "/" + arity))
					newExports.add(export);
			}

			bodyList.remove(bodyElementIndex);
			if (!newExports.isEmpty())
			{
				final OtpErlangObject[] elements = new OtpErlangObject[4];
				elements[0] = new OtpErlangAtom("attribute");
				elements[1] = new OtpErlangLong(1);
				elements[2] = new OtpErlangAtom("export");
				elements[3] = new OtpErlangList(this.toArray(newExports));

				final OtpErlangTuple newElement = new OtpErlangTuple(elements);
				bodyList.add(bodyElementIndex, newElement);
			}
		}
	}
	private OtpErlangObject[] toArray(List<OtpErlangObject> list)
	{
		final int listSize = list.size();
		final OtpErlangObject[] elements = new OtpErlangObject[listSize];

		for (int elementIndex = 0; elementIndex < listSize; elementIndex++)
			elements[elementIndex] = list.get(elementIndex);

		return elements;
	}
}