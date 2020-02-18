package eknife.edg.generator;

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

import eknife.edg.EDG;
import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.traverser.GraphTraverser;

public class ErlangGenerator
{
	private List<Node> slice;
	private final List<String> functions = new LinkedList<String>();
	private final Map<Integer, Boolean> funundef = new Hashtable<Integer, Boolean>();

	public OtpErlangList generate(EDG EDG, List<Node> slice)
	{
		this.slice = slice;

		return this.parseProgram(EDG);
	}
	private OtpErlangList parseProgram(EDG EDG)
	{
		final List<OtpErlangObject> bodyList = new LinkedList<OtpErlangObject>();

		this.parseEDG(bodyList, EDG);
		this.adaptEDG(bodyList);

		final OtpErlangObject[] bodyElements = this.toArray(bodyList);
		return new OtpErlangList(bodyElements);
	}
	private void parseEDG(List<OtpErlangObject> bodyList, EDG EDG)
	{
		final Node root = EDG.getRootNode();

		// Others
		final OtpErlangList others = (OtpErlangList) root.getData().getAST();
		for (OtpErlangObject other : others)
			bodyList.add(other);

		// Functions
		final List<Node> functions = GraphTraverser.getChildren(root, EdgeInfo.Type.NormalControl);
		for (Node function : functions)
		{
			if (!this.slice.contains(function))
				continue;

			final OtpErlangTuple functionTuple = this.parseFunction(function);
			bodyList.add(functionTuple);
		}

		// Eof
		final OtpErlangObject[] eofElements = new OtpErlangObject[2];
		eofElements[0] = new OtpErlangAtom("eof");
		eofElements[1] = new OtpErlangLong(1);
		final OtpErlangTuple eofTuple = new OtpErlangTuple(eofElements);
		bodyList.add(eofTuple);
	}
	private void adaptEDG(List<OtpErlangObject> bodyList)
	{
		// Funundef functions
		final OtpErlangTuple[] funundefs = this.createFunundefs();
		for (OtpErlangTuple funundef : funundefs)
			bodyList.add(funundef);

		// Create exports
		this.removeExports(bodyList);
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

	// Functions
	private OtpErlangTuple parseFunction(Node function)
	{
		final String functionName = function.getData().getName();
		final long functionArity = function.getData().getArity();
		final List<Node> clauses = GraphTraverser.getChildren(function, EdgeInfo.Type.NormalControl);
		final OtpErlangObject[] functionElements = new OtpErlangObject[5];

		functionElements[0] = new OtpErlangAtom("function");
		functionElements[1] = new OtpErlangLong(1);
		functionElements[2] = new OtpErlangAtom(functionName);
		functionElements[3] = new OtpErlangLong(functionArity);
		functionElements[4] = this.parseClauses(clauses);

		this.functions.add(functionName + "/" + functionArity);

		return new OtpErlangTuple(functionElements);
	}
	private OtpErlangTuple parseFunctionExpression(Node function)
	{
		final NodeInfo.Type functionType = function.getData().getType();
		final OtpErlangObject[] functionElements = new OtpErlangObject[3];

		functionElements[0] = new OtpErlangAtom("fun");
		functionElements[1] = new OtpErlangLong(1);

		switch (functionType)
		{
			case FunctionIdentifier:
				functionElements[2] = this.parseReferencedFunction(function);
				break;
			case AnonymousFunction:
				functionElements[2] = this.parseAnonymousFunction(function);
				break;
			default:
				throw new RuntimeException("Function type not contempled: " + functionType);
		}

		return new OtpErlangTuple(functionElements);
	}
	private OtpErlangTuple parseReferencedFunction(Node function)
	{
		final String functionName = function.getData().getName();
		final long functionArity = function.getData().getArity();
		final OtpErlangObject[] functionElements = new OtpErlangObject[3];

		functionElements[0] = new OtpErlangAtom("function");
		functionElements[1] = new OtpErlangAtom(functionName);
		functionElements[2] = new OtpErlangLong(functionArity);

		return new OtpErlangTuple(functionElements);
	}
	private OtpErlangTuple parseAnonymousFunction(Node function)
	{
		final List<Node> clauses = GraphTraverser.getChildren(function, EdgeInfo.Type.NormalControl);
		final OtpErlangObject[] functionElements = new OtpErlangObject[2];

		functionElements[0] = new OtpErlangAtom("clauses");
		functionElements[1] = this.parseClauses(clauses);

		return new OtpErlangTuple(functionElements);
	}

	// Clauses
	private OtpErlangList parseClauses(List<Node> clauses)
	{
		final List<OtpErlangObject> clausesList = new LinkedList<OtpErlangObject>();

		for (Node clause : clauses)
		{
			if (!this.slice.contains(clause))
				continue;

			final OtpErlangTuple clauseTuple = this.parseClause(clause);
			clausesList.add(clauseTuple);
		}

		final OtpErlangObject[] clausesElements = this.toArray(clausesList);
		return new OtpErlangList(clausesElements);
	}
	private OtpErlangTuple parseClause(Node clause)
	{
		final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
		final Node guards = parameters.remove(parameters.size() - 1);
		final List<Node> bodies = GraphTraverser.getChildren(guards, EdgeInfo.Type.NormalControl);
		final Node body = bodies.get(0);
		final List<Node> expressions = GraphTraverser.getChildren(body, EdgeInfo.Type.NormalControl);
		final OtpErlangObject[] clauseElements = new OtpErlangObject[5];

		clauseElements[0] = new OtpErlangAtom("clause");
		clauseElements[1] = new OtpErlangLong(1);
		clauseElements[2] = this.parsePatterns(parameters);
		clauseElements[3] = guards.getData().getAST();
		clauseElements[4] = this.parseExpressions(expressions, false);

		return new OtpErlangTuple(clauseElements);
	}

	// Patterns
	private OtpErlangObject parsePatterns(List<Node> patterns)
	{
		final List<OtpErlangObject> patternsList = new LinkedList<OtpErlangObject>();

		for (Node pattern : patterns)
		{
			final OtpErlangTuple patternTuple = this.parsePattern(pattern);
			patternsList.add(patternTuple);
		}

		final OtpErlangObject[] patternsElements = this.toArray(patternsList);
		return new OtpErlangList(patternsElements);
	}
	private OtpErlangTuple parsePattern(Node pattern)
	{
		if (!this.slice.contains(pattern))
			return this.parseEmptyVar();

		final NodeInfo.Type patternType = pattern.getData().getType();

		switch (patternType)
		{
			case Variable:
				return this.parseVar(pattern);
			case Atom:
				return this.parseAtom(pattern);
			case String:
				return this.parseString(pattern);
			case Integer:
				return this.parseInteger(pattern);
			case TuplePattern:
				return this.parseTuplePattern(pattern);
			case ListPattern:
				return this.parseListPattern(pattern);
			case BinPattern:
				return this.parseBinPattern(pattern);
			case BinElementPattern:
				return this.parseBinElementPattern(pattern);
			case CompoundPattern:
				return this.parseCompoundPattern(pattern);
			case Operation:
				return this.parseUnaryOperationPattern(pattern);
			default:
				throw new RuntimeException("Pattern type not contempled: " + patternType);
		}
	}
	private OtpErlangTuple parseCompoundPattern(Node compoundPattern)
	{
		final List<Node> compoundPatternExpressions = GraphTraverser.getChildren(compoundPattern, EdgeInfo.Type.NormalControl);
		final Node compoundPatternVariable1 = compoundPatternExpressions.remove(0);
		final Node compoundPatternVariable2 = compoundPatternExpressions.remove(0);
		final OtpErlangObject[] compoundPatternElements = new OtpErlangObject[4];

		compoundPatternElements[0] = new OtpErlangAtom("match");
		compoundPatternElements[1] = new OtpErlangLong(1);
		compoundPatternElements[2] = this.parsePattern(compoundPatternVariable1);
		compoundPatternElements[3] = this.parsePattern(compoundPatternVariable2);

		return new OtpErlangTuple(compoundPatternElements);
	}
	private OtpErlangTuple parseTuplePattern(Node tuple)
	{
		final List<Node> tupleExpressions = GraphTraverser.getChildren(tuple, EdgeInfo.Type.StructuralControl);
		final OtpErlangObject[] tupleElements = new OtpErlangObject[3];

		tupleElements[0] = new OtpErlangAtom("tuple");
		tupleElements[1] = new OtpErlangLong(1);
		tupleElements[2] = this.parsePatterns(tupleExpressions);

		return new OtpErlangTuple(tupleElements);
	}
	private OtpErlangTuple parseListPattern(Node list)
	{
		final List<Node> listExpressions = GraphTraverser.getChildren(list, EdgeInfo.Type.StructuralControl);
		if (listExpressions.isEmpty())
		{
			final OtpErlangObject[] nilElements = new OtpErlangObject[2];
			nilElements[0] = new OtpErlangAtom("nil");
			nilElements[1] = new OtpErlangLong(1);
			return new OtpErlangTuple(nilElements);
		}

		final Node listHead = listExpressions.remove(0);
		final Node listTail = listExpressions.remove(0);
		final OtpErlangObject[] listElements = new OtpErlangObject[4];

		listElements[0] = new OtpErlangAtom("cons");
		listElements[1] = new OtpErlangLong(1);
		listElements[2] = this.parsePattern(listHead);
		listElements[3] = this.parsePattern(listTail);

		return new OtpErlangTuple(listElements);
	}
	private OtpErlangTuple parseBinPattern(Node bin)
	{
		final List<Node> binPatterns = GraphTraverser.getChildren(bin, EdgeInfo.Type.StructuralControl);
		final OtpErlangObject[] binElements = new OtpErlangObject[3];

		binElements[0] = new OtpErlangAtom("bin");
		binElements[1] = new OtpErlangLong(1);
		binElements[2] = this.parsePatterns(binPatterns);

		return new OtpErlangTuple(binElements);
	}
	private OtpErlangTuple parseBinElementPattern(Node binElement)
	{
		final List<Node> binElementPatterns = GraphTraverser.getChildren(binElement, EdgeInfo.Type.StructuralControl);
		final Node binElementPattern1 = binElementPatterns.remove(0);
		final Node binElementPattern2 = binElementPatterns.remove(0);
		final OtpErlangObject[] operationElements = new OtpErlangObject[5];

		operationElements[0] = new OtpErlangAtom("bin_element");
		operationElements[1] = new OtpErlangLong(1);
		operationElements[2] = this.parsePattern(binElementPattern1);
		operationElements[3] = binElementPattern2.getData().getType() == NodeInfo.Type.Default ? new OtpErlangAtom("default") : this.parsePattern(binElementPattern2);
		operationElements[4] = new OtpErlangAtom("default");

		return new OtpErlangTuple(operationElements);
	}
	private OtpErlangTuple parseUnaryOperationPattern(Node operation)
	{
		final List<Node> operationValuePattern = GraphTraverser.getChildren(operation, EdgeInfo.Type.NormalControl);
		final Node operationValue = operationValuePattern.remove(0);
		final String operationSignValue = operation.getData().getName();
		final OtpErlangObject[] operationElements = new OtpErlangObject[4];

		operationElements[0] = new OtpErlangAtom("op");
		operationElements[1] = new OtpErlangLong(1);
		operationElements[2] = new OtpErlangAtom(operationSignValue);
		operationElements[3] = this.parsePattern(operationValue);

		return new OtpErlangTuple(operationElements);
	}

	// Expressions
	private OtpErlangList parseExpressions(List<Node> expressions, boolean transformUnused)
	{
		final List<OtpErlangObject> expressionsList = new LinkedList<OtpErlangObject>();

		for (Node expression : expressions)
		{
			if (!transformUnused && !this.slice.contains(expression))
				continue;

			final OtpErlangTuple expressionTuple = this.parseExpression(expression);
			expressionsList.add(expressionTuple);
		}

		final OtpErlangObject[] expressionsElements = this.toArray(expressionsList);
		return new OtpErlangList(expressionsElements);
	}
	private OtpErlangTuple parseExpression(Node expression)
	{
		if (!this.slice.contains(expression))
			return this.parseEmptyLiteral();

		final NodeInfo.Type expressionType = expression.getData().getType();

		switch (expressionType)
		{
			case Block:
				return this.parseBlock(expression);
			case Case:
				return this.parseCase(expression);
			case FunctionCall:
				return this.parseCall(expression);
			case Remote:
				return this.parseRemote(expression);
			case Atom:
				return this.parseAtom(expression);
			case String:
				return this.parseString(expression);
			case Variable:
				return this.parseVar(expression);
			case Integer:
				return this.parseInteger(expression);
			case Char:
				return this.parseChar(expression);
			case Operation:
				return this.parseOperation(expression);
			case PatternMatching:
				return this.parsePatternMatching(expression);
			case FunctionIdentifier:
			case AnonymousFunction:
				return this.parseFunctionExpression(expression);
			case If:
				return this.parseIf(expression);
			case TupleExpression:
				return this.parseTupleExpression(expression);
			case ListExpression:
				return this.parseListExpression(expression);
			case BinExpression:
				return this.parseBinExpression(expression);
			case BinElementExpression:
				return this.parseBinElementExpression(expression);
			case ListComprehension:
				return this.parseListComprehension(expression);
			case BinComprehension:
				return this.parseBinComprehension(expression);
			case Generator:
				return this.parseGenerator(expression);
			case BinGenerator:
				return this.parseBinGenerator(expression);
			default:
				throw new RuntimeException("Instruction type not contempled: " + expressionType);
		}
	}
	private OtpErlangTuple parseBlock(Node block)
	{
		final List<Node> blockExpressions = GraphTraverser.getChildren(block, EdgeInfo.Type.NormalControl);
		final OtpErlangObject[] blockElements = new OtpErlangObject[3];

		blockElements[0] = new OtpErlangAtom("block");
		blockElements[1] = new OtpErlangLong(1);
		blockElements[2] = this.parseExpressions(blockExpressions, true);

		return new OtpErlangTuple(blockElements);
	}
	private OtpErlangTuple parseCase(Node _case)
	{
		final List<Node> caseClauses = GraphTraverser.getChildren(_case, EdgeInfo.Type.NormalControl);
		final Node caseExpression = caseClauses.remove(0);
		final OtpErlangObject[] clauseElements = new OtpErlangObject[4];

		clauseElements[0] = new OtpErlangAtom("case");
		clauseElements[1] = new OtpErlangLong(1);
		clauseElements[2] = this.parseExpression(caseExpression);
		clauseElements[3] = this.parseClauses(caseClauses);

		return new OtpErlangTuple(clauseElements);
	}
	private OtpErlangTuple parseCall(Node call)
	{
		final List<Node> callArguments = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
		final Node callFunction = callArguments.remove(0);
		callArguments.remove(callArguments.size() - 1);
		final OtpErlangObject[] callElements = new OtpErlangObject[4];

		callElements[0] = new OtpErlangAtom("call");
		callElements[1] = new OtpErlangLong(1);
		callElements[2] = this.parseExpression(callFunction);
		callElements[3] = this.parseExpressions(callArguments, true);

		// Replace undef with funundef
		final OtpErlangObject[] funName = ((OtpErlangTuple) callElements[2]).elements();
		if (funName[2].toString().equals("undef"))
		{
			funName[2] = new OtpErlangAtom("funundef");
			callElements[2] = new OtpErlangTuple(funName);
			this.funundef.put(callArguments.size(), true);
		}

		return new OtpErlangTuple(callElements);
	}
	private OtpErlangTuple parseRemote(Node remote)
	{
		final String name = remote.getData().getName();
		final int remoteDelimiterIndex = name.indexOf(":");
		final String remoteClassId = name.substring(0, remoteDelimiterIndex);
		final String remoteFunctionId = name.substring(remoteDelimiterIndex + 1);

		final OtpErlangObject[] remoteClassElements = new OtpErlangObject[3];
		remoteClassElements[0] = new OtpErlangAtom("atom");
		remoteClassElements[1] = new OtpErlangLong(1);
		remoteClassElements[2] = new OtpErlangAtom(remoteClassId);

		final OtpErlangObject[] remoteFunctionElements = new OtpErlangObject[3];
		remoteFunctionElements[0] = new OtpErlangAtom("atom");
		remoteFunctionElements[1] = new OtpErlangLong(1);
		remoteFunctionElements[2] = new OtpErlangAtom(remoteFunctionId);

		final OtpErlangObject[] remoteElements = new OtpErlangObject[4];
		remoteElements[0] = new OtpErlangAtom("remote");
		remoteElements[1] = new OtpErlangLong(1);
		remoteElements[2] = new OtpErlangTuple(remoteClassElements);
		remoteElements[3] = new OtpErlangTuple(remoteFunctionElements);

		return new OtpErlangTuple(remoteElements);
	}
	private OtpErlangTuple parseOperation(Node operation)
	{
		final List<Node> operationExpressions = GraphTraverser.getChildren(operation, EdgeInfo.Type.NormalControl);

		switch (operationExpressions.size())
		{
			case 1:
				return this.parseUnaryOperation(operation);
			case 2:
				return this.parseBinaryOperation(operation);
			default:
				throw new RuntimeException("Operation size not contempled: " + operationExpressions.size());
		}
	}
	private OtpErlangTuple parseUnaryOperation(Node operation)
	{
		final String nodeLabel = operation.getData().getName();
		final List<Node> operationExpressions = GraphTraverser.getChildren(operation, EdgeInfo.Type.NormalControl);
		final Node operationExpression1 = operationExpressions.remove(0);
		final OtpErlangObject[] operationElements = new OtpErlangObject[4];

		operationElements[0] = new OtpErlangAtom("op");
		operationElements[1] = new OtpErlangLong(1);
		operationElements[2] = new OtpErlangAtom(nodeLabel);
		operationElements[3] = this.parseExpression(operationExpression1);

		return new OtpErlangTuple(operationElements);
	}
	private OtpErlangTuple parseBinaryOperation(Node operation)
	{
		final String nodeLabel = operation.getData().getName();
		final List<Node> operationExpressions = GraphTraverser.getChildren(operation, EdgeInfo.Type.NormalControl);
		final Node operationExpression1 = operationExpressions.remove(0);
		final Node operationExpression2 = operationExpressions.remove(0);
		final OtpErlangObject[] operationElements = new OtpErlangObject[5];

		operationElements[0] = new OtpErlangAtom("op");
		operationElements[1] = new OtpErlangLong(1);
		operationElements[2] = new OtpErlangAtom(nodeLabel);
		operationElements[3] = this.parseExpression(operationExpression1);
		operationElements[4] = this.parseExpression(operationExpression2);

		return new OtpErlangTuple(operationElements);
	}
	private OtpErlangTuple parseTupleExpression(Node tuple)
	{
		final List<Node> tupleExpressions = GraphTraverser.getChildren(tuple, EdgeInfo.Type.StructuralControl);
		final OtpErlangObject[] tupleElements = new OtpErlangObject[3];

		tupleElements[0] = new OtpErlangAtom("tuple");
		tupleElements[1] = new OtpErlangLong(1);
		tupleElements[2] = this.parseExpressions(tupleExpressions, true);

		return new OtpErlangTuple(tupleElements);
	}
	private OtpErlangTuple parseListExpression(Node list)
	{
		final List<Node> listExpressions = GraphTraverser.getChildren(list, EdgeInfo.Type.StructuralControl);
		if (listExpressions.isEmpty())
		{
			final OtpErlangObject[] nilElements = new OtpErlangObject[2];
			nilElements[0] = new OtpErlangAtom("nil");
			nilElements[1] = new OtpErlangLong(1);
			return new OtpErlangTuple(nilElements);
		}

		final Node listHead = listExpressions.remove(0);
		final Node listTail = listExpressions.remove(0);
		final OtpErlangObject[] listElements = new OtpErlangObject[4];

		listElements[0] = new OtpErlangAtom("cons");
		listElements[1] = new OtpErlangLong(1);
		listElements[2] = this.parseExpression(listHead);
		listElements[3] = this.parseExpression(listTail);

		return new OtpErlangTuple(listElements);
	}
	private OtpErlangTuple parseBinExpression(Node bin)
	{
		final List<Node> binExpressions = GraphTraverser.getChildren(bin, EdgeInfo.Type.StructuralControl);
		final OtpErlangObject[] binElements = new OtpErlangObject[3];

		binElements[0] = new OtpErlangAtom("bin");
		binElements[1] = new OtpErlangLong(1);
		binElements[2] = this.parseExpressions(binExpressions, true);

		return new OtpErlangTuple(binElements);
	}
	private OtpErlangTuple parseBinElementExpression(Node binElement)
	{
		final List<Node> binElementExpressions = GraphTraverser.getChildren(binElement, EdgeInfo.Type.StructuralControl);
		final Node binElementExpression1 = binElementExpressions.remove(0);
		final Node binElementExpression2 = binElementExpressions.remove(0);
		final OtpErlangObject[] binElementElements = new OtpErlangObject[5];

		binElementElements[0] = new OtpErlangAtom("bin_element");
		binElementElements[1] = new OtpErlangLong(1);
		binElementElements[2] = this.parseExpression(binElementExpression1);
		binElementElements[3] = binElementExpression2.getData().getType() == NodeInfo.Type.Default ? new OtpErlangAtom("default") : this.parseExpression(binElementExpression2);
		binElementElements[4] = new OtpErlangAtom("default");

		// The default value for non-slice nodes of a bin element is 0 instead of undef
		final OtpErlangObject valueElement = ((OtpErlangTuple) binElementElements[2]).elementAt(2);
		if (valueElement instanceof OtpErlangAtom && valueElement.toString().equals("undef"))
		{
			binElementElements[2] = this.parseEmptyNumber(0);

			// When the string can be removed, the size must be adjusted
			if (binElementExpression1.getData().getType() == NodeInfo.Type.String)
			{
				final int binElements = binElementExpression1.getData().getName().length();
				final OtpErlangObject[] operationElements = new OtpErlangObject[5];

				operationElements[0] = new OtpErlangAtom("op");
				operationElements[1] = new OtpErlangLong(1);
				operationElements[2] = new OtpErlangAtom("*");
				operationElements[3] = this.parseEmptyNumber(binElements);
				operationElements[4] = binElementExpression2.getData().getType() == NodeInfo.Type.Default ? this.parseEmptyNumber(8) : binElementElements[3];
				binElementElements[3] = new OtpErlangTuple(operationElements);
			}
		}

		return new OtpErlangTuple(binElementElements);
	}
	private OtpErlangTuple parseListComprehension(Node listComprehension)
	{
		final List<Node> listComprehensionExpressions = GraphTraverser.getChildren(listComprehension, EdgeInfo.Type.NormalControl);
		final Node listComprehensionValue = listComprehensionExpressions.remove(listComprehensionExpressions.size() - 1);
		final OtpErlangObject[] listComprehensionElements = new OtpErlangObject[4];

		listComprehensionExpressions.remove(listComprehensionExpressions.size() - 1);
		listComprehensionElements[0] = new OtpErlangAtom("lc");
		listComprehensionElements[1] = new OtpErlangLong(1);
		listComprehensionElements[2] = this.parseExpression(listComprehensionValue);
		listComprehensionElements[3] = this.parseExpressions(listComprehensionExpressions, true);

		return new OtpErlangTuple(listComprehensionElements);
	}
	private OtpErlangTuple parseBinComprehension(Node binComprehension)
	{
		final List<Node> binComprehensionExpressions = GraphTraverser.getChildren(binComprehension, EdgeInfo.Type.NormalControl);
		final Node binComprehensionValue = binComprehensionExpressions.remove(binComprehensionExpressions.size() - 1);
		final OtpErlangObject[] binComprehensionElements = new OtpErlangObject[4];

		binComprehensionExpressions.remove(binComprehensionExpressions.size() - 1);
		binComprehensionElements[0] = new OtpErlangAtom("lc");
		binComprehensionElements[1] = new OtpErlangLong(1);
		binComprehensionElements[2] = this.parseExpression(binComprehensionValue);
		binComprehensionElements[3] = this.parseExpressions(binComprehensionExpressions, true);

		return new OtpErlangTuple(binComprehensionElements);
	}
	private OtpErlangTuple parseGenerator(Node generator)
	{
		final List<Node> generatorChildren = GraphTraverser.getChildren(generator, EdgeInfo.Type.NormalControl);
		final Node generatorPattern = generatorChildren.remove(0);
		final Node generatorExpression = generatorChildren.remove(0);
		final OtpErlangObject[] generatorElements = new OtpErlangObject[4];

		generatorElements[0] = new OtpErlangAtom("generate");
		generatorElements[1] = new OtpErlangLong(1);
		generatorElements[2] = this.parsePattern(generatorPattern);
		generatorElements[3] = this.parseExpression(generatorExpression);

		return new OtpErlangTuple(generatorElements);
	}
	private OtpErlangTuple parseBinGenerator(Node binGenerator)
	{
		final List<Node> binGeneratorChildren = GraphTraverser.getChildren(binGenerator, EdgeInfo.Type.NormalControl);
		final Node binGeneratorPattern = binGeneratorChildren.remove(0);
		final Node binGeneratorExpression = binGeneratorChildren.remove(0);
		final OtpErlangObject[] binGeneratorElements = new OtpErlangObject[4];

		binGeneratorElements[0] = new OtpErlangAtom("b_generate");
		binGeneratorElements[1] = new OtpErlangLong(1);
		binGeneratorElements[2] = this.parsePattern(binGeneratorPattern);
		binGeneratorElements[3] = this.parseExpression(binGeneratorExpression);

		return new OtpErlangTuple(binGeneratorElements);
	}
	private OtpErlangTuple parseIf(Node _if)
	{
		final List<Node> clauses = GraphTraverser.getChildren(_if, EdgeInfo.Type.NormalControl);
		final OtpErlangObject[] ifElements = new OtpErlangObject[3];

		ifElements[0] = new OtpErlangAtom("if");
		ifElements[1] = new OtpErlangLong(1);
		ifElements[2] = this.parseClauses(clauses);

		return new OtpErlangTuple(ifElements);
	}
	private OtpErlangTuple parsePatternMatching(Node patternMatching)
	{
		final List<Node> patternMatchingExpressions = GraphTraverser.getChildren(patternMatching, EdgeInfo.Type.NormalControl);
		final Node patternMatchingVariable = patternMatchingExpressions.remove(0);
		final Node patternMatchingValue = patternMatchingExpressions.remove(0);
		final OtpErlangObject[] patternMatchingElements = new OtpErlangObject[4];

		patternMatchingElements[0] = new OtpErlangAtom("match");
		patternMatchingElements[1] = new OtpErlangLong(1);
		patternMatchingElements[2] = this.parsePattern(patternMatchingVariable);
		patternMatchingElements[3] = this.parseExpression(patternMatchingValue);

		return new OtpErlangTuple(patternMatchingElements);
	}

	// Others
	private OtpErlangTuple parseVar(Node var)
	{
		final String varValue = var.getData().getName();
		final OtpErlangObject[] varElements = new OtpErlangObject[3];

		varElements[0] = new OtpErlangAtom("var");
		varElements[1] = new OtpErlangLong(1);
		varElements[2] = new OtpErlangAtom(varValue);

		return new OtpErlangTuple(varElements);
	}
	private OtpErlangTuple parseAtom(Node atom)
	{
		final String atomValue = atom.getData().getName();
		final OtpErlangObject[] atomElements = new OtpErlangObject[3];

		atomElements[0] = new OtpErlangAtom("atom");
		atomElements[1] = new OtpErlangLong(1);
		atomElements[2] = new OtpErlangAtom(atomValue);

		return new OtpErlangTuple(atomElements);
	}
	private OtpErlangTuple parseString(Node string)
	{
		final String stringValue = string.getData().getName();
		final OtpErlangObject[] stringElements = new OtpErlangObject[3];

		stringElements[0] = new OtpErlangAtom("string");
		stringElements[1] = new OtpErlangLong(1);
		stringElements[2] = new OtpErlangString(stringValue);

		return new OtpErlangTuple(stringElements);
	}
	private OtpErlangTuple parseInteger(Node integer)
	{
		final String nodeName = integer.getData().getName();
		final long value = Long.parseLong(nodeName);
		final OtpErlangObject[] integerElements = new OtpErlangObject[3];

		integerElements[0] = new OtpErlangAtom("integer");
		integerElements[1] = new OtpErlangLong(1);
		integerElements[2] = new OtpErlangLong(value);

		return new OtpErlangTuple(integerElements);
	}
	private OtpErlangTuple parseChar(Node _char)
	{
		final String nodeName = _char.getData().getName();
		final long value = Long.parseLong(nodeName);
		final OtpErlangObject[] charElements = new OtpErlangObject[3];

		charElements[0] = new OtpErlangAtom("char");
		charElements[1] = new OtpErlangLong(1);
		charElements[2] = new OtpErlangLong(value);

		return new OtpErlangTuple(charElements);
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
	private OtpErlangTuple parseEmptyNumber(long number)
	{
		final OtpErlangObject[] numberElements = new OtpErlangObject[3];

		numberElements[0] = new OtpErlangAtom("integer");
		numberElements[1] = new OtpErlangLong(1);
		numberElements[2] = new OtpErlangLong(number);

		return new OtpErlangTuple(numberElements);
	}
}