package eknife.edg.generator;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.EDG;
import eknife.edg.Edge;
import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.constraint.AccessConstraint;
import eknife.edg.constraint.BinConstraint;
import eknife.edg.constraint.BinElementConstraint;
import eknife.edg.constraint.ListConstraint;
import eknife.edg.constraint.TupleConstraint;
import eknife.edg.traverser.GraphTraverser;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class GraphGenerator
{
	private EDG graph;
	private int nodeId;

	public EDG generate(OtpErlangList body)
	{
		final List<OtpErlangTuple> attributes = new LinkedList<OtpErlangTuple>();
		final List<OtpErlangTuple> functions = new LinkedList<OtpErlangTuple>();
		final int listArity = body.arity();

		for (int elemIndex = 0; elemIndex < listArity; elemIndex++)
		{
			final OtpErlangTuple tuple = (OtpErlangTuple) body.elementAt(elemIndex);
			final OtpErlangAtom type = (OtpErlangAtom) tuple.elementAt(0);

			switch (type.atomValue())
			{
				case "attribute":
					attributes.add(tuple);
					break;
				case "function":
					functions.add(tuple);
					break;
				case "eof":
					break;
				default:
					throw new RuntimeException("Type not contempled: " + type.atomValue());
			}
		}

		final int attributesSize = attributes.size();
		final OtpErlangObject[] attributesObjects = new OtpErlangObject[attributesSize];
		for (int attributesIndex = 0; attributesIndex < attributesSize; attributesIndex++)
			attributesObjects[attributesIndex] = attributes.get(attributesIndex);
		final OtpErlangList attributesList = new OtpErlangList(attributesObjects);

		this.graph = new EDG();
		this.nodeId = 0;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Root, attributesList);
		final Node root = new Node("Root", info);
		this.graph.setRootNode(root);

		for (OtpErlangTuple function : functions)
			this.parseFunction(root, function);

		return this.graph;
	}

	// Functions
	private void parseFunction(Node root, OtpErlangTuple function)
	{
		// Add function
		final OtpErlangAtom functionId = (OtpErlangAtom) function.elementAt(2);
		final OtpErlangLong functionArity = (OtpErlangLong) function.elementAt(3);
		final String funcionNodeName = "function" + "\\n" + functionId + "/" + functionArity;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Function, functionId.atomValue(), functionArity.longValue());
		final Node functionNode = new Node(funcionNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(root);
		this.graph.addNode(functionNode);
		this.graph.addEdge(root, functionNode, 0, edgeInfo);

		// Parse clauses
		final OtpErlangList functionClauses = (OtpErlangList) function.elementAt(4);
		this.parseClauses(functionNode, functionClauses);
	}
	private void parseFunctionExpression(Node parent, OtpErlangTuple function)
	{
		final OtpErlangTuple functionTuple = (OtpErlangTuple) function.elementAt(2);
		final OtpErlangAtom functionId = (OtpErlangAtom) functionTuple.elementAt(0);

		switch (functionId.atomValue())
		{
			case "function":
				if (functionTuple.arity() == 3)
					this.parseReferencedLocalFunction(parent, functionTuple);
				else if (functionTuple.arity() == 4)
					this.parseReferencedRemoteFunction(parent, functionTuple);
				else
					throw new RuntimeException("Function type not contempled: " + functionId.atomValue());
				break;
			case "clauses":
				this.parseAnonymousFunction(parent, functionTuple);
				break;
			default:
				throw new RuntimeException("Function type not contempled: " + functionId.atomValue());
		}
	}
	private void parseReferencedLocalFunction(Node parent, OtpErlangTuple function)
	{
		// Add function
		final OtpErlangAtom functionId = (OtpErlangAtom) function.elementAt(1);
		final OtpErlangLong functionArity = (OtpErlangLong) function.elementAt(2);
		final String funcionNodeName = "function" + "\\n" + functionId + "/" + functionArity;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.FunctionIdentifier, functionId.atomValue(), functionArity.longValue());
		final Node functionNode = new Node(funcionNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(functionNode);
		this.graph.addEdge(parent, functionNode, 0, edgeInfo);
	}
	private void parseReferencedRemoteFunction(Node parent, OtpErlangTuple function)
	{
		// Add function
		final OtpErlangTuple moduleIdTuple = (OtpErlangTuple) function.elementAt(1);
		final OtpErlangAtom moduleId = (OtpErlangAtom) moduleIdTuple.elementAt(2);
		final OtpErlangTuple functionIdTuple = (OtpErlangTuple) function.elementAt(2);
		final OtpErlangAtom functionId = (OtpErlangAtom) functionIdTuple.elementAt(2);
		final OtpErlangTuple functionArityTuple = (OtpErlangTuple) function.elementAt(3);
		final OtpErlangLong functionArity = (OtpErlangLong) functionArityTuple.elementAt(2);
		final String funcionNodeName = "function" + "\\n" + moduleId + ":" + functionId + "/" + functionArity;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.FunctionIdentifier, moduleId.atomValue(), functionId.atomValue(), functionArity.longValue());
		final Node functionNode = new Node(funcionNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(functionNode);
		this.graph.addEdge(parent, functionNode, 0, edgeInfo);
	}
	private void parseAnonymousFunction(Node parent, OtpErlangTuple function)
	{
		// Add function
		final OtpErlangList functionClauses0 = (OtpErlangList) function.elementAt(1);
		final OtpErlangTuple firstClause = (OtpErlangTuple) functionClauses0.elementAt(0);
		final OtpErlangList firstClauseParameters = (OtpErlangList) firstClause.elementAt(2);
		final int functionArity = firstClauseParameters.arity();
		final String funcionNodeName = "function" + "\\n" + "_" + "/" + functionArity;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.AnonymousFunction, "_", functionArity);
		final Node functionNode = new Node(funcionNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(functionNode);
		this.graph.addEdge(parent, functionNode, 0, edgeInfo);

		// Parse clauses
		final OtpErlangList functionClauses = (OtpErlangList) function.elementAt(1);
		this.parseClauses(functionNode, functionClauses);
	}

	// Clauses
	private void parseClauses(Node parent, OtpErlangList clauses)
	{
		final int clausesArity = clauses.arity();

		for (int clauseIndex = 0; clauseIndex < clausesArity; clauseIndex++)
		{
			final OtpErlangTuple clause = (OtpErlangTuple) clauses.elementAt(clauseIndex);

			this.parseClause(parent, clause);
		}
	}
	private void parseClause(Node parent, OtpErlangTuple clause)
	{
		// Add clause
		final String clauseNodeName = "clause";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Clause);
		final Node clauseNode = new Node(clauseNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(clauseNode);
		this.graph.addEdge(parent, clauseNode, 0, edgeInfo);

		// Parse parameters
		final OtpErlangList clauseParameters = (OtpErlangList) clause.elementAt(2);
		this.parsePatterns(clauseNode, clauseParameters);

		// Add guards
		final OtpErlangList clauseGuards = (OtpErlangList) clause.elementAt(3);
		final String guardsText = "(guards)" + "\\n" + this.getGuardsValue(clauseGuards);
		final NodeInfo info2 = new NodeInfo(this.nodeId++, NodeInfo.Type.Guard, clauseGuards);
		final Node guardsNode = new Node(guardsText, info2);
		final EdgeInfo edgeInfo2 = this.getEdgeInfo(parent);
		this.graph.addNode(guardsNode);
		this.graph.addEdge(clauseNode, guardsNode, 0, edgeInfo2);

		// Add body
		final String bodyNodeName = "body";
		final NodeInfo info3 = new NodeInfo(this.nodeId++, NodeInfo.Type.Body);
		final Node bodyNode = new Node(bodyNodeName, info3);
		final EdgeInfo edgeInfo3 = this.getEdgeInfo(parent);
		this.graph.addNode(bodyNode);
		this.graph.addEdge(guardsNode, bodyNode, 0, edgeInfo3);

		// Parse expressions
		final OtpErlangList clauseExpressions = (OtpErlangList) clause.elementAt(4);
		this.parseExpressions(bodyNode, clauseExpressions);
	}

	// Patterns
	private void parsePatterns(Node parent, OtpErlangList patterns)
	{
		final int patternsArity = patterns.arity();

		for (int patternIndex = 0; patternIndex < patternsArity; patternIndex++)
		{
			final OtpErlangTuple pattern = (OtpErlangTuple) patterns.elementAt(patternIndex);

			this.parsePattern(parent, pattern);
		}
	}
	private void parsePattern(Node parent, OtpErlangTuple pattern)
	{
		final OtpErlangAtom patternType = (OtpErlangAtom) pattern.elementAt(0);

		switch (patternType.atomValue())
		{
			case "var":
				this.parseVar(parent, pattern);
				break;
			case "atom":
				this.parseAtom(parent, pattern);
				break;
			case "integer":
				this.parseInteger(parent, pattern);
				break;
			case "string":
				this.parseString(parent, pattern);
				break;
			case "char":
				this.parseChar(parent, pattern);
				break;
			case "tuple":
				this.parseTuplePattern(parent, pattern);
				break;
			case "match":
				this.parseCompoundPattern(parent, pattern);
				break;
			case "cons":
			case "nil":
				this.parseListPattern(parent, pattern);
				break;
			case "op":
				this.parseUnaryOperationPattern(parent, pattern);
				break;
			case "bin":
				this.parseBinPattern(parent, pattern);
				break;
			case "bin_element":
				this.parseBinElementPattern(parent, pattern);;
				break;
			default:
				throw new RuntimeException("Pattern type not contempled: " + patternType.atomValue());
		}
	}
	private void parseTuplePattern(Node parent, OtpErlangTuple tuple)
	{
		// Add tuple
		final String tupleNodeName = "{}";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.TuplePattern);
		final Node tupleNode = new Node(tupleNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(tupleNode);
		this.graph.addEdge(parent, tupleNode, 0, edgeInfo);

		// Parse elements
		final OtpErlangList tupleElements = (OtpErlangList) tuple.elementAt(2);
		this.parsePatterns(tupleNode, tupleElements);
	}
	private void parseListPattern(Node parent, OtpErlangTuple list)
	{
		// Add tuple
		final String listNodeName = "[]";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.ListPattern);
		final Node listNode = new Node(listNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(listNode);
		this.graph.addEdge(parent, listNode, 0, edgeInfo);

		final OtpErlangAtom listId = (OtpErlangAtom) list.elementAt(0);
		if (listId.atomValue().equals("nil"))
			return;

		// Parse head
		final OtpErlangTuple head = (OtpErlangTuple) list.elementAt(2);
		this.parsePattern(listNode, head);

		// Parse tail
		final OtpErlangTuple tail = (OtpErlangTuple) list.elementAt(3);
		this.parsePattern(listNode, tail);
	}
	private void parseBinPattern(Node parent, OtpErlangTuple bin)
	{
		// Add bin
		final String binNodeName = "<< >>";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.BinPattern);
		final Node binNode = new Node(binNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(binNode);
		this.graph.addEdge(parent, binNode, 0, edgeInfo);

		// Parse elements
		final OtpErlangList binElements = (OtpErlangList) bin.elementAt(2);
		this.parsePatterns(binNode, binElements);
	}
	private void parseBinElementPattern(Node parent, OtpErlangTuple binElement)
	{
		// Add bin element
		final OtpErlangTuple binElementNumValue = (OtpErlangTuple) binElement.elementAt(2);
		final OtpErlangObject binElementSizeValue = (OtpErlangObject) binElement.elementAt(3);
		final String binElementNumValue0 = this.getValue((OtpErlangTuple) binElementNumValue);
		final String binElementSizeValue0 = binElementSizeValue instanceof OtpErlangAtom ? "" : this.getValue((OtpErlangTuple) binElementSizeValue);
		final String binElementValue = binElementSizeValue0.equals("") ? binElementNumValue0 : binElementNumValue0 + ":" + binElementSizeValue0;
		final String binElementNodeName = "(bin element)" + "\\n" + binElementValue;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.BinElementPattern, binElementValue);
		final Node binElementNode = new Node(binElementNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent, binElement);
		this.graph.addNode(binElementNode);
		this.graph.addEdge(parent, binElementNode, 0, edgeInfo);

		// Parse value
		this.parsePattern(binElementNode, binElementNumValue);

		// Parse size
		if (binElementSizeValue instanceof OtpErlangTuple)
			this.parsePattern(binElementNode, (OtpErlangTuple) binElementSizeValue);
		else if (binElementSizeValue instanceof OtpErlangAtom)
			this.parseDefault(binElementNode, (OtpErlangAtom) binElementSizeValue);
		else
			throw new RuntimeException("Type not contempled");
	}
	private void parseCompoundPattern(Node parent, OtpErlangTuple compoundPattern)
	{
		// Add compound pattern
		final String compoundPatternNodeName = "cp";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.CompoundPattern);
		final Node compoundPatternNode = new Node(compoundPatternNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(compoundPatternNode);
		this.graph.addEdge(parent, compoundPatternNode, 0, edgeInfo);

		// Parse variable 1
		final OtpErlangTuple compoundPatternVariable1 = (OtpErlangTuple) compoundPattern.elementAt(2);
		this.parsePattern(compoundPatternNode, compoundPatternVariable1);

		// Parse variable 2
		final OtpErlangTuple compoundPatternVariable2 = (OtpErlangTuple) compoundPattern.elementAt(3);
		this.parsePattern(compoundPatternNode, compoundPatternVariable2);
	}
	private void parseUnaryOperationPattern(Node parent, OtpErlangTuple operation)
	{
		// Add operation
		final OtpErlangAtom operationSign = (OtpErlangAtom) operation.elementAt(2);
		final String operationSignValue = operationSign.atomValue();
		final String operationNodeName = "(op)" + "\\n" + operationSignValue;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Operation, operationSignValue);
		final Node operationNode = new Node(operationNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(operationNode);
		this.graph.addEdge(parent, operationNode, 0, edgeInfo);

		// Process pattern
		final OtpErlangTuple operationPattern = (OtpErlangTuple) operation.elementAt(3);
		this.parsePattern(operationNode, operationPattern);
	}

	// Expressions
	private void parseExpressions(Node parent, OtpErlangList expressions)
	{
		final int expressionsArity = expressions.arity();

		for (int expressionIndex = 0; expressionIndex < expressionsArity; expressionIndex++)
		{
			final OtpErlangTuple expression = (OtpErlangTuple) expressions.elementAt(expressionIndex);

			this.parseExpression(parent, expression);
		}
	}
	private void parseExpression(Node parent, OtpErlangTuple expression)
	{
		final OtpErlangAtom expressionType = (OtpErlangAtom) expression.elementAt(0);

		switch (expressionType.atomValue())
		{
			case "block":
				this.parseBlock(parent, expression);
				break;
			case "case":
				this.parseCase(parent, expression);
				break;
			case "call":
				this.parseCall(parent, expression);
				break;
			case "remote":
				this.parseRemote(parent, expression);
				break;
			case "string":
				this.parseString(parent, expression);
				break;
			case "atom":
				this.parseAtom(parent, expression);
				break;
			case "var":
				this.parseVar(parent, expression);
				break;
			case "integer":
				this.parseInteger(parent, expression);
				break;
			case "char":
				this.parseChar(parent, expression);
				break;
			case "op":
				this.parseOperationExpression(parent, expression);
				break;
			case "match":
				this.parsePatternMatching(parent, expression);
				break;
			case "fun":
				this.parseFunctionExpression(parent, expression);
				break;
			case "if":
				this.parseIf(parent, expression);
				break;
			case "tuple":
				this.parseTupleExpression(parent, expression);
				break;
			case "lc":
				this.parseListComprehension(parent, expression);
				break;
			case "generate":
				this.parseGenerationExpression(parent, expression);
				break;
			case "cons":
			case "nil":
				this.parseListExpression(parent, expression);
				break;
			case "bin":
				this.parseBinExpression(parent, expression);
				break;
			case "bin_element":
				this.parseBinElementExpression(parent, expression);;
				break;
			case "bc":
				this.parseBinComprehension(parent, expression);
				break;
			case "b_generate":
				this.parseBinGenerationExpression(parent, expression);
				break;
			default:
				throw new RuntimeException("Expression type not contempled: " + expressionType.atomValue());
		}
	}
	private void parseBlock(Node parent, OtpErlangTuple block)
	{
		// Add block
		final String blockNodeName = "block";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Block);
		final Node blockNode = new Node(blockNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(blockNode);
		this.graph.addEdge(parent, blockNode, 0, edgeInfo);

		// Parse elements
		final OtpErlangList blockElements = (OtpErlangList) block.elementAt(2);
		this.parseExpressions(blockNode, blockElements);
	}
	private void parseCase(Node parent, OtpErlangTuple _case)
	{
		// Add case
		final String caseNodeName = "case";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Case);
		final Node caseNode = new Node(caseNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(caseNode);
		this.graph.addEdge(parent, caseNode, 0, edgeInfo);

		// Parse pattern
		final OtpErlangTuple caseExpression = (OtpErlangTuple) _case.elementAt(2);
		this.parseExpression(caseNode, caseExpression);

		// Parse clauses
		final OtpErlangList caseClauses = (OtpErlangList) _case.elementAt(3);
		this.parseClauses(caseNode, caseClauses);
	}
	private void parseCall(Node parent, OtpErlangTuple call)
	{
		// Add call
		final String callNodeName = "call";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.FunctionCall);
		final Node callNode = new Node(callNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(callNode);
		this.graph.addEdge(parent, callNode, 0, edgeInfo);

		// Parse function
		final OtpErlangTuple callFunction = (OtpErlangTuple) call.elementAt(2);
		this.parseExpression(callNode, callFunction);

		// Parse arguments
		final OtpErlangList callArguments = (OtpErlangList) call.elementAt(3);
		this.parseExpressions(callNode, callArguments);

		// Add return
		final String returnNodeName = "return";
		final NodeInfo info2 = new NodeInfo(this.nodeId++, NodeInfo.Type.Return);
		final Node returnNode = new Node(returnNodeName, info2);
		final EdgeInfo edgeInfo2 = this.getEdgeInfo(callNode);
		this.graph.addNode(returnNode);
		this.graph.addEdge(callNode, returnNode, 0, edgeInfo2);
	}
	private void parseRemote(Node parent, OtpErlangTuple remote)
	{
		final OtpErlangTuple remoteClass = (OtpErlangTuple) remote.elementAt(2);
		final OtpErlangTuple remoteFunction = (OtpErlangTuple) remote.elementAt(3);
		final OtpErlangAtom remoteClassValue = (OtpErlangAtom) remoteClass.elementAt(2);
		final OtpErlangAtom remoteFunctionValue = (OtpErlangAtom) remoteFunction.elementAt(2);
		final String remoteName = remoteClassValue.atomValue() + ":" + remoteFunctionValue.atomValue();
		final String remoteNodeName = "(remote)" + "\\n" + remoteName;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Remote, remoteName);
		final Node remoteNode = new Node(remoteNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(remoteNode);
		this.graph.addEdge(parent, remoteNode, 0, edgeInfo);
	}
	private void parsePatternMatching(Node parent, OtpErlangTuple patternMatching)
	{
		// Add pattern matching
		final String patternMatchingNodeName = "pm";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.PatternMatching);
		final Node patternMatchingNode = new Node(patternMatchingNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(patternMatchingNode);
		this.graph.addEdge(parent, patternMatchingNode, 0, edgeInfo);

		// Parse variable
		final OtpErlangTuple patternMatchingVariable = (OtpErlangTuple) patternMatching.elementAt(2);
		this.parsePattern(patternMatchingNode, patternMatchingVariable);

		// Parse value
		final OtpErlangTuple patternMatchingValue = (OtpErlangTuple) patternMatching.elementAt(3);
		this.parseExpression(patternMatchingNode, patternMatchingValue);
	}
	private void parseIf(Node parent, OtpErlangTuple _if)
	{
		// Add if
		final String ifNodeName = "if";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.If);
		final Node ifNode = new Node(ifNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(ifNode);
		this.graph.addEdge(parent, ifNode, 0, edgeInfo);

		// Parse clauses
		final OtpErlangList ifClauses = (OtpErlangList) _if.elementAt(2);
		this.parseClauses(ifNode, ifClauses);
	}
	private void parseTupleExpression(Node parent, OtpErlangTuple tuple)
	{
		// Add tuple
		final String tupleNodeName = "{}";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.TupleExpression);
		final Node tupleNode = new Node(tupleNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(tupleNode);
		this.graph.addEdge(parent, tupleNode, 0, edgeInfo);

		// Parse elements
		final OtpErlangList tupleElements = (OtpErlangList) tuple.elementAt(2);
		this.parseExpressions(tupleNode, tupleElements);
	}
	private void parseListExpression(Node parent, OtpErlangTuple list)
	{
		// Add tuple
		final String listNodeName = "[]";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.ListExpression);
		final Node listNode = new Node(listNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(listNode);
		this.graph.addEdge(parent, listNode, 0, edgeInfo);

		final OtpErlangAtom listId = (OtpErlangAtom) list.elementAt(0);
		if (listId.atomValue().equals("nil"))
			return;

		// Parse head
		final OtpErlangTuple head = (OtpErlangTuple) list.elementAt(2);
		this.parseExpression(listNode, head);

		// Parse tail
		final OtpErlangTuple tail = (OtpErlangTuple) list.elementAt(3);
		this.parseExpression(listNode, tail);
	}
	private void parseBinExpression(Node parent, OtpErlangTuple bin)
	{
		// Add bin
		final String binNodeName = "<< >>";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.BinExpression);
		final Node binNode = new Node(binNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(binNode);
		this.graph.addEdge(parent, binNode, 0, edgeInfo);

		// Parse elements
		final OtpErlangList binElements = (OtpErlangList) bin.elementAt(2);
		this.parseExpressions(binNode, binElements);
	}
	private void parseBinElementExpression(Node parent, OtpErlangTuple binElement)
	{
		// Add bin element
		final OtpErlangTuple binElementNumValue = (OtpErlangTuple) binElement.elementAt(2);
		final OtpErlangObject binElementSizeValue = (OtpErlangObject) binElement.elementAt(3);
		final String binElementNumValue0 = this.getValue((OtpErlangTuple) binElementNumValue);
		final String binElementSizeValue0 = binElementSizeValue instanceof OtpErlangAtom ? "" : this.getValue((OtpErlangTuple) binElementSizeValue);
		final String binElementValue = binElementSizeValue0.equals("") ? binElementNumValue0 : binElementNumValue0 + ":" + binElementSizeValue0;
		final String binElementNodeName = "(bin element)" + "\\n" + binElementValue;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.BinElementExpression, binElementValue);
		final Node binElementNode = new Node(binElementNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent, binElement);
		this.graph.addNode(binElementNode);
		this.graph.addEdge(parent, binElementNode, 0, edgeInfo);

		// Parse value
		this.parseExpression(binElementNode, binElementNumValue);

		// Parse size
		if (binElementSizeValue instanceof OtpErlangTuple)
			this.parseExpression(binElementNode, (OtpErlangTuple) binElementSizeValue);
		else if (binElementSizeValue instanceof OtpErlangAtom)
			this.parseDefault(binElementNode, (OtpErlangAtom) binElementSizeValue);
		else
			throw new RuntimeException("Type not contempled");
	}
	private void parseListComprehension(Node parent, OtpErlangTuple listComprehension)
	{
		// Add list comprehension
		final String listComprehensionNodeName = "lc";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.ListComprehension);
		final Node listComprehensionNode = new Node(listComprehensionNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(listComprehensionNode);
		this.graph.addEdge(parent, listComprehensionNode, 0, edgeInfo);

		// Parse restrictions
		final OtpErlangList listComprehensionRestrictions = (OtpErlangList) listComprehension.elementAt(3);
		this.parseExpressions(listComprehensionNode, listComprehensionRestrictions);

		// Add list comprehension result
		final String listComprehensionResultNodeName = "[]";
		final NodeInfo info0 = new NodeInfo(this.nodeId++, NodeInfo.Type.ListComprehensionResult);
		final Node listComprehensionResultNode = new Node(listComprehensionResultNodeName, info0);
		final EdgeInfo edgeInfo0 = this.getEdgeInfo(listComprehensionNode);
		this.graph.addNode(listComprehensionResultNode);
		this.graph.addEdge(listComprehensionNode, listComprehensionResultNode, 0, edgeInfo0);

		// Parse value
		final OtpErlangTuple listComprehensionValue = (OtpErlangTuple) listComprehension.elementAt(2);
		this.parseExpression(listComprehensionNode, listComprehensionValue);
	}
	private void parseBinComprehension(Node parent, OtpErlangTuple binComprehension)
	{
		// Add bin comprehension
		final String binComprehensionNodeName = "bc";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.BinComprehension);
		final Node binComprehensionNode = new Node(binComprehensionNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(binComprehensionNode);
		this.graph.addEdge(parent, binComprehensionNode, 0, edgeInfo);

		// Parse restrictions
		final OtpErlangList binComprehensionRestrictions = (OtpErlangList) binComprehension.elementAt(3);
		this.parseExpressions(binComprehensionNode, binComprehensionRestrictions);

		// Add bin comprehension result
		final String binComprehensionResultNodeName = "<< >>";
		final NodeInfo info0 = new NodeInfo(this.nodeId++, NodeInfo.Type.BinComprehensionResult);
		final Node binComprehensionResultNode = new Node(binComprehensionResultNodeName, info0);
		final EdgeInfo edgeInfo0 = this.getEdgeInfo(binComprehensionNode);
		this.graph.addNode(binComprehensionResultNode);
		this.graph.addEdge(binComprehensionNode, binComprehensionResultNode, 0, edgeInfo0);

		// Parse value
		final OtpErlangTuple binComprehensionValue = (OtpErlangTuple) binComprehension.elementAt(2);
		this.parseExpression(binComprehensionNode, binComprehensionValue);
	}
	private void parseGenerationExpression(Node parent, OtpErlangTuple generator)
	{
		// Add generator
		final String generatorNodeName = "generator";
		final NodeInfo generatorInfo = new NodeInfo(this.nodeId++, NodeInfo.Type.Generator);
		final Node generatorNode = new Node(generatorNodeName, generatorInfo);
		final EdgeInfo generatorEdgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(generatorNode);
		this.graph.addEdge(parent, generatorNode, 0, generatorEdgeInfo);

		// Parse pattern
		final OtpErlangTuple generatePattern = (OtpErlangTuple) generator.elementAt(2);
		this.parsePattern(generatorNode, generatePattern);

		// Parse expression
		final OtpErlangTuple generateExpression = (OtpErlangTuple) generator.elementAt(3);
		this.parseExpression(generatorNode, generateExpression);
	}
	private void parseBinGenerationExpression(Node parent, OtpErlangTuple binGenerator)
	{
		// Add bin generator
		final String binGeneratorNodeName = "bin generator";
		final NodeInfo binGeneratorInfo = new NodeInfo(this.nodeId++, NodeInfo.Type.BinGenerator);
		final Node binGeneratorNode = new Node(binGeneratorNodeName, binGeneratorInfo);
		final EdgeInfo binGeneratorEdgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(binGeneratorNode);
		this.graph.addEdge(parent, binGeneratorNode, 0, binGeneratorEdgeInfo);

		// Parse pattern
		final OtpErlangTuple binGeneratePattern = (OtpErlangTuple) binGenerator.elementAt(2);
		this.parsePattern(binGeneratorNode, binGeneratePattern);

		// Parse expression
		final OtpErlangTuple binGenerateExpression = (OtpErlangTuple) binGenerator.elementAt(3);
		this.parseExpression(binGeneratorNode, binGenerateExpression);
	}
	private void parseOperationExpression(Node parent, OtpErlangTuple operation)
	{
		final OtpErlangAtom operationType = (OtpErlangAtom) operation.elementAt(2);

		switch (operation.arity())
		{
			case 4:
				this.parseUnaryOperationExpression(parent, operation);
				break;
			case 5:
				this.parseBinaryOperationExpression(parent, operation);
				break;
			default:
				throw new RuntimeException("Operation type not contempled: " + operationType.atomValue());
		}
	}
	private void parseUnaryOperationExpression(Node parent, OtpErlangTuple operation)
	{
		// Add operation
		final OtpErlangAtom operationSign = (OtpErlangAtom) operation.elementAt(2);
		final String operationSignValue = operationSign.atomValue();
		final String operationNodeName = "(op)" + "\\n" + operationSignValue;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Operation, operationSignValue);
		final Node operationNode = new Node(operationNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(operationNode);
		this.graph.addEdge(parent, operationNode, 0, edgeInfo);

		// Process expression
		final OtpErlangTuple operationExpression = (OtpErlangTuple) operation.elementAt(3);
		this.parseExpression(operationNode, operationExpression);
	}
	private void parseBinaryOperationExpression(Node parent, OtpErlangTuple operation)
	{
		// Add operation
		final OtpErlangAtom operationSign = (OtpErlangAtom) operation.elementAt(2);
		final String operationSignValue = operationSign.atomValue();
		final String operationNodeName = "(op)" + "\\n" + operationSignValue;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Operation, operationSignValue);
		final Node operationNode = new Node(operationNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(operationNode);
		this.graph.addEdge(parent, operationNode, 0, edgeInfo);

		// Process expression1
		final OtpErlangTuple operationExpression1 = (OtpErlangTuple) operation.elementAt(3);
		this.parseExpression(operationNode, operationExpression1);

		// Process expression2
		final OtpErlangTuple operationExpression2 = (OtpErlangTuple) operation.elementAt(4);
		this.parseExpression(operationNode, operationExpression2);
	}

	// Others
	private void parseVar(Node parent, OtpErlangTuple var)
	{
		final OtpErlangAtom varValue = (OtpErlangAtom) var.elementAt(2);
		final String varValue0 = varValue.atomValue();
		final String varNodeName = "(var)" + "\\n" + varValue0;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Variable, varValue0);
		final Node varNode = new Node(varNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(varNode);
		this.graph.addEdge(parent, varNode, 0, edgeInfo);
	}
	private void parseAtom(Node parent, OtpErlangTuple atom)
	{
		final OtpErlangAtom atomValue = (OtpErlangAtom) atom.elementAt(2);
		final String atomValue0 = atomValue.atomValue();
		final String atomNodeName = "(atom)" + "\\n" + atomValue0;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Atom, atomValue0);
		final Node atomNode = new Node(atomNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(atomNode);
		this.graph.addEdge(parent, atomNode, 0, edgeInfo);
	}
	private void parseDefault(Node parent, OtpErlangAtom _default)
	{
		final String atomNodeName = "default";
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Default);
		final Node atomNode = new Node(atomNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(atomNode);
		this.graph.addEdge(parent, atomNode, 0, edgeInfo);
	}
	private void parseString(Node parent, OtpErlangTuple string)
	{
		final OtpErlangString stringValue = (OtpErlangString) string.elementAt(2);
		final String stringNodeName = "(string)" + "\\n" + stringValue.stringValue();
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.String, stringValue.stringValue());
		final Node stringNode = new Node(stringNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(stringNode);
		this.graph.addEdge(parent, stringNode, 0, edgeInfo);
	}
	private void parseInteger(Node parent, OtpErlangTuple integer)
	{
		final OtpErlangLong integerValue = (OtpErlangLong) integer.elementAt(2);
		final String nodeName = integerValue.longValue() + "";
		final String integerNodeName = "(integer)" + "\\n" + nodeName;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Integer, nodeName);
		final Node integerNode = new Node(integerNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(integerNode);
		this.graph.addEdge(parent, integerNode, 0, edgeInfo);
	}
	private void parseChar(Node parent, OtpErlangTuple _char)
	{
		final OtpErlangLong charValue = (OtpErlangLong) _char.elementAt(2);
		final String nodeName = charValue.longValue() + "";
		final String charNodeName = "(char)" + "\\n" + nodeName;
		final NodeInfo info = new NodeInfo(this.nodeId++, NodeInfo.Type.Char, nodeName);
		final Node charNode = new Node(charNodeName, info);
		final EdgeInfo edgeInfo = this.getEdgeInfo(parent);
		this.graph.addNode(charNode);
		this.graph.addEdge(parent, charNode, 0, edgeInfo);
	}

	// Edge info
	private EdgeInfo getEdgeInfo(Node parent)
	{
		return this.getEdgeInfo(parent, null);
	}
	private EdgeInfo getEdgeInfo(Node parent, OtpErlangTuple child)
	{
		final NodeInfo.Type parentType = parent.getData().getType();
		final int childCount = GraphTraverser.getChildren(parent, EdgeInfo.Type.StructuralControl).size();

		switch (parentType)
		{
			case TuplePattern:
				return new EdgeInfo(EdgeInfo.Type.StructuralControl, new TupleConstraint(AccessConstraint.Operation.Add, childCount + 1));
			case ListPattern:
				final ListConstraint.Position position1 = childCount == 0 ? ListConstraint.Position.H : ListConstraint.Position.T;
				return new EdgeInfo(EdgeInfo.Type.StructuralControl, new ListConstraint(AccessConstraint.Operation.Add, position1));
			case BinPattern:
				return this.getBinEdgeInfo(parent, child);
			case BinElementPattern:
				final BinElementConstraint.Component component1 = childCount == 0 ? BinElementConstraint.Component.V : childCount == 1 ? BinElementConstraint.Component.S : BinElementConstraint.Component.T;
				return new EdgeInfo(EdgeInfo.Type.StructuralControl, new BinElementConstraint(AccessConstraint.Operation.Add, component1));
			case TupleExpression:
				return new EdgeInfo(EdgeInfo.Type.StructuralControl, new TupleConstraint(AccessConstraint.Operation.Remove, childCount + 1));
			case ListExpression:
				final ListConstraint.Position position2 = childCount == 0 ? ListConstraint.Position.H : ListConstraint.Position.T;
				return new EdgeInfo(EdgeInfo.Type.StructuralControl, new ListConstraint(AccessConstraint.Operation.Remove, position2));
			case BinExpression:
				return this.getBinEdgeInfo(parent, child);
			case BinElementExpression:
				final BinElementConstraint.Component component2 = childCount == 0 ? BinElementConstraint.Component.V : childCount == 1 ? BinElementConstraint.Component.S : BinElementConstraint.Component.T;
				return new EdgeInfo(EdgeInfo.Type.StructuralControl, new BinElementConstraint(AccessConstraint.Operation.Remove, component2));
			default:
				return new EdgeInfo(EdgeInfo.Type.NormalControl);
		}
	}
	private EdgeInfo getBinEdgeInfo(Node parent, OtpErlangTuple child)
	{
		final NodeInfo.Type parentType = parent.getData().getType();
		final AccessConstraint.Operation operation = parentType == NodeInfo.Type.BinPattern ? AccessConstraint.Operation.Add : AccessConstraint.Operation.Remove;
		final List<Edge> childEdges = GraphTraverser.getChildEdges(parent, EdgeInfo.Type.StructuralControl);
		final Edge lastChildEdge = childEdges.isEmpty() ? null : childEdges.get(childEdges.size() - 1);
		final long prevMin = lastChildEdge == null ? 0 : ((BinConstraint) lastChildEdge.getData().getConstraint()).getMin();
		final long prevMax = lastChildEdge == null ? 0 : ((BinConstraint) lastChildEdge.getData().getConstraint()).getMax();
		final long min = prevMax == Long.MAX_VALUE ? prevMin : prevMax;
		final long max = prevMax == Long.MAX_VALUE ? prevMax : min;

		if (max == Long.MAX_VALUE)
			return new EdgeInfo(EdgeInfo.Type.StructuralControl, new BinConstraint(operation, min, max));

		final OtpErlangObject value = child.elementAt(2);
		final OtpErlangObject size = child.elementAt(3);
		final long binElementSize = this.getBinElementSize(value, size);

		if (binElementSize == Long.MAX_VALUE)
			return new EdgeInfo(EdgeInfo.Type.StructuralControl, new BinConstraint(operation, min, binElementSize));
		return new EdgeInfo(EdgeInfo.Type.StructuralControl, new BinConstraint(operation, min, max + binElementSize));
	}

	// Value
	private long getBinElementSize(OtpErlangObject value, OtpErlangObject size)
	{
		final long binSize;
		final long binElements;

		if (size instanceof OtpErlangTuple) // Size specified
		{
			final OtpErlangTuple sizeTuple = (OtpErlangTuple) size;
			if (sizeTuple.elementAt(0).toString().equals("integer"))
				binSize = ((OtpErlangLong) sizeTuple.elementAt(2)).longValue();
			else
				binSize = Long.MAX_VALUE;
		}
		else // default
			binSize = 8;

		final OtpErlangTuple valueTuple = (OtpErlangTuple) value;
		if (valueTuple.elementAt(0).toString().equals("string"))
			binElements = ((OtpErlangString) valueTuple.elementAt(2)).stringValue().length();
		else
			binElements = 1;

		if (binSize == Long.MAX_VALUE)
			return Long.MAX_VALUE;
		return binSize * binElements;
	}

	// Text
	private String getGuardsValue(OtpErlangList guards)
	{
		// ";" means OR whereas "," means AND
		String guardsText = "";
		final int guardsArity = guards.arity();

		guardsText += guardsArity > 1 ? "(" : "";
		for (int guardIndex = 0; guardIndex < guardsArity; guardIndex++)
		{
			final OtpErlangList guard = (OtpErlangList) guards.elementAt(guardIndex);

			guardsText += this.getGuardsValues(guard);
			guardsText += ") or (";
		}
		if (guardsArity > 0)
			guardsText = guardsText.substring(0, guardsText.length() - ") or (".length());
		guardsText += guardsArity > 1 ? ")" : "";

		return guardsText;
	}
	private String getGuardsValues(OtpErlangList elements)
	{
		String value = "";
		final int elementArity = elements.arity();

		for (int elementIndex = 0; elementIndex < elementArity; elementIndex++)
		{
			final OtpErlangTuple element = (OtpErlangTuple) elements.elementAt(elementIndex);

			value += this.getValue(element);
			if (elementIndex < elementArity - 1)
				value += " and ";
		}

		return value;
	}
	private String getValues(OtpErlangList elements)
	{
		String value = "";
		final int elementArity = elements.arity();

		for (int elementIndex = 0; elementIndex < elementArity; elementIndex++)
		{
			final OtpErlangTuple element = (OtpErlangTuple) elements.elementAt(elementIndex);

			value += this.getValue(element);
			if (elementIndex < elementArity - 1)
				value += ", ";
		}

		return value;
	}
	private String getValue(OtpErlangTuple element)
	{
		final OtpErlangAtom elementType = (OtpErlangAtom) element.elementAt(0);

		switch (elementType.atomValue())
		{
			case "op":
				return this.getOperationValue(element);
			case "tuple":
				return this.getTupleValue(element);
			case "cons":
			case "nil":
				return this.getListValue(element);
			case "atom":
				return this.getAtomValue(element);
			case "var":
				return this.getVariableValue(element);
			case "integer":
				return this.getIntegerValue(element);
			case "string":
				return this.getStringValue(element);
			case "call":
				return this.getCallValue(element);
			default:
				throw new RuntimeException("Type not contempled: " + elementType.atomValue());
		}
	}
	private String getTupleValue(OtpErlangTuple tuple)
	{
		final OtpErlangList tupleExpressions = (OtpErlangList) tuple.elementAt(2);
		final String tupleValue = this.getTupleValue0(tupleExpressions);

		return "{ " + tupleValue + " }";
	}
	private String getTupleValue0(OtpErlangList tupleExpressions)
	{
		String value = "";
		final int expressionsArity = tupleExpressions.arity();

		for (int expressionIndex = 0; expressionIndex < expressionsArity; expressionIndex++)
		{
			final OtpErlangTuple expressionTuple = (OtpErlangTuple) tupleExpressions.elementAt(expressionIndex);

			value += this.getValue(expressionTuple);
			if (expressionIndex < expressionsArity - 1)
				value += ", ";
		}

		return value;
	}
	private String getListValue(OtpErlangTuple list)
	{
		String text = "";

		text += this.getListValue0(list);
		text = text.length() > 0 ? text.substring(0, text.length() - 2) : text;
		text = "[" + text + "]";

		return text;
	}
	private String getListValue0(OtpErlangTuple list)
	{
		final OtpErlangAtom type = (OtpErlangAtom) list.elementAt(0);
		if (type.atomValue().equals("nil"))
			return "";

		final OtpErlangTuple head = (OtpErlangTuple) list.elementAt(2);
		final OtpErlangTuple tail = (OtpErlangTuple) list.elementAt(3);
		final String headText = this.getValue(head);
		final String tailText = this.getListValue0(tail);

		return headText + ", " + tailText;
	}
	private String getOperationValue(OtpErlangTuple operation)
	{
		final OtpErlangAtom operationType = (OtpErlangAtom) operation.elementAt(2);

		switch (operation.arity())
		{
			case 4:
				return this.getUnaryOperationValue(operation);
			case 5:
				return this.getBinaryOperationValue(operation);
			default:
				throw new RuntimeException("Operation type not contempled: " + operationType.atomValue());
		}
	}
	private String getUnaryOperationValue(OtpErlangTuple operation)
	{
		final OtpErlangAtom operationSign = (OtpErlangAtom) operation.elementAt(2);
		final OtpErlangTuple operationExpression = (OtpErlangTuple) operation.elementAt(3);
		final String expressionValue = this.getValue(operationExpression);

		return operationSign.atomValue() + " " + expressionValue;
	}
	private String getBinaryOperationValue(OtpErlangTuple operation)
	{
		final OtpErlangAtom operationSign = (OtpErlangAtom) operation.elementAt(2);
		final OtpErlangTuple operationExpression1 = (OtpErlangTuple) operation.elementAt(3);
		final String expression1Value = this.getValue(operationExpression1);
		final OtpErlangTuple operationExpression2 = (OtpErlangTuple) operation.elementAt(4);
		final String expression2Value = this.getValue(operationExpression2);

		return expression1Value + " " + operationSign.atomValue() + " " + expression2Value;
	}
	private String getVariableValue(OtpErlangTuple variable)
	{
		final OtpErlangAtom value = (OtpErlangAtom) variable.elementAt(2);

		return value.atomValue();
	}
	private String getAtomValue(OtpErlangTuple atom)
	{
		final OtpErlangAtom value = (OtpErlangAtom) atom.elementAt(2);

		return value.atomValue();
	}
	private String getIntegerValue(OtpErlangTuple integer)
	{
		final OtpErlangLong value = (OtpErlangLong) integer.elementAt(2);

		return value.longValue() + "";
	}
	private String getStringValue(OtpErlangTuple string)
	{
		final OtpErlangString value = (OtpErlangString) string.elementAt(2);

		return value.stringValue();
	}
	private String getCallValue(OtpErlangTuple call)
	{
		// Get function
		final OtpErlangTuple callFunction = (OtpErlangTuple) call.elementAt(2);
		final String callFunctionValue = this.getValue(callFunction);

		// Get arguments
		final OtpErlangList callArguments = (OtpErlangList) call.elementAt(3);
		final String callArgumentsValue = this.getValues(callArguments);

		return callFunctionValue + "(" + callArgumentsValue + ")";
	}
}