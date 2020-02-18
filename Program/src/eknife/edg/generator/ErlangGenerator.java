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

// ADDED BY SERGIO BECAUSE OF THE parseAttribute FUNCTION
import eknife.edg.constraint.RecordConstraint;
import eknife.edg.Edge; 

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
	private OtpErlangTuple parseClauseOLD(Node clause) // DEPRECATED
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
	private OtpErlangTuple parseClause(Node clause) // ADDED BY SERGIO
	{
		final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
		final Node guards = parameters.remove(parameters.size() - 2);
		final Node body = parameters.remove(parameters.size() - 1);
		final List<Node> expressions = GraphTraverser.getChildren(body, EdgeInfo.Type.NormalControl);
		final OtpErlangObject[] clauseElements = new OtpErlangObject[5];

		clauseElements[0] = new OtpErlangAtom("clause");
		clauseElements[1] = new OtpErlangLong(1);
		clauseElements[2] = this.parsePatterns(parameters);
		clauseElements[3] = this.parseGuards(guards);
		clauseElements[4] = this.parseExpressions(expressions, false);

		return new OtpErlangTuple(clauseElements);
	}
	
	private OtpErlangTuple parseEmptyClause()
	{
		final OtpErlangObject[] clauseElements = new OtpErlangObject[5];

		clauseElements[0] = new OtpErlangAtom("clause");
		clauseElements[1] = new OtpErlangLong(1);
		clauseElements[2] = new OtpErlangList(this.parseEmptyVar());
		clauseElements[3] = new OtpErlangList(new OtpErlangObject[0]);
		clauseElements[4] = new OtpErlangList(this.parseEmptyLiteral());
		
		return new OtpErlangTuple(clauseElements);
	}
// ADDED BY SERGIO
	// Guards
	private OtpErlangList parseGuards(Node guards)
	{
		final List<Node> ors = GraphTraverser.getChildren(guards, EdgeInfo.Type.NormalControl);
		if (ors.size() == 0)
			return new OtpErlangList(new OtpErlangObject[0]);
		final Node or = ors.get(0);
				
		final List<Node> ands = GraphTraverser.getChildren(or, EdgeInfo.Type.NormalControl);
		final int andsNumber = ands.size();
		final OtpErlangObject[] guardElements = new OtpErlangObject[andsNumber];
		
		for (int and = 0; and < andsNumber; and++)
		{
			guardElements[and] = this.parseAnd(ands.get(and));
		}
		
		return new OtpErlangList(guardElements);
	}
	private OtpErlangList parseAnd(Node and)
	{
		final List<Node> andExpressions = GraphTraverser.getChildren(and, EdgeInfo.Type.NormalControl);
		return this.parseExpressions(andExpressions, false);
	}
	
	// Throw,Catch,Receive
	private OtpErlangTuple parseThrow(Node _throw)
	{
		final List<Node> throwArguments = GraphTraverser.getChildren(_throw, EdgeInfo.Type.NormalControl);
		
		final OtpErlangObject[] throwAtom = new OtpErlangObject[3];  
		throwAtom[0] = new OtpErlangAtom("atom");
		throwAtom[1] = new OtpErlangLong(1);
		throwAtom[2] = new OtpErlangAtom("throw");
		
		final OtpErlangObject[] throwElements = new OtpErlangObject[4];
		throwElements[0] = new OtpErlangAtom("call");
		throwElements[1] = new OtpErlangLong(1);
		throwElements[2] = new OtpErlangTuple(throwAtom);
		throwElements[3] = this.parseExpressions(throwArguments, true);

		return new OtpErlangTuple(throwElements);
	}
	private OtpErlangTuple parseCatch(Node _catch)
	{
		final List<Node> catchExpressions = GraphTraverser.getChildren(_catch, EdgeInfo.Type.NormalControl);
		final Node catch0 = catchExpressions.get(0);
		final Node expression = GraphTraverser.getChild(catch0, 0);
		
		final OtpErlangObject[] catchElements = new OtpErlangObject[3];
		catchElements[0] = new OtpErlangAtom("catch");
		catchElements[1] = new OtpErlangLong(1);
		catchElements[2] = this.parseExpression(expression);
		
		return new OtpErlangTuple(catchElements);
	}
	private OtpErlangTuple parseReceive(Node receive)
	{
		final List<Node> receiveClauses = GraphTraverser.getChildren(receive, EdgeInfo.Type.NormalControl);
		final Node last = receiveClauses.get(receiveClauses.size() - 1);
		final OtpErlangObject[] receiveElements;
		
		if (last.getData().getType() == NodeInfo.Type.AfterReceive)
		{
			receiveElements = new OtpErlangObject[5];
			receiveElements[0] = new OtpErlangAtom("receive");
			receiveElements[1] = new OtpErlangLong(1);
			receiveElements[2] = this.parseClauses(receiveClauses);
			
			final List<Node> afterChildren = GraphTraverser.getChildren(last, EdgeInfo.Type.NormalControl);
			final Node Timeout =  afterChildren.remove(0);
			final List<Node> afterExpressions = GraphTraverser.getChildren(afterChildren.get(0), EdgeInfo.Type.NormalControl);
			receiveElements[3] = this.parseExpression(Timeout);
			receiveElements[4] = this.parseExpressions(afterExpressions, false);
		}
		else
		{
			receiveElements = new OtpErlangObject[3];
			receiveElements[0] = new OtpErlangAtom("receive");
			receiveElements[1] = new OtpErlangLong(1);
			receiveElements[2] = this.parseClauses(receiveClauses);
		}
		return new OtpErlangTuple(receiveElements);
	}
	
	// Try Catch
	private OtpErlangTuple parseTry(Node _try)
	{
		final List<Node> tryClauses = GraphTraverser.getChildren(_try, EdgeInfo.Type.NormalControl);
		final Node last = tryClauses.get(tryClauses.size()-1);
		final OtpErlangObject[] tryElements = new OtpErlangObject[6];

		// Parse after Part
		if (last.getData().getType() == NodeInfo.Type.AfterTry) 
		{
			tryClauses.remove(last);
			final List<Node> body = GraphTraverser.getChildren(last, EdgeInfo.Type.NormalControl);
			final List<Node> afterExpressions = GraphTraverser.getChildren(body.get(0), EdgeInfo.Type.NormalControl);
			tryElements[5] = this.parseAfterExpressions(afterExpressions, false); 
		}
		else
		{
			tryElements[5] = new OtpErlangList(new OtpErlangObject[0]);
		}
		
		tryElements[0] = new OtpErlangAtom("try"); 
		tryElements[1] = new OtpErlangLong(1);
		
		// Parse try part
		final Node tryPart = tryClauses.remove(0);
		final List<Node> tryExpressions = GraphTraverser.getChildren(tryPart, EdgeInfo.Type.NormalControl);
		tryElements[2] = this.parseExpressions(tryExpressions, false);
		
		// Parse catch part
		final Node catchPart = tryClauses.remove(tryClauses.size() - 1);
		final List<Node> catchClauses = GraphTraverser.getChildren(catchPart, EdgeInfo.Type.NormalControl);
		tryElements[4] = this.parseCatchClauses(catchClauses);
		
		// Parse try_of clauses
		tryElements[3] = this.parseClauses(tryClauses);
		if (_try.getData().getType() == NodeInfo.Type.TryOf && tryElements[3].equals(new OtpErlangList(new OtpErlangObject[0])))
			tryElements[3] = new OtpErlangList(this.parseEmptyClause());
		return new OtpErlangTuple(tryElements);
	}
	private OtpErlangList parseAfterExpressions(List<Node> expressions, boolean transformUnused)
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
	private OtpErlangList parseCatchClauses(List<Node> clauses)
	{
		final List<OtpErlangObject> clausesList = new LinkedList<OtpErlangObject>();

		for (Node clause : clauses)
		{
			if (!this.slice.contains(clause))
				continue;

			final OtpErlangTuple clauseTuple = this.parseCatchClause(clause);
			clausesList.add(clauseTuple);
		}
		
		if(clausesList.isEmpty())
			clausesList.add(this.parseEmptyCatch());
		
		final OtpErlangObject[] clausesElements = this.toArray(clausesList);
		return new OtpErlangList(clausesElements);
	}
	private OtpErlangTuple parseEmptyCatch()
	{
		final OtpErlangObject[] emptyCatch = new OtpErlangObject[5];
		emptyCatch[0] = new OtpErlangAtom("clause");
		emptyCatch[1] = new OtpErlangLong(1);
		emptyCatch[2] = new OtpErlangList(this.parseCatchEmptyPattern());
		emptyCatch[3] = new OtpErlangList(new OtpErlangObject[0]);
		emptyCatch[4] = new OtpErlangList(this.parseEmptyLiteral());
		
		return new OtpErlangTuple(emptyCatch);
		
	}
	private OtpErlangTuple parseCatchEmptyPattern()
	{
		final OtpErlangObject[] patternElements = new OtpErlangObject[3];
				
		patternElements[0] = new OtpErlangAtom("tuple");
		patternElements[1] = new OtpErlangLong(1);
		
		final OtpErlangObject[] throwPattern = new OtpErlangObject[3];
		throwPattern[0] = this.parseThrowLiteral();
		throwPattern[1] = this.parseEmptyCatchLiteral();
		throwPattern[2] = this.parseEmptyVar();
		
		patternElements[2] = new OtpErlangList(throwPattern);
		
		return new OtpErlangTuple(patternElements);
		
	}
	private OtpErlangTuple parseCatchClause(Node clause) 
	{
		final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
		final Node guards = parameters.remove(parameters.size() - 2);
		final Node body = parameters.remove(parameters.size() - 1);
		final List<Node> expressions = GraphTraverser.getChildren(body, EdgeInfo.Type.NormalControl);
		final OtpErlangObject[] clauseElements = new OtpErlangObject[5];

		clauseElements[0] = new OtpErlangAtom("clause");
		clauseElements[1] = new OtpErlangLong(1);
		clauseElements[2] = this.parseCatchPatterns(parameters.get(0));
		clauseElements[3] = this.parseGuards(guards);
		clauseElements[4] = this.parseExpressions(expressions, false);
		
		return new OtpErlangTuple(clauseElements);
	}
	private OtpErlangList parseCatchPatterns(Node pattern)
	{
		return new OtpErlangList(this.parseCatchPattern(pattern));
	}
	private OtpErlangTuple parseCatchPattern(Node pattern)
	{
		final OtpErlangObject[] patternElements = new OtpErlangObject[3];
		
		patternElements[0] = new OtpErlangAtom("tuple");
		patternElements[1] = new OtpErlangLong(1);
		
		if(pattern.getData().getType() == NodeInfo.Type.ExceptionPattern)
		{
			final List<Node> exceptionPatternElements = GraphTraverser.getChildren(pattern, EdgeInfo.Type.NormalControl);
			patternElements[2] = this.parseExceptionPattern(exceptionPatternElements);
		}
		else
		{
			final OtpErlangObject[] throwPattern = new OtpErlangObject[3];
			throwPattern[0] = this.parseThrowLiteral();
			throwPattern[1] = this.parsePattern(pattern);
			throwPattern[2] = this.parseEmptyVar(); // Still don't know the use of this part of the pattern
			
			patternElements[2] = new OtpErlangList(throwPattern);
		}
		return new OtpErlangTuple(patternElements);
	}
	
	private OtpErlangList parseExceptionPattern(List<Node> patternElements)
	{
		final Node error = patternElements.get(0);
		final Node reason = patternElements.get(1);
		final OtpErlangObject[] exceptionExpression = new OtpErlangObject[3];
		
		exceptionExpression[0] = this.parsePattern(error);
		exceptionExpression[1] = this.parsePattern(reason);
		exceptionExpression[2] = this.parseEmptyVar();
		
		return new OtpErlangList(exceptionExpression);
	}
	private OtpErlangTuple parseThrowLiteral()
	{
		final OtpErlangObject[] throwElements = new OtpErlangObject[3];

		throwElements[0] = new OtpErlangAtom("atom");
		throwElements[1] = new OtpErlangLong(1);
		throwElements[2] = new OtpErlangAtom("throw");

		return new OtpErlangTuple(throwElements);
	}

	// Record
	private OtpErlangTuple parseRecord(Node record)
	{
		final OtpErlangObject[] recordElements = new OtpErlangObject[4];
		recordElements[0] = new OtpErlangAtom("record");
		recordElements[1] = new OtpErlangLong(1);
		recordElements[2] = new OtpErlangAtom(record.getData().getName());
		
		final List<Node> recordFields = GraphTraverser.getChildren(record, EdgeInfo.Type.StructuralControl); 
		recordElements[3] = this.parseRecordFields(recordFields);
		
		return new OtpErlangTuple(recordElements);
	}
	private OtpErlangList parseRecordFields(List<Node> fields)
	{
		final List<OtpErlangObject> fieldsList = new LinkedList<OtpErlangObject>();

		for (Node field : fields)
		{
			if (!this.slice.contains(field))
				continue;

			final OtpErlangTuple fieldTuple = this.parseRecordField(field);
			fieldsList.add(fieldTuple);
		}

		final OtpErlangObject[] fieldsElements = this.toArray(fieldsList);
		return new OtpErlangList(fieldsElements);
	}
	private OtpErlangTuple parseRecordField(Node field)
	{
		final OtpErlangObject[] fieldElements = new OtpErlangObject[4];
		fieldElements[0] = new OtpErlangAtom("record_field");
		fieldElements[1] = new OtpErlangLong(1);
		
		// Name of the attribute in the constraint
		fieldElements[2] = this.parseAttribute(field);
		fieldElements[3] = this.parseField(field);
		return new OtpErlangTuple(fieldElements);
	}
	private OtpErlangTuple parseAttribute(Node node)
	{
		final List<Edge> incomingEdges = node.getIncomingEdges();
		String attrName = "";
		for(Edge edge : incomingEdges)
		{
			if(edge.getData().getType() == EdgeInfo.Type.StructuralControl)
			{
				attrName = ((RecordConstraint) edge.getData().getConstraint()).getField();
			}
		}
		final OtpErlangObject[] attributeElements = new OtpErlangObject[3];
		attributeElements[0] = new OtpErlangAtom("atom");
		attributeElements[1] = new OtpErlangLong(1);
		attributeElements[2] = new OtpErlangAtom(attrName);
		
		return new OtpErlangTuple(attributeElements);
	}
	private OtpErlangTuple parseField(Node field)
	{
		final List<Node> expressionElements = GraphTraverser.getChildren(field, EdgeInfo.Type.Control);
		final Node expression = expressionElements.get(0);
		return this.parseExpression(expression);
	}
	
	// Record Field
	private OtpErlangTuple parseRecordAccess(Node recordField)
	{
		final List<Node> recordFieldChildren = GraphTraverser.getChildren(recordField, EdgeInfo.Type.StructuralControl);
		final Node expression = recordFieldChildren.get(0);
		final Node attribute = recordFieldChildren.get(1);

		final OtpErlangObject[] recordFieldElements = new OtpErlangObject[5];
		recordFieldElements[0] = new OtpErlangAtom("record_field");
		recordFieldElements[1] = new OtpErlangLong(1);
		recordFieldElements[2] = this.parseExpression(expression);
		recordFieldElements[3] = new OtpErlangAtom(recordField.getData().getName());
		recordFieldElements[4] = this.parseFieldName(attribute);
		
		return new OtpErlangTuple(recordFieldElements);
	}
	private OtpErlangTuple parseFieldName(Node field)
	{
		final String attrName = field.getData().getName();
		final OtpErlangObject[] attributeElements = new OtpErlangObject[3];
		attributeElements[0] = new OtpErlangAtom("atom");
		attributeElements[1] = new OtpErlangLong(1);
		attributeElements[2] = new OtpErlangAtom(attrName);
		return new OtpErlangTuple(attributeElements);
	}

	// Map
	private OtpErlangTuple parseMap(Node map)
	{
		final List<Node> mapFields = GraphTraverser.getChildren(map, EdgeInfo.Type.StructuralControl);
		final OtpErlangObject[] mapElements = new OtpErlangObject[3];
		mapElements[0] = new OtpErlangAtom("map");
		mapElements[1] = new OtpErlangLong(1);
		mapElements[2] = this.parseMapFields(mapFields);
		
		return new OtpErlangTuple(mapElements);
	}
	private OtpErlangList parseMapFields(List<Node> fields)
	{
		final List<OtpErlangObject> fieldsList = new LinkedList<OtpErlangObject>();

		for (Node field : fields)
		{
			if (!this.slice.contains(field))
				continue;

			final OtpErlangTuple fieldTuple = this.parseMapField(field);
			fieldsList.add(fieldTuple);
		}

		final OtpErlangObject[] fieldsElements = this.toArray(fieldsList);
		return new OtpErlangList(fieldsElements);
	}
	private OtpErlangTuple parseMapField(Node field)
	{
		final List<Node> fieldExpressions = GraphTraverser.getChildren(field, EdgeInfo.Type.NormalControl);
		final Node key = fieldExpressions.get(0);
		final Node value = fieldExpressions.get(1);
		
		final OtpErlangObject[] fieldElements = new OtpErlangObject[4];
		if (field.getData().getType() == NodeInfo.Type.MapFieldAssoc)
		{
			fieldElements[0] = new OtpErlangAtom("map_field_assoc");
		}
		else
		{
			fieldElements[0] = new OtpErlangAtom("map_field_exact");
		}
		fieldElements[1] = new OtpErlangLong(1);
		fieldElements[2] = this.parseExpression(key);
		fieldElements[3] = this.parseExpression(value);
		
		return new OtpErlangTuple(fieldElements);
	}
	private OtpErlangTuple parseMapUpdate(Node map)
	{
		final List<Node> mapFields = GraphTraverser.getChildren(map, EdgeInfo.Type.StructuralControl);
		final Node expression = mapFields.remove(0);
		
		final OtpErlangObject[] mapElements = new OtpErlangObject[4];
		mapElements[0] = new OtpErlangAtom("map");
		mapElements[1] = new OtpErlangLong(1);
		
		if(!this.slice.contains(expression))
			mapElements[2] = this.parseEmptyMap();
		else
			mapElements[2] = this.parseExpression(expression);
		
		mapElements[3] = this.parseMapFields(mapFields);
		
		return new OtpErlangTuple(mapElements);
	}
	private OtpErlangTuple parseMapMatching(Node map_m)
	{
		final List<Node> mapMatchingElements = GraphTraverser.getChildren(map_m, EdgeInfo.Type.NormalControl);
		final Node mapPattern = mapMatchingElements.remove(0);
		final Node mapExpression = mapMatchingElements.remove(0);
		
		final OtpErlangObject[] patternMatchingElements = new OtpErlangObject[4];
		patternMatchingElements[0] = new OtpErlangAtom("match");
		patternMatchingElements[1] = new OtpErlangLong(1);
		patternMatchingElements[2] = this.parsePattern(mapPattern);
		patternMatchingElements[3] = this.parseExpression(mapExpression);
		
		return new OtpErlangTuple(patternMatchingElements);
	}
	private OtpErlangTuple parseEmptyMap()
	{
		final OtpErlangObject[] emptyMapElements = new OtpErlangObject[3];
		emptyMapElements[0] = new OtpErlangAtom("map");
		emptyMapElements[1] = new OtpErlangLong(1);
		emptyMapElements[2] = new OtpErlangList(new OtpErlangObject[0]);
		
		return new OtpErlangTuple(emptyMapElements);
	}
//----------------
		
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
				return this.parseOperationPattern(pattern);
//ADDED BY SERGIO
			case Char:
				return this.parseChar(pattern);
			case Map:
				return this.parseMap(pattern);
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
	private OtpErlangTuple parseOperationPattern(Node operation)
	{
		final List<Node> operationExpressions = GraphTraverser.getChildren(operation, EdgeInfo.Type.NormalControl);

		switch (operationExpressions.size())
		{
			case 1:
				return this.parseUnaryOperationPattern(operation);
			case 2:
				return this.parseBinaryOperationPattern(operation);
			default:
				throw new RuntimeException("Operation size not contempled: " + operationExpressions.size());
		}

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
	private OtpErlangTuple parseBinaryOperationPattern(Node operation)
	{
		final String nodeLabel = operation.getData().getName();
		final List<Node> operationExpressions = GraphTraverser.getChildren(operation, EdgeInfo.Type.NormalControl);
		final Node operationPattern1 = operationExpressions.remove(0);
		final Node operationPattern2 = operationExpressions.remove(0);
		final OtpErlangObject[] operationElements = new OtpErlangObject[5];

		operationElements[0] = new OtpErlangAtom("op");
		operationElements[1] = new OtpErlangLong(1);
		operationElements[2] = new OtpErlangAtom(nodeLabel);
		operationElements[3] = this.parsePattern(operationPattern1);
		operationElements[4] = this.parsePattern(operationPattern2);

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
		
		if (expressionsList.isEmpty() && expressions.size() != 0) // ADDED SECOND CONDITION BY SERGIO
			expressionsList.add(this.parseEmptyLiteral());
		
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
				
// ADDED BY SERGIO
			case Throw:
				return this.parseThrow(expression);
			case Catch:
				return this.parseCatch(expression);
			case Receive:
				return this.parseReceive(expression);
			case TryCatch:
			case TryOf:
				return this.parseTry(expression);
			case Record:
				return this.parseRecord(expression);
			case Field:
				return this.parseField(expression);
			case RecordField:
				return this.parseRecordAccess(expression);
			case Map:
				return this.parseMap(expression);
			case MapUpdate:
				return this.parseMapUpdate(expression);
			case MapMatching:
				return this.parseMapMatching(expression);
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
		blockElements[2] = this.parseExpressions(blockExpressions, false);

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
		callArguments.remove(callArguments.size() - 1); // Delete exceptionReturn Node
		callArguments.remove(callArguments.size() - 1); // Delete return Node

		
		final OtpErlangObject[] callElements = new OtpErlangObject[4];

		callElements[0] = new OtpErlangAtom("call");
		callElements[1] = new OtpErlangLong(1);
		callElements[2] = this.parseExpression(callFunction);
		callElements[3] = this.parseExpressions(callArguments, true);//this.parsePossibleEmptyExpressions(callArguments, true);

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
		listComprehensionElements[3] = this.parseExpressions(listComprehensionExpressions, false);

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
	private OtpErlangTuple parseEmptyCatchLiteral()
	{
		final OtpErlangObject[] varElements = new OtpErlangObject[3];

		varElements[0] = new OtpErlangAtom("atom");
		varElements[1] = new OtpErlangLong(1);
		varElements[2] = new OtpErlangAtom("undefPattern");

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