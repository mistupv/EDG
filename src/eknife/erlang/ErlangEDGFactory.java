package eknife.erlang;

import java.io.File;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import edg.ASTBuilder.Where;
import edg.EDGFactory;
import edg.LDASTNodeInfo;
import edg.graph.EDG;
import edg.graph.NodeInfo;

public class ErlangEDGFactory extends EDGFactory
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static EDG createEDG(String sourcePath)
	{
		return ErlangEDGFactory.createEDG(sourcePath, true);
	}
	public static EDG createEDG(String sourcePath, boolean generateArcs)
	{
		final Launcher launcher = Launcher.getLauncher();
		final OtpErlangObject response = launcher.launch("ast", "getASTs", sourcePath);
		final OtpErlangTuple tuple = (OtpErlangTuple) response;
		final OtpErlangList asts = (OtpErlangList) tuple.elementAt(1);
		final ErlangEDGFactory edgFactory = new ErlangEDGFactory(sourcePath);

		return edgFactory.createEDG(generateArcs, asts);
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private final String codebase;

	private ErlangEDGFactory(String codebase)
	{
		this.codebase = codebase;
	}

	private EDG createEDG(boolean generateArcs, OtpErlangList asts)
	{
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "EDG", this.codebase);
		return super.createEDG(generateArcs, asts, ldNodeInfo);
	}
	protected void processElement(Object element, Map<String, Object> info)
	{
		if (this.needsReturn(info))
			this.processReturn(element, info);
		else if (element instanceof OtpErlangList)
			this.process((OtpErlangList) element);
		else if (element instanceof OtpErlangTuple)
			this.process((OtpErlangTuple) element, info);
	}

	// Return
	private boolean needsReturn(Map<String, Object> info)
	{
		final EDGFactory.Branch parent = (EDGFactory.Branch) info.get("parent");
		if (parent == null)
			return false;

		final NodeInfo.Type parentType = parent.getNodeType();
		final Where where = parent.getWhere();
		final int index = parent.getIndex();
		final int length = parent.getLength();

		if (index != length)
			return false;
		switch (parentType)
		{
			case Block:
				return true;
			case Case:
			case Clause:
				return where == Where.Body;
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
			if (type != NodeInfo.Type.Switch && type != NodeInfo.Type.Clause && type != NodeInfo.Type.Block)
				continue;

			final int id = ancestor.getNodeId();
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "return");
			super.addReturn(expression, id, ldNodeInfo);
			break;
		}
	}

	// List elements
	private void process(OtpErlangList elements)
	{
		if (elements.arity() == 0)
			return;

		final OtpErlangObject head = elements.getHead();

		if (head instanceof OtpErlangTuple)
		{
			final OtpErlangTuple head0 = (OtpErlangTuple) head;
			final String type = ((OtpErlangAtom) head0.elementAt(0)).atomValue();
			if (type.equals("attribute") || type.equals("function"))
				this.processModule(elements);
			else
				this.processGuards(elements);
		}
		else if (head instanceof OtpErlangList)
			this.processGuards(elements);
		else
			throw new RuntimeException("Element not contemplated");
	}
	private void processModule(OtpErlangList module)
	{
		final List<OtpErlangTuple> attributes = new LinkedList<OtpErlangTuple>();
		final List<OtpErlangTuple> functions = new LinkedList<OtpErlangTuple>();
		final int listArity = module.arity();
		String fileName = null;
		String moduleName = null;

		for (int elemIndex = 0; elemIndex < listArity; elemIndex++)
		{
			final OtpErlangTuple tuple = (OtpErlangTuple) module.elementAt(elemIndex);
			final OtpErlangAtom type = (OtpErlangAtom) tuple.elementAt(0);

			switch (type.atomValue())
			{
				case "attribute":
					final String attributeType = ((OtpErlangAtom) tuple.elementAt(2)).atomValue();
					if (attributeType.equals("file"))
						fileName = ((OtpErlangString) ((OtpErlangTuple) tuple.elementAt(3)).elementAt(0)).stringValue();
					if (attributeType.equals("module"))
						moduleName = ((OtpErlangAtom) tuple.elementAt(3)).atomValue();
					attributes.add(tuple);
					break;
				case "function":
					functions.add(tuple);
					break;
				case "eof":
					break;
				default:
					throw new RuntimeException("Type not contemplated: " + type.atomValue());
			}
		}

		if (fileName == null || this.codebase.equals(fileName))
			fileName = new File(this.codebase).getName();
		else
			fileName = fileName.substring(this.codebase.length());

		final int attributesSize = attributes.size();
		final OtpErlangObject[] attributesObjects = new OtpErlangObject[attributesSize];
		for (int attributesIndex = 0; attributesIndex < attributesSize; attributesIndex++)
			attributesObjects[attributesIndex] = attributes.get(attributesIndex);
		final OtpErlangList attributesList = new OtpErlangList(attributesObjects);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(fileName, 1, "module", attributesList);

		super.addModule(moduleName, functions, ldNodeInfo);
	}
	private void processGuards(OtpErlangList guards)
	{
		final OtpErlangObject head = guards.getHead();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(0, "guard");

		if (head instanceof OtpErlangList)
			super.addOperation("or", guards, ldNodeInfo);
		else
			super.addOperation("and", guards, ldNodeInfo);
	}

	// Tuple elements
	private void process(OtpErlangTuple element, Map<String, Object> info)
	{
		final String elementType = ((OtpErlangAtom) element.elementAt(0)).atomValue();
		final EDGFactory.Branch parent = (EDGFactory.Branch) info.get("parent");

		if (parent != null && parent.getNodeType() == NodeInfo.Type.ListComprehension && parent.getWhere() == Where.Restrictions && !elementType.equals("generate"))
			this.processFilter(element);
		else
			switch (elementType)
			{
				case "function":
					this.processFunction(element);
					break;
				case "fun":
					this.processAnonymousFunction(element, info);
					break;
				case "named_fun":
					this.processNamedFunction(element);
					break;
				case "var":
					this.processVar(element, info);
					break;
				case "atom":
					this.processAtom(element);
					break;
				case "integer":
					this.processInteger(element);
					break;
				case "char":
					this.processChar(element);
					break;
				case "string":
					this.processString(element);
					break;
				case "match":
					this.processPatternMatching(element);
					break;
				case "op":
					this.processOperation(element, info);
					break;
				case "tuple":
					this.processTuple(element);
					break;
				case "cons":
				case "nil":
					this.processList(element);
					break;
				case "block":
					this.processBlock(element);
					break;
				case "if":
					this.processIf(element);
					break;
				case "case":
					this.processCase(element);
					break;
				case "clause":
					if (parent.getNodeType() == NodeInfo.Type.Routine)
						this.processClause(element);
					else
						this.processClause(element, info);
					break;
				case "call":
					this.processCall(element);
					break;
				case "lc":
					this.processListComprehension(element);
					break;
				case "generate":
					this.processGenerate(element);
					break;
				default:
					throw new RuntimeException("Element type not contemplated: " + elementType);
			}
	}
	private void processFunction(OtpErlangTuple function)
	{
		final long line = ((OtpErlangLong) function.elementAt(1)).longValue();
		final String name = ((OtpErlangAtom) function.elementAt(2)).atomValue();
		final int arity = (int) ((OtpErlangLong) function.elementAt(3)).longValue();
		final OtpErlangList functionClauses = (OtpErlangList) function.elementAt(4);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "function", arity);

		super.addRoutine(name, functionClauses, ldNodeInfo);
	}
	private void processAnonymousFunction(OtpErlangTuple anonymousFunction, Map<String, Object> info)
	{
		final OtpErlangTuple anonymousFun = (OtpErlangTuple) anonymousFunction.elementAt(2);
		final String type = ((OtpErlangAtom) anonymousFun.elementAt(0)).atomValue();

		switch (type)
		{
			case "function":
				this.processSugarAnonymousFunction(anonymousFunction, info);
				break;
			case "clauses":
				this.processNormalAnonymousFunction(anonymousFunction, false);
				break;
			default:
				throw new RuntimeException("Anonymous function type not contemplated: " + type);
		}
	}
	private void processSugarAnonymousFunction(OtpErlangTuple anonymousFunction, Map<String, Object> info)
	{
		final long line = ((OtpErlangLong) anonymousFunction.elementAt(1)).longValue();
		final OtpErlangTuple anonymousFun = (OtpErlangTuple) anonymousFunction.elementAt(2);
		final OtpErlangTuple module;
		final OtpErlangTuple name;
		final int arity;

		switch (anonymousFun.arity())
		{
			case 3:
				module = null;
				name = new OtpErlangTuple(new OtpErlangObject[] { new OtpErlangAtom("atom"), new OtpErlangLong(line), anonymousFun.elementAt(1) });
				arity = (int) ((OtpErlangLong) anonymousFun.elementAt(2)).longValue();
				break;
			case 4:
				module = (OtpErlangTuple) anonymousFun.elementAt(1);
				name = (OtpErlangTuple) anonymousFun.elementAt(2);
				arity = (int) ((OtpErlangLong) ((OtpErlangTuple) anonymousFun.elementAt(3)).elementAt(2)).longValue();
				break;
			default:
				throw new RuntimeException("Sugared anonymous function not contemplated");
		}

		final OtpErlangObject[] normalAnonymousFunctionElements = new OtpErlangObject[3];
		final OtpErlangObject[] clausesElements = new OtpErlangObject[2];
		final OtpErlangObject[] clauseElements = new OtpErlangObject[5];
		final OtpErlangObject[] parameters = new OtpErlangObject[arity];
		final OtpErlangObject[] body = new OtpErlangObject[1];
		final OtpErlangObject[] callElements = new OtpErlangObject[4];
		final OtpErlangObject[] arguments = new OtpErlangObject[arity];
		final OtpErlangObject callee;

		if (module != null)
		{
			final OtpErlangObject[] remoteElements = new OtpErlangObject[4];
			remoteElements[0] = new OtpErlangAtom("remote");
			remoteElements[1] = new OtpErlangLong(line);
			remoteElements[2] = module;
			remoteElements[3] = name;
			callee = new OtpErlangTuple(remoteElements);
		}
		else
			callee = name;
		for (int parameterIndex = 0; parameterIndex < arity; parameterIndex++)
		{
			final OtpErlangObject[] varElements = new OtpErlangObject[3];
			varElements[0] = new OtpErlangAtom("var");
			varElements[1] = new OtpErlangLong(line);
			varElements[2] = new OtpErlangAtom("FreshVar" + (parameterIndex + 1));
			parameters[parameterIndex] = new OtpErlangTuple(varElements);
			arguments[parameterIndex] = new OtpErlangTuple(varElements);
		}

		normalAnonymousFunctionElements[0] = new OtpErlangAtom("fun");
		normalAnonymousFunctionElements[1] = new OtpErlangLong(line);
		clausesElements[0] = new OtpErlangAtom("clauses");
		clauseElements[0] = new OtpErlangAtom("clause");
		clauseElements[1] = new OtpErlangLong(line);
		clauseElements[2] = new OtpErlangList(parameters);
		clauseElements[3] = new OtpErlangList();
		callElements[0] = new OtpErlangAtom("call");
		callElements[1] = new OtpErlangLong(line);
		callElements[2] = callee;
		callElements[3] = new OtpErlangList(arguments);
		body[0] = new OtpErlangTuple(callElements);
		clauseElements[4] = new OtpErlangList(body);
		clausesElements[1] = new OtpErlangList(new OtpErlangTuple(clauseElements));
		normalAnonymousFunctionElements[2] = new OtpErlangTuple(clausesElements);
		this.processNormalAnonymousFunction(new OtpErlangTuple(normalAnonymousFunctionElements), true);
	}
	private void processNormalAnonymousFunction(OtpErlangTuple anonymousFunction, boolean sugar)
	{
		final long line = ((OtpErlangLong) anonymousFunction.elementAt(1)).longValue();
		final OtpErlangTuple anonymousFun = (OtpErlangTuple) anonymousFunction.elementAt(2);
		final OtpErlangList functionClauses = (OtpErlangList) anonymousFun.elementAt(1);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "normal anonymous", sugar);

		super.addRoutine(null, functionClauses, ldNodeInfo);
	}
	private void processNamedFunction(OtpErlangTuple namedFunction)
	{
		final long line = ((OtpErlangLong) namedFunction.elementAt(1)).longValue();
		final String name = ((OtpErlangAtom) namedFunction.elementAt(2)).atomValue();
		final OtpErlangList functionClauses = (OtpErlangList) namedFunction.elementAt(3);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "named function");

		super.addRoutine(name, functionClauses, ldNodeInfo);
	}
	private void processClause(OtpErlangTuple clause)
	{
		final long line = ((OtpErlangLong) clause.elementAt(1)).longValue();
		final OtpErlangList parameters = (OtpErlangList) clause.elementAt(2);
		final OtpErlangList guard = (OtpErlangList) clause.elementAt(3);
		final OtpErlangList guards = guard.arity() == 0 ? null : guard;
		final OtpErlangList expressions = (OtpErlangList) clause.elementAt(4);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "clause");

		this.createContext();
		super.addClause(parameters, guards, expressions, ldNodeInfo);
		this.destroyContext();
	}
		// Variables and literals
	private void processVar(OtpErlangTuple var, Map<String, Object> info)
	{
		final long line = ((OtpErlangLong) var.elementAt(1)).longValue();
		final String name = ((OtpErlangAtom) var.elementAt(2)).atomValue();
		final boolean assignation = this.isAssignation(name, info);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "var");

		this.addVariableToContext(name);
		super.addVariable(name, assignation, assignation, false, ldNodeInfo);
	}
	private void processAtom(OtpErlangTuple atom)
	{
		final long line = ((OtpErlangLong) atom.elementAt(1)).longValue();
		final String value = ((OtpErlangAtom) atom.elementAt(2)).atomValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "atom");

		super.addLiteral(value, ldNodeInfo);
	}
	private void processInteger(OtpErlangTuple integer)
	{
		final long line = ((OtpErlangLong) integer.elementAt(1)).longValue();
		final long value = ((OtpErlangLong) integer.elementAt(2)).longValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "integer");

		super.addLiteral(value + "", ldNodeInfo);
	}
	private void processChar(OtpErlangTuple _char)
	{
		final long line = ((OtpErlangLong) _char.elementAt(1)).longValue();
		final long value = ((OtpErlangLong) _char.elementAt(2)).longValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "char");

		super.addLiteral(value + "", ldNodeInfo);
	}
	private void processString(OtpErlangTuple string)
	{
		final long line = ((OtpErlangLong) string.elementAt(1)).longValue();
		final String value = ((OtpErlangString) string.elementAt(2)).stringValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "string");

		super.addLiteral(value + "", ldNodeInfo);
	}
		// Expressions
	private void processPatternMatching(OtpErlangTuple patternMatching)
	{
		final long line = ((OtpErlangLong) patternMatching.elementAt(1)).longValue();
		final OtpErlangTuple left = (OtpErlangTuple) patternMatching.elementAt(2);
		final OtpErlangTuple right = (OtpErlangTuple) patternMatching.elementAt(3);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "pattern matching");

		super.addEquality(left, right, ldNodeInfo);
	}
	private void processOperation(OtpErlangTuple operation, Map<String, Object> info)
	{
		final boolean isPatternZone = (boolean) info.get("patternZone");

		if (isPatternZone)
		{
			final long line = ((OtpErlangLong) operation.elementAt(1)).longValue();
			final String sign = ((OtpErlangAtom) operation.elementAt(2)).atomValue();
			final OtpErlangTuple operator = (OtpErlangTuple) operation.elementAt(3);
			final long value = ((OtpErlangLong) operator.elementAt(2)).longValue();
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "integer");
			super.addLiteral(sign + value, ldNodeInfo);
		}
		else
		{
			final long line = ((OtpErlangLong) operation.elementAt(1)).longValue();
			final String sign = ((OtpErlangAtom) operation.elementAt(2)).atomValue();
			final List<OtpErlangTuple> operands = new LinkedList<OtpErlangTuple>();
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "operation");

			for (int elementIndex = 3; elementIndex < operation.arity(); elementIndex++)
			{
				final OtpErlangTuple operationExpression = (OtpErlangTuple) operation.elementAt(elementIndex);
				operands.add(operationExpression);
			}
			super.addOperation(sign, operands, ldNodeInfo);
		}
	}
	private void processBlock(OtpErlangTuple block)
	{
		final long line = ((OtpErlangLong) block.elementAt(1)).longValue();
		final OtpErlangList expressions = (OtpErlangList) block.elementAt(2);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "block");

		super.addBlock(expressions, ldNodeInfo);
	}
	private void processIf(OtpErlangTuple _if)
	{
		final long line = ((OtpErlangLong) _if.elementAt(1)).longValue();
		final OtpErlangList cases = (OtpErlangList) _if.elementAt(2);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "if");

		super.addSwitch(null, cases, ldNodeInfo);
	}
	private void processCase(OtpErlangTuple _case)
	{
		final long line = ((OtpErlangLong) _case.elementAt(1)).longValue();
		final OtpErlangTuple selector = (OtpErlangTuple) _case.elementAt(2);
		final OtpErlangList cases = (OtpErlangList) _case.elementAt(3);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "case");

		super.addSwitch(selector, cases, ldNodeInfo);
	}
	private void processClause(OtpErlangTuple clause, Map<String, Object> info)
	{
		final OtpErlangTuple clause0 = (OtpErlangTuple) clause;
		final long line = ((OtpErlangLong) clause0.elementAt(1)).longValue();
		final OtpErlangList selectables = (OtpErlangList) clause0.elementAt(2);
		final OtpErlangList guards = (OtpErlangList) clause0.elementAt(3);
		final OtpErlangList expressions = (OtpErlangList) clause0.elementAt(4);
		final OtpErlangList guard = guards.arity() == 0 ? null : guards;
		final OtpErlangObject selectable = selectables.arity() == 0 ? null : selectables.elementAt(0);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "clause");

		this.createContext();
		super.addCase(selectable, guard, expressions, ldNodeInfo);
		this.destroyContext(info);
	}
	private void processCall(OtpErlangTuple call)
	{
		final long line = ((OtpErlangLong) call.elementAt(1)).longValue();
		final OtpErlangTuple callee = (OtpErlangTuple) call.elementAt(2);
		final OtpErlangList arguments = (OtpErlangList) call.elementAt(3);
		final String calleeType = ((OtpErlangAtom) callee.elementAt(0)).atomValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "call");
		final OtpErlangTuple module;
		final OtpErlangTuple function;

		switch (calleeType)
		{
			case "remote":
				module = (OtpErlangTuple) callee.elementAt(2);
				function = (OtpErlangTuple) callee.elementAt(3);
				break;
			default:
				module = null;
				function = callee;
				break;
		}

		super.addCall(module, function, arguments, ldNodeInfo);
	}
	private void processTuple(OtpErlangTuple tuple)
	{
		final long line = ((OtpErlangLong) tuple.elementAt(1)).longValue();
		final OtpErlangList elements = (OtpErlangList) tuple.elementAt(2);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "tuple");

		super.addDataConstructor(elements, ldNodeInfo);
	}
	private void processList(OtpErlangTuple list)
	{
		final long line = ((OtpErlangLong) list.elementAt(1)).longValue();
		final OtpErlangAtom listType = (OtpErlangAtom) list.elementAt(0);
		final List<OtpErlangTuple> elements = new LinkedList<OtpErlangTuple>();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "list");

		if (!listType.atomValue().equals("nil"))
		{
			final OtpErlangTuple head = (OtpErlangTuple) list.elementAt(2);
			final OtpErlangTuple tail = (OtpErlangTuple) list.elementAt(3);

			elements.add(head);
			elements.add(tail);
		}
		super.addList(elements, ldNodeInfo);
	}
	private void processListComprehension(OtpErlangTuple listComprehension)
	{
		final long line = ((OtpErlangLong) listComprehension.elementAt(1)).longValue();
		final OtpErlangTuple value = (OtpErlangTuple) listComprehension.elementAt(2);
		final OtpErlangList restrictions = (OtpErlangList) listComprehension.elementAt(3);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "list comprehension");

		this.createContext();
		super.addListComprehension(restrictions, value, ldNodeInfo);
		this.destroyContext();
	}
	private void processGenerate(OtpErlangTuple generate)
	{
		final long line = ((OtpErlangLong) generate.elementAt(1)).longValue();
		final OtpErlangTuple pattern = (OtpErlangTuple) generate.elementAt(2);
		final OtpErlangTuple expression = (OtpErlangTuple) generate.elementAt(3);
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "generate");

		super.addGenerator(pattern, expression, ldNodeInfo);
	}
	private void processFilter(OtpErlangTuple filter)
	{
		final long line = ((OtpErlangLong) filter.elementAt(1)).longValue();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(line, "filter");

		super.addFilter(filter, ldNodeInfo);
	}

	// Variable declaration/use
	private Stack<List<String>> variableContexts = new Stack<List<String>>();

	private void createContext()
	{
		final List<String> variableContext = new LinkedList<String>();
		final List<String> lastVariableContext = this.variableContexts.size() == 0 ? new LinkedList<String>() : this.variableContexts.peek();

		variableContext.addAll(lastVariableContext);
		this.variableContexts.add(variableContext);
	}
	private void destroyContext()
	{
		this.variableContexts.pop();
	}
	private void destroyContext(Map<String, Object> info)
	{
		final List<String> lastVariableContext = this.variableContexts.pop();
		final EDGFactory.Branch parent = (EDGFactory.Branch) info.get("parent");
		final NodeInfo.Type type = parent.getNodeType();
		final Where where = parent.getWhere();
		final int index = parent.getIndex();
		final int length = parent.getLength();

		if (type == NodeInfo.Type.Switch && where == Where.Cases && index == length)
		{
			this.variableContexts.pop();
			this.variableContexts.push(lastVariableContext);
		}
	}
	private void addVariableToContext(String variableName)
	{
		if (variableName.equals("_"))
			return;

		final List<String> variableContext = this.variableContexts.peek();
		if (!variableContext.contains(variableName))
			variableContext.add(variableName);
	}
	private boolean isAssignation(String variableName, Map<String, Object> info)
	{
		if (variableName.equals("_"))
			return true;
		if (this.isAssignation(info))
			return true;
		if (!(boolean) info.get("patternZone"))
			return false;
		final List<String> variableContext = this.variableContexts.peek();
		return !variableContext.contains(variableName);
	}
	private boolean isAssignation(Map<String, Object> info)
	{
		@SuppressWarnings("unchecked")
		final List<EDGFactory.Branch> ancestors = (List<EDGFactory.Branch>) info.get("ancestors");

		for (int ancestorIndex = ancestors.size() - 1; ancestorIndex >= 0; ancestorIndex--)
		{
			final EDGFactory.Branch ancestor = ancestors.get(ancestorIndex);
			final NodeInfo.Type type = ancestor.getNodeType();
			final int index = ancestor.getIndex();

			switch (type)
			{
				case Generator:
					if (index == 1)
						return true;
					break;
				default:
					break;
			}
		}

		return false;
	}
}