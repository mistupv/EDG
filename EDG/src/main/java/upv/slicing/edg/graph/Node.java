package upv.slicing.edg.graph;

import com.sun.source.tree.Scope;
import upv.slicing.edg.LDASTNodeInfo;
import upv.slicing.edg.visitor.GenericVisitor;
import upv.slicing.edg.visitor.Visitable;
import upv.slicing.edg.visitor.VoidVisitor;

import java.beans.Expression;
import java.util.Objects;

public class Node implements Visitable {
	public enum Type {
		// Module
		Module,

		// Routine
		Routine, Clause, Parameters(true),

		// Expressions
		List, DataConstructor, DataConstructorAccess, FieldAccess,
		Block, Operation, Equality, Pattern, Enclosed,
		If, Condition(true), Then, Else,
		Switch, Selector(true), Cases(true), Case, DefaultCase, Selectable(true),
		Call, Callee, Scope(true), Name(true), Arguments(true),
		ListComprehension, Restrictions(true), Generator, Filter, Value(true),
		Loop, // <- DEPRECATED USED IN PHP
		CLoop, FLoop, RLoop, Foreach, Iterator, // LOOPS
		ExHandler, Try, Catch, Finally, CatchClause, Throw, // EXCEPTIONS

		// Others
		Body(true), Guard(true),
		Expression(true), Result(false, true),
		Variable, Literal,
		Return(false, true), Break(false, true), Continue(false, true),
		Root(true),
		Init(true), Update(true),
		TypeCheck, // JAVA instanceof
		TypeTransformation,
		Type(false, true),
		Reference, Label,
		ArgumentIn(true), ArgumentOut(true),
		ParameterIn, ParameterOut,
		Index; // To identify DataConstructorAccess Indexes

		private final boolean fictitious;
		private final boolean sliceable;

		Type()
		{
			this(false);
		}

		Type(boolean fictitious)
		{
			this(fictitious, false);
		}

		Type(boolean fictitious, boolean sliceable)
		{
			this.fictitious = fictitious;
			this.sliceable = sliceable;
		}

		public boolean isFictitious()
		{
			return fictitious;
		}

		public boolean isSliceable()
		{
			return sliceable;
		}
	}

	private String label;
	private final int id;
	private final LDASTNodeInfo ldASTNodeInfo;
	private String name;
	private final Type type;

	public Node(int id, Type type, String name, LDASTNodeInfo info)
	{
		this(null, id, type, name, info);
	}

	public Node(String label, int id, Type type, String name, LDASTNodeInfo info)
	{
		Objects.requireNonNull(type, "Type can't be null!");
		this.label = label;
		this.id = id;
		this.type = type;
		this.name = name;
		this.ldASTNodeInfo = info;
	}

	public String getLabel()
	{
		return label;
	}

	public void setLabel(String label)
	{
		Objects.requireNonNull(label);
		this.label = label;
	}

	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		Objects.requireNonNull(name);
		this.name = name;
	}

	public Type getType()
	{
		return type;
	}

	public int getId()
	{
		return id;
	}

	public LDASTNodeInfo getInfo()
	{
		return ldASTNodeInfo;
	}

	@Override
	public String toString()
	{
		return String.format("Node(label=%s,id=%d,type=%s,name=%s)", label, id, type.name(), name);
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;
		Node node = (Node) o;
		return id == node.id &&
				type == node.type;
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(id, type);
	}

	@Override
	public <R, A> R accept(GenericVisitor<R, A> visitor, A argument)
	{
		switch (type) {
			case Module:		return visitor.visitModule(this, argument);
			case Routine:		return visitor.visitRoutine(this, argument);
			case Clause:		return visitor.visitClause(this, argument);
			case Parameters:	return visitor.visitParameters(this, argument);
			case List:			return visitor.visitList(this, argument);
			case DataConstructor:	return visitor.visitDataConstructor(this, argument);
			case DataConstructorAccess:	return visitor.visitDataConstructorAccess(this, argument);
			case FieldAccess:	return visitor.visitFieldAccess(this, argument);
			case Block:			return visitor.visitBlock(this, argument);
			case Operation:		return visitor.visitOperation(this, argument);
			case Equality:		return visitor.visitEquality(this, argument);
			case Pattern:		return visitor.visitPattern(this, argument);
			case Enclosed:		return visitor.visitEnclosed(this, argument);
			case If:			return visitor.visitIf(this, argument);
			case Condition:		return visitor.visitCondition(this, argument);
			case Then:			return visitor.visitThen(this, argument);
			case Else:			return visitor.visitElse(this, argument);
			case Switch:		return visitor.visitSwitch(this, argument);
			case Selector:		return visitor.visitSelector(this, argument);
			case Cases:			return visitor.visitCases(this, argument);
			case Case:			return visitor.visitCase(this, argument);
			case DefaultCase:	return visitor.visitDefaultCase(this, argument);
			case Selectable:	return visitor.visitSelectable(this, argument);
			case Call:			return visitor.visitCall(this, argument);
			case Callee:		return visitor.visitCallee(this, argument);
			case Scope:			return visitor.visitScope(this, argument);
			case Name:			return visitor.visitName(this, argument);
			case Arguments:		return visitor.visitArguments(this, argument);
			case ListComprehension:	return visitor.visitListComprehension(this, argument);
			case Restrictions:	return visitor.visitRestrictions(this, argument);
			case Generator:		return visitor.visitGenerator(this, argument);
			case Filter:		return visitor.visitFilter(this, argument);
			case Value:			return visitor.visitValue(this, argument);
			case Loop:			return visitor.visitLoop(this, argument);
			case CLoop:			return visitor.visitCLoop(this, argument);
			case FLoop:			return visitor.visitFLoop(this, argument);
			case RLoop:			return visitor.visitRLoop(this, argument);
			case Foreach:		return visitor.visitForeach(this, argument);
			case Iterator:		return visitor.visitIterator(this, argument);
			case ExHandler:		return visitor.visitExHandler(this, argument);
			case Try:			return visitor.visitTry(this, argument);
			case Catch:			return visitor.visitCatch(this, argument);
			case Finally:		return visitor.visitFinally(this, argument);
			case CatchClause:	return visitor.visitCatchClause(this, argument);
			case Throw:			return visitor.visitThrow(this, argument);
			case Body:			return visitor.visitBody(this, argument);
			case Guard:			return visitor.visitGuard(this, argument);
			case Expression:	return visitor.visitExpression(this, argument);
			case Result:		return visitor.visitResult(this, argument);
			case Variable:		return visitor.visitVariable(this, argument);
			case Literal:		return visitor.visitLiteral(this, argument);
			case Return:		return visitor.visitReturn(this, argument);
			case Break:			return visitor.visitBreak(this, argument);
			case Continue:		return visitor.visitContinue(this, argument);
			case Root:			return visitor.visitRoot(this, argument);
			case Init:			return visitor.visitInit(this, argument);
			case Update:		return visitor.visitUpdate(this, argument);
			case TypeCheck:		return visitor.visitTypeCheck(this, argument);
			case TypeTransformation:	return visitor.visitTypeTransformation(this, argument);
			case Type:			return visitor.visitType(this, argument);
			case Reference:		return visitor.visitReference(this, argument);
			case Label:			return visitor.visitLabel(this, argument);
			case ArgumentIn:	return visitor.visitArgumentIn(this, argument);
			case ArgumentOut:	return visitor.visitArgumentOut(this, argument);
			case ParameterIn:	return visitor.visitParameterIn(this, argument);
			case ParameterOut:	return visitor.visitParameterOut(this, argument);
			case Index:			return visitor.visitIndex(this, argument);
			default:
				throw new IllegalStateException("Type cannot be visited: " + type);
		}
	}

	@Override
	public <A> void accept(VoidVisitor<A> visitor, A argument)
	{
		switch (type)
		{
			case Module:
				visitor.visitModule(this, argument);
				break;
			case Routine:
				visitor.visitRoutine(this, argument);
				break;
			case Clause:
				visitor.visitClause(this, argument);
				break;
			case Parameters:
				visitor.visitParameters(this, argument);
				break;
			case List:
				visitor.visitList(this, argument);
				break;
			case DataConstructor:
				visitor.visitDataConstructor(this, argument);
				break;
			case DataConstructorAccess:
				visitor.visitDataConstructorAccess(this, argument);
				break;
			case FieldAccess:
				visitor.visitFieldAccess(this, argument);
				break;
			case Block:
				visitor.visitBlock(this, argument);
				break;
			case Operation:
				visitor.visitOperation(this, argument);
				break;
			case Equality:
				visitor.visitEquality(this, argument);
				break;
			case Pattern:
				visitor.visitPattern(this, argument);
				break;
			case Enclosed:
				visitor.visitEnclosed(this, argument);
				break;
			case If:
				visitor.visitIf(this, argument);
				break;
			case Condition:
				visitor.visitCondition(this, argument);
				break;
			case Then:
				visitor.visitThen(this, argument);
				break;
			case Else:
				visitor.visitElse(this, argument);
				break;
			case Switch:
				visitor.visitSwitch(this, argument);
				break;
			case Selector:
				visitor.visitSelector(this, argument);
				break;
			case Cases:
				visitor.visitCases(this, argument);
				break;
			case Case:
				visitor.visitCase(this, argument);
				break;
			case DefaultCase:
				visitor.visitDefaultCase(this, argument);
				break;
			case Selectable:
				visitor.visitSelectable(this, argument);
				break;
			case Call:
				visitor.visitCall(this, argument);
				break;
			case Callee:
				visitor.visitCallee(this, argument);
				break;
			case Scope:
				visitor.visitScope(this, argument);
				break;
			case Name:
				visitor.visitName(this, argument);
				break;
			case Arguments:
				visitor.visitArguments(this, argument);
				break;
			case ListComprehension:
				visitor.visitListComprehension(this, argument);
				break;
			case Restrictions:
				visitor.visitRestrictions(this, argument);
				break;
			case Generator:
				visitor.visitGenerator(this, argument);
				break;
			case Filter:
				visitor.visitFilter(this, argument);
				break;
			case Value:
				visitor.visitValue(this, argument);
				break;
			case Loop:
				visitor.visitLoop(this, argument);
				break;
			case CLoop:
				visitor.visitCLoop(this, argument);
				break;
			case FLoop:
				visitor.visitFLoop(this, argument);
				break;
			case RLoop:
				visitor.visitRLoop(this, argument);
				break;
			case Foreach:
				visitor.visitForeach(this, argument);
				break;
			case Iterator:
				visitor.visitIterator(this, argument);
				break;
			case ExHandler:
				visitor.visitExHandler(this, argument);
				break;
			case Try:
				visitor.visitTry(this, argument);
				break;
			case Catch:
				visitor.visitCatch(this, argument);
				break;
			case Finally:
				visitor.visitFinally(this, argument);
				break;
			case CatchClause:
				visitor.visitCatchClause(this, argument);
				break;
			case Throw:
				visitor.visitThrow(this, argument);
				break;
			case Body:
				visitor.visitBody(this, argument);
				break;
			case Guard:
				visitor.visitGuard(this, argument);
				break;
			case Expression:
				visitor.visitExpression(this, argument);
				break;
			case Result:
				visitor.visitResult(this, argument);
				break;
			case Variable:
				visitor.visitVariable(this, argument);
				break;
			case Literal:
				visitor.visitLiteral(this, argument);
				break;
			case Return:
				visitor.visitReturn(this, argument);
				break;
			case Break:
				visitor.visitBreak(this, argument);
				break;
			case Continue:
				visitor.visitContinue(this, argument);
				break;
			case Root:
				visitor.visitRoot(this, argument);
				break;
			case Init:
				visitor.visitInit(this, argument);
				break;
			case Update:
				visitor.visitUpdate(this, argument);
				break;
			case TypeCheck:
				visitor.visitTypeCheck(this, argument);
				break;
			case TypeTransformation:
				visitor.visitTypeTransformation(this, argument);
				break;
			case Type:
				visitor.visitType(this, argument);
				break;
			case Reference:
				visitor.visitReference(this, argument);
				break;
			case Label:
				visitor.visitLabel(this, argument);
				break;
			case ArgumentIn:
				visitor.visitArgumentIn(this, argument);
				break;
			case ArgumentOut:
				visitor.visitArgumentOut(this, argument);
				break;
			case ParameterIn:
				visitor.visitParameterIn(this, argument);
				break;
			case ParameterOut:
				visitor.visitParameterOut(this, argument);
				break;
			case Index:
				visitor.visitIndex(this, argument);
				break;
			default:
				throw new IllegalStateException("Type cannot be visited: " + type);
		}
	}
}
