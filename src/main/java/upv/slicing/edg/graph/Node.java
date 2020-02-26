package upv.slicing.edg.graph;

import upv.slicing.edg.LDASTNodeInfo;

import java.util.Objects;

public class Node {
	public enum Type {
		// Module
		Module,

		// Routine
		Routine, Clause, Parameters(true),

		// Expressions
		List, DataConstructor, DataConstructorAccess, FieldAccess,
		Block, Operation, Equality, Pattern,
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
}
