package edg.graph;

import edg.constraint.EdgeConstraint;
import edg.constraint.EmptyConstraint;
import edg.constraint.AsteriskConstraint;

public class EdgeInfo
{
	public static enum Type
	{
		ControlFlow,
		Structural, Control,
		Value, Flow,
		Call, Input, Output, Summary,
		Exception
	}

	private final Type type;
	private final EdgeConstraint constraint;

	public EdgeInfo(Type type)
	{
		this(type, type == Type.Structural || type == Type.Control ? AsteriskConstraint.getConstraint() : EmptyConstraint.getConstraint());
	}
	public EdgeInfo(Type type, EdgeConstraint constraint)
	{
		this.type = type;
		this.constraint = constraint;
	}

	public Type getType()
	{
		return this.type;
	}
	public EdgeConstraint getConstraint()
	{
		return this.constraint;
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof EdgeInfo))
			return false;

		final EdgeInfo edgeInfo = (EdgeInfo) object;

		if (this.type != edgeInfo.type)
			return false;
		if (this.constraint == edgeInfo.constraint)
			return true;
		if (this.constraint == null || edgeInfo.constraint == null)
			return false;
		if (!this.constraint.equals(edgeInfo.constraint))
			return false;
		return true;
	}
}