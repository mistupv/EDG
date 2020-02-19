package upv.slicing.edg.graph;

import upv.slicing.edg.constraint.AsteriskConstraint;
import upv.slicing.edg.constraint.EdgeConstraint;
import upv.slicing.edg.constraint.EmptyConstraint;

public class EdgeInfo {
	public enum Type {
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
		return this.constraint.equals(edgeInfo.constraint);
	}
}