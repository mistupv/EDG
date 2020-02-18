package eknife.edg;

import eknife.edg.constraint.Constraint;

public class EdgeInfo
{
	public enum Type
	{
		Control,
		NormalControl,
		StructuralControl,
		GuardRestriction,
		FlowDependence,
		ValueDependence,
		Input,
		Output,
		Summary,
		Exception,
		ExceptionFlag,
		ExceptionGetAll,
		Other
	}

	private final Type type;
	private final Constraint constraint;

	public EdgeInfo(Type type)
	{
		this(type, null);
	}
	public EdgeInfo(Type type, Constraint constraint)
	{
		this.type = type;
		this.constraint = constraint;
	}

	public Type getType()
	{
		return this.type;
	}
	public Constraint getConstraint()
	{
		return this.constraint;
	}

	public boolean equals(Object object)
	{
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