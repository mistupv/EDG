package eknife.edg.constraint;

public class MapConstraint extends AccessConstraint{

	public MapConstraint(Operation operation)
	{
		super(operation, AccessConstraint.CompositeType.Map);
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof MapConstraint))
			return false;
		if (!super.equals(object))
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString();
	}

	public MapConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new MapConstraint(Operation.Remove);
		return new MapConstraint(Operation.Add);
	}
}