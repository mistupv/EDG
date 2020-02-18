package edg.constraint;

public class ListConstraint extends AccessConstraint
{
	public enum Position { H, T }

	private Position position;

	public ListConstraint(Operation operation, Position position)
	{
		super(operation, AccessConstraint.CompositeType.List);

		this.position = position;
	}

	public Position getPosition()
	{
		return this.position;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof ListConstraint))
			return false;

		final ListConstraint constraint = (ListConstraint) object;

		if (!super.equals(constraint))
			return false;
		if (this.position != constraint.position)
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString() + this.position.toString();
	}

	public ListConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new ListConstraint(Operation.Remove, this.position);
		return new ListConstraint(Operation.Add, this.position);
	}
}