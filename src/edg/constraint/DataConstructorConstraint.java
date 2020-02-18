package edg.constraint;

public class DataConstructorConstraint extends AccessConstraint
{
	private final String index;

	public DataConstructorConstraint(Operation operation)
	{
		this(operation, null);
	}
	public DataConstructorConstraint(Operation operation, String index)
	{
		super(operation, AccessConstraint.CompositeType.DataConstructor);

		this.index = index;
	}

	public String getIndex()
	{
		return this.index;
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof DataConstructorConstraint))
			return false;

		final DataConstructorConstraint constraint = (DataConstructorConstraint) object;

		if (!super.equals(constraint))
			return false;
		if (this.index == null || constraint.index == null)
			if (this.index != null || constraint.index != null)
				return false;
		return this.index == null || this.index.equals(constraint.index);
	}
	public String toString()
	{
		if (this.index != null)
			return super.toString() + this.index;
		return super.toString();
	}

	public DataConstructorConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new DataConstructorConstraint(Operation.Remove, this.index);
		return new DataConstructorConstraint(Operation.Add, this.index);
	}
	public boolean cancels(AccessConstraint constraint)
	{
		if (!(constraint instanceof DataConstructorConstraint))
			return super.cancels(constraint);

		final DataConstructorConstraint dataConstructorConstraint = (DataConstructorConstraint) constraint;
		if (dataConstructorConstraint.index != null && dataConstructorConstraint.index.equals("*"))
			return true;
		return super.cancels(constraint);
	}
}