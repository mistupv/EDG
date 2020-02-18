package eknife.edg.constraint;

public class RecordConstraint extends AccessConstraint
{
	private String field;

	public RecordConstraint(Operation operation, String field)
	{
		super(operation, AccessConstraint.CompositeType.Record);

		this.field = field;
	}

	public String getField()
	{
		return this.field;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof RecordConstraint))
			return false;
		if (!super.equals(object))
			return false;

		final RecordConstraint constraint = (RecordConstraint) object;

		if (!this.field.equals(constraint.field))
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString() + this.field;
	}

	public RecordConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new RecordConstraint(Operation.Remove, this.field);
		return new RecordConstraint(Operation.Add, this.field);
	}
}