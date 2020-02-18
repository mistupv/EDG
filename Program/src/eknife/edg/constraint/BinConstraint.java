package eknife.edg.constraint;

public class BinConstraint extends AccessConstraint
{
	private final long min;
	private final long max;

	public BinConstraint(Operation operation, long min, long max)
	{
		super(operation, AccessConstraint.CompositeType.Bin);

		this.min = min;
		this.max = max;
	}

	public long getMin()
	{
		return this.min;
	}
	public long getMax()
	{
		return this.max;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof BinConstraint))
			return false;

		final BinConstraint constraint = (BinConstraint) object;

		if (!super.equals(constraint))
			return false;
		if (this.min != constraint.min)
			return false;
		if (this.max != constraint.max)
			return false;
		return true;
	}
	public String toString()
	{
		if (this.max != Long.MAX_VALUE)
			return super.toString() + "]" + this.min + "," + this.max + "]";
		return super.toString() + "]" + this.min + ",*]";
	}

	public BinConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new BinConstraint(Operation.Remove, this.min, this.max);
		return new BinConstraint(Operation.Add, this.min, this.max);
	}
	public boolean cancels(AccessConstraint constraint)
	{
		if (!(constraint instanceof BinConstraint))
			return false;

		final BinConstraint binConstraint = (BinConstraint) constraint;

		if (binConstraint.operation == this.operation)
			return false;
		if (binConstraint.min > this.min && binConstraint.min < this.max)
			return true;
		if (binConstraint.max > this.min && binConstraint.max <= this.max)
			return true;
		if (binConstraint.min <= this.min && binConstraint.max > this.max)
			return true;
		return false;
	}
}