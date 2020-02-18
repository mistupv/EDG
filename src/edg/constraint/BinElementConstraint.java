package edg.constraint;

public class BinElementConstraint extends AccessConstraint
{
	public enum Component { V, S, T }

	private Component component;

	public BinElementConstraint(Operation operation, Component component)
	{
		super(operation, CompositeType.BinElement);

		this.component = component;
	}

	public Component getComponent()
	{
		return this.component;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof BinElementConstraint))
			return false;

		final BinElementConstraint constraint = (BinElementConstraint) object;

		if (!super.equals(constraint))
			return false;
		if (this.component != constraint.component)
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString() + this.component;
	}

	public BinElementConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new BinElementConstraint(Operation.Remove, this.component);
		return new BinElementConstraint(Operation.Add, this.component);
	}
}