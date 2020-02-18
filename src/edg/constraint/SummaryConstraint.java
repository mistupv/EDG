package edg.constraint;

import edg.graph.Node;

public class SummaryConstraint extends Constraint
{
	private final Node formalIn;

	public SummaryConstraint(Node formalIn)
	{
		this.formalIn = formalIn;
	}

	public Node getFormalIn()
	{
		return this.formalIn;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof SummaryConstraint))
			return false;

		final SummaryConstraint constraint = (SummaryConstraint) object;

		if (this.formalIn != constraint.formalIn)
			return false;
		return true;
	}
	public String toString()
	{
		return this.formalIn.getData().getId() + "";
	}
}