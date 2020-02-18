package eknife.edg.constraint;

public class ListComprehensionConstraint extends Constraint
{
	public boolean equals(Object object)
	{
		return object instanceof ListComprehensionConstraint;
	}
	public String toString()
	{
		return "-[]*";
	}
}