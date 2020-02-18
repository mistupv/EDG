package edg.constraint;

public class UnresolvableConstraint extends Constraint
{
	public boolean equals(Object object)
	{
		return object instanceof UnresolvableConstraint;
	}
	public String toString()
	{
		return "-><-";
	}
}