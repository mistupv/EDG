package edg.constraint;

public class StarConstraint extends Constraint
{
	public boolean equals(Object object)
	{
		return object instanceof StarConstraint;
	}
	public String toString()
	{
		return "*";
	}
}