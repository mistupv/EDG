package edg.constraint;

public class BinComprehensionConstraint extends Constraint
{
	public boolean equals(Object object)
	{
		return object instanceof BinComprehensionConstraint;
	}
	public String toString()
	{
		return "-<<>>*";
	}
}