package edg.constraint;

public class GlobalVariableConstraint extends SeekingConstraint
{
	private final String variableName;

	public GlobalVariableConstraint(Operation operation, String variableName)
	{
		super(operation, SeekingConstraint.CompositeType.GlobalVariable);

		this.variableName = variableName;
	}

	public String getVariableName()
	{
		return this.variableName;
	}

	public GlobalVariableConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new GlobalVariableConstraint(Operation.Remove, this.variableName);
		if (this.operation == Operation.Remove)
			return new GlobalVariableConstraint(Operation.Add, this.variableName);
		return new GlobalVariableConstraint(Operation.LetThrough, this.variableName);
	}
	public boolean cancels(SeekingConstraint constraint)
	{
		if (this.operation == Operation.LetThrough)
			return false;
		if (this.operation == Operation.Remove)
		{
			if (!(constraint instanceof GlobalVariableConstraint))
				return false;
			final GlobalVariableConstraint gvConstraint = (GlobalVariableConstraint) constraint;
			if (gvConstraint.operation == Operation.Add && gvConstraint.variableName.equals("*"))
				return true;
		}	
		return this.equals(constraint.opposite());
	}
	public boolean letThrough(SeekingConstraint constraint)
	{
		if (!(constraint instanceof GlobalVariableConstraint))
			return false;

		final GlobalVariableConstraint gvConstraint = (GlobalVariableConstraint) constraint;

		if (!super.letThrough(gvConstraint))
			return false;
		return this.variableName.equals(gvConstraint.variableName) || gvConstraint.variableName.equals("*");
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof GlobalVariableConstraint))
			return false;

		final GlobalVariableConstraint constraint = (GlobalVariableConstraint) object;

		if (!super.equals(constraint))
			return false;
		return this.variableName.equals(constraint.variableName);
	}
	public String toString()
	{
		return super.toString() + this.variableName;
	}
}