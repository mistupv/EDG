package edg.constraint;

public class GlobalVariableConstraint extends SeekingConstraint
{
	private final String variableName;

	public GlobalVariableConstraint(Operation operation, String variableName)
	{
		super(operation, CompositeType.GlobalVariable);

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
	public boolean letThrough(SeekingConstraint constraint)
	{
		if (!(constraint instanceof GlobalVariableConstraint))
			return false;

		final GlobalVariableConstraint gvConstraint = (GlobalVariableConstraint) constraint;

		if (!super.letThrough(gvConstraint))
			return false;
		if (!this.variableName.equals(gvConstraint.variableName))
			return false;
		return true;
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
		if (!this.variableName.equals(constraint.variableName))
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString() + this.variableName;
	}
}