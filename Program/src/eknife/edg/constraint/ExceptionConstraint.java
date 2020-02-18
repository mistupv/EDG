package eknife.edg.constraint;

public class ExceptionConstraint extends AccessConstraint
{
	private String exceptionField;

	public ExceptionConstraint(Operation operation, String field)
	{
		super(operation, AccessConstraint.CompositeType.Exception);

		this.exceptionField = field;
	}

	public String getField()
	{
		return this.exceptionField;
	}

	public boolean letThrough(AccessConstraint constraint) // Método paso a nivel para poner lo de la excepcion en la pila en lugar de un flag
	{
		if (!(constraint instanceof ExceptionConstraint))
			return false;

		ExceptionConstraint ec = (ExceptionConstraint) constraint;

		if (ec.getField().equals(this.exceptionField))
			return true;
		if (ec.getField().equals("*"))
			return true;
		if (ec.getOperation() == AccessConstraint.Operation.Add && this.exceptionField.equals("*"))
			return true;
		return false;
	}
	public boolean letThroughWithEmptyStack(boolean resolveSummary)
	{
		return !resolveSummary || this.operation == AccessConstraint.Operation.Add;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof ExceptionConstraint))
			return false;
		if (!super.equals(object))
			return false;

		final ExceptionConstraint constraint = (ExceptionConstraint) object;

		if (!this.exceptionField.equals(constraint.exceptionField))
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString() + this.exceptionField;
	}

	public ExceptionConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new ExceptionConstraint(Operation.Remove, this.exceptionField);
		if (this.operation == null)
			return new ExceptionConstraint(null, this.exceptionField);
		return new ExceptionConstraint(Operation.Add, this.exceptionField);
	}
	public boolean cancels(AccessConstraint constraint)
	{
		if (!(constraint instanceof ExceptionConstraint))
			return false;
		ExceptionConstraint ec = (ExceptionConstraint) constraint;
		if (constraint.getOperation() == Operation.Add && this.operation == Operation.Remove && ec.getField().equals("*"))
			return true;
		if (this.exceptionField.equals("*"))
			return true;
		else if (ec.equals(this.opposite()))
			return true;
		else
			return false;
	}
}