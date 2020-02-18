package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.constraint.SeekingConstraint.Operation;
import eknife.edg.traverser.EdgeTraverser.Phase;

public class ExceptionConstraint extends SeekingConstraint
{
	private String exceptionField;

	public ExceptionConstraint(Operation operation, String field)
	{
		super(operation, SeekingConstraint.CompositeType.Exception);

		this.exceptionField = field;
	}

	public String getField()
	{
		return this.exceptionField;
	}

	public boolean letThrough(SeekingConstraint constraint) // Método paso a nivel para poner lo de la excepcion en la pila en lugar de un flag
	{
		if (!(constraint instanceof ExceptionConstraint))
			return false;

		ExceptionConstraint ec = (ExceptionConstraint) constraint;

		if (ec.getField().equals(this.exceptionField))
			return true;
		if (ec.getField().equals("*"))
			return true;
		if (ec.getOperation() == SeekingConstraint.Operation.Add && this.exceptionField.equals("*"))
			return true;
		return false;
	}
	public boolean letThroughWithEmptyStack(Phase phase)
	{
		return phase == Phase.Summary || this.operation == SeekingConstraint.Operation.Add;
	}
	public boolean match(SeekingConstraint seekingConstraint)
	{
		if (!(seekingConstraint instanceof ExceptionConstraint))
			return false;

		ExceptionConstraint ec = (ExceptionConstraint) seekingConstraint;

		if (ec.getField().equals(this.exceptionField))
			return true;
		if (ec.getField().equals("*"))
			return true;
		return false;
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
		if (this.exceptionField == null)
			return "";
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
	public boolean cancels(SeekingConstraint constraint)
	{
		if (!(constraint instanceof ExceptionConstraint))
			return false;
		ExceptionConstraint ec = (ExceptionConstraint) constraint;
		if (ec.getOperation() == Operation.Add && this.operation == Operation.Remove && ec.getField().equals("*"))
			return true;
		if (ec.getOperation() == Operation.Add && this.operation == Operation.Remove && this.exceptionField.equals("*"))
			return true;
		else if (ec.equals(this.opposite()))
			return true;
		else
			return false;
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		if (this.exceptionField == null)
		{
			if (this.operation == Operation.GetAll && phase == Phase.Slicing)
				((SlicingConstraints)constraintsStack).setExceptionGetAll(true);
			
			final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
			constraintsStacks.add(constraintsStack);
			return constraintsStacks;
		}

		return super.resolve(phase, constraintsStack, productionDepth);
	}
}