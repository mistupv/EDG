package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.constraint.AccessConstraint.Operation;
import eknife.edg.traverser.EdgeTraverser.Phase;

public class ExceptionArgumentConstraint  extends AccessConstraint{
	
	private String argumentField;
	
	public ExceptionArgumentConstraint(Operation operation, String field)
	{
		super(operation, AccessConstraint.CompositeType.ExceptionArgument);
		
		this.argumentField = field;
	}

	public String getField()
	{
		return this.argumentField;
	}
	public boolean equals(Object object)
	{
		if (!(object instanceof ExceptionArgumentConstraint))
			return false;
		if (!super.equals(object))
			return false;
		
		final ExceptionArgumentConstraint constraint = (ExceptionArgumentConstraint) object;
		
		if (!this.argumentField.equals(constraint.argumentField))
			return false;
		return true;
		
	}
	public String toString()
	{
		return super.toString() + this.argumentField;
	}

	public ExceptionArgumentConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new ExceptionArgumentConstraint(Operation.Remove, this.argumentField);
		return new ExceptionArgumentConstraint(Operation.Add, this.argumentField);
	}
	public boolean cancels(AccessConstraint constraint)
	{
		if (!(constraint instanceof ExceptionArgumentConstraint))
			return false;
		ExceptionArgumentConstraint eac = (ExceptionArgumentConstraint) constraint;
		if (constraint.getOperation() == Operation.Add && this.operation == Operation.Remove && eac.getField().equals("*"))
			return true;
		if (this.argumentField.equals("*"))
			return true;
		else if (eac.equals(this.opposite()))
			return true;
		else
			return false;
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		if (phase == Phase.Slicing && this.operation == Operation.Remove)
			return new LinkedList<Constraints>();
		return super.resolve(phase, constraintsStack, productionDepth);
	}
}