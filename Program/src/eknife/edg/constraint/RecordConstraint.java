package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class RecordConstraint extends AccessConstraint
{
	private String field;

	public RecordConstraint(Operation operation, String field)
	{
		super(operation, AccessConstraint.CompositeType.Record);

		this.field = field;
	}

	public String getField()
	{
		return this.field;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof RecordConstraint))
			return false;
		if (!super.equals(object))
			return false;

		final RecordConstraint constraint = (RecordConstraint) object;

		if (!this.field.equals(constraint.field))
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString() + this.field;
	}

	public RecordConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new RecordConstraint(Operation.Remove, this.field);
		return new RecordConstraint(Operation.Add, this.field);
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();



		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Constraint topConstraint)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();



		return constraintsStacks;
	}
}