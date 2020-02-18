package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class TupleConstraint extends AccessConstraint
{
	private int index;

	public TupleConstraint(Operation operation, int index)
	{
		super(operation, AccessConstraint.CompositeType.Tuple);

		this.index = index;
	}

	public int getIndex()
	{
		return this.index;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof TupleConstraint))
			return false;

		final TupleConstraint constraint = (TupleConstraint) object;

		if (!super.equals(constraint))
			return false;
		if (this.index != constraint.index)
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString() + this.index;
	}

	public TupleConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new TupleConstraint(Operation.Remove, this.index);
		return new TupleConstraint(Operation.Add, this.index);
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