package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class MapConstraint extends AccessConstraint{

	public MapConstraint(Operation operation)
	{
		super(operation, AccessConstraint.CompositeType.Map);
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof MapConstraint))
			return false;
		if (!super.equals(object))
			return false;
		return true;
	}
	public String toString()
	{
		return super.toString();
	}

	public MapConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new MapConstraint(Operation.Remove);
		return new MapConstraint(Operation.Add);
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