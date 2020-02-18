package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class UnresolvableConstraint extends Constraint
{
	public boolean equals(Object object)
	{
		return object instanceof UnresolvableConstraint;
	}
	public String toString()
	{
		return "-><-";
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		if (phase == Phase.Summary && ((SummaryConstraints)constraintsStack).isSeekingConstraint())
			return new LinkedList<Constraints>();
		
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		constraintsStack.push(this);
		constraintsStacks.add(constraintsStack);

		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, AccessConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SeekingConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase,constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, StarConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SummaryConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, UnresolvableConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
}