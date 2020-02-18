package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class StarConstraint extends Constraint
{
	public boolean equals(Object object)
	{
		return object instanceof StarConstraint;
	}
	public String toString()
	{
		return "*";
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		if (phase == Phase.Slicing)
			constraintsStack = new SlicingConstraints();
		else 
		{
			constraintsStack.clear();
			constraintsStack.push(this);
		}

		constraintsStacks.add(constraintsStack);
		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, AccessConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SeekingConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, StarConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		constraintsStacks.add(constraintsStack);
		
		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SummaryConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, UnresolvableConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, EmptyConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
}