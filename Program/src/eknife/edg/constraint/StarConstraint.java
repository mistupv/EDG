package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.Edge;
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

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		if (phase == Phase.Slicing)
			constraintsStack = new SlicingConstraints();
		else 
		{
			if (!constraintsStack.isEmpty())
			{
				Constraint base = constraintsStack.firstElement();
				constraintsStack.clear();
				if (base instanceof ExceptionConstraint)
					constraintsStack.push(base);
			}
			constraintsStack.push(this);
		}

		constraintsStacks.add(constraintsStack);
		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, AccessConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SeekingConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, StarConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		constraintsStacks.add(constraintsStack);
		
		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SummaryConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, UnresolvableConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, EmptyConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
}