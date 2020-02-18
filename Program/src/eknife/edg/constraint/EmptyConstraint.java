package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class EmptyConstraint extends Constraint
{
	public boolean equals(Object object)
	{
		return object instanceof EmptyConstraint;
	}
	public String toString()
	{
		return "O";
	}
	
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth) 
	{
		if (constraintsStack.isSeekingConstraint())
			return new LinkedList<Constraints>();
		
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		//if (phase == Phase.Summary)
			//constraintsStack.push(this);
		
		constraintsStacks.add(constraintsStack);
		
		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, AccessConstraint accessConstraint, int productionDepth) 
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SeekingConstraint topConstraint, int productionDepth) 
	{
		return new LinkedList<Constraints>();
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
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, EmptyConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		constraintsStacks.add(constraintsStack);
		
		return constraintsStacks;
	}
}
