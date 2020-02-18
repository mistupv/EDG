package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.Edge;
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
	
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, int productionDepth) 
	{
		if (phase == Phase.Summary && ((SummaryConstraints)constraintsStack).isSeekingConstraint())
			return new LinkedList<Constraints>();
		
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		if (phase == Phase.Summary && constraintsStack.isEmpty())
			constraintsStack.push(this);
		
		constraintsStacks.add(constraintsStack);
		
		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, AccessConstraint accessConstraint, int productionDepth) 
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SeekingConstraint topConstraint, int productionDepth) 
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, StarConstraint topConstraint, int productionDepth) 
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
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
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		constraintsStacks.add(constraintsStack);
		
		return constraintsStacks;
	}
}
