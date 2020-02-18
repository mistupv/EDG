package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.Edge;
import eknife.edg.traverser.EdgeTraverser.Phase;

public abstract class Constraint
{
	public abstract boolean equals(Object object);
	public abstract String toString();

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, Constraint topConstraint, int productionDepth)
	{
		if (topConstraint == null)
			return this.resolve(phase, constraintsStack, edge, productionDepth);
		else if (topConstraint instanceof AccessConstraint)
			return this.resolve(phase, constraintsStack, edge, (AccessConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof BinComprehensionConstraint)
			return this.resolve(phase, constraintsStack, edge, (BinComprehensionConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof ListComprehensionConstraint)
			return this.resolve(phase, constraintsStack, edge, (ListComprehensionConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof SeekingConstraint)
			return this.resolve(phase, constraintsStack, edge, (SeekingConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof StarConstraint)
			return this.resolve(phase, constraintsStack, edge, (StarConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof SummaryConstraint)
			return this.resolve(phase, constraintsStack, edge, (SummaryConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof UnresolvableConstraint)
			return this.resolve(phase, constraintsStack, edge, (UnresolvableConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof EmptyConstraint)
			return this.resolve(phase, constraintsStack, edge, (EmptyConstraint) topConstraint, productionDepth);
		else
			throw new RuntimeException("Constraint type not contemplated: " + topConstraint.getClass().getName());
	}

	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, int productionDepth);
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, AccessConstraint accessConstraint, int productionDepth);
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, BinComprehensionConstraint topConstraint, int productionDepth)
	{
		throw new RuntimeException("Bin comprehension not contemplated");
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, ListComprehensionConstraint topConstraint, int productionDepth)
	{
		throw new RuntimeException("List comprehension not contemplated");
	}
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SeekingConstraint topConstraint, int productionDepth);
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, StarConstraint topConstraint, int productionDepth);
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SummaryConstraint topConstraint, int productionDepth);
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, UnresolvableConstraint topConstraint, int productionDepth);
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, EmptyConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		//ONLY AT SUMMARY PHASE
		if (!(this instanceof EmptyConstraint))
			constraintsStack.push(this);
		constraintsStacks.add(constraintsStack);
		return constraintsStacks;
	}
}