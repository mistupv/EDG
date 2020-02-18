package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public abstract class Constraint
{
	public abstract boolean equals(Object object);
	public abstract String toString();

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Constraint topConstraint, int productionDepth)
	{
		if (topConstraint == null)
			return this.resolve(phase, constraintsStack, productionDepth);
		else if (topConstraint instanceof AccessConstraint)
			return this.resolve(phase, constraintsStack, (AccessConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof BinComprehensionConstraint)
			return this.resolve(phase, constraintsStack, (BinComprehensionConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof ListComprehensionConstraint)
			return this.resolve(phase, constraintsStack, (ListComprehensionConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof SeekingConstraint)
			return this.resolve(phase, constraintsStack, (SeekingConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof StarConstraint)
			return this.resolve(phase, constraintsStack, (StarConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof SummaryConstraint)
			return this.resolve(phase, constraintsStack, (SummaryConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof UnresolvableConstraint)
			return this.resolve(phase, constraintsStack, (UnresolvableConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof EmptyConstraint)
			return this.resolve(phase, constraintsStack, (EmptyConstraint) topConstraint, productionDepth);
		else
			throw new RuntimeException("Constraint type not contemplated: " + topConstraint.getClass().getName());
	}

	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth);
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, AccessConstraint accessConstraint, int productionDepth);
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, BinComprehensionConstraint topConstraint, int productionDepth)
	{
		throw new RuntimeException("Bin comprehension not contemplated");
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, ListComprehensionConstraint topConstraint, int productionDepth)
	{
		throw new RuntimeException("List comprehension not contemplated");
	}
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, SeekingConstraint topConstraint, int productionDepth);
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, StarConstraint topConstraint, int productionDepth);
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, SummaryConstraint topConstraint, int productionDepth);
	public abstract List<Constraints> resolve(Phase phase, Constraints constraintsStack, UnresolvableConstraint topConstraint, int productionDepth);
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, EmptyConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		//ONLY AT SUMMARY PHASE
		constraintsStack.push(this);
		constraintsStacks.add(constraintsStack);
		return constraintsStacks;
	}
}