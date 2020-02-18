package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.config.Config;
import eknife.edg.Grammar;
import eknife.edg.Node;
import eknife.edg.constraint.SummaryConstraints.SummaryType;
import eknife.edg.traverser.EdgeTraverser.Phase;

public class SummaryConstraint extends Constraint
{
	private final Grammar grammar;
	private final Node formalIn;

	public SummaryConstraint(Grammar grammar, Node formalIn)
	{
		this.grammar = grammar;
		this.formalIn = formalIn;
	}

	public Node getFormalIn()
	{
		return this.formalIn;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof SummaryConstraint))
			return false;

		final SummaryConstraint constraint = (SummaryConstraint) object;

		if (this.formalIn != constraint.formalIn)
			return false;
		return true;
	}
	public String toString()
	{
		return this.formalIn.getData().getId() + "";
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();

		if (phase == Phase.Slicing)
		{
			if (productionDepth == Config.maxProductionDepth)
			{
				constraintsStacks.add(null);
				return constraintsStacks;
			}
			constraintsStacks.addAll(this.resolve0(phase, constraintsStack, productionDepth));
		}
		else if (phase == Phase.Summary)
		{
			constraintsStack.push(this);
			((SummaryConstraints) constraintsStack).setSeekingConstraint(null);
			constraintsStacks.add(constraintsStack);
		}
		return constraintsStacks;
	}
	private List<Constraints> resolve0(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
		final List<SummaryConstraints> summaryStackList = this.grammar.getProductions(this);

		for (SummaryConstraints summaryStack : summaryStackList)
		{
			final Constraints constraintsStackClone = (Constraints) constraintsStack.clone();
			final List<Constraints> stacks = new LinkedList<Constraints>();
			
			stacks.add(constraintsStackClone);

			final int stackSize = summaryStack.size();
			for (int index = 0; index < stackSize; index++)
			{
				final Constraint element = summaryStack.get(index);
				final List<Constraints> newConstraintsStacks0 = new LinkedList<Constraints>();

				for (Constraints stack : stacks)
				{
					final Constraint topConstraint = stack.isEmpty() ? null : stack.peek();

					newConstraintsStacks0.addAll(element.resolve(phase, stack, topConstraint, productionDepth));
					if (newConstraintsStacks0.size() == 1 && newConstraintsStacks0.get(0) == null)
						return newConstraintsStacks0;
				}
				stacks.clear();
				stacks.addAll(newConstraintsStacks0);
			}

			newConstraintsStacks.addAll(stacks);
		}

		return newConstraintsStacks;
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