package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class BinComprehensionConstraint extends Constraint
{
	public boolean equals(Object object)
	{
		return object instanceof BinComprehensionConstraint;
	}
	public String toString()
	{
		return "-<<>>*";
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, AccessConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SeekingConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, StarConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SummaryConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, UnresolvableConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
}