package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.Edge;
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

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, AccessConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SeekingConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, StarConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SummaryConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, UnresolvableConstraint topConstraint, int productionDepth)
	{
		return new LinkedList<Constraints>();
	}
}