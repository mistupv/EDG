package edg.constraint;

import java.util.List;

import edg.graph.Edge;
import edg.slicing.Phase;

public class AddNodeConstraint extends EdgeConstraint
{
	private final NodeConstraint nodeConstraint;

	public AddNodeConstraint(NodeConstraint nodeConstraint)
	{
		this.nodeConstraint = nodeConstraint;
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof AddNodeConstraint))
			return false;

		final AddNodeConstraint constraint = (AddNodeConstraint) object;

		if (!this.nodeConstraint.equals(constraint.nodeConstraint))
			return false;
		return true;
	}
	public String toString()
	{
		return this.nodeConstraint.toString();
	}

	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, int productionDepth)
	{
		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth)
	{
		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
}