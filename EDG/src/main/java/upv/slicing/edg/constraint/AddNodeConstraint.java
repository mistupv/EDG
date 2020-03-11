package upv.slicing.edg.constraint;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.slicing.Phase;

import java.util.List;

public class AddNodeConstraint extends EdgeConstraint {
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

		return this.nodeConstraint.equals(constraint.nodeConstraint);
	}
	public String toString()
	{
		return this.nodeConstraint.toString();
	}

	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, int productionDepth)
	{
		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth)
	{
		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		constraints.nodeConstraints.add(this.nodeConstraint);
		return super.wrap(constraints);
	}
}