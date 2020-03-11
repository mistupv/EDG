package upv.slicing.edg.constraint;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.slicing.Phase;

import java.util.List;

public class AsteriskConstraint extends EdgeConstraint
{
	private static AsteriskConstraint constraint = new AsteriskConstraint();
	public static AsteriskConstraint getConstraint()
	{
		return AsteriskConstraint.constraint;
	}

	private AsteriskConstraint()
	{
		
	}

	public boolean equals(Object object)
	{
		return object instanceof AsteriskConstraint;
	}
	public String toString()
	{
		return "*";
	}

	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, int productionDepth)
	{
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		if (phase.isInstanceof(Phase.Slicing))
			return super.wrap(new Constraints());
		super.check(phase, Phase.SummaryGeneration);

		if (topConstraint.operation == AccessConstraint.Operation.Remove)
			return super.wrap(super.push(phase, constraints));
		super.check(topConstraint.operation, AccessConstraint.Operation.Add);

		final Constraints newConstraints = super.pop(constraints);
		if (newConstraints.isEdgeConstraintsEmpty())
			return super.wrap(newConstraints);

		final EdgeConstraint peekConstraint = newConstraints.peekEdgeConstraint();
		return super.resolve(phase, edg, edge, newConstraints, peekConstraint, productionDepth);
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth)
	{
		if (phase.isInstanceof(Phase.Slicing))
			return super.wrap(new Constraints());
		super.check(phase, Phase.SummaryGeneration);

		if (topConstraint.operation == SeekingConstraint.Operation.LetThrough || topConstraint.operation == SeekingConstraint.Operation.Remove)
			return super.wrap(super.push(phase, constraints));
		super.check(topConstraint.operation, SeekingConstraint.Operation.Add);

		final Constraints newConstraints = super.pop(constraints);
		if (newConstraints.isEdgeConstraintsEmpty())
			return super.wrap(newConstraints);

		final EdgeConstraint peekConstraint = newConstraints.peekEdgeConstraint();
		return super.resolve(phase, edg, edge, newConstraints, peekConstraint, productionDepth);
	}
		
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(constraints);
	}
}