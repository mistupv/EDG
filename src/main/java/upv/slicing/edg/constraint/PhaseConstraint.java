package upv.slicing.edg.constraint;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.slicing.Phase;

import java.util.Arrays;
import java.util.List;

public class PhaseConstraint extends EdgeConstraint {
	private final List<Phase> allowedPhases;

	public PhaseConstraint(Phase... phases)
	{
		this.allowedPhases = Arrays.asList(phases);
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof PhaseConstraint))
			return false;

		final PhaseConstraint constraint = (PhaseConstraint) object;

		return !this.allowedPhases.equals(constraint.allowedPhases);
	}
	public String toString()
	{
		if (this.allowedPhases.size() == 1)
			return this.allowedPhases.get(0).name();
		return this.allowedPhases.toString();
	}

	private boolean allows(Phase phase)
	{
		for (Phase allowedPhase : this.allowedPhases)
			if (phase.isInstanceof(allowedPhase))
				return true;
		return false;
	}

	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, int productionDepth)
	{
		if (this.allows(phase))
			return super.wrap(constraints);
		return super.wrap();
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		if (this.allows(phase))
			return super.wrap(constraints);
		return super.wrap();
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		if (this.allows(phase))
			return super.wrap(constraints);
		return super.wrap();
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth)
	{
		if (this.allows(phase))
			return super.wrap(constraints);
		return super.wrap();
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		if (this.allows(phase))
			return super.wrap(constraints);
		return super.wrap();
	}
}