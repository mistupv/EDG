package edg.constraint;

import java.util.List;

import edg.graph.Edge;
import edg.slicing.Phase;

public class EmptyConstraint extends EdgeConstraint
{
	private static EmptyConstraint constraint = new EmptyConstraint();
	public static EmptyConstraint getConstraint()
	{
		return EmptyConstraint.constraint;
	}

	private EmptyConstraint()
	{
		
	}

	public boolean equals(Object object)
	{
		return object instanceof EmptyConstraint;
	}
	public String toString()
	{
		return "";
	}

	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, int productionDepth)
	{
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		return super.wrap(constraints);
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(constraints);
	}	
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth)
	{
		if (topConstraint.operation == SeekingConstraint.Operation.LetThrough)
			super.check(phase, Phase.SummaryGeneration);
		if (topConstraint.operation == SeekingConstraint.Operation.Add || topConstraint.operation == SeekingConstraint.Operation.LetThrough)
			return super.wrap();

		super.check(topConstraint.operation, SeekingConstraint.Operation.Remove);
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(constraints);
	}

	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(constraints);
	}
}