package upv.slicing.edg.constraint;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.slicing.Phase;

import java.util.List;

public abstract class EdgeConstraint extends Constraint
{
	public List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, EdgeConstraint topConstraint, int productionDepth)
	{
		if (topConstraint == null)
			return this.resolve(phase, edg, edge, constraints, productionDepth);
		else if (topConstraint instanceof ListComprehensionConstraint)
			return this.resolve(phase, edg, edge, constraints, (ListComprehensionConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof AccessConstraint)
			return this.resolve(phase, edg, edge, constraints, (AccessConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof EmptyConstraint)
			return this.resolve(phase, edg, edge, constraints, (EmptyConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof PhaseConstraint)
			return this.resolve(phase, edg, edge, constraints, (PhaseConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof SeekingConstraint)
			return this.resolve(phase, edg, edge, constraints, (SeekingConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof AsteriskConstraint)
			return this.resolve(phase, edg, edge, constraints, (AsteriskConstraint) topConstraint, productionDepth);
		else if (topConstraint instanceof GrammarConstraint)
			return this.resolve(phase, edg, edge, constraints, (GrammarConstraint) topConstraint, productionDepth);
		else
			throw new RuntimeException("Constraint type not contemplated: " + topConstraint.getClass().getName());
	}

	protected abstract List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, int productionDepth);
	protected abstract List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth);
	protected final List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AddNodeConstraint topConstraint, int productionDepth)
	{
		throw new RuntimeException("The add node constraint should not be on the stack");
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, ListComprehensionConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, edg, edge, constraints, (AccessConstraint) topConstraint, productionDepth);
	}
	protected final List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, EmptyConstraint topConstraint, int productionDepth)
	{
		throw new RuntimeException("The empty constraint should not be on the stack");
	}
	protected final List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, PhaseConstraint topConstraint, int productionDepth)
	{
		throw new RuntimeException("The phase constraint should not be on the stack");
	}
	protected abstract List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth);
	protected abstract List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth);
	protected abstract List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth);

	protected Constraints pop(Constraints constraints)
	{
		constraints.popEdgeConstraint();

		return constraints;
	}
	protected Constraints push(Phase phase, Constraints constraints)
	{
		if (phase.isInstanceof(Phase.Slicing) && constraints.sizeEdgeConstraints() == this.config.maxStackSize)
			throw new StackOverflowError();
//if (phase.isInstanceof(Phase.SummaryGeneration) && constraints.sizeEdgeConstraints() == this.config.maxStackSize)
//	throw new StackOverflowError();
		constraints.pushEdgeConstraint(this);

		return constraints;
	}
}