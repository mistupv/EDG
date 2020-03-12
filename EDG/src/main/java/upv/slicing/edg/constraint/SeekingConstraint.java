package upv.slicing.edg.constraint;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.slicing.Phase;

import java.util.List;

public abstract class SeekingConstraint extends EdgeConstraint {
	public enum Operation {Add, LetThrough, Remove}

	public enum CompositeType {GlobalVariable, Exception}

	protected final Operation operation;
	protected final CompositeType compositeType;

	public SeekingConstraint(Operation operation, CompositeType compositeType)
	{
		this.operation = operation;
		this.compositeType = compositeType;
	}

	public Operation getOperation()
	{
		return this.operation;
	}
	public CompositeType getCompositeType()
	{
		return this.compositeType;
	}

	public abstract SeekingConstraint opposite();
	public boolean cancels(SeekingConstraint constraint)
	{
		if (this.operation == Operation.LetThrough)
			return false;
		return this.equals(constraint.opposite());
	}
	public boolean letThrough(SeekingConstraint constraint)
	{
		if (this.operation != Operation.LetThrough)
			return false;
		if (constraint.operation != Operation.Add && constraint.operation != Operation.LetThrough)
			return false;
		return this.compositeType == constraint.compositeType;
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof SeekingConstraint))
			return false;

		final SeekingConstraint constraint = (SeekingConstraint) object;

		if (this.operation != constraint.operation)
			return false;
		return this.compositeType == constraint.compositeType;
	}
	public String toString()
	{
		String toString = "";

		toString += this.operation == Operation.Add ? "+" :
					this.operation == Operation.LetThrough ? "" :
					this.operation == Operation.Remove ? "-" : "";
		toString += this.compositeType == CompositeType.GlobalVariable ? "GV" :
					this.compositeType == CompositeType.Exception ? "Ex" : "";

		return toString;
	}

	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, int productionDepth)
	{
		if (this.operation == Operation.Add)
			return super.wrap(super.push(phase, constraints));
		if (phase.isInstanceof(Phase.Slicing))
			return super.wrap();
		super.check(phase, Phase.SummaryGeneration);
		final Node to = edg.getEdgeTarget(edge);
		if (this.operation == Operation.LetThrough)
		{
			final Node parent = edg.getParent(to);
			if (parent.getType() != Node.Type.Clause)
				return super.wrap();
			final Node result = edg.getChild(parent, 3);
			if (to != result)
				return super.wrap();
		}
		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		if (this.operation == Operation.Add)
			return super.wrap(super.push(phase, constraints));
		return super.wrap();
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth)
	{
		if (this.cancels(topConstraint))
			return super.wrap(super.pop(constraints));
		if (this.letThrough(topConstraint))
			return super.wrap(constraints);
		if (phase.isInstanceof(Phase.Slicing))
			return super.wrap();
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, EDG edg, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
}
