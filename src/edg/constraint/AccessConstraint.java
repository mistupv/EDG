package edg.constraint;

import java.util.List;

import edg.graph.Edge;
import edg.slicing.Phase;

public abstract class AccessConstraint extends EdgeConstraint
{
	public enum Operation { Add, Remove }
	public enum CompositeType { DataConstructor, List, ListComprehension }

	protected final Operation operation;
	protected final CompositeType compositeType;

	public AccessConstraint(Operation operation, CompositeType compositeType)
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

	public abstract AccessConstraint opposite();
	public boolean cancels(AccessConstraint constraint)
	{
		return this.equals(constraint.opposite());
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof AccessConstraint))
			return false;

		final AccessConstraint constraint = (AccessConstraint) object;

		if (this.operation != constraint.operation)
			return false;
		if (this.compositeType != constraint.compositeType)
			return false;
		return true;
	}
	public String toString()
	{
		String toString = "";

		toString += this.operation == Operation.Add ? "+" :
					this.operation == Operation.Remove ? "-" : "";
		toString += this.compositeType == CompositeType.DataConstructor ? "{}" :
					this.compositeType == CompositeType.List ? "[]" :
					this.compositeType == CompositeType.ListComprehension ? "[]" : "";

		return toString;
	}

	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, int productionDepth)
	{
		if (this.operation == Operation.Add)
			return super.wrap(super.push(phase, constraints));
		super.check(this.operation, Operation.Remove);
		if (phase.isInstanceof(Phase.Slicing))
			return super.wrap(constraints);
		super.check(phase, Phase.SummaryGeneration);
		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		if (topConstraint.operation == Operation.Add)
		{
			if (this.operation == Operation.Add)
				return super.wrap(super.push(phase, constraints));
			if (this.cancels(topConstraint))
				return super.wrap(super.pop(constraints));
			return super.wrap();
		}

		super.check(topConstraint.operation, Operation.Remove);
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, ListComprehensionConstraint topConstraint, int productionDepth)
	{
		if (topConstraint.operation == Operation.Add)
		{
			if (this.operation == Operation.Add)
				return super.wrap(super.push(phase, constraints));
			if (this.compositeType == CompositeType.List)
			{
				final ListConstraint listConstraint = (ListConstraint) this;
				if (listConstraint.position == ListConstraint.Position.H)
					return super.wrap(super.pop(constraints));
				else if (listConstraint.position == ListConstraint.Position.T)
					return super.wrap(constraints);
			}
			return super.wrap();
		}

		super.check(topConstraint.operation, Operation.Remove);
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth)
	{
		if (topConstraint.operation == SeekingConstraint.Operation.LetThrough)
			super.check(phase, Phase.SummaryGeneration);
		if (topConstraint.operation == SeekingConstraint.Operation.Add || topConstraint.operation == SeekingConstraint.Operation.LetThrough)
			return super.wrap();

		super.check(topConstraint.operation, SeekingConstraint.Operation.Remove);
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);

		return super.wrap(super.push(phase, constraints));
	}
}