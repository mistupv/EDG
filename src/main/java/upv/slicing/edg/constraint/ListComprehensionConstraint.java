package upv.slicing.edg.constraint;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.slicing.Phase;

import java.util.List;

public class ListComprehensionConstraint extends AccessConstraint {
	public ListComprehensionConstraint(Operation operation)
	{
		super(operation, AccessConstraint.CompositeType.ListComprehension);
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof ListComprehensionConstraint))
			return false;

		final ListComprehensionConstraint constraint = (ListComprehensionConstraint) object;

		return super.equals(constraint);

	}
	public String toString()
	{
		return super.toString() + "*";
	}

	public ListComprehensionConstraint opposite()
	{
		if (this.operation == Operation.Add)
			return new ListComprehensionConstraint(Operation.Remove);
		return new ListComprehensionConstraint(Operation.Add);
	}

	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		if (this.operation == Operation.Add)
			return super.wrap(super.push(phase, constraints));

		final Constraints constraintsClone = (Constraints) constraints.clone();
		boolean correctlyTreated = false;

		while (!correctlyTreated && !constraintsClone.isEdgeConstraintsEmpty())
		{
			final EdgeConstraint peekedConstraint = constraintsClone.peekEdgeConstraint();
			if (!(peekedConstraint instanceof ListConstraint))
				break;

			final ListConstraint listConstraint = (ListConstraint) peekedConstraint;
			if (listConstraint.operation != AccessConstraint.Operation.Add)
				break;

			if (listConstraint.position == ListConstraint.Position.H)
				correctlyTreated = true;
			constraintsClone.popEdgeConstraint();
		}

		if (correctlyTreated || constraintsClone.isEdgeConstraintsEmpty())
			return super.wrap(constraintsClone);
		if (phase.isInstanceof(Phase.Slicing))
			return super.wrap();
		super.check(phase, Phase.SummaryGeneration);
		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, ListComprehensionConstraint topConstraint, int productionDepth)
	{
		if (topConstraint.operation == Operation.Add && this.cancels(topConstraint))
			return super.wrap(super.pop(constraints));
		super.check(phase, Phase.SummaryGeneration);
		return super.wrap(super.push(phase, constraints));
	}
}