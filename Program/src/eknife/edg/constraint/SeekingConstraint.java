package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class SeekingConstraint  extends Constraint
{
	public enum Operation { Add, Remove }
	public enum CompositeType { Exception, GetAll }
	
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
	
	public SeekingConstraint opposite() {
		return null;
	}
	public boolean cancels(SeekingConstraint constraint)
	{
		return this.equals(constraint.opposite());
	}
	public boolean letThrough(SeekingConstraint lastEdgeConstraint)
	{
		return false;
	}
	public boolean letThroughWithEmptyStack(Phase phase)
	{
		return true;
	}
	public boolean match(SeekingConstraint seekingConstraint)
	{
		return false;
	}
	public boolean equals(Object object)
	{
		if (!(object instanceof SeekingConstraint))
			return false;

		final SeekingConstraint constraint = (SeekingConstraint) object;

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
		toString += this.compositeType == CompositeType.Exception ? "Ex" : 
					"";

		return toString;
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		if (this.operation == Operation.Add)
			constraintsStack.push(this);
//		else if (this.operation == Operation.GetAll && phase == Phase.Slicing)
//			((SlicingConstraints)constraintsStack).setExceptionGetAll(true);
		else if (phase == Phase.Summary)
		{
			final SummaryConstraints summaryConstraintsStack = (SummaryConstraints) constraintsStack;
			if (summaryConstraintsStack.isSeekingConstraint())
			{
				if (!this.match(summaryConstraintsStack.getSeekingConstraint()))
					return new LinkedList<Constraints>();
			}
			else
				if (this.operation == null)
					summaryConstraintsStack.setSeekingConstraint(this);
				else
					summaryConstraintsStack.push(this);
		}
		else
			return new LinkedList<Constraints>();
		
		constraintsStacks.add(constraintsStack);

		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, AccessConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase,constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SeekingConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
			
		if (topConstraint.operation == Operation.Add && this.cancels(topConstraint))
		{
			constraintsStack.pop();
			//constraintsStack.setSeekingConstraint(null);
		}
		else if (topConstraint.operation == Operation.Add && this.operation == null && this.letThrough(topConstraint))
			;
//		else if (this.operation == Operation.GetAll && phase == Phase.Slicing)
//			((SlicingConstraints)constraintsStack).setExceptionGetAll(true);
		else
			return new LinkedList<Constraints>();

		constraintsStacks.add(constraintsStack);

		return constraintsStacks;

	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, StarConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SummaryConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, UnresolvableConstraint topConstraint, int productionDepth)
	{
//		if (this.operation == Operation.GetAll && phase == Phase.Slicing)
//			((SlicingConstraints)constraintsStack).setExceptionGetAll(true);
		
		if (this.operation != SeekingConstraint.Operation.Add)
			return new LinkedList<Constraints>();

		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();

		constraintsStack.push(this);
		//constraintsStack.setSeekingConstraint(this);
		constraintsStacks.add(constraintsStack);

		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, EmptyConstraint topConstraint, int productionDepth)
	{
//		if (this.operation == Operation.GetAll && phase == Phase.Slicing)
//			((SlicingConstraints)constraintsStack).setExceptionGetAll(true);
		if (this.operation != null)
			return super.resolve(phase, constraintsStack, topConstraint, productionDepth);
		
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		constraintsStacks.add(constraintsStack);
		
		return constraintsStacks;
	}
}