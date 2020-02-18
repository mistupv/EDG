package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.traverser.EdgeTraverser.Phase;

public abstract class AccessConstraint extends Constraint
{
	public enum Operation { Add, Remove }
	public enum CompositeType { Tuple, List, Bin, BinElement, Record, Exception, Map, ExceptionArgument }

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
	
// TODO BORRAR
	public boolean letThrough(AccessConstraint constraint)
	{
		return false;
	}
	public boolean letThroughWithEmptyStack(Phase phase)
	{
		return true;
	}
// -------------
	public boolean equals(Object object)
	{
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
		toString += this.compositeType == CompositeType.Tuple ? "{}" :
					this.compositeType == CompositeType.List ? "[]" :
					this.compositeType == CompositeType.Bin ? "<<>>" :
					this.compositeType == CompositeType.BinElement ? ":" :
					this.compositeType == CompositeType.Record ? "Re" : // ADDED LINE (BY SERGIO)
					this.compositeType == CompositeType.Exception ? "Ex" : // ADDED LINE (BY SERGIO)
					this.compositeType == CompositeType.ExceptionArgument ? "ExArg" : // ADDED LINE (BY SERGIO)
					this.compositeType == CompositeType.Map ? "Map": // ADDED LINE (BY SERGIO)
					"";

		return toString;
	}

	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
	{
		if (constraintsStack.isSeekingConstraint())
			return new LinkedList<Constraints>();
	
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();

		if (phase == Phase.Summary || this.operation == Operation.Add)
			constraintsStack.push(this);
		constraintsStacks.add(constraintsStack);

		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, AccessConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();

		if (topConstraint.operation == AccessConstraint.Operation.Add && this.cancels(topConstraint))
			constraintsStack.pop();
		else if (topConstraint.operation == AccessConstraint.Operation.Add && this.operation == AccessConstraint.Operation.Add) 
			constraintsStack.push(this);
		else
		{
			if (phase != Phase.Summary)
				return new LinkedList<Constraints>();
			if (topConstraint.operation != AccessConstraint.Operation.Remove || this.operation != AccessConstraint.Operation.Remove)
				return new LinkedList<Constraints>();

			constraintsStack.push(this);
		}

		constraintsStacks.add(constraintsStack);

		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SeekingConstraint topConstraint, int productionDepth)
	{
		if (phase != Phase.Summary)
			return new LinkedList<Constraints>();
		if (topConstraint.operation != SeekingConstraint.Operation.Remove)
			return new LinkedList<Constraints>();

		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();

		constraintsStack.push(this);
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
		if (this.operation != AccessConstraint.Operation.Add)
			return new LinkedList<Constraints>();

		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();

		constraintsStack.push(this);
		constraintsStacks.add(constraintsStack);

		return constraintsStacks;
	}
}