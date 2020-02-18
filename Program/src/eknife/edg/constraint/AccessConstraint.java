package eknife.edg.constraint;

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
	public boolean letThrough(AccessConstraint constraint)
	{
		return false;
	}
	public boolean letThroughWithEmptyStack(boolean resolveSummary)
	{
		return true;
	}

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
}