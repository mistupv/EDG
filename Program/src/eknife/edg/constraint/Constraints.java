package eknife.edg.constraint;

import java.util.Stack;

import eknife.edg.traverser.EdgeTraverser.Phase;

public class Constraints
{
	public static Constraints getConstraints(Phase phase)
	{
		switch (phase)
		{
			case Slicing:
				return new SlicingConstraints();
			case Summary:
				return new SummaryConstraints();
			default:
				throw new RuntimeException("Phase not contemplated: " + phase);
		}
	}

	protected final Stack<Constraint> stack = new Stack<Constraint>();

	public Constraint push(Constraint constraint)
	{
		return this.stack.push(constraint);
	}
	public Constraint pop()
	{
		return this.stack.pop();
	}
	public Constraint peek()
	{
		return this.stack.peek();
	}
	public Constraint firstElement()
	{
		return this.stack.firstElement();
	}
	public boolean isEmpty()
	{
		return this.stack.isEmpty();
	}
	public void clear()
	{
		this.stack.clear();
	}
	public int size()
	{
		return this.stack.size();
	}
	public Object[] toArray()
	{
		return this.stack.toArray();
	}
	public boolean equals(Object object)
	{
		if (!(object instanceof Constraints))
			return false;

		final Constraints constraints = (Constraints) object;
		if (!this.stack.equals(constraints.stack))
			return false;

		return true;
	}
	public Object clone()
	{
		final Constraints constraints = new Constraints();

		constraints.stack.addAll(this.stack);
		
		return constraints;
	}
	public String toString()
	{
		return this.stack.toString();
	}
}