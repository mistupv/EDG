package eknife.edg.constraint;

import java.util.Stack;

public class Constraints
{
	public static enum SummaryType { Exception, Return }
	
	private SummaryType summaryType = null;
	protected final Stack<Constraint> stack = new Stack<Constraint>();
	private SeekingConstraint seekingConstraint = null;

	public Constraints()
	{
		this(null,null);
	}
	public Constraints(SeekingConstraint seekingConstraint, SummaryType summaryType)
	{
		this.seekingConstraint = seekingConstraint;
		this.summaryType = summaryType;
	}

	public void setSummaryType(SummaryType summaryType)
	{
		this.summaryType = summaryType;
	}
	public SummaryType getSummaryType()
	{
		return this.summaryType;
	}

	public boolean isSeekingConstraint()
	{
		return seekingConstraint != null;
	}
	public void setSeekingConstraint(SeekingConstraint constraint)
	{
		this.seekingConstraint = constraint;
	}
	public SeekingConstraint getSeekingConstraint()
	{
		return this.seekingConstraint;
	}

// Flag to know if we should cross the exceptionGetAll edges or not
private boolean crossExceptionGetAll = false;
public boolean getExceptionGetAll()
{
	return this.crossExceptionGetAll;
}
public void setExceptionGetAll(boolean state)
{
	this.crossExceptionGetAll = state;
}

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
		if (this.summaryType != constraints.summaryType)
			return false;
		if (!this.stack.equals(constraints.stack))
			return false;
		if (this.seekingConstraint != constraints.seekingConstraint)
			return false;
		if (this.crossExceptionGetAll != constraints.crossExceptionGetAll)
			return false;

		return true;
	}
	public Object clone()
	{
		final Constraints constraints = new Constraints();

		constraints.stack.addAll(this.stack);
		constraints.seekingConstraint = this.seekingConstraint;
		constraints.summaryType = this.summaryType;
		
		return constraints;
	}
	public String toString()
	{
		return this.stack.toString();
	}
}