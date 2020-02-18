package eknife.edg.constraint;

import java.util.Stack;

public class Constraints extends Stack<Constraint>
{
	private static final long serialVersionUID = 1L;
	private boolean isExceptionSummary = false;
	
	public Constraints()
	{
		super();
	}
	public Constraints(boolean exceptionSummary)
	{
		super();
		this.isExceptionSummary = exceptionSummary;
	}
	public void setExceptionSummary(boolean state)
	{
		this.isExceptionSummary = state;
	}
	public boolean getExceptionSummary()
	{
		return this.isExceptionSummary;
	}
	public boolean equals(Object object)
	{
		if (!(object instanceof Constraints))
			return false;
		if (!super.equals(object))
			return false;
		
		final Constraints constraints = (Constraints) object;

		if (this.isExceptionSummary != constraints.getExceptionSummary())
			return false;
		return true;
	}
}