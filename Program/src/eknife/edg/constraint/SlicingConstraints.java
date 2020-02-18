package eknife.edg.constraint;

public class SlicingConstraints extends Constraints
{
//	private boolean crossExceptionGetAll = false;
	
/*	public boolean getExceptionGetAll()
	{
		return this.crossExceptionGetAll;
	}
	public void setExceptionGetAll(boolean state)
	{
		this.crossExceptionGetAll = state;
	}
*/
	
	public boolean equals(Object object)
	{
		if (!(object instanceof SlicingConstraints))
			return false;
		if (!super.equals(object))
			return false;
		final SlicingConstraints slicingConstraints = (SlicingConstraints) object;
		if (!this.stack.equals(slicingConstraints.stack))
			return false;
//		if (this.crossExceptionGetAll != slicingConstraints.crossExceptionGetAll)
//			return false;

		return true;
	}
	public Object clone()
	{
		final SlicingConstraints slicingConstraints = new SlicingConstraints();

		slicingConstraints.stack.addAll(this.stack);
//		slicingConstraints.crossExceptionGetAll = this.crossExceptionGetAll;
		
		return slicingConstraints;
	}
}
