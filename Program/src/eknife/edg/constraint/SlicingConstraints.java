package eknife.edg.constraint;

public class SlicingConstraints extends Constraints
{
	public boolean equals(Object object)
	{
		if (!(object instanceof SlicingConstraints))
			return false;

		final SlicingConstraints slicingConstraints = (SlicingConstraints) object;
		if (!this.stack.equals(slicingConstraints.stack))
			return false;

		return true;
	}
	public Object clone()
	{
		final SlicingConstraints slicingConstraints = new SlicingConstraints();

		slicingConstraints.stack.addAll(this.stack);
		
		return slicingConstraints;
	}
}
