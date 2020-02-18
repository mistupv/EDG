package eknife.edg.constraint;

public class SummaryConstraints extends Constraints
{	
	public static enum SummaryType { Exception, Return }
	
	private SummaryType summaryType = null;
	private SeekingConstraint seekingConstraint = null;
	
	public SummaryConstraints()
	{
		this(null,null);
	}
	public SummaryConstraints(SeekingConstraint seekingConstraint, SummaryType summaryType)
	{
		super();
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

	public boolean equals(Object object)
	{
		if (!(object instanceof SummaryConstraints))
			return false;
		if (!super.equals(object))
			return false;
		
		final SummaryConstraints summaryConstraints = (SummaryConstraints) object;
		
		if (this.summaryType != summaryConstraints.summaryType)
			return false;
		if (this.seekingConstraint != summaryConstraints.seekingConstraint)
			return false;
		
		return true;
	}
	public Object clone()
	{
		final SummaryConstraints summaryConstraints = new SummaryConstraints();

		summaryConstraints.stack.addAll(this.stack);
		summaryConstraints.seekingConstraint = this.seekingConstraint;
		summaryConstraints.summaryType = this.summaryType;
		
		return summaryConstraints;
	}
}
