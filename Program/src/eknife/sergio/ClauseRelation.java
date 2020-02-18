package eknife.sergio;

public class ClauseRelation {

	boolean related;
	boolean total;
	
	public ClauseRelation(boolean related, boolean total)
	{
		this.related = related;
		this.total = total;
	}
	
	public void setRelated(boolean related)
	{
		this.related = related;
	}
	public void setTotal(boolean total)
	{
		this.total = total;
	}
	public boolean getRelated()
	{
		return this.related;
	}
	public boolean getTotal()
	{
		return this.total;
	}
}
