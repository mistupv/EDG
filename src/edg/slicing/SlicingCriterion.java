package edg.slicing;

public class SlicingCriterion
{
	protected final String archive;
	protected final int line;
	protected final String name;
	protected final int occurrence;

	public SlicingCriterion(String archive, int line, String name)
	{
		this(archive, line, name, 1);
	}
	public SlicingCriterion(String archive, int line, String name, int occurrence)
	{
		this.archive = archive;
		this.line = line;
		this.name = name;
		this.occurrence = occurrence;
	}

	public String getArchive()
	{
		return this.archive;
	}
	public int getLine()
	{
		return this.line;
	}
	public String getName()
	{
		return this.name;
	}
	public int getOccurrence()
	{
		return this.occurrence;
	}

	public boolean equals(Object obj)
	{
		if (obj == this)
			return true;
		if (!(obj instanceof SlicingCriterion))
			return false;

		final SlicingCriterion sc = (SlicingCriterion) obj;

		if (!this.archive.equals(sc.archive))
			return false;
		if (this.line != sc.line)
			return false;
		if (!this.name.equals(sc.name))
			return false;
		if (this.occurrence != sc.occurrence)
			return false;
		return true;
	}
	public String toString()
	{
		return "[" + this.archive + ", " + this.line + ", " + this.name + ", " + this.occurrence + "]";
	}
}