package upv.slicing.edg.slicing;

import upv.slicing.edg.graph.Node;

public class SlicingCriterion
{
	protected final String file;
	protected final int line;
	protected final String name;
	protected final int occurrence;

	public SlicingCriterion(String file, int line, String name)
	{
		this(file, line, name, 1);
	}
	public SlicingCriterion(String file, int line, String name, int occurrence)
	{
		this.file = file;
		this.line = line;
		this.name = name;
		this.occurrence = occurrence;
	}

	public String getFile()
	{
		return this.file;
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

		if (!this.file.equals(sc.file))
			return false;
		if (this.line != sc.line)
			return false;
		if (!this.name.equals(sc.name))
			return false;
		return this.occurrence == sc.occurrence;
	}
	public String toString()
	{
		return "[" + this.file + ", " + this.line + ", " + this.name + ", " + this.occurrence + "]";
	}

	public boolean matchesNode(Node node) {
		return node != null
				&& node.getInfo() != null
				&& line == node.getInfo().getLine()
				&& name.equals(node.getName())
				&& file.equals(node.getInfo().getFile());
	}
}
