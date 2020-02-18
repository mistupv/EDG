package edg.slicing;

public enum Phase
{
	Construction,
	SummaryGeneration(Construction),

	Slicing,
	Input(Slicing), Output(Slicing);

	private final Phase parent;

	private Phase()
	{
		this(null);
	}
	private Phase(Phase parent)
	{
		this.parent = parent;
	}

	public boolean isInstanceof(Phase phase)
	{
		Phase ancestor = this;

		do
		{
			if (ancestor == phase)
				return true;
			ancestor = ancestor.parent;
		}
		while (ancestor != null);

		return false;
	}
}