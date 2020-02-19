package upv.slicing.misc.math;

public abstract class Permutator
{
	protected int size;
	protected int limit;
	protected Integer[] numbers;

	public Permutator(int size, int limit)
	{
		this.size = size;
		this.limit = limit;

		this.restart();
	}

	public abstract void restart();

	public abstract Integer[] first();
	public abstract Integer[] previous();
	public abstract Integer[] current();
	public abstract Integer[] next();
	public abstract Integer[] last();
}