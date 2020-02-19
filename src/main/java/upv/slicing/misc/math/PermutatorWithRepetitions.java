package upv.slicing.misc.math;

public class PermutatorWithRepetitions extends Permutator
{
	public static int LEFTWARDS = 0;
	public static int RIGHTWARDS = 1;

	private int direction;

	public PermutatorWithRepetitions(int size, int limit)
	{
		this(size, limit, PermutatorWithRepetitions.RIGHTWARDS);
	}
	public PermutatorWithRepetitions(int size, int limit, int direction)
	{
		super(size, limit);

		this.direction = direction;
		this.restart();
	}

	private int firstPosition()
	{
		if (this.direction == PermutatorWithRepetitions.LEFTWARDS)
			return 0;
		if (this.direction == PermutatorWithRepetitions.RIGHTWARDS)
			return super.size - 1;
		return 0;
	}
	private int nextPosition(int position)
	{
		if (this.direction == PermutatorWithRepetitions.LEFTWARDS)
			return position + 1;
		if (this.direction == PermutatorWithRepetitions.RIGHTWARDS)
			return position - 1;
		return position + 1;
	}
	private boolean lastPosition(int position)
	{
		if (this.direction == PermutatorWithRepetitions.LEFTWARDS)
			return position >= super.size;
		if (this.direction == PermutatorWithRepetitions.RIGHTWARDS)
			return position < 0;
		return position >= super.size;
	}

	public void restart()
	{
		int size = super.size - 1;

		super.numbers = new Integer[super.size];
		for (int position = 0; position <= size; position++)
			super.numbers[position] = 0;
		super.numbers[this.firstPosition()] = -1;
	}

	public Integer[] first()
	{
		for (int position = 0; position < super.size; position++)
			super.numbers[position] = 0;

		return super.numbers;
	}
	public Integer[] previous()
	{
		int position = this.firstPosition();

		super.numbers[position]--;
		while (super.numbers[position] < 0)
		{
			super.numbers[position] = super.limit - 1;
			position = this.nextPosition(position);
			if (this.lastPosition(position))
				return null;
			super.numbers[position]--;
		}

		return super.numbers;
	}
	public Integer[] current()
	{
		if (super.numbers[this.firstPosition()] == -1)
			super.numbers[this.firstPosition()] = 0;

		return super.numbers;
	}
	public Integer[] next()
	{
		int position = this.firstPosition();

		super.numbers[position]++;
		while (super.numbers[position] == super.limit + 1)
		{
			super.numbers[position] = 0;
			position = this.nextPosition(position);
			if (this.lastPosition(position))
				return null;
			super.numbers[position]++;
		}

		return super.numbers;
	}
	public Integer[] last()
	{
		int limit = super.limit - 1;

		for (int position = 0; position < super.size; position++)
			super.numbers[position] = limit;

		return super.numbers;
	}
}