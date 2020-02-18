package misc.math;

public class PermutatorWithoutRepetitions extends Permutator
{
	public PermutatorWithoutRepetitions(int size, int limit)
	{
		super(size, limit);
	}

	public void restart()
	{
		int size = super.size - 1;

		super.numbers = new Integer[super.size];
		for (int position = 0; position <= size; position++)
			super.numbers[position] = position;
		super.numbers[super.size - 1] = super.numbers[super.size - 1] - 1;
	}

	public Integer[] first()
	{
		for (int position = 0; position < super.size; position++)
			super.numbers[position] = position;

		return super.numbers;
	}
	public Integer[] previous()
	{
		if (true)
			throw new RuntimeException("No implementado, pedirselo a David");
		@SuppressWarnings("unused")
		int position = super.size - 1;

		super.numbers[position]--;
		while (super.numbers[position] < 0)
		{
			super.numbers[position] = super.limit - 1;
			position--;
			if (position < 0)
				return null;
			super.numbers[position]--;
		}

		return super.numbers;
	}
	public Integer[] current()
	{
		if (super.numbers[super.size - 1] == super.size - 2)
			super.numbers[super.size - 1] = super.size - 1;

		return super.numbers;
	}
	public Integer[] next()
	{
		int size = super.size - 1;
		int position = size;

		if (super.numbers[0] == super.limit - size)
			return null;

		while (super.numbers[position] == super.limit - (size - position))
			position--;
		super.numbers[position]++;
		for (; position < size; position++)
			super.numbers[position + 1] = super.numbers[position] + 1;

		return super.numbers;
	}
	public Integer[] last()
	{
		int limite = super.limit - 1;

		for (int posicion = 0; posicion < super.size; posicion++)
			super.numbers[posicion] = limite - posicion;

		return super.numbers;
	}
}