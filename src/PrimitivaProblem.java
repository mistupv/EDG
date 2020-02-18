import java.util.Random;

import misc.Misc;
import misc.math.PermutatorWithoutRepetition;

public class PrimitivaProblem
{
	public static void main(String[] args)
	{
		final int size = 6;
		final int limit = 50;
		final int empiricTimes = 10000000;
		final int decimals = 15;

		System.out.println("Teorico");
		PrimitivaProblem.theoric(size, limit, decimals);
		System.out.println();
		System.out.println("Empirico con " + empiricTimes + " experimentos");
		PrimitivaProblem.empiric(size, limit, empiricTimes, decimals);
		System.out.println();
		System.out.println("Fuerza bruta");
		PrimitivaProblem.bruteForce(size, limit, decimals);
	}
	private static void theoric(int size, int limit, int decimals)
	{
		double variable = 1;
		double result = 0;
		int sign = 1;

		for (int variableIndex = 0; variableIndex < size; variableIndex++)
		{
			final int pascalTriangleNumber = PrimitivaProblem.pascalTriangle(limit, variableIndex + 1);

			variable *= Math.pow(1.0 * (size - variableIndex) / (limit - variableIndex), 3);
			result += sign * pascalTriangleNumber * variable;
			sign *= -1;
		}

		System.out.println(Misc.round(100.0 * result, decimals) + "%");
	}
	private static int pascalTriangle(int row, int column)
	{
		int[] previousRow = new int[1];

		for (int rowIndex = 1; rowIndex <= row + 1; rowIndex++)
		{
			final int maxLength = Math.min(rowIndex, column + 1);
			final int[] currentRow = new int[maxLength];

			for (int columnIndex = 0; columnIndex < maxLength; columnIndex++)
			{
				if (columnIndex == 0 || columnIndex == (rowIndex - 1))
					currentRow[columnIndex] = 1;
				else
					currentRow[columnIndex] = previousRow[columnIndex] + previousRow[columnIndex - 1];
			}
			previousRow = currentRow;
		}

		return previousRow[column];
	}
	private static void empiric(int size, int limit, int times, int decimals)
	{
		int hits = 0;

		System.out.println(Misc.getCurrentTime());
		for (int i = 0; i < times; i++)
		{
			final Integer[] combination1 = PrimitivaProblem.getCombination(size, limit);
			final Integer[] combination2 = PrimitivaProblem.getCombination(size, limit);
			final Integer[] combination3 = PrimitivaProblem.getCombination(size, limit);

			combinations:
			for (int combination1Number : combination1)
				for (int combination2Number : combination2)
					for (int combination3Number : combination3)
						if (combination1Number == combination2Number && combination2Number == combination3Number)
						{
							hits++;
							break combinations;
						}
		}
		System.out.println(Misc.getCurrentTime());
		System.out.println(hits + " / " + times + " => " + Misc.round(100.0 * hits / times, decimals) + "%");
	}
	private static Integer[] getCombination(int size, int limit)
	{
		Integer[] numbers = new Integer[size];
		Random random = new Random();

		randoms:
		for (int numberIndex = 0; numberIndex < numbers.length; numberIndex++)
		{
			final int number = random.nextInt(limit);

			for (int prevNumberIndex = 0; prevNumberIndex < numberIndex; prevNumberIndex++)
				if (numbers[prevNumberIndex] == number)
				{
					numberIndex--;
					continue randoms;
				}
			numbers[numberIndex] = number;
		}

		return numbers;
	}
	private static void bruteForce(int size, int limit, int decimals)
	{
		Integer[] combination1;
		Integer[] combination2;
		Integer[] combination3;
		int hits = 0;
		int times = 0;

		System.out.println(Misc.getCurrentTime());
		final PermutatorWithoutRepetition pwr1 = new PermutatorWithoutRepetition(size, limit - 1);
		while ((combination1 = pwr1.next()) != null)
		{
			final PermutatorWithoutRepetition pwr2 = new PermutatorWithoutRepetition(size, limit - 1);
			while ((combination2 = pwr2.next()) != null)
			{
				final PermutatorWithoutRepetition pwr3 = new PermutatorWithoutRepetition(size, limit - 1);
				while ((combination3 = pwr3.next()) != null)
				{
					combinations:
					for (int combination1Number : combination1)
						for (int combination2Number : combination2)
							for (int combination3Number : combination3)
								if (combination1Number == combination2Number && combination2Number == combination3Number)
								{
									hits++;
									break combinations;
								}

					times++;
				}
			}
		}
		System.out.println(Misc.getCurrentTime());
		System.out.println(hits + " / " + times + " => " + Misc.round(100.0 * hits / times, decimals) + "%");
	}

	private static void teorico2(int size, int limit)
	{
		final double x = Math.pow(1.0 * size / limit, 3);
		final double y = x * Math.pow(1.0 * (size - 1) / (limit - 1), 3);
		final double z = y * Math.pow(1.0 * (size - 2) / (limit - 2), 3);
		final double result = 5 * x - 10 * y + 10 * z;

		final int cof1 = PrimitivaProblem.pascalTriangle(limit, 1);
		final int cof2 = PrimitivaProblem.pascalTriangle(limit, 2);
		final int cof3 = PrimitivaProblem.pascalTriangle(limit, 3);

		System.out.println(Misc.round(100.0 * result, 3) + "%");
	}
	private static int pascalTriangle2(int row, int column)
	{
		int numerator = 1;
		int denominator1 = 1;
		int denominator2 = 1;

		for (int i = 2; i <= row; i++)
			numerator *= i;
		for (int i = 2; i <= column; i++)
			denominator1 *= i;
		for (int i = 2; i <= row - column; i++)
			denominator2 *= i;

		return numerator / (denominator1 * denominator2);
	}
	private static String show(Integer[] perm)
	{
		String value = "";

		for (Integer num : perm)
			value += num + ", ";
		if (!value.isEmpty())
			value = value.substring(0, value.length() - 2);

		return value;
	}
}