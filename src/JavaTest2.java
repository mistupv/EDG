public class JavaTest2
{
//	public static int x = 1;
//	public static int y = 2;
/*
	public static void main(String[] args)
	{
//		y = 0;
//		JavaTest2.fun1();
//		System.out.print(x);

		int[][] numbers = { {1, 2}, {3, 5}, {4} };
		int x = 1;
		int[] num1 = JavaTest2.fun(numbers[x]);
		int[] num2 = numbers[x];
		int n = num1[0];
	}
	private static int[] fun(int[] num)
	{
		int n = num[0];
		int[] numbers = {1, 2};
		int[][] num2 = { num };
		return num2[0];
	}
*/
//	public static void main(String[] args)
//	{
//		JavaTest2.function();
//	}
	public static void main()
	{
		JavaTest2.function(1, 2);
	}
	public static void function(int a, int b)
	{
		int x = 0;
		int[][] numbers = { { 2 }, { 40, 39 } };
		b = 6;
		while (JavaTest2.foo(a, numbers) > x) {
			x = x + b;
		}
		Meaning.life(x);
	}
	public static int foo(int a, int[][] numbers)
	{
		return numbers[0 + 1][0];
	}
/*
	public static void fun1()
	{
		y = 0;
		while (y < 10)
		{
			while (y < 10)
				y = y + 1;
			y = y + 1;
		}
		x = JavaTest2.fun2();
//		while (y < 10)
//			y = JavaTest2.fun2() + 1;
//		x = JavaTest2.fun2();
	}
*/
/*
	public static int fun2()
	{
		return y;
	}
*/

/*
	public static int calculateStatistics(double[] statistics, double[][] studentsInfo, int age)
	{
		statistics[0] = 0.0;
		statistics[1] = 1.0;
		statistics[2] = Double.POSITIVE_INFINITY;
		statistics[3] = Double.NEGATIVE_INFINITY;

		int studentIndex = 0;
		int count = 0;
		while (studentIndex < studentsInfo.length)
		{
			double[] studentInfo = studentsInfo[studentIndex];
			double reliability = getReliability(studentInfo);
			studentIndex = studentIndex + 1;

			if (age != getInfo(studentInfo, reliability)[0])
				continue;

			statistics[0] = statistics[0] + studentInfo[1];
			statistics[1] = statistics[1] * studentInfo[1];
			statistics[2] = Math.min(statistics[2], studentInfo[1]);
			statistics[3] = Math.max(statistics[3], studentInfo[1]);
			count = count + 1;
		}

		return count;
	}
	public static int[] getInfo(double[] studentInfo, double reliability)
	{
		int age = (int) studentInfo[0];

		...

		return new int[] { age, ... };
	}
	public static double getReliability(double[] studentInfo)
	{
		return 0.0;
	}
*/
}