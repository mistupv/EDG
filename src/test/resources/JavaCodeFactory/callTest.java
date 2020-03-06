public class callTest{
	public static void main(String[] args)
	{
		int x = 2;
		int y = 3;
		int z = f((x = 1), (y = 7));	// SC: (6,z,1)
		int w = x + y;				// SC: (7,w,1)

		A a = new A(1);
		int b = a.getX();			// SC: (10,a,1)

		int c = g(1, f((x = 1), (y = 3)));
		int d = x + y + c;			// SC: (13,d,1)
	}
	public static int f(int a, int b)
	{
		return a + b;
	}
	public static int g(int a, int b)
	{
		return a;
	}
}

class A
{
	int x;

	public A(int a)
	{
		x = a;
	}

	public int getX()
	{
		return x;
	}
}