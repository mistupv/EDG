class switchTest{
	public static void main(String[] args)
	{
		int a, b, c; int d = 9;
		switch(f(a = 4, b = 3))
		{
			case 1:
			case 2:
			case 3:
				c = 9;
				break;
			default:
				c = 7;
				d = 2;
				break;
		}

		System.out.println(a);			// SC: (18,a,1)
		System.out.println(d);			// SC: (19,d,1)
		System.out.println(c);			// SC: (20,c,1)

		d = a + b;
		System.out.println(d);			// SC: (23,d,1)
	}

	public static int f(int a, int b)
	{
		return a + b;
	}
}