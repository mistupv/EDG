class tryCatchFinallyTest{
	public static void main(String[] args)
	{
		int a = 1;
		int b = 2;
		int c = 0;
		int d = 21;
		try
		{
			a = 9;
			b = 3;
			c = 9;
			int e = a + b;
		}
		catch(RuntimeException e)
		{
			c = 1;
			d = 14;									// SC: (18,d,1)
		}
		finally
		{
			d = 9;
		}

		int f = a + b;								// SC: (25,f,1)
		System.out.println(c);						// SC: (26,c,1)
		System.out.println(d);						// SC: (27,d,1)

	}
}