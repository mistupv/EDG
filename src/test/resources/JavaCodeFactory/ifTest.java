class ifTest{
	public static void main(String[] args)
	{
		int x = 2;
		int y;
		int z = 8;

		if ( x = 1 < 3)
		{
			y = 5;
		}
		else
		{
			z = 6;
			y = 3;
		}

		System.out.println(x);	// SC: (18,x,1)
		System.out.println(y);	// SC: (19,y,1)
		System.out.println(z);	// SC: (20,z,1)
	}
}