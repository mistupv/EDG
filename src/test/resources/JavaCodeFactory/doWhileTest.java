class doWhileTest{
	public static void main(String[] args)
	{
		int x = 2;
		int y;
		do
			y = 1;
		while(x = 3 > 4);

		System.out.println(x);	// SC: (10,x,1)
		System.out.println(y);	// SC: (11,y,1)
	}
}