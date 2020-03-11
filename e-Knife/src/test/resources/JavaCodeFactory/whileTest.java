class whileTest{
	public static void main(String[] args)
	{
		int x = 2;
		int y;
		while(x = 3 > 4)
			y = 1;
		
		System.out.println(x);	// SC: (9,x,1)
		System.out.println(y);	// SC: (10,y,1)
	}
}