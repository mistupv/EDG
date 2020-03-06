class operationTest{
	public static void main(String[] args)
	{
		int a = 1;
		int b = 7;
		int c = 0;
		
		int d = a + b * c + (a = 2) - (c = 3);		// SC: (8,d,1) (8,a,1) (8,c,2)

		int e = a + c;								// SC: (10,e,1)
	}
}