class ternaryTest{
	public static void main(String[] args)
	{
		int a,b;
		int e = 15;
		int c = ((a = 3) == (b = 3)) ? a + (e = 2) : e - a;		// SC: (5,c,1)

		int d = a + b;											// SC: (7,d,1)
		int f = e;												// SC: (8,e,1)
	}
}