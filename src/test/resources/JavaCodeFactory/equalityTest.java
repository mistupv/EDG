class equialityTest{
	public static void main(String[] args)
	{
		int a, b; 
		int c = (a = 3) + (b = 5);
		int d = b + a;			// SC: (6,d,1) (6,b,1)

		int e = b = 20;			// SC: (8,e,1) (8,b,1)
	}
}