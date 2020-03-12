class fieldAccessTest{
	public static void main(String[] args)
	{
		A a = new A(1,2);
		a.y = a.x;			// SC: (5,a,1) (5,a,2) (5,y,1) (5,x,1)
	}

	private static class A {
		public int x;
		public int y;

		public A(int a, int b)
		{
			x = a;
			y = b;
		}
	}
}



