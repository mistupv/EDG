class instanceOfTest{
	public static void main(String[] args)
	{
		B b = new B(1,2);
		boolean bool = (b instanceof A);			// SC: (5,bool,1) (5,b,1) ¿(5,A,1)?
	}
}

class A
{
	int x;

	public A(int a)
	{
		x = a;
	}
}



class B extends A
{
	int y;
	
	public B(int a, int b)
	{
		super(a);
		y = b;
	}
}