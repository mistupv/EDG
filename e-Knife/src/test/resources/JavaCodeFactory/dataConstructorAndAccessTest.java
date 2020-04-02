class dataConstructorAndAccessTest{
	public static void main(String[] args)
	{
		int x;
		int y;

		int array[] = {(x = 1), (y = 2), 3};		// SC: (7,array,1)
		int z = x + y;								// SC: (8,z,1)

		int w = array[2];							// SC: (10,array,1) (10,w,1)
	}
}