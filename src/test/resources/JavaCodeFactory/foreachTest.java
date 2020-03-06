class foreachTest{
	public static void main(String[] args)
	{
		int sum = 0;
		Integer array[3] = {0,1,2};
		
		for (Integer i : array)			// SC: (7,array,1) (7,i,1)
			sum = sum + 1;

		System.out.println(sum);		// SC: (10,sum,1)
	}
}