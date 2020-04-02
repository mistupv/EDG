class forTest{
	public static void main(String[] args)
	{
		int total = 0;

		for (int i = 0; i < 2; i++)						// SC: (6,i,1) (6,i,2)
			total += i;

		System.out.println(total);

		int i;
		for (i = 0, total = 0; i < 3; i++, total++)
			total += i;

		System.out.println(i);							// SC: (15,i,1)
		System.out.println(total);						// SC: (16,total,1)

		int j = 0;
		for (int k = 0; (j = 2) < 2; k++)
			total += k;

		System.out.println(j);							// SC (22,j,1)
	}
}