package upv.slicing.misc.methodology;

public class DinamicProgramming
{
	/****************************************************************/
	/************************* Edit distance ************************/
	/****************************************************************/
	public static int getEditDistance(String str1, String str2)
	{
		return DinamicProgramming.getLevenshteinDistance(str1.toCharArray(), str2.toCharArray(), 1, 1, 1);
	}
	public static int getEditDistance(String str1, String str2, int deletionCost, int insertionCost, int substitutionCost)
	{
		return DinamicProgramming.getLevenshteinDistance(str1.toCharArray(), str2.toCharArray(), deletionCost, insertionCost, substitutionCost);
	}
	private static int getLevenshteinDistance(char[] str1, char[] str2, int deletionCost, int insertionCost, int substitutionCost)
	{
		final int[][] distance = new int[str1.length + 1][str2.length + 1];

		for (int i = 0; i <= str1.length; i++)
			distance[i][0] = i * deletionCost;
		for (int j = 0; j <= str2.length; j++)
			distance[0][j] = j * insertionCost;

		for (int i = 1; i <= str1.length; i++)
			for (int j = 1; j <= str2.length; j++)
			{
				final int deletion = distance[i - 1][j] + deletionCost;
				final int insertion = distance[i][j - 1] + insertionCost;
				final int substitution = distance[i - 1][j - 1] + (str1[i - 1] == str2[j - 1] ? 0 : substitutionCost);

				distance[i][j] = Math.min(deletion, Math.min(insertion, substitution));
			}

		return distance[str1.length][str2.length];
	}
}