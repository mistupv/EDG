import java.io.File;
import java.util.Hashtable;
import java.util.Map;

import edg.slicing.SlicingCriterion;
import eknife.EKnife;

public class ErlangTest
{
	public static void main(String[] args)
	{
		final String codebase = "/Users/Fenix/Desktop/Slicing/";
		final Map<Integer, SlicingCriterion> slicingCriteria = new Hashtable<Integer, SlicingCriterion>();
//		slicingCriteria.put(1, new SlicingCriterion("prueba.erl", 20, "C", 1));
		slicingCriteria.put(1, new SlicingCriterion("b1.erl", 20, "C", 1));
		slicingCriteria.put(2, new SlicingCriterion("b2.erl", 21, "C", 1));
		slicingCriteria.put(3, new SlicingCriterion("b3.erl", 21, "C", 1));
		slicingCriteria.put(4, new SlicingCriterion("b4.erl", 25, "Abb", 1));
		slicingCriteria.put(5, new SlicingCriterion("b5.erl", 24, "C", 1));
		slicingCriteria.put(6, new SlicingCriterion("b6.erl", 24, "C", 1));
		slicingCriteria.put(7, new SlicingCriterion("b7.erl", 25, "D", 1));
		slicingCriteria.put(8, new SlicingCriterion("b8.erl", 27, "C", 1));
		slicingCriteria.put(9, new SlicingCriterion("b9.erl", 28, "D", 1));
		slicingCriteria.put(10, new SlicingCriterion("b10.erl", 22, "Deposits", 1));
		slicingCriteria.put(11, new SlicingCriterion("b11.erl", 52, "A", 1));
		slicingCriteria.put(12, new SlicingCriterion("b12.erl", 58, "Shown", 1));
		slicingCriteria.put(13, new SlicingCriterion("b13.erl", 28, "DB", 1));
		slicingCriteria.put(14, new SlicingCriterion("b14.erl", 29, "BS", 1));
		slicingCriteria.put(15, new SlicingCriterion("b15.erl", 81, "A", 1));
		slicingCriteria.put(16, new SlicingCriterion("b16.erl", 31, "NewI", 1));
		slicingCriteria.put(17, new SlicingCriterion("b17.erl", 33, "V", 1));
		slicingCriteria.put(18, new SlicingCriterion("b18.erl", 34, "W", 1));
		slicingCriteria.put(19, new SlicingCriterion("b19.erl", 35, "Z", 1));
		slicingCriteria.put(20, new SlicingCriterion("b20.erl", 49, "Year", 1));

		for (int benchmarkIndex = 1; benchmarkIndex <= 20; benchmarkIndex++)
		{
			if (benchmarkIndex != 20)
				continue;

System.out.print("Benchmark " + benchmarkIndex);
final long time1 = System.currentTimeMillis();
			final SlicingCriterion slicingCriterion = slicingCriteria.get(benchmarkIndex);
			final String path = codebase + "BenchmarksPrueba/";
			final String benchmark = "B";

			final String sourcePath = path + benchmark + benchmarkIndex + File.separator + slicingCriterion.getArchive();
			final String outputErlPath = codebase + "output.erl";
			final String outputDotPath = codebase + "output.dot";
			final String outputPdfPath = codebase + "output.pdf";

			args = new String[] {
				"-ln", "Erlang",
				"-ip", sourcePath,
				"-op", outputErlPath,
				"-ar", slicingCriterion.getArchive(),
				"-li", slicingCriterion.getLine() + "",
				"-na", slicingCriterion.getName(),
				"-oc", slicingCriterion.getOccurrence() + "",
//				"-dot", outputDotPath,
				"-pdf", outputPdfPath,
//				"-omitarcs",
				"-allarcs",
//				"-controlFlow",
//				"-control",
//				"-value",
//				"-flow",
//				"-call",
//				"-input",
//				"-output",
//				"-summary",
			};
			EKnife.main(args);
final long time2 = System.currentTimeMillis();
final long time = time2 - time1;
System.out.println(" => " + time + " milliseconds");
		}
	}
}