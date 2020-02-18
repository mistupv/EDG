import edg.slicing.SlicingCriterion;
import eknife.EKnife;

public class FileTest
{
	public static void main(String[] args)
	{
		final String codebase = "/Users/Fenix/Desktop/Slicing/";
		final SlicingCriterion slicingCriterion = new SlicingCriterion("prueba.erl", 20, "C", 1);
		final String sourcePath = codebase + "BenchmarksPrueba/" + slicingCriterion.getArchive();
		final String outputErlPath = codebase + "output.erl";
		final String outputPdfPath = codebase + "output.pdf";

		args = new String[] {
			"-ln", "Erlang",
			"-ip", sourcePath,
			"-op", outputErlPath,
			"-ar", slicingCriterion.getArchive(),
			"-li", slicingCriterion.getLine() + "",
			"-na", slicingCriterion.getName(),
			"-oc", slicingCriterion.getOccurrence() + "",
			"-pdf", outputPdfPath,
//			"-omitarcs",
			"-allarcs",
//			"-controlFlow",
//			"-control",
//			"-value",
//			"-flow",
//			"-call",
//			"-input",
//			"-output",
//			"-summary",
		};
		EKnife.main(args);
	}
}