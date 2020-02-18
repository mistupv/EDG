import java.io.File;

import edg.PdfFactory;
import edg.graph.EDG;
import eknife.CodeFactory;
import eknife.EDGFactory;
import eknife.EKnife.Language;

public class ErlangTest
{
	public static void main(String[] args)
	{
		final String path = "/Users/Fenix/Desktop/BenchmarksPrueba/";
		final String benchmark = "B1";
		final String sourcePath = path + benchmark + File.separator + benchmark.toLowerCase() + ".erl";
		final EDG edg = EDGFactory.createEDG(Language.Erlang, sourcePath);
		final String outputPath = "/Users/Fenix/Desktop/output.pdf";

		PdfFactory.createPdf(outputPath, edg);
//		CodeFactory.createCode(Language.Erlang, outputPath, edg);
	}
}