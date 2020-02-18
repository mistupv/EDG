import edg.PdfFactory;
import edg.graph.EDG;
import eknife.EDGFactory;
import eknife.EKnife.Language;
import eknife.config.Config;

public class JavaTest
{
	public static void main(String[] args)
	{
		int num1 = 0, num2 = 1;

		final Config config = Config.getConfig();
		final String sourcePath = config.getSourcesPath() + "JavaTest.java";
		final EDG edg = EDGFactory.createEDG(Language.Java, sourcePath);
		final String outputPath = "/Users/Fenix/Desktop/output.pdf";

		PdfFactory.createPdf(outputPath, edg);
//		CodeFactory.createCode(Language.Java, outputPath, edg);
	}
}