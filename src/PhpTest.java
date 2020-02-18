import java.io.File;

import edg.PdfFactory;
import edg.graph.EDG;
import eknife.CodeFactory;
import eknife.EDGFactory;
import eknife.EKnife.Language;

public class PhpTest
{
	public static void main(String[] args)
	{
		final String sourcePath = "/Library/WebServer/Documents/Alerts/alerts/src/logic/Alert.php";
//		final String sourcePath = "/Library/WebServer/Documents/Alerts/alerts/src/logic/User.php";
		final String codebase = "/Users/Fenix/Desktop/Slicing/";
		final String outputPdfPath = codebase + "output.pdf";
		final String outputPhpPath = codebase + "output.php";
		final File outputPdfFile = new File(outputPdfPath);
		final File outputPhpFile = new File(outputPhpPath);
		final EDG edg = EDGFactory.createEDG(Language.Php, sourcePath);

		PdfFactory.createPdf(outputPdfFile, edg);
		CodeFactory.createCode(Language.Php, outputPhpFile, edg);
	}
}