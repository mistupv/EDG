import java.io.File;
import java.util.List;

import edg.DotFactory;
import edg.PdfFactory;
import edg.graph.EDG;
import edg.graph.Node;
import edg.slicing.ConstrainedAlgorithm;
import edg.slicing.SlicingAlgorithm;
import edg.slicing.SlicingCriterion;
import eknife.CodeFactory;
import eknife.EDGFactory;
import eknife.EKnife.Language;
import eknife.config.Config;

public class JavaTest
{
	public static void main(String[] args)
	{
		final Config config = Config.getConfig();
		final String className = "JavaTest2.java";
		final String sourcePath = config.getSourcesPath() + className;
		final String codebase = "/Users/Fenix/Desktop/Slicing/";
		final String outputDotPath = codebase + "output.dot";
		final String outputPdfPath = codebase + "output.pdf";
		final String outputJavaPath = codebase + "output.java";
		final File outputDotFile = new File(outputDotPath);
		final File outputPdfFile = new File(outputPdfPath);
		final File outputJavaFile = new File(outputJavaPath);

		final EDG edg = EDGFactory.createEDG(Language.Java, sourcePath);
		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 42, "x", 1);
		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();
		final List<Node> slice = slicingAlgorithm.slice(SC);

		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);
	}
}