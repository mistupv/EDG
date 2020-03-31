package upv.slicing.eknife.test;

import upv.slicing.edg.Config;
import upv.slicing.edg.DotFactory;
import upv.slicing.edg.EDGFactory;
import upv.slicing.edg.PdfFactory;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.slicing.ConstrainedAlgorithm;
import upv.slicing.edg.slicing.ConstrainedPPDGAlgorithm;
import upv.slicing.edg.slicing.SlicingAlgorithm;
import upv.slicing.edg.slicing.SlicingCriterion;
import upv.slicing.eknife.CodeFactory;
import upv.slicing.eknife.LASTFactory;
import java.io.File;
import java.util.Set;
import upv.slicing.eknife.EKnife.Language;

public class LASTest {
	public static void main(String[] args)
	{
		final String className = "break.java";
		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 18, "c");

		// BENCHMARK DIRECTORY
		final String codebase = "./e-Knife/src/test/resources/simple/";
		final String sourcePath = codebase + className;

		final String outputDotPath = codebase + "output.dot";
		final String outputPdfPath = codebase + "output.pdf";
		final String outputJavaPath = codebase + "output.java";
		final File outputDotFile = new File(outputDotPath);
		final File outputPdfFile = new File(outputPdfPath);
		final File outputJavaFile = new File(outputJavaPath);

		final LAST last = LASTFactory.createLAST(Language.Java, sourcePath);
		final EDG edg = new EDGFactory(last).createEDG();

		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = Config.CREATE_SLICING_ALGORITHM.apply(edg);
		final Set<Node> slice = slicingAlgorithm.slice(SC);

		DotFactory.createDot(new File(outputDotFile.getParentFile(), "output-whole.dot"), edg);
		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);

		System.out.println("****** FIN *******");
	}
}
