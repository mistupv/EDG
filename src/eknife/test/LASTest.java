package eknife.test;

import edg.DotFactory;
import edg.EDGFactory;
import edg.PdfFactory;
import edg.graph.EDG;
import edg.graph.LAST;
import edg.graph.Node;
import edg.slicing.ConstrainedAlgorithm;
import edg.slicing.SlicingAlgorithm;
import edg.slicing.SlicingCriterion;
import eknife.EKnife.Language;
import eknife.LASTFactory;
import eknife.config.Config;

import java.io.File;
import java.util.List;

public class LASTest {
	public static void main(String[] args)
	{
		final Config config = Config.getConfig();

		final String className = "prueba2.java";
		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 8, "x", 1); // Test.java

//		final String className = "Test.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 38, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 39, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "n", 1); // Test.java

//		final String className = "Test0.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 38, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 21, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "n", 1); // Test.java

		final String codebase = "/Users/serperu/Desktop/Benchmarks/";
		final String sourcePath = config.getTestPathBenchmarks() + className; // BENCHMARK DIRECTORY (DESKTOP)

		final String outputDotPath = codebase + "output.dot";
		final String outputPdfPath = codebase + "output.pdf";
		final String outputJavaPath = codebase + "output.java";
		final File outputDotFile = new File(outputDotPath);
		final File outputPdfFile = new File(outputPdfPath);
		final File outputJavaFile = new File(outputJavaPath);

		final LAST last = LASTFactory.createLAST(Language.Java, sourcePath);
		final EDG edg = EDGFactory.createEDG(last);


//		for (Node n : edg.getNodes())
//		{
//			if (n.getData().getInfo() != null && n.getData().getInfo().isExpression())
//			{
//				if (n.getData() instanceof VariableInfo)
//				{	
//					VariableInfo vf = (VariableInfo) n.getData();
//					System.out.println("Node: "+n.getData().getId()+" -> "+n.getData().getType()+" Context: "+vf.getContext());
//				}
//				else
//					System.out.println("Node: "+n.getData().getId()+" -> "+n.getData().getType());
//			}
//		}
		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();
		final List<Node> slice = slicingAlgorithm.slice(SC);
//		final List<Node> slice = null; //new LinkedList<Node>(); // EMPTY SLICE

//		System.out.println("************************");
//		System.out.println("****** EDG SLICE *******");
//		System.out.println("************************\n");

		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
//		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);

		System.out.println("****** FIN *******");
	}
}
