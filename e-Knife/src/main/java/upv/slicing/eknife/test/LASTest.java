package upv.slicing.eknife.test;

import upv.slicing.edg.DotFactory;
import upv.slicing.edg.EDGFactory;
import upv.slicing.edg.PdfFactory;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.slicing.ConstrainedAlgorithm;
import upv.slicing.edg.slicing.SlicingAlgorithm;
import upv.slicing.edg.slicing.SlicingCriterion;
import upv.slicing.eknife.EKnife.Language;
import upv.slicing.eknife.LASTFactory;

import java.io.File;
import java.util.Set;

public class LASTest {
	public static void main(String[] args)
	{
		final String className = "Eval_1.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 18, "z", 1); // Test.java
		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 64, "x", 1); // Eval_1.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 13, "z", 1); // TypeTest.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 24, "y", 1); // TypeTest.java 2

//		final String className = "Test.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 38, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 39, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "n", 1); // Test.java

//		final String className = "Test0.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 38, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 21, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "n", 1); // Test.java

		final String codebase = "./src/test/res/carlos/";
		final String sourcePath = codebase + className; // BENCHMARK DIRECTORY (DESKTOP)

		final String outputDotPath = codebase + "output.dot";
		final String outputPdfPath = codebase + "output.pdf";
		final String outputJavaPath = codebase + "output.java";
		final File outputDotFile = new File(outputDotPath);
		final File outputPdfFile = new File(outputPdfPath);
		final File outputJavaFile = new File(outputJavaPath);

		final LAST last = LASTFactory.createLAST(Language.Java, sourcePath);
		final EDG edg = new EDGFactory(last).createEDG();


//		for (Node n : edg.getNodes())
//		{
//			if (n.getInfo().getInfo() != null && n.getInfo().getInfo().isExpression())
//			{
//				if (n.getInfo() instanceof VariableInfo)
//				{	
//					VariableInfo vf = (VariableInfo) n.getInfo();
//					System.out.println("Node: "+n.getInfo().getId()+" -> "+n.getInfo().getType()+" Context: "+vf.getContext());
//				}
//				else
//					System.out.println("Node: "+n.getInfo().getId()+" -> "+n.getInfo().getType());
//			}
//		}
		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm(edg);
		final Set<Node> slice = slicingAlgorithm.slice(SC);
//		final List<Node> slice = null; //new LinkedList<Node>(); // EMPTY SLICE

//		System.out.println("************************");
//		System.out.println("****** EDG SLICE *******");
//		System.out.println("************************\n");

		DotFactory.createDot(new File(outputDotFile.getParentFile(), "output-whole.dot"), edg);
		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
//		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);

		System.out.println("****** FIN *******");
	}
}
