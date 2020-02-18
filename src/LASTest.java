import java.io.File;
import java.util.LinkedList;
import java.util.List;

import edg.DotFactory;
import edg.EDGFactoryNew;
import edg.PdfFactory;
import edg.graph.EDG;
import edg.graph.LAST;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.slicing.ConstrainedAlgorithm;
import edg.slicing.SDGAlgorithm;
import edg.slicing.SlicingAlgorithm;
import edg.slicing.SlicingCriterion;
import eknife.CodeFactory;
import eknife.LASTFactory;
import eknife.EKnife.Language;
import eknife.config.Config;
import eknife.java.ControlFlowEdgeGenerator;
import eknife.java.ValueEdgeGenerator;

public class LASTest
{
	public static void main(String[] args)
	{			
		final Config config = Config.getConfig();
	
		final String className = "prueba2.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 38, "a", 1); // Test.java
		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 6, "y", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "n", 1); // Test.java
				
//		final String className = "Test0.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 38, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 21, "a", 1); // Test.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "n", 1); // Test.java
		
		final String codebase = "/Users/serperu/Desktop/Benchmarks/";
		final String sourcePath = config.getTestPathBenchmarks() + className; // BENCHMARK DIRECTORY (DESKTOP)
		
		final String outputDotPath = codebase + "outputOLD.dot";
		final String outputPdfPath = codebase + "outputOLD.pdf";
		final String outputJavaPath = codebase + "outputOLD.java";
		final File outputDotFile = new File(outputDotPath);
		final File outputPdfFile = new File(outputPdfPath);
		final File outputJavaFile = new File(outputJavaPath);

		final LAST last = LASTFactory.createLAST(Language.Java, sourcePath);
		
		final EDG edg = EDGFactoryNew.createEDG(last);
		
		
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
//		final List<Node> slice = new LinkedList<Node>(); // EMPTY SLICE

//		System.out.println("************************");
//		System.out.println("****** EDG SLICE *******");
//		System.out.println("************************\n");
		
		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);
		
		System.out.println("****** FIN *******");
	}
}
