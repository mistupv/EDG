import java.io.File;
import java.util.LinkedList;
import java.util.List;

import edg.DotFactory;
import edg.PdfFactory;
import edg.graph.EDG;
import edg.graph.Node;
import edg.slicing.ConstrainedAlgorithm;
import edg.slicing.SDGAlgorithm;
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
	
/* ********************************************************************************************* */
		// GLOBAL VARIABLES TESTS:
/* ********************************************************************************************* */
		//testGlobalVariables(config);
//		final String className = "Explicit_Explicit.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 7, "n", 1); 
		
//		final String className = "Explicit_Call.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "n", 1);
		
//		final String className = "Explicit_No_use.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "n", 1);
		
//		final String className = "Call_Explicit.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 11, "n", 1);
		
//		final String className = "Call_Call.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 18, "ene", 1);
		
//		final String className = "Call_No_use.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "n", 1);
		
//		final String className = "Parameters_Explicit.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 6, "n", 1);
		
//		final String className = "Parameters_Call.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 16, "ene", 1);
		
//		final String className = "AdvancedTest.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 17, "v", 1);
		
//		final String codebase = "/Users/serperu/Desktop/Benchmarks/GVTests/";
//		final String sourcePath = config.getTestPath() + className; // GLOBAL VARIABLES TESTS
/* ********************************************************************************************* */		
/* ********************************************************************************************* */	
		
		final String className = "Test.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "p", 1); // Test.java
		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 6, "x", 1); // Test.java
		
//		final String className = "Test2.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 8, "minMN", 1); // Test.java
		
//		final String className = "GVLoop.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 9, "v", 1); // GVLoop.java
		
//		final String className = "HorwitzOO.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 14, "sum", 1); // HorwitzOO.java
		
//		final String className = "LoopBounds.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 45, "index", 1); // LoopBounds.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 105, "n", 1); // LoopBounds.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 107, "r", 1); // LoopBounds.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 93, "v", 1); // LoopBounds.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 31, "f0", 1); // LoopBounds.java

//		final String className = "LoopBoundsGV.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 26, "n", 1); // LoopBoundsGV.java
		
//		final String className = "TypeChecker.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 42, "v", 1); // TypeChecker.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 27, "k", 1); // TypeChecker.java
		
//		final String className = "FloatingPointCheck.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 125, "check", 1); // FloatingPointCheck.java
		
//		final String className = "WhileLoop.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "sum", 1); // WhileLoop.java
		
//		final String className = "DoWhileLoop.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "sum", 1); // DoWhileLoop.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 8, "i", 1); // DoWhileLoop.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 9, "i", 1); // DoWhileLoop.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 11, "i", 1); // DoWhileLoop.java
		
//		final String className = "ForLoop.java";
// 		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 4, "i", 2); // ForLoop.java   
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 4, "i", 3); // ForLoop.java   
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "i", 1); // ForLoop.java
// 		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "sum", 1); // ForLoop.java
// 		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 7, "sum", 1); // ForLoop.java

//		final String className = "ForLoopv2.java";
// 		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 7, "sum", 1); // ForLoopv2.java
		
//		final String className = "DeclarationSimple.java";
// 		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "sum", 1); // DeclarationSimple.java
		
// 		final String className = "Declaration.java";
// 		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 18, "uve", 1); // Declaration.java
 		
//		final String className = "Instance.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 6, "i", 1); // Instance.java
		
//		final String className = "Casting.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 7, "d", 1); // Casting.java
		
//		final String className = "ObjectCreation.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 4, "d", 1); // ObjectCreation.java
		
//		final String sourcePath = config.getSourcesPath() + className;

//		final String className = "TryClass.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 8, "e", 1); // TryClass.java
		
		final String codebase = "/Users/serperu/Desktop/Benchmarks/";
		final String sourcePath = config.getTestPathBenchmarks() + className; // BENCHMARK DIRECTORY (DESKTOP)
		
		final String outputDotPath = codebase + "output.dot";
		final String outputPdfPath = codebase + "output.pdf";
		final String outputJavaPath = codebase + "output.java";
		final File outputDotFile = new File(outputDotPath);
		final File outputPdfFile = new File(outputPdfPath);
		final File outputJavaFile = new File(outputJavaPath);

		final EDG edg = EDGFactory.createEDG(Language.Java, sourcePath);	
		
		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();
		final List<Node> slice = slicingAlgorithm.slice(SC); 
		//final List<Node> slice = new LinkedList<Node>(); // EMPTY SLICE

		System.out.println("************************");
		System.out.println("****** EDG SLICE *******");
		System.out.println("************************\n");
		
		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);
		
		// SDG SLICE
		
		System.out.println("************************");
		System.out.println("****** SDG SLICE *******");
		System.out.println("************************\n");
		
		final String outputDotPathSDG = codebase + "outputSDG.dot";
		final String outputPdfPathSDG = codebase + "outputSDG.pdf";
		final String outputJavaPathSDG = codebase + "outputSDG.java";
		final File outputDotFileSDG = new File(outputDotPathSDG);
		final File outputPdfFileSDG = new File(outputPdfPathSDG);
		final File outputJavaFileSDG = new File(outputJavaPathSDG);
		
		final SlicingAlgorithm slicingSDGAlgorithm = new SDGAlgorithm();
		final List<Node> SDGSlice = slicingSDGAlgorithm.slice(SC); 
		
		DotFactory.createDot(outputDotFileSDG, edg, SC, SDGSlice);
		PdfFactory.createPdf(outputPdfFileSDG, outputDotFileSDG);
		CodeFactory.createCode(Language.Java, outputJavaFileSDG, edg, SDGSlice);
		
		slice.removeIf(sliceNode -> sliceNode.getData().isFictitious());
		SDGSlice.removeIf(sliceNode -> sliceNode.getData().isFictitious());
		
		System.out.println("Total EDG Nodes: "+slice.size());
		System.out.println("Total SDG Nodes: "+SDGSlice.size());
	}

	private static void testGlobalVariables(Config config) {
		
		final TestGV[] GVList = new TestGV[9];
		GVList[0] = new TestGV("Explicit_Explicit.java", new SlicingCriterion("Explicit_Explicit.java", 7, "n", 1));
		GVList[1] = new TestGV("Explicit_Call.java", new SlicingCriterion("Explicit_Call.java",5, "n", 1));
		GVList[2] = new TestGV("Explicit_No_use.java", new SlicingCriterion("Explicit_No_use.java", 5, "n", 1));
		GVList[3] = new TestGV("Call_Explicit.java", new SlicingCriterion("Call_Explicit.java", 11, "n", 1));
		GVList[4] = new TestGV("Call_Call.java", new SlicingCriterion("Call_Call.java",18, "ene", 1));
		GVList[5] = new TestGV("Call_No_use.java", new SlicingCriterion("Call_No_use.java", 5, "n", 1));
		GVList[6] = new TestGV("Parameters_Explicit.java", new SlicingCriterion("Parameters_Explicit.java", 6, "n", 1));
		GVList[7] = new TestGV("Parameters_Call.java", new SlicingCriterion("Parameters_Call.java", 16, "ene", 1));
		GVList[8] = new TestGV("AdvancedTest.java", new SlicingCriterion("AdvancedTest.java", 17, "v", 1));
		
		final String codebase = "/Users/serperu/Desktop/Benchmarks/GVTests/";
		
		
		for (int i = 0; i < GVList.length; i++)
		{
			final String className = GVList[i].className;
			final SlicingCriterion slicingCriterion = GVList[i].sc;
			final String sourcePath = config.getTestPath() + className; // GLOBAL VARIABLES TESTS
			
			//final String outputDotPath = codebase + "output.dot";
			//final String outputPdfPath = codebase + "output.pdf";
			final String outputJavaPath = codebase + "output.java";
			//final File outputDotFile = new File(outputDotPath);
			//final File outputPdfFile = new File(outputPdfPath);
			final File outputJavaFile = new File(outputJavaPath);
			
			final EDG edg = EDGFactory.createEDG(Language.Java, sourcePath);
			
			final Node SC = edg.getNode(slicingCriterion);
			final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();
			final List<Node> slice = slicingAlgorithm.slice(SC); 

			//DotFactory.createDot(outputDotFile, edg, SC, slice);
			//PdfFactory.createPdf(outputPdfFile, outputDotFile);
			CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);
			
			//ABRIR Y COMPARAR LOS .java del Gold Standard y del output
			
		}
	}
	private static class TestGV
	{
		private String className;
		private SlicingCriterion sc;
		
		public TestGV(String cn, SlicingCriterion sc)
		{
			this.className = cn;
			this.sc = sc;
		}
	}
}