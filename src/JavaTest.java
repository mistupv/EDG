import java.io.File;
import java.util.LinkedList;
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
		
//		final String className = "Test.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 9, "z", 1); // Test.java

/* ********************************************************************************************* */
		// GLOBAL VARIABLES TESTS:
/* ********************************************************************************************* */

//		final String className = "Explicit_Explicit.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 7, "n", 1); 
		
//		final String className = "Explicit_Call.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "n", 1);
//		
//		final String className = "Explicit_No_use.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 5, "n", 1);
//		
//		final String className = "Call_Explicit.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 11, "n", 1);
//		
		final String className = "Call_Call.java";
		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 18, "ene", 1);
//		
//		final String className = "Call_No_use.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "n", 1);
//		
//		final String className = "Parameters_Explicit.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 6, "n", 1);
//		
//		final String className = "Parameters_Call.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 16, "ene", 1);
		
//		final String className = "AdvancedTest.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 10, "z", 1);
		
		final String codebase = "/Users/serperu/Desktop/Benchmarks/GVTests/";
		if (true)
			throw new RuntimeException("SET THE TEST PATH BEFORE CONTINUING");
		final String sourcePath = null;// config.getTestPath() + className; // GLOBAL VARIABLES TESTS
/* ********************************************************************************************* */		
/* ********************************************************************************************* */		
//		final String className = "Test2.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 16, "a", 1); // Test.java
		
//		final String className = "GVLoop.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 9, "v", 1); // GVLoop.java
		
//		final String className = "LoopBounds.java";
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 45, "index", 1); // LoopBounds.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 105, "n", 1); // LoopBounds.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 107, "r", 1); // LoopBounds.java
//		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, 113, "v", 1); // LoopBounds.java

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

//		final String codebase = "/Users/serperu/Desktop/Benchmarks/";
//		final String sourcePath = config.getTestPathBenchmarks() + className; // BENCHMARK DIRECTORY (DESKTOP)
		
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

		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);
	}
}