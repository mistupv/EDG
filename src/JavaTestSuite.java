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

public class JavaTestSuite {
	
	final static Config config = Config.getConfig();
	final static String codebase = "/Users/serperu/Desktop/Benchmarks/Benchmark_Suites/Suite20/tmp/";
	
	public static void main(String[] args)
	{			
		testDacapo(1);
	}
/***********************************************************************************************
*							BENCHMARK SUITE PROGRAMS										   *
***********************************************************************************************/		

/****************************	
* 		SPECJVM2008 		* 
****************************/

/**********	
** Check **
**********/
	public static void testCheck(int index){
		
		
		BenchTest[] checkBenchList = new BenchTest[3];
		checkBenchList[0] = new BenchTest("LoopBounds.java", new SlicingCriterion("LoopBounds.java", 45, "index", 1)); // LoopBounds.java
		checkBenchList[1] = new BenchTest("FloatingPointCheck.java", new SlicingCriterion("FloatingPointCheck.java", 125, "check", 1)); // FloatingPointCheck.java
		checkBenchList[2] = new BenchTest("PepTest.java", new SlicingCriterion("PepTest.java", 48, "xx", 1)); // PepTest.java
		
		final String className = checkBenchList[index].className;
		final SlicingCriterion slicingCriterion = checkBenchList[index].sc;
		final String sourcePath = config.getTestPathSuite() + "specjvm2008/check/" + className;
		
		testBench(className, slicingCriterion, sourcePath);
	}
	
/*************	
** Compress **
*************/
	public static void testCompress(int index){
		BenchTest[] compressBenchList = new BenchTest[2];
		compressBenchList[0] = new BenchTest("Compress.java", new SlicingCriterion("Compress.java", 0, "X", 1)); // Compress.java
		compressBenchList[1] = new BenchTest("Harness.java", new SlicingCriterion("Harness.java", 0, "X", 1)); // Harness.java
		
		final String className = compressBenchList[index].className;
		final SlicingCriterion slicingCriterion = compressBenchList[index].sc;
		final String sourcePath = config.getTestPathSuite() + "specjvm2008/compress/" + className;
		
		testBench(className, slicingCriterion, sourcePath);
	}
	
/**************	
** MPEGAudio **
**************/
	public static void testMpegaudio(int index){
		BenchTest[] mpegaudioBenchList = new BenchTest[1];
		mpegaudioBenchList[0] = new BenchTest("Harness.java", new SlicingCriterion("Harness.java", 0, "X", 1)); // Harness.java
		
		final String className = mpegaudioBenchList[index].className;
		final SlicingCriterion slicingCriterion = mpegaudioBenchList[index].sc;
		final String sourcePath = config.getTestPathSuite() + "specjvm2008/mpegaudio/" + className;
		
		testBench(className, slicingCriterion, sourcePath);
	}
	
/************	
** Scimark **
************/	
	public static void testScimark(int index){
		BenchTest[] scimarkBenchList = new BenchTest[5];
		scimarkBenchList[0] = new BenchTest("FFT.java", new SlicingCriterion("FFT.java", 0, "X", 1)); // FFT.java
		scimarkBenchList[1] = new BenchTest("LU.java", new SlicingCriterion("LU.java", 87, "T", 1)); // LU.java
		scimarkBenchList[2] = new BenchTest("MonteCarlo.java", new SlicingCriterion("MonteCarlo.java", 0, "X", 1)); // MonteCarlo.java
		scimarkBenchList[3] = new BenchTest("SOR.java", new SlicingCriterion("SOR.java", 0, "X", 1)); // SOR.java
		scimarkBenchList[4] = new BenchTest("SparseCompRow.java", new SlicingCriterion("SparseCompRow.java", 0, "X", 1)); // SparseCompRow.java
		
		final String className = scimarkBenchList[index].className;
		final SlicingCriterion slicingCriterion = scimarkBenchList[index].sc;
		final String sourcePath = config.getTestPathSuite() + "specjvm2008/scimark/" + className;
		
		testBench(className, slicingCriterion, sourcePath);
	}
	
/****************************	
*   		DACAPO  		* 
****************************/
	public static void testDacapo(int index){
		BenchTest[] dacapoBenchList = new BenchTest[7];
		dacapoBenchList[0] = new BenchTest("Callback.java", new SlicingCriterion("Callback.java", 0, "X", 1)); // Callback.java
		dacapoBenchList[1] = new BenchTest("CommandLineArgs.java", new SlicingCriterion("CommandLineArgs.java", 0, "X", 1)); // CommandLineArgs.java
		dacapoBenchList[2] = new BenchTest("FileDigest.java", new SlicingCriterion("FileDigest.java", 0, "X", 1)); // FileDigest.java
		dacapoBenchList[3] = new BenchTest("LatexDescriptions.java", new SlicingCriterion("LatexDescriptions.java", 0, "X", 1)); // LatexDescriptions.java
		dacapoBenchList[4] = new BenchTest("Matcher.java", new SlicingCriterion("Matcher.java", 0, "X", 1)); // Matcher.java
		dacapoBenchList[5] = new BenchTest("Slice.java", new SlicingCriterion("Slice.java", 0, "X", 1)); // Slice.java
		dacapoBenchList[6] = new BenchTest("TestHarness.java", new SlicingCriterion("TestHarness.java", 0, "X", 1)); // TestHarness.java
		
		final String className = dacapoBenchList[index].className;
		final SlicingCriterion slicingCriterion = dacapoBenchList[index].sc;
		final String sourcePath = config.getTestPathSuite() + "dacapo/harness/" + className;
		
		testBench(className, slicingCriterion, sourcePath);
	}
	
/****************************	
*   		OTHERS  		* 
****************************/
	public static void testOthers(int index){
		BenchTest[] othersBenchList = new BenchTest[2];
		othersBenchList[0] = new BenchTest("HorwitzOO.java", new SlicingCriterion("HorwitzOO.java", 0, "X", 1)); // HorwitzOO.java
		othersBenchList[1] = new BenchTest("WordCharCount.java", new SlicingCriterion("WordCharCount.java", 0, "X", 1)); // WordCharCount.java
		
		final String className = othersBenchList[index].className;
		final SlicingCriterion slicingCriterion = othersBenchList[index].sc;
		final String sourcePath = config.getTestPathSuite() + "Other/" + className;
		
		testBench(className, slicingCriterion, sourcePath);
	}
	

	// AUXILIAR FUNCTIONS AND CLASSES	
	public static void testBench(String className, SlicingCriterion slicingCriterion, String sourcePath)
	{	
		final String outputJavaPath = codebase + "output.java";

		final File outputJavaFile = new File(outputJavaPath);
		
		final EDG edg = EDGFactory.createEDG(Language.Java, sourcePath);
		
		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();
		final List<Node> slice = slicingAlgorithm.slice(SC); 

		final String outputDotPath = codebase + "output.dot";
		final String outputPdfPath = codebase + "output.pdf";
		final File outputDotFile = new File(outputDotPath);
		final File outputPdfFile = new File(outputPdfPath);
		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
		
		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);
	}
	
	private static class BenchTest
	{
		private String className;
		private SlicingCriterion sc;
		
		public BenchTest(String cn, SlicingCriterion sc)
		{
			this.className = cn;
			this.sc = sc;
		}
	}
}
