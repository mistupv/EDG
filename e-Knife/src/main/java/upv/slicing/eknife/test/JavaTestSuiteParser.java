package upv.slicing.eknife.test;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import upv.slicing.edg.EDGFactory;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.EDG.GraphGeneratorTimer;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.slicing.ConstrainedAlgorithm;
import upv.slicing.edg.slicing.SlicingAlgorithm;
import upv.slicing.edg.slicing.SlicingCriterion;
import upv.slicing.eknife.CodeFactory;
import upv.slicing.eknife.EKnife.Language;
import upv.slicing.eknife.LASTFactory;
import upv.slicing.eknife.Config;

import java.io.*;
import java.util.*;

public class JavaTestSuiteParser {

	final static Config config = Config.getConfig();
	final static String codebase = "/Users/serperu/Desktop/Benchmarks/Benchmark_Suites/Suite20/tmp/";
	final static int windowSize = 10;

	static int currentRow = 1;
	final static int startColumn = 1;
	final static Workbook workbook = new HSSFWorkbook();
	final static Sheet resultsSheet = workbook.createSheet("Results");
	final static String currentPath = codebase + "ResultsSlicingTime.xls";
	final static File file = new File(currentPath);

	public static void initializeSheet()
	{
		// Create header
		final Row headerRow = resultsSheet.createRow(currentRow);
		currentRow++;

		final Cell benchmarkNameCell = headerRow.createCell(startColumn);
		final Cell EDGSliceTime = headerRow.createCell(startColumn + 1);

		benchmarkNameCell.setCellValue("Benchmark Name");
		EDGSliceTime.setCellValue("EDG Slice Time (ms)");
	}

	public static void main(String[] args)
	{

		initializeSheet();

		/****************************	
		 * 		SPECJVM2008 		*
		 ****************************/
		System.out.println("****************************\n*       SPECJVM2008        *\n****************************");

		System.out.println("\n***********\n** Check **\n***********");
		for (int i = 0; i < 3; i++)
			testCheck(i);

		System.out.println("\n**************\n** Compress **\n**************");
		for (int i = 0; i < 2; i++)
			testCompress(i);

		System.out.println("\n***************\n** MPEGAudio **\n***************");
		for (int i = 0; i < 1; i++)
			testMpegaudio(i);

		System.out.println("\n*************\n** Scimark **\n*************");
		for (int i = 0; i < 5; i++)
			testScimark(i);

		/****************************	
		 *   		DACAPO  		*
		 ****************************/
		System.out
				.println("*****************************\n*   	    DACAPO          *\n*****************************");

		for (int i = 0; i < 7; i++) // Faltara la de Dacapo[1] (CommandLineArgs, 100secs flow)
			if (i != 2)
				testDacapo(i);
		/****************************
		 *   		OTHERS  		*
		 ****************************/
		System.out.println("**************************\n*        OTHERS          *\n**************************");

		for (int i = 0; i < 2; i++)
			testOthers(i);

		writeXLS();

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
//		public static void testCheck(int index){
//			
//			BenchTest[] checkBenchList = new BenchTest[3];
//			checkBenchList[0] = new BenchTest("LoopBounds.java", new SlicingCriterion("LoopBounds.java", 45, "index", 1)); // LoopBounds.java
//			checkBenchList[1] = new BenchTest("FloatingPointCheck.java", new SlicingCriterion("FloatingPointCheck.java", 125, "check", 1)); // FloatingPointCheck.java
//			checkBenchList[2] = new BenchTest("PepTest.java", new SlicingCriterion("PepTest.java", 48, "xx", 1)); // PepTest.java
//			
//			final String className = checkBenchList[index].className;
//			final SlicingCriterion slicingCriterion = checkBenchList[index].sc;
//			final String sourcePath = config.getTestPathSuite() + "specjvm2008/check/" + className;
//			
//			testBench(className, slicingCriterion, sourcePath);
//		}
	public static void testCheck(int index)
	{

		BenchTest[] checkBenchList = new BenchTest[3];
		checkBenchList[0] = new BenchTest("LoopBounds.java"); // LoopBounds.java
		checkBenchList[1] = new BenchTest("FloatingPointCheck.java"); // FloatingPointCheck.java
		checkBenchList[2] = new BenchTest("PepTest.java"); // PepTest.java

		final String className = checkBenchList[index].className;

		System.out.println();
		System.out.println("** " + className + "**");
		System.out.println();
		final String sourcePath = config.getTestPathSuite() + "specjvm2008/check/" + className;

		parseInTmpXls(className);
		//readFileAndCalculateStatistics(className);
		//testBench(className, sourcePath);
	}

	/*************
	 ** Compress **
	 *************/
//		public static void testCompress(int index){
//			BenchTest[] compressBenchList = new BenchTest[2];
//			compressBenchList[0] = new BenchTest("Compress.java", new SlicingCriterion("Compress.java", 0, "X", 1)); // Compress.java
//			compressBenchList[1] = new BenchTest("Harness.java", new SlicingCriterion("Harness.java", 0, "X", 1)); // Harness.java
//			
//			final String className = compressBenchList[index].className;
//			final SlicingCriterion slicingCriterion = compressBenchList[index].sc;
//			final String sourcePath = config.getTestPathSuite() + "specjvm2008/compress/" + className;
//			
//			testBench(className, slicingCriterion, sourcePath);
//		}
	public static void testCompress(int index)
	{
		BenchTest[] compressBenchList = new BenchTest[2];
		compressBenchList[0] = new BenchTest("Compress.java"); // Compress.java
		compressBenchList[1] = new BenchTest("Harness.java"); // Harness.java

		final String className = compressBenchList[index].className;
		System.out.println();
		System.out.println("** " + className + "**");
		System.out.println();
		final String sourcePath = config.getTestPathSuite() + "specjvm2008/compress/" + className;

		parseInTmpXls(className);
		//readFileAndCalculateStatistics(className);
		//testBench(className, sourcePath);
	}

	/**************
	 ** MPEGAudio **
	 **************/
//		public static void testMpegaudio(int index){
//			BenchTest[] mpegaudioBenchList = new BenchTest[1];
//			mpegaudioBenchList[0] = new BenchTest("Harness.java", new SlicingCriterion("Harness.java", 0, "X", 1)); // Harness.java
//			
//			final String className = mpegaudioBenchList[index].className;
//			final SlicingCriterion slicingCriterion = mpegaudioBenchList[index].sc;
//			final String sourcePath = config.getTestPathSuite() + "specjvm2008/mpegaudio/" + className;
//			
//			testBench(className, slicingCriterion, sourcePath);
//		}
	public static void testMpegaudio(int index)
	{
		BenchTest[] mpegaudioBenchList = new BenchTest[1];
		mpegaudioBenchList[0] = new BenchTest("MHarness.java"); // Harness.java

		final String className = mpegaudioBenchList[index].className;
		System.out.println();
		System.out.println("** " + className + "**");
		System.out.println();
		final String sourcePath = config.getTestPathSuite() + "specjvm2008/mpegaudio/" + className;

		parseInTmpXls(className);
		//readFileAndCalculateStatistics(className);
		//testBench(className, sourcePath);
	}

	/************
	 ** Scimark **
	 ************/
//		public static void testScimark(int index){
//			BenchTest[] scimarkBenchList = new BenchTest[5];
//			scimarkBenchList[0] = new BenchTest("FFT.java", new SlicingCriterion("FFT.java", 0, "X", 1)); // FFT.java
//			scimarkBenchList[1] = new BenchTest("LU.java", new SlicingCriterion("LU.java", 87, "T", 1)); // LU.java
//			scimarkBenchList[2] = new BenchTest("MonteCarlo.java", new SlicingCriterion("MonteCarlo.java", 0, "X", 1)); // MonteCarlo.java
//			scimarkBenchList[3] = new BenchTest("SOR.java", new SlicingCriterion("SOR.java", 0, "X", 1)); // SOR.java
//			scimarkBenchList[4] = new BenchTest("SparseCompRow.java", new SlicingCriterion("SparseCompRow.java", 0, "X", 1)); // SparseCompRow.java
//			
//			final String className = scimarkBenchList[index].className;
//			final SlicingCriterion slicingCriterion = scimarkBenchList[index].sc;
//			final String sourcePath = config.getTestPathSuite() + "specjvm2008/scimark/" + className;
//			
//			testBench(className, slicingCriterion, sourcePath);
//		}
	public static void testScimark(int index)
	{
		BenchTest[] scimarkBenchList = new BenchTest[5];
		scimarkBenchList[0] = new BenchTest("FFT.java"); // FFT.java
		scimarkBenchList[1] = new BenchTest("LU.java"); // LU.java
		scimarkBenchList[2] = new BenchTest("MonteCarlo.java"); // MonteCarlo.java
		scimarkBenchList[3] = new BenchTest("SOR.java"); // SOR.java
		scimarkBenchList[4] = new BenchTest("SparseCompRow.java"); // SparseCompRow.java

		final String className = scimarkBenchList[index].className;
		System.out.println();
		System.out.println("** " + className + "**");
		System.out.println();
		final String sourcePath = config.getTestPathSuite() + "specjvm2008/scimark/" + className;

		parseInTmpXls(className);
		//readFileAndCalculateStatistics(className);
		//testBench(className, sourcePath);
	}

	/****************************
	 *   		DACAPO  		*
	 ****************************/
//		public static void testDacapo(int index){
//			BenchTest[] dacapoBenchList = new BenchTest[7];
//			dacapoBenchList[0] = new BenchTest("Callback.java", new SlicingCriterion("Callback.java", 0, "X", 1)); // Callback.java
//			dacapoBenchList[1] = new BenchTest("CommandLineArgs.java", new SlicingCriterion("CommandLineArgs.java", 0, "X", 1)); // CommandLineArgs.java
//			dacapoBenchList[2] = new BenchTest("FileDigest.java", new SlicingCriterion("FileDigest.java", 0, "X", 1)); // FileDigest.java
//			dacapoBenchList[3] = new BenchTest("LatexDescriptions.java", new SlicingCriterion("LatexDescriptions.java", 0, "X", 1)); // LatexDescriptions.java
//			dacapoBenchList[4] = new BenchTest("Matcher.java", new SlicingCriterion("Matcher.java", 0, "X", 1)); // Matcher.java
//			dacapoBenchList[5] = new BenchTest("Slice.java", new SlicingCriterion("Slice.java", 0, "X", 1)); // Slice.java
//			dacapoBenchList[6] = new BenchTest("TestHarness.java", new SlicingCriterion("TestHarness.java", 0, "X", 1)); // TestHarness.java
//			
//			final String className = dacapoBenchList[index].className;
//			final SlicingCriterion slicingCriterion = dacapoBenchList[index].sc;
//			final String sourcePath = config.getTestPathSuite() + "dacapo/harness/" + className;
//			
//			testBench(className, slicingCriterion, sourcePath);
//		}
	public static void testDacapo(int index)
	{
		BenchTest[] dacapoBenchList = new BenchTest[7];
		dacapoBenchList[0] = new BenchTest("Callback.java"); // Callback.java
		dacapoBenchList[1] = new BenchTest("CommandLineArgs.java"); // CommandLineArgs.java
		dacapoBenchList[2] = new BenchTest("FileDigest.java"); // FileDigest.java
		dacapoBenchList[3] = new BenchTest("LatexDescriptions.java"); // LatexDescriptions.java
		dacapoBenchList[4] = new BenchTest("Matcher.java"); // Matcher.java
		dacapoBenchList[5] = new BenchTest("Slice.java"); // Slice.java
		dacapoBenchList[6] = new BenchTest("TestHarness.java"); // TestHarness.java

		final String className = dacapoBenchList[index].className;
		System.out.println();
		System.out.println("** " + className + "**");
		System.out.println();
		final String sourcePath = config.getTestPathSuite() + "dacapo/harness/" + className;

		parseInTmpXls(className);
		//readFileAndCalculateStatistics(className);
		//testBench(className, sourcePath);
	}

	/****************************
	 *   		OTHERS  		*
	 ****************************/
//		public static void testOthers(int index){
//			BenchTest[] othersBenchList = new BenchTest[2];
//			othersBenchList[0] = new BenchTest("HorwitzOO.java", new SlicingCriterion("HorwitzOO.java", 0, "X", 1)); // HorwitzOO.java
//			othersBenchList[1] = new BenchTest("WordCharCount.java", new SlicingCriterion("WordCharCount.java", 0, "X", 1)); // WordCharCount.java
//			
//			final String className = othersBenchList[index].className;
//			final SlicingCriterion slicingCriterion = othersBenchList[index].sc;
//			final String sourcePath = config.getTestPathSuite() + "Other/" + className;
//			
//			testBench(className, slicingCriterion, sourcePath);
//		}
	public static void testOthers(int index)
	{
		BenchTest[] othersBenchList = new BenchTest[2];
		othersBenchList[0] = new BenchTest("HorwitzOO.java"); // HorwitzOO.java
		othersBenchList[1] = new BenchTest("WordCharCount.java"); // WordCharCount.java

		final String className = othersBenchList[index].className;
		System.out.println();
		System.out.println("** " + className + "**");
		System.out.println();
		final String sourcePath = config.getTestPathSuite() + "Other/" + className;

		parseInTmpXls(className);
		//readFileAndCalculateStatistics(className);
		//testBench(className, sourcePath);
	}


	// AUXILIAR FUNCTIONS AND CLASSES	
	public static void testBench(String className, SlicingCriterion slicingCriterion, String sourcePath)
	{
		final String outputJavaPath = codebase + "output.java";

		final File outputJavaFile = new File(outputJavaPath);

		final LAST last = LASTFactory.createLAST(Language.Java, sourcePath);
		final EDG edg = new EDGFactory(last).createEDG();

		final Node SC = edg.getNode(slicingCriterion);

		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm(edg);
		final Set<Node> slice = slicingAlgorithm.slice(SC);

//			final String outputDotPath = codebase + "output.dot";
//			final String outputPdfPath = codebase + "output.pdf";
//			final File outputDotFile = new File(outputDotPath);
//			final File outputPdfFile = new File(outputPdfPath);
//			DotFactory.createDot(outputDotFile, edg, SC, slice);
//			PdfFactory.createPdf(outputPdfFile, outputDotFile);
		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);

		// SDG SLICE

//			final String outputDotPathSDG = codebase + "outputSDG.dot";
//			final String outputPdfPathSDG = codebase + "outputSDG.pdf";
//			final String outputJavaPathSDG = codebase + "outputSDG.java";
//			final File outputDotFileSDG = new File(outputDotPathSDG);
//			final File outputPdfFileSDG = new File(outputPdfPathSDG);
//			final File outputJavaFileSDG = new File(outputJavaPathSDG);
//			
//			final SlicingAlgorithm slicingSDGAlgorithm = new SDGAlgorithm();
//			final List<Node> SDGSlice = slicingSDGAlgorithm.slice(SC); 
//			
//			DotFactory.createDot(outputDotFileSDG, edg, SC, SDGSlice);
//			PdfFactory.createPdf(outputPdfFileSDG, outputDotFileSDG);
//			CodeFactory.createCode(Language.Java, outputJavaFileSDG, edg, SDGSlice);
//			
//			slice.removeIf(sliceNode -> sliceNode.getInfo().isFictitious());
//			SDGSlice.removeIf(sliceNode -> sliceNode.getInfo().isFictitious());
//			
//			System.out.println("Total EDG Nodes: "+slice.size());
//			System.out.println("Total SDG Nodes: "+SDGSlice.size());
//			
	}

//		public static void testBench(String className, String sourcePath)
//		{
//			final EDG edg = EDGFactory.createEDG(Language.Java, sourcePath);
//			
//			final List<Node> edgNodesCopy = edg.getNodes();
//			final int SDGNumberNodes = countSDGNodes(edgNodesCopy);;
//			edgNodesCopy.removeIf(node -> node.getInfo().isFictitious());
//			
//			final List<Node> edgNodes = edg.getNodes();
//			for (Node edgNode : edgNodes)
//			{
//				if (!edgNode.getInfo().isSliceable(edgNode))
//					continue;
//				
//				final Row newRow = resultsSheet.createRow(currentRow);
//				currentRow++;
//				
//				final Cell benchmarkNameCell = newRow.createCell(startColumn);
//				final Cell criterionEDGNumberCell = newRow.createCell(startColumn + 1);
//				final Cell criterionSDGNumberCell = newRow.createCell(startColumn + 2);
//				final Cell totalEDGNodes = newRow.createCell(startColumn + 3);
//				final Cell totalSDGNodes = newRow.createCell(startColumn + 4);
//				final Cell totalEDGNonFititiousNodes = newRow.createCell(startColumn + 5);
//				final Cell sliceEDG = newRow.createCell(startColumn + 6);
//				final Cell sliceSDG = newRow.createCell(startColumn + 7);
//				final Cell sliceSDGEDGNodes = newRow.createCell(startColumn + 8);
//				final Cell formulaCell = newRow.createCell(startColumn + 9);
//				final Cell EDGSliceTime = newRow.createCell(startColumn + 10);
//				
//				benchmarkNameCell.setCellValue(className);
//				totalEDGNodes.setCellValue(edgNodes.size());
//				totalSDGNodes.setCellValue(SDGNumberNodes);
//				totalEDGNonFititiousNodes.setCellValue(edgNodesCopy.size());
//				
//				criterionEDGNumberCell.setCellValue(edgNode.getInfo().getId());
//				criterionSDGNumberCell.setCellValue(edgNode.getInfo().getSDGId());
//				
//				final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();
//				final SlicingAlgorithm slicingSDGAlgorithm = new SDGAlgorithm();
//				
//				final long start = System.currentTimeMillis(); 
//				final List<Node> EDGSlice = slicingAlgorithm.slice(edgNode);
//				final long end = System.currentTimeMillis();
//				EDGSliceTime.setCellValue(end-start);
//
//				final List<Node> SDGSlice = slicingSDGAlgorithm.slice(edgNode); 
//				final int numberOfSDGNodes = countSDGNodes(SDGSlice);
//				sliceSDG.setCellValue(numberOfSDGNodes);
//				
//				EDGSlice.removeIf(sliceNode -> sliceNode.getInfo().isFictitious());
//				SDGSlice.removeIf(sliceNode -> sliceNode.getInfo().isFictitious());
//				
//				final CellReference cellRefSliceEDG = new CellReference(sliceEDG);
//				final CellReference cellRefSliceSDG = new CellReference(sliceSDGEDGNodes);
//				
//				sliceEDG.setCellValue(EDGSlice.size());
//				sliceSDGEDGNodes.setCellValue(SDGSlice.size());
//				
//				formulaCell.setCellFormula("100-("+cellRefSliceEDG.formatAsString()+"*100/"+cellRefSliceSDG.formatAsString()+")");
//				
////				System.out.println("Total EDG Nodes: "+EDGSlice.size());
////				System.out.println("Total SDG Nodes: "+SDGSlice.size());
//			}
//			currentRow += 2;
//		}

	public static void testBench(String className, String sourcePath) // Time Graph Generation
	{
		final String className0 = className.substring(0, className.lastIndexOf("."));
		final File benchGenTimeFile = new File(codebase + className0 + "_GraphGeneratioTimes1000.txt");
		benchGenTimeFile.delete();

//			final double[] arrayEDG = new double[1000];
//			final double[] arraySDG = new double[1000];

//			for (int i = 0; i < 1001; i++){
		for (int i = 0; i < 214; i++) {
			final LAST last = LASTFactory.createLAST(Language.Java, sourcePath);
			final EDG edg = new EDGFactory(last).createEDG();

			if (i == 0)
				continue;

			GraphGeneratorTimer ggt = edg.getGenerationTime();
			double EDGTime = ggt.getGenerationEDGTime();

			if (i != 0) {
				try (PrintWriter writer = new PrintWriter(benchGenTimeFile)) {
					writer.println(EDGTime);
				} catch (FileNotFoundException e) {
					e.printStackTrace();
				}
			}
		}
//			final int windowSize = 10;

//			for (int i = 0; i < 1000; i++)
//				try (PrintWriter writer = new PrintWriter(benchSliceTimeFile)) {
//					writer.println(arrayEDG[i] + " " + arraySDG[i]);
//				} catch (FileNotFoundException e) {
//					e.printStackTrace();
//				}
//			final double avgEDG = obtainMinimalWindowCoV(arrayEDG, windowSize);
//			final double avgSDG = obtainMinimalWindowCoV(arraySDG, windowSize);

	}


	public static void testBenchSliceTime(String className, String sourcePath) // Generate Slice 
	{
		final LAST last = LASTFactory.createLAST(Language.Java, sourcePath);
		final EDG edg = new EDGFactory(last).createEDG();

//			final List<Node> edgNodesCopy = edg.getNodes();
//			final int SDGNumberNodes = countSDGNodes(edgNodesCopy);;
//			edgNodesCopy.removeIf(node -> node.getInfo().isFictitious());


		final String className0 = className.substring(0, className.lastIndexOf("."));
		final File benchSliceTimeFile = new File(codebase + className0 + "_sliceTime.txt");
		benchSliceTimeFile.delete();

		for (Node edgNode : edg.vertexSet())
		{
			if (!edgNode.getType().isSliceable())
				continue;

//				final Row newRow = resultsSheet.createRow(currentRow);
//				currentRow++;

//				final Cell benchmarkNameCell = newRow.createCell(startColumn);
//				final Cell criterionEDGNumberCell = newRow.createCell(startColumn + 1);
//				final Cell criterionSDGNumberCell = newRow.createCell(startColumn + 2);
//				final Cell totalEDGNodes = newRow.createCell(startColumn + 3);
//				final Cell totalSDGNodes = newRow.createCell(startColumn + 4);
//				final Cell totalEDGNonFititiousNodes = newRow.createCell(startColumn + 5);
//				final Cell sliceEDG = newRow.createCell(startColumn + 6);
//				final Cell sliceSDG = newRow.createCell(startColumn + 7);
//				final Cell sliceSDGEDGNodes = newRow.createCell(startColumn + 8);
//				final Cell formulaCell = newRow.createCell(startColumn + 9);
//				final Cell EDGSliceTime = newRow.createCell(startColumn + 10);
//				
//				benchmarkNameCell.setCellValue(className);
//				totalEDGNodes.setCellValue(edgNodes.size());
//				totalSDGNodes.setCellValue(SDGNumberNodes);
//				totalEDGNonFititiousNodes.setCellValue(edgNodesCopy.size());
//				
//				criterionEDGNumberCell.setCellValue(edgNode.getInfo().getId());
//				criterionSDGNumberCell.setCellValue(edgNode.getInfo().getSDGId());

			final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm(edg);
//				final SlicingAlgorithm slicingSDGAlgorithm = new SDGAlgorithm();

			//String line = "";

			final double[] array = new double[1000];
			for (int i = 0; i < 1001; i++)
			{
				final long start = System.nanoTime();
				slicingAlgorithm.slice(edgNode);
				final long end = System.nanoTime();
				if (i != 0)
					array[i - 1] = (end - start) / 1000000.0;
			}

			final double avg = obtainMinimalWindowCoV(array, windowSize).getAvg();
			try (PrintWriter writer = new PrintWriter(benchSliceTimeFile)) {
				writer.println(avg);
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			}
//				EDGSliceTime.setCellValue(end-start);

//				final List<Node> SDGSlice = slicingSDGAlgorithm.slice(edgNode); 
//				final int numberOfSDGNodes = countSDGNodes(SDGSlice);
//				sliceSDG.setCellValue(numberOfSDGNodes);

//				EDGSlice.removeIf(sliceNode -> sliceNode.getInfo().isFictitious());
//				SDGSlice.removeIf(sliceNode -> sliceNode.getInfo().isFictitious());

//				final CellReference cellRefSliceEDG = new CellReference(sliceEDG);
//				final CellReference cellRefSliceSDG = new CellReference(sliceSDGEDGNodes);

//				sliceEDG.setCellValue(EDGSlice.size());
//				sliceSDGEDGNodes.setCellValue(SDGSlice.size());

//				formulaCell.setCellFormula("100-("+cellRefSliceEDG.formatAsString()+"*100/"+cellRefSliceSDG.formatAsString()+")");

//				System.out.println("Total EDG Nodes: "+EDGSlice.size());
//				System.out.println("Total SDG Nodes: "+SDGSlice.size());
		}
//			currentRow += 2;
	}


	private static AvgStd obtainMinimalWindowCoV(double[] array, int windowSize)
	{
		double avg = -1.0;
		double std = 0;

		for (int i = 0; i < 1000 - windowSize; i++)
		{
			double[] window = new double[windowSize];
			System.arraycopy(array, i, window, 0, windowSize);


			double CoV = Statistics.coefficientOfVariation(window);
			if (CoV < 0.01 || Double.isNaN(CoV))
			{
				avg = Statistics.average(window);
				std = Statistics.standardDeviation(window);
				break;
			}
		}
		if (avg == -1.0 && windowSize > 1)
			return obtainMinimalWindowCoV(array, windowSize - 1);
		return new AvgStd(avg, std);
	}


	private static AvgStd obtainMinimalWindowCoVAccurated(double[] array, int windowSize)
	{
		double avg = -1.0;
		double std = 0;
		AvgStd windowResult = new AvgStd();

		double avgDifference = Double.MAX_VALUE;
		double sampleAvg = Statistics.average(array);

		for (int i = 0; i < 1000 - windowSize; i++)
		{
			double[] window = new double[windowSize];
			System.arraycopy(array, i, window, 0, windowSize);

			double CoV = Statistics.coefficientOfVariation(window);
			if (CoV < 0.01 || Double.isNaN(CoV))
			{
				avg = Statistics.average(window);
				std = Statistics.standardDeviation(window);

				double avgDifferenceCandidate = Math.abs(sampleAvg - avg);
				if (avgDifferenceCandidate < avgDifference)
				{
					windowResult.setAvg(avg);
					windowResult.setStd(std);
				}
			}
		}
		if (avg == -1.0 && windowSize > 1)
			return obtainMinimalWindowCoV(array, windowSize - 1);
		return windowResult;
	}

	public static void writeXLS()
	{
		try
		{
			final FileOutputStream fos = new FileOutputStream(file);
			workbook.write(fos);
			fos.close();
			workbook.close();
		} catch (IOException e)
		{
			e.printStackTrace();
		}
	}

	private static void readFileAndCalculateStatistics(String className)
	{
		// Read and parse File
		final String tmpPath = config.getTestPathSuite() + "Results/";
		final String fileSliceTimeName =
				tmpPath + "SCTimes/" + className.substring(0, className.lastIndexOf(".")) + "_sliceTime.txt";
		final String fileGraphGenerationName =
				tmpPath + "GraphGenTimes/" + className.substring(0, className.lastIndexOf(".")) +
				"_GraphGeneratioTimes1000.txt";

		final List<Double> programSliceTimeValues = new LinkedList<>();
		final File file = new File(fileSliceTimeName);
		try (Scanner in = new Scanner(file)) {
			while (in.hasNextLine())
				programSliceTimeValues.add(Double.parseDouble(in.nextLine()));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		// Calculate AVG and STD

		final double avg = Statistics.average(programSliceTimeValues);
		final double std = Statistics.standardDeviation(programSliceTimeValues);

		System.out.printf("Slice time : %f +- %f miliseconds (ms)\n", avg, std);

		// Calculate Time Generation Time
		final double[] programGenerationEDGTimeValues = new double[1000];

		final File fileGeneration = new File(fileGraphGenerationName);
		try (Scanner in = new Scanner(fileGeneration)) {
			int i = 0;

			while (in.hasNextLine()) {
				final StringTokenizer stk = new StringTokenizer(in.nextLine());
				final String EDGTime = stk.nextToken();

				programGenerationEDGTimeValues[i] = Double.parseDouble(EDGTime);

				i++;
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		AvgStd EDGGen = obtainMinimalWindowCoVAccurated(programGenerationEDGTimeValues, windowSize);

		System.out.printf("Generation time (EDG): %f +- %f miliseconds (ms)\n", EDGGen.getAvg(), EDGGen.getStd());
	}

	private static void parseInTmpXls(String className)
	{
		// Read and parse File
		final String tmpPath = config.getTestPathSuite() + "Results/";
		final String fileSliceTimeName =
				tmpPath + "SCTimes/" + className.substring(0, className.lastIndexOf(".")) + "_sliceTime.txt";

		final List<Double> programSliceTimeValues = new LinkedList<>();
		final File file = new File(fileSliceTimeName);
		try (Scanner in = new Scanner(file)) {
			while (in.hasNextLine())
				programSliceTimeValues.add(Double.parseDouble(in.nextLine()));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		for (Double time : programSliceTimeValues)
		{
			final Row newRow = resultsSheet.createRow(currentRow);
			currentRow++;

			final Cell benchmarkNameCell = newRow.createCell(startColumn);
			final Cell EDGSliceTime = newRow.createCell(startColumn + 1);

			benchmarkNameCell.setCellValue(className);

			EDGSliceTime.setCellValue(Math.round(time * Math.pow(10, 3)) / Math.pow(10, 3));
		}

		currentRow += 2;
	}


	private static class BenchTest {
		private String className;
		private SlicingCriterion sc;

		public BenchTest(String cn)
		{
			this(cn, null);

		}

		public BenchTest(String cn, SlicingCriterion sc)
		{
			this.className = cn;
			this.sc = sc;
		}
	}

	private static class AvgStd {
		private double avg;
		private double std;

		public AvgStd()
		{
			this.avg = 0.0;
			this.std = 0.0;
		}

		public AvgStd(double avg, double std)
		{
			this.avg = avg;
			this.std = std;
		}

		public double getAvg()
		{
			return this.avg;
		}

		public double getStd()
		{
			return this.std;
		}

		public void setAvg(double avg)
		{
			this.avg = avg;
		}

		public void setStd(double std)
		{
			this.std = std;
		}
	}
}
