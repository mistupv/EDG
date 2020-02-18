import java.io.File;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import edg.DotFactory;
import eknife.erlang.EDGFactory;
import eknife.erlang.ErlangFactory;
import edg.graph.EDG;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.slicingAlgorithm.AdvancedAlgorithm;
import edg.slicingAlgorithm.SlicingAlgorithm;
import edg.slicingAlgorithm.StandardAlgorithm;
import eknife.config.Config;
import misc.Misc;

public class Main
{
	private static int slicingAlgorithm = 2;
	private static int stepByStepMilliseconds = 0;
	private static List<TestCase> tests = new LinkedList<TestCase>();
	private static String testPath = "/Users/Fenix/Desktop/Dropbox/Implementaciones/David/Trabajo/e-Knife/Tests/";
	private static final Config config = Config.getConfig();

	public static void main(String[] args)
	{
		int launch = 0;

		switch (launch)
		{
			case 0:
				Main.sergio();
				break;
			case 1:
				Main.example();
				break;
			case 2:
				Main.slicErlang();
				break;
			case 3:
				Main.slicErlangTesting();
				break;
			case 4:
				Main.output1();
				break;
			case 5:
				Main.input1();
				break;
			case 99:
				Main.testCases();
				break;
		}
	}
	public static void sergio()
	{
		final int nodeId;
		final String programName;

//		programName = "test1"; nodeId = 8;		// Slice 1
//		programName = "test1"; nodeId = 41;		// Slice 2
//		programName = "test1"; nodeId = 88;		// Slice 3
//		programName = "test2"; nodeId = 15;		// Slice 1
//		programName = "test2"; nodeId = 64;		// Slice 2
//		programName = "test2"; nodeId = 131;	// Slice 3
//		programName = "test2"; nodeId = 138;	// Slice 4
//		programName = "test3"; nodeId = 28;		// Slice 1
//		programName = "test3"; nodeId = 30;		// Slice 2
//		programName = "test4"; nodeId = ¿?;		// Slice 1
//		programName = "test4"; nodeId = ¿?;		// Slice 2
//		programName = "test5"; nodeId = 9;		// Slice 1
//		programName = "test5"; nodeId = 10;		// Slice 2
//		programName = "test6"; nodeId = ¿?;		// Slice 1
//		programName = "test6"; nodeId = ¿?;		// Slice 2
//		programName = "test7"; nodeId = ¿?;		// Slice 1
//		programName = "test8"; nodeId = ¿?;		// Slice 1
//		programName = "test8"; nodeId = ¿?;		// Slice 2
//		programName = "test9"; nodeId = ¿?;		// Slice 1
//		programName = "test9"; nodeId = ¿?;		// Slice 2
//		programName = "test10"; nodeId = ¿?;	// Slice 1
//		programName = "test10"; nodeId = ¿?;	// Slice 2
//		programName = "test11"; nodeId = 65;	// Slice 1
//		programName = "test12"; nodeId = 244;	// Slice 1
//		programName = "test13"; nodeId = 415;	// Slice 1
//		programName = "test14"; nodeId = ¿?;	// Slice 1
//		programName = "test15"; nodeId = ¿?;	// Slice 1
//		programName = "test15"; nodeId = ¿?;	// Slice 2

//		programName = "prueba1"; nodeId = 39;	// Value edges 
//		programName = "prueba2"; nodeId = 50;	// List comprehension
//		programName = "prueba3"; nodeId = 29;	// Bit string
//		programName = "prueba4"; nodeId = 100;	// Bin comprehension
//		programName = "prueba5"; nodeId = 124;	// List comprehension complex
//		programName = "prueba6"; nodeId = 124;	// Bin comprehension complex
		programName = "prueba7"; nodeId = 81;	// Chars

		final String testPath =  Main.testPath + "sergio/benchmarks";
		final String testName = programName + ".erl";
		final TestCase test = new TestCase(testPath, testName, nodeId);

		Main.openTest(test);
	}
	public static void example()
	{
		final int nodeId;
		final String programName;

//		nodeId = 35; programName = "constraints1";
//		nodeId = 35; programName = "constraints2";
//		nodeId = 35; programName = "constraints3";
//		nodeId = 35; programName = "constraints4";
//		nodeId = 35; programName = "constraints5";
//		nodeId = 28; programName = "constraints6";
//		nodeId = 26; programName = "constraints7";
//		nodeId = 17; programName = "constraints8";
//		nodeId = 46; programName = "constraints9";
//		nodeId = 56; programName = "constraints10";
//		nodeId = 20; programName = "constraints11";
//		nodeId = 20; programName = "constraints12";
//		nodeId = 20; programName = "recursion1";
//		nodeId = 56; programName = "recursion2";
//		nodeId = 24; programName = "recursion3";
		nodeId = 12; programName = "recursion4";

		final String testPath =  Main.testPath + "krinke/" + programName;
		final String testName = programName + ".erl";
		final TestCase test = new TestCase(testPath, testName, nodeId);

		Main.openTest(test);
	}
	public static void slicErlang()
	{
		final int nodeId;
		final String programName;

		nodeId = 5912; programName = "slicErlang";
//		nodeId = 35; programName = "typer";

		final String testPath = Main.testPath + "benchmarks/slicerl";
		final String testName = programName + ".erl";
		final TestCase test = new TestCase(testPath, testName, nodeId);

		Main.openTest(test);
	}
	public static void slicErlangTesting()
	{
//		for (int id = 5036; id < 10200; id++)
		{
//			System.out.println(id);

			int nodeId = 1;// id;
			String programName = "slicErlang";

			final String testPath = Main.testPath + "benchmarks/slicerl";
			final String testName = programName + ".erl";
			final TestCase test = new TestCase(testPath, testName, nodeId);

			Main.openTest(test);
		}
	}
	public static void output1()
	{
		final int nodeId;
		final String programName;

		nodeId = 15; programName = "code";

		final String testPath = Main.testPath + "problems/output1";
		final String testName = programName + ".erl";
		final TestCase test = new TestCase(testPath, testName, nodeId);

		Main.openTest(test);
	}
	public static void input1()
	{
		final int nodeId;
		final String programName;

		nodeId = 19; programName = "code";

		final String testPath = Main.testPath + "problems/input1";
		final String testName = programName + ".erl";
		final TestCase test = new TestCase(testPath, testName, nodeId);

		Main.openTest(test);
	}
	public static void openTest(TestCase test)
	{
 		final String programPath = test.getTestPath() + File.separator + test.getTestName();
		final EDG EDG = EDGFactory.createEDG(programPath, true, true);
		Main.generateDot(EDG);

		final int nodeId = test.getNodeId();
		final Node slicingCriterium = EDG.findNodeByData(new NodeInfo(nodeId, null, 0, null), new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				return o1.getId() - o2.getId();
			}
		});
		final List<Node> slice = Main.sliceEDG(EDG, slicingCriterium);
		Main.generateDot(EDG, slicingCriterium, slice);

		final File originalFile = new File(programPath);
		System.out.println(Misc.read(originalFile) + "\n\n\n");

		final File sliceFile = ErlangFactory.createErlangFile(test.getTestPath(), EDG, slice);
		System.out.println(Misc.read(sliceFile));
		Misc.delete(sliceFile);
	}
	private static void openTest2(TestCase test)
	{
 		final String programPath = test.getTestPath() + File.separator + test.getTestName();
		final EDG EDG = EDGFactory.createEDG(programPath, true, true);
		Main.generateDot(EDG);

// TODO Delete
int[] idsFallan = { 7381, 7511, 8035 };
int num = 3474;
//for (int id = num; id <= 10200; id++)
for (int id : idsFallan)
{
System.out.print(id);
long startTime = System.currentTimeMillis();
		final int nodeId = id;
//		final int nodeId = test.getNodeId();
		final Node slicingCriterium = EDG.findNodeByData(new NodeInfo(nodeId, null, 0, null), new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				return o1.getId() - o2.getId();
			}
		});
		final List<Node> slice = Main.sliceEDG(EDG, slicingCriterium);

		if (Main.stepByStepMilliseconds <= 0)
			Main.generateDot(EDG, slicingCriterium, slice);
		else
		{
			final List<Node> sliceNodes = new LinkedList<Node>();
			for (Node sliceNode : slice)
			{
				sliceNodes.add(sliceNode);
				Main.generateDot(EDG, slicingCriterium, sliceNodes);
				Misc.wait(Main.stepByStepMilliseconds);
			}
		}

//		final File originalFile = new File(program);
//		System.out.println(Misc.read(originalFile) + "\n\n\n");

		ErlangFactory.createErlangFile(test.getTestPath(), EDG, slice);
//		System.out.println(Misc.read(sliceFile));
//		Misc.delete(sliceFile);
// TODO Delete
long endTime = System.currentTimeMillis();
double time = ((endTime - startTime) / 1000.0) / 60.0;
double roundTime = Math.round(time * 100) / 100.0;
System.out.println(" " + roundTime);
}
	}

	public static void testCases()
	{
		Main.createTestCases();
		for (TestCase test : Main.tests)
			Main.checkTest(test);
	}
	private static void createTestCases()
	{
		final String testsPath = Main.config.getProgramPath() + ".." + File.separator + "Tests" + File.separator + "simple" + File.pathSeparator;
		final File tests = new File(testsPath);
		final List<File> testsFolders = Misc.getFolders(tests, false);

		for (File testFolder : testsFolders)
		{
			final List<File> testSources = Misc.getFiles(testFolder, new String[] { ".erl" }, false);
			final File testSource = testSources.get(0);
			final String testSourcePath = testSource.getParentFile().getAbsolutePath();
			final String testSourceName = testSource.getName();
			final List<File> testSlicesFolders = Misc.getFolders(testFolder, false);

			for (File testSliceFolder : testSlicesFolders)
			{
				final String testSliceFolderName = testSliceFolder.getName();
				final int nodeId = Integer.parseInt(testSliceFolderName);

				Main.tests.add(new TestCase(testSourcePath, testSourceName, nodeId));
			}
		}
	}
	private static void checkTest(TestCase test)
	{
		final String programPath = test.getTestPath() + File.separator + test.getTestName();
		final EDG EDG = EDGFactory.createEDG(programPath, true, true);
		final Node slicingCriterium = EDG.findNodeByData(new NodeInfo(test.getNodeId(), null, 0, null), new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				return o1.getId() - o2.getId();
			}
		});
		final List<Node> slice = Main.sliceEDG(EDG, slicingCriterium);
		final File obtainedFile = ErlangFactory.createErlangFile(test.getTestPath() + File.separator + test.getNodeId(), EDG, slice);

		// Compare slice with gold standard
		final String testPath = test.getTestPath();
		final File expectedFile = new File(testPath + File.separator + test.getNodeId() + File.separator + "Expected.erl");
		final String expectedText = Misc.read(expectedFile);
		final String obtainedText = Misc.read(obtainedFile);

		if (!expectedText.equals(obtainedText))
		{
			System.out.println("======================");
			System.out.println(test.getTestName() + " - " + test.getNodeId());
			System.out.println("======================");
			System.out.println(Misc.read(expectedFile));
			System.out.println("----------------------");
			System.out.println(Misc.read(obtainedFile));
			System.out.println("======================");
		}
	}

	private static File generateDot(EDG EDG)
	{
		return Main.generateDot(EDG, null, new LinkedList<Node>());
	}
	private static File generateDot(EDG EDG, Node slicingCriterium, List<Node> slice)
	{
		final String dotPath = Main.config.getTemporaryPath() + File.separator + "graph.dot";

		return DotFactory.createDot(dotPath, EDG, slicingCriterium, slice);
	}
	private static List<Node> sliceEDG(EDG EDG, Node slicingCriterium)
	{
		final SlicingAlgorithm slicingAlgorithm = Main.getSlicingAlgorithm(EDG);
		final List<Node> slice = slicingAlgorithm.slice(slicingCriterium);

		slice.remove(EDG.getRootNode());

		return slice;
	}

	private static SlicingAlgorithm getSlicingAlgorithm(EDG EDG)
	{
		switch (Main.slicingAlgorithm)
		{
			case 1:
				return new StandardAlgorithm();
			case 2:
				return new AdvancedAlgorithm(EDG, true, true);
			default:
				throw new RuntimeException("Slicing algorithm not contemplated: " + Main.slicingAlgorithm);
		}
	}
}