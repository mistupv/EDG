import java.io.File;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eknife.edg.EdgeInfo;
import eknife.edg.traverser.GraphTraverser;
import eknife.edg.traverser.EdgeTraverser.Phase;
import eknife.sergio.ErlPermaConnect;
import test.Test;
import eknife.edg.slicingAlgorithm.SlicingORBS;
import eknife.config.Config;
import eknife.edg.EDG;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.generator.DependenceGenerator;
import eknife.edg.generator.DotGenerator;
import eknife.edg.generator.ErlangGenerator;
import eknife.edg.generator.GraphGenerator;
import eknife.edg.slicingAlgorithm.GraphPermutator;
import eknife.edg.slicingAlgorithm.SlicingAlgorithm;
import eknife.edg.slicingAlgorithm.SlicingAlgorithm1;
import eknife.edg.slicingAlgorithm.SlicingAlgorithm2;
import eknife.erlang.Launcher;
import eknife.misc.Misc;

public class Main
{
	//--------------------------------------------Added by Sergio Para ORBS--------------------------------------------
	
	private static ErlPermaConnect launcher = new ErlPermaConnect(); 
	private static String pulpath = "/Users/serperu/Desktop/Trabajo/Proyecto_Slicing/15_Programas/Generadores_de_entradas/";
	private static long time_start;
	private static long time_end;
	private static double time_expent;
	private static final int numDelta = 10;
	private static final int combiElems = 1;  //Numero de nodos eliminados
	
	//-----------------------------------------------------------------------------------------------------------------
	
	private static int slicingAlgorithm = 2;
	private static int stepByStepMilliseconds = 0;
	private static List<TestCase> tests = new LinkedList<TestCase>();
	private static String testPath = "/Users/serperu/Desktop/ORBS-Knife/Tests/";

	public static void main(String[] args)
	{
		int launch = 0;

		switch (launch)
		{
			case 0:
				//Main.bucleRecuento();
				Main.sergio();
				//Main.sergioBucle();
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

//		programName = "bench1"; nodeId = 8;		// Slice 1 b1
//		programName = "bench2"; nodeId = 8;		// Slice 2 b2
//		programName = "bench3"; nodeId = 8;		// Slice 3 b3

//		programName = "bench5"; nodeId = 8;		// Slice 2 b5
//		programName = "bench6"; nodeId = 8;		// Slice 3 b6
//		programName = "bench6"; nodeId = 15;	// Slice 4 b7

//		programName = "test1"; nodeId = 8;		// Slice 1 b1
//		programName = "test1"; nodeId = 41;		// Slice 2 b2
//		programName = "test1"; nodeId = 88;		// Slice 3 b3
//X		programName = "test2"; nodeId = 15;		// Slice 1 Candidato a la eliminación
//		programName = "test2"; nodeId = 64;		// Slice 2 b5
//		programName = "test2"; nodeId = 131;	// Slice 3 b6
//		programName = "test2"; nodeId = 138;	// Slice 4 b7
//		programName = "test3"; nodeId = 28;		// Slice 1 b8
//		programName = "test3"; nodeId = 30;		// Slice 2 b9
//		programName = "test4"; nodeId = 15;		// Slice 1 b4
//		programName = "test5"; nodeId = 9;		// Slice 1 b10
//X		programName = "test5"; nodeId = 10;		// Slice 2 Candidato a la eliminación
//X		programName = "test6"; nodeId = ¿?;		// Slice 1 
//X		programName = "test6"; nodeId = ¿?;		// Slice 2
//X		programName = "test7"; nodeId = ¿?;		// Slice 1
//X		programName = "test8"; nodeId = ¿?;		// Slice 1
//X		programName = "test8"; nodeId = ¿?;		// Slice 2
//X		programName = "test9"; nodeId = ¿?;		// Slice 1
//X		programName = "test9"; nodeId = ¿?;		// Slice 2
//X		programName = "test10"; nodeId = ¿?;	// Slice 1
//X		programName = "test10"; nodeId = ¿?;	// Slice 2
//		programName = "test11"; nodeId = 65;	// Slice 1 b11
//		programName = "test12"; nodeId = 244;	// Slice 1 b12
//		programName = "test13"; nodeId = 415;	// Slice 1 b13
//X		programName = "test14"; nodeId = 10;	// Slice 1
//		programName = "test15"; nodeId = 73;	// Slice 1 b14
//		programName = "test15"; nodeId = 348;	// Slice 2 b15

//		programName = "test16"; nodeId = 41;	// Slice 1 b16 
//		programName = "test17"; nodeId = ¿?;	// Test17 ets
//		programName = "test18"; nodeId = 56;	// Slice 1 b17
//		programName = "test18"; nodeId = 72;	// Slice 2 b18
//		programName = "test18"; nodeId = 88;	// Slice 3 b19
//X		programName = "test18"; nodeId = 91;	// Slice 4 No incluido
//		programName = "test19"; nodeId = 392;	// Slice 1 b20
//		programName = "test20"; nodeId = 41;	// Slice 1 b21
	
//		programName = "b1Tama"; nodeId = 8;
//		programName = "b2Tama"; nodeId = 8;
//		programName = "b3Tama"; nodeId = 8;
//		programName = "b5Tama"; nodeId = 8;
//		programName = "b6Tama"; nodeId = 8;
//		programName = "b7Tama"; nodeId = 8;
//		programName = "b8Tama"; nodeId = 28;
//		programName = "b9Tama"; nodeId = 29;
//		programName = "b10Tama"; nodeId = 9;
//		programName = "b11Tama"; nodeId = 65;
//		programName = "b16Tama"; nodeId = 28;
//		programName = "b17Tama"; nodeId = 8;
//		programName = "b18Tama"; nodeId = 8;
//		programName = "b19Tama"; nodeId = 8;
		programName = "prueba"; nodeId = 46;

		final String testPath =  Main.testPath + "sergio/benchmarks";
		final String testName = programName + ".erl";
		final TestCase test = new TestCase(testPath, testName, nodeId);
		long tiempo_inicial = System.currentTimeMillis();
		Main.openTest(test);
		long tiempo_final = System.currentTimeMillis();
		long tiempoTotal = tiempo_final - tiempo_inicial;
		System.out.println(tiempoTotal);
		//Test executionTime = new Test("e-Knife");
		//executionTime.setExecutionTime(tiempoTotal);
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
 		final String program = test.getTestPath() + File.separator + test.getTestName();
		final OtpErlangObject response = Main.obtainAST(program);
		final EDG EDG = Main.generateEDG(response);
		Main.generateDot(EDG);

		final int nodeId = EDG.getSC();
		//final int nodeId = test.getNodeId();
		final Node slicingCriterium = EDG.findNodeByData(new NodeInfo(nodeId, null), new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				return o1.getId() - o2.getId();
			}
		});
		final List<Node> slice = Main.sliceEDG(EDG, slicingCriterium);
		Main.generateDot(EDG, slicingCriterium, slice);

		final File originalFile = new File(program);
		System.out.println(Misc.read(originalFile) + "\n\n\n");

		final File sliceFile = Main.saveSlice(test.getTestPath(), EDG, slice, true);
		System.out.println(Misc.read(sliceFile));
		Misc.delete(sliceFile);
	}
	private static void openTest2(TestCase test)
	{
 		final String program = test.getTestPath() + File.separator + test.getTestName();
		final OtpErlangObject response = Main.obtainAST(program);
		final EDG EDG = Main.generateEDG(response);
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
		final Node slicingCriterium = EDG.findNodeByData(new NodeInfo(nodeId, null), new Comparator<NodeInfo>() {
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

		final File sliceFile = Main.saveSlice(test.getTestPath(), EDG, slice, false);
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
		final File tests = Config.getTestsFile();
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
		final String program = test.getTestPath() + File.separator + test.getTestName();
		final OtpErlangObject response = Main.obtainAST(program);
		final EDG EDG = Main.generateEDG(response);
		final Node slicingCriterium = EDG.findNodeByData(new NodeInfo(test.getNodeId(), null), new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				return o1.getId() - o2.getId();
			}
		});
		final List<Node> slice = Main.sliceEDG(EDG, slicingCriterium);
		final File obtainedFile = Main.saveSlice(test.getTestPath() + File.separator + test.getNodeId(), EDG, slice, true);

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

	private static OtpErlangObject obtainAST(String program)
	{
		final Launcher launcher = new Launcher();

		return launcher.launch("ast", "getAST", program);
	}
	private static EDG generateEDG(OtpErlangObject response)
	{
		final OtpErlangTuple tuple = (OtpErlangTuple) response;
		final OtpErlangList body = (OtpErlangList) tuple.elementAt(1);
		final GraphGenerator graphGenerator = new GraphGenerator();
		final EDG EDG = graphGenerator.generate(body);
		final DependenceGenerator dependenceGenerator = new DependenceGenerator();

		dependenceGenerator.generateEdges(EDG);

		return EDG;
	}
	private static File generateDot(EDG EDG)
	{
		return Main.generateDot(EDG, null, new LinkedList<Node>());
	}
	private static File generateDot(EDG EDG, Node slicingCriterium, List<Node> slice) // MODIFIED TO GENERATE PDF
	{
		final String dotPath = Config.getTemporalPath() + File.separator + "graph.dot";
		final String pdfPath = Config.getTemporalPath() + File.separator + "graph.pdf";
		final String tempDotPath = Config.getTemporalPath() + File.separator + "temp.dot";
		final File dotFile = new File(dotPath);
		final File pdfFile = new File(pdfPath);
		final File tempDotFile = new File(tempDotPath);
		final DotGenerator dotGenerator = new DotGenerator();

		dotGenerator.generate(EDG, slicingCriterium, slice, tempDotFile, dotFile, pdfFile);

		return dotFile;
	}
	private static List<Node> sliceEDG(EDG EDG, Node slicingCriterium)
	{
		final SlicingAlgorithm slicingAlgorithm = Main.getSlicingAlgorithm(EDG);
		final List<Node> slice = slicingAlgorithm.slice(slicingCriterium);

		slice.remove(EDG.getRootNode());

		return slice;
	}
	private static File saveSlice(String testPath, EDG EDG, List<Node> slice, boolean saveFile)
	{
// TODO Arreglame
		final File tempFile = new File(Config.getLauncherPath() + "tempSliced.erl");
		final File obtainedFile = new File(testPath + File.separator + "Obtained.erl");

		final ErlangGenerator erlangGenerator = new ErlangGenerator();
		final OtpErlangList program = erlangGenerator.generate(EDG, slice);

if (!saveFile)
	return null;

		final Launcher launcher = new Launcher();

		launcher.launch("saver", "save", program);
		Misc.moveFile(tempFile, obtainedFile);

		final String text = Misc.read(obtainedFile).trim();
		final int newLineIndex = text.indexOf("-module");
		final String newText = text.substring(newLineIndex);

		Misc.write(obtainedFile, newText, false);

		return obtainedFile;
	}

	private static SlicingAlgorithm getSlicingAlgorithm(EDG EDG)
	{
		switch (Main.slicingAlgorithm)
		{
			case 1:
				return new SlicingAlgorithm1();
			case 2:
				return new SlicingAlgorithm2(EDG, Phase.Slicing);
			default:
				throw new RuntimeException("Slicing algorithm not contempled: " + Main.slicingAlgorithm);
		}
	}
	//Added by Sergio
	private static void saveSliceSergio(String testPath, EDG EDG, List<Node> slice, boolean saveFile, int numTest, int numSlice)
	{
		String ruta = "test"+numTest+"/test"+numTest+"Slice"+numSlice+"Tmp.erl";
		final File tempFile = new File(testPath + File.separator + "tempSliced.erl");
		final File obtainedFile = new File(testPath + File.separator + ruta);
	
		final ErlangGenerator erlangGenerator = new ErlangGenerator();
		final OtpErlangList program = erlangGenerator.generate(EDG, slice);
	
		Main.launcher.invokeErlang("launchCode", "save", program);
		Misc.moveFile(tempFile, obtainedFile);
	
		final String text = Misc.read(obtainedFile).trim();
		final int newLineIndex = text.indexOf("-module");
		final String newText = text.substring(newLineIndex);
	
		Misc.write(obtainedFile, newText, false);
	}
	private static void saveResultado(String testPath, EDG EDG, List<Node> slice, boolean saveFile)
	{
		final File tempFile = new File(testPath + File.separator + "tempSliced.erl");
		final File obtainedFile = new File(Main.testPath + "../Program/tmp/result.erl");
	
		final ErlangGenerator erlangGenerator = new ErlangGenerator();
		final OtpErlangList program = erlangGenerator.generate(EDG, slice);
	
		Main.launcher.invokeErlang("launchCode", "save", program);
		Misc.moveFile(tempFile, obtainedFile);
	
		final String text = Misc.read(obtainedFile).trim();
		final int newLineIndex = text.indexOf("-module");
		final String newText = text.substring(newLineIndex);
	
		Misc.write(obtainedFile, newText, false);
	}
	public static void sergioBucle()
	{
		String origen="";
		String destino="/Users/serperu/Desktop/ORBS-Knife/Tests/problems/code/code.erl"; //Donde este code.erl
		int line = 0;
		String var = "";
		String modulo = "";
		//for(int i=1;i<=19;i++)
		for(int i=20;i<=20;i++)
		{
			if( i == 6 || i == 7 || i == 8 || i == 9 || i == 10 || i == 14 || i == 17 || i == 16 )
				continue;
			int k;
			switch (i)
			{
				case 1:
				case 18:
					k=3; 
					break;
				case 2: 
					k=4; 
					break;
				case 4:
				case 5:
				case 7:
				case 11:
				case 12:
				case 13:
				case 14: 
				case 16:
				case 19:
				case 20:
					k=1; 
					break;
				default: 
					k=2; 
					break;
			}
			for(int j=1;j<=k;j++)
			//for(int j=1;j<=1;j++)
			{
				switch (i)
				{
					case 1:
						switch (j) {
							case 1: line=17; var="C"; break;
							case 2: line=17; var="C"; break;
							default: line=17; var="C"; break;
						}
						break;
					case 2:
						switch (j) {
							case 1: line=18; var="D"; break;
							case 2: line=17; var="C"; break;
							case 3: line=17; var="C"; break;
							default: line=17; var="D"; break;
						} break;
					case 3:
						switch (j) {
							case 1: line=21; var="C"; break;
							default: line=16; var="D"; break;
						} break;
					case 4:
						switch (j) {
							case 1: line=18; var="Abb"; break;
							default: line=17; var="Res"; break;
						} break;
					case 5:
						switch (j) {
							case 1: line=18; var="Deposits"; break;
							default: line=18; var="Withdraws"; break;
						} break;
					case 6:
						switch (j) {
							case 1: line=18; var="HexList"; break;
							default: line=17; var="DirectHex"; break;
						}break;
					case 7: line=17; var="D"; break;
					case 8:
						switch (j) {
							case 1: line=23; var="Value"; break;
							default: line=23; var="Belongs"; break;
						} break;
					case 9:
						switch (j) {
							case 1: line=22; var="Negation"; break;
							default: line=22; var="Inversion#complex.img"; break;
						} break;
					case 10:
						switch (j) {
							case 1: line=32; var="Encod"; break;
							default: line=33; var="Decod"; break;
						} break;
					case 11: line=29; var="A"; break;
					case 12: line=50; var="Shown"; break;
					case 13: line=69; var="Res"; break;
					case 14: line=27; var="List"; break;
					case 15:
						switch (j) {
							case 1: line=19; var="BS"; break;
							default: line=39; var="A"; break;
						} break;
					case 16: line = 11; var="NewI"; break;
					case 17: line = 10; var="Games"; break;
					case 18: 
						switch (j) {
						case 1: line=6; var="V"; break;
						case 2: line=5; var="W"; break;
						default: line=13; var="Z"; break;
					} break;
					case 19: line = 81; var="Year"; break;
					case 20: line = 12; var="DB"; break;
					default:
						break;
				}
				
				origen = Main.pulpath+"test"+i+"/test"+i+"Slice"+j+".erl";
				modulo = "test"+i+"Slice"+j;
				// Llamada a erlang para que convierta el codigo con sustituir
				final String[] arguments = {Integer.toString(i), modulo, Integer.toString(line), var};
				// Cierro y abro la conexion en cada test porque se para sin sentido
				Main.launcher.closeServer();
				Main.launcher.getReadyServer();
				
				Main.launcher.invokeErlang("sustituir", "main", arguments);
				origen = Main.pulpath+"test"+i+"/test"+i+"Slice"+j+"Tmp.erl";
				Misc.copyFile(new File(origen), new File(destino));
				
				Main.PermutORBS(i, j);
				//Main.CalcularCombinaciones(i, j);
				
				final String[] args = {Main.testPath + "../Program/tmp/", var};
				Main.launcher.invokeErlang("reverse_change", "main", args);
				Misc.delete(new File(origen));
			}
		}
		Main.launcher.closeServer();
	}
	private static void ORBS(int numTest, int numSlice)
	{
		final String testPath = Main.testPath + "problems" + File.separator + "code";
 		final String program = testPath + File.separator + "code.erl";
 		final OtpErlangObject response = Main.launcher.invokeErlang("launchCode", "getAST",program);
 		final EDG EDG = Main.generateEDG(response);
		final SlicingORBS slicingAlgorithm = new SlicingORBS(EDG);
		List<Node> slice;
		int i = 1;
		File f = new File("/Users/serperu/Desktop/Trabajo/Proyecto_Slicing/Resultados.txt");
		Misc.createFile(f);
		Misc.write(f, "----- Test"+numTest+"Slice"+numSlice+" -----\n", true);
		System.out.println("----- Test"+numTest+"Slice"+numSlice+" -----");
		time_start = System.currentTimeMillis();
		while ((slice = slicingAlgorithm.ORBS3()) != null)
		{
			//Guardarlo donde yo quiera
			Main.saveSliceSergio(Main.pulpath, EDG, slice, true, numTest, numSlice); 
			if (i%70 == 0)
			{
				Main.launcher.closeServer();
				Main.launcher.getReadyServer();
			}
			//------------------------------------
			final String[] arguments = {Integer.toString(numDelta),Integer.toString(numTest),Integer.toString(numSlice)};
			final OtpErlangObject result = Main.launcher.invokeErlang("testDelta", "main", arguments);
			final OtpErlangTuple tuple = (OtpErlangTuple) result;
			final OtpErlangObject primero = (OtpErlangObject) tuple.elementAt(1);
			String aux = primero.toString();
			switch (aux)
			{	
				case "ok":
					Node destroyed = slicingAlgorithm.getCurrent();
					Node paired = slicingAlgorithm.getPaired();
					Node triple = slicingAlgorithm.getTriple();
					if(paired != null) // Solo si vamos de 2 en 2
						slicingAlgorithm.addUnnecessary(paired);
					if(triple != null) // Solo de 3 en 3
						slicingAlgorithm.addUnnecessary(triple);
					slicingAlgorithm.addUnnecessary(destroyed);
					slicingAlgorithm.overwriteSlice(destroyed);
					break;
				case "errorCompilacion":
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Tmp.txt"));
					break;
					
				case "error": // Si es error es unicamente porque no han coincidido resultados
				case "errorEjecutar": 
				default:
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Tmp.txt"));
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Slice"+numSlice+"Tmp.txt"));
					break;
			}
			i++;
		}
		time_end = System.currentTimeMillis();
		time_expent = (time_end-time_start)/1000;
		System.out.println("Total de combinaciones de nodos procesadas:"+(i-1));
		slice = slicingAlgorithm.getNodes();
		Main.saveResultado(Main.pulpath, EDG, slice, true);
		Misc.write(f, "Total de combinaciones de nodos procesadas:"+(i-1)+"\n", true);
		System.out.println("Tiempo invertido: "+time_expent+" segundos");
		Misc.write(f, "Tiempo invertido: "+time_expent+" segundos\n", true);
		List<Node> eliminables = slicingAlgorithm.getUnnecessary();
		int j=0;
		int cont=0;
		while(j<eliminables.size())
		{	
			Node n = eliminables.get(j);
			if (!isFictitious(n)) {
				System.out.println(n.getName()+" Linea (Solo para variables): "+n.getLine());
				Misc.write(f, n.getName()+" Linea (Solo para variables): "+n.getLine()+"\n", true);
				cont++;
			}
			j++;
		}
		System.out.println("("+cont+") Nodos eliminados en "+slicingAlgorithm.getEliminations()+" eliminaciones");
		Misc.write(f, "("+cont+") Nodos eliminados en "+slicingAlgorithm.getEliminations()+" eliminaciones\n", true);
	}
	public static boolean isFictitious(Node node)
	{
		boolean fictitious;
		switch(node.getName())
		{
			case "(var)\\n_":
			case "(atom)\\nundef":
			case "return":
			case "body":
			case "(atom)\\nfunundef":
				fictitious = true;
				break;
			default:
				fictitious = false;
				break;
		}
		return fictitious;
	}
	public static int sergioNodos()
	{
		final String testPath = Main.testPath + "problems" + File.separator + "code";
 		final String program = testPath + File.separator + "code.erl";
 		final OtpErlangObject response = Main.launcher.invokeErlang("launchCode", "getAST",program);
 		final EDG EDG = Main.generateEDG(response);
 		List<Node> nodes = EDG.getNodes();
 		int NumNodes = 0;
 		for(int index = 1;index < nodes.size();index++)
 		{
 			Node node = nodes.get(index);
 			String val = node.getName();
 			switch (val) 
			{
 				case "body":
 				case "return":
				case "(var)\\n_":
				case "(atom)\\nundef":
				case "(atom)\\nfunundef":
					break;
				default: 
					if (val.length() >= 20 && val.substring(0,19).equals("function\\nfunundef/"))
						break;
					else if (isFatherUndef(node))
						break;
					else
					{
						NumNodes++; 
						break;
					}
			}
 		}
 		return NumNodes;
	}
	public static void bucleRecuento()
	{
		String origenO="/Users/serperu/Desktop/INFO_POR_FASES/Originales/"; //bench1-bench16
		String origenAux="/Users/serperu/Desktop/INFO_POR_FASES/Auxiliar/";
		String origenM="/Users/serperu/Desktop/INFO_POR_FASES/Manuales/"; //b1 - b21
		String origenSD="/Users/serperu/Desktop/INFO_POR_FASES/SlicersDT/David/";//b1David - b21David
		String origenST="/Users/serperu/Desktop/INFO_POR_FASES/SlicersDT/Tama/";//b1Tama - b21Tama
		String origenF="/Users/serperu/Desktop/INFO_POR_FASES/Fusion/"; //b1 - b12
		String origenE="/Users/serperu/Desktop/INFO_POR_FASES/Final/"; //b1 - b21
		String destino="/Users/serperu/Desktop/ORBS-Knife/Tests/problems/code/code.erl";
		String origen="";
		File f = new File("/Users/serperu/Desktop/INFO_POR_FASES/NodosDeTodos.txt");
		Misc.createFile(f);
		Misc.write(f, "--- Nodos de los programas Originales ---\n", true);
		for(int i=1;i<=16;i++)
		{
			origen =origenO+"bench"+i+".erl";
			Misc.copyFile(new File(origen), new File(destino));
			int Nodes = sergioNodos();
			Misc.write(f, "El programa bench"+i+" tiene "+Nodes+" nodos\n", true);
		}
		/*Misc.write(f, "--- Nodos de los programas Auxiliares ---\n", true);
		for(int i=1;i<=20;i++)
		{
			if (i != 4 && i != 12 && i != 13 && i != 14 && i != 15 && i != 20){
				origen =origenAux+"b"+i+".erl";
				Misc.copyFile(new File(origen), new File(destino));
				int Nodes = sergioNodos();
				Misc.write(f, "El slice b"+i+" tiene "+Nodes+" nodos\n", true);
			}
		}*/
		Misc.write(f, "--- Nodos de los programas Manuales ---\n", true);
		for(int i=1;i<=21;i++)
		{
			origen =origenM+"b"+i+".erl";
			Misc.copyFile(new File(origen), new File(destino));
			int Nodes = sergioNodos();
			Misc.write(f, "El slice b"+i+" tiene "+Nodes+" nodos\n", true);
		}
		Misc.write(f, "--- Nodos de los programas Slicer David ---\n", true);
		for(int i=1;i<=21;i++)
		{
			origen =origenSD+"b"+i+"David.erl";
			Misc.copyFile(new File(origen), new File(destino));
			int Nodes = sergioNodos();
			Misc.write(f, "El slice b"+i+"David tiene "+Nodes+" nodos\n", true);
		}
		/*
		Misc.write(f, "--- Nodos de los programas Slicer Tama ---\n", true);
		for(int i=1;i<=20;i++)
		{
			if (i != 4 && i != 12 && i != 13 && i != 14 && i != 15 && i != 20){
				origen =origenST+"b"+i+"Tama.erl";
				Misc.copyFile(new File(origen), new File(destino));
				int Nodes = sergioNodos();
				Misc.write(f, "El slice b"+i+"Tama tiene "+Nodes+" nodos\n", true);
			}
		}*/
		/*Misc.write(f, "--- Nodos de los programas Fusion ---\n", true);
		for(int i=1;i<=20;i++)
		{
			origen =origenF+"b"+i+".erl";
			Misc.copyFile(new File(origen), new File(destino));
			int Nodes = sergioNodos();
			Misc.write(f, "El slice b"+i+" tiene "+Nodes+" nodos\n", true);
		}
		Misc.write(f, "--- Nodos de los programas Finales ---\n", true);
		for(int i=1;i<=20;i++)
		{
			origen =origenE+"b"+i+".erl";
			Misc.copyFile(new File(origen), new File(destino));
			int Nodes = sergioNodos();
			Misc.write(f, "El slice b"+i+" tiene "+Nodes+" nodos\n", true);
		}*/
	}
	private static boolean isFatherUndef(Node node)
	{
		Node father = GraphTraverser.getParent(node, EdgeInfo.Type.Control);
		if (father == null)
			return false;
		else if (father.getName().length()>=20 && father.getName().substring(0,19).equals("function\\nfunundef/"))
			return true;
		else
			return isFatherUndef(father);
	}
	
	private static void PermutORBS(int numTest, int numSlice)
	{
		final String testPath = Main.testPath + "problems" + File.separator + "code";
 		final String program = testPath + File.separator + "code.erl";
 		final OtpErlangObject response = Main.launcher.invokeErlang("launchCode", "getAST",program);
 		final EDG EDG = Main.generateEDG(response);
		int i = 1;
		File f = new File("/Users/serperu/Desktop/Trabajo/Proyecto_Slicing/Resultados.txt");
		Misc.createFile(f);
		Misc.write(f, "----- Test"+numTest+"Slice"+numSlice+" -----\n", true);
		System.out.println("----- Test"+numTest+"Slice"+numSlice+" -----");
		time_start = System.currentTimeMillis();
		
		final GraphPermutator.Filter nodeFilter = new GraphPermutator.Filter()
		{
			public boolean accept(Node node)
			{
				String name = node.getData().getName();
				NodeInfo.Type type = node.getData().getType();

				switch (type)
				{
					case Body:
					case Return:
					case Guard:
					case ListComprehensionResult:
						return false;
					default:
						break;
				}

				if (name == null)
					return true;
				if (type == NodeInfo.Type.Variable && name.equals("_"))
					return false;
				if (type == NodeInfo.Type.Atom && name.equals("undef"))
					return false;
				if (type == NodeInfo.Type.Atom && name.equals("funundef"))
					return false;

				return true;
			}
		};
		final GraphPermutator.Filter subtreeFilter = new GraphPermutator.Filter()
		{
			public boolean accept(Node node)
			{
				String name = node.getData().getName();
				NodeInfo.Type type = node.getData().getType();

				if (type == NodeInfo.Type.FunctionCall)
				{
					Node nodeCall = GraphTraverser.getChild(node, 0);
					String nameCall = nodeCall.getData().getName();
					if (nameCall.equals("slice") || nameCall.equals("slice_void"))
						return false;
				}

				if (name == null)
					return true;
				if (type == NodeInfo.Type.Function && name.equals("slice"))
					return false;
				if (type == NodeInfo.Type.Function && name.equals("slice_void"))
					return false;
				if (type == NodeInfo.Type.Function && name.equals("funundef"))
					return false;

				return true;
			}
		};
		final GraphPermutator gp = new GraphPermutator(EDG.getNodes().get(0), combiElems, true, nodeFilter, subtreeFilter);
		List<Node> finalSlice = null;
		long time1 = 0;
		long time2 = 0;
		long time3 = 0;
		while (true)
		{
			boolean hasNext = gp.next();
			if (!hasNext)
				break;
			List<Node> slice = gp.getRemainingNodes();

			Main.saveSliceSergio(Main.pulpath, EDG, slice, true, numTest, numSlice); 

			if (i%30 == 0)
			{
				Main.launcher.closeServer();
				Main.launcher.getReadyServer();
			}
			//Usar y borrar
			if(i == 20) 
			{
				time1 = System.currentTimeMillis();
				System.out.println("Tiempo antes de 20: "+(time1-time_start));
			}
			if(i == 99)
			{
				time2 = System.currentTimeMillis();
				System.out.println("Tiempo entre 20 y 99: "+(time2-time1));
			}
			if(i == 178)
			{
				time3 = System.currentTimeMillis();
				System.out.println("Tiempo entre 99 y 178: "+(time3-time2));
			}
				
			//------------------------------------
			final String[] arguments = {Integer.toString(numDelta),Integer.toString(numTest),Integer.toString(numSlice)};
			final OtpErlangObject result = Main.launcher.invokeErlang("testDelta", "main", arguments);
			final OtpErlangTuple tuple = (OtpErlangTuple) result;
			final OtpErlangObject primero = (OtpErlangObject) tuple.elementAt(1);
			String aux = primero.toString();
			switch (aux)
			{
				// En caso de que sea otro tipo de error (Error Compilacion, lineaNocoincide, 
				// error de abrir un fichero q no existe) habra otro tipo de error
				// Modificados comparator.erl (throw(archiv)) y test1Generator2 (Añadido el param SliceLine)
				
				case "ok":
					finalSlice = gp.getRemainingNodes();
					gp.remove();
					break;
				case "errorCompilacion":
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Tmp.txt"));
					break;
					
				case "error": // Si es error es unicamente porque no han coincidido resultados
				case "errorEjecutar": 
				default:
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Tmp.txt"));
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Slice"+numSlice+"Tmp.txt"));
					break;
			}
			//Usar y borrar
			if(i == 20) time1 = System.currentTimeMillis();
			if(i == 99) time2 = System.currentTimeMillis();
			if(i == 178) time3 = System.currentTimeMillis();
			//------------
			i++;
		}
		time_end = System.currentTimeMillis();
		System.out.println("Tiempo entre 178 y final: "+(time_end-time3)+"\n");
		time_expent = (time_end-time_start)/1000;
		if (finalSlice != null)
			Main.saveResultado(Main.pulpath, EDG, finalSlice, true);
		else 
			Misc.copyFile(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Slice"+numSlice+".erl"), new File(Main.testPath + "../Program/tmp/result.erl"));
		System.out.println("Total de combinaciones de nodos procesadas:"+(i-1));
		Misc.write(f, "Total de combinaciones de nodos procesadas:"+(i-1)+"\n", true);
		
		System.out.println("Tiempo invertido: "+time_expent+" segundos");
		Misc.write(f, "Tiempo invertido: "+time_expent+" segundos\n", true);
		List<Node> eliminables = gp.getRemovedNodes();
		int j=0;
		int cont=0;
		while(j<eliminables.size())
		{	
			Node n = eliminables.get(j);
			if (!isFictitious(n)) {
				System.out.println(n.getName()+" Linea (Solo para variables): "+n.getLine());
				Misc.write(f, n.getName()+" Linea (Solo para variables): "+n.getLine()+"\n", true);
				cont++;
			}
			j++;
		}	
		System.out.println("("+cont+") Nodos eliminados en "+gp.getEliminations()+" eliminaciones");
		Misc.write(f, "("+cont+") Nodos eliminados en "+gp.getEliminations()+" eliminaciones\n", true);
	}
	private static void CalcularCombinaciones(int numTest, int numSlice)
	{
		final String testPath = Main.testPath + "problems" + File.separator + "code";
 		final String program = testPath + File.separator + "code.erl";
 		final OtpErlangObject response = Main.launcher.invokeErlang("launchCode", "getAST",program);
 		final EDG EDG = Main.generateEDG(response);
		int i = 1;
		System.out.println("----- Test"+numTest+"Slice"+numSlice+" -----");
		time_start = System.currentTimeMillis();
		
		final GraphPermutator.Filter nodeFilter = new GraphPermutator.Filter()
		{
			public boolean accept(Node node)
			{
				String name = node.getData().getName();
				NodeInfo.Type type = node.getData().getType();

				switch (type)
				{
					case Body:
					case Return:
					case Guard:
					case ListComprehensionResult:
						return false;
					default:
						break;
				}

				if (name == null)
					return true;
				if (type == NodeInfo.Type.Variable && name.equals("_"))
					return false;
				if (type == NodeInfo.Type.Atom && name.equals("undef"))
					return false;
				if (type == NodeInfo.Type.Atom && name.equals("funundef"))
					return false;

				return true;
			}
		};
		final GraphPermutator.Filter subtreeFilter = new GraphPermutator.Filter()
		{
			public boolean accept(Node node)
			{
				String name = node.getData().getName();
				NodeInfo.Type type = node.getData().getType();

				if (type == NodeInfo.Type.FunctionCall)
				{
					Node nodeCall = GraphTraverser.getChild(node, 0);
					String nameCall = nodeCall.getData().getName();
					if (nameCall.equals("slice") || nameCall.equals("slice_void"))
						return false;
				}

				if (name == null)
					return true;
				if (type == NodeInfo.Type.Function && name.equals("slice"))
					return false;
				if (type == NodeInfo.Type.Function && name.equals("slice_void"))
					return false;
				if (type == NodeInfo.Type.Function && name.equals("funundef"))
					return false;

				return true;
			}
		};
		final GraphPermutator gp = new GraphPermutator(EDG.getNodes().get(0), combiElems, true, nodeFilter, subtreeFilter);
		List<Node> finalSlice = null;
		while (true)
		{
			boolean hasNext = gp.next();
			if (!hasNext)
				break;
			List<Node> slice = gp.getRemainingNodes();

			/*Main.saveSliceSergio(Main.pulpath, EDG, slice, true, numTest, numSlice); 
			if (i%70 == 0)
			{
				Main.launcher.closeServer();
				Main.launcher.getReadyServer();
			}
			//------------------------------------
			final String[] arguments = {Integer.toString(numDelta),Integer.toString(numTest),Integer.toString(numSlice)};
			final OtpErlangObject result = Main.launcher.invokeErlang("testDelta", "main", arguments);
			final OtpErlangTuple tuple = (OtpErlangTuple) result;
			final OtpErlangObject primero = (OtpErlangObject) tuple.elementAt(1);
			String aux = primero.toString();
			switch (aux)
			{
				// En caso de que sea otro tipo de error (Error Compilacion, lineaNocoincide, 
				// error de abrir un fichero q no existe) habra otro tipo de error
				// Modificados comparator.erl (throw(archiv)) y test1Generator2 (Añadido el param SliceLine)
				
				case "ok":
					finalSlice = gp.getRemainingNodes();
					gp.remove();
					break;
				case "errorCompilacion":
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Tmp.txt"));
					break;
					
				case "error": // Si es error es unicamente porque no han coincidido resultados
				case "errorEjecutar": 
				default:
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Tmp.txt"));
					Misc.delete(new File(Main.pulpath+"test"+numTest+"/test"+numTest+"Slice"+numSlice+"Tmp.txt"));
					break;
			}*/
			i++;
		}
		System.out.println("Numero de combinacones: "+(i-1));

	}

}
