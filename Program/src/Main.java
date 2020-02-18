import java.io.File;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eknife.sergio.ErlPermaConnect;
import eknife.edg.slicingAlgorithm.SlicingORBS;
import eknife.config.Config;
import eknife.edg.EDG;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.generator.DependenceGenerator;
import eknife.edg.generator.DotGenerator;
import eknife.edg.generator.ErlangGenerator;
import eknife.edg.generator.GraphGenerator;
import eknife.edg.slicingAlgorithm.SlicingAlgorithm;
import eknife.edg.slicingAlgorithm.SlicingAlgorithm1;
import eknife.edg.slicingAlgorithm.SlicingAlgorithm2;
import eknife.erlang.Launcher;
import eknife.misc.Misc;

public class Main
{
	//--------------------------------------------Added by Sergio Para ORBS--------------------------------------------
	
	private static ErlPermaConnect launcher = new ErlPermaConnect(); 
	private static String pulpath = "";//"/Users/serperu/Desktop/Trabajo/Proyecto_Slicing/15_Programas/Generadores_de_entradas/";
	private static long time_start;
	private static long time_end;
	private static double time_expent;
	private static final int numDelta = 10;
	
	//-----------------------------------------------------------------------------------------------------------------
	
	private static int slicingAlgorithm = 2;
	private static int stepByStepMilliseconds = 0;
	private static List<TestCase> tests = new LinkedList<TestCase>();
	private static String testPath = "/Users/serperu/Desktop/Dropbox/UPV/ProgramSlicing(MagicToolSeparado)/e-knife/e-KnifePrimigenio/Tests/";

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

//		programName = "b1"; nodeId = 8;		
//		programName = "b2"; nodeId = 41;
//		programName = "b3"; nodeId = 88;	
//		programName = "b4"; nodeId = 15;	// V
//		programName = "b5"; nodeId = 15;		
//		programName = "b6"; nodeId = 64;		
//		programName = "b7"; nodeId = 131;	
//		programName = "b8"; nodeId = 138;	
//		programName = "b9"; nodeId = 28;		
//		programName = "b10"; nodeId = 30;		
//		programName = "b11"; nodeId = 8;		
//		programName = "b12"; nodeId = 15;		
//		programName = "b13"; nodeId = 9;		
//		programName = "b14"; nodeId = 10;		
//		programName = "b15"; nodeId = ¿?;		
//		programName = "b16"; nodeId = ¿?;		
//		programName = "b17"; nodeId = ¿?;		
//		programName = "b18"; nodeId = ¿?;				
//		programName = "b19"; nodeId = ¿?;		
//		programName = "b20"; nodeId = ¿?;		
//		programName = "b21"; nodeId = 15;		
//		programName = "b22"; nodeId = ¿?;	
//		programName = "b23"; nodeId = 65;	
		programName = "mbe"; nodeId = 25;

		
//		programName = "horwitz"; nodeId = 41;	// Horwitz
//		programName = "putaditas_test"; nodeId = 106;	
//		programName = "compressed_structures"; nodeId = 366; 
//		programName = "compressed_structures2"; nodeId = 152; 
		
//		programName = "word_count"; nodeId = 15;

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
 		final String program = test.getTestPath() + File.separator + test.getTestName();
		final OtpErlangObject response = Main.obtainAST(program);
		final EDG EDG = Main.generateEDG(response);
		Main.generateDot(EDG);

		final int nodeId = test.getNodeId();
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
	private static File generateDot(EDG EDG, Node slicingCriterium, List<Node> slice)
	{
		final String dotPath = Config.getTemporalPath() + File.separator + "graph.dot";
		final String tempDotPath = Config.getTemporalPath() + File.separator + "temp.dot";
		final File dotFile = new File(dotPath);
		final File tempDotFile = new File(tempDotPath);
		final DotGenerator dotGenerator = new DotGenerator();

		dotGenerator.generate(EDG, slicingCriterium, slice, tempDotFile, dotFile);

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
				return new SlicingAlgorithm2(EDG, true);
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
	public static void sergioBucle()
	{
		String origen="";
		String destino="/Users/serperu/Desktop/Trabajo/Proyecto_Slicing/Erlang/Tests/problems/code/code.erl"; //Donde este code.erl
		int line = 0;
		String var = "";
		String modulo = "";
		//for(int i=1;i<=15;i++)
		for(int i=1;i<=1;i++)
		{
			if(i == 4 || i == 6 || i == 7 || i == 8 || i == 9 || i == 10 || i == 14 || i == 15 )
				continue;
			int k;
			switch (i)
			{
				case 1: 
					k=3; 
					break;
				case 2: 
					k=4; 
					break;
				case 7:
				case 11:
				case 12:
				case 13:
				case 14: 
					k=1; 
					break;
				default: 
					k=2; 
					break;
			}
			//for(int j=1;j<=k;j++)
			for(int j=1;j<=1;j++)
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
							case 1: line=20; var="C"; break;
							default: line=16; var="D"; break;
						} break;
					case 4:
						switch (j) {
							case 1: line=18; var="Res"; break;
							default: line=18; var="Abb"; break;
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
					case 12: line=52; var="Shown"; break;
					case 13: line=71; var="Res"; break;
					case 14: line=16; var="List"; break;
					case 15:
						switch (j) {
							case 1: line=19; var="BS"; break;
							default: line=41; var="A"; break;
						}
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
				//Main.sergio2(i, j);//"test"+i+"Slice"+j+"Tmp.erl"
				Main.ORBS(i, j);
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
		while ((slice = slicingAlgorithm.ORBS1()) != null)
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
				// En caso de que sea otro tipo de error (Error Compilacion, lineaNocoincide, 
				// error de abrir un fichero q no existe) habra otro tipo de error
				// Modificados comparator.erl (throw(archiv)) y test1Generator2 (Añadido el param SliceLine)
				
				case "ok":
					Node destroyed = slicingAlgorithm.getCurrent();
					slicingAlgorithm.overwriteSlice(destroyed);
					slicingAlgorithm.addUnnecessary(destroyed);
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
		Misc.write(f, "Total de combinaciones de nodos procesadas:"+(i-1)+"\n", true);
		
		System.out.println("Tiempo invertido: "+time_expent+" segundos");
		Misc.write(f, "Tiempo invertido: "+time_expent+" segundos\n", true);
		List<Node> eliminables = slicingAlgorithm.getUnnecessary();
		int j=0;
		while(j<eliminables.size())
		{	
			System.out.println(eliminables.get(j).getName()+" Linea (Solo para variables): "+eliminables.get(j).getName());
			j++;
		}
	}
}
