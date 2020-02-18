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
	private static int slicingAlgorithm = 2;
	private static String testPath = "/Users/serperu/Desktop/Benchmarks/Magic/"; // Computer-Dependent

	public static void main(String[] args)
	{
		Main.slice();
	}
	public static void slice()
	{
		final int nodeId;
		final String programName;
	
//		programName = "magic2"; nodeId = 9;
		programName = "magic3"; nodeId = 44;
//		programName = "magic3"; nodeId = 18;
//		programName = "magic4"; nodeId = 8;
		

		final String testPath =  Main.testPath;
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
		final Node slicingCriterium = EDG.findNodeByData(new NodeInfo(nodeId, 0, null), new Comparator<NodeInfo>() {
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
	
}
