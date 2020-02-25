package upv.slicing.eknife;

import upv.slicing.edg.DotFactory;
import upv.slicing.edg.EDGFactory;
import upv.slicing.edg.PdfFactory;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.EdgeInfo;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.slicing.ConstrainedAlgorithm;
import upv.slicing.edg.slicing.SlicingAlgorithm;
import upv.slicing.edg.slicing.SlicingCriterion;

import java.io.File;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

public class EKnife {
	public enum Language {Java, Erlang, Php}

	public static void main(String[] args)
	{
		final Object[] arguments = EKnife.processArguments(args);

		if (arguments == null)
			EKnife.printHelp();
		else
			EKnife.run(arguments);
	}

	private static Object[] processArguments(String[] args)
	{
		Language language = null;
		String inputPath = null;
		String outputPath = null;
		String archive = null;
		int line = 0;
		String name = null;
		int occurrence = 0;
		String dot = null;
		String pdf = null;
		boolean omitedges = true;
		boolean alledges = false;
		boolean controlFlow = false;
		boolean control = false;
		boolean value = false;
		boolean flow = false;
		boolean call = false;
		boolean input = false;
		boolean output = false;
		boolean summary = false;

		for (int argIndex = 0; argIndex < args.length; argIndex++)
		{
			final String arg = args[argIndex];

			switch (arg)
			{
				case "-ln":
				case "-ip":
				case "-op":
				case "-ar":
				case "-li":
				case "-na":
				case "-oc":
				case "-dot":
				case "-pdf":
					if (argIndex == args.length - 1)
						return null;
				default:
					break;
			}
			switch (arg)
			{
				case "-ln":
					language = Language.valueOf(args[argIndex + 1]);
					break;
				case "-ip":
					inputPath = args[argIndex + 1];
					break;
				case "-op":
					outputPath = args[argIndex + 1];
					break;
				case "-ar":
					archive = args[argIndex + 1];
					break;
				case "-li":
					line = Integer.parseInt(args[argIndex + 1]);
					break;
				case "-na":
					name = args[argIndex + 1];
					break;
				case "-oc":
					occurrence = Integer.parseInt(args[argIndex + 1]);
					break;
				case "-dot":
					dot = args[argIndex + 1];
					break;
				case "-pdf":
					pdf = args[argIndex + 1];
					break;

				case "-omitarcs":
					omitedges = false;
					break;

				case "-allarcs":
					alledges = true;
					break;
				case "-controlFlow":
					controlFlow = true;
					break;
				case "-control":
					control = true;
					break;
				case "-value":
					value = true;
					break;
				case "-flow":
					flow = true;
					break;
				case "-call":
					call = true;
					break;
				case "-input":
					input = true;
					break;
				case "-output":
					output = true;
					break;
				case "-summary":
					summary = true;
					break;
			}
		}

		if (alledges)
			controlFlow = control = value = flow = call = input = output = summary = true;
		if (archive == null && inputPath != null && new File(inputPath).isFile())
			archive = inputPath;
		if (archive != null)
			archive = new File(archive).getName();

		if (language == null || inputPath == null || outputPath == null || archive == null || line <= 0 || name == null || occurrence <= 0)
			return null;
		return new Object[] { language, inputPath, outputPath, archive, line, name, occurrence, dot, pdf, omitedges, controlFlow, control, value, flow, call, input, output, summary };
	}
	private static void printHelp()
	{
		String help = "Use the following options:\n" +
				"  -ln <language>      To specify the target language (Java, Erlang) \n" +
				"  -ip <file/folder>   To specify the file/folder where the source code is\n" +
				"  -op <file/folder>   To specify the file/folder where the slice will be stored\n" +
				"  -ar <file>          To specify the archive (relative to -ip) where the slicing criterion is\n" +
				"  -li <num>           To specify the line of the slicing criterion\n" +
				"  -na <name>          To specify the name of the slicing criterion (must be a variable)\n" +
				"  -oc <num>           To specify the occurrence of the slicing criterion in that line\n" +
				"  -dot <file>         To generate a dot that represents the EDG\n" +
				"  -pdf <file>         To generate a pdf that represents the EDG\n" +
				"  -omitarcs           To omit the generation of arcs\n" +
				"  -allarcs            To draw all arcs\n" +
				"  -controlFlow        To draw control flow arcs\n" +
				"  -control            To draw control arcs\n" +
				"  -value              To draw value arcs\n" +
				"  -flow               To draw flow arcs\n" +
				"  -call               To draw call arcs\n" +
				"  -input              To draw input arcs\n" +
				"  -output             To draw output arcs\n" +
				"  -summary            To draw summary arcs";
		System.out.print(help);
	}

	private static void run(Object[] arguments)
	{
		final Language language = (Language) arguments[0];
		final String inputPath = (String) arguments[1];
		final String outputPath = (String) arguments[2];
		final File outputFile = new File(outputPath);
		final String archive = (String) arguments[3];
		final int line = (int) arguments[4];
		final String name = (String) arguments[5];
		final int occurrence = (int) arguments[6];
		final String dotPath = (String) arguments[7];
		final String pdfPath = (String) arguments[8];
		final File dotFile = dotPath == null ? null : new File(dotPath);
		final File pdfFile = pdfPath == null ? null : new File(pdfPath);
		final boolean edges = (boolean) arguments[9];
		final Map<EdgeInfo.Type, Boolean> edgeFlags = new Hashtable<EdgeInfo.Type, Boolean>();
		edgeFlags.put(EdgeInfo.Type.ControlFlow, (boolean) arguments[10]);
		edgeFlags.put(EdgeInfo.Type.Control, (boolean) arguments[11]);
		edgeFlags.put(EdgeInfo.Type.Value, (boolean) arguments[12]);
		edgeFlags.put(EdgeInfo.Type.Flow, (boolean) arguments[13]);
		edgeFlags.put(EdgeInfo.Type.Call, (boolean) arguments[14]);
		edgeFlags.put(EdgeInfo.Type.Input, (boolean) arguments[15]);
		edgeFlags.put(EdgeInfo.Type.Output, (boolean) arguments[16]);
		edgeFlags.put(EdgeInfo.Type.Summary, (boolean) arguments[17]);

		final LAST last = LASTFactory.createLAST(Language.Java, inputPath, edges);
		final EDG edg = EDGFactory.createEDG(last);
		final SlicingCriterion slicingCriterion = new SlicingCriterion(archive, line, name, occurrence);
		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();
		final List<Node> slice = slicingAlgorithm.slice(SC);

		CodeFactory.createCode(language, outputFile, edg, slice);
		if (dotFile != null)
			DotFactory.createDot(dotFile, edg, SC, slice, edgeFlags);
		if (pdfFile != null && dotFile != null)
			PdfFactory.createPdf(pdfFile, dotFile);
		if (pdfFile != null && dotFile == null)
			PdfFactory.createPdf(pdfFile, edg, SC, slice, edgeFlags);
	}
}