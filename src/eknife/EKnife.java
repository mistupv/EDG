package eknife;

import java.io.File;
import java.util.List;

import edg.DotFactory;
import edg.PdfFactory;
import edg.graph.EDG;
import edg.graph.Node;
import edg.slicingAlgorithm.AdvancedAlgorithm;
import edg.slicingAlgorithm.SlicingAlgorithm;

public class EKnife
{
	public static enum Language { Java, Erlang }

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
		int line = 0;
		String name = null;
		int occurrence = 0;
		String dot = null;
		String pdf = null;

		for (int argIndex = 0; argIndex < args.length; argIndex++)
		{
			final String arg = args[argIndex];

			switch (arg)
			{
				case "-ln":
				case "-ip":
				case "-op":
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
			}
		}

		if (language == null || inputPath == null || outputPath == null || line <= 0 || name == null || occurrence <= 0)
			return null;
		return new Object[] { language, inputPath, outputPath, line, name, occurrence, dot, pdf };
	}
	private static void printHelp()
	{
		String help = "";

		help += "Use the following options:\n";
		help += "  -ln <language>   To specify the target language (Java, Erlang) \n";
		help += "  -ip <file>       To specify the file where the source code is\n";
		help += "  -op <file>       To specify the file where the slice will be stored\n";
		help += "  -li <num>        To specify the line\n";
		help += "  -na <name>       To specify the name of the variable\n";
		help += "  -oc <num>        To specify the occurrence of the variable in that line\n";
		help += "  -dot <file>      To generate a dot that represents the EDG\n";
		help += "  -pdf <file>      To generate a pdf that represents the EDG";

		System.out.print(help);
	}

	private static void run(Object[] arguments)
	{
		final Language language = (Language) arguments[0];
		final String inputPath = (String) arguments[1];
		final String outputPath = (String) arguments[2];
		final int line = (int) arguments[3];
		final String name = (String) arguments[4];
		final int occurrence = (int) arguments[5];
		final String dotPath = (String) arguments[6];
		final String pdfPath = (String) arguments[7];
		final File dotFile = dotPath == null ? null : new File(dotPath);
		final File pdfFile = pdfPath == null ? null : new File(pdfPath);

		final EDG edg = EDGFactory.createEDG(language, inputPath, true);
		final SlicingCriterion slicingCriterion = new SlicingCriterion(line, name, occurrence);
		final Node SC = slicingCriterion.parseToNode(edg);
		final SlicingAlgorithm slicingAlgorithm = new AdvancedAlgorithm(edg, true, true);
		final List<Node> slice = slicingAlgorithm.slice(SC);

		slice.remove(edg.getRootNode());
		CodeFactory.createCode(language, outputPath, edg, slice);
		if (dotFile != null)
			DotFactory.createDot(dotFile, edg, SC, slice);
		if (pdfFile != null && dotFile != null)
			PdfFactory.createPdf(pdfFile, dotFile);
		if (pdfFile != null && dotFile == null)
			PdfFactory.createPdf(pdfFile, edg, SC, slice);
	}
}