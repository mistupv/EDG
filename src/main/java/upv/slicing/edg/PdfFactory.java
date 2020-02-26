package upv.slicing.edg;

import upv.slicing.edg.config.Config;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;
import upv.slicing.misc.Misc;
import upv.slicing.misc.util.Flusher;

import java.io.File;
import java.util.Map;
import java.util.Set;

public class PdfFactory
{
	private static final Config config = Config.getConfig();

	private static File getTempDotFile()
	{
		final String temporaryPath = PdfFactory.config.getTemporaryPath();
		int temporaryPathIndex = 0;

		while (true)
		{
			final String extraDotPath = temporaryPathIndex++ > 0 ? temporaryPathIndex + "" : "";
			final String temporaryDotPath = temporaryPath + "temp" + extraDotPath + ".dot";
			final File temporatyDotFile = new File(temporaryDotPath);

			if (!temporatyDotFile.exists())
				return temporatyDotFile;
		}
	}

	public static void createPdf(File outputFile, EDG edg)
	{
		PdfFactory.createPdf(outputFile, edg, null, null, null);
	}
	public static void createPdf(File outputFile, EDG edg, Map<Edge.Type, Boolean> edgeFlags)
	{
		PdfFactory.createPdf(outputFile, edg, null, null, edgeFlags);
	}
	public static void createPdf(File outputFile, EDG edg, Node slicingCriterion, Set<Node> slice)
	{
		PdfFactory.createPdf(outputFile, edg, slicingCriterion, slice, null);
	}
	public static void createPdf(File outputFile, EDG edg, Node slicingCriterion, Set<Node> slice, Map<Edge.Type, Boolean> edgeFlags)
	{
		final File dotOutputFile = PdfFactory.getTempDotFile();

		DotFactory.createDot(dotOutputFile, edg, slicingCriterion, slice, edgeFlags);
		PdfFactory.createPdf(outputFile, dotOutputFile);
		Misc.delete(dotOutputFile);
	}
	public static void createPdf(File outputFile, File dotFile)
	{
		try
		{
			final String dotPath = dotFile.getAbsolutePath();
			final String outputPath = outputFile.getAbsolutePath();
			final String command = "dot -Tpdf \"" + dotPath + "\" > \"" + outputPath + "\"";
			final String path = "PATH=/usr/local/bin:" + System.getenv("PATH");
			final Runtime runtime = Runtime.getRuntime();
			final Process process = runtime.exec(new String[] { "/bin/sh", "-c", command }, new String[] { path }, null);

			new Flusher(process).start();
			process.waitFor();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	private PdfFactory()
	{
		
	}
}