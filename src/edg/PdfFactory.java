package edg;

import java.io.File;
import java.util.List;
import java.util.Map;

import edg.config.Config;
import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import misc.Misc;
import misc.util.Flusher;

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
	public static void createPdf(File outputFile, EDG edg, Map<EdgeInfo.Type, Boolean> edgeFlags)
	{
		PdfFactory.createPdf(outputFile, edg, null, null, edgeFlags);
	}
	public static void createPdf(File outputFile, EDG edg, Node slicingCriterion, List<Node> slice)
	{
		PdfFactory.createPdf(outputFile, edg, slicingCriterion, slice, null);
	}
	public static void createPdf(File outputFile, EDG edg, Node slicingCriterion, List<Node> slice, Map<EdgeInfo.Type, Boolean> edgeFlags)
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