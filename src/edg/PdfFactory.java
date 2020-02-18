package edg;

import java.io.File;
import java.util.List;

import edg.config.Config;
import edg.graph.EDG;
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

	public static File createPdf(String outputPath, EDG edg)
	{
		return PdfFactory.createPdf(outputPath, edg, null, null);
	}
	public static File createPdf(String outputPath, EDG edg, Node slicingCriterium, List<Node> slice)
	{
		final File outputFile = new File(outputPath);

		PdfFactory.createPdf(outputFile, edg, slicingCriterium, slice);

		return outputFile;
	}
	public static File createPdf(String outputPath, File dotFile)
	{
		final File outputFile = new File(outputPath);

		PdfFactory.createPdf(outputFile, dotFile);

		return outputFile;
	}
	public static void createPdf(File outputFile, EDG edg)
	{
		PdfFactory.createPdf(outputFile, edg, null, null);
	}
	public static void createPdf(File outputFile, EDG edg, Node slicingCriterium, List<Node> slice)
	{
		final File dotOutputFile = PdfFactory.getTempDotFile();

		DotFactory.createDot(dotOutputFile, edg, slicingCriterium, slice);
		PdfFactory.createPdf(outputFile, dotOutputFile);
		Misc.delete(dotOutputFile);
	}
	public static void createPdf(File outputFile, File dotFile)
	{
		try
		{
			final String dotPath = dotFile.getAbsolutePath();
			final String outputPath = outputFile.getAbsolutePath();
			final String command = "/usr/local/bin/dot -Tpdf \"" + dotPath + "\" > \"" + outputPath + "\"";
			final Runtime runtime = Runtime.getRuntime();
			final Process process = runtime.exec(new String[] { "/bin/sh", "-c", command }, null, null);

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