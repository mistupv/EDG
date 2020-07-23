package upv.slicing.edg;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

public class PdfFactory
{
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
		final File dotOutputFile;
		try {
			dotOutputFile = File.createTempFile("PdfFactory-graph", ".dot");
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		DotFactory.createDot(dotOutputFile, edg, slicingCriterion, slice, edgeFlags);
		PdfFactory.createPdf(outputFile, dotOutputFile);
		dotOutputFile.delete();
	}

	public static void createPdf(File outputFile, File dotFile)
	{
		try
		{
			final String dotPath = dotFile.getAbsolutePath();
			final String outputPath = outputFile.getAbsolutePath();
			final Process process = new ProcessBuilder()
					.command("dot", "-Tpdf", "-o", outputPath, dotPath)
					.inheritIO()
					.start();

			int result = process.waitFor();
			if (result != 0)
				throw new Exception("Error generating pdf from dot file, exit code: " + result);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public static void createSvg(File outputFile, EDG edg, Node slicingCriterion, Set<Node> slice)
	{
		PdfFactory.createSvg(outputFile, edg, slicingCriterion, slice, null);
	}

	public static void createSvg(File outputFile, EDG edg, Node slicingCriterion, Set<Node> slice, Map<Edge.Type, Boolean> edgeFlags)
	{
		final File dotOutputFile;
		try {
			dotOutputFile = File.createTempFile("PdfFactory-graph", ".dot");
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}

		DotFactory.createDot(dotOutputFile, edg, slicingCriterion, slice, edgeFlags);
		PdfFactory.createSvg(outputFile, dotOutputFile);
		dotOutputFile.delete();
	}

	public static void createSvg(File outputFile, File dotFile)
	{
		try
		{
			final String dotPath = dotFile.getAbsolutePath();
			final String outputPath = outputFile.getAbsolutePath();
			final Process process = new ProcessBuilder()
					.command("dot", "-Tsvg", "-o", outputPath, dotPath)
					.inheritIO()
					.start();

			int result = process.waitFor();
			if (result != 0)
				throw new Exception("Error generating pdf from dot file, exit code: " + result);
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
