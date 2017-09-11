package misc.util;

import java.io.File;
import java.util.List;

import misc.Misc;

public class Compressor
{
// TODO Que se pueda descomprimir en cualquier sistema operativo
//	private static String[] suportedFormats = { ".gzip", ".tgz" };
	private static String[] suportedFormats = { ".tar.gz", ".tar", ".gz", ".zip", ".rar" };

	public static boolean canUncompress(File file)
	{
		final String fileName = file.getName();

		for (String suportedFormat : Compressor.suportedFormats)
			if (fileName.endsWith(suportedFormat))
				return true;
		return false;
	}

	public static void uncompress(File file, File folder) throws Exception
	{
		final File workingFile = new File(".");
		final List<File> startFiles = Misc.getFiles(workingFile, null, false);
		final List<File> startFolders = Misc.getFolders(workingFile, false);
		final List<File> startFiles0 = Misc.union(startFiles, startFolders);
		Compressor.uncompressFile(file);

		final List<File> endFiles = Misc.getFiles(workingFile, null, false);
		final List<File> endFolders = Misc.getFolders(workingFile, false);
		final List<File> endFiles0 = Misc.union(endFiles, endFolders);

		final List<File> newFiles = Misc.disjunt(endFiles0, startFiles0);
		if (newFiles.isEmpty())
			return;

		final File firstFile = newFiles.get(0);
		if (newFiles.size() == 1 && firstFile.isDirectory())
			Misc.moveFolder(firstFile, folder);
		else
			Misc.moveToFolder(newFiles, folder);
	}
	private static void uncompressFile(File file) throws Exception
	{
		final String filePath = file.getPath();

		if (filePath.endsWith(".tar.gz"))
			Compressor.uncompressTarGz(file);
		else if (filePath.endsWith(".tar"))
			Compressor.uncompressTar(file);
		else if (filePath.endsWith(".gz"))
			Compressor.uncompressGz(file);
		else if (filePath.endsWith(".zip"))
			Compressor.uncompressZip(file);
		else if (filePath.endsWith(".rar"))
			Compressor.uncompressRar(file);
		else
			throw new RuntimeException("Compression type not contemplated");
	}
	private static void uncompressTarGz(File file) throws Exception
	{
		final String filePath = file.getAbsolutePath();
		final String[] commands = { "tar", "-xzvf", filePath };

		Compressor.uncompress(commands);
	}
	private static void uncompressTar(File file) throws Exception
	{
		final String filePath = file.getAbsolutePath();
		final String[] commands = { "tar", "-xvf", filePath };

		Compressor.uncompress(commands);
	}
	private static void uncompressGz(File file) throws Exception
	{
		final String filePath = file.getAbsolutePath();
		final String[] commands = { "gzip", "-d", filePath };

		Compressor.uncompress(commands);
	}
	private static void uncompressZip(File file) throws Exception
	{
		final String filePath = file.getAbsolutePath();
		final String[] commands = { "unzip", filePath };

		Compressor.uncompress(commands);
	}
	private static void uncompressRar(File file) throws Exception
	{
		final String filePath = file.getAbsolutePath();
		final String[] commands = { "unrar", "e", filePath };

		Compressor.uncompress(commands);
	}
	private static void uncompress(String[] commands) throws Exception
	{
		final Runtime runtime = Runtime.getRuntime();
		final Process process = runtime.exec(commands);

		process.waitFor();
	}
}