package misc.java;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import javax.tools.JavaCompiler.CompilationTask;

import misc.Misc;

public final class JavaCompiler
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static void main(String[] args)
	{
		if (args.length != 1)
			throw new RuntimeException("The args parameter must be a project path");

		final String projectPath = args[0];

		JavaCompiler.compileProject(projectPath);
	}

	/****************************************************************/
	/************************ Main functions ************************/
	/****************************************************************/
	public static boolean compileProject(String projectPath)
	{
		return JavaCompiler.compileProject(null, projectPath, null);
	}
	public static boolean compileFiles(List<File> files)
	{
		return JavaCompiler.compileFiles(null, files, null);
	}
	public static boolean compileFile(File file)
	{
		return JavaCompiler.compileFile(null, file, null);
	}

	public static boolean compileProject(String classpath, String projectPath)
	{
		return JavaCompiler.compileProject(classpath, projectPath, null);
	}
	public static boolean compileFiles(String classpath, List<File> files)
	{
		return JavaCompiler.compileFiles(classpath, files, null);
	}
	public static boolean compileFile(String classpath, File file)
	{
		return JavaCompiler.compileFile(classpath, file, null);
	}

	public static boolean compileProject(String projectPath, Writer errorsWriter)
	{
		return JavaCompiler.compileProject(null, projectPath, errorsWriter);
	}
	public static boolean compileFiles(List<File> files, Writer errorsWriter)
	{
		return JavaCompiler.compileFiles(null, files, errorsWriter);
	}
	public static boolean compileFile(File file, Writer errorsWriter)
	{
		return JavaCompiler.compileFile(null, file, errorsWriter);
	}

	public static boolean compileProject(String classpath, String projectPath, Writer errorsWriter)
	{
		final File root = new File(projectPath);
		final String[] extensions = { ".java" };
		final List<File> files = Misc.getFiles(root, extensions, true);

		return JavaCompiler.compileFiles(classpath, files, errorsWriter);
	}
	public static boolean compileFiles(String classpath, List<File> files, Writer errorsWriter)
	{
		boolean allCompiled = true;

		for (File file : files)
			allCompiled = allCompiled && JavaCompiler.compileFile(classpath, file, errorsWriter);

		return allCompiled;
	}
	public static boolean compileFile1(String classpath, File file, Writer errorsWriter)
	{
		try
		{
			final List<String> optionList = new LinkedList<String>();

			if (classpath != null)
				optionList.addAll(Arrays.asList("-classpath", classpath));

			final javax.tools.JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
			final StandardJavaFileManager fileManager = compiler.getStandardFileManager(null, null, null);
			final Iterable<? extends JavaFileObject> compilationUnits = fileManager.getJavaFileObjects(file);
			final CompilationTask compilationTask = compiler.getTask(errorsWriter, fileManager, null, optionList, null, compilationUnits);

			return compilationTask.call();
		}
		catch (Throwable e)
		{
			return false;
		}
	}
	public static boolean compileFile(String classpath, File file, Writer errorsWriter)
	{
		try
		{
			String finalClasspath = ".";
			finalClasspath += File.pathSeparator + file.getParent() + File.separator;
			finalClasspath += classpath != null ? File.pathSeparator + classpath : "";
			errorsWriter = errorsWriter != null ? errorsWriter : new StringWriter();

			final PrintWriter printWriter = new PrintWriter(errorsWriter);
			final String[] optionsAndSources = {
//				"-g", "-source", "1.8",
//				"-target", "1.8",
				"-encoding", "utf-8",
				"-parameters",
				"-cp", finalClasspath,
				file.getAbsolutePath()
			};
			final int status = com.sun.tools.javac.Main.compile(optionsAndSources, printWriter);

			return status == 0;
		}
		catch (Exception e)
		{
			return false;
		}
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private JavaCompiler()
	{
		
	}
}