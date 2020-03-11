package upv.slicing.misc.java;

import upv.slicing.misc.Misc;

import java.io.File;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.LinkedList;
import java.util.List;

public class MainSearcher
{
	public static void main(String[] args)
	{
		if (args.length != 1)
			throw new RuntimeException("The args parameter must contains a directory path");

		final String projectPath = args[0];
		final List<File> files = MainSearcher.search(projectPath, true);

		for (File file : files)
			System.out.println(file.getPath());
	}

	public static List<File> search(String projectPath, boolean recursive)
	{
		MainSearcher.checkProject(projectPath);

		final File root = new File(projectPath);
		final String[] extensions = { ".class" };
		final List<File> files = Misc.getFiles(root, extensions, recursive);
		final List<File> mainFiles = new LinkedList<File>();

		for (File file : files)
		{
			final int projectPathLength = projectPath.length();
			final String filePath = file.getPath().substring(projectPathLength);

			if (MainSearcher.checkMain(projectPath, filePath))
				mainFiles.add(file);
		}

		return mainFiles;
	}
	private static void checkProject(String projectPath)
	{
		if (projectPath == null)
			throw new RuntimeException("Null project path");

		final File file = new File(projectPath);
		if (!file.exists())
			throw new RuntimeException("The project path does not exist");
		if (!file.isDirectory())
			throw new RuntimeException("The project path is not a directory");
	}
	private static boolean checkMain(String projectPath, String filePath)
	{
		if (filePath == null || !filePath.endsWith(".class"))
			return false;

		try
		{
			final String separator = File.separator;
			final File file = new File(projectPath);
			final URL url = file.toURI().toURL();
			final URL[] urls = new URL[]{ url };
			final URLClassLoader ucl = new URLClassLoader(urls);
			final String fileName = filePath.substring(0, filePath.length() - ".class".length()).replaceAll(separator, ".");
			final Class<?> classFile = ucl.loadClass(fileName);
			final java.lang.reflect.Method[] methods = classFile.getDeclaredMethods();

			ucl.close();
			for (java.lang.reflect.Method method : methods)
			{
				// Static main method
				final int modifiers = method.getModifiers();
				final boolean publicMethod = Modifier.isPublic(modifiers);
				final boolean staticMethod = Modifier.isStatic(modifiers);
				final String methodName = method.getName();
				final Class<?>[] parameters = method.getParameterTypes();

				if (!(publicMethod && staticMethod && methodName.equals("main") && parameters != null && parameters.length == 1))
					continue;

				// Correct parameter
				final String type = parameters[0].getSimpleName();

				if (type.equals("String[]"))
					return true;
			}
		}
		catch (Throwable t)
		{
			t.printStackTrace();
		}

		return false;
	}
}