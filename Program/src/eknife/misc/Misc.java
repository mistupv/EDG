package eknife.misc;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class Misc
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	/****************************************************************/
	/*************************** Computer ***************************/
	/****************************************************************/
	public static String getOSName()
	{
		return System.getProperty("os.name");
	}
	public static String getURLFileProtocol()
	{
		final String OSName = Misc.getOSName();

		if (OSName.startsWith("Win"))
			return "file:/";
		if (OSName.startsWith("Mac"))
			return "file://";
		throw new RuntimeException(OSName + " is not controlled");
	}

	/****************************************************************/
	/***************************** Null *****************************/
	/****************************************************************/
	public static void checkNotNull(Object object)
	{
		if (object == null)
			throw new NullPointerException();
	}
	public static void checkNotNull(Object[] objects)
	{
		if (objects == null)
			throw new NullPointerException();
		for (Object object : objects)
			if (object == null)
				throw new NullPointerException();
	}

	/****************************************************************/
	/***************************** Math *****************************/
	/****************************************************************/
	public static double round(double number, int decimals)
	{
		final double factor = Math.pow(10, decimals);

		return Math.round(number * factor) / factor;
	}

	/****************************************************************/
	/***************************** Time *****************************/
	/****************************************************************/
	public static Date add(Date date1, Date date2)
	{
		if (date1 == null || date2 == null)
			return null;

		final long milliseconds = date1.getTime() + date2.getTime();

		return new Date(milliseconds);
	}
	public static Date substract(Date date1, Date date2)
	{
		if (date1 == null || date2 == null)
			return null;

		final long milliseconds = date1.getTime() - date2.getTime();

		return new Date(milliseconds >= 0 ? milliseconds : -milliseconds);
	}
	public static String getCurrentTime()
	{
		final Date currentTime = new Date();
		final SimpleDateFormat format = new SimpleDateFormat("[dd/MM/yyyy HH:mm:ss SSS]");

		return format.format(currentTime);
	}

	/****************************************************************/
	/**************************** Paths *****************************/
	/****************************************************************/
	public static String getProjectPath()
	{
		final int miscNameLength = (Misc.class.getName() + ".class").length();
		final String classPath = Misc.getClassPath(Misc.class);
		final String projectPath = classPath.substring(0, classPath.length() - miscNameLength);

		return projectPath;
	}
	public static String getClassPath(Class<?> clazz)
	{
		final String className = clazz.getName().replace('.', '/') + ".class";
		final URL classUrl = clazz.getClassLoader().getResource(className);
		final File classFile = new File(classUrl.getPath());

		return classFile.getPath().replace("%20", " ");
	}

	/****************************************************************/
	/***************************** Wait *****************************/
	/****************************************************************/
	public static void wait(int milliseconds)
	{
		try
		{
			synchronized (Thread.currentThread())
			{
				Thread.currentThread().wait(milliseconds);
			}
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();
		}
	}
	public static void waitUntilExists(String filePath, int checkEveryMilliseconds, int maximumMilliseconds)
	{
		final File file = new File(filePath);

		Misc.waitUntilExists(file, checkEveryMilliseconds, maximumMilliseconds);
	}
	public static void waitUntilExists(File file, int checkEveryMilliseconds, int maximumMilliseconds)
	{
		for (int millisecondsWaited = 0; !file.exists() && millisecondsWaited < maximumMilliseconds; millisecondsWaited += checkEveryMilliseconds)
			Misc.wait(checkEveryMilliseconds);
	}

	/************************************************************************************************/
	/******************************************** Lists *********************************************/
	/************************************************************************************************/
	/****************************************************************/
	/**************************** Clone *****************************/
	/****************************************************************/
	@SuppressWarnings("unchecked")
	private static <E> List<E> generateNewList(Class<?> listClass)
	{
		try
		{
			return (List<E>)listClass.newInstance();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

		throw new RuntimeException("Cannot generate new list");
	}
	public static <E> List<E> clone(List<E> elements)
	{
		if (elements == null)
			return null;

		final Class<?> elementsClass = elements.getClass();
		final List<E> clon = Misc.generateNewList(elementsClass);

		for (E element : elements)
			clon.add(element);

		return clon;
	}

	/****************************************************************/
	/*************************** Reverse ****************************/
	/****************************************************************/
	public static <T> List<T> reverse(List<T> list)
	{
		final Class<?> listClass = list.getClass();
		final List<T> newList = Misc.generateNewList(listClass);

		for (T element : list)
			newList.add(0, element);

		return newList;
	}

	/****************************************************************/
	/*********************** Convert to list ************************/
	/****************************************************************/
	public static <K, T> List<K> getSortedKeysAsList(Map<K, T> map)
	{
		final Set<K> keySet = map.keySet();
		@SuppressWarnings("unchecked")
		final K[] keys = (K[])keySet.toArray();

		Arrays.sort(keys);

		return Arrays.asList(keys);
	}
	public static <K, T> List<K> getKeysAsList(Map<K, T> map)
	{
		final Set<K> keySet = map.keySet();
		@SuppressWarnings("unchecked")
		final K[] keys = (K[])keySet.toArray();

		return Arrays.asList(keys);
	}
	public static <K, T> List<T> getSortedValuesAsList(Map<K, T> map)
	{
		final Collection<T> valuesCollection = map.values();
		@SuppressWarnings("unchecked")
		final T[] values = (T[])valuesCollection.toArray();

		Arrays.sort(values);

		return Arrays.asList(values);
	}
	public static <K, T> List<T> getValuesAsList(Map<K, T> map)
	{
		final Collection<T> valuesCollection = map.values();
		@SuppressWarnings("unchecked")
		final T[] values = (T[])valuesCollection.toArray();

		return Arrays.asList(values);
	}

	/****************************************************************/
	/***************************** Sets *****************************/
	/****************************************************************/
	public static <E> List<E> disjunt(List<E> elements, E element)
	{
		if (elements == null)
			return null;

		final Class<?> elementsClass = elements.getClass();
		final List<E> elements2 = Misc.generateNewList(elementsClass);

		elements2.add(element);

		return Misc.disjunt(elements, elements2);
	}
	public static <E> List<E> disjunt(List<E> elements1, List<E> elements2)
	{
		if (elements1 == null)
			return null;

		final List<E> disjunt = Misc.clone(elements1);
		final int elements1Size = elements1.size();

		if (elements2 != null)
			for (int elements1Index = elements1Size - 1; elements1Index >= 0; elements1Index--)
				if (elements2.contains(disjunt.get(elements1Index)))
					disjunt.remove(elements1Index);

		return disjunt;
	}
	public static <E> List<E> union(List<E> elements, E element)
	{
		if (elements == null)
			return null;

		final Class<?> listClass = elements.getClass();
		final List<E> elements2 = Misc.generateNewList(listClass);

		elements2.add(element);

		return Misc.union(elements, elements2);
	}
	public static <E> List<E> union(List<E> elements1, List<E> elements2)
	{
		if (elements1 == null || elements2 == null)
			return null;

		final List<E> union = Misc.clone(elements1);

		for (E element : elements2)
			if (!union.contains(element))
				union.add(element);

		return union;
	}
	public static <E> List<E> intersect(List<E> elements, E element)
	{
		if (elements == null)
			return null;

		final Class<?> listClass = elements.getClass();
		final List<E> elements2 = Misc.generateNewList(listClass);

		elements2.add(element);

		return Misc.union(elements, elements2);
	}
	public static <E> List<E> intersect(List<E> elements1, List<E> elements2)
	{
		if (elements1 == null || elements2 == null)
			return null;

		final List<E> intersect = Misc.clone(elements1);
		final int elements1Size = elements1.size();

		for (int elements1Index = elements1Size - 1; elements1Index >= 0; elements1Index--)
			if (!elements2.contains(intersect.get(elements1Index)))
				intersect.remove(elements1Index);

		return intersect;
	}

	/************************************************************************************************/
	/******************************************** Files *********************************************/
	/************************************************************************************************/
	public static boolean existsFile(File file)
	{
		if (!file.exists())
			return false;

		final File realFile = Misc.getRealFile(file);
		if (realFile == null)
			return false;

		final String path = file.getPath();
		final String realPath = realFile.getPath();

		return path.equals(realPath);
	}
	public static File getRealFile(File file)
	{
		try
		{
			return file.getCanonicalFile();
		}
		catch (IOException e)
		{
			return null;
		}
	}
	public static String getFileName(File file)
	{
		final String fileName = file.getName();

		return fileName.substring(0, fileName.lastIndexOf("."));
	}

	/****************************************************************/
	/************************** Get folders *************************/
	/****************************************************************/
	public static List<File> getFolders(File root, boolean recursive)
	{
		if (!root.exists() || !root.isDirectory())
			return new LinkedList<File>();
		if (recursive)
			return Misc.getFolders(root);
		return Misc.getFolderFolders(root);
	}
	private static List<File> getFolders(File root)
	{
		int numChildToVisit = 0;
		final LinkedList<Integer> bifurcations = new LinkedList<Integer>();

		File folder = root;
		final List<File> folders = new LinkedList<File>();

		while (true)
		{
			final List<File> folderFolders = Misc.getFolderFolders(folder);
			final int numChildren = folderFolders.size();

			// Evaluate node
			if (numChildToVisit == 0)
				folders.addAll(folderFolders);

			if (numChildToVisit == numChildren)
			{
				// Back to the parent
				// If the parent is the root then finish
				if (folder.getPath().equals(root.getPath()))
					break;

				// Otherwise, back to the parent
				folder = folder.getParentFile();
				numChildToVisit = bifurcations.removeLast();
			}
			else
			{
				// Go to the child
				folder = folderFolders.get(numChildToVisit);
				bifurcations.add(numChildToVisit + 1);
				numChildToVisit = 0;
			}
		}

		return folders;
	}
	private static List<File> getFolderFolders(File folder)
	{
		final File[] folders = folder.listFiles(new FilenameFilter(){
			public boolean accept(File dir, String name)
			{
				final File file = new File(dir.getAbsolutePath() + File.separator + name);

				return file.isDirectory();
			}});

		return new LinkedList<File>(Arrays.asList(folders));
	}

	/****************************************************************/
	/*************************** Get files **************************/
	/****************************************************************/
	public static List<File> getFiles(File root, String[] extensions, boolean recursive)
	{
		if (!root.exists() || !root.isDirectory())
			return new LinkedList<File>();
		if (recursive)
			return Misc.getFiles(root, extensions);
		return Misc.getFolderFiles(root, extensions);
	}
	private static List<File> getFiles(File root, String[] extensions)
	{
		int numChildToVisit = 0;
		final LinkedList<Integer> bifurcations = new LinkedList<Integer>();

		File folder = root;
		final List<File> files = new LinkedList<File>();

		while (true)
		{
			final List<File> folders = Misc.getFolderFolders(folder);
			final int numChildren = folders.size();

			// Evaluate node
			if (numChildToVisit == 0)
				files.addAll(Misc.getFolderFiles(folder, extensions));

			if (numChildToVisit == numChildren)
			{
				// Back to the parent
				// If the parent is the root then finish
				if (folder.getPath().equals(root.getPath()))
					break;

				// Otherwise, back to the parent
				folder = folder.getParentFile();
				numChildToVisit = bifurcations.removeLast();
			}
			else
			{
				// Go to the child
				folder = folders.get(numChildToVisit);
				bifurcations.add(numChildToVisit + 1);
				numChildToVisit = 0;
			}
		}

		return files;
	}
	private static List<File> getFolderFiles(File folder)
	{
		return Misc.getFolderFiles(folder, null);
	}
	private static List<File> getFolderFiles(File folder, final String[] extensions)
	{
		final File[] files = folder.listFiles(new FilenameFilter(){
			public boolean accept(File dir, String name)
			{
				final File file = new File(dir.getAbsolutePath() + File.separator + name);
				if (!file.isFile())
					return false;

				if (extensions == null)
					return true;

				for (String extension : extensions)
					if (name.endsWith(extension))
						return true;

				return false;
			}});

		return new LinkedList<File>(Arrays.asList(files));
	}

	/****************************************************************/
	/**************************** Delete ****************************/
	/****************************************************************/
	public static void delete(List<File> roots)
	{
		Misc.delete(roots, false);
	}
	public static void delete(List<File> roots, boolean recursive)
	{
		for (File root : roots)
			Misc.delete(root, recursive);
	}
	public static void delete(File root)
	{
		Misc.delete(root, false);
	}
	public static void delete(File root, boolean recursive)
	{
		if (!root.exists())
			return;

		if (root.isFile())
			Misc.deleteFile(root);
		else if (root.isDirectory())
		{
			if (recursive)
				Misc.deleteRecursively(root);
			else
				Misc.deleteFolderFiles(root);
			Misc.deleteFolder(root);
		}
	}
	private static void deleteRecursively(File root)
	{
		int numChildToVisit = 0;
		final LinkedList<Integer> bifurcations = new LinkedList<Integer>();

		File folder = root;

		while (true)
		{
			final List<File> folders = Misc.getFolderFolders(folder);
			final int numChildren = folders.size();

			// Evaluate node
			if (numChildToVisit == numChildren)
				Misc.deleteFolderFiles(folder);

			if (numChildToVisit == numChildren)
			{
				// Back to the parent
				// If the parent is the root then finish
				if (folder.getPath().equals(root.getPath()))
					break;

				// Otherwise, back to the parent
				folder = folder.getParentFile();
				numChildToVisit = bifurcations.removeLast();
			}
			else
			{
				// Go to the child
				folder = folders.get(numChildToVisit);
				bifurcations.add(numChildToVisit + 1);
				numChildToVisit = 0;
			}
		}
	}
	private static void deleteFolderFiles(File folder)
	{
		for (File folderFolder : Misc.getFolderFolders(folder))
			folderFolder.delete();
		for (File folderFile : Misc.getFolderFiles(folder))
			Misc.deleteFile(folderFile);
	}
	private static void deleteFile(File file)
	{
		file.delete();
	}
	private static void deleteFolder(File folder)
	{
		folder.delete();
	}

	/****************************************************************/
	/**************************** Clean *****************************/
	/****************************************************************/
	public static void cleanFolder(File root)
	{
		Misc.cleanFolder(root, false);
	}
	public static void cleanFolder(File root, boolean recursive)
	{
		if (!root.exists() || !root.isDirectory())
			return;

		if (recursive)
			Misc.deleteRecursively(root);
		else
			Misc.deleteFolderFiles(root);
	}

	/****************************************************************/
	/**************************** Create ****************************/
	/****************************************************************/
	public static void createFolder(File dir)
	{
		if (!dir.exists())
			dir.mkdirs();
		if (!dir.exists())
			throw new RuntimeException("Cannot create the destination directory");
	}
	public static void createFile(File file)
	{
		if (file.exists())
			return;

		try
		{
			final File parent = file.getParentFile();

			Misc.createFolder(parent);
			file.createNewFile();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}

	/****************************************************************/
	/***************************** Copy *****************************/
	/****************************************************************/
	public static void copy(String text, File dst)
	{
		final File destinationDir = dst.getParentFile();

		Misc.createFolder(destinationDir);
		try
		{
			final FileOutputStream out = new FileOutputStream(dst);
			final PrintStream print = new PrintStream(out);

			print.print(text);
			print.close();
		}
		catch (FileNotFoundException e)
		{
			e.printStackTrace();
		}
	}
	public static void copyFile(File src, File dst)
	{
		final File destinationDir = dst.getParentFile();

		Misc.createFolder(destinationDir);
		try
		{
			final InputStream in = new FileInputStream(src);
			final OutputStream out = new FileOutputStream(dst);

			int len;
			final byte[] buf = new byte[1024];
			while ((len = in.read(buf)) > 0)
				out.write(buf, 0, len);
			in.close();
			out.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}
	public static void copyFolder(File src, File dst)
	{
		Misc.copyFolder(src, dst, null);
	}
	public static void copyFolder(File src, File dst, String[] extensions)
	{
		if (!src.exists() || !src.isDirectory())
			return;

		final String srcAbsolutePath = src.getAbsolutePath();
		final int srcAbsolutePathLength = srcAbsolutePath.length();
		final String dstAbsolutePath = dst.getAbsolutePath();
		final List<File> files = Misc.getFiles(src, extensions, true);

		for (File file : files)
		{
			final String fileAbsolutePath = file.getAbsolutePath();
			final String fileRelativePath = fileAbsolutePath.substring(srcAbsolutePathLength);
			final String dstFileAbsolutePath = dstAbsolutePath + fileRelativePath;
			final File dstFile = new File(dstFileAbsolutePath);

			Misc.copyFile(file, dstFile);
		}
	}
	public static void copyToFolder(List<File> files, File folder)
	{
		for (File file : files)
			Misc.copyToFolder(file, folder);
	}
	public static void copyToFolder(File file, File folder)
	{
		final String folderPath = folder.getPath();
		final String fileName = file.getName();
		final String dstPath = folderPath + File.separator + fileName;
		final File dst = new File(dstPath);

		Misc.copyFile(file, dst);
	}

	/****************************************************************/
	/***************************** Move *****************************/
	/****************************************************************/
	public static void moveFolder(File src, File dst)
	{
		Misc.moveFolder(src, dst, null);
	}
	public static void moveFolder(File src, File dst, String[] extensions)
	{
		if (!src.exists() || !src.isDirectory())
			return;

		Misc.copyFolder(src, dst, extensions);
		Misc.delete(src, true);
	}
	public static void moveFile(File src, File dst)
	{
		if (!src.exists())
			return;

		Misc.copyFile(src, dst);
		Misc.delete(src);
	}
	public static void moveToFolder(List<File> files, File folder)
	{
		for (File file : files)
			Misc.moveToFolder(file, folder);
	}
	public static void moveToFolder(File file, File folder)
	{
		final String folderPath = folder.getPath();
		final String fileName = file.getName();
		final String dstPath = folderPath + File.separator + fileName;
		final File dst = new File(dstPath);

		Misc.moveFile(file, dst);
	}

	/****************************************************************/
	/***************************** Read *****************************/
	/****************************************************************/
	public static String read(String path)
	{
		final File file = new File(path);

		return Misc.read(file);
	}
	public static String read(File file)
	{
		String text = "";
		FileReader fr = null;
		BufferedReader br = null;

		try
		{
			fr = new FileReader(file);
			br = new BufferedReader(fr);

			String line;
			while ((line = br.readLine()) != null)
				text += line + "\n";

			int lastNewLine = text.lastIndexOf("\n");
			if (lastNewLine != -1)
				text = text.substring(0, lastNewLine);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		finally
		{
			try
			{
				if (br != null)
					br.close();
				if (fr != null)
					fr.close();
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		}

		return text;
	}

	/****************************************************************/
	/**************************** Write *****************************/
	/****************************************************************/
	public static void write(String path, String text, boolean append)
	{
		final File file = new File(path);

		Misc.write(file, text, append);
	}
	public static void write(File file, String text, boolean append)
	{
		Misc.createFile(file);
		try
		{
			final FileWriter fw = new FileWriter(file, append);

			fw.write(text);
			fw.flush();
			fw.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}
	public static void write(String path, InputStream inputStream, boolean append)
	{
		final File file = new File(path);

		Misc.write(file, inputStream, append);
	}
	public static void write(final File file, final InputStream inputStream, final boolean append)
	{
		new Thread()
		{
			public void run()
			{
				final InputStreamReader isr = new InputStreamReader(inputStream);
				final BufferedReader br = new BufferedReader(isr);

				try
				{
					String line;
					while ((line = br.readLine()) != null)
						Misc.write(file, line + "\n", append);
					br.close();
					isr.close();
				}
				catch (IOException e)
				{
					e.printStackTrace();
				}
			}
		}.start();
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private Misc()
	{
		
	}
}