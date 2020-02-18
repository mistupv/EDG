package misc;

import java.awt.Point;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.StringTokenizer;

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

//		if (Utils.isWindows())
		if (OSName.startsWith("Win"))
			return "file:///";
//		if (Utils.isMac())
		if (OSName.startsWith("Mac"))
			return "file://";
//		if (Utils.isUnix())
		if (OSName.startsWith("Linux"))
			return "file://";
		throw new RuntimeException(OSName + " is not handled");
	}

	/****************************************************************/
	/***************************** Java *****************************/
	/****************************************************************/
	public static String getTypeName(Class<?> type)
	{
		return Misc.getTypeName(type, false);
	}
	private static String getTypeName(Class<?> type, boolean qualifyClass)
	{
		final String typeName = type.getName();

		if (type.isArray())
			return "[" + Misc.getTypeName(type.getComponentType(), true);
		if (type.isPrimitive())
		{
			final String typeName0 = Misc.getTypeName0(typeName);
			if (typeName0 != null)
				return typeName0;
			throw new RuntimeException("Primitive type not contemplated: " + typeName);
		}

		final String typeName1 = qualifyClass ? typeName.replace('.', '/') : typeName;
		return "L" + typeName1 + ";";
	}
	public static String getTypeName(String typeName)
	{
		return Misc.getTypeName(typeName, false);
	}
	private static String getTypeName(String typeName, boolean qualifyClass)
	{
		if (typeName == null)
			throw new NullPointerException(typeName);

		if (typeName.endsWith("[]"))
			return "[" + Misc.getTypeName(typeName.substring(0, typeName.lastIndexOf("[]")), true);
		final String typeName0 = Misc.getTypeName0(typeName);
		if (typeName0 != null)
			return typeName0;

		final String typeName1 = qualifyClass ? typeName.replace('.', '/') : typeName;
		return "L" + typeName1 + ";";
	}
	private static String getTypeName0(String typeName)
	{
		final String[] primitiveTypeNames = { "byte", "short", "int", "long", "float", "double", "char", "boolean", "void" };
		final String[] primitiveVMTypeNames = { "B", "S", "I", "J", "F", "D", "C", "Z", "V" };

		for (int primitiveTypeNameIndex = 0; primitiveTypeNameIndex < primitiveTypeNames.length; primitiveTypeNameIndex++)
		{
			final String primitiveTypeName = primitiveTypeNames[primitiveTypeNameIndex];

			if (primitiveTypeName.equals(typeName))
				return primitiveVMTypeNames[primitiveTypeNameIndex];
		}

		return null;
	}

	/****************************************************************/
	/***************************** Null *****************************/
	/****************************************************************/
	public static void checkNotNull(Object object)
	{
		Objects.requireNonNull(object);
	}
	public static void checkNotNull(Object[] objects)
	{
		Objects.requireNonNull(objects);
		for (Object object : objects)
			Objects.requireNonNull(object);
	}

	/****************************************************************/
	/***************************** Text *****************************/
	/****************************************************************/
	public static List<String> getLines(String text)
	{
		final String lineSeparator = System.lineSeparator();

		return Misc.getLines(text, lineSeparator);
	}
	public static List<String> getLines(String text, String lineSeparator)
	{
		final List<String> lines = new LinkedList<String>();
		final StringTokenizer st = new StringTokenizer(text, lineSeparator, true);

		while (st.hasMoreTokens())
		{
			String token = st.nextToken();

			if (token.equals(lineSeparator))
				token = "";
			lines.add(token);
			if (!token.isEmpty() && st.hasMoreTokens())
				st.nextToken();
		}

		return lines;
	}
	public static String getSubstring(String text, Point start, Point end)
	{
		final String lineSeparator = System.lineSeparator();
		final List<String> lines = Misc.getLines(text, lineSeparator);

		return Misc.getSubstring(lines, start, end, lineSeparator);
	}
	public static String getSubstring(String text, Point start, Point end, String lineSeparator)
	{
		final List<String> lines = Misc.getLines(text, lineSeparator);

		return Misc.getSubstring(lines, start, end, lineSeparator);
	}
	public static String getSubstring(List<String> lines, Point start, Point end)
	{
		final String lineSeparator = System.lineSeparator();

		return Misc.getSubstring(lines, start, end, lineSeparator);
	}
	public static String getSubstring(List<String> lines, Point start, Point end, String lineSeparator)
	{
		if (start.y == end.y)
			return lines.get(start.y - 1).substring(start.x - 1, end.x - 1);

		String substring = lines.get(start.y - 1).substring(start.x - 1) + lineSeparator;
		for (int lineIndex = start.y; lineIndex < end.y - 1; lineIndex++)
			substring += lines.get(lineIndex) + lineSeparator;
		substring += lines.get(end.y - 1).substring(0, end.x - 1);

		return substring;
	}
	public static int getOffset(String text, Point point)
	{
		final String lineSeparator = System.lineSeparator();
		final List<String> lines = Misc.getLines(text, lineSeparator);

		return Misc.getOffset(lines, point, lineSeparator);
	}
	public static int getOffset(String text, Point point, String lineSeparator)
	{
		final List<String> lines = Misc.getLines(text, lineSeparator);

		return Misc.getOffset(lines, point, lineSeparator);
	}
	public static int getOffset(List<String> lines, Point point)
	{
		final String lineSeparator = System.lineSeparator();

		return Misc.getOffset(lines, point, lineSeparator);
	}
	public static int getOffset(List<String> lines, Point point, String lineSeparator)
	{
		final int line = point.y;
		final int column = point.x;
		int offset = 0;

		for (int lineIndex = 0; lineIndex < line - 1; lineIndex++)
			offset += lines.get(lineIndex).length() + lineSeparator.length();
		offset += column - 1;

		return offset;
	}
	public static Point getPoint(String text, int offset)
	{
		final String lineSeparator = System.lineSeparator();
		final List<String> lines = Misc.getLines(text, lineSeparator);

		return Misc.getPoint(lines, offset, lineSeparator);
	}
	public static Point getPoint(String text, int offset, String lineSeparator)
	{
		final List<String> lines = Misc.getLines(text, lineSeparator);

		return Misc.getPoint(lines, offset, lineSeparator);
	}
	public static Point getPoint(List<String> lines, int offset)
	{
		final String lineSeparator = System.lineSeparator();

		return Misc.getPoint(lines, offset, lineSeparator);
	}
	public static Point getPoint(List<String> lines, int offset, String lineSeparator)
	{
		int y = 1;
		int x = 1;

		for (String line : lines)
		{
			final int length = line.length();
			if (offset <= length)
				break;

			offset -= length + lineSeparator.length();
			y++;
		}
		x = offset + 1;

		return new Point(x, y);
	}
	public static Point getEndPoint(String text)
	{
		final String lineSeparator = System.lineSeparator();
		final List<String> lines = Misc.getLines(text, lineSeparator);

		return Misc.getEndPoint(lines);
	}
	public static Point getEndPoint(String text, String lineSeparator)
	{
		final List<String> lines = Misc.getLines(text, lineSeparator);

		return Misc.getEndPoint(lines);
	}
	public static Point getEndPoint(List<String> lines)
	{
		return new Point(lines.get(lines.size() - 1).length() + 1, lines.size());
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
		return Misc.getCurrentTime("[dd/MM/yyyy HH:mm:ss SSS]");
	}
	public static String getCurrentTime(String dateFormat)
	{
		final Date currentTime = new Date();
		final SimpleDateFormat format = new SimpleDateFormat(dateFormat);

		return format.format(currentTime);
	}

	/****************************************************************/
	/**************************** Paths *****************************/
	/****************************************************************/
	public static String getProjectPath(Class<?> referenceClass)
	{
		final int classNameLength = (referenceClass.getName() + ".class").length();
		final String classPath = Misc.getClassPath(referenceClass);

		return classPath.substring(0, classPath.length() - classNameLength);
	}
	public static String getClassPath(Class<?> clazz)
	{
		final String className = clazz.getName().replace('.', '/') + ".class";
		final URL classUrl = clazz.getClassLoader().getResource(className);
		final File classFile = new File(classUrl.getPath());
		final String classPath0 = classFile.getPath().replace("%20", " ");
		final String classPath1 = classPath0.startsWith("file:\\") ? classPath0.substring("file:\\".length()) : classPath0;
		final String classPath2 = classPath1.startsWith("file:/") ? classPath1.substring("file:".length()) : classPath1;

		return classPath2;
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
			return (List<E>) listClass.newInstance();
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

		clon.addAll(elements);

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
		final T[] values = (T[]) valuesCollection.toArray();

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

		return Misc.intersect(elements, elements2);
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
		final int lastDot = fileName.lastIndexOf(".");

		return lastDot == -1 ? fileName : fileName.substring(0, lastDot);
	}
	public static String getFileExtension(File file)
	{
		final String fileName = file.getName();
		final int lastDot = fileName.lastIndexOf(".");

		return lastDot == -1 ? "" : fileName.substring(lastDot + 1);
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
		final File[] folders = folder.listFiles(new FilenameFilter() {
			public boolean accept(File dir, String name)
			{
				final File file = new File(dir.getAbsolutePath() + File.separator + name);

				try
				{
					final String canonicalPath = file.getCanonicalPath();
					final String absolutePath = file.getAbsolutePath();
					if (!canonicalPath.equals(absolutePath))
						return false;
				}
				catch (IOException e)
				{
					return false;
				}

				return file.isDirectory();
			}
		});

		if (folders == null)
			return new LinkedList<File>();
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
		final File[] files = folder.listFiles(new FilenameFilter() {
			public boolean accept(File dir, String name)
			{
				final File file = new File(dir.getAbsolutePath() + File.separator + name);
				if (!file.isFile())
					return false;

				try
				{
					final String canonicalPath = file.getCanonicalPath();
					final String absolutePath = file.getAbsolutePath();
					if (!canonicalPath.equals(absolutePath))
						return false;
				}
				catch (IOException e)
				{
					return false;
				}

				if (extensions == null)
					return true;

				name = name.toLowerCase();
				for (String extension : extensions)
					if (name.endsWith(extension.toLowerCase()))
						return true;

				return false;
			}
		});

		if (files == null)
			return new LinkedList<File>();
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
	public static void copyFile(File src, File dst, boolean preserveLastModified)
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
			if (preserveLastModified)
				dst.setLastModified(src.lastModified());
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}
	public static void copyFolder(File src, File dst, boolean preserveLastModified)
	{
		Misc.copyFolder(src, dst, null, preserveLastModified);
	}
	public static void copyFolder(File src, File dst, String[] extensions, boolean preserveLastModified)
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

			Misc.copyFile(file, dstFile, preserveLastModified);
		}
	}
	public static void copyToFolder(List<File> files, File folder, boolean preserveLastModified)
	{
		for (File file : files)
			Misc.copyToFolder(file, folder, preserveLastModified);
	}
	public static void copyToFolder(File file, File folder, boolean preserveLastModified)
	{
		final String folderPath = folder.getPath();
		final String fileName = file.getName();
		final String dstPath = folderPath + File.separator + fileName;
		final File dst = new File(dstPath);

		Misc.copyFile(file, dst, preserveLastModified);
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

		Misc.copyFolder(src, dst, extensions, true);
		Misc.delete(src, true);
	}
	public static void moveFile(File src, File dst)
	{
		if (!src.exists())
			return;

		Misc.copyFile(src, dst, true);
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
	public static List<String> readLines(String path)
	{
		final File file = new File(path);

		return Misc.readLines(file, null);
	}
	public static List<String> readLines(String path, String charsetName)
	{
		final File file = new File(path);

		return Misc.readLines(file, charsetName);
	}
	public static List<String> readLines(File file)
	{
		return Misc.readLines(file, null);
	}
	public static List<String> readLines(File file, String charsetName)
	{
		final String text = Misc.read(file);

		return Misc.getLines(text);
	}
	public static String read(String path)
	{
		final File file = new File(path);

		return Misc.read(file, null);
	}
	public static String read(String path, String charsetName)
	{
		final File file = new File(path);

		return Misc.read(file, charsetName);
	}
	public static String read(File file)
	{
		return Misc.read(file, null);
	}
	public static String read(File file, String charsetName)
	{
		String text = "";
		FileInputStream fis = null;
		InputStreamReader isr = null;
		BufferedReader br = null;
		String lineSeparator = System.lineSeparator();

		try
		{
			fis = new FileInputStream(file);
			isr = charsetName != null ? new InputStreamReader(fis, charsetName) : new InputStreamReader(fis);
			br = new BufferedReader(isr);

			String line;
			while ((line = br.readLine()) != null)
				text += line + lineSeparator;

			int lastNewLine = text.lastIndexOf(lineSeparator);
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
				if (isr != null)
					isr.close();
				if (fis != null)
					fis.close();
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		}

		return text;
	}
	public static String read2(File file, Charset charset)
	{
		try
		{
			final byte[] encoded = Files.readAllBytes(file.toPath());
			return new String(encoded, charset);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

		return "";
	}

	/****************************************************************/
	/**************************** Write *****************************/
	/****************************************************************/
	public static void write(String path, String text, boolean append)
	{
		final File file = new File(path);

		Misc.write(file, text, append, null);
	}
	public static void write(String path, String text, boolean append, String charsetName)
	{
		final File file = new File(path);

		Misc.write(file, text, append, charsetName);
	}
	public static void write(File file, String text, boolean append)
	{
		Misc.write(file, text, append, null);
	}
	public static void write(File file, String text, boolean append, String charsetName)
	{
		Misc.createFile(file);
		try
		{
			final FileOutputStream fos = new FileOutputStream(file, append);
			final OutputStreamWriter osw = charsetName != null ? new OutputStreamWriter(fos, charsetName) : new OutputStreamWriter(fos);

			osw.write(text);
			osw.flush();
			osw.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}
	public static void write(File file, InputStream inputStream, boolean background, boolean append)
	{
		if (background)
			new Thread()
			{
				public void run()
				{
					Misc.write(file, inputStream, append);
				}
			}.start();
		else
			Misc.write(file, inputStream, append);
	}
	public static void write(File file, InputStream inputStream, boolean append)
	{
		final Path path = file.toPath();

		try
		{
			Files.createDirectories(path);
			if (append)
				Files.copy(inputStream, path);
			else
				Files.copy(inputStream, path, StandardCopyOption.REPLACE_EXISTING);
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}
	private static void write2(File file, InputStream inputStream, boolean append, String charsetName)
	{
		final InputStreamReader isr = new InputStreamReader(inputStream);
		final BufferedReader br = new BufferedReader(isr);
		final String lineSeparator = System.lineSeparator();

		try
		{
			String line;
			if (!append)
				Misc.delete(file);
			while ((line = br.readLine()) != null)
				Misc.write(file, line + lineSeparator, true, charsetName);
			br.close();
			isr.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private Misc()
	{
		
	}
}