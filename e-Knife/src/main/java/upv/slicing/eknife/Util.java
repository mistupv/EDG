package upv.slicing.eknife;

import java.io.File;
import java.io.FilenameFilter;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Util {
	/** Joins multiple lists into a single list. */
	@SafeVarargs
	public static <T> List<T> join(List<T>... lists)
	{
		return Stream.of(lists).flatMap(Collection::stream).collect(Collectors.toList());
	}

	/**
	 * Obtain all files match any of the extensions given in the source path or file.
	 * @param source The path you want to explore or file to check.
	 * @param extensions A set of extensions, any of which must match a file for it to
	 *                   be included. Use {@code Set.of("")} to capture all files found.
	 * @throws NullPointerException If any of the parameters are null.
	 */
	public static List<File> getFiles(String source, Set<String> extensions)
	{
		return getFiles(source, (dir, name) -> {
			for (String ext : extensions)
				if (name.endsWith(ext))
					return true;
			return false;
		});
	}

	/**
	 * Obtain all files that pass the given filter in the source path or file.
	 * @param source The path you want to explore or file to check.
	 * @param filter A filter, may not be null. Use {@code (a, b) -> true} if
	 *               you want to capture all files found.
	 * @throws NullPointerException If any of the parameters are null.
	 */
	public static List<File> getFiles(String source, FilenameFilter filter)
	{
		return getFiles(new File(source), filter);
	}

	/** @see #getFiles(String, FilenameFilter) */
	private static List<File> getFiles(File source, FilenameFilter filter)
	{
		List<File> result = new LinkedList<>();
		Deque<File> remaining = new LinkedList<>();
		remaining.add(source);
		while (!remaining.isEmpty())
		{
			File f = remaining.removeFirst();
			if (!f.exists())
				continue;
			if (f.isFile() && filter.accept(f.getParentFile(), f.getName()))
			{
				result.add(f);
			}
			else if (f.isDirectory())
			{
				File[] containedFiles = f.listFiles();
				if (containedFiles != null)
					remaining.addAll(List.of(containedFiles));
			}
		}
		return result;
	}

	private Util()
	{
		throw new IllegalStateException("This is a static-only class!");
	}
}
