package upv.slicing.eknife;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Util {
	/** Joins multiple lists into a single list. */
	@SafeVarargs
	public static <T> List<T> join(List<T>... lists) {
		return Stream.of(lists).flatMap(Collection::stream).collect(Collectors.toList());
	}

	private Util()
	{
		throw new IllegalStateException("This is a static-only class!");
	}
}
