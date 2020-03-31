package upv.slicing.edg.graph;

import org.jgrapht.graph.DirectedPseudograph;

import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;

/**
 * A graph with the following properties:
 * <ul>
 *     <li>Nodes are of type {@link N}.</li>
 *     <li>A root node (can be set any number of times).</li>
 *     <li>Directed, unweighted edges of type {@link E}.</li>
 *     <li>Self-loops and parallel edges.</li>
 *     <li>Both nodes and edges must be unique (stored on sets).</li>
 * </ul>
 */
public class GraphWithRoot<N, E> extends DirectedPseudograph<N, E> {
	/** The vertex identified as the root of the graph. */
	protected N rootNode;

	/** Construct a new graph without any vertices or edges */
	public GraphWithRoot()
	{
		super(null, null, false);
	}

	/** Returns the root vertex if it exists, or null otherwise. */
	public N getRootNode()
	{
		return rootNode;
	}

	/**
	 * Set a root vertex. If {@code root} does no exist in the graph it is added.
	 * @param root the vertex to set as the root and optionally add if it does not
	 *             exist in the graph.
	 * @throws NullPointerException If the argument is null.
	 */
	public void setRootNode(N root)
	{
		Objects.requireNonNull(root, "Root vertex can't be null!");
		this.rootNode = root;
		addVertex(root);
	}

	/**
	 * Search the vertices' data against an arbitrary criterion.
	 * @param predicate A condition that must be fulfilled
	 * @return the first vertex with a matching data, null if no matches are found
	 */
	public N findFirstNode(Predicate<N> predicate)
	{
		for (N v : vertexSet())
			if (predicate.test(v))
				return v;
		return null;
	}

	/**
	 * Search the vertices with an arbitrary criterion.
	 * @param predicate A condition that must be fulfilled to be included in the result
	 * @return all vertex with a matching data, empty list if no matches are found
	 */
	public List<N> findAllNodes(Predicate<N> predicate)
	{
		List<N> matches = new LinkedList<>();
		for (N v : vertexSet())
			if (predicate.test(v))
				matches.add(v);
		return matches;
	}

	@Override
	public String toString()
	{
		String str = "Graph[";
		Optional<String> vertices = vertexSet().stream().map(Object::toString).reduce((a, b) -> a + ", " + b);
		if (vertices.isPresent())
			str += vertices;
		str += ']';
		return str;
	}
}
