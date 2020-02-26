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
 *     <li>Nodes are of type {@link Node}.</li>
 *     <li>A root node (can be set any number of times).</li>
 *     <li>Directed, unweighted edges of type {@link Edge}.</li>
 *     <li>Self-loops and parallel edges.</li>
 *     <li>Both nodes and edges must be unique (stored on sets).</li>
 * </ul>
 */
public class GraphWithRoot extends DirectedPseudograph<Node, Edge> {
	/** The vertex identified as the root of the graph. */
	protected Node rootNode;

	/** Construct a new graph without any vertices or edges */
	public GraphWithRoot()
	{
		super(null, null, false);
	}

	/** Returns the root vertex if it exists, or null otherwise. */
	public Node getRootNode()
	{
		return rootNode;
	}

	/**
	 * Set a root vertex. If {@code root} does no exist in the graph it is added.
	 * @param root the vertex to set as the root and optionally add if it does not
	 *             exist in the graph.
	 * @throws NullPointerException If the argument is null.
	 */
	public void setRootNode(Node root)
	{
		Objects.requireNonNull(root, "Root vertex can't be null!");
		this.rootNode = root;
		addVertex(root);
	}

	/**
	 * Insert a directed Edge into the graph of the corresponding type.
	 * @return true if the edge was added, false if from already has this edge
	 * @throws IllegalArgumentException if {@code from} or {@code to} are not vertices in the graph
	 * @throws NullPointerException if {@code from} or {@code to} are null
	 */
	public boolean addEdge(Node from, Node to, Edge.Type type)
	{
		return this.addEdge(from, to, new Edge(type));
	}

	/**
	 * Search the vertices' data against an arbitrary criterion.
	 * @param predicate A condition that must be fulfilled
	 * @return the first vertex with a matching data, null if no matches are found
	 */
	public Node findFirstNode(Predicate<Node> predicate)
	{
		for (Node v : vertexSet())
			if (predicate.test(v))
				return v;
		return null;
	}

	/**
	 * Search the vertices with an arbitrary criterion.
	 * @param predicate A condition that must be fulfilled to be included in the result
	 * @return all vertex with a matching data, empty list if no matches are found
	 */
	public List<Node> findAllNodes(Predicate<Node> predicate)
	{
		List<Node> matches = new LinkedList<>();
		for (Node v : vertexSet())
			if (predicate.test(v))
				matches.add(v);
		return matches;
	}

	@Override
	public String toString()
	{
		String str = "Graph[";
		Optional<String> vertices = vertexSet().stream().map(Node::toString).reduce((a, b) -> a + ", " + b);
		if (vertices.isPresent())
			str += vertices;
		str += ']';
		return str;
	}
}