package upv.slicing.edg.graph;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * A labelled AST graph. The AST itself is kept in a copy within, and can
 * be accessed via {@link #structuralOutgoingEdgesOf(Node)} and {@link #structuralIncomingEdgesOf(Node)}.
 */
public class LAST extends GraphWithRoot {

	private Map<Node,Node> resultFromNode = new HashMap<>();
	private Map<Node,Node> nodeFromResult = new HashMap<>();

	/** @see #addVertex(Node) */
	public boolean addNode(Node node)
	{
		return addVertex(node);
	}

	/**
	 * Adds a node to the graph. Nodes are kept in a set, so it must be unique within the graph.
	 * @param node The node to be added.
	 * @return True if the node has been added.
	 * @throws NullPointerException If the argument is null.
	 */
	@Override
	public boolean addVertex(Node node)
	{
		return super.addVertex(node);
	}

	/** Add an edge to the graph. If the type is Structural, then it will also be
	 * added to the AST representation of the program. */
	public boolean addEdge(Node from, Node to, Edge.Type type)
	{
		final Edge e = new Edge(type);
		if (type == Edge.Type.Structural)
			addStructuralEdge(from, to, e);
		return super.addEdge(from, to, e);
	}

	/** @see #addStructuralEdge(Node, Node, Edge) */
	public boolean addStructuralEdge(Node from, Node to)
	{
		return addStructuralEdge(from, to, new Edge(Edge.Type.Structural));
	}

	/**
	 * Adds a structural edge, representing the AST.
	 * @return true if the Edge was added, false if from already has this Edge
	 * @throws IllegalArgumentException if {@code from} or {@code to} are not vertices in the graph
	 * @throws NullPointerException if {@code from} or {@code to} are null
	 */
	public boolean addStructuralEdge(Node from, Node to, Edge e)
	{
		return addEdge(from, to, new Edge.NonTraversable(e));
	}

	/**
	 * Obtain the list of structural edges that end in {@code node}.
	 * These edges represent the AST of the code.
	 * @see org.jgrapht.graph.AbstractGraph#incomingEdgesOf(Object)
	 */
	public Set<Edge> structuralIncomingEdgesOf(Node node)
	{
		return super.incomingEdgesOf(node).stream()
				.filter(Edge.NonTraversable.class::isInstance)
				.collect(Collectors.toSet());
	}

	/**
	 * Obtain the list of edges that start in {@code node}.
	 * These edges represent the AST of the code.
	 * @see org.jgrapht.graph.AbstractGraph#outgoingEdgesOf(Object)
	 */
	public Set<Edge> structuralOutgoingEdgesOf(Node node)
	{
		return super.outgoingEdgesOf(node).stream()
			.filter(Edge.NonTraversable.class::isInstance)
			.collect(Collectors.toSet());
	}

	@Override
	public Set<Edge> incomingEdgesOf(Node node)
	{
		return super.incomingEdgesOf(node).stream()
				.filter(e -> !(e instanceof Edge.NonTraversable))
				.collect(Collectors.toSet());
	}

	@Override
	public Set<Edge> outgoingEdgesOf(Node node)
	{

		return super.outgoingEdgesOf(node).stream()
				.filter(e -> !(e instanceof Edge.NonTraversable))
				.collect(Collectors.toSet());
	}

	/**
	 * Remove all edges that match the arguments from this graph.
	 * @param from The source of the edge.
	 * @param to The target of the edge.
	 * @param edgeType The edge's type.
	 * @return True if any edge has been removed from the graph.
	 */
	public boolean removeEDGEdge(Node from, Node to, Edge.Type edgeType)
	{
		boolean result = false;
		for (Edge edge : outgoingEdgesOf(from))
			if (getEdgeTarget(edge).equals(to) && edge.getType() == edgeType)
				result |= super.removeEdge(edge);
		return result;
	}

	/**
	 * Mark all edges that match the argument as removable and remove them from this graph.
	 * @param from The edge's source node.
	 * @param to The edge's target node.
	 * @param edgeType The type of the edge.
	 */
	public void setRemovableEdge(Node from, Node to, Edge.Type edgeType)
	{
		edgeSet().parallelStream()
				.filter(e -> getEdgeSource(e).equals(from))
				.filter(e -> getEdgeTarget(e).equals(to))
				.filter(e -> e.getType() != edgeType)
				.forEach(this::removeEdge);
		structuralIncomingEdgesOf(to).stream()
				.filter(e -> getEdgeSource(e).equals(from) && e.getType() != edgeType)
				.forEach(this::removeEdge);
		for (Edge edge : structuralIncomingEdgesOf(to))
			if (getEdgeSource(edge).equals(from) && edge.getType() != edgeType)
			{
				removeEdge(edge);
				edge.mark();
			}
	}

	/**
	 * Add expression nodes and associated results to graph node-result and result-node maps.
	 * @param node An expression node of the graph.
	 * @param result The result of an expression node of the graph.
	 */

	public void addNodeResInfo(Node node, Node result)
	{
		resultFromNode.put(node, result);
		nodeFromResult.put(result, node);
	}

	public Node getResFromNode(Node node)
	{
		return resultFromNode.get(node);
	}

	public Node getNodeFromRes(Node res)
	{
		return nodeFromResult.get(res);
	}
}