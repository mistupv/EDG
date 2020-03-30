package upv.slicing.edg.graph;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * A labelled AST graph. The AST itself is kept in a copy within, and can
 * be accessed via {@link #structuralOutgoingEdgesOf(Node)} and {@link #structuralIncomingEdgesOf(Node)}.
 */
public class LAST extends GraphWithRoot {

	/** Map connecting each node to its corresponding result node. */
	private Map<Node, Node> resultFromNode = new HashMap<>();
	/** Map connecting each result node to its corresponding node. */
	private Map<Node, Node> nodeFromResult = new HashMap<>();

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
	 * Once registered, the result node associated with a given node should not be altered.
	 * @param node An expression node of the graph.
	 * @param result The result of an expression node of the graph.
	 * @throws NullPointerException If any of the arguments are null;
	 */
	public void registerNodeResPair(Node node, Node result)
	{
		Objects.requireNonNull(node);
		Objects.requireNonNull(result);
		assert result.getType() == Node.Type.Result;
		assert !resultFromNode.containsKey(node);
		assert !nodeFromResult.containsKey(result);
		resultFromNode.put(node, result);
		nodeFromResult.put(result, node);
	}

	/**
	 * Obtain the result node associated with a given node.
	 * If called with a non-expression as an argument, the result
	 * will be the argument itself.
	 */
	public Node getResFromNode(Node node)
	{
		if (!node.getInfo().isExpression())
			return node;
		if (resultFromNode.containsKey(node))
			return resultFromNode.get(node);
		return node;
	}

	/**
	 * Obtain the node associated with a given result node.
	 * If called with a non-result node as an argument, the result
	 * will be the result node itself.
	 */
	public Node getNodeFromRes(Node res)
	{
		if (res.getType() != Node.Type.Result)
			return res;
		assert nodeFromResult.containsKey(res);
		return nodeFromResult.get(res);
	}

	/** A direction when traversing graphs. */
	public enum Direction {Forwards, Backwards}

	/** Find the first node that matches the given id in this graph. */
	public Node getNode(int id)
	{
		return findFirstNode(o -> o.getId() == id);
	}

	/** Find all nodes that match the given type in this graph. */
	public List<Node> getNodes(Node.Type type)
	{
		return getNodes(n -> type.equals(n.getType()));
	}

	/** Find all nodes that match the given predicate in this graph. */
	public List<Node> getNodes(Predicate<Node> predicate)
	{
		return vertexSet().stream().filter(predicate).collect(Collectors.toList());
	}

	/** Find all nodes that descend from the argument. */
	public List<Node> getDescendants(Node root)
	{
		return getDescendants(root, n -> true);
	}

	/** Find all nodes of a given type that descend from the argument. */
	public List<Node> getDescendants(Node root, Node.Type type)
	{
		return getDescendants(root, n -> n.getType() == type);
	}

	/** Find all nodes that descend from the argument and match the given predicate. */
	public List<Node> getDescendants(Node root, Predicate<Node> predicate)
	{
		// Skip DFS if we want to scan the full graph.
		if (root == rootNode)
			return getNodes(predicate);
		final List<Node> descendants = new LinkedList<>();
		final List<Node> children = getChildren(root);

		for (Node child : children)
		{
			if (predicate.test(child))
				descendants.add(child);
			descendants.addAll(getDescendants(child, predicate));
		}

		 return descendants;
	}

	/** Find the first ancestor of the argument that matches the given type. */
	public Node getAncestor(Node node, Node.Type nodeType)
	{
		node = getParent(node);
		while (node != null)
		{
			if (node.getType() == nodeType)
				return node;
			node = getParent(node);
		}
		return null;
	}

	/**
	 * Finds the parent of the current node in the AST.
	 * If no parent is found, {@code null} is returned.
	 * @throws IllegalStateException If the node has multiple parents.
	 */
	public Node getParent(Node node)
	{
		final List<Node> parentList = getStructuralNodes(node, Direction.Backwards);
		if (parentList.size() > 1)
			throw new IllegalStateException("More than one parent");
		if (parentList.isEmpty())
			return null;
		return parentList.get(0);
	}

	/**
	 * Finds the given sibling in order for the given node.
	 * @param node  The node whose sibling should be found.
	 * @param index The sibling's index.
	 */
	public Node getSibling(Node node, int index)
	{
		return getSiblings(node).get(index);
	}

	/** Finds all siblings of a node (the nodes that share a parent with the current one). */
	public List<Node> getSiblings(Node node)
	{
		return getChildren(getParent(node));
	}

	/** Finds all children of a node, in the AST. */
	public List<Node> getChildren(Node node)
	{
		return getStructuralNodes(node, Direction.Forwards);
	}

	/** Finds an indexed child of a node. You should probably use {@link #getChild(Node, Node.Type)} */
	public Node getChild(Node node, int childIndex)
	{
		return getChildren(node).get(childIndex);
	}

	/** Finds the given node's index among its siblings. */
	public int getChildIndex(Node node)
	{
		return getSiblings(node).indexOf(node);
	}

	/** Finds all nodes connected via an edge of the given direction to the current node
	 * via {@link Edge.Type#Input Input} edges. */
	public List<Node> getInputs(Node node, Direction direction)
	{
		return getNodes(node, direction, Edge.Type.Input);
	}

	/** Finds all nodes connected via an edge of the given direction to the current node
	 * via {@link Edge.Type#Input Input} edges. */
	public List<Node> getOutputs(Node node, Direction direction)
	{
		return getNodes(node, direction, Edge.Type.Output);
	}

	/** Finds all nodes connected to the current node in the given direction.
	 * This method excludes AST edges. */
	public List<Node> getNodes(Node node, Direction direction)
	{
		final Set<Edge> edges = getEdges(node, direction);
		return getNodesOfSet(edges, direction);
	}

	/** Finds all nodes connected to the current node in the given direction
	 * by a structural edge (AST) in the given direction. */
	public List<Node> getStructuralNodes(Node node, Direction direction)
	{
		final Set<Edge> edges = getStructuralEdges(node, direction);
		return getNodesOfSet(edges, direction);
	}

	/** Finds all nodes connected to the current node in the given direction
	 * by an edge of the given type. Excludes AST edges. */
	public List<Node> getNodes(Node node, Direction direction, Edge.Type edgeType)
	{
		final Set<Edge> edges = getEdges(node, direction, edgeType);
		return getNodesOfSet(edges, direction);
	}

	/**
	 * Converts a set of edges to a list of nodes, by extracting the source/target
	 * (depending on the direction) of each edge. Does not filter for duplicates.
	 * The resulting list is sorted: first the positive ids in ascending order and then
	 * the negative ids in any order. This allows the search of children by index,
	 * and transitively by type ({@link #getChild(Node, int)} and {@link #getChild(Node, Node.Type)}).
	 */
	private List<Node> getNodesOfSet(Set<Edge> edges, Direction direction)
	{
		return edges.stream()
				.map(e -> direction == Direction.Backwards ? getEdgeSource(e) : getEdgeTarget(e))
				.sorted((a, b) -> (a.getId() > 0 && b.getId() > 0) ? Integer.compare(a.getId(), b.getId()) : Integer.compare(b.getId(), a.getId()))
				.collect(Collectors.toList());
	}

	/** Obtain an unsorted set of edges incoming/outgoing from the given node
	 * (depending on the direction arg). Ignores AST edges. */
	public Set<Edge> getEdges(Node node, Direction direction)
	{
		switch (direction)
		{
			case Backwards: return incomingEdgesOf(node);
			case Forwards:  return outgoingEdgesOf(node);
			default: throw new IllegalArgumentException("Unknown direction");
		}
	}

	/** Obtain an unsorted set of AST edges incoming/outgoing from the given node
	 * (depending on the direction arg). */
	public Set<Edge> getStructuralEdges(Node node, Direction direction)
	{
		switch (direction)
		{
			case Backwards: return structuralIncomingEdgesOf(node);
			case Forwards:  return structuralOutgoingEdgesOf(node);
			default: throw new IllegalArgumentException("Unknown direction");
		}
	}

	/** Obtain an unsorted set of edges of the given type incoming/outgoing from
	 * the given node (depending on the direction arg). Ignores AST edges. */
	public Set<Edge> getEdges(Node node, Direction direction, Edge.Type edgeType)
	{
		return getEdges(node, direction).stream()
				.filter(e -> e.getType() == edgeType)
				.collect(Collectors.toSet());
	}

	/**
	 * Obtain the result node from a given node. This should only be used
	 * when the result nodes have not been generated yet.
	 * @deprecated Replaced by {@link #getResFromNode(Node)}.
	 */
	@Deprecated
	public Node getResult(Node node)
	{
		final List<Node> siblings = getSiblings(node);
		final List<Node> children = getChildren(node);

		switch (node.getType())
		{
			// This expression
			case DefaultCase:
			case Result:
			case Update:
			case ParameterIn:
				return node;

			// Last sibling
			case Literal:
			case Block:
			case If:
			case Call:
			case DataConstructorAccess:
			case FieldAccess:
			case List:
			case DataConstructor:
			case Operation:
			case Switch:
			case Equality:
			case ListComprehension:
			case Routine:
			case CLoop:
			case RLoop:
			case FLoop:
			case Throw:
			case Reference:
				return siblings.get(siblings.size() - 1);

			case Variable:
				return getResFromNode(node);
			// Last child
			case Clause:
			case Case:
			case Expression:
			case Generator:
			case Callee:
				return children.get(children.size() - 1);

			// Last child (optional)
			case Condition:
			// Parameters: esto pasa cuando el scope de una call no se encuentra.
			// Va a buscarlo a los Parametros de entrada de la funcion
			case Parameters:
			case Return:
				return children.isEmpty() ? null : children.get(children.size() - 1);

			// TYPES
			case Type:
			case TypeCheck:
			case TypeTransformation:
				return siblings.get(siblings.size() - 1);

			// Others
			case Root:
			default:
				throw new RuntimeException("Type not contemplated: " + node.getType());
		}
	}

	/** Finds a module by its name. Throws exceptions if none or multiple matching nodes are found. */
	public Node getModuleByName(String name)
	{
		List<Node> list = findAllNodes(n -> n.getType().equals(Node.Type.Module) && n.getName().equals(name));
		if (list.size() > 1)
			throw new IllegalStateException("Multiple modules share the same name!");
		if (list.isEmpty())
			throw new IllegalArgumentException("No module name " + name + " has been found!");
		return list.get(0);
	}

	/**
	 * Checks if a given node is within a pattern zone. A pattern zone is defined as being within:
	 * <ul>
	 *     <li>A {@link Node.Type#Parameters} or {@link Node.Type#Selectable} node.</li>
	 *     <li>A first child of a {@link Node.Type#Generator} or {@link Node.Type#Equality} node.</li>
	 * </ul>
	 */
	public boolean isPatternZone(Node node)
	{
		while (node.getType() != Node.Type.Root)
		{
			if (node.getType() == Node.Type.Parameters || node.getType() == Node.Type.Selectable)
				return true;

			int childIndex = getChildIndex(node);
			node = getParent(node);

			if ((node.getType() == Node.Type.Generator || node.getType() == Node.Type.Equality) && childIndex == 0)
				return true;
		}

		return false;
	}

	/** Obtains the child of a node given its type. This method will not look through the real types
	 * of the children, relying on a (node, type) -> index pre-build relationship. */
	public Node getChild(Node node, Node.Type type)
	{
		switch (node.getType())
		{
			case Clause:
				switch (type)
				{
					case ParameterIn:
						return getChild(node, 0);
					case Parameters:
						return getChild(node, 1);
					case ParameterOut:
						return getChild(node, 2);
					case Guard:
						return getChild(node, 3);
					case Body:
						return getChild(node, 4);
				}
				break;
			case Call:
				switch (type)
				{
					case Callee:
						return getChild(node, 0);
					case ArgumentIn:
						return getChild(node, 1);
					case Arguments:
						return getChild(node, 2);
					case ArgumentOut:
						return getChild(node, 3);
				}
				break;
			case Callee:
				switch (type)
				{
					case Scope:
						return getChild(node, 0);
					case Name:
						return getChild(node, 1);
				}
				break;
			case If:
				switch (type)
				{
					case Condition:
						return getChild(node, 0);
					case Then:
						return getChild(node, 1);
					case Else:
						return getChild(node, 2);
				}
				break;
			case Switch:
				switch (type)
				{
					case Selector:
						return getChild(node, 0);
					case Cases:
						return getChild(node, 1);
				}
				break;
			case Case:
				switch (type)
				{
					case Selectable:
						return getChild(node, 0);
					case Guard:
						return getChild(node, 1);
					case Body:
						return getChild(node, 2);
				}
				break;
			case DefaultCase:
				switch (type)
				{
					case Body:
						return getChild(node, 0);
				}
			case FLoop:
				switch (type)
				{
					case Init:
						return getChild(node, 0);
					case Condition:
						return getChild(node, 1);
					case Body:
						return getChild(node, 2);
					case Update:
						return getChild(node, 3);
				}
				break;
			case CLoop:
				switch (type)
				{
					case Condition:
						return getChild(node, 0);
					case Body:
						return getChild(node, 1);
				}
				break;
			case RLoop:
				switch (type)
				{
					case Body:
						return getChild(node, 0);
					case Condition:
						return getChild(node, 1);
				}
				break;
			case Foreach:
				switch (type)
				{
					case Iterator:
						return getChild(node, 0);
					case Body:
						return getChild(node, 1);
				}
			case ExHandler:
				switch (type)
				{
					case Try:
						return getChild(node, 0);
					case Catch:
						return getChild(node, 1);
					case Finally:
						return getChild(node, 2);
				}
				break;
			case TypeTransformation:
				switch (type)
				{
					case Type:
						return getChild(node, 0);
					case Variable:
						return getChild(node, 1);
				}
				break;
			case TypeCheck:
				switch (type)
				{
					case Variable:
						return getChild(node, 0);
					case Type:
						return getChild(node, 1);
				}
			case FieldAccess:
			case DataConstructorAccess:
				switch (type)
				{
					case Variable:
						return getChild(node, 0);
					case Index:
						return getChild(node, 1);
				}
				break;
			case Scope:
			case Name:
			case Condition:
			case Selector:
			case Selectable:
			case Literal:
			case Return:
			case Enclosed:
				switch (type)
				{
					case Value:
						return getChild(node, 0);
				}
				break;
			case Equality:
				switch (type)
				{
					case Pattern:
						return getChild(node, 0);
					case Value:
						return getChild(node, 1);
				}
				break;
			case Generator:
				switch (type)
				{
					case Variable:
						return getChild(node, 0);
					case Iterator:
						return getChild(node, 1);
				}
			default:
				break;
		}
		throw new IllegalStateException("Parent-Child combination not considered: " + node.getType() + ", " + type);
	}

	/** Obtains the sibling of a node by its type.
	 * @see #getChild(Node, Node.Type) */
	public Node getSibling(Node node, Node.Type type)
	{
		final Node parent = getParent(node);

		return getChild(parent, type);
	}
}
