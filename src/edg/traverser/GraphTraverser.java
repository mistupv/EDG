package edg.traverser;

import java.util.LinkedList;
import java.util.List;

import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;

public class GraphTraverser
{
	public static enum Direction { Forwards, Backwards }

	public static Node getParent(Node node, EdgeInfo.Type edgeType)
	{
		if (edgeType != EdgeInfo.Type.Control && edgeType != EdgeInfo.Type.NormalControl && edgeType != EdgeInfo.Type.StructuralControl)
			throw new RuntimeException("Incorrect edge type: " + edgeType);

		final List<Edge> incomingControlEdges = GraphTraverser.getEdges(node, Direction.Backwards, edgeType);
		if (incomingControlEdges.size() > 1)
			throw new RuntimeException("More than one control edge");
		if (incomingControlEdges.isEmpty())
			return null;
		final Edge incomingControlEdge = incomingControlEdges.get(0);

		return incomingControlEdge.getFrom();
	}
	public static List<Node> getChildren(Node node, EdgeInfo.Type edgeType)
	{
		final List<Edge> outgoingControlEdges = GraphTraverser.getChildEdges(node, edgeType);
		final List<Node> children = new LinkedList<Node>();

		for (Edge outgoingControlEdge : outgoingControlEdges)
		{
			final Node child = outgoingControlEdge.getTo();

			children.add(child);
		}

		return children;
	}
	public static List<Edge> getChildEdges(Node node, EdgeInfo.Type edgeType)
	{
		if (edgeType != EdgeInfo.Type.Control && edgeType != EdgeInfo.Type.NormalControl && edgeType != EdgeInfo.Type.StructuralControl)
			throw new RuntimeException("Incorrect edge type: " + edgeType);
		return GraphTraverser.getEdges(node, Direction.Forwards, edgeType);
	}
	public static int getChildIndex(Node node, EdgeInfo.Type edgeType)
	{
		if (edgeType != EdgeInfo.Type.Control && edgeType != EdgeInfo.Type.NormalControl && edgeType != EdgeInfo.Type.StructuralControl)
			throw new RuntimeException("Incorrect edge type: " + edgeType);

		final Node parent = GraphTraverser.getParent(node, edgeType);
		final List<Node> children = GraphTraverser.getChildren(parent, edgeType);

		for (int childIndex = 0; childIndex < children.size(); childIndex++)
			if (children.get(childIndex) == node)
				return childIndex;
		throw new RuntimeException("Error while looking for the child index");
	}

	public static List<Node> getInputs(Node node, Direction direction)
	{
		final List<Node> inputs = new LinkedList<Node>();
		final EdgeInfo.Type edgeType = EdgeInfo.Type.Input;
		final List<Edge> inputEdges = GraphTraverser.getEdges(node, direction, edgeType);

		for (Edge inputEdge : inputEdges)
		{
			final Node input = direction == Direction.Backwards ? inputEdge.getFrom() : inputEdge.getTo();

			inputs.add(input);
		}

		return inputs;
	}
	public static List<Node> getOutputs(Node node, Direction direction)
	{
		final List<Node> outputs = new LinkedList<Node>();
		final EdgeInfo.Type edgeType = EdgeInfo.Type.Output;
		final List<Edge> outputEdges = GraphTraverser.getEdges(node, direction, edgeType);

		for (Edge outputEdge : outputEdges)
		{
			final Node output = direction == Direction.Backwards ? outputEdge.getFrom() : outputEdge.getTo();

			outputs.add(output);
		}

		return outputs;
	}
	public static List<Node> getIncomings(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Edge> allEdges = node.getIncomingEdges();

		for (Edge edge : allEdges)
		{
			final Node incomingNode = edge.getFrom();
			if (!nodes.contains(incomingNode))
				nodes.add(incomingNode);
		}

		return nodes;
	}
	public static List<Node> getOutgoings(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Edge> allEdges = node.getOutgoingEdges();

		for (Edge edge : allEdges)
		{
			final Node outgoingNode = edge.getTo();
			if (!nodes.contains(outgoingNode))
				nodes.add(outgoingNode);
		}

		return nodes;
	}

	private static List<Edge> getEdges(Node node, Direction direction, EdgeInfo.Type edgeType)
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Edge> allEdges = direction == Direction.Backwards ? node.getIncomingEdges() : node.getOutgoingEdges();
		final List<EdgeInfo.Type> edgeTypes = new LinkedList<EdgeInfo.Type>();

		if (edgeType == EdgeInfo.Type.Control)
		{
			edgeTypes.add(EdgeInfo.Type.NormalControl);
			edgeTypes.add(EdgeInfo.Type.StructuralControl);
		}
		else
			edgeTypes.add(edgeType);

		for (Edge edge : allEdges)
			if (edgeTypes.contains(edge.getData().getType()))
				edges.add(edge);

		return edges;
	}

	public static List<Node> getDescendantNodes(Node root)
	{
		final List<Node> descendants = new LinkedList<Node>();
		final List<Node> children = GraphTraverser.getChildren(root, EdgeInfo.Type.Control);

		for (Node child : children)
		{
			descendants.add(child);
			descendants.addAll(GraphTraverser.getDescendantNodes(child));
		}

		return descendants;
	}
	public static List<Node> getDescendantNodes(Node root, List<Node> nodes)
	{
		final List<Node> descendants = new LinkedList<Node>();

		for (Node node : nodes)
			if (GraphTraverser.isDescendantNode(node, root))
				descendants.add(node);

		return descendants;
	}
	private static boolean isDescendantNode(Node node, Node root)
	{
		Node nodeAncestor = node;

		while (nodeAncestor != root && nodeAncestor != null)
			nodeAncestor = GraphTraverser.getParent(nodeAncestor, EdgeInfo.Type.Control);

		return nodeAncestor != null;
	}

	public static List<Node> getLasts(Node node, boolean constraintsActivated)
	{
		if (constraintsActivated)
			return GraphTraverser.getSelf(node);
// TODO Last roots es la version de Tama
// El siguiente if nunca se ejecutara
		if (constraintsActivated)
			return GraphTraverser.getLastRoots(node);
		return GraphTraverser.getLastNodes(node);
	}
	private static List<Node> getSelf(Node node)
	{
		final List<Node> lastSelf = new LinkedList<Node>();

		lastSelf.add(node);

		return lastSelf;
	}
	private static List<Node> getLastRoots(Node node)
	{
		final List<Node> lastRoots = new LinkedList<Node>();
		final NodeInfo info = node.getData();
		final List<Node> children = GraphTraverser.getChildren(node, EdgeInfo.Type.NormalControl);

		switch (info.getType())
		{
			// All except the first one
			case Case:
				children.remove(0);

			// All expressions
			case Operation:
			case Function:
			case CompoundPattern:
			case If:
			case AnonymousFunction:
				for (Node child : children)
					lastRoots.addAll(GraphTraverser.getLastRoots(child));
				break;

			// This expression
			case TupleExpression:
			case ListExpression:
			case Variable:
			case FunctionIdentifier:
			case Return:
			case Atom:
			case String:
			case Integer:
			case Char:
			case ListComprehensionResult:
				lastRoots.add(node);
				break;

			// Penultimate expression
			case ListComprehension:
				final Node penultimateChild = children.get(children.size() - 2);
				lastRoots.addAll(GraphTraverser.getLastRoots(penultimateChild));
				break;

			// Last expression
			case Clause:
			case Guard:
			case Block:
			case PatternMatching:
			case Body:
			case FunctionCall:
			case Generator:
				final Node lastChild = children.get(children.size() - 1);
				lastRoots.addAll(GraphTraverser.getLastRoots(lastChild));
				break;

			case Root:
			case Other:
			default:
				throw new RuntimeException("Type not contemplated: " + info.getType());
		}

		return lastRoots;
	}
	private static List<Node> getLastNodes(Node node)
	{
		final List<Node> lastNodes = new LinkedList<Node>();
		final NodeInfo info = node.getData();
		final List<Node> children = GraphTraverser.getChildren(node, EdgeInfo.Type.Control);

		switch (info.getType())
		{
			// All except the first one
			case Case:
				children.remove(0);

			// All expressions
			case Function:
			case TupleExpression:
			case ListExpression:
			case CompoundPattern:
			case If:
			case Operation:
			case AnonymousFunction:
				for (Node child : children)
					lastNodes.addAll(GraphTraverser.getLastNodes(child));
				if (info.getType() == NodeInfo.Type.ListExpression && children.isEmpty())
					lastNodes.add(node);
				break;

			// This expression
			case Variable:
			case FunctionIdentifier:
			case Return:
			case Atom:
			case String:
			case Integer:
			case Char:
			case ListComprehensionResult:
				lastNodes.add(node);
				break;

			// Penultimate expression
			case ListComprehension:
				final Node penultimateChild = children.get(children.size() - 2);
				lastNodes.addAll(GraphTraverser.getLastNodes(penultimateChild));
				break;

			// Last expression
			case Clause:
			case Guard:
			case Block:
			case PatternMatching:
			case Body:
			case FunctionCall:
			case Generator:
				final Node lastChild = children.get(children.size() - 1);
				lastNodes.addAll(GraphTraverser.getLastNodes(lastChild));
				break;

			case Root:
			case Other:
			default:
				throw new RuntimeException("Type not contemplated: " + info.getType());
		}

		return lastNodes;
	}

	public static List<Node> getValueNodes(Node node)
	{
		final List<Node> valueNodes = new LinkedList<Node>();
		final NodeInfo info = node.getData();
		final List<Node> children = GraphTraverser.getChildren(node, EdgeInfo.Type.Control);

		switch (info.getType())
		{
			// None
			case Guard:
			case TuplePattern:
			case TupleExpression:
			case ListPattern:
			case ListExpression:
			case BinPattern:
			case BinExpression:
			case BinElementPattern:
			case BinElementExpression:
			case Remote:
				break;

			// All except the first one
			case Case:
				children.remove(0);

			// All expressions
			case Function:
			case CompoundPattern:
			case If:
			case Operation:
			case AnonymousFunction:
				valueNodes.addAll(children);
				break;

			// This expression
			case Variable:
			case FunctionIdentifier:
			case Return:
			case Atom:
			case Default:
			case String:
			case Integer:
			case Char:
			case ListComprehensionResult:
			case BinComprehensionResult:
				break;

			// Penultimate expression
			case ListComprehension:
			case BinComprehension:
				final Node penultimateChild = children.get(children.size() - 2);
				valueNodes.add(penultimateChild);
				break;

			// Last expression
			case Block:
			case PatternMatching:
			case Body:
			case FunctionCall:
			case Generator:
			case BinGenerator:
				final Node lastChild = children.get(children.size() - 1);
				valueNodes.add(lastChild);
				break;

			// Special
			case Clause:
				final Node lastChild0 = children.get(children.size() - 1);
				final List<Node> children0 = GraphTraverser.getChildren(lastChild0, EdgeInfo.Type.Control);
				final Node lastChild1 = children0.get(children0.size() - 1);
				valueNodes.add(lastChild1);
				break;

			case Root:
			case Other:
			default:
				throw new RuntimeException("Type not contemplated: " + info.getType());
		}

		return valueNodes;
	}
}