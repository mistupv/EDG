package edg.traverser;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;

import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;

public class EDGTraverser
{
	public static enum Direction { Forwards, Backwards }

	public static Node getNode(EDG edg, int id)
	{
		return edg.findNodeByData(null, ( o1, o2 ) -> o2.getId() - id);
	}
	public static List<Node> getNodes(EDG edg, NodeInfo.Type type)
	{
		final Node root = edg.getRootNode();

		return EDGTraverser.getDescendants(root, type);
	}
	public static List<Node> getNodes(EDG edg, Predicate<Node> predicate)
	{
		final Node root = edg.getRootNode();

		return EDGTraverser.getDescendants(root, predicate);
	}
	public static List<Node> getDescendants(Node root)
	{
		return EDGTraverser.getDescendants(root, node -> true);
	}
	public static List<Node> getDescendants(Node root, NodeInfo.Type type)
	{
		return EDGTraverser.getDescendants(root, node -> node.getData().getType() == type);
	}
	public static List<Node> getDescendants(Node root, Predicate<Node> predicate)
	{
		final List<Node> descendants = new LinkedList<Node>();
		final List<Node> children = EDGTraverser.getChildren(root);

		for (Node child : children)
		{
			if (predicate.test(child))
				descendants.add(child);
			descendants.addAll(EDGTraverser.getDescendants(child, predicate));
		}

		return descendants;
	}
	public static List<Node> getDescendants(Node root, List<Node> nodes)
	{
		final List<Node> descendants = new LinkedList<Node>();

		for (Node node : nodes)
			if (EDGTraverser.isDescendant(root, node))
				descendants.add(node);

		return descendants;
	}
	public static boolean isDescendant(Node root, Node node)
	{
		Node ancestor = node;

		while (ancestor != root && ancestor != null)
			ancestor = EDGTraverser.getParent(ancestor);

		return ancestor != null;
	}

	public static Node getAncestor(Node node, NodeInfo.Type nodeType)
	{
		Node ancestor = node;
		NodeInfo.Type ancestorType = ancestor.getData().getType();

		while (ancestorType != null && ancestorType != nodeType)
		{
			ancestor = EDGTraverser.getParent(ancestor);
			ancestorType = ancestor == null ? null : ancestor.getData().getType();
		}

		return ancestor;
	}
	public static Node getParent(Node node)
	{
		final List<Node> nodes = EDGTraverser.getNodes(node, Direction.Backwards, EdgeInfo.Type.Structural);
		if (nodes.size() > 1)
			throw new RuntimeException("More than one parent");
		if (nodes.isEmpty())
			return null;
		return nodes.get(0);
	}
	public static Node getSibling(Node node, int index)
	{
		final List<Node> siblings = EDGTraverser.getSiblings(node);

		return siblings.get(index);
	}
	public static List<Node> getSiblings(Node node)
	{
		final Node parent = EDGTraverser.getParent(node);

		return EDGTraverser.getChildren(parent);
	}
	public static List<Node> getChildren(Node node)
	{
		return EDGTraverser.getNodes(node, Direction.Forwards, EdgeInfo.Type.Structural);
	}
	public static Node getChild(Node node, int childIndex)
	{
		final List<Node> children = EDGTraverser.getChildren(node);

		return children.get(childIndex);
	}
//SEARCH BY NODETYPE
	public static Node getChild(Node node, NodeInfo.Type type)
	{
		final List<Node> children = EDGTraverser.getChildren(node);
		
		for (Node child: children)
			if (child.getData().getType() == type)
				return child;
		return null;
	}
	public static int getChildIndex(Node node)
	{
		final List<Node> siblings = EDGTraverser.getSiblings(node);

		return siblings.indexOf(node);
	}

	public static List<Node> getInputs(Node node, Direction direction)
	{
		return EDGTraverser.getNodes(node, direction, EdgeInfo.Type.Input);
	}
	public static List<Node> getOutputs(Node node, Direction direction)
	{
		return EDGTraverser.getNodes(node, direction, EdgeInfo.Type.Output);
	}

	public static List<Node> getNodes(Node node, Direction direction)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Edge> edges = EDGTraverser.getEdges(node, direction);

		for (Edge edge : edges)
		{
			final Node node0 = direction == Direction.Backwards ? edge.getFrom() : edge.getTo();
			if (!nodes.contains(node0))
				nodes.add(node0);
		}

		return nodes;
	}
	public static List<Node> getNodes(Node node, Direction direction, EdgeInfo.Type edgeType)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Edge> edges = EDGTraverser.getEdges(node, direction, edgeType);

		for (Edge edge : edges)
		{
			final Node node0 = direction == Direction.Backwards ? edge.getFrom() : edge.getTo();
			if (!nodes.contains(node0))
				nodes.add(node0);
		}

		return nodes;
	}
	public static List<Edge> getEdges(Node node, Direction direction)
	{
		return direction == Direction.Backwards ? node.getIncomingEdges() : node.getOutgoingEdges();
	}
	public static List<Edge> getEdges(Node node, Direction direction, EdgeInfo.Type edgeType)
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Edge> allEdges = direction == Direction.Backwards ? node.getIncomingEdges() : node.getOutgoingEdges();

		for (Edge edge : allEdges)
			if (edge.getData().getType() == edgeType)
				edges.add(edge);

		return edges;
	}

	public static Node getResult(Node node)
	{
		final NodeInfo info = node.getData();
		final List<Node> siblings = EDGTraverser.getSiblings(node);
		final List<Node> children = EDGTraverser.getChildren(node);

		switch (info.getType())
		{
			// This expression
			case Result:
				return node;

			// Last sibling
			case Variable:
			case Literal:
			case Block:
			case If:
			case Call:
			case DataConstructorAccess:
			case List:
			case DataConstructor:
			case Operation:
			case Switch:
			case Equality:
			case ListComprehension:
			case Routine:
				return siblings.get(siblings.size() - 1);

			// Last child
			case Clause:
			case Case:
			case Expression:
			case Generator:
			case Callee:
				return children.get(children.size() - 1);

			// Last child (optional)
			case Condition:
				// Parameters (esto pasa cuando el scope de una call no se encuentra. Va a buscarlo a los Parametros de entrada de la funcion)
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
				throw new RuntimeException("Type not contemplated: " + info.getType());
		}
	}

	public static boolean isPatternZone(Node node)
	{
		Node ancestor = node;
		NodeInfo.Type ancestorType = ancestor.getData().getType();

		while (ancestorType != NodeInfo.Type.Root)
		{
			if (ancestorType == NodeInfo.Type.Parameters || ancestorType == NodeInfo.Type.Selectable)
				return true;

			final Node ancestorParent = EDGTraverser.getParent(ancestor);
			final NodeInfo.Type ancestorParentType = ancestorParent.getData().getType();
			final int ancestorChildIndex = EDGTraverser.getChildIndex(ancestor);
			if ((ancestorParentType == NodeInfo.Type.Generator || ancestorParentType == NodeInfo.Type.Equality) && ancestorChildIndex == 0)
				return true;

			ancestor = ancestorParent;
			ancestorType = ancestor.getData().getType();
		}

		return false;
	}
}