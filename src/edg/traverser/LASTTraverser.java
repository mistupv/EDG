package edg.traverser;

import edg.graph.*;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

public class LASTTraverser {
	public enum Direction {Forwards, Backwards}

	public static Node getNode(LAST last, int id)
	{
		return last.findNodeByData(null, (o1, o2) -> o2.getId() - id);
	}

	public static List<Node> getNodes(LAST last, NodeInfo.Type type)
	{
		final Node root = last.getRootNode();

		return LASTTraverser.getDescendants(root, type);
	}
	public static List<Node> getNodes(LAST last, Predicate<Node> predicate)
	{
		final Node root = last.getRootNode();

		return LASTTraverser.getDescendants(root, predicate);
	}
	public static List<Node> getDescendants(Node root)
	{
		return LASTTraverser.getDescendants(root, node -> true);
	}
	public static List<Node> getDescendants(Node root, NodeInfo.Type type)
	{
		return LASTTraverser.getDescendants(root, node -> node.getData().getType() == type);
	}
	public static List<Node> getDescendants(Node root, Predicate<Node> predicate)
	{
		final List<Node> descendants = new LinkedList<Node>();
		final List<Node> children = LASTTraverser.getChildren(root);

		for (Node child : children)
		{
			if (predicate.test(child))
				descendants.add(child);
			descendants.addAll(LASTTraverser.getDescendants(child, predicate));
		}

		return descendants;
	}
	public static List<Node> getDescendants(Node root, List<Node> nodes)
	{
		final List<Node> descendants = new LinkedList<Node>();

		for (Node node : nodes)
			if (LASTTraverser.isDescendant(root, node))
				descendants.add(node);

		return descendants;
	}
	public static boolean isDescendant(Node root, Node node)
	{
		Node ancestor = node;

		while (ancestor != root && ancestor != null)
			ancestor = LASTTraverser.getParent(ancestor);

		return ancestor != null;
	}

	public static Node getAncestor(Node node, NodeInfo.Type nodeType)
	{
		Node ancestor = node;
		NodeInfo.Type ancestorType = ancestor.getData().getType();

		while (ancestorType != null && ancestorType != nodeType)
		{
			ancestor = LASTTraverser.getParent(ancestor);
			ancestorType = ancestor == null ? null : ancestor.getData().getType();
		}

		return ancestor;
	}
	public static Node getParent(Node node)
	{
		final List<Node> nodes = LASTTraverser.getStructuralNodes(node, Direction.Backwards);
		if (nodes.size() > 1)
			throw new RuntimeException("More than one parent");
		if (nodes.isEmpty())
			return null;
		return nodes.get(0);
	}
	public static Node getSibling(Node node, int index)
	{
		final List<Node> siblings = LASTTraverser.getSiblings(node);

		return siblings.get(index);
	}
	public static List<Node> getSiblings(Node node)
	{
		final Node parent = LASTTraverser.getParent(node);

		return LASTTraverser.getChildren(parent);
	}
	public static List<Node> getChildren(Node node)
	{
		return LASTTraverser.getStructuralNodes(node, Direction.Forwards);
	}
	public static Node getChild(Node node, int childIndex)
	{
		final List<Node> children = LASTTraverser.getChildren(node);

		return children.get(childIndex);
	}

//SEARCH BY NODETYPE
	public static Node getChild(Node node, NodeInfo.Type type)
	{
		final List<Node> children = LASTTraverser.getChildren(node);
		
		for (Node child: children)
			if (child.getData().getType() == type)
				return child;
		return null;
	}
	public static int getChildIndex(Node node)
	{
		final List<Node> siblings = LASTTraverser.getSiblings(node);

		return siblings.indexOf(node);
	}

	public static List<Node> getInputs(Node node, Direction direction)
	{
		return LASTTraverser.getNodes(node, direction, EdgeInfo.Type.Input);
	}
	public static List<Node> getOutputs(Node node, Direction direction)
	{
		return LASTTraverser.getNodes(node, direction, EdgeInfo.Type.Output);
	}

	public static List<Node> getNodes(Node node, Direction direction)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Edge> edges = LASTTraverser.getEdges(node, direction);

		for (Edge edge : edges)
		{
			final Node node0 = direction == Direction.Backwards ? edge.getFrom() : edge.getTo();
			if (!nodes.contains(node0))
				nodes.add(node0);
		}

		return nodes;
	}

	public static List<Node> getStructuralNodes(Node node, Direction direction)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Edge> edges = LASTTraverser.getStructuralEdges(node, direction);

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
		final List<Edge> edges = LASTTraverser.getEdges(node, direction, edgeType);

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

	public static List<Edge> getStructuralEdges(Node node, Direction direction)
	{
		return direction == Direction.Backwards ? node.getIncomingStructuralEdges() : node.getOutgoingStructuralEdges();
	}

	public static List<Edge> getEdges(Node node, Direction direction, EdgeInfo.Type edgeType)
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Edge> allEdges =
				direction == Direction.Backwards ? node.getIncomingEdges() : node.getOutgoingEdges();

		for (Edge edge : allEdges)
			if (edge.getData().getType() == edgeType)
				edges.add(edge);

		return edges;
	}

	public static Node getResult(Node node)
	{
		final NodeInfo info = node.getData();
		final List<Node> siblings = LASTTraverser.getSiblings(node);
		final List<Node> children = LASTTraverser.getChildren(node);

		switch (info.getType())
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
			//return getVarResult(node, siblings);
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

	public static Node getResFromNode(Node node)
	{
		Set<Node> next = ControlFlowTraverser.step(node, ControlFlowTraverser.Direction.Forwards);
		if (next.size() == 1)
			return next.iterator().next();
		throw new RuntimeException("The next element of a variable in the CFG is only its result");
	}

	public static Node getNodeFromRes(Node resNode)
	{
		Set<Node> next = ControlFlowTraverser.step(resNode, ControlFlowTraverser.Direction.Forwards);
		if (next.size() == 1)
			return next.iterator().next();
		throw new RuntimeException("The next element of a variable in the CFG is only its result");
	}

	// PREVIOUS METHOD FOR DATA CONSTRUCTOR ACCESSES
	private static Node getVarResult(Node node, List<Node> siblings)
	{

		final Node grandParentUseNode = LASTTraverser.getParent(LASTTraverser.getParent(node));
		if (grandParentUseNode.getData().getType() == NodeInfo.Type.DataConstructorAccess)
		{
			final Node parentNode = LASTTraverser.getParent(node);
			if (LASTTraverser.getChildIndex(parentNode) == 0)
				return LASTTraverser.getSibling(grandParentUseNode, 1);
		}
		return siblings.get(siblings.size() - 1);
	}
	
	public static Node getModuleByName(LAST last, String name)
	{
		final List<Node> moduleNodes = LASTTraverser.getNodes(last, NodeInfo.Type.Module);
		
		for (Node moduleNode : moduleNodes)
		{
			if(moduleNode.getData().getName().equals(name))
				return moduleNode;
		}
		return null;
	}
	
	public static boolean isPatternZone(Node node)
	{
		Node ancestor = node;
		NodeInfo.Type ancestorType = ancestor.getData().getType();

		while (ancestorType != NodeInfo.Type.Root)
		{
			if (ancestorType == NodeInfo.Type.Parameters || ancestorType == NodeInfo.Type.Selectable)
				return true;

			final Node ancestorParent = LASTTraverser.getParent(ancestor);
			final NodeInfo.Type ancestorParentType = ancestorParent.getData().getType();
			final int ancestorChildIndex = LASTTraverser.getChildIndex(ancestor);
			if ((ancestorParentType == NodeInfo.Type.Generator || ancestorParentType == NodeInfo.Type.Equality) && ancestorChildIndex == 0)
				return true;

			ancestor = ancestorParent;
			ancestorType = ancestor.getData().getType();
		}

		return false;
	}

	public static boolean isDefinedClass(LAST last, Node node)
	{
		if (node.getData().getType() != NodeInfo.Type.Variable)
			return true;
		final List<Node> modules = LASTTraverser.getNodes(last, NodeInfo.Type.Module);
		final String nodeType =  node.getData().getInfo().getInfo()[1].toString();
		for (Node module : modules)
			if (module.getData().getName().equals(nodeType))
				return true;
		return false;
			
	}
}