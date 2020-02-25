package upv.slicing.edg.traverser;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class LASTTraverser {
	public enum Direction {Forwards, Backwards}

	public static Node getNode(LAST last, int id)
	{
		return last.findFirstNode(o -> o.getId() == id);
	}

	public static List<Node> getNodes(LAST last, Node.Type type)
	{
		final Node root = last.getRootNode();

		return getDescendants(last, root, type);
	}
	public static List<Node> getNodes(LAST last, Predicate<Node> predicate)
	{
		final Node root = last.getRootNode();

		return getDescendants(last, root, predicate);
	}
	public static List<Node> getDescendants(LAST last, Node root)
	{
		return getDescendants(last, root, node -> true);
	}
	public static List<Node> getDescendants(LAST last, Node root, Node.Type type)
	{
		return getDescendants(last, root, node -> node.getType() == type);
	}
	public static List<Node> getDescendants(LAST last, Node root, Predicate<Node> predicate)
	{
		final List<Node> descendants = new LinkedList<>();
		final List<Node> children = getChildren(last, root);

		for (Node child : children)
		{
			if (predicate.test(child))
				descendants.add(child);
			descendants.addAll(getDescendants(last, child, predicate));
		}

		return descendants;
	}
	public static List<Node> getDescendants(LAST last, Node root, List<Node> nodes)
	{
		final List<Node> descendants = new LinkedList<>();

		for (Node node : nodes)
			if (isDescendant(last, root, node))
				descendants.add(node);

		return descendants;
	}
	public static boolean isDescendant(LAST last, Node root, Node node)
	{
		Node ancestor = node;

		while (ancestor != root && ancestor != null)
			ancestor = getParent(last, ancestor);

		return ancestor != null;
	}

	public static Node getAncestor(LAST last, Node node, Node.Type nodeType)
	{
		Node ancestor = node;
		Node.Type ancestorType = ancestor.getType();

		while (ancestorType != null && ancestorType != nodeType)
		{
			ancestor = getParent(last, ancestor);
			ancestorType = ancestor == null ? null : ancestor.getType();
		}

		return ancestor;
	}
	public static Node getParent(LAST last, Node node)
	{
		final List<Node> nodes = getStructuralNodes(last, node, Direction.Backwards);
		if (nodes.size() > 1)
			throw new RuntimeException("More than one parent");
		if (nodes.isEmpty())
			return null;
		return nodes.get(0);
	}
	public static Node getSibling(LAST last, Node node, int index)
	{
		final List<Node> siblings = getSiblings(last, node);

		return siblings.get(index);
	}
	public static List<Node> getSiblings(LAST last, Node node)
	{
		final Node parent = getParent(last, node);

		return getChildren(last, parent);
	}
	public static List<Node> getChildren(LAST last, Node node)
	{
		return getStructuralNodes(last, node, Direction.Forwards);
	}
	public static Node getChild(LAST last, Node node, int childIndex)
	{
		final List<Node> children = getChildren(last, node);

		return children.get(childIndex);
	}

//SEARCH BY NODETYPE
	public static Node getChild(LAST last, Node node, Node.Type type)
	{
		final List<Node> children = getChildren(last, node);
		
		for (Node child: children)
			if (child.getType() == type)
				return child;
		return null;
	}
	public static int getChildIndex(LAST last, Node node)
	{
		final List<Node> siblings = getSiblings(last, node);

		return siblings.indexOf(node);
	}

	public static List<Node> getInputs(LAST last, Node node, Direction direction)
	{
		return getNodes(last, node, direction, Edge.Type.Input);
	}
	public static List<Node> getOutputs(LAST last, Node node, Direction direction)
	{
		return getNodes(last, node, direction, Edge.Type.Output);
	}

	public static List<Node> getNodes(LAST last, Node node, Direction direction)
	{
		final Set<Edge> edges = getEdges(last, node, direction);
		return getNodesOfSet(last, edges, direction);
	}

	public static List<Node> getStructuralNodes(LAST last, Node node, Direction direction)
	{
		final Set<Edge> edges = getStructuralEdges(last, node, direction);
		return getNodesOfSet(last, edges, direction);
	}

	public static List<Node> getNodes(LAST last, Node node, Direction direction, Edge.Type edgeType)
	{
		final Set<Edge> edges = getEdges(last, node, direction, edgeType);
		return getNodesOfSet(last, edges, direction);
	}

	private static List<Node> getNodesOfSet(LAST last, Set<Edge> edges, Direction direction)
	{
		return edges.stream()
				.map(e -> direction == Direction.Backwards ? last.getEdgeSource(e) : last.getEdgeTarget(e))
				.sorted((a, b) -> (a.getId() > 0 && b.getId() > 0) ? Integer.compare(a.getId(), b.getId()) : Integer.compare(b.getId(), a.getId()))
				.collect(Collectors.toList());
	}

	public static Set<Edge> getEdges(LAST last, Node node, Direction direction)
	{
		return direction == Direction.Backwards ? last.incomingEdgesOf(node) : last.outgoingEdgesOf(node);
	}

	public static Set<Edge> getStructuralEdges(LAST last, Node node, Direction direction)
	{
		return direction == Direction.Backwards ? last.structuralIncomingEdgesOf(node) : last.structuralOutgoingEdgesOf(node);
	}

	public static Set<Edge> getEdges(LAST last, Node node, Direction direction, Edge.Type edgeType)
	{
		return getEdges(last, node, direction).stream()
				.filter(e -> e.getType() == edgeType)
				.collect(Collectors.toSet());
	}

	public static Node getResult(LAST last, Node node)
	{
		final List<Node> siblings = getSiblings(last, node);
		final List<Node> children = getChildren(last, node);

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
				return getResFromNode(last, node);
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
				throw new RuntimeException("Type not contemplated: " + node.getType());
		}
	}

	public static Node getResFromNode(LAST last, Node node)
	{
		Set<Node> next = ControlFlowTraverser.step(last, node, ControlFlowTraverser.Direction.Forwards);
		if (next.size() == 1)
			return next.iterator().next();
		throw new RuntimeException("The next element of a variable in the CFG is only its result");
	}

	public static Node getNodeFromRes(LAST last, Node resNode)
	{
		if (resNode.getType() != Node.Type.Result)
			return resNode;

		Set<Node> prev = ControlFlowTraverser.step(last, resNode, ControlFlowTraverser.Direction.Backwards);
		if (prev.size() == 1)
			return prev.iterator().next();
		throw new RuntimeException("The previous element of a result in the CFG is only its expression");
	}

	// PREVIOUS METHOD FOR DATA CONSTRUCTOR ACCESSES
	private static Node getVarResult(LAST last, Node node, List<Node> siblings)
	{

		final Node grandParentUseNode = getParent(last, getParent(last, node));
		if (grandParentUseNode.getType() == Node.Type.DataConstructorAccess)
		{
			final Node parentNode = getParent(last, node);
			if (getChildIndex(last, parentNode) == 0)
				return getSibling(last, grandParentUseNode, 1);
		}
		return siblings.get(siblings.size() - 1);
	}
	
	public static Node getModuleByName(LAST last, String name)
	{
		final List<Node> moduleNodes = getNodes(last, Node.Type.Module);
		
		for (Node moduleNode : moduleNodes)
		{
			if(moduleNode.getName().equals(name))
				return moduleNode;
		}
		return null;
	}
	
	public static boolean isPatternZone(LAST last, Node node)
	{
		Node ancestor = node;
		Node.Type ancestorType = ancestor.getType();

		while (ancestorType != Node.Type.Root)
		{
			if (ancestorType == Node.Type.Parameters || ancestorType == Node.Type.Selectable)
				return true;

			final Node ancestorParent = getParent(last, ancestor);
			final Node.Type ancestorParentType = ancestorParent.getType();
			final int ancestorChildIndex = getChildIndex(last, ancestor);
			if ((ancestorParentType == Node.Type.Generator || ancestorParentType == Node.Type.Equality) && ancestorChildIndex == 0)
				return true;

			ancestor = ancestorParent;
			ancestorType = ancestor.getType();
		}

		return false;
	}

	public static boolean isDefinedClass(LAST last, Node node)
	{
		if (node.getType() != Node.Type.Variable)
			return true;
		final List<Node> modules = getNodes(last, Node.Type.Module);
		final String nodeType =  node.getInfo().getInfo()[1].toString();
		for (Node module : modules)
			if (module.getName().equals(nodeType))
				return true;
		return false;
			
	}
}