package eknife.edg.slicingAlgorithm;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.traverser.GraphTraverser;

public class GraphPermutator
{
	private final Node root;
	private final int elements;
	private final boolean keepInfo;
	private final Filter nodeFilter;
	private final Filter subtreeFilter;
	private int eliminations = 0;

	private final List<Node> selectedNodes = new LinkedList<Node>();
	private final List<Node> removedNodes = new LinkedList<Node>();
	private final List<Node> ignoredNodes = new LinkedList<Node>();

	private final List<Node> remainingNodes = new LinkedList<Node>();

	public GraphPermutator(Node root, int elements, boolean keepInfo)
	{
		this(root, elements, keepInfo, null, null);
	}
	public GraphPermutator(Node root, int elements, boolean keepInfo, Filter nodeFilter)
	{
		this(root, elements, keepInfo, nodeFilter, null);
	}
	public GraphPermutator(Node root, int elements, boolean keepInfo, Filter nodeFilter, Filter subtreeFilter)
	{
		this.root = root;
		this.elements = elements;
		this.keepInfo = keepInfo;
		this.nodeFilter = nodeFilter;
		this.subtreeFilter = subtreeFilter;

		this.ignoreNodes();
	}
	private void ignoreNodes()
	{
		this.ignoreNodes(this.root);
	}
	private void ignoreNodes(Node node)
	{
		if (!this.subtreeFilter.accept(node))
			this.ignoredNodes.add(node);
		else
			for (Node child : GraphTraverser.getChildren(node, EdgeInfo.Type.Control))
				this.ignoreNodes(child);
	}

	public boolean next()
	{
		this.nextPermutation();

		return !this.selectedNodes.isEmpty();
	}
	private void nextPermutation()
	{
		for (int elementIndex = Math.max(this.selectedNodes.size() - 1, 0); 0 <= elementIndex && elementIndex < this.elements; elementIndex++)
		{
			final boolean addPrevious = this.selectedNodes.size() > elementIndex || this.selectedNodes.isEmpty();
			final Node previousNode = this.selectedNodes.size() > elementIndex ? this.selectedNodes.remove(elementIndex) : this.selectedNodes.isEmpty() ? this.root : this.selectedNodes.get(this.selectedNodes.size() - 1);
			final Node nextNode = this.nextNode(previousNode);
			final Node nodeToAdd = addPrevious ? previousNode : null;

			if (this.keepInfo)
				this.updateRemaining(nodeToAdd, nextNode);
			if (nextNode != null)
				this.selectedNodes.add(nextNode);
			else
				elementIndex -= 2;
		}
	}

	private Node nextNode(Node node)
	{
		do
			node = this.nextNode0(node);
		while (node != null && this.nodeFilter != null && !this.nodeFilter.accept(node));

		return node;
	}
	private Node nextNode0(Node node)
	{
		Node current = node;
		int childNum = -1;
		int childCount = GraphTraverser.getChildCount(current);

		while (true)
		{
			for (int childIndex = childNum + 1; childIndex < childCount; childIndex++)
			{
				final Node child = GraphTraverser.getChild(current, childIndex);
				if (this.isValid(child))
					return child;
			}

			if (current == this.root)
				break;

			childNum = GraphTraverser.getChildNum(current);
			current = GraphTraverser.getParent(current,EdgeInfo.Type.Control);
			childCount = GraphTraverser.getChildCount(current);
		}

		return null;
	}
	private boolean isValid(Node node)
	{
		final List<Node> ignoreNodes = new LinkedList<Node>();

		ignoreNodes.addAll(this.selectedNodes);
		ignoreNodes.addAll(this.removedNodes);
		ignoreNodes.addAll(this.ignoredNodes);

		for (Node ignoreNode : ignoreNodes)
		{
			// Is ancestor
			if (this.selectedNodes.contains(ignoreNode))
				if (this.isDescendant(ignoreNode, node))
					return false;

			// Is descendant
			if (this.isDescendant(node, ignoreNode))
				return false;
		}

		return true;
	}
	private boolean isDescendant(Node node, Node ancestor)
	{
		Node current = node;

		do
		{
			if (current == ancestor)
				return true;
			current = GraphTraverser.getParent(current,EdgeInfo.Type.Control);
		}
		while (current != null);

		return false;
	}

	public void remove()
	{
		this.removedNodes.addAll(this.selectedNodes);
		this.eliminations++;
		this.reset();
	}
	private void reset()
	{
		this.selectedNodes.clear();
		this.remainingNodes.clear();
	}

	private void updateRemaining(Node nodeToAdd, Node nodeToRemove)
	{
		if (nodeToAdd != null)
			this.updateRemainingViaAdding(nodeToAdd);
		if (nodeToRemove != null)
			this.updateRemainingViaRemoving(nodeToRemove);
	}
	private void updateRemainingViaAdding(Node node)
	{
		if (this.removedNodes.contains(node))
			return;

		this.remainingNodes.add(node);
		for (Node child : GraphTraverser.getChildren(node, EdgeInfo.Type.Control))
			this.updateRemainingViaAdding(child);
	}
	private void updateRemainingViaRemoving(Node node)
	{
		if (this.removedNodes.contains(node))
			return;

		this.remainingNodes.remove(node);
		for (Node child : GraphTraverser.getChildren(node, EdgeInfo.Type.Control))
			this.updateRemainingViaRemoving(child);
	}

	public List<Node> getSelectedNodes()
	{
		if (this.selectedNodes.isEmpty())
			return null;

		final List<Node> nodes = new LinkedList<Node>();

		nodes.addAll(this.selectedNodes);

		return nodes;
	}
	public List<Node> getRemainingNodes()
	{
		if (this.selectedNodes.isEmpty())
			return null;
		if (this.keepInfo)
			return new LinkedList<Node>(this.remainingNodes);
		return this.getRemainingNodes(this.root);
	}
	private List<Node> getRemainingNodes(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> ignoreNodes = new LinkedList<Node>();
		boolean ignore = false;

		ignoreNodes.addAll(this.selectedNodes);
		ignoreNodes.addAll(this.removedNodes);

		for (Node ignoreNode : ignoreNodes)
			if (this.isDescendant(node, ignoreNode))
				ignore = true;

		if (!ignore)
			nodes.add(node);

		for (Node child : GraphTraverser.getChildren(node, EdgeInfo.Type.Control))
			nodes.addAll(this.getRemainingNodes(child));

		return nodes;
	}
	public List<Node> getRemovedNodes()
	{
		return this.getRemovedNodes(this.root);
	}
	private List<Node> getRemovedNodes(Node node)
	{
		final List<Node> removedNodes = new LinkedList<Node>();

		for (Node removedNode : this.removedNodes)
			if (this.isDescendant(node, removedNode))
			{
				removedNodes.add(node);
				break;
			}

		final List<Node> children = GraphTraverser.getChildren(node, EdgeInfo.Type.Control);
		for (Node child : children)
			removedNodes.addAll(this.getRemovedNodes(child));

		return removedNodes;
	}
	public int getEliminations()
	{
		return this.eliminations;
	}

	public interface Filter
	{
		boolean accept(Node node);
	}
}