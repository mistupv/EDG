package upv.slicing.edg.slicing;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;

import java.util.*;

public class StandardAlgorithm implements SlicingAlgorithm
{
	protected final EDG edg;

	public StandardAlgorithm(EDG edg)
	{
		this.edg = edg;
	}

	public Set<Node> slice(Node node)
	{
		final Set<Node> slice = new HashSet<>();
		if (node == null)
			return slice;

		slice.add(node);
		this.traverse(slice, Edge.Type.Output);
		this.traverse(slice, Edge.Type.Input);

		return slice;
	}

	private void traverse(Set<Node> slice, Edge.Type... ignoreEdgeTypes)
	{
		final Deque<Node> pendingNodes = new LinkedList<>(slice);
		final Set<Edge.Type> ignoreEdgeTypesSet = new HashSet<>(Arrays.asList(ignoreEdgeTypes));
		ignoreEdgeTypesSet.add(Edge.Type.ControlFlow);

		while (!pendingNodes.isEmpty())
		{
			final Node pendingNode = pendingNodes.removeFirst();
			final Set<Edge> incomingEdges = edg.incomingEdgesOf(pendingNode);

			incomingEdges.removeIf(e -> ignoreEdgeTypesSet.contains(e.getType()));
			incomingEdges.removeIf(e -> !e.isTraversable());
			for (Edge incomingEdge : incomingEdges)
			{
				final Node nodeFrom = edg.getEdgeSource(incomingEdge);
				if (!slice.contains(nodeFrom))
				{
					pendingNodes.addLast(nodeFrom);
					slice.add(nodeFrom);
				}
			}
		}
	}
}