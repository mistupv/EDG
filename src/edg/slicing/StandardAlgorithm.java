package edg.slicing;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;

public class StandardAlgorithm implements SlicingAlgorithm
{
	public List<Node> slice(Node node)
	{
		final List<Node> slice = new LinkedList<Node>();
		if (node == null)
			return slice;

		slice.add(node);
		this.traverse(slice, EdgeInfo.Type.Output);
		this.traverse(slice, EdgeInfo.Type.Input);

		return slice;
	}
	private void traverse(List<Node> slice, EdgeInfo.Type... ignoreEdgeTypes)
	{
		final List<Node> pendingNodes = new LinkedList<Node>(slice);
		final List<EdgeInfo.Type> listIgnoreEdgeTypes = Arrays.asList(ignoreEdgeTypes);

		while (!pendingNodes.isEmpty())
		{
			final Node pendingNode = pendingNodes.remove(0);
			final List<Edge> incomingEdges = pendingNode.getIncomingEdges();

			incomingEdges.removeIf(edge -> edge.getData().getType() == EdgeInfo.Type.ControlFlow);
			incomingEdges.removeIf(edge -> listIgnoreEdgeTypes.contains(edge.getData().getType()));
			for (Edge incomingEdge : incomingEdges)
			{
				final Node nodeFrom = incomingEdge.getFrom();
				if (slice.contains(nodeFrom))
					continue;

				pendingNodes.add(nodeFrom);
				slice.add(nodeFrom);
			}
		}
	}
}