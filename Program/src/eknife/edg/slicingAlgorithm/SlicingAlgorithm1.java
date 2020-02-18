package eknife.edg.slicingAlgorithm;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.Edge;
import eknife.edg.EdgeInfo;
import eknife.edg.Node;

public class SlicingAlgorithm1 implements SlicingAlgorithm
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
	private void traverse(List<Node> slice, EdgeInfo.Type ignoreEdgeType)
	{
		final List<Node> pendingNodes = new LinkedList<Node>(slice);

		while (!pendingNodes.isEmpty())
		{
			final Node pendingNode = pendingNodes.remove(0);
			final List<Edge> incomingEdges = pendingNode.getIncomingEdges();

			for (Edge incomingEdge : incomingEdges)
			{
				final Node nodeFrom = incomingEdge.getFrom();
				if (slice.contains(nodeFrom))
					continue;
				if (incomingEdge.getData().getType() == ignoreEdgeType)
					continue;

				pendingNodes.add(nodeFrom);
				slice.add(nodeFrom);
			}
		}
	}
}