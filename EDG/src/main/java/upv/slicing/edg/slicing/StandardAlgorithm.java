package upv.slicing.edg.slicing;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.graph.LAST.Direction;

import java.util.*;

public class StandardAlgorithm implements SlicingAlgorithm
{
	protected final EDG edg;

	public StandardAlgorithm(EDG edg)
	{
		this.edg = edg;
	}

	public Set<Node> slice(Node slicingCriterion)
	{
		final Set<Node> slice = new HashSet<>();
		if (slicingCriterion == null)
			return slice;

		slice.add(slicingCriterion);
		this.traverse(slicingCriterion, slice, Edge.Type.Output);
		this.traverse(slicingCriterion, slice, Edge.Type.Input);

		return slice;
	}

	protected void traverse(Node slicingCriterion, Set<Node> slice, Edge.Type... ignoreEdgeTypes)
	{
		this.traverse(slice, ignoreEdgeTypes);
	}

	private void traverse(Set<Node> slice, Edge.Type... ignoreEdgeTypes)
	{
		final Deque<Node> pendingNodes = new LinkedList<>(slice);
		final Set<Edge.Type> ignoreEdgeTypesSet = new HashSet<>(Arrays.asList(ignoreEdgeTypes));

		while (!pendingNodes.isEmpty())
		{
			final Node pendingNode = pendingNodes.removeFirst();
			final Set<Edge> nextEdges = edg.getEdges(pendingNode, sliceDirection);

			nextEdges.removeIf(e -> ignoreEdgeTypesSet.contains(e.getType()));
			nextEdges.removeIf(Edge::isControlFlowEdge);
			nextEdges.removeIf(e -> !e.isTraversable());
			for (Edge nextEdge : nextEdges)
			{
				final Node nextNode = sliceDirection == Direction.Backwards ?
						edg.getEdgeSource(nextEdge): edg.getEdgeTarget(nextEdge);
				if (!slice.contains(nextNode))
				{
					pendingNodes.addLast(nextNode);
					slice.add(nextNode);
				}
			}
		}
	}
}
