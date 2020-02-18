package edg.slicingAlgorithm;

import java.util.LinkedList;
import java.util.List;

import edg.constraint.Constraints;
import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.traverser.EdgeTraverser;
import edg.util.Work;
import edg.util.WorkList;

public class AdvancedAlgorithm implements SlicingAlgorithm
{
	private final EdgeTraverser edgeTraverser;

	public AdvancedAlgorithm(EDG edg, boolean resolveSummary, boolean constraintsActivated)
	{
		this.edgeTraverser = new EdgeTraverser(edg, resolveSummary, constraintsActivated);
	}

	public List<Node> slice(Node node)
	{
		final List<Node> slice = new LinkedList<Node>();
		if (node == null)
			return slice;

		final WorkList workList = new WorkList();
		workList.add(new Work(node, false, true));
		this.traverse(workList, EdgeInfo.Type.Output);
		this.traverse(workList, EdgeInfo.Type.Input);

		final List<Work> works = workList.toList();
		for (Work work : works)
			slice.add(work.getCurrentNode());
		return slice;
	}
	private void traverse(WorkList workList, EdgeInfo.Type ignoreEdgeType)
	{
		final List<Work> pendingWorks = workList.toList();

		while (!pendingWorks.isEmpty())
		{
			final Work pendingWork = pendingWorks.remove(0);
			final List<Work> newWorks = this.processWork(pendingWork, ignoreEdgeType);

			for (Work newWork : newWorks)
			{
				if (workList.contains(newWork))
					continue;

				workList.add(newWork);
				pendingWorks.add(newWork);
			}
		}
	}
	public List<Work> processWork(Work work, EdgeInfo.Type... ignoreEdgeTypes)
	{
		final List<Work> newWorks = new LinkedList<Work>();
		final Node initialNode = work.getInitialNode();
		final Node currentNode = work.getCurrentNode();
		final Constraints constraints = work.getConstraints();
		final boolean ignoreUp = work.getIgnoreUp();
		final boolean ignoreDown = work.getIgnoreDown();

		// Incoming edges
		final List<Edge> incomingEdges = currentNode.getIncomingEdges();

		processIncommings:
		for (Edge incomingEdge : incomingEdges)
		{
			final EdgeInfo.Type edgeType = incomingEdge.getData().getType();
			// Do not go up after going down in a composite data
			if (edgeType == EdgeInfo.Type.StructuralControl && ignoreUp)
				continue;
			// Do not traverse value edges after going up
			if (edgeType == EdgeInfo.Type.ValueDependence && ignoreDown)
				continue;
			// Ignore edges of the current phase
			for (EdgeInfo.Type ignoreEdgeType : ignoreEdgeTypes)
				if (ignoreEdgeType == edgeType)
					continue processIncommings;

			// All restrictions passed, add this node to the slice if it does not exist yet
			final Node nodeFrom = incomingEdge.getFrom();
			final boolean newIgnoreDown = edgeType == EdgeInfo.Type.StructuralControl || edgeType == EdgeInfo.Type.NormalControl;
			// Resolve the constraints, when cannot traverse it an empty list is returned
			final List<Constraints> newConstraintsStacks = this.edgeTraverser.traverseIncomingEdge(incomingEdge, constraints);

			for (Constraints newConstraintsStack : newConstraintsStacks)
				newWorks.add(new Work(initialNode, nodeFrom, newConstraintsStack, false, newIgnoreDown));
		}

		// Outgoing edges, go down in a composite data
		if (!ignoreDown)
		{
			final List<Edge> outgoingEdges = currentNode.getOutgoingEdges();

			processOutgoings:
			for (Edge outgoingEdge : outgoingEdges)
			{
				final EdgeInfo.Type edgeType = outgoingEdge.getData().getType();

				// Only consider composite data
				if (edgeType != EdgeInfo.Type.StructuralControl)
					continue;
				// Ignore edges of the current phase
				for (EdgeInfo.Type ignoreEdgeType : ignoreEdgeTypes)
					if (ignoreEdgeType == edgeType)
						continue processOutgoings;

				// All restrictions passed, add this node to the slice if it does not exist yet
				final Node nodeTo = outgoingEdge.getTo();
				// Resolve the constraints, when cannot traverse an empty list is returned
				final List<Constraints> newConstraintsStacks = this.edgeTraverser.traverseOutgoingEdge(outgoingEdge, constraints);

				for (Constraints newConstraintsStack : newConstraintsStacks)
					newWorks.add(new Work(initialNode, nodeTo, newConstraintsStack, true, false));
			}
		}

		return newWorks;
	}
}