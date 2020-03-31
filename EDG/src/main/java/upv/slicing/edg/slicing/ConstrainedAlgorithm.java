package upv.slicing.edg.slicing;

import upv.slicing.edg.constraint.Constraints;
import upv.slicing.edg.constraint.EdgeConstraint;
import upv.slicing.edg.constraint.NodeConstraint;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.work.EdgeWork;
import upv.slicing.edg.work.NodeWork;
import upv.slicing.edg.work.Work;
import upv.slicing.edg.work.WorkList;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class ConstrainedAlgorithm implements SlicingAlgorithm
{
	protected final EDG edg;

	public ConstrainedAlgorithm(EDG edg)
	{
		this.edg = edg;
	}

	public Set<Node> slice(Node node)
	{
		final Set<Node> slice = new HashSet<>();
		if (node == null)
			return slice;

		final WorkList workList = new WorkList();
		workList.pend(new NodeWork(node, node, new Constraints()));
		this.traverse(Phase.Input, workList);
		workList.repend();
		this.traverse(Phase.Output, workList);

		slice.addAll(workList.getDoneNodes());

		return slice;
	}

	private void traverse(Phase phase, WorkList workList)
	{
		while (workList.hasMore())
		{
			final Work pendingWork = workList.next();
			final List<Work> newWorks = this.processWork(phase, pendingWork);

			workList.done(pendingWork);
			workList.pendAll(newWorks);
		}
	}

	public List<Work> processWork(Phase phase, Work work)
	{
		if (work instanceof NodeWork)
			return this.processWork(phase, (NodeWork) work);
		if (work instanceof EdgeWork)
			return this.processWork(phase, (EdgeWork) work);
		throw new RuntimeException("Work type not contemplated");
	}

	protected List<Work> processWork(Phase phase, NodeWork work)
	{
		final List<Work> newWorks = new LinkedList<>();
		final Node initialNode = work.getInitialNode();
		final Node currentNode = work.getCurrentNode();
		final Constraints constraints = work.getConstraints();
		final Set<NodeConstraint> nodeConstraints = constraints.getNodeConstraints();

		final Set<Edge> edges = edg.incomingEdgesOf(currentNode);
		edges.removeIf(edge -> edge.getType() == Edge.Type.ControlFlow ||
								edge.getType() == Edge.Type.NonExecControlFlow);
		if(phase == Phase.SummaryGeneration)
			edges.removeIf(edge -> edge.getType() == Edge.Type.Exception);
		
		for (NodeConstraint nodeConstraint : nodeConstraints)
			nodeConstraint.resolve(phase, edges);

		final Constraints constraintsClone = (Constraints) constraints.clone();
		constraintsClone.clearNodeConstraints();
		for (Edge edge : edges)
			newWorks.add(new EdgeWork(edg, initialNode, edge, constraintsClone));

		return newWorks;
	}

	private List<Work> processWork(Phase phase, EdgeWork work)
	{
		final List<Work> newWorks = new LinkedList<>();
		final Node initialNode = work.getInitialNode();
		final Edge currentEdge = work.getCurrentEdge();
		final Node nodeFrom = edg.getEdgeSource(currentEdge);

		// NECESSARY TO CONTROL THE OUTPUT EDGES WITH LET_THROUGH_CONSTRAINTS
		final Edge.Type edgeType = currentEdge.getType();
		if (phase == Phase.Input && edgeType == Edge.Type.Output)
			return newWorks;
		if (phase == Phase.Output && edgeType == Edge.Type.Input)
			return newWorks;
		if (phase == Phase.SummaryGeneration && (edgeType == Edge.Type.Input || edgeType == Edge.Type.Output))
			return newWorks;
		// Do not traverse non-traversable edges
		if (!currentEdge.isTraversable())
			return newWorks;

		try
		{
			final Constraints constraints = work.getConstraints();
			final Constraints constraintsClone = (Constraints) constraints.clone();
			final EdgeConstraint constraint = currentEdge.getConstraint();
			final EdgeConstraint lastConstraint = constraintsClone.isEdgeConstraintsEmpty() ? null : constraintsClone.peekEdgeConstraint();
			final List<Constraints> newConstraintsList = constraint.resolve(phase, edg, currentEdge, constraintsClone, lastConstraint, 0);

			for (Constraints newConstraints : newConstraintsList)
				newWorks.add(new NodeWork(initialNode, nodeFrom, newConstraints));

			return newWorks;
		}
		catch (StackOverflowError e)
		{
			if (!phase.isInstanceof(Phase.Slicing))
				throw new RuntimeException("Constraint situation not contemplated");
			newWorks.add(new NodeWork(initialNode, nodeFrom, new Constraints()));
		}

		return newWorks;
	}
}
