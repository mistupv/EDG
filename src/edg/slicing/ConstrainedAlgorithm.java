package edg.slicing;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import edg.constraint.EdgeConstraint;
import edg.constraint.NodeConstraint;
import edg.constraint.Constraints;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.work.EdgeWork;
import edg.work.NodeWork;
import edg.work.Work;
import edg.work.WorkList;

public class ConstrainedAlgorithm implements SlicingAlgorithm
{
	public List<Node> slice(Node node)
	{
		final List<Node> slice = new LinkedList<Node>();
		if (node == null)
			return slice;

		final WorkList workList = new WorkList();
		workList.pend(new NodeWork(null, node, new Constraints()));
		this.traverse(Phase.Input, workList);
		workList.repend();
		this.traverse(Phase.Output, workList);

		final Set<Node> nodes = workList.getDoneNodes();
		slice.addAll(nodes);

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
	private List<Work> processWork(Phase phase, NodeWork work)
	{
		final List<Work> newWorks = new LinkedList<Work>();
		final Node initialNode = work.getInitialNode();
		final Node currentNode = work.getCurrentNode();
		final Constraints constraints = work.getConstraints();
		final Set<NodeConstraint> nodeConstraints = constraints.getNodeConstraints();
		final List<Edge> edges = currentNode.getIncomingEdges();

		edges.removeIf(edge -> edge.getData().getType() == EdgeInfo.Type.ControlFlow);
		for (NodeConstraint nodeConstraint : nodeConstraints)
			nodeConstraint.resolve(phase, edges);

		final Constraints constraintsClone = (Constraints) constraints.clone();
		constraintsClone.clearNodeConstraints();
		for (Edge edge : edges)
			newWorks.add(new EdgeWork(initialNode, edge, constraintsClone));

		return newWorks;
	}
	private List<Work> processWork(Phase phase, EdgeWork work)
	{
		final List<Work> newWorks = new LinkedList<Work>();
		final Node initialNode = work.getInitialNode();
		final Edge currentEdge = work.getCurrentEdge();
		final Node nodeFrom = currentEdge.getFrom();

// TODO Borrame
final List<Phase> phases = Arrays.asList(Phase.Input);
final List<Integer> nodesIds = Arrays.asList();
final Node nodeTo = currentEdge.getTo();
final int currentId = nodeTo.getData().getId();
final int nextId = nodeFrom.getData().getId();
if (phases.contains(phase) && (nodesIds.contains(currentId) || nodesIds.contains(nextId)))
System.out.print("");

		try
		{
			final Constraints constraints = work.getConstraints();
			final Constraints constraintsClone = (Constraints) constraints.clone();
			final EdgeConstraint constraint = currentEdge.getData().getConstraint();
			final EdgeConstraint lastConstraint = constraintsClone.isEdgeConstraintsEmpty() ? null : constraintsClone.peekEdgeConstraint();
			final List<Constraints> newConstraintsList = constraint.resolve(phase, currentEdge, constraintsClone, lastConstraint, 0);

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