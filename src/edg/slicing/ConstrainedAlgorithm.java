package edg.slicing;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import edg.constraint.EdgeConstraint;
import edg.constraint.ExceptionConstraint;
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
		if(phase == Phase.SummaryGeneration)
			edges.removeIf(edge -> edge.getData().getType() == EdgeInfo.Type.Exception);
		
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
//if (initialNode.getData().getId() == 7)
//System.out.print(initialNode.getData().getId()+": ");
		
final Node nodeTo = currentEdge.getTo();		
//if(nodeTo.getData().getId() == 212 && nodeFrom.getData().getId() == 228)
//	System.out.print("STOP");
		// NECESSARY TO CONTROL THE OUTPUT EDGES WITH LET_THROUGH_CONSTRAINTS
		final EdgeInfo.Type edgeType = currentEdge.getData().getType(); 
		if (phase == Phase.Input && edgeType == EdgeInfo.Type.Output)
			return newWorks;
		if (phase == Phase.Output && edgeType == EdgeInfo.Type.Input)
			return newWorks;
		if (phase == Phase.SummaryGeneration && (edgeType == EdgeInfo.Type.Input || edgeType == EdgeInfo.Type.Output))
			return newWorks;
		
// TODO Borrame
final List<Phase> phases = Arrays.asList(Phase.Input);
final List<Integer> nodesIds = Arrays.asList();
//if(initialNode.getData().getId() == 200 && nodeTo.getData().getId() == 209 && currentEdge.getData().getType() != EdgeInfo.Type.Structural)
//{
//if (initialNode.getData().getId() == 7)
//System.out.print(nodeTo.getData().getId() + " -> " + nodeFrom.getData().getId()+" - EdgeType: " + currentEdge.getData().getType().name());
//	System.out.print("");
//}
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
			
//if(initialNode.getData().getId() == 7) // && currentEdge.getData().getType() != EdgeInfo.Type.Structural)
//{
//	boolean atravesado = !newConstraintsList.isEmpty();
//	if (atravesado)
//		System.out.println(" Yes");
//	else
//		System.out.println(" No");
//}

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