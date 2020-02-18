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
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.graph.VariableInfo.Context;
import edg.traverser.EDGTraverser;
import edg.work.EdgeWork;
import edg.work.NodeWork;
import edg.work.Work;
import edg.work.WorkList;

public class SDGAlgorithm implements SlicingAlgorithm
{
	final List<Node> slice = new LinkedList<Node>();
	
	public List<Node> slice(Node node)
	{
		if (node == null)
			return slice;

		final WorkList workList = new WorkList();
		
		final List<Node> initialNodes = EDGTraverser.getNodesSameSDGId(node);
		slice.addAll(initialNodes);
		
		List<Work> initialWorks = new LinkedList<Work>();
		for (Node initialNode : initialNodes)
			initialWorks.addAll(getRelatedSDGUseNodes(initialNode));
		workList.pendAll(initialWorks);
		
		workList.pend(new NodeWork(null, node, new Constraints()));
		this.traverse(Phase.Input, workList);
		workList.repend();
		this.traverse(Phase.Output, workList);

		final Set<Node> nodes = workList.getDoneNodes();
		for (Node doneNode : nodes)
			if (!slice.contains(doneNode))
				slice.add(doneNode);

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
		
//final Node nodeTo = currentEdge.getTo();		
//if(nodeTo.getData().getId() == 212 && nodeFrom.getData().getId() == 228)
//	System.out.print("STOP");
		// NECESSARY TO CONTROL THE OUTPUT EDGES WITH LET_THROUGH_CONSTRAINTS
		final EdgeInfo.Type edgeType = currentEdge.getData().getType(); 
		if (phase == Phase.Input && edgeType == EdgeInfo.Type.Output)
			return newWorks;
		if (phase == Phase.Output && edgeType == EdgeInfo.Type.Input)
			return newWorks;
		
// TODO Borrame
//final List<Phase> phases = Arrays.asList(Phase.Input);
//final List<Integer> nodesIds = Arrays.asList();
//if(currentEdge.getData().getType() != EdgeInfo.Type.Structural)
//{
////if (initialNode.getData().getId() == 7)
//final Constraints constraints0 = work.getConstraints();
//final String lastConstraint0 = constraints0.isEdgeConstraintsEmpty() ? "Empty" : constraints0.peekEdgeConstraint().toString();
//System.out.print(nodeTo.getData().getId() + " -> " + nodeFrom.getData().getId()+" - EdgeType: " + currentEdge.getData().getType().name()+ " Last Constraint "+lastConstraint0);
//System.out.print("");
//}
//final int currentId = nodeTo.getData().getId();
//final int nextId = nodeFrom.getData().getId();
//if (phases.contains(phase) && (nodesIds.contains(currentId) || nodesIds.contains(nextId)))
//System.out.print("");

		try
		{
			final Constraints constraints = work.getConstraints();
			final Constraints constraintsClone = (Constraints) constraints.clone();
			final EdgeConstraint constraint = currentEdge.getData().getConstraint();
			final EdgeConstraint lastConstraint = constraintsClone.isEdgeConstraintsEmpty() ? null : constraintsClone.peekEdgeConstraint();
			final List<Constraints> newConstraintsList = constraint.resolve(phase, currentEdge, constraintsClone, lastConstraint, 0);

			for (Constraints newConstraints : newConstraintsList)
			{
				if (slice.contains(nodeFrom))
				{
					final List<Node> newNodes = EDGTraverser.getNodesSameSDGId(nodeFrom);
					for (Node newNode : newNodes)
					{
						if (!slice.contains(newNode))
						{
							slice.add(newNode);
							newWorks.addAll(getRelatedSDGUseNodes(newNode));
						}
					}
				}
				newWorks.add(new NodeWork(initialNode, nodeFrom, newConstraints));
			}
			
//if(currentEdge.getData().getType() != EdgeInfo.Type.Structural)
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
	private List<Work> getRelatedSDGUseNodes(Node newNode) {
		
		final List<Work> newWorks = new LinkedList<Work>();
		final NodeInfo.Type newNodeType = newNode.getData().getType();
		switch(newNodeType)
		{
			case Variable:
				final VariableInfo vi = (VariableInfo) newNode.getData();
				if (vi.getContext() == Context.Use || vi.getContext() == Context.Def_Use)
				{
					final Node newNodeResult = EDGTraverser.getResult(newNode);
					newWorks.add(new NodeWork(null, newNodeResult, new Constraints()));
				}
				break;
			case DataConstructorAccess:
			case FieldAccess:
				final Node newNodeResult = EDGTraverser.getResult(newNode);
				newWorks.add(new NodeWork(null, newNodeResult, new Constraints()));
				break;
			case Call:
				final Node newNodeCallResult = EDGTraverser.getResult(newNode);
				if (!slice.contains(newNodeCallResult))
					slice.add(newNodeCallResult);
				newWorks.add(new NodeWork(null, newNodeCallResult, new Constraints()));
				break;
			default:
				break;
		}
		return newWorks;
	}
}


