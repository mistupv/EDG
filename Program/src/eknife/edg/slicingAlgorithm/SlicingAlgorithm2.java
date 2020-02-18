package eknife.edg.slicingAlgorithm;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.EDG;
import eknife.edg.Edge;
import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.constraint.AccessConstraint;
import eknife.edg.constraint.Constraints;
import eknife.edg.constraint.ExceptionArgumentConstraint;
import eknife.edg.constraint.ExceptionConstraint;
import eknife.edg.traverser.EdgeTraverser;
import eknife.edg.util.Work;
import eknife.edg.util.WorkList;

public class SlicingAlgorithm2 implements SlicingAlgorithm
{
	private final EDG graph;
	private final EdgeTraverser edgeTraverser;

	public SlicingAlgorithm2(EDG graph, boolean resolveSummary)
	{
		this.graph = graph;
		this.edgeTraverser = new EdgeTraverser(this.graph, resolveSummary);
	}

	public List<Node> slice(Node node)
	{
		final List<Node> slice = new LinkedList<Node>();
		if (node == null)
			return slice;

		final WorkList workList = new WorkList();
		final Work initialWork = this.getInitialWork(node);
		workList.add(initialWork);
		this.traverse(workList, EdgeInfo.Type.Output, EdgeInfo.Type.Exception);
		this.traverse(workList, EdgeInfo.Type.Input);

		final List<Work> works = workList.toList();
		for (Work work : works)
			slice.add(work.getCurrentNode());
		return slice;
	}
	private void traverse(WorkList workList, EdgeInfo.Type... ignoreEdgeTypes)
	{
		final List<Work> pendingWorks = workList.toList();

		while (!pendingWorks.isEmpty())
		{
			final Work pendingWork = pendingWorks.remove(0);
			final List<Work> newWorks = this.processWork(pendingWork, ignoreEdgeTypes);

			for (Work newWork : newWorks)
			{
				if (workList.contains(newWork))
					continue;
				workList.add(newWork);
				pendingWorks.add(newWork);
			}
		}
	}
private int[] hechos = new int[10200];
	public List<Work> processWork(Work work, EdgeInfo.Type... ignoreEdgeTypes)
	{
		final List<Work> newWorks = new LinkedList<Work>();
		final Node initialNode = work.getInitialNode();
		final Node currentNode = work.getCurrentNode();
		final Constraints constraints = work.getConstraints();
		final boolean ignoreUp = work.getIgnoreUp();
		final boolean ignoreDown = work.getIgnoreDown();

// TODO Delete
final int id = currentNode.getData().getId();
if (this.hechos[id] > 100000)
{
System.out.println();
for (Edge edge : currentNode.getOutgoingEdges())
System.out.println(edge.getTo().getData().getId());
System.out.println(id);
}
else
this.hechos[id]++;

int[] ids1 = { };//5397 }; // 181
int[] ids2 = { };//319, 407 }; // 299
int[] ids3 = { };//449, 1981, 2479, 2632, 2727, 2916, 3021 }; // 301
int[] ids4 = { };//918 }; // 900
int[] ids5 = { };//999 }; // 983
int[] ids6 = { };//2108 }; // 1089
int[] ids7 = { };//2917 }; // 2915
int[] ids8 = { };//4762, 5042 }; // 3105
int[] ids9 = { };//5317, 5905 }; // 3106
int[] ids10 = { };//4419 }; // 4417
int[] ids11 = { };//4870 }; // 4868
int[] ids12 = { };//4934 }; // 4932
int[] ids13 = { };//5039 }; // 5037
int[] ids14 = { };//5145 }; // 5143
int[] ids15 = { };//5148 }; // 5146
int[] ids16 = { };//5235 }; // 5233
int[] ids17 = { };//5236 }; // 5274
int[] ids18 = { };//5425 }; // 5408
int[] ids19 = { };//5814 }; // 5809
int[] ids20 = { };//6074 }; // 5818
int[] ids21 = { };//6145 }; // 6128
int[] ids22 = { };//6329 }; // 6313
int[] ids23 = { };//6361 }; // 6347
int[] ids24 = { };//6472 }; // 6470
int[] ids25 = { };//6757 }; // 6747
int[] ids26 = { };//7086 }; // 7076
int[] ids27 = { };//7412, 7453, 7473, 7494 }; // 7381
int[] ids28 = { };//7544, 7559, 7578, 7615 }; // 7511
int[] ids29 = { };//7712 }; // 7700
int[] ids30 = { };//7790 }; // 7777
int[] ids31 = { };//7903 }; // 7889
int[] ids32 = { };//7988 }; // 7979
int[] ids33 = { };//7989 }; // 7981
int[] ids34 = { };//9964 }; // 8035
int[] ids35 = { };//8140 }; // 8125
int[] ids36 = { };//8258 }; // 8155
int[] ids37 = { };//8337, 8806 }; // 8276
int[] ids38 = { };//8363 }; // 8352
int[] ids39 = { };//8631, 8651 }; // 8619
int[] ids40 = { };//8701, 8679 }; // 8665
int[] ids41 = { };//8724 }; // 8713
int[] ids42 = { };//8727 }; // 8725
int[] ids43 = { };//8910, 8927 }; // 8897
int[] ids44 = { };//8949, 8966 }; // 8936

int[][] idsAll = {	ids1, ids2, ids3, ids4, ids5, ids6, ids7, ids8, ids9, ids10,
					ids11, ids12, ids13, ids14, ids15, ids16, ids17, ids18, ids19, ids20,
					ids21, ids22, ids23, ids24, ids25, ids26, ids27, ids28, ids29, ids30,
					ids31, ids32, ids33, ids34, ids35, ids36, ids37, ids38, ids39, ids40,
					ids41, ids42, ids43, ids44 };
List<Integer> ids = new LinkedList<Integer>();
for (int[] idsEach : idsAll)
for (int id0 : idsEach)
ids.add(id0);
if (ids.contains(id))
return newWorks;


		// Incoming edges
		final List<Edge> incomingEdges = currentNode.getIncomingEdges();

		processIncommings:
		for (Edge incomingEdge : incomingEdges)
		{
final Node nodeFrom0 = incomingEdge.getFrom();
final int id0 = nodeFrom0.getData().getId();
if (id == 33 && id0 == 27)
System.out.println("Aqui");

			final EdgeInfo.Type edgeType = incomingEdge.getData().getType();
			// Do not go up after going down in a composite data
			if (edgeType == EdgeInfo.Type.StructuralControl && ignoreUp)
				continue;
			// Do not traverse value edges after going up
			if (edgeType == EdgeInfo.Type.ValueDependence && ignoreDown)
				continue;
			if (edgeType == EdgeInfo.Type.ExceptionGetAll)
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
	
	private Work getInitialWork(Node node)
	{
		if (node.getData().getType() == NodeInfo.Type.ExceptionReturn)
		{
			final Constraints stack = new Constraints();
			final ExceptionArgumentConstraint argument = new ExceptionArgumentConstraint(AccessConstraint.Operation.Add, "*");
			final ExceptionConstraint exception = new ExceptionConstraint(AccessConstraint.Operation.Add, "*");
			stack.push(argument);
			stack.push(exception);
			return new Work(node, stack, false, true);
		}
		return new Work(node, false, true);
	}
}