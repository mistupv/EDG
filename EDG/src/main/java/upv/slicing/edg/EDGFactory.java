package upv.slicing.edg;

import upv.slicing.edg.constraint.AccessConstraint;
import upv.slicing.edg.constraint.DataConstructorConstraint;
import upv.slicing.edg.constraint.EdgeConstraint;
import upv.slicing.edg.edge.ControlEdgeGenerator;
import upv.slicing.edg.edge.FlowEdgeGenerator;
import upv.slicing.edg.edge.InterproceduralEdgeGenerator;
import upv.slicing.edg.edge.SummaryEdgeGenerator;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

import java.util.List;
import java.util.Set;

public class EDGFactory {
	private final LAST last;
	private EDG edg;
	private int fictitiousId = -1;

	public EDGFactory(LAST last)
	{
		this.last = last;
	}

	// Required Configuration Parameters (Questions made to the users in order to decide the dependencies built on the graph) 
	private static boolean isOOLanguage = true;

	public EDG createEDG()
	{
		initializeEDG();
		transformExpressionNodes();
		if (isOOLanguage)
			LASTBuilder.addInheritanceInfomation(edg); // Only For OOPrograms With Inheritance
		generateDependencies();
		return edg;
	}
	
	private void initializeEDG()
	{
		edg = new EDG(last);
	}

	private void transformExpressionNodes()
	{
		final List<Node> expressionNodes = edg.getNodes(node -> node.getInfo() != null &&
				node.getInfo().isExpression());
		for (Node expression : expressionNodes)
			createThreeNodeStructures(expression);

		// TODO : When structural false edges are implemented, move this to LASTBuilder
		edg.vertexSet().stream()
				.filter(EDGFactory::isCallNode)
				.forEach(node -> {
					final Node parent = edg.getParent(node);
					edg.removeEDGEdge(parent, node, Edge.Type.Structural);
					edg.setRemovableEdge(parent, node, Edge.Type.Structural);
				});
	}

	public static boolean isCallNode(Node node)
	{
		switch(node.getType())
		{
			case Scope:
			case Name:
			case Arguments:
			case ArgumentIn:
				return true;
			default:
				return false;
		}
	}

	private void generateDependencies()
	{
		new ControlEdgeGenerator(edg).generate();

		if (isOOLanguage)
			new InterproceduralEdgeGenerator(edg).generate(); // Specially Generated for OOPrograms
		else
			new InterproceduralEdgeGenerator(edg).generateNoInheritance(); // TODO Implement without module analysis

		new FlowEdgeGenerator(edg).generate(); // TODO Testear y verlo paso a paso con el ControlFlowTraverser
		new SummaryEdgeGenerator(edg).generate();
//		new ExceptionEdgeGenerator(edg).generate();
	}

	private void createThreeNodeStructures(Node node)
	{
		// Conversion of 1 to 2 nodes & structural change
		final LDASTNodeInfo nodeInfo = node.getInfo();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(nodeInfo.getFile(), nodeInfo.getClassName(),
														   nodeInfo.getLine(), nodeInfo.getConstruction());

		// Result Node
		final Node result = new Node("result", fictitiousId--, Node.Type.Result, "", ldNodeInfo);

		final Node parent = last.getParent(node);

		edg.addVertex(result);
		edg.registerNodeResPair(node, result);

		// Remove the Structural edges if the parent is an expression -> The hidden structural edges inside nodes remain in the graph
		if (!parent.getType().isFictitious() && parent.getInfo().isExpression())
		{
			// THIS IS AN EXCEPTION IN ORDER TO INCLUDE ROUTINES IN THE SLICE
			// WHEN CFG IS FINISHED THEY WILL BE INCLUDED BY CONTROL
			if (parent.getType() != Node.Type.Routine && parent.getType() != Node.Type.Enclosed)
			{
				edg.removeEDGEdge(parent, node, Edge.Type.Structural);
				edg.setRemovableEdge(parent, node, Edge.Type.Structural);
			}
		}

		// Add the structural edge parent -> result to perform tree traversal (not considered for slicing)
		edg.addStructuralEdge(parent, result);

		// Modify Value Arcs
		switch (node.getType())
		{
			case DataConstructor:
				final boolean isPatternZone = edg.isPatternZone(node);
				if (!isPatternZone)
				{
					treatDataConstructorExpressions(node, result);
					break;
				}
			case Variable: // Variables scope of a call, don't add the result node to the slice
				// 3 levels (in case it is a casting)
				final Node grandParent = edg.getParent(parent);
				final Node grandGrandParent = edg.getParent(grandParent);
				if (parent.getType() == Node.Type.Scope ||
						grandParent.getType() == Node.Type.Scope ||
						grandGrandParent.getType() == Node.Type.Scope)
					break;
				// When there is a result of a callee, the value arc is only joined from the name node, and there is another one from the scope to the name
			default:
				treatCommonNodes(node, result);
				break;
		}

		// Value arcs between initial node and its result
		edg.addEdge(node, result, Edge.Type.Value);

		Set<Node.Type> specialResultTypes = Set.of(Node.Type.Routine, Node.Type.Clause);
		if (specialResultTypes.contains(node.getType()))
		{
			createThreeNodeStructuresSpecial(node, result);
			return;
		}

		// Modify CFG Arcs to add Results to the CFG
		final Set<Edge> outgoingCFGEdges = edg.outgoingEdgesOf(node);
		outgoingCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow && edge.getType() != Edge.Type.NonExecControlFlow);

		if (!outgoingCFGEdges.isEmpty())
			edg.addEdge(node, result, Edge.Type.ControlFlow);

		for (Edge CFGEdge : outgoingCFGEdges)
		{
			final Node to = edg.getEdgeTarget(CFGEdge);
			edg.removeEdge(CFGEdge);
			edg.addEdge(result, to, CFGEdge.getType());
		}


	}

	private void createThreeNodeStructuresSpecial(Node node, Node result)
	{

		switch(node.getType())
		{
			case Routine:
				final Set<Edge> incomingCFGEdges = edg.incomingEdgesOf(node);
				incomingCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);
				for (Edge CFGEdge : incomingCFGEdges)
				{
					final Node from = edg.getEdgeSource(CFGEdge);
					edg.removeEdge(CFGEdge);
					edg.addEdge(from,result, Edge.Type.ControlFlow);
				}
				break;
			case Clause:
				final Set<Edge> incomingClauseCFGEdges = edg.incomingEdgesOf(node);
				incomingClauseCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);
				for (Edge CFGEdge : incomingClauseCFGEdges)
				{
					final Node from = edg.getEdgeSource(CFGEdge);
					if (from.getType() != Node.Type.Routine)
					{
						edg.removeEdge(CFGEdge);
						edg.addEdge(from, result, Edge.Type.ControlFlow);
					}
				}

				final Set<Edge> outgoingClauseCFGEdges = edg.outgoingEdgesOf(node);
				outgoingClauseCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);
				for (Edge CFGEdge : outgoingClauseCFGEdges)
				{
					final Node finalToDestination = edg.getEdgeTarget(CFGEdge);
					Node to = finalToDestination;
					if (to.getType() == Node.Type.Result)
						to = edg.getNodeFromRes(to);

					if (to.getType() == Node.Type.Routine)
					{
						edg.removeEdge(CFGEdge);
						edg.addEdge(result, finalToDestination, Edge.Type.ControlFlow);
					}
				}
				break;
			default:
				throw new IllegalArgumentException("The node has not an allowed type: " +node.getType());
		}
	}

	private void treatCommonNodes(Node node, Node result)
	{
		final Set<Edge> outgoingEdges = edg.outgoingEdgesOf(node);
		outgoingEdges.removeIf(edge -> edge.getType() != Edge.Type.Value);
		
		for (Edge valueEdge : outgoingEdges)
		{
			final Node from = edg.getEdgeSource(valueEdge);
			final Node to = edg.getEdgeTarget(valueEdge);
			final EdgeConstraint edgeConstraint = valueEdge.getConstraint();
			edg.removeEdge(valueEdge);
			final Edge e = new Edge(Edge.Type.Value, edgeConstraint);
			edg.addEdge(result, to, e);
		}
		
		final Set<Edge> incomingEdges = edg.incomingEdgesOf(node);
		incomingEdges.removeIf(edge -> edge.getType() != Edge.Type.Value);

		for (Edge valueEdge : incomingEdges)
		{
			final Node from = edg.getEdgeSource(valueEdge);
			final Node to = edg.getEdgeTarget(valueEdge);
			final EdgeConstraint edgeConstraint = valueEdge.getConstraint();
			edg.removeEdge(valueEdge);
			final Edge e = new Edge(Edge.Type.Value, edgeConstraint);
			edg.addEdge(from, result, e);
		}
	}

	private void treatDataConstructorExpressions(Node dataConstructor, Node result)
	{
		deleteIncomingValueArcs(dataConstructor);
		modifyDataConstructorArcs(dataConstructor, result);
	}

	private void deleteIncomingValueArcs(Node dataConstructor)
	{
		for (Edge edge : edg.incomingEdgesOf(dataConstructor))
			if (edge.getType() == Edge.Type.Value)
				edg.removeEdge(edge);
	}

	private void modifyDataConstructorArcs(Node dataConstructor, Node result)
	{
		final List<Node> dataConstructorChildren = edg.getChildren(dataConstructor);
		dataConstructorChildren.removeIf(n -> n.getType() == Node.Type.Result);

		final int dataConstructorChildrenCount = dataConstructorChildren.size();

		for (int childIndex = 0; childIndex < dataConstructorChildrenCount; childIndex++)
		{
			final Node dataConstructorChild = edg.getResFromNode(dataConstructorChildren.get(childIndex));
			final DataConstructorConstraint constraint = new DataConstructorConstraint(
					AccessConstraint.Operation.Remove, childIndex + "");
			edg.addEdge(dataConstructorChild, result, new Edge(Edge.Type.Value, constraint));
		}
		
		final Set<Edge> outgoingEdges = edg.outgoingEdgesOf(dataConstructor);
		outgoingEdges.removeIf(edge -> edge.getType() != Edge.Type.Value);
		
		for (Edge valueEdge : outgoingEdges)
		{
			final Node from = edg.getEdgeSource(valueEdge);
			final Node to = edg.getEdgeTarget(valueEdge);
			final EdgeConstraint edgeConstraint = valueEdge.getConstraint();
			edg.removeEdge(valueEdge);
			final Edge e = new Edge(Edge.Type.Value, edgeConstraint);
			edg.addEdge(result, to, e);
		}
	}
}
