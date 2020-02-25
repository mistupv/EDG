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
import upv.slicing.edg.traverser.EDGTraverser;
import upv.slicing.edg.traverser.LASTTraverser;

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
		final List<Node> expressionNodes = EDGTraverser
				.getNodes(edg, node -> node.getInfo() != null && node.getInfo().isExpression());
		for (Node expression : expressionNodes)
			createThreeNodeStructures(expression);
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
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(nodeInfo.getArchive(), nodeInfo.getClassName(),
														   nodeInfo.getLine(), nodeInfo.getConstruction());

		// Result Node
		final Node result = new Node("result", fictitiousId--, Node.Type.Result, "", ldNodeInfo);

		final Node parent = EDGTraverser.getParent(last, node);

		edg.addNode(result);

		// Remove the Structural edges if the parent is an expression -> The hidden structural edges inside nodes remain in the graph
		if (!parent.getType().isFictitious() && parent.getInfo().isExpression())
		{
			if (!LASTTraverser.isPatternZone(last, node))
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
				final boolean isPatternZone = EDGTraverser.isPatternZone(last, node);
				if (!isPatternZone)
				{
					treatDataConstructorExpressions(node, result);
					break;
				}
			case Variable: // Variables scope of a call, don't add the result node to the slice
				// 3 levels (in case it is a casting)
				final Node grandParent = EDGTraverser.getParent(last, parent);
				final Node grandGrandParent = EDGTraverser.getParent(last, grandParent);
				if (parent.getType() == Node.Type.Scope ||
						grandParent.getType() == Node.Type.Scope ||
						grandGrandParent.getType() == Node.Type.Scope)
					break;
				// When there is a result of a callee, the value arc is only joined from the name node, and there is another one from the scope to the name
			default:
				treatCommonNodes(node, result);
				break;
		}

		// Modify CFG Arcs to add Results to the CFG
		final Set<Edge> outgoingCFGEdges = edg.outgoingEdgesOf(node);
		outgoingCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);

		for (Edge CFGEdge : outgoingCFGEdges)
		{
			final Node from = edg.getEdgeSource(CFGEdge);
			final Node to = edg.getEdgeTarget(CFGEdge);
			edg.removeEdge(CFGEdge);
			final Edge e1 = new Edge(Edge.Type.ControlFlow);
			final Edge e2 = new Edge(Edge.Type.ControlFlow);
			edg.addEdge(from, result, e1);
			edg.addEdge(result, to, e2);
		}

		// Value arcs between initial node and its result
		edg.addEdge(node, result, Edge.Type.Value);
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
			edg.addEdge(from, to, e);
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
			edg.addEdge(from, to, e);
		}
	}

	private void treatDataConstructorExpressions(Node dataConstructor, Node result) // TODO: REVIEW AFTER DELETING "EXPRESSION" NODES
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
		final List<Node> dataConstructorChildren = EDGTraverser.getChildren(edg, dataConstructor);
		final int dataConstructorChildrenCount = dataConstructorChildren.size();

		for (int childIndex = 0; childIndex < dataConstructorChildrenCount; childIndex++)
		{
			final Node dataConstructorChild = dataConstructorChildren.get(childIndex);
			final Node from = dataConstructorChild.getType() == Node.Type.Expression ?
					EDGTraverser.getChild(edg, dataConstructorChild, Node.Type.Result) : dataConstructorChild;
			final DataConstructorConstraint constraint = new DataConstructorConstraint(
					AccessConstraint.Operation.Remove, childIndex + "");
			edg.addEdge(from, result, new Edge(Edge.Type.Value, constraint));
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
			edg.addEdge(from, to, e);
		}
	}
}
