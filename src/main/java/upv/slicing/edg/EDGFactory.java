package upv.slicing.edg;

import upv.slicing.edg.constraint.AccessConstraint;
import upv.slicing.edg.constraint.DataConstructorConstraint;
import upv.slicing.edg.constraint.EdgeConstraint;
import upv.slicing.edg.edge.ControlEdgeGenerator;
import upv.slicing.edg.edge.FlowEdgeGenerator;
import upv.slicing.edg.edge.InterproceduralEdgeGenerator;
import upv.slicing.edg.edge.SummaryEdgeGenerator;
import edg.graph.*;
import upv.slicing.edg.traverser.EDGTraverser;
import upv.slicing.edg.traverser.LASTTraverser;
import upv.slicing.edg.graph.*;

import java.util.List;

public class EDGFactory {
	private static EDG edg;
	private static int fictitiousId = -1;

	// Required Configuration Parameters (Questions made to the users in order to decide the dependencies built on the graph) 
	private static boolean isOOLanguage = true;

	public static EDG createEDG(LAST last)
	{
		initializeEDG(last);
		transformExpressionNodes();
		//return edg;
		if (isOOLanguage)
			LASTBuilder.addInheritanceInfomation(edg); // Only For OOPrograms With Inheritance
		generateDependencies();
		return edg;
	}
	
	private static void initializeEDG(LAST last)
	{
		edg = new EDG(last);
	}
	private static void transformExpressionNodes()
	{
		final List<Node> expressionNodes = EDGTraverser
				.getNodes(edg, node -> node.getData().getInfo() != null && node.getData().getInfo().isExpression());
		for (Node expression : expressionNodes)
			createThreeNodeStructures(expression);
	}
	private static void generateDependencies()
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
	
	private static void createThreeNodeStructures(Node node)
	{
		// Conversion of 1 to 3 nodes & structural change
		final LDASTNodeInfo nodeInfo = node.getData().getInfo();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(nodeInfo.getArchive(), nodeInfo.getClassName(),
														   nodeInfo.getLine(), nodeInfo.getConstruction());

		// Expr Node
//		final NodeInfo exprNodeInfo = new NodeInfo(fictitiousId--, NodeInfo.Type.Expression, "", ldNodeInfo);
//		final Node expr = new Node("expression", exprNodeInfo);

		// Result Node
		final NodeInfo resultNodeInfo = new NodeInfo(fictitiousId--, NodeInfo.Type.Result, "", ldNodeInfo);
		final Node result = new Node("result", resultNodeInfo);

		final Node parent = EDGTraverser.getParent(node);

//		edg.addNode(expr);
		edg.addNode(result);

		// Remove the Structural edges if the parent is an expression -> The hidden structural edges inside nodes remain in the graph
		if (!parent.getData().isFictitious() && parent.getData().getInfo().isExpression())
		{
			if (!LASTTraverser.isPatternZone(node))
			{
				edg.removeEDGEdge(parent, node, EdgeInfo.Type.Structural);
				edg.setRemovableEdge(parent, node, EdgeInfo.Type.Structural);
			}
		}

		// Add the structural edge parent -> result to perform tree traversal (not considered for slicing)
		edg.addStructuralEdge(parent, result);

		// Modify Value Arcs
		switch (node.getData().getType())
		{
			case NodeInfo.Type.DataConstructor:
				final boolean isPatternZone = EDGTraverser.isPatternZone(node);
				if (!isPatternZone)
				{
					treatDataConstructorExpressions(node, result);
					break;
				}
			case NodeInfo.Type.Variable: // Variables scope of a call, don't add the result node to the slice
				// 3 levels (in case it is a casting)
				final Node grandParent = EDGTraverser.getParent(parent);
				final Node grandGrandParent = EDGTraverser.getParent(grandParent);
				if (parent.getData().getType() == NodeInfo.Type.Scope ||
					grandParent.getData().getType() == NodeInfo.Type.Scope ||
					grandGrandParent.getData().getType() == NodeInfo.Type.Scope)
					break;
				// When there is a result of a callee, the value arc is only joined from the name node, and there is another one from the scope to the name
			default:
				treatCommonNodes(node, result);
				break;
		}

		// Modify CFG Arcs to add Results to the CFG
		final List<Edge> outgoingCFGEdges = node.getOutgoingEdges();
		outgoingCFGEdges.removeIf(edge -> edge.getData().getType() != EdgeInfo.Type.ControlFlow);

		for (Edge CFGEdge : outgoingCFGEdges)
		{
			final Node to = CFGEdge.getTo();
			edg.removeEdge(CFGEdge);
			final Edge e1 = new Edge(node, result, 0, new EdgeInfo(EdgeInfo.Type.ControlFlow));
			final Edge e2 = new Edge(result, to, 0, new EdgeInfo(EdgeInfo.Type.ControlFlow));
			edg.addEdge(e1);
			edg.addEdge(e2);
		}

		// Value arcs between initial node and its result
		edg.addEdge(node, result, 0, new EdgeInfo(EdgeInfo.Type.Value));
	}

	private static void treatCommonNodes(Node node, Node result)
	{
		final List<Edge> outgoingEdges = node.getOutgoingEdges();
		outgoingEdges.removeIf(edge -> edge.getData().getType() != EdgeInfo.Type.Value);
		
		for (Edge valueEdge : outgoingEdges)
		{
			final Node to = valueEdge.getTo();
			final EdgeConstraint edgeConstraint = valueEdge.getData().getConstraint(); 
			edg.removeEdge(valueEdge);
			final Edge e = new Edge(result, to, 0, new EdgeInfo(EdgeInfo.Type.Value, edgeConstraint));
			edg.addEdge(e);
		}
		
		final List<Edge> incomingEdges = node.getIncomingEdges();
		incomingEdges.removeIf(edge -> edge.getData().getType() != EdgeInfo.Type.Value);

		for (Edge valueEdge : incomingEdges)
		{
			final Node from = valueEdge.getFrom();
			final EdgeConstraint edgeConstraint = valueEdge.getData().getConstraint();
			edg.removeEdge(valueEdge);
			final Edge e = new Edge(from, result, 0, new EdgeInfo(EdgeInfo.Type.Value, edgeConstraint));
			edg.addEdge(e);
		}
	}

	private static void treatDataConstructorExpressions(Node dataConstructor,
														Node result) // TODO: REVIEW AFTER DELETING "EXPRESSION" NODES
	{
		deleteIncomingValueArcs(dataConstructor);
		modifyDataConstructorArcs(dataConstructor, result);
	}

	private static void deleteIncomingValueArcs(Node dataConstructor)
	{
		final List<Edge> incomingEdges = dataConstructor.getIncomingEdges();
		incomingEdges.removeIf(edge -> edge.getData().getType() != EdgeInfo.Type.Value);

		for (Edge valueEdge : incomingEdges)
			edg.removeEdge(valueEdge);	
	}
	private static void modifyDataConstructorArcs(Node dataConstructor, Node result)
	{	
		final List<Node> dataConstructorChildren = EDGTraverser.getChildren(dataConstructor);
		final int dataConstructorChildrenCount = dataConstructorChildren.size();

		for (int childIndex = 0; childIndex < dataConstructorChildrenCount; childIndex++)
		{
			final Node dataConstructorChild = dataConstructorChildren.get(childIndex);
			final Node from = dataConstructorChild.getData().getType() == NodeInfo.Type.Expression ?
					EDGTraverser.getChild(dataConstructorChild, NodeInfo.Type.Result) : dataConstructorChild;
			final DataConstructorConstraint constraint = new DataConstructorConstraint(
					AccessConstraint.Operation.Remove, childIndex + "");
			edg.addEdge(from, result, 0, new EdgeInfo(EdgeInfo.Type.Value, constraint));
		}
		
		final List<Edge> outgoingEdges = dataConstructor.getOutgoingEdges();
		outgoingEdges.removeIf(edge -> edge.getData().getType() != EdgeInfo.Type.Value);
		
		for (Edge valueEdge : outgoingEdges)
		{
			final Node to = valueEdge.getTo();
			final EdgeConstraint edgeConstraint = valueEdge.getData().getConstraint(); 
			edg.removeEdge(valueEdge);
			final Edge e = new Edge(result, to, 0, new EdgeInfo(EdgeInfo.Type.Value, edgeConstraint));
			edg.addEdge(e);
		}
	}
}