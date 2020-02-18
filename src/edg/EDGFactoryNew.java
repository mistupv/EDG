package edg;

import java.util.List;

import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.constraint.AccessConstraint;
import edg.constraint.DataConstructorConstraint;
import edg.constraint.EdgeConstraint;
import edg.edge.ControlEdgeGenerator;
import edg.edge.ExceptionEdgeGenerator;
import edg.edge.FlowEdgeGeneratorNew;
import edg.edge.InterproceduralEdgeGeneratorNew;
import edg.edge.SummaryEdgeGeneratorNew;

import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.traverser.EDGTraverser;
import edg.traverser.EDGTraverserNew;
import edg.graph.LAST;

public class EDGFactoryNew
{
	private static EDG edg;
	private static int fictitiousId = -1;
	
	// Required Configuration Parameters (Questions made to the users in order to decide the dependencies built on the graph) 
	private static boolean isOOLanguage = true;
	
	public static EDG createEDG(LAST last)
	{
		initializeEDG(last);
		transformExpressionNodes();
		if (isOOLanguage)
			LASTBuilder.addInheritanceInfomation(edg); // Only For OOPrograms With Inheritance 
		generateDependencies();
		deleteRemovableStructuralArcs();
		return edg;
	}
	
	private static void initializeEDG(LAST last)
	{
		edg = new EDG(last);
	}
	private static void transformExpressionNodes()
	{
		final List<Node> expressionNodes = EDGTraverserNew.getNodes(edg, node -> node.getData().getInfo() != null && node.getData().getInfo().isExpression());		
		for (Node expression : expressionNodes)
			createThreeNodeStructures(expression);
	}
	private static void generateDependencies() 
	{
		new ControlEdgeGenerator(edg).generate();
		
		if (isOOLanguage)
			new InterproceduralEdgeGeneratorNew(edg).generate(); // Specially Generated for OOPrograms
		else
			new InterproceduralEdgeGeneratorNew(edg).generateNoInheritance(); // TODO Implement without module analysis

		new FlowEdgeGeneratorNew(edg).generate(); // TODO Testear y verlo paso a paso con el ControlFlowTraverser
		new SummaryEdgeGeneratorNew(edg).generate();
//		new ExceptionEdgeGenerator(edg).generate();
	}
	
	
	private static void createThreeNodeStructures(Node node)
	{	
		// Conversion of 1 to 3 nodes & structural change
		final LDASTNodeInfo nodeInfo = node.getData().getInfo();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(nodeInfo.getArchive(),nodeInfo.getClassName(),
				nodeInfo.getLine(),nodeInfo.getConstruction());

		// Expr Node
//		final NodeInfo exprNodeInfo = new NodeInfo(fictitiousId--, NodeInfo.Type.Expression, "", ldNodeInfo);
//		final Node expr = new Node("expression", exprNodeInfo);

		// Result Node
		final NodeInfo resultNodeInfo = new NodeInfo(fictitiousId--, NodeInfo.Type.Result, "", ldNodeInfo);
		final Node result = new Node("result", resultNodeInfo);

		final EdgeInfo edgeInfo = new EdgeInfo(EdgeInfo.Type.Structural);

		final Node parent = EDGTraverserNew.getParent(node);

//		edg.addNode(expr);
		edg.addNode(result);

		// Remove the Structural edges if the parent is an expression
		if (!parent.getData().isFictitious() && parent.getData().getInfo().isExpression())
			edg.setRemovableEdge(parent, node, EdgeInfo.Type.Structural);

//edg.removeEdge(parent, node, EdgeInfo.Type.Structural);

		// Value arcs for between initial node and its result
		edg.addEdge(node, result, 0, new EdgeInfo(EdgeInfo.Type.Value));

		// Modify Value Arcs
		switch(node.getData().getType())
		{
			case DataConstructor:
				final boolean isPatternZone = EDGTraverser.isPatternZone(node);
				if (!isPatternZone)
				{
					treatDataConstructorExpressions(node, result);
					break;
				}
			case Variable: // Variables scope of a call, don't add the result node to the slice
				// 3 levels (in case it is a casting)
				final Node grandParent = EDGTraverserNew.getParent(parent);
				final Node grandGrandParent = EDGTraverserNew.getParent(grandParent);
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
	
	private static void treatDataConstructorExpressions(Node dataConstructor, Node result)
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
					EDGTraverserNew.getChild(dataConstructorChild, NodeInfo.Type.Result) : dataConstructorChild; 
			final DataConstructorConstraint constraint = new DataConstructorConstraint(AccessConstraint.Operation.Remove, childIndex + "");
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

	private static void deleteRemovableStructuralArcs()
	{
		List<Edge> edges = edg.getEdges();
		for (Edge edge : edges)
			if (edge.isMarked())
				edg.removeEdge(edge);
	}
}