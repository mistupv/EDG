package upv.slicing.edg;

import upv.slicing.edg.constraint.AccessConstraint;
import upv.slicing.edg.constraint.DataConstructorConstraint;
import upv.slicing.edg.constraint.EdgeConstraint;
import upv.slicing.edg.edge.*;
import upv.slicing.edg.graph.*;

import java.util.*;
import java.util.function.Predicate;

public class EDGFactory {
	private final LAST last;
	private EDG edg;


	public EDGFactory(LAST last)
	{
		this.last = last;
	}

	// Required Configuration Parameters (Questions made to the users in order to decide the dependencies built on the graph) 
	private static boolean isOOLanguage = true;

	public EDG createEDG()
	{
		initializeEDG();
		if (isOOLanguage)
			new DynamicTypesGenerator(edg).generate();
		transformExpressionNodes();
		if (isOOLanguage) {
			// Only For OOPrograms With Inheritance
			LASTBuilder.addInheritanceInformation(edg);
			LASTBuilder.completeFieldAccessTypes(edg);
		}
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

		InterproceduralEdgeGeneratorNew ieg = new InterproceduralEdgeGeneratorNew(edg);
		if (isOOLanguage)
			ieg.generateCallEdges(); // Specially Generated for OOPrograms
		else
			new InterproceduralEdgeGenerator(edg).generateNoInheritance(); // TODO Implement without module analysis

		new FlowEdgeGeneratorNew(edg).generate(); // TODO Testear y verlo paso a paso con el ControlFlowTraverser
//		ieg.generateIO();
//		new SummaryEdgeGenerator(edg).generate();
//		new ExceptionEdgeGenerator(edg).generate();
	}

	private void createThreeNodeStructures(Node node)
	{
		// Conversion of 1 to 2 nodes & structural change
		final LDASTNodeInfo nodeInfo = node.getInfo();
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(nodeInfo.getFile(), nodeInfo.getClassName(),
														   nodeInfo.getLine(), nodeInfo.getConstruction());

		// Result Node
		final Node result = new Node("result", edg.getNextFictitiousId(), Node.Type.Result, "", ldNodeInfo);

		final Node parent = last.getParent(node);

		edg.addVertex(result);
		edg.registerNodeResPair(node, result);

		// Remove the Structural edges if the parent is an expression -> The hidden structural edges inside nodes remain in the graph
		if (!parent.getType().isFictitious() && parent.getInfo().isExpression())
		{
			// THIS IS AN EXCEPTION IN ORDER TO INCLUDE ROUTINES IN THE SLICE
			// WHEN CFG IS FINISHED THEY WILL BE INCLUDED BY CONTROL
			if (parent.getType() != Node.Type.Routine)
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
		outgoingCFGEdges.removeIf(Predicate.not(Edge::isControlFlowEdge));

		if (!outgoingCFGEdges.isEmpty())
			edg.addEdge(node, result, Edge.Type.ControlFlow);

		for (Edge cfgEdge : outgoingCFGEdges)
		{
			final Node to = edg.getEdgeTarget(cfgEdge);
			edg.removeEdge(cfgEdge);
			edg.addEdge(result, to, cfgEdge.getType());
		}
	}

	private void createThreeNodeStructuresSpecial(Node node, Node result)
	{

		switch(node.getType())
		{
			case Routine:
				final Set<Edge> incomingCFGEdges = edg.incomingEdgesOf(node);
				incomingCFGEdges.removeIf(Predicate.not(Edge::isControlFlowEdge));
				for (Edge cfgEdge : incomingCFGEdges)
				{
					final Node from = edg.getEdgeSource(cfgEdge);
					edg.removeEdge(cfgEdge);
					edg.addEdge(from,result, cfgEdge.getType());
				}
				break;
			case Clause:
				final Set<Edge> incomingClauseCFGEdges = edg.incomingEdgesOf(node);
				incomingClauseCFGEdges.removeIf(Predicate.not(Edge::isControlFlowEdge));
				for (Edge cfgEdge : incomingClauseCFGEdges)
				{
					final Node from = edg.getEdgeSource(cfgEdge);
					if (from.getType() != Node.Type.Routine)
					{
						edg.removeEdge(cfgEdge);
						edg.addEdge(from, result, cfgEdge.getType());
					}
				}

				final Set<Edge> outgoingClauseCFGEdges = edg.outgoingEdgesOf(node);
				outgoingClauseCFGEdges.removeIf(Predicate.not(Edge::isControlFlowEdge));
				for (Edge cfgEdge : outgoingClauseCFGEdges)
				{
					final Node finalToDestination = edg.getEdgeTarget(cfgEdge);
					Node to = finalToDestination;
					if (to.getType() == Node.Type.Result)
						to = edg.getNodeFromRes(to);

					if (to.getType() == Node.Type.Routine)
					{
						edg.removeEdge(cfgEdge);
						edg.addEdge(result, finalToDestination, cfgEdge.getType());
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
