package upv.slicing.edg.edge;

import upv.slicing.edg.Config;
import upv.slicing.edg.constraint.*;
import upv.slicing.edg.graph.*;
import upv.slicing.edg.slicing.ConstrainedAlgorithm;
import upv.slicing.edg.slicing.Phase;
import upv.slicing.edg.slicing.SlicingAlgorithm;
import upv.slicing.edg.work.NodeWork;
import upv.slicing.edg.work.Work;
import upv.slicing.edg.work.WorkList;

import java.util.*;

public class SummaryEdgeGenerator extends EdgeGenerator {
	public SummaryEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		this.generateExternalSummaryEdges();
		this.generateInternalSummaryEdges();
	}

	// ---------------------------------- //
	// ------------ External ------------ //
	// ---------------------------------- //
	private void generateExternalSummaryEdges()
	{
		final List<Node> calls = this.edg.getNodes(Node.Type.Call);

		for (Node call : calls)
		{
			final Node callee = edg.getChild(call, Node.Type.Callee);
			final Node calleeResult = edg.getResFromNode(callee);
			final Set<Edge> callEdges = edg.getEdges(calleeResult, LAST.Direction.Forwards, Edge.Type.Call);
			if (!callEdges.isEmpty())
				continue;

			final Node callResult = edg.getResFromNode(call);
			final Node argumentsNode = edg.getChild(call, Node.Type.Arguments);
			final List<Node> arguments = edg.getChildren(argumentsNode);
			arguments.removeIf(n -> n.getType() == Node.Type.Result);

			for (Node argument : arguments)
			{
				final Node argumentResult = edg.getResFromNode(argument);
				this.edg.addEdge(argumentResult, callResult, new Edge(Edge.Type.Summary, AsteriskConstraint.getConstraint()));
			}
		}
	}

	// ---------------------------------- //
	// ------------ Internal ------------ //
	// ---------------------------------- //
	private void generateInternalSummaryEdges()
	{
		final List<Work> initialWorks = this.getInitialWorks();
		final WorkList workList = new WorkList(initialWorks);
		final ConstrainedAlgorithm slicingAlgorithm = (ConstrainedAlgorithm) Config.CREATE_SLICING_ALGORITHM.apply(edg);

		while (workList.hasMore())
		{
			final Work work = workList.next();
			final Node initialNode = work.getInitialNode();
			final Constraints constraints = work.getConstraints();
			boolean isFormalIn = false;
			
			if (work instanceof NodeWork)
			{
				final NodeWork nodeWork = (NodeWork) work;
				final Node currentNode = nodeWork.getCurrentNode();

				// When we reach a clause node when generating summaries, there is no need to continue, and
				// The routine must not be treated (there is a control arc between the clause result and the routine)
				// Declaration arcs to data members must be ignored

				if (currentNode.getType() == Node.Type.Clause ||
						currentNode.getType() == Node.Type.Routine ||
						currentNode.getType() == Node.Type.Module)
					continue;

				// ESTO SE USA PARA EVITAR BUCLES INFINITOS AL GENERAR SUMMARIES,
				// SE PIERDE PRECISION Y PUEDE DAR COMO RESULTADO GRAMATICAS INCOMPLETAS O INCLUSO ERRONEAS
				if (workList.getDoneNodes().contains(currentNode) && edg.getNodeFromRes(initialNode).getType() != Node.Type.Clause)
					continue;

				if (isFormalIn = this.isFormalIn(currentNode, initialNode)) {
					final List<Node> nodesToContinue = this.createSummaryEdges(initialNode, currentNode, constraints);
					this.rependWorks(workList, nodesToContinue);
				}
			}
			workList.done(work);
			if (isFormalIn)
				continue;

			final List<Work> newWorks = slicingAlgorithm.processWork(Phase.SummaryGeneration, work);
			workList.pendAll(newWorks);
		}
	}
	private List<Work> getInitialWorks()
	{
		final List<Work> workList = new LinkedList<>();
		final List<Node> clauses = this.edg.getNodes(Node.Type.Clause);

		for (Node clause : clauses)
		{
			List<Node> calls = edg.getNodes(clause, LAST.Direction.Backwards, Edge.Type.Call);
			if (calls.isEmpty())
				continue;

			// Summary Edges for the result node
			final Node clauseResult = edg.getResFromNode(clause);
			workList.add(new NodeWork(clauseResult, clauseResult, new Constraints()));

			// Summary Edges for Reference variables (Global Variables)
			final Node clauseParameterOut = edg.getChild(clause, Node.Type.ParameterOut);
			final List<Node> gvDefinitions = edg.getChildren(clauseParameterOut);
			gvDefinitions.removeIf(node -> node.getType() == Node.Type.Result);

			for (Node gvDefinition : gvDefinitions) {
				final Node gvRes = edg.getResFromNode(gvDefinition);
				workList.add(new NodeWork(gvRes, gvRes, new Constraints()));
			}
		}

		return workList;
	}
	private boolean isFormalIn(Node node, Node formalOutNode)
	{
/*		final Node parent = edg.getParent(node);
		final Node.Type nodeType = node.getType();
		final Node.Type parentType = parent == null ? null : parent.getType();
		if (parent == null)
			return false;

		if (nodeType != Node.Type.Parameters)
			if (nodeType != Node.Type.Result || parentType != Node.Type.Parameters)
				return false;
*/
		if (node == formalOutNode)
			return false;

		// TODO: Only formal-ins of 1 lvl depth (Erlang may differ due to tuple parameters)
		final Node parent = edg.getParent(node);
		final Node.Type parentType = parent.getType();

		// TODO: Parent module in global variable declaration arcs
		//  (Make them interprocedural to avoid traversals in summaries?)
		if (parent == null || parent.getType() == Node.Type.Module)
			return false;

		if (parentType != Node.Type.Parameters && parentType != Node.Type.ParameterIn)
			return false;

		// The formal in must be related to the formal out
		final Node clause = edg.getAncestor(node, Node.Type.Clause);
		final Node clauseResult = edg.getResFromNode(clause);

		// The corresponding clause result for parameter out summaries
		if (edg.getParent(formalOutNode).getType() == Node.Type.ParameterOut) {
			final Node routineFormalOut = edg.getResFromNode(edg.getAncestor(formalOutNode, Node.Type.Clause));
			return clauseResult == routineFormalOut;
		}

		return clauseResult == formalOutNode;
	}
	private List<Node> createSummaryEdges(Node formalOut, Node formalIn, Constraints constraints)
	{
		final Grammar grammar = this.edg.getGrammar();
		final GrammarConstraint grammarConstraint = new GrammarConstraint(grammar, formalIn);
		this.edg.addProduction(grammarConstraint, constraints);

		final List<Node> nodesToContinue = new LinkedList<>();
		final List<Node> inputs = edg.getInputs(formalIn, LAST.Direction.Backwards);

		for (Node input : inputs)
		{
			final Node call = edg.getAncestor(input, Node.Type.Call);
			if (call == null)
				continue;

			final Node callResult = edg.getResFromNode(call);

			// PART FOR FUNCTION RESULT'S SUMMARIES
			if (edg.getNodeFromRes(formalOut).getType() == Node.Type.Clause)
			{
				this.edg.addEdge(input, callResult, new Edge(Edge.Type.Summary, grammarConstraint));
				// TODO: Mejorable, no me detengo en las calls, hago como si no hubiera summaries y
				//  cada vez que creo un summary lo hago a partir de ahi otra vez.
				nodesToContinue.add(callResult);
			}
			else { // PART FOR ARG OUT SUMMARIES
				final int argOutIndex = edg.getChildIndex(formalOut);
				final Node callArgOut = edg.getPolymorphicNode(call, formalIn.getInfo().getClassName(), Edge.Type.Output);
				final Node argOut = edg.getChild(callArgOut, argOutIndex);
				this.edg.addEdge(input, argOut, new Edge(Edge.Type.Summary, grammarConstraint));
				nodesToContinue.add(callArgOut);
			}
		}
		return nodesToContinue;
	}
	private void rependWorks(WorkList workList, List<Node> nodesToContinue)
	{
		for (Node nodeToContinue : nodesToContinue)
		{
			final String id = nodeToContinue.getId() + "";
			workList.repend(id);
		}
	}

	private Node getOutNode(Node call, String className)
	{
		final Node callee = edg.getChild(call, Node.Type.Callee);
		final Node name = edg.getChild(callee, Node.Type.Name);
		final String routineName = edg.getChild(name, Node.Type.Value).getName();

		Node objectVar;
		final Node argOut = edg.getChild(call, Node.Type.ArgumentOut);
		final List<Node> argOutChildren = edg.getChildren(argOut);

		if (argOutChildren.size() == 0 || routineName.equals("<constructor>"))
			return argOut;

		objectVar = edg.getChildren(argOut).get(0);

		if (objectVar == null)
			throw new RuntimeException("The scope expression type is not contemplated");

		final List<Node> polymorphicNodes = edg.getChildren(objectVar);
		for (Node node : polymorphicNodes)
			if (node.getName().startsWith(className + "."))
				return node;

		throw new RuntimeException("There is no polymorphic node for class " + className);
	}
}
