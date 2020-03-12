package upv.slicing.edg.edge;

import upv.slicing.edg.constraint.*;
import upv.slicing.edg.graph.*;
import upv.slicing.edg.slicing.ConstrainedAlgorithm;
import upv.slicing.edg.slicing.Phase;
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

	/************************************/
	/************* External *************/
	/************************************/
	private void generateExternalSummaryEdges()
	{
		final List<Node> calls = this.edg.getNodes(Node.Type.Call);

		for (Node call : calls)
		{
			final Node calleeNode = edg.getChild(call, Node.Type.Callee);
			final Node calleeResultNode = edg.getResFromNode(calleeNode);
			final List<Node> inputs = edg.getInputs(calleeResultNode, LAST.Direction.Forwards);
			if (!inputs.isEmpty())
				continue;

			final Node callResult = edg.getResFromNode(call);
			final Node arguments = edg.getChild(call, Node.Type.Arguments);
			final List<Node> argumentNodes = edg.getChildren(arguments);

			for (Node argumentNode : argumentNodes)
			{
				final Node result = edg.getResult(argumentNode);

				if (result != null)
					this.edg.addEdge(result, callResult, new Edge(Edge.Type.Summary, AsteriskConstraint.getConstraint()));
			}
		}
	}

	/************************************/
	/************* Internal *************/
	/************************************/
private final Map<String, Integer> ids = new Hashtable<>();
	private void generateInternalSummaryEdges()
	{
		final List<Work> initialWorks = this.getInitialWorks();
		final WorkList workList = new WorkList(initialWorks);
		final ConstrainedAlgorithm slicingAlgorithm = new ConstrainedAlgorithm(edg);

long worksProcessed = 0;
		while (workList.hasMore())
		{
		
//if (workList.getPendingWorks().size() == 1)
//	System.out.println("PARA");
//if (workList.getPendingWorks().containsKey("203->228"))
//	System.out.println("Aun esta 1");
			final Work work = workList.next();
//if (workList.contains(work))
//	continue;
			
// TODO Borrame
worksProcessed++;
final String id = work.getId();
final Integer prev = ids.get(id);
ids.put(id, prev == null ? 0 : prev + 1);
if (prev != null && prev == 100000)
System.out.println(work.getId() + " - " + work.getConstraints().getEdgeConstraints().toString());
			final Node initialNode = work.getInitialNode();
			
//final int initialNodeId = initialNode.getId();
//if(initialNodeId == 48)
//	System.out.print("");
			final Constraints constraints = work.getConstraints();
			boolean isFormalIn = false;
			
			if (work instanceof NodeWork)
			{
				final NodeWork nodeWork = (NodeWork) work;
				final Node currentNode = nodeWork.getCurrentNode();
//final int currentNodeId = currentNode.getId();
//if(currentNodeId == 228)
//System.out.println(" Node "+currentNodeId);

				// ESTO SE USA PARA EVITAR BUCLES INFINITOS AL GENERAR SUMMARIES, SE PIERDE PRECISION Y PUEDE DAR COMO RESULTADO GRAMATICAS INCOMPLETAS O INCLUSO ERRONEAS
				if (workList.getDoneNodes().contains(currentNode) && edg.getParent(initialNode).getType() != Node.Type.Clause)
					continue;

				if (isFormalIn = this.isFormalIn(currentNode, initialNode))
				{	
					final List<Node> nodesToContinue = this.createSummaryEdges(initialNode, currentNode, constraints);
					this.rependWorks(workList, nodesToContinue);
				}
			}
			workList.done(work);
			if (isFormalIn)
				continue;

			final List<Work> newWorks = slicingAlgorithm.processWork(Phase.SummaryGeneration, work);
//if(initialNodeId == 48)
//	System.out.print("");
			workList.pendAll(newWorks);
		}
// TODO Borrame
//System.out.println("Works done: " + workList.getDoneNodes().size());
//System.out.println("Works processed: " + worksProcessed);
//System.out.println();
	}
	private List<Work> getInitialWorks()
	{
		final List<Work> workList = new LinkedList<>();
		final List<Node> routines = this.edg.getNodes(Node.Type.Routine);

		for (Node routine : routines)
		{
			final List<Node> clauses = edg.getChildren(routine);
			clauses.removeIf(node -> node.getType() == Node.Type.Result);
			
			for (Node clause : clauses)
			{
				// Summary Edges for the result node
				final Node clauseResult = edg.getResFromNode(clause);
				workList.add(new NodeWork(clauseResult, clauseResult, new Constraints()));

				// Summary Edges for Reference variables (Global Variables)
				final Node clauseParameterOut = edg.getChild(clause, Node.Type.ParameterOut);
				final Set<Edge> globalVarDefinitions = edg.getEdges(clauseParameterOut, LAST.Direction.Backwards, Edge.Type.Flow);
				for (Edge globalVarDefinition : globalVarDefinitions)
				{
					if (globalVarDefinition.getConstraint() instanceof GlobalVariableConstraint)
					{
						final Node gvDefNode = edg.getEdgeSource(globalVarDefinition);
						workList.add(new NodeWork(gvDefNode, gvDefNode, new Constraints()));
					}
				}
			}
		}
		return workList;
	}
	private boolean isFormalIn(Node node, Node formalOutNode)
	{
		final Node parent = edg.getParent(node);
		final Node.Type nodeType = node.getType();
		final Node.Type parentType = parent == null ? null : parent.getType();
		final Node grandParent = parent == null ? null : edg.getParent(parent);
		final Node.Type grandParentType = grandParent == null ? null : grandParent.getType();
		if (parent == null)
			return false;
		if (nodeType != Node.Type.Parameters)
			if (nodeType != Node.Type.Result || parentType != Node.Type.Expression ||
				grandParentType != Node.Type.Parameters)
				return false;

		// The formal in must be related to the formal out
		final Node clause = edg.getAncestor(node, Node.Type.Clause);
		final Node clauseResult = edg.getResFromNode(clause);

		final Node formalOutResult = edg.getResult(edg.getAncestor(formalOutNode, Node.Type.Clause));

		return clauseResult == formalOutResult;
	}
	private List<Node> createSummaryEdges(Node formalOut, Node formalIn, Constraints constraints)
	{
		final Grammar grammar = this.edg.getGrammar();
		final GrammarConstraint grammarConstraint = new GrammarConstraint(grammar, formalIn);
		this.edg.addProduction(grammarConstraint, constraints);

		final List<Node> nodesToContinue = new LinkedList<>();
		
		if (formalIn.getType() != Node.Type.Parameters)
		{
			final List<Node> inputs = edg.getInputs(formalIn, LAST.Direction.Backwards);
	
			for (Node input : inputs)
			{
				final Node call = edg.getAncestor(input, Node.Type.Call);
				if (call == null)
					continue;

				final Node callResult = edg.getResFromNode(call);
				if (edg.getParent(formalOut).getType() != Node.Type.Clause) // PART FOR GLOBAL VARIABLE'S SUMMARIES
				{
					final Set<Edge> GVOutEdges = edg.getEdges(formalOut, LAST.Direction.Forwards, Edge.Type.Flow);
					for (Edge GVOutEdge : GVOutEdges)
						if (GVOutEdge.getConstraint() instanceof GlobalVariableConstraint)
						{
							final String varName = ((GlobalVariableConstraint) GVOutEdge.getConstraint()).getVariableName();
							final GlobalVariableConstraint removeConstraint = new GlobalVariableConstraint(
									SeekingConstraint.Operation.Remove, varName);
							final Node callNode = edg.getSibling(callResult, Node.Type.Value);
							final Node callArgOut = edg.getChild(callNode, Node.Type.ArgumentOut);
							this.edg.addEdge(input, callArgOut, new Edge(Edge.Type.Summary, removeConstraint));
						}
				}
				else // PART FOR FUNCTION RESULT'S SUMMARIES
					this.edg.addEdge(input, callResult, new Edge(Edge.Type.Summary, grammarConstraint));
				
				nodesToContinue.add(callResult);
			}
		}
		else
		{
			final Node functionClauseNode = edg.getParent(formalIn);
			final List<Node> inputs = edg.getInputs(functionClauseNode, LAST.Direction.Backwards);
			
			for (Node input : inputs)
			{
				final Node call = edg.getAncestor(input, Node.Type.Call);
				if (call == null)
					continue;
				
				final Node callResult = edg.getSibling(call, 1);
				if (edg.getParent(formalOut).getType() != Node.Type.Clause) // PART FOR GLOBAL VARIABLE'S SUMMARIES
				{
					final Set<Edge> GVInEdges = edg.getEdges(formalIn, LAST.Direction.Forwards, Edge.Type.Flow);
				
					for (Edge GVInEdge : GVInEdges)
						if (GVInEdge.getConstraint() instanceof GlobalVariableConstraint)
						{
							final String varName = ((GlobalVariableConstraint) GVInEdge.getConstraint()).getVariableName();
							final GlobalVariableConstraint letThroughConstraint = new GlobalVariableConstraint(
									SeekingConstraint.Operation.LetThrough, varName);
							final Node callNode = edg.getSibling(callResult, Node.Type.Value);
							final Node callArgOut = edg.getChild(callNode, Node.Type.ArgumentOut);
							final Node callArgIn = edg.getChild(callNode, Node.Type.ArgumentIn);
							this.edg.addEdge(callArgIn, callArgOut, new Edge(Edge.Type.Flow, letThroughConstraint));
						}
				}
				nodesToContinue.add(callResult);
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
}
