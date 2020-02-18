package edg.edge;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import edg.constraint.AsteriskConstraint;
import edg.constraint.Constraints;
import edg.constraint.GlobalVariableConstraint;
import edg.constraint.GrammarConstraint;
import edg.constraint.SeekingConstraint;
import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Grammar;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.slicing.ConstrainedAlgorithm;
import edg.slicing.Phase;
import edg.traverser.EDGTraverser;
import edg.traverser.EDGTraverser.Direction;
import edg.work.NodeWork;
import edg.work.Work;
import edg.work.WorkList;

public class SummaryEdgeGenerator extends EdgeGenerator
{
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
		final List<Node> calls = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Call);

		for (Node call : calls)
		{
			final Node calleeNode = EDGTraverser.getChild(call, 0);
			final Node calleeResultNode = EDGTraverser.getChild(calleeNode, 2);
			final List<Node> inputs = EDGTraverser.getInputs(calleeResultNode, EDGTraverser.Direction.Forwards);
			if (!inputs.isEmpty())
				continue;

			final Node callResult = EDGTraverser.getSibling(call, 1);
			final Node arguments = EDGTraverser.getChild(call, 1);
			final List<Node> argumentNodes = EDGTraverser.getChildren(arguments);
			
			for (Node argumentNode : argumentNodes)
			{
				final Node result = EDGTraverser.getResult(argumentNode);

				if (result != null)
					this.edg.addEdge(result, callResult, 0, new EdgeInfo(EdgeInfo.Type.Summary, AsteriskConstraint.getConstraint()));
			}
		}
	}

	/************************************/
	/************* Internal *************/
	/************************************/
private final Map<String, Integer> ids = new Hashtable<String, Integer>();
	private void generateInternalSummaryEdges()
	{
		final List<Work> initialWorks = this.getInitialWorks();
		final WorkList workList = new WorkList(initialWorks);
		final ConstrainedAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();

long worksProcessed = 0;
		while (workList.hasMore())
		{
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
			final Constraints constraints = work.getConstraints();
			boolean isFormalIn = false;

			if (work instanceof NodeWork)
			{
				final NodeWork nodeWork = (NodeWork) work;
				final Node currentNode = nodeWork.getCurrentNode();

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
			workList.pendAll(newWorks);
		}
// TODO Borrame
System.out.println("Works done: " + workList.getDoneNodes().size());
System.out.println("Works processed: " + worksProcessed);
System.out.println();
	}
	private List<Work> getInitialWorks()
	{
		final List<Work> workList = new LinkedList<Work>();
		final List<Node> routines = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Routine);

		for (Node routine : routines)
		{
			final List<Node> clauses = EDGTraverser.getChildren(routine);
			
			for (Node clause : clauses)
			{
				// Summary Edges for the result node
				final Node clauseResult = EDGTraverser.getChild(clause, 3);
				workList.add(new NodeWork(clauseResult, clauseResult, new Constraints()));
				
				// Summary Edges for Reference variables (Global Variables)
				final Node clauseParameters = EDGTraverser.getChild(clause, 0);
				final List<Edge> globalVarDefinitions = EDGTraverser.getEdges(clauseParameters, Direction.Backwards, EdgeInfo.Type.Flow);
				for (Edge globalVarDefinition : globalVarDefinitions)
				{
					if (globalVarDefinition.getData().getConstraint() instanceof GlobalVariableConstraint)
					{
						final Node gvDefNode = globalVarDefinition.getFrom();
						workList.add(new NodeWork(gvDefNode, gvDefNode, new Constraints()));
					}
				}
			}
		}
		return workList;
	}
	private boolean isFormalIn(Node node, Node formalOutNode)
	{
		final Node parent = EDGTraverser.getParent(node);
		final NodeInfo.Type nodeType = node.getData().getType();
		final NodeInfo.Type parentType = parent == null ? null : parent.getData().getType();
		final Node grandParent = parent == null ? null : EDGTraverser.getParent(parent);
		final NodeInfo.Type grandParentType = grandParent == null ? null : grandParent.getData().getType();
		if (parent == null)
			return false;
		if (nodeType != NodeInfo.Type.Parameters)
			if (nodeType != NodeInfo.Type.Result || parentType != NodeInfo.Type.Expression || grandParentType != NodeInfo.Type.Parameters)
				return false;

		// The formal in must be related to the formal out
		final Node parameters = EDGTraverser.getAncestor(node, NodeInfo.Type.Parameters);
		final Node clauseResult = EDGTraverser.getSibling(parameters, 3);
		
		final Node formalOutResult = EDGTraverser.getResult(EDGTraverser.getAncestor(formalOutNode, NodeInfo.Type.Clause));
		
		return clauseResult == formalOutResult;
	}
	private List<Node> createSummaryEdges(Node formalOut, Node formalIn, Constraints constraints)
	{
		final Grammar grammar = this.edg.getGrammar();
		final GrammarConstraint grammarConstraint = new GrammarConstraint(grammar, formalIn);
		this.edg.addProduction(grammarConstraint, constraints);

		final List<Node> nodesToContinue = new LinkedList<Node>();
		
		if (formalIn.getData().getType() != NodeInfo.Type.Parameters)
		{
			final List<Node> inputs = EDGTraverser.getInputs(formalIn, EDGTraverser.Direction.Backwards);
	
			for (Node input : inputs)
			{
				final Node call = EDGTraverser.getAncestor(input, NodeInfo.Type.Call);
				if (call == null)
					continue;
	
				final Node callResult = EDGTraverser.getSibling(call, 1);
				if (EDGTraverser.getParent(formalOut).getData().getType() != NodeInfo.Type.Clause) // PART FOR GLOBAL VARIABLE'S SUMMARIES
				{
					final List<Edge> GVOutEdges = EDGTraverser.getEdges(formalOut, Direction.Forwards, EdgeInfo.Type.Flow); 
					for (Edge GVOutEdge : GVOutEdges)
						if (GVOutEdge.getData().getConstraint() instanceof GlobalVariableConstraint)
						{
							final String varName = ((GlobalVariableConstraint) GVOutEdge.getData().getConstraint()).getVariableName(); 
							final GlobalVariableConstraint removeConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Remove, varName);
							final Node callNode = EDGTraverser.getSibling(callResult, 0);
							final Node callArgOut = EDGTraverser.getChild(callNode, 3);
							this.edg.addEdge(input, callArgOut, 0, new EdgeInfo(EdgeInfo.Type.Flow, removeConstraint));
						}
				}
				else // PART FOR FUNCTION RESULT'S SUMMARIES
					this.edg.addEdge(input, callResult, 0, new EdgeInfo(EdgeInfo.Type.Summary, grammarConstraint));
				
				nodesToContinue.add(callResult);
			}
		}
		else
		{
			final Node functionClauseNode = EDGTraverser.getParent(formalIn);
			final List<Node> inputs = EDGTraverser.getInputs(functionClauseNode, EDGTraverser.Direction.Backwards);
			
			for (Node input : inputs)
			{
				final Node call = EDGTraverser.getAncestor(input, NodeInfo.Type.Call);
				if (call == null)
					continue;
				
				final Node callResult = EDGTraverser.getSibling(call, 1);
				if (EDGTraverser.getParent(formalOut).getData().getType() != NodeInfo.Type.Clause) // PART FOR GLOBAL VARIABLE'S SUMMARIES
				{	final List<Edge> GVInEdges = EDGTraverser.getEdges(formalIn, Direction.Forwards, EdgeInfo.Type.Flow);
				
					for (Edge GVInEdge : GVInEdges)	
						if (GVInEdge.getData().getConstraint() instanceof GlobalVariableConstraint)
						{
							final String varName = ((GlobalVariableConstraint) GVInEdge.getData().getConstraint()).getVariableName(); 
							final GlobalVariableConstraint letThroughConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.LetThrough, varName);
							final Node callNode = EDGTraverser.getSibling(callResult, 0);
							final Node callArgOut = EDGTraverser.getChild(callNode, 3);
							final Node callArgIn = EDGTraverser.getChild(callNode, 2);
							this.edg.addEdge(callArgIn, callArgOut, 0, new EdgeInfo(EdgeInfo.Type.Flow, letThroughConstraint));
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
			final String id = nodeToContinue.getData().getId() + "";
			workList.repend(id);
		}
	}
}