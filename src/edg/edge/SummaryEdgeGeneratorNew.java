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
import edg.traverser.EDGTraverserNew;
import edg.traverser.LASTTraverser.Direction;
import edg.work.NodeWork;
import edg.work.Work;
import edg.work.WorkList;

public class SummaryEdgeGeneratorNew extends EdgeGenerator
{
	public SummaryEdgeGeneratorNew(EDG edg)
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
		final List<Node> calls = EDGTraverserNew.getNodes(this.edg, NodeInfo.Type.Call);

		for (Node call : calls)
		{
			final Node calleeNode = EDGTraverserNew.getChild(call, NodeInfo.Type.Callee);
			final Node calleeResultNode = EDGTraverserNew.getChild(calleeNode, NodeInfo.Type.Result);
			final List<Node> inputs = EDGTraverserNew.getInputs(calleeResultNode, EDGTraverserNew.Direction.Forwards);
			if (!inputs.isEmpty())
				continue;

			final Node callResult = EDGTraverserNew.getSibling(call, NodeInfo.Type.Result);
			final Node arguments = EDGTraverserNew.getChild(call, NodeInfo.Type.Arguments);
			final List<Node> argumentNodes = EDGTraverserNew.getChildren(arguments);
			
			for (Node argumentNode : argumentNodes)
			{
				final Node result = EDGTraverserNew.getResult(argumentNode);

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
			
//final int initialNodeId = initialNode.getData().getId();
//if(initialNodeId == 48)
//	System.out.print("");
			final Constraints constraints = work.getConstraints();
			boolean isFormalIn = false;
			
			if (work instanceof NodeWork)
			{
				final NodeWork nodeWork = (NodeWork) work;
				final Node currentNode = nodeWork.getCurrentNode();
//final int currentNodeId = currentNode.getData().getId();
//if(currentNodeId == 228)
//System.out.println(" Node "+currentNodeId);

				// ESTO SE USA PARA EVITAR BUCLES INFINITOS AL GENERAR SUMMARIES, SE PIERDE PRECISION Y PUEDE DAR COMO RESULTADO GRAMATICAS INCOMPLETAS O INCLUSO ERRONEAS
				if (workList.getDoneNodes().contains(currentNode) && EDGTraverserNew.getParent(initialNode).getData().getType() != NodeInfo.Type.Clause)
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
		final List<Work> workList = new LinkedList<Work>();
		final List<Node> routines = EDGTraverserNew.getNodes(this.edg, NodeInfo.Type.Routine);

		for (Node routine : routines)
		{
			final List<Node> clauses = EDGTraverserNew.getChildren(routine);
			
			for (Node clause : clauses)
			{
				// Summary Edges for the result node
				final Node clauseResult = EDGTraverserNew.getChild(clause, NodeInfo.Type.Result);
				workList.add(new NodeWork(clauseResult, clauseResult, new Constraints()));
				
				// Summary Edges for Reference variables (Global Variables)
				final Node clauseParameterOut = EDGTraverserNew.getChild(clause, NodeInfo.Type.ParameterOut);
				final List<Edge> globalVarDefinitions = EDGTraverserNew.getEdges(clauseParameterOut, Direction.Backwards, EdgeInfo.Type.Flow);
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
		final Node parent = EDGTraverserNew.getParent(node);
		final NodeInfo.Type nodeType = node.getData().getType();
		final NodeInfo.Type parentType = parent == null ? null : parent.getData().getType();
		final Node grandParent = parent == null ? null : EDGTraverserNew.getParent(parent);
		final NodeInfo.Type grandParentType = grandParent == null ? null : grandParent.getData().getType();
		if (parent == null)
			return false;
		if (nodeType != NodeInfo.Type.Parameters)
			if (nodeType != NodeInfo.Type.Result || parentType != NodeInfo.Type.Expression || grandParentType != NodeInfo.Type.Parameters)
				return false;

		// The formal in must be related to the formal out
		final Node parameters = EDGTraverserNew.getAncestor(node, NodeInfo.Type.Parameters);
		final Node clauseResult = EDGTraverserNew.getSibling(parameters, NodeInfo.Type.Result);
		
		final Node formalOutResult = EDGTraverserNew.getResult(EDGTraverserNew.getAncestor(formalOutNode, NodeInfo.Type.Clause));
		
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
			final List<Node> inputs = EDGTraverserNew.getInputs(formalIn, EDGTraverserNew.Direction.Backwards);
	
			for (Node input : inputs)
			{
				final Node call = EDGTraverserNew.getAncestor(input, NodeInfo.Type.Call);
				if (call == null)
					continue;
	
				final Node callResult = EDGTraverserNew.getSibling(call, NodeInfo.Type.Result);
				if (EDGTraverserNew.getParent(formalOut).getData().getType() != NodeInfo.Type.Clause) // PART FOR GLOBAL VARIABLE'S SUMMARIES
				{
					final List<Edge> GVOutEdges = EDGTraverserNew.getEdges(formalOut, Direction.Forwards, EdgeInfo.Type.Flow); 
					for (Edge GVOutEdge : GVOutEdges)
						if (GVOutEdge.getData().getConstraint() instanceof GlobalVariableConstraint)
						{
							final String varName = ((GlobalVariableConstraint) GVOutEdge.getData().getConstraint()).getVariableName(); 
							final GlobalVariableConstraint removeConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.Remove, varName);
							final Node callNode = EDGTraverserNew.getSibling(callResult, NodeInfo.Type.Value);
							final Node callArgOut = EDGTraverserNew.getChild(callNode, NodeInfo.Type.ArgumentOut);
							this.edg.addEdge(input, callArgOut, 0, new EdgeInfo(EdgeInfo.Type.Summary, removeConstraint));
						}
				}
				else // PART FOR FUNCTION RESULT'S SUMMARIES
					this.edg.addEdge(input, callResult, 0, new EdgeInfo(EdgeInfo.Type.Summary, grammarConstraint));
				
				nodesToContinue.add(callResult);
			}
		}
		else
		{
			final Node functionClauseNode = EDGTraverserNew.getParent(formalIn);
			final List<Node> inputs = EDGTraverserNew.getInputs(functionClauseNode, EDGTraverserNew.Direction.Backwards);
			
			for (Node input : inputs)
			{
				final Node call = EDGTraverserNew.getAncestor(input, NodeInfo.Type.Call);
				if (call == null)
					continue;
				
				final Node callResult = EDGTraverserNew.getSibling(call, 1);
				if (EDGTraverserNew.getParent(formalOut).getData().getType() != NodeInfo.Type.Clause) // PART FOR GLOBAL VARIABLE'S SUMMARIES
				{	final List<Edge> GVInEdges = EDGTraverserNew.getEdges(formalIn, Direction.Forwards, EdgeInfo.Type.Flow);
				
					for (Edge GVInEdge : GVInEdges)	
						if (GVInEdge.getData().getConstraint() instanceof GlobalVariableConstraint)
						{
							final String varName = ((GlobalVariableConstraint) GVInEdge.getData().getConstraint()).getVariableName(); 
							final GlobalVariableConstraint letThroughConstraint = new GlobalVariableConstraint(SeekingConstraint.Operation.LetThrough, varName);
							final Node callNode = EDGTraverserNew.getSibling(callResult, NodeInfo.Type.Value);
							final Node callArgOut = EDGTraverserNew.getChild(callNode, NodeInfo.Type.ArgumentOut);
							final Node callArgIn = EDGTraverserNew.getChild(callNode, NodeInfo.Type.ArgumentIn);
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