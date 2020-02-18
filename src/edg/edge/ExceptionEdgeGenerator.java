package edg.edge;

import java.util.LinkedList;
import java.util.List;

import edg.graph.EDG;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.EdgeInfo;
import edg.constraint.ExceptionConstraint;
import edg.constraint.SeekingConstraint.Operation;
import edg.traverser.EDGTraverser;
import edg.traverser.EDGTraverser.Direction;

public class ExceptionEdgeGenerator extends EdgeGenerator{

	final List<Node> exceptionGeneratedClauses= new LinkedList<Node>();
	
	public ExceptionEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		final List<Node> nodes = this.edg.getNodes();
		boolean exception = false;
		for (int index = 0; index < nodes.size() && !exception; index++)
		{
			final NodeInfo.Type nodeType = nodes.get(index).getData().getType();
			if (nodeType == NodeInfo.Type.ExHandler)
				exception = true;
		}
		if (exception)
			this.generateExceptionEdges();

	}
	
	private void generateExceptionEdges()
	{
		final List<Node> exHandlerNodes = EDGTraverser.getNodes(edg, NodeInfo.Type.ExHandler);
		final EdgeInfo info = new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(Operation.LetThrough));
		
		
		for (Node exHandlerNode : exHandlerNodes)
		{
			final Node tryNode = EDGTraverser.getChild(exHandlerNode, 0);
			final List<Node> tryNodeExpressions = EDGTraverser.getChildren(tryNode);
			
			final Node lastTryNodeResult = EDGTraverser.getResult(tryNodeExpressions.get(tryNodeExpressions.size()-1));
			
			final Node catchNode = EDGTraverser.getChild(exHandlerNode, 1);
			this.edg.addEdge(lastTryNodeResult, catchNode, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(Operation.Add)));
			
			final List<Node> tryCalls = EDGTraverser.getDescendants(tryNode,NodeInfo.Type.Call);
			for (Node tryCall : tryCalls)
			{
				generate(tryCall, info);
			}
			
//			if (child.getData().getType() != NodeInfo.Type.Return)
//				this.edg.addEdge(child, node, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null))); // MISSING OPERATION
//			if (child.getData().getType() == NodeInfo.Type.Call)
//			{
//				 final Node functionName = EDGTraverser.getChild(child, 0);
//				 final List<Node> functionClauses = this.getRelatedClauses(functionName);
//				 for (Node functionClause : functionClauses)
//				 {	 
//					 final Node exceptionReturn = EDGTraverser.getChild(child, EDGTraverser.getChildren(child).size() - 1);
//					 this.edg.addEdge(functionClause, exceptionReturn, 0, new EdgeInfo(EdgeInfo.Type.Output));
//					 this.generateExceptionEdges(functionClause);
//				 }
//			}
//			this.generateExceptionEdges(child);
		}
	}
	
	private void generateAssociatedFunctionStructure(Node node, EdgeInfo info)
	{
		final Node callResultNode = EDGTraverser.getSibling(node,1);
		final List<Node> calledFunctionResults = EDGTraverser.getOutputs(callResultNode, Direction.Backwards);
		for (Node functionResult : calledFunctionResults)
		{
			if (!exceptionGeneratedClauses.contains(functionResult))
			{
				exceptionGeneratedClauses.add(functionResult);
				final Node functionClause = EDGTraverser.getParent(functionResult);
				this.generate(functionClause, info);
				this.edg.addEdge(functionResult, callResultNode, 0, new EdgeInfo(EdgeInfo.Type.Output, new ExceptionConstraint(Operation.LetThrough)));
			}
		}
	}
	
	private static enum Way { Forwards, Backwards }
	
	private List<Node> generate(Node node, EdgeInfo info)
	{
		final NodeInfo.Type type = node.getData().getType();
		final List<Node> children = EDGTraverser.getChildren(node);

		if (type != NodeInfo.Type.Break && type != NodeInfo.Type.Return)
			if (children.isEmpty())
				return this.generateSingleNode(node);

		switch (type)
		{
			// This node
			case Variable:
			case Literal:
			case Result:
			case Routine:
				return this.generateSingleNode(node);

			// All children (left to right)
			case Clause:
			case Parameters:
			case Guard:
			case Body:
			case Expression:
			case Callee:
			case Scope:
			case Name:
			case Arguments:
			case Operation:
			case Switch:
			case Case:
			case DefaultCase:
			case Selector:
			case Selectable:
			case DataConstructorAccess:
			case FieldAccess:
			case List:
			case DataConstructor:
			case Block:
			case Condition:
			case ListComprehension:
			case Restrictions:
			case Filter:
			case Value:
			case Init:
			case Update:
			case TypeCheck:	//ADDED
			case TypeTransformation: //ADDED
			// EXCEPTIONS
			//case Try:
			case CatchClause:
			case Finally:
			case Throw:
			case Foreach:
				return this.generateStructure(node, Way.Forwards, info);
			
			case Call:
				return this.generateCallStructure(node, Way.Forwards, info);
				
			case Equality:
			case Generator:
				return this.generateStructure(node, Way.Backwards, info);

			case Module:
				return this.generateModuleStructure(node, info);

			// CONDITIONALS
			case If:
				return this.generateIfStructure(node, info);

			case Cases:
				return this.generateCasesStructure(node, info);

			// LOOPS
			case FLoop:
				return this.generateFLoopStructure(node, info);
			case CLoop:
				return this.generateCLoopStructure(node, info);
			case RLoop:
				return this.generateRLoopStructure(node, info);
				
			case ExHandler:
				return this.generateExHandlerStructure(node, info);
			case Catch:
				
			case Break:
			case Return:
				return this.generateJumpStructure(node, info);

			case Continue:
			case Try:
				return new LinkedList<Node>();

			case Root:
			default:
				throw new RuntimeException("Node type not contemplated: " + type);
		}
	}

	private List<Node> generateSingleNode(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();

		nodes.add(node);

		return nodes;
	}

	private List<Node> generateStructure(Node node, Way way, EdgeInfo info)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> children = EDGTraverser.getChildren(node);
		final int beginIndex = way == Way.Forwards ? 0 : children.size() - 1;
		final int endIndex = way == Way.Forwards ? children.size() - 1 : 0;
		final int increment = way == Way.Forwards ? 1 : -1;
		final Node firstChild = children.get(beginIndex);
		final Node lastChild = children.get(endIndex);
		final List<Node> lastChildResultNodes = this.generate(lastChild, info);

		this.edg.addEdge(node, firstChild, 0, info);
		for (int childIndex = beginIndex; childIndex != endIndex; childIndex += increment)
		{
			final Node child = children.get(childIndex);
			final Node nextChild = children.get(childIndex + increment);
			final List<Node> resultNodes = this.generate(child, info);
			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, nextChild, 0, info);
		}
		nodes.addAll(lastChildResultNodes);

		return nodes;
	}
	private List<Node> generateModuleStructure(Node node, EdgeInfo info)
	{
		final List<Node> children = EDGTraverser.getChildren(node);
		List<Node> resultNodes = new LinkedList<Node>();

		for (Node child : children)
		{
			final NodeInfo.Type type = child.getData().getType();
			if (type == NodeInfo.Type.Routine)
				continue;

			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, child, 0, info);

			resultNodes = this.generate(child, info);
		}

		return resultNodes;
	}
	private List<Node> generateIfStructure(Node node, EdgeInfo info)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node conditionChild = EDGTraverser.getChild(node, 0);
		final Node thenChild = EDGTraverser.getChild(node, 1);
		final Node elseChild = EDGTraverser.getChild(node, 2);
		final List<Node> conditionResultNodes = this.generate(conditionChild, info);
		final List<Node> thenResultNodes = this.generate(thenChild, info);
		final List<Node> elseResultNodes = this.generate(elseChild, info);

		this.edg.addEdge(node, conditionChild, 0, info);
		for (Node resultNode : conditionResultNodes)
		{				
			this.edg.addEdge(resultNode, thenChild, 0, info);
			this.edg.addEdge(resultNode, elseChild, 0, info);
		}
		nodes.addAll(thenResultNodes);
		nodes.addAll(elseResultNodes);

		return nodes;
	}
	private List<Node> generateCasesStructure(Node node, EdgeInfo info)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> children = EDGTraverser.getChildren(node);
		final List<Node> cases = new LinkedList<Node>(children);
		final List<Node> defaults = new LinkedList<Node>(children);
		cases.removeIf((c) -> c.getData().getType() != NodeInfo.Type.Case);
		defaults.removeIf((c) -> c.getData().getType() != NodeInfo.Type.DefaultCase);
		final Node defaultCase = defaults.isEmpty() ? null : defaults.get(0);
		if (defaultCase != null)
			cases.add(defaultCase);

		final Node firstCase = cases.isEmpty() ? defaultCase : cases.get(0);
		this.edg.addEdge(node, firstCase, 0, info);

		// Cases
		for (int caseIndex = 0; caseIndex < cases.size(); caseIndex++)
		{
			// Current case
			final Node _case = cases.get(caseIndex);
			final NodeInfo.Type caseType = _case.getData().getType();
			final Node caseSelectors = caseType == NodeInfo.Type.DefaultCase ? null : EDGTraverser.getChild(_case, 0);
			final List<Node> caseSelectorsChildren = caseType == NodeInfo.Type.DefaultCase ? new LinkedList<Node>() : EDGTraverser.getChildren(caseSelectors);
			final Node caseGuard = caseType == NodeInfo.Type.DefaultCase ? null : EDGTraverser.getChild(_case, 1);
			final List<Node> caseGuardChildren = caseType == NodeInfo.Type.DefaultCase ? new LinkedList<Node>() : EDGTraverser.getChildren(caseGuard);
			final Node caseGuardChild = caseGuardChildren.isEmpty() ? null : caseGuardChildren.get(0);
			final Node caseGuardResult = caseGuardChild == null ? null : EDGTraverser.getResult(caseGuardChild);
			final List<Node> resultNodes = this.generate(_case, info);

			// Next case
			final Node nextCase = caseIndex == cases.size() - 1 ? defaultCase : cases.get(caseIndex + 1);
			if (nextCase != null)
			{
				if (!caseSelectorsChildren.isEmpty())
					this.edg.addEdge(caseGuard, nextCase, 0, info);

				if (caseGuardResult != null)
					this.edg.addEdge(caseGuardResult, nextCase, 0, info);
			}
			else
			{
				if (!caseSelectorsChildren.isEmpty())
					nodes.add(caseGuard);
				if (caseGuardResult != null)
					nodes.add(caseGuardResult);
			}

			// Next child
			final int childIndex = children.indexOf(_case);
			final Node nextChild = childIndex == children.size() - 1 ? null : children.get(childIndex + 1);
			if (nextChild != null)
			{
				final NodeInfo.Type nextChildType = nextChild.getData().getType();
				final int bodyIndex = nextChildType == NodeInfo.Type.DefaultCase ? 0 : 2;
				final Node nextCaseBody = EDGTraverser.getChild(nextChild, bodyIndex);
				for (Node resultNode : resultNodes)
					this.edg.addEdge(resultNode, nextCaseBody, 0, info);
			}
			else
				nodes.addAll(resultNodes);
		}

		return nodes;
	}

	private List<Node> generateFLoopStructure(Node node, EdgeInfo info)
	{
		final Node initChild = EDGTraverser.getChild(node, NodeInfo.Type.Init);
		final Node conditionChild = EDGTraverser.getChild(node, NodeInfo.Type.Condition);
		final Node bodyChild = EDGTraverser.getChild(node, NodeInfo.Type.Body);
		final Node updateChild = EDGTraverser.getChild(node, NodeInfo.Type.Update);
		
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> initResultNodes = this.generate(initChild, info);
		final List<Node> conditionResultNodes = this.generate(conditionChild, info);
		final List<Node> bodyResultNodes = this.generate(bodyChild, info);
		final List<Node> updateResultNodes = this.generate(updateChild, info);

		this.edg.addEdge(node, initChild, 0, info);
		for (Node resultNode : initResultNodes)
			this.edg.addEdge(resultNode, conditionChild, 0, info);

		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, 0, info);

		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, updateChild, 0, info);

		for (Node resultNode : updateResultNodes)
			this.edg.addEdge(resultNode, conditionChild, 0, info);

		nodes.addAll(conditionResultNodes);
		
		return nodes;
	}
	private List<Node> generateCLoopStructure(Node node, EdgeInfo info)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node conditionChild = EDGTraverser.getChild(node, NodeInfo.Type.Condition);
		final Node bodyChild = EDGTraverser.getChild(node, NodeInfo.Type.Body);
		final List<Node> conditionResultNodes = this.generate(conditionChild, info);
		final List<Node> bodyResultNodes = this.generate(bodyChild, info);

		this.edg.addEdge(node, conditionChild, 0, info);
		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, 0, info);
			
		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, conditionChild, 0, info);

		nodes.addAll(conditionResultNodes);

		return nodes;
	}
	private List<Node> generateRLoopStructure(Node node, EdgeInfo info)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node bodyChild = EDGTraverser.getChild(node, NodeInfo.Type.Body);
		final Node conditionChild = EDGTraverser.getChild(node, NodeInfo.Type.Condition);
		final List<Node> bodyResultNodes = this.generate(bodyChild, info);
		final List<Node> conditionResultNodes = this.generate(conditionChild, info);

		this.edg.addEdge(node, bodyChild, 0, info);
		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, conditionChild, 0, info);

		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, 0, info);

		nodes.addAll(conditionResultNodes);

		return nodes;
	}
	
	private List<Node> generateExHandlerStructure(Node node, EdgeInfo info)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node tryChild = EDGTraverser.getChild(node, NodeInfo.Type.Try);
		final Node catchChild = EDGTraverser.getChild(node, NodeInfo.Type.Catch);
		final Node finallyChild = EDGTraverser.getChild(node, NodeInfo.Type.Finally);
		final List<Node> tryResultNodes = this.generate(tryChild,info);
		final List<Node> catchResultNodes = this.generateCatchStructure(catchChild, info);
		final List<Node> finallyResultNodes = this.generate(finallyChild,info);

		this.edg.addEdge(node, tryChild, 0, info);
		
		for (Node resultNode : tryResultNodes)
			this.edg.addEdge(resultNode, finallyChild, 0, info);

		List<Node> catchClauses = EDGTraverser.getChildren(catchChild);
		for (Node clause : catchClauses)
			this.edg.addEdge(catchChild, clause, 0, info);

		for (Node resultNode : catchResultNodes)
			this.edg.addEdge(resultNode, finallyChild, 0, info);

		nodes.addAll(finallyResultNodes);

		return nodes;
	}
	private List<Node> generateCatchStructure(Node node, EdgeInfo info)
	{
		List<Node> catchResultNodes = new LinkedList<Node>();
		List<Node> catchClauses = EDGTraverser.getChildren(node);
		for (Node clause : catchClauses)
		{
			this.edg.addEdge(node, clause, 0, info);
			List<Node> resultNodes = this.generate(clause, info);
			catchResultNodes.addAll(resultNodes);
		}
		return catchResultNodes;
	}
	private List<Node> generateCallStructure(Node node, Way way, EdgeInfo info)
	{
		final List<Node> resultNodes = new LinkedList<Node>();
		
		if (EDGTraverser.getAncestor(node, NodeInfo.Type.Try) == null)
			resultNodes.addAll(generateStructure(node,way,info));

		generateAssociatedFunctionStructure(node, info);
		
		return resultNodes;
	}
	
	private List<Node> generateJumpStructure(Node node, EdgeInfo info)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> children = EDGTraverser.getChildren(node);
		final Node child = children.isEmpty() ? null : children.get(0);
		final List<Node> resultNodes = child == null ? new LinkedList<Node>() : this.generate(child, info);
		final String dstText = node.getData().getName();
		final int dstId = Integer.parseInt(dstText.substring(dstText.lastIndexOf(" ") + 1));
		final Node dstNode = EDGTraverser.getNode(this.edg, dstId);
		final Node dstResult = EDGTraverser.getResult(dstNode);

		if (child != null)
			this.edg.addEdge(node, child, 0, info);
		else
			resultNodes.add(node);
		if (dstResult != null)
			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, dstResult, 0, info);

		return nodes;
	}
}
