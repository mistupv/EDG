package upv.slicing.edg.edge;

import upv.slicing.edg.constraint.ExceptionConstraint;
import upv.slicing.edg.constraint.SeekingConstraint.Operation;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

import java.util.LinkedList;
import java.util.List;

public class ExceptionEdgeGenerator extends EdgeGenerator{

	final List<Node> exceptionGeneratedClauses = new LinkedList<>();
	
	public ExceptionEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		for (Node n : edg.vertexSet())
			if (n.getType() == Node.Type.ExHandler)
				this.generateExceptionEdges();
	}
	
	private void generateExceptionEdges()
	{
		final List<Node> exHandlerNodes = edg.getNodes(Node.Type.ExHandler);
		final Edge edge = new Edge(Edge.Type.Exception, new ExceptionConstraint(Operation.LetThrough));
		
		
		for (Node exHandlerNode : exHandlerNodes)
		{
			final Node tryNode = edg.getChild(exHandlerNode, 0);
			final List<Node> tryNodeExpressions = edg.getChildren(tryNode);
			
			final Node lastTryNodeResult = edg.getResult(tryNodeExpressions.get(tryNodeExpressions.size()-1));
			
			final Node catchNode = edg.getChild(exHandlerNode, 1);
			this.edg.addEdge(lastTryNodeResult, catchNode, new Edge(Edge.Type.Exception, new ExceptionConstraint(Operation.Add)));
			
			final List<Node> tryCalls = edg.getDescendants(tryNode, Node.Type.Call);
			for (Node tryCall : tryCalls)
			{
				generate(tryCall, edge);
			}
			
//			if (child.getType() != Node.Type.Return)
//				this.edg.addEdge(child, node, 0, new Edge(Edge.Type.Exception, new ExceptionConstraint(null))); // MISSING OPERATION
//			if (child.getType() == Node.Type.Call)
//			{
//				 final Node functionName = EDGTraverserNew.getChild(child, 0);
//				 final List<Node> functionClauses = this.getRelatedClauses(functionName);
//				 for (Node functionClause : functionClauses)
//				 {	 
//					 final Node exceptionReturn = EDGTraverserNew.getChild(child, EDGTraverserNew.getChildren(child).size() - 1);
//					 this.edg.addEdge(functionClause, exceptionReturn, 0, new Edge(Edge.Type.Output));
//					 this.generateExceptionEdges(functionClause);
//				 }
//			}
//			this.generateExceptionEdges(child);
		}
	}
	
	private void generateAssociatedFunctionStructure(Node node, Edge edge)
	{
		final Node callResultNode = edg.getSibling(node,1);
		final List<Node> calledFunctionResults = edg.getOutputs(callResultNode, LAST.Direction.Backwards);
		for (Node functionResult : calledFunctionResults)
		{
            if (!exceptionGeneratedClauses.contains(functionResult))
            {
                exceptionGeneratedClauses.add(functionResult);
                final Node functionClause = edg.getParent(functionResult);
                this.generate(functionClause, edge);
                this.edg.addEdge(functionResult, callResultNode,
						new Edge(Edge.Type.Output, new ExceptionConstraint(Operation.LetThrough)));
            }
        }
    }

    private enum Way {Forwards, Backwards}

    private List<Node> generate(Node node, Edge edge)
    {
        final Node.Type type = node.getType();
        final List<Node> children = edg.getChildren(node);

        if (type != Node.Type.Break && type != Node.Type.Return)
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
				return this.generateStructure(node, Way.Forwards, edge);
			
			case Call:
				return this.generateCallStructure(node, Way.Forwards, edge);
				
			case Equality:
			case Generator:
				return this.generateStructure(node, Way.Backwards, edge);

			case Module:
				return this.generateModuleStructure(node, edge);

			// CONDITIONALS
			case If:
				return this.generateIfStructure(node, edge);

			case Cases:
				return this.generateCasesStructure(node, edge);

			// LOOPS
			case FLoop:
				return this.generateFLoopStructure(node, edge);
			case CLoop:
				return this.generateCLoopStructure(node, edge);
			case RLoop:
				return this.generateRLoopStructure(node, edge);
				
			case ExHandler:
				return this.generateExHandlerStructure(node, edge);
			case Catch:
				
			case Break:
			case Return:
				return this.generateJumpStructure(node, edge);

			case Continue:
			case Try:
				return new LinkedList<>();

			case Root:
			default:
				throw new RuntimeException("Node type not contemplated: " + type);
		}
	}

	private List<Node> generateSingleNode(Node node)
	{
		final List<Node> nodes = new LinkedList<>();

		nodes.add(node);

		return nodes;
	}

	private List<Node> generateStructure(Node node, Way way, Edge edge)
	{
		final List<Node> children = edg.getChildren(node);
		final int beginIndex = way == Way.Forwards ? 0 : children.size() - 1;
		final int endIndex = way == Way.Forwards ? children.size() - 1 : 0;
		final int increment = way == Way.Forwards ? 1 : -1;
		final Node firstChild = children.get(beginIndex);
		final Node lastChild = children.get(endIndex);
		final List<Node> lastChildResultNodes = this.generate(lastChild, edge);

		this.edg.addEdge(node, firstChild, edge);
		for (int childIndex = beginIndex; childIndex != endIndex; childIndex += increment)
		{
			final Node child = children.get(childIndex);
			final Node nextChild = children.get(childIndex + increment);
			final List<Node> resultNodes = this.generate(child, edge);
			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, nextChild, edge);
		}

		return new LinkedList<>(lastChildResultNodes);
	}
	private List<Node> generateModuleStructure(Node node, Edge edge)
	{
		final List<Node> children = edg.getChildren(node);
		List<Node> resultNodes = new LinkedList<>();

		for (Node child : children)
		{
			final Node.Type type = child.getType();
			if (type == Node.Type.Routine)
				continue;

			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, child, edge);

			resultNodes = this.generate(child, edge);
		}

		return resultNodes;
	}
	private List<Node> generateIfStructure(Node node, Edge edge)
	{
		final List<Node> nodes = new LinkedList<>();
		final Node conditionChild = edg.getChild(node, 0);
		final Node thenChild = edg.getChild(node, 1);
		final Node elseChild = edg.getChild(node, 2);
		final List<Node> conditionResultNodes = this.generate(conditionChild, edge);
		final List<Node> thenResultNodes = this.generate(thenChild, edge);
		final List<Node> elseResultNodes = this.generate(elseChild, edge);

		this.edg.addEdge(node, conditionChild, edge);
		for (Node resultNode : conditionResultNodes)
		{				
			this.edg.addEdge(resultNode, thenChild, edge);
			this.edg.addEdge(resultNode, elseChild, edge);
		}
		nodes.addAll(thenResultNodes);
		nodes.addAll(elseResultNodes);

		return nodes;
	}
	private List<Node> generateCasesStructure(Node node, Edge edge)
	{
		final List<Node> nodes = new LinkedList<>();
		final List<Node> children = edg.getChildren(node);
		final List<Node> cases = new LinkedList<>(children);
		final List<Node> defaults = new LinkedList<>(children);
		cases.removeIf((c) -> c.getType() != Node.Type.Case);
		defaults.removeIf((c) -> c.getType() != Node.Type.DefaultCase);
		final Node defaultCase = defaults.isEmpty() ? null : defaults.get(0);
		if (defaultCase != null)
			cases.add(defaultCase);

		final Node firstCase = cases.isEmpty() ? defaultCase : cases.get(0);
		this.edg.addEdge(node, firstCase, edge);

		// Cases
		for (int caseIndex = 0; caseIndex < cases.size(); caseIndex++)
		{
			// Current case
			final Node _case = cases.get(caseIndex);
			final Node.Type caseType = _case.getType();
			final Node caseSelectors = caseType == Node.Type.DefaultCase ? null : edg.getChild(_case, 0);
			final List<Node> caseSelectorsChildren = caseType == Node.Type.DefaultCase ? new LinkedList<>() : edg.getChildren(caseSelectors);
			final Node caseGuard = caseType == Node.Type.DefaultCase ? null : edg.getChild(_case, 1);
			final List<Node> caseGuardChildren = caseType == Node.Type.DefaultCase ? new LinkedList<>() : edg.getChildren(caseGuard);
			final Node caseGuardChild = caseGuardChildren.isEmpty() ? null : caseGuardChildren.get(0);
			final Node caseGuardResult = caseGuardChild == null ? null : edg.getResult(caseGuardChild);
			final List<Node> resultNodes = this.generate(_case, edge);

			// Next case
			final Node nextCase = caseIndex == cases.size() - 1 ? defaultCase : cases.get(caseIndex + 1);
			if (nextCase != null)
			{
				if (!caseSelectorsChildren.isEmpty())
					this.edg.addEdge(caseGuard, nextCase, edge);

				if (caseGuardResult != null)
					this.edg.addEdge(caseGuardResult, nextCase, edge);
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
				final Node.Type nextChildType = nextChild.getType();
				final int bodyIndex = nextChildType == Node.Type.DefaultCase ? 0 : 2;
				final Node nextCaseBody = edg.getChild(nextChild, bodyIndex);
				for (Node resultNode : resultNodes)
					this.edg.addEdge(resultNode, nextCaseBody, edge);
			}
			else
				nodes.addAll(resultNodes);
		}

		return nodes;
	}

	private List<Node> generateFLoopStructure(Node node, Edge edge)
	{
		final Node initChild = edg.getChild(node, Node.Type.Init);
		final Node conditionChild = edg.getChild(node, Node.Type.Condition);
		final Node bodyChild = edg.getChild(node, Node.Type.Body);
		final Node updateChild = edg.getChild(node, Node.Type.Update);
		
		final List<Node> initResultNodes = this.generate(initChild, edge);
		final List<Node> conditionResultNodes = this.generate(conditionChild, edge);
		final List<Node> bodyResultNodes = this.generate(bodyChild, edge);
		final List<Node> updateResultNodes = this.generate(updateChild, edge);

		this.edg.addEdge(node, initChild, edge);
		for (Node resultNode : initResultNodes)
			this.edg.addEdge(resultNode, conditionChild, edge);

		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, edge);

		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, updateChild, edge);

		for (Node resultNode : updateResultNodes)
			this.edg.addEdge(resultNode, conditionChild, edge);

		return new LinkedList<>(conditionResultNodes);
	}
	private List<Node> generateCLoopStructure(Node node, Edge edge)
	{
		final Node conditionChild = edg.getChild(node, Node.Type.Condition);
		final Node bodyChild = edg.getChild(node, Node.Type.Body);
		final List<Node> conditionResultNodes = this.generate(conditionChild, edge);
		final List<Node> bodyResultNodes = this.generate(bodyChild, edge);

		this.edg.addEdge(node, conditionChild, edge);
		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, edge);
			
		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, conditionChild, edge);

		return new LinkedList<>(conditionResultNodes);
	}
	private List<Node> generateRLoopStructure(Node node, Edge edge)
	{
		final Node bodyChild = edg.getChild(node, Node.Type.Body);
		final Node conditionChild = edg.getChild(node, Node.Type.Condition);
		final List<Node> bodyResultNodes = this.generate(bodyChild, edge);
		final List<Node> conditionResultNodes = this.generate(conditionChild, edge);

		this.edg.addEdge(node, bodyChild, edge);
		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, conditionChild, edge);

		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, edge);

		return new LinkedList<>(conditionResultNodes);
	}
	
	private List<Node> generateExHandlerStructure(Node node, Edge edge)
	{
		final Node tryChild = edg.getChild(node, Node.Type.Try);
		final Node catchChild = edg.getChild(node, Node.Type.Catch);
		final Node finallyChild = edg.getChild(node, Node.Type.Finally);
		final List<Node> tryResultNodes = this.generate(tryChild,edge);
		final List<Node> catchResultNodes = this.generateCatchStructure(catchChild, edge);
		final List<Node> finallyResultNodes = this.generate(finallyChild,edge);

		this.edg.addEdge(node, tryChild, edge);
		
		for (Node resultNode : tryResultNodes)
			this.edg.addEdge(resultNode, finallyChild, edge);

		List<Node> catchClauses = edg.getChildren(catchChild);
		for (Node clause : catchClauses)
			this.edg.addEdge(catchChild, clause, edge);

		for (Node resultNode : catchResultNodes)
			this.edg.addEdge(resultNode, finallyChild, edge);

		return new LinkedList<>(finallyResultNodes);
	}
	private List<Node> generateCatchStructure(Node node, Edge edge)
	{
		List<Node> catchResultNodes = new LinkedList<>();
		List<Node> catchClauses = edg.getChildren(node);
		for (Node clause : catchClauses)
		{
			this.edg.addEdge(node, clause, edge);
			List<Node> resultNodes = this.generate(clause, edge);
			catchResultNodes.addAll(resultNodes);
		}
		return catchResultNodes;
	}
	private List<Node> generateCallStructure(Node node, Way way, Edge edge)
	{
		final List<Node> resultNodes = new LinkedList<>();
		
		if (edg.getAncestor(node, Node.Type.Try) == null)
			resultNodes.addAll(generateStructure(node,way,edge));

		generateAssociatedFunctionStructure(node, edge);
		
		return resultNodes;
	}
	
	private List<Node> generateJumpStructure(Node node, Edge edge)
	{
		final List<Node> nodes = new LinkedList<>();
		final List<Node> children = edg.getChildren(node);
		final Node child = children.isEmpty() ? null : children.get(0);
		final List<Node> resultNodes = child == null ? new LinkedList<>() : this.generate(child, edge);
		final String dstText = node.getName();
		final int dstId = Integer.parseInt(dstText.substring(dstText.lastIndexOf(" ") + 1));
		final Node dstNode = this.edg.getNode(dstId);
		final Node dstResult = edg.getResult(dstNode);

		if (child != null)
			this.edg.addEdge(node, child, edge);
		else
			resultNodes.add(node);
		if (dstResult != null)
			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, dstResult, edge);

		return nodes;
	}
}
