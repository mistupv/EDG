package edg.edge;

import java.util.LinkedList;
import java.util.List;

import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.traverser.EDGTraverser;

public class ControlFlowEdgeGenerator extends EdgeGenerator
{
	private static enum Way { Forwards, Backwards }

	private final EdgeInfo controlFlowEdgeInfo = new EdgeInfo(EdgeInfo.Type.ControlFlow);

	public ControlFlowEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		final List<Node> modules = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Module);
		for (Node module : modules)
			this.generate(module);

		final List<Node> clauses = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Clause);
		for (Node clause : clauses)
			this.generate(clause);
	}
	private List<Node> generate(Node node)
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
			case Call:
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
			case Try:
			case CatchClause:
			case Finally:
				return this.generateStructure(node, Way.Forwards);

			case Equality:
			case Generator:
				return this.generateStructure(node, Way.Backwards);

			case Module:
				return this.generateModuleStructure(node);

			// CONDITIONALS
			case If:
				return this.generateIfStructure(node);

			case Cases:
				return this.generateCasesStructure(node);

			// LOOPS
			case FLoop:
				return this.generateFLoopStructure(node);
			case CLoop:
				return this.generateCLoopStructure(node);
			case RLoop:
				return this.generateRLoopStructure(node);
				
			case ExHandler:
				return this.generateExHandlerStructure(node);
			case Catch:
				
			case Break:
			case Return:
				return this.generateJumpStructure(node);

			case Continue:
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
	private List<Node> generateStructure(Node node, Way way)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> children = EDGTraverser.getChildren(node);
		final int beginIndex = way == Way.Forwards ? 0 : children.size() - 1;
		final int endIndex = way == Way.Forwards ? children.size() - 1 : 0;
		final int increment = way == Way.Forwards ? 1 : -1;
		final Node firstChild = children.get(beginIndex);
		final Node lastChild = children.get(endIndex);
		final List<Node> lastChildResultNodes = this.generate(lastChild);

		this.edg.addEdge(node, firstChild, 0, this.controlFlowEdgeInfo);
		for (int childIndex = beginIndex; childIndex != endIndex; childIndex += increment)
		{
			final Node child = children.get(childIndex);
			final Node nextChild = children.get(childIndex + increment);
			final List<Node> resultNodes = this.generate(child);
			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, nextChild, 0, this.controlFlowEdgeInfo);
		}
		nodes.addAll(lastChildResultNodes);

		return nodes;
	}
	private List<Node> generateModuleStructure(Node node)
	{
		final List<Node> children = EDGTraverser.getChildren(node);
		List<Node> resultNodes = new LinkedList<Node>();

		for (Node child : children)
		{
			final NodeInfo.Type type = child.getData().getType();
			if (type == NodeInfo.Type.Routine)
				continue;

			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, child, 0, this.controlFlowEdgeInfo);
			resultNodes = this.generate(child);
		}

		return resultNodes;
	}
	private List<Node> generateIfStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node conditionChild = EDGTraverser.getChild(node, 0);
		final Node thenChild = EDGTraverser.getChild(node, 1);
		final Node elseChild = EDGTraverser.getChild(node, 2);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> thenResultNodes = this.generate(thenChild);
		final List<Node> elseResultNodes = this.generate(elseChild);

		this.edg.addEdge(node, conditionChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : conditionResultNodes)
		{
			this.edg.addEdge(resultNode, thenChild, 0, this.controlFlowEdgeInfo);
			this.edg.addEdge(resultNode, elseChild, 0, this.controlFlowEdgeInfo);
		}
		nodes.addAll(thenResultNodes);
		nodes.addAll(elseResultNodes);

		return nodes;
	}
	private List<Node> generateCasesStructure(Node node)
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
		this.edg.addEdge(node, firstCase, 0, this.controlFlowEdgeInfo);

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
			final List<Node> resultNodes = this.generate(_case);

			// Next case
			final Node nextCase = caseIndex == cases.size() - 1 ? defaultCase : cases.get(caseIndex + 1);
			if (nextCase != null)
			{
				if (!caseSelectorsChildren.isEmpty())
					this.edg.addEdge(caseGuard, nextCase, 0, this.controlFlowEdgeInfo);
				if (caseGuardResult != null)
					this.edg.addEdge(caseGuardResult, nextCase, 0, this.controlFlowEdgeInfo);
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
					this.edg.addEdge(resultNode, nextCaseBody, 0, this.controlFlowEdgeInfo);
			}
			else
				nodes.addAll(resultNodes);
		}

		return nodes;
	}
//	private List<Node> generateLoopStructure(Node node)
//	{
//		final List<Node> nodes = new LinkedList<Node>();
//		final Node conditionChild = EDGTraverser.getChild(node, NodeInfo.Type.Condition);
//		final Node bodyChild = EDGTraverser.getChild(node, NodeInfo.Type.Body);
//		final List<Node> conditionResultNodes = this.generate(conditionChild);
//		final List<Node> bodyResultNodes = this.generate(bodyChild);
//
//		this.edg.addEdge(node, conditionChild, 0, this.controlFlowEdgeInfo);
//		for (Node resultNode : conditionResultNodes)
//			this.edg.addEdge(resultNode, bodyChild, 0, this.controlFlowEdgeInfo);
//		for (Node resultNode : bodyResultNodes)
//			this.edg.addEdge(resultNode, conditionChild, 0, this.controlFlowEdgeInfo);
//		nodes.addAll(conditionResultNodes);
//
//		return nodes;
//	}
//ADDED 2 different type of loops
	private List<Node> generateFLoopStructure(Node node)
	{
		final Node initChild = EDGTraverser.getChild(node, NodeInfo.Type.Init);
		final Node conditionChild = EDGTraverser.getChild(node, NodeInfo.Type.Condition);
		final Node bodyChild = EDGTraverser.getChild(node, NodeInfo.Type.Body);
		final Node updateChild = EDGTraverser.getChild(node, NodeInfo.Type.Update);
		
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> initResultNodes = this.generate(initChild);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> bodyResultNodes = this.generate(bodyChild);
		final List<Node> updateResultNodes = this.generate(updateChild);

		this.edg.addEdge(node, initChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : initResultNodes)
			this.edg.addEdge(resultNode, conditionChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, updateChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : updateResultNodes)
			this.edg.addEdge(resultNode, conditionChild, 0, this.controlFlowEdgeInfo);
		nodes.addAll(conditionResultNodes);
		
		return nodes;
	}
	private List<Node> generateCLoopStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node conditionChild = EDGTraverser.getChild(node, NodeInfo.Type.Condition);
		final Node bodyChild = EDGTraverser.getChild(node, NodeInfo.Type.Body);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> bodyResultNodes = this.generate(bodyChild);

		this.edg.addEdge(node, conditionChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, conditionChild, 0, this.controlFlowEdgeInfo);
		nodes.addAll(conditionResultNodes);

		return nodes;
	}
	private List<Node> generateRLoopStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node bodyChild = EDGTraverser.getChild(node, NodeInfo.Type.Body);
		final Node conditionChild = EDGTraverser.getChild(node, NodeInfo.Type.Condition);
		final List<Node> bodyResultNodes = this.generate(bodyChild);
		final List<Node> conditionResultNodes = this.generate(conditionChild);

		this.edg.addEdge(node, bodyChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : bodyResultNodes)
			this.edg.addEdge(resultNode, conditionChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : conditionResultNodes)
			this.edg.addEdge(resultNode, bodyChild, 0, this.controlFlowEdgeInfo);
		nodes.addAll(conditionResultNodes);

		return nodes;
	}
	
	private List<Node> generateExHandlerStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node tryChild = EDGTraverser.getChild(node, NodeInfo.Type.Try);
		final Node catchChild = EDGTraverser.getChild(node, NodeInfo.Type.Catch);
		final Node finallyChild = EDGTraverser.getChild(node, NodeInfo.Type.Finally);
		final List<Node> tryResultNodes = this.generate(tryChild);
		final List<Node> catchResultNodes = this.generateCatchStructure(catchChild);
		final List<Node> finallyResultNodes = this.generate(finallyChild);

		this.edg.addEdge(node, tryChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : tryResultNodes)
		{
			this.edg.addEdge(resultNode, catchChild, 0, this.controlFlowEdgeInfo);
			this.edg.addEdge(resultNode, finallyChild, 0, this.controlFlowEdgeInfo);
		}
		List<Node> catchClauses = EDGTraverser.getChildren(catchChild);
		for (Node clause : catchClauses)
			this.edg.addEdge(catchChild, clause, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : catchResultNodes)
			this.edg.addEdge(resultNode, finallyChild, 0, this.controlFlowEdgeInfo);
		nodes.addAll(finallyResultNodes);

		return nodes;
	}
	
	private List<Node> generateCatchStructure(Node node)
	{
		List<Node> catchResultNodes = new LinkedList<Node>();
		List<Node> catchClauses = EDGTraverser.getChildren(node);
		for (Node clause : catchClauses)
		{
			this.edg.addEdge(node, clause, 0, this.controlFlowEdgeInfo);
			List<Node> resultNodes = this.generate(clause);
			catchResultNodes.addAll(resultNodes);
		}
		return catchResultNodes;
	}
	
	private List<Node> generateJumpStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> children = EDGTraverser.getChildren(node);
		final Node child = children.isEmpty() ? null : children.get(0);
		final List<Node> resultNodes = child == null ? new LinkedList<Node>() : this.generate(child);
		final String dstText = node.getData().getName();
		final int dstId = Integer.parseInt(dstText.substring(dstText.lastIndexOf(" ") + 1));
		final Node dstNode = EDGTraverser.getNode(this.edg, dstId);
		final Node dstResult = EDGTraverser.getResult(dstNode);

		if (child != null)
			this.edg.addEdge(node, child, 0, this.controlFlowEdgeInfo);
		else
			resultNodes.add(node);
		if (dstResult != null)
			for (Node resultNode : resultNodes)
				this.edg.addEdge(resultNode, dstResult, 0, this.controlFlowEdgeInfo);

		return nodes;
	}
}