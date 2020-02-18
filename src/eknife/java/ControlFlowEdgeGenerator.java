package eknife.java;

import java.util.LinkedList;
import java.util.List;

import edg.graph.EdgeInfo;
import edg.graph.LAST;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.traverser.LASTTraverser;

public class ControlFlowEdgeGenerator
{
	private static enum Way { Forwards, Backwards }

	private final EdgeInfo controlFlowEdgeInfo = new EdgeInfo(EdgeInfo.Type.ControlFlow);	
	protected final LAST last;
	
	public ControlFlowEdgeGenerator(LAST last)
	{
		this.last = last;
	}

	public void generate()
	{
		final List<Node> modules = LASTTraverser.getNodes(this.last, NodeInfo.Type.Module);
		for (Node module : modules)
			this.generate(module);

		final List<Node> clauses = LASTTraverser.getNodes(this.last, NodeInfo.Type.Clause);
		for (Node clause : clauses)
			this.generate(clause);
	}
	private List<Node> generate(Node node)
	{
		final NodeInfo.Type type = node.getData().getType();
		final List<Node> children = LASTTraverser.getChildren(node);

		if (type != NodeInfo.Type.Break && type != NodeInfo.Type.Return && type != NodeInfo.Type.Continue)
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
			case ParameterIn:
			case ParameterOut:
			case Guard:
			case Body:
			case Expression:
			case Call:
			case Callee:
			case Scope:
			case Name:
			case Arguments:
			case ArgumentIn:
			case ArgumentOut:
			case Operation:
			case Switch:
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
			case CatchClause:
			case Finally:
			case Throw:
			case Label:
			case Iterator:
			case FieldAccess:
			case Try:
				return this.generateStructure(node, Way.Forwards);
				
//			case Try:
//				this.isTryContext = true;
//				final List<Node> result = this.generateStructure(node, Way.Forwards);
//				this.isTryContext = false;
//				return result;
				
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
			case Case:
				return this.generateCaseStructure(node);

			// LOOPS
			case CLoop:
				return this.generateCLoopStructure(node);
			case FLoop:
				return this.generateFLoopStructure(node);
			case RLoop:
				return this.generateRLoopStructure(node);
			case Foreach:
				return this.generateForeachStructure(node);
			// EXCEPTIONS	
			case ExHandler:
				return this.generateExHandlerStructure(node);
			case Catch:
			
			// JUMPS
			case Break:
			case Return:
			case Continue:
				return this.generateJumpStructure(node);

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
		final List<Node> children = LASTTraverser.getChildren(node);
		final int beginIndex = way == Way.Forwards ? 0 : children.size() - 1;
		final int endIndex = way == Way.Forwards ? children.size() - 1 : 0;
		final int increment = way == Way.Forwards ? 1 : -1;
		final Node firstChild = children.get(beginIndex);
		final List<Node> firstChildResultNodes = this.generate(firstChild);
		
		for (int childIndex = beginIndex; childIndex != endIndex; childIndex += increment)
		{
			final Node child = children.get(childIndex);
			final Node nextChild = children.get(childIndex + increment);
			final List<Node> resultNodes = this.generate(nextChild); 
			// if (nextChild.getData().getType() != NodeInfo.Type.Result) // ¿Why is this "if" here?
			for (Node resultNode : resultNodes)
				this.last.addEdge(child, resultNode, 0, this.controlFlowEdgeInfo);
		}
		
		final Node lastChild = children.get(endIndex);
		NodeInfo.Type lastChildType = lastChild.getData().getType();
		if (lastChildType != NodeInfo.Type.Break && lastChildType != NodeInfo.Type.Continue && lastChildType != NodeInfo.Type.Return)
			this.last.addEdge(lastChild, node, 0, this.controlFlowEdgeInfo);
		
		return firstChildResultNodes;
	}
	private List<Node> generateModuleStructure(Node node)
	{
		final List<Node> children = LASTTraverser.getChildren(node);
		List<Node> resultNodes = new LinkedList<Node>();

		for (Node child : children)
		{
			final NodeInfo.Type type = child.getData().getType();
			if (type == NodeInfo.Type.Routine)
				continue;

			final List<Node> childResults = this.generate(child);
			
			for (Node resultNode : resultNodes)
				for (Node childResult : childResults)
					this.last.addEdge(resultNode, childResult, 0, this.controlFlowEdgeInfo);

			resultNodes.clear();
			resultNodes.add(child);
		}

		return resultNodes;
	}
	private List<Node> generateIfStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node conditionChild = LASTTraverser.getChild(node, 0);
		final Node thenChild = LASTTraverser.getChild(node, 1);
		final Node elseChild = LASTTraverser.getChild(node, 2);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> thenResultNodes = this.generate(thenChild);
		final List<Node> elseResultNodes = this.generate(elseChild);

		// Condition -> Then First Node
		for (Node resultNode : thenResultNodes)		
			this.last.addEdge(conditionChild, resultNode, 0, this.controlFlowEdgeInfo);
		// Condition -> Else First Node
		for (Node resultNode : elseResultNodes)		
			this.last.addEdge(conditionChild, resultNode, 0, this.controlFlowEdgeInfo);
		
		this.last.addEdge(thenChild, node, 0, this.controlFlowEdgeInfo);
		this.last.addEdge(elseChild, node, 0, this.controlFlowEdgeInfo);
		
		nodes.addAll(conditionResultNodes);

		return nodes;
	}
	private List<Node> generateCasesStructure(Node node)
	{
		
		final List<Node> selectorDestinationNodes = new LinkedList<Node>();
	
		final List<Node> children = LASTTraverser.getChildren(node);
		final List<Node> cases = new LinkedList<Node>(children);
		final List<Node> defaults = new LinkedList<Node>(children);
		cases.removeIf((c) -> c.getData().getType() != NodeInfo.Type.Case);
		defaults.removeIf((c) -> c.getData().getType() != NodeInfo.Type.DefaultCase);
		
		// Put the default at the end of the treated cases
		final Node defaultCase = defaults.isEmpty() ? null : defaults.get(0);
		if (defaultCase != null)
			cases.add(defaultCase);

		final Node lastCase = cases.isEmpty() ? defaultCase : cases.get(cases.size() - 1);
		this.last.addEdge(lastCase, node, 0, this.controlFlowEdgeInfo);
		
		Node previousCase = null;

		// Cases
		for (int caseIndex = 0; caseIndex < cases.size(); caseIndex++)
		{
			// Current case
			final Node _case = cases.get(caseIndex);
			final List<Node> caseResults = generate(_case);
			
			selectorDestinationNodes.addAll(caseResults);
			if(previousCase != null)
				for (Node caseResult : caseResults)
					this.last.addEdge(previousCase, caseResult, 0, this.controlFlowEdgeInfo);
			previousCase = _case;	
		}

		return selectorDestinationNodes;
	}
	private List<Node> generateCaseStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> children = LASTTraverser.getChildren(node);
		final Node firstChild = children.get(0);
		this.generate(firstChild);
		
		final Node lastChild = children.get(children.size() - 1);
		
		for (int childIndex = 0; childIndex != children.size() - 1; childIndex++)
		{
			final Node child = children.get(childIndex);
			final Node nextChild = children.get(childIndex + 1);
			final List<Node> resultNodes = this.generate(nextChild); 
			if (nextChild == lastChild)
				nodes.addAll(resultNodes);
			for (Node resultNode : resultNodes)
				this.last.addEdge(child, resultNode, 0, this.controlFlowEdgeInfo);
		}
		
		NodeInfo.Type lastChildType = lastChild.getData().getType();
		if (lastChildType != NodeInfo.Type.Break && lastChildType != NodeInfo.Type.Continue && lastChildType != NodeInfo.Type.Return)
			this.last.addEdge(lastChild, node, 0, this.controlFlowEdgeInfo);
		
		return nodes;
	}
	
	private List<Node> generateForeachStructure(Node node)
	{
		final Node iteratorChild = LASTTraverser.getChild(node, NodeInfo.Type.Iterator);
		final Node bodyChild = LASTTraverser.getChild(node, NodeInfo.Type.Body);
		
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> iteratorResultNodes = this.generate(iteratorChild);
		final List<Node> bodyResultNodes = this.generate(bodyChild);

		this.last.addEdge(node, iteratorChild, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : iteratorResultNodes)
			this.last.addEdge(resultNode, bodyChild, 0, this.controlFlowEdgeInfo);

		for (Node resultNode : bodyResultNodes)
			this.last.addEdge(resultNode, iteratorChild, 0, this.controlFlowEdgeInfo);

		nodes.addAll(iteratorResultNodes);
		
		return nodes;
	}
	
	private List<Node> generateCLoopStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node conditionChild = LASTTraverser.getChild(node, NodeInfo.Type.Condition);
		final Node bodyChild = LASTTraverser.getChild(node, NodeInfo.Type.Body);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> bodyResultNodes = this.generate(bodyChild);

		this.last.addEdge(conditionChild, node, 0, this.controlFlowEdgeInfo);
		
		for (Node resultNode : bodyResultNodes)
			this.last.addEdge(conditionChild, resultNode, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : conditionResultNodes)
			this.last.addEdge(bodyChild, resultNode, 0, this.controlFlowEdgeInfo);
		
		nodes.addAll(conditionResultNodes);

		return nodes;
	}
	private List<Node> generateFLoopStructure(Node node)
	{
		final Node initChild = LASTTraverser.getChild(node, NodeInfo.Type.Init);
		final Node conditionChild = LASTTraverser.getChild(node, NodeInfo.Type.Condition);
		final Node bodyChild = LASTTraverser.getChild(node, NodeInfo.Type.Body);
		final Node updateChild = LASTTraverser.getChild(node, NodeInfo.Type.Update);
		
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> initResultNodes = this.generate(initChild);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> bodyResultNodes = this.generate(bodyChild);
		final List<Node> updateResultNodes = this.generate(updateChild);

		this.last.addEdge(conditionChild, node, 0, this.controlFlowEdgeInfo);
		
		for (Node resultNode : conditionResultNodes)
		{
			this.last.addEdge(initChild, resultNode,  0, this.controlFlowEdgeInfo);
			this.last.addEdge(updateChild, resultNode,  0, this.controlFlowEdgeInfo);
		}

		for (Node resultNode : bodyResultNodes)
			this.last.addEdge(conditionChild, resultNode, 0, this.controlFlowEdgeInfo);

		for (Node resultNode : updateResultNodes)
			this.last.addEdge(bodyChild, resultNode, 0, this.controlFlowEdgeInfo);

		nodes.addAll(initResultNodes);
		
		return nodes;
	}
	private List<Node> generateRLoopStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node bodyChild = LASTTraverser.getChild(node, NodeInfo.Type.Body);
		final Node conditionChild = LASTTraverser.getChild(node, NodeInfo.Type.Condition);
		final List<Node> bodyResultNodes = this.generate(bodyChild);
		final List<Node> conditionResultNodes = this.generate(conditionChild);

		this.last.addEdge(conditionChild, node, 0, this.controlFlowEdgeInfo);
		for (Node resultNode : conditionResultNodes)
			this.last.addEdge(bodyChild, resultNode, 0, this.controlFlowEdgeInfo);

		for (Node resultNode : bodyResultNodes)
			this.last.addEdge(conditionChild, resultNode, 0, this.controlFlowEdgeInfo);

		nodes.addAll(bodyResultNodes);

		return nodes;
	}
	
	private List<Node> generateExHandlerStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final Node tryChild = LASTTraverser.getChild(node, NodeInfo.Type.Try);
		final Node catchChild = LASTTraverser.getChild(node, NodeInfo.Type.Catch);
		final Node finallyChild = LASTTraverser.getChild(node, NodeInfo.Type.Finally);
		nodes.add(tryChild); // This CFG arc is necessary to select definitions before the try block when slicing a catch clause
		final List<Node> tryResultNodes = this.generate(tryChild);
		final List<Node> catchResultNodes = this.generateCatchStructure(catchChild);
		final List<Node> finallyResultNodes = this.generate(finallyChild);

		this.last.addEdge(finallyChild, node, 0, this.controlFlowEdgeInfo);
		
		for (Node resultNode : catchResultNodes)
			this.last.addEdge(tryChild, resultNode, 0, this.controlFlowEdgeInfo);

		for (Node resultNode : finallyResultNodes)
		{
			this.last.addEdge(tryChild, resultNode, 0, this.controlFlowEdgeInfo);
			this.last.addEdge(catchChild, resultNode, 0, this.controlFlowEdgeInfo);
		}
		
		nodes.addAll(tryResultNodes);

		return nodes;
	}
	private List<Node> generateCatchStructure(Node node)
	{
		List<Node> catchResultNodes = new LinkedList<Node>();
		List<Node> catchClauses = LASTTraverser.getChildren(node);
		for (Node clause : catchClauses)
		{
			this.last.addEdge(clause, node, 0, this.controlFlowEdgeInfo);
			List<Node> resultNodes = this.generate(clause);
			catchResultNodes.addAll(resultNodes);
		}
		return catchResultNodes;
	}
	
	private List<Node> generateJumpStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Node> children = LASTTraverser.getChildren(node);
		final Node child = children.isEmpty() ? null : children.get(0);

		if (child != null)
		{
			nodes.addAll(generate(child));
			this.last.addEdge(child, node, 0, this.controlFlowEdgeInfo);
		}
		else
			nodes.add(node);
		
		final String dstText = node.getData().getName();
		final int dstId = Integer.parseInt(dstText.substring(dstText.lastIndexOf(" ") + 1));
		final Node dstNode = LASTTraverser.getNode(this.last, dstId);
		
		if (dstNode != null)
			this.last.addEdge(node, dstNode, 0, this.controlFlowEdgeInfo);

		return nodes;
	}
}