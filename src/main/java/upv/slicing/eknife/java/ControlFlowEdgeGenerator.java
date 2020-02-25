package upv.slicing.eknife.java;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.traverser.LASTTraverser;

import java.util.LinkedList;
import java.util.List;

public class ControlFlowEdgeGenerator {
	private enum Way {Forwards, Backwards}

	protected final LAST last;

	public ControlFlowEdgeGenerator(LAST last)
	{
		this.last = last;
	}

	public void generate()
	{
		final List<Node> modules = LASTTraverser.getNodes(this.last, Node.Type.Module);
		for (Node module : modules)
			this.generate(module);

		final List<Node> clauses = LASTTraverser.getNodes(this.last, Node.Type.Clause);
		for (Node clause : clauses)
			this.generate(clause);
	}

	private List<Node> generate(Node node)
	{
		final Node.Type type = node.getType();
		final List<Node> children = LASTTraverser.getChildren(last, node);

		if (type != Node.Type.Break && type != Node.Type.Return && type != Node.Type.Continue)
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
			case TypeCheck:    //ADDED
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
		final List<Node> nodes = new LinkedList<>();
		nodes.add(node);
		return nodes;
	}

	private List<Node> generateStructure(Node node, Way way)
	{
		final List<Node> children = LASTTraverser.getChildren(last, node);
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
			// if (nextChild.getType() != Node.Info.Type.Result) // ¿Why is this "if" here?
			for (Node resultNode : resultNodes)
				this.last.addEdge(child, resultNode, Edge.Type.ControlFlow);
		}

		final Node lastChild = children.get(endIndex);
		Node.Type lastChildType = lastChild.getType();
		if (lastChildType != Node.Type.Break && lastChildType != Node.Type.Continue &&
			lastChildType != Node.Type.Return)
			this.last.addEdge(lastChild, node, Edge.Type.ControlFlow);

		return firstChildResultNodes;
	}

	private List<Node> generateModuleStructure(Node node)
	{
		final List<Node> children = LASTTraverser.getChildren(last, node);
		List<Node> resultNodes = new LinkedList<>();

		for (Node child : children)
		{
			final Node.Type type = child.getType();
			if (type == Node.Type.Routine)
				continue;

			final List<Node> childResults = this.generate(child);

			for (Node resultNode : resultNodes)
				for (Node childResult : childResults)
					this.last.addEdge(resultNode, childResult, Edge.Type.ControlFlow);

			resultNodes.clear();
			resultNodes.add(child);
		}

		return resultNodes;
	}

	private List<Node> generateIfStructure(Node node)
	{
		final Node conditionChild = LASTTraverser.getChild(last, node, 0);
		final Node thenChild = LASTTraverser.getChild(last, node, 1);
		final Node elseChild = LASTTraverser.getChild(last, node, 2);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> thenResultNodes = this.generate(thenChild);
		final List<Node> elseResultNodes = this.generate(elseChild);

		// Condition -> Then First Node
		for (Node resultNode : thenResultNodes)
			this.last.addEdge(conditionChild, resultNode, Edge.Type.ControlFlow);
		// Condition -> Else First Node
		for (Node resultNode : elseResultNodes)
			this.last.addEdge(conditionChild, resultNode, Edge.Type.ControlFlow);

		this.last.addEdge(thenChild, node, Edge.Type.ControlFlow);
		this.last.addEdge(elseChild, node, Edge.Type.ControlFlow);

		return new LinkedList<>(conditionResultNodes);
	}

	private List<Node> generateCasesStructure(Node node)
	{

		final List<Node> selectorDestinationNodes = new LinkedList<>();

		final List<Node> children = LASTTraverser.getChildren(last, node);
		final List<Node> cases = new LinkedList<>(children);
		final List<Node> defaults = new LinkedList<>(children);
		cases.removeIf((c) -> c.getType() != Node.Type.Case);
		defaults.removeIf((c) -> c.getType() != Node.Type.DefaultCase);

		// Put the default at the end of the treated cases
		final Node defaultCase = defaults.isEmpty() ? null : defaults.get(0);
		if (defaultCase != null)
			cases.add(defaultCase);

		final Node lastCase = cases.isEmpty() ? defaultCase : cases.get(cases.size() - 1);
		this.last.addEdge(lastCase, node, Edge.Type.ControlFlow);

		Node previousCase = null;

		// Cases
		for (final Node _case : cases)
		{
			// Current case
			final List<Node> caseResults = generate(_case);

			selectorDestinationNodes.addAll(caseResults);
			if (previousCase != null)
				for (Node caseResult : caseResults)
					this.last.addEdge(previousCase, caseResult, Edge.Type.ControlFlow);
			previousCase = _case;
		}

		return selectorDestinationNodes;
	}

	private List<Node> generateCaseStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<>();
		final List<Node> children = LASTTraverser.getChildren(last, node);
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
				this.last.addEdge(child, resultNode, Edge.Type.ControlFlow);
		}

		Node.Type lastChildType = lastChild.getType();
		if (lastChildType != Node.Type.Break && lastChildType != Node.Type.Continue &&
			lastChildType != Node.Type.Return)
			this.last.addEdge(lastChild, node, Edge.Type.ControlFlow);

		return nodes;
	}

	private List<Node> generateForeachStructure(Node node)
	{
		final Node iteratorChild = LASTTraverser.getChild(last, node, Node.Type.Iterator);
		final Node bodyChild = LASTTraverser.getChild(last, node, Node.Type.Body);

		final List<Node> iteratorResultNodes = this.generate(iteratorChild);
		final List<Node> bodyResultNodes = this.generate(bodyChild);

		this.last.addEdge(node, iteratorChild, Edge.Type.ControlFlow);
		for (Node resultNode : iteratorResultNodes)
			this.last.addEdge(resultNode, bodyChild, Edge.Type.ControlFlow);

		for (Node resultNode : bodyResultNodes)
			this.last.addEdge(resultNode, iteratorChild, Edge.Type.ControlFlow);

		return new LinkedList<>(iteratorResultNodes);
	}

	private List<Node> generateCLoopStructure(Node node)
	{
		final Node conditionChild = LASTTraverser.getChild(last, node, Node.Type.Condition);
		final Node bodyChild = LASTTraverser.getChild(last, node, Node.Type.Body);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> bodyResultNodes = this.generate(bodyChild);

		this.last.addEdge(conditionChild, node, Edge.Type.ControlFlow);

		for (Node resultNode : bodyResultNodes)
			this.last.addEdge(conditionChild, resultNode, Edge.Type.ControlFlow);
		for (Node resultNode : conditionResultNodes)
			this.last.addEdge(bodyChild, resultNode, Edge.Type.ControlFlow);

		return new LinkedList<>(conditionResultNodes);
	}

	private List<Node> generateFLoopStructure(Node node)
	{
		final Node initChild = LASTTraverser.getChild(last, node, Node.Type.Init);
		final Node conditionChild = LASTTraverser.getChild(last, node, Node.Type.Condition);
		final Node bodyChild = LASTTraverser.getChild(last, node, Node.Type.Body);
		final Node updateChild = LASTTraverser.getChild(last, node, Node.Type.Update);

		final List<Node> initResultNodes = this.generate(initChild);
		final List<Node> conditionResultNodes = this.generate(conditionChild);
		final List<Node> bodyResultNodes = this.generate(bodyChild);
		final List<Node> updateResultNodes = this.generate(updateChild);

		this.last.addEdge(conditionChild, node, Edge.Type.ControlFlow);

		for (Node resultNode : conditionResultNodes)
		{
			this.last.addEdge(initChild, resultNode, Edge.Type.ControlFlow);
			this.last.addEdge(updateChild, resultNode, Edge.Type.ControlFlow);
		}

		for (Node resultNode : bodyResultNodes)
			this.last.addEdge(conditionChild, resultNode, Edge.Type.ControlFlow);

		for (Node resultNode : updateResultNodes)
			this.last.addEdge(bodyChild, resultNode, Edge.Type.ControlFlow);

		return new LinkedList<>(initResultNodes);
	}

	private List<Node> generateRLoopStructure(Node node)
	{
		final Node bodyChild = LASTTraverser.getChild(last, node, Node.Type.Body);
		final Node conditionChild = LASTTraverser.getChild(last, node, Node.Type.Condition);
		final List<Node> bodyResultNodes = this.generate(bodyChild);
		final List<Node> conditionResultNodes = this.generate(conditionChild);

		this.last.addEdge(conditionChild, node, Edge.Type.ControlFlow);
		for (Node resultNode : conditionResultNodes)
			this.last.addEdge(bodyChild, resultNode, Edge.Type.ControlFlow);

		for (Node resultNode : bodyResultNodes)
			this.last.addEdge(conditionChild, resultNode, Edge.Type.ControlFlow);

		return new LinkedList<>(bodyResultNodes);
	}

	private List<Node> generateExHandlerStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<>();
		final Node tryChild = LASTTraverser.getChild(last, node, Node.Type.Try);
		final Node catchChild = LASTTraverser.getChild(last, node, Node.Type.Catch);
		final Node finallyChild = LASTTraverser.getChild(last, node, Node.Type.Finally);
		nodes.add(tryChild); // This CFG arc is necessary to select definitions before the try block when slicing a catch clause
		final List<Node> tryResultNodes = this.generate(tryChild);
		final List<Node> catchResultNodes = this.generateCatchStructure(catchChild);
		final List<Node> finallyResultNodes = this.generate(finallyChild);

		this.last.addEdge(finallyChild, node, Edge.Type.ControlFlow);

		for (Node resultNode : catchResultNodes)
			this.last.addEdge(tryChild, resultNode, Edge.Type.ControlFlow);

		for (Node resultNode : finallyResultNodes)
		{
			this.last.addEdge(tryChild, resultNode, Edge.Type.ControlFlow);
			this.last.addEdge(catchChild, resultNode, Edge.Type.ControlFlow);
		}

		nodes.addAll(tryResultNodes);

		return nodes;
	}

	private List<Node> generateCatchStructure(Node node)
	{
		List<Node> catchResultNodes = new LinkedList<>();
		List<Node> catchClauses = LASTTraverser.getChildren(last, node);
		for (Node clause : catchClauses)
		{
			this.last.addEdge(clause, node, Edge.Type.ControlFlow);
			List<Node> resultNodes = this.generate(clause);
			catchResultNodes.addAll(resultNodes);
		}
		return catchResultNodes;
	}

	private List<Node> generateJumpStructure(Node node)
	{
		final List<Node> nodes = new LinkedList<>();
		final List<Node> children = LASTTraverser.getChildren(last, node);
		final Node child = children.isEmpty() ? null : children.get(0);

		if (child != null)
		{
			nodes.addAll(generate(child));
			this.last.addEdge(child, node, Edge.Type.ControlFlow);
		} else
			nodes.add(node);

		final String dstText = node.getName();
		final int dstId = Integer.parseInt(dstText.substring(dstText.lastIndexOf(" ") + 1));
		final Node dstNode = LASTTraverser.getNode(this.last, dstId);

		if (dstNode != null)
			this.last.addEdge(node, dstNode, Edge.Type.ControlFlow);

		return nodes;
	}
}