package edg.traverser;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;

// TODO Cuando una variable busca su declaracion/definicion debe salir de la funcion anonima
public class ControlFlowTraverser
{
	public static enum Direction { Forwards, Backwards }
	public static class Configuration
	{
		private final Direction direction;
		private final boolean ignoreInitialNode;
		private final boolean enterContexts;
		private final boolean exitContexts;
		private final boolean enterSubCFG;
		private final boolean exitSubCFG;

		public Configuration(Direction direction)
		{
			this(direction, true, true, true, false, false);
		}
		public Configuration(Direction direction, boolean ignoreInitialNode, boolean enterContexts, boolean exitContexts, boolean enterSubCFG, boolean exitSubCFG)
		{
			this.direction = direction;
			this.ignoreInitialNode = ignoreInitialNode;
			this.enterContexts = enterContexts;
			this.exitContexts = exitContexts;
			this.enterSubCFG = enterSubCFG;
			this.exitSubCFG = exitSubCFG;
		}
	}
	// La clase que le de valor a "state" tiene que implementar el metodo "hashCode"
	public static class NodeWork<T>
	{
		private Node node;
		private T state;

		public NodeWork(Node node, T state)
		{
			this.node = node;
			this.state = state;
		}

		public Node getNode()
		{
			return this.node;
		}
		public T getState()
		{
			return this.state;
		}

		public boolean equals(Object obj)
		{
			if (this == obj)
				return true;
			if (!(obj instanceof NodeWork))
				return false;

			@SuppressWarnings("unchecked")
			final NodeWork<T> nodeWork = (NodeWork<T>) obj;
			final Node node = nodeWork.node;
			final T state = nodeWork.state;

			if (this.node != node)
				return false;
			if (this.state != null && !this.state.equals(state))
				return false;
			if (state != null && !state.equals(this.state))
				return false;
			return true;
		}
		public int hashCode()
		{
			if (this.state == null)
				return this.node.getData().getId();
			return this.node.getData().getId() + this.state.hashCode();
		}
	}

	private static EDGTraverser.Direction getGraphTraverserDirection(Direction direction)
	{
		return EDGTraverser.Direction.valueOf(direction.name());
	}

	public static Set<Node> traverse(Node node, Configuration configuration, Predicate<Node> collectAndStop)
	{
		return ControlFlowTraverser.traverse(node, configuration, null, collectAndStop, collectAndStop);
	}
	public static Set<Node> traverse(Node node, Configuration configuration, Predicate<Node> collect, Predicate<Node> stop)
	{
		return ControlFlowTraverser.traverse(node, configuration, null, collect, stop);
	}
	public static Set<Node> traverse(Node node, Configuration configuration, Consumer<Node> newNode, Predicate<Node> collectAndStop)
	{
		return ControlFlowTraverser.traverse(node, configuration, newNode, collectAndStop, collectAndStop);
	}
	public static Set<Node> traverse(Node node, Configuration configuration, Consumer<Node> newNode, Predicate<Node> collect, Predicate<Node> stop)
	{
		final NodeWork<Object> nodeWork = new NodeWork<Object>(node, null);
		final Function<NodeWork<Object>, Set<NodeWork<Object>>> newStates = state -> { final Set<NodeWork<Object>> states = new HashSet<NodeWork<Object>>(); states.add(state); return states; };
		final Predicate<NodeWork<Object>> collect0 = work -> collect.test(work.node);
		final Predicate<NodeWork<Object>> stop0 = work -> stop.test(work.node);
		final Set<NodeWork<Object>> states = ControlFlowTraverser.traverse(nodeWork, configuration, newNode, newStates, collect0, stop0);
		final Set<Node> nodes = new HashSet<Node>();

		states.forEach(state -> nodes.add(state.node));

		return nodes;
	}
	public static <T> Set<NodeWork<T>> traverse(NodeWork<T> nodeWork, Configuration configuration, Function<NodeWork<T>, Set<NodeWork<T>>> newWorks, Predicate<NodeWork<T>> collectAndStop)
	{
		return ControlFlowTraverser.traverse(nodeWork, configuration, null, newWorks, collectAndStop, collectAndStop);
	}
	public static <T> Set<NodeWork<T>> traverse(NodeWork<T> nodeWork, Configuration configuration, Function<NodeWork<T>, Set<NodeWork<T>>> newWorks, Predicate<NodeWork<T>> collect, Predicate<NodeWork<T>> stop)
	{
		return ControlFlowTraverser.traverse(nodeWork, configuration, null, newWorks, collect, stop);
	}
	public static <T> Set<NodeWork<T>> traverse(NodeWork<T> nodeWork, Configuration configuration, Consumer<Node> newNode, Function<NodeWork<T>, Set<NodeWork<T>>> newWorks, Predicate<NodeWork<T>> collectAndStop)
	{
		return ControlFlowTraverser.traverse(nodeWork, configuration, newNode, newWorks, collectAndStop, collectAndStop);
	}
	public static <T> Set<NodeWork<T>> traverse(NodeWork<T> nodeWork, Configuration configuration, Consumer<Node> newNode, Function<NodeWork<T>, Set<NodeWork<T>>> newWorks, Predicate<NodeWork<T>> collect, Predicate<NodeWork<T>> stop)
	{
		final Set<NodeWork<T>> collectedWorks = new HashSet<NodeWork<T>>();
		final Set<NodeWork<T>> pendingWorks = new HashSet<NodeWork<T>>();
		final Set<NodeWork<T>> doneWorks = new HashSet<NodeWork<T>>();
		final boolean collectAndStop = collect == stop;
		boolean ignore = configuration.ignoreInitialNode;
// ADDED FOR ARRAYS		
		final List<String> visitedArrayDefinitions = new LinkedList<String>();
		final Node nodeWorkNode = nodeWork.node;
		final Node nodeWorkSibling = EDGTraverser.getSibling(nodeWorkNode, 0);
		final NodeInfo.Type nodeWorkSiblingType = nodeWorkSibling.getData().getType();
//System.out.println("--------------");
//System.out.println(nodeWorkNode.getData().getId());
//System.out.println("--------------"); 
// ----------------
		pendingWorks.add(nodeWork);
		while (!pendingWorks.isEmpty())
		{
//System.out.println(++cont);
			final NodeWork<T> currentNodeWork = pendingWorks.iterator().next();			
			final Node node = currentNodeWork.node;
//int k = node.getData().getId();
//System.out.print(k + " -> ");

			final Node grandParent = EDGTraverser.getParent(EDGTraverser.getParent(node));
			final NodeInfo.Type grandParentType = grandParent.getData().getType();
			boolean collectWork = ignore ? false : collect.test(currentNodeWork);
			boolean stopHere = collectAndStop || ignore ? collectWork : stop.test(currentNodeWork);
			
			final Set<NodeWork<T>> nextWorks = new HashSet<NodeWork<T>>();

			pendingWorks.remove(currentNodeWork);
// ADDED FOR ARRAYS (TREATING THE USES)
			if (collectWork)
			{
				if (grandParentType == NodeInfo.Type.DataConstructorAccess)
				{
					final Node index = EDGTraverser.getChild(EDGTraverser.getChild(grandParent, 1),0);
					if (nodeWorkSiblingType == NodeInfo.Type.DataConstructorAccess)
					{
						final Node nodeWorkIndex = EDGTraverser.getChild(EDGTraverser.getChild(nodeWorkSibling, 1),0);
						if (nodeWorkIndex.getData().getType() == NodeInfo.Type.Literal && index.getData().getType() == NodeInfo.Type.Literal && !nodeWorkIndex.getData().getName().equals(index.getData().getName()))
							collectWork = false;
					}
					else
					{
						final String fullVarName = node.getData().getName() +"["+ index.getData().getName()+"]";
						if (visitedArrayDefinitions.contains(fullVarName))
							collectWork = false;
					}
				}
			}
// ------------------
			if (collectWork)
				collectedWorks.add(currentNodeWork);
// ADDED FOR ARRAYS (TREATING THE DEFINITIONS)
			if (stopHere)
			{
				if (grandParentType == NodeInfo.Type.DataConstructorAccess)
				{	
					final Node index = EDGTraverser.getChild(EDGTraverser.getChild(grandParent, 1),0);
					if (nodeWorkSiblingType == NodeInfo.Type.DataConstructorAccess)
					{
						final Node nodeWorkIndex = EDGTraverser.getChild(EDGTraverser.getChild(nodeWorkSibling, 1),0);
						if (!(index.getData().getType() == NodeInfo.Type.Literal) || !nodeWorkIndex.getData().getName().equals(index.getData().getName()))
							stopHere = false;
					}
					else
					{
						if (index.getData().getType() == NodeInfo.Type.Literal)
							visitedArrayDefinitions.add(node.getData().getName() +"["+ index.getData().getName() + "]");
						stopHere = false;
					}
				}
			}
// ------------------
			if (!stopHere)
			{
				final Set<Node> nextNodes = ControlFlowTraverser.step(currentNodeWork.node, configuration);
				nextNodes.forEach(nextNode -> nextWorks.addAll(newWorks.apply(new NodeWork<T>(nextNode, currentNodeWork.state))));
			}

			doneWorks.add(currentNodeWork);
			nextWorks.removeIf(work -> doneWorks.contains(work) || pendingWorks.contains(work));
			if (newNode != null)
				nextWorks.forEach(nextWork -> newNode.accept(nextWork.node));
			pendingWorks.addAll(nextWorks);

			ignore = false;
//nextWorks.forEach(nextWork -> System.out.print(nextWork.node.getData().getId()+", "));
//System.out.println();
		}
		return collectedWorks;
	}

	public static Set<Node> step(Node node, Configuration configuration)
	{
		final Set<Node> nextNodes = new HashSet<Node>();
		final EDGTraverser.Direction direction = ControlFlowTraverser.getGraphTraverserDirection(configuration.direction);
		final List<Node> nodes = EDGTraverser.getNodes(node, direction, EdgeInfo.Type.ControlFlow);
		final boolean isContextBridgeNode = ControlFlowTraverser.isContextBridgeNode(node);
		final boolean hasSubCFG = ControlFlowTraverser.hasSubCFG(node);
		final boolean hasSuperCFG = ControlFlowTraverser.hasSuperCFG(node);
		final boolean isCFGBridgeNode = hasSubCFG || hasSuperCFG;
		final boolean isExiting = ControlFlowTraverser.isExiting(node, configuration.direction);

		if (!isContextBridgeNode && !isCFGBridgeNode)
			nextNodes.addAll(nodes);
		else if (isContextBridgeNode)
		{
			final NodeInfo.Type nodeType = node.getData().getType();
			if (isExiting)
				nextNodes.addAll(nodes);
			else
			{
				if (configuration.enterContexts && nodeType != NodeInfo.Type.Result)
					nextNodes.addAll(nodes);
// TODO Falta que no se pueda salir de los contextos
//				if (!configuration.exitContexts)
//					nextNodes.removeIf(n -> isContextBridgeNode(n) && isExiting(n, configuration.direction));
				nextNodes.add(EDGTraverser.getSibling(node, configuration.direction == Direction.Forwards ? 1 : 0));
			}
		}
		else if (isCFGBridgeNode)
		{
			final NodeInfo.Type nodeType = node.getData().getType();
			if (configuration.enterSubCFG && hasSubCFG && nodeType != NodeInfo.Type.Result && !isExiting)
				nextNodes.addAll(EDGTraverser.getChildren(node));
			if (configuration.exitSubCFG && hasSuperCFG && nodeType != NodeInfo.Type.Result && isExiting)
				nextNodes.add(EDGTraverser.getParent(node));
			nextNodes.addAll(nodes);
		}

		return nextNodes;
	}
	public static Set<Node> step(Node node, Direction direction)
	{
		final EDGTraverser.Direction graphTraverserDirection = ControlFlowTraverser.getGraphTraverserDirection(direction);
		return new HashSet<Node>(EDGTraverser.getNodes(node, graphTraverserDirection, EdgeInfo.Type.ControlFlow));
	}

	private static boolean hasSubCFG(Node node)
	{
		final Set<NodeInfo.Type> shadowingTypes = new HashSet<NodeInfo.Type>();
		shadowingTypes.add(NodeInfo.Type.Routine);
		return ControlFlowTraverser.isBridgeNode(node, shadowingTypes);
	}
	private static boolean hasSuperCFG(Node node)
	{
		final Set<NodeInfo.Type> shadowingTypes = new HashSet<NodeInfo.Type>();
		shadowingTypes.add(NodeInfo.Type.Clause);
		return ControlFlowTraverser.isBridgeNode(node, shadowingTypes);
	}
	private static boolean isContextBridgeNode(Node node)
	{
		final Set<NodeInfo.Type> contextTypes = new HashSet<NodeInfo.Type>();
		contextTypes.add(NodeInfo.Type.ListComprehension);
		return ControlFlowTraverser.isBridgeNode(node, contextTypes);
	}
	private static boolean isBridgeNode(Node node, Set<NodeInfo.Type> types)
	{
		final NodeInfo.Type nodeType = node.getData().getType();
		if (types.contains(nodeType))
			return true;
		final Node parent = EDGTraverser.getParent(node);
		final NodeInfo.Type parentType = parent.getData().getType();
		if (types.contains(parentType) && nodeType == NodeInfo.Type.Result)
			return true;
		final Node sibling = EDGTraverser.getChild(parent, 0);
		final NodeInfo.Type siblingType = sibling.getData().getType();
		if (types.contains(siblingType) && nodeType == NodeInfo.Type.Result)
			return true;
		return false;
	}
	private static boolean isExiting(Node node, Direction direction)
	{
		final NodeInfo.Type nodeType = node.getData().getType();

		switch (direction)
		{
			case Backwards:
				return nodeType != NodeInfo.Type.Result;
			case Forwards:
				return nodeType == NodeInfo.Type.Result;
			default:
				throw new RuntimeException("Direction not contemplated: " + direction);
		}
	}
}