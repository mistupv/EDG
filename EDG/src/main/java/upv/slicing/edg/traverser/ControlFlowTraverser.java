package upv.slicing.edg.traverser;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

// TODO Cuando una variable busca su declaracion/definicion debe salir de la funcion anonima
public class ControlFlowTraverser {
	public enum Direction {Forwards, Backwards}

	public static class Configuration {
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
			return Objects.equals(this.node, nodeWork.node) &&
					Objects.equals(this.state, nodeWork.state);
		}
		public int hashCode()
		{
			return Objects.hash(node.getId(), state);
		}
	}

	private static LAST.Direction getGraphTraverserDirection(Direction direction)
	{
		return LAST.Direction.valueOf(direction.name());
	}

	public static Set<Node> traverse(EDG edg, Node node, Configuration configuration, Predicate<Node> collectAndStop)
	{
		return ControlFlowTraverser.traverse(edg, node, configuration, null, collectAndStop, collectAndStop);
	}
	public static Set<Node> traverse(EDG edg, Node node, Configuration configuration, Predicate<Node> collect, Predicate<Node> stop)
	{
		return ControlFlowTraverser.traverse(edg, node, configuration, null, collect, stop);
	}
	public static Set<Node> traverse(EDG edg, Node node, Configuration configuration, Consumer<Node> newNode, Predicate<Node> collectAndStop)
	{
		return ControlFlowTraverser.traverse(edg, node, configuration, newNode, collectAndStop, collectAndStop);
	}
	public static Set<Node> traverse(EDG edg, Node node, Configuration configuration, Consumer<Node> newNode, Predicate<Node> collect, Predicate<Node> stop)
	{
		final NodeWork<Object> nodeWork = new NodeWork<>(node, null);
		final Function<NodeWork<Object>, Set<NodeWork<Object>>> newStates = state -> { final Set<NodeWork<Object>> states = new HashSet<>(); states.add(state); return states; };
		final Predicate<NodeWork<Object>> collect0 = work -> collect.test(work.node);
		final Predicate<NodeWork<Object>> stop0 = work -> stop.test(work.node);
		final Set<NodeWork<Object>> states = ControlFlowTraverser.traverse(edg, nodeWork, configuration, newNode, newStates, collect0, stop0);
		final Set<Node> nodes = new HashSet<>();

		states.forEach(state -> nodes.add(state.node));

		return nodes;
	}
	public static <T> Set<NodeWork<T>> traverse(EDG edg, NodeWork<T> nodeWork, Configuration configuration, Function<NodeWork<T>, Set<NodeWork<T>>> newWorks, Predicate<NodeWork<T>> collectAndStop)
	{
		return ControlFlowTraverser.traverse(edg, nodeWork, configuration, null, newWorks, collectAndStop, collectAndStop);
	}
	public static <T> Set<NodeWork<T>> traverse(EDG edg, NodeWork<T> nodeWork, Configuration configuration, Function<NodeWork<T>, Set<NodeWork<T>>> newWorks, Predicate<NodeWork<T>> collect, Predicate<NodeWork<T>> stop)
	{
		return ControlFlowTraverser.traverse(edg, nodeWork, configuration, null, newWorks, collect, stop);
	}
	public static <T> Set<NodeWork<T>> traverse(EDG edg, NodeWork<T> nodeWork, Configuration configuration, Consumer<Node> newNode, Function<NodeWork<T>, Set<NodeWork<T>>> newWorks, Predicate<NodeWork<T>> collectAndStop)
	{
		return ControlFlowTraverser.traverse(edg, nodeWork, configuration, newNode, newWorks, collectAndStop, collectAndStop);
	}
	public static <T> Set<NodeWork<T>> traverse(EDG edg, NodeWork<T> nodeWork, Configuration configuration, Consumer<Node> newNode, Function<NodeWork<T>, Set<NodeWork<T>>> newWorks, Predicate<NodeWork<T>> collect, Predicate<NodeWork<T>> stop)
	{
		final Set<NodeWork<T>> collectedWorks = new HashSet<>();
		final Set<NodeWork<T>> pendingWorks = new HashSet<>();
		final Set<NodeWork<T>> doneWorks = new HashSet<>();
		final boolean collectAndStop = collect == stop;
		boolean ignore = configuration.ignoreInitialNode;
// ADDED FOR ARRAYS		
		final List<String> visitedArrayDefinitions = new LinkedList<>();                // Variable to accumulate which positions of the array have been re-defined after declaration
		final Node nodeWorkNode = nodeWork.node;                                        // Expected result node
		final Node nodeWorkSibling = edg.getNodeFromRes(nodeWorkNode);    // Expected DataConstructorAccess node
		final Node.Type nodeWorkSiblingType = nodeWorkSibling.getType();    // Expected NodeInfo.Type.DataConstructorAccess
//System.out.println("--------------");
//System.out.println(nodeWorkNode.getId());
//System.out.println("--------------"); 
// ----------------
		pendingWorks.add(nodeWork);
		while (!pendingWorks.isEmpty())
		{
//System.out.println(++cont);
			final NodeWork<T> currentNodeWork = pendingWorks.iterator().next();
			final Node node = currentNodeWork.node;
//int k = node.getId();
//System.out.print(k + " -> ");

			boolean collectWork = !ignore && collect.test(currentNodeWork);
			boolean stopHere = collectAndStop || ignore ? collectWork : stop.test(currentNodeWork);
			final Set<NodeWork<T>> nextWorks = new HashSet<>();

			pendingWorks.remove(currentNodeWork);
// ADDED FOR ARRAYS (TREATING THE USES)
			if (collectWork)
			{
				final Node parent = edg.getParent(node);
				final Node.Type parentType = parent.getType();
				if (parentType == Node.Type.DataConstructorAccess)            // The use is a data access (x[0])
				{
					final Node index = edg.getChild(parent, Node.Type.Index);
					if (nodeWorkSiblingType ==
						Node.Type.DataConstructorAccess)        // The definition of the variable is also a data access (x[0])
					{
						final Node nodeWorkIndex = edg.getChild(nodeWorkSibling, Node.Type.Index);
						if (nodeWorkIndex.getType() == Node.Type.Literal &&
							// Both data accesses DO NOT refer to the same position
							index.getType() == Node.Type.Literal &&
							!nodeWorkIndex.getName().equals(index.getName()))
							collectWork = false;                                            // IT IS NOT A USE, DON'T ACCUMULATE IT
					} else                                                                    // The definition of the variable is not a data access (int[] x = ...)
					{
						final String fullVarName = node.getName() + "[" + index.getName() +
												   "]";    // fullVarName => String representing a concrete position ("x[1]")
						if (visitedArrayDefinitions.contains(
								fullVarName))                                            // The used data access has been previously defined after input definition
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
				final Node parent = edg.getParent(node);
				final Node.Type parentType = parent.getType();
				if (parentType == Node.Type.DataConstructorAccess)            // The definition is a data access (x[0])
				{
					final Node index = edg.getChild(parent, Node.Type.Index);
					if (nodeWorkSiblingType == Node.Type.DataConstructorAccess)        // The definition of the input variable is also a data access (x[0])
					{
						final Node nodeWorkIndex = edg.getChild(nodeWorkSibling, Node.Type.Index);
						if (!(index.getType() == Node.Type.Literal) ||			// The re-definition of the input variable is not concerte (x[y] = ...)
							!nodeWorkIndex.getName().equals(index.getName()))	// or they are not defining the same position (x[0] /= x[1])
							stopHere = false;									// The input variable may not be defined, look for the next definition
					} else														// The definition of the variable is not a data access (int[] x = ...)
					{
						if (index.getType() == Node.Type.Literal)				// The current definition is defining a concrete position of the array (x[0] = ...)
							// Store the definition for not linking a use with the input definition
							visitedArrayDefinitions.add(node.getName() + "[" + index.getName() + "]");
						stopHere = false;                                                // Continue looking for a total definition
					}
				}
			}


// ------------------
			if (!stopHere)
			{
				final Set<Node> nextNodes = ControlFlowTraverser.step(edg, currentNodeWork.node, configuration);
				nextNodes.forEach(nextNode -> nextWorks.addAll(newWorks.apply(new NodeWork<>(nextNode, currentNodeWork.state))));
			}

			doneWorks.add(currentNodeWork);
			nextWorks.removeIf(work -> doneWorks.contains(work) || pendingWorks.contains(work));
			if (newNode != null)
				nextWorks.forEach(nextWork -> newNode.accept(nextWork.node));
			pendingWorks.addAll(nextWorks);

			ignore = false;
//nextWorks.forEach(nextWork -> System.out.print(nextWork.node.getId()+", "));
//System.out.println();
		}
		return collectedWorks;
	}

	public static Set<Node> step(LAST last, Node node, Configuration configuration)
	{
		final Set<Node> nextNodes = new HashSet<>();
		final LAST.Direction direction = ControlFlowTraverser.getGraphTraverserDirection(configuration.direction);
		final List<Node> nodes = last.getNodes(node, direction, Edge.Type.ControlFlow);
		final boolean isContextBridgeNode = ControlFlowTraverser.isContextBridgeNode(last, node);
		final boolean hasSubCFG = ControlFlowTraverser.hasSubCFG(last, node);
		final boolean hasSuperCFG = ControlFlowTraverser.hasSuperCFG(last, node);
		final boolean isCFGBridgeNode = hasSubCFG || hasSuperCFG;
		final boolean isExiting = ControlFlowTraverser.isExiting(node, configuration.direction);

		if (!isContextBridgeNode && !isCFGBridgeNode)
			nextNodes.addAll(nodes);
		else if (isContextBridgeNode)
		{
			final Node.Type nodeType = node.getType();
			if (isExiting)
				nextNodes.addAll(nodes);
			else
			{
				if (configuration.enterContexts && nodeType != Node.Type.Result)
					nextNodes.addAll(nodes);
// TODO Falta que no se pueda salir de los contextos
//				if (!configuration.exitContexts)
//					nextNodes.removeIf(n -> isContextBridgeNode(n) && isExiting(n, configuration.direction));
				nextNodes.add(last.getSibling(node, configuration.direction == Direction.Forwards ? 1 : 0));
			}
		}
		else if (isCFGBridgeNode)
		{
			final Node.Type nodeType = node.getType();
			if (configuration.enterSubCFG && hasSubCFG && nodeType != Node.Type.Result && !isExiting)
				nextNodes.addAll(last.getChildren(node));
			if (configuration.exitSubCFG && hasSuperCFG && nodeType != Node.Type.Result && isExiting)
				nextNodes.add(last.getParent(node));
			nextNodes.addAll(nodes);
		}

		return nextNodes;
	}
	public static Set<Node> step(LAST last, Node node, Direction direction)
	{
		final LAST.Direction graphTraverserDirection = ControlFlowTraverser.getGraphTraverserDirection(direction);
		return new HashSet<>(last.getNodes(node, graphTraverserDirection, Edge.Type.ControlFlow));
	}

	private static boolean hasSubCFG(LAST last, Node node)
	{
		final Set<Node.Type> shadowingTypes = new HashSet<>();
		shadowingTypes.add(Node.Type.Routine);
		return ControlFlowTraverser.isBridgeNode(last, node, shadowingTypes);
	}
	private static boolean hasSuperCFG(LAST last, Node node)
	{
		final Set<Node.Type> shadowingTypes = new HashSet<>();
		shadowingTypes.add(Node.Type.Clause);
		return ControlFlowTraverser.isBridgeNode(last, node, shadowingTypes);
	}
	private static boolean isContextBridgeNode(LAST last, Node node)
	{
		final Set<Node.Type> contextTypes = new HashSet<>();
		contextTypes.add(Node.Type.ListComprehension);
		return ControlFlowTraverser.isBridgeNode(last, node, contextTypes);
	}
	private static boolean isBridgeNode(LAST last, Node node, Set<Node.Type> types)
	{
		final Node.Type nodeType = node.getType();
		if (types.contains(nodeType))
			return true;
		final Node parent = last.getParent(node);
		final Node.Type parentType = parent.getType();
		if (types.contains(parentType) && nodeType == Node.Type.Result)
			return true;
		final Node sibling = last.getChild(parent, 0);
		final Node.Type siblingType = sibling.getType();
		return types.contains(siblingType) && nodeType == Node.Type.Result;
	}
	private static boolean isExiting(Node node, Direction direction)
	{
		final Node.Type nodeType = node.getType();

		switch (direction)
		{
			case Backwards:
				return nodeType != Node.Type.Result;
			case Forwards:
				return nodeType == Node.Type.Result;
			default:
				throw new RuntimeException("Direction not contemplated: " + direction);
		}
	}


	// FlowEdgeGeneratorNew

	public static Set<Node> step(LAST last, Node node, LAST.Direction direction)
	{
		return new HashSet<>(last.getNodes(node, direction, Edge.Type.ControlFlow));
	}

}
