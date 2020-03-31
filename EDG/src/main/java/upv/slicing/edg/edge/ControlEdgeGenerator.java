package upv.slicing.edg.edge;

import org.jgrapht.graph.DefaultEdge;
import upv.slicing.edg.Config;
import upv.slicing.edg.graph.*;
import upv.slicing.edg.util.PostdominatorTree;

import java.util.*;

public class ControlEdgeGenerator extends EdgeGenerator {
	public ControlEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	@Override
	public void generate()
	{
		Config.CONTROL_EDGE_GENERATOR_FUNCTION.apply(edg).generate();
	}

	public static boolean postdominates(EDG edg, Node a, Node b)
	{
		return postdominates(edg, a, b, new HashSet<>());
	}

	private static boolean postdominates(EDG edg, Node a, Node b, Set<Node> visited)
	{
		// Stop w/ success if a == b or a has already been visited
		if (a.equals(b) || visited.contains(a))
			return true;
		List<Node> following = edg.getNodes(a, LAST.Direction.Forwards, Edge.Type.ControlFlow);
		// Stop w/ failure if there are no edges to traverse from a
		if (following.isEmpty())
			return false;
		// Find all possible paths starting from a, if ALL find b, then true, else false
		visited.add(a);
		for (Node next : following)
			if (!postdominates(edg, next, b, visited))
				return false;
		return true;
	}

	public abstract static class AbstractControlEdgeGenerator extends EdgeGenerator {
		public AbstractControlEdgeGenerator(EDG edg)
		{
			super(edg);
		}

		@Override
		public void generate()
		{
			for (Node node : edg.getNodes(Node.Type.Routine))
				analyzeRoutine(node);
		}

		public void analyzeNodeList(List<Node> nodeList)
		{
			for (Node src : nodeList) {
				for (Node dest : nodeList) {
					if (src == dest) continue;
					if (hasControlDependence(src, dest))
						edg.addEdge(src, dest, Edge.Type.Control);
				}
			}
		}

		protected abstract boolean hasControlDependence(Node a, Node b);

		public void analyzeRoutine(Node routine)
		{
			// Add an edge from the Start to the End to generate all control dependencies
			Set<Edge> extraEdges = new HashSet<>();
			Node methodRes = edg.getResFromNode(routine);
			if (!hasControlFlowEdge(routine, methodRes)) {
				Edge e = new Edge(Edge.Type.ControlFlow);
				extraEdges.add(e);
				edg.addEdge(routine, methodRes, e);
			}
			// Do the same for each Clause contained within this Routine.
			for (Node n : edg.getChildren(routine)) {
				if (n.getType() == Node.Type.Clause) {
					Node res = edg.getResFromNode(n);
					if (!hasControlFlowEdge(n, res)) {
						Edge e = new Edge(Edge.Type.ControlFlow);
						extraEdges.add(e);
						edg.addEdge(n, res, e);
					}
				}
			}

			List<Node> nodes = edg.getDescendants(routine);
			// The routine and its result should be included (are Start and End).
			nodes.add(methodRes);
			nodes.add(routine);
			analyzeNodeList(nodes);

			// Remove Start --> End edge if it was added by us.
			edg.removeAllEdges(extraEdges);
		}

		private boolean hasControlFlowEdge(Node source, Node target)
		{
			return edg.outgoingEdgesOf(source).stream()
					.filter(e -> e.getType() == Edge.Type.ControlFlow)
					.map(edg::getEdgeTarget)
					.anyMatch(target::equals);
		}
	}

	public static class Classic extends AbstractControlEdgeGenerator {
		public Classic(EDG edg)
		{
			super(edg);
		}

		@Override
		protected boolean hasControlDependence(Node a, Node b)
		{
			int yes = 0;
			List<Node> list = edg.getNodes(a, LAST.Direction.Forwards, Edge.Type.ControlFlow);
			// Nodes with less than 1 outgoing arc cannot control another node.
			if (list.size() < 2)
				return false;
			for (Node n : list) {
				if (postdominates(edg, n, b))
					yes++;
			}
			int no = list.size() - yes;
			return yes > 0 && no > 0;
		}
	}

	public static class StrongPodgurskiClarke extends AbstractControlEdgeGenerator {
		private PostdominatorTree<Node, DefaultEdge> ipdTree = null;
		private GraphWithRoot<Node, DefaultEdge> cfg = new GraphWithRoot<>();

		public StrongPodgurskiClarke(EDG edg)
		{
			super(edg);
			// Create a graph with only Control Flow edges
			edg.vertexSet().forEach(cfg::addVertex);
			edg.edgeSet().stream()
					.filter(e -> e.getType() != Edge.Type.ControlFlow)
					.forEach(this::copyEdge);
		}

		public void copyEdge(Edge e)
		{
			cfg.addEdge(edg.getEdgeSource(e), edg.getEdgeTarget(e), new DefaultEdge());
		}

		@Override
		public void analyzeRoutine(Node routine)
		{
			ipdTree = new PostdominatorTree<>(cfg, routine);
			super.analyzeRoutine(routine);
			ipdTree = null;
		}

		@Override
		protected boolean hasControlDependence(Node a, Node b)
		{
			Node ipd = ipdTree.parentOf(a);
			if (a.equals(ipd) || b.equals(ipd))
				return false;
			for (Node succ : edg.getNodes(a, LAST.Direction.Forwards, Edge.Type.ControlFlow))
				if (hasControlDependence(succ, b, new HashSet<>()))
					return true;
			return false;
		}

		private boolean hasControlDependence(Node a, Node b, Set<Node> path)
		{
			Node ipd = ipdTree.parentOf(a);
			if (path.contains(a) || a.equals(ipd))
				return false;
			if (a.equals(b))
				return true;
			path.add(a);
			for (Node succ : edg.getNodes(a, LAST.Direction.Forwards, Edge.Type.ControlFlow))
				if (hasControlDependence(succ, b, Set.copyOf(path)))
					return true;
			return false;
		}
	}
}
