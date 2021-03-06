package upv.slicing.edg.edge;

import upv.slicing.edg.Config;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ControlEdgeGenerator extends EdgeGenerator {
	public ControlEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		for (Node n : edg.getNodes(Node.Type.Routine))
			analyze(n);
	}

	private boolean hasControlFlowEdge(Node source, Node target) {
		return edg.outgoingEdgesOf(source).stream()
				.filter(e -> e.getType() == Edge.Type.ControlFlow)
				.map(edg::getEdgeTarget)
				.anyMatch(target::equals);
	}

	public void analyze(Node method) {
		// Add an edge from the Start to the End to generate all control dependencies
		Set<Edge> extraEdges = new HashSet<>();
		Node methodRes = edg.getResFromNode(method);
		if (!hasControlFlowEdge(method, methodRes)) {
			Edge e = new Edge(Edge.Type.ControlFlow);
			extraEdges.add(e);
			edg.addEdge(method, methodRes, e);
		}
		// Do the same for each Clause contained within this Routine.
		for (Node n : edg.getChildren(method)) {
			if (n.getType() == Node.Type.Clause) {
				Node res = edg.getResFromNode(n);
				if (!hasControlFlowEdge(n, res)) {
					Edge e = new Edge(Edge.Type.ControlFlow);
					extraEdges.add(e);
					edg.addEdge(n, res, e);
				}
			}
		}

		List<Node> nodes = edg.getDescendants(method);
		// The routine and its result should be included (are Start and End).
		nodes.add(methodRes);
		nodes.add(method);
		for (Node src : nodes) {
			for (Node dest : nodes) {
				if (src == dest)
					continue;
				if (hasControlDependence(src, dest))
					edg.addEdge(src, dest, Edge.Type.Control);
			}
		}

		// Remove Start --> End edge if it was added by us.
		edg.removeAllEdges(extraEdges);
	}

	public boolean hasControlDependence(Node a, Node b) {
		int yes = 0;
		List<Node> list = edg.getNodes(a, LAST.Direction.Forwards, Edge.Type.ControlFlow);

		// Add Edge.Type.NonExecControlFlow to the variable list containing "a" nextCFG nodes
		if (Config.APDG || Config.PPDG)
			list.addAll(edg.getNodes(a, LAST.Direction.Forwards, Edge.Type.NonExecControlFlow));

		// Nodes with less than 1 outgoing arc cannot control another node.
		if (list.size() < 2)
			return false;
		for (Node n : list) {
			if (postdominates(n, b))
				yes++;
		}
		int no = list.size() - yes;
		return yes > 0 && no > 0;
	}

	public boolean postdominates(Node a, Node b) {
		return postdominates(a, b, new HashSet<>());
	}

	private boolean postdominates(Node a, Node b, Set<Node> visited) {
		// Stop w/ success if a == b or a has already been visited
		if (a.equals(b) || visited.contains(a))
			return true;
		List<Node> following = edg.getNodes(a, LAST.Direction.Forwards, Edge.Type.ControlFlow);

		// Add Edge.Type.NonExecControlFlow to the variable following containing "a" nextCFG nodes
		if (Config.APDG)
			following.addAll(edg.getNodes(a, LAST.Direction.Forwards, Edge.Type.NonExecControlFlow));

		// Stop w/ failure if there are no edges to traverse from a
		if (following.isEmpty())
			return false;
		// Find all possible paths starting from a, if ALL find b, then true, else false
		visited.add(a);
		for (Node next : following) {
			if (!postdominates(next, b, visited))
				return false;
		}
		return true;
	}
}
