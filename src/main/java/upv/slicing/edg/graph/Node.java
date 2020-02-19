package upv.slicing.edg.graph;

import upv.slicing.edg.graphlib.Arrow;
import upv.slicing.edg.graphlib.Vertex;

import java.util.LinkedList;
import java.util.List;

public class Node extends Vertex<NodeInfo, EdgeInfo> {
	public Node()
	{
		this(null, null);
	}

	public Node(String n)
	{
		this(n, null);
	}

	public Node(String n, NodeInfo data)
	{
		super(n, data);
	}

	public List<Edge> getIncomingEdges()
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Arrow<NodeInfo, EdgeInfo>> arrows = super.getIncomingArrows();

		for (Arrow<NodeInfo, EdgeInfo> arrow : arrows)
			edges.add((Edge) arrow);

		return edges;
	}

	public List<Edge> getOutgoingEdges()
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Arrow<NodeInfo, EdgeInfo>> arrows = super.getOutgoingArrows();

		for (Arrow<NodeInfo, EdgeInfo> arrow : arrows)
			edges.add((Edge) arrow);

		return edges;
	}

	public List<Edge> getIncomingStructuralEdges()
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Arrow<NodeInfo, EdgeInfo>> arrows = super.getIncomingStructuralArrows();

		for (Arrow<NodeInfo, EdgeInfo> arrow : arrows)
			edges.add((Edge) arrow);

		return edges;
	}

	public List<Edge> getOutgoingStructuralEdges()
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Arrow<NodeInfo, EdgeInfo>> arrows = super.getOutgoingStructuralArrows();

		for (Arrow<NodeInfo, EdgeInfo> arrow : arrows)
			edges.add((Edge) arrow);

		return edges;
	}

	public List<Edge> findEdges(Node dest)
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Arrow<NodeInfo, EdgeInfo>> arrows = super.findEdges(dest);

		for (Arrow<NodeInfo, EdgeInfo> arrow : arrows)
			edges.add((Edge) arrow);

		return edges;
	}

	public void setName(String newName)
	{
		this.name = newName;
	}
}