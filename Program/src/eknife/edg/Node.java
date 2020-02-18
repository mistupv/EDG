package eknife.edg;

import java.util.LinkedList;
import java.util.List;

import eknife.lib.graph.Arrow;
import eknife.lib.graph.Vertex;

public class Node extends Vertex<NodeInfo, EdgeInfo>
{
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

	public List<Edge> findEdges(Node dest)
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Arrow<NodeInfo, EdgeInfo>> arrows = super.findEdges(dest);

		for (Arrow<NodeInfo, EdgeInfo> arrow : arrows)
			edges.add((Edge) arrow);

		return edges;
	}
}