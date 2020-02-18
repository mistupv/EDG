package edg.graph;

import edg.graphlib.Arrow;

public class Edge extends Arrow<NodeInfo, EdgeInfo>
{
	public Edge(Node from, Node to)
	{
		this(from, to, 0, null);
	}
	public Edge(Node from, Node to, EdgeInfo data)
	{
		this(from, to, 0, data);
	}
	public Edge(Node from, Node to, int cost)
	{
		this(from, to, cost, null);
	}
	public Edge(Node from, Node to, int cost, EdgeInfo data)
	{
		super(from, to, cost, data);
	}

	public Node getFrom()
	{
		return (Node) super.getFrom();
	}
	public Node getTo()
	{
		return (Node) super.getTo();
	}
}