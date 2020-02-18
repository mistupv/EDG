package edg.graph;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import edg.constraint.Constraint;
import edg.constraint.SummaryConstraint;
import edg.graphlib.Arrow;
import edg.graphlib.Graph;
import edg.graphlib.Vertex;

public class EDG extends Graph<NodeInfo, EdgeInfo>
{
	private final Grammar grammar = new Grammar();

	public Node getRootNode()
	{
		return (Node) super.getRootVertex();
	}
	public List<Node> getNodes()
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Vertex<NodeInfo, EdgeInfo>> verticies = super.getVerticies();

		for (Vertex<NodeInfo, EdgeInfo> vertex : verticies)
			nodes.add((Node) vertex);

		return nodes;
	}
	public List<Edge> getEdges()
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Arrow<NodeInfo, EdgeInfo>> arrows = super.getArrows();

		for (Arrow<NodeInfo, EdgeInfo> arrow : arrows)
			edges.add((Edge) arrow);

		return edges;
	}
	public List<List<Constraint>> getProductions(SummaryConstraint summaryConstraint)
	{
		return this.grammar.getProductions(summaryConstraint);
	}

	public void setRootNode(Node node)
	{
		super.setRootVertex(node);
	}

	public boolean addNode(Node node)
	{
		return super.addVertex(node);
	}
	public boolean addEdge(Node from, Node to, int cost, EdgeInfo data) throws IllegalArgumentException
	{
		if (super.verticies.contains(from) == false)
			throw new IllegalArgumentException("from is not in graph");
		if (super.verticies.contains(to) == false)
			throw new IllegalArgumentException("to is not in graph");

		Edge e = new Edge(from, to, cost, data);
		List<Edge> es2 = from.findEdges(to);

		for (Edge e2 : es2)
			if (e2 != null && cost == e2.getCost() &&
				((data == null && e2.getData() == null) ||
				(data != null && data.equals(e2.getData()))))
				return false;

		from.addEdge(e);
		to.addEdge(e);
		edges.add(e);
		return true;
	}
	public void addProduction(SummaryConstraint summaryConstraint, List<Constraint> production)
	{
		this.grammar.addProduction(summaryConstraint, production);
	}

	public Node findNodeByData(NodeInfo data, Comparator<NodeInfo> compare)
	{
		return (Node) super.findNodeByData(data, compare);
	}
	public List<Node> findNodesByData(NodeInfo data, Comparator<NodeInfo> compare)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Vertex<NodeInfo, EdgeInfo>> verticies = super.findVerticiesByData(data, compare);

		for (Vertex<NodeInfo, EdgeInfo> vertex : verticies)
			nodes.add((Node) vertex);

		return nodes;
	}

	public List<Node> getFicticiousNodes()
	{
		final List<Node> ficticiousFunct = this.findNodesByData(null, new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				if (o2.isFictitious())
					return 0;
				return -1;
			}
		});

		return ficticiousFunct;
	}
}