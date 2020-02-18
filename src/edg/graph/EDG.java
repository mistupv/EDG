package edg.graph;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import edg.LDASTNodeInfo;
import edg.constraint.Constraints;
import edg.constraint.GrammarConstraint;
import edg.graphlib.Arrow;
import edg.graphlib.Graph;
import edg.graphlib.Vertex;
import edg.slicing.SlicingCriterion;
import edg.traverser.EDGTraverser;

public class EDG
{
	/*****************/
	/***** Nodes *****/
	/*****************/
	private final Graph<NodeInfo, EdgeInfo> graph = new Graph<NodeInfo, EdgeInfo>();

	public Node getRootNode()
	{
		return (Node) this.graph.getRootVertex();
	}
	public List<Node> getNodes()
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Vertex<NodeInfo, EdgeInfo>> verticies = this.graph.getVerticies();

		for (Vertex<NodeInfo, EdgeInfo> vertex : verticies)
			nodes.add((Node) vertex);

		return nodes;
	}
	public List<Edge> getEdges()
	{
		final List<Edge> edges = new LinkedList<Edge>();
		final List<Arrow<NodeInfo, EdgeInfo>> arrows = this.graph.getArrows();

		for (Arrow<NodeInfo, EdgeInfo> arrow : arrows)
			edges.add((Edge) arrow);

		return edges;
	}

	public void setRootNode(Node node)
	{
		this.graph.setRootVertex(node);
	}

	public boolean addNode(Node node)
	{
		return this.graph.addVertex(node);
	}
	public boolean addEdge(Node from, Node to, int cost, EdgeInfo data) throws IllegalArgumentException
	{
		final Edge e = new Edge(from, to, cost, data);

		return this.graph.addEdge(e);
	}

	public Node getNode(SlicingCriterion sc)
	{
		if (sc == null)
			return null;

		final String scArchive = sc.getArchive();
		final int scLine = sc.getLine();
		final String scName = sc.getName();
		final List<Node> nodes = this.findNodesByData(null, new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				final LDASTNodeInfo ldNodeInfo = o2.getInfo();
				if (ldNodeInfo == null)
					return -1;
				if (scLine != ldNodeInfo.getLine())
					return -1;
				if (!scName.equals(o2.getName()))
					return -1;
				if (!scArchive.equals(ldNodeInfo.getArchive()))
					return -1;
				return 0;
			}
		});
		final int scOccurrence = sc.getOccurrence();
		if (nodes.isEmpty())
			return null;
		final Node node = nodes.get(scOccurrence - 1);
		return EDGTraverser.getResult(node);
	}
	public Node findNodeByData(NodeInfo data, Comparator<NodeInfo> compare)
	{
		return (Node) this.graph.findNodeByData(data, compare);
	}
	public List<Node> findNodesByData(NodeInfo data, Comparator<NodeInfo> compare)
	{
		final List<Node> nodes = new LinkedList<Node>();
		final List<Vertex<NodeInfo, EdgeInfo>> verticies = this.graph.findVerticiesByData(data, compare);

		for (Vertex<NodeInfo, EdgeInfo> vertex : verticies)
			nodes.add((Node) vertex);

		return nodes;
	}

	/*****************/
	/**** Grammar ****/
	/*****************/
	private final Grammar grammar = new Grammar();

	public Grammar getGrammar()
	{
		return this.grammar;
	}
	public List<Constraints> getProductions(GrammarConstraint grammarConstraint)
	{
		return this.grammar.getProductions(grammarConstraint);
	}

	public void addProduction(GrammarConstraint grammarConstraint, Constraints production)
	{
		this.grammar.addProduction(grammarConstraint, production);
	}
}