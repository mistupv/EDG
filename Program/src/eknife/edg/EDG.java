package eknife.edg;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import eknife.edg.constraint.Constraint;
import eknife.edg.constraint.SummaryConstraint;
import eknife.edg.traverser.GraphTraverser;
import eknife.lib.graph.Arrow;
import eknife.lib.graph.Graph;
import eknife.lib.graph.Vertex;

public class EDG
{
	/*****************/
	/***** Nodes *****/
	/*****************/
	private Graph<NodeInfo, EdgeInfo> graph = new Graph<NodeInfo, EdgeInfo>();

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
		// TODO Si el programa sigue funcionando bien, esto sobra!!
/*
		if (this.graph.getVerticies().contains(from) == false)
			throw new IllegalArgumentException("from is not in graph");
		if (this.graph.getVerticies().contains(to) == false)
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
		this.graph.addEdge(e);
		return true;
*/
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
	public boolean removeVertex(Node node)
	{
		return this.graph.removeVertex(node);
	}
	public int size()
	{
		return this.graph.size();
	}

	public int getSC()
	{
		final List<Node> nodes = this.getNodes();
		Node sliceAtomNode = null;
		for (Node node : nodes)
		{
			if (node.getData().getType() == NodeInfo.Type.Atom && node.getData().getName().equals("slice"))
				sliceAtomNode = node;
		}
		final Node tuple = GraphTraverser.getParent(sliceAtomNode, EdgeInfo.Type.Control);
		final Node sliceNode = GraphTraverser.getChild(tuple, 1);
		return sliceNode.getData().getId();
	}

	/*****************/
	/**** Grammar ****/
	/*****************/
	public static enum GrammarType { Value, Exception }

	private final Grammar valueGrammar = new Grammar();
	private final Grammar exceptionGrammar = new Grammar();

	public Grammar getGrammar(GrammarType grammarType)
	{
		if (grammarType == GrammarType.Value)
			return this.valueGrammar;
		else if (grammarType == GrammarType.Exception)
			return this.exceptionGrammar;
		else
			throw new RuntimeException("Grammar type not contemplated: " + grammarType);
	}
	public List<List<Constraint>> getProductions(GrammarType grammarType, SummaryConstraint summaryConstraint)
	{
		final Grammar grammar = this.getGrammar(grammarType);

		return grammar.getProductions(summaryConstraint);
	}

	public void addProduction(GrammarType grammarType, SummaryConstraint summaryConstraint, List<Constraint> production)
	{
		final Grammar grammar = this.getGrammar(grammarType);

		grammar.addProduction(summaryConstraint, production);
	}
}