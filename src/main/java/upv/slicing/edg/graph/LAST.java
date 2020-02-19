package upv.slicing.edg.graph;

import upv.slicing.edg.graphlib.Arrow;
import upv.slicing.edg.graphlib.Graph;
import upv.slicing.edg.graphlib.Vertex;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

public class LAST
{
	/*****************/
	/***** Nodes *****/
	/*****************/
	protected Graph<NodeInfo, EdgeInfo> graph = new Graph<NodeInfo, EdgeInfo>();
	
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

		if (data.getType() == EdgeInfo.Type.Structural)
			this.graph.addEdgeAndStructural(e);
		return this.graph.addEdge(e);
	}

	public boolean addEdge(Edge e)
	{
		return this.graph.addEdge(e);
	}

	public boolean addStructuralEdge(Node from, Node to)
	{
		return this.graph.addStructural(new Edge(from, to, 0, new EdgeInfo(EdgeInfo.Type.Structural)));
	}

	public boolean removeEDGEdge(Node from, Node to, EdgeInfo.Type edgeType)
	{
		final List<Edge> incomingEdges = to.getIncomingEdges();
		incomingEdges.removeIf(edge -> edge.getData().getType() != edgeType);

		if (incomingEdges.isEmpty())
			return false;

		for (Edge edge : incomingEdges)
			this.graph.removeEDGEdge(edge, from, to);

		return true;
	}

	public boolean removeEdge(Edge edge)
	{
		return this.graph.removeEdge(edge, edge.getFrom(), edge.getTo());
	}

	public void setRemovableEdge(Node from, Node to, EdgeInfo.Type edgeType)
	{
		final List<Edge> incomingEdges = to.getIncomingStructuralEdges();
		incomingEdges.removeIf(edge -> edge.getData().getType() != edgeType);

		if (!incomingEdges.isEmpty())
			for (Edge edge : incomingEdges)
				if (edge.getFrom().equals(from))
				{
					from.getOutgoingEdges().remove(edge);
					to.getIncomingEdges().remove(edge);
					edge.mark();
				}
	}

	public boolean updateToEdge(Node from, Node to, EdgeInfo.Type edgeType, Node newTo)
	{
		final List<Edge> incomingEdges = to.getIncomingEdges();
		incomingEdges.removeIf(edge -> edge.getData().getType() != edgeType);

		if (incomingEdges.isEmpty())
			return false;
		
		for(Edge edge : incomingEdges)
			this.graph.updateToEdge(edge, from, to, newTo);
		
		return true;
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
}