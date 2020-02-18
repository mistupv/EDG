package edg.graph.util;

import java.util.*;

import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.traverser.EDGTraverser;

@SuppressWarnings("unchecked")
public class TarjanStronglyConnectedComponent<Graph extends EDG, Vertex extends Node>
{
	private List<Vertex> getVerticies(Graph graph)
	{
		return (List<Vertex>) graph.getNodes();
	}
	private List<Vertex> getAdjacentVerticies(Vertex vertex)
	{
		final List<Edge> edges = EDGTraverser.getEdges(vertex, EDGTraverser.Direction.Forwards);
		edges.removeIf(e -> e.getData().getType() == EdgeInfo.Type.ControlFlow);
		final List<Vertex> verticies = new ArrayList<Vertex>();

		for (Edge edge : edges)
			verticies.add((Vertex) edge.getTo());

		return verticies;
	}

	private final Graph graph;
	private final Map<Vertex, Integer> visitedTime = new HashMap<Vertex, Integer>();
	private final Map<Vertex, Integer> lowTime = new HashMap<Vertex, Integer>();
	private final Set<Vertex> onStack = new HashSet<Vertex>();
	private final Deque<Vertex> stack = new LinkedList<Vertex>();
	private final Set<Vertex> visited = new HashSet<Vertex>();
	private final List<Set<Vertex>> result = new ArrayList<Set<Vertex>>();
	private int time;

	public TarjanStronglyConnectedComponent(Graph graph)
	{
		this.graph = graph;
	}
	private void restart()
	{
		this.time = 0;
		this.visitedTime.clear();
		this.lowTime.clear();
		this.onStack.clear();
		this.stack.clear();
		this.visited.clear();
		this.result.clear();
	}

	public List<Set<Vertex>> scc()
	{
		this.restart();

		final List<Vertex> verticies = this.getVerticies(this.graph);
		for (Vertex vertex : verticies)
		{
			if (this.visited.contains(vertex))
				continue;
			sccUtil(vertex);
		}

		return this.result;
	}
	private void sccUtil(Vertex vertex)
	{
		this.visited.add(vertex);
		this.visitedTime.put(vertex, this.time);
		this.lowTime.put(vertex, this.time);
		this.time++;
		this.stack.addFirst(vertex);
		this.onStack.add(vertex);

		final List<Vertex> children = this.getAdjacentVerticies(vertex);
		for (Vertex child : children)
		{
			if (!this.visited.contains(child))
			{
				sccUtil(child);
				this.lowTime.compute(vertex, (v, low) -> Math.min(low, this.lowTime.get(child)));
			}
			else if (this.onStack.contains(child))
				this.lowTime.compute(vertex, (v, low) -> Math.min(low, this.visitedTime.get(child)));
		}

		if (this.visitedTime.get(vertex) == this.lowTime.get(vertex))
		{
			final Set<Vertex> stronglyConnectedComponent = new HashSet<Vertex>();
			Vertex v;
			do
			{
				v = this.stack.pollFirst();
				this.onStack.remove(v);
				stronglyConnectedComponent.add(v);
			}
			while (!vertex.equals(v));
			this.result.add(stronglyConnectedComponent);
		}
	}
}