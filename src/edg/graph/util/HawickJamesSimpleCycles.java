package edg.graph.util;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.traverser.EDGTraverser;

@SuppressWarnings({ "unchecked", "rawtypes" })
public class HawickJamesSimpleCycles<Graph extends EDG, Vertex extends Node>
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
	private String toString(Vertex vertex)
	{
		return vertex.getData().getId() + "";
	}

	private static enum Operation { ENUMERATE, PRINT_ONLY, COUNT_ONLY };

	private int numVertices = 0;
	private long numCycles = 0;
	private List<List<Vertex>> cycles = null;

	// The main state of the algorithm
	private Integer start = 0;
	private List<Integer>[] Ak = null;
	private List<Integer>[] B = null;
	private boolean[] blocked = null;
	private ArrayDeque<Integer> stack = null;

	// Giving an index to every Vertex
	private final List<Vertex> indexToVertex;
	private final Map<Vertex, Integer> vertexToIndex;

	public HawickJamesSimpleCycles(Graph graph)
	{
		final List<Vertex> verticies = this.getVerticies(graph);

		this.numVertices = verticies.size();
		this.indexToVertex = new ArrayList<Vertex>(verticies);
		this.vertexToIndex = new HashMap<Vertex, Integer>();
		for (int index = 0; index < this.indexToVertex.size(); index++)
			this.vertexToIndex.put(this.indexToVertex.get(index), index);
	}
	private void init(Operation operation)
	{
		this.numCycles = 0;
		if (operation == Operation.ENUMERATE)
			this.cycles = new ArrayList<List<Vertex>>();
		this.blocked = new boolean[this.numVertices];
		this.stack = new ArrayDeque<Integer>(this.numVertices);

		this.B = new ArrayList[this.numVertices];
		for (int i = 0; i < this.numVertices; i++)
			this.B[i] = new ArrayList<Integer>();

		this.Ak = this.buildAdjacencyList();
		this.stack.clear();
	}
	private List<Integer>[] buildAdjacencyList()
	{
		final List[] Ak = new ArrayList[this.numVertices];

		for (int vertexIndex = 0; vertexIndex < this.numVertices; vertexIndex++)
		{
			final Vertex vertex = this.getVertex(vertexIndex);
			final List<Vertex> s = this.getAdjacentVerticies(vertex);

			Ak[vertexIndex] = new ArrayList<Integer>(s.size());
			for (Vertex adjacenceVertex : s)
				Ak[vertexIndex].add(this.getIndex(adjacenceVertex));
		}

		return Ak;
	}
	private void reset()
	{
		this.Ak = null;
		this.B = null;
		this.blocked = null;
		this.stack = null;
	}

	public List<List<Vertex>> findSimpleCycles()
	{
		this.getSimpleCycles(Operation.ENUMERATE);
		this.reset();

		return this.cycles;
	}
	public void printSimpleCycles()
	{
		this.getSimpleCycles(Operation.PRINT_ONLY);
		this.reset();
	}
	public long countSimpleCycles()
	{
		this.getSimpleCycles(Operation.COUNT_ONLY);
		this.reset();

		return this.numCycles;
	}
	private void getSimpleCycles(Operation operation)
	{
		this.init(operation);
		for (int vertexIndex = 0; vertexIndex < this.numVertices; vertexIndex++)
		{
			for (int vertexIndex2 = 0; vertexIndex2 < this.numVertices; vertexIndex2++)
			{
				this.blocked[vertexIndex2] = false;
				this.B[vertexIndex2].clear();
			}

			this.start = vertexIndex;
			this.circuit(this.start, operation);
		}
	}

	private boolean circuit(Integer vertexIndex, Operation operation)
	{
		boolean found = false;

		this.stack.push(vertexIndex);
		this.blocked[vertexIndex] = true;

		for (Integer w : this.Ak[vertexIndex])
		{
			if (w < this.start)
				continue;

			if (w == this.start)
			{
				this.cycleFound(operation);
				found = true;
			}
			else if (!this.blocked[w])
				if (this.circuit(w, operation))
					found = true;
		}

		if (found)
			this.unblock(vertexIndex);
		else
		{
			final List<Integer> ak = this.Ak[vertexIndex];

			for (Integer w : ak)
			{
				if (w < this.start)
					continue;
				if (!this.B[w].contains(vertexIndex))
					this.B[w].add(vertexIndex);
			}
		}

		this.stack.pop();

		return found;
	}
	private void unblock(Integer u)
	{
		final List<Integer> list = this.B[u];

		this.blocked[u] = false;
		for (int wPos = 0; wPos < list.size(); wPos++)
		{
			final Integer w = list.get(wPos);
			final int prevSize = list.size();
			list.removeIf(elem -> elem == w );
			final int newSize = list.size();

			wPos -= prevSize - newSize;
			if (this.blocked[w])
				this.unblock(w);
		}
	}
	private void cycleFound(Operation operation)
	{
		switch (operation)
		{
			case ENUMERATE:
				final List<Vertex> cycle = new ArrayList<Vertex>(this.stack.size());
				for (Integer id : this.stack)
					cycle.add(this.getVertex(id));
				this.cycles.add(cycle);
				break;
			case PRINT_ONLY:
				for (Integer id : this.stack)
					System.out.print(this.toString(this.getVertex(id)) + " ");
				System.out.println("");
				break;
			case COUNT_ONLY:
				this.numCycles++;
				break;
		}
	}

	private Vertex getVertex(int index)
	{
		return this.indexToVertex.get(index);
	}
	private Integer getIndex(Vertex vertex)
	{
		return this.vertexToIndex.get(vertex);
	}
}