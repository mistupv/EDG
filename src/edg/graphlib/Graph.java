package edg.graphlib;

/*
 * JBoss, Home of Professional Open Source
 * Copyright 2006, Red Hat Middleware LLC, and individual contributors
 * by the @authors tag. See the copyright.txt in the distribution for a
 * full listing of individual contributors.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA, or see the FSF site: http://www.fsf.org.
 */

import java.util.*;

/**
 * A directed graph data structure.
 *
 * @param <T, S>
 * @author Scott.Stark@jboss.org
 * @version $Revision$
 */
@SuppressWarnings("unchecked")
public class Graph<T, S> {
	/**
	 * Color used to mark unvisited nodes
	 */
	public static final int VISIT_COLOR_WHITE = 1;

	/**
	 * Color used to mark nodes as they are first visited in DFS order
	 */
	public static final int VISIT_COLOR_GREY = 2;

	/**
	 * Color used to mark nodes after descendants are completely visited
	 */
	public static final int VISIT_COLOR_BLACK = 3;

	/**
	 * Vector<Vertex> of graph verticies
	 */
	protected List<Vertex<T, S>> verticies;

	/**
	 * Vector<Edge> of edges in the graph
	 */
	protected List<Arrow<T, S>> edges;

	/**
	 * The vertex identified as the root of the graph
	 */
	protected Vertex<T, S> rootVertex;

	/**
	 * Construct a new graph without any vertices or edges
	 */
	public Graph()
	{
		verticies = new ArrayList<Vertex<T, S>>();
		edges = new ArrayList<Arrow<T, S>>();
	}

	/**
	 * Are there any verticies in the graph
	 *
	 * @return true if there are no verticies in the graph
	 */
	public boolean isEmpty()
	{
		return verticies.size() == 0;
	}

	/**
	 * Add a vertex to the graph
	 *
	 * @param v the Vertex to add
	 * @return true if the vertex was added, false if it was already in the graph.
	 */
	public boolean addVertex(Vertex<T, S> v)
	{
		boolean added = false;
		if (!verticies.contains(v))
		{
			added = verticies.add(v);
		}
		return added;
	}

	/**
	 * Get the vertex count.
	 *
	 * @return the number of verticies in the graph.
	 */
	public int size()
	{
		return verticies.size();
	}

	/**
	 * Get the root vertex
	 *
	 * @return the root vertex if one is set, null if no vertex has been set as
	 * the root.
	 */
	public Vertex<T, S> getRootVertex()
	{
		return rootVertex;
	}

	/**
	 * Set a root vertex. If root does no exist in the graph it is added.
	 *
	 * @param root -
	 *             the vertex to set as the root and optionally add if it does not
	 *             exist in the graph.
	 */
	public void setRootVertex(Vertex<T, S> root)
	{
		this.rootVertex = root;
		if (!verticies.contains(root))
			this.addVertex(root);
	}

	/**
	 * Get the given Vertex.
	 *
	 * @param n the index [0, size()-1] of the Vertex to access
	 * @return the nth Vertex
	 */
	public Vertex<T, S> getVertex(int n)
	{
		return verticies.get(n);
	}

	/**
	 * Get the graph verticies
	 *
	 * @return the graph verticies
	 */
	public List<Vertex<T, S>> getVerticies()
	{
		return this.verticies;
	}

	/**
	 * Insert a directed, weighted Edge<T, S> into the graph.
	 *
	 * @param from -
	 *             the Edge<T, S> starting vertex
	 * @param to   -
	 *             the Edge<T, S> ending vertex
	 * @param cost -
	 *             the Edge<T, S> weight/cost
	 * @return true if the Edge<T, S> was added, false if from already has this Edge<T, S>
	 * @throws IllegalArgumentException if from/to are not verticies in the graph
	 */
	public boolean addEdge(Vertex<T, S> from, Vertex<T, S> to, int cost) throws IllegalArgumentException
	{
		return this.addEdge(from, to, cost, null);
	}

	/**
	 * Insert a directed, weighted Edge<T, S> into the graph.
	 *
	 * @param from -
	 *             the Edge<T, S> starting vertex
	 * @param to   -
	 *             the Edge<T, S> ending vertex
	 * @param cost -
	 *             the Edge<T, S> weight/cost
	 * @param data -
	 *             the Edge<T, S> data
	 * @return true if the Edge<T, S> was added, false if from already has this Edge<T, S>
	 * @throws IllegalArgumentException if from/to are not verticies in the graph
	 */
	public boolean addEdge(Vertex<T, S> from, Vertex<T, S> to, int cost, S data) throws IllegalArgumentException
	{
		Arrow<T, S> e = new Arrow<T, S>(from, to, cost, data);
		return this.addEdge(e);
	}

	/**
	 * Insert a directed, weighted Edge<T, S> into the graph.
	 *
	 * @param e -
	 *          the Edge<T, S>
	 * @return true if the Edge<T, S> was added, false if from already has this Edge<T, S>
	 * @throws IllegalArgumentException if from/to are not verticies in the graph
	 */
	public boolean addEdge(Arrow<T, S> e)
	{
		Vertex<T, S> from = e.getFrom();
		Vertex<T, S> to = e.getTo();
		int cost = e.getCost();
		S data = e.getData();

		if (!verticies.contains(from))
			throw new IllegalArgumentException("from is not in graph");
		if (!verticies.contains(to))
			throw new IllegalArgumentException("to is not in graph");

		List<Arrow<T, S>> es2 = from.findEdges(to);

		for (Arrow<T, S> e2 : es2)
			if (e2 != null && cost == e2.getCost() &&
					((data == null && e2.getData() == null) ||
							(data != null && data.equals(e2.getData()))))
				return false;

		// Can't rely on Vertex#addEdge(Arrow) when from == to; it adds 2 outgoing and 0 incoming
		if (Objects.equals(from, to))
		{
			from.getOutgoingArrows().add(e);
			to.getOutgoingArrows().add(e);
		} else
		{
			from.addEdge(e);
			to.addEdge(e);
		}
		edges.add(e);
		return true;
	}

	public boolean addEdgeAndStructural(Arrow<T, S> e)
	{
		Vertex<T, S> from = e.getFrom();
		Vertex<T, S> to = e.getTo();
		int cost = e.getCost();
		S data = e.getData();

		if (!verticies.contains(from))
			throw new IllegalArgumentException("from is not in graph");
		if (!verticies.contains(to))
			throw new IllegalArgumentException("to is not in graph");

		List<Arrow<T, S>> es2 = from.findEdges(to);

		for (Arrow<T, S> e2 : es2)
			if (e2 != null && cost == e2.getCost() &&
					((data == null && e2.getData() == null) ||
							(data != null && data.equals(e2.getData()))))
				return false;

		from.addStructural(e);
		to.addStructural(e);
		from.addEdge(e);
		to.addEdge(e);
		edges.add(e);
		return true;
	}

	public boolean addStructural(Arrow<T, S> e)
	{
		Vertex<T, S> from = e.getFrom();
		Vertex<T, S> to = e.getTo();
		int cost = e.getCost();
		S data = e.getData();

		if (!verticies.contains(from))
			throw new IllegalArgumentException("from is not in graph");
		if (!verticies.contains(to))
			throw new IllegalArgumentException("to is not in graph");

		List<Arrow<T, S>> es2 = from.findEdges(to);

		for (Arrow<T, S> e2 : es2)
			if (e2 != null && cost == e2.getCost() &&
					((data == null && e2.getData() == null) ||
							(data != null && data.equals(e2.getData()))))
				return false;

		from.addStructural(e);
		to.addStructural(e);
		edges.add(e);
		e.mark();
		return true;
	}

	/**
	 * Insert a bidirectional Edge<T, S> in the graph
	 *
	 * @param from -
	 *             the Edge<T, S> starting vertex
	 * @param to   -
	 *             the Edge<T, S> ending vertex
	 * @param cost -
	 *             the Edge<T, S> weight/cost
	 * @return true if edges between both nodes were added, false otherwise
	 * @throws IllegalArgumentException if from/to are not verticies in the graph
	 */
	public boolean insertBiEdge(Vertex<T, S> from, Vertex<T, S> to, int cost)
			throws IllegalArgumentException
	{
		return addEdge(from, to, cost) && addEdge(to, from, cost);
	}

	/**
	 * Get the graph edges
	 *
	 * @return the graph edges
	 */
	public List<Arrow<T, S>> getArrows()
	{
		return this.edges;
	}

	/**
	 * Remove a vertex from the graph
	 *
	 * @param v the Vertex to remove
	 * @return true if the Vertex was removed
	 */
	// TODO: review!
	public boolean removeVertex(Vertex<T, S> v)
	{
		if (!verticies.contains(v))
			return false;

		verticies.remove(v);
		if (v == rootVertex)
			rootVertex = null;

		// Remove the edges associated with v
		for (int n = 0; n < v.getOutgoingEdgeCount(); n++)
		{
			Arrow<T, S> e = v.getOutgoingEdge(n);
			v.remove(e);
			Vertex<T, S> to = e.getTo();
			to.remove(e);
			edges.remove(e);
		}
		for (int n = 0; n < v.getIncomingEdgeCount(); n++)
		{
			Arrow<T, S> e = v.getIncomingEdge(n);
			v.remove(e);
			Vertex<T, S> from = e.getFrom();
			from.remove(e);
			edges.remove(e);
		}
		return true;
	}

	/**
	 * Remove edges from the graph
	 *
	 * @param from -
	 *             the edges starting vertex
	 * @param to   -
	 *             the edges ending vertex
	 * @return true if any edge exists, false otherwise
	 */
	public boolean removeEdges(Vertex<T, S> from, Vertex<T, S> to)
	{
		List<Arrow<T, S>> es = from.findEdges(to);
		if (es.isEmpty())
			return false;

		for (Arrow<T, S> e : es)
		{
			from.remove(e);
			to.remove(e);
			edges.remove(e);
		}
		return true;
	}

	/**
	 * Remove edges from the graph
	 *
	 * @param from -
	 *             the edges starting vertex
	 * @param to   -
	 *             the edges ending vertex
	 * @return true if any edge exists, false otherwise
	 */
	public boolean removeEdge(Arrow<T, S> arrow, Vertex<T, S> from, Vertex<T, S> to)
	{
		int index = edges.indexOf(arrow);
		if (index == -1)
			return false;

		Arrow<T, S> e = edges.get(index);

		from.remove(e);
		to.remove(e);
		edges.remove(e);

		return true;
	}

	public boolean removeEDGEdge(Arrow<T, S> arrow, Vertex<T, S> from, Vertex<T, S> to)
	{
		int index = edges.indexOf(arrow);
		if (index == -1)
			return false;

		Arrow<T, S> e = edges.get(index);

		from.remove(e);
		to.remove(e);

		return true;
	}

	/**
	 * Update the to node an arrow is pointing to
	 * <p>
	 * * @param from -
	 * the edges starting vertex
	 *
	 * @param to    -
	 *              the edges ending vertex
	 * @param newTo -
	 *              the new edges ending vertex
	 * @return true if any edge exists, false otherwise
	 */
	public boolean updateToEdge(Arrow<T, S> arrow, Vertex<T, S> from, Vertex<T, S> to, Vertex<T, S> newTo)
	{ // ADDED
		int index = edges.indexOf(arrow);
		if (index == -1)
			return false;

		Arrow<T, S> e = edges.get(index);
		to.remove(e);
		e.updateTo(newTo);
		newTo.addEdge(e);

		return true;
	}

	/**
	 * Clear the mark state of all verticies in the graph by calling clearMark()
	 * on all verticies.
	 *
	 * @see Vertex#clearMark()
	 */
	public void clearMark()
	{
		for (Vertex<T, S> w : verticies)
			w.clearMark();
	}

	/**
	 * Clear the mark state of all edges in the graph by calling clearMark() on
	 * all edges.
	 */
	public void clearEdges()
	{
		for (Arrow<T, S> e : edges)
			e.clearMark();
	}

	/**
	 * Perform a depth first serach using recursion.
	 *
	 * @param v       -
	 *                the Vertex to start the search from
	 * @param visitor -
	 *                the vistor to inform prior to
	 * @see Visitor#visit(Graph, Vertex)
	 */
	public void depthFirstSearch(Vertex<T, S> v, final Visitor<T, S> visitor)
	{
		VisitorEX<T, S, RuntimeException> wrapper = new VisitorEX<T, S, RuntimeException>() {
			public void visit(Graph<T, S> g, Vertex<T, S> v) throws RuntimeException
			{
				if (visitor != null)
					visitor.visit(g, v);
			}
		};
		this.depthFirstSearch(v, wrapper);
	}

	/**
	 * Perform a depth first serach using recursion. The search may be cut short
	 * if the visitor throws an exception.
	 *
	 * @param <E>
	 * @param v       -
	 *                the Vertex to start the search from
	 * @param visitor -
	 *                the vistor to inform prior to
	 * @throws E if visitor.visit throws an exception
	 * @see Visitor#visit(Graph, Vertex)
	 */
	public <E extends Exception> void depthFirstSearch(Vertex<T, S> v, VisitorEX<T, S, E> visitor) throws E
	{
		if (visitor != null)
			visitor.visit(this, v);
		v.visit();
		for (int i = 0; i < v.getOutgoingEdgeCount(); i++)
		{
			Arrow<T, S> e = v.getOutgoingEdge(i);
			if (!e.getTo().visited())
			{
				depthFirstSearch(e.getTo(), visitor);
			}
		}
	}

	/**
	 * Perform a breadth first search of this graph, starting at v.
	 *
	 * @param v       -
	 *                the search starting point
	 * @param visitor -
	 *                the vistor whose vist method is called prior to visting a vertex.
	 */
	public void breadthFirstSearch(Vertex<T, S> v, final Visitor<T, S> visitor)
	{
		VisitorEX<T, S, RuntimeException> wrapper = new VisitorEX<T, S, RuntimeException>() {
			public void visit(Graph<T, S> g, Vertex<T, S> v) throws RuntimeException
			{
				if (visitor != null)
					visitor.visit(g, v);
			}
		};
		this.breadthFirstSearch(v, wrapper);
	}

	/**
	 * Perform a breadth first search of this graph, starting at v. The vist may
	 * be cut short if visitor throws an exception during a vist callback.
	 *
	 * @param <E>
	 * @param v       -
	 *                the search starting point
	 * @param visitor -
	 *                the vistor whose vist method is called prior to visting a vertex.
	 * @throws E if vistor.visit throws an exception
	 */
	public <E extends Exception> void breadthFirstSearch(Vertex<T, S> v, VisitorEX<T, S, E> visitor)
			throws E
	{
		LinkedList<Vertex<T, S>> q = new LinkedList<Vertex<T, S>>();

		q.add(v);
		if (visitor != null)
			visitor.visit(this, v);
		v.visit();
		while (!q.isEmpty())
		{
			v = q.removeFirst();
			for (int i = 0; i < v.getOutgoingEdgeCount(); i++)
			{
				Arrow<T, S> e = v.getOutgoingEdge(i);
				Vertex<T, S> to = e.getTo();
				if (!to.visited())
				{
					q.add(to);
					if (visitor != null)
						visitor.visit(this, to);
					to.visit();
				}
			}
		}
	}

	/**
	 * Find the spanning tree using a DFS starting from v.
	 *
	 * @param v       -
	 *                the vertex to start the search from
	 * @param visitor -
	 *                visitor invoked after each vertex is visited and an edge is added
	 *                to the tree.
	 */
	public void dfsSpanningTree(Vertex<T, S> v, DFSVisitor<T, S> visitor)
	{
		v.visit();
		if (visitor != null)
			visitor.visit(this, v);

		for (int i = 0; i < v.getOutgoingEdgeCount(); i++)
		{
			Arrow<T, S> e = v.getOutgoingEdge(i);
			if (!e.getTo().visited())
			{
				if (visitor != null)
					visitor.visit(this, v, e);
				e.mark();
				dfsSpanningTree(e.getTo(), visitor);
			}
		}
	}

	/**
	 * Search the verticies for one with name.
	 *
	 * @param name -
	 *             the vertex name
	 * @return the first vertex with a matching name, null if no matches are found
	 */
	public Vertex<T, S> findVertexByName(String name)
	{
		Vertex<T, S> match = null;
		for (Vertex<T, S> v : verticies)
		{
			if (name.equals(v.getName()))
			{
				match = v;
				break;
			}
		}
		return match;
	}

	/**
	 * Search the verticies for one with data.
	 *
	 * @param data    -
	 *                the vertex data to match
	 * @param compare -
	 *                the comparator to perform the match
	 * @return the first vertex with a matching data, null if no matches are found
	 */
	public Vertex<T, S> findNodeByData(T data, Comparator<T> compare)
	{
		Vertex<T, S> match = null;
		for (Vertex<T, S> v : verticies)
		{
			if (compare.compare(data, v.getData()) == 0)
			{
				match = v;
				break;
			}
		}
		return match;
	}

	/**
	 * Search the verticies with data.
	 *
	 * @param data    -
	 *                the vertex data to match
	 * @param compare -
	 *                the comparator to perform the match
	 * @return all vertex with a matching data, empty list if no matches are found
	 */
	public List<Vertex<T, S>> findVerticiesByData(T data, Comparator<T> compare)
	{
		List<Vertex<T, S>> matches = new LinkedList<Vertex<T, S>>();
		for (Vertex<T, S> v : verticies)
		{
			if (compare.compare(data, v.getData()) == 0)
			{
				matches.add(v);
			}
		}
		return matches;
	}

	/**
	 * Search the graph for cycles. In order to detect cycles, we use a modified
	 * depth first search called a colored DFS. All nodes are initially marked
	 * white. When a node is encountered, it is marked grey, and when its
	 * descendants are completely visited, it is marked black. If a grey node is
	 * ever encountered, then there is a cycle.
	 *
	 * @return the edges that form cycles in the graph. The array will be empty if
	 * there are no cycles.
	 */
	public Arrow<T, S>[] findCycles()
	{
		ArrayList<Arrow<T, S>> cycleEdges = new ArrayList<Arrow<T, S>>();
		// Mark all verticies as white
		for (int n = 0; n < verticies.size(); n++)
		{
			Vertex<T, S> v = getVertex(n);
			v.setMarkState(VISIT_COLOR_WHITE);
		}
		for (int n = 0; n < verticies.size(); n++)
		{
			Vertex<T, S> v = getVertex(n);
			visit(v, cycleEdges);
		}

		Arrow<T, S>[] cycles = new Arrow[cycleEdges.size()];
		cycleEdges.toArray(cycles);
		return cycles;
	}

	private void visit(Vertex<T, S> v, ArrayList<Arrow<T, S>> cycleEdges)
	{
		v.setMarkState(VISIT_COLOR_GREY);
		int count = v.getOutgoingEdgeCount();
		for (int n = 0; n < count; n++)
		{
			Arrow<T, S> e = v.getOutgoingEdge(n);
			Vertex<T, S> u = e.getTo();
			if (u.getMarkState() == VISIT_COLOR_GREY)
			{
				// A cycle Edge<T, S>
				cycleEdges.add(e);
			} else if (u.getMarkState() == VISIT_COLOR_WHITE)
			{
				visit(u, cycleEdges);
			}
		}
		v.setMarkState(VISIT_COLOR_BLACK);
	}

	public String toString()
	{
		StringBuffer tmp = new StringBuffer("Graph[");
		for (Vertex<T, S> v : verticies)
			tmp.append(v);
		tmp.append(']');
		return tmp.toString();
	}
}