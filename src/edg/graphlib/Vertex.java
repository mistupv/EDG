package edg.graphlib;

import java.util.ArrayList;
import java.util.List;

/*
 * JBoss, Home of Professional Open Source Copyright 2006, Red Hat Middleware
 * LLC, and individual contributors by the @authors tag. See the copyright.txt
 * in the distribution for a full listing of individual contributors.
 * 
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this software; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA, or see the FSF
 * site: http://www.fsf.org.
 */

/**
 * A named graph vertex with optional data.
 * 
 * @author Scott.Stark@jboss.org
 * @version $Revision$
 * @param <T, S>
 */
public class Vertex<T, S> {
  private List<Arrow<T, S>> incomingEdges;

  private List<Arrow<T, S>> outgoingEdges;

  private String name;

  private boolean mark;

  private int markState;

  private T data;

  /**
   * Calls this(null, null).
   */
  public Vertex() {
    this(null, null);
  }

  /**
   * Create a vertex with the given name and no data
   * 
   * @param n
   */
  public Vertex(String n) {
    this(n, null);
  }

  /**
   * Create a Vertex with name n and given data
   * 
   * @param n -
   *          name of vertex
   * @param data -
   *          data associated with vertex
   */
  public Vertex(String n, T data) {
    incomingEdges = new ArrayList<Arrow<T, S>>();
    outgoingEdges = new ArrayList<Arrow<T, S>>();
    name = n;
    mark = false;
    this.data = data;
  }

  /**
   * @return the possibly null name of the vertex
   */
  public String getName() {
    return name;
  }

  /**
   * @return the possibly null data of the vertex
   */
  public T getData() {
    return this.data;
  }

  /**
   * @param data
   *          The data to set.
   */
  public void setData(T data) {
    this.data = data;
  }

  /**
   * Add an edge to the vertex. If edge.from is this vertex, its an outgoing
   * edge. If edge.to is this vertex, its an incoming edge. If neither from or
   * to is this vertex, the edge is not added.
   * 
   * @param e -
   *          the edge to add
   * @return true if the edge was added, false otherwise
   */
  public boolean addEdge(Arrow<T, S> e) {
    if (e.getFrom() == this)
      outgoingEdges.add(e);
    else if (e.getTo() == this)
      incomingEdges.add(e);
    else
      return false;
    return true;
  }

  /**
   * Add an outgoing edge ending at to.
   * 
   * @param to -
   *          the destination vertex
   * @param cost
   *          the edge cost
   */
  public void addOutgoingEdge(Vertex<T, S> to, int cost) {
    Arrow<T, S> out = new Arrow<T, S>(this, to, cost);
    outgoingEdges.add(out);
  }

  /**
   * Add an incoming edge starting at from
   * 
   * @param from -
   *          the starting vertex
   * @param cost
   *          the edge cost
   */
  public void addIncomingEdge(Vertex<T, S> from, int cost) {
    Arrow<T, S> out = new Arrow<T, S>(this, from, cost);
    incomingEdges.add(out);
  }

  /**
   * Check the vertex for either an incoming or outgoing edge mathcing e.
   * 
   * @param e
   *          the edge to check
   * @return true it has an edge
   */
  public boolean hasEdge(Arrow<T, S> e) {
    if (e.getFrom() == this)
      return incomingEdges.contains(e);
    else if (e.getTo() == this)
      return outgoingEdges.contains(e);
    else
      return false;
  }

  /**
   * Remove an edge from this vertex
   * 
   * @param e -
   *          the edge to remove
   * @return true if the edge was removed, false if the edge was not connected
   *         to this vertex
   */
  public boolean remove(Arrow<T, S> e) {
    if (e.getFrom() == this)
      outgoingEdges.remove(e);
    else if (e.getTo() == this)
      incomingEdges.remove(e);
    else
      return false;
    return true;
  }

  /**
   * 
   * @return the count of incoming edges
   */
  public int getIncomingEdgeCount() {
    return incomingEdges.size();
  }

  /**
   * Get the ith incoming edge
   * 
   * @param i
   *          the index into incoming edges
   * @return ith incoming edge
   */
  public Arrow<T, S> getIncomingEdge(int i) {
    return incomingEdges.get(i);
  }

  /**
   * Get the incoming edges
   * 
   * @return incoming edge list
   */
  public List<Arrow<T, S>> getIncomingArrows() {
    return this.incomingEdges;
  }

  /**
   * 
   * @return the count of incoming edges
   */
  public int getOutgoingEdgeCount() {
    return outgoingEdges.size();
  }

  /**
   * Get the ith outgoing edge
   * 
   * @param i
   *          the index into outgoing edges
   * @return ith outgoing edge
   */
  public Arrow<T, S> getOutgoingEdge(int i) {
    return outgoingEdges.get(i);
  }

  /**
   * Get the outgoing edges
   * 
   * @return outgoing edge list
   */
  public List<Arrow<T, S>> getOutgoingArrows() {
    return this.outgoingEdges;
  }

  /**
   * Search the outgoing edges looking for edges whose's edge.to == dest.
   * 
   * @param dest
   *          the destination
   * @return the outgoing edges going to dest if one exists, empty list otherwise.
   */
  public List<Arrow<T, S>> findEdges(Vertex<T, S> dest) {
    List<Arrow<T, S>> edges = new ArrayList<Arrow<T, S>>();
    for (Arrow<T, S> e : outgoingEdges) {
      if (e.getTo() == dest)
        edges.add(e);
    }
    return edges;
  }

  /**
   * Search the outgoing edges for a match to e.
   * 
   * @param e -
   *          the edge to check
   * @return e if its a member of the outgoing edges, null otherwise.
   */
  public Arrow<T, S> findEdge(Arrow<T, S> e) {
    if (outgoingEdges.contains(e))
      return e;
    else
      return null;
  }

  /**
   * What is the cost from this vertex to the dest vertex.
   * 
   * @param dest -
   *          the destination vertex.
   * @return Return Integer.MAX_VALUE if we have no edge to dest, 0 if dest is
   *         this vertex, the cost of the outgoing edge otherwise.
   */
  public int cost(Vertex<T, S> dest) {
    if (dest == this)
      return 0;

    int cost = Integer.MAX_VALUE;
    List<Arrow<T, S>> edges = findEdges(dest);
    for (Arrow<T, S> e : edges)
      cost = Math.min(cost, e.getCost());
    return cost;
  }

  /**
   * Is there an outgoing edge ending at dest.
   * 
   * @param dest -
   *          the vertex to check
   * @return true if there is an outgoing edge ending at vertex, false
   *         otherwise.
   */
  public boolean hasEdge(Vertex<T, S> dest) {
    return !findEdges(dest).isEmpty();
  }

  /**
   * Has this vertex been marked during a visit
   * 
   * @return true is visit has been called
   */
  public boolean visited() {
    return mark;
  }

  /**
   * Set the vertex mark flag.
   * 
   */
  public void mark() {
    mark = true;
  }

  /**
   * Set the mark state to state.
   * 
   * @param state
   *          the state
   */
  public void setMarkState(int state) {
    markState = state;
  }

  /**
   * Get the mark state value.
   * 
   * @return the mark state
   */
  public int getMarkState() {
    return markState;
  }

  /**
   * Visit the vertex and set the mark flag to true.
   * 
   */
  public void visit() {
    mark();
  }

  /**
   * Clear the visited mark flag.
   * 
   */
  public void clearMark() {
    mark = false;
  }

  /**
   * @return a string form of the vertex with in and out edges.
   */
  public String toString() {
    StringBuffer tmp = new StringBuffer("Vertex(");
    tmp.append(name);
    tmp.append(", data=");
    tmp.append(data);
    tmp.append("), in:[");
    for (int i = 0; i < incomingEdges.size(); i++) {
      Arrow<T, S> e = incomingEdges.get(i);
      if (i > 0)
        tmp.append(',');
      tmp.append('{');
      tmp.append(e.getFrom().name);
      tmp.append(',');
      tmp.append(e.getCost());
      tmp.append('}');
    }
    tmp.append("], out:[");
    for (int i = 0; i < outgoingEdges.size(); i++) {
      Arrow<T, S> e = outgoingEdges.get(i);
      if (i > 0)
        tmp.append(',');
      tmp.append('{');
      tmp.append(e.getTo().name);
      tmp.append(',');
      tmp.append(e.getCost());
      tmp.append('}');
    }
    tmp.append(']');
    return tmp.toString();
  }
}