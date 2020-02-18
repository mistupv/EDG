package edg.graphlib;

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
 * A directed, weighted edge in a graph
 * 
 * @author Scott.Stark@jboss.org
 * @version $Revision$
 * @param <T>
 */
public class Arrow<T, S> {
  private Vertex<T, S> from;

  private Vertex<T, S> to;

  private int cost;

  private boolean mark;

  private S data;

  /**
   * Create a zero cost edge between from and to
   * 
   * @param from
   *          the starting vertex
   * @param to
   *          the ending vertex
   */
  public Arrow(Vertex<T, S> from, Vertex<T, S> to) {
    this(from, to, 0, null);
  }

  /**
   * Create a zero cost edge between from and to with given data
   * 
   * @param from
   *          the starting vertex
   * @param to
   *          the ending vertex
   * @param data
   *          data associated with edge
   */
  public Arrow(Vertex<T, S> from, Vertex<T, S> to, S data) {
	  this(from, to, 0, data);
  }

  /**
   * Create an edge between from and to with the given cost.
   * 
   * @param from
   *          the starting vertex
   * @param to
   *          the ending vertex
   * @param cost
   *          the cost of the edge
   */
  public Arrow(Vertex<T, S> from, Vertex<T, S> to, int cost) {
	  this(from, to, cost, null);
  }

  /**
   * Create an edge between from and to with the given cost and data.
   * 
   * @param from
   *          the starting vertex
   * @param to
   *          the ending vertex
   * @param cost
   *          the cost of the edge
   * @param data
   *          data associated with edge
   */
  public Arrow(Vertex<T, S> from, Vertex<T, S> to, int cost, S data) {
	    this.from = from;
	    this.to = to;
	    this.cost = cost;
	    this.data = data;
	    mark = false;
  }

  /**
   * Get the ending vertex
   * 
   * @return ending vertex
   */
  public Vertex<T, S> getTo() {
    return to;
  }

  /**
   * Get the ending vertex
   * 
   * @param new ending vertex
   * 
   */
  
  public void updateTo(Vertex<T, S> newTo) { // ADDED
	    to = newTo;
	  }

  /**
   * Get the starting vertex
   * 
   * @return starting vertex
   */
  public Vertex<T, S> getFrom() {
    return from;
  }

  /**
   * Get the cost of the edge
   * 
   * @return cost of the edge
   */
  public int getCost() {
    return cost;
  }

  /**
   * @return the possibly null data of the vertex
   */
  public S getData() {
    return this.data;
  }

  /**
   * @param data
   *          The data to set.
   */
  public void setData(S data) { 
    this.data = data;
  }

  /**
   * Set the mark flag of the edge
   * 
   */
  public void mark() {
    mark = true;
  }

  /**
   * Clear the edge mark flag
   * 
   */
  public void clearMark() {
    mark = false;
  }

  /**
   * Get the edge mark flag
   * 
   * @return edge mark flag
   */
  public boolean isMarked() {
    return mark;
  }

  /**
   * String rep of edge
   * 
   * @return string rep with from/to vertex names and cost
   */
  public String toString() {
    StringBuffer tmp = new StringBuffer("Edge[from: ");
    tmp.append(from.getName());
    tmp.append(",to: ");
    tmp.append(to.getName());
    tmp.append(", cost: ");
    tmp.append(cost);
    tmp.append("]");
    return tmp.toString();
  }
}