package upv.slicing.edg.work;

import upv.slicing.edg.constraint.Constraints;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;

public class NodeWork extends Work
{
	private final Node currentNode;
	private final Edge.Type previousEdgeType;

	public NodeWork(Node initialNode, Node currentNode, Constraints constraints)
	{
		super(initialNode, constraints);

		this.id = currentNode.getId() + "";
		this.currentNode = currentNode;
		this.previousEdgeType = null;
	}

	public NodeWork(Node initialNode, Node currentNode, Constraints constraints, Edge.Type type)
	{
		super(initialNode, constraints);

		this.id = currentNode.getId() + "";
		this.currentNode = currentNode;
		this.previousEdgeType = type;
	}

	public Node getCurrentNode()
	{
		return this.currentNode;
	}

	public Edge.Type getPreviousEdgeType() { return this.previousEdgeType; }

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof NodeWork))
			return false;

		final NodeWork nodeWork = (NodeWork) object;

		if (!super.equals(nodeWork))
			return false;
		return this.currentNode == nodeWork.currentNode;
	}
}