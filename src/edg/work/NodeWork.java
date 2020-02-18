package edg.work;

import edg.constraint.Constraints;
import edg.graph.Node;

public class NodeWork extends Work
{
	private final Node currentNode;

	public NodeWork(Node initialNode, Node currentNode, Constraints constraints)
	{
		super(initialNode, constraints);

		this.id = currentNode.getData().getId() + "";
		this.currentNode = currentNode;
	}

	public Node getCurrentNode()
	{
		return this.currentNode;
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof NodeWork))
			return false;

		final NodeWork nodeWork = (NodeWork) object;

		if (!super.equals(nodeWork))
			return false;
		if (this.currentNode != nodeWork.currentNode)
			return false;
		return true;
	}
}