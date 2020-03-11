package upv.slicing.edg.work;

import upv.slicing.edg.constraint.Constraints;
import upv.slicing.edg.graph.Node;

public class NodeWork extends Work
{
	private final Node currentNode;

	public NodeWork(Node initialNode, Node currentNode, Constraints constraints)
	{
		super(initialNode, constraints);

		this.id = currentNode.getId() + "";
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
		return this.currentNode == nodeWork.currentNode;
	}
}