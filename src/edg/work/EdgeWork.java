package edg.work;

import edg.constraint.Constraints;
import edg.graph.Edge;
import edg.graph.Node;

public class EdgeWork extends Work
{
	private final Edge currentEdge;

	public EdgeWork(Node initialNode, Edge currentEdge, Constraints constraints)
	{
		super(initialNode, constraints);

		this.id = currentEdge.getFrom().getData().getId() + "->" + currentEdge.getTo().getData().getId();
		this.currentEdge = currentEdge;
	}

	public Edge getCurrentEdge()
	{
		return this.currentEdge;
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof EdgeWork))
			return false;

		final EdgeWork edgeWork = (EdgeWork) object;

		if (!super.equals(edgeWork))
			return false;
		if (this.currentEdge != edgeWork.currentEdge)
			return false;
		return true;
	}
}