package edg.work;

import edg.constraint.Constraints;
import edg.graph.Node;

public abstract class Work
{
	protected String id;
	protected final Node initialNode;
	protected final Constraints constraints;

	public Work(Node initialNode, Constraints constraints)
	{
		this.initialNode = initialNode;
		this.constraints = constraints;
	}

	public String getId()
	{
		return this.id;
	}
	public Node getInitialNode()
	{
		return this.initialNode;
	}
	public Constraints getConstraints()
	{
		return this.constraints;
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof Work))
			return false;

		final Work work = (Work) object;

		// I think the formal out is not important
//		if (this.initialNode != work.initialNode)
//			return false;
		if (!this.constraints.equals(work.constraints))
			return false;
		return true;
	}
	public int hashCode()
	{
		return this.id.hashCode() + this.constraints.hashCode();
	}
}