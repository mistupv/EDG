package eknife.edg.util;

import eknife.edg.Node;
import eknife.edg.constraint.Constraints;
import eknife.edg.constraint.SlicingConstraints;

public class Work
{
	private final Node initialNode;
	private final Node currentNode;
	private final Constraints constraints;
	private final boolean ignoreUp;
	private final boolean ignoreDown;

	public Work(Node currentNode)
	{
		this(null, currentNode, new SlicingConstraints(), false, false);
	}
	public Work(Node currentNode, boolean ignoreUp, boolean ignoreDown)
	{
		this(null, currentNode, new SlicingConstraints(), ignoreUp, ignoreDown);
	}
	public Work(Node currentNode, Constraints constraints)
	{
		this(null, currentNode, constraints, false, false);
	}
	public Work(Node currentNode, Constraints constraints, boolean ignoreUp, boolean ignoreDown)
	{
		this(null, currentNode, constraints, ignoreUp, ignoreDown);
	}

	public Work(Node initialNode, Node currentNode)
	{
		this(initialNode, currentNode, new SlicingConstraints(), false, false);
	}
	public Work(Node initialNode, Node currentNode, boolean ignoreUp, boolean ignoreDown)
	{
		this(initialNode, currentNode, new SlicingConstraints(), ignoreUp, ignoreDown);
	}
	public Work(Node initialNode, Node currentNode, Constraints constraints)
	{
		this(initialNode, currentNode, constraints, false, false);
	}
	public Work(Node initialNode, Node currentNode, Constraints constraints, boolean ignoreUp, boolean ignoreDown)
	{
		this.initialNode = initialNode;
		this.currentNode = currentNode;
		this.constraints = constraints;
		this.ignoreUp = ignoreUp;
		this.ignoreDown = ignoreDown;
	}

	public Node getInitialNode()
	{
		return this.initialNode;
	}
	public Node getCurrentNode()
	{
		return this.currentNode;
	}
	public Constraints getConstraints()
	{
		return this.constraints;
	}
	public boolean getIgnoreUp()
	{
		return this.ignoreUp;
	}
	public boolean getIgnoreDown()
	{
		return this.ignoreDown;
	}

	public boolean equals(Object object)
	{
		if (!(object instanceof Work))
			return false;

		final Work work = (Work) object;

		// I think which is the formal out is not important
//		if (this.initialNode != work.initialNode)
//			return false;
		if (this.currentNode != work.currentNode)
			return false;
		if (this.constraints.size() != work.constraints.size())
			return false;
		if (!this.constraints.equals(work.constraints))
			return false;
		if (this.ignoreUp != work.ignoreUp)
			return false;
		if (this.ignoreDown != work.ignoreDown)
			return false;
		return true;
	}
}