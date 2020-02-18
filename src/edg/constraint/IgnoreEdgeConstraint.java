package edg.constraint;

import java.util.LinkedList;
import java.util.List;

import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.slicing.Phase;

public class IgnoreEdgeConstraint extends NodeConstraint
{
	private final List<EdgeInfo.Type> types = new LinkedList<EdgeInfo.Type>();

	public IgnoreEdgeConstraint(EdgeInfo.Type type, EdgeInfo.Type... types)
	{
		this.types.add(type);
		for (EdgeInfo.Type type0 : types)
			this.types.add(type0);
	}

	public void resolve(Phase phase, List<Edge> edges)
	{
		edges.removeIf(edge -> this.types.contains(edge.getData().getType()));
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof IgnoreEdgeConstraint))
			return false;

		final IgnoreEdgeConstraint constraint = (IgnoreEdgeConstraint) object;

		if (!this.types.equals(constraint.types))
			return false;
		return true;
	}
	public String toString()
	{
		String toString = "";

		for (EdgeInfo.Type type : this.types)
			toString += type.name() + ",";
		if (!toString.isEmpty())
			toString = toString.substring(0, toString.length() - 1);

		return "I-[" + toString + "]";
	}
}