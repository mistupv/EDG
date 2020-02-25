package upv.slicing.edg.constraint;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.slicing.Phase;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class IgnoreEdgeConstraint extends NodeConstraint {
	private final List<Edge.Type> types = new LinkedList<>();

	public IgnoreEdgeConstraint(Edge.Type type, Edge.Type... types)
	{
		this.types.add(type);
		this.types.addAll(Arrays.asList(types));
	}

	public void resolve(Phase phase, Set<Edge> edges)
	{
		edges.removeIf(edge -> this.types.contains(edge.getType()));
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof IgnoreEdgeConstraint))
			return false;

		final IgnoreEdgeConstraint constraint = (IgnoreEdgeConstraint) object;

		return this.types.equals(constraint.types);
	}
	public String toString()
	{
		StringBuilder str = new StringBuilder();

		if (types.size() > 0)
			str.append(types.get(0).name());
		for (int i = 1; i < types.size(); i++)
			str.append(", ").append(types.get(i).name());

		return "I-[" + str.toString() + "]";
	}
}