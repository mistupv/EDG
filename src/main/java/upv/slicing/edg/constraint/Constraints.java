package upv.slicing.edg.constraint;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

public class Constraints {
	public enum Type {Node, Edge}

	protected final Set<NodeConstraint> nodeConstraints = new HashSet<NodeConstraint>();
	protected final Stack<EdgeConstraint> edgeConstraints = new Stack<EdgeConstraint>();

	public Set<NodeConstraint> getNodeConstraints()
	{
		return this.nodeConstraints;
	}

	public void clearNodeConstraints()
	{
		this.nodeConstraints.clear();
	}

	public EdgeConstraint pushEdgeConstraint(EdgeConstraint constraint)
	{
		return this.edgeConstraints.push(constraint);
	}
	public EdgeConstraint popEdgeConstraint()
	{
		return this.edgeConstraints.pop();
	}
	public EdgeConstraint peekEdgeConstraint()
	{
		return this.edgeConstraints.peek();
	}
	public Stack<EdgeConstraint> getEdgeConstraints()
	{
		return this.edgeConstraints;
	}
	public EdgeConstraint getEdgeConstraint(int index)
	{
		return this.edgeConstraints.get(index);
	}

	public boolean isEdgeConstraintsEmpty()
	{
		return this.edgeConstraints.isEmpty();
	}
	public int sizeEdgeConstraints()
	{
		return this.edgeConstraints.size();
	}

	public Object clone()
	{
		final Constraints constraints = new Constraints();

		constraints.nodeConstraints.addAll(this.nodeConstraints);
		constraints.edgeConstraints.addAll(this.edgeConstraints);

		return constraints;
	}
	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof Constraints))
			return false;

		final Constraints constraints = (Constraints) object;

		if (this.nodeConstraints.size() != constraints.nodeConstraints.size())
			return false;
		if (this.edgeConstraints.size() != constraints.edgeConstraints.size())
			return false;
		if (!this.nodeConstraints.equals(constraints.nodeConstraints))
			return false;
		return this.edgeConstraints.equals(constraints.edgeConstraints);
	}
	public int hashCode()
	{
		return this.nodeConstraints.size() + this.edgeConstraints.size();
	}
}