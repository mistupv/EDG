package upv.slicing.edg.graph;

import upv.slicing.edg.constraint.AsteriskConstraint;
import upv.slicing.edg.constraint.EdgeConstraint;
import upv.slicing.edg.constraint.EmptyConstraint;

import java.util.Objects;
import java.util.Random;

public class Edge
{
	public enum Type {
		ControlFlow, NonExecControlFlow,
		Structural, Control,
		Value, Flow,
		Call, Input, Output, Summary,
		Exception
	}

	private boolean mark;
	private final Type type;
	private final EdgeConstraint constraint;
	protected boolean traversable = true;
	private final int hash;

	public Edge(Type type)
	{
		this(type, type == Edge.Type.Structural || type == Edge.Type.Control ? AsteriskConstraint.getConstraint() : EmptyConstraint.getConstraint());
	}

	public Edge(Type type, EdgeConstraint constraint)
	{
		Objects.requireNonNull(type, "Edge type can't be null!");
		Objects.requireNonNull(constraint, "Constraint can't be null, use 'EmptyConstraint#getConstraint()'");
		this.type = type;
		this.constraint = constraint;
		this.hash = new Random().nextInt();
	}

	public void mark()
	{
		mark = true;
	}

	public boolean isMarked()
	{
		return mark;
	}

	public Type getType()
	{
		return type;
	}

	public EdgeConstraint getConstraint()
	{
		return constraint;
	}

	public boolean isStructuralEdge()
	{
		return false;
	}

	public boolean isTraversable()
	{
		return traversable;
	}

	@Override
	public String toString()
	{
		String str = "type=" + type;
		if (constraint != null)
			str += ",constraint=" + constraint;
		return getClass().getSimpleName() + "[" + str + "]";
	}

	@Override
	public boolean equals(Object o)
	{
		return this == o;
//		if (this == o) return true;
//		if (o == null || getClass() != o.getClass())
//			return false;
//		Edge edge = (Edge) o;
//		return mark == edge.mark &&
//				type == edge.type &&
//				constraint.equals(edge.constraint);
	}

	@Override
	public int hashCode()
	{
		return hash;
	}

	public static class NonTraversable extends Edge
	{
		public NonTraversable(Edge edge)
		{
			super(edge.getType(), edge.getConstraint());
			mark();
			traversable = false;
		}

		@Override
		public boolean isStructuralEdge() {
			return true;
		}
	}
}
