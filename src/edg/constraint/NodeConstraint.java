package edg.constraint;

import java.util.List;

import edg.graph.Edge;
import edg.slicing.Phase;

public abstract class NodeConstraint extends Constraint
{
	public abstract void resolve(Phase phase, List<Edge> edges);
}