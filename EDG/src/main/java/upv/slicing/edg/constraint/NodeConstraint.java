package upv.slicing.edg.constraint;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.slicing.Phase;

import java.util.Set;

public abstract class NodeConstraint extends Constraint
{
	public abstract void resolve(Phase phase, Set<Edge> edges);
}