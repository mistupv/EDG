package upv.slicing.edg.constraint;

import java.util.List;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.slicing.Phase;

public abstract class NodeConstraint extends Constraint
{
	public abstract void resolve(Phase phase, List<Edge> edges);
}