package upv.slicing.edg.slicing;

import upv.slicing.edg.Config;
import upv.slicing.edg.graph.LAST.Direction;
import upv.slicing.edg.graph.Node;

import java.util.Set;

public interface SlicingAlgorithm
{
	Direction sliceDirection = Config.SLICE_DIRECTION;
	/** Obtain a slice from an initial node. */
	Set<Node> slice(Node node);
}