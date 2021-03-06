package upv.slicing.edg.slicing;

import upv.slicing.edg.graph.Node;

import java.util.Set;

public interface SlicingAlgorithm
{
	/** Obtain a slice from an initial node. */
	Set<Node> slice(Node node);
}