package upv.slicing.edg.slicing;

import java.util.List;

import upv.slicing.edg.graph.Node;

public interface SlicingAlgorithm
{
	List<Node> slice(Node node);
}