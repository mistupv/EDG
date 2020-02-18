package edg.slicing;

import java.util.List;

import edg.graph.Node;

public interface SlicingAlgorithm
{
	List<Node> slice(Node node);
}