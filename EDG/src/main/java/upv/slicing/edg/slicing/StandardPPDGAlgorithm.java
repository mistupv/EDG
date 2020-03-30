package upv.slicing.edg.slicing;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;

import java.util.*;

public class StandardPPDGAlgorithm {
    protected final EDG edg;

    public StandardPPDGAlgorithm(EDG edg)
    {
        this.edg = edg;
    }

    public Set<Node> slice(Node slicingCriterion)
    {
        final Set<Node> slice = new HashSet<>();
        if (slicingCriterion == null)
            return slice;

        slice.add(slicingCriterion);
        this.traverse(slicingCriterion, slice, Edge.Type.Output);
        this.traverse(slicingCriterion, slice, Edge.Type.Input);

        return slice;
    }

    private void traverse(Node slicingCriterion, Set<Node> slice, Edge.Type... ignoreEdgeTypes)
    {
        final Deque<Node> pendingNodes = new LinkedList<>(slice);
        final Set<Edge.Type> ignoreEdgeTypesSet = new HashSet<>(Arrays.asList(ignoreEdgeTypes));
        ignoreEdgeTypesSet.add(Edge.Type.ControlFlow);
        ignoreEdgeTypesSet.add(Edge.Type.NonExecControlFlow);

        while (!pendingNodes.isEmpty())
        {
            final Node pendingNode = pendingNodes.removeFirst();

            // PPDG Traversal limiting pseudo-predicates
            if (isPseudoPredicate(pendingNode) && slicingCriterion != pendingNode)
                continue;

            final Set<Edge> incomingEdges = edg.incomingEdgesOf(pendingNode);

            incomingEdges.removeIf(e -> ignoreEdgeTypesSet.contains(e.getType()));
            incomingEdges.removeIf(e -> !e.isTraversable());
            for (Edge incomingEdge : incomingEdges)
            {
                final Node nodeFrom = edg.getEdgeSource(incomingEdge);
                if (!slice.contains(nodeFrom))
                {
                    pendingNodes.addLast(nodeFrom);
                    slice.add(nodeFrom);
                }
            }
        }
    }

    private boolean isPseudoPredicate(Node n)
    {
        switch(n.getType())
        {
            case Return:
            case Continue:
            case Break:
                return true;
            default:
                return false;
        }
    }
}
