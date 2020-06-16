package upv.slicing.edg.slicing;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST.Direction;
import upv.slicing.edg.graph.Node;

import java.util.*;

public class StandardPPDGAlgorithm extends StandardAlgorithm{

    public StandardPPDGAlgorithm(EDG edg) {
        super(edg);
    }

    @Override
    protected void traverse(Node slicingCriterion, Set<Node> slice, Edge.Type... ignoreEdgeTypes)
    {
        final Deque<Node> pendingNodes = new LinkedList<>(slice);
        final Set<Edge.Type> ignoreEdgeTypesSet = new HashSet<>(Arrays.asList(ignoreEdgeTypes));

        while (!pendingNodes.isEmpty())
        {
            final Node pendingNode = pendingNodes.removeFirst();

            // PPDG Traversal limiting pseudo-predicates for Control Edges
            boolean pseudoPredicate = isPseudoPredicate(pendingNode) && slicingCriterion != pendingNode;

            final Set<Edge> nextEdges = edg.getEdges(pendingNode, sliceDirection);
            if (pseudoPredicate)
                nextEdges.removeIf(edge -> edge.getType() == Edge.Type.Control);

            nextEdges.removeIf(e -> ignoreEdgeTypesSet.contains(e.getType()));
            nextEdges.removeIf(Edge::isControlFlowEdge);
            nextEdges.removeIf(e -> !e.isTraversable());
            for (Edge nextEdge : nextEdges)
            {
                final Node nextNode = sliceDirection == Direction.Backwards ?
                        edg.getEdgeSource(nextEdge): edg.getEdgeTarget(nextEdge);
                if (!slice.contains(nextNode))
                {
                    pendingNodes.addLast(nextNode);
                    slice.add(nextNode);
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
