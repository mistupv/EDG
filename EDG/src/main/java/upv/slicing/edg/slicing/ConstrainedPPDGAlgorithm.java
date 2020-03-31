package upv.slicing.edg.slicing;

import upv.slicing.edg.constraint.Constraints;
import upv.slicing.edg.constraint.NodeConstraint;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.work.EdgeWork;
import upv.slicing.edg.work.NodeWork;
import upv.slicing.edg.work.Work;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class ConstrainedPPDGAlgorithm extends ConstrainedAlgorithm
{
    public ConstrainedPPDGAlgorithm(EDG edg) {
        super(edg);
    }

    @Override
    protected List<Work> processWork(Phase phase, NodeWork work)
    {
        final List<Work> newWorks = new LinkedList<>();
        final Node initialNode = work.getInitialNode();
        final Node currentNode = work.getCurrentNode();
        final Constraints constraints = work.getConstraints();
        final Set<NodeConstraint> nodeConstraints = constraints.getNodeConstraints();

        // PPDG Traversal limiting pseudo-predicates
        if (isPseudoPredicate(currentNode) && initialNode != currentNode)
            return newWorks;

        final Set<Edge> edges = edg.incomingEdgesOf(currentNode);
        edges.removeIf(Edge::isControlFlowEdge);
        if(phase == Phase.SummaryGeneration)
            edges.removeIf(edge -> edge.getType() == Edge.Type.Exception);

        for (NodeConstraint nodeConstraint : nodeConstraints)
            nodeConstraint.resolve(phase, edges);

        final Constraints constraintsClone = (Constraints) constraints.clone();
        constraintsClone.clearNodeConstraints();
        for (Edge edge : edges)
            newWorks.add(new EdgeWork(edg, initialNode, edge, constraintsClone));

        return newWorks;
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
