package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

import eknife.edg.Edge;
import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.traverser.EdgeTraverser.Phase;

public class GetAllConstraint extends SeekingConstraint {

	private String field;
	
	public GetAllConstraint(Operation operation) 
	{
		super(operation, SeekingConstraint.CompositeType.GetAll);
		this.field = "getAll";
	}
	public String toString()
	{
		return this.field;
	}
	
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		if (this.operation == Operation.Add)
		{
			constraintsStack.clear();
			constraintsStack.push(this);
			constraintsStacks.add(constraintsStack);
		}
		
		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, AccessConstraint accessConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SeekingConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		if (topConstraint instanceof GetAllConstraint)
		{
			constraintsStacks.add(constraintsStack);
			final Constraints emptyStack = this.getAllWorks(edge);
			if (emptyStack != null)
				constraintsStacks.add(emptyStack);
		}	

		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, StarConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SummaryConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, UnresolvableConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, EmptyConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, edge, productionDepth);
	}

	private Constraints getAllWorks(Edge edge)
	{
		final Node nodeFrom = edge.getFrom();
		final List<Edge> incomingEdges = nodeFrom.getIncomingEdges();
		for (Edge incomingEdge : incomingEdges)
		{
			final EdgeInfo.Type edgeType = incomingEdge.getData().getType();
			if (edgeType != EdgeInfo.Type.ExceptionGetAll && edgeType != EdgeInfo.Type.NormalControl  
					&& edgeType != EdgeInfo.Type.StructuralControl && edgeType != EdgeInfo.Type.ValueDependence)
				return new Constraints();
		}
		return null;
	}
}
