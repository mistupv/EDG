package edg.constraint;

public class ExceptionConstraint extends SeekingConstraint {

	private String field;
	
	public ExceptionConstraint(Operation operation) 
	{
		super(operation, SeekingConstraint.CompositeType.Exception);
		this.field = "";
	}
	
	public SeekingConstraint opposite() {
		if (this.operation == Operation.Add)
			return new ExceptionConstraint(Operation.Remove);
		if (this.operation == Operation.Remove)
			return new ExceptionConstraint(Operation.Add);
		return new ExceptionConstraint(Operation.LetThrough);
	}
	
	public boolean letThrough(SeekingConstraint constraint)
	{
		if (!(constraint instanceof ExceptionConstraint))
			return false;

		final ExceptionConstraint exConstraint = (ExceptionConstraint) constraint;

		return super.letThrough(exConstraint);
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof ExceptionConstraint))
			return false;

		final ExceptionConstraint constraint = (ExceptionConstraint) object;

		return super.equals(constraint);
	}
	public String toString()
	{
		return super.toString() + this.field;
	}	
}
	
	
	
//	
//	
//	
//	
//	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, int productionDepth)
//	{
//		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
//		
//		if (this.operation == Operation.Add)
//		{
//			constraintsStack.clear();
//			constraintsStack.push(this);
//			constraintsStacks.add(constraintsStack);
//		}
//		
//		return constraintsStacks;
//	}
//	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, AccessConstraint accessConstraint, int productionDepth)
//	{
//		return this.resolve(phase, constraintsStack, edge, productionDepth);
//	}
//	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SeekingConstraint topConstraint, int productionDepth)
//	{
//		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
//		
//		if (topConstraint instanceof ExceptionConstraint)
//		{
//			constraintsStacks.add(constraintsStack);
//			final Constraints emptyStack = this.getAllWorks(edge);
//			if (emptyStack != null)
//				constraintsStacks.add(emptyStack);
//		}	
//
//		return constraintsStacks;
//	}
//	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, StarConstraint topConstraint, int productionDepth)
//	{
//		return this.resolve(phase, constraintsStack, edge, productionDepth);
//	}
//	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, SummaryConstraint topConstraint, int productionDepth)
//	{
//		return this.resolve(phase, constraintsStack, edge, productionDepth);
//	}
//	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, UnresolvableConstraint topConstraint, int productionDepth)
//	{
//		return this.resolve(phase, constraintsStack, edge, productionDepth);
//	}
//	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, Edge edge, EmptyConstraint topConstraint, int productionDepth)
//	{
//		return this.resolve(phase, constraintsStack, edge, productionDepth);
//	}
//
//	private Constraints getAllWorks(Edge edge)
//	{
//		final Node nodeFrom = edge.getFrom();
//		final List<Edge> incomingEdges = nodeFrom.getIncomingEdges();
//		for (Edge incomingEdge : incomingEdges)
//		{
//			final EdgeInfo.Type edgeType = incomingEdge.getData().getType();
//			if (edgeType != EdgeInfo.Type.ExceptionGetAll && edgeType != EdgeInfo.Type.NormalControl  
//					&& edgeType != EdgeInfo.Type.StructuralControl && edgeType != EdgeInfo.Type.ValueDependence)
//				return new Constraints();
//		}
//		return null;
//	}
//
//
