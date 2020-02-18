package eknife.edg.constraint;

import java.util.LinkedList;
import java.util.List;

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
	
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, int productionDepth)
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
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, AccessConstraint accessConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SeekingConstraint topConstraint, int productionDepth)
	{
		final List<Constraints> constraintsStacks = new LinkedList<Constraints>();
		
		if (topConstraint instanceof GetAllConstraint)
			constraintsStacks.add(constraintsStack);

		return constraintsStacks;
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, StarConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, SummaryConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, UnresolvableConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}
	public List<Constraints> resolve(Phase phase, Constraints constraintsStack, EmptyConstraint topConstraint, int productionDepth)
	{
		return this.resolve(phase, constraintsStack, productionDepth);
	}

}
