package edg.traverser;

import java.util.LinkedList;
import java.util.List;

import edg.constraint.AccessConstraint;
import edg.constraint.BinComprehensionConstraint;
import edg.constraint.Constraint;
import edg.constraint.Constraints;
import edg.constraint.ListComprehensionConstraint;
import edg.constraint.ListConstraint;
import edg.constraint.StarConstraint;
import edg.constraint.SummaryConstraint;
import edg.constraint.UnresolvableConstraint;
import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;

public class EdgeTraverser
{
	private static final int maxDepth = 5;
	private static final int maxStackSize = 9;

	private final EDG edg;
	private final boolean resolveSummary;
	private final boolean constraintsActivated;

	public EdgeTraverser(EDG edg, boolean resolveSummary, boolean constraintsActivated)
	{
		this.edg = edg;
		this.resolveSummary = resolveSummary;
		this.constraintsActivated = constraintsActivated;
	}

	public List<Constraints> traverseIncomingEdge(Edge edge, Constraints constraints)
	{
		final EdgeInfo.Type edgeType = edge.getData().getType();
		final Node nodeFrom = edge.getFrom();
		final NodeInfo.Type newNodeType = nodeFrom.getData().getType();
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

		// Going up using control edges empties the stack
		if (edgeType == EdgeInfo.Type.NormalControl)
			newConstraintsStacks.add(new Constraints());
		// Structural control edges that belong to expressions can only be traversed the other way round.
		// Traverse it this way means that one descendant is the slicing criterion, and we let go up emptying the stack.
		if (edgeType == EdgeInfo.Type.StructuralControl &&
			(newNodeType == NodeInfo.Type.TupleExpression || newNodeType == NodeInfo.Type.ListExpression ||
			 newNodeType == NodeInfo.Type.BinExpression || newNodeType == NodeInfo.Type.BinElementExpression))
			newConstraintsStacks.add(new Constraints());
		if (!newConstraintsStacks.isEmpty())
			return newConstraintsStacks;

		final Constraint constraint = edge.getData().getConstraint();
		return this.traverseEdge(constraints, constraint);
	}
	public List<Constraints> traverseOutgoingEdge(Edge edge, Constraints constraints)
	{
		final Node nodeFrom = edge.getFrom();
		final NodeInfo.Type nodeType = nodeFrom.getData().getType();
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

		// Traverse the edge this way can only be done using structural control edges that belong to expressions
		if (nodeType != NodeInfo.Type.TupleExpression && nodeType != NodeInfo.Type.ListExpression &&
			nodeType != NodeInfo.Type.BinExpression && nodeType != NodeInfo.Type.BinElementExpression)
			return newConstraintsStacks;

		final Constraint constraint = edge.getData().getConstraint();
		return this.traverseEdge(constraints, constraint);
	}
	private List<Constraints> traverseEdge(Constraints constraintsStack, Constraint constraint)
	{
		if (this.constraintsActivated)
			return this.traverseEdgeTreatingConstraints(constraintsStack, constraint);
		return this.traverseEdgeIgnoringConstraints(constraintsStack, constraint);
	}
	private List<Constraints> traverseEdgeTreatingConstraints(Constraints constraintsStack, Constraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

		if (constraint == null)
			newConstraintsStacks.add(constraintsStack);
		else if (!this.resolveSummary)
			newConstraintsStacks.add(this.resolveCollectingNonTerminals(constraintsStack, constraint));
		else
			newConstraintsStacks.addAll(this.resolveProcessingNonTerminals(constraintsStack, constraint, 0));
		if (newConstraintsStacks.size() == 1 && newConstraintsStacks.get(0) == null)
		{
			newConstraintsStacks.clear();
			if (this.resolveSummary)
				newConstraintsStacks.add(new Constraints());
		}

		return newConstraintsStacks;
	}
	private List<Constraints> traverseEdgeIgnoringConstraints(Constraints constraintsStack, Constraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

		newConstraintsStacks.add(new Constraints());

		return newConstraintsStacks;
	}

	private Constraints resolveCollectingNonTerminals(Constraints constraintsStack, Constraint constraint)
	{
		if (constraint instanceof AccessConstraint)
			return this.resolveAccessConstraint(constraintsStack, (AccessConstraint) constraint);
		if (constraint instanceof StarConstraint)
			return this.resolveStarConstraint(constraintsStack, (StarConstraint) constraint);
		if (constraint instanceof UnresolvableConstraint)
			return this.resolveUnresolvableConstraint(constraintsStack, (UnresolvableConstraint) constraint);
		if (constraint instanceof ListComprehensionConstraint)
			return this.resolveListComprehensionConstraint(constraintsStack, (ListComprehensionConstraint) constraint);
		if (constraint instanceof BinComprehensionConstraint)
			return this.resolveBinComprehensionConstraint(constraintsStack, (BinComprehensionConstraint) constraint);
		if (constraint instanceof SummaryConstraint)
			return this.resolveSummaryConstraint(constraintsStack, (SummaryConstraint) constraint);
		throw new RuntimeException("Type of constraint not contemplated");
	}
	private List<Constraints> resolveProcessingNonTerminals(Constraints constraintsStack, Constraint constraint, int depth)
	{
// FIXME Arreglame
if (constraintsStack.size() >= EdgeTraverser.maxStackSize)
{
	final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
//	newConstraintsStacks.add(null);
	newConstraintsStacks.add(constraintsStack);
	return newConstraintsStacks;
}

		if (constraint instanceof AccessConstraint)
			return this.resolveAccessConstraintAdapted(constraintsStack, (AccessConstraint) constraint);
		if (constraint instanceof UnresolvableConstraint)
			return this.resolveUnresolvableConstraintAdapted(constraintsStack, (UnresolvableConstraint) constraint);
		if (constraint instanceof StarConstraint)
			return this.resolveStarConstraintAdapted(constraintsStack, (StarConstraint) constraint);
		if (constraint instanceof ListComprehensionConstraint)
			return this.resolveListComprehensionConstraintAdapted(constraintsStack, (ListComprehensionConstraint) constraint);
		if (constraint instanceof BinComprehensionConstraint)
			return this.resolveBinComprehensionConstraintAdapted(constraintsStack, (BinComprehensionConstraint) constraint);

		if (depth == EdgeTraverser.maxDepth)
		{
			final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
			newConstraintsStacks.add(null);
			return newConstraintsStacks;
		}

		if (constraint instanceof SummaryConstraint)
			return this.resolveSummaryConstraintAdapted(constraintsStack, (SummaryConstraint) constraint, depth);
		throw new RuntimeException("Type of constraint not contemplated");
	}

	private Constraints resolveAccessConstraint(Constraints constraintsStack, AccessConstraint constraint)
	{
		if (constraintsStack.isEmpty())
		{
			final Constraints newConstraintsStack = new Constraints();
			newConstraintsStack.push(constraint);
			return newConstraintsStack;
		}

		final Constraints constraintsStackClone = (Constraints) constraintsStack.clone();
		final Constraint lastConstraint = constraintsStackClone.pop();

		if (lastConstraint instanceof AccessConstraint)
		{
			final AccessConstraint lastEdgeConstraint = (AccessConstraint) lastConstraint;
			final AccessConstraint.Operation constraintOperation = constraint.getOperation();
			final AccessConstraint.Operation lastConstraintOperation = lastEdgeConstraint.getOperation();

			if (constraintOperation == AccessConstraint.Operation.Remove && lastConstraintOperation == AccessConstraint.Operation.Add)
				return constraint.cancels(lastEdgeConstraint) ? constraintsStackClone : null;
		}

		final Constraints constraintsStackClone2 = (Constraints) constraintsStack.clone();
		constraintsStackClone2.push(constraint);
		return constraintsStackClone2;
	}
	private Constraints resolveUnresolvableConstraint(Constraints constraintsStack, UnresolvableConstraint constraint)
	{
		final Constraints constraintsStackClone = (Constraints) constraintsStack.clone();

		constraintsStackClone.push(constraint);

		return constraintsStackClone;
	}
	private Constraints resolveStarConstraint(Constraints constraintsStack, StarConstraint constraint)
	{
		return new Constraints();
	}
	private Constraints resolveListComprehensionConstraint(Constraints constraintsStack, ListComprehensionConstraint constraint)
	{
		final Constraints constraintsStackClone = (Constraints) constraintsStack.clone();
		Constraint topConstraint = null;

		while (!constraintsStackClone.isEmpty())
		{
			topConstraint = constraintsStackClone.pop();
			if (!(topConstraint instanceof ListConstraint))
				break;

			final ListConstraint edgeConstraint = (ListConstraint) topConstraint;

			if (edgeConstraint.getOperation() != AccessConstraint.Operation.Add)
				break;
			topConstraint = null;
			if (edgeConstraint.getPosition() == ListConstraint.Position.H)
				break;
		}
		if (topConstraint != null)
			constraintsStackClone.push(topConstraint);

		return constraintsStackClone;
	}
	private Constraints resolveBinComprehensionConstraint(Constraints constraintsStack, BinComprehensionConstraint constraint)
	{
		return new Constraints();
	}
	private Constraints resolveSummaryConstraint(Constraints constraintsStack, SummaryConstraint constraint)
	{
		final Constraints newConstraintsStack = (Constraints) constraintsStack.clone();

		newConstraintsStack.push(constraint);

		return newConstraintsStack;
	}

	private List<Constraints> resolveAccessConstraintAdapted(Constraints constraintsStack, AccessConstraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
		final Constraints newConstraintsStack = this.resolveAccessConstraint(constraintsStack, constraint);

		if (newConstraintsStack != null)
			newConstraintsStacks.add(newConstraintsStack);

		return newConstraintsStacks;
	}
	private List<Constraints> resolveUnresolvableConstraintAdapted(Constraints constraintsStack, UnresolvableConstraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
		final Constraints newConstraintsStack = this.resolveUnresolvableConstraint(constraintsStack, constraint);

		newConstraintsStacks.add(newConstraintsStack);

		return newConstraintsStacks;
	}
	private List<Constraints> resolveStarConstraintAdapted(Constraints constraintsStack, StarConstraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
		final Constraints newConstraintsStack = this.resolveStarConstraint(constraintsStack, constraint);

		newConstraintsStacks.add(newConstraintsStack);

		return newConstraintsStacks;
	}
	private List<Constraints> resolveListComprehensionConstraintAdapted(Constraints constraintsStack, ListComprehensionConstraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
		final Constraints newConstraintsStack = this.resolveListComprehensionConstraint(constraintsStack, constraint);

		newConstraintsStacks.add(newConstraintsStack);

		return newConstraintsStacks;
	}
	private List<Constraints> resolveBinComprehensionConstraintAdapted(Constraints constraintsStack, BinComprehensionConstraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
		final Constraints newConstraintsStack = this.resolveBinComprehensionConstraint(constraintsStack, constraint);

		newConstraintsStacks.add(newConstraintsStack);

		return newConstraintsStacks;
	}
	private List<Constraints> resolveSummaryConstraintAdapted(Constraints constraintsStack, SummaryConstraint constraint, int depth)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();
		final List<List<Constraint>> productions = this.edg.getProductions(constraint);

		for (List<Constraint> production : productions)
		{
			final List<Constraints> newConstraintsStacks0 = this.resolveSummaryConstraintAdapted(constraintsStack, production, depth + 1);
			if (newConstraintsStacks0.size() == 1 && newConstraintsStacks0.get(0) == null)
				return newConstraintsStacks0;
			newConstraintsStacks.addAll(newConstraintsStacks0);
		}

		return newConstraintsStacks;
	}
	private List<Constraints> resolveSummaryConstraintAdapted(Constraints constraintsStack, List<Constraint> production, int depth)
	{
		final List<Constraints> stacks = new LinkedList<Constraints>();

		stacks.add(constraintsStack);
		for (Constraint element : production)
		{
			final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

			for (Constraints stack : stacks)
			{
				newConstraintsStacks.addAll(this.resolveProcessingNonTerminals(stack, element, depth));
				if (newConstraintsStacks.size() == 1 && newConstraintsStacks.get(0) == null)
					return newConstraintsStacks;
			}
			stacks.clear();
			stacks.addAll(newConstraintsStacks);
		}

		return stacks;
	}
}