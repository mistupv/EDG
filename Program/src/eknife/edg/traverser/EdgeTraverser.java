package eknife.edg.traverser;

import java.util.LinkedList;
import java.util.List;

import eknife.config.Config;
import eknife.edg.EDG;
import eknife.edg.EDG.GrammarType;
import eknife.edg.Edge;
import eknife.edg.EdgeInfo;
import eknife.edg.Grammar;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.constraint.AccessConstraint;
import eknife.edg.constraint.Constraint;
import eknife.edg.constraint.Constraints;
import eknife.edg.constraint.SummaryConstraints.SummaryType;
import eknife.edg.constraint.ExceptionArgumentConstraint;
import eknife.edg.constraint.ExceptionConstraint;
import eknife.edg.constraint.SeekingConstraint;
import eknife.edg.constraint.SummaryConstraint;
import eknife.edg.constraint.SummaryConstraints;
import eknife.edg.constraint.TupleConstraint;
import eknife.edg.constraint.AccessConstraint.Operation;


public class EdgeTraverser
{
/***	// TODO Delete me
	public static void main(String[] args)
	{
		EdgeTraverser.example1();
		System.out.println("============================");
		EdgeTraverser.example2();
		System.out.println("============================");
		EdgeTraverser.example3();
		System.out.println("============================");
	}
	public static void example1()
	{
		final EDG graph = new EDG();
		final Grammar grammar = graph.getGrammar(GrammarType.Value);

		final SummaryConstraint procedureP1 = new SummaryConstraint(grammar, new Node());
		final SummaryConstraint procedureP2 = new SummaryConstraint(grammar, new Node());
		final List<Constraint> productionP1a = new LinkedList<Constraint>();
		final List<Constraint> productionP2a = new LinkedList<Constraint>();
		final List<Constraint> productionP2b = new LinkedList<Constraint>();
		final TupleConstraint minusB = new TupleConstraint(AccessConstraint.Operation.Remove, 2);
		final TupleConstraint minusF = new TupleConstraint(AccessConstraint.Operation.Remove, 6);
		final TupleConstraint plusC = new TupleConstraint(AccessConstraint.Operation.Add, 3);

		productionP1a.add(minusF);
		productionP1a.add(procedureP2);
		productionP2a.add(plusC);
		productionP2b.add(minusB);
		productionP2b.add(plusC);

		final Constraints constraintsStack1 = new Constraints();

		graph.addProduction(GrammarType.Value, procedureP1, productionP1a);
		graph.addProduction(GrammarType.Value, procedureP2, productionP2a);
		graph.addProduction(GrammarType.Value, procedureP2, productionP2b);

		final EdgeTraverser edgeTraverser = new EdgeTraverser(graph, Phase.Slicing);

		final List<Constraints> newConstraintsStacks = edgeTraverser.traverseEdge(constraintsStack1, procedureP1);
		System.out.println("Starts");
		for (Constraints newConstraintsStack : newConstraintsStacks)
			System.out.println(newConstraintsStack);
		System.out.println("Finishes");
	}
	public static void example2()
	{
		final EDG graph = new EDG();
		final Grammar grammar = graph.getGrammar(GrammarType.Value);

		final SummaryConstraint procedureP1 = new SummaryConstraint(grammar, new Node());
		final List<Constraint> productionP1a = new LinkedList<Constraint>();
		final List<Constraint> productionP1b = new LinkedList<Constraint>();
		final List<Constraint> productionP1c = new LinkedList<Constraint>();
		final List<Constraint> productionP1d = new LinkedList<Constraint>();
		final TupleConstraint minusA = new TupleConstraint(AccessConstraint.Operation.Remove, 1);
		final TupleConstraint minusB = new TupleConstraint(AccessConstraint.Operation.Remove, 2);
		final TupleConstraint minusF = new TupleConstraint(AccessConstraint.Operation.Remove, 6);
		final TupleConstraint plusA = new TupleConstraint(AccessConstraint.Operation.Add, 1);
		final TupleConstraint plusB = new TupleConstraint(AccessConstraint.Operation.Add, 2);
		final TupleConstraint plusC = new TupleConstraint(AccessConstraint.Operation.Add, 3);

		productionP1a.add(plusC);
		productionP1b.add(minusB);
		productionP1b.add(plusC);
		productionP1c.add(minusA);
		productionP1c.add(procedureP1);
		productionP1c.add(plusB);
		productionP1d.add(minusF);
		productionP1d.add(procedureP1);

		final Constraints constraintsStack1 = new Constraints();
		final Constraints constraintsStack2 = new Constraints();

		constraintsStack1.push(plusB);
		constraintsStack1.push(plusA);

		graph.addProduction(GrammarType.Value, procedureP1, productionP1a);
		graph.addProduction(GrammarType.Value, procedureP1, productionP1b);
		graph.addProduction(GrammarType.Value, procedureP1, productionP1c);
		graph.addProduction(GrammarType.Value, procedureP1, productionP1d);

		final EdgeTraverser edgeTraverser = new EdgeTraverser(graph, Phase.Slicing);

		final List<Constraints> newConstraintsStacks1 = edgeTraverser.traverseEdge(constraintsStack1, procedureP1);
		System.out.println("Starts");
		for (Constraints newConstraintsStack : newConstraintsStacks1)
			System.out.println(newConstraintsStack);
		System.out.println("Finishes");
		final List<Constraints> newConstraintsStacks2 = edgeTraverser.traverseEdge(constraintsStack2, procedureP1);
		System.out.println("Starts");
		for (Constraints newConstraintsStack : newConstraintsStacks2)
			System.out.println(newConstraintsStack);
		System.out.println("Finishes");
	}
	public static void example3()
	{
		final EDG graph = new EDG();
		final Grammar grammar = graph.getGrammar(GrammarType.Value);

		final SummaryConstraint procedureP1 = new SummaryConstraint(grammar, new Node());
		final SummaryConstraint procedureP2 = new SummaryConstraint(grammar, new Node());
		final SummaryConstraint procedureP3 = new SummaryConstraint(grammar, new Node());
		final List<Constraint> productionP1a = new LinkedList<Constraint>();
		final List<Constraint> productionP1b = new LinkedList<Constraint>();
		final List<Constraint> productionP2a = new LinkedList<Constraint>();
		final List<Constraint> productionP3a = new LinkedList<Constraint>();
		final List<Constraint> productionP3b = new LinkedList<Constraint>();
		final TupleConstraint minus1 = new TupleConstraint(AccessConstraint.Operation.Remove, 1);
		final TupleConstraint minus2 = new TupleConstraint(AccessConstraint.Operation.Remove, 2);
		final TupleConstraint plus1 = new TupleConstraint(AccessConstraint.Operation.Add, 1);
		final TupleConstraint plus2 = new TupleConstraint(AccessConstraint.Operation.Add, 2);

		productionP1a.add(minus2);
		productionP1a.add(procedureP3);
		productionP1a.add(plus2);
		productionP1b.add(minus2);
		productionP1b.add(procedureP2);
		productionP1b.add(plus1);
		productionP2a.add(minus1);
		productionP2a.add(minus2);
		productionP3a.add(minus2);
		productionP3b.add(minus1);
		productionP3b.add(minus1);

		final Constraints constraintsStack1 = new Constraints();

		graph.addProduction(GrammarType.Value, procedureP1, productionP1a);
		graph.addProduction(GrammarType.Value, procedureP1, productionP1b);
		graph.addProduction(GrammarType.Value, procedureP2, productionP2a);
		graph.addProduction(GrammarType.Value, procedureP3, productionP3a);
		graph.addProduction(GrammarType.Value, procedureP3, productionP3b);

		final EdgeTraverser edgeTraverser = new EdgeTraverser(graph, Phase.Slicing);

		final List<Constraints> newConstraintsStacks1 = edgeTraverser.traverseEdge(constraintsStack1, procedureP1);
		System.out.println("Starts");
		for (Constraints newConstraintsStack : newConstraintsStacks1)
			System.out.println(newConstraintsStack);
		System.out.println("Finishes");
	}
***/





	public enum Phase { Summary, Slicing };

	private final EDG graph;
	private final Phase phase;

	public EdgeTraverser(EDG graph, Phase phase)
	{
		this.graph = graph;
		this.phase = phase;
	}

	public List<Constraints> traverseIncomingEdge(Edge edge, Constraints constraints)
	{
		final EdgeInfo.Type edgeType = edge.getData().getType();
		final Node nodeFrom = edge.getFrom();
		final NodeInfo.Type newNodeType = nodeFrom.getData().getType();
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

		Constraints newConstraints = (Constraints) constraints.clone();
		
		if (this.phase == Phase.Summary && ((SummaryConstraints)constraints).getSummaryType() == null)
		{
			final Constraint edgeConstraint = edge.getData().getConstraint();
			final SummaryType summaryType = (edgeConstraint instanceof SeekingConstraint) ? SummaryType.Exception : SummaryType.Return;
			((SummaryConstraints) newConstraints).setSummaryType(summaryType);
		}
		
		// Going up using control edges empties the stack only in slicing time
		// TODO Borrar si funciona bien
/**
		if (edgeType == EdgeInfo.Type.NormalControl) // PLANTEAR BORRAR ESTE IF Y HACER QUE SE EJECUTE EL resolve DE StarConstraint
		{
			newConstraints.clear();
			if (this.phase == Phase.Summary)
				newConstraints.push(edge.getData().getConstraint());
			newConstraintsStacks.add(newConstraints);
		}
**/
		// Structural control edges that belong to expressions can only be traversed the other way round.
		// Traverse it this way means that one descendant is the slicing criterion, and we let go up emptying the stack.
		if (edgeType == EdgeInfo.Type.StructuralControl &&
			(newNodeType == NodeInfo.Type.TupleExpression || newNodeType == NodeInfo.Type.ListExpression || 
			 newNodeType == NodeInfo.Type.Record || newNodeType == NodeInfo.Type.RecordField || newNodeType == NodeInfo.Type.Map || newNodeType == NodeInfo.Type.MapUpdate ||// ADDED BY SERGIO
			 newNodeType == NodeInfo.Type.BinExpression || newNodeType == NodeInfo.Type.BinElementExpression))
			newConstraintsStacks.add(Constraints.getConstraints(this.phase));
		if (!newConstraintsStacks.isEmpty())
			return newConstraintsStacks;
		// ExceptionArgument constraints forbid to traverse the non-exception edges
		if (!newConstraints.isEmpty())
		{
			final Constraint topConstraint = newConstraints.peek();
			final Constraint edgeConstraint = edge.getData().getConstraint();
			if (topConstraint instanceof ExceptionArgumentConstraint &&
					((ExceptionArgumentConstraint) topConstraint).getOperation() == Operation.Add &&
					(!(edgeConstraint instanceof ExceptionConstraint || edgeConstraint instanceof ExceptionArgumentConstraint)))
				return new LinkedList<Constraints>();
		}

		final Constraints constraintsClone = (Constraints) newConstraints.clone();
		final Constraint constraint = edge.getData().getConstraint();
		return this.traverseEdge(constraintsClone, constraint);
	}
	public List<Constraints> traverseOutgoingEdge(Edge edge, Constraints constraints)
	{
		final Node nodeFrom = edge.getFrom();
		final NodeInfo.Type nodeType = nodeFrom.getData().getType();
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

		// Traverse the edge this way can only be done using structural control edges that belong to expressions
		if (nodeType != NodeInfo.Type.TupleExpression && nodeType != NodeInfo.Type.ListExpression && 
			nodeType != NodeInfo.Type.Record && nodeType != NodeInfo.Type.RecordField && nodeType != NodeInfo.Type.Map && nodeType != NodeInfo.Type.MapUpdate && // ADDED BY SERGIO
			nodeType != NodeInfo.Type.BinExpression && nodeType != NodeInfo.Type.BinElementExpression)
			return newConstraintsStacks;

//if (this.phase == Phase.Summary && edge.getData().getType() == EdgeInfo.Type.Exception)
//	return new LinkedList<Constraints>();
		
		final Constraint constraint = edge.getData().getConstraint();
		return this.traverseEdge(constraints, constraint);
	}
	private List<Constraints> traverseEdge(Constraints constraintsStack, Constraint constraint)
	{
		if (Config.constraintsActivated)
			return this.traverseEdgeTreatingConstraints(constraintsStack, constraint);
		return this.traverseEdgeIgnoringConstraints(constraintsStack, constraint);
	}
	private List<Constraints> traverseEdgeTreatingConstraints(Constraints constraintsStack, Constraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

		if (constraint == null) // ONLY INPUT & OUTPUT EDGES
			newConstraintsStacks.add(constraintsStack); 
		else
			newConstraintsStacks.addAll(this.resolveConstraint(constraintsStack, constraint));

		if (newConstraintsStacks.size() == 1 && newConstraintsStacks.get(0) == null)
		{
			newConstraintsStacks.clear();
			if (this.phase == Phase.Slicing)
				newConstraintsStacks.add(Constraints.getConstraints(this.phase));
		}

		return newConstraintsStacks;
	}
	private List<Constraints> traverseEdgeIgnoringConstraints(Constraints constraintsStack, Constraint constraint)
	{
		final List<Constraints> newConstraintsStacks = new LinkedList<Constraints>();

		newConstraintsStacks.add(Constraints.getConstraints(this.phase));

		return newConstraintsStacks;
	}

	private List<Constraints> resolveConstraint(Constraints constraintsStack, Constraint constraint)
	{
		final Constraints constraintsStackClone = (Constraints) constraintsStack.clone();
		final Constraint lastConstraint = constraintsStackClone.isEmpty() ? null : constraintsStackClone.peek();
		final List<Constraints> constraintsStacks = constraint.resolve(this.phase, constraintsStackClone, lastConstraint, 0);

		if (constraintsStacks.size() == 1 && constraintsStacks.get(0) == null)
		{
			constraintsStacks.clear();
			constraintsStacks.add(Constraints.getConstraints(this.phase));
		}
		return constraintsStacks;
	}






/**************
 	TODO Borrar si todo funciona bien
 	
	private static final int maxDepth = 5;
	private static final int maxStackSize = 9;

	private Constraints resolveCollectingNonTerminals(Constraints constraintsStack, Constraint constraint)
	{
// FIXME Arreglame
//if (constraintsStack.size() >= EdgeTraverser.maxStackSize)
//	return constraintsStack;

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
		throw new RuntimeException("Type of constraint not contempled");
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
		throw new RuntimeException("Type of constraint not contempled");
	}

	private Constraints resolveSeekingConstraint(Constraints constraintsStack, SeekingConstraint constraint)
	{
		if (constraintsStack.isEmpty())
		{
			if (!constraint.letThroughWithEmptyStack(this.phase))
				return null;
			final Constraints newConstraintsStack = new Constraints();

			if (this.phase == Phase.Summary)
				newConstraintsStack.setExceptionSummary(true);
						
			if (constraint.getOperation() != null)
				newConstraintsStack.push(constraint);
			return newConstraintsStack;
		}
		
		final Constraints constraintsStackClone = (Constraints) constraintsStack.clone();
		final Constraint lastConstraint = constraintsStackClone.pop();
		final Constraints constraintsStackClone2 = (Constraints) constraintsStack.clone();
		constraintsStackClone2.push(constraint);

		if (lastConstraint instanceof SeekingConstraint)
		{
			final SeekingConstraint lastEdgeConstraint = (SeekingConstraint) lastConstraint;
			final SeekingConstraint.Operation constraintOperation = constraint.getOperation();
			final SeekingConstraint.Operation lastConstraintOperation = lastEdgeConstraint.getOperation();

			// TODO Falta por terminar
			if (constraintOperation == null && lastConstraintOperation == SeekingConstraint.Operation.Add)
				return constraint.letThrough(lastEdgeConstraint) ? constraintsStack : null;
			if (constraintOperation == SeekingConstraint.Operation.Remove && lastConstraintOperation == SeekingConstraint.Operation.Add)
				return constraint.cancels(lastEdgeConstraint) ? constraintsStackClone : null;
			return constraintsStackClone2;
		}
		if (lastConstraint instanceof SummaryConstraint)
			return constraintsStackClone2;
		if (lastConstraint instanceof UnresolvableConstraint && constraint.getOperation() == SeekingConstraint.Operation.Add)
			return constraintsStackClone2;
		return null;
		
	}
	private Constraints resolveAccessConstraint(Constraints constraintsStack, AccessConstraint constraint)
	{
		if (constraintsStack.isEmpty())
		{
			if (!constraint.letThroughWithEmptyStack(this.phase))
				return null;

			final Constraints newConstraintsStack = new Constraints();

			if (constraint.getOperation() != null)
				newConstraintsStack.push(constraint);
			return newConstraintsStack;
		}

		final Constraints constraintsStackClone = (Constraints) constraintsStack.clone();
		final Constraint lastConstraint = constraintsStackClone.pop();
		final Constraints constraintsStackClone2 = (Constraints) constraintsStack.clone();
		constraintsStackClone2.push(constraint);

		if (lastConstraint instanceof AccessConstraint)
		{
			final AccessConstraint lastEdgeConstraint = (AccessConstraint) lastConstraint;
			final AccessConstraint.Operation constraintOperation = constraint.getOperation();
			final AccessConstraint.Operation lastConstraintOperation = lastEdgeConstraint.getOperation();

			// TODO Falta por terminar
			if (constraintOperation == null && lastConstraintOperation == AccessConstraint.Operation.Add)
				return constraint.letThrough(lastEdgeConstraint) ? constraintsStack : null;
			if (constraintOperation == AccessConstraint.Operation.Remove && lastConstraintOperation == AccessConstraint.Operation.Add)
				return constraint.cancels(lastEdgeConstraint) ? constraintsStackClone : null;
			return constraintsStackClone2;
		}
		if (lastConstraint instanceof SummaryConstraint)
			return constraintsStackClone2;
		if (lastConstraint instanceof UnresolvableConstraint && constraint.getOperation() == AccessConstraint.Operation.Add)
			return constraintsStackClone2;
		return null;
	}
	private Constraints resolveUnresolvableConstraint(Constraints constraintsStack, UnresolvableConstraint constraint)
	{
		final Constraints constraintsStackClone = (Constraints) constraintsStack.clone();
		
		constraintsStackClone.clear();
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
		final List<List<Constraint>> productions = this.graph.getProductions(constraint);

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
************/
}