package edg.constraint;

import java.util.LinkedList;
import java.util.List;

import edg.config.Config;
import edg.graph.Edge;
import edg.graph.Grammar;
import edg.graph.Node;
import edg.slicing.Phase;

public class GrammarConstraint extends EdgeConstraint
{
	private final Config config = Config.getConfig();
	private final Grammar grammar;
	private final Node refNode;

	public GrammarConstraint(Grammar grammar, Node refNode)
	{
		this.grammar = grammar;
		this.refNode = refNode;
	}

	public Node getRefNode()
	{
		return this.refNode;
	}

	public boolean equals(Object object)
	{
		if (object == this)
			return true;
		if (!(object instanceof GrammarConstraint))
			return false;

		final GrammarConstraint constraint = (GrammarConstraint) object;

		if (this.grammar != constraint.grammar)
			return false;
		if (this.refNode != constraint.refNode)
			return false;
		return true;
	}
	public String toString()
	{
		final String refNodeId = this.refNode.getData().getId() + "";
		return refNodeId;
// TODO Arreglar el getHashCode del WorkList antes de descomentar esto
/*
		final List<Constraints> productions = this.grammar.getProductions(this);
		if (productions.isEmpty())
			return new EmptyConstraint().toString();
		if (productions.size() != 1)
			return refNodeId;

		final Constraints production = productions.get(0);
		if (production.isEmpty())
			return new EmptyConstraint().toString();
		if (production.size() != 1)
			return refNodeId;

		final Constraint constraint = production.get(0);
		return constraint.toString();
*/
	}

	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, int productionDepth)
	{
		if (phase.isInstanceof(Phase.Slicing))
			return this.resolveProductions(phase, edge, constraints, productionDepth);
		super.check(phase, Phase.SummaryGeneration);
		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AccessConstraint topConstraint, int productionDepth)
	{
		if (phase.isInstanceof(Phase.Slicing))
			return this.resolveProductions(phase, edge, constraints, productionDepth);
		super.check(phase, Phase.SummaryGeneration);
		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, GrammarConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);
		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, SeekingConstraint topConstraint, int productionDepth)
	{
		if (topConstraint.operation == SeekingConstraint.Operation.Add)
			if (phase.isInstanceof(Phase.Slicing))
				return this.resolveProductions(phase, edge, constraints, productionDepth);

		super.check(phase, Phase.SummaryGeneration);
		return super.wrap(super.push(phase, constraints));
	}
	protected List<Constraints> resolve(Phase phase, Edge edge, Constraints constraints, AsteriskConstraint topConstraint, int productionDepth)
	{
		super.check(phase, Phase.SummaryGeneration);
		return super.wrap(super.push(phase, constraints));
	}

	private List<Constraints> resolveProductions(Phase phase, Edge edge, Constraints constraints, int productionDepth)
	{
		super.check(phase, Phase.Slicing);
		if (productionDepth == this.config.maxProductionDepth)
			throw new StackOverflowError();

		final List<Constraints> newConstraintsList = new LinkedList<Constraints>();
		final List<Constraints> productions = this.grammar.getProductions(this);

		for (Constraints production : productions)
		{
			final Constraints constraintsClone = (Constraints) constraints.clone();
			final List<Constraints> pendingConstraintsList = new LinkedList<Constraints>();
			pendingConstraintsList.add(constraintsClone);
			final int productionSize = production.sizeEdgeConstraints();

// TODO NO SE SI ESTO PUEDE TENER CONSECUENCIAS, REVISAR.
// Esto es para que los summaries que no pueden resolver una Seeking Constraint no se atraviesen
/* ************************************* */
if (productionSize == 0)   
	continue;
/* ************************************* */
			for (int constraintIndex = 0; constraintIndex < productionSize; constraintIndex++)
			{
				final EdgeConstraint constraint = production.getEdgeConstraint(constraintIndex);
				final List<Constraints> resolvedConstraintsList = new LinkedList<Constraints>();

				for (Constraints pendingConstraints : pendingConstraintsList)
				{
					final EdgeConstraint topConstraint = pendingConstraints.isEdgeConstraintsEmpty() ? null : pendingConstraints.peekEdgeConstraint();
					resolvedConstraintsList.addAll(constraint.resolve(phase, edge, pendingConstraints, topConstraint, productionDepth + 1));
				}
				pendingConstraintsList.clear();
				pendingConstraintsList.addAll(resolvedConstraintsList);
			}

			newConstraintsList.addAll(pendingConstraintsList);
		}

		return newConstraintsList;
	}
}