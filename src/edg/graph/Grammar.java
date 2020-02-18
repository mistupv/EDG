package edg.graph;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import edg.constraint.Constraints;
import edg.constraint.GrammarConstraint;

public class Grammar
{
	private final Map<Node, GrammarConstraint> grammarConstraints = new HashMap<Node, GrammarConstraint>();
	private final Map<GrammarConstraint, List<Constraints>> grammar = new HashMap<GrammarConstraint, List<Constraints>>();

	private List<Constraints> createProductions(GrammarConstraint grammarConstraint)
	{
		List<Constraints> productions = this.getProductions(grammarConstraint);

		if (productions == null)
		{
			productions = new LinkedList<Constraints>();
			this.grammar.put(grammarConstraint, productions);
		}

		return productions;
	}
	public List<Constraints> getProductions(GrammarConstraint grammarConstraint)
	{
		final Node refNode = grammarConstraint.getRefNode();
		GrammarConstraint grammarConstraint0 = this.grammarConstraints.get(refNode);

		if (grammarConstraint0 == null)
		{
			grammarConstraint0 = grammarConstraint;
			this.grammarConstraints.put(refNode, grammarConstraint0);
		}

		return this.grammar.get(grammarConstraint0);
	}
	public void addProduction(GrammarConstraint grammarConstraint, Constraints production)
	{
		if (production.sizeEdgeConstraints() == 1 && production.getEdgeConstraint(0).equals(grammarConstraint))
			return; // Ignore recursive productions

		final List<Constraints> productions = this.createProductions(grammarConstraint);
		if (productions.contains(production))
			return; // Do not add duplicated productions

		productions.add(production);
	}
}