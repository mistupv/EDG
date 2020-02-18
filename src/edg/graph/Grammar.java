package edg.graph;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import edg.constraint.Constraint;
import edg.constraint.SummaryConstraint;

public class Grammar
{
	private final Map<Node, SummaryConstraint> summaries = new HashMap<Node, SummaryConstraint>();
	private final Map<SummaryConstraint, List<List<Constraint>>> grammar = new HashMap<SummaryConstraint, List<List<Constraint>>>();

	private List<List<Constraint>> createProductions(SummaryConstraint summaryConstraint)
	{
		List<List<Constraint>> productions = this.getProductions(summaryConstraint);

		if (productions == null)
		{
			productions = new LinkedList<List<Constraint>>();
			this.grammar.put(summaryConstraint, productions);
		}

		return productions;
	}
	public List<List<Constraint>> getProductions(SummaryConstraint summaryConstraint)
	{
		final Node summaryFormalIn = summaryConstraint.getFormalIn();
		SummaryConstraint summary = this.summaries.get(summaryFormalIn);

		if (summary == null)
		{
			summary = summaryConstraint;
			this.summaries.put(summaryFormalIn, summary);
		}

		return this.grammar.get(summary);
	}
	public void addProduction(SummaryConstraint summaryConstraint, List<Constraint> production)
	{
		final List<List<Constraint>> productions = this.createProductions(summaryConstraint);

		if (productions.contains(production))
			return; // Do not add duplicated productions
		if (production.size() == 1 && production.get(0).equals(summaryConstraint))
			return; // Ignore recursive productions

		productions.add(production);
	}

// TODO Delete
public void printGrammar(Constraint constraint)
{
	this.printGrammar(constraint, new LinkedList<Constraint>());
}
public void printGrammar(Constraint constraint, List<Constraint> processedConstraints)
{
	if (!(constraint instanceof SummaryConstraint) || processedConstraints.contains(constraint))
		return;
	final SummaryConstraint summaryConstraint = (SummaryConstraint) constraint;
	final List<List<Constraint>> productions = this.getProductions(summaryConstraint);

	System.out.println(summaryConstraint + ")");
	for (List<Constraint> production : productions)
		System.out.println("\t" + production);
	processedConstraints.add(constraint);
	for (List<Constraint> production : productions)
		for (Constraint constraint0 : production)
			this.printGrammar(constraint0, processedConstraints);
}
}