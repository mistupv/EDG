package upv.slicing.edg.graph;

import upv.slicing.edg.constraint.Constraints;
import upv.slicing.edg.constraint.GrammarConstraint;
import upv.slicing.edg.slicing.SlicingCriterion;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public class EDG extends LAST {
	/** Create an empty EDG with an empty AST. */
	public EDG()
	{
		super();
	}

	/** Create an EDG from a LAST, copying the AST (structural) info. */
	public EDG(LAST last)
	{
		last.vertexSet().forEach(this::addVertex);
		last.edgeSet().forEach(e -> addEdge(last.getEdgeSource(e), last.getEdgeTarget(e), e));
		this.rootNode = last.rootNode;
		this.nextId = last.nextId;
		this.fictitiousId = last.fictitiousId;
	}

	// ================================================= //
	// ===================== NODES ===================== //
	// ================================================= //

	/**
	 * Obtains the <i>res</i> node that represents the slicing criterion.
	 * @throws IllegalArgumentException If the criterion cannot be found,
	 * or the occurrence in the slicing criterion is higher than the number of matching nodes.
	 * @throws NullPointerException If the argument is null.
	 */
	public Node getNode(SlicingCriterion sc)
	{
		Objects.requireNonNull(sc, "The slicing criterion must not be null");
		final List<Node> nodes = this.findAllNodes(sc::matchesNode);
		if (nodes.isEmpty() || sc.getOccurrence() > nodes.size())
			throw new IllegalArgumentException("Slicing criterion could not be mapped to graph: " + sc);
		nodes.sort(Comparator.comparingInt(Node::getId));
		final Node node = nodes.get(sc.getOccurrence() - 1);
		return getResFromNode(node);
	}

	// ================================================= //
	// ==================== GRAMMAR ==================== //
	// ================================================= //

	private final Grammar grammar = new Grammar();

	public Grammar getGrammar()
	{
		return this.grammar;
	}

	public List<Constraints> getProductions(GrammarConstraint grammarConstraint)
	{
		return this.grammar.getProductions(grammarConstraint);
	}

	public void addProduction(GrammarConstraint grammarConstraint, Constraints production)
	{
		this.grammar.addProduction(grammarConstraint, production);
	}

	// ================================================= //
	// =========== GENERATION TIME MEASURER ============ //
	// ================================================= //

	private final GraphGeneratorTimer timer = new GraphGeneratorTimer();

	public GraphGeneratorTimer getGenerationTime()
	{
		return this.timer;
	}

	public static class GraphGeneratorTimer
	{	
		private double structureTime;
		private double controlFlowTime;
		private double controlTime;
		private double interproceduralTime;
		private double flowTime;
		private double valueTime;
		private double summaryTime;
		private double exceptionTime;

		public void setStructureTime(double structureTime)
		{
			this.structureTime = structureTime;
		}

		public void setControlFlowTime(double controlFlowTime)
		{
			this.controlFlowTime = controlFlowTime;
		}

		public void setControlTime(double controlTime)
		{
			this.controlTime = controlTime;
		}

		public void setInterproceduralTime(double interproceduralTime)
		{
			this.interproceduralTime = interproceduralTime;
		}

		public void setFlowTime(double flowTime)
		{
			this.flowTime = flowTime;
		}

		public void setValueTime(double valueTime)
		{
			this.valueTime = valueTime;
		}

		public void setSummaryTime(double summaryTime)
		{
			this.summaryTime = summaryTime;
		}

		public void setExceptionTime(double exceptionTime)
		{
			this.exceptionTime = exceptionTime;
		}

		public double getGenerationEDGTime()
		{
			return structureTime + controlFlowTime + controlTime + interproceduralTime + flowTime + 
					 valueTime + summaryTime + exceptionTime;
		}
	}
}
