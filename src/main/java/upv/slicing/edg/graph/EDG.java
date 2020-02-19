package upv.slicing.edg.graph;

import upv.slicing.edg.LDASTNodeInfo;
import upv.slicing.edg.constraint.Constraints;
import upv.slicing.edg.constraint.GrammarConstraint;
import upv.slicing.edg.slicing.SlicingCriterion;
import upv.slicing.edg.traverser.EDGTraverser;

import java.util.Comparator;
import java.util.List;

public class EDG extends LAST {
	public EDG()
	{

	}

	public EDG(LAST last)
	{
		this.graph = last.graph; 
	}
	/*****************/
	/***** Nodes *****/
	/*****************/
	public Node getNode(SlicingCriterion sc)
	{
		if (sc == null)
			return null;

		final String scArchive = sc.getArchive();
		final int scLine = sc.getLine();
		final String scName = sc.getName();
		final List<Node> nodes = this.findNodesByData(null, new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				final LDASTNodeInfo ldNodeInfo = o2.getInfo();
				if (ldNodeInfo == null)
					return -1;
				if (scLine != ldNodeInfo.getLine())
					return -1;
				if (!scName.equals(o2.getName()))
					return -1;
				if (!scArchive.equals(ldNodeInfo.getArchive()))
					return -1;
				return 0;
			}
		});
		final int scOccurrence = sc.getOccurrence();
		if (nodes.isEmpty())
			return null;
		final Node node = nodes.get(scOccurrence - 1);
		return EDGTraverser.getResFromNode(node);
	}

	/*****************/
	/**** Grammar ****/
	/*****************/
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
	
	/**********************************/
	/**** Generation Time Measurer ****/
	/**********************************/
	private final GraphGeneratorTimer ggt = new GraphGeneratorTimer();
	
	public void setStructureTime(double time) {	this.ggt.setStructureTime(time); }
	public void setControlFlowTime(double time) {	this.ggt.setControlFlowTime(time); }
	public void setControlTime(double time) {	this.ggt.setControlTime(time);	}
	public void setInterproceduralTime(double time) {	this.ggt.setInterproceduralTime(time); }
	public void setFlowTime(double time) {	this.ggt.setFlowTime(time); }
	public void setValueTime(double time) {	this.ggt.setValueTime(time); }
	public void setSummaryTime(double time) {	this.ggt.setSummaryTime(time); }
	public void setExceptionTime(double time) {	this.ggt.setExceptionTime(time); }
	
	public GraphGeneratorTimer getGenerationTime()
	{
		return this.ggt;
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
		
		public GraphGeneratorTimer()
		{
			
		}
		
		public void setStructureTime(double time) {	this.structureTime = time;	}
		public void setControlFlowTime(double time) {	this.controlFlowTime = time;	}
		public void setControlTime(double time) {	this.controlTime = time;	}
		public void setInterproceduralTime(double time) {	this.interproceduralTime = time;	}
		public void setFlowTime(double time) {	this.flowTime = time;	}
		public void setValueTime(double time) {	this.valueTime = time;	}
		public void setSummaryTime(double time) {	this.summaryTime = time;	}
		public void setExceptionTime(double time) {	this.exceptionTime = time;	}
		
		public double getGenerationEDGTime()
		{
			return structureTime + controlFlowTime + controlTime + interproceduralTime + flowTime + 
					 valueTime + summaryTime + exceptionTime;
		}
	}
}