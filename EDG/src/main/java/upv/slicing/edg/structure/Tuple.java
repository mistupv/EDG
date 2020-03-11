package upv.slicing.edg.structure;

import upv.slicing.edg.LASTBuilder;
import upv.slicing.edg.LDASTNodeInfo;
import upv.slicing.edg.graph.EDG;

public class Tuple extends Structure {
	public Tuple(EDG edg)
	{
		super(edg);
	}

	@Override
	public int construct(int parentId, LASTBuilder.Where where, LDASTNodeInfo info)
	{
		return 0;
	}

	public void generateControlEdges()
	{

	}

	public void generateControlFlowEdges()
	{
		
	}

	public void generateValueEdges()
	{
		
	}
}