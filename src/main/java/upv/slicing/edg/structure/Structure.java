package upv.slicing.edg.structure;

import upv.slicing.edg.LASTBuilder;
import upv.slicing.edg.LDASTNodeInfo;
import upv.slicing.edg.graph.EDG;

public abstract class Structure {
	protected final EDG edg;

	public Structure(EDG edg)
	{
		this.edg = edg;
	}

	public abstract int construct(int parentId, LASTBuilder.Where where, LDASTNodeInfo info);

	public abstract void generateControlEdges();

	public abstract void generateControlFlowEdges();

	public abstract void generateValueEdges();
}