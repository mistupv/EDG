package edg.structure;

import edg.LDASTNodeInfo;
import edg.ASTBuilder.Where;
import edg.graph.EDG;

public abstract class Structure
{
	protected final EDG edg;

	public Structure(EDG edg)
	{
		this.edg = edg;
	}

	public abstract int construct(int parentId, Where where, LDASTNodeInfo info);
	public abstract void generateControlEdges();
	public abstract void generateControlFlowEdges();
	public abstract void generateValueEdges();
}