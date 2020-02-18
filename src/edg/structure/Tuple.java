package edg.structure;

import edg.ASTBuilder.Where;
import edg.LDASTNodeInfo;
import edg.graph.EDG;

public class Tuple extends Structure
{
	public Tuple(EDG edg)
	{
		super(edg);
	}

	public int construct(int parentId, Where where, LDASTNodeInfo info)
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