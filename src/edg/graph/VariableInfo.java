package edg.graph;

import edg.LDASTNodeInfo;

public class VariableInfo extends NodeInfo
{
	public static enum Context { Definition, Use, Def_Use } // Def_Use es para unaryExpressions (i++ etc.)

	private boolean declaration;
	private Context context;
	private boolean global;

	public VariableInfo(int id, Type type, String name, LDASTNodeInfo ldASTNodeInfo)
	{
		super(id, type, name, ldASTNodeInfo);
	}

	public boolean isDeclaration()
	{
		return this.declaration;
	}
	public Context getContext()
	{
		return this.context;
	}
	public boolean isGlobal()
	{
		return this.global;
	}

	public void setDeclaration(boolean declaration)
	{
		this.declaration = declaration;
	}
	public void setContext(Context context)
	{
		this.context = context;
	}
	public void setGlobal(boolean global)
	{
		this.global = global;
	}
}