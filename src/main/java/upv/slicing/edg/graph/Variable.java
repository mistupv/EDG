package upv.slicing.edg.graph;

import upv.slicing.edg.LDASTNodeInfo;

public class Variable extends Node {
	public enum Context {Definition, Use, Def_Use, Declaration} // Def_Use es para unaryExpressions (i++ etc.)

	private boolean declaration;
	private Context context;
	private boolean global;

	public Variable(int id, Node.Type type, String name, LDASTNodeInfo ldASTNodeInfo)
	{
		super(id, type, name, ldASTNodeInfo);
	}

	public Variable(int id, int SDGId, Node.Type type, String name, LDASTNodeInfo ldASTNodeInfo)
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