package upv.slicing.edg.graph;

import upv.slicing.edg.LDASTNodeInfo;

import java.util.List;

public class Variable extends Node {
	public enum Context {Definition, Use, Def_Use, Declaration} // Def_Use es para unaryExpressions (i++ etc.)

	private boolean declaration;
	private Context context;
	private boolean global;
	private String staticType;
	private List<String> dynamicTypes; // TODO: Determine dynamic type in the last definition (may be difficult)

	public Variable(int id, Node.Type type, String name, LDASTNodeInfo ldASTNodeInfo)
	{
		super(id, type, name, ldASTNodeInfo);
	}

	public Variable(int id, int SDGId, Node.Type type, String name, LDASTNodeInfo ldASTNodeInfo)
	{
		super(id, type, name, ldASTNodeInfo);
	}

	public Variable(int id, Node.Type type, String name, String varType, LDASTNodeInfo ldASTNodeInfo)
	{
		super(id, type, name, ldASTNodeInfo);
		this.staticType = varType;
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
	public String getStaticType() { return this.staticType; }

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
	public void setStaticType(String type) { this.staticType = type; }
}