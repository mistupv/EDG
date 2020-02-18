package edg.graph;

import edg.LDASTNodeInfo;

public class NodeInfo
{
	public static enum Type
	{
		// Module
		Module,

		// Routine
		Routine, Clause, Parameters,

		// Expressions
		List, DataConstructor, DataConstructorAccess,
		Block, Operation, Equality,
		If, Condition,
		Switch, Selector, Cases, Case, DefaultCase, Selectable,
		Call, Callee, Scope, Name, Arguments,
		ListComprehension, Restrictions, Generator, Filter, Value,
		Loop, // <- DEPRECATED 
		CLoop, FLoop, RLoop,

		// Others
		Body, Guard,
		Expression, Result,
		Variable, Literal,
		Return, Break, Continue,
		Root,
		
		// ADDED 
		Init, Update, 
		TypeCheck, // JAVA instanceof 
		TypeTransformation,
		Type  	
	}

	private final int id;
	private final Type type;
	private final String name;
	private final LDASTNodeInfo ldASTNodeInfo;

	public NodeInfo(int id, Type type, String name, LDASTNodeInfo ldASTNodeInfo)
	{
		this.id = id;
		this.type = type;
		this.name = name;
		this.ldASTNodeInfo = ldASTNodeInfo;
	}

	public int getId()
	{
		return this.id;
	}
	public Type getType()
	{
		return this.type;
	}
	public String getName()
	{
		return this.name;
	}
	public LDASTNodeInfo getInfo()
	{
		return this.ldASTNodeInfo;
	}

	public boolean isFictitious()
	{
		switch (this.type)
		{
			case Root:
			case Parameters:
			case Guard:
			case Body:
			case Selector:
			case Cases:
			case Selectable:
			case Callee:
			case Arguments:
			case Expression:
			case Result:
			case Restrictions:
			case Value:
				return true;
			default:
				return false;
		}
	}
}