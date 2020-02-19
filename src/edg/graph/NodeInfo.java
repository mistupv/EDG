package edg.graph;

import edg.LDASTNodeInfo;

public class NodeInfo {
	public enum Type {
		// Module
		Module,

		// Routine
		Routine, Clause, Parameters,

		// Expressions
		List, DataConstructor, DataConstructorAccess, FieldAccess,
		Block, Operation, Equality,
		If, Condition, Then, Else,
		Switch, Selector, Cases, Case, DefaultCase, Selectable,
		Call, Callee, Scope, Name, Arguments,
		ListComprehension, Restrictions, Generator, Filter, Value,
		Loop, // <- DEPRECATED USED IN PHP
		CLoop, FLoop, RLoop, Foreach, Iterator, // LOOPS
		ExHandler, Try, Catch, Finally, CatchClause, Throw, // EXCEPTIONS

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
		Type,
		Reference, Label,
		ArgumentIn, ArgumentOut,
		ParameterIn, ParameterOut,
		
		Index // To identify DataContructorAccess Indexes
		
		
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
			case ArgumentIn:
			case ArgumentOut:
			case Expression:
			case Scope:
			case Name:
//			case Result:
			case Restrictions:
			case Value:
			case Condition:

				return true;
			default:
				return false;
		}
	}
	public boolean isSliceable(Node node)
	{
		switch (this.type)
		{
//			case Variable:
//			case Literal:
			case Result: // Eliminar Literales y Definiciones de variables
//				final Node sibling = EDGTraverser.getSibling(node, 0);
//				switch(sibling.getData().getType())
//				{
//					case Literal:
//						return false;
//					case Variable:
//						if (((VariableInfo) sibling.getData()).getContext() == Context.Definition)
//							return false;
//						return true;
//					default:
//						return true;
//				}
			case Return:
			case Break:
			case Continue:
			case Type:
				return true;
			default:
				return false;
		}
	}
}