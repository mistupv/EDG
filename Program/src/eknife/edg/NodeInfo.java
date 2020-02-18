package eknife.edg;

import com.ericsson.otp.erlang.OtpErlangObject;

public class NodeInfo
{
	// TODO Group types
	public enum Type
	{
		// Functions
		Function,

		// Clauses
		Clause, Guard,

		// Patterns
		TuplePattern, ListPattern, BinPattern, BinElementPattern, CompoundPattern,

		// Expressions
		FunctionIdentifier, Block, TupleExpression, ListExpression, BinExpression, BinElementExpression, Operation,
		PatternMatching, Case, If, ListComprehension, BinComprehension, FunctionCall,
		AnonymousFunction, 

		// Others
		Body, Return, ListComprehensionResult, BinComprehensionResult,
		Record, Remote, Atom, String, Integer, Char, Default, Variable, Generator, BinGenerator,
		Root, Other,
		
//New Types added (by Sergio)
		Catch, Catch0, Throw, Receive, AfterReceive, TryCatch, TryOf, Try, CatchClause, ExceptionPattern, ExceptionReturn, AfterTry, Field, 
		RecordField, Map, MapUpdate, MapFieldAssoc, MapFieldExact, MapMatching, Or, And
	}

	private final int id;
	private final long line;
	private final Type type;
	private final String module;
	private final String name;
	private final long arity;
	private final OtpErlangObject AST;

	public NodeInfo(int id, long line, Type type)
	{
		this(id, line, type, null, null, 0, null);
	}
	public NodeInfo(int id, long line, Type type, String name)
	{
		this(id, line, type, null, name, 0, null);
	}
	public NodeInfo(int id, long line, Type type, String name, long arity)
	{
		this(id, line, type, null, name, arity, null);
	}
	public NodeInfo(int id, long line, Type type, OtpErlangObject AST)
	{
		this(id, line, type, null, null, 0, AST);
	}
	public NodeInfo(int id, long line, Type type, String module, String name, long arity)
	{
		this(id, line, type, module, name, arity, null);
	}
	private NodeInfo(int id, long line, Type type, String module, String name, long arity, OtpErlangObject AST)
	{
		this.id = id;
		this.line = line;
		this.type = type;
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.AST = AST;
	}

	public int getId()
	{
		return this.id;
	}
	public long getLine()
	{
		return this.line;
	}
	public Type getType()
	{
		return this.type;
	}
	public String getModule()
	{
		return this.module;
	}
	public String getName()
	{
		return this.name;
	}
	public long getArity()
	{
		return this.arity;
	}
	public OtpErlangObject getAST()
	{
		return this.AST;
	}
	public boolean isFictitious()
	{
		if (this.type == Type.Root)
			return true;
		if (this.type == Type.Body)
			return true;
		if (this.type == Type.Return)
			return true;
		if (this.type == Type.ExceptionReturn)
			return true;
		if (this.type == Type.ListComprehensionResult)
			return true;
		if (this.type == Type.Variable && this.name.equals("_"))
			return true;
		if (this.type == Type.Atom && (this.name.equals("undef") || this.name.equals("funundef")))
			return true;
		return false;
	}
	public boolean isFictitious0()
	{
		if (this.type == Type.Root)
			return true;
		if (this.type == Type.Body)
			return true;
		if (this.type == Type.Return)
			return true;
		if (this.type == Type.ExceptionReturn)
			return true;
		if (this.type == Type.ListComprehensionResult)
			return true;
		return false;
	}
}