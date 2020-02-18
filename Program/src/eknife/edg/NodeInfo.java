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
		Root, Other
	}

	private final int id;
	private final Type type;
	private final String module;
	private final String name;
	private final long arity;
	private final OtpErlangObject AST;

	public NodeInfo(int id, Type type)
	{
		this(id, type, null, null, 0, null);
	}
	public NodeInfo(int id, Type type, String name)
	{
		this(id, type, null, name, 0, null);
	}
	public NodeInfo(int id, Type type, String name, long arity)
	{
		this(id, type, null, name, arity, null);
	}
	public NodeInfo(int id, Type type, OtpErlangObject AST)
	{
		this(id, type, null, null, 0, AST);
	}
	public NodeInfo(int id, Type type, String module, String name, long arity)
	{
		this(id, type, module, name, arity, null);
	}
	private NodeInfo(int id, Type type, String module, String name, long arity, OtpErlangObject AST)
	{
		this.id = id;
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
}