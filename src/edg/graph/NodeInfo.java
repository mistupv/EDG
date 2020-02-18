package edg.graph;

public class NodeInfo
{
	// TODO Group types
	public static enum Type
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

	private static int nextId = 0;

	private final int id;
	private final Type type;
	private final long line;
	private final String text;
	private final Object[] info;

	public NodeInfo(Type type, long line, String text, Object... info)
	{
		this(NodeInfo.nextId++, type, line, text, info);
	}
	public NodeInfo(int id, Type type, long line, String text, Object... info)
	{
		this.id = id;
		this.line = line;
		this.type = type;
		this.text = text;
		this.info = info;
	}

	public int getId()
	{
		return this.id;
	}
	public Type getType()
	{
		return this.type;
	}
	public long getLine()
	{
		return this.line;
	}
	public String getText()
	{
		return this.text;
	}
	public Object[] getInfo()
	{
		return this.info;
	}

	public boolean isFictitious()
	{
		if (this.type == Type.Root)
			return true;
		if (this.type == Type.Body)
			return true;
		if (this.type == Type.Return)
			return true;
		if (this.type == Type.ListComprehensionResult)
			return true;
		if (this.type == Type.Variable && this.text.equals("_"))
			return true;
		if (this.type == Type.Atom && (this.text.equals("undef") || this.text.equals("funundef")))
			return true;
		return false;
	}
}