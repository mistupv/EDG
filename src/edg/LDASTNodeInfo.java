package edg;

public class LDASTNodeInfo
{
	private String archive;
	private String className;
	private final long line;
	private final String construction;
	private final boolean isExpression;
	private Object[] info;

	public LDASTNodeInfo(long line, String construction, Object... info)
	{
		this(null, null, line, false, construction, info);
	}
	public LDASTNodeInfo(long line, boolean expression, String construction,  Object... info)
	{
		this(null, null, line, expression, construction, info);
	}
	public LDASTNodeInfo(String archive, long line, String construction, Object... info)
	{
		this(archive, null, line, false, construction, info);
	}
	public LDASTNodeInfo(String archive, long line, boolean expression, String construction, Object... info)
	{
		this(archive, null, line, expression, construction, info);
	}
	public LDASTNodeInfo(String archive, String className, long line, String construction, Object... info)
	{
		this(archive, className, line, false, construction, info);
	}
	public LDASTNodeInfo(String archive, String className, long line, boolean expression, String construction, Object... info)
	{
		this.archive = archive;
		this.className = className;
		this.line = line;
		this.construction = construction;
		this.isExpression = expression;
		this.info = info;
	}

	public String getArchive()
	{
		return this.archive;
	}
	public String getClassName()
	{
		return this.className;
	}
	public long getLine()
	{
		return this.line;
	}
	public String getConstruction()
	{
		return this.construction;
	}
	public boolean isExpression()
	{
		return this.isExpression;
	}
	public Object[] getInfo()
	{
		return this.info;
	}
	public void setArchive(String archive)
	{
		this.archive = archive;
	}
	public void setClassName(String className)
	{
		this.className = className;
	}
	public void addInfo(Object o)
	{
		int lastIndex = this.info.length;
		Object[] newInfo = new Object[lastIndex+1];
		for (int i = 0; i < lastIndex; i++)
			newInfo[i] = this.info[i];
		newInfo[lastIndex] = o;
		this.info = newInfo;
	}
}