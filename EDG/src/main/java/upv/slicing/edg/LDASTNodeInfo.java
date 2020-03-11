package upv.slicing.edg;

import java.util.Arrays;

public class LDASTNodeInfo
{
	private String file;
	private String className;
	private final long line;
	private final String construction;
	private final boolean expression;
	private Object[] info;

	public LDASTNodeInfo(long line, String construction, Object... info)
	{
		this(null, null, line, false, construction, info);
	}
	public LDASTNodeInfo(long line, boolean expression, String construction,  Object... info)
	{
		this(null, null, line, expression, construction, info);
	}
	public LDASTNodeInfo(String file, long line, String construction, Object... info)
	{
		this(file, null, line, false, construction, info);
	}
	public LDASTNodeInfo(String file, long line, boolean expression, String construction, Object... info)
	{
		this(file, null, line, expression, construction, info);
	}
	public LDASTNodeInfo(String file, String className, long line, String construction, Object... info)
	{
		this(file, className, line, false, construction, info);
	}
	public LDASTNodeInfo(String file, String className, long line, boolean expression, String construction, Object... info)
	{
		this.file = file;
		this.className = className;
		this.line = line;
		this.construction = construction;
		this.expression = expression;
		this.info = info;
	}

	public LDASTNodeInfo(LDASTNodeInfo info, boolean expression) {
		this(info.file, info.className, info.line, expression, info.construction, Arrays.copyOf(info.info, info.info.length));
	}

	public String getFile()
	{
		return this.file;
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
		return this.expression;
	}
	public Object[] getInfo()
	{
		return this.info;
	}
	public void setFile(String file)
	{
		this.file = file;
	}
	public void setClassName(String className)
	{
		this.className = className;
	}
	public void addInfo(Object o)
	{
		int lastIndex = this.info.length;
		Object[] newInfo = new Object[lastIndex+1];
		System.arraycopy(this.info, 0, newInfo, 0, lastIndex);
		newInfo[lastIndex] = o;
		this.info = newInfo;
	}
}
