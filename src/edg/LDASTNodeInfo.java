package edg;

public class LDASTNodeInfo
{
	private String archive;
	private String className;
	private final long line;
	private final String construction;
	private Object[] info;

	public LDASTNodeInfo(long line, String construction, Object... info)
	{
		this(null, null, line, construction, info);
	}
	public LDASTNodeInfo(String archive, long line, String construction, Object... info)
	{
		this(archive, null, line, construction, info);
	}
	
	public LDASTNodeInfo(String archive, String className, long line, String construction, Object... info)
	{
		this.archive = archive;
		this.className = className;
		this.line = line;
		this.construction = construction;
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