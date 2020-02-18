public class TestCase
{
	private final String testPath;
	private final String testName;
	private final int nodeId;

	public TestCase(String testPath, String testName, int nodeId)
	{
		this.testPath = testPath;
		this.testName = testName;
		this.nodeId = nodeId;
	}

	public String getTestPath()
	{
		return this.testPath;
	}
	public String getTestName()
	{
		return this.testName;
	}
	public int getNodeId()
	{
		return this.nodeId;
	}
}