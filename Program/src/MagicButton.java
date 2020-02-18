import java.io.File;

import eknife.misc.Misc;

public class MagicButton
{
	public static void main(String[] args)
	{
		final String origin = args[0];
		final String destination = args[1];
		final int nodeId = Integer.parseInt(args[2]);
		final File originFile = new File(origin);
		final File originParentFile = originFile.getParentFile();
		final File destinationFile = new File(destination);
		final File dotFile = new File("./tmp/graph.dot");
		final String testPath =  originParentFile.getAbsolutePath();
		final String testName = originFile.getName();
		final TestCase test = new TestCase(testPath, testName, nodeId);

		Main.openTest(test);
		Misc.moveFile(dotFile, destinationFile);
	}
}