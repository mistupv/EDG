import java.io.IOException;

import org.antlr.v4.Tool;

import eknife.php.PhpEDGFactory;

public class AntlrTest
{
	public static void main1(String[] args)
	{
		final Tool antlr = new Tool(new String[] { "PHPLexer.g4", "PHPParser.g4" });
		antlr.processGrammarsOnCommandLine();
	}
	public static void main(String[] args) throws Exception
	{
//		final String path = "/Library/WebServer/Documents/Alerts/alerts/src/logic/User.php";
		final String path = "/Library/WebServer/Documents/Alerts/alerts/src/logic/Alert.php";
		org.antlr.v4.gui.TestRig.main(new String[] { "eknife.php.PHP", "htmlDocument", "-gui", path });
	}
	public static void main4(String[] args) throws IOException
	{
//		final String path = "/Library/WebServer/Documents/Alerts/alerts/src/logic/User.php";
		final String path = "/Library/WebServer/Documents/Alerts/alerts/src/logic/Alert.php";
		PhpEDGFactory.createEDG(path);
	}
}