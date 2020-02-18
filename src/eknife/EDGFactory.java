package eknife;

import edg.graph.EDG;
import eknife.EKnife.Language;

public class EDGFactory
{
	public static EDG createEDG(Language language, String sourcePath)
	{
		return EDGFactory.createEDG(language, sourcePath, true);
	}
	public static EDG createEDG(Language language, String sourcePath, boolean generateArcs)
	{
		switch (language)
		{
			case Java:
				return eknife.java.JavaEDGFactory.createEDG(sourcePath, generateArcs);
			case Erlang:
				return eknife.erlang.ErlangEDGFactory.createEDG(sourcePath, generateArcs);
			case Php:
				return eknife.php.PhpEDGFactory.createEDG(sourcePath, generateArcs);
			default:
				throw new RuntimeException("Language not contemplated: " + language);
		}
	}
}