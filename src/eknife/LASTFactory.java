package eknife;

import edg.graph.LAST;
import eknife.EKnife.Language;

public class LASTFactory {
	public static LAST createLAST(Language language, String sourcePath)
	{
		return LASTFactory.createLAST(language, sourcePath, true);
	}

	public static LAST createLAST(Language language, String sourcePath, boolean generateArcs)
	{
		switch (language)
		{
			case Java:
				return eknife.java.JavaLASTFactory.createLAST(sourcePath, generateArcs);
//			case Erlang:
//				return eknife.erlang.ErlangEDGFactory.createEDG(sourcePath, generateArcs);
//			case Php:
//				return eknife.php.PhpEDGFactory.createEDG(sourcePath, generateArcs);
			default:
				throw new RuntimeException("Language not contemplated: " + language);
		}
	}
}