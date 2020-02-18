package eknife;

import java.io.File;
import java.util.List;

import edg.graph.EDG;
import edg.graph.Node;
import eknife.EKnife.Language;

public class CodeFactory
{
	public static void createCode(Language language, File outputFile, EDG edg)
	{
		CodeFactory.createCode(language, outputFile, edg, null);
	}
	public static void createCode(Language language, File outputFile, EDG edg, List<Node> slice)
	{
		switch (language)
		{
			case Java:
				eknife.java.JavaCodeFactoryNew.createJavaFile(outputFile, edg, slice);
				break;
			case Erlang:
				eknife.erlang.ErlangCodeFactory.createErlangFile(outputFile, edg, slice);
				break;
			case Php:
				eknife.php.PhpCodeFactory.createPhpFile(outputFile, edg, slice);
				break;
			default:
				throw new RuntimeException("Language not contemplated: " + language);
		}
	}
}