package upv.slicing.eknife;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Node;
import upv.slicing.eknife.java.JavaCodeFactory;

import java.io.File;
import java.util.Set;

public class CodeFactory
{
	public static void createCode(EKnife.Language language, File outputFile, EDG edg)
	{
		CodeFactory.createCode(language, outputFile, edg, null);
	}
	public static void createCode(EKnife.Language language, File outputFile, EDG edg, Set<Node> slice)
	{
		switch (language)
		{
			case Java:
				JavaCodeFactory.createJavaFile(outputFile, edg, slice);
				break;
//			case Erlang:
//				eknife.erlang.ErlangCodeFactory.createErlangFile(outputFile, edg, slice);
//				break;
//			case Php:
//				eknife.php.PhpCodeFactory.createPhpFile(outputFile, edg, slice);
//				break;
			default:
				throw new RuntimeException("Language not contemplated: " + language);
		}
	}
}