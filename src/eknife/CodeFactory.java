package eknife;

import java.io.File;
import java.util.List;

import edg.graph.EDG;
import edg.graph.Node;
import eknife.EKnife.Language;

public class CodeFactory
{
	public static File createCode(Language language, String outputPath, EDG edg)
	{
		return CodeFactory.createCode(language, outputPath, edg, null);
	}
	public static File createCode(Language language, String outputPath, EDG edg, List<Node> slice)
	{
		switch (language)
		{
			case Java:
				return eknife.java.JavaFactory.createJavaFile(outputPath, edg, slice);
			case Erlang:
				return eknife.erlang.ErlangFactory.createErlangFile(outputPath, edg, slice);
			default:
				throw new RuntimeException("Language not contemplated: " + language);
		}
	}
}