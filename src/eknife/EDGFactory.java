package eknife;

import edg.graph.EDG;
import eknife.EKnife.Language;

public class EDGFactory
{
	public static EDG createEDG(Language language, String sourcePath)
	{
		return EDGFactory.createEDG(language, sourcePath, true, true);
	}
	public static EDG createEDG(Language language, String sourcePath, boolean createDependencies)
	{
		return EDGFactory.createEDG(language, sourcePath, createDependencies, true);
	}
	public static EDG createEDG(Language language, String sourcePath, boolean createDependencies, boolean constraintsActivated)
	{
		switch (language)
		{
			case Java:
				return eknife.java.EDGFactory.createEDG(sourcePath, createDependencies, constraintsActivated);
			case Erlang:
				return eknife.erlang.EDGFactory.createEDG(sourcePath, createDependencies, constraintsActivated);
			default:
				throw new RuntimeException("Language not contemplated: " + language);
		}
	}
}