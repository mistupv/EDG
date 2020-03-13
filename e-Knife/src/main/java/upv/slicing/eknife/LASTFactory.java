package upv.slicing.eknife;

import upv.slicing.edg.graph.LAST;
import upv.slicing.eknife.java.JavaLASTFactory;

public class LASTFactory {
	public static LAST createLAST(EKnife.Language language, String sourcePath)
	{
		return LASTFactory.createLAST(language, sourcePath, true);
	}

	public static LAST createLAST(EKnife.Language language, String sourcePath, boolean generateArcs)
	{
		switch (language)
		{
			case Java:
				return JavaLASTFactory.createLAST(sourcePath, generateArcs);
			default:
				throw new RuntimeException("Language not contemplated: " + language);
		}
	}
}
