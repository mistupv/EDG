package eknife.java;

import java.io.File;
import java.util.List;

import edg.graph.EDG;
import edg.graph.Node;

public class JavaFactory
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static File createJavaFile(String outputPath, EDG EDG)
	{
		return JavaFactory.createJavaFile(outputPath, EDG, null);
	}
	public static File createJavaFile(String outputPath, EDG EDG, List<Node> slice)
	{
		return new JavaFactory().generate(EDG, slice);
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private EDG edg;
	private List<Node> slice;

	private JavaFactory()
	{
		
	}

	private File generate(EDG edg, List<Node> slice)
	{
		this.edg = edg;
		this.slice = slice;

		return null;
	}
}