import java.io.File;
import java.util.List;

import edg.graph.EDG;
import edg.graph.Node;
import edg.slicing.ConstrainedAlgorithm;
import edg.slicing.SlicingAlgorithm;
import edg.slicing.SlicingCriterion;
import eknife.CodeFactory;
import eknife.EDGFactory;
import eknife.EKnife.Language;


public class Main {
	public static void main(String[] args) // java -jar eknife.jar "/Users/serperu/Desktop/Benchmarks/Test.java" 5 "a" 1  -> FULL_CLASS_PATH LINENO VARNAME OCCURRENCE
	{			
		final String sourcePath = args[0]; 
		final String lineNo = args[1];
		final String varName = args[2];
		final String occurrence = args[3];		
		
//		final String sourcePath = "/Users/serperu/Desktop/Benchmarks/Test.java";
//		final String lineNo = "5";
//		final String varName = "a";
//		final String occurrence = "1";
		
		final int lastSeparator = sourcePath.lastIndexOf(File.separator);
		final String className = sourcePath.substring(lastSeparator + 1);
		final String codebase = sourcePath.substring(0, lastSeparator + 1);
		
		final SlicingCriterion slicingCriterion = new SlicingCriterion(className, Integer.parseInt(lineNo), varName, Integer.parseInt(occurrence));
		
		final String outputJavaPath = codebase + "output.java";
		final File outputJavaFile = new File(outputJavaPath);

		final EDG edg = EDGFactory.createEDG(Language.Java, sourcePath);	
		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm();
		final List<Node> slice = slicingAlgorithm.slice(SC); 

		System.out.println("************************");
		System.out.println("****** EDG SLICE *******");
		System.out.println("************************\n");
		
		CodeFactory.createCode(Language.Java, outputJavaFile, edg, slice);
	}
}
