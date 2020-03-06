package upv.slicing.eknife.java;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import upv.slicing.edg.DotFactory;
import upv.slicing.edg.EDGFactory;
import upv.slicing.edg.PdfFactory;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.slicing.ConstrainedAlgorithm;
import upv.slicing.edg.slicing.SlicingAlgorithm;
import upv.slicing.edg.slicing.SlicingCriterion;
import upv.slicing.eknife.EKnife.Language;
import upv.slicing.eknife.LASTFactory;

import java.io.File;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class JavaCodeTest {
	private static final String CODE_ROOT = "./src/test/resources/JavaCodeFactory/";

	static Iterable<Arguments> genTests()
	{
		List<Arguments> argList = new LinkedList<>();
		argList.add(Arguments.of("ifTest.java", new SlicingCriterion("ifTest.java", 18, "x")));
		argList.add(Arguments.of("ifTest.java", new SlicingCriterion("ifTest.java", 19, "y")));
		argList.add(Arguments.of("ifTest.java", new SlicingCriterion("ifTest.java", 20, "z")));

		argList.add(Arguments.of("equalityTest.java", new SlicingCriterion("equalityTest.java", 6, "d")));
		argList.add(Arguments.of("equalityTest.java", new SlicingCriterion("equalityTest.java", 6, "b")));
		argList.add(Arguments.of("equalityTest.java", new SlicingCriterion("equalityTest.java", 8, "e")));
		argList.add(Arguments.of("equalityTest.java", new SlicingCriterion("equalityTest.java", 8, "b")));

		argList.add(Arguments.of("operationTest.java", new SlicingCriterion("operationTest.java", 8, "d")));
		argList.add(Arguments.of("operationTest.java", new SlicingCriterion("operationTest.java", 8, "a")));
		argList.add(Arguments.of("operationTest.java", new SlicingCriterion("operationTest.java", 8, "c", 2)));
		argList.add(Arguments.of("operationTest.java", new SlicingCriterion("operationTest.java", 10, "e")));

		argList.add(Arguments.of("callTest.java", new SlicingCriterion("callTest.java", 6, "z")));
		argList.add(Arguments.of("callTest.java", new SlicingCriterion("callTest.java", 7, "w")));
		argList.add(Arguments.of("callTest.java", new SlicingCriterion("callTest.java", 10, "a")));
		argList.add(Arguments.of("callTest.java", new SlicingCriterion("callTest.java", 13, "d")));

		argList.add(Arguments.of("whileTest.java", new SlicingCriterion("whileTest.java", 9, "x")));
		argList.add(Arguments.of("whileTest.java", new SlicingCriterion("whileTest.java", 10, "y")));

		argList.add(Arguments.of("doWhileTest.java", new SlicingCriterion("doWhileTest.java", 10, "x")));
		argList.add(Arguments.of("doWhileTest.java", new SlicingCriterion("doWhileTest.java", 11, "y")));

		argList.add(Arguments.of("forTest.java", new SlicingCriterion("forTest.java", 6, "i")));
		argList.add(Arguments.of("forTest.java", new SlicingCriterion("forTest.java", 6, "i", 2)));
		argList.add(Arguments.of("forTest.java", new SlicingCriterion("forTest.java", 15, "i")));
		argList.add(Arguments.of("forTest.java", new SlicingCriterion("forTest.java", 16, "total")));

		argList.add(Arguments.of("foreachTest.java", new SlicingCriterion("foreachTest.java", 7, "array")));
		argList.add(Arguments.of("foreachTest.java", new SlicingCriterion("foreachTest.java", 7, "i")));
		argList.add(Arguments.of("foreachTest.java", new SlicingCriterion("foreachTest.java", 10, "sum")));

		argList.add(Arguments.of("dataConstructorAndAccessTest.java", new SlicingCriterion("dataConstructorAndAccessTest.java", 7, "array")));
		argList.add(Arguments.of("dataConstructorAndAccessTest.java", new SlicingCriterion("dataConstructorAndAccessTest.java", 8, "z")));
		argList.add(Arguments.of("dataConstructorAndAccessTest.java", new SlicingCriterion("dataConstructorAndAccessTest.java", 10, "array")));
		argList.add(Arguments.of("dataConstructorAndAccessTest.java", new SlicingCriterion("dataConstructorAndAccessTest.java", 10, "w")));

		argList.add(Arguments.of("fieldAccessTest.java", new SlicingCriterion("fieldAccessTest.java", 5, "a", 1)));
		argList.add(Arguments.of("fieldAccessTest.java", new SlicingCriterion("fieldAccessTest.java", 5, "a", 2)));
		argList.add(Arguments.of("fieldAccessTest.java", new SlicingCriterion("fieldAccessTest.java", 5, "y")));
		argList.add(Arguments.of("fieldAccessTest.java", new SlicingCriterion("fieldAccessTest.java", 5, "x")));

		argList.add(Arguments.of("castingTest.java", new SlicingCriterion("castingTest.java", 5, "a")));
		argList.add(Arguments.of("castingTest.java", new SlicingCriterion("castingTest.java", 5, "d")));

		argList.add(Arguments.of("instanceOfTest.java", new SlicingCriterion("castingTest.java", 5, "bool")));
		argList.add(Arguments.of("instanceOfTest.java", new SlicingCriterion("castingTest.java", 5, "b")));

		argList.add(Arguments.of("ternaryTest.java", new SlicingCriterion("ternaryTest.java", 5, "c")));
		argList.add(Arguments.of("ternaryTest.java", new SlicingCriterion("ternaryTest.java", 7, "d")));
		argList.add(Arguments.of("ternaryTest.java", new SlicingCriterion("ternaryTest.java", 8, "e")));

		argList.add(Arguments.of("switchTest.java", new SlicingCriterion("switchTest.java", 18, "a")));
		argList.add(Arguments.of("switchTest.java", new SlicingCriterion("switchTest.java", 19, "d")));
		argList.add(Arguments.of("switchTest.java", new SlicingCriterion("switchTest.java", 20, "c")));
		argList.add(Arguments.of("switchTest.java", new SlicingCriterion("switchTest.java", 23, "d")));

		argList.add(Arguments.of("tryCatchFinallyTest.java", new SlicingCriterion("tryCatchFinallyTest.java", 15, "e")));
		argList.add(Arguments.of("tryCatchFinallyTest.java", new SlicingCriterion("tryCatchFinallyTest.java", 18, "d")));
		argList.add(Arguments.of("tryCatchFinallyTest.java", new SlicingCriterion("tryCatchFinallyTest.java", 25, "f")));
		argList.add(Arguments.of("tryCatchFinallyTest.java", new SlicingCriterion("tryCatchFinallyTest.java", 26, "c")));
		argList.add(Arguments.of("tryCatchFinallyTest.java", new SlicingCriterion("tryCatchFinallyTest.java", 27, "d")));

		return argList;
	}
	
	@ParameterizedTest
	@MethodSource("genTests")
	void test(String className, SlicingCriterion slicingCriterion)
	{
		String sourcePath = CODE_ROOT + className;

		String outName = className + "_" + slicingCriterion.getLine() + "_" + slicingCriterion.getName();

		final String outputDotPath = CODE_ROOT + outName + ".dot";
		final String outputPdfPath = CODE_ROOT + outName + ".pdf";
		final String outputJavaPath = CODE_ROOT + outName + ".java";
		final File outputDotFile = new File(outputDotPath);
		final File outputPdfFile = new File(outputPdfPath);
		final File outputJavaFile = new File(outputJavaPath);

		final LAST last = LASTFactory.createLAST(Language.Java, sourcePath);
		final EDG edg = new EDGFactory(last).createEDG();

		final Node SC = edg.getNode(slicingCriterion);
		final SlicingAlgorithm slicingAlgorithm = new ConstrainedAlgorithm(edg);
		final Set<Node> slice = slicingAlgorithm.slice(SC);

		DotFactory.createDot(outputDotFile, edg, SC, slice);
		PdfFactory.createPdf(outputPdfFile, outputDotFile);
		JavaCodeFactory.createJavaFile(outputJavaFile, edg, slice);
	}
}
