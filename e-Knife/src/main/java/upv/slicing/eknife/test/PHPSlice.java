package upv.slicing.eknife.test;

import upv.slicing.edg.Config;
import upv.slicing.edg.EDGFactory;
import upv.slicing.edg.PdfFactory;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.slicing.SlicingAlgorithm;
import upv.slicing.edg.slicing.SlicingCriterion;
import upv.slicing.eknife.CodeFactory;
import upv.slicing.eknife.EKnife;
import upv.slicing.eknife.LASTFactory;

import java.io.File;
import java.util.Set;

public class PHPSlice {
    public static void main(String args[]){
        // Slice con nombre del input file en OutputDir
        // Grafo en OutputDir (*EDG.svg)

        final File sourcePath = new File(args[1]);
		final SlicingCriterion slicingCriterion = new SlicingCriterion(sourcePath.getName(), Integer.parseInt(args[5]), args[7], Integer.parseInt(args[9]));

        final File outputPath = new File(args[3]);

        final File outputSvgFile = new File(outputPath, "EDG.svg");
        final File outputJavaFile = new File(outputPath, sourcePath.getName());

        System.out.println("\n***********************************************");
        System.out.println("Object-Oriented e-Knife Version 0.1 (22/07/2020)");
        System.out.println("***********************************************\n");

        final LAST last = LASTFactory.createLAST(EKnife.Language.Java, sourcePath.toString());
        final EDG edg = new EDGFactory(last).createEDG();

        final Node SC = edg.getNode(slicingCriterion);
        final SlicingAlgorithm slicingAlgorithm = Config.CREATE_SLICING_ALGORITHM.apply(edg);
        final Set<Node> slice = slicingAlgorithm.slice(SC);

        PdfFactory.createSvg(outputSvgFile, edg, SC, slice);
        CodeFactory.createCode(EKnife.Language.Java, outputJavaFile, edg, slice);

    }
}
