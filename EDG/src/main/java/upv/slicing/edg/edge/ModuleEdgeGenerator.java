package upv.slicing.edg.edge;

import upv.slicing.edg.LASTBuilder;
import upv.slicing.edg.LASTBuilder.ClassInfo;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;


import java.util.*;

public class ModuleEdgeGenerator extends EdgeGenerator {

    public ModuleEdgeGenerator(EDG edg) {
        super(edg);
    }

    public void generate() {
        this.createClassEdges();
    }

    public void createClassEdges() {
        final Node root = edg.getRootNode();
        final List<Node> modules = edg.getChildren(root);

        for (Node module : modules){
            final ClassInfo classInfo = (ClassInfo) module.getInfo().getInfo()[2];
            final List<ClassInfo> childrenCI = classInfo.getChildrenClasses();
            for (ClassInfo ci : childrenCI){
                edg.addEdge(module, ci.getClassNode(), Edge.Type.Class);
            }
        }
    }

    public Node getModuleByName(List<Node> modules, String moduleName){
        for (Node module : modules)
            if (module.getName().equals(moduleName))
                return module;
        return null;
    }
}
