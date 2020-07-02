package upv.slicing.edg.edge;

import upv.slicing.edg.graph.*;
import upv.slicing.edg.traverser.ControlFlowTraverser;

import java.util.*;

public class DynamicTypesGenerator extends EdgeGenerator {

    private class DynamicTypeWork{
        Node node;
        Map<String, List<String>> dynTypeMap;


        private DynamicTypeWork(Node node, Map<String,List<String>> map)
        {
            this.node = node;
            this.dynTypeMap = map;
        }
        public String toString() {
            return this.node.toString() + " " + this.dynTypeMap.toString();
        }

        public boolean equals(Object o) {
            if (o == this)
                return true;
            if (!(o instanceof DynamicTypeWork))
                return false;

            final DynamicTypeWork dtw = (DynamicTypeWork) o;
            return Objects.equals(this.node, dtw.node) &&
                    Objects.equals(this.dynTypeMap, dtw.dynTypeMap);
        }

        public int hashCode() {
            return Objects.hash(node.getId(), dynTypeMap);
        }
    }

    public DynamicTypesGenerator(EDG edg) {
        super(edg);
    }

    public void generate() {
        this.addDynamicTypes();
    }

    public void addDynamicTypes(){
        final List<Node> clauses = edg.getNodes(Node.Type.Clause);
        final Map<String,List<String>> dynamicTypeMap = new HashMap<>();
        for (Node clause : clauses){
            final Set<DynamicTypeWork> pendingWorks = new HashSet<>();
            final Set<DynamicTypeWork> doneWorks = new HashSet<>();

            final Set<Node> initialNodes = ControlFlowTraverser.step(edg, clause, LAST.Direction.Forwards);
            initialNodes.removeIf(node -> node.getType() != Node.Type.ParameterIn);
            for (Node initialNode : initialNodes) {
                pendingWorks.add(new DynamicTypeWork(initialNode, dynamicTypeMap));
            }

            while(!pendingWorks.isEmpty()){
                final DynamicTypeWork work = pendingWorks.iterator().next();
                final Node workNode = work.node;
                pendingWorks.remove(work);

                if (workNode == clause || doneWorks.contains(work))
                    continue;

                if (workNode.getType() == Node.Type.Variable) {
                    final Variable var = (Variable) workNode;
                    if (edg.getParent(workNode).getType() == Node.Type.Parameters ||
                            this.isPrimitiveType(var.getStaticType())) {
                        // TODO: DYNAMIC TYPES NOT ANALYZED FOR PARAMETERS
                        final List<String> varTypes = new LinkedList<>();
                        varTypes.add(var.getStaticType());
                        work.dynTypeMap.put(var.getName(), varTypes);
                    } else {
                        switch (var.getContext()) {
                            case Use:
                            case Def_Use:
                                List<String> dynTypes = work.dynTypeMap.get(var.getName());
                                if (dynTypes == null)
                                    if (var.isGlobal()) {
                                        dynTypes = new LinkedList<>();
                                        dynTypes.add(var.getStaticType());
                                        work.dynTypeMap.put(var.getName(), dynTypes);
                                    }
                                    else
                                        throw new RuntimeException("The dynamic type of a variable cannot be null");

                                var.addDynamicTypes(dynTypes);
                                break;
                            case Definition:
                                final Node parentNode = edg.getParent(workNode);
                                if (parentNode.getType() == Node.Type.Equality) {
                                    final Node rightHandSide = edg.getChild(parentNode, Node.Type.Value);
                                    switch (rightHandSide.getType()){
                                        case Variable:
                                            final List<String> dynamicTypes = ((Variable) rightHandSide).getDynamicTypes();
                                            var.setDynamicTypes(dynamicTypes);
                                            work.dynTypeMap.put(var.getName(), dynamicTypes);
                                            break;
                                        case Call:
                                            final Node callee = edg.getChild(rightHandSide, Node.Type.Callee);
                                            final Node name = edg.getChild(callee, Node.Type.Name);
                                            final Node scope = edg.getChild(callee, Node.Type.Scope);
                                            final String funName = edg.getChild(name,0).getName();
                                            final List<String> dynamicTypes0 = new LinkedList<>();
                                            if (funName.equals("<constructor>")) {
                                                final String scopeType = edg.getChild(scope,0).getName();
                                                dynamicTypes0.add(scopeType);
                                            } else {
                                                // TODO: Possible Return Types
                                                dynamicTypes0.add("StaticType");
                                            }
                                            work.dynTypeMap.put(var.getName(), dynamicTypes0);
                                            var.setDynamicTypes(dynamicTypes0);
                                            break;
                                        default:
                                            break;
                                    }
                                }

                                break;
                            default:
                                break;
                        }
                    }
                }
                doneWorks.add(work);
                final Set<Node> nextNodes = ControlFlowTraverser.step(edg, workNode, LAST.Direction.Forwards);
                nextNodes.forEach(nextNode -> pendingWorks.add(new DynamicTypeWork(nextNode, new HashMap<>(work.dynTypeMap))));
            }
        }
    }

    public boolean isPrimitiveType(String type)
    {
        String rawType = type;
        if(type.endsWith("[]"))
            rawType = type.substring(0,type.lastIndexOf("["));
        switch(rawType)
        {
            case "boolean":
            case "byte":
            case "short":
            case "int":
            case "long":
            case "float":
            case "double":
            case "char":
                return true;
            default:
                return false;
        }
    }
    
}
