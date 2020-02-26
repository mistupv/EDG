package upv.slicing.edg.traverser;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Node;

public class EDGTraverser extends LASTTraverser
{
	public static Node getChild(EDG edg, Node node, Node.Type type)
	{
        final Node.Type nodeType = node.getType();

        switch (nodeType)
        {
            case Clause:
                switch (type)
                {
                    case ParameterIn:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Parameters:
                        return EDGTraverser.getChild(edg, node, 1);
                    case ParameterOut:
                        return EDGTraverser.getChild(edg, node, 2);
                    case Guard:
                        return EDGTraverser.getChild(edg, node, 3);
                    case Body:
                        return EDGTraverser.getChild(edg, node, 4);
                    case Result:
                        return EDGTraverser.getChild(edg, node, 5);
                }
                break;
            case Call:
                switch (type)
                {
                    case Callee:
                        return EDGTraverser.getChild(edg, node, 0);
                    case ArgumentIn:
                        return EDGTraverser.getChild(edg, node, 1);
                    case Arguments:
                        return EDGTraverser.getChild(edg, node, 2);
                    case ArgumentOut:
                        return EDGTraverser.getChild(edg, node, 3);
                }
                break;
            case Callee:
                switch (type)
                {
                    case Scope:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Name:
                        return EDGTraverser.getChild(edg, node, 1);
                    case Result:
                        return EDGTraverser.getResFromNode(edg, node);
                }
                break;
            case If:
                switch (type)
                {
                    case Condition:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Then:
                        return EDGTraverser.getChild(edg, node, 1);
                    case Else:
                        return EDGTraverser.getChild(edg, node, 2);
                }
                break;
            case Switch:
                switch (type)
                {
                    case Selector:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Cases:
                        return EDGTraverser.getChild(edg, node, 1);
                }
                break;
            case Case:
                switch (type)
                {
                    case Selectable:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Body:
                        return EDGTraverser.getChild(edg, node, 1);
                }
                break;

            case FLoop:
                switch (type)
                {
                    case Init:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Condition:
                        return EDGTraverser.getChild(edg, node, 1);
                    case Body:
                        return EDGTraverser.getChild(edg, node, 2);
                    case Update:
                        return EDGTraverser.getChild(edg, node, 3);
                }
                break;
            case CLoop:
                switch (type)
                {
                    case Condition:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Body:
                        return EDGTraverser.getChild(edg, node, 1);
                }
                break;
            case RLoop:
                switch (type)
                {
                    case Body:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Condition:
                        return EDGTraverser.getChild(edg, node, 1);
                }
                break;
            case ExHandler:
                switch (type)
                {
                    case Try:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Catch:
                        return EDGTraverser.getChild(edg, node, 1);
                    case Finally:
                        return EDGTraverser.getChild(edg, node, 2);
                }
                break;
            case TypeTransformation:
                switch (type)
                {
                    case Type:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Variable:
                        return EDGTraverser.getChild(edg, node, 1);
                }
                break;
            case FieldAccess:
            case DataConstructorAccess:
                switch (type)
                {
                    case Variable:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Index:
                        return EDGTraverser.getChild(edg, node, 1);
                }
                break;
            case Scope:
            case Name:
            case Expression:
            case Literal:
                switch (type)
                {
                    case Value:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Result:
                        return EDGTraverser.getChild(edg, node, 1);
                }
                break;
            case Equality:
                switch (type)
                {
                    case Pattern:
                        return EDGTraverser.getChild(edg, node, 0);
                    case Value:
                        return EDGTraverser.getChild(edg, node, 1);
                }
                break;
            default:
                break;
        }
        throw new IllegalStateException("Parent-Child combination not considered: " + nodeType + ", " + type);
    }

    public static Node getSibling(EDG edg, Node node, Node.Type type)
    {
        final Node parent = LASTTraverser.getParent(edg, node);

        return getChild(edg, parent, type);
    }


}