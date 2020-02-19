package upv.slicing.edg.traverser;

import upv.slicing.edg.graph.Node;
import upv.slicing.edg.graph.NodeInfo;

public class EDGTraverser extends LASTTraverser
{
	public static Node getChild(Node node, NodeInfo.Type type)
	{
        final NodeInfo.Type nodeType = node.getData().getType();

        switch (nodeType)
        {
            case Clause:
                switch (type)
                {
                    case ParameterIn:
                        return EDGTraverser.getChild(node, 0);
                    case Parameters:
                        return EDGTraverser.getChild(node, 1);
                    case ParameterOut:
                        return EDGTraverser.getChild(node, 2);
                    case Guard:
                        return EDGTraverser.getChild(node, 3);
                    case Body:
                        return EDGTraverser.getChild(node, 4);
                    case Result:
                        return EDGTraverser.getChild(node, 5);
                }
                break;
            case Call:
                switch (type)
                {
                    case Callee:
                        return EDGTraverser.getChild(node, 0);
                    case ArgumentIn:
                        return EDGTraverser.getChild(node, 1);
                    case Arguments:
                        return EDGTraverser.getChild(node, 2);
                    case ArgumentOut:
                        return EDGTraverser.getChild(node, 3);
                }
                break;
            case Callee:
                switch (type)
                {
                    case Scope:
                        return EDGTraverser.getChild(node, 0);
                    case Name:
                        return EDGTraverser.getChild(node, 1);
                    case Result:
                        return EDGTraverser.getChild(node, 2);
                }
                break;
            case If:
                switch (type)
                {
                    case Condition:
                        return EDGTraverser.getChild(node, 0);
                    case Then:
                        return EDGTraverser.getChild(node, 1);
                    case Else:
                        return EDGTraverser.getChild(node, 2);
                }
                break;
            case Switch:
                switch (type)
                {
                    case Selector:
                        return EDGTraverser.getChild(node, 0);
                    case Cases:
                        return EDGTraverser.getChild(node, 1);
                }
                break;
            case Case:
                switch (type)
                {
                    case Selectable:
                        return EDGTraverser.getChild(node, 0);
                    case Body:
                        return EDGTraverser.getChild(node, 1);
                }
                break;

            case FLoop:
                switch (type)
                {
                    case Init:
                        return EDGTraverser.getChild(node, 0);
                    case Condition:
                        return EDGTraverser.getChild(node, 1);
                    case Body:
                        return EDGTraverser.getChild(node, 2);
                    case Update:
                        return EDGTraverser.getChild(node, 3);
                }
                break;
            case CLoop:
                switch (type)
                {
                    case Condition:
                        return EDGTraverser.getChild(node, 0);
                    case Body:
                        return EDGTraverser.getChild(node, 1);
                }
                break;
            case RLoop:
                switch (type)
                {
                    case Body:
                        return EDGTraverser.getChild(node, 0);
                    case Condition:
                        return EDGTraverser.getChild(node, 1);
                }
                break;
            case ExHandler:
                switch (type)
                {
                    case Try:
                        return EDGTraverser.getChild(node, 0);
                    case Catch:
                        return EDGTraverser.getChild(node, 1);
                    case Finally:
                        return EDGTraverser.getChild(node, 2);
                }
                break;
            case TypeTransformation:
                switch (type)
                {
                    case Type:
                        return EDGTraverser.getChild(node, 0);
                    case Variable:
                        return EDGTraverser.getChild(node, 1);
                }
                break;
            case FieldAccess:
            case DataConstructorAccess:
                switch (type)
                {
                    case Variable:
                        return EDGTraverser.getChild(node, 0);
                    case Index:
                        return EDGTraverser.getChild(node, 1);
                }
                break;
            case Expression:
            case Literal:
                switch (type)
                {
                    case Value:
                        return EDGTraverser.getChild(node, 0);
                    case Result:
                        return EDGTraverser.getChild(node, 1);
                }
            default:
                break;
        }
        return null;
    }

    public static Node getSibling(Node node, NodeInfo.Type type)
    {
        final Node parent = LASTTraverser.getParent(node);

        return getChild(parent, type);
    }


}