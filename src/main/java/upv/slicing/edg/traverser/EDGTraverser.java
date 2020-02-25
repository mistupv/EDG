package upv.slicing.edg.traverser;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Node;

public class EDGTraverser extends LASTTraverser
{
    public static Node getChild(EDG edg, Node node, Node.Type type)
    {
        int index = getChildIndex(node, type);
        if (index == -1)
            return null;
        return EDGTraverser.getChild(edg, node, index);
    }

	private static int getChildIndex(Node node, Node.Type type)
	{
        final Node.Type nodeType = node.getType();

        switch (nodeType)
        {
            case Clause:
                switch (type)
                {
                    case ParameterIn:  return 0;
                    case Parameters:   return 1;
                    case ParameterOut: return 2;
                    case Guard:        return 3;
                    case Body:         return 4;
                    case Result:       return 5;
                }
                break;
            case Call:
                switch (type)
                {
                    case Callee:      return 0;
                    case ArgumentIn:  return 1;
                    case Arguments:   return 2;
                    case ArgumentOut: return 3;
                }
                break;
            case Callee:
                switch (type)
                {
                    case Scope:  return 0;
                    case Name:   return 1;
                    case Result: return 2;
                }
                break;
            case If:
                switch (type)
                {
                    case Condition: return 0;
                    case Then:      return 1;
                    case Else:      return 2;
                }
                break;
            case Switch:
                switch (type)
                {
                    case Selector: return 0;
                    case Cases:    return 1;
                }
                break;
            case Case:
                switch (type)
                {
                    case Selectable: return 0;
                    case Body:       return 1;
                }
                break;

            case FLoop:
                switch (type)
                {
                    case Init:      return 0;
                    case Condition: return 1;
                    case Body:      return 2;
                    case Update:    return 3;
                }
                break;
            case CLoop:
                switch (type)
                {
                    case Condition: return 0;
                    case Body:      return 1;
                }
                break;
            case RLoop:
                switch (type)
                {
                    case Body:      return 0;
                    case Condition: return 1;
                }
                break;
            case ExHandler:
                switch (type)
                {
                    case Try:     return 0;
                    case Catch:   return 1;
                    case Finally: return 2;
                }
                break;
            case TypeTransformation:
                switch (type)
                {
                    case Type:     return 0;
                    case Variable: return 1;
                }
                break;
            case FieldAccess:
            case DataConstructorAccess:
                switch (type)
                {
                    case Variable: return 0;
                    case Index:    return 1;
                }
                break;
            case Expression:
            case Literal:
                switch (type)
                {
                    case Value:  return 0;
                    case Result: return 1;
                }
            default:
                break;
        }
        return -1;
    }

    public static Node getSibling(EDG edg, Node node, Node.Type type)
    {
        final Node parent = LASTTraverser.getParent(edg, node);
        return getChild(edg, parent, type);
    }
}
