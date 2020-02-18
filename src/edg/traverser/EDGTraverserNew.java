package edg.traverser;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;

import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.graph.VariableInfo.Context;

public class EDGTraverserNew extends LASTTraverser
{
	public static Node getChild(Node node, NodeInfo.Type type)
	{
		final NodeInfo.Type nodeType = node.getData().getType();
		
		switch(nodeType)
		{
			case Clause:
				switch(type)
				{
					case ParameterIn:
						return EDGTraverserNew.getChild(node, 0);
					case Parameters: 
						return EDGTraverserNew.getChild(node, 1);
					case ParameterOut:
						return EDGTraverserNew.getChild(node, 2);
					case Guard:
						return EDGTraverserNew.getChild(node, 3);
					case Body:
						return EDGTraverserNew.getChild(node, 4);
					case Result:
						return EDGTraverserNew.getChild(node, 5);
				}
				break;
			case Call:
				switch(type)
				{
					case Callee:
						return EDGTraverserNew.getChild(node, 0);
					case ArgumentIn:
						return EDGTraverserNew.getChild(node, 1);
					case Arguments:
						return EDGTraverserNew.getChild(node, 2);
					case ArgumentOut:
						return EDGTraverserNew.getChild(node, 3);
				}
				break;
			case Callee:
				switch(type)
				{
					case Scope:
						return EDGTraverserNew.getChild(node, 0);
					case Name:
						return EDGTraverserNew.getChild(node, 1);
					case Result:
						return EDGTraverserNew.getChild(node, 2);
				}
				break;
			case If:
				switch(type)
				{
					case Condition:
						return EDGTraverserNew.getChild(node, 0);
					case Then:
						return EDGTraverserNew.getChild(node, 1);
					case Else:
						return EDGTraverserNew.getChild(node, 2);
				}
				break;
			case Switch:
				switch(type)
				{
					case Selector:
						return EDGTraverserNew.getChild(node, 0);
					case Cases:
						return EDGTraverserNew.getChild(node, 1);
				}
				break;
			case Case:
				switch(type)
				{
					case Selectable:
						return EDGTraverserNew.getChild(node, 0);
					case Body:
						return EDGTraverserNew.getChild(node, 1);
				}
				break;
			
			case FLoop:
				switch(type)
				{
					case Init:
						return EDGTraverserNew.getChild(node, 0);
					case Condition:
						return EDGTraverserNew.getChild(node, 1);
					case Body:
						return EDGTraverserNew.getChild(node, 2);
					case Update:
						return EDGTraverserNew.getChild(node, 3);
				}
				break;
			case CLoop:
				switch(type)
				{
					case Condition:
						return EDGTraverserNew.getChild(node, 0);
					case Body:
						return EDGTraverserNew.getChild(node, 1);
				}
				break;
			case RLoop:
				switch(type)
				{
					case Body:
						return EDGTraverserNew.getChild(node, 0);
					case Condition:
						return EDGTraverserNew.getChild(node, 1);
				}
				break;
			case ExHandler:
				switch(type)
				{
					case Try:
						return EDGTraverserNew.getChild(node, 0);
					case Catch:
						return EDGTraverserNew.getChild(node, 1);
					case Finally:
						return EDGTraverserNew.getChild(node, 2);
				}
				break;
			case TypeTransformation:
				switch(type)
				{
					case Type:
						return EDGTraverserNew.getChild(node, 0);
					case Variable:
						return EDGTraverserNew.getChild(node, 1);
				}
				break;
			case DataConstructorAccess:
				switch(type)
				{
					case Variable:
						return EDGTraverserNew.getChild(node, 0);
					case Index:
						return EDGTraverserNew.getChild(node, 1);		
				}
				break;
			case Expression:
			case Literal:
				switch(type)
				{
					case Value:
						return EDGTraverserNew.getChild(node, 0);
					case Result:
						return EDGTraverserNew.getChild(node, 1);
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