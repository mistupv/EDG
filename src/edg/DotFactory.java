package edg;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import edg.constraint.EdgeConstraint;
import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import misc.Misc;

public class DotFactory
{
	public static void createDot(File outputFile, EDG edg)
	{
		DotFactory.createDot(outputFile, edg, null, null, null);
	}
	public static void createDot(File outputFile, EDG edg, Map<EdgeInfo.Type, Boolean> edgeFlags)
	{
		DotFactory.createDot(outputFile, edg, null, null, edgeFlags);
	}
	public static void createDot(File outputFile, EDG edg, Node slicingCriterion, List<Node> slice)
	{
		DotFactory.createDot(outputFile, edg, slicingCriterion, slice, null);
	}
	public static void createDot(File outputFile, EDG edg, Node slicingCriterion, List<Node> slice, Map<EdgeInfo.Type, Boolean> edgeFlags)
	{
		final List<Node> nodes = edg.getNodes();
		final List<Edge> edges = edg.getEdges();
		String text = "digraph PDG {\n";
		int lines = 0;

		Misc.write(outputFile, "", false);
		for (Node node : nodes)
		{
			final boolean sliceCriterion = node == slicingCriterion;
			final boolean sliceNode = slice != null && slice.contains(node);
			final String nodeText = DotFactory.parseNode(node, sliceCriterion, sliceNode);
			text += "\t" + nodeText + ";\n";

			if (++lines % 100 == 0)
			{
				Misc.write(outputFile, text, true);
				text = "";
			}
		}
		for (Edge edge : edges)
		{
			final EdgeInfo.Type edgeType = edge.getData().getType();
			if (edgeType != EdgeInfo.Type.Structural && edgeFlags != null && !edgeFlags.get(edgeType))
				continue;

			final String edgeText = DotFactory.parseEdge(edge);
			text += edgeText.isEmpty() ? "" : "\t" + edgeText + ";\n";

			if (++lines % 100 == 0)
			{
				Misc.write(outputFile, text, true);
				text = "";
			}
		}
		text += "}";

		Misc.write(outputFile, text, true);
	}

	private static String parseNode(Node node, boolean sliceCriterion, boolean sliceNode)
	{
		final String id = node.getData().getId() + "";
		final String name = node.getName().replace("\n", "\\n");
		String text = "";

		text += id + " ";
		text += "[";
		text += "shape=ellipse ";
		text += "penwidth=" + (sliceCriterion ? "4" : "1") + " ";
		text += "style=filled ";
		text += "color=\"" + (sliceCriterion ? "blue" : "gray") + "\" ";
		text += "label=\"" + "Id = " + id + "\\n" + name + "\" ";
		text += "fontcolor=\"" + (sliceNode ? "blue" : "black") + "\" ";
		text += "fillcolor=\"" + (sliceNode ? "green" : "gray") + "\" ";
		text = text.trim();
		text += "]";

		return text;
	}
	private static String parseEdge(Edge edge)
	{
		final EdgeInfo.Type edgeType = edge.getData().getType();
		final EdgeConstraint constraint = edge.getData().getConstraint();
		final int idFrom = edge.getFrom().getData().getId();
		final int idTo = edge.getTo().getData().getId();
		String text = "";

// TODO Borrame
			
if (edgeType != EdgeInfo.Type.Structural) {
final List<EdgeInfo.Type> ignoreEdgeTypes = Arrays.asList();
final List<EdgeInfo.Type> edgeTypes = Arrays.asList(EdgeInfo.Type.Flow, EdgeInfo.Type.Control,EdgeInfo.Type.ControlFlow,EdgeInfo.Type.Call); // Introducir aqui el tipo de arcos que quieres mostrar
final int[] boundNodeIds = {}; // Introducir aqui los extremos del intervalo en el que se quieren ver los arcos
final List<Integer> nodesIds = Arrays.asList(); // Introducir aqui los nodos de los que se quieren ver los arcos

if (ignoreEdgeTypes.contains(edgeType))
return "";
if (!edgeTypes.isEmpty() && !edgeTypes.contains(edgeType))
return "";
if (boundNodeIds.length == 2)
if (idFrom < boundNodeIds[0] || idTo < boundNodeIds[0] || idFrom > boundNodeIds[1] || idTo > boundNodeIds[1])
return "";
if (!nodesIds.isEmpty() && !nodesIds.contains(idFrom) && !nodesIds.contains(idTo))
return "";
}

		text += idFrom + " -> " + idTo + " ";
		text += "[";
		if (constraint != null && edgeType != EdgeInfo.Type.Structural && edgeType != EdgeInfo.Type.Control)
			text += "label=\"" + constraint + "\", ";

		switch (edgeType)
		{
			case Structural:
				text += "color=black, ";
				text += "penwidth=3";
				break;
			case ControlFlow:
				text += "color=red, ";
				text += "penwidth=3, ";
				text += "constraint=false";
				break;
			case Control:
				text += "color=orange, ";
				text += "constraint=false, ";
				text += "penwidth=3";
				break;
			case Value:
				text += "color=red, ";
				text += "constraint=false, ";
				text += "style=\"dotted\"";
				break;
			case Flow:
				text += "color=blue, ";
				text += "constraint=false, ";
				text += "style=\"dotted\"";
				break;
			case Call:
				text += "color=green3, ";
				text += "penwidth=3, ";
				text += "constraint=false, ";
				text += "style=\"dashed\"";
				break;
			case Input:
				text += "color=green3, ";
				text += "penwidth=3, ";
				text += "constraint=false, ";
				text += "style=\"dashed\"";
				break;
			case Output:
				text += "color=pink, ";
				text += "penwidth=3, ";
				text += "constraint=false, ";
				text += "style=\"dashed\"";
				break;
			case Summary:
				text += "color=brown, ";
				text += "penwidth=4, ";
				text += "constraint=false";
				break;
			case Exception:
				text += "color=orange, ";
				text += "penwidth=3, ";
				text += "constraint=false";
				break;
			default:
				throw new RuntimeException("Edge type not contemplated: " + edgeType);
		}
		text += "]";

		return text;
	}

	private DotFactory()
	{
		
	}
}