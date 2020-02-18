package edg;

import java.io.File;
import java.util.List;

import edg.constraint.Constraint;
import edg.graph.EDG;
import edg.graph.Edge;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import misc.Misc;

public class DotFactory
{
	public static File createDot(String outputPath, EDG edg)
	{
		return DotFactory.createDot(outputPath, edg, null, null);
	}
	public static File createDot(String outputPath, EDG edg, Node slicingCriterium, List<Node> slice)
	{
		final File outputFile = new File(outputPath);

		DotFactory.createDot(outputFile, edg, slicingCriterium, slice);

		return outputFile;
	}
	public static void createDot(File outputFile, EDG edg, Node slicingCriterium, List<Node> slice)
	{
		final Node root = edg.getRootNode();
		final List<Node> nodes = edg.getNodes();
		final List<Edge> edges = edg.getEdges();
		String text = "digraph PDG {\n";
		int lines = 0;

		Misc.write(outputFile, "", false);
		for (Node node : nodes)
		{
			if (node == root)
				continue;
			final boolean sliceCriterium = node == slicingCriterium;
			final boolean sliceNode = slice != null && slice.contains(node);
			final String nodeText = DotFactory.parseNode(node, sliceCriterium, sliceNode);
			text += "\t" + nodeText + ";\n";

			if (++lines % 100 == 0)
			{
				Misc.write(outputFile, text, true);
				text = "";
			}
		}
		for (Edge edge : edges)
		{
			if (edge.getFrom() == root || edge.getTo() == root)
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

	private static String parseNode(Node node, boolean sliceCriterium, boolean sliceNode)
	{
		final String id = node.getData().getId() + "";
		final String name = node.getName().replace("\n", "\\n");
		final boolean fictitious = node.getData().isFictitious();

		String text = "";

		text += id + " ";
		text += "[";
		text += "shape=ellipse ";
		text += "penwidth=" + (sliceCriterium ? "4" : "1") + " ";
		text += "style=filled ";
		text += "color=\"" + (sliceCriterium ? "blue" : "gray") + "\" ";
		text += "label=\"" + "Id = " + id + "\\n" + name + "\" ";
		text += "fontcolor=\"" + (sliceNode ? "blue" : "black") + "\" ";
		text += "fillcolor=\"" + (sliceNode ? "green" : "gray") + "\" ";
		if (fictitious)
			text += "fictitious=true ";
		text = text.trim();
		text += "]";

		return text;
	}
	private static String parseEdge(Edge edge)
	{
		final EdgeInfo.Type edgeType = edge.getData().getType();
		final Constraint constraint = edge.getData().getConstraint();
		String text = "";

		text += edge.getFrom().getData().getId() + " -> " + edge.getTo().getData().getId() + " ";
		text += "[";
		if (constraint != null)
			text += "label=\"" + constraint + "\", ";

		switch (edgeType)
		{
			case NormalControl:
				text += "color=black, ";
				text += "penwidth=3";
				break;
			case StructuralControl:
				text += "color=black, ";
				text += "penwidth=3, ";
				text += "style=\"dotted\"";
				break;
			case GuardRestriction:
			case FlowDependence:
				text += "color=red, ";
				text += "constraint=false, ";
				text += "style=\"dotted\"";
				break;
			case ValueDependence:
				text += "color=black, ";
				text += "constraint=false, ";
				text += "style=\"dotted\"";
				break;
			case Input:
				text += "color=green3, ";
				text += "penwidth=3, ";
				text += "constraint=false, ";
				text += "style=\"dashed\"";
				break;
			case Output:
				text += "color=blue3, ";
				text += "penwidth=3, ";
				text += "constraint=false, ";
				text += "style=\"dashed\"";
				break;
			case Summary:
				text += "color=brown, ";
				text += "penwidth=7, ";
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