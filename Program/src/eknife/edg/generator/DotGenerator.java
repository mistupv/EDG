package eknife.edg.generator;

import java.io.File;
import java.util.LinkedList;
import java.util.List;

import eknife.edg.EDG;
import eknife.edg.Edge;
import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.constraint.Constraint;
import eknife.edg.constraint.ExceptionArgumentConstraint;
import eknife.edg.constraint.GetAllConstraint;
import eknife.edg.constraint.SeekingConstraint;
import eknife.edg.constraint.UnresolvableConstraint;
import eknife.misc.Misc;

public class DotGenerator
{
	public void generate(EDG graph, Node slicingCriterium, List<Node> slice, File tempFile, File dotFile, File pdfFile)
	{
// TODO Delete
int ignore = -1;
int minNode = ignore;
int maxNode = ignore;
int[] showEdgesIds = { ignore };

		final Node root = graph.getRootNode();
		final List<Node> nodes = graph.getNodes();
		final List<Edge> edges = graph.getEdges();
		String text = "digraph PDG {\n";
		int lines = 0;

// TODO Delete
List<Integer> showEdges = new LinkedList<Integer>();
for (int showEdgesId : showEdgesIds)
 showEdges.add(showEdgesId);

		Misc.write(tempFile, "", false);
		for (Node node : nodes)
		{
// TODO Delete
if (minNode != ignore && maxNode != ignore)
 if (node.getData().getId() < minNode || node.getData().getId() > maxNode)
  continue;
			if (node == root)
				continue;
			final boolean sliceCriterium = node == slicingCriterium;
			final boolean sliceNode = slice.contains(node);
			final String nodeText = this.parseNode(node, sliceCriterium, sliceNode);
			text += "\t" + nodeText + ";\n";

			if (++lines % 100 == 0)
			{
				Misc.write(tempFile, text, true);
				text = "";
			}
		}
		for (Edge edge : edges)
		{
// TODO Delete
if (minNode != ignore && maxNode != ignore) {
 if (edge.getFrom().getData().getId() < minNode || edge.getFrom().getData().getId() > maxNode)
  continue;
 if (edge.getTo().getData().getId() < minNode || edge.getTo().getData().getId() > maxNode)
  continue;
}
if (!showEdges.contains(ignore))
 if (edge.getData().getType() != EdgeInfo.Type.NormalControl &&
 edge.getData().getType() != EdgeInfo.Type.StructuralControl &&
 !showEdges.contains(edge.getFrom().getData().getId()) && !showEdges.contains(edge.getTo().getData().getId()))
  continue;
			if (edge.getFrom() == root || edge.getTo() == root)
				continue;
			final String edgeText = this.parseEdge(edge);
			text += edgeText.isEmpty() ? "" : "\t" + edgeText + ";\n";

			if (++lines % 100 == 0)
			{
				Misc.write(tempFile, text, true);
				text = "";
			}
		}
		text += "}";

		Misc.write(tempFile, text, true);
		Misc.moveFile(tempFile, dotFile);
		try {
//			String command = "dot -Tpdf "+dotFile.getAbsolutePath()+" > "+ pdfFile.getAbsolutePath();
//			String[] cmd = {"/bin/sh","-c",command};
			String[] cmd = { "../Launcher/pdf.sh", dotFile.getAbsolutePath(), pdfFile.getAbsolutePath() };
			Runtime.getRuntime().exec(cmd);
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		
	}

	private String parseNode(Node node, boolean sliceCriterium, boolean sliceNode)
	{
		final String id = node.getData().getId() + "";
		final String name = node.getName().replace("\n", "\\n");
		final NodeInfo.Type nodeType = node.getData().getType();
		final boolean fictitious = nodeType == NodeInfo.Type.Body || nodeType == NodeInfo.Type.Return;

		String text = "";
if (id.equals("0"))
System.out.println("STOP");
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
	private String parseEdge(Edge edge)
	{
		final EdgeInfo.Type edgeType = edge.getData().getType();
		final Constraint constraint = edge.getData().getConstraint();
		String text = "";

		text += edge.getFrom().getData().getId() + " -> " + edge.getTo().getData().getId() + " ";
		text += "[";
		if (constraint != null && (constraint instanceof SeekingConstraint || constraint instanceof ExceptionArgumentConstraint))
			if (!(constraint instanceof GetAllConstraint))
				text += "label=\"" + constraint + "\", ";
		if (constraint != null && !(edgeType == EdgeInfo.Type.NormalControl || edgeType == EdgeInfo.Type.ValueDependence ||
				edgeType == EdgeInfo.Type.GuardRestriction || edgeType == EdgeInfo.Type.FlowDependence || edgeType == EdgeInfo.Type.ExceptionGetAll))
			text += "label=\"" + constraint + "\", ";
		if (constraint != null && 
				(edgeType == EdgeInfo.Type.GuardRestriction || edgeType == EdgeInfo.Type.FlowDependence) && 
				(constraint instanceof UnresolvableConstraint))
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
				if (constraint instanceof SeekingConstraint)
				{
					text += "color=orange, ";
					text += "penwidth=3, ";
					text += "constraint=false, ";
					text += "style=\"dashed\"";
				}
				else
				{
					text += "color=red, ";
					text += "constraint=false, ";
					text += "style=\"dotted\"";
				}
				break;
			case ValueDependence:
				if (constraint instanceof SeekingConstraint || constraint instanceof ExceptionArgumentConstraint)
				{
					text += "color=orange, ";
					text += "penwidth=3, ";
					text += "constraint=false, ";
					text += "style=\"dashed\"";
				}
				else
				{
					text += "color=black, ";
					text += "constraint=false, ";
					text += "style=\"dotted\"";
				}
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
				text += "penwidth=3";
				//text += "penwidth=2, ";
				//text += "constraint=false";
				break;
			case ExceptionGetAll:
				text += "color=deepskyblue1, ";
				text += "penwidth=2, ";
				text += "constraint=false ";
				break;
			default:
				throw new RuntimeException("Edge type not contempled: " + edgeType);
		}
		text += "]";

		return text;
	}
}