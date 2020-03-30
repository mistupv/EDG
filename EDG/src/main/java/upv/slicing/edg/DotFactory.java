package upv.slicing.edg;

import org.jgrapht.io.Attribute;
import org.jgrapht.io.DOTExporter;
import org.jgrapht.io.DefaultAttribute;
import org.jgrapht.io.ExportException;
import upv.slicing.edg.constraint.EdgeConstraint;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.Node;

import java.io.File;
import java.util.*;
import java.util.function.Predicate;

public class DotFactory {
	// ============================= DEBUG CONFIGURATION ============================= //
	// This configuration will get applied to filter out some edges from the dot
	// representation, to ease the debugging of graphs.

	// Edge types that will be ignored (none if empty)
	static final List<Edge.Type> ignoreEdgeTypes = Arrays.asList();
	// Edge types that will be included (all if empty)
	static final List<Edge.Type> edgeTypes = Arrays.asList();
	// Lower and upper bound for node inclusion (both ends of an edge must be included for
	// the edge to be included)
	static final int lowerBound = Integer.MIN_VALUE;
	static final int upperBound = Integer.MAX_VALUE;
	// Specific nodes, for which all edges must be included (all if empty)
	static final List<Integer> nodeIds = Arrays.asList();
	// =========================== END DEBUG CONFIGURATION =========================== //

    public static void createDot(File outputFile, EDG edg)
    {
        DotFactory.createDot(outputFile, edg, null, null, null);
    }

    public static void createDot(File outputFile, EDG edg, Map<Edge.Type, Boolean> edgeFlags)
    {
        DotFactory.createDot(outputFile, edg, null, null, edgeFlags);
	}

	public static void createDot(File outputFile, EDG edg, Node slicingCriterion, Set<Node> slice)
	{
		DotFactory.createDot(outputFile, edg, slicingCriterion, slice, null);
	}

	public static void createDot(File outputFile, EDG edg, Node slicingCriterion, Set<Node> slice, Map<Edge.Type, Boolean> edgeFlags)
	{
		SlicedGraph slicedGraph = new SlicedGraph(slicingCriterion, slice);
		slicedGraph.setEdgeFilter(edge -> {
			Edge.Type edgeType = edge.getType();
			int idFrom = edg.getEdgeSource(edge).getId();
			int idTo = edg.getEdgeTarget(edge).getId();

			// Structural edges are always kept!
			if (edgeType == Edge.Type.Structural)
				return true;

			return (edgeFlags == null || edgeFlags.get(edgeType)) &&
					!ignoreEdgeTypes.contains(edgeType) &&
					(edgeTypes.isEmpty() || edgeTypes.contains(edgeType)) &&
					(idFrom >= lowerBound && idTo >= lowerBound && idFrom <= upperBound && idTo <= upperBound) &&
					(nodeIds.isEmpty() || nodeIds.contains(idFrom) || nodeIds.contains(idTo));
		});

		DOTExporter<Node, Edge> exporter = new DOTExporter<>(
				n -> String.valueOf(n.getId()), // Node --> id
				slicedGraph::getNodeLabel,
				slicedGraph::getEdgeLabel,
				slicedGraph::getNodeAttributes,
				slicedGraph::getEdgeAttributes);

		try
		{
			exporter.exportGraph(edg, outputFile);
		}
		catch (ExportException e)
		{
			System.err.println("Error generating dot from EDG and writing it to " + outputFile.getPath());
			e.printStackTrace();
		}
	}

	// ======================== DOT ATTRIBUTES ======================== //
	// Colors
	private static final Attribute BLACK  = DefaultAttribute.createAttribute("black");
	private static final Attribute BLUE   = DefaultAttribute.createAttribute("blue");
	private static final Attribute GREEN  = DefaultAttribute.createAttribute("green");
	private static final Attribute GREEN3 = DefaultAttribute.createAttribute("green3");
	private static final Attribute GRAY   = DefaultAttribute.createAttribute("gray");
	private static final Attribute RED    = DefaultAttribute.createAttribute("red");
	private static final Attribute ORANGE = DefaultAttribute.createAttribute("orange");
	private static final Attribute PINK   = DefaultAttribute.createAttribute("pink");
	private static final Attribute BROWN  = DefaultAttribute.createAttribute("brown");
	// Numbers
	private static final Attribute ONE   = DefaultAttribute.createAttribute(1);
	private static final Attribute THREE = DefaultAttribute.createAttribute(3);
	private static final Attribute FOUR  = DefaultAttribute.createAttribute(4);
	// Booleans
	private static final Attribute FALSE = DefaultAttribute.createAttribute(false);
	// Shapes
	private static final Attribute ELLIPSE = DefaultAttribute.createAttribute("ellipse");
	// Styles
	private static final Attribute FILLED    = DefaultAttribute.createAttribute("filled");
	private static final Attribute DASHED    = DefaultAttribute.createAttribute("dashed");
	private static final Attribute DOTTED    = DefaultAttribute.createAttribute("dotted");
	private static final Attribute INVISIBLE = DefaultAttribute.createAttribute("invis");

	private static class SlicedGraph {
    	private final Node slicingCriterion;
    	private final Set<Node> slice;
    	private Predicate<Edge> edgeFilter = null;

    	public SlicedGraph(Node slicingCriterion, Set<Node> slice)
		{
			this.slicingCriterion = slicingCriterion;
			this.slice = slice;
		}

		public void setEdgeFilter(Predicate<Edge> edgeFilter)
		{
			this.edgeFilter = edgeFilter;
		}

		private String getNodeLabel(Node node)
		{
			return String.format("Id = %d\n%s", node.getId(), node.getLabel());
		}

		private String getEdgeLabel(Edge edge)
		{
			final Edge.Type edgeType = edge.getType();
			final EdgeConstraint constraint = edge.getConstraint();
			if (constraint != null && edgeType != Edge.Type.Structural && edgeType != Edge.Type.Control)
				return constraint.toString();
			return null;
		}

		private Map<String, Attribute> getNodeAttributes(Node node)
		{
			boolean inSlice = slice != null && slice.contains(node);
			Map<String, Attribute> attrs = new HashMap<>();
			attrs.put("shape", ELLIPSE);
			attrs.put("style", FILLED);
			attrs.put("color", node == slicingCriterion ? BLUE : GRAY);
			attrs.put("penwidth", node == slicingCriterion ? FOUR : ONE);
			attrs.put("fontcolor", inSlice ? BLUE : BLACK);
			attrs.put("fillcolor", inSlice ? GREEN : GRAY);
			return attrs;
		}

		private Map<String, Attribute> getEdgeAttributes(Edge edge)
		{
			final Edge.Type edgeType = edge.getType();
			Map<String, Attribute> attrs = new HashMap<>();
			switch (edgeType)
			{
				case Structural:
					attrs.put("color", edge.isMarked() ? GREEN : BLACK);
					attrs.put("penwidth", THREE);
					break;
				case ControlFlow:
					attrs.put("color", RED);
					attrs.put("penwidth", THREE);
					attrs.put("constraint", FALSE);
					break;
				case Control:
					attrs.put("color", ORANGE);
					attrs.put("constraint", FALSE);
					attrs.put("penwidth", THREE);
					break;
				case Value:
					attrs.put("color", RED);
					attrs.put("constraint", FALSE);
					attrs.put("style", DOTTED);
					break;
				case Flow:
					attrs.put("color", BLUE);
					attrs.put("constraint", FALSE);
					attrs.put("style", DOTTED);
					break;
				case Call:
				case Input:
					attrs.put("color", GREEN3);
					attrs.put("penwidth", THREE);
					attrs.put("constraint", FALSE);
					attrs.put("style", DASHED);
					break;
				case Output:
					attrs.put("color", PINK);
					attrs.put("penwidth", THREE);
					attrs.put("constraint", FALSE);
					attrs.put("style", DASHED);
					break;
				case Summary:
					attrs.put("color", BROWN);
					attrs.put("penwidth", FOUR);
					attrs.put("constraint", FALSE);
					break;
				case Exception:
					attrs.put("color", ORANGE);
					attrs.put("penwidth", THREE);
					attrs.put("constraint", FALSE);
					break;
				default:
					throw new RuntimeException("Edge type not contemplated: " + edgeType);
			}

			if (!edgeFilter.test(edge))
			{
				attrs.put("constraint", FALSE);
				attrs.put("style", INVISIBLE);
			}
			return attrs;
		}
	}
}
