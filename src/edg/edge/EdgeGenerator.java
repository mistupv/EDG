package edg.edge;

import java.text.DecimalFormat;
import java.util.LinkedList;
import java.util.List;

import edg.graph.EDG;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.traverser.EDGTraverser;

public abstract class EdgeGenerator
{
//	public static void generateEdges(EDG edg)
//	{
//		new ControlFlowEdgeGenerator(edg).generate();
//		new ControlEdgeGenerator(edg).generate();
//		new InterproceduralEdgeGenerator(edg).generate();
//		new FlowEdgeGenerator(edg).generate();
//		new ValueEdgeGenerator(edg).generateJava();
//		new SummaryEdgeGenerator(edg).generate();
//		// EXCEPTIONS
//		new ExceptionEdgeGenerator(edg).generate();
//	}

	public static void generateEdges(EDG edg)
	{
		long start;
		start = System.nanoTime();
		new ControlFlowEdgeGenerator(edg).generate();
		long cfedges = System.nanoTime();
		//System.out.println("ControlFlow Arcs: "+(cfedges-start)/1000.0+" miliseconds");
		edg.setControlFlowTime((cfedges-start)/1000000.0);
		
		start = System.nanoTime();
		new ControlEdgeGenerator(edg).generate();
		long controledges = System.nanoTime();
		//System.out.println("Control Arcs: "+(controledges-start)/1000.0+" miliseconds");
		edg.setControlTime((controledges-start)/1000000.0);

		start = System.nanoTime();
		new InterproceduralEdgeGenerator(edg).generate();
		long ipedges = System.nanoTime();
		//System.out.println("Interprocedural Arcs: "+(ipedges-start)/1000.0+" miliseconds");
		edg.setInterproceduralTime((ipedges-start)/1000000.0);
		
		start = System.nanoTime();
		new FlowEdgeGenerator(edg).generate();
		long flowedges = System.nanoTime();
		//System.out.println("Flow Arcs: "+(flowedges-start)/1000.0+" miliseconds");
		edg.setFlowTime((flowedges-start)/1000000.0);
		
		start = System.nanoTime();
		new ValueEdgeGenerator(edg).generateJava();
		long valedges = System.nanoTime();
		//System.out.println("Value Arcs: "+(valedges-start)/1000.0+" miliseconds");
		edg.setValueTime((valedges-start)/1000000.0);
		
		start = System.nanoTime();
		new SummaryEdgeGenerator(edg).generate();
		long sumedges = System.nanoTime();
		//System.out.println("Summary Arcs: "+(sumedges-start)/1000.0+" miliseconds");
		edg.setSummaryTime((sumedges-start)/1000000.0);
		
		start = System.nanoTime();
		new ExceptionEdgeGenerator(edg).generate();
		long exedges = System.nanoTime();
		//System.out.println("Exception Arcs: "+(exedges-start)/1000.0+" miliseconds");
		edg.setExceptionTime((exedges-start)/1000000.0);
		
//		DecimalFormat df = new DecimalFormat("#.###");  
//		System.out.println("EDG Generation Time: "+df.format(ggt.getGenerationEDGTime()));
//		System.out.println("SDG Generation Time: "+df.format(ggt.getGenerationSDGTime(edg)));
	}
	

	protected final EDG edg;

	public EdgeGenerator(EDG edg)
	{
		this.edg = edg;
	}

	public abstract void generate();

	protected List<Node> getImplicitRestrictions(List<Node> patterns)
	{
		final List<Node> implicitRestrictions = new LinkedList<Node>();

		for (Node pattern : patterns)
		{
			final List<Node> patternRestrictions = this.getImplicitRestrictions(pattern);

			implicitRestrictions.addAll(patternRestrictions);
		}

		return implicitRestrictions;
	}
	protected List<Node> getImplicitRestrictions(Node pattern)
	{
		final List<Node> implicitRestrictions = new LinkedList<Node>();

		pattern = pattern.getData().getType() == NodeInfo.Type.Expression ? EDGTraverser.getChild(pattern, 0) : pattern;
		final NodeInfo nodeInfo = pattern.getData();

		switch (nodeInfo.getType())
		{
			case Literal:
				implicitRestrictions.add(pattern);
				break;
			case Variable:
				final VariableInfo variableInfo = (VariableInfo) nodeInfo;
				if (variableInfo.getContext() == VariableInfo.Context.Use)
					implicitRestrictions.add(pattern);
				break;
			case List:
			case DataConstructor:
				final List<Node> children = EDGTraverser.getChildren(pattern);
				implicitRestrictions.add(pattern);
				for (Node child : children)
					implicitRestrictions.addAll(this.getImplicitRestrictions(child));
				break;
			default:
				throw new RuntimeException("Node type not contemplated: " + nodeInfo.getType());
		}

		return implicitRestrictions;
	}
	protected List<Node[]> getMatches(Node pattern, Node expression)
	{
		final List<Node[]> matches = new LinkedList<Node[]>();

		final Node patternNode = pattern.getData().getType() == NodeInfo.Type.Expression ? EDGTraverser.getChild(pattern, 0) : pattern;
		final Node expressionNode = expression.getData().getType() == NodeInfo.Type.Expression ? EDGTraverser.getChild(expression, 0) : expression;

		final NodeInfo patternData = patternNode.getData();
		final NodeInfo expressionData = expressionNode.getData();
		final NodeInfo.Type patternType = patternData.getType();
		final NodeInfo.Type expressionType = expressionData.getType();
		final String patternText = patternData.getName();
		final String expressionText = expressionData.getName();

// TODO Revisar
		if (expressionType == NodeInfo.Type.Variable ||
			((patternType == NodeInfo.Type.Literal) &&
			(expressionType == NodeInfo.Type.Literal) &&
			patternText.equals(expressionText)))
			matches.add(new Node[] { patternNode, expressionNode } );
else		if (patternType == NodeInfo.Type.Variable ||
			((patternType == NodeInfo.Type.Literal) && (expressionType == NodeInfo.Type.Operation || expressionType == NodeInfo.Type.Call)) ||
			(patternType == NodeInfo.Type.DataConstructor && expressionType == NodeInfo.Type.Call) ||
			(patternType == NodeInfo.Type.List && (expressionType == NodeInfo.Type.Operation || expressionType == NodeInfo.Type.Call || expressionType == NodeInfo.Type.ListComprehension)))
		{
			final Node result = EDGTraverser.getResult(expressionNode);
			if (result != null)
				matches.add(new Node[] { patternNode, result } );
		}
else		if ((patternType == NodeInfo.Type.Literal || patternType == NodeInfo.Type.DataConstructor || patternType == NodeInfo.Type.List) &&
			(expressionType == NodeInfo.Type.Case || expressionType == NodeInfo.Type.If || expressionType == NodeInfo.Type.Equality || expressionType == NodeInfo.Type.Block))
		{
			final Node resultRoot = EDGTraverser.getResult(expressionNode);
			if (resultRoot != null)
				matches.addAll(this.getMatches(patternNode, resultRoot));
		}
		else if (patternType == NodeInfo.Type.Equality)
		{
			final Node result = EDGTraverser.getResult(patternNode);
			if (result != null)
				matches.addAll(this.getMatches(result, expressionNode));
		}

else		if ((patternType == NodeInfo.Type.DataConstructor || patternType == NodeInfo.Type.List) && expressionType == patternType)
		{
			final List<Node> patternNodeChildren = EDGTraverser.getChildren(patternNode);
			final List<Node> expressionNodeChildren = EDGTraverser.getChildren(expressionNode);

			if (patternNodeChildren.size() == expressionNodeChildren.size())
			{
				final List<Node[]> childrenMatches = new LinkedList<Node[]>();

				for (int childIndex = 0; childIndex < patternNodeChildren.size(); childIndex++)
				{
					final Node patternChild = patternNodeChildren.get(childIndex);
					final Node expressionChild = expressionNodeChildren.get(childIndex);

					childrenMatches.addAll(this.getMatches(patternChild, expressionChild));
				}

				if (!childrenMatches.isEmpty())
				{
					matches.add(new Node[] { patternNode, expressionNode } );
					matches.addAll(childrenMatches);
				}
			}
		}

		return matches;
	}

	protected List<Node> getVariables(String id, VariableInfo.Context context, Boolean global)
	{
		final List<Node> variableNodes = new LinkedList<Node>();
		final List<Node> variables = EDGTraverser.getNodes(this.edg, node -> node.getData() instanceof VariableInfo);

		for (Node variable : variables)
		{
			final VariableInfo info = (VariableInfo) variable.getData();
			final String variableId = this.getId(variable);
			if (id != null && !variableId.equals(id))
				continue;
			if (context != null && info.getContext() != context)
				continue;
			if (global != null && info.isGlobal() != global)
				continue;
			variableNodes.add(variable);
		}

		return variableNodes;
	}
	
	protected List<Node> getVariables(String id, VariableInfo.Context context, String className, Boolean global)
	{
		final List<Node> variableNodes = new LinkedList<Node>();
		final List<Node> variables = EDGTraverser.getNodes(this.edg, node -> node.getData() instanceof VariableInfo);

		for (Node variable : variables)
		{
			final VariableInfo info = (VariableInfo) variable.getData();
			final String variableId = this.getId(variable);
			if (id != null && !variableId.equals(id))
				continue;
			if (context != null && context == VariableInfo.Context.Declaration && !info.isDeclaration())
				continue;
			else if (context != null && context != VariableInfo.Context.Declaration && info.getContext() != context)
				continue;
			if (global != null && info.isGlobal() != global)
				continue;
			if (className != null && !className.equals(info.getInfo().getClassName()))
				continue;
			variableNodes.add(variable);
		}

		return variableNodes;
	}
	
	protected String getId(Node variable)
	{
		if (!(variable.getData() instanceof VariableInfo))
			throw new RuntimeException("The node is not a variable");

		final VariableInfo variableInfo = (VariableInfo) variable.getData();
		return variableInfo.getName();
	}
	protected boolean sameVariables(String id1, String id2)
	{
		final int id1ArrayIndex = id1.indexOf('[');
		final int id2ArrayIndex = id2.indexOf('[');
		final String identifier1 = id1ArrayIndex == -1 ? id1 : id1.substring(0,  id1ArrayIndex);
		final String identifier2 = id2ArrayIndex == -1 ? id2 : id2.substring(0,  id2ArrayIndex);

		return identifier1.equals(identifier2);
	}
}