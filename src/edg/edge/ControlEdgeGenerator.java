package edg.edge;

import java.util.List;

import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.traverser.EDGTraverser;

public class ControlEdgeGenerator extends EdgeGenerator
{
	public ControlEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		this.generateBodyEdges();
		this.generateListComprehensionEdges();
		this.generateGuardEdges();
	}

	private void generateBodyEdges()
	{
		final List<Node> bodies = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Body);

		for (Node body : bodies)
		{
			final int bodyIndex = EDGTraverser.getChildIndex(body);
			final List<Node> children = EDGTraverser.getSiblings(body);

			for (int childIndex = 0; childIndex < bodyIndex; childIndex++)
			{
				final Node child = children.get(childIndex);
				final NodeInfo.Type type = child.getData().getType();
				if (type != NodeInfo.Type.Condition && type != NodeInfo.Type.Guard)
					continue;

				final List<Node> expressions = EDGTraverser.getChildren(child);
				if (expressions.size() > 1)
					throw new RuntimeException("More than one expression");

				if (expressions.isEmpty())
					this.edg.addEdge(child, body, 0, new EdgeInfo(EdgeInfo.Type.Control));
				else
				{
					final Node expression = expressions.get(0);
					final Node resultNode = EDGTraverser.getResult(expression);
					this.edg.addEdge(resultNode, body, 0, new EdgeInfo(EdgeInfo.Type.Control));
				}
			}
		}
	}
	private void generateListComprehensionEdges()
	{
		final List<Node> comprehensionNodes = EDGTraverser.getNodes(this.edg, NodeInfo.Type.ListComprehension);

		for (Node comprehensionNode : comprehensionNodes)
		{
			final Node restrictionsNode = EDGTraverser.getChild(comprehensionNode, 0);
			final List<Node> restrictions = EDGTraverser.getChildren(restrictionsNode);
			final int restrictionCount = restrictions.size();
			final Node value = EDGTraverser.getChild(comprehensionNode, 1);

			for (int restrictionIndex = 0; restrictionIndex < restrictionCount - 1; restrictionIndex++)
			{
				final Node restriction = restrictions.get(restrictionIndex);
				final List<Node> restrictionChildren = EDGTraverser.getChildren(restriction);
				final Node restrictionLastChild = restrictionChildren.get(restrictionChildren.size() - 1);
				final Node restrictionResult = EDGTraverser.getResult(restrictionLastChild);
				final Node nextRestriction = restrictions.get(restrictionIndex + 1);
				this.edg.addEdge(restrictionResult, nextRestriction, 0, new EdgeInfo(EdgeInfo.Type.Control));

				final NodeInfo.Type restrictionType = restriction.getData().getType();
				if (restrictionType != NodeInfo.Type.Generator)
					continue;

				final Node generatorPattern = EDGTraverser.getChild(restriction, 0);
				final List<Node> implicitRestrictions = this.getImplicitRestrictions(generatorPattern);
				for (Node implicitRestriction : implicitRestrictions)
				{
					final Node implicitRestrictionResult = EDGTraverser.getResult(implicitRestriction);
					this.edg.addEdge(implicitRestrictionResult, nextRestriction, 0, new EdgeInfo(EdgeInfo.Type.Control));
				}
			}

			final Node lastRestriction = restrictions.get(restrictionCount - 1);
			final List<Node> lastRestrictionChildren = EDGTraverser.getChildren(lastRestriction);
			final Node lastRestrictionLastChild = lastRestrictionChildren.get(lastRestrictionChildren.size() - 1);
			final Node lastRestrictionResult = EDGTraverser.getResult(lastRestrictionLastChild);
			this.edg.addEdge(lastRestrictionResult, value, 0, new EdgeInfo(EdgeInfo.Type.Control));
		}
	}
	private void generateGuardEdges()
	{
		final List<Node> guards = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Guard);

		for (Node guard : guards)
		{
			final Node declarators = EDGTraverser.getSibling(guard, 0);
			final List<Node> declaratorNodes = EDGTraverser.getChildren(declarators);
			final List<Node> implicitRestrictions = this.getImplicitRestrictions(declaratorNodes);

			for (Node implicitRestriction : implicitRestrictions)
			{
				final Node implicitRestrictionResult = EDGTraverser.getResult(implicitRestriction);
				this.edg.addEdge(implicitRestrictionResult, guard, 0, new EdgeInfo(EdgeInfo.Type.Control));
			}
		}
	}
}