package upv.slicing.edg.edge;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.EdgeInfo;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.graph.NodeInfo;
import upv.slicing.edg.traverser.EDGTraverser;

import java.util.List;

public class ControlEdgeGenerator extends EdgeGenerator {
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
		// ADDED UPDATE nodes in FOR loops, they also depend on the for condition, as it happens with body
		bodies.addAll(EDGTraverser.getNodes(this.edg, NodeInfo.Type.Update));

		for (Node body : bodies)
		{
			//final int bodyIndex = EDGTraverserNew.getChildIndex(body);
			final List<Node> children = EDGTraverser.getSiblings(body);

			// Node condition before node body (Doesn't work with do_while loop if we put the expressions in the appearance order 1) Body 2) Condition )
			// for (int childIndex = 0; childIndex < bodyIndex; childIndex++)

			//for (int childIndex = 0; childIndex < bodies.size(); childIndex++) // Node condition after node body (Less efficient)
			for (int childIndex = 0; childIndex < children.size();
				 childIndex++) // Node condition after node body (Less efficient)
			{
				final Node child = children.get(childIndex);
				final NodeInfo.Type type = child.getData().getType();
				if (type != NodeInfo.Type.Condition && type != NodeInfo.Type.Guard)
					continue;

				final List<Node> expressions = EDGTraverser.getChildren(child);
				if (expressions.size() > 2)
					throw new RuntimeException("More than one expression");

				// PLANTEARSE USAR EL BREAK O HACER UNA BUSQUEDA EN LUGAR DE UN RECORRIDO
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