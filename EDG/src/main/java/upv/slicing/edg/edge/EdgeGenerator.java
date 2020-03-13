package upv.slicing.edg.edge;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.graph.Variable;

import java.util.LinkedList;
import java.util.List;

public abstract class EdgeGenerator implements Generator {
	protected final EDG edg;

	public EdgeGenerator(EDG edg)
	{
		this.edg = edg;
	}

	protected List<Node> getImplicitRestrictions(List<Node> patterns)
	{
		final List<Node> implicitRestrictions = new LinkedList<>();

		for (Node pattern : patterns)
		{
			final List<Node> patternRestrictions = this.getImplicitRestrictions(pattern);

			implicitRestrictions.addAll(patternRestrictions);
		}

		return implicitRestrictions;
	}

	protected List<Node> getImplicitRestrictions(Node pattern)
	{
		final List<Node> implicitRestrictions = new LinkedList<>();

		pattern = pattern.getType() == Node.Type.Expression ? edg.getChild(pattern, 0) : pattern;

		switch (pattern.getType())
		{
			case Literal:
				implicitRestrictions.add(pattern);
				break;
			case Variable:
				final Variable variable = (Variable) pattern;
				if (variable.getContext() == Variable.Context.Use)
					implicitRestrictions.add(pattern);
				break;
			case List:
			case DataConstructor:
				final List<Node> children = edg.getChildren(pattern);
				implicitRestrictions.add(pattern);
				for (Node child : children)
					implicitRestrictions.addAll(this.getImplicitRestrictions(child));
				break;
			default:
				throw new RuntimeException("Node type not contemplated: " + pattern.getType());
		}

		return implicitRestrictions;
	}

	protected List<Node[]> getMatches(Node pattern, Node expression)
	{
		final List<Node[]> matches = new LinkedList<>();

		final Node patternNode = pattern.getType() == Node.Type.Expression ? edg.getChild(pattern, 0) : pattern;
		final Node expressionNode = expression.getType() == Node.Type.Expression ? edg.getChild(expression, 0) : expression;

		final Node.Type patternType = patternNode.getType();
		final Node.Type expressionType = expressionNode.getType();
		final String patternText = patternNode.getName();
		final String expressionText = expressionNode.getName();

// TODO Revisar
		if (expressionType == Node.Type.Variable ||
			((patternType == Node.Type.Literal) &&
			(expressionType == Node.Type.Literal) &&
			patternText.equals(expressionText)))
			matches.add(new Node[] { patternNode, expressionNode } );
else		if (patternType == Node.Type.Variable ||
			((patternType == Node.Type.Literal) && (expressionType == Node.Type.Operation || expressionType == Node.Type.Call)) ||
			(patternType == Node.Type.DataConstructor && expressionType == Node.Type.Call) ||
			(patternType == Node.Type.List && (expressionType == Node.Type.Operation || expressionType == Node.Type.Call || expressionType == Node.Type.ListComprehension)))
		{
			final Node result = edg.getResult(expressionNode);
			if (result != null)
				matches.add(new Node[] { patternNode, result } );
		}
else		if ((patternType == Node.Type.Literal || patternType == Node.Type.DataConstructor || patternType == Node.Type.List) &&
			(expressionType == Node.Type.Case || expressionType == Node.Type.If || expressionType == Node.Type.Equality || expressionType == Node.Type.Block))
		{
			final Node resultRoot = edg.getResult(expressionNode);
			if (resultRoot != null)
				matches.addAll(this.getMatches(patternNode, resultRoot));
		}
		else if (patternType == Node.Type.Equality)
		{
			final Node result = edg.getResult(patternNode);
			if (result != null)
				matches.addAll(this.getMatches(result, expressionNode));
		}

else		if ((patternType == Node.Type.DataConstructor || patternType == Node.Type.List) && expressionType == patternType)
		{
			final List<Node> patternNodeChildren = edg.getChildren(patternNode);
			final List<Node> expressionNodeChildren = edg.getChildren(expressionNode);

			if (patternNodeChildren.size() == expressionNodeChildren.size())
			{
				final List<Node[]> childrenMatches = new LinkedList<>();

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

	protected List<Node> getVariables(String id, Variable.Context context, Boolean global)
	{
		final List<Node> variableNodes = new LinkedList<>();
		final List<Node> variables = this.edg.getNodes(node -> node instanceof Variable);

		for (Node variable : variables)
		{
			final Variable info = (Variable) variable;
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
	
	protected List<Node> getVariables(String id, Variable.Context context, String className, Boolean global)
	{
		final List<Node> variableNodes = new LinkedList<>();
		final List<Node> variables = this.edg.getNodes(node -> node instanceof Variable);

		for (Node variable : variables)
		{
			final Variable info = (Variable) variable;
			final String variableId = this.getId(variable);
			if (id != null && !variableId.equals(id))
				continue;
			if (context != null && context == Variable.Context.Declaration && !info.isDeclaration())
				continue;
			else if (context != null && context != Variable.Context.Declaration && info.getContext() != context)
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
		if (!(variable instanceof Variable))
			throw new RuntimeException("The node is not a variable");

		final Variable variableInfo = (Variable) variable;
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
