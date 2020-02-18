package eknife.erlang;

import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

import edg.constraint.BinComprehensionConstraint;
import edg.constraint.Constraint;
import edg.constraint.Constraints;
import edg.constraint.ListComprehensionConstraint;
import edg.constraint.StarConstraint;
import edg.constraint.SummaryConstraint;
import edg.constraint.UnresolvableConstraint;
import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.slicingAlgorithm.AdvancedAlgorithm;
import edg.traverser.GraphTraverser;
import edg.util.Work;
import edg.util.WorkList;
import misc.Misc;
import misc.util.Chronometer;

public class DependenceGenerator
{
	private EDG edg;
	private boolean constraintsActivated;

	private Comparator<NodeInfo> getTypeComparator()
	{
		return new Comparator<NodeInfo>()
		{
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				return o1.getType() == o2.getType() ? 0 : 1;
			}
		};
	}
	private List<Node> getDescendants(Node root)
	{
		return this.getDescendants(root, null);
	}
	private List<Node> getDescendants(Node root, NodeInfo.Type type)
	{
		final List<Node> allNodes;

		if (type == null)
			allNodes = this.edg.getNodes();
		else
		{
			final NodeInfo data = new NodeInfo(0, type, 0, null);
			final Comparator<NodeInfo> compare = this.getTypeComparator();
			allNodes = this.edg.findNodesByData(data, compare);
		}

		if (root == this.edg.getRootNode())
			return allNodes;
		return GraphTraverser.getDescendantNodes(root, allNodes);
	}
	private List<Node> getScope(Node node)
	{
		return this.getScope(node, NodeInfo.Type.Variable);
	}
	private List<Node> getScope(Node node, NodeInfo.Type type)
	{
		final List<Node> scope = new LinkedList<Node>();
		final Node root = this.edg.getRootNode();
		Node ancestor = node;

		while (ancestor != root)
		{
			final Node ancestorParent = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
			if (ancestorParent.getData().getType() == NodeInfo.Type.Function)
				break;

			final List<Node> ancestorSiblings = GraphTraverser.getChildren(ancestorParent, EdgeInfo.Type.Control);
			final int childIndex = GraphTraverser.getChildIndex(ancestor, EdgeInfo.Type.Control);

			for (int siblingIndex = 0; siblingIndex < childIndex; siblingIndex++)
			{
				final Node ancestorPreviousSibling = ancestorSiblings.get(siblingIndex);
				final NodeInfo.Type ancestorPreviousSiblingType = ancestorPreviousSibling.getData().getType();
				if (ancestorPreviousSiblingType == NodeInfo.Type.Clause)
					continue;

				final List<Node> siblingDescendants = this.getDescendants(ancestorPreviousSibling, type);
				scope.addAll(siblingDescendants);
			}

			ancestor = ancestorParent;
		}

		return scope;
	}
	private List<Node> getDeclaredVariables(Node root)
	{
		final List<Node> declaredVariables = new LinkedList<Node>();
		final List<Node> descendantsVariables = this.getDescendants(root, NodeInfo.Type.Variable);

		for (Node descendantVariable : descendantsVariables)
		{
			boolean isDeclaration = false;
			Node ancestor = descendantVariable;

			while (ancestor != root && !isDeclaration)
			{
				final NodeInfo.Type ancestorType = ancestor.getData().getType();
				final int ancestorChildIndex = GraphTraverser.getChildIndex(ancestor, EdgeInfo.Type.Control);
				final Node ancestorParent = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
				final NodeInfo.Type ancestorParentType = ancestorParent.getData().getType();

				if (ancestorParentType == NodeInfo.Type.PatternMatching && ancestorChildIndex == 0)
					isDeclaration = true;
				else if (ancestorParentType == NodeInfo.Type.Generator && ancestorChildIndex == 0)
					isDeclaration = true;
				else if (ancestorParentType == NodeInfo.Type.BinGenerator && ancestorChildIndex == 0)
					isDeclaration = true;
				else if (ancestorParentType == NodeInfo.Type.Clause && ancestorType != NodeInfo.Type.Guard)
					isDeclaration = true;

				ancestor = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
			}

			if (isDeclaration)
				declaredVariables.add(descendantVariable);
		}

		return declaredVariables;
	}
	private List<Node> getUsedVariables(Node root)
	{
		final List<Node> variables = this.getDescendants(root, NodeInfo.Type.Variable);
		final List<Node> declaredVariables = this.getDeclaredVariables(root);
		final List<Node> usedVariables = Misc.disjunt(variables, declaredVariables);

		// Add those declared variables that are using variables at the same time
		for (Node declaredVariable : declaredVariables)
		{
			final Node declaredVariableParent = GraphTraverser.getParent(declaredVariable, EdgeInfo.Type.Control);
			final List<Node> scopeVariables = this.getScope(declaredVariableParent);
			final String declaredVariableName = declaredVariable.getName();

			for (Node scopeVariable : scopeVariables)
				if (scopeVariable.getName().equals(declaredVariableName))
				{
					usedVariables.add(declaredVariable);
					break;
				}
		}

		return usedVariables;
	}

	/************************************************************************/
	/********************************* Edges ********************************/
	/************************************************************************/
	public void generateEdges(EDG edg)
	{
		this.generateEdges(edg, true);
	}
	public void generateEdges(EDG edg, boolean constraintsActivated)
	{
		this.edg = edg;
		this.constraintsActivated = constraintsActivated;

		this.generateDataEdges();
		this.generateInputOutputEdges();
		this.generateSummaryEdges();
	}

	/************************************************************************/
	/****************************** Data edges ******************************/
	/************************************************************************/
	private void generateDataEdges()
	{
		final Node root = this.edg.getRootNode();
		final List<Node> functions = GraphTraverser.getChildren(root, EdgeInfo.Type.NormalControl);

		for (Node function : functions)
		{
			final List<Node> functionClauses = GraphTraverser.getChildren(function, EdgeInfo.Type.NormalControl);
			for (Node functionClause : functionClauses)
				this.generateDataEdges(functionClause);
		}
	}
	private void generateDataEdges(Node functionClause)
	{
		this.generateFlowRelationEdges(functionClause);
		this.generateMatchingEdges(functionClause);
		this.generateComprehensionEdges(functionClause);
		this.generateRestrictionsEdges(functionClause);
		this.generateFunctionCallsEdges(functionClause);
		if (this.constraintsActivated)
			this.generateValueEdges(functionClause);
		this.generateBinEdges(functionClause);

		// Special edges
		this.generateClauseGuardsEdges(functionClause);
	}

	/************************************/
	/***** Flow relation dependences ****/
	/************************************/
	private void generateFlowRelationEdges(Node functionClause)
	{
		final List<Node> usedVariables = this.getUsedVariables(functionClause);
		final List<Node> declaredVariables = this.getDeclaredVariables(functionClause);

		for (Node usedVariable : usedVariables)
		{
			final String usedVariableName = usedVariable.getData().getText();
			final List<Node> scope = this.getScope(usedVariable);
			final List<Node> declaredVariablesInScope = Misc.intersect(declaredVariables, scope);

			for (Node declaredVariableInScope : declaredVariablesInScope)
			{
				final String declaredVariableName = declaredVariableInScope.getData().getText();
				if (declaredVariableName.equals(usedVariableName))
					this.edg.addEdge(declaredVariableInScope, usedVariable, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence));
			}
		}
	}

	/************************************/
	/******* Matching dependences *******/
	/************************************/
	private void generateMatchingEdges(Node functionClause)
	{
		this.generateCaseMatchingEdges(functionClause);
		this.generateAssignmentMatchingEdges(functionClause);
	}
	private void generateCaseMatchingEdges(Node functionClause)
	{
		final List<Node> cases = this.getDescendants(functionClause, NodeInfo.Type.Case);

		for (Node case0 : cases)
		{
			final List<Node> caseClauses = GraphTraverser.getChildren(case0, EdgeInfo.Type.NormalControl);
			final Node expression = caseClauses.remove(0);

			for (Node caseClause : caseClauses)
			{
				final List<Node> caseChildren = GraphTraverser.getChildren(caseClause, EdgeInfo.Type.NormalControl);
				final Node pattern = caseChildren.get(0);
				final List<Node[]> matches = this.getMatches(pattern, expression);

				for (Node[] match : matches)
					this.edg.addEdge(match[0], match[1], 0, new EdgeInfo(EdgeInfo.Type.FlowDependence));
			}
		}
	}
	private void generateAssignmentMatchingEdges(Node functionClause)
	{
		final List<Node> patternMatchingNodes = this.getDescendants(functionClause, NodeInfo.Type.PatternMatching);
		final List<Node> generatorNodes = this.getDescendants(functionClause, NodeInfo.Type.Generator);
		final List<Node> assignments = Misc.union(patternMatchingNodes, generatorNodes);

		for (Node assignment : assignments)
		{
			final List<Node> children = GraphTraverser.getChildren(assignment, EdgeInfo.Type.NormalControl);
			final Node pattern = children.get(0);
			final Node expression = children.get(1);
			final List<Node[]> matches = this.getMatches(pattern, expression);

			for (Node[] match : matches)
				this.edg.addEdge(match[0], match[1], 0, new EdgeInfo(EdgeInfo.Type.FlowDependence));
		}
	}
	private List<Node[]> getMatches(Node pattern, Node expression)
	{
		final List<Node[]> matches = new LinkedList<Node[]>();
		final NodeInfo patternData = pattern.getData();
		final NodeInfo expressionData = expression.getData();
		final NodeInfo.Type patternType = patternData.getType();
		final NodeInfo.Type expressionType = expressionData.getType();
		final String patternText = patternData.getText();
		final String expressionText = expressionData.getText();

		// TODO Group types
		if (expressionType == NodeInfo.Type.Variable ||
			((patternType == NodeInfo.Type.Atom || patternType == NodeInfo.Type.String || patternType == NodeInfo.Type.Integer || patternType == NodeInfo.Type.Char || patternType == NodeInfo.Type.FunctionIdentifier) &&
			(expressionType == NodeInfo.Type.Atom || expressionType == NodeInfo.Type.String || expressionType == NodeInfo.Type.Integer || patternType == NodeInfo.Type.Char || expressionType == NodeInfo.Type.FunctionIdentifier) &&
			patternText.equals(expressionText)))
			matches.add(new Node[] { expression, pattern } );
		if (patternType == NodeInfo.Type.Variable ||
			((patternType == NodeInfo.Type.Atom || patternType == NodeInfo.Type.String || patternType == NodeInfo.Type.Integer || patternType == NodeInfo.Type.Char) && (expressionType == NodeInfo.Type.Operation || expressionType == NodeInfo.Type.FunctionCall)) ||
			(patternType == NodeInfo.Type.TuplePattern && expressionType == NodeInfo.Type.FunctionCall) ||
			(patternType == NodeInfo.Type.ListPattern && (expressionType == NodeInfo.Type.Operation || expressionType == NodeInfo.Type.FunctionCall || expressionType == NodeInfo.Type.ListComprehension)))
		{
			final List<Node> lasts = GraphTraverser.getLasts(expression, this.constraintsActivated);
			for (Node last : lasts)
				matches.add(new Node[] { last, pattern } );
		}
		if ((patternType == NodeInfo.Type.Atom || patternType == NodeInfo.Type.String || patternType == NodeInfo.Type.Integer || patternType == NodeInfo.Type.Char || patternType == NodeInfo.Type.TuplePattern || patternType == NodeInfo.Type.ListPattern) &&
			(expressionType == NodeInfo.Type.Case || expressionType == NodeInfo.Type.If || expressionType == NodeInfo.Type.PatternMatching || expressionType == NodeInfo.Type.Block))
		{
			final List<Node> lastRoots = GraphTraverser.getLasts(expression, this.constraintsActivated);
			for (Node lastRoot : lastRoots)
				matches.addAll(this.getMatches(pattern, lastRoot));
		}
		else if (patternType == NodeInfo.Type.PatternMatching)
		{
			final List<Node> lasts = GraphTraverser.getLasts(pattern, this.constraintsActivated);
			for (Node last : lasts)
				matches.addAll(this.getMatches(last, expression));
		}
		if ((patternType == NodeInfo.Type.TuplePattern && expressionType == NodeInfo.Type.TupleExpression) || (patternType == NodeInfo.Type.ListPattern && expressionType == NodeInfo.Type.ListExpression))
		{
			final List<Node> patternChildren = GraphTraverser.getChildren(pattern, EdgeInfo.Type.StructuralControl);
			final List<Node> expressionChildren = GraphTraverser.getChildren(expression, EdgeInfo.Type.StructuralControl);

			if (patternChildren.size() == expressionChildren.size())
			{
				final List<Node[]> childrenMatches = new LinkedList<Node[]>();

				for (int childIndex = 0; childIndex < patternChildren.size(); childIndex++)
				{
					final Node patternChild = patternChildren.get(childIndex);
					final Node expressionChild = expressionChildren.get(childIndex);

					childrenMatches.addAll(this.getMatches(patternChild, expressionChild));
				}

				if (!childrenMatches.isEmpty())
				{
					matches.add(new Node[] { expression, pattern } );
					matches.addAll(childrenMatches);
				}
			}
		}

		return matches;
	}

	/************************************/
	/**** Comprehension dependences *****/
	/************************************/
	private void generateComprehensionEdges(Node functionClause)
	{
		final List<Node> listComprehensionNodes = this.getDescendants(functionClause, NodeInfo.Type.ListComprehension);
		final List<Node> binComprehensionNodes = this.getDescendants(functionClause, NodeInfo.Type.BinComprehension);
		final List<Node> comprehensionNodes = Misc.union(listComprehensionNodes, binComprehensionNodes);

		for (Node comprehensionNode : comprehensionNodes)
		{
			this.generateComprehensionEdges1(comprehensionNode);
			this.generateComprehensionEdges2(comprehensionNode);
		}
	}
	private void generateComprehensionEdges1(Node comprehension)
	{
		final List<Node> children = GraphTraverser.getChildren(comprehension, EdgeInfo.Type.NormalControl);
		final Node result = children.get(children.size() - 2);
		final Node expression = children.get(children.size() - 1);
		final List<Node> lasts = GraphTraverser.getLasts(expression, this.constraintsActivated);
		final Constraint comprehensionConstraint = comprehension.getData().getType() == NodeInfo.Type.ListComprehension ? new ListComprehensionConstraint() : new BinComprehensionConstraint();

		for (Node last : lasts)
			this.edg.addEdge(last, result, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, comprehensionConstraint));
	}
	private void generateComprehensionEdges2(Node comprehension)
	{
		final List<Node> generators = GraphTraverser.getChildren(comprehension, EdgeInfo.Type.NormalControl);
		generators.remove(generators.size() - 2);
		final Node expression = generators.remove(generators.size() - 1);
		final StarConstraint starConstraint = this.getStarConstraint();

		for (Node generator : generators)
		{
			final List<Node> lasts = GraphTraverser.getLasts(generator, this.constraintsActivated);

			for (Node last : lasts)
				this.edg.addEdge(last, expression, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, starConstraint));
		}
	}

	/************************************/
	/** Bin comprehension dependences ***/
	/************************************/
	private void generateBinEdges(Node functionClause)
	{
		final List<Node> binPatternNodes = this.getDescendants(functionClause, NodeInfo.Type.BinPattern);
		final List<Node> binExpressionNodes = this.getDescendants(functionClause, NodeInfo.Type.BinExpression);
		final List<Node> binNodes = Misc.union(binPatternNodes, binExpressionNodes);
		final StarConstraint starConstraint = this.getStarConstraint();

		for (Node binNode : binNodes)
		{
			final List<Node> binElementNodes = GraphTraverser.getChildren(binNode, EdgeInfo.Type.StructuralControl);

			for (Node binElementNode : binElementNodes)
			{
				final List<Node> binElementChildNodes = GraphTraverser.getChildren(binElementNode, EdgeInfo.Type.StructuralControl);
				final Node binElementSizeNode = binElementChildNodes.get(1);

				this.edg.addEdge(binElementSizeNode, binNode, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, starConstraint));
			}
		}
	}

	/************************************/
	/***** Restrictions dependences *****/
	/************************************/
	private void generateRestrictionsEdges(Node functionClause)
	{
		final List<Node> declaredVariables = this.getDeclaredVariables(functionClause);
		final List<Node> clauses = this.getDescendants(functionClause, NodeInfo.Type.Clause);

		for (Node clause : clauses)
		{
			// TODO Mirar P5S1 => Nodos 41, 55, 56 => ¿Deberia haber una restriccion?
			final List<Node> parametersConstraints = this.getConstraints(clause);
			final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
			final Node guard = parameters.remove(parameters.size() - 1);

			for (Node parameterConstraint : parametersConstraints)
			{
				final NodeInfo.Type nodeType = parameterConstraint.getData().getType();

				if (nodeType != NodeInfo.Type.TuplePattern && nodeType != NodeInfo.Type.ListPattern)
					this.edg.addEdge(parameterConstraint, guard, 0, new EdgeInfo(EdgeInfo.Type.GuardRestriction));
				else
				{
					final UnresolvableConstraint constraint = new UnresolvableConstraint();
					this.edg.addEdge(parameterConstraint, guard, 0, new EdgeInfo(EdgeInfo.Type.GuardRestriction, constraint));
				}
			}

			final List<Node> scope = this.getScope(guard);
			final List<Node> declaredVariablesInScope = Misc.intersect(declaredVariables, scope);
			final List<Node> variablesInGuards = this.getVariablesInGuards(guard, declaredVariablesInScope);
			for (Node variableInGuards : variablesInGuards)
				this.edg.addEdge(variableInGuards, guard, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence));

			// TODO Delete
//			final List<Node> restrictions = Misc.union(constraints, variablesInGuards);
//			for (Node restriction : restrictions)
//				this.graph.addEdge(restriction, guard, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence));
		}
	}

	/*******************/
	/*** Constraints ***/
	/*******************/
	private List<Node> getConstraints(Node clause)
	{
		final List<Node> constraints = new LinkedList<Node>();
		final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
		final Node guard = parameters.remove(parameters.size() - 1);
		final List<Node> variables = this.getScope(guard);
		final List<String> repeatedVariables = this.getRepeatedVariables(variables);

		for (Node parameter : parameters)
		{
			final List<Node> parameterConstraints = this.getConstraints(parameter, repeatedVariables);

			constraints.addAll(parameterConstraints);
		}

		return constraints;
	}
	private List<String> getRepeatedVariables(List<Node> variables)
	{
		final List<String> repeatedVariables = new LinkedList<String>();
		final List<String> variableNames = new LinkedList<String>();

		for (Node variable : variables)
		{
			final String variableName = variable.getData().getText();

			if (variableName.equals("_"))
				continue;
			if (!variableNames.contains(variableName))
				variableNames.add(variableName);
			else if (!repeatedVariables.contains(variableName))
				repeatedVariables.add(variableName);
		}

		return repeatedVariables;
	}
	private List<Node> getConstraints(Node parameter, List<String> repeatedVariables)
	{
		final List<Node> constraints = new LinkedList<Node>();
		final NodeInfo info = parameter.getData();

		// TODO Group types
		switch (info.getType())
		{
			case Atom:
			case String:
			case Integer:
			case Char:
			case Operation:
				constraints.add(parameter);
				break;
			case Variable:
				if (repeatedVariables.contains(info.getText()))
					constraints.add(parameter);
				break;
			case ListPattern:
			case TuplePattern:
				constraints.add(parameter);
			case PatternMatching:
				final List<Node> children = GraphTraverser.getChildren(parameter, EdgeInfo.Type.Control);
				for (Node child : children)
					constraints.addAll(this.getConstraints(child, repeatedVariables));
				break;
			case CompoundPattern:
				break;
			default:
				throw new RuntimeException("Parameter type not contemplated: " + info.getType());
		}

		return constraints;
	}

	/*******************/
	/** Used in guard **/
	/*******************/
	private List<Node> getVariablesInGuards(Node guard, List<Node> variables)
	{
		final List<Node> variablesInGuards = new LinkedList<Node>();
		final OtpErlangList guards = (OtpErlangList) guard.getData().getInfo()[0];
		final List<String> guardsVariables = this.getGuardsVariables(guards);

		for (Node variable : variables)
		{
			final String variableName = variable.getData().getText();

			if (guardsVariables.contains(variableName))
				variablesInGuards.add(variable);
		}

		return variablesInGuards;
	}
	private List<String> getGuardsVariables(OtpErlangList guards)
	{
		final List<String> variables = new LinkedList<String>();
		final int guardsArity = guards.arity();

		for (int guardIndex = 0; guardIndex < guardsArity; guardIndex++)
		{
			final OtpErlangList guard = (OtpErlangList) guards.elementAt(guardIndex);
			final List<String> expressionsVariables = this.getExpressionsVariables(guard);

			variables.addAll(expressionsVariables);
		}

		return variables;
	}
	private List<String> getExpressionsVariables(OtpErlangList expressions)
	{
		final List<String> variables = new LinkedList<String>();
		final int expressionsArity = expressions.arity();

		for (int expressionIndex = 0; expressionIndex < expressionsArity; expressionIndex++)
		{
			final OtpErlangTuple expressionTuple = (OtpErlangTuple) expressions.elementAt(expressionIndex);
			final List<String> expressionVariable = this.getExpressionVariables(expressionTuple);

			variables.addAll(expressionVariable);
		}

		return variables;
	}
	private List<String> getExpressionVariables(OtpErlangTuple expression)
	{
		final OtpErlangAtom expressionType = (OtpErlangAtom) expression.elementAt(0);

		switch (expressionType.atomValue())
		{
			case "op":
				return this.getOperationVariables(expression);
			case "tuple":
				return this.getTupleVariables(expression);
			case "cons":
			case "nil":
				return this.getListVariables(expression);
			case "call":
				return this.getCallVariables(expression);
			case "var":
				return this.getVariable(expression);
			case "integer":
			case "char":
			case "atom":
				return new LinkedList<String>();
			default:
				throw new RuntimeException("Expression type not contemplated: " + expressionType);
		}
	}
	private List<String> getTupleVariables(OtpErlangTuple tuple)
	{
		final OtpErlangList tupleExpressions = (OtpErlangList) tuple.elementAt(2);

		return this.getVariables(tupleExpressions);
	}
	private List<String> getListVariables(OtpErlangTuple list)
	{
		return this.getListVariables0(list);
	}
	private List<String> getListVariables0(OtpErlangTuple list)
	{
		final List<String> variables = new LinkedList<String>();
		final OtpErlangAtom type = (OtpErlangAtom) list.elementAt(0);
		if (type.atomValue().equals("nil"))
			return variables;

		final OtpErlangTuple head = (OtpErlangTuple) list.elementAt(2);
		final OtpErlangTuple tail = (OtpErlangTuple) list.elementAt(3);
		final List<String> headVariables = this.getExpressionVariables(head);
		final List<String> tailVariables = this.getListVariables0(tail);

		variables.addAll(headVariables);
		variables.addAll(tailVariables);

		return variables;
	}
	private List<String> getOperationVariables(OtpErlangTuple operation)
	{
		final OtpErlangAtom operationType = (OtpErlangAtom) operation.elementAt(2);

		switch (operation.arity())
		{
			case 4:
				return this.getUnaryOperationVariables(operation);
			case 5:
				return this.getBinaryOperationVariables(operation);
			default:
				throw new RuntimeException("Operation type not contemplated: " + operationType.atomValue());
		}
	}
	private List<String> getUnaryOperationVariables(OtpErlangTuple operation)
	{
		final List<String> variables = new LinkedList<String>();
		final OtpErlangTuple operationExpression1 = (OtpErlangTuple) operation.elementAt(3);
		final List<String> expression1Variables = this.getExpressionVariables(operationExpression1);

		variables.addAll(expression1Variables);

		return variables;
	}
	private List<String> getBinaryOperationVariables(OtpErlangTuple operation)
	{
		final List<String> variables = new LinkedList<String>();
		final OtpErlangTuple operationExpression1 = (OtpErlangTuple) operation.elementAt(3);
		final List<String> expression1Variables = this.getExpressionVariables(operationExpression1);
		final OtpErlangTuple operationExpression2 = (OtpErlangTuple) operation.elementAt(4);
		final List<String> expression2Variables = this.getExpressionVariables(operationExpression2);

		variables.addAll(expression1Variables);
		variables.addAll(expression2Variables);

		return variables;
	}
	private List<String> getCallVariables(OtpErlangTuple call)
	{
		final List<String> variables = new LinkedList<String>();
		final OtpErlangTuple function = (OtpErlangTuple) call.elementAt(2);
		final List<String> functionVariables = this.getExpressionVariables(function);
		final OtpErlangList parameters = (OtpErlangList) call.elementAt(3);
		final List<String> parametersVariables = this.getVariables(parameters);

		variables.addAll(functionVariables);
		variables.addAll(parametersVariables);

		return variables;
	}
	private List<String> getVariable(OtpErlangTuple variable)
	{
		final List<String> variables = new LinkedList<String>();
		final OtpErlangAtom value = (OtpErlangAtom) variable.elementAt(2);

		variables.add(value.atomValue());

		return variables;
	}
	private List<String> getVariables(OtpErlangList expressions)
	{
		final List<String> variables = new LinkedList<String>();
		final int expressionsArity = expressions.arity();

		for (int expressionIndex = 0; expressionIndex < expressionsArity; expressionIndex++)
		{
			final OtpErlangTuple expressionTuple = (OtpErlangTuple) expressions.elementAt(expressionIndex);
			final List<String> expressionVariables = this.getExpressionVariables(expressionTuple);

			variables.addAll(expressionVariables);
		}

		return variables;
	}

	/************************************/
	/**** Function calls dependences ****/
	/************************************/
	private void generateFunctionCallsEdges(Node functionClause)
	{
		final List<Node> calls = this.getDescendants(functionClause, NodeInfo.Type.FunctionCall);

		for (Node call : calls)
		{
			final List<Node> children = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
			final Node functionName = children.get(0);
			final Node returnNode = children.get(children.size() - 1);

			this.edg.addEdge(functionName, returnNode, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence));
		}
	}

	/************************************/
	/************** Value ***************/
	/************************************/
	private void generateValueEdges(Node functionClause)
	{
		final List<Node> nodes = this.getDescendants(functionClause);

		for (Node node : nodes)
		{
			final List<Node> valueNodes = GraphTraverser.getValueNodes(node);

			for (Node valueNode : valueNodes)
				this.edg.addEdge(valueNode, node, 0, new EdgeInfo(EdgeInfo.Type.ValueDependence));
		}
	}

	/************************************/
	/************** Others **************/
	/************************************/
	/*******************/
	/** Clause/Guards **/
	/*******************/
	private void generateClauseGuardsEdges(Node functionClause)
	{
		final List<Node> clauses = this.getDescendants(functionClause, NodeInfo.Type.Clause);

		for (Node clause : clauses)
		{
			final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
			final Node guards = parameters.remove(parameters.size() - 1);

			this.edg.addEdge(guards, clause, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence));
		}
	}

	/************************************************************************/
	/************************** Input/Output edges **************************/
	/************************************************************************/
	private void generateInputOutputEdges()
	{
		final Node root = this.edg.getRootNode();
		final List<Node> calls = this.getDescendants(root, NodeInfo.Type.FunctionCall);

		for (Node call : calls)
			this.generateInputOutputEdges(call);
	}
	private void generateInputOutputEdges(Node call)
	{
		this.generateInputEdges(call);
		this.generateOutputEdges(call);
	}

	/************************************/
	/************ Input edges ***********/
	/************************************/
	private void generateInputEdges(Node call)
	{
		final List<Node> possibleClauses = this.getPossibleClauses(call);
		final List<Node> matchingClauses = this.getMatchingClauses(possibleClauses, call);
		final List<Node> arguments = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
		final Node function = arguments.remove(0);
		arguments.remove(arguments.size() - 1);

		for (Node matchingClause : matchingClauses)
		{
			final List<Node> parameters = GraphTraverser.getChildren(matchingClause, EdgeInfo.Type.NormalControl);
			parameters.remove(parameters.size() - 1);

			this.edg.addEdge(function, matchingClause, 0, new EdgeInfo(EdgeInfo.Type.Input));
			for (int argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
			{
				final Node argument = arguments.get(argumentIndex);
				final Node parameter = parameters.get(argumentIndex);
				final List<Node> lasts = GraphTraverser.getLasts(argument, this.constraintsActivated);

				for (Node last : lasts)
					this.edg.addEdge(last, parameter, 0, new EdgeInfo(EdgeInfo.Type.Input));
			}
		}
	}
	private List<Node> getPossibleClauses(Node call)
	{
		final List<Node> arguments = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
		final Node function = arguments.remove(0);
		final NodeInfo functionInfo = function.getData();
		String functionName = functionInfo.getText();
		arguments.remove(arguments.size() - 1);

		switch (functionInfo.getType())
		{
			case Remote:
				return new LinkedList<Node>();
			case Variable:
				functionName = "_";
			case Atom:
			case String:
				return this.getPossibleNormalClauses(functionName, arguments.size());
			case AnonymousFunction:
				return this.getPossibleAnonymousClauses(function, arguments.size());
			default:
				throw new RuntimeException("Call type not contemplated: " + functionInfo.getType());
		}
	}
	private List<Node> getPossibleNormalClauses(String functionName, long functionArity)
	{
		final Node root = this.edg.getRootNode();
		final List<Node> clauses = this.getDescendants(root, NodeInfo.Type.Clause);

		return this.getPossibleClauses(functionName, functionArity, clauses);
	}
	private List<Node> getPossibleAnonymousClauses(Node anonymousFunction, long functionArity)
	{
		final List<Node> clauses = GraphTraverser.getChildren(anonymousFunction, EdgeInfo.Type.NormalControl);

		return this.getPossibleClauses("_", functionArity, clauses);
	}
	private List<Node> getPossibleClauses(String functionName, long functionArity, List<Node> clauses)
	{
		final List<Node> possibleClauses = new LinkedList<Node>();

		for (Node clause : clauses)
		{
			final Node clauseParent = GraphTraverser.getParent(clause, EdgeInfo.Type.NormalControl);
			final NodeInfo parentInfo = clauseParent.getData();
			if (parentInfo.getType() != NodeInfo.Type.Function && parentInfo.getType() != NodeInfo.Type.AnonymousFunction)
				continue;
			final String parentText = parentInfo.getText();
			final long parentArity = (int) parentInfo.getInfo()[0];
			if ((!functionName.equals(parentText) && !functionName.equals("_")) || functionArity != parentArity)
				continue;

			possibleClauses.add(clause);
		}

		return possibleClauses;
	}
	private List<Node> getMatchingClauses(List<Node> possibleClauses, Node call)
	{
		final List<Node> matchingClauses = new LinkedList<Node>();

		for (Node possibleClause : possibleClauses)
			if (this.matchClause(possibleClause, call))
				matchingClauses.add(possibleClause);

		return matchingClauses;
	}
	private boolean matchClause(Node possibleClause, Node call)
	{
		final List<Node> parameters = GraphTraverser.getChildren(possibleClause, EdgeInfo.Type.NormalControl);
		parameters.remove(parameters.size() - 1);
		final List<Node> arguments = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
		arguments.remove(0);
		arguments.remove(arguments.size() - 1);
		if (arguments.size() != parameters.size())
			return false;

		for (int parameterIndex = 0; parameterIndex < parameters.size(); parameterIndex++)
		{
			final Node parameter = parameters.get(parameterIndex);
			final Node argument = arguments.get(parameterIndex);
			final List<Node[]> matches = this.getMatches(parameter, argument);

			if (matches.isEmpty())
				return false;
		}
		return true;
	}

	/************************************/
	/*********** Output edges ***********/
	/************************************/
	private void generateOutputEdges(Node call)
	{
		final List<Node> arguments = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
		final Node function = arguments.get(0);
		final Node callReturn = arguments.get(arguments.size() - 1);
		final List<Node> callingFunctions = GraphTraverser.getInputs(function, GraphTraverser.Direction.Forwards);

		for (Node callingFunction : callingFunctions)
		{
			final List<Node> lasts = GraphTraverser.getLasts(callingFunction, this.constraintsActivated);

			for (Node last : lasts)
				this.edg.addEdge(last, callReturn, 0, new EdgeInfo(EdgeInfo.Type.Output));
		}
	}

	/************************************************************************/
	/***************************** Summary edges ****************************/
	/************************************************************************/
	private void generateSummaryEdges()
	{
		this.generateExternalSummaryEdges();
		this.generateInternalSummaryEdges();
	}

	/************************************/
	/************* External *************/
	/************************************/
	private void generateExternalSummaryEdges()
	{
		final Node root = this.edg.getRootNode();
		final List<Node> calls = this.getDescendants(root, NodeInfo.Type.FunctionCall);
		final StarConstraint starConstraint = this.getStarConstraint();

		for (Node call : calls)
		{
			final List<Node> children = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
			final Node functionNameNode = children.get(0);
			if (functionNameNode.getData().getType() == NodeInfo.Type.AnonymousFunction)
				continue;
			final List<Node> inputs = GraphTraverser.getInputs(functionNameNode, GraphTraverser.Direction.Forwards);
			if (!inputs.isEmpty())
				continue;

			final List<Node> arguments = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
			arguments.remove(0);
			final Node returnNode = arguments.remove(arguments.size() - 1);

			for (Node argumentNode : arguments)
			{
				final List<Node> lasts = GraphTraverser.getLasts(argumentNode, this.constraintsActivated);

				for (Node last : lasts)
					this.edg.addEdge(last, returnNode, 0, new EdgeInfo(EdgeInfo.Type.Summary, starConstraint));
			}
		}
	}
	private StarConstraint getStarConstraint()
	{
		return new StarConstraint();
	}

	/************************************/
	/************* Internal *************/
	/************************************/
	private void generateInternalSummaryEdges()
	{
		final List<Work> initialWorks = this.getInitialWorks();
		final WorkList workList = new WorkList(initialWorks);

// TODO Delete
// TODO Arreglar
/*
List<Integer> summaryNodesIds = new LinkedList<Integer>();
summaryNodesIds.add(5185);
summaryNodesIds.add(8973);
summaryNodesIds.add(974);
summaryNodesIds.add(3044);
summaryNodesIds.add(2673);
summaryNodesIds.add(4984);
summaryNodesIds.add(6126);
summaryNodesIds.add(6074);
summaryNodesIds.add(8033);
summaryNodesIds.add(551);
summaryNodesIds.add(2655);
summaryNodesIds.add(2690);
summaryNodesIds.add(8934);
summaryNodesIds.add(365);
summaryNodesIds.add(2420);
summaryNodesIds.add(6652);
summaryNodesIds.add(3373);
summaryNodesIds.add(2137);
summaryNodesIds.add(6438);
summaryNodesIds.add(1326);
summaryNodesIds.add(1451);
summaryNodesIds.add(1470);
summaryNodesIds.add(1266);
summaryNodesIds.add(8793);
summaryNodesIds.add(8344);
summaryNodesIds.add(1857);
summaryNodesIds.add(5944);
summaryNodesIds.add(8147);
summaryNodesIds.add(2767);
summaryNodesIds.add(4391);
summaryNodesIds.add(4908);
summaryNodesIds.add(4384);
summaryNodesIds.add(1652);
summaryNodesIds.add(5862);
summaryNodesIds.add(5828);
summaryNodesIds.add(1726);
summaryNodesIds.add(347);
summaryNodesIds.add(1411);
summaryNodesIds.add(1431);
summaryNodesIds.add(2403);
*/

int iteraciones = 0;
		while (workList.hasMore())
		{
iteraciones++;
Chronometer.start("1 - Dentro");
Chronometer.start("1.1 - Empieza");
			final Work work = workList.next();
			final Node initialNode = work.getInitialNode();
			final Node currentNode = work.getCurrentNode();
			final Constraints constraints = work.getConstraints();
Chronometer.finish("1.1 - Empieza");

Chronometer.start("1.2 - IsFormalIn");
			if (this.isFormalIn(currentNode, initialNode))
			{
				// Formal in found
				final List<Node> nodesToContinue = this.createSummaryEdges(initialNode, currentNode, constraints);
				final List<Work> worksToContinue = this.getWorksToContinue(workList, nodesToContinue);

				workList.repending(worksToContinue);
			}
Chronometer.finish("1.2 - IsFormalIn");
Chronometer.start("1.3 - Done");
			workList.done(work);
Chronometer.finish("1.3 - Done");

Chronometer.start("1.4 - NewWorks");
			final List<Work> newWorks = this.getNewWorks(workList, work);
Chronometer.finish("1.4 - NewWorks");
Chronometer.start("1.5 - AddNewWorks");
			for (Work newWork : newWorks)
				workList.add(newWork);
Chronometer.finish("1.5 - AddNewWorks");
Chronometer.finish("1 - Dentro");
if (iteraciones % 10000 == 0)
Chronometer.showTimes();
		}
Chronometer.showTimes();
	}
	private List<Work> getInitialWorks()
	{
		final List<Work> workList = new LinkedList<Work>();
		final List<Node> functions = this.edg.findNodesByData(null, new Comparator<NodeInfo>() {
			public int compare(NodeInfo o1, NodeInfo o2)
			{
				return o2.getType() == NodeInfo.Type.Function || o2.getType() == NodeInfo.Type.AnonymousFunction ? 0 : 1;
			}
		});

		for (Node function : functions)
		{
			final List<Node> clauses = GraphTraverser.getChildren(function, EdgeInfo.Type.NormalControl);
			for (Node clause : clauses)
			{
				final List<Node> initialWorkNodes = GraphTraverser.getLasts(clause, this.constraintsActivated);

				for (Node initialWorkNode : initialWorkNodes)
					workList.add(new Work(initialWorkNode, initialWorkNode, new Constraints(), false, false));
			}
		}

		return workList;
	}
	private boolean isFormalIn(Node node, Node formalOutNode)
	{
		if (node.getData().getType() == NodeInfo.Type.Guard)
			return false;

		final Node parent = GraphTraverser.getParent(node, EdgeInfo.Type.NormalControl);
		final NodeInfo.Type parentType = parent == null ? null : parent.getData().getType();
		if (parent == null || parentType != NodeInfo.Type.Clause)
			return false;

		final Node grandparent = GraphTraverser.getParent(parent, EdgeInfo.Type.NormalControl);
		final NodeInfo.Type grandparentType = grandparent == null ? null : grandparent.getData().getType();
		if (grandparent == null || (grandparentType != NodeInfo.Type.Function && grandparentType != NodeInfo.Type.AnonymousFunction))
			return false;

		// The formal in should be related to the formal out
		Node ancestor = formalOutNode;
		while (ancestor != grandparent)
		{
			final NodeInfo.Type ancestorType = ancestor.getData().getType();
			if (ancestorType == NodeInfo.Type.Function || ancestorType == NodeInfo.Type.AnonymousFunction)
				return false;
			ancestor = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
		}

		return true;
	}
	private List<Node> createSummaryEdges(Node formalOut, Node formalIn, Constraints constraints)
	{
		final List<Node> nodesToContinue = new LinkedList<Node>();

		final Node clause = GraphTraverser.getParent(formalIn, EdgeInfo.Type.NormalControl);
		final List<Node> functionCallers = GraphTraverser.getInputs(clause, GraphTraverser.Direction.Backwards);
		final List<Node> inputs = GraphTraverser.getInputs(formalIn, GraphTraverser.Direction.Backwards);
		final List<Node> outputs = GraphTraverser.getOutputs(formalOut, GraphTraverser.Direction.Forwards);

		final SummaryConstraint summaryConstraint = this.getSummaryConstraint(formalIn);
		final List<Object> production0 = Arrays.asList(constraints.toArray());
		final List<Constraint> production = new LinkedList<Constraint>();

		for (Object element : production0)
			production.add((Constraint) element);
		this.edg.addProduction(summaryConstraint, production);

		for (Node functionCaller : functionCallers)
		{
			final Node caller = GraphTraverser.getParent(functionCaller, EdgeInfo.Type.NormalControl);
			final List<Node> argumentNodes = this.getDescendantsNode(caller, inputs);
			final Node returnNode = this.getDescendantsNode(caller, outputs).get(0);

			for (Node argumentNode : argumentNodes)
				this.edg.addEdge(argumentNode, returnNode, 0, new EdgeInfo(EdgeInfo.Type.Summary, summaryConstraint));
			nodesToContinue.add(returnNode);
		}

		return nodesToContinue;
	}
	private List<Node> getDescendantsNode(Node node, List<Node> list)
	{
		final List<Node> descendants = new LinkedList<Node>();

		for (Node node1 : list)
		{
			Node ancestor = node1;
			while (ancestor != null && ancestor != node)
				ancestor = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
			if (ancestor == node && node != node1)
				descendants.add(node1);
		}

		return descendants;
	}
	private SummaryConstraint getSummaryConstraint(Node clause)
	{
		// TODO Get/Create the correct SummaryConstraint
		return new SummaryConstraint(clause);
	}
	private List<Work> getWorksToContinue(WorkList workList, List<Node> nodesToContinue)
	{
		final List<Work> worksToContinue = new LinkedList<Work>();

		for (Node nodeToContinue : nodesToContinue)
		{
			final int id = nodeToContinue.getData().getId();
			final List<Work> worksToContinue0 = workList.getDone(id);

			worksToContinue.addAll(worksToContinue0);
		}

		return worksToContinue;
	}
	private List<Work> getNewWorks(WorkList workList, Work work)
	{
Chronometer.start("1.4 - getNewWorks");
Chronometer.start("1.4.1 - slicingAlgorithm");
		final AdvancedAlgorithm slicingAlgorithm = new AdvancedAlgorithm(this.edg, false, true);
Chronometer.finish("1.4.1 - slicingAlgorithm");
Chronometer.start("1.4.2 - processWork");
		final List<Work> works = slicingAlgorithm.processWork(work, EdgeInfo.Type.Input, EdgeInfo.Type.Output);
Chronometer.finish("1.4.2 - processWork");
Chronometer.start("1.4.3 - addWorks");
		final List<Work> newWorks = new LinkedList<Work>();

		for (Work work0 : works)
		{
Chronometer.start("1.4.3.1 - workList");
			if (workList.contains(work0))
{
Chronometer.finish("1.4.3.1 - workList");
				continue;
}
Chronometer.finish("1.4.3.1 - workList");
			newWorks.add(work0);
		}
Chronometer.finish("1.4.3 - addWorks");
Chronometer.finish("1.4 - getNewWorks");

		return newWorks;
	}
}