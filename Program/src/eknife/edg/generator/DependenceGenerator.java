package eknife.edg.generator;

import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eknife.config.Config;
import eknife.edg.EDG;
import eknife.edg.Edge; // ADDED BY SERGIO
import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.EDG.GrammarType;
import eknife.edg.constraint.AccessConstraint.Operation;
import eknife.edg.constraint.AccessConstraint;
import eknife.edg.constraint.BinComprehensionConstraint;
import eknife.edg.constraint.Constraint;
import eknife.edg.constraint.Constraints;
import eknife.edg.constraint.SummaryConstraints.SummaryType;
import eknife.edg.constraint.EmptyConstraint;
import eknife.edg.constraint.ExceptionArgumentConstraint;
import eknife.edg.constraint.ExceptionConstraint;
import eknife.edg.constraint.ListComprehensionConstraint;
import eknife.edg.constraint.MapConstraint;
import eknife.edg.constraint.RecordConstraint;
import eknife.edg.constraint.SlicingConstraints;
import eknife.edg.constraint.StarConstraint;
import eknife.edg.constraint.SummaryConstraint;
import eknife.edg.constraint.SummaryConstraints;
import eknife.edg.constraint.UnresolvableConstraint;
//import eknife.edg.constraint.RecordConstraint.Operation;
import eknife.edg.slicingAlgorithm.SlicingAlgorithm2;
import eknife.edg.traverser.EdgeTraverser.Phase;
import eknife.edg.traverser.GraphTraverser;
import eknife.edg.util.Work;
import eknife.edg.util.WorkList;
import eknife.misc.Cronometro;
import eknife.misc.Misc;
import eknife.sergio.ClauseRelation;

public class DependenceGenerator
{
	private EDG graph;

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
			allNodes = this.graph.getNodes();
		else
		{
			final NodeInfo data = new NodeInfo(0, type);
			final Comparator<NodeInfo> compare = this.getTypeComparator();
			allNodes = this.graph.findNodesByData(data, compare);
		}

		if (root == this.graph.getRootNode())
			return allNodes;
		return GraphTraverser.getDescendantNodes(root, allNodes);
	}
	private List<Node> getScope(Node node)
	{
		return this.getScope(node, null);
	}
	private List<Node> getScope(Node node, NodeInfo.Type type)
	{
		final List<Node> scope = new LinkedList<Node>();
		final Node root = this.graph.getRootNode();
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
				if (ancestorPreviousSiblingType == NodeInfo.Type.Clause 
						|| ancestorPreviousSiblingType == NodeInfo.Type.AfterTry //ADDED BY SERGIO
						|| ancestorPreviousSiblingType == NodeInfo.Type.Try //ADDED BY SERGIO
						|| ancestorPreviousSiblingType == NodeInfo.Type.CatchClause //ADDED BY SERGIO
						|| ancestorPreviousSiblingType == NodeInfo.Type.Guard) //ADDED BY SERGIO
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
				else if (ancestorParentType == NodeInfo.Type.Clause && (ancestorType != NodeInfo.Type.Guard && ancestorType != NodeInfo.Type.Body)) 
					isDeclaration = true;  // MODIFIED (ancestorType != NodeInfo.Type.Guard) -> (ancestorType != NodeInfo.Type.Guard && ancestorType != NodeInfo.Type.Body) (BY SERGIO)

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
			final List<Node> scopeVariables = this.getScope(declaredVariableParent, NodeInfo.Type.Variable);
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
	public void generateEdges(EDG graph)
	{
		this.graph = graph;

		this.generateDataEdges();
		this.generateInputOutputEdges();
		this.generateExceptionEdges();
		this.generateSummaryEdges();
	}

	/************************************************************************/
	/****************************** Data edges ******************************/
	/************************************************************************/
	private void generateDataEdges()
	{
		final Node root = this.graph.getRootNode();
		final List<Node> functions = GraphTraverser.getChildren(root, EdgeInfo.Type.NormalControl);
		
		for (Node function : functions)
		{		
			final List<Node> functionClauses = GraphTraverser.getChildren(function, EdgeInfo.Type.NormalControl);
			for (Node functionClause : functionClauses)
				this.generateDataEdges(functionClause);
		}
		this.generateGuardDependenceEdges(); // TODO CAMBIAR ESTO DE SITIO Y HACERLO PARA CADA FUNCION AQUI NO ESTA BIEN
	}
	private void generateDataEdges(Node functionClause)
	{
		this.generateFlowRelationEdges(functionClause);
		this.generateMatchingEdges(functionClause);
		this.generateComprehensionEdges(functionClause);
		this.generateRestrictionsEdges(functionClause);
		this.generateFunctionCallsEdges(functionClause);
		if (Config.constraintsActivated)
			this.generateValueEdges(functionClause);
		this.generateBinEdges(functionClause);
		
		// ADDED BY SERGIO FOR EXCEPTION DATA CONTROL
		this.generateExceptionPatternEdges(functionClause);
		this.generateTryOfClausesEdges(functionClause);
		this.generateMapMatchingEdges(functionClause);
		// ------------------------------------------
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
			final String usedVariableName = usedVariable.getData().getName();
			final List<Node> scope = this.getScope(usedVariable, NodeInfo.Type.Variable);
			final List<Node> declaredVariablesInScope = Misc.intersect(declaredVariables, scope);

			for (Node declaredVariableInScope : declaredVariablesInScope)
			{
				final String declaredVariableName = declaredVariableInScope.getData().getName();
				if (declaredVariableName.equals(usedVariableName))
					this.graph.addEdge(declaredVariableInScope, usedVariable, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint()));
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
					this.graph.addEdge(match[0], match[1], 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint()));
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
				this.graph.addEdge(match[0], match[1], 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint()));
		}
	}
	private List<Node[]> getMatches(Node pattern, Node expression)
	{
		final List<Node[]> matches = new LinkedList<Node[]>();
		final NodeInfo patternData = pattern.getData();
		final NodeInfo expressionData = expression.getData();
		final NodeInfo.Type patternType = patternData.getType();
		final NodeInfo.Type expressionType = expressionData.getType();
		final String patternName = patternData.getName();
		final String expressionName = expressionData.getName();

		// TODO Group types
		if (expressionType == NodeInfo.Type.Variable ||
			((patternType == NodeInfo.Type.Atom || patternType == NodeInfo.Type.String || patternType == NodeInfo.Type.Integer || patternType == NodeInfo.Type.Char || patternType == NodeInfo.Type.FunctionIdentifier) &&
			(expressionType == NodeInfo.Type.Atom || expressionType == NodeInfo.Type.String || expressionType == NodeInfo.Type.Integer || patternType == NodeInfo.Type.Char || expressionType == NodeInfo.Type.FunctionIdentifier) &&
			patternName.equals(expressionName)))
			matches.add(new Node[] { expression, pattern } );
		if (patternType == NodeInfo.Type.Variable ||
			((patternType == NodeInfo.Type.Atom || patternType == NodeInfo.Type.String || patternType == NodeInfo.Type.Integer || patternType == NodeInfo.Type.Char) && (expressionType == NodeInfo.Type.Operation || expressionType == NodeInfo.Type.FunctionCall)) ||
			(patternType == NodeInfo.Type.TuplePattern && expressionType == NodeInfo.Type.FunctionCall) ||
			(patternType == NodeInfo.Type.ListPattern && (expressionType == NodeInfo.Type.Operation || expressionType == NodeInfo.Type.FunctionCall || expressionType == NodeInfo.Type.ListComprehension)))
		{
			final List<Node> lasts = GraphTraverser.getLasts(expression);
			for (Node last : lasts)
				matches.add(new Node[] { last, pattern } );
		}
		if ((patternType == NodeInfo.Type.Atom || patternType == NodeInfo.Type.String || patternType == NodeInfo.Type.Integer || patternType == NodeInfo.Type.Char || patternType == NodeInfo.Type.TuplePattern || patternType == NodeInfo.Type.ListPattern) &&
			(expressionType == NodeInfo.Type.Case || expressionType == NodeInfo.Type.If || expressionType == NodeInfo.Type.PatternMatching || expressionType == NodeInfo.Type.Block))
		{
			final List<Node> lastRoots = GraphTraverser.getLasts(expression);
			for (Node lastRoot : lastRoots)
				matches.addAll(this.getMatches(pattern, lastRoot));
		}
		else if (patternType == NodeInfo.Type.PatternMatching)
		{
			final List<Node> lasts = GraphTraverser.getLasts(pattern);
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
		final List<Node> lasts = GraphTraverser.getLasts(expression);
		final Constraint comprehensionConstraint = comprehension.getData().getType() == NodeInfo.Type.ListComprehension ? new ListComprehensionConstraint() : new BinComprehensionConstraint();

		for (Node last : lasts)
			this.graph.addEdge(last, result, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, comprehensionConstraint));
	}
	private void generateComprehensionEdges2(Node comprehension)
	{
		final List<Node> generators = GraphTraverser.getChildren(comprehension, EdgeInfo.Type.NormalControl);
		generators.remove(generators.size() - 2);
		final Node expression = generators.remove(generators.size() - 1);
		final StarConstraint starConstraint = this.getStarConstraint();

		for (Node generator : generators)
		{
			final List<Node> lasts = GraphTraverser.getLasts(generator);

			for (Node last : lasts)
				this.graph.addEdge(last, expression, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, starConstraint));
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

				this.graph.addEdge(binElementSizeNode, binNode, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, starConstraint));
			}
		}
	}

	/************************************/
	/***** Restrictions dependences *****/
	/************************************/
	private void generateRestrictionsEdges(Node functionClause)
	{
		//final List<Node> declaredVariables = this.getDeclaredVariables(functionClause);
		final List<Node> clauses = this.getDescendants(functionClause, NodeInfo.Type.Clause);

		for (Node clause : clauses)
		{
			// TODO Mirar P5S1 => Nodos 41, 55, 56 => ¿Deberia haber una restriccion?
			final List<Node> parametersConstraints = this.getConstraints(clause);
			final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
			
			// MODIFIED (BY SERGIO)
			//final Node guard = parameters.remove(parameters.size() - 1);
			
			// ADD edge guard -> body
			final Node guard = parameters.remove(parameters.size() - 2); 
			parameters.remove(parameters.size() - 1); 
			
			for (Node parameterConstraint : parametersConstraints)
			{
				final NodeInfo.Type nodeType = parameterConstraint.getData().getType();

				if (nodeType != NodeInfo.Type.TuplePattern && nodeType != NodeInfo.Type.ListPattern)
					this.graph.addEdge(parameterConstraint, guard, 0, new EdgeInfo(EdgeInfo.Type.GuardRestriction, new EmptyConstraint()));
				else
				{
					final UnresolvableConstraint constraint = new UnresolvableConstraint();
					this.graph.addEdge(parameterConstraint, guard, 0, new EdgeInfo(EdgeInfo.Type.GuardRestriction, constraint));
				}
			}
// Aqui se vincula cada variable del scope con su uso en la guarda, esto ya no es necesario porque la guarda ya esta desglosada (BY SERGIO)
			
//			final List<Node> scope = this.getScope(guard, NodeInfo.Type.Variable);
//			final List<Node> declaredVariablesInScope = Misc.intersect(declaredVariables, scope);
//			final List<Node> variablesInGuards = this.getVariablesInGuards(guard, declaredVariablesInScope);
//			for (Node variableInGuards : variablesInGuards)
//				this.graph.addEdge(variableInGuards, guard, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint()));

			// TODO Delete
//			final List<Node> restrictions = Misc.union(constraints, variablesInGuards);
//			for (Node restriction : restrictions)
//				this.graph.addEdge(restriction, guard, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint()));
		}
	}

	/*******************/
	/*** Constraints ***/
	/*******************/
	private List<Node> getConstraints(Node clause)
	{
		final List<Node> constraints = new LinkedList<Node>();
		final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
		
		//MODIFIED (BY SERGIO)
		//final Node guard = parameters.remove(parameters.size() - 1);
		final Node guard = parameters.remove(parameters.size() - 2);
		parameters.remove(parameters.size() - 1); 
		
		final List<Node> variables = this.getScope(guard, NodeInfo.Type.Variable);
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
			final String variableName = variable.getData().getName();

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
				if (repeatedVariables.contains(info.getName()))
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
				final List<Node> compoundChildren = GraphTraverser.getChildren(parameter, EdgeInfo.Type.Control);
				for (Node compoundChild : compoundChildren)
					constraints.addAll(this.getConstraints(compoundChild, repeatedVariables));
				break;
			case ExceptionPattern:
				break;
			default:
				throw new RuntimeException("Parameter type not contempled: " + info.getType());
		}

		return constraints;
	}

// ---------------------------------------DEPRECATED---------------------------------------
	/*******************/
	/** Used in guard **/
	/*******************/
	private List<Node> getVariablesInGuards(Node guard, List<Node> variables)
	{
		final List<Node> variablesInGuards = new LinkedList<Node>();
		final OtpErlangList guards = (OtpErlangList) guard.getData().getAST();
		final List<String> guardsVariables = this.getGuardsVariables(guards);

		for (Node variable : variables)
		{
			final String variableName = variable.getData().getName();

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
			case "atom":
			case "char":
				return new LinkedList<String>();
			default:
				throw new RuntimeException("Expression type not contempled: " + expressionType);
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
				throw new RuntimeException("Operation type not contempled: " + operationType.atomValue());
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
// --------------------------------------------------------------------------------------------
	
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
//final Node exceptionReturnNode = children.get(children.size() - 2); // ADDED BY SERGIO (ExceptionReturn)

			this.graph.addEdge(functionName, returnNode, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new StarConstraint()));
//this.graph.addEdge(functionName, exceptionReturnNode, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint())); // ADDED BY SERGIO (ExceptionReturn)
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
				this.graph.addEdge(valueNode, node, 0, new EdgeInfo(EdgeInfo.Type.ValueDependence, new EmptyConstraint()));
		}
	}

	/*************************************/
	/*** Exception pattern dependences ***/
	/*************************************/
	private void generateExceptionPatternEdges(Node functionClause)
	{
		final List<Node> catchNodes = this.getDescendants(functionClause, NodeInfo.Type.CatchClause);

		for (Node _catch : catchNodes)
		{
			final List<Node> clauses = GraphTraverser.getChildren(_catch, EdgeInfo.Type.NormalControl);
			for (Node clause : clauses)
			{
				final List<Node> children = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
				final Node exceptionPattern = children.get(0);
				if (exceptionPattern.getData().getType() == NodeInfo.Type.ExceptionPattern)
				{
					final Node guard = children.get(1);
					this.graph.addEdge(exceptionPattern, guard, 0, new EdgeInfo(EdgeInfo.Type.GuardRestriction, new EmptyConstraint()));
				}	
			}
		}
	}
	private void generateTryOfClausesEdges(Node functionClause)
	{
		final List<Node> tryOfNodes = this.getDescendants(functionClause, NodeInfo.Type.TryOf);
		
		for (Node tryOf : tryOfNodes)
		{
			final List<Node> clauses = GraphTraverser.getChildren(tryOf, EdgeInfo.Type.NormalControl);
			final Node _try = clauses.remove(0);
			if ((clauses.get(clauses.size()-1)).getData().getType() == NodeInfo.Type.AfterTry)
			{
				clauses.remove(clauses.size()-2);
				clauses.remove(clauses.size()-1);
			}
			else
				clauses.remove(clauses.size()-1);
			
			for (Node clause : clauses)
			{
				final List<Node> children = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
				final Node pattern = children.get(0);
				this.graph.addEdge(_try, pattern, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint()));	
			}
		}
		
	}
	
	/************************************/
	/************** Value ***************/
	/************************************/
	private void generateMapMatchingEdges(Node functionClause)
	{
		final List<Node> mapMatchingNodes = this.getDescendants(functionClause, NodeInfo.Type.MapMatching);
		
		for (Node mapMatching : mapMatchingNodes)
		{
			final Node map = GraphTraverser.getChild(mapMatching, 0);
			final List<Node> fields = GraphTraverser.getChildren(map, EdgeInfo.Type.StructuralControl);
			boolean existsFreshVariable = false;
			
			for(Node field : fields)
			{
				final List<Node> fieldExpressions = GraphTraverser.getChildren(field, EdgeInfo.Type.Control);
				final Node key = fieldExpressions.remove(0);
				final Node value = fieldExpressions.remove(0);
				
				if(value.getData().getType() == NodeInfo.Type.Variable && isFreshVariable(value))
				{
					existsFreshVariable = true;
					this.graph.addEdge(key, value, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new MapConstraint(MapConstraint.Operation.Add))); // Edge Key -> Value
				}
			}
			if (existsFreshVariable)
			{
				final Node mapVariable = GraphTraverser.getChild(mapMatching, 1);
				// Edge MapVariable -> map (with constraint)
				this.graph.addEdge(mapVariable, map, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new MapConstraint(MapConstraint.Operation.Remove)));
			}
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
		
//Node previousParent = null;
//final List<Node> expressionPatterns = new LinkedList<Node>();
		
		for (Node clause : clauses)
		{
			final List<Node> parameters = GraphTraverser.getChildren(clause, EdgeInfo.Type.NormalControl);
			final Node guards = parameters.remove(parameters.size() - 2); // MODIFIED BY SERGIO parameters.size() - 1 => parameters.size() - 2 
			final Node body = parameters.remove(parameters.size() - 1);
			
			this.graph.addEdge(guards, body, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint())); // Edge guards -> body 
			this.graph.addEdge(guards, clause, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint())); // Edge guards -> clause
			
// ADDED BY SERGIO
/*final Node clauseParent = GraphTraverser.getParent(clause, EdgeInfo.Type.NormalControl);
if (clauseParent.getData().getType() != NodeInfo.Type.Function)
	if (previousParent == null)
	{
		previousParent = clauseParent;
		expressionPatterns.add(GraphTraverser.getChild(clause, 0));
	}
	else if (previousParent == clauseParent)
	{
		expressionPatterns.add(GraphTraverser.getChild(clause, 0));
	}
	else 
	{
		this.generateGuardGuardEdges(expressionPatterns);
		expressionPatterns.clear();
		expressionPatterns.add(GraphTraverser.getChild(clause, 0));
		previousParent = clauseParent;
	}*/
		}
//if (!expressionPatterns.isEmpty())	// ADDED BY SERGIO
//	this.generateGuardGuardEdges(expressionPatterns);
	}
	private void generateGuardDependenceEdges()
	{
		final List<Node> nodes = this.graph.getNodes();
		final List<Node> clauseContainers = new LinkedList<Node>();
		for (Node node : nodes)
			switch (node.getData().getType())
			{
				case Case:
				case CatchClause:
				case TryOf:
				//case If:
				//case Function:
					clauseContainers.add(node);
					break;
				default:
					break;
			}
		
		for (Node clauseContainer : clauseContainers)
		{
			
			final List<Node> clauses = new LinkedList<Node>();
			final List<Node> containerChildren = GraphTraverser.getChildren(clauseContainer, EdgeInfo.Type.Control);
			final NodeInfo.Type clauseContainerType = clauseContainer.getData().getType(); 
			if (clauseContainerType == NodeInfo.Type.Case)
				containerChildren.remove(0);
			else if (clauseContainerType == NodeInfo.Type.TryOf)
			{
				containerChildren.remove(0);
				if (this.hasAfter(containerChildren))
					containerChildren.remove(containerChildren.size() - 1);
				containerChildren.remove(containerChildren.size() - 1);
			}
			
			clauses.addAll(containerChildren); 
			final List<Node> clausePatterns = new LinkedList<Node>();
			for (Node clause : clauses)
			{
				clausePatterns.add(GraphTraverser.getChild(clause, 0));
			}
			this.generateGuardGuardEdges(clausePatterns);
		}
	}
	
// ADDED BY SERGIO
	private void generateGuardGuardEdges(List<Node> expressionPatterns) 
	{
		final int clauseSize = expressionPatterns.size();
		ClauseRelation[][] clauseRelations = new ClauseRelation[clauseSize][clauseSize];
		
		if ( clauseSize != 1)
		{
			for (int comparePatternIndex = 1; comparePatternIndex < expressionPatterns.size(); comparePatternIndex++)
			{
				final Node comparePattern = expressionPatterns.get(comparePatternIndex);
				
				for (int previousPatternIndex = comparePatternIndex - 1 ; previousPatternIndex >= 0; previousPatternIndex--)
				{
					final Node previousPattern = expressionPatterns.get(previousPatternIndex);
					
					if ( clauseRelations[comparePatternIndex][previousPatternIndex] == null || clauseRelations[comparePatternIndex][previousPatternIndex].getRelated() == false 
							|| (clauseRelations[comparePatternIndex][previousPatternIndex].getRelated() && !clauseRelations[comparePatternIndex][previousPatternIndex].getTotal()) ) 
					{
						final ClauseRelation cr = isRelated(comparePattern, previousPattern);
						if (cr.getRelated())
						{
							// DRAW EDGE
							clauseRelations[comparePatternIndex][previousPatternIndex] = cr;
							final Node compareBody= GraphTraverser.getChild(GraphTraverser.getParent(comparePattern,EdgeInfo.Type.Control), 2);
							final Node previousGuard = GraphTraverser.getChild(GraphTraverser.getParent(previousPattern,EdgeInfo.Type.Control), 1);
							this.graph.addEdge(previousGuard, compareBody, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint()));
							this.copyExistentRelations(clauseRelations, comparePatternIndex, previousPatternIndex);
						}
					}
				}
			}
		}
	}

	private ClauseRelation isRelated(Node compare, Node previous) // ADDED BY SERGIO FOR CLAUSES CONTAINED IN PREVIOUS CLAUSES
	{
		if (previous.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(previous))
			return new ClauseRelation(true,false); // THE PATTERN "compare" IS DEAD CODE
		if (previous.getData().getType() == NodeInfo.Type.CompoundPattern)
		{
			previous = this.resolveCompoundPattern(previous);
			if (previous == null)
				return new ClauseRelation(false,false);
		}
		switch(compare.getData().getType())
		{
			case Atom:
			case Integer:
			case String:
			case Char:
				switch (previous.getData().getType())
				{
					case Atom:
					case Integer:
					case String:
					case Char:
						if (compare.getData().getType() == previous.getData().getType() &&
							compare.getData().getName().equals(previous.getData().getName()))
							return new ClauseRelation(true,true);
						return new ClauseRelation(false,false);
					case Variable:
					default:
						return new ClauseRelation(true,true);
				}
			case Variable:
				if (this.isFreshVariable(compare))
					return new ClauseRelation(true,true);
				else
					return new ClauseRelation(true,false); // Al no saber el valor de la variable debo suponer que no es total 
														   // (pueden incluso no estar relacionadas pero tengo que suponer que si lo estan)
			case TuplePattern:
				return this.isRelatedTuple(compare,previous);
			case ListPattern:
				return this.isRelatedList(compare,previous);
			case CompoundPattern:
				final Node resultNode = this.resolveCompoundPattern(compare);
				if (resultNode == null)
					return new ClauseRelation(false,false);
				return this.isRelated(resultNode, previous);
			default:
				return new ClauseRelation(false,false);
		
		}
	}
	private ClauseRelation isRelatedTuple(Node tuple, Node previousTuple)
	{
		if (previousTuple.getData().getType() == NodeInfo.Type.Variable)
			return new ClauseRelation(true,false); // DEAD CODE
		if (previousTuple.getData().getType() == NodeInfo.Type.TuplePattern || previousTuple.getData().getType() == NodeInfo.Type.TupleExpression)
		{
			if (GraphTraverser.getChildCount(tuple) != GraphTraverser.getChildCount(previousTuple))
				return new ClauseRelation(false,false);
			else
			{
				final List<Node> tupleChildren = GraphTraverser.getChildren(tuple, EdgeInfo.Type.StructuralControl);
				final List<Node> previousTupleChildren = GraphTraverser.getChildren(previousTuple, EdgeInfo.Type.StructuralControl);
				boolean total = true;
				for (int childIndex = 0; childIndex < GraphTraverser.getChildCount(tuple); childIndex++)
				{
					final Node tupleChild = tupleChildren.get(childIndex);
					final Node previousTupleChild = previousTupleChildren.get(childIndex);
					if (tupleChild.getData().getType() == previousTupleChild.getData().getType())
					{
						if (tupleChild.getData().getType() != NodeInfo.Type.Variable && 
							tupleChild.getData().getType() != NodeInfo.Type.TuplePattern &&
							tupleChild.getData().getType() != NodeInfo.Type.ListPattern &&
							!tupleChild.getData().getName().equals(previousTupleChild.getData().getName()))
							return new ClauseRelation(false,false);
						
						if (!isRelated(tupleChild, previousTupleChild).getRelated())
							return new ClauseRelation(false,false);
					}
					else
					{
						final ClauseRelation cr = isRelated(tupleChild, previousTupleChild);
						total = total && cr.getTotal();
					}
				}
				return new ClauseRelation(true,total);
			}
		}
		else
			return new ClauseRelation(false,false);
	}
	private ClauseRelation isRelatedList(Node list, Node previousList)
	{
		if (previousList.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(previousList))
			return new ClauseRelation(true,false); // DEAD CODE
		if (previousList.getData().getType() == NodeInfo.Type.ListPattern 
				&& (GraphTraverser.getChildCount(previousList) == 0 || GraphTraverser.getChildCount(list) == 0))
			return new ClauseRelation(false,false);
		if (previousList.getData().getType() == NodeInfo.Type.ListPattern)
		{
			Node listRef = list;
			Node previousListRef = previousList;
			boolean lastList = false;
			boolean lastPrevious = false;
			boolean total = true;
			while(!lastList && !lastPrevious)
			{
				final Node listElem;
				final Node previousListElem;
				if (GraphTraverser.getChildCount(listRef) != 0)
					listElem = GraphTraverser.getChild(listRef, 0);
				else 
				{
					listElem = listRef;
					lastList = true;
				}	
				if (GraphTraverser.getChildCount(previousListRef) != 0)
					previousListElem = GraphTraverser.getChild(previousListRef, 0);
				else 
				{
					previousListElem = previousListRef;
					lastPrevious = true;
				}
				
				if (lastList && !lastPrevious)		// LA LISTA DE LA SEGUNDA CLAUSULA ACABA Y LA PRIMERA NO
				{	
					if (listElem.getName().equals("[]")) // LISTAS SIN COLA
					{
						return new ClauseRelation(false,false);
					}
					else if (!(listElem.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(listElem)))
							return new ClauseRelation(false,false);
				}
				else if (!lastList && lastPrevious) // LA LISTA DE LA PRIMERA CLAUSULA ACABA Y LA SEGUNDA NO
				{
					if (previousListElem.getName().equals("[]")) //LISTAS SIN COLA
					{
						return new ClauseRelation(false,false);
					}
					else if (previousListElem.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(previousListElem))
						return new ClauseRelation(true,false);
					else if (!(listElem.getData().getType() == NodeInfo.Type.Variable && 	// El elemento es una variable
							(previousListElem.getData().getType() == NodeInfo.Type.Variable || // Previous es una variable o una lista de un elemento 
								(previousListElem.getData().getType() == NodeInfo.Type.ListPattern && // (Al ser variable y no haber dataflow no sabemos si la variable será una lista de un elemento o no)
								GraphTraverser.getChildCount(GraphTraverser.getChild(previousListElem, 1)) == 0) &&
								GraphTraverser.getChild(previousListElem, 1).getName().equals("[]")) &&  
							GraphTraverser.getChildCount(GraphTraverser.getChild(listRef, 1)) == 0))	// Y es el ultimo elemento 
						return new ClauseRelation(false,false);
				}
				else if (lastList && lastPrevious) // AMBAS LISTAS ACABAN
				{
					if (previousListElem.getName().equals("[]")) //LISTAS SIN COLA
					{
						if (!(listElem.getName().equals("[]") || listElem.getData().getType() == NodeInfo.Type.Variable))
							return new ClauseRelation(false,false);
					}
					else if ( !(previousListElem.getData().getType() == listElem.getData().getType() &&
							    previousListElem.getData().getType() != NodeInfo.Type.Variable &&
							    previousListElem.getData().getName().equals(listElem.getData().getName())) )
					{
						final ClauseRelation cr;
						if ((previousListElem.getData().getType() == NodeInfo.Type.Variable && listElem.getData().getType() == NodeInfo.Type.Variable) && 
								(this.isFreshVariable(previousListElem) && this.isFreshVariable(listElem)))
							cr = new ClauseRelation(true,true);
						else
							cr = isRelated(listElem, previousListElem);
						if (!cr.getRelated())
							return new ClauseRelation(false,false);
						total = total && cr.getTotal();
					}
				}
				else
				{
					final ClauseRelation cr = isRelated(listElem, previousListElem);
					if (!cr.getRelated())
						return new ClauseRelation(false,false);
					total = total && cr.getTotal();
				}
				if (!lastList)
					listRef = GraphTraverser.getChild(listRef, 1);
				if (!lastPrevious)
					previousListRef = GraphTraverser.getChild(previousListRef, 1); 	
			}
			return new ClauseRelation(true,total);
		}
		return new ClauseRelation(false,false);
	}
	
	private void copyExistentRelations (ClauseRelation[][] matrix, int row, int column)
	{
		for (int i = 0; i < column; i++)
			matrix [row][i] = matrix [column][i];
	}
	private Node resolveCompoundPattern(Node compoundPattern)
	{
		Node leftChild = GraphTraverser.getChild(compoundPattern, 0);
		Node rightChild = GraphTraverser.getChild(compoundPattern, 1);
		return this.resolvePattern(leftChild,rightChild);
	}

	private Node resolvePattern(Node leftChild, Node rightChild)
	{
		if (leftChild.getData().getType() == NodeInfo.Type.CompoundPattern)
			leftChild = this.resolveCompoundPattern(leftChild);
		if (rightChild.getData().getType() == NodeInfo.Type.CompoundPattern)
			rightChild = this.resolveCompoundPattern(rightChild);

		NodeInfo.Type leftChildType = leftChild.getData().getType();
		NodeInfo.Type rightChildType = rightChild.getData().getType();
		if (leftChildType == NodeInfo.Type.Variable) //&& this.isFreshVariable(leftChild))
			return rightChild;
		if (rightChildType == NodeInfo.Type.Variable) //&& this.isFreshVariable(rightChild))
			return leftChild;
		if (leftChildType != rightChildType)
			return null;
		
		switch (leftChildType)
		{
			case Atom:
			case String:
			case Integer:
			case Char:
				if (leftChild.getData().getName().equals(rightChild.getData().getName()))
					return leftChild;
				return null;
			case TuplePattern:
				final int leftChildNumElems =GraphTraverser.getChildCount(leftChild);
				final int rightChildNumElems =GraphTraverser.getChildCount(rightChild);
				if (leftChildNumElems != rightChildNumElems)
					return null;
				
				// Create a tuple Node with the fusion of two expressions				
				final String tupleName = "{}";
				final NodeInfo tupleInfo = new NodeInfo(0, NodeInfo.Type.TuplePattern);
				final Node tupleNode = new Node(tupleName, tupleInfo);
				
				final List<Node> leftChildChildren = GraphTraverser.getChildren(leftChild, EdgeInfo.Type.Control);
				final List<Node> rightChildChildren = GraphTraverser.getChildren(rightChild, EdgeInfo.Type.Control);
				
				for (int index = 0; index < leftChildNumElems; index++)
				{
					final Node childLeft = leftChildChildren.get(index);
					final Node childRight = rightChildChildren.get(index);
					final Node result = this.resolvePattern(childLeft,childRight);
					this.linkNodes(tupleNode,result);
				}
				
				return tupleNode;
			case ListPattern:
				final String listName = "[]";
				final NodeInfo listInfo = new NodeInfo(0, NodeInfo.Type.ListPattern);
				final Node listNode = new Node(listName, listInfo);
				
				if (!this.hasMoreElements(leftChild) && !this.hasMoreElements(rightChild))
					return listNode;
				if (!this.hasMoreElements(leftChild) || !this.hasMoreElements(rightChild))
					return null;

				final Node leftHead = GraphTraverser.getChild(leftChild, 0);
				final Node rightHead = GraphTraverser.getChild(rightChild, 0);
				final Node resultHead = this.resolvePattern(leftHead, rightHead);
				if (resultHead == null)
					return null;
				this.linkNodes(listNode, resultHead);

				final Node leftTail = GraphTraverser.getChild(leftChild, 1);
				final Node rightTail = GraphTraverser.getChild(rightChild, 1);
				final Node resultTail = this.resolvePattern(leftTail, rightTail);
				if (resultTail == null)
					return null;
				this.linkNodes(listNode, resultTail);
				
				return listNode;
			default:
				return null;
		}
	}

	private void linkNodes(Node parent, Node child)
	{
		final String nodeName = child.getData().getName();
		final NodeInfo nodeInfo = new NodeInfo(0, child.getData().getType(),nodeName);
		child.setData(nodeInfo);
		
		final EdgeInfo edgeInfo = new EdgeInfo(EdgeInfo.Type.StructuralControl,null);
		final Edge e = new Edge(parent, child, 0, edgeInfo);
		parent.addEdge(e);
	}
/*******************************	
generateGuardGuardEdges v1.0 sin tener en cuenta la eliminación de cláusulas contenidas en otras
private void generateGuardGuardEdges(List<Node> expressionPatterns) 
{
	final int clauseSize = expressionPatterns.size();
	boolean[][] clauseRelations = this.diagonalMatrix(clauseSize);
	boolean[][] totalRelations = new boolean[clauseSize][clauseSize];
	if ( clauseSize != 1)
	{
		for (int comparePatternIndex = 1; comparePatternIndex < expressionPatterns.size(); comparePatternIndex++)
		{
			final Node comparePattern = expressionPatterns.get(comparePatternIndex);
			
			for (int previousPatternIndex = comparePatternIndex - 1 ; previousPatternIndex >= 0; previousPatternIndex--)
			{
				final Node previousPattern = expressionPatterns.get(previousPatternIndex);
				
				if (clauseRelations[comparePatternIndex][previousPatternIndex] == false && isRelated(comparePattern, previousPattern))
				{
					// DRAW EDGE
					final Node compareBody= GraphTraverser.getChild(GraphTraverser.getParent(comparePattern,EdgeInfo.Type.Control), 2);
					final Node previousGuard = GraphTraverser.getChild(GraphTraverser.getParent(previousPattern,EdgeInfo.Type.Control), 1);
					this.graph.addEdge(previousGuard, compareBody, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new EmptyConstraint()));
					this.copyExistentRelations(clauseRelations, comparePatternIndex, previousPatternIndex);
				}
			}
		}
	}
}

private boolean isRelated(Node compare, Node previous) // ADDED BY SERGIO FOR CLAUSES CONTAINED IN PREVIOUS CLAUSES
{
	if (previous.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(previous))
		return true; // THE PATTERN "compare" IS DEAD CODE
	
	switch(compare.getData().getType())
	{
		case Atom:
		case Integer:
		case String:
		case Char:
			switch (previous.getData().getType())
			{
				case Atom:
				case Integer:
				case String:
				case Char:
					if (compare.getData().getType() == previous.getData().getType() &&
						compare.getData().getName().equals(previous.getData().getName()))
						return true;
					return false;
				case Variable:
				default:
					return true;
			}
		case Variable:
			return true;
		case TuplePattern:
			return this.isRelatedTuple(compare,previous);
		case ListPattern:
			return this.isRelatedList(compare,previous);
		default:
			return false;
	
	}
}
private boolean isRelatedTuple(Node tuple, Node previousTuple)
{
	if (previousTuple.getData().getType() == NodeInfo.Type.Variable)
		return true;
	if (previousTuple.getData().getType() == NodeInfo.Type.TuplePattern)
	{
		if (GraphTraverser.getChildCount(tuple) != GraphTraverser.getChildCount(previousTuple))
			return false;
		else
		{
			final List<Node> tupleChildren = GraphTraverser.getChildren(tuple, EdgeInfo.Type.StructuralControl);
			final List<Node> previousTupleChildren = GraphTraverser.getChildren(previousTuple, EdgeInfo.Type.StructuralControl);
			for (int childIndex = 0; childIndex < GraphTraverser.getChildCount(tuple); childIndex++)
			{
				final Node tupleChild = tupleChildren.get(childIndex);
				final Node previousTupleChild = previousTupleChildren.get(childIndex);
				if (tupleChild.getData().getType() == previousTupleChild.getData().getType())
				{
					if (tupleChild.getData().getType() != NodeInfo.Type.Variable && 
						tupleChild.getData().getType() != NodeInfo.Type.TuplePattern &&
						tupleChild.getData().getType() != NodeInfo.Type.ListPattern &&
						!tupleChild.getData().getName().equals(previousTupleChild.getData().getName()))
						return false;
					if (!isRelated(tupleChild, previousTupleChild))
						return false;
				}
			}
			return true;
		}
	}
	else
		return false;
}
private boolean isRelatedList(Node list, Node previousList)
{
	if (previousList.getData().getType() == NodeInfo.Type.Variable)
		return true;
	if (previousList.getData().getType() == NodeInfo.Type.ListPattern 
			&& (GraphTraverser.getChildCount(previousList) == 0 || GraphTraverser.getChildCount(list) == 0))
		return false;
	
	if (previousList.getData().getType() == NodeInfo.Type.ListPattern)
	{
		Node listRef = list;
		Node previousListRef = previousList;
		boolean lastList = false;
		boolean lastPrevious = false;
		while(!lastList && !lastPrevious)
		{
			final Node listElem;
			final Node previousListElem;
			if (GraphTraverser.getChildCount(listRef) != 0)
				listElem = GraphTraverser.getChild(listRef, 0);
			else 
			{
				listElem = listRef;
				lastList = true;
			}	
			if (GraphTraverser.getChildCount(previousListRef) != 0)
				previousListElem = GraphTraverser.getChild(previousListRef, 0);
			else 
			{
				previousListElem = previousListRef;
				lastPrevious = true;
			}
			
			if (lastList && !lastPrevious)		// LA LISTA DE LA SEGUNDA CLAUSULA ACABA Y LA PRIMERA NO
			{	
				if (listElem.getName().equals("[]")) // LISTAS SIN COLA
				{
					return false;
				}
				else if (!(listElem.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(listElem)))
						return false;
			}
			else if (!lastList && lastPrevious) // LA LISTA DE LA PRIMERA CLAUSULA ACABA Y LA SEGUNDA NO
			{
				if (previousListElem.getName().equals("[]")) //LISTAS SIN COLA
				{
					return false;
				}
				else if (!(previousListElem.getData().getType() == NodeInfo.Type.Variable && 	// Previous es una variable
						(listElem.getData().getType() == NodeInfo.Type.Variable || listElem.getData().getType() == NodeInfo.Type.ListPattern) && // El elemento es una variable o una lista 
						GraphTraverser.getChildCount(GraphTraverser.getChild(listRef, 1)) == 0))	// Y es el ultimo elemento 
					return false;
			}
			else if (lastList && lastPrevious) // AMBAS LISTAS ACABAN
			{
				if (previousListElem.getName().equals("[]")) //LISTAS SIN COLA
				{
					if (!(listElem.getName().equals("[]") || listElem.getData().getType() == NodeInfo.Type.Variable))
						return false;
				}
				else if ( !(previousListElem.getData().getType() == listElem.getData().getType() &&
						    previousListElem.getData().getType() != NodeInfo.Type.Variable &&
						    previousListElem.getData().getName().equals(listElem.getData().getName())) )
					return false;
			}
			else
				if (!isRelated(listElem, previousListElem))
					return false;
	
			if (!lastList)
				listRef = GraphTraverser.getChild(listRef, 1);
			if (!lastPrevious)
				previousListRef = GraphTraverser.getChild(previousListRef, 1); 	
		}
	}
	return true;
}

private boolean[][] diagonalMatrix(int size)
{
	boolean[][] matrix = new boolean[size][size];
	for (int i = 0; i < size; i++)
		matrix[i][i] = true;
	return matrix;
	
}
private void copyExistentRelations (boolean[][] matrix, int row, int column)
{
	for (int i = 0; i <= column; i++)
		matrix [row][i] = matrix [column][i];
}

	
*******************************/	
	
	/************************************************************************/
	/************************** Input/Output edges **************************/
	/************************************************************************/

	private void generateInputOutputEdges()
	{
		final Node root = this.graph.getRootNode();
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
//arguments.remove(arguments.size() - 2); // ADDED BY SERGIO (ExceptionReturn)

		for (Node matchingClause : matchingClauses)
		{
			final List<Node> parameters = GraphTraverser.getChildren(matchingClause, EdgeInfo.Type.NormalControl);
			parameters.remove(parameters.size() - 1);

			this.graph.addEdge(function, matchingClause, 0, new EdgeInfo(EdgeInfo.Type.Input));
			for (int argumentIndex = 0; argumentIndex < arguments.size(); argumentIndex++)
			{
				final Node argument = arguments.get(argumentIndex);
				final Node parameter = parameters.get(argumentIndex);
				final List<Node> lasts = GraphTraverser.getLasts(argument);

				for (Node last : lasts)
					this.graph.addEdge(last, parameter, 0, new EdgeInfo(EdgeInfo.Type.Input));
			}
		}
	}
	private List<Node> getPossibleClauses(Node call)
	{
		final List<Node> arguments = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
		final Node function = arguments.remove(0);
		final NodeInfo functionInfo = function.getData();
		String functionName = functionInfo.getName();
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
				throw new RuntimeException("Call type not contempled: " + functionInfo.getType());
		}
	}
	private List<Node> getPossibleNormalClauses(String functionName, long functionArity)
	{
		final Node root = this.graph.getRootNode();
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
			final String parentName = parentInfo.getName();
			final long parentArity = parentInfo.getArity();
			if ((!functionName.equals(parentName) && !functionName.equals("_")) || functionArity != parentArity)
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
		parameters.remove(parameters.size() - 1); // ADDED BY SERGIO delete the nodes body and guards
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
			final List<Node> lasts = GraphTraverser.getLasts(callingFunction);

			for (Node last : lasts)
				this.graph.addEdge(last, callReturn, 0, new EdgeInfo(EdgeInfo.Type.Output));
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
		final Node root = this.graph.getRootNode();
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
			final Node exceptionReturnNode = (arguments.get(arguments.size() - 1).getData().getType() == NodeInfo.Type.ExceptionReturn) ?  arguments.remove(arguments.size() - 1) : null;
			final Node returnNode = arguments.remove(arguments.size() - 1);
			
			for (Node argumentNode : arguments)
			{
				final List<Node> lasts = GraphTraverser.getLasts(argumentNode);

				for (Node last : lasts)
				{
					this.graph.addEdge(last, returnNode, 0, new EdgeInfo(EdgeInfo.Type.Summary, starConstraint));
					if (exceptionReturnNode != null)
						this.graph.addEdge(last, exceptionReturnNode, 0, new EdgeInfo(EdgeInfo.Type.Summary, starConstraint));
				}
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
//Cronometro.empezar("1 - Dentro");
//Cronometro.empezar("1.1 - Empieza");
			final Work work = workList.next();
			final Node initialNode = work.getInitialNode();
			final Node currentNode = work.getCurrentNode();
			final Constraints constraints = work.getConstraints();
//Cronometro.terminar("1.1 - Empieza");

//int iid = initialNode.getData().getId();
//int cid = currentNode.getData().getId();
//if (cid == 63 && constraints.getSummaryType() == SummaryType.Exception)
//(System.out.println(constraints.getSummaryType());
//Cronometro.empezar("1.2 - IsFormalIn");
			if (this.isFormalIn(currentNode, initialNode))
			{
				// Formal in found
				final List<Node> nodesToContinue = this.createSummaryEdges(initialNode, currentNode, (SummaryConstraints) constraints);
				final List<Work> worksToContinue = this.getWorksToContinue(workList, nodesToContinue);

				workList.repending(worksToContinue);
			}
//Cronometro.terminar("1.2 - IsFormalIn");
//Cronometro.empezar("1.3 - Done");
			workList.done(work);
//Cronometro.terminar("1.3 - Done");

//Cronometro.empezar("1.4 - NewWorks");
			final List<Work> newWorks = this.getNewWorks(workList, work);
//Cronometro.terminar("1.4 - NewWorks");
//Cronometro.empezar("1.5 - AddNewWorks");
			for (Work newWork : newWorks)
				workList.add(newWork);
//Cronometro.terminar("1.5 - AddNewWorks");
//Cronometro.terminar("1 - Dentro");
//if (iteraciones % 10000 == 0)
	//Cronometro.mostrarCronometros();
		}
//Cronometro.mostrarCronometros();
	}
	private List<Work> getInitialWorks()
	{
		final List<Work> workList = new LinkedList<Work>();
		final List<Node> functions = this.graph.findNodesByData(null, new Comparator<NodeInfo>() {
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
				final List<Node> initialWorkNodes = GraphTraverser.getLasts(clause);

				for (Node initialWorkNode : initialWorkNodes)
					workList.add(new Work(initialWorkNode, initialWorkNode, new SummaryConstraints(), false, false));
			}
		}

		return workList;
	}
	private boolean isFormalIn(Node node, Node formalOutNode)
	{
		final NodeInfo.Type nodeType = node.getData().getType();
		if (nodeType == NodeInfo.Type.Guard || nodeType == NodeInfo.Type.Body)
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
	private List<Node> createSummaryEdges(Node formalOut, Node formalIn, SummaryConstraints constraints)
	{
		final List<Node> nodesToContinue = new LinkedList<Node>();

		final Node clause = GraphTraverser.getParent(formalIn, EdgeInfo.Type.NormalControl);
		final List<Node> functionCallers = GraphTraverser.getInputs(clause, GraphTraverser.Direction.Backwards);
		final List<Node> inputs = GraphTraverser.getInputs(formalIn, GraphTraverser.Direction.Backwards);
		final List<Node> outputs = GraphTraverser.getOutputs(formalOut, GraphTraverser.Direction.Forwards);
		final List<Node> exceptions = GraphTraverser.getExceptions(formalOut, GraphTraverser.Direction.Forwards);

		final SummaryType summaryType = constraints.getSummaryType();
		final GrammarType grammarType = (summaryType == SummaryType.Return) ? GrammarType.Value : GrammarType.Exception;
				
		final SummaryConstraint summaryConstraint = this.getSummaryConstraint(grammarType, formalIn);
		final List<Object> production0 = Arrays.asList(constraints.toArray());
		final List<Constraint> production = new LinkedList<Constraint>();

		for (Object element : production0)					// Se generan 2 arcos del return y exception return con la misma producción
			production.add((Constraint) element);

		this.graph.addProduction(grammarType, summaryConstraint, production);

		for (Node functionCaller : functionCallers)
		{
			final Node caller = GraphTraverser.getParent(functionCaller, EdgeInfo.Type.NormalControl);
			final List<Node> argumentNodes = this.getDescendantsNode(caller, inputs);
			final Node valueReturnNode = this.getDescendantsNode(caller, outputs).get(0);
			final Node exceptionReturnNode = exceptions.isEmpty() ? null : this.getDescendantsNode(caller, exceptions).get(0);
			final Node returnNode = (summaryType == SummaryType.Return) ? valueReturnNode : exceptionReturnNode;

			for (Node argumentNode : argumentNodes)
				this.graph.addEdge(argumentNode, returnNode, 0, new EdgeInfo(EdgeInfo.Type.Summary, summaryConstraint));
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
	private SummaryConstraint getSummaryConstraint(GrammarType grammarType, Node clause)
	{
		// TODO Get/Create the correct SummaryConstraint
		return new SummaryConstraint(this.graph.getGrammar(grammarType), clause);
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
Cronometro.empezar("1.4 - getNewWorks");
Cronometro.empezar("1.4.1 - slicingAlgorithm");
		final SlicingAlgorithm2 slicingAlgorithm = new SlicingAlgorithm2(this.graph, Phase.Summary);
Cronometro.terminar("1.4.1 - slicingAlgorithm");
Cronometro.empezar("1.4.2 - processWork");
		final List<Work> works = slicingAlgorithm.processWork(work, EdgeInfo.Type.Input, EdgeInfo.Type.Output);
Cronometro.terminar("1.4.2 - processWork");
Cronometro.empezar("1.4.3 - addWorks");
		final List<Work> newWorks = new LinkedList<Work>();

		for (Work work0 : works)
		{
Cronometro.empezar("1.4.3.1 - workList");
			if (workList.contains(work0))
{
Cronometro.terminar("1.4.3.1 - workList");
				continue;
}
Cronometro.terminar("1.4.3.1 - workList");
			newWorks.add(work0);
		}
Cronometro.terminar("1.4.3 - addWorks");
Cronometro.terminar("1.4 - getNewWorks");

		return newWorks;
	}

// ADDED BY SERGIO
	/************************************************************************/
	/*************************** Exception edges ****************************/
	/************************************************************************/
	// PERSONALIZED EXCEPTIONS 
	private void generateExceptionEdges()
	{
		final List<Node> nodes = this.graph.getNodes();
		boolean exception = false;
		for (int index = 0; index < nodes.size() && !exception; index++)
		{
			final NodeInfo.Type nodeType = nodes.get(index).getData().getType();
			if (nodeType == NodeInfo.Type.Try || nodeType == NodeInfo.Type.Catch)
				exception = true;
		}
		if (exception)
			this.generateExceptionEdges(nodes);
	}
	private void generateExceptionEdges(List<Node> nodes)
	{
		for (Node node : nodes)
		{
			if (node.getData().getType() == NodeInfo.Type.Throw)
			{
				this.generateThrowArgumentEdge(node);
				this.generateExceptionEdges(node, null);
			}
		}
		
		this.generateHighOrderEdges();
		
		for (Node node : nodes)
		{
			if (node.getData().getType() == NodeInfo.Type.CatchClause)
			{	
				final List<Node> catchClauses = GraphTraverser.getChildren(node, EdgeInfo.Type.Control);
				for (Node catchClause : catchClauses)
					this.generateExceptionPatternClauseEdges(catchClause);
			}
		}
	}
	private void generateExceptionEdges(Node node, Node exceptionExpression)
	{
		final Node captureNode = getCaptureFromThrow(node);
		if (captureNode != null)
		{
			if (!isThrownExpression(node, captureNode, exceptionExpression))
				this.generateThrowCatchEdges(node,captureNode, exceptionExpression);	
		}
		else
			this.generateThrowClauseEdges(node, exceptionExpression);
	}

	private void generateThrowArgumentEdge(Node _throw)
	{
		final Node throwExpression = GraphTraverser.getChild(_throw, 0);
		
		final String patternName = this.getExpressionName(throwExpression);
		this.graph.addEdge(throwExpression, _throw, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionArgumentConstraint(AccessConstraint.Operation.Remove, patternName)));
		
	}
	private void generateExceptionPatternClauseEdges(Node catchClause)
	{
		final Node pattern = GraphTraverser.getChild(catchClause, 0);
		if (pattern.getData().getType() == NodeInfo.Type.ExceptionPattern)
		{
			final Node exceptionClass = GraphTraverser.getChild(pattern, 0);
			final NodeInfo.Type exceptionClassType = exceptionClass.getData().getType();
			if ((exceptionClassType == NodeInfo.Type.Variable) || // && this.isFreshVariable(exceptionClass)) || -> Without dataflow we must assume that any variable can be the error atom 
				(exceptionClassType == NodeInfo.Type.Atom && exceptionClass.getData().getName().equals("error")))
					this.generateExceptionClauseEdges(pattern);
		}
		else 
		{
			final String patternName = this.getExpressionName(pattern);
			this.graph.addEdge(catchClause, pattern, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionArgumentConstraint(AccessConstraint.Operation.Add, patternName)));
		}
	}
	
	/************************************/
	/******* User exception edges *******/
	/************************************/
	private void generateThrowCatchEdges(Node _throw,Node _try, Node exceptionExpression)
	{
		final List<Node> catchClauses = this.getCatchClauses(_try);
		boolean unCatchedThrow = true;
		
		for (Node catchClause : catchClauses)
		{
			final Node catchExpression = GraphTraverser.getChild(catchClause, 0);
			if (unCatchedThrow && !this.isUnreachableThrow(_throw, _try) && !this.isThrownExpression(_throw, _try, catchExpression) )
			{
				final Node throwExpression = (exceptionExpression == null) ? GraphTraverser.getChild(_throw, 0) : exceptionExpression;
				
				if (this.isThrowCatch(catchClause)) 				// CLAUSES OF TYPE _ or throw:_ ->
					unCatchedThrow = this.generateUserExceptionEdges(_throw, catchClause, throwExpression, catchExpression);
				else 												// CLAUSES OF TYPE _:_ ->
					unCatchedThrow = this.generateErlangExceptionEdges(throwExpression, catchExpression);
			}
		}
		if (unCatchedThrow)
			this.generateThrowClauseEdges(_throw, (exceptionExpression == null) ? GraphTraverser.getChild(_throw, 0) : exceptionExpression);
	}
	
/*** DEPRECATED METHOD ***	
	private void generateThrowCatchEdges0(Node _throw,Node _try, Node exceptionExpression) // OLD VERSION
	{
		final List<Node> catchClauses = this.getCatchClauses(_try);
		boolean unCatchedThrow = true;
		
		for (Node catchClause : catchClauses)
		{
			final Node catchExpression = GraphTraverser.getChild(catchClause, 0);
			if (unCatchedThrow && !this.isUnreachableThrow(_throw, _try) && !this.isThrownExpression(_throw, _try, catchExpression) )
			{
				final Node throwExpression = (exceptionExpression == null) ? GraphTraverser.getChild(_throw, 0) : exceptionExpression;
				
				if (this.isThrowCatch(catchClause)) // CLAUSES OF TYPE _ or throw:_ ->
				{
					switch (throwExpression.getData().getType())
					{
						case Integer:
						case Atom:
						case Char:
						case String:
							if ((throwExpression.getData().getType() == catchExpression.getData().getType() && // SAME TYPE AND
									throwExpression.getData().getName().equals(catchExpression.getData().getName())) // SAME NAME
									|| (catchExpression.getData().getType() == NodeInfo.Type.Variable)) // CATCH PATTERN IS A VAR 
							{
								// Add edge throw -> catch clause (ENSURE CAPTURE)
								if (_throw.getData().getType() == NodeInfo.Type.Throw)
									this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
								else
								{
									//No se captura una call que tiene un throw(X) despues de una expression throw(X) previa a la call
									final Node tryCatch = GraphTraverser.getParent(GraphTraverser.getParent(catchClause, EdgeInfo.Type.Control),EdgeInfo.Type.Control);
									if(!isThrownExpression(_throw, tryCatch, exceptionExpression))
									{
										final String throwExpressionName = throwExpression.getData().getName();
										// Add edge throw -> catch clause (with constraint)
										this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, throwExpressionName) ));
									}
								}
								unCatchedThrow = false; 
							}
							if (catchExpression.getData().getType() == NodeInfo.Type.Variable && !this.isFreshVariable(catchExpression))
								unCatchedThrow = true;
							break;
							
						case Variable:
						case FunctionCall:
						case Operation:
	// TODO complete with the rest of expressions
							if (catchExpression.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(catchExpression))
							{
								// Add edge throw -> catch clause (ENSURE CAPTURE)
								if (_throw.getData().getType() == NodeInfo.Type.Throw)
									this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
								else
								{
									final String throwExpressionName = "*";
									// Add edge throw -> catch clause (with constraint)
									this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, throwExpressionName) ));
								}
								unCatchedThrow = false;
							}
							else 
								// Add edge throw -> catch clause
								if (_throw.getData().getType() == NodeInfo.Type.Throw)
								{
									//if (!this.isThrownExpression(_throw, _try, catchExpression))
									this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
								}
								else
								{
									// Si el patron es una variable instanciada uso ex*
									// Si el patron es un literal conocido uso exLITERAL
									
										final String throwExpressionName = this.getExpressionName(catchExpression);
										// Add edge throw -> catch clause (with constraint)
										this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, throwExpressionName)));
								}
							break;
					// TODO LISTS & TUPLES
						case TupleExpression:
						case ListExpression:
							final boolean[] matchResult = this.isMatching(throwExpression,catchExpression);
							if (matchResult[0])
							{
								if (_throw.getData().getType() == NodeInfo.Type.Throw)
								{
									this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
								}
								else{
									final String throwExpressionName = matchResult[1] ? this.getExpressionName(catchExpression) : "*";
									// Add edge throw -> catch clause (with constraint)
									this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, throwExpressionName) ));
								}
							}
							if (matchResult[1])
								unCatchedThrow = false;
							break;
						default:  
							break;
					}
				}
				else // CLAUSES OF TYPE _:_ ->
				{
					final List <Node> exceptionPatternExpressions = GraphTraverser.getChildren(catchExpression, EdgeInfo.Type.Control);
					final Node errorExpression = exceptionPatternExpressions.remove(0);
					final Node reasonExpression = exceptionPatternExpressions.remove(0);
					switch (throwExpression.getData().getType())
					{
						case Integer:
						case Atom:
						case Char:
						case String:	
							if ((errorExpression.getData().getType() == NodeInfo.Type.Variable && isFreshVariable(errorExpression)))
								if ((reasonExpression.getData().getType() == NodeInfo.Type.Variable && isFreshVariable(reasonExpression))||
									(reasonExpression.getData().getType() == throwExpression.getData().getType() && 
									reasonExpression.getData().getName().equals(throwExpression.getData().getName())))
										unCatchedThrow = false;
								
							break;
						case Variable:
						case FunctionCall:
						case Operation:
	// TODO complete with the rest of expressions
							if (errorExpression.getData().getType() == NodeInfo.Type.Variable && isFreshVariable(errorExpression))
								if (reasonExpression.getData().getType() == NodeInfo.Type.Variable && isFreshVariable(reasonExpression))
									unCatchedThrow = false;
							break;
	// TODO LISTS & TUPLES
						case ListExpression:
						case TupleExpression:
						default:
							break;
					}
				}
			}
		}
		if (unCatchedThrow)
		{
			this.generateThrowClauseEdges(_throw, (exceptionExpression == null) ? GraphTraverser.getChild(_throw, 0) : exceptionExpression);
		}
	}
***/
	
	private boolean generateUserExceptionEdges(Node _throw, Node catchClause, Node throwExpression, Node catchExpression)
	{
		boolean unCatchedThrow = true;
		switch (throwExpression.getData().getType())
		{
			case Integer:
			case Atom:
			case Char:
			case String:
				if (isSameExpression(throwExpression, catchExpression))  
				{
					// Add edge throw -> catch clause (ENSURE CAPTURE)
					if (_throw.getData().getType() == NodeInfo.Type.Throw)
						this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
					else // Es una call
					{
						// Add edge throw -> catch clause (with constraint)
						final String throwExpressionName = throwExpression.getData().getName();
						this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, throwExpressionName) ));
					}
					unCatchedThrow = false; 
				}
				if (catchExpression.getData().getType() == NodeInfo.Type.Variable && !this.isFreshVariable(catchExpression))
					unCatchedThrow = true;
				
				break;
			// TODO complete with the rest of expressions
			case Variable:
			case FunctionCall:
			case Operation:
				final String ConstraintName;
				if (catchExpression.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(catchExpression))
				{
					ConstraintName = "*";
					unCatchedThrow = false;
				}
				else
					ConstraintName = this.getExpressionName(catchExpression);
				
				if (_throw.getData().getType() == NodeInfo.Type.Throw)
					this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
				else
					this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, ConstraintName)));
				
				break;



/*****
				if (catchExpression.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(catchExpression))
				{
					// Add edge throw -> catch clause (ENSURE CAPTURE)
					
					{
						// Add edge throw -> catch clause (with constraint *)
						final String throwExpressionName = "*";
						this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, throwExpressionName) ));
					}
					unCatchedThrow = false;
				}
				else 
					// Add edge throw -> catch clause
					if (_throw.getData().getType() == NodeInfo.Type.Throw)
						this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
					else
					{
						// Si el patron es una variable instanciada uso ex*
						// Si el patron es un literal conocido uso exLITERAL
						
						// Add edge throw -> catch clause (with constraint)						
						final String catchExpressionName = this.getExpressionName(catchExpression);
						this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, catchExpressionName)));
					}
*****/
				
		// TODO LISTS & TUPLES
			case TupleExpression:
			case ListExpression:
				final boolean[] matchResult = this.isMatching(throwExpression,catchExpression);
				if (matchResult[0])
				{
					if (_throw.getData().getType() == NodeInfo.Type.Throw)
					{
						this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
					}
					else{
						final String throwExpressionName = matchResult[1] ? this.getExpressionName(catchExpression) : "*";
						// Add edge throw -> catch clause (with constraint)
						this.graph.addEdge(_throw, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, throwExpressionName) ));
					}
				}
				if (matchResult[1])
					unCatchedThrow = false;
				break;
			default:  
				break;
		}
		return unCatchedThrow;
	}
	
	private boolean generateErlangExceptionEdges(Node throwExpression, Node catchExpression)
	{
		boolean unCatchedThrow = true; 
		final List <Node> exceptionPatternExpressions = GraphTraverser.getChildren(catchExpression, EdgeInfo.Type.Control);
		final Node errorExpression = exceptionPatternExpressions.remove(0);
		final Node reasonExpression = exceptionPatternExpressions.remove(0);
		
		final NodeInfo.Type errorExpressionType =  errorExpression.getData().getType();
		final NodeInfo.Type reasonExpressionType =  reasonExpression.getData().getType();
		
		switch (throwExpression.getData().getType())
		{
			case Integer:
			case Atom:
			case Char:
			case String:	
				if ((errorExpressionType == NodeInfo.Type.Variable && isFreshVariable(errorExpression)))
					if ((reasonExpressionType == NodeInfo.Type.Variable && isFreshVariable(reasonExpression))||
						(reasonExpressionType == throwExpression.getData().getType() && 
						reasonExpression.getData().getName().equals(throwExpression.getData().getName())))
							unCatchedThrow = false;
					
				break;
			case Variable:
			case FunctionCall:
			case Operation:
// TODO complete with the rest of expressions
				if (errorExpressionType == NodeInfo.Type.Variable && isFreshVariable(errorExpression))
					if (reasonExpressionType == NodeInfo.Type.Variable && isFreshVariable(reasonExpression))
						unCatchedThrow = false;
				break;
// TODO LISTS & TUPLES
			case ListExpression:
			case TupleExpression:
			default:
				break;
		}
		return unCatchedThrow;
	}
	private boolean isSameExpression(Node throwExpression, Node catchExpression)
	{
		return isMatching(throwExpression,catchExpression)[1];	
	}
	
	private void generateThrowClauseEdges(Node node, Node exceptionExpression)
	{
		
		final Node body = this.getBodyFromThrow(node);
		if (!this.isUnreachableThrow(node, body) && !this.isThrownExpression(node, body, GraphTraverser.getChild(node, 0)))
		{
			// TODO REFACTORIZAR
			
			final Node root = this.graph.getRootNode();
			boolean foundAncestor = false;
			Node ancestor = node;
			while(ancestor != root && !foundAncestor)
			{
				ancestor = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
				final Node ancestorParent = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
				final NodeInfo.Type ancestorType = ancestor.getData().getType();
				final Node throwExpression;
				if(ancestorType == NodeInfo.Type.Catch)
				{
					final Node catch0 = GraphTraverser.getChild(ancestor, 0);
					this.graph.addEdge(catch0, ancestor, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionArgumentConstraint(AccessConstraint.Operation.Add,"*")));
					if (node.getData().getType() == NodeInfo.Type.Throw)
					{					
						// Add edge throw -> catch (without constraint)
						this.graph.addEdge(node, catch0, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
					}
					else 
					{
						throwExpression = exceptionExpression;
						final String throwExpressionName = this.getExpressionName(throwExpression);
						// Add edge throw -> try_catch (with constraint)
						this.graph.addEdge(node, catch0, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, throwExpressionName)));
					}
					foundAncestor = true;
				}
				else if (ancestorType == NodeInfo.Type.TryCatch || ancestorType == NodeInfo.Type.TryOf)
				{
					if (node.getData().getType() == NodeInfo.Type.Throw)
					{
						throwExpression = GraphTraverser.getChild(node, 0);
						final String throwExpressionName = this.getExpressionName(throwExpression);					
						// Add edge throw -> try_catch (with constraint)
						this.graph.addEdge(node, ancestor, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Remove, throwExpressionName)));
					}
					else 
					{
						throwExpression = exceptionExpression;
						final String throwExpressionName = this.getExpressionName(throwExpression);
						// Add edge throw -> try_catch (without constraint)
						this.graph.addEdge(node, ancestor, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null, throwExpressionName)));
					}
					this.generateExceptionEdges(ancestor,throwExpression);
					foundAncestor = true;
				}
				else if (ancestorType == NodeInfo.Type.Clause && ancestorParent.getData().getType() == NodeInfo.Type.Function)
				{
					if (node.getData().getType() == NodeInfo.Type.Throw)
					{
						throwExpression = GraphTraverser.getChild(node, 0);
						final String throwExpressionName = this.getExpressionName(throwExpression);
						
						// Add edge function clause -> function calls (with constraint)
						this.graph.addEdge(node, ancestor, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Remove, throwExpressionName)));
					}
					else 
					{
						throwExpression = exceptionExpression;
						final String throwExpressionName = this.getExpressionName(throwExpression);
						// Add edge function clause -> function calls  (without constraint) 
						this.graph.addEdge(node, ancestor, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null, throwExpressionName)));
					}
					this.generateClauseCallExceptionEdges(ancestor, throwExpression);
					foundAncestor = true;
				}
			}
			this.generateThrowDependenceEdges(node);
		}
	}
	private void generateClauseCallExceptionEdges(Node node, Node throwExpression)
	{
		final List<Edge> outComingEdges = node.getOutgoingEdges();
		for (Edge edge : outComingEdges)
		{
			if (edge.getData().getType() == EdgeInfo.Type.Output)
			{
				final Node callNode = GraphTraverser.getParent(edge.getTo(), EdgeInfo.Type.Control);
				final List<Node> callChildren = GraphTraverser.getChildren(callNode, EdgeInfo.Type.Control);
				
				final Node exceptionReturnNode;
				if (callChildren.get(callChildren.size()-1).getData().getType() != NodeInfo.Type.ExceptionReturn)
				{
					exceptionReturnNode = this.addExceptionReturnNode(callNode);
				}
				else
					exceptionReturnNode = callChildren.get(callChildren.size() - 1);
				
				// MIRAR SI ESE ARCO YA EXISTE
				boolean addEdge = true;
				final List<Edge> edges = node.getOutgoingEdges();
				
				for (Edge existentEdge : edges)
					if (existentEdge.getData().getType() == EdgeInfo.Type.Exception && existentEdge.getTo() == exceptionReturnNode && 
							existentEdge.getData().getConstraint().toString().equals("Ex"+throwExpression.getData().getName()))
					{	
						addEdge = false;
					}
					
				if (addEdge)
				{
					final String throwExpressionName = getExpressionName(throwExpression);
					this.graph.addEdge(node, exceptionReturnNode, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null, throwExpressionName)));
					this.graph.addEdge(exceptionReturnNode, callNode, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null, throwExpressionName)));
				}
				this.generateExceptionEdges(callNode, throwExpression);
			}
		}
	}
	private void generateThrowDependenceEdges(Node throwNode)
	{
		final List<Node> previousThrows = this.getPreviousThrows(throwNode);
		for (Node previousThrow : previousThrows)
		{
			if (previousThrow.getData().getType() == NodeInfo.Type.FunctionCall)
			{
				this.graph.addEdge(previousThrow, throwNode, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(ExceptionConstraint.Operation.Add, "*")));
			}
			else
				this.graph.addEdge(previousThrow, throwNode, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
		}
	}
	
	private void generateHighOrderEdges() 
	{
		final List<Node> funcCalls = this.getDescendants(this.graph.getRootNode(), NodeInfo.Type.FunctionCall);
		for (Node funcCall : funcCalls)
		{
			final List<Node> callChildren = GraphTraverser.getChildren(funcCall, EdgeInfo.Type.Control);
			final Node funcName = callChildren.remove(0);
			if (funcName.getData().getType() == NodeInfo.Type.Variable)
			{
				final Node lastNode = callChildren.get(callChildren.size()-1);
				final Node exceptionReturnNode = (lastNode.getData().getType() != NodeInfo.Type.ExceptionReturn) ? this.addExceptionReturnNode(funcCall) : lastNode;
				this.graph.addEdge(exceptionReturnNode, funcCall, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,"*")));
				
				// An Exception of a Variable is always captured by any pattern because it's value is not known and can be any thing
				final String fictitiousExceptionName = "*";
				final NodeInfo info = new NodeInfo(0, NodeInfo.Type.Variable); 	
				final Node fictitiousExceptionNode = new Node(fictitiousExceptionName, info);
				
				this.generateExceptionEdges(funcCall, fictitiousExceptionNode);
			}
		}
	}
	
	/************************************/
	/********* Internal Methods *********/
	/************************************/

	private Node getCaptureFromThrow(Node node)
	{
		final Node root = this.graph.getRootNode();
		Node ancestor = node;
		while(ancestor != root)
		{
			ancestor = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
			final NodeInfo.Type ancestorType = ancestor.getData().getType(); 
			if (ancestorType == NodeInfo.Type.Catch)
				return null;
			if (ancestorType == NodeInfo.Type.Try)
				return ancestor;
		}
		return null;
	}
	private List<Node> getCatchClauses(Node _try)
	{
		final Node tryCatch = GraphTraverser.getParent(_try, EdgeInfo.Type.Control);
		final List<Node> tryDescendants = GraphTraverser.getChildren(tryCatch, EdgeInfo.Type.Control);
		Node last = tryDescendants.get(tryDescendants.size()-1);
		if (last.getData().getType() == NodeInfo.Type.AfterTry)
			last = tryDescendants.get(tryDescendants.size()-2);
		return GraphTraverser.getChildren(last, EdgeInfo.Type.Control);
	}
	private boolean isThrowCatch(Node node)
	{
		final Node pattern = GraphTraverser.getChild(node, 0);
		if (pattern.getData().getType() != NodeInfo.Type.ExceptionPattern)
			return true;
		else 
			return false;
	}
	private boolean isFreshVariable(Node variable)
	{
		final String variableName = variable.getData().getName();
		if (variableName.equals("_"))
			return true;
		
		final List<Node> declaredVariables = this.getScope(variable, NodeInfo.Type.Variable);
		for (Node declaredVariable : declaredVariables)
		{
			if (declaredVariable.getData().getName().equals(variableName))
				return false;
		}
		return true;
	}
	private boolean hasAfter(List<Node> tryNodeChildren)
	{
		return tryNodeChildren.get(tryNodeChildren.size() - 1).getData().getType() == NodeInfo.Type.AfterTry;
	}
	
	private Node addExceptionReturnNode(Node parent)
	{
		final String exceptionReturnNodeName = "exceptionReturn";
		final NodeInfo info = new NodeInfo(this.graph.size(), NodeInfo.Type.ExceptionReturn);
		final Node exceptionReturnNode = new Node(exceptionReturnNodeName, info);
		final EdgeInfo edgeInfo = new EdgeInfo(EdgeInfo.Type.NormalControl);;
		this.graph.addNode(exceptionReturnNode);
		this.graph.addEdge(parent, exceptionReturnNode, 0, edgeInfo);
		
		this.addExceptionReturnEdges(exceptionReturnNode);
		
		return exceptionReturnNode;
	}
	private void addExceptionReturnEdges(Node exceptionReturn)
	{
		final Node parent = GraphTraverser.getParent(exceptionReturn, EdgeInfo.Type.Control);
		final Node funcName = GraphTraverser.getChild(parent, 0);
		this.graph.addEdge(funcName, exceptionReturn, 0, new EdgeInfo(EdgeInfo.Type.FlowDependence, new StarConstraint()));
	}
	
	private boolean isThrownExpression(Node throwNode, Node _try, Node exceptionExpressionNode) // Is a throw expression previously thrown?
	{
		exceptionExpressionNode = (exceptionExpressionNode == null) ? GraphTraverser.getChild(throwNode, 0) : exceptionExpressionNode;
		Node ancestor = throwNode;
		Node ancestorParent = GraphTraverser.getParent(ancestor, EdgeInfo.Type.NormalControl);
		while (ancestorParent != _try)
		{
			final NodeInfo.Type ancestorParentType = ancestorParent.getData().getType();
			if (ancestorParentType == NodeInfo.Type.Block || ancestorParentType == NodeInfo.Type.Body)
			{ 
				if (isPreviousThrow(exceptionExpressionNode,ancestor))
					 return true;
			}
			ancestor = ancestorParent;
			ancestorParent = GraphTraverser.getParent(ancestorParent, EdgeInfo.Type.NormalControl);
		}
		
		return this.isPreviousThrow(exceptionExpressionNode, ancestor);
	}
	private boolean isUnreachableThrow(Node throwNode, Node _try)	// Is a previous throw always executed before? Only going up
	{
		final Node tryParent = GraphTraverser.getParent(_try, EdgeInfo.Type.Control);
		Node ancestor = throwNode;
		Node ancestorParent = GraphTraverser.getParent(ancestor, EdgeInfo.Type.NormalControl);
		while (ancestorParent != tryParent)
		{
			final NodeInfo.Type ancestorParentType = ancestorParent.getData().getType();
			if (ancestorParentType == NodeInfo.Type.Block || ancestorParentType == NodeInfo.Type.Body)
			{ 
				if (isPreviousThrow(null, ancestor))
					 return true;
			}
			ancestor = ancestorParent;
			ancestorParent = GraphTraverser.getParent(ancestorParent, EdgeInfo.Type.NormalControl);
		}
		
		return false;
	}
	private boolean isPreviousThrow(Node exceptionExpression, Node expression) // Is a throw always executed inside a block or body context? Going down into the body 
	{
		final int throwExpressionIndex = GraphTraverser.getChildIndex(expression, EdgeInfo.Type.NormalControl);
		final Node expressionParent = GraphTraverser.getParent(expression, EdgeInfo.Type.NormalControl);
		
		for (int childIndex = 0; childIndex < throwExpressionIndex ; childIndex++)
		{
			final Node previousExpression = GraphTraverser.getChild(expressionParent, childIndex);
			final NodeInfo.Type previousExpressionType = previousExpression.getData().getType();
			if (previousExpressionType != NodeInfo.Type.Block &&
					previousExpressionType != NodeInfo.Type.Body &&
					previousExpressionType != NodeInfo.Type.Throw && 				
					previousExpressionType != NodeInfo.Type.FunctionCall)
				continue;
			else if (previousExpressionType == NodeInfo.Type.Throw)
			{
				if (this.throwThrowingExpression(exceptionExpression,previousExpression))
					return true;
				continue;
			}
			else if (previousExpressionType == NodeInfo.Type.Block || previousExpressionType == NodeInfo.Type.Body)
			{
				if (blockThrowingExpression(exceptionExpression, previousExpression))
					return true;
				continue;
			}
			else // call
			{
				if (callThrowingExpression(exceptionExpression, previousExpression))
					return true;
				continue;
			}
		}
		return false;
	}
	private boolean throwThrowingExpression(Node exceptionExpression, Node _throw) // Is a previous throw throwing the same expression I am evaluating?
	{
		final Node throwExpression = GraphTraverser.getChild(_throw, 0);
		final Node captureNode = this.getCaptureFromThrow(_throw);
		if (this.isCatchedException(_throw, captureNode))
			return false;
		if (exceptionExpression == null)
			return true;
		
		if (throwExpression.getData().getType() == exceptionExpression.getData().getType() && 
				this.getExpressionName(throwExpression).equals(this.getExpressionName(exceptionExpression)))
			return true;
		return false;
	}
	private boolean blockThrowingExpression(Node exceptionExpression, Node block) // Is a block throwing the same expression I am evaluating?
	{
		final List<Node> blockExpressions = GraphTraverser.getChildren(block, EdgeInfo.Type.NormalControl);
		
		for (Node blockExpression : blockExpressions)
		{
			final NodeInfo.Type blockExpressionType = blockExpression.getData().getType();
			if (blockExpression.getData().getType() == NodeInfo.Type.Throw)
			{
				if (exceptionExpression == null)
					return true;
				if (this.throwThrowingExpression(exceptionExpression,blockExpression))
					return true;
			}
			else if (blockExpressionType == NodeInfo.Type.Block || blockExpressionType == NodeInfo.Type.Body)
			{
				if (this.blockThrowingExpression(exceptionExpression, blockExpression))
					return true;
			}
			else if (blockExpressionType == NodeInfo.Type.FunctionCall)
				if (callThrowingExpression(exceptionExpression, blockExpression))
					return true;
		}
		return false;
	}
	private boolean callThrowingExpression(Node exceptionExpression, Node call) // Evaluate all the clauses of a function
	{
		final List<Node> callChildren = GraphTraverser.getChildren(call, EdgeInfo.Type.NormalControl);
		final Node lastNode = callChildren.get(callChildren.size() - 1);
		final Node returnNode = (lastNode.getData().getType() == NodeInfo.Type.Return) ? lastNode : callChildren.get(callChildren.size() - 2);
		
		final List<Edge> incomingEdges = returnNode.getIncomingEdges();
		for (Edge edge : incomingEdges)
		{
			if (edge.getData().getType() == EdgeInfo.Type.Output)
			{
				final Node clause = edge.getFrom();
				
				if (this.functionClauseThrowingExpression(exceptionExpression, clause))
					return true;
			}
		}
		return false;
	}
	private boolean functionClauseThrowingExpression(Node exceptionExpression, Node clause) // Is a call throwing the same expression I am evaluating?
	{
		final Node body = GraphTraverser.getChild(clause, GraphTraverser.getChildCount(clause) - 1);
		
		final List<Node> throwNodes = this.getDescendants(body, NodeInfo.Type.Throw);
		for (Node throwNode : throwNodes)
		{	
			final Node captureNode = getCaptureFromThrow(throwNode);
			if (!this.isUnreachableThrow(throwNode, body) && !this.isCatchedException(throwNode, captureNode))
				return true;
		}
		
		final List<Node> callNodes = this.getDescendants(body, NodeInfo.Type.FunctionCall);
		for (Node callNode : callNodes)
			if (this.callThrowingExpression(null, callNode) && !this.isCatchedException(callNode, this.getCaptureFromThrow(callNode)))
				return true;
		
		return false;
	}
	private boolean isCatchedException(Node throwNode, Node tryNode) // Is a throw expression captured in a function?
	{ 
		if (tryNode == null)
		{
			Node ancestor = throwNode;
			while(ancestor.getData().getType() != NodeInfo.Type.Function)
			{
				ancestor = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
				if (ancestor.getData().getType() == NodeInfo.Type.Catch)
					return true;
			}
		}
		else{
			final Node throwExpression = GraphTraverser.getChild(throwNode, 0);
			final List<Node> catchClauses = this.getCatchClauses(tryNode);
			for (Node catchClause : catchClauses)
			{
				final Node catchPattern = GraphTraverser.getChild(catchClause, 0);
				if (this.isMatching(throwExpression, catchPattern)[1])
					return true;
			}
			final Node captureNode = this.getCaptureFromThrow(tryNode);
			return this.isCatchedException(tryNode, captureNode);
		}
		return false;
	}
	
	private boolean[] isMatching(Node throwExpression, Node catchPattern)
	{
		final boolean[] result = new boolean[2];  // result[0] matching: true/false. result[1] fullyCaptured: true/false.
		final NodeInfo.Type throwType = throwExpression.getData().getType(); 
		final NodeInfo.Type catchType = catchPattern.getData().getType();
		
		switch(throwType){
			case Integer:
			case Atom:
			case Char:
			case String:
				if ((throwType == catchType &&
				throwExpression.getData().getName().equals(catchPattern.getData().getName())) 
				|| (catchType == NodeInfo.Type.Variable)) 
				{ 
					result[0] = true;
					result[1] = true;
				}
				break;
			case Variable:
				result[0] = true;
				if (catchType == NodeInfo.Type.Variable && this.isFreshVariable(catchPattern))
					result[1] = true;
				else
					result[1] = false;
				break;
			case TupleExpression:
				if (catchType == NodeInfo.Type.TuplePattern)
				{
					final boolean[] tupleResult = this.isMatchingTuple(throwExpression,catchPattern);
					result[0] = tupleResult[0];
					result[1] = tupleResult[1];
				}
				break;
			case ListExpression:
				if (catchType == NodeInfo.Type.ListPattern)
				{
					final boolean[] listResult = this.isMatchingList(throwExpression, catchPattern);
					result[0] = listResult[0];
					result[1] = listResult[1];
				}
				break;
			default:
				break;
		}
		return result;
	}
	private boolean[] isMatchingTuple(Node throwExpression, Node catchPattern)
	{
		if (GraphTraverser.getChildCount(throwExpression) != GraphTraverser.getChildCount(catchPattern))
			return new boolean[2];
		
		final boolean[] result = new boolean[2];
		result[0] = true;
		result[1] = true;
		
		final List<Node> throwTupleChildren = GraphTraverser.getChildren(throwExpression, EdgeInfo.Type.StructuralControl);
		final List<Node> catchTupleChildren = GraphTraverser.getChildren(catchPattern, EdgeInfo.Type.StructuralControl);
		for (int childIndex = 0; childIndex < GraphTraverser.getChildCount(throwExpression); childIndex++)
		{
			final Node throwTupleChild = throwTupleChildren.get(childIndex);
			final Node catchTupleChild = catchTupleChildren.get(childIndex);
			final NodeInfo.Type throwChildType = throwTupleChild.getData().getType();
			final NodeInfo.Type catchChildType = catchTupleChild.getData().getType();
			
			if (throwChildType == catchChildType &&
				catchChildType != NodeInfo.Type.Variable && 
				!catchTupleChild.getData().getName().equals(throwTupleChild.getData().getName()))
			{
				result[0] = false;
				result[1] = false;
				return result;
			}
			
			final boolean[] subMatching = this.isMatching(throwTupleChild,catchTupleChild);
			result[0] = result[0] & subMatching[0];
			result[1] = result[1] & subMatching[1];
		}
		return result;
	}
	private boolean[] isMatchingList(Node throwExpression, Node catchPattern)
	{
		final boolean[] result = new boolean[2];
		result[0] = true;
		result[1] = true;
		if (GraphTraverser.getChildCount(throwExpression) == 0 && GraphTraverser.getChildCount(catchPattern) == 0)
			return result;
		
		Node throwRef = throwExpression;
		Node catchRef = catchPattern;
		boolean lastThrow = false;
		boolean lastCatch = false;

		while(!lastThrow && !lastCatch)
		{
			// SELECCIONAR EL ELEMENTO CORRESPONDIENTE DE CADA LISTA MIRANDO SI ES O NO EL ULTIMO ELEMENTO
			
			//CASO NO CONTEMPLADO: [1,2|{1}]
			final Node throwElem;
			if (GraphTraverser.getChildCount(throwRef) != 0)		// LISTA THROW 
				throwElem = GraphTraverser.getChild(throwRef, 0);
			else 
			{
				throwElem = throwRef;
				lastThrow = true;
			}
			
			final Node catchElem;
			if (GraphTraverser.getChildCount(catchRef) != 0)		// LISTA CATCH
				catchElem = GraphTraverser.getChild(catchRef, 0);
			else 
			{
				catchElem = catchRef;
				lastCatch = true;
			}
		
			if (lastThrow && !lastCatch)		// LA LISTA DEL THROW ACABA Y LA DEL CATCH NO
			{	
				if (throwElem.getName().equals("[]")) // LISTA SIN COLA Ej: [1,2]
				{
					return new boolean[2];
				}
				else 
				{
					if (!(catchElem.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(catchElem) && this.isLastElement(catchRef)))
					{
						return new boolean[2];
					}
				}
			}
			else if (!lastThrow && lastCatch) // LA LISTA DEL CATCH ACABA Y LA DEL THROW NO
			{
				if (catchElem.getName().equals("[]")) 
				{
					return new boolean[2];
				}
				else if (catchElem.getData().getType() == NodeInfo.Type.Variable && !this.isFreshVariable(catchElem))
				{
					result[0] = result[0] & true;
					result[1] = result[1] & false;
				}
				else if (!(catchElem.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(catchElem))) // VARIABLE FRESCA COMO COLA
				{
					return new boolean[2];
				}	
			}
			else if (lastThrow && lastCatch) // THROW Y CATCH ACABAN A LA VEZ
			{
				if (throwElem.getName().equals("[]")) 
				{
					if (!(catchElem.getName().equals("[]") || 
						(catchElem.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(catchElem))))
					{
						return new boolean[2];
					}
				}
				else 
				{
					boolean[] matchingTail = this.isMatching(throwElem, catchElem);
					if (!(matchingTail[0]))
					{
						return new boolean[2];
					}
					else
					{
						result[0] = result[0] & matchingTail[0];
						result[1] = result[1] & matchingTail[1];
					}
				}
			}
			else	// ELEMENTOS NO FINALES DE NINGUNA LISTA
			{
				boolean[] matchingTail = this.isMatching(throwElem, catchElem);
				if (!(matchingTail[0]))
				{
					return new boolean[2];
				}
				else
				{
					result[0] = result[0] & matchingTail[0];
					result[1] = result[1] & matchingTail[1];
				}
			}
			if (!lastThrow)
				throwRef = GraphTraverser.getChild(throwRef, 1);
			if (!lastCatch)
				catchRef = GraphTraverser.getChild(catchRef, 1); 	
		}
		return result;
	}
	private boolean isLastElement(Node listElement)
	{
		final Node nextChild = GraphTraverser.getChild(listElement, 1);
		return GraphTraverser.getChildCount(nextChild) == 0 && nextChild.getName().equals("[]");
	}
	
	private String getExpressionName(Node throwExpression)
	{
		switch(throwExpression.getData().getType())
		{
			case Variable:
			case FunctionCall:
			case Operation:
				return "*";
			case TupleExpression:
			case TuplePattern:
				return getTupleConstraintName(throwExpression);
			case ListExpression:
			case ListPattern:
				return getListConstraintName(throwExpression);
			default:
				return throwExpression.getData().getName();
				
		}
	}
	private String getTupleConstraintName(Node tuple)
	{
		if (tuple.getData().getType() == NodeInfo.Type.TupleExpression)
		{
			String name = "{";
			final List<Node> children = GraphTraverser.getChildren(tuple, EdgeInfo.Type.StructuralControl);
			for (int childIndex=0; childIndex < children.size()-2; childIndex++)
			{
				String childName = children.get(childIndex).getData().getName();
				name += childName+",";
			}
			name += children.get(children.size()-1).getData().getName()+"}";
			return name;
		}
		else
		{
			String name = "{";
			final List<Node> children = GraphTraverser.getChildren(tuple, EdgeInfo.Type.StructuralControl);
			for (int childIndex=0; childIndex < children.size()-2; childIndex++)
			{
				Node child = children.get(childIndex);
				if (child.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(child))
					return "*";
					
				String childName = children.get(childIndex).getData().getName();
				name += childName+",";
			}
			// TREATING LAST ELEMENT
			Node child = children.get(children.size()-1);
			if (child.getData().getType() == NodeInfo.Type.Variable && this.isFreshVariable(child))
				return "*";
			
			name += children.get(children.size()-1).getData().getName()+"}";
			return name;
		}
	}
	private String getListConstraintName(Node list)
	{
		String name = "[";
		Node listElement = list; 
		while(hasMoreElements(listElement))
		{
			final Node elementValue = GraphTraverser.getChild(listElement, 0);
			String elementValueName = getExpressionName(elementValue);
			if (elementValueName.equals("*"))
				return "*";
			name += elementValueName;
			listElement = GraphTraverser.getChild(listElement, 1);
			if (hasMoreElements(listElement))
				name+=",";
				
		}	
		if (!listElement.getName().equals("[]"))
		{
			String elementValueName = getExpressionName(listElement);
			elementValueName = getExpressionName(listElement);
			if (elementValueName.equals("*"))
				return "*";
			name += "|"+elementValueName;
		}
		name += "]";
		return name;
	}
	private boolean hasMoreElements(Node list)
	{
		if (GraphTraverser.getChildCount(list) == 0)
			return false;
		final NodeInfo.Type elementType = list.getData().getType();
		if (elementType != NodeInfo.Type.ListPattern && elementType != NodeInfo.Type.ListExpression)
			return false;
		return true;
	}

	private Node getBodyFromThrow(Node throwNode)
	{
		Node ancestor = throwNode;
		Node ancestorParent = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
		
		while (ancestorParent.getData().getType() != NodeInfo.Type.Function)
		{
			ancestor = ancestorParent;
			ancestorParent = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
		}
		
		return GraphTraverser.getChild(ancestor, GraphTraverser.getChildCount(ancestor) - 1);
	}
	private List<Node> getPreviousThrows(Node throwNode) // Returns a list with the throws that may be executed before a throw statement
	{
		final Node body = this.getBodyFromThrow(throwNode); // Returns the body of the function clause that contains the throw node
		final Node bodyParent = GraphTraverser.getParent(body, EdgeInfo.Type.Control);
		final List<Node> previousThrows = new LinkedList<Node>();
		Node ancestor = throwNode;
		Node ancestorParent = GraphTraverser.getParent(ancestor, EdgeInfo.Type.Control);
		
		while (ancestorParent != bodyParent)
		{
			final int ancestorIndex = GraphTraverser.getChildIndex(ancestor, EdgeInfo.Type.Control);
			for (int index = 0; index < ancestorIndex; index++)
			{
				final Node previousChild = GraphTraverser.getChild(ancestorParent, index);
				
				final List<Node> previousThrowNodes = this.getDescendants(previousChild, NodeInfo.Type.Throw);
				for (Node previousThrowNode : previousThrowNodes)
				{	
					final Node captureNode = getCaptureFromThrow(previousThrowNode);
					if (!this.isUnreachableThrow(previousThrowNode, body) && 
						!this.isCatchedException(previousThrowNode, captureNode))
					{
						final Node throwNodeExpression = GraphTraverser.getChild(throwNode, 0);
						if (!this.throwThrowingExpression(throwNodeExpression, previousThrowNode))
							previousThrows.add(previousThrowNode);
					}
				}
				
				final List<Node> callNodes = this.getDescendants(previousChild, NodeInfo.Type.FunctionCall);
				for (Node callNode : callNodes)
					if (this.callThrowingExpression(null, callNode) && !this.isCatchedException(callNode, this.getCaptureFromThrow(callNode)))
						previousThrows.add(callNode);
			}
			ancestor = ancestorParent;
			ancestorParent = GraphTraverser.getParent(ancestorParent, EdgeInfo.Type.Control);
		}
		return previousThrows;
	}

	/************************************/
	/****** Erlang exception edges ******/
	/************************************/
	private void generateExceptionClauseEdges(Node node)
	{
		
		final Node exceptionPatternError = GraphTraverser.getChild(node, 0);
		if(!exceptionPatternError.getData().getName().equals("throw"))
		{	
			final Node catchClause = GraphTraverser.getParent(node, EdgeInfo.Type.Control);
			final Node tryCatchNode = GraphTraverser.getParent((GraphTraverser.getParent(catchClause, EdgeInfo.Type.Control)), EdgeInfo.Type.Control);
			final Node tryNode = GraphTraverser.getChild(tryCatchNode, 0);
			
			this.graph.addEdge(tryNode, catchClause, 0, new EdgeInfo(EdgeInfo.Type.Exception, new ExceptionConstraint(null,null)));
			generateExceptionGetAllEdges(tryNode);
		}
	}
	private void generateExceptionGetAllEdges(Node node)
	{
		final List<Node> children = GraphTraverser.getChildren(node, EdgeInfo.Type.Control);
		
		for (Node child : children)
		{
			this.graph.addEdge(child, node, 0, new EdgeInfo(EdgeInfo.Type.ExceptionGetAll));
			if (child.getData().getType() == NodeInfo.Type.FunctionCall)
			{
				 final Node functionName = GraphTraverser.getChild(child, 0);
				 final List<Node> functionCalls = this.getRelatedClauses(functionName);
				 for (Node functionCall : functionCalls)
				 {	
					this.graph.addEdge(functionCall, child, 0, new EdgeInfo(EdgeInfo.Type.ExceptionGetAll));
					this.generateExceptionGetAllEdges(functionCall);
				 }
			}
			this.generateExceptionGetAllEdges(child);
		}
	}
	private List<Node> getRelatedClauses(Node node)
	{
		final List<Edge> outComingEdges = node.getOutgoingEdges();
		final List<Node> functionClauses = new LinkedList<Node>();
		for (Edge outComingEdge : outComingEdges)
		{
			if (outComingEdge.getData().getType() == EdgeInfo.Type.Input)
				functionClauses.add(outComingEdge.getTo());
		}
		return functionClauses;
	}
}