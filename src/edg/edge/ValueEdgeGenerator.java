package edg.edge;

import java.util.List;

import edg.constraint.AccessConstraint;
import edg.constraint.EdgeConstraint;
import edg.constraint.ListComprehensionConstraint;
import edg.constraint.ListConstraint;
import edg.constraint.AsteriskConstraint;
import edg.constraint.DataConstructorConstraint;
import edg.graph.EDG;
import edg.graph.EdgeInfo;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.traverser.EDGTraverser;

public class ValueEdgeGenerator extends EdgeGenerator
{
	public ValueEdgeGenerator(EDG edg)
	{
		super(edg);
	}

	public void generate()
	{
		this.generateVariableEdges();
		this.generateLiteralEdges();
		this.generateEqualityEdges();
		this.generateDataConstructorEdges();
		this.generateListEdges();
		this.generateDataConstructorAccessEdges();
		this.generateOperationEdges();
		this.generateSwitchEdges();
		this.generateListComprehensionEdges();
		this.generateRoutineCallEdges();
		this.generateReturnEdges();
		this.generateRoutineEdges();
		this.generateTypeEdges(); // ADDED
	}

	private void generateVariableEdges()
	{
		final List<Node> variables = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Variable);

		for (Node variable : variables)
		{
			final Node variableResult = EDGTraverser.getSibling(variable, 1);

			this.edg.addEdge(variable, variableResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateLiteralEdges()
	{
		final List<Node> literals = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Literal);

		for (Node literal : literals)
		{
			final Node literalResult = EDGTraverser.getSibling(literal, 1);

			this.edg.addEdge(literal, literalResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateEqualityEdges()
	{
		final List<Node> equalities = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Equality);

		for (Node equality : equalities)
		{
			final Node equalityResult = EDGTraverser.getSibling(equality, 1);
			final Node pattern = EDGTraverser.getChild(equality, 0);
			final Node expression = EDGTraverser.getChild(equality, 1);
			final Node patternResult = EDGTraverser.getResult(pattern);
			final Node expressionResult = EDGTraverser.getResult(expression);

			this.edg.addEdge(expressionResult, equalityResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			this.edg.addEdge(expressionResult, patternResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateDataConstructorEdges()
	{
		final List<Node> dataConstructors = EDGTraverser.getNodes(this.edg, NodeInfo.Type.DataConstructor);

		for (Node dataConstructor : dataConstructors)
		{
			final boolean isPatternZone = EDGTraverser.isPatternZone(dataConstructor);
			final AccessConstraint.Operation operation = isPatternZone ? AccessConstraint.Operation.Add : AccessConstraint.Operation.Remove;
			final Node dataConstructorResult = EDGTraverser.getResult(dataConstructor);
			final List<Node> dataConstructorChildren = EDGTraverser.getChildren(dataConstructor);
			final int dataConstructorChildrenCount = dataConstructorChildren.size();

			final Node dataConstructorFrom = isPatternZone ? dataConstructorResult : dataConstructor;
			final Node dataConstructorTo = isPatternZone ? dataConstructor : dataConstructorResult;
			final DataConstructorConstraint dataConstructorConstraint = new DataConstructorConstraint(operation);
			this.edg.addEdge(dataConstructorFrom, dataConstructorTo, 0, new EdgeInfo(EdgeInfo.Type.Value, dataConstructorConstraint));

			for (int childIndex = 0; childIndex < dataConstructorChildrenCount; childIndex++)
			{
				final Node dataConstructorChild = dataConstructorChildren.get(childIndex);
				final Node dataConstructorChildResult = EDGTraverser.getResult(dataConstructorChild);
				final Node from = isPatternZone ? dataConstructorResult : dataConstructorChildResult;
				final Node to = isPatternZone ? dataConstructorChildResult : dataConstructorResult;
				final DataConstructorConstraint constraint = new DataConstructorConstraint(operation, childIndex + "");
				this.edg.addEdge(from, to, 0, new EdgeInfo(EdgeInfo.Type.Value, constraint));
			}
		}
	}
	private void generateListEdges()
	{
		final List<Node> lists = EDGTraverser.getNodes(this.edg, NodeInfo.Type.List);

		for (Node list : lists)
		{
			final boolean isPatternZone = EDGTraverser.isPatternZone(list);
			final AccessConstraint.Operation operation = isPatternZone ? AccessConstraint.Operation.Add : AccessConstraint.Operation.Remove;
			final Node listResult = EDGTraverser.getResult(list);

			final Node listFrom = isPatternZone ? listResult : list;
			final Node listTo = isPatternZone ? list : listResult;
			final ListConstraint listConstraint = new ListConstraint(operation);
			this.edg.addEdge(listFrom, listTo, 0, new EdgeInfo(EdgeInfo.Type.Value, listConstraint));

			final List<Node> listChildren = EDGTraverser.getChildren(list);
			if (listChildren.isEmpty())
				continue;

			final Node head = listChildren.get(0);
			final Node headResult = EDGTraverser.getResult(head);
			final Node headFrom = isPatternZone ? listResult : headResult;
			final Node headTo = isPatternZone ? headResult : listResult;
			final ListConstraint headConstraint = new ListConstraint(operation, ListConstraint.Position.H);
			this.edg.addEdge(headFrom, headTo, 0, new EdgeInfo(EdgeInfo.Type.Value, headConstraint));

			final Node tail = listChildren.get(1);
			final Node tailResult = EDGTraverser.getResult(tail);
			final Node tailFrom = isPatternZone ? listResult : tailResult;
			final Node tailTo = isPatternZone ? tailResult : listResult;
			final ListConstraint tailConstraint = new ListConstraint(operation, ListConstraint.Position.T);
			this.edg.addEdge(tailFrom, tailTo, 0, new EdgeInfo(EdgeInfo.Type.Value, tailConstraint));
		}
	}
	private void generateDataConstructorAccessEdges()
	{
		final List<Node> dataConstructorAccesses = EDGTraverser.getNodes(this.edg, NodeInfo.Type.DataConstructorAccess);

		for (Node dataConstructorAccess : dataConstructorAccesses)
		{
			final Node dataConstructorAccessResult = EDGTraverser.getResult(dataConstructorAccess);
			final Node dataConstructor = EDGTraverser.getChild(dataConstructorAccess, 0);
			final Node dataConstructorResult = EDGTraverser.getResult(dataConstructor);
			final Node indexExpression = EDGTraverser.getChild(dataConstructorAccess, 1);
			final Node index = EDGTraverser.getChild(indexExpression, 0);
			final Node indexResult = EDGTraverser.getResult(index);
			final String indexValue = index.getData().getType() == NodeInfo.Type.Literal ? index.getData().getName() : "*";
			final EdgeConstraint constraint = new DataConstructorConstraint(AccessConstraint.Operation.Add, indexValue);

			this.edg.addEdge(dataConstructorResult, dataConstructorAccessResult, 0, new EdgeInfo(EdgeInfo.Type.Value, constraint));
			this.edg.addEdge(indexResult, dataConstructorAccessResult, 0, new EdgeInfo(EdgeInfo.Type.Value, AsteriskConstraint.getConstraint()));
		}
	}
	private void generateOperationEdges()
	{
		final List<Node> operations = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Operation);

		for (Node operation : operations)
		{
			final Node operationResult = EDGTraverser.getResult(operation);
			final List<Node> operators = EDGTraverser.getChildren(operation);

			for (Node operator : operators)
			{
				final Node operatorResult = EDGTraverser.getResult(operator);
				this.edg.addEdge(operatorResult, operationResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}
		}
	}
	private void generateSwitchEdges()
	{
		final List<Node> switches = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Switch);

		for (Node _switch : switches)
		{
			final Node selectorNode = EDGTraverser.getChild(_switch, 0);
			final Node casesNode = EDGTraverser.getChild(_switch, 1);
			final List<Node> selectors = EDGTraverser.getChildren(selectorNode);
			final List<Node> cases = EDGTraverser.getChildren(casesNode);

			for (Node _case : cases)
			{
				final Node selectableNode = EDGTraverser.getChild(_case, 0);
				final List<Node> selectables = EDGTraverser.getChildren(selectableNode);
				if (selectors.isEmpty() || selectables.isEmpty())
					continue;

				final Node selector = selectors.get(0);
				final Node selectable = selectables.get(0);
				final Node selectorResult = EDGTraverser.getResult(selector);
				final Node selectableResult = EDGTraverser.getResult(selectable);
				this.edg.addEdge(selectorResult, selectableResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}
		}
	}
	private void generateListComprehensionEdges()
	{
		final List<Node> listComprehensionNodes = EDGTraverser.getNodes(this.edg, NodeInfo.Type.ListComprehension);
		final EdgeConstraint negativeConstraint = new ListComprehensionConstraint(ListComprehensionConstraint.Operation.Remove);
		for (Node listComprehensionNode : listComprehensionNodes)
		{
			final Node lcResult = EDGTraverser.getResult(listComprehensionNode);
			final Node value = EDGTraverser.getChild(listComprehensionNode, 1);
			final Node valueExpression = EDGTraverser.getChild(value, 0);
			final Node valueResult = EDGTraverser.getResult(valueExpression);
			this.edg.addEdge(valueResult, lcResult, 0, new EdgeInfo(EdgeInfo.Type.Value, negativeConstraint));
		}

		final List<Node> generators = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Generator);
		final EdgeConstraint positiveConstraint = new ListComprehensionConstraint(ListComprehensionConstraint.Operation.Add);
		for (Node generator : generators)
		{
			final Node pattern = EDGTraverser.getChild(generator, 0);
			final Node expression = EDGTraverser.getChild(generator, 1);
			final Node patternResult = EDGTraverser.getResult(pattern);
			final Node expressionResult = EDGTraverser.getResult(expression);
			this.edg.addEdge(expressionResult, patternResult, 0, new EdgeInfo(EdgeInfo.Type.Value, positiveConstraint));
		}
	}
	private void generateRoutineCallEdges()
	{
		final List<Node> calls = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Call);

		for (Node call : calls)
		{
			final Node callResult = EDGTraverser.getResult(call);
			final Node callee = EDGTraverser.getChild(call, 0);
			final Node calleeResult = EDGTraverser.getChild(callee, 2);
			this.edg.addEdge(calleeResult, callResult, 0, new EdgeInfo(EdgeInfo.Type.Value, AsteriskConstraint.getConstraint()));

			final Node scopeNode = EDGTraverser.getChild(callee, 0);
			final List<Node> scopeChildren = EDGTraverser.getChildren(scopeNode);
			if (!scopeChildren.isEmpty())
			{
				final Node scope = EDGTraverser.getChild(scopeNode, 0);
				final Node scopeResult = EDGTraverser.getResult(scope);
				this.edg.addEdge(scopeResult, calleeResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
				final List<Node> inputs = EDGTraverser.getInputs(calleeResult, EDGTraverser.Direction.Forwards);
				if (inputs.isEmpty())
					this.edg.addEdge(callResult, scopeResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}	
			
			final Node nameNode = EDGTraverser.getChild(callee, 1);
			final Node name = EDGTraverser.getChild(nameNode, 0);
			final Node nameResult = EDGTraverser.getResult(name);
			this.edg.addEdge(nameResult, calleeResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateReturnEdges()
	{
		final List<Node> returns = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Return);

		for (Node returnNode : returns)
		{
			final List<Node> returnChildren = EDGTraverser.getChildren(returnNode);
			if (returnChildren.isEmpty())
				continue;

			final Node returnChild = returnChildren.get(0);
			final Node returnChildResult = EDGTraverser.getResult(returnChild);
			final String returnText = returnNode.getData().getName();
			final int dstId = Integer.parseInt(returnText.substring(returnText.lastIndexOf(" ") + 1));
			final Node dstNode = EDGTraverser.getNode(this.edg, dstId);
			final Node dstResult = EDGTraverser.getResult(dstNode);
			this.edg.addEdge(returnChildResult, dstResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateRoutineEdges()
	{
		final List<Node> routines = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Routine);

		for (Node routine : routines)
		{
			final Node routineParent = EDGTraverser.getParent(routine);
			final NodeInfo.Type routineParentType = routineParent.getData().getType();
			if (routineParentType == NodeInfo.Type.Module)
				continue;
			final Node routineResult = EDGTraverser.getChild(routineParent, 1);
			final List<Node> clauses = EDGTraverser.getChildren(routine);
			for (Node clause : clauses)
			{
				final Node clauseResult = EDGTraverser.getChild(clause, 3);
				this.edg.addEdge(clauseResult, routineResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}
		}
	}

	// TYPES
	private void generateTypeEdges()
	{
		generateRawTypeEdges();
		generateTypeCheckEdges();
		generateTypeTransformationEdges();
	}
	private void generateRawTypeEdges()
	{
		final List<Node> types = EDGTraverser.getNodes(this.edg, NodeInfo.Type.Type);

		for (Node type : types)
		{
			final Node typeResult = EDGTraverser.getSibling(type, 1);

			this.edg.addEdge(type, typeResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}

	}
	private void generateTypeCheckEdges()
	{
		final List<Node> typeChecks = EDGTraverser.getNodes(this.edg, NodeInfo.Type.TypeCheck);
		
		for (Node typeCheck : typeChecks)
		{
			final Node typeCheckResult = EDGTraverser.getResult(typeCheck);
			final List<Node> operators = EDGTraverser.getChildren(typeCheck);
			
			for (Node operator : operators)
			{
				final Node operatorResult = EDGTraverser.getResult(operator);
				this.edg.addEdge(operatorResult, typeCheckResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}
		}
	}
	private void generateTypeTransformationEdges()
	{
		final List<Node> typeTrans = EDGTraverser.getNodes(this.edg, NodeInfo.Type.TypeTransformation);
		
		for (Node typeTran : typeTrans)
		{
			final Node typeTranResult = EDGTraverser.getResult(typeTran);
			final List<Node> operators = EDGTraverser.getChildren(typeTran);
			
			for (Node operator : operators)
			{
				final Node operatorResult = EDGTraverser.getResult(operator);
				this.edg.addEdge(operatorResult, typeTranResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}
		}
	}
}