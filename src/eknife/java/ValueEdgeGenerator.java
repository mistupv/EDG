package eknife.java;

import java.util.List;

import edg.constraint.AccessConstraint;
import edg.constraint.AddNodeConstraint;
import edg.constraint.EdgeConstraint;
import edg.constraint.IgnoreEdgeConstraint;
import edg.constraint.ListComprehensionConstraint;
import edg.constraint.ListConstraint;
import edg.constraint.NodeConstraint;
import edg.constraint.AsteriskConstraint;
import edg.constraint.DataConstructorConstraint;
import edg.graph.EdgeInfo;
import edg.graph.LAST;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.traverser.LASTTraverser;

public class ValueEdgeGenerator 
{
	
	protected final LAST last;

	public ValueEdgeGenerator(LAST last)
	{
		this.last = last;
	}
	
	public void generate()	// GLOBAL VARIABLE ADAPTATION
	{
		this.generateEqualityEdges();
		this.generateDataConstructorEdges();
		this.generateDataConstructorAccessEdges();
		this.generateOperationEdges();
		this.generateRoutineCallEdgesJava(); 
		this.generateFunctionEdges();
		this.generateRoutineEdges();
		this.generateTypeEdges(); 
	}

	private void generateEqualityEdges()
	{
		final List<Node> equalities = LASTTraverser.getNodes(this.last, NodeInfo.Type.Equality);

		for (Node equality : equalities)
		{
			final Node pattern = LASTTraverser.getChild(equality, 0);
			final Node expression = LASTTraverser.getChild(equality, 1);
		
			this.last.addEdge(expression, pattern, 0, new EdgeInfo(EdgeInfo.Type.Value));
			this.last.addEdge(expression, equality, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateDataConstructorEdges()
	{
		final List<Node> dataConstructors = LASTTraverser.getNodes(this.last, NodeInfo.Type.DataConstructor);

		for (Node dataConstructor : dataConstructors)
		{
			final List<Node> dataConstructorChildren = LASTTraverser.getChildren(dataConstructor);
			final int dataConstructorChildrenCount = dataConstructorChildren.size();
			
			for (int childIndex = 0; childIndex < dataConstructorChildrenCount; childIndex++)
			{
				final Node dataConstructorChild = dataConstructorChildren.get(childIndex);
				this.last.addEdge(dataConstructorChild, dataConstructor, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}
		}
	}
	private void generateDataConstructorAccessEdges()
	{
		final List<Node> dataConstructorAccesses = LASTTraverser.getNodes(this.last, NodeInfo.Type.DataConstructorAccess);
		final List<Node> fieldAccesses = LASTTraverser.getNodes(this.last, NodeInfo.Type.FieldAccess);
		dataConstructorAccesses.addAll(fieldAccesses);

		for (Node dataConstructorAccess : dataConstructorAccesses)
		{
			final Node dataConstructor = LASTTraverser.getChild(dataConstructorAccess, 0);
			final Node indexExpression = LASTTraverser.getChild(dataConstructorAccess, 1);
			
			this.last.addEdge(dataConstructor, dataConstructorAccess, 0, new EdgeInfo(EdgeInfo.Type.Value));
			this.last.addEdge(indexExpression, dataConstructorAccess, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateOperationEdges()
	{
		final List<Node> operations = LASTTraverser.getNodes(this.last, NodeInfo.Type.Operation);

		for (Node operation : operations)
		{
			final List<Node> operators = LASTTraverser.getChildren(operation);

			for (Node operator : operators)
				this.last.addEdge(operator, operation, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateRoutineCallEdgesJava()
	{
		final List<Node> calls = LASTTraverser.getNodes(this.last, NodeInfo.Type.Call);

		for (Node call : calls)
		{
			final Node callee = LASTTraverser.getChild(call, 0);
			final Node calleeResult = LASTTraverser.getResult(callee);
			this.last.addEdge(calleeResult, call, 0, new EdgeInfo(EdgeInfo.Type.Value));	
			
			final Node nameNode = LASTTraverser.getChild(callee, 1);
			final Node name = LASTTraverser.getChild(nameNode, 0);
			this.last.addEdge(name, calleeResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			
			final Node scopeNode = LASTTraverser.getChild(callee, 0);
			final List<Node> scopeChildren = LASTTraverser.getChildren(scopeNode);
			if (!scopeChildren.isEmpty())
			{
				final Node scope = LASTTraverser.getChild(scopeNode, 0);
				this.last.addEdge(scope, name, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}
		}
	}
	private void generateFunctionEdges()
	{
		final List<Node> returns = LASTTraverser.getNodes(this.last, NodeInfo.Type.Return);

		for (Node returnNode : returns)
		{
			final List<Node> returnChildren = LASTTraverser.getChildren(returnNode);
			if (returnChildren.isEmpty())
				continue;

			final Node returnChild = returnChildren.get(0);
			final String returnText = returnNode.getData().getName();
			final int dstId = Integer.parseInt(returnText.substring(returnText.lastIndexOf(" ") + 1));
			final Node dstNode = LASTTraverser.getNode(this.last, dstId);
			final Node dstResult = LASTTraverser.getResult(dstNode);
			this.last.addEdge(returnChild, dstResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateRoutineEdges()
	{
		final List<Node> routines = LASTTraverser.getNodes(this.last, NodeInfo.Type.Routine);

		for (Node routine : routines)
		{
			final Node routineParent = LASTTraverser.getParent(routine);
			final NodeInfo.Type routineParentType = routineParent.getData().getType();
			if (routineParentType == NodeInfo.Type.Module)
				continue;
			
			final List<Node> clauses = LASTTraverser.getChildren(routine);
			for (Node clause : clauses)
			{
				final Node clauseResult = LASTTraverser.getChild(clause, 3);
				this.last.addEdge(routine, clauseResult, 0, new EdgeInfo(EdgeInfo.Type.Value));
			}
		}
	}

	// TYPES
	private void generateTypeEdges()
	{
//		generateRawTypeEdges();
		generateTypeCheckEdges();
		generateTypeTransformationEdges();
	}
	private void generateTypeCheckEdges()
	{
		final List<Node> typeChecks = LASTTraverser.getNodes(this.last, NodeInfo.Type.TypeCheck);
		
		for (Node typeCheck : typeChecks)
		{
			final List<Node> operators = LASTTraverser.getChildren(typeCheck);
			
			for (Node operator : operators)
				this.last.addEdge(operator, typeCheck, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
	private void generateTypeTransformationEdges()
	{
		final List<Node> typeTransformations = LASTTraverser.getNodes(this.last, NodeInfo.Type.TypeTransformation);
		
		for (Node typeTransformation : typeTransformations)
		{
			final List<Node> operators = LASTTraverser.getChildren(typeTransformation);
			
			for (Node operator : operators)
				this.last.addEdge(operator, typeTransformation, 0, new EdgeInfo(EdgeInfo.Type.Value));
		}
	}
}