package upv.slicing.eknife.java;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.traverser.EDGTraverser;
import upv.slicing.edg.traverser.LASTTraverser;

import java.util.List;

public class ValueEdgeGenerator {

	protected final LAST last;

	public ValueEdgeGenerator(LAST last)
	{
		this.last = last;
	}

	public void generate()    // GLOBAL VARIABLE ADAPTATION
	{
		this.generateRoutineEdges();
		this.generateFunctionEdges();
		this.generateRoutineCallEdgesJava();
		this.generateEqualityEdges();
		this.generateOperationEdges();
		this.generateTernaryEdges();
		this.generateDataConstructorEdges();
		this.generateDataConstructorAccessEdges();
		this.generateTypeEdges();
		this.generateEnclosedEdges();
	}

	private void generateRoutineEdges()
	{
		final List<Node> routines = LASTTraverser.getNodes(this.last, Node.Type.Routine);

		for (Node routine : routines)
		{
			final List<Node> clauses = LASTTraverser.getChildren(last, routine);
			for (Node clause : clauses)
				this.last.addEdge(clause, routine, Edge.Type.Value);
		}
	}

	private void generateFunctionEdges()
	{
		final List<Node> returns = LASTTraverser.getNodes(this.last, Node.Type.Return);

		for (Node returnNode : returns)
		{
			final List<Node> returnChildren = LASTTraverser.getChildren(last, returnNode);
			if (returnChildren.isEmpty())
				continue;

			final Node returnChild = returnChildren.get(0);
			final String returnText = returnNode.getName();
			final int dstId = Integer.parseInt(returnText.substring(returnText.lastIndexOf(" ") + 1));
			final Node dstNode = LASTTraverser.getNode(this.last, dstId);
			final Node dstResult = LASTTraverser.getResult(last, dstNode);
			this.last.addEdge(returnChild, dstResult, Edge.Type.Value);
		}
	}

	private void generateRoutineCallEdgesJava()
	{
		final List<Node> calls = LASTTraverser.getNodes(this.last, Node.Type.Call);

		for (Node call : calls)
		{
			final Node callee = LASTTraverser.getChild(last, call, Node.Type.Callee);
			this.last.addEdge(callee, call, Edge.Type.Value);

			final Node nameNode = LASTTraverser.getChild(last, callee, Node.Type.Name);
			final Node name = LASTTraverser.getChild(last, nameNode, 0);
			this.last.addEdge(name, callee, Edge.Type.Value);

			final Node scopeNode = LASTTraverser.getChild(last, callee, Node.Type.Scope);
			final List<Node> scopeChildren = LASTTraverser.getChildren(last, scopeNode);
			if (!scopeChildren.isEmpty())
			{
				final Node scope = LASTTraverser.getChild(last, scopeNode, 0);
				this.last.addEdge(scope, name, Edge.Type.Value);
			}
		}
	}

	private void generateEqualityEdges()
	{
		final List<Node> equalities = LASTTraverser.getNodes(this.last, Node.Type.Equality);

		for (Node equality : equalities)
		{
			final Node pattern = LASTTraverser.getChild(last, equality, 0);
			final Node value = LASTTraverser.getChild(last, equality, 1);

			this.last.addEdge(equality, pattern, Edge.Type.Value);
			this.last.addEdge(value, equality, Edge.Type.Value);
		}
	}

	private void generateOperationEdges()
	{
		final List<Node> operations = LASTTraverser.getNodes(this.last, Node.Type.Operation);

		for (Node operation : operations)
		{
			final List<Node> operators = LASTTraverser.getChildren(last, operation);

			for (Node operator : operators)
				this.last.addEdge(operator, operation, Edge.Type.Value);
		}
	}

	private void generateTernaryEdges()
	{
		final List<Node> ternaries = LASTTraverser.getNodes(this.last, Node.Type.If);
		ternaries.removeIf(n -> !n.getInfo().isExpression());

		for (Node ternary : ternaries)
		{
			final Node thenNode = EDGTraverser.getChild(new EDG(this.last), ternary, Node.Type.Then);
			final Node elseNode = EDGTraverser.getChild(new EDG(this.last), ternary, Node.Type.Else);

			final Node thenExpr = LASTTraverser.getChild(this.last, thenNode, 0);
			final Node elseExpr = LASTTraverser.getChild(this.last, elseNode, 0);

			this.last.addEdge(thenExpr, ternary, Edge.Type.Value);
			this.last.addEdge(elseExpr, ternary, Edge.Type.Value);
		}
	}

	private void generateTypeEdges()
	{
		generateTypeCheckEdges();
		generateTypeTransformationEdges();
	}

	private void generateTypeCheckEdges()
	{
		final List<Node> typeChecks = LASTTraverser.getNodes(this.last, Node.Type.TypeCheck);

		for (Node typeCheck : typeChecks)
		{
			final List<Node> operators = LASTTraverser.getChildren(last, typeCheck);

			for (Node operator : operators)
				this.last.addEdge(operator, typeCheck, Edge.Type.Value);
		}
	}

	private void generateDataConstructorEdges()
	{
		for (Node dataConstructor : LASTTraverser.getNodes(this.last, Node.Type.DataConstructor))
			for (final Node dataConstructorChild : LASTTraverser.getChildren(last, dataConstructor))
				this.last.addEdge(dataConstructorChild, dataConstructor, Edge.Type.Value);
	}

	private void generateDataConstructorAccessEdges()
	{
		final List<Node> dataConstructorAccesses = LASTTraverser
				.getNodes(this.last, Node.Type.DataConstructorAccess);
		final List<Node> fieldAccesses = LASTTraverser.getNodes(this.last, Node.Type.FieldAccess);
		dataConstructorAccesses.addAll(fieldAccesses);

		for (Node dataConstructorAccess : dataConstructorAccesses)
		{
			final Node dataConstructor = LASTTraverser.getChild(last, dataConstructorAccess, 0);
			final Node indexExpression = LASTTraverser.getChild(last, dataConstructorAccess, 1);

			this.last.addEdge(dataConstructor, dataConstructorAccess, Edge.Type.Value);
			this.last.addEdge(indexExpression, dataConstructorAccess, Edge.Type.Value);
		}
	}

	private void generateTypeTransformationEdges()
	{
		final List<Node> typeTransformations = LASTTraverser.getNodes(this.last, Node.Type.TypeTransformation);

		for (Node typeTransformation : typeTransformations)
		{
			final List<Node> operators = LASTTraverser.getChildren(last, typeTransformation);

			for (Node operator : operators)
				this.last.addEdge(operator, typeTransformation, Edge.Type.Value);
		}
	}

	private void generateEnclosedEdges()
	{
		final List<Node> enclosedExprs = LASTTraverser.getNodes(this.last, Node.Type.Enclosed);

		for (Node enclosed : enclosedExprs)
		{
			final Node expr = LASTTraverser.getChild(this.last, enclosed, 0);
			this.last.addEdge(expr, enclosed, Edge.Type.Value);
		}
	}
}
