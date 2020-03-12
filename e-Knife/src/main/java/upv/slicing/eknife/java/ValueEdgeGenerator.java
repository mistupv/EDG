package upv.slicing.eknife.java;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

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
		final List<Node> routines = this.last.getNodes(Node.Type.Routine);

		for (Node routine : routines)
		{
			final List<Node> clauses = last.getChildren(routine);
			for (Node clause : clauses)
				this.last.addEdge(clause, routine, Edge.Type.Value);
		}
	}

	private void generateFunctionEdges()
	{
		final List<Node> returns = this.last.getNodes(Node.Type.Return);

		for (Node returnNode : returns)
		{
			final List<Node> returnChildren = last.getChildren(returnNode);
			if (returnChildren.isEmpty())
				continue;

			final Node returnChild = returnChildren.get(0);
			final String returnText = returnNode.getName();
			final int dstId = Integer.parseInt(returnText.substring(returnText.lastIndexOf(" ") + 1));
			final Node dstNode = this.last.getNode(dstId);
			final Node dstResult = last.getResult(dstNode);
			this.last.addEdge(returnChild, dstResult, Edge.Type.Value);
		}
	}

	private void generateRoutineCallEdgesJava()
	{
		final List<Node> calls = this.last.getNodes(Node.Type.Call);

		for (Node call : calls)
		{
			final Node callee = last.getChild(call, Node.Type.Callee);
			this.last.addEdge(callee, call, Edge.Type.Value);

			final Node nameNode = last.getChild(callee, Node.Type.Name);
			final Node name = last.getChild(nameNode, 0);
			this.last.addEdge(name, callee, Edge.Type.Value);

			final Node scopeNode = last.getChild(callee, Node.Type.Scope);
			final List<Node> scopeChildren = last.getChildren(scopeNode);
			if (!scopeChildren.isEmpty())
			{
				final Node scope = last.getChild(scopeNode, 0);
				this.last.addEdge(scope, name, Edge.Type.Value);
			}
		}
	}

	private void generateEqualityEdges()
	{
		final List<Node> equalities = this.last.getNodes(Node.Type.Equality);

		for (Node equality : equalities)
		{
			final Node pattern = last.getChild(equality, 0);
			final Node value = last.getChild(equality, 1);

			this.last.addEdge(equality, pattern, Edge.Type.Value);
			this.last.addEdge(value, equality, Edge.Type.Value);
		}
	}

	private void generateOperationEdges()
	{
		final List<Node> operations = this.last.getNodes(Node.Type.Operation);

		for (Node operation : operations)
		{
			final List<Node> operators = last.getChildren(operation);

			for (Node operator : operators)
				this.last.addEdge(operator, operation, Edge.Type.Value);
		}
	}

	private void generateTernaryEdges()
	{
		final List<Node> ternaries = this.last.getNodes(Node.Type.If);
		ternaries.removeIf(n -> !n.getInfo().isExpression());

		for (Node ternary : ternaries)
		{
			final Node thenNode = new EDG(this.last).getChild(ternary, Node.Type.Then);
			final Node elseNode = new EDG(this.last).getChild(ternary, Node.Type.Else);

			final Node thenExpr = this.last.getChild(thenNode, 0);
			final Node elseExpr = this.last.getChild(elseNode, 0);

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
		final List<Node> typeChecks = this.last.getNodes(Node.Type.TypeCheck);

		for (Node typeCheck : typeChecks)
		{
			final List<Node> operators = last.getChildren(typeCheck);

			for (Node operator : operators)
				this.last.addEdge(operator, typeCheck, Edge.Type.Value);
		}
	}

	private void generateDataConstructorEdges()
	{
		for (Node dataConstructor : this.last.getNodes(Node.Type.DataConstructor))
			for (final Node dataConstructorChild : last.getChildren(dataConstructor))
				this.last.addEdge(dataConstructorChild, dataConstructor, Edge.Type.Value);
	}

	private void generateDataConstructorAccessEdges()
	{
		final List<Node> dataConstructorAccesses = this.last.getNodes(Node.Type.DataConstructorAccess);
		final List<Node> fieldAccesses = this.last.getNodes(Node.Type.FieldAccess);
		dataConstructorAccesses.addAll(fieldAccesses);

		for (Node dataConstructorAccess : dataConstructorAccesses)
		{
			final Node dataConstructor = last.getChild(dataConstructorAccess, 0);
			final Node indexExpression = last.getChild(dataConstructorAccess, 1);

			this.last.addEdge(dataConstructor, dataConstructorAccess, Edge.Type.Value);
			this.last.addEdge(indexExpression, dataConstructorAccess, Edge.Type.Value);
		}
	}

	private void generateTypeTransformationEdges()
	{
		final List<Node> typeTransformations = this.last.getNodes(Node.Type.TypeTransformation);

		for (Node typeTransformation : typeTransformations)
		{
			final List<Node> operators = last.getChildren(typeTransformation);

			for (Node operator : operators)
				this.last.addEdge(operator, typeTransformation, Edge.Type.Value);
		}
	}

	private void generateEnclosedEdges()
	{
		final List<Node> enclosedExprs = this.last.getNodes(Node.Type.Enclosed);

		for (Node enclosed : enclosedExprs)
		{
			final Node expr = this.last.getChild(enclosed, 0);
			this.last.addEdge(expr, enclosed, Edge.Type.Value);
		}
	}
}
