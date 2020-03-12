package upv.slicing.edg.visitor;

import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.traverser.LASTTraverser;

public class VoidVisitor<A> {
	protected final LAST graph;

	public VoidVisitor(LAST graph) {
		this.graph = graph;
	}

	private void visitChildren(Node n, A arg)
	{
		for (Node child : LASTTraverser.getChildren(graph, n))
			child.accept(this, arg);
	}

	public void visitModule(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitRoutine(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitClause(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitParameters(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitList(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitDataConstructor(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitDataConstructorAccess(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitFieldAccess(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitBlock(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitOperation(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitEquality(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitPattern(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitEnclosed(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitIf(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitCondition(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitThen(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitElse(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitSwitch(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitSelector(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitCases(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitCase(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitDefaultCase(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitSelectable(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitCall(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitCallee(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitScope(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitName(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitArguments(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitListComprehension(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitRestrictions(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitGenerator(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitFilter(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitValue(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitLoop(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitCLoop(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitFLoop(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitRLoop(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitForeach(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitIterator(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitExHandler(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitTry(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitCatch(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitFinally(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitCatchClause(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitThrow(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitBody(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitGuard(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitExpression(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitResult(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitVariable(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitLiteral(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitReturn(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitBreak(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitContinue(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitRoot(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitInit(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitUpdate(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitTypeCheck(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitTypeTransformation(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitType(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitReference(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitLabel(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitArgumentIn(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitArgumentOut(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitParameterIn(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitParameterOut(Node n, A arg)
	{
		visitChildren(n, arg);
	}

	public void visitIndex(Node n, A arg)
	{
		visitChildren(n, arg);
	}
}
