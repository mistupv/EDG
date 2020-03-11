package upv.slicing.edg.visitor;

import org.jgrapht.Graph;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

public abstract class GenericVisitor<R,A> {
	protected final Graph graph;

	public GenericVisitor(Graph graph) {
		this.graph = graph;
	}

	public abstract R visit(Node n, A arg);
	public abstract R visitModule(Node n, A arg);
	public abstract R visitRoutine(Node n, A arg);
	public abstract R visitClause(Node n, A arg);
	public abstract R visitParameters(Node n, A arg);
	public abstract R visitList(Node n, A arg);
	public abstract R visitDataConstructor(Node n, A arg);
	public abstract R visitDataConstructorAccess(Node n, A arg);
	public abstract R visitFieldAccess(Node n, A arg);
	public abstract R visitBlock(Node n, A arg);
	public abstract R visitOperation(Node n, A arg);
	public abstract R visitEquality(Node n, A arg);
	public abstract R visitPattern(Node n, A arg);
	public abstract R visitIf(Node n, A arg);
	public abstract R visitCondition(Node n, A arg);
	public abstract R visitThen(Node n, A arg);
	public abstract R visitElse(Node n, A arg);
	public abstract R visitSwitch(Node n, A arg);
	public abstract R visitSelector(Node n, A arg);
	public abstract R visitCases(Node n, A arg);
	public abstract R visitCase(Node n, A arg);
	public abstract R visitDefaultCase(Node n, A arg);
	public abstract R visitSelectable(Node n, A arg);
	public abstract R visitCall(Node n, A arg);
	public abstract R visitCallee(Node n, A arg);
	public abstract R visitScope(Node n, A arg);
	public abstract R visitName(Node n, A arg);
	public abstract R visitArguments(Node n, A arg);
	public abstract R visitListComprehension(Node n, A arg);
	public abstract R visitRestrictions(Node n, A arg);
	public abstract R visitGenerator(Node n, A arg);
	public abstract R visitFilter(Node n, A arg);
	public abstract R visitValue(Node n, A arg);
	public abstract R visitLoop(Node n, A arg);
	public abstract R visitCLoop(Node n, A arg);
	public abstract R visitFLoop(Node n, A arg);
	public abstract R visitRLoop(Node n, A arg);
	public abstract R visitForeach(Node n, A arg);
	public abstract R visitIterator(Node n, A arg);
	public abstract R visitExHandler(Node n, A arg);
	public abstract R visitTry(Node n, A arg);
	public abstract R visitCatch(Node n, A arg);
	public abstract R visitFinally(Node n, A arg);
	public abstract R visitCatchClause(Node n, A arg);
	public abstract R visitThrow(Node n, A arg);
	public abstract R visitBody(Node n, A arg);
	public abstract R visitGuard(Node n, A arg);
	public abstract R visitExpression(Node n, A arg);
	public abstract R visitResult(Node n, A arg);
	public abstract R visitVariable(Node n, A arg);
	public abstract R visitLiteral(Node n, A arg);
	public abstract R visitReturn(Node n, A arg);
	public abstract R visitBreak(Node n, A arg);
	public abstract R visitContinue(Node n, A arg);
	public abstract R visitRoot(Node n, A arg);
	public abstract R visitInit(Node n, A arg);
	public abstract R visitUpdate(Node n, A arg);
	public abstract R visitTypeCheck(Node n, A arg);
	public abstract R visitTypeTransformation(Node n, A arg);
	public abstract R visitType(Node n, A arg);
	public abstract R visitReference(Node n, A arg);
	public abstract R visitLabel(Node n, A arg);
	public abstract R visitArgumentIn(Node n, A arg);
	public abstract R visitArgumentOut(Node n, A arg);
	public abstract R visitParameterIn(Node n, A arg);
	public abstract R visitParameterOut(Node n, A arg);
	public abstract R visitIndex(Node n, A arg);
}
