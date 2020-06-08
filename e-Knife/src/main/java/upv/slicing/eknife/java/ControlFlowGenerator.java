package upv.slicing.eknife.java;

import upv.slicing.edg.edge.Generator;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.visitor.VoidVisitor;

import java.util.*;

public class ControlFlowGenerator extends VoidVisitor<Void> implements Generator {
	protected static final int INFINITY = -1;

	protected final Set<Node> hangingNodes = new HashSet<>();
	protected final Set<Node> returnSet = new HashSet<>();
	protected final Deque<Set<Node>> breakStack = new LinkedList<>();
	protected final Deque<Set<Node>> continueStack = new LinkedList<>();
	protected final Map<String, Set<Node>> breakMap = new HashMap<>();
	protected final Map<String, Set<Node>> continueMap = new HashMap<>();
	protected final Deque<Set<Node>> switchSelectorStack = new LinkedList<>();
	protected final Deque<Boolean> switchHasDefaultStack = new LinkedList<>();

	protected int ttl = INFINITY;
	protected boolean generating = false;

	public ControlFlowGenerator(LAST graph)
	{
		super(graph);
	}

	@Override
	public void generate()
	{
		graph.getRootNode().accept(this, null);
	}

	protected void connectTo(Node n)
	{
		if (!generating)
			return;
		if (ttl > 0 || ttl == INFINITY)
		{
			makeConnections(n);
			if (ttl > 0) ttl--;
		}
		clearHanging();
		hangingNodes.add(n);
	}

	protected void makeConnections(Node n)
	{
		for (Node src : hangingNodes)
			graph.addEdge(src, n, Edge.Type.ControlFlow);
	}

	protected void clearHanging() {
		hangingNodes.clear();
	}

	@Override
	public void visitRoutine(Node n, Void arg)
	{
		assert hangingNodes.isEmpty();
		generating = true;
		hangingNodes.add(n);
		super.visitRoutine(n, arg);
		connectTo(n);
		generating = false;
		clearHanging();
	}

	@Override
	public void visitClause(Node n, Void arg)
	{
		connectTo(n);
		assert returnSet.isEmpty();
		graph.getChild(n, Node.Type.ParameterIn).accept(this, arg);
		graph.getChild(n, Node.Type.Parameters).accept(this, arg);
		graph.getChild(n, Node.Type.Guard).accept(this, arg);
		graph.getChild(n, Node.Type.Body).accept(this, arg);
		graph.getChild(n, Node.Type.ParameterOut).accept(this, arg);
		hangingNodes.addAll(returnSet);
		returnSet.clear();
		connectTo(n);
	}

	@Override
	public void visitEnclosed(Node n, Void arg)
	{
		super.visitEnclosed(n, arg);
		connectTo(n);
	}

	@Override
	public void visitEquality(Node n, Void arg)
	{
		new EDG(graph).getChild(n, Node.Type.Value).accept(this, arg);
		new EDG(graph).getChild(n, Node.Type.Pattern).accept(this, arg);
		connectTo(n);
	}

	@Override
	public void visitOperation(Node n, Void arg)
	{
		super.visitOperation(n, arg);
		connectTo(n);
	}

	@Override
	public void visitDataConstructor(Node n, Void arg)
	{
		super.visitDataConstructor(n, arg);
		connectTo(n);
	}

	@Override
	public void visitDataConstructorAccess(Node n, Void arg)
	{
		super.visitDataConstructorAccess(n, arg);
		connectTo(n);
	}

	@Override
	public void visitFieldAccess(Node n, Void arg)
	{
		super.visitFieldAccess(n, arg);
		connectTo(n);
	}

	@Override
	public void visitSwitch(Node n, Void arg)
	{
		breakStack.push(new HashSet<>());
		graph.getChild(n, Node.Type.Selector).accept(this, arg);
		switchSelectorStack.push(Set.copyOf(hangingNodes));
		switchHasDefaultStack.push(false);
		clearHanging();
		graph.getChild(n, Node.Type.Cases).accept(this, arg);
		hangingNodes.addAll(breakStack.pop());
		if (!switchHasDefaultStack.pop())
			hangingNodes.addAll(switchSelectorStack.getFirst());
		switchSelectorStack.pop();
	}

	@Override
	public void visitCase(Node n, Void arg)
	{
		Set<Node> prevCaseHanging = Set.copyOf(hangingNodes);
		clearHanging();
		hangingNodes.addAll(switchSelectorStack.getFirst());
		graph.getChild(n, Node.Type.Selectable).accept(this, arg);
		hangingNodes.addAll(prevCaseHanging);
		graph.getChild(n, Node.Type.Body).accept(this, arg);
	}

	@Override
	public void visitDefaultCase(Node n, Void arg)
	{
		hangingNodes.addAll(switchSelectorStack.getFirst());
		graph.getChild(n, Node.Type.Body).accept(this, arg);
		switchHasDefaultStack.pop();
		switchHasDefaultStack.push(true);
	}

	@Override
	public void visitCall(Node n, Void arg)
	{
		graph.getChild(n, Node.Type.Callee).accept(this, arg);
		graph.getChild(n, Node.Type.Arguments).accept(this, arg);
		connectTo(n);
	}

	@Override
	public void visitCallee(Node n, Void arg)
	{
		super.visitCallee(n, arg);
		connectTo(n);
	}

	@Override
	public void visitGenerator(Node n, Void arg)
	{
		new EDG(graph).getChild(n, Node.Type.Iterator).accept(this, arg);
		new EDG(graph).getChild(n, Node.Type.Variable).accept(this, arg);
		//connectTo(n);
	}

	@Override
	public void visitCLoop(Node n, Void arg)
	{
		// Efficiency: store the "first" visited element through the arg.
		breakStack.push(new HashSet<>());
		continueStack.push(new HashSet<>());
		graph.getChild(n, Node.Type.Condition).accept(this, arg);
		graph.getChild(n, Node.Type.Body).accept(this, arg);
		ttl = 1;
		hangingNodes.addAll(continueStack.pop());
		graph.getChild(n, Node.Type.Condition).accept(this, arg);
		ttl = INFINITY;
		hangingNodes.addAll(breakStack.pop());
	}

	@Override
	public void visitFLoop(Node n, Void arg)
	{
		breakStack.push(new HashSet<>());
		continueStack.push(new HashSet<>());
		graph.getChild(n, Node.Type.Init).accept(this, arg);
		graph.getChild(n, Node.Type.Condition).accept(this, arg);
		graph.getChild(n, Node.Type.Body).accept(this, arg);
		graph.getChild(n, Node.Type.Update).accept(this, arg);
		ttl = 1;
		hangingNodes.addAll(continueStack.pop());
		graph.getChild(n, Node.Type.Condition).accept(this, arg);
		ttl = INFINITY;
		hangingNodes.addAll(breakStack.pop());
	}

	@Override
	public void visitRLoop(Node n, Void arg)
	{
		breakStack.push(new HashSet<>());
		continueStack.push(new HashSet<>());
		graph.getChild(n, Node.Type.Body).accept(this, arg);
		hangingNodes.addAll(continueStack.getFirst());
		graph.getChild(n, Node.Type.Condition).accept(this, arg);
		Set<Node> condHanging = Set.copyOf(hangingNodes);
		ttl = 1;
		graph.getChild(n, Node.Type.Body).accept(this, arg);
		ttl = INFINITY;
		continueStack.pop();
		clearHanging();
		hangingNodes.addAll(condHanging);
		hangingNodes.addAll(breakStack.pop());
	}

	@Override
	public void visitForeach(Node n, Void arg)
	{
		// Efficiency: store the "first" visited element through the arg.
		breakStack.push(new HashSet<>());
		continueStack.push(new HashSet<>());
		graph.getChild(n, Node.Type.Iterator).accept(this, arg);
		graph.getChild(n, Node.Type.Body).accept(this, arg);
		ttl = 1;
		hangingNodes.addAll(continueStack.pop());
		graph.getChild(n, Node.Type.Iterator).accept(this, arg);
		ttl = INFINITY;
		hangingNodes.addAll(breakStack.pop());
	}

	@Override
	public void visitVariable(Node n, Void arg)
	{
		connectTo(n);
	}

	@Override
	public void visitLiteral(Node n, Void arg)
	{
		connectTo(n);
	}

	@Override
	public void visitReturn(Node n, Void arg)
	{
		super.visitReturn(n, arg);
		connectTo(n);
		returnSet.add(n);
		clearHanging();
	}

	@Override
	public void visitBreak(Node n, Void arg)
	{
		connectTo(n);
		if (graph.getChildren(n).isEmpty())
			breakStack.peek().add(n);
		else {
			String label = graph.getChild(n, Node.Type.Label).getLabel();
			breakMap.get(label).add(n);
		}
		clearHanging();
	}

	@Override
	public void visitContinue(Node n, Void arg)
	{
		connectTo(n);
		if (graph.getChildren(n).isEmpty())
			continueStack.peek().add(n);
		else {
			String label = graph.getChild(n, Node.Type.Label).getLabel();
			continueMap.get(label).add(n);
		}
		clearHanging();
	}

	@Override
	public void visitTypeCheck(Node n, Void arg)
	{
		super.visitTypeCheck(n, arg);
		connectTo(n);
	}

	@Override
	public void visitTypeTransformation(Node n, Void arg)
	{
		graph.getChild(n, Node.Type.Variable).accept(this, arg);
		graph.getChild(n, Node.Type.Type).accept(this, arg);
		connectTo(n);
	}

	@Override
	public void visitType(Node n, Void arg)
	{
		connectTo(n);
	}

	@Override
	public void visitReference(Node n, Void arg)
	{
		connectTo(n);
	}

	@Override
	public void visitLabel(Node n, Void arg)
	{
		breakMap.put(n.getLabel(), new HashSet<>());
		continueMap.put(n.getLabel(), new HashSet<>());
		super.visitLabel(n, arg);
		hangingNodes.addAll(breakMap.remove(n.getLabel()));
		assert !continueMap.containsKey(n.getLabel());
	}

	@Override
	public void visitIf(Node n, Void arg)
	{
		// <before> --> *if*
		graph.getChild(n, Node.Type.Condition).accept(this, arg);
		Set<Node> condHanging = Set.copyOf(hangingNodes);

		// if --> *then*
		new EDG(graph).getChild(n, Node.Type.Then).accept(this, arg);
		Set<Node> thenHanging = Set.copyOf(hangingNodes);

		Node _else = new EDG(graph).getChild(n, Node.Type.Else);
		if (graph.getChildren(_else).size() != 0) { // TODO: IMPROVE CONDITION
			// if --> *else*
			clearHanging();
			hangingNodes.addAll(condHanging);
			new EDG(graph).getChild(n, Node.Type.Else).accept(this, arg);
			hangingNodes.addAll(thenHanging);
		} else {
			// if --> <after>
			hangingNodes.addAll(condHanging);
		}
		// {then else|if} --> <after>
	}

	@Override
	public void visitParameterIn(Node n, Void arg)
	{
		connectTo(n);
	}

	@Override
	public void visitParameterOut(Node n, Void arg)
	{
		connectTo(n);
	}
}
