package upv.slicing.eknife.java;

import upv.slicing.edg.edge.Generator;
import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;
import upv.slicing.edg.traverser.EDGTraverser;
import upv.slicing.edg.traverser.LASTTraverser;
import upv.slicing.edg.visitor.VoidVisitor;

import java.util.*;

public final class ControlFlowGenerator extends VoidVisitor<Void> implements Generator {
	private static final int INFINITY = -1;

	private final Set<Node> hangingNodes = new HashSet<>();
	private final Set<Node> returnSet = new HashSet<>();
	private final Deque<Set<Node>> breakStack = new LinkedList<>();
	private final Deque<Set<Node>> continueStack = new LinkedList<>();
	private final Map<String, Set<Node>> breakMap = new HashMap<>();
	private final Map<String, Set<Node>> continueMap = new HashMap<>();
	private final Deque<Set<Node>> switchSelectorStack = new LinkedList<>();
	private final Deque<Boolean> switchHasDefaultStack = new LinkedList<>();

	private int ttl = INFINITY;
	private boolean generating = false;

	public ControlFlowGenerator(LAST graph)
	{
		super(graph);
	}

	@Override
	public void generate()
	{
		graph.getRootNode().accept(this, null);
	}

	private void connectTo(Node n)
	{
		if (!generating)
			return;
		if (ttl > 0 || ttl == INFINITY)
		{
			for (Node src : hangingNodes)
				graph.addEdge(src, n, Edge.Type.ControlFlow);
			if (ttl > 0) ttl--;
		}
		clearHanging();
		hangingNodes.add(n);
	}

	private void clearHanging() {
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
		LASTTraverser.getChild(graph, n, Node.Type.Guard).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Body).accept(this, arg);
		hangingNodes.addAll(returnSet);
		returnSet.clear();
		connectTo(n);
	}

	@Override
	public void visitEquality(Node n, Void arg)
	{
		EDGTraverser.getChild(new EDG(graph), n, Node.Type.Value).accept(this, arg);
		EDGTraverser.getChild(new EDG(graph), n, Node.Type.Pattern).accept(this, arg);
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
		LASTTraverser.getChild(graph, n, Node.Type.Selector).accept(this, arg);
		switchSelectorStack.push(Set.copyOf(hangingNodes));
		switchHasDefaultStack.push(false);
		clearHanging();
		LASTTraverser.getChild(graph, n, Node.Type.Cases).accept(this, arg);
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
		LASTTraverser.getChild(graph, n, Node.Type.Selectable).accept(this, arg);
		hangingNodes.addAll(prevCaseHanging);
		LASTTraverser.getChild(graph, n, Node.Type.Body).accept(this, arg);
	}

	@Override
	public void visitDefaultCase(Node n, Void arg)
	{
		hangingNodes.addAll(switchSelectorStack.getFirst());
		LASTTraverser.getChild(graph, n, Node.Type.Body).accept(this, arg);
		switchHasDefaultStack.pop();
		switchHasDefaultStack.push(true);
	}

	@Override
	public void visitCall(Node n, Void arg)
	{
		LASTTraverser.getChild(graph, n, Node.Type.Callee).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Arguments).accept(this, arg);
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
		LASTTraverser.getChild(graph, n, Node.Type.Iterator).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Variable).accept(this, arg);
	}

	@Override
	public void visitCLoop(Node n, Void arg)
	{
		// Efficiency: store the "first" visited element through the arg.
		breakStack.push(new HashSet<>());
		continueStack.push(new HashSet<>());
		LASTTraverser.getChild(graph, n, Node.Type.Condition).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Body).accept(this, arg);
		ttl = 1;
		hangingNodes.addAll(continueStack.pop());
		LASTTraverser.getChild(graph, n, Node.Type.Condition).accept(this, arg);
		ttl = INFINITY;
		hangingNodes.addAll(breakStack.pop());
	}

	@Override
	public void visitFLoop(Node n, Void arg)
	{
		breakStack.push(new HashSet<>());
		continueStack.push(new HashSet<>());
		LASTTraverser.getChild(graph, n, Node.Type.Init).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Condition).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Body).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Update).accept(this, arg);
		ttl = 1;
		hangingNodes.addAll(continueStack.pop());
		LASTTraverser.getChild(graph, n, Node.Type.Condition).accept(this, arg);
		ttl = INFINITY;
		hangingNodes.addAll(breakStack.pop());
	}

	@Override
	public void visitRLoop(Node n, Void arg)
	{
		breakStack.push(new HashSet<>());
		continueStack.push(new HashSet<>());
		LASTTraverser.getChild(graph, n, Node.Type.Body).accept(this, arg);
		hangingNodes.addAll(continueStack.getFirst());
		LASTTraverser.getChild(graph, n, Node.Type.Condition).accept(this, arg);
		Set<Node> condHanging = Set.copyOf(hangingNodes);
		ttl = 1;
		LASTTraverser.getChild(graph, n, Node.Type.Body).accept(this, arg);
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
		LASTTraverser.getChild(graph, n, Node.Type.Iterator).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Body).accept(this, arg);
		ttl = 1;
		hangingNodes.addAll(continueStack.pop());
		LASTTraverser.getChild(graph, n, Node.Type.Iterator).accept(this, arg);
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
		clearHanging(); // ACFG: delete this
	}

	@Override
	public void visitBreak(Node n, Void arg)
	{
		connectTo(n);
		if (LASTTraverser.getChildren(graph, n).isEmpty())
			breakStack.peek().add(n);
		else {
			String label = LASTTraverser.getChild(graph, n, Node.Type.Label).getLabel();
			breakMap.get(label).add(n);
		}
		clearHanging(); // ACFG: delete this
	}

	@Override
	public void visitContinue(Node n, Void arg)
	{
		connectTo(n);
		if (LASTTraverser.getChildren(graph, n).isEmpty())
			continueStack.peek().add(n);
		else {
			String label = LASTTraverser.getChild(graph, n, Node.Type.Label).getLabel();
			continueMap.get(label).add(n);
		}
		clearHanging(); // ACFG: delete this
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
		LASTTraverser.getChild(graph, n, Node.Type.Variable).accept(this, arg);
		LASTTraverser.getChild(graph, n, Node.Type.Type).accept(this, arg);
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
		LASTTraverser.getChild(graph, n, Node.Type.Condition).accept(this, arg);
		Set<Node> condHanging = Set.copyOf(hangingNodes);

		// if --> *then*
		EDGTraverser.getChild(new EDG(graph), n, Node.Type.Then).accept(this, arg);
		Set<Node> thenHanging = Set.copyOf(hangingNodes);

		if (LASTTraverser.getChildren(graph, n).size() == 3) { // TODO: IMPROVE CONDITION
			// if --> *else*
			clearHanging();
			hangingNodes.addAll(condHanging);
			EDGTraverser.getChild(new EDG(graph), n, Node.Type.Else).accept(this, arg);
			hangingNodes.addAll(thenHanging);
		} else {
			// if --> <after>
			hangingNodes.addAll(condHanging);
		}
		// {then else|if} --> <after>
	}
}
