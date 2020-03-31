package upv.slicing.eknife.java;

import upv.slicing.edg.graph.Edge;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

public final class AugmentedControlFlowGenerator extends ControlFlowGenerator {
	public AugmentedControlFlowGenerator(LAST graph)
	{
		super(graph);
	}

	private final Set<Node> nonExecHangingNodes = new HashSet<>();
	private final Deque<Set<Node>> switchSelectableStack = new LinkedList<>();
	protected final Deque<Node> switchDefaultStack = new LinkedList<>();

	@Override
	protected void clearHanging() {
		super.clearHanging();
		nonExecHangingNodes.clear();
	}

	@Override
	protected void makeConnections(Node n) {
		super.makeConnections(n);
		for (Node src : nonExecHangingNodes)
			graph.addEdge(src, n, Edge.Type.NonExecControlFlow);
	}

	@Override
	protected void connectTo(Node n)
	{
		connectTo(n, true);
	}

	protected void connectTo(Node n, boolean executable)
	{
		if (!generating)
			return;
		if (ttl > 0 || ttl == INFINITY)
		{
			makeConnections(n);
			if (ttl != INFINITY && ttl > 0) ttl--;
		}
		clearHanging();
		(executable ? hangingNodes : nonExecHangingNodes).add(n);
	}

	@Override
	public void visitReturn(Node n, Void arg)
	{
		super.visitReturn(n, arg);
		connectTo(n, false);
		returnSet.add(n);
	}

	@Override
	public void visitBreak(Node n, Void arg)
	{
		connectTo(n, false);
		if (ttl > 0 || ttl == INFINITY)
			if (graph.getChildren(n).isEmpty())
				breakStack.peek().add(n);
			else
			{
				String label = graph.getChild(n, Node.Type.Label).getLabel();
				breakMap.get(label).add(n);
			}
	}

	@Override
	public void visitContinue(Node n, Void arg)
	{
		connectTo(n, false);
		if (graph.getChildren(n).isEmpty())
			continueStack.peek().add(n);
		else
		{
			String label = graph.getChild(n, Node.Type.Label).getLabel();
			continueMap.get(label).add(n);
		}
	}

	@Override
	public void visitSwitch(Node n, Void arg)
	{
		switchSelectableStack.push(new HashSet<>());
		breakStack.push(new HashSet<>());
		graph.getChild(n, Node.Type.Selector).accept(this, arg);
		switchSelectorStack.push(Set.copyOf(hangingNodes));
		switchHasDefaultStack.push(false);
		clearHanging();
		graph.getChild(n, Node.Type.Cases).accept(this, arg);
		if (!switchHasDefaultStack.pop())
		{
			hangingNodes.addAll(switchSelectorStack.getFirst());
			nonExecHangingNodes.addAll(switchSelectableStack.getFirst());
		}
		else
		{
			Set<Node> prevHanging = Set.copyOf(hangingNodes);
			Set<Node> prevNonExecHanging = Set.copyOf(nonExecHangingNodes);
			clearHanging();

			nonExecHangingNodes.addAll(switchSelectableStack.getFirst());
			final Node _default = switchDefaultStack.getFirst();
			ttl = 1;
			graph.getChild(_default, Node.Type.Body).accept(this, arg);
			ttl = INFINITY;
			clearHanging();

			switchDefaultStack.pop();
			hangingNodes.addAll(prevHanging);
			nonExecHangingNodes.addAll(prevNonExecHanging);
			nonExecHangingNodes.addAll(switchSelectorStack.getFirst());

		}
		hangingNodes.addAll(breakStack.pop());
		switchSelectorStack.pop();
		switchSelectableStack.pop();
	}

	@Override
	public void visitCase(Node n, Void arg)
	{
		Set<Node> prevCaseHanging = Set.copyOf(hangingNodes);
		clearHanging();
		hangingNodes.addAll(switchSelectorStack.getFirst());
		graph.getChild(n, Node.Type.Selectable).accept(this, arg);

		switchSelectableStack.getFirst().addAll(hangingNodes);

		hangingNodes.addAll(prevCaseHanging);
		graph.getChild(n, Node.Type.Body).accept(this, arg);
	}

	@Override
	public void visitDefaultCase(Node n, Void arg)
	{
		switchDefaultStack.push(n);
		super.visitDefaultCase(n, arg);
	}
}
