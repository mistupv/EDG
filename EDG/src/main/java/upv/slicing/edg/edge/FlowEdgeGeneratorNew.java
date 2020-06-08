package upv.slicing.edg.edge;

import upv.slicing.edg.LASTBuilder;
import upv.slicing.edg.LDASTNodeInfo;
import upv.slicing.edg.graph.*;
import upv.slicing.edg.graph.Variable;
import upv.slicing.edg.traverser.ControlFlowTraverser;

import java.util.*;
import java.util.stream.Collectors;

public class FlowEdgeGeneratorNew extends EdgeGenerator {

	private static class DefUseState {
		private final Set<String> uses;
		private final Set<String> definitions;

		private DefUseState()
		{
			this(new HashSet<>(), new HashSet<>());
		}

		private DefUseState(Set<String> uses, Set<String> definitions)
		{
			this.uses = uses;
			this.definitions = definitions;
		}
		public String toString() {
			return "U" + this.uses.toString() + ", D" + this.definitions.toString();
		}
		public boolean equals(Object o)
		{
			if (o == this)
				return true;
			if (!(o instanceof DefUseState))
				return false;

			final DefUseState defUseState = (DefUseState) o;

			return Objects.equals(this.uses, defUseState.uses) &&
					Objects.equals(this.definitions, defUseState.definitions);
		}
		public int hashCode() {
			return Objects.hash( uses, definitions);
		}

	}

	private static class State {
		private final Node clauseNode;
		private final Node traversalNode;
		private final DefUseState defUseState;

		private State(Node clauseNode, Node traversalNode) {
			this(clauseNode, traversalNode, new DefUseState());
		}

		private State(Node clauseNode, Node traversalNode, DefUseState state) {
			this.clauseNode = clauseNode;
			this.traversalNode = traversalNode;
			this.defUseState = state;
		}

		public String toString() {
			return "{" + " clauseNode: " + this.clauseNode + " travesalNode: " + this.traversalNode.getId() +  "=>" + defUseState.toString() + "}";
		}

		public boolean equals(Object o) {
			if (o == this)
				return true;
			if (!(o instanceof State))
				return false;

			final State state = (State) o;
			return Objects.equals(this.clauseNode, state.clauseNode) &&
					Objects.equals(this.traversalNode, state.traversalNode) &&
					Objects.equals(this.defUseState, state.defUseState);
		}

		public int hashCode() {
			return Objects.hash(traversalNode.getId(), defUseState);
		}
	}


	/*	private static class Variable
		{
			private final VariableId variableId;
			private final Node node;

			private Variable(VariableId variableId, Node node)
			{
				this.variableId = variableId;
				this.node = node;
			}
		}
		*/

	/*private static class VariableId {
		private final String variableId;
		private final Node index;

		private VariableId(String variableId) {
			this(variableId, null);
		}

		private VariableId(String variableId, Node index) {
			this.variableId = variableId;
			this.index = index;
		}

		public String toString() {
			return this.variableId;
		}

		public boolean equals(Object o) {
			if (o == this)
				return true;
			if (!(o instanceof VariableId))
				return false;

			final VariableId variableId = (VariableId) o;

			return this.variableId.equals(variableId.variableId);
		}

		public int hashCode() {
			return this.variableId.hashCode();
		}

		public String getVariableName() {
			return this.split()[0];
		}

		public String getFullName() {
			if (this.index != null)
				return this.getVariableName() + this.index.getName();
			return this.getVariableName();
		}

		public Node getVariableIndex() {
			return this.index;
		}

		public Node.Type getVariableIndexType() {
			if (index == null)
				return null;
			return this.index.getType();
		}

		public boolean greater(VariableId variableId) {
			final String[] split = variableId.split();
			if (split.length < 2)
				return false;
			final String variable = split[0];
			return this.variableId.equals(variable);
		}

		private String[] split() {
			final StringTokenizer st = new StringTokenizer(this.variableId, "[]");
			final List<String> split = new LinkedList<>();
			while (st.hasMoreTokens())
				split.add(st.nextToken());
			return split.toArray(new String[0]);
		}
	}*/

	public FlowEdgeGeneratorNew(EDG edg) {
		super(edg);
	}

	public void generate() {
		this.resolveParamInOutClauses(); // Atraviesa el CFG e identifica usos y definiciones rellenando el clauseMap
		this.createParamInOutNodes(); // Añade los nodos en param-in y param out modificando el CFG

		//this.addEdges();
	}

/*	private Set<State> getNodeStates(Node node, boolean global) {
		if (this.callMap.containsKey(node))
			return this.callMap.get(node);
		return this.getVariableStates(node, global);
	}

	private Set<State> getVariableStates(Node node, boolean global) {
		final Set<State> states = new HashSet<>();
		final Set<VariableId> uses = new HashSet<>();
		final Set<VariableId> definitions = new HashSet<>();

		if (node instanceof upv.slicing.edg.graph.Variable) {
			final upv.slicing.edg.graph.Variable variable = (upv.slicing.edg.graph.Variable) node;

			if (!global || variable.isGlobal()) {
				final Context context = variable.getContext();

				final Node grandParent = edg.getParent(edg.getParent(node));
				final VariableId variableId;
				if (grandParent.getType() == Node.Type.DataConstructorAccess) {
					final Node index = edg.getChild(edg.getChild(grandParent, 1), 0);
					variableId = new VariableId(node.getName(), index);
				} else
					variableId = this.getVariableId(node);

				switch (context) {
					case Use:
						uses.add(variableId);
						break;
					case Definition:
						definitions.add(variableId);
						break;
					case Def_Use:            // ADDED FOR UnaryOperations (++/--) that both define and use a variable, Scope Variables too (v.addElement(elem))
						uses.add(variableId);
						definitions.add(variableId);
						break;
					case Declaration:
						break;
				}
			}
		}
		states.add(new State(node, uses, definitions));

		return states;
	}

	private VariableId getVariableId(Node variable) {
		if (!(variable instanceof upv.slicing.edg.graph.Variable))
			throw new RuntimeException("The node is not a variable");

		final upv.slicing.edg.graph.Variable variableInfo = (upv.slicing.edg.graph.Variable) variable;
		final String variableName = variableInfo.getName();
		return new VariableId(variableName);
	}
*/
	/** Map that contains each clause with definitions and uses of global variables */
	private final Map<Node, DefUseState> clauseMap = new HashMap<>();
	// private final Map<Node, Set<DefUseState>> callMap = new HashMap<>();

	/** Iterates over all clauses and fulfils the clauseMap */
	public void resolveParamInOutClauses() {

		final List<Node> clauses = this.edg.getNodes(Node.Type.Clause);
		final List<Node> resolvedClauses = new LinkedList<>();
		final Map<Node, Set<State>> suspendedWorks = new HashMap<>();
		for (Node clause : clauses) {
			clauseMap.put(clause, new DefUseState());
			this.resolveClauseDefUse(clause, suspendedWorks, resolvedClauses);
		}

		while(!suspendedWorks.isEmpty()){
			this.resumeWorks(suspendedWorks, resolvedClauses);
		}
	}

/*	public void resolveClauseDefUse(Node clause) {

		final Node clauseResult = edg.getResFromNode(clause);
		final List<State> finalStates = new LinkedList<>();

		final Set<State> pendingWorks = new HashSet<>();
		pendingWorks.add(new State(clause));

		final Set<State> doneWorks = new HashSet<>();

		while (!pendingWorks.isEmpty()) {
			final State work = pendingWorks.iterator().next();
			final Node workNode = work.traversalNode;
			pendingWorks.remove(work);

			if(workNode == clauseResult) {
				finalStates.add(work);
				continue;
			}

			if (doneWorks.contains(work))
				continue;

			if (workNode.getType() == Node.Type.Variable) {
				final Variable v = (Variable) workNode;
				if (v.isGlobal())
				{
					String varName = v.getName();
					switch (v.getContext())
					{
						case Definition:
							if (!work.definitions.contains(varName))
								work.definitions.add(varName);
							break;
						case Def_Use:
							if (!work.definitions.contains(varName))
								work.definitions.add(varName);
						case Use:
							if (!work.uses.contains(varName) && !work.definitions.contains(varName))
								work.uses.add(varName);
							break;
						default:
							break;
					}
				}
			}
			doneWorks.add(work);
			Set<Node> nextNodes = ControlFlowTraverser.step(this.edg, workNode, LAST.Direction.Forwards);
			nextNodes.forEach(nextNode -> pendingWorks.add(new State(nextNode, work.uses, work.definitions)));
		}

		final Set<String> globalDefs = new HashSet<>();
		final Set<String> globalUses = new HashSet<>();

		for (State s : finalStates) {
			globalDefs.addAll(s.definitions);
			globalUses.addAll(s.uses);
		}

		this.createParamInOutNodes(clause, globalDefs, globalUses);
	}*/

	/** This method traverse all the clauses looking for DEF and USE sets, 
	 * marking which ones are blocked by an unresolved function call */ 
	public void resolveClauseDefUse(Node initialNode, Map<Node,Set<State>> suspendedWorks, List<Node> resolvedClauses) {
		final Set<State> doneWorks = new HashSet<>();
		final Set<State> pendingWorks = new HashSet<>();
		final State initialWork = new State(initialNode, initialNode);
		pendingWorks.add(initialWork);

		boolean anySuspended = processWorks(suspendedWorks, resolvedClauses, pendingWorks, doneWorks);

		if (!anySuspended)
			resolvedClauses.add(initialNode);
	}

	/** Traverses the CFG, annotating DEF and USE sets.
	 * @return 	true if any CFG path is suspended by an unresolved function call 
	 * 			false otherwise */
	public boolean processWorks(Map<Node,Set<State>> suspendedWorks, List<Node> resolvedClauses,
							 Set<State> pendingWorks, Set<State> doneWorks)
	{
		boolean suspended = false;
		while (!pendingWorks.isEmpty()) {
			final State work = pendingWorks.iterator().next();
			final Node workNode = work.traversalNode;
			pendingWorks.remove(work);

			if(workNode == edg.getResFromNode(work.clauseNode)) {
				this.addDefUsesToMap(work);
				continue;
			}

			if (doneWorks.contains(work))
				continue;

			final Node.Type nodeType = workNode.getType();
			if (nodeType == Node.Type.Variable) {
				final Variable v = (Variable) workNode;
				if (v.isGlobal())
					this.treatVariable(v,work.defUseState);
			}
			else if (nodeType == Node.Type.Call)
			{
				final Node calleeResNode = edg.getResFromNode(edg.getChild(workNode, Node.Type.Callee));
				final Set<Edge> callEdges = edg.getEdges(calleeResNode, LAST.Direction.Forwards, Edge.Type.Call);
				for( Edge callEdge : callEdges ){
					final Node target = edg.getEdgeTarget(callEdge);
					if (resolvedClauses.contains(target)) {
						final DefUseState defUses = clauseMap.get(target);
						this.addCallState(work, defUses);
						continue;
					}

					this.suspendWork(suspendedWorks,work,target);
					suspended = true;
				}
				if (suspended)
					continue;
			}
			doneWorks.add(work);
			final Set<Node> nextNodes = ControlFlowTraverser.step(this.edg, workNode, LAST.Direction.Forwards);
			nextNodes.forEach(nextNode -> pendingWorks.add(new State(work.clauseNode, nextNode, work.defUseState)));
		}
		return suspended;
	}

	/** Resume those suspended works that can be resolved after resolving other function calls */
	public void resumeWorks(Map<Node,Set<State>> suspendedWorks, List<Node> resolvedClauses)
	{
		final Set<State> doneWorks = new HashSet<>();
		final Set<State> pendingWorks = new HashSet<>();
		for (Node resolvedClause : resolvedClauses)
		{
			Set<State> resumableWorks = suspendedWorks.remove(resolvedClause);
			if (resumableWorks != null) {
				pendingWorks.addAll(resumableWorks);
			}
		}
		processWorks(suspendedWorks, resolvedClauses, pendingWorks, doneWorks);

		Set<Node> allClauses = clauseMap.keySet();
		List<Node> suspendedClauses = getDifferentSuspendedClauses(suspendedWorks);
		for (Node clause : allClauses)
			if (!resolvedClauses.contains(clause) && !suspendedClauses.contains(clause))
				resolvedClauses.add(clause);
	}

	/** Provides the list of clause nodes suspended in the suspendedWorks map */
	public List<Node> getDifferentSuspendedClauses(Map<Node,Set<State>> suspendedWorks)
	{
		Collection<Set<State>> suspended = suspendedWorks.values();
		List<State> states = suspended.stream()
				.flatMap(list -> list.stream())
				.collect(Collectors.toList());
		List<Node> suspendedClauses = new LinkedList<>();
		for(State s : states)
			if (!suspendedClauses.contains(s.clauseNode))
				suspendedClauses.add(s.clauseNode);

		return suspendedClauses;
	}

	/** Adds definitions and uses of global variabels to the clauseMap after resolving a CFG path */
	public void addDefUsesToMap(State work)
	{
		DefUseState clauseDefUse = clauseMap.get(work.clauseNode);
		for (String use : work.defUseState.uses)
			if(!clauseDefUse.uses.contains(use))
				clauseDefUse.uses.add(use);

		for (String def : work.defUseState.definitions)
			if(!clauseDefUse.definitions.contains(def))
				clauseDefUse.definitions.add(def);
	}

	/** Adds the list of definitions and uses to a current State.
	 * @apiNote This method is used to add the DEF of USE sets of a method call in the CFG traversal */
	public void addCallState(State work, DefUseState defUses)
	{
		for (String def : defUses.definitions)
			if (!work.defUseState.definitions.contains(def))
				work.defUseState.definitions.add(def);

		for (String use : defUses.uses)
			if (!work.defUseState.uses.contains(use) && !work.defUseState.definitions.contains(use))
				work.defUseState.uses.add(use);
	}

	/** Add a global variable to the corresponding DEF or USE set if necessary */
	public void treatVariable(Variable v, DefUseState workState)
	{
		String varName = v.getName();
		switch (v.getContext())
		{
			case Definition:
				if (!workState.definitions.contains(varName))
					workState.definitions.add(varName);
				break;
			case Def_Use:
				if (!workState.definitions.contains(varName))
					workState.definitions.add(varName);
			case Use:
				if (!workState.uses.contains(varName) && !workState.definitions.contains(varName))
					workState.uses.add(varName);
				break;
			default:
				break;
		}
	}

	/** Adds a work State into a list of suspended works.
	 * @apiNote The Key is the clause node not resolved that prevents the traversal to continue
	 * 			The Value is the work itself */
	public void suspendWork(Map<Node,Set<State>> suspendedWorks, State work, Node target)
	{
		final Set<State> clauseSuspendedWorks = suspendedWorks.get(target);
		if (clauseSuspendedWorks == null) {
			Set<State> clauseStates = new HashSet<>();
			clauseStates.add(work);
			suspendedWorks.put(target, clauseStates);
		}
		else
			clauseSuspendedWorks.add(work);
	}

	/** Creates the global var DEF and USE corresponding nodes in the formal-in and
	 * formal-out nodes of every function definition contained in the clauseMap */
	public void createParamInOutNodes()
	{
		for( Node clause : clauseMap.keySet())
		{
			final DefUseState defUses = clauseMap.get(clause);
			this.createParamInOutNodes(clause, defUses.definitions, defUses.uses);
		}
	}

	/** Creates the global var DEF and USE corresponding nodes in the formal-in and
	 * formal-out nodes of a single function definition given as an input parameter */
	public void createParamInOutNodes(Node clause, Set<String> defs, Set<String> uses)
	{
		final Node paramIn = this.edg.getChild(clause, Node.Type.ParameterIn);
		final Set<Edge> paramInIncomingCFGEdges = edg.incomingEdgesOf(paramIn);
		paramInIncomingCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);
		assert(paramInIncomingCFGEdges.size() == 1);
		final Edge incomingEdgeIn = paramInIncomingCFGEdges.iterator().next();

		// Transformation of Global Vars in ParameterIn Node
		this.transformParamInOutStructure(clause, paramIn, incomingEdgeIn, uses);

		final Node paramOut = this.edg.getChild(clause, Node.Type.ParameterOut);
		final Set<Edge> paramOutIncomingCFGEdges = edg.incomingEdgesOf(paramOut);
		paramOutIncomingCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);
		assert(paramOutIncomingCFGEdges.size() == 1);
		final Edge incomingEdgeOut = paramOutIncomingCFGEdges.iterator().next();

		// Transformation of Global Vars in ParameterOut Node
		this.transformParamInOutStructure(clause, paramOut, incomingEdgeOut, defs);

	}

	public void addEdges()
	{
		// final Set<Node> worksDone = new HashSet<>(); // Probably for loops in DEF-USE dependencies (Class State)
	}

	/** Creates the corresponding Value and ControlFlow arcs when adding
	 * formal-in and formal-out nodes to a function definition */
	public void transformParamInOutStructure(Node clause, Node params, Edge incomingEdge, Set<String> set)
	{
		Node hangingNode = edg.getEdgeSource(incomingEdge);
		for (String item : set)
		{
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(clause.getInfo().getFile(), clause.getInfo().getClassName(),
					clause.getInfo().getLine(), true, "var", null);
			final int nodeId = LASTBuilder.addVariable(this.edg, params.getId(), null, item, false, true, false, false, ldNodeInfo);
			final Node paramVar = this.edg.getNode(nodeId);

			final LDASTNodeInfo resultInfo = new LDASTNodeInfo(clause.getInfo().getFile(), clause.getInfo().getClassName(),
					clause.getInfo().getLine(), "var");
			final Node result = new Node("result", this.edg.getNextFictitiousId(), Node.Type.Result, "", resultInfo);

			edg.addVertex(result);
			edg.registerNodeResPair(paramVar, result);
			edg.addEdge(paramVar,result, Edge.Type.Value);
			edg.addEdge(paramVar,result, Edge.Type.ControlFlow);
			edg.addStructuralEdge(params, result);

			edg.addEdge(hangingNode, paramVar, Edge.Type.ControlFlow);
			hangingNode = result;
		}

		if (set.size() > 0)
		{
			edg.addEdge(hangingNode, params, Edge.Type.ControlFlow);
			edg.removeEdge(incomingEdge);
		}
	}
}