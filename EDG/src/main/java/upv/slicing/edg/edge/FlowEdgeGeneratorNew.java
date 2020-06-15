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
		private int numOfPaths = 0;
		private final Set<String> uses;
		private final Set<String> definitions;

		private DefUseState()
		{
			this(new HashSet<>(), new HashSet<>());
		}
		private DefUseState(Set<String> uses, Set<String> definitions)
		{
			this.uses = uses.stream().collect(Collectors.toSet());
			this.definitions = definitions.stream().collect(Collectors.toSet());
		}
		private DefUseState(Set<String> uses, Set<String> definitions, int paths)
		{
			this.uses = uses.stream().collect(Collectors.toSet());
			this.definitions = definitions.stream().collect(Collectors.toSet());
			this.numOfPaths = paths;
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
	public static class EdgeGenState {
		private final Node node;
		private final Map <String,Node> defMap;
		private final Map <String,Node> decMap;

		private EdgeGenState(Node node)
		{
			this(node, new HashMap<>(), new HashMap<>());
		}

		private EdgeGenState(Node node, Map<String,Node> definitions, Map<String,Node> declarations)
		{
			this.node = node;
			this.defMap = definitions;
			this.decMap = declarations;
		}
		public String toString() {
			return "Node" + this.node.getId() + ", DefVars" + this.defMap.keySet().toString();
		}
		public boolean equals(Object o)
		{
			if (o == this)
				return true;
			if (!(o instanceof EdgeGenState))
				return false;

			final EdgeGenState state = (EdgeGenState) o;

			return Objects.equals(this.node, state.node) &&
					Objects.equals(this.defMap, state.defMap) &&
					Objects.equals(this.decMap, state.decMap);
		}
		public int hashCode() {
			return Objects.hash(node.getId(), defMap, decMap);
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
		this.resolveParamInOutClauses(); // Traverses the  CFG and fills clauseMap with DEF and USE sets for each clause
		this.createParamInOutNodes(); // Add defined and used GV to param-in and param-out nodes modifying the CFG
		this.createArgInOutNodes(); // Add defined and used GV to arg-in and arg-out nodes modifying the CFG
		this.addEdges();
		this.addOrphanClauseEdges();
	}

	/** Map that contains each clause with definitions and uses of global variables */
	private final Map<Node, DefUseState> clauseMap = new HashMap<>();

	// ---------------------------------------------------------- //
	// ----- OBTAIN GLOBAL VARS DEFINED AND USED IN METHODS ----- //
	// ---------------------------------------------------------- //

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

	/** This method traverse all the clauses looking for DEF and USE sets, 
	 * marking which ones are blocked by an unresolved function call */ 
	public void resolveClauseDefUse(Node initialNode, Map<Node,Set<State>> suspendedWorks, List<Node> resolvedClauses) {

		final Set<State> pendingWorks = new HashSet<>();
		final State initialWork = new State(initialNode, initialNode);
		pendingWorks.add(initialWork);

		boolean anySuspended = processWorks(suspendedWorks, resolvedClauses, pendingWorks, clauseMap);

		if (!anySuspended)
			resolvedClauses.add(initialNode);
	}

	/** Traverses the CFG, annotating DEF and USE sets.
	 * @return 	true if any CFG path is suspended by an unresolved function call 
	 * 			false otherwise */
	public boolean processWorks(Map<Node,Set<State>> suspendedWorks, List<Node> resolvedClauses,
							 Set<State> pendingWorks, Map<Node,DefUseState> usedClauseMap)
	{
		final Set<State> doneWorks = new HashSet<>();
		boolean suspended = false;
		while (!pendingWorks.isEmpty()) {
			final State work = pendingWorks.iterator().next();
			final Node workNode = work.traversalNode;
			pendingWorks.remove(work);

			if(workNode == edg.getResFromNode(work.clauseNode)) {
				this.addDefUsesToMap(work, usedClauseMap);
				continue;
			}

			if (doneWorks.contains(work))
				continue;

			final Node.Type nodeType = workNode.getType();
			if (nodeType == Node.Type.Variable) {
				final Variable v = (Variable) workNode;
				if (v.isGlobal())
					this.treatVariable(v, work.defUseState);
			}
			else if (nodeType == Node.Type.Call)
			{
				final Node calleeResNode = edg.getResFromNode(edg.getChild(workNode, Node.Type.Callee));
				final Set<Edge> callEdges = edg.getEdges(calleeResNode, LAST.Direction.Forwards, Edge.Type.Call);
				for( Edge callEdge : callEdges ){	// TODO: This may change with polymorphism
					final Node calledClause = edg.getEdgeTarget(callEdge);
					if (work.clauseNode == calledClause)
						continue;
					if (resolvedClauses.contains(calledClause)) {
						final DefUseState defUses = usedClauseMap.get(calledClause);
						this.treatCall(work, defUses);
						continue;
					}

					this.suspendWork(suspendedWorks,work,calledClause);
					suspended = true;
				}
				if (suspended)
					continue;
			}
			doneWorks.add(work);
			final Set<Node> nextNodes = ControlFlowTraverser.step(this.edg, workNode, LAST.Direction.Forwards);
			nextNodes.forEach(nextNode -> pendingWorks.add(new State(work.clauseNode, nextNode, new DefUseState(work.defUseState.uses, work.defUseState.definitions))));
		}
		return suspended;
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
			case Use:
				if (!workState.uses.contains(varName) && !workState.definitions.contains(varName))
					workState.uses.add(varName);
				break;
			case Def_Use:
				if (!workState.uses.contains(varName) && !workState.definitions.contains(varName))
					workState.uses.add(varName);
				if (!workState.definitions.contains(varName))
					workState.definitions.add(varName);
				break;
			default:
				break;
		}
	}

	/** Adds the list of definitions and uses to a current State.
	 * @apiNote This method is used to add the DEF of USE sets of a method call in the CFG traversal */
	public void treatCall(State work, DefUseState callDefUses)
	{
		Set<String> uses = work.defUseState.uses;
		Set<String> definitions = work.defUseState.definitions;

		for (String use : callDefUses.uses)
			if (!uses.contains(use) && !definitions.contains(use))
				uses.add(use);

		for (String def : callDefUses.definitions)
			if (!definitions.contains(def))
				definitions.add(def);
	}

	/** Adds definitions and uses of global variabels to the clauseMap after resolving a CFG path
	 * @implNote Every time a new global variable definition is added by traversing a method declaration,
	 * if there is any another path of the method that does not define this global variable, a use of
	 * this global variable is needed in order to preserve the correct value of the variable in both
	 * possible executions. */
	public void addDefUsesToMap(State work, Map<Node,DefUseState> usedClauseMap)
	{
		DefUseState clauseDefUse = usedClauseMap.get(work.clauseNode);

		Set<String> uses = work.defUseState.uses;
		Set<String> definitions = work.defUseState.definitions;
		//---------------------------------------------------------------------//
		// Added for method paths that does not define every defined variable  //
		//---------------------------------------------------------------------//
		if (clauseDefUse.numOfPaths != 0) {
			Set<String> symDiff = this.symmetricDifference(definitions, clauseDefUse.definitions);
			for (String s : symDiff) {
				if (!uses.contains(s))
					uses.add(s);
				clauseMap.get(work.clauseNode).uses.add(s);
			}
		}
		clauseDefUse.numOfPaths++;
		//---------------------------------------------------------------------//
		//---------------------------------------------------------------------//

		for (String use : uses) {
			if (!clauseDefUse.uses.contains(use))
				clauseDefUse.uses.add(use);
		}
		for (String def : definitions) {
			if (!clauseDefUse.definitions.contains(def))
				clauseDefUse.definitions.add(def);
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

	/** Resume those suspended works that can be resolved after resolving other function calls */
	public void resumeWorks(Map<Node, Set<State>> suspendedWorks, List<Node> resolvedClauses){

		assert(!suspendedWorks.isEmpty());
		final Set<State> pendingWorks  = this.getNextNormalPending(suspendedWorks, resolvedClauses);

		if (!pendingWorks.isEmpty()) {
			this.processAndAnnotate(suspendedWorks, pendingWorks, resolvedClauses, clauseMap);
		}
		else{
			// There is a cyclic dependence
			this.resumeUntilFixPoint(suspendedWorks, resolvedClauses);
		}
	}

	/** Traverses all the CFG annotating the DEF and USE sets and copy them to a map structure */
	public void processAndAnnotate(Map<Node, Set<State>> suspendedWorks, Set<State> pendingWorks,
								   List<Node> resolvedClauses, Map<Node,DefUseState> usedClauseMap)
	{
		processWorks(suspendedWorks, resolvedClauses, pendingWorks, usedClauseMap);

		Set<Node> allClauses = usedClauseMap.keySet();
		List<Node> suspendedClauses = getDifferentSuspendedClauses(suspendedWorks);
		for (Node clause : allClauses) {
			if (!resolvedClauses.contains(clause) && !suspendedClauses.contains(clause))
				resolvedClauses.add(clause);
		}
	}

	/** Resolves the scenario where all methods are blocking each other. Loops until a fix point is reached  */
	public void resumeUntilFixPoint(Map<Node,Set<State>> initialSuspendedWorks, List<Node> resolvedClauses)
	{
		final Map<Node, DefUseState> tempClauseMap = this.cloneMapDefUse(clauseMap);
		final List<Node> nonDefinitiveClauses = getDifferentSuspendedClauses(initialSuspendedWorks);

		Map<Node,Set<State>> suspendedWorks = new HashMap<>(initialSuspendedWorks);
		Set<State> pendingWorks = new HashSet<>();

		Map<Node,DefUseState> previousState = this.cloneMapDefUse(tempClauseMap);
		boolean fixPoint = false;
		while(!fixPoint) {

			while (!suspendedWorks.isEmpty()) {
				pendingWorks.addAll(this.getNextNormalPending(suspendedWorks, resolvedClauses));
				if (pendingWorks.isEmpty())
					pendingWorks.addAll(this.getNextCyclicPending(suspendedWorks));
				this.processAndAnnotate(suspendedWorks, pendingWorks, resolvedClauses, tempClauseMap);
			}

			fixPoint = true;
			for(Node clause : nonDefinitiveClauses)
			{
				if (!previousState.get(clause).equals(
						tempClauseMap.get(clause))) {
					fixPoint = false;
					suspendedWorks = new HashMap<>(initialSuspendedWorks);
					previousState = this.cloneMapDefUse(tempClauseMap);
					this.deleteGVUses(tempClauseMap, nonDefinitiveClauses);
					break;
				}
			}
		}
		initialSuspendedWorks.clear();
		this.clauseMapUnion(tempClauseMap);
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

	/** Clones a map copying the creating new objects for DEF and USE sets */
	public Map<Node,DefUseState> cloneMapDefUse(Map<Node,DefUseState> map)
	{
		final Map<Node, DefUseState> mapCopy = new HashMap<>();
		Set<Node> keys = map.keySet();
		for (Node key : keys)
		{
			DefUseState dus = map.get(key);
			mapCopy.put(key, new DefUseState(new HashSet<>(dus.uses), new HashSet<>(dus.definitions), dus.numOfPaths));
		}
		return mapCopy;
	}

	/** Return a list of States representing CFG nodes paused in a function call waiting for DEF and
	 * USE sets that can now be resumed because the corresponding method declaration has been resolved */
	public Set<State> getNextNormalPending(Map<Node, Set<State>> suspendedWorks, List<Node> resolvedClauses)
	{
		if (suspendedWorks.isEmpty())
			return Set.of();

		final Set<State> pendingWorks = new HashSet<>();
		for (Node resolvedClause : resolvedClauses)
		{
			Set<State> resumableWorks = suspendedWorks.remove(resolvedClause);
			if (resumableWorks != null) {
				pendingWorks.addAll(resumableWorks);
			}
		}
		return pendingWorks;
	}

	/** Return a list of States that must be resolved. These States represent CFG nodes paused in a function call
	 * waiting for another method declaration to be resolved. One of these sets is without the information it is
	 * waiting for because all the unresolved method declarations are blocking each other.
	 * @implNote The States are blocked in a call node, so this node is ignored and the processing of the method
	 * continues from the next instruction */
	public Set<State> getNextCyclicPending(Map<Node,Set<State>> suspendedWorks)
	{
		if (suspendedWorks.isEmpty())
			return Set.of();

		final Set<Node> suspendedNodes = suspendedWorks.keySet();
		final Node blockingClause = suspendedNodes.iterator().next();
		final Set<State> resumedWorks = suspendedWorks.remove(blockingClause); // States parados en nodo Call
		final Set<State> pendingWorks = new HashSet<>();

		// Nos saltamos el nodo Call y seguimos
		for(State work : resumedWorks)
		{
			final Node workNode = work.traversalNode;
			final Set<Node> nextNodes = ControlFlowTraverser.step(this.edg, workNode, LAST.Direction.Forwards);
			nextNodes.forEach(nextNode -> pendingWorks.add(new State(work.clauseNode, nextNode,
					new DefUseState(work.defUseState.uses, work.defUseState.definitions))));
		}
		return pendingWorks;
	}

	/** Deleted the uses of Global variables of a list of nodes in a map */
	public void deleteGVUses(Map<Node,DefUseState> map, List<Node> clauses)
	{
		final Set<Node> keys = map.keySet();
		for (Node key : keys)
			if (clauses.contains(key)) {
				DefUseState dus = map.get(key);
				DefUseState currentDUS = clauseMap.get(key);

				Set<String> removableUses = new HashSet<>();
				for (String s : dus.uses)
					if (!currentDUS.uses.contains(s))
						removableUses.add(s);

				dus.uses.removeAll(removableUses);
				dus.numOfPaths = clauseMap.get(key).numOfPaths;
			}
	}

	/** Copy all the items in the DEF and USE sets of a map to the
	 * corresponding items of the clauseMap map field */
	public void clauseMapUnion(Map<Node,DefUseState> map)
	{
		final Set<Node> keys = map.keySet();
		for (Node key : keys)
		{
			DefUseState clauseMapDUS = clauseMap.get(key);
			DefUseState dus = map.get(key);
			clauseMapDUS.numOfPaths += dus.numOfPaths;

			for (String use : dus.uses)
				if(!clauseMapDUS.uses.contains(use))
					clauseMapDUS.uses.add(use);

			for (String def : dus.definitions)
				if(!clauseMapDUS.definitions.contains(def))
					clauseMapDUS.definitions.add(def);
		}
	}

	// ----------------------------------------------------------------------------------- //
	// ------ CREATE NODES IN PARAMETER IN AND PARAMETER OUT OF METHOD DECLARATIONS ------ //
	// ----------------------------------------------------------------------------------- //

	/** Creates the global var DEF and USE corresponding nodes in the formal-in and
	 * formal-out nodes of every function definition contained in the clauseMap */
	public void createParamInOutNodes()
	{
		for( Node clause : clauseMap.keySet())
		{
			final DefUseState defUses = clauseMap.get(clause);
			this.createParamInOutNodes(clause, defUses);
		}
	}

	/** Creates the global var DEF and USE corresponding nodes in the formal-in and
	 * formal-out nodes of a single function definition given as an input parameter */
	public void createParamInOutNodes(Node clause, DefUseState defUses)
	{
		final Node paramIn = this.edg.getChild(clause, Node.Type.ParameterIn);
		final Set<Edge> paramInIncomingCFGEdges = edg.incomingEdgesOf(paramIn);
		paramInIncomingCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);

		// Transformation of Global Vars in ParameterIn Node
		this.addGlobalVarsStructure(clause, paramIn, paramInIncomingCFGEdges, defUses.uses, true);

		final Node paramOut = this.edg.getChild(clause, Node.Type.ParameterOut);
		final Set<Edge> paramOutIncomingCFGEdges = edg.incomingEdgesOf(paramOut);
		paramOutIncomingCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);

		// Transformation of Global Vars in ParameterOut Node
		this.addGlobalVarsStructure(clause, paramOut, paramOutIncomingCFGEdges, defUses.definitions, false);

	}

	/** Creates the corresponding Value and ControlFlow arcs when adding formal-in/value-in
	 * and formal-out/value-out nodes in function definitions or function calls */
	public void addGlobalVarsStructure(Node parent, Node container, Set<Edge> incomingEdges,
									   Set<String> set, boolean isDefinition)
	{
		Set<Node> hangingNodes = new HashSet<>();
		for (Edge incomingEdge : incomingEdges)
			hangingNodes.add(edg.getEdgeSource(incomingEdge));

		for (String item : set)
		{
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(parent.getInfo().getFile(), parent.getInfo().getClassName(),
					parent.getInfo().getLine(), true, "var", null);
			final int nodeId = LASTBuilder.addVariable(this.edg, container.getId(), null, item, false, isDefinition, !isDefinition, true, ldNodeInfo);
			final Node paramVar = this.edg.getNode(nodeId);

			final LDASTNodeInfo resultInfo = new LDASTNodeInfo(parent.getInfo().getFile(), parent.getInfo().getClassName(),
					parent.getInfo().getLine(), "var");
			final Node result = new Node("result", this.edg.getNextFictitiousId(), Node.Type.Result, "", resultInfo);

			edg.addVertex(result);
			edg.registerNodeResPair(paramVar, result);
			edg.addEdge(paramVar,result, Edge.Type.Value);
			edg.addEdge(paramVar,result, Edge.Type.ControlFlow);
			edg.addStructuralEdge(container, result);

			for (Node hangingNode : hangingNodes)
				edg.addEdge(hangingNode, paramVar, Edge.Type.ControlFlow);
			hangingNodes.clear();
			hangingNodes.add(result);
		}

		if (set.size() > 0)
		{
			for (Node hangingNode : hangingNodes)
				edg.addEdge(hangingNode, container, Edge.Type.ControlFlow);
			for (Edge incomingEdge : incomingEdges)
				edg.removeEdge(incomingEdge);
		}
	}

	// -------------------------------------------------------------------------- //
	// ------ CREATE NODES IN ARGUMENT IN AND ARGUMENT OUT OF METHOD CALLS ------ //
	// -------------------------------------------------------------------------- //

	/** Creates the global var DEF and USE corresponding nodes in the actual-in and
	 * actual-out nodes of a every function call of the program */
	public void createArgInOutNodes()
	{
		for( Node clause : clauseMap.keySet())
		{
			final Set<Edge> callEdges = edg.getEdges(clause, LAST.Direction.Backwards, Edge.Type.Call);
			final DefUseState defUses = clauseMap.get(clause);
			for (Edge edge : callEdges)
			{
				final Node callee = edg.getNodeFromRes(edg.getEdgeSource(edge));
				final Node call = edg.getParent(callee);
				final Node argIn = edg.getChild(call, Node.Type.ArgumentIn);
				final Node argOut = edg.getChild(call, Node.Type.ArgumentOut);

				// Edge from the callee result to the ArgOut node
				edg.addEdge(edg.getEdgeSource(edge), argOut, Edge.Type.Flow);

				final Set<Edge> incomingArgInCFGEdges = edg.getEdges(argIn, LAST.Direction.Backwards, Edge.Type.ControlFlow);
				final Set<Edge> incomingArgOutCFGEdges = edg.getEdges(argOut, LAST.Direction.Backwards, Edge.Type.ControlFlow);

				this.addGlobalVarsStructure(call, argIn, incomingArgInCFGEdges, defUses.uses, false);
				this.addGlobalVarsStructure(call, argOut, incomingArgOutCFGEdges, defUses.definitions, true);
			}
		}
	}

	// ---------------------------------------------------------------- //
	// ------ TRAVERSE THE CFG TO GENERATE FLOW DEPENDENCY EDGES ------ //
	// ---------------------------------------------------------------- //

	/** Select all the method clauses and traverse them generating flow edges */
	public void addEdges()
	{
		final Set<Node> clauses = clauseMap.keySet();

		for (Node clause : clauses)
			this.generateFlowEdges(clause);
	}

	/** Traverse every method definition generating flow
	 * dependencies between definitions and uses */
	public void generateFlowEdges(Node startNode)
	{
		final Node lastNode = edg.getResFromNode(startNode);
		final Set<EdgeGenState> doneWorks = new HashSet<>();
		final Set<EdgeGenState> pendingWorks = new HashSet<>();
		pendingWorks.add(new EdgeGenState(startNode, new HashMap<>(), new HashMap<>()));

		while(!pendingWorks.isEmpty())
		{
			final EdgeGenState work =  pendingWorks.iterator().next();
			final Node workNode = work.node;
			final Map<String, Node> workMap = work.defMap;
			pendingWorks.remove(work);

			if (workNode == lastNode)
				continue;
			if (doneWorks.contains(work))
				continue;

			if (workNode.getType() == Node.Type.Variable)
			{
				final Variable v = (Variable) workNode;
				if (v.isDeclaration())
					work.decMap.put(v.getName(),workNode);
				switch(v.getContext())
				{
					case Def_Use:
						final Node defNode = workMap.get(v.getName());
						if (defNode != null)
							edg.addEdge(edg.getResFromNode(defNode), edg.getResFromNode(workNode), Edge.Type.Flow);
						else
							throw new RuntimeException("The definition node cannot be null");
					case Definition:
						workMap.put(v.getName(),workNode);
						if (!v.isDeclaration()){
							 if (!v.isGlobal()) {
								 Node decNode = work.decMap.get(v.getName());
								 edg.addEdge(decNode, workNode, Edge.Type.Flow);
							 }
							 else{
							 	final Node clazz = edg.getAncestor(workNode, Node.Type.Module);
							 	final LASTBuilder.ClassInfo ci = (LASTBuilder.ClassInfo) clazz.getInfo().getInfo()[2];
							 	final Map<String,Node> varMap = ci.getVariables();
							 	final Node gVarDec = varMap.get(v.getName());
							 	edg.addEdge(gVarDec, workNode, Edge.Type.Flow);
							 }
						}

						break;
					case Use:
						final Node definition = workMap.get(v.getName());
						if (definition != null)
							edg.addEdge(edg.getResFromNode(definition), edg.getResFromNode(workNode), Edge.Type.Flow);
						else
							throw new RuntimeException("The definition node cannot be null");
						break;
					default:
						break;
				}
			}

			doneWorks.add(work);
			final Set<Node> nextNodes = ControlFlowTraverser.step(this.edg, workNode, LAST.Direction.Forwards);
			nextNodes.forEach(nextNode -> pendingWorks.add(new EdgeGenState(nextNode, new HashMap<>(workMap), new HashMap<>(work.decMap))));
		}
	}

	// ------------------------------------------------------------------- //
	// ------ ADD EDGES FORM NON-CALLED CLAUSES TO GLOBAL VARIABLES ------ //
	// ------------------------------------------------------------------- //

	/** Connects the paramIn global variables to the initial definition of the module for those
	 * clauses that are not called.
	 * @implNote Currently they are directly linked to the declaration of the data member,
	 * 			 but they should be linked to them through the constructor (if exists) */
	public void addOrphanClauseEdges()
	{
		final List<Node> clauses = edg.getNodes(Node.Type.Clause);
		clauses.removeIf(clause -> !edg.getEdges(clause, LAST.Direction.Backwards, Edge.Type.Call).isEmpty());

		for (Node clause : clauses) {
			final Node paramIn = edg.getChild(clause, Node.Type.ParameterIn);
			final Node module = edg.getAncestor(clause, Node.Type.Module);
			final LASTBuilder.ClassInfo ci = (LASTBuilder.ClassInfo) module.getInfo().getInfo()[2];

			final List<Node> children = edg.getChildren(paramIn);
			children.removeIf(node -> node.getType() == Node.Type.Result);

			for (Node child : children)
			{
				final Node varDef = ci.getVariables().get(child.getName());
				edg.addEdge(edg.getResFromNode(varDef), edg.getResFromNode(child), Edge.Type.Flow);
			}
		}
	}


	// METHOD FOR Set<String> : Algun archivo rollo utils/miscelanea?
	/** Given two sets, returns another set with their symmetric difference */
	public Set<String> symmetricDifference(Set<String> s1, Set<String> s2)
	{
		Set<String> s1Aux = new HashSet<>(s1);
		Set<String> s2Aux = new HashSet<>(s2);

		s1Aux.removeAll(s2);
		s2Aux.removeAll(s1);

		Set<String> res = new HashSet<>();
		res.addAll(s1Aux);
		res.addAll(s2Aux);

		return res;
	}

}