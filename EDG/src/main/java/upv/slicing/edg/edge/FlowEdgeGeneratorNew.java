package upv.slicing.edg.edge;

import upv.slicing.edg.LASTBuilder;
import upv.slicing.edg.LASTBuilder.ClassInfo;
import upv.slicing.edg.LDASTNodeInfo;
import upv.slicing.edg.graph.*;
import upv.slicing.edg.graph.Variable;
import upv.slicing.edg.traverser.ControlFlowTraverser;

import java.util.*;
import java.util.stream.Collectors;

public class FlowEdgeGeneratorNew extends EdgeGenerator {

	private static class DefUseState {
		private int numOfPaths = 0;
		private final Set<VarTypeInfo> uses;
		private final Set<VarTypeInfo> definitions;

		private DefUseState()
		{
			this(new HashSet<>(), new HashSet<>());
		}
		private DefUseState(Set<VarTypeInfo> uses, Set<VarTypeInfo> definitions)
		{
			this.uses = uses.stream().collect(Collectors.toSet());
			this.definitions = definitions.stream().collect(Collectors.toSet());
		}
		private DefUseState(Set<VarTypeInfo> uses, Set<VarTypeInfo> definitions, int paths)
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
	private static class VarTypeInfo
	{
		String variable;
		String type;

		private VarTypeInfo(String var, String type)
		{
			this.variable = var;
			this.type = type;
		}
		public String toString() {
			return this.type + " " + this.variable;
		}

		public boolean equals(Object o) {
			if (o == this)
				return true;
			if (!(o instanceof VarTypeInfo))
				return false;

			final VarTypeInfo vti = (VarTypeInfo) o;
			return Objects.equals(this.variable, vti.variable) &&
					Objects.equals(this.type, vti.type);
		}

		public int hashCode() {
			return Objects.hash(variable, type);
		}

	}

	public FlowEdgeGeneratorNew(EDG edg) {
		super(edg);
	}

	public void generate() {
		this.resolveParamInOutClauses(); // Traverses the CFG and fills clauseMap with DEF and USE sets for each clause
		this.createParamInOutNodes(); // Add defined and used GV to param-in and param-out nodes modifying the CFG
		this.createArgInOutNodes(); // Add defined and used GV to arg-in and arg-out of call nodes modifying the CFG
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
		boolean pendingCFGPath = false;
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
				final Node callee = edg.getChild(workNode, Node.Type.Callee);
				final Node calleeResNode = edg.getResFromNode(callee);
				final Set<Edge> callEdges = edg.getEdges(calleeResNode, LAST.Direction.Forwards, Edge.Type.Call);
				for( Edge callEdge : callEdges ){	// TODO: This may change with polymorphism
					final Node calledClause = edg.getEdgeTarget(callEdge);
					if (work.clauseNode == calledClause)
						continue;
					if (resolvedClauses.contains(calledClause)) {
						final Node nameNode = edg.getChild(callee, Node.Type.Name);
						final String funName = edg.getChild(nameNode, 0).getName();
						final DefUseState defUses;
						if (funName.equals("<constructor>"))
							defUses = new DefUseState();
						else
							defUses = usedClauseMap.get(calledClause);
						this.treatCall(work, defUses);
						continue;
					}

					this.suspendWork(suspendedWorks,work,calledClause);
					suspended = true;
				}
				if (suspended) {
					pendingCFGPath = true;
					suspended = false;
					continue;
				}
			}
			doneWorks.add(work);
			final Set<Node> nextNodes = ControlFlowTraverser.step(this.edg, workNode, LAST.Direction.Forwards);
			nextNodes.forEach(nextNode -> pendingWorks.add(new State(work.clauseNode, nextNode, new DefUseState(work.defUseState.uses, work.defUseState.definitions))));
		}
		return pendingCFGPath;
	}

	/** Add a global variable to the corresponding DEF or USE set if necessary */
	public void treatVariable(Variable v, DefUseState workState)
	{
		VarTypeInfo var = new VarTypeInfo(v.getName(),v.getStaticType());
		switch (v.getContext())
		{
			case Definition:
				if (!workState.definitions.contains(var))
					workState.definitions.add(var);
				break;
			case Use:
				if (!workState.uses.contains(var) && !workState.definitions.contains(var))
					workState.uses.add(var);
				break;
			case Def_Use:
				if (!workState.uses.contains(var) && !workState.definitions.contains(var))
					workState.uses.add(var);
				if (!workState.definitions.contains(var))
					workState.definitions.add(var);
				break;
			default:
				break;
		}
	}

	/** Adds the list of definitions and uses to a current State.
	 * @apiNote This method is used to add the DEF of USE sets of a method call in the CFG traversal */
	public void treatCall(State work, DefUseState callDefUses)
	{
		Set<VarTypeInfo> uses = work.defUseState.uses;
		Set<VarTypeInfo> definitions = work.defUseState.definitions;

		for (VarTypeInfo use : callDefUses.uses)
			if (!uses.contains(use) && !definitions.contains(use))
				uses.add(use);

		for (VarTypeInfo def : callDefUses.definitions)
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

		Set<VarTypeInfo> uses = work.defUseState.uses;
		Set<VarTypeInfo> definitions = work.defUseState.definitions;
		//---------------------------------------------------------------------//
		// Added for method paths that does not define every defined variable  //
		//---------------------------------------------------------------------//
		if (clauseDefUse.numOfPaths != 0) {
			Set<VarTypeInfo> symDiff = this.symmetricDifference(definitions, clauseDefUse.definitions);
			for (VarTypeInfo s : symDiff) {
				if (!uses.contains(s))
					uses.add(s);
				clauseMap.get(work.clauseNode).uses.add(s);
			}
		}
		clauseDefUse.numOfPaths++;
		//---------------------------------------------------------------------//
		//---------------------------------------------------------------------//

		for (VarTypeInfo use : uses) {
			if (!clauseDefUse.uses.contains(use))
				clauseDefUse.uses.add(use);
		}
		for (VarTypeInfo def : definitions) {
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

				Set<VarTypeInfo> removableUses = new HashSet<>();
				for (VarTypeInfo s : dus.uses)
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

			for (VarTypeInfo use : dus.uses)
				if(!clauseMapDUS.uses.contains(use))
					clauseMapDUS.uses.add(use);

			for (VarTypeInfo def : dus.definitions)
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

			final List<Node> modules = edg.getNodes(Node.Type.Module);
			this.unfoldObjectParameters(clause, modules);
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

		final Node outNode;
		final String routineName = edg.getParent(clause).getName();
		if (routineName.equals("<constructor>"))
			outNode = this.edg.getResFromNode(clause);
		else
			outNode = this.edg.getChild(clause, Node.Type.ParameterOut);
		final Set<Edge> paramOutIncomingCFGEdges = edg.incomingEdgesOf(outNode);
		paramOutIncomingCFGEdges.removeIf(edge -> edge.getType() != Edge.Type.ControlFlow);

		// Transformation of Global Vars in ParameterOut Node or Result structures
		this.addGlobalVarsStructure(clause, outNode, paramOutIncomingCFGEdges, defUses.definitions, false);
	}

	/** Creates the corresponding Value and ControlFlow arcs when adding formal-in/value-in
	 * and formal-out/value-out nodes in function definitions or function calls */
	public void addGlobalVarsStructure(Node parent, Node container, Set<Edge> incomingEdges,
									   Set<VarTypeInfo> set, boolean isDefinition)
	{
		Set<Node> hangingNodes = new HashSet<>();
		for (Edge incomingEdge : incomingEdges)
			hangingNodes.add(edg.getEdgeSource(incomingEdge));

		for (VarTypeInfo item : set) {
			final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(parent.getInfo().getFile(), parent.getInfo().getClassName(),
					parent.getInfo().getLine(), true, "var", null);
			final int nodeId;
			switch (container.getType()){
				case Result:	// Constructors add their DM in the result Node
					nodeId = LASTBuilder.addVariable(this.edg, container.getId(), null, item.variable,
							item.type, false, isDefinition, !isDefinition, false, ldNodeInfo);
					break;
				case ParameterIn:
				case ParameterOut:
				case ArgumentIn:
				case ArgumentOut:
					nodeId = LASTBuilder.addVariable(this.edg, container.getId(), null,  item.variable,
							item.type, false, isDefinition, !isDefinition, true, ldNodeInfo);
					break;
				default:
					final Node objectVar;
					if (container.getType() == Node.Type.PolymorphicCall)
						objectVar = edg.getParent(container);
					else
						objectVar = container;

					nodeId = LASTBuilder.addVariable(this.edg, container.getId(), null, objectVar.getName() + "." + item.variable,
							item.type, false, isDefinition, !isDefinition, true, ldNodeInfo);
					break;
			}

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

		if (set.size() > 0) {
			for (Node hangingNode : hangingNodes)
				edg.addEdge(hangingNode, container, Edge.Type.ControlFlow);
			for (Edge incomingEdge : incomingEdges)
				edg.removeEdge(incomingEdge);
		}
	}

	/** Limit of unfolding levels for objects parameters */
	final int kLimit = 1;

	/** Creates the structure tree structure for object parameters with all
	 * their data members using a k-limiting approach */
	public void unfoldObjectParameters(Node clause, List<Node> modules){
		int unfoldingLvl = 0;
		final Node parameters = edg.getChild(clause, Node.Type.Parameters);
		final List<Node> parameterChildren = edg.getChildren(parameters);
		parameterChildren.removeIf(child -> child.getType() == Node.Type.Result);

		// Unfold object parameters
		for (Node child : parameterChildren){
			if (!(child instanceof Variable))
				throw new RuntimeException("All parameters must be variable nodes");

			final Variable param = (Variable) child;
			List<Variable> unfoldingSet = new LinkedList<>();
			unfoldingSet.add(param);

			while(unfoldingLvl < kLimit) {
				List<Variable> newDM = new LinkedList<>();
				for (Variable var : unfoldingSet)
					newDM.addAll(this.unfoldObject(var, modules));
				unfoldingSet = newDM;
				unfoldingLvl++;
			}
		}

		// Copy structure to parameter out node
		for (Node child : parameterChildren){
			final Variable param = (Variable) child;
			final String paramType = param.getStaticType();
			final Node paramOut = edg.getChild(clause, Node.Type.ParameterOut);
			if (!DynamicTypesGenerator.isPrimitiveType(paramType)){
				final Node paramOutVarNode = this.copyVariable(param, paramOut, false, false);
				this.copySubtree(param, paramOutVarNode, false);
			}
		}
	}

	public List<Variable> unfoldObject(Variable param, List<Node> modules){
		final String paramType = param.getStaticType();
		if (DynamicTypesGenerator.isPrimitiveType(paramType))
			return List.of();

		final List<Variable> dataMembers = new LinkedList<>();
		final List<String> possibleTypes = edg.getChildrenClasses(paramType);

		final List<Node> polymorphicNodes = new LinkedList<>();

		for (String type : possibleTypes){
			final Node module = this.getModuleByName(type, modules);
			if (module == null) // Source code unavailable in the implementation
				continue;

			final ClassInfo ci = (ClassInfo) module.getInfo().getInfo()[2];
			final Map<String, Node> variables = ci.getVariables();

			final Node polymorphicNode = this.addPolymorphicNode(param, type);
			polymorphicNodes.add(polymorphicNode);
			this.addPolymorphicCFGEdges(param, polymorphicNode);

			for (String varName : variables.keySet()) {
				final Variable var = (Variable) variables.get(varName);
				final Node DMNode = this.addDefinitionVarCopy(polymorphicNode, param, param.getName()+ "." + varName, var.getStaticType());
				dataMembers.add((Variable) DMNode);
			}
		}
		this.updatePolymorphicCFGEdges(param, polymorphicNodes);

		return dataMembers;
	}

	// TODO: THIS METHOD IS ALSO IMPLEMENTED IN InterproceduralEdgeGenerator.java
	public Node getModuleByName(String name, List<Node> modules){
		for (Node module : modules)
			if (module.getName().equals(name))
				return module;
		return null;
	}

	/** Duplicates the incoming CFG edges of an object parameter to the Polymorphic node given */
	public void addPolymorphicCFGEdges(Node parent, Node child){
		Set<Edge> incomingEdges = edg.getEdges(parent, LAST.Direction.Backwards, Edge.Type.ControlFlow);
		for (Edge edge : incomingEdges)
			edg.addEdge(edg.getEdgeSource(edge), child, Edge.Type.ControlFlow);
	}

	/** Deletes the incoming CFG edges of an object parameter and adds an
	 * edge from the Polymorphic node to the parameter. */
	public void updatePolymorphicCFGEdges(Node parent, List<Node> polymorphicNodes){
		Set<Edge> incomingEdges = edg.getEdges(parent, LAST.Direction.Backwards, Edge.Type.ControlFlow);
		for(Edge e : incomingEdges)
			edg.removeEdge(e);
		for (Node node : polymorphicNodes)
			edg.addEdge(node, parent, Edge.Type.ControlFlow);
	}

	public Node copyVariable(Variable var, Node container, boolean isDefinition, boolean isGlobal){
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(var.getInfo().getFile(), var.getInfo().getClassName(),
				var.getInfo().getLine(), true, "var", null);
		final int nodeId = LASTBuilder.addVariable(this.edg, container.getId(), null,
				var.getName(), var.getStaticType(), false, isDefinition, !isDefinition, isGlobal, ldNodeInfo);
		final Node newVarNode = edg.getNode(nodeId);
		final LDASTNodeInfo resultInfo = new LDASTNodeInfo(var.getInfo().getFile(), var.getInfo().getClassName(),
				var.getInfo().getLine(), "var");
		final Node result = new Node("result", this.edg.getNextFictitiousId(), Node.Type.Result, "", resultInfo);

		edg.addVertex(result);
		edg.registerNodeResPair(newVarNode, result);
		edg.addEdge(newVarNode, result, Edge.Type.Value);
		edg.addEdge(newVarNode, result, Edge.Type.ControlFlow);
		edg.addStructuralEdge(container, result);

		Set<Edge> incomingEdges = edg.getEdges(container, LAST.Direction.Backwards, Edge.Type.ControlFlow);
		for (Edge edge : incomingEdges) {
			edg.addEdge(edg.getEdgeSource(edge), newVarNode, Edge.Type.ControlFlow);
			edg.removeEdge(edge);
		}
		edg.addEdge(result, container, Edge.Type.ControlFlow);
		return newVarNode;
	}

	/** Copy the whole subtree of origin in container, adding the corresponding CFG into the new structure */
	public void copySubtree(Node origin, Node container, boolean isDefinition){
		final List<Node> children = edg.getChildren(origin);
		children.removeIf(child -> child.getType() == Node.Type.Result);

		final Set<Edge> incomingContainerEdges = edg.getEdges(container, LAST.Direction.Backwards, Edge.Type.ControlFlow);
		boolean isPolymorphicLvl = false;

		for (Node varNode : children) {
			final Node newContainer;
			if (varNode.getType() == Node.Type.PolymorphicCall) {
				isPolymorphicLvl = true;
				newContainer = this.addPolymorphicNode(container, varNode.getName());
				// The control flow is independent for each Polymorphic Type
				for (Edge incoming : incomingContainerEdges)
					edg.addEdge(edg.getEdgeSource(incoming), newContainer, Edge.Type.ControlFlow);
				edg.addEdge(newContainer, container, Edge.Type.ControlFlow);
			}
			else {
				Variable var = (Variable) varNode;
				newContainer = this.copyVariable(var, container, isDefinition, false);
			}
			this.copySubtree(varNode, newContainer, isDefinition);
		}

		if (isPolymorphicLvl)
			for (Edge e : incomingContainerEdges)
				edg.removeEdge(e);
	}

	// -------------------------------------------------------------------------- //
	// ------ CREATE NODES IN ARGUMENT IN AND ARGUMENT OUT OF METHOD CALLS ------ //
	// -------------------------------------------------------------------------- //

	/** Creates the global var DEF and USE corresponding nodes in the actual-in and
	 * actual-out nodes at every function call of the program */
	public void createArgInOutNodes()
	{
		Set<Node> pendingResClauses = new HashSet<>();
		for (Node clause : clauseMap.keySet()) {
			if (edg.getParent(clause).getName().equals("<constructor>"))
				this.addConstructorCallStructures(clause, pendingResClauses);
			else
				this.addCallStructures(clause);
		}

		// Methods that return object creations
		while (!pendingResClauses.isEmpty()){
			final Node clause = pendingResClauses.iterator().next();
			pendingResClauses.remove(clause);
			this.addResCallStructure(clause, pendingResClauses);
		}
	}

	/** Create the argument out nodes of constructors calls. This argument out nodes are associated to the result
	 * node of the constructor call. If the constructor call is inside a return statement this structure is propagated
	 * across the result node of the method, and the method is annotated for possible further propagation */
	public void addConstructorCallStructures(Node clause, Set<Node> pendingResClauses)
	{
		final Set<Edge> callEdges = edg.getEdges(clause, LAST.Direction.Backwards, Edge.Type.Call);
		final DefUseState defUses = clauseMap.get(clause);

		for (Edge edge : callEdges) {
			final Node callee = edg.getNodeFromRes(edg.getEdgeSource(edge));
			final Node call = edg.getParent(callee);
			final Node argOut = edg.getChild(call, Node.Type.ArgumentOut);

			final Node callParent = edg.getParent(call);
			final Node callResult = edg.getResFromNode(call);

			// Edge from the callee result to the ArgOut node
			edg.addEdge(edg.getEdgeSource(edge), argOut, Edge.Type.Flow);

			// Objects created but not directly assigned
			if (callParent.getType() != Node.Type.Equality) {
				for (VarTypeInfo def : defUses.definitions)
					this.addDefinitionVarCopy(callResult, call, "call." + def.variable, def.type);
				if (callParent.getType() == Node.Type.Return){
					final Node callClause = edg.getAncestor(callParent, Node.Type.Clause);
					final Node callClauseRes = edg.getResFromNode(callClause);
					this.copyChildren(callResult, callClauseRes, false);
					pendingResClauses.add(callClause);
				}
			}
			else // Objects created and directly assigned
			{
				final Node objectVar = edg.getChild(callParent, Node.Type.Pattern);
				for (VarTypeInfo def : defUses.definitions) {
					final Node varDataMember = this.addDefinitionVarCopy(objectVar, objectVar, objectVar.getName() + "." + def.variable, def.type);
					final Node dataMemberOutVar = this.addDefinitionVarCopy(callResult, objectVar, objectVar.getName() + "." + def.variable, def.type);
					edg.addEdge(edg.getResFromNode(dataMemberOutVar), edg.getResFromNode(varDataMember), Edge.Type.Value);
				}
			}
		}
	}

	/** Create the argument out nodes of non-constructors calls, this nodes are located at the argument out node
	 * but can be inside a polymorphic variable node if the caller is an object variable. */
	public void addCallStructures(Node clause)
	{
		final Set<Edge> callEdges = edg.getEdges(clause, LAST.Direction.Backwards, Edge.Type.Call);
		final DefUseState defUses = clauseMap.get(clause);
		final Node parameters = edg.getChild(clause, Node.Type.Parameters);

		for (Edge edge : callEdges) {
			final Node callee = edg.getNodeFromRes(edg.getEdgeSource(edge));
			final Node call = edg.getParent(callee);

			final Node arguments = edg.getChild(call, Node.Type.Arguments);
			this.unfoldObjectArguments(arguments, parameters);

			final Node scope = edg.getChild(callee, Node.Type.Scope);
			final Node objectVar =  edg.getScopeLeaf(scope);
			final Node argOut = edg.getChild(call, Node.Type.ArgumentOut);

			// Edge from the callee result to the ArgOut node
			edg.addEdge(edg.getEdgeSource(edge), argOut, Edge.Type.Flow);

			final Set<Edge> incomingInCFGEdges, incomingOutCFGEdges;
			final Node containerIn, containerOut;

			// The caller is not an object variable
			if (objectVar == null || objectVar.getType() == Node.Type.Reference) {
				final Node argIn = edg.getChild(call, Node.Type.ArgumentIn);

				incomingInCFGEdges = edg.getEdges(argIn, LAST.Direction.Backwards, Edge.Type.ControlFlow);
				incomingOutCFGEdges = edg.getEdges(argOut, LAST.Direction.Backwards, Edge.Type.ControlFlow);

				containerIn = argIn;
				containerOut = argOut;
			} else { // The caller is an object variable. Only variables and castings considered
				final String className = clause.getInfo().getClassName();
				final Node calleeNameNode = edg.getChild(callee, Node.Type.Name);
				final String calleeName = edg.getChildren(calleeNameNode).get(0).getName();

				containerIn = this.addPolymorphicNode(objectVar, className + "." + calleeName);
				incomingInCFGEdges = edg.getEdges(objectVar, LAST.Direction.Backwards, Edge.Type.ControlFlow);
				edg.addEdge(containerIn, objectVar, Edge.Type.ControlFlow);

				if (defUses.definitions.isEmpty()) {
					containerOut = argOut;
					incomingOutCFGEdges = edg.getEdges(argOut, LAST.Direction.Backwards, Edge.Type.ControlFlow);
				}
				else {
					final Node scopeVarOut = this.getVarOut(argOut, objectVar);;
					containerOut = defUses.definitions.isEmpty()? argOut : this.addPolymorphicNode(scopeVarOut, className + "." + calleeName);
					incomingOutCFGEdges = edg.getEdges(scopeVarOut, LAST.Direction.Backwards, Edge.Type.ControlFlow);
					edg.addEdge(containerOut, scopeVarOut, Edge.Type.ControlFlow);
				}
			}
			this.addGlobalVarsStructure(call, containerIn, incomingInCFGEdges, defUses.uses, false);
			this.addGlobalVarsStructure(call, containerOut, incomingOutCFGEdges, defUses.definitions, true);
		}
	}

	public void unfoldObjectArguments(Node argumentsNode, Node parametersNode){
		final List<Node> argsList = edg.getChildren(argumentsNode);
		argsList.removeIf(arg -> arg.getType() == Node.Type.Result);

		final List<Node> paramList = edg.getChildren(parametersNode);
		paramList.removeIf(param -> param.getType() == Node.Type.Result);

		for (int index = 0; index < argsList.size(); index++){
			final Node arg = argsList.get(index);
			final List<String> possibleTypes = this.getTypes(arg);
			if (DynamicTypesGenerator.isPrimitiveType(possibleTypes.get(0)))
				continue;

			if (arg.getType() != Node.Type.Variable)
				continue;

			final Node param = paramList.get(index);

			if (possibleTypes.contains("StaticType")){
				// TODO: Possibility to join arguments and parameters. Interprocedural arcs.
				this.copySubtree(param, arg, false);
			}
			else{
				// Unfold object in arguments node
				final List<Node> polymorphicParams = edg.getChildren(param);
				// The incoming edges to the arg now goes to the new polymorphicNode
				final Set<Edge> incomingEdges = edg.getEdges(arg, LAST.Direction.Backwards, Edge.Type.ControlFlow);

				for (Node polymorphicParam : polymorphicParams) {
					if (possibleTypes.contains(polymorphicParam.getName())) {
						// This is for variables
						final Node polymorphicArg = this.addPolymorphicNode(arg, polymorphicParam.getName());
						for (Edge edge : incomingEdges)
							edg.addEdge(edg.getEdgeSource(edge), polymorphicArg, Edge.Type.ControlFlow);
						edg.addEdge(polymorphicArg, arg, Edge.Type.ControlFlow);
						this.copySubtree(polymorphicParam, polymorphicArg, false);
					}
				}

				for (Edge edge : incomingEdges)
					edg.removeEdge(edge);

				// Unfold object in argument out node
				final Node argumentOut = edg.getChild(edg.getParent(argumentsNode), Node.Type.ArgumentOut);
				final Node outArgVar = this.copyVariable((Variable) arg, argumentOut, true, false);
				this.copySubtree(arg, outArgVar, true);
			}
		}
	}

	private List<String> getTypes(Node arg)
	{
		switch(arg.getType()){
			case Literal:	// Only for primitive types
				final LDASTNodeInfo ldNodeInfo = arg.getInfo();
				return List.of(ldNodeInfo.getConstruction());
			case Variable: // May be object or primitive type
				Variable argument = (Variable) arg;
				return argument.getDynamicTypes();
			case Call:
				String returnType = (String) arg.getInfo().getInfo()[0];
				// Return the primitive type
				if (DynamicTypesGenerator.isPrimitiveType(returnType))
					return List.of(returnType);
				// Return all the possible dynamic types of the call return type
				return edg.getChildrenClasses(returnType);
			default: // TODO: the rest of types are non contemplated
				throw new RuntimeException("This argument type is not considered");
		}
	}


	/** Completes those methods that returns an object creation treat the recursive return.
	 * This method only add the corresponding structure to the result node */
	public void addResCallStructure(Node clause, Set<Node> pendingResClauses){
		final Set<Edge> callEdges = edg.getEdges(clause, LAST.Direction.Backwards, Edge.Type.Call);
		final Node clauseRes = edg.getResFromNode(clause);
		for (Edge edge : callEdges) {
			final Node callee = edg.getNodeFromRes(edg.getEdgeSource(edge));
			final Node call = edg.getParent(callee);
			final Node callResult = edg.getResFromNode(call);

			final Node callParent = edg.getParent(call);

			if (callParent.getType() != Node.Type.Equality) {
				this.copyChildren(clauseRes, callResult, true);
				if (callParent.getType() == Node.Type.Return) {
					final Node callClause = edg.getAncestor(callParent, Node.Type.Clause);
					final Node callClauseRes = edg.getResFromNode(callClause);
					this.copyChildren(callResult, callClauseRes, false);
					pendingResClauses.add(callClause);
				}
			}
			else {
				final Node objectVar = edg.getChild(callParent, Node.Type.Pattern);
				final List<Node> clauseResChildren = edg.getChildren(clauseRes);
				clauseResChildren.removeIf(child -> child.getType() == Node.Type.Result);

				for (Node varNode : clauseResChildren) {
					Variable var = (Variable) varNode;
					String varName = var.getName().substring(var.getName().indexOf(".") + 1);
					final Node varDataMember = this.addDefinitionVarCopy(objectVar, objectVar, objectVar.getName() + "." + varName, var.getStaticType());
					final Node dataMemberOutVar = this.addDefinitionVarCopy(callResult, objectVar, objectVar.getName() + "." + varName, var.getStaticType());
					edg.addEdge(edg.getResFromNode(dataMemberOutVar), edg.getResFromNode(varDataMember), Edge.Type.Value);
				}
			}
		}
	}

	/** Copy the children of a node "origin" (usually a set of data members) to another node "container" to
	 * replicate the structure. The set of data members can be definitions or uses according to the
	 * "isDefinition" parameter. */
	public void copyChildren(Node origin, Node container, boolean isDefinition){
		final List<Node> children = edg.getChildren(origin);
		children.removeIf(child -> child.getType() == Node.Type.Result);

		final List<Node> containerChildren = edg.getChildren(container);

		for (Node varNode : children) {
			Variable var = (Variable) varNode;

			if (this.isContained(var.getName(), containerChildren))
				continue;

			this.copyVariable(var, container, isDefinition, true);
		}
	}

	/** Returns true if a node with the name "name" is contained in a list of nodes */
	public boolean isContained(String name, List<Node> list){
		for (Node node : list)
			if (node.getName().equals(name))
				return true;
		return false;
	}

	/** Adds a polymorphic call node to a object variable node (object unfolding process) */
	public Node addPolymorphicNode(Node container, String nodeName)
	{
		final LDASTNodeInfo info = new LDASTNodeInfo(container.getInfo().getFile(), container.getInfo().getClassName(),
				container.getInfo().getLine(), "polymorphic call");
		final Node polymorphicCallNode = new Node (nodeName, this.edg.getNextId(), Node.Type.PolymorphicCall, nodeName, info);

		edg.addVertex(polymorphicCallNode);
		edg.addEdge(container, polymorphicCallNode, Edge.Type.Structural);
		return polymorphicCallNode;
	}

	/** Replicates the scope var node in the ArgumentOut node */
	public Node getVarOut(Node argOut, Node objectVar)
	{
		List<Node> children = edg.getChildren(argOut);
		children.removeIf(node -> node.getType() == Node.Type.Result);

		for (Node child : children)
			if (child.getName().equals(objectVar.getName()))
				return child;

		return this.addDefinitionVarCopy(argOut, objectVar, objectVar.getName(), ((Variable) objectVar).getStaticType());
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

		while(!pendingWorks.isEmpty()) {
			final EdgeGenState work =  pendingWorks.iterator().next();
			final Node workNode = work.node;
			final Map<String, Node> workMap = work.defMap;
			pendingWorks.remove(work);

			if (workNode == lastNode)
				continue;
			if (doneWorks.contains(work))
				continue;

			if (workNode.getType() == Node.Type.Variable) {
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
								 if (decNode != null)
								 	edg.addEdge(decNode, workNode, Edge.Type.Flow);
								 else
									 // If decNode is null the variable is a definition without declaration,
									 // this happens in a constructor data member at a result node to avoid
									 // considering it as a global variable
								 	assert(edg.getParent(v).getType() == Node.Type.Result);
							 }
							 else{
							 	final Node clazz = edg.getAncestor(workNode, Node.Type.Module);
							 	final LASTBuilder.ClassInfo ci = (LASTBuilder.ClassInfo) clazz.getInfo().getInfo()[2];
							 	final Map<String,Node> varMap = ci.getVariables();
							 	final Node gVarDec = varMap.get(v.getName());
							 	if (gVarDec != null) // Non declared variables
							 		edg.addEdge(gVarDec, workNode, Edge.Type.Flow);
							 }
						}

						break;
					case Use:
						final Node definition = workMap.get(v.getName());
						if (definition != null)
							edg.addEdge(edg.getResFromNode(definition), edg.getResFromNode(workNode), Edge.Type.Flow);
//						else
//							throw new RuntimeException("The definition node cannot be null");
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
				if (varDef != null)
					edg.addEdge(edg.getResFromNode(varDef), edg.getResFromNode(child), Edge.Type.Flow);
			}
		}
	}


	// METHOD FOR Set<String> : Algun archivo rollo utils/miscelanea?
	/** Given two sets, returns another set with their symmetric difference */
	public Set<VarTypeInfo> symmetricDifference(Set<VarTypeInfo> s1, Set<VarTypeInfo> s2)
	{
		Set<VarTypeInfo> s1Aux = new HashSet<>(s1);
		Set<VarTypeInfo> s2Aux = new HashSet<>(s2);

		s1Aux.removeAll(s2);
		s2Aux.removeAll(s1);

		Set<VarTypeInfo> res = new HashSet<>();
		res.addAll(s1Aux);
		res.addAll(s2Aux);

		return res;
	}

	/** Add a new node, copy of a variable node varNode with varName and varType as a child of a container node */
	public Node addDefinitionVarCopy(Node container, Node varNode, String varName, String varType)
	{
		final LDASTNodeInfo ldNodeInfo = new LDASTNodeInfo(varNode.getInfo().getFile(), varNode.getInfo().getClassName(),
				varNode.getInfo().getLine(), true, "var", null);
		final int nodeId = LASTBuilder.addVariable(this.edg, container.getId(), null,
				varName, varType, false, true, false, true, ldNodeInfo);
		final Node newVarNode = edg.getNode(nodeId);
		final LDASTNodeInfo resultInfo = new LDASTNodeInfo(varNode.getInfo().getFile(), varNode.getInfo().getClassName(),
				varNode.getInfo().getLine(), "var");
		final Node result = new Node("result", this.edg.getNextFictitiousId(), Node.Type.Result, "", resultInfo);

		edg.addVertex(result);
		edg.registerNodeResPair(newVarNode, result);
		edg.addEdge(newVarNode, result, Edge.Type.Value);
		edg.addEdge(newVarNode, result, Edge.Type.ControlFlow);
		edg.addStructuralEdge(container, result);

		if (container.getType() == Node.Type.Variable)
			edg.addEdge(edg.getResFromNode(container), result, Edge.Type.TotalDefinition);

		Set<Edge> incomingEdges = edg.getEdges(container, LAST.Direction.Backwards, Edge.Type.ControlFlow);
		for (Edge edge : incomingEdges){
			edg.addEdge(edg.getEdgeSource(edge), newVarNode, Edge.Type.ControlFlow);
			edg.removeEdge(edge);
		}
		edg.addEdge(result, container, Edge.Type.ControlFlow);
		return newVarNode;
	}
}