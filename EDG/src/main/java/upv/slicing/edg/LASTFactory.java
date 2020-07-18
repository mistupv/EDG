package upv.slicing.edg;

import upv.slicing.edg.LASTBuilder.Where;
import upv.slicing.edg.graph.LAST;
import upv.slicing.edg.graph.Node;

import java.util.*;

public abstract class LASTFactory {
	private LAST last;
	private final Deque<Branch> branches = new LinkedList<>();
	final protected Map<String, Integer> currentLabels = new HashMap<>();
	final protected Map<String, List<Integer>> unresolvedLabels = new HashMap<>();

	private <R> void processElements(List<R> elements)
	{
		for (int i = 0; i < elements.size(); i++)
			this.processElement(elements.get(i), i + 1, elements.size());
	}

	private void processElement(Object element, int index, int length)
	{
		final Map<String, Object> info = new HashMap<>();
		final Branch parent = branches.peekFirst();
		if (parent != null)
		{
			parent.setIndex(index);
			parent.setLength(length);
		}

		info.put("ancestors", branches);
		info.put("parent", parent);
		info.put("patternZone", isPatternZone(branches));

		this.processElement(element, info);
	}

	protected abstract void processElement(Object element, Map<String, Object> info);

	protected <R> LAST createLAST(Iterable<R> classes, LDASTNodeInfo info)
	{
		return this.createLAST(true, iterableToList(classes), info);
	}

	protected <R> LAST createLAST(boolean generateArcs, Iterable<R> classes, LDASTNodeInfo info)
	{
		final List<R> classes0 = iterableToList(classes);
		return this.createLAST(generateArcs, classes0, info);
	}

	protected <R> LAST createLAST(List<R> classes, LDASTNodeInfo info)
	{
		return this.createLAST(true, classes, info);
	}

	protected <R> LAST createLAST(boolean generateArcs, List<R> classes, LDASTNodeInfo info)
	{
		this.branches.clear();
		this.last = LASTBuilder.createLAST(info);
		this.processElements(classes);
		LASTBuilder.completeLAST(this.last);

		// Añadir en el EDG la info de metodos y fields
//		LASTBuilder.addInheritanceInfomation(this.last);
//		if (generateArcs)
//			LASTBuilder.generateDependencies(this.last);

		return this.last;
	}

	protected <R> void addModule(String name, Iterable<R> members, LDASTNodeInfo info)
	{
		final List<R> members0 = iterableToList(members);
		this.addModule(name, members0, info);
	}

	protected <R> void addModule(String name, List<R> members, LDASTNodeInfo info)
	{
		final int moduleId = LASTBuilder.addModule(this.last, name, info);

		this.branches.push(new Branch(moduleId, Node.Type.Module, info));
		this.processElements(members);
		this.branches.pop();
	}
	
	protected <R> void addRoutine(String name, List<R> clauses, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int routineId = LASTBuilder.addRoutine(this.last, parentId, where, name, info);

		this.branches.push(new Branch(routineId, Node.Type.Routine, info));
		this.processElements(clauses);
		this.branches.pop();
	}

	protected <R, S, T> void addClause(Iterable<R> parameters, S guard, T body, LDASTNodeInfo info)
	{
		final List<R> parameters0 = iterableToList(parameters);
		this.addClause(parameters0, guard, body, info);
	}

	protected <R, S, T> void addClause(List<R> parameters, S guard, T body, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final int clauseId = LASTBuilder.addClause(this.last, parentId, info);
		final Branch branch = new Branch(clauseId, Node.Type.Clause, info);
		this.branches.push(branch);

		currentLabels.clear(); // Clear Label list when generating a Clause

		branch.setWhere(Where.Parameters);
		this.processElements(parameters);
		branch.setWhere(Where.Guard);
		if (guard != null)
			this.processElement(guard, 1, 1);
		branch.setWhere(Where.Body);
		if (body != null)
			this.processElement(body, 1, 1);
		this.branches.pop();
	}

	protected <R, S, T> void addClause(Iterable<R> parameters, S guard, Iterable<T> expressions, LDASTNodeInfo info)
	{
		final List<R> parameters0 = iterableToList(parameters);
		final List<T> expressions0 = iterableToList(expressions);
		this.addClause(parameters0, guard, expressions0, info);
	}

	protected <R, S, T> void addClause(List<R> parameters, S guard, List<T> expressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final int clauseId = LASTBuilder.addClause(this.last, parentId, info);
		final Branch branch = new Branch(clauseId, Node.Type.Clause, info);
		this.branches.push(branch);

		branch.setWhere(Where.ParameterIn);
		branch.setWhere(Where.Parameters);
		this.processElements(parameters);
		branch.setWhere(Where.ParameterOut);
		branch.setWhere(Where.Guard);
		if (guard != null)
			this.processElement(guard, 1, 1);
		branch.setWhere(Where.Body);
		this.processElements(expressions);
		this.branches.pop();
	}

	protected void addVariable(String name, String varType, boolean declaration, boolean definition, boolean use, boolean global, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();

		LASTBuilder.addVariable(this.last, parentId, where, name, varType, declaration, definition, use, global, info);
	}

	protected void addLiteral(String value, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();

		LASTBuilder.addLiteral(this.last, parentId, where, value, info);
	}

	protected <R, S> void addEquality(R left, S right, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int equalityId = LASTBuilder.addEquality(this.last, parentId, where, info);

		this.branches.push(new Branch(equalityId, Node.Type.Equality, info));
		this.processElement(left, 1, 2);
		this.processElement(right, 2, 2);
		this.branches.pop();
	}

	protected <R, S> void addEquality(String operator, R left, S right, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int equalityId = LASTBuilder.addEquality(this.last, parentId, where, operator, info);

		this.branches.push(new Branch(equalityId, Node.Type.Equality, info));
		this.processElement(left, 1, 2);
		this.processElement(right, 2, 2);
		this.branches.pop();
	}

	protected <R> void addOperation(String operation, Iterable<R> operands, LDASTNodeInfo info)
	{
		final List<R> operands0 = iterableToList(operands);
		this.addOperation(operation, operands0, info);
	}

	protected <R> void addOperation(String operation, List<R> operands, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int operationId = LASTBuilder.addOperation(this.last, parentId, where, operation, info);

		this.branches.push(new Branch(operationId, Node.Type.Operation, info));
		this.processElements(operands);
		this.branches.pop();
	}

	protected <R> void addUnaryOperation(String operation, R expression, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int operationId = LASTBuilder.addOperation(this.last, parentId, where, operation, info);

		this.branches.push(new Branch(operationId, Node.Type.Operation, info));
		this.processElement(expression, 1, 1);
		this.branches.pop();
	}

	// Types
	protected <R, T> void addTypeCheck(R expression, T type, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int typeCheckId = LASTBuilder.addTypeCheck(this.last, parentId, where, info);

		this.branches.push(new Branch(typeCheckId, Node.Type.TypeCheck, info));
		this.processElement(expression, 1, 1);
		this.processElement(type, 2, 1);
		this.branches.pop();
	}

	protected <R, T> void addTypeTransformation(T type, R expression, LDASTNodeInfo info, boolean isEnclosedExpr)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int typeTransformId = LASTBuilder.addTypeTransformation(this.last, parentId, where, info, isEnclosedExpr);

		this.branches.push(new Branch(typeTransformId, Node.Type.TypeTransformation, info));
		this.processElement(type, 1, 1);
		this.processElement(expression, 2, 1);
		this.branches.pop();
	}

	protected void addType(String value, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();

		LASTBuilder.addType(this.last, parentId, where, value, info);
	}

	// Expressions
	protected <R> void addDataConstructor(Iterable<R> elements, LDASTNodeInfo info)
	{
		final List<R> elements0 = iterableToList(elements);
		this.addDataConstructor(elements0, info);
	}

	protected <R> void addDataConstructor(List<R> elements, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int dataConstructorId = LASTBuilder.addDataConstructor(this.last, parentId, where, info);

		this.branches.push(new Branch(dataConstructorId, Node.Type.DataConstructor, info));
		this.processElements(elements);
		this.branches.pop();
	}

	protected <R> void addList(Iterable<R> elements, LDASTNodeInfo info)
	{
		final List<R> elements0 = iterableToList(elements);
		this.addList(elements0, info);
	}

	protected <R> void addList(List<R> elements, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int listId = LASTBuilder.addList(this.last, parentId, where, info);

		this.branches.push(new Branch(listId, Node.Type.List, info));
		this.processElements(elements);
		this.branches.pop();
	}

	protected <R, S> void addDataConstructorAccess(R dataConstructor, S access, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int dataConstructorId = LASTBuilder.addDataConstructorAccess(this.last, parentId, where, info);

		this.branches.push(new Branch(dataConstructorId, Node.Type.DataConstructorAccess, info));
		this.processElement(dataConstructor, 1, 2);
		this.processElement(access, 2, 2);
		this.branches.pop();
	}

	protected <R, S> void addFieldAccess(R structure, S field, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int fieldAccessId = LASTBuilder.addFieldAccess(this.last, parentId, where, info);

		this.branches.push(new Branch(fieldAccessId, Node.Type.FieldAccess, info));
		this.processElement(structure, 1, 2);
		this.processElement(field, 2, 2);
		this.branches.pop();
	}

	protected <R> void addBlock(Iterable<R> expressions, LDASTNodeInfo info)
	{
		final List<R> expressions0 = iterableToList(expressions);
		this.addBlock(expressions0, info);
	}

	protected <R> void addBlock(List<R> expressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int blockId = LASTBuilder.addBlock(this.last, parentId, where, info);

		this.branches.push(new Branch(blockId, Node.Type.Block, info));
		this.processElements(expressions);
		this.branches.pop();
	}

	protected <R, S, T> void addIf(R condition, Iterable<S> thenExpressions, Iterable<T> elseExpressions, LDASTNodeInfo info)
	{
		final List<S> thenExpressions0 = iterableToList(thenExpressions);
		final List<T> elseExpressions0 = iterableToList(elseExpressions);
		this.addIf(condition, thenExpressions0, elseExpressions0, info);
	}

	protected <R, S, T> void addIf(R condition, List<S> thenExpressions, List<T> elseExpressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int ifId = LASTBuilder.addIf(this.last, parentId, where, info);
		final Branch branch = new Branch(ifId, Node.Type.If, info);
		this.branches.push(branch);

		branch.setWhere(Where.Condition);
		this.processElement(condition, 1, 1);
		branch.setWhere(Where.Then);
		this.processElements(thenExpressions);
		branch.setWhere(Where.Else);
		this.processElements(elseExpressions);
		this.branches.pop();
	}

	protected <R, S> void addSwitch(R selector, Iterable<S> cases, LDASTNodeInfo info)
	{
		final List<S> cases0 = iterableToList(cases);
		this.addSwitch(selector, cases0, info);
	}

	protected <R, S> void addSwitch(R selector, List<S> cases, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int switchId = LASTBuilder.addSwitch(this.last, parentId, where, info);
		final Branch branch = new Branch(switchId, Node.Type.Switch, info);
		this.branches.push(branch);

		branch.setWhere(Where.Selector);
		if (selector != null)
			this.processElement(selector, 1, 1);
		branch.setWhere(Where.Cases);
		this.processElements(cases);
		this.branches.pop();
	}

	protected <R, S, T> void addCase(R selectable, S guard, T body, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final int caseId = LASTBuilder.addCase(this.last, parentId, info);
		final Branch branch = new Branch(caseId, Node.Type.Case, info);
		this.branches.push(branch);

		branch.setWhere(Where.Selectable);
		this.processElement(selectable, 1, 1);
		branch.setWhere(Where.Guard);
		if (guard != null)
			this.processElement(guard, 1, 1);
		branch.setWhere(Where.Body);
		if (body != null)
			this.processElement(body, 1, 1);
		this.branches.pop();
	}

	protected <R, S, T> void addCase(R selectable, S guard, Iterable<T> expressions, LDASTNodeInfo info)
	{
		final List<T> expressions0 = iterableToList(expressions);
		this.addCase(selectable, guard, expressions0, info);
	}

	protected <R, S, T> void addCase(R selectable, S guard, List<T> expressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final int caseId = LASTBuilder.addCase(this.last, parentId, info);
		final Branch branch = new Branch(caseId, Node.Type.Case, info);
		this.branches.push(branch);

		branch.setWhere(Where.Selectable);
		if (selectable != null)
			this.processElement(selectable, 1, 1);
		branch.setWhere(Where.Guard);
		if (guard != null)
			this.processElement(guard, 1, 1);
		branch.setWhere(Where.Body);
		this.processElements(expressions);
		this.branches.pop();
	}

	protected <R> void addDefaultCase(R body, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final int defaultCaseId = LASTBuilder.addDefaultCase(this.last, parentId, info);
		final Branch branch = new Branch(defaultCaseId, Node.Type.DefaultCase, info);
		this.branches.push(branch);

		branch.setWhere(Where.Body);
		if (body != null)
			this.processElement(body, 1, 1);
		this.branches.pop();
	}

	protected <R, S, T> void addDefaultCase(Iterable<T> expressions, LDASTNodeInfo info)
	{
		final List<T> expressions0 = iterableToList(expressions);
		this.addDefaultCase(expressions0, info);
	}

	protected <R> void addDefaultCase(List<R> expressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final int defaultCaseId = LASTBuilder.addDefaultCase(this.last, parentId, info);
		final Branch branch = new Branch(defaultCaseId, Node.Type.DefaultCase, info);
		this.branches.push(branch);

		branch.setWhere(Where.Body);
		this.processElements(expressions);
		this.branches.pop();
	}

	protected <R, S, T> void addCall(R scope, S function, Iterable<T> arguments, LDASTNodeInfo info)
	{
		final List<T> arguments0 = iterableToList(arguments);
		this.addCall(scope, function, arguments0, info);
	}

	protected <R, S, T> void addCall(R scope, S function, List<T> arguments, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int callId = LASTBuilder.addCall(this.last, parentId, where, info);
		final Branch branch = new Branch(callId, Node.Type.Call, info);
		this.branches.push(branch);

		branch.setWhere(Where.Scope);
		if (scope != null)
			this.processElement(scope, 1, 2);
		branch.setWhere(Where.Name);
		if (function != null)
			this.processElement(function, 2, 2);
		branch.setWhere(Where.ArgumentIn);
		branch.setWhere(Where.Arguments);
		this.processElements(arguments);
		branch.setWhere(Where.ArgumentOut);

		this.branches.pop();
	}

	protected <R, S, T> void addStaticCall(String scopeName, S function, List<T> arguments, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int callId = LASTBuilder.addCall(this.last, parentId, where, info);
		final Branch branch = new Branch(callId, Node.Type.Call, info);
		this.branches.push(branch);

		branch.setWhere(Where.Scope);
		this.addType(scopeName, info);
		branch.setWhere(Where.Name);
		if (function != null)
			this.processElement(function, 2, 2);
		branch.setWhere(Where.ArgumentIn);
		branch.setWhere(Where.Arguments);
		this.processElements(arguments);
		branch.setWhere(Where.ArgumentOut);

		this.branches.pop();
	}


	protected <R, S> void addListComprehension(Iterable<R> restrictions, S value, LDASTNodeInfo info)
	{
		final List<R> restrictions0 = iterableToList(restrictions);
		this.addListComprehension(restrictions0, value, info);
	}

	protected <R, S> void addListComprehension(List<R> restrictions, S value, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int listComprehensionId = LASTBuilder.addListComprehension(this.last, parentId, where, info);
		final Branch branch = new Branch(listComprehensionId, Node.Type.ListComprehension, info);
		this.branches.push(branch);

		branch.setWhere(Where.Restrictions);
		this.processElements(restrictions);
		branch.setWhere(Where.Value);
		if (value != null)
			this.processElement(value, 1, 1);
		this.branches.pop();
	}

	protected <R, S> void addGenerator(R left, S right, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int generatorId = LASTBuilder.addGenerator(this.last, parentId, where, info);

		this.branches.push(new Branch(generatorId, Node.Type.Generator, info));
		if (left != null)
			this.processElement(left, 1, 2);
		if (right != null)
			this.processElement(right, 2, 2);
		this.branches.pop();
	}

	protected <R, S> void addFilter(R filter, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int filterId = LASTBuilder.addFilter(this.last, parentId, where, info);

		this.branches.push(new Branch(filterId, Node.Type.Filter, info));
		this.processElement(filter, 1, 1);
		this.branches.pop();
	}

	// LOOPS
	protected <R, S, T> void addForLoop(Iterable<T> initialization, R condition, Iterable<S> bodyExpressions, Iterable<T> update, LDASTNodeInfo info)
	{
		final List<T> initExpressions = iterableToList(initialization);
		final List<S> bodyExpressions0 = iterableToList(bodyExpressions);
		final List<T> updateExpressions = iterableToList(update);
		this.addForLoop(initExpressions, condition, bodyExpressions0, updateExpressions, info);
	}

	protected <R, S, T> void addForLoop(List<T> initExpressions, R condition, List<S> bodyExpressions, List<T> updateExpressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int loopId = LASTBuilder.addForLoop(this.last, parentId, where, info, true);
		final Branch branch = new Branch(loopId, Node.Type.FLoop, info);
		this.branches.push(branch);


		branch.setWhere(Where.Init);
		this.processElements(initExpressions);
		branch.setWhere(Where.Condition);
		this.processElement(condition, 1, 1);
		branch.setWhere(Where.Body);
		this.processElements(bodyExpressions);
		branch.setWhere(Where.Update);
		this.processElements(updateExpressions);
		this.branches.pop();
	}

	protected <R, S, T> void addCondLoop(R condition, Iterable<S> bodyExpressions, LDASTNodeInfo info)
	{
		final List<S> bodyExpressions0 = iterableToList(bodyExpressions);
		this.addCondLoop(condition, bodyExpressions0, info);
	}

	protected <R, S, T> void addCondLoop(R condition, List<S> bodyExpressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int loopId = LASTBuilder.addCondLoop(this.last, parentId, where, info, false);
		final Branch branch = new Branch(loopId, Node.Type.CLoop, info);
		this.branches.push(branch);

		branch.setWhere(Where.Condition);
		this.processElement(condition, 1, 1);
		branch.setWhere(Where.Body);
		this.processElements(bodyExpressions);
		this.branches.pop();
	}

	protected <R, S, T> void addRepeatLoop(R condition, Iterable<S> bodyExpressions, LDASTNodeInfo info)
	{
		final List<S> bodyExpressions0 = iterableToList(bodyExpressions);
		this.addRepeatLoop(condition, bodyExpressions0, info);
	}

	protected <R, S, T> void addRepeatLoop(R condition, List<S> bodyExpressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int loopId = LASTBuilder.addRepeatLoop(this.last, parentId, where, info, false);
		final Branch branch = new Branch(loopId, Node.Type.RLoop, info);
		this.branches.push(branch);

		branch.setWhere(Where.Body);
		this.processElements(bodyExpressions);
		branch.setWhere(Where.Condition);
		this.processElement(condition, 1, 1);
		this.branches.pop();
	}

	protected <R, S, T> void addForeach(R varDeclaration, S iterableExpr, Iterable<T> bodyExpressions, LDASTNodeInfo info)
	{
		final List<T> bodyExpressions0 = iterableToList(bodyExpressions);
		this.addForeach(varDeclaration, iterableExpr, bodyExpressions0, info);
	}

	protected <R, S, T> void addForeach(R varDeclaration, S iterableExpr, List<T> bodyExpressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int foreachId = LASTBuilder.addForeach(this.last, parentId, where, info);
		final Branch branch = new Branch(foreachId, Node.Type.Foreach, info);
		this.branches.push(branch);

		branch.setWhere(Where.Iterator);
		this.addGenerator(varDeclaration, iterableExpr, info);
		// TODO: Body node must be inserted after inserting the generator
		branch.setWhere(Where.Body);
		this.processElements(bodyExpressions);
		this.branches.pop();

	}

	protected <R> void addEnclosed(R expression, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int enclosedId = LASTBuilder.addEnclosed(this.last, parentId, where, info);

		this.branches.push(new Branch(enclosedId, Node.Type.Enclosed, info));
		this.processElement(expression, 1, 1);
		this.branches.pop();
	}

	protected <R> void addReturn(R expression, int dstId, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int returnId = LASTBuilder.addReturn(this.last, parentId, where, dstId, info);

		this.branches.push(new Branch(returnId, Node.Type.Return, info));
		if (expression != null)
			this.processElement(expression, 1, 1);
		this.branches.pop();
	}

	protected void addBreak(int dstId, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();

		LASTBuilder.addBreak(this.last, parentId, where, dstId, info);
	}

	protected void addContinue(int dstId, String labelText, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();

		final int continueId = LASTBuilder.addContinue(this.last, parentId, where, dstId, info);

		if (dstId == -1)
		{
			final List<Integer> unresolvedContinues = unresolvedLabels.get(labelText);
			final List<Integer> continueList = new LinkedList<>();
			if (unresolvedContinues != null)
				continueList.addAll(unresolvedContinues);
			continueList.add(continueId);
			unresolvedLabels.put(labelText, continueList);
		}
	}

	// Exceptions
	protected <R, S, T> void addExHandler(Iterable<R> tryExpressions, Iterable<S> catchClauses, Iterable<T> finallyExpressions, LDASTNodeInfo info)
	{
		final List<R> tryExpressions0 = iterableToList(tryExpressions);
		final List<S> catchClauses0 = iterableToList(catchClauses);
		final List<T> finallyExpressions0 = iterableToList(finallyExpressions);
		this.addExHandler(tryExpressions0, catchClauses0, finallyExpressions0, info);
	}

	protected <R, S, T> void addExHandler(List<R> tryExpressions, List<S> catchClauses, List<T> finallyExpressions, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int tryId = LASTBuilder.addExHandler(this.last, parentId, where, info);
		final Branch branch = new Branch(tryId, Node.Type.ExHandler, info);
		this.branches.push(branch);

		branch.setWhere(Where.Try);
		this.processElements(tryExpressions);
		branch.setWhere(Where.Catch);
		this.processElements(catchClauses);
		branch.setWhere(Where.Finally);
		this.processElements(finallyExpressions);
		this.branches.pop();

	}

	protected <R, S, T> void addCatchClause(R parameter, S guard, Iterable<T> catchBlock, LDASTNodeInfo info)
	{
		final List<T> catchBlock0 = iterableToList(catchBlock);
		this.addCatchClause(parameter, guard, catchBlock0, info);
	}

	protected <R, S, T> void addCatchClause(R parameter, S guard, List<T> catchBlock, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int catchClauseId = LASTBuilder.addCatchClause(this.last, parentId, where, info);
		final Branch branch = new Branch(catchClauseId, Node.Type.CatchClause, info);
		this.branches.push(branch);

		branch.setWhere(Where.Parameters);
		this.processElement(parameter, 1, 1);
		branch.setWhere(Where.Body);
		this.processElements(catchBlock);
		this.branches.pop();
	}

	protected <R> void addThrow(R expression, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int throwId = LASTBuilder.addThrow(this.last, parentId, where, info);

		this.branches.push(new Branch(throwId, Node.Type.Throw, info));
		this.processElement(expression, 1, 1);
		this.branches.pop();
	}

	// Reference
	protected void addSuperReference(String value, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();

		LASTBuilder.addSuperReference(this.last, parentId, where, value, info);
	}

	protected void addThisReference(String value, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();

		LASTBuilder.addSuperReference(this.last, parentId, where, value, info);
	}

	protected void addReference(String value, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();

		LASTBuilder.addReference(this.last, parentId, where, value, info);
	}

	protected <R> void addLabel(String labelText, R labeledExpr, LDASTNodeInfo info)
	{
		final Branch parent = this.branches.peek();
		final int parentId = parent.getNodeId();
		final Where where = parent.getWhere();
		final int labelId = LASTBuilder.addLabel(this.last, parentId, where, labelText, info);

		currentLabels.put(labelText, labelId);
		resolveLabel(labelText, labelId);

		this.branches.push(new Branch(labelId, Node.Type.Label, info));
		this.processElement(labeledExpr, 1, 1);
		this.branches.pop();

	}

	private static <R> List<R> iterableToList(Iterable<R> iterable)
	{
		final List<R> list = new LinkedList<>();
		for (R element : iterable)
			list.add(element);
		return list;
	}

	private static boolean isPatternZone(Deque<Branch> ancestors)
	{
		for (Branch ancestor : ancestors)
		{
			switch (ancestor.getNodeType())
			{
				case Clause:
					if (ancestor.getWhere() == Where.Parameters)
						return true;
					break;
				case Case:
					if (ancestor.getWhere() == Where.Selectable)
						return true;
					break;
				case Equality:
				case Generator:
					if (ancestor.getIndex() == 1)
						return true;
					break;
				default:
					break;
			}
		}
		return false;
	}

	private void resolveLabel(String labelText, Integer jumpId)
	{
		if (!unresolvedLabels.containsKey(labelText))
			return;
		for (Integer id : unresolvedLabels.get(labelText))
		{
			Node jumpNode = last.getNode(id);
			jumpNode.setName("continue " + jumpId);
		}
	}

	public static class Branch {
		private final int nodeId;
		private final Node.Type nodeType;
		private final LDASTNodeInfo ldNodeInfo;
		private Where where;
		private int index;
		private int length;

		public Branch(int nodeId, Node.Type nodeType, LDASTNodeInfo ldNodeInfo)
		{
			this.nodeId = nodeId;
			this.nodeType = nodeType;
			this.ldNodeInfo = ldNodeInfo;
		}

		public int getNodeId()
		{
			return this.nodeId;
		}

		public Node.Type getNodeType()
		{
			return this.nodeType;
		}

		public LDASTNodeInfo getLdASTNodeInfo()
		{
			return this.ldNodeInfo;
		}

		public Where getWhere()
		{
			return this.where;
		}

		public int getIndex()
		{
			return this.index;
		}

		public int getLength()
		{
			return this.length;
		}

		public void setWhere(Where where)
		{
			this.where = where;
		}

		public void setIndex(int index)
		{
			this.index = index;
		}

		public void setLength(int length)
		{
			this.length = length;
		}
	}
}
