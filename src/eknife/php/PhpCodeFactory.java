package eknife.php;

import java.io.File;
import java.util.LinkedList;
import java.util.List;

import edg.graph.EDG;
import edg.graph.Node;
import edg.graph.NodeInfo;
import edg.graph.VariableInfo;
import edg.traverser.EDGTraverser;
import misc.Misc;

public class PhpCodeFactory
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	public static void createPhpFile(File outputFile, EDG edg)
	{
		PhpCodeFactory.createPhpFile(outputFile, edg, null);
	}
	public static void createPhpFile(File outputFile, EDG edg, List<Node> slice)
	{
		final PhpCodeFactory phpFactory = new PhpCodeFactory(edg, slice);
		final String text = phpFactory.generate();

		Misc.write(outputFile, text, false);
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private final EDG edg;
	private final List<Node> slice;
	private int indent = 0;

	private PhpCodeFactory(EDG edg, List<Node> slice)
	{
		this.edg = edg;
		this.slice = slice;
	}

	private String generate()
	{
		String code = "";
		final Node root = this.edg.getRootNode();

		// Modules
		final List<Node> modules = EDGTraverser.getChildren(root);
		for (Node module : modules)
		{
			if (this.slice != null && !this.slice.contains(module))
				continue;

			code += this.parseModule(module);
		}

		return "<?php\n" + code + "?>";
	}

	// Structure
	private String parseModule(Node module)
	{
		String code = "";
		final String identation = this.getIdentation();
		final String name = module.getData().getName();
		final List<String> members = this.parseMembers(module);

		code += identation + "class " + name + "\n";
		code += identation + "{\n";
		for (String member : members)
			code += member + "\n\n";
		code += identation + "}\n";

		return code;
	}
	private List<String> parseMembers(Node module)
	{
		final List<Node> members = EDGTraverser.getChildren(module);
		final List<String> members0 = new LinkedList<String>();

		this.indent++;
		for (Node member : members)
		{
			if (this.slice != null && !this.slice.contains(member))
				continue;

			if (member.getData().getType() != NodeInfo.Type.Routine)
				members0.add(this.parseGlobalVariable(member));
			else
				members0.add(this.parseRoutine(member));
		}
		this.indent--;

		return members0;
	}
	private String parseGlobalVariable(Node globalVariable)
	{
		String code = "";
		final String identation = this.getIdentation();

		final NodeInfo.Type nodeType = globalVariable.getData().getType();
		final Node expression = nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(globalVariable, 0) : globalVariable;
		final Node variable = (expression.getData().getType() == NodeInfo.Type.Equality) ? EDGTraverser.getChild(expression, 0) : expression;
		final NodeInfo info = variable.getData();
		final String modifiers = "pending";
		final String name = info.getName();

		code += identation + modifiers + " " + name + ";";

		return code;
	}
	private String parseRoutine(Node routine)
	{
		String code = "";
		final String identation = this.getIdentation();
		final Node clause = EDGTraverser.getChild(routine, 0);

		// Callable declaration
		final String modifiers = "pending";
		final String name = routine.getData().getName();

		code += identation + modifiers + " " + name;

		// Parameters
		final Node parametersNode = EDGTraverser.getChild(clause, 0);
		final List<Node> parameters = EDGTraverser.getChildren(parametersNode);
		String parametersStr = "";

		code += "(";
		for (Node parameter : parameters)
			parametersStr += this.parseParameter(parameter) + ", ";
		if (!parametersStr.isEmpty())
			parametersStr = parametersStr.substring(0, parametersStr.length() - 2);
		code += parametersStr + ")\n";

		// Statements
		final Node body = EDGTraverser.getChild(clause, 2);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final List<String> statementsList = this.parseStatements(bodyChildren, false);

		code += identation + "{\n";
		for (String statementStr : statementsList)
			code += statementStr + "\n";
		code += identation + "}";

		return code;
	}
	private String parseParameter(Node parameter)
	{
		final NodeInfo info = parameter.getData();
		final String name = this.slice != null && !this.slice.contains(parameter) ? "fresh" : info.getName();

		return name;
	}

	// Statements
	private List<String> parseStatements(List<Node> nodes, boolean transformUnused)
	{
		final List<String> statementsList = new LinkedList<String>();

		this.indent++;
		for (Node node : nodes)
		{
			if (!transformUnused && this.slice != null && !this.slice.contains(node))
				continue;

			statementsList.add(this.parseStatement(node));
		}
		if (!nodes.isEmpty() && statementsList.isEmpty())
			statementsList.add(";");
		this.indent--;

		return statementsList;
	}
	private String parseStatement(Node node)
	{
		if (this.slice != null && !this.slice.contains(node))
			return ";";

		final NodeInfo.Type nodeType = node.getData().getType();
		final Node statement = nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(node, 0) : node;
		final NodeInfo.Type statementType = statement.getData().getType();

		switch (statementType)
		{
			case If:
				return this.parseIf(statement);
			case Switch:
				return this.parseSwitch(statement);
			case Case:
				return this.parseCase(statement);
			case Loop:
				return this.parseLoop(statement);
			case Return:
				return this.parseReturn(statement);
			default:
				return this.parseExpression(statement);
		}
	}
	private String parseIf(Node _if)
	{
		String code = "";
		final String identation = this.getIdentation();

		// Condition
		final Node condition = EDGTraverser.getChild(_if, 0);
		final List<Node> conditionChildren = EDGTraverser.getChildren(condition);
		final Node conditionNode = conditionChildren.get(0);
		final String conditionStr = this.parseExpression(conditionNode);

		code += identation + "if (" + conditionStr + ")\n";

		// Then
		final Node then = EDGTraverser.getChild(_if, 1);
		final List<Node> thenChildren = EDGTraverser.getChildren(then);
		final List<String> thenStatements = this.parseStatements(thenChildren, false);
		String thenStatementsStr = "";

		code += thenStatements.size() == 1 ? "" : identation + "{\n";
		for (String thenStatement : thenStatements)
			thenStatementsStr += thenStatement + "\n";
		if (!thenStatementsStr.isEmpty())
			thenStatementsStr = thenStatementsStr.substring(0, thenStatementsStr.length() - 1);
		code += thenStatementsStr;
		code += thenStatements.size() == 1 ? "" : identation + "}";

		// Else
		final Node _else = EDGTraverser.getChild(_if, 2);
		final List<Node> elseChildren = EDGTraverser.getChildren(_else);
		final List<String> elseStatements = this.parseStatements(elseChildren, false);
		String elseStatementsStr = "";

		if (!elseStatements.isEmpty())
		{
			code += identation + "\nelse\n";
			code += identation + (elseStatements.size() == 1 ? "\n" : "{\n");
			for (String elseStatement : elseStatements)
				elseStatementsStr += elseStatement + "\n";
			if (!elseStatementsStr.isEmpty())
				elseStatementsStr = elseStatementsStr.substring(0, elseStatementsStr.length() - 1);
			code += elseStatementsStr;
			code += identation + (elseStatements.size() == 1 ? "" : "}");
		}

		return code;
	}
	private String parseSwitch(Node _switch)
	{
		String code = "";
		final String identation = this.getIdentation();

		// Selector
		final Node selectors = EDGTraverser.getChild(_switch, 0);
		final Node selectorNode = EDGTraverser.getChild(selectors, 0);
		final String selector = this.parseExpression(selectorNode);

		code += identation + "switch (" + selector + ")\n";

		// Cases
		final Node cases = EDGTraverser.getChild(_switch, 1);
		final List<Node> casesChildren = EDGTraverser.getChildren(cases);
		final List<String> statements = this.parseStatements(casesChildren, false);

		for (String statement : statements)
			code += statement;

		return code;
	}
	private String parseCase(Node _case)
	{
		String code = "";
		final String identation = this.getIdentation();

		// Label
		final Node selectables = EDGTraverser.getChild(_case, 0);
		final List<Node> selectablesChildren = EDGTraverser.getChildren(selectables);
		final Node selectableNode = selectablesChildren.get(0);
		final String label = this.parseExpression(selectableNode);

		code += identation + label + ":\n";

		// Statements
		final Node body = EDGTraverser.getChild(_case, 2);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final List<String> statements = this.parseStatements(bodyChildren, false);

		for (String statement : statements)
			code += statement;

		return code;
	}
	private String parseLoop(Node loop)
	{
		String code = "";
		final String identation = this.getIdentation();

		// Condition
		final Node condition = EDGTraverser.getChild(loop, 0);
		final List<Node> conditionChildren = EDGTraverser.getChildren(condition);
		final Node conditionNode = conditionChildren.get(0);
		final String conditionExpression = this.parseExpression(conditionNode);

		code += identation + "while (" + conditionExpression + ")\n";

		// Body
		final Node body = EDGTraverser.getChild(loop, 1);
		final List<Node> bodyChildren = EDGTraverser.getChildren(body);
		final List<String> bodyStatements = this.parseStatements(bodyChildren, false);

		for (String bodyStatement : bodyStatements)
			code += bodyStatement;

		return code;
	}
	private String parseReturn(Node _return)
	{
		final String identation = this.getIdentation();

		final List<Node> returnChildren = EDGTraverser.getChildren(_return);
		if (returnChildren.isEmpty())
			return identation + "return;";

		final Node returnChild = returnChildren.get(0);
		final String returnExpression = this.parseExpression(returnChild);

		return identation + "return " + returnExpression + ";";
	}

	// Expressions
	private List<String> parseExpressions(List<Node> nodes, boolean transformUnused)
	{
		final List<String> expressionsList = new LinkedList<String>();

		for (Node node : nodes)
		{
			if (!transformUnused && this.slice != null && !this.slice.contains(node))
				continue;

			expressionsList.add(this.parseExpression(node));
		}
		if (!nodes.isEmpty() && expressionsList.isEmpty())
			expressionsList.add("null");

		return expressionsList;
	}
	private String parseExpression(Node node)
	{
		if (this.slice != null && !this.slice.contains(node))
			return "";

		final NodeInfo.Type nodeType = node.getData().getType();
		final Node expression = nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(node, 0) : node;
		final NodeInfo.Type expressionType = expression.getData().getType();

		switch (expressionType)
		{
			case Equality:
				return this.parseEquality(expression);
			case Call:
				return this.parseCall(expression);
			case DataConstructor:
				return this.parseDataConstructor(expression);
			case Operation:
				return this.parseOperation(expression);
			case DataConstructorAccess:
				return this.parseDataConstructorAccess(expression);
			case Variable:
				return this.parseVariable(expression);
			case Literal:
				return this.parseLiteral(expression);
			default:
				throw new RuntimeException("Expression type not contemplated: " + expressionType);
		}
	}
	private String parseEquality(Node equality)
	{
		final Node target = EDGTraverser.getChild(equality, 0);
		final Object[] info = target.getData().getInfo().getInfo();

		if (info == null || info.length == 0)
			return this.parseDefinition(equality);
		return this.parseDeclaration(equality);
	}
	private String parseDeclaration(Node declaration)
	{
		final String identation = this.getIdentation();

		final NodeInfo.Type nodeType = declaration.getData().getType();
		final Node expression = nodeType == NodeInfo.Type.Expression ? EDGTraverser.getChild(declaration, 0) : declaration;
		final Node variable = (expression.getData().getType() == NodeInfo.Type.Equality) ? EDGTraverser.getChild(expression, 0) : expression;
		final NodeInfo info = variable.getData();
		final String name = info.getName();

		return identation + name + ";";
	}
	private String parseDefinition(Node definition)
	{
		final String identation = this.getIdentation();

		final Node target = EDGTraverser.getChild(definition, 0);
		final Node value = EDGTraverser.getChild(definition, 1);

		final String targetExpr = this.parseExpression(target);
		final String valueExpr = this.parseExpression(value);

		if (targetExpr.equals(""))
			return identation + valueExpr + ";";
		return identation + targetExpr + " = " + valueExpr + ";";
	}
	private String parseDataConstructor(Node dataConstructor)
	{
		String code = "";

		final List<Node> elements = EDGTraverser.getChildren(dataConstructor);
		final List<String> expressions = this.parseExpressions(elements, true);

		code += "array(";
		for (String expression : expressions)
			code += expression + ", ";
		if (!code.isEmpty())
			code = code.substring(0, code.length() - 2);
		code += ");";

		return code;
	}
	private String parseCall(Node call)
	{
		String code = "";

		final Node callee = EDGTraverser.getChild(call, 0);
		final Node scopeNode = EDGTraverser.getChild(callee, 0);
		final Node scope = EDGTraverser.getChild(scopeNode, 0);
		final Node nameNode = EDGTraverser.getChild(callee, 1);
		final Node name = EDGTraverser.getChild(nameNode, 0);
		final String scopeExpression = this.parseExpression(scope);
		final String nameExpression = this.parseExpression(name);
		final String scopeText = scope.getData().getName();
		final String nameText = name.getData().getName();

		if (scopeExpression.equals("") || nameExpression.equals(""))
			code += "funundef";
		else if (nameText.equals("__constructor"))
			code += "new " + scopeText;
		else
			code += scopeText + "->" + nameText;

		String argumentsStr = "";
		final Node arguments = EDGTraverser.getChild(call, 1);
		final List<String> argumentsList = new LinkedList<String>();
		final List<Node> argumentsChildren = EDGTraverser.getChildren(arguments);
		argumentsList.addAll(this.parseExpressions(argumentsChildren, true));

		for (String argument : argumentsList)
			argumentsStr += argument + ", ";
		if (!argumentsStr.isEmpty())
			argumentsStr = argumentsStr.substring(0, argumentsStr.length() - 2);
		code += "(" + argumentsStr + ")";

		return code;
	}
	private String parseOperation(Node operation)
	{
		final String sign = operation.getData().getName();
		final List<Node> operands = EDGTraverser.getChildren(operation);
		final Node expression = operands.get(0);
		final String firstExpression = this.parseExpression(expression);

		switch (operands.size())
		{
			case 1:
				return sign + " " + firstExpression;
			case 2:
				final String secondExpression = this.parseExpression(operands.get(1));
				return firstExpression + " " + sign + " " + secondExpression;
			default:
				throw new RuntimeException("Operation arity not contemplated: " + operands.size());
		}
	}
	private String parseDataConstructorAccess(Node dataConstructorAccess)
	{
		final Node dataConstructor = EDGTraverser.getChild(dataConstructorAccess, 0);
		final Node access = EDGTraverser.getChild(dataConstructorAccess, 1);

		final String dataConstructorExpr = this.parseExpression(dataConstructor);
		final String accessExpr = this.parseExpression(access);

		return dataConstructorExpr + "[" + accessExpr + "]";
	}

	// Variables & literals
	private String parseVariable(Node variable)
	{
		final VariableInfo info = (VariableInfo) variable.getData();
		if (info.isDeclaration())
			return this.parseDeclaration(variable);

		final String value = info.getName();
		return value;
	}
	private String parseLiteral(Node literal)
	{
		final NodeInfo info = literal.getData();
		final String value = info.getName();

		return value;
	}

	// Auxiliaries
	private String getIdentation()
	{
		String indentation = "";

		for (int indent = 0; indent < this.indent; indent++)
			indentation += "\t";

		return indentation;
	}
}