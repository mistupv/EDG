package eknife.php;

// Generated from PHPParser.g4 by ANTLR 4.7
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class PHPParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		SeaWhitespace=1, HtmlText=2, PHPStart=3, HtmlScriptOpen=4, HtmlStyleOpen=5, 
		HtmlComment=6, HtmlDtd=7, HtmlOpen=8, Shebang=9, Error=10, PHPStartInside=11, 
		HtmlClose=12, HtmlSlashClose=13, HtmlSlash=14, HtmlEquals=15, HtmlStartQuoteString=16, 
		HtmlStartDoubleQuoteString=17, HtmlHex=18, HtmlDecimal=19, HtmlSpace=20, 
		HtmlName=21, ErrorInside=22, PHPStartInsideQuoteString=23, HtmlEndQuoteString=24, 
		HtmlQuoteString=25, ErrorHtmlQuote=26, PHPStartDoubleQuoteString=27, HtmlEndDoubleQuoteString=28, 
		HtmlDoubleQuoteString=29, ErrorHtmlDoubleQuote=30, ScriptText=31, ScriptClose=32, 
		PHPStartInsideScript=33, StyleBody=34, PHPEnd=35, Whitespace=36, MultiLineComment=37, 
		SingleLineComment=38, ShellStyleComment=39, Abstract=40, Array=41, As=42, 
		BinaryCast=43, BoolType=44, BooleanConstant=45, Break=46, Callable=47, 
		Case=48, Catch=49, Class=50, Clone=51, Const=52, Continue=53, Declare=54, 
		Default=55, Do=56, DoubleCast=57, DoubleType=58, Echo=59, Else=60, ElseIf=61, 
		Empty=62, EndDeclare=63, EndFor=64, EndForeach=65, EndIf=66, EndSwitch=67, 
		EndWhile=68, Eval=69, Exit=70, Extends=71, Final=72, Finally=73, FloatCast=74, 
		For=75, Foreach=76, Function=77, Global=78, Goto=79, If=80, Implements=81, 
		Import=82, Include=83, IncludeOnce=84, InstanceOf=85, InsteadOf=86, Int8Cast=87, 
		Int16Cast=88, Int64Type=89, IntType=90, Interface=91, IsSet=92, List=93, 
		LogicalAnd=94, LogicalOr=95, LogicalXor=96, Namespace=97, New=98, Null=99, 
		ObjectType=100, Parent_=101, Partial=102, Print=103, Private=104, Protected=105, 
		Public=106, Require=107, RequireOnce=108, Resource=109, Return=110, Static=111, 
		StringType=112, Switch=113, Throw=114, Trait=115, Try=116, Typeof=117, 
		UintCast=118, UnicodeCast=119, Unset=120, Use=121, Var=122, While=123, 
		Yield=124, Get=125, Set=126, Call=127, CallStatic=128, Constructor=129, 
		Destruct=130, Wakeup=131, Sleep=132, Autoload=133, IsSet__=134, Unset__=135, 
		ToString__=136, Invoke=137, SetState=138, Clone__=139, DebugInfo=140, 
		Namespace__=141, Class__=142, Traic__=143, Function__=144, Method__=145, 
		Line__=146, File__=147, Dir__=148, Lgeneric=149, Rgeneric=150, DoubleArrow=151, 
		Inc=152, Dec=153, IsIdentical=154, IsNoidentical=155, IsEqual=156, IsNotEq=157, 
		IsSmallerOrEqual=158, IsGreaterOrEqual=159, PlusEqual=160, MinusEqual=161, 
		MulEqual=162, Pow=163, PowEqual=164, DivEqual=165, Concaequal=166, ModEqual=167, 
		ShiftLeftEqual=168, ShiftRightEqual=169, AndEqual=170, OrEqual=171, XorEqual=172, 
		BooleanOr=173, BooleanAnd=174, ShiftLeft=175, ShiftRight=176, DoubleColon=177, 
		ObjectOperator=178, NamespaceSeparator=179, Ellipsis=180, Less=181, Greater=182, 
		Ampersand=183, Pipe=184, Bang=185, Caret=186, Plus=187, Minus=188, Asterisk=189, 
		Percent=190, Divide=191, Tilde=192, SuppressWarnings=193, Dollar=194, 
		Dot=195, QuestionMark=196, OpenRoundBracket=197, CloseRoundBracket=198, 
		OpenSquareBracket=199, CloseSquareBracket=200, OpenCurlyBracket=201, CloseCurlyBracket=202, 
		Comma=203, Colon=204, SemiColon=205, Eq=206, Quote=207, BackQuote=208, 
		VarName=209, Label=210, Octal=211, Decimal=212, Real=213, Hex=214, Binary=215, 
		BackQuoteString=216, SingleQuoteString=217, DoubleQuote=218, StartNowDoc=219, 
		StartHereDoc=220, ErrorPhp=221, CurlyDollar=222, StringPart=223, Comment=224, 
		PHPEndSingleLineComment=225, CommentEnd=226, HereDocText=227;
	public static final int
		RULE_htmlDocument = 0, RULE_htmlElementOrPhpBlock = 1, RULE_htmlElement = 2, 
		RULE_scriptTextPart = 3, RULE_phpBlock = 4, RULE_importStatement = 5, 
		RULE_topStatement = 6, RULE_useDeclaration = 7, RULE_useDeclarationContentList = 8, 
		RULE_useDeclarationContent = 9, RULE_namespaceDeclaration = 10, RULE_namespaceStatement = 11, 
		RULE_functionDeclaration = 12, RULE_classDeclaration = 13, RULE_classEntryType = 14, 
		RULE_interfaceList = 15, RULE_typeParameterListInBrackets = 16, RULE_typeParameterList = 17, 
		RULE_typeParameterWithDefaultsList = 18, RULE_typeParameterDecl = 19, 
		RULE_typeParameterWithDefaultDecl = 20, RULE_genericDynamicArgs = 21, 
		RULE_attributes = 22, RULE_attributesGroup = 23, RULE_attribute = 24, 
		RULE_attributeArgList = 25, RULE_attributeNamedArgList = 26, RULE_attributeNamedArg = 27, 
		RULE_innerStatementList = 28, RULE_innerStatement = 29, RULE_statement = 30, 
		RULE_emptyStatement = 31, RULE_nonEmptyStatement = 32, RULE_blockStatement = 33, 
		RULE_ifStatement = 34, RULE_elseIfStatement = 35, RULE_elseIfColonStatement = 36, 
		RULE_elseStatement = 37, RULE_elseColonStatement = 38, RULE_whileStatement = 39, 
		RULE_doWhileStatement = 40, RULE_forStatement = 41, RULE_forInit = 42, 
		RULE_forUpdate = 43, RULE_switchStatement = 44, RULE_switchBlock = 45, 
		RULE_breakStatement = 46, RULE_continueStatement = 47, RULE_returnStatement = 48, 
		RULE_expressionStatement = 49, RULE_unsetStatement = 50, RULE_foreachStatement = 51, 
		RULE_tryCatchFinally = 52, RULE_catchClause = 53, RULE_finallyStatement = 54, 
		RULE_throwStatement = 55, RULE_gotoStatement = 56, RULE_declareStatement = 57, 
		RULE_inlineHtml = 58, RULE_declareList = 59, RULE_formalParameterList = 60, 
		RULE_formalParameter = 61, RULE_typeHint = 62, RULE_globalStatement = 63, 
		RULE_globalVar = 64, RULE_echoStatement = 65, RULE_staticVariableStatement = 66, 
		RULE_classStatement = 67, RULE_traitAdaptations = 68, RULE_traitAdaptationStatement = 69, 
		RULE_traitPrecedence = 70, RULE_traitAlias = 71, RULE_traitMethodReference = 72, 
		RULE_baseCtorCall = 73, RULE_methodBody = 74, RULE_propertyModifiers = 75, 
		RULE_memberModifiers = 76, RULE_variableInitializer = 77, RULE_identifierInititalizer = 78, 
		RULE_globalConstantDeclaration = 79, RULE_expressionList = 80, RULE_parenthesis = 81, 
		RULE_expression = 82, RULE_andOrExpression = 83, RULE_comparisonExpression = 84, 
		RULE_additionExpression = 85, RULE_multiplicationExpression = 86, RULE_notLeftRecursionExpression = 87, 
		RULE_newExpr = 88, RULE_assignmentOperator = 89, RULE_yieldExpression = 90, 
		RULE_arrayItemList = 91, RULE_arrayItem = 92, RULE_lambdaFunctionUseVars = 93, 
		RULE_lambdaFunctionUseVar = 94, RULE_qualifiedStaticTypeRef = 95, RULE_typeRef = 96, 
		RULE_indirectTypeRef = 97, RULE_qualifiedNamespaceName = 98, RULE_namespaceNameList = 99, 
		RULE_qualifiedNamespaceNameList = 100, RULE_arguments = 101, RULE_actualArgument = 102, 
		RULE_constantInititalizer = 103, RULE_constantArrayItemList = 104, RULE_constantArrayItem = 105, 
		RULE_constant = 106, RULE_literalConstant = 107, RULE_numericConstant = 108, 
		RULE_classConstant = 109, RULE_stringConstant = 110, RULE_string = 111, 
		RULE_interpolatedStringPart = 112, RULE_chainList = 113, RULE_chain = 114, 
		RULE_memberAccess = 115, RULE_functionCall = 116, RULE_functionCallName = 117, 
		RULE_actualArguments = 118, RULE_chainBase = 119, RULE_keyedFieldName = 120, 
		RULE_keyedSimpleFieldName = 121, RULE_keyedVariable = 122, RULE_squareCurlyExpression = 123, 
		RULE_assignmentList = 124, RULE_assignmentListElement = 125, RULE_modifier = 126, 
		RULE_identifier = 127, RULE_memberModifier = 128, RULE_magicConstant = 129, 
		RULE_magicMethod = 130, RULE_primitiveType = 131, RULE_castOperation = 132;
	public static final String[] ruleNames = {
		"htmlDocument", "htmlElementOrPhpBlock", "htmlElement", "scriptTextPart", 
		"phpBlock", "importStatement", "topStatement", "useDeclaration", "useDeclarationContentList", 
		"useDeclarationContent", "namespaceDeclaration", "namespaceStatement", 
		"functionDeclaration", "classDeclaration", "classEntryType", "interfaceList", 
		"typeParameterListInBrackets", "typeParameterList", "typeParameterWithDefaultsList", 
		"typeParameterDecl", "typeParameterWithDefaultDecl", "genericDynamicArgs", 
		"attributes", "attributesGroup", "attribute", "attributeArgList", "attributeNamedArgList", 
		"attributeNamedArg", "innerStatementList", "innerStatement", "statement", 
		"emptyStatement", "nonEmptyStatement", "blockStatement", "ifStatement", 
		"elseIfStatement", "elseIfColonStatement", "elseStatement", "elseColonStatement", 
		"whileStatement", "doWhileStatement", "forStatement", "forInit", "forUpdate", 
		"switchStatement", "switchBlock", "breakStatement", "continueStatement", 
		"returnStatement", "expressionStatement", "unsetStatement", "foreachStatement", 
		"tryCatchFinally", "catchClause", "finallyStatement", "throwStatement", 
		"gotoStatement", "declareStatement", "inlineHtml", "declareList", "formalParameterList", 
		"formalParameter", "typeHint", "globalStatement", "globalVar", "echoStatement", 
		"staticVariableStatement", "classStatement", "traitAdaptations", "traitAdaptationStatement", 
		"traitPrecedence", "traitAlias", "traitMethodReference", "baseCtorCall", 
		"methodBody", "propertyModifiers", "memberModifiers", "variableInitializer", 
		"identifierInititalizer", "globalConstantDeclaration", "expressionList", 
		"parenthesis", "expression", "andOrExpression", "comparisonExpression", 
		"additionExpression", "multiplicationExpression", "notLeftRecursionExpression", 
		"newExpr", "assignmentOperator", "yieldExpression", "arrayItemList", "arrayItem", 
		"lambdaFunctionUseVars", "lambdaFunctionUseVar", "qualifiedStaticTypeRef", 
		"typeRef", "indirectTypeRef", "qualifiedNamespaceName", "namespaceNameList", 
		"qualifiedNamespaceNameList", "arguments", "actualArgument", "constantInititalizer", 
		"constantArrayItemList", "constantArrayItem", "constant", "literalConstant", 
		"numericConstant", "classConstant", "stringConstant", "string", "interpolatedStringPart", 
		"chainList", "chain", "memberAccess", "functionCall", "functionCallName", 
		"actualArguments", "chainBase", "keyedFieldName", "keyedSimpleFieldName", 
		"keyedVariable", "squareCurlyExpression", "assignmentList", "assignmentListElement", 
		"modifier", "identifier", "memberModifier", "magicConstant", "magicMethod", 
		"primitiveType", "castOperation"
	};

	private static final String[] _LITERAL_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, "'/>'", null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, "'//'", "'#'", "'abstract'", "'array'", "'as'", "'binary'", 
		null, null, "'break'", "'callable'", "'case'", "'catch'", "'class'", "'clone'", 
		"'const'", "'continue'", "'declare'", "'default'", "'do'", "'real'", "'double'", 
		"'echo'", "'else'", "'elseif'", "'empty'", "'enddeclare'", "'endfor'", 
		"'endforeach'", "'endif'", "'endswitch'", "'endwhile'", "'eval'", "'die'", 
		"'extends'", "'final'", "'finally'", "'float'", "'for'", "'foreach'", 
		"'function'", "'global'", "'goto'", "'if'", "'implements'", "'import'", 
		"'include'", "'include_once'", "'instanceof'", "'insteadof'", "'int8'", 
		"'int16'", "'int64'", null, "'interface'", "'isset'", "'list'", "'and'", 
		"'or'", "'xor'", "'namespace'", "'new'", "'null'", "'object'", "'parent'", 
		"'partial'", "'print'", "'private'", "'protected'", "'public'", "'require'", 
		"'require_once'", "'resource'", "'return'", "'static'", "'string'", "'switch'", 
		"'throw'", "'trait'", "'try'", "'clrtypeof'", null, "'unicode'", "'unset'", 
		"'use'", "'var'", "'while'", "'yield'", "'__get'", "'__set'", "'__call'", 
		"'__callstatic'", "'__construct'", "'__destruct'", "'__wakeup'", "'__sleep'", 
		"'__autoload'", "'__isset'", "'__unset'", "'__tostring'", "'__invoke'", 
		"'__set_state'", "'__clone'", "'__debuginfo'", "'__namespace__'", "'__class__'", 
		"'__trait__'", "'__function__'", "'__method__'", "'__line__'", "'__file__'", 
		"'__dir__'", "'<:'", "':>'", "'=>'", "'++'", "'--'", "'==='", "'!=='", 
		"'=='", null, "'<='", "'>='", "'+='", "'-='", "'*='", "'**'", "'**='", 
		"'/='", "'.='", "'%='", "'<<='", "'>>='", "'&='", "'|='", "'^='", "'||'", 
		"'&&'", "'<<'", "'>>'", "'::'", "'->'", "'\\'", "'...'", null, null, "'&'", 
		"'|'", "'!'", "'^'", "'+'", "'-'", "'*'", "'%'", null, "'~'", "'@'", null, 
		"'.'", "'?'", "'('", "')'", "'['", "']'", null, "'}'", "','", "':'", "';'", 
		null, "'''", "'`'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SeaWhitespace", "HtmlText", "PHPStart", "HtmlScriptOpen", "HtmlStyleOpen", 
		"HtmlComment", "HtmlDtd", "HtmlOpen", "Shebang", "Error", "PHPStartInside", 
		"HtmlClose", "HtmlSlashClose", "HtmlSlash", "HtmlEquals", "HtmlStartQuoteString", 
		"HtmlStartDoubleQuoteString", "HtmlHex", "HtmlDecimal", "HtmlSpace", "HtmlName", 
		"ErrorInside", "PHPStartInsideQuoteString", "HtmlEndQuoteString", "HtmlQuoteString", 
		"ErrorHtmlQuote", "PHPStartDoubleQuoteString", "HtmlEndDoubleQuoteString", 
		"HtmlDoubleQuoteString", "ErrorHtmlDoubleQuote", "ScriptText", "ScriptClose", 
		"PHPStartInsideScript", "StyleBody", "PHPEnd", "Whitespace", "MultiLineComment", 
		"SingleLineComment", "ShellStyleComment", "Abstract", "Array", "As", "BinaryCast", 
		"BoolType", "BooleanConstant", "Break", "Callable", "Case", "Catch", "Class", 
		"Clone", "Const", "Continue", "Declare", "Default", "Do", "DoubleCast", 
		"DoubleType", "Echo", "Else", "ElseIf", "Empty", "EndDeclare", "EndFor", 
		"EndForeach", "EndIf", "EndSwitch", "EndWhile", "Eval", "Exit", "Extends", 
		"Final", "Finally", "FloatCast", "For", "Foreach", "Function", "Global", 
		"Goto", "If", "Implements", "Import", "Include", "IncludeOnce", "InstanceOf", 
		"InsteadOf", "Int8Cast", "Int16Cast", "Int64Type", "IntType", "Interface", 
		"IsSet", "List", "LogicalAnd", "LogicalOr", "LogicalXor", "Namespace", 
		"New", "Null", "ObjectType", "Parent_", "Partial", "Print", "Private", 
		"Protected", "Public", "Require", "RequireOnce", "Resource", "Return", 
		"Static", "StringType", "Switch", "Throw", "Trait", "Try", "Typeof", "UintCast", 
		"UnicodeCast", "Unset", "Use", "Var", "While", "Yield", "Get", "Set", 
		"Call", "CallStatic", "Constructor", "Destruct", "Wakeup", "Sleep", "Autoload", 
		"IsSet__", "Unset__", "ToString__", "Invoke", "SetState", "Clone__", "DebugInfo", 
		"Namespace__", "Class__", "Traic__", "Function__", "Method__", "Line__", 
		"File__", "Dir__", "Lgeneric", "Rgeneric", "DoubleArrow", "Inc", "Dec", 
		"IsIdentical", "IsNoidentical", "IsEqual", "IsNotEq", "IsSmallerOrEqual", 
		"IsGreaterOrEqual", "PlusEqual", "MinusEqual", "MulEqual", "Pow", "PowEqual", 
		"DivEqual", "Concaequal", "ModEqual", "ShiftLeftEqual", "ShiftRightEqual", 
		"AndEqual", "OrEqual", "XorEqual", "BooleanOr", "BooleanAnd", "ShiftLeft", 
		"ShiftRight", "DoubleColon", "ObjectOperator", "NamespaceSeparator", "Ellipsis", 
		"Less", "Greater", "Ampersand", "Pipe", "Bang", "Caret", "Plus", "Minus", 
		"Asterisk", "Percent", "Divide", "Tilde", "SuppressWarnings", "Dollar", 
		"Dot", "QuestionMark", "OpenRoundBracket", "CloseRoundBracket", "OpenSquareBracket", 
		"CloseSquareBracket", "OpenCurlyBracket", "CloseCurlyBracket", "Comma", 
		"Colon", "SemiColon", "Eq", "Quote", "BackQuote", "VarName", "Label", 
		"Octal", "Decimal", "Real", "Hex", "Binary", "BackQuoteString", "SingleQuoteString", 
		"DoubleQuote", "StartNowDoc", "StartHereDoc", "ErrorPhp", "CurlyDollar", 
		"StringPart", "Comment", "PHPEndSingleLineComment", "CommentEnd", "HereDocText"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "PHPParser.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public PHPParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class HtmlDocumentContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(PHPParser.EOF, 0); }
		public TerminalNode Shebang() { return getToken(PHPParser.Shebang, 0); }
		public List<HtmlElementOrPhpBlockContext> htmlElementOrPhpBlock() {
			return getRuleContexts(HtmlElementOrPhpBlockContext.class);
		}
		public HtmlElementOrPhpBlockContext htmlElementOrPhpBlock(int i) {
			return getRuleContext(HtmlElementOrPhpBlockContext.class,i);
		}
		public HtmlDocumentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_htmlDocument; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterHtmlDocument(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitHtmlDocument(this);
		}
	}

	public final HtmlDocumentContext htmlDocument() throws RecognitionException {
		HtmlDocumentContext _localctx = new HtmlDocumentContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_htmlDocument);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(267);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Shebang) {
				{
				setState(266);
				match(Shebang);
				}
			}

			setState(272);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << HtmlText) | (1L << HtmlScriptOpen) | (1L << HtmlStyleOpen) | (1L << HtmlDtd) | (1L << HtmlOpen) | (1L << HtmlClose) | (1L << HtmlSlashClose) | (1L << HtmlSlash) | (1L << HtmlEquals) | (1L << HtmlStartQuoteString) | (1L << HtmlStartDoubleQuoteString) | (1L << HtmlHex) | (1L << HtmlDecimal) | (1L << HtmlName) | (1L << HtmlEndQuoteString) | (1L << HtmlQuoteString) | (1L << HtmlEndDoubleQuoteString) | (1L << HtmlDoubleQuoteString) | (1L << ScriptText) | (1L << ScriptClose) | (1L << StyleBody) | (1L << Abstract) | (1L << Array) | (1L << As) | (1L << BinaryCast) | (1L << BoolType) | (1L << BooleanConstant) | (1L << Break) | (1L << Callable) | (1L << Case) | (1L << Catch) | (1L << Class) | (1L << Clone) | (1L << Const) | (1L << Continue) | (1L << Declare) | (1L << Default) | (1L << Do) | (1L << DoubleCast) | (1L << DoubleType) | (1L << Echo) | (1L << Else) | (1L << ElseIf) | (1L << Empty) | (1L << EndDeclare))) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & ((1L << (EndFor - 64)) | (1L << (EndForeach - 64)) | (1L << (EndIf - 64)) | (1L << (EndSwitch - 64)) | (1L << (EndWhile - 64)) | (1L << (Eval - 64)) | (1L << (Exit - 64)) | (1L << (Extends - 64)) | (1L << (Final - 64)) | (1L << (Finally - 64)) | (1L << (FloatCast - 64)) | (1L << (For - 64)) | (1L << (Foreach - 64)) | (1L << (Function - 64)) | (1L << (Global - 64)) | (1L << (Goto - 64)) | (1L << (If - 64)) | (1L << (Implements - 64)) | (1L << (Import - 64)) | (1L << (Include - 64)) | (1L << (IncludeOnce - 64)) | (1L << (InstanceOf - 64)) | (1L << (InsteadOf - 64)) | (1L << (Int8Cast - 64)) | (1L << (Int16Cast - 64)) | (1L << (Int64Type - 64)) | (1L << (IntType - 64)) | (1L << (Interface - 64)) | (1L << (IsSet - 64)) | (1L << (List - 64)) | (1L << (LogicalAnd - 64)) | (1L << (LogicalOr - 64)) | (1L << (LogicalXor - 64)) | (1L << (Namespace - 64)) | (1L << (New - 64)) | (1L << (Null - 64)) | (1L << (ObjectType - 64)) | (1L << (Parent_ - 64)) | (1L << (Partial - 64)) | (1L << (Print - 64)) | (1L << (Private - 64)) | (1L << (Protected - 64)) | (1L << (Public - 64)) | (1L << (Require - 64)) | (1L << (RequireOnce - 64)) | (1L << (Resource - 64)) | (1L << (Return - 64)) | (1L << (Static - 64)) | (1L << (StringType - 64)) | (1L << (Switch - 64)) | (1L << (Throw - 64)) | (1L << (Trait - 64)) | (1L << (Try - 64)) | (1L << (Typeof - 64)) | (1L << (UintCast - 64)) | (1L << (UnicodeCast - 64)) | (1L << (Unset - 64)) | (1L << (Use - 64)) | (1L << (Var - 64)) | (1L << (While - 64)) | (1L << (Yield - 64)) | (1L << (Get - 64)) | (1L << (Set - 64)) | (1L << (Call - 64)))) != 0) || ((((_la - 128)) & ~0x3f) == 0 && ((1L << (_la - 128)) & ((1L << (CallStatic - 128)) | (1L << (Constructor - 128)) | (1L << (Destruct - 128)) | (1L << (Wakeup - 128)) | (1L << (Sleep - 128)) | (1L << (Autoload - 128)) | (1L << (IsSet__ - 128)) | (1L << (Unset__ - 128)) | (1L << (ToString__ - 128)) | (1L << (Invoke - 128)) | (1L << (SetState - 128)) | (1L << (Clone__ - 128)) | (1L << (DebugInfo - 128)) | (1L << (Namespace__ - 128)) | (1L << (Class__ - 128)) | (1L << (Traic__ - 128)) | (1L << (Function__ - 128)) | (1L << (Method__ - 128)) | (1L << (Line__ - 128)) | (1L << (File__ - 128)) | (1L << (Dir__ - 128)) | (1L << (Inc - 128)) | (1L << (Dec - 128)) | (1L << (NamespaceSeparator - 128)) | (1L << (Bang - 128)) | (1L << (Plus - 128)) | (1L << (Minus - 128)))) != 0) || ((((_la - 192)) & ~0x3f) == 0 && ((1L << (_la - 192)) & ((1L << (Tilde - 192)) | (1L << (SuppressWarnings - 192)) | (1L << (Dollar - 192)) | (1L << (OpenRoundBracket - 192)) | (1L << (OpenSquareBracket - 192)) | (1L << (OpenCurlyBracket - 192)) | (1L << (SemiColon - 192)) | (1L << (VarName - 192)) | (1L << (Label - 192)) | (1L << (Octal - 192)) | (1L << (Decimal - 192)) | (1L << (Real - 192)) | (1L << (Hex - 192)) | (1L << (Binary - 192)) | (1L << (BackQuoteString - 192)) | (1L << (SingleQuoteString - 192)) | (1L << (DoubleQuote - 192)) | (1L << (StartNowDoc - 192)) | (1L << (StartHereDoc - 192)))) != 0)) {
				{
				{
				setState(269);
				htmlElementOrPhpBlock();
				}
				}
				setState(274);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(275);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class HtmlElementOrPhpBlockContext extends ParserRuleContext {
		public HtmlElementContext htmlElement() {
			return getRuleContext(HtmlElementContext.class,0);
		}
		public PhpBlockContext phpBlock() {
			return getRuleContext(PhpBlockContext.class,0);
		}
		public ScriptTextPartContext scriptTextPart() {
			return getRuleContext(ScriptTextPartContext.class,0);
		}
		public HtmlElementOrPhpBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_htmlElementOrPhpBlock; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterHtmlElementOrPhpBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitHtmlElementOrPhpBlock(this);
		}
	}

	public final HtmlElementOrPhpBlockContext htmlElementOrPhpBlock() throws RecognitionException {
		HtmlElementOrPhpBlockContext _localctx = new HtmlElementOrPhpBlockContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_htmlElementOrPhpBlock);
		try {
			setState(280);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(277);
				htmlElement();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(278);
				phpBlock();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(279);
				scriptTextPart();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class HtmlElementContext extends ParserRuleContext {
		public TerminalNode HtmlDtd() { return getToken(PHPParser.HtmlDtd, 0); }
		public TerminalNode HtmlScriptOpen() { return getToken(PHPParser.HtmlScriptOpen, 0); }
		public TerminalNode HtmlClose() { return getToken(PHPParser.HtmlClose, 0); }
		public TerminalNode HtmlStyleOpen() { return getToken(PHPParser.HtmlStyleOpen, 0); }
		public TerminalNode ScriptClose() { return getToken(PHPParser.ScriptClose, 0); }
		public TerminalNode StyleBody() { return getToken(PHPParser.StyleBody, 0); }
		public TerminalNode HtmlOpen() { return getToken(PHPParser.HtmlOpen, 0); }
		public TerminalNode HtmlName() { return getToken(PHPParser.HtmlName, 0); }
		public TerminalNode HtmlSlash() { return getToken(PHPParser.HtmlSlash, 0); }
		public TerminalNode HtmlText() { return getToken(PHPParser.HtmlText, 0); }
		public TerminalNode HtmlEquals() { return getToken(PHPParser.HtmlEquals, 0); }
		public TerminalNode HtmlStartQuoteString() { return getToken(PHPParser.HtmlStartQuoteString, 0); }
		public TerminalNode HtmlEndQuoteString() { return getToken(PHPParser.HtmlEndQuoteString, 0); }
		public TerminalNode HtmlStartDoubleQuoteString() { return getToken(PHPParser.HtmlStartDoubleQuoteString, 0); }
		public TerminalNode HtmlEndDoubleQuoteString() { return getToken(PHPParser.HtmlEndDoubleQuoteString, 0); }
		public TerminalNode HtmlHex() { return getToken(PHPParser.HtmlHex, 0); }
		public TerminalNode HtmlDecimal() { return getToken(PHPParser.HtmlDecimal, 0); }
		public TerminalNode HtmlQuoteString() { return getToken(PHPParser.HtmlQuoteString, 0); }
		public TerminalNode HtmlDoubleQuoteString() { return getToken(PHPParser.HtmlDoubleQuoteString, 0); }
		public HtmlElementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_htmlElement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterHtmlElement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitHtmlElement(this);
		}
	}

	public final HtmlElementContext htmlElement() throws RecognitionException {
		HtmlElementContext _localctx = new HtmlElementContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_htmlElement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(282);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << HtmlText) | (1L << HtmlScriptOpen) | (1L << HtmlStyleOpen) | (1L << HtmlDtd) | (1L << HtmlOpen) | (1L << HtmlClose) | (1L << HtmlSlashClose) | (1L << HtmlSlash) | (1L << HtmlEquals) | (1L << HtmlStartQuoteString) | (1L << HtmlStartDoubleQuoteString) | (1L << HtmlHex) | (1L << HtmlDecimal) | (1L << HtmlName) | (1L << HtmlEndQuoteString) | (1L << HtmlQuoteString) | (1L << HtmlEndDoubleQuoteString) | (1L << HtmlDoubleQuoteString) | (1L << ScriptClose) | (1L << StyleBody))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ScriptTextPartContext extends ParserRuleContext {
		public List<TerminalNode> ScriptText() { return getTokens(PHPParser.ScriptText); }
		public TerminalNode ScriptText(int i) {
			return getToken(PHPParser.ScriptText, i);
		}
		public ScriptTextPartContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_scriptTextPart; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterScriptTextPart(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitScriptTextPart(this);
		}
	}

	public final ScriptTextPartContext scriptTextPart() throws RecognitionException {
		ScriptTextPartContext _localctx = new ScriptTextPartContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_scriptTextPart);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(285); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(284);
					match(ScriptText);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(287); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
			} while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PhpBlockContext extends ParserRuleContext {
		public List<ImportStatementContext> importStatement() {
			return getRuleContexts(ImportStatementContext.class);
		}
		public ImportStatementContext importStatement(int i) {
			return getRuleContext(ImportStatementContext.class,i);
		}
		public List<TopStatementContext> topStatement() {
			return getRuleContexts(TopStatementContext.class);
		}
		public TopStatementContext topStatement(int i) {
			return getRuleContext(TopStatementContext.class,i);
		}
		public PhpBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_phpBlock; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterPhpBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitPhpBlock(this);
		}
	}

	public final PhpBlockContext phpBlock() throws RecognitionException {
		PhpBlockContext _localctx = new PhpBlockContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_phpBlock);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(292);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,4,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(289);
					importStatement();
					}
					} 
				}
				setState(294);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,4,_ctx);
			}
			setState(296); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(295);
					topStatement();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(298); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			} while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ImportStatementContext extends ParserRuleContext {
		public TerminalNode Import() { return getToken(PHPParser.Import, 0); }
		public TerminalNode Namespace() { return getToken(PHPParser.Namespace, 0); }
		public NamespaceNameListContext namespaceNameList() {
			return getRuleContext(NamespaceNameListContext.class,0);
		}
		public ImportStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_importStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterImportStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitImportStatement(this);
		}
	}

	public final ImportStatementContext importStatement() throws RecognitionException {
		ImportStatementContext _localctx = new ImportStatementContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_importStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(300);
			match(Import);
			setState(301);
			match(Namespace);
			setState(302);
			namespaceNameList();
			setState(303);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TopStatementContext extends ParserRuleContext {
		public EmptyStatementContext emptyStatement() {
			return getRuleContext(EmptyStatementContext.class,0);
		}
		public NonEmptyStatementContext nonEmptyStatement() {
			return getRuleContext(NonEmptyStatementContext.class,0);
		}
		public UseDeclarationContext useDeclaration() {
			return getRuleContext(UseDeclarationContext.class,0);
		}
		public NamespaceDeclarationContext namespaceDeclaration() {
			return getRuleContext(NamespaceDeclarationContext.class,0);
		}
		public FunctionDeclarationContext functionDeclaration() {
			return getRuleContext(FunctionDeclarationContext.class,0);
		}
		public ClassDeclarationContext classDeclaration() {
			return getRuleContext(ClassDeclarationContext.class,0);
		}
		public GlobalConstantDeclarationContext globalConstantDeclaration() {
			return getRuleContext(GlobalConstantDeclarationContext.class,0);
		}
		public TopStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_topStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTopStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTopStatement(this);
		}
	}

	public final TopStatementContext topStatement() throws RecognitionException {
		TopStatementContext _localctx = new TopStatementContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_topStatement);
		try {
			setState(312);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(305);
				emptyStatement();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(306);
				nonEmptyStatement();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(307);
				useDeclaration();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(308);
				namespaceDeclaration();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(309);
				functionDeclaration();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(310);
				classDeclaration();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(311);
				globalConstantDeclaration();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class UseDeclarationContext extends ParserRuleContext {
		public TerminalNode Use() { return getToken(PHPParser.Use, 0); }
		public UseDeclarationContentListContext useDeclarationContentList() {
			return getRuleContext(UseDeclarationContentListContext.class,0);
		}
		public TerminalNode Function() { return getToken(PHPParser.Function, 0); }
		public TerminalNode Const() { return getToken(PHPParser.Const, 0); }
		public UseDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_useDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterUseDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitUseDeclaration(this);
		}
	}

	public final UseDeclarationContext useDeclaration() throws RecognitionException {
		UseDeclarationContext _localctx = new UseDeclarationContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_useDeclaration);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(314);
			match(Use);
			setState(316);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,7,_ctx) ) {
			case 1:
				{
				setState(315);
				_la = _input.LA(1);
				if ( !(_la==Const || _la==Function) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				}
				break;
			}
			setState(318);
			useDeclarationContentList();
			setState(319);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class UseDeclarationContentListContext extends ParserRuleContext {
		public List<UseDeclarationContentContext> useDeclarationContent() {
			return getRuleContexts(UseDeclarationContentContext.class);
		}
		public UseDeclarationContentContext useDeclarationContent(int i) {
			return getRuleContext(UseDeclarationContentContext.class,i);
		}
		public UseDeclarationContentListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_useDeclarationContentList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterUseDeclarationContentList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitUseDeclarationContentList(this);
		}
	}

	public final UseDeclarationContentListContext useDeclarationContentList() throws RecognitionException {
		UseDeclarationContentListContext _localctx = new UseDeclarationContentListContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_useDeclarationContentList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(322);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NamespaceSeparator) {
				{
				setState(321);
				match(NamespaceSeparator);
				}
			}

			setState(324);
			useDeclarationContent();
			setState(332);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(325);
				match(Comma);
				setState(327);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NamespaceSeparator) {
					{
					setState(326);
					match(NamespaceSeparator);
					}
				}

				setState(329);
				useDeclarationContent();
				}
				}
				setState(334);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class UseDeclarationContentContext extends ParserRuleContext {
		public NamespaceNameListContext namespaceNameList() {
			return getRuleContext(NamespaceNameListContext.class,0);
		}
		public TerminalNode As() { return getToken(PHPParser.As, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public UseDeclarationContentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_useDeclarationContent; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterUseDeclarationContent(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitUseDeclarationContent(this);
		}
	}

	public final UseDeclarationContentContext useDeclarationContent() throws RecognitionException {
		UseDeclarationContentContext _localctx = new UseDeclarationContentContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_useDeclarationContent);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(335);
			namespaceNameList();
			setState(338);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==As) {
				{
				setState(336);
				match(As);
				setState(337);
				identifier();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NamespaceDeclarationContext extends ParserRuleContext {
		public TerminalNode Namespace() { return getToken(PHPParser.Namespace, 0); }
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public NamespaceNameListContext namespaceNameList() {
			return getRuleContext(NamespaceNameListContext.class,0);
		}
		public List<NamespaceStatementContext> namespaceStatement() {
			return getRuleContexts(NamespaceStatementContext.class);
		}
		public NamespaceStatementContext namespaceStatement(int i) {
			return getRuleContext(NamespaceStatementContext.class,i);
		}
		public NamespaceDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_namespaceDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterNamespaceDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitNamespaceDeclaration(this);
		}
	}

	public final NamespaceDeclarationContext namespaceDeclaration() throws RecognitionException {
		NamespaceDeclarationContext _localctx = new NamespaceDeclarationContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_namespaceDeclaration);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(340);
			match(Namespace);
			setState(355);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,14,_ctx) ) {
			case 1:
				{
				setState(342);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || _la==Label) {
					{
					setState(341);
					namespaceNameList();
					}
				}

				setState(344);
				match(OpenCurlyBracket);
				setState(348);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << HtmlText) | (1L << HtmlScriptOpen) | (1L << HtmlStyleOpen) | (1L << HtmlDtd) | (1L << HtmlOpen) | (1L << HtmlClose) | (1L << HtmlSlashClose) | (1L << HtmlSlash) | (1L << HtmlEquals) | (1L << HtmlStartQuoteString) | (1L << HtmlStartDoubleQuoteString) | (1L << HtmlHex) | (1L << HtmlDecimal) | (1L << HtmlName) | (1L << HtmlEndQuoteString) | (1L << HtmlQuoteString) | (1L << HtmlEndDoubleQuoteString) | (1L << HtmlDoubleQuoteString) | (1L << ScriptText) | (1L << ScriptClose) | (1L << StyleBody) | (1L << Abstract) | (1L << Array) | (1L << As) | (1L << BinaryCast) | (1L << BoolType) | (1L << BooleanConstant) | (1L << Break) | (1L << Callable) | (1L << Case) | (1L << Catch) | (1L << Class) | (1L << Clone) | (1L << Const) | (1L << Continue) | (1L << Declare) | (1L << Default) | (1L << Do) | (1L << DoubleCast) | (1L << DoubleType) | (1L << Echo) | (1L << Else) | (1L << ElseIf) | (1L << Empty) | (1L << EndDeclare))) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & ((1L << (EndFor - 64)) | (1L << (EndForeach - 64)) | (1L << (EndIf - 64)) | (1L << (EndSwitch - 64)) | (1L << (EndWhile - 64)) | (1L << (Eval - 64)) | (1L << (Exit - 64)) | (1L << (Extends - 64)) | (1L << (Final - 64)) | (1L << (Finally - 64)) | (1L << (FloatCast - 64)) | (1L << (For - 64)) | (1L << (Foreach - 64)) | (1L << (Function - 64)) | (1L << (Global - 64)) | (1L << (Goto - 64)) | (1L << (If - 64)) | (1L << (Implements - 64)) | (1L << (Import - 64)) | (1L << (Include - 64)) | (1L << (IncludeOnce - 64)) | (1L << (InstanceOf - 64)) | (1L << (InsteadOf - 64)) | (1L << (Int8Cast - 64)) | (1L << (Int16Cast - 64)) | (1L << (Int64Type - 64)) | (1L << (IntType - 64)) | (1L << (Interface - 64)) | (1L << (IsSet - 64)) | (1L << (List - 64)) | (1L << (LogicalAnd - 64)) | (1L << (LogicalOr - 64)) | (1L << (LogicalXor - 64)) | (1L << (Namespace - 64)) | (1L << (New - 64)) | (1L << (Null - 64)) | (1L << (ObjectType - 64)) | (1L << (Parent_ - 64)) | (1L << (Partial - 64)) | (1L << (Print - 64)) | (1L << (Private - 64)) | (1L << (Protected - 64)) | (1L << (Public - 64)) | (1L << (Require - 64)) | (1L << (RequireOnce - 64)) | (1L << (Resource - 64)) | (1L << (Return - 64)) | (1L << (Static - 64)) | (1L << (StringType - 64)) | (1L << (Switch - 64)) | (1L << (Throw - 64)) | (1L << (Trait - 64)) | (1L << (Try - 64)) | (1L << (Typeof - 64)) | (1L << (UintCast - 64)) | (1L << (UnicodeCast - 64)) | (1L << (Unset - 64)) | (1L << (Use - 64)) | (1L << (Var - 64)) | (1L << (While - 64)) | (1L << (Yield - 64)) | (1L << (Get - 64)) | (1L << (Set - 64)) | (1L << (Call - 64)))) != 0) || ((((_la - 128)) & ~0x3f) == 0 && ((1L << (_la - 128)) & ((1L << (CallStatic - 128)) | (1L << (Constructor - 128)) | (1L << (Destruct - 128)) | (1L << (Wakeup - 128)) | (1L << (Sleep - 128)) | (1L << (Autoload - 128)) | (1L << (IsSet__ - 128)) | (1L << (Unset__ - 128)) | (1L << (ToString__ - 128)) | (1L << (Invoke - 128)) | (1L << (SetState - 128)) | (1L << (Clone__ - 128)) | (1L << (DebugInfo - 128)) | (1L << (Namespace__ - 128)) | (1L << (Class__ - 128)) | (1L << (Traic__ - 128)) | (1L << (Function__ - 128)) | (1L << (Method__ - 128)) | (1L << (Line__ - 128)) | (1L << (File__ - 128)) | (1L << (Dir__ - 128)) | (1L << (Inc - 128)) | (1L << (Dec - 128)) | (1L << (NamespaceSeparator - 128)) | (1L << (Bang - 128)) | (1L << (Plus - 128)) | (1L << (Minus - 128)))) != 0) || ((((_la - 192)) & ~0x3f) == 0 && ((1L << (_la - 192)) & ((1L << (Tilde - 192)) | (1L << (SuppressWarnings - 192)) | (1L << (Dollar - 192)) | (1L << (OpenRoundBracket - 192)) | (1L << (OpenSquareBracket - 192)) | (1L << (OpenCurlyBracket - 192)) | (1L << (VarName - 192)) | (1L << (Label - 192)) | (1L << (Octal - 192)) | (1L << (Decimal - 192)) | (1L << (Real - 192)) | (1L << (Hex - 192)) | (1L << (Binary - 192)) | (1L << (BackQuoteString - 192)) | (1L << (SingleQuoteString - 192)) | (1L << (DoubleQuote - 192)) | (1L << (StartNowDoc - 192)) | (1L << (StartHereDoc - 192)))) != 0)) {
					{
					{
					setState(345);
					namespaceStatement();
					}
					}
					setState(350);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(351);
				match(CloseCurlyBracket);
				}
				break;
			case 2:
				{
				setState(352);
				namespaceNameList();
				setState(353);
				match(SemiColon);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NamespaceStatementContext extends ParserRuleContext {
		public NonEmptyStatementContext nonEmptyStatement() {
			return getRuleContext(NonEmptyStatementContext.class,0);
		}
		public UseDeclarationContext useDeclaration() {
			return getRuleContext(UseDeclarationContext.class,0);
		}
		public FunctionDeclarationContext functionDeclaration() {
			return getRuleContext(FunctionDeclarationContext.class,0);
		}
		public ClassDeclarationContext classDeclaration() {
			return getRuleContext(ClassDeclarationContext.class,0);
		}
		public GlobalConstantDeclarationContext globalConstantDeclaration() {
			return getRuleContext(GlobalConstantDeclarationContext.class,0);
		}
		public NamespaceStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_namespaceStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterNamespaceStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitNamespaceStatement(this);
		}
	}

	public final NamespaceStatementContext namespaceStatement() throws RecognitionException {
		NamespaceStatementContext _localctx = new NamespaceStatementContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_namespaceStatement);
		try {
			setState(362);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,15,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(357);
				nonEmptyStatement();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(358);
				useDeclaration();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(359);
				functionDeclaration();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(360);
				classDeclaration();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(361);
				globalConstantDeclaration();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FunctionDeclarationContext extends ParserRuleContext {
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public TerminalNode Function() { return getToken(PHPParser.Function, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public FormalParameterListContext formalParameterList() {
			return getRuleContext(FormalParameterListContext.class,0);
		}
		public BlockStatementContext blockStatement() {
			return getRuleContext(BlockStatementContext.class,0);
		}
		public TypeParameterListInBracketsContext typeParameterListInBrackets() {
			return getRuleContext(TypeParameterListInBracketsContext.class,0);
		}
		public FunctionDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_functionDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterFunctionDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitFunctionDeclaration(this);
		}
	}

	public final FunctionDeclarationContext functionDeclaration() throws RecognitionException {
		FunctionDeclarationContext _localctx = new FunctionDeclarationContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_functionDeclaration);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(364);
			attributes();
			setState(365);
			match(Function);
			setState(367);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Ampersand) {
				{
				setState(366);
				match(Ampersand);
				}
			}

			setState(369);
			identifier();
			setState(371);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Lgeneric) {
				{
				setState(370);
				typeParameterListInBrackets();
				}
			}

			setState(373);
			match(OpenRoundBracket);
			setState(374);
			formalParameterList();
			setState(375);
			match(CloseRoundBracket);
			setState(376);
			blockStatement();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ClassDeclarationContext extends ParserRuleContext {
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public ClassEntryTypeContext classEntryType() {
			return getRuleContext(ClassEntryTypeContext.class,0);
		}
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode Interface() { return getToken(PHPParser.Interface, 0); }
		public TerminalNode Private() { return getToken(PHPParser.Private, 0); }
		public ModifierContext modifier() {
			return getRuleContext(ModifierContext.class,0);
		}
		public TerminalNode Partial() { return getToken(PHPParser.Partial, 0); }
		public List<ClassStatementContext> classStatement() {
			return getRuleContexts(ClassStatementContext.class);
		}
		public ClassStatementContext classStatement(int i) {
			return getRuleContext(ClassStatementContext.class,i);
		}
		public TypeParameterListInBracketsContext typeParameterListInBrackets() {
			return getRuleContext(TypeParameterListInBracketsContext.class,0);
		}
		public TerminalNode Extends() { return getToken(PHPParser.Extends, 0); }
		public QualifiedStaticTypeRefContext qualifiedStaticTypeRef() {
			return getRuleContext(QualifiedStaticTypeRefContext.class,0);
		}
		public TerminalNode Implements() { return getToken(PHPParser.Implements, 0); }
		public InterfaceListContext interfaceList() {
			return getRuleContext(InterfaceListContext.class,0);
		}
		public ClassDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_classDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterClassDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitClassDeclaration(this);
		}
	}

	public final ClassDeclarationContext classDeclaration() throws RecognitionException {
		ClassDeclarationContext _localctx = new ClassDeclarationContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_classDeclaration);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(378);
			attributes();
			setState(380);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Private) {
				{
				setState(379);
				match(Private);
				}
			}

			setState(383);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Abstract || _la==Final) {
				{
				setState(382);
				modifier();
				}
			}

			setState(386);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Partial) {
				{
				setState(385);
				match(Partial);
				}
			}

			setState(410);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Class:
			case Trait:
				{
				setState(388);
				classEntryType();
				setState(389);
				identifier();
				setState(391);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Lgeneric) {
					{
					setState(390);
					typeParameterListInBrackets();
					}
				}

				setState(395);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Extends) {
					{
					setState(393);
					match(Extends);
					setState(394);
					qualifiedStaticTypeRef();
					}
				}

				setState(399);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Implements) {
					{
					setState(397);
					match(Implements);
					setState(398);
					interfaceList();
					}
				}

				}
				break;
			case Interface:
				{
				setState(401);
				match(Interface);
				setState(402);
				identifier();
				setState(404);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Lgeneric) {
					{
					setState(403);
					typeParameterListInBrackets();
					}
				}

				setState(408);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Extends) {
					{
					setState(406);
					match(Extends);
					setState(407);
					interfaceList();
					}
				}

				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(412);
			match(OpenCurlyBracket);
			setState(416);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Abstract || _la==Const || ((((_la - 72)) & ~0x3f) == 0 && ((1L << (_la - 72)) & ((1L << (Final - 72)) | (1L << (Function - 72)) | (1L << (Private - 72)) | (1L << (Protected - 72)) | (1L << (Public - 72)) | (1L << (Static - 72)) | (1L << (Use - 72)) | (1L << (Var - 72)))) != 0) || _la==OpenSquareBracket) {
				{
				{
				setState(413);
				classStatement();
				}
				}
				setState(418);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(419);
			match(CloseCurlyBracket);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ClassEntryTypeContext extends ParserRuleContext {
		public TerminalNode Class() { return getToken(PHPParser.Class, 0); }
		public TerminalNode Trait() { return getToken(PHPParser.Trait, 0); }
		public ClassEntryTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_classEntryType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterClassEntryType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitClassEntryType(this);
		}
	}

	public final ClassEntryTypeContext classEntryType() throws RecognitionException {
		ClassEntryTypeContext _localctx = new ClassEntryTypeContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_classEntryType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(421);
			_la = _input.LA(1);
			if ( !(_la==Class || _la==Trait) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InterfaceListContext extends ParserRuleContext {
		public List<QualifiedStaticTypeRefContext> qualifiedStaticTypeRef() {
			return getRuleContexts(QualifiedStaticTypeRefContext.class);
		}
		public QualifiedStaticTypeRefContext qualifiedStaticTypeRef(int i) {
			return getRuleContext(QualifiedStaticTypeRefContext.class,i);
		}
		public InterfaceListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_interfaceList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterInterfaceList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitInterfaceList(this);
		}
	}

	public final InterfaceListContext interfaceList() throws RecognitionException {
		InterfaceListContext _localctx = new InterfaceListContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_interfaceList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(423);
			qualifiedStaticTypeRef();
			setState(428);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(424);
				match(Comma);
				setState(425);
				qualifiedStaticTypeRef();
				}
				}
				setState(430);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeParameterListInBracketsContext extends ParserRuleContext {
		public TypeParameterListContext typeParameterList() {
			return getRuleContext(TypeParameterListContext.class,0);
		}
		public TypeParameterWithDefaultsListContext typeParameterWithDefaultsList() {
			return getRuleContext(TypeParameterWithDefaultsListContext.class,0);
		}
		public TypeParameterListInBracketsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeParameterListInBrackets; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTypeParameterListInBrackets(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTypeParameterListInBrackets(this);
		}
	}

	public final TypeParameterListInBracketsContext typeParameterListInBrackets() throws RecognitionException {
		TypeParameterListInBracketsContext _localctx = new TypeParameterListInBracketsContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_typeParameterListInBrackets);
		try {
			setState(445);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,29,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(431);
				match(Lgeneric);
				setState(432);
				typeParameterList();
				setState(433);
				match(Rgeneric);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(435);
				match(Lgeneric);
				setState(436);
				typeParameterWithDefaultsList();
				setState(437);
				match(Rgeneric);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(439);
				match(Lgeneric);
				setState(440);
				typeParameterList();
				setState(441);
				match(Comma);
				setState(442);
				typeParameterWithDefaultsList();
				setState(443);
				match(Rgeneric);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeParameterListContext extends ParserRuleContext {
		public List<TypeParameterDeclContext> typeParameterDecl() {
			return getRuleContexts(TypeParameterDeclContext.class);
		}
		public TypeParameterDeclContext typeParameterDecl(int i) {
			return getRuleContext(TypeParameterDeclContext.class,i);
		}
		public TypeParameterListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeParameterList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTypeParameterList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTypeParameterList(this);
		}
	}

	public final TypeParameterListContext typeParameterList() throws RecognitionException {
		TypeParameterListContext _localctx = new TypeParameterListContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_typeParameterList);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(447);
			typeParameterDecl();
			setState(452);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,30,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(448);
					match(Comma);
					setState(449);
					typeParameterDecl();
					}
					} 
				}
				setState(454);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,30,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeParameterWithDefaultsListContext extends ParserRuleContext {
		public List<TypeParameterWithDefaultDeclContext> typeParameterWithDefaultDecl() {
			return getRuleContexts(TypeParameterWithDefaultDeclContext.class);
		}
		public TypeParameterWithDefaultDeclContext typeParameterWithDefaultDecl(int i) {
			return getRuleContext(TypeParameterWithDefaultDeclContext.class,i);
		}
		public TypeParameterWithDefaultsListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeParameterWithDefaultsList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTypeParameterWithDefaultsList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTypeParameterWithDefaultsList(this);
		}
	}

	public final TypeParameterWithDefaultsListContext typeParameterWithDefaultsList() throws RecognitionException {
		TypeParameterWithDefaultsListContext _localctx = new TypeParameterWithDefaultsListContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_typeParameterWithDefaultsList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(455);
			typeParameterWithDefaultDecl();
			setState(460);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(456);
				match(Comma);
				setState(457);
				typeParameterWithDefaultDecl();
				}
				}
				setState(462);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeParameterDeclContext extends ParserRuleContext {
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TypeParameterDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeParameterDecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTypeParameterDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTypeParameterDecl(this);
		}
	}

	public final TypeParameterDeclContext typeParameterDecl() throws RecognitionException {
		TypeParameterDeclContext _localctx = new TypeParameterDeclContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_typeParameterDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(463);
			attributes();
			setState(464);
			identifier();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeParameterWithDefaultDeclContext extends ParserRuleContext {
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode Eq() { return getToken(PHPParser.Eq, 0); }
		public QualifiedStaticTypeRefContext qualifiedStaticTypeRef() {
			return getRuleContext(QualifiedStaticTypeRefContext.class,0);
		}
		public PrimitiveTypeContext primitiveType() {
			return getRuleContext(PrimitiveTypeContext.class,0);
		}
		public TypeParameterWithDefaultDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeParameterWithDefaultDecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTypeParameterWithDefaultDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTypeParameterWithDefaultDecl(this);
		}
	}

	public final TypeParameterWithDefaultDeclContext typeParameterWithDefaultDecl() throws RecognitionException {
		TypeParameterWithDefaultDeclContext _localctx = new TypeParameterWithDefaultDeclContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_typeParameterWithDefaultDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(466);
			attributes();
			setState(467);
			identifier();
			setState(468);
			match(Eq);
			setState(471);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,32,_ctx) ) {
			case 1:
				{
				setState(469);
				qualifiedStaticTypeRef();
				}
				break;
			case 2:
				{
				setState(470);
				primitiveType();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GenericDynamicArgsContext extends ParserRuleContext {
		public List<TypeRefContext> typeRef() {
			return getRuleContexts(TypeRefContext.class);
		}
		public TypeRefContext typeRef(int i) {
			return getRuleContext(TypeRefContext.class,i);
		}
		public GenericDynamicArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_genericDynamicArgs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterGenericDynamicArgs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitGenericDynamicArgs(this);
		}
	}

	public final GenericDynamicArgsContext genericDynamicArgs() throws RecognitionException {
		GenericDynamicArgsContext _localctx = new GenericDynamicArgsContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_genericDynamicArgs);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(473);
			match(Lgeneric);
			setState(474);
			typeRef();
			setState(479);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(475);
				match(Comma);
				setState(476);
				typeRef();
				}
				}
				setState(481);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(482);
			match(Rgeneric);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributesContext extends ParserRuleContext {
		public List<AttributesGroupContext> attributesGroup() {
			return getRuleContexts(AttributesGroupContext.class);
		}
		public AttributesGroupContext attributesGroup(int i) {
			return getRuleContext(AttributesGroupContext.class,i);
		}
		public AttributesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attributes; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAttributes(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAttributes(this);
		}
	}

	public final AttributesContext attributes() throws RecognitionException {
		AttributesContext _localctx = new AttributesContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_attributes);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(487);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==OpenSquareBracket) {
				{
				{
				setState(484);
				attributesGroup();
				}
				}
				setState(489);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributesGroupContext extends ParserRuleContext {
		public List<AttributeContext> attribute() {
			return getRuleContexts(AttributeContext.class);
		}
		public AttributeContext attribute(int i) {
			return getRuleContext(AttributeContext.class,i);
		}
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public AttributesGroupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attributesGroup; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAttributesGroup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAttributesGroup(this);
		}
	}

	public final AttributesGroupContext attributesGroup() throws RecognitionException {
		AttributesGroupContext _localctx = new AttributesGroupContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_attributesGroup);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(490);
			match(OpenSquareBracket);
			setState(494);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,35,_ctx) ) {
			case 1:
				{
				setState(491);
				identifier();
				setState(492);
				match(Colon);
				}
				break;
			}
			setState(496);
			attribute();
			setState(501);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(497);
				match(Comma);
				setState(498);
				attribute();
				}
				}
				setState(503);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(504);
			match(CloseSquareBracket);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributeContext extends ParserRuleContext {
		public QualifiedNamespaceNameContext qualifiedNamespaceName() {
			return getRuleContext(QualifiedNamespaceNameContext.class,0);
		}
		public AttributeArgListContext attributeArgList() {
			return getRuleContext(AttributeArgListContext.class,0);
		}
		public AttributeNamedArgListContext attributeNamedArgList() {
			return getRuleContext(AttributeNamedArgListContext.class,0);
		}
		public AttributeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attribute; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAttribute(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAttribute(this);
		}
	}

	public final AttributeContext attribute() throws RecognitionException {
		AttributeContext _localctx = new AttributeContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_attribute);
		try {
			setState(524);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,37,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(506);
				qualifiedNamespaceName();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(507);
				qualifiedNamespaceName();
				setState(508);
				match(OpenRoundBracket);
				setState(509);
				attributeArgList();
				setState(510);
				match(CloseRoundBracket);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(512);
				qualifiedNamespaceName();
				setState(513);
				match(OpenRoundBracket);
				setState(514);
				attributeNamedArgList();
				setState(515);
				match(CloseRoundBracket);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(517);
				qualifiedNamespaceName();
				setState(518);
				match(OpenRoundBracket);
				setState(519);
				attributeArgList();
				setState(520);
				match(Comma);
				setState(521);
				attributeNamedArgList();
				setState(522);
				match(CloseRoundBracket);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributeArgListContext extends ParserRuleContext {
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public AttributeArgListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attributeArgList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAttributeArgList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAttributeArgList(this);
		}
	}

	public final AttributeArgListContext attributeArgList() throws RecognitionException {
		AttributeArgListContext _localctx = new AttributeArgListContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_attributeArgList);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(526);
			expression(0);
			setState(531);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,38,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(527);
					match(Comma);
					setState(528);
					expression(0);
					}
					} 
				}
				setState(533);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,38,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributeNamedArgListContext extends ParserRuleContext {
		public List<AttributeNamedArgContext> attributeNamedArg() {
			return getRuleContexts(AttributeNamedArgContext.class);
		}
		public AttributeNamedArgContext attributeNamedArg(int i) {
			return getRuleContext(AttributeNamedArgContext.class,i);
		}
		public AttributeNamedArgListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attributeNamedArgList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAttributeNamedArgList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAttributeNamedArgList(this);
		}
	}

	public final AttributeNamedArgListContext attributeNamedArgList() throws RecognitionException {
		AttributeNamedArgListContext _localctx = new AttributeNamedArgListContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_attributeNamedArgList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(534);
			attributeNamedArg();
			setState(539);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(535);
				match(Comma);
				setState(536);
				attributeNamedArg();
				}
				}
				setState(541);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AttributeNamedArgContext extends ParserRuleContext {
		public TerminalNode VarName() { return getToken(PHPParser.VarName, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public AttributeNamedArgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attributeNamedArg; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAttributeNamedArg(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAttributeNamedArg(this);
		}
	}

	public final AttributeNamedArgContext attributeNamedArg() throws RecognitionException {
		AttributeNamedArgContext _localctx = new AttributeNamedArgContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_attributeNamedArg);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(542);
			match(VarName);
			setState(543);
			match(DoubleArrow);
			setState(544);
			expression(0);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InnerStatementListContext extends ParserRuleContext {
		public List<InnerStatementContext> innerStatement() {
			return getRuleContexts(InnerStatementContext.class);
		}
		public InnerStatementContext innerStatement(int i) {
			return getRuleContext(InnerStatementContext.class,i);
		}
		public InnerStatementListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_innerStatementList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterInnerStatementList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitInnerStatementList(this);
		}
	}

	public final InnerStatementListContext innerStatementList() throws RecognitionException {
		InnerStatementListContext _localctx = new InnerStatementListContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_innerStatementList);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(549);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,40,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(546);
					innerStatement();
					}
					} 
				}
				setState(551);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,40,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InnerStatementContext extends ParserRuleContext {
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public FunctionDeclarationContext functionDeclaration() {
			return getRuleContext(FunctionDeclarationContext.class,0);
		}
		public ClassDeclarationContext classDeclaration() {
			return getRuleContext(ClassDeclarationContext.class,0);
		}
		public InnerStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_innerStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterInnerStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitInnerStatement(this);
		}
	}

	public final InnerStatementContext innerStatement() throws RecognitionException {
		InnerStatementContext _localctx = new InnerStatementContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_innerStatement);
		try {
			setState(555);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,41,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(552);
				statement();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(553);
				functionDeclaration();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(554);
				classDeclaration();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StatementContext extends ParserRuleContext {
		public NonEmptyStatementContext nonEmptyStatement() {
			return getRuleContext(NonEmptyStatementContext.class,0);
		}
		public EmptyStatementContext emptyStatement() {
			return getRuleContext(EmptyStatementContext.class,0);
		}
		public StatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_statement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitStatement(this);
		}
	}

	public final StatementContext statement() throws RecognitionException {
		StatementContext _localctx = new StatementContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_statement);
		try {
			setState(559);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case HtmlText:
			case HtmlScriptOpen:
			case HtmlStyleOpen:
			case HtmlDtd:
			case HtmlOpen:
			case HtmlClose:
			case HtmlSlashClose:
			case HtmlSlash:
			case HtmlEquals:
			case HtmlStartQuoteString:
			case HtmlStartDoubleQuoteString:
			case HtmlHex:
			case HtmlDecimal:
			case HtmlName:
			case HtmlEndQuoteString:
			case HtmlQuoteString:
			case HtmlEndDoubleQuoteString:
			case HtmlDoubleQuoteString:
			case ScriptText:
			case ScriptClose:
			case StyleBody:
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case Inc:
			case Dec:
			case NamespaceSeparator:
			case Bang:
			case Plus:
			case Minus:
			case Tilde:
			case SuppressWarnings:
			case Dollar:
			case OpenRoundBracket:
			case OpenSquareBracket:
			case OpenCurlyBracket:
			case VarName:
			case Label:
			case Octal:
			case Decimal:
			case Real:
			case Hex:
			case Binary:
			case BackQuoteString:
			case SingleQuoteString:
			case DoubleQuote:
			case StartNowDoc:
			case StartHereDoc:
				enterOuterAlt(_localctx, 1);
				{
				setState(557);
				nonEmptyStatement();
				}
				break;
			case SemiColon:
				enterOuterAlt(_localctx, 2);
				{
				setState(558);
				emptyStatement();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class EmptyStatementContext extends ParserRuleContext {
		public EmptyStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_emptyStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterEmptyStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitEmptyStatement(this);
		}
	}

	public final EmptyStatementContext emptyStatement() throws RecognitionException {
		EmptyStatementContext _localctx = new EmptyStatementContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_emptyStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(561);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NonEmptyStatementContext extends ParserRuleContext {
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public BlockStatementContext blockStatement() {
			return getRuleContext(BlockStatementContext.class,0);
		}
		public IfStatementContext ifStatement() {
			return getRuleContext(IfStatementContext.class,0);
		}
		public WhileStatementContext whileStatement() {
			return getRuleContext(WhileStatementContext.class,0);
		}
		public DoWhileStatementContext doWhileStatement() {
			return getRuleContext(DoWhileStatementContext.class,0);
		}
		public ForStatementContext forStatement() {
			return getRuleContext(ForStatementContext.class,0);
		}
		public SwitchStatementContext switchStatement() {
			return getRuleContext(SwitchStatementContext.class,0);
		}
		public BreakStatementContext breakStatement() {
			return getRuleContext(BreakStatementContext.class,0);
		}
		public ContinueStatementContext continueStatement() {
			return getRuleContext(ContinueStatementContext.class,0);
		}
		public ReturnStatementContext returnStatement() {
			return getRuleContext(ReturnStatementContext.class,0);
		}
		public YieldExpressionContext yieldExpression() {
			return getRuleContext(YieldExpressionContext.class,0);
		}
		public GlobalStatementContext globalStatement() {
			return getRuleContext(GlobalStatementContext.class,0);
		}
		public StaticVariableStatementContext staticVariableStatement() {
			return getRuleContext(StaticVariableStatementContext.class,0);
		}
		public EchoStatementContext echoStatement() {
			return getRuleContext(EchoStatementContext.class,0);
		}
		public ExpressionStatementContext expressionStatement() {
			return getRuleContext(ExpressionStatementContext.class,0);
		}
		public UnsetStatementContext unsetStatement() {
			return getRuleContext(UnsetStatementContext.class,0);
		}
		public ForeachStatementContext foreachStatement() {
			return getRuleContext(ForeachStatementContext.class,0);
		}
		public TryCatchFinallyContext tryCatchFinally() {
			return getRuleContext(TryCatchFinallyContext.class,0);
		}
		public ThrowStatementContext throwStatement() {
			return getRuleContext(ThrowStatementContext.class,0);
		}
		public GotoStatementContext gotoStatement() {
			return getRuleContext(GotoStatementContext.class,0);
		}
		public DeclareStatementContext declareStatement() {
			return getRuleContext(DeclareStatementContext.class,0);
		}
		public InlineHtmlContext inlineHtml() {
			return getRuleContext(InlineHtmlContext.class,0);
		}
		public NonEmptyStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_nonEmptyStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterNonEmptyStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitNonEmptyStatement(this);
		}
	}

	public final NonEmptyStatementContext nonEmptyStatement() throws RecognitionException {
		NonEmptyStatementContext _localctx = new NonEmptyStatementContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_nonEmptyStatement);
		try {
			setState(589);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,43,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(563);
				identifier();
				setState(564);
				match(Colon);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(566);
				blockStatement();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(567);
				ifStatement();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(568);
				whileStatement();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(569);
				doWhileStatement();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(570);
				forStatement();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(571);
				switchStatement();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(572);
				breakStatement();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(573);
				continueStatement();
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(574);
				returnStatement();
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(575);
				yieldExpression();
				setState(576);
				match(SemiColon);
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(578);
				globalStatement();
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(579);
				staticVariableStatement();
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(580);
				echoStatement();
				}
				break;
			case 15:
				enterOuterAlt(_localctx, 15);
				{
				setState(581);
				expressionStatement();
				}
				break;
			case 16:
				enterOuterAlt(_localctx, 16);
				{
				setState(582);
				unsetStatement();
				}
				break;
			case 17:
				enterOuterAlt(_localctx, 17);
				{
				setState(583);
				foreachStatement();
				}
				break;
			case 18:
				enterOuterAlt(_localctx, 18);
				{
				setState(584);
				tryCatchFinally();
				}
				break;
			case 19:
				enterOuterAlt(_localctx, 19);
				{
				setState(585);
				throwStatement();
				}
				break;
			case 20:
				enterOuterAlt(_localctx, 20);
				{
				setState(586);
				gotoStatement();
				}
				break;
			case 21:
				enterOuterAlt(_localctx, 21);
				{
				setState(587);
				declareStatement();
				}
				break;
			case 22:
				enterOuterAlt(_localctx, 22);
				{
				setState(588);
				inlineHtml();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BlockStatementContext extends ParserRuleContext {
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public BlockStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_blockStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterBlockStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitBlockStatement(this);
		}
	}

	public final BlockStatementContext blockStatement() throws RecognitionException {
		BlockStatementContext _localctx = new BlockStatementContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_blockStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(591);
			match(OpenCurlyBracket);
			setState(592);
			innerStatementList();
			setState(593);
			match(CloseCurlyBracket);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IfStatementContext extends ParserRuleContext {
		public TerminalNode If() { return getToken(PHPParser.If, 0); }
		public ParenthesisContext parenthesis() {
			return getRuleContext(ParenthesisContext.class,0);
		}
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public List<ElseIfStatementContext> elseIfStatement() {
			return getRuleContexts(ElseIfStatementContext.class);
		}
		public ElseIfStatementContext elseIfStatement(int i) {
			return getRuleContext(ElseIfStatementContext.class,i);
		}
		public ElseStatementContext elseStatement() {
			return getRuleContext(ElseStatementContext.class,0);
		}
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public TerminalNode EndIf() { return getToken(PHPParser.EndIf, 0); }
		public List<ElseIfColonStatementContext> elseIfColonStatement() {
			return getRuleContexts(ElseIfColonStatementContext.class);
		}
		public ElseIfColonStatementContext elseIfColonStatement(int i) {
			return getRuleContext(ElseIfColonStatementContext.class,i);
		}
		public ElseColonStatementContext elseColonStatement() {
			return getRuleContext(ElseColonStatementContext.class,0);
		}
		public IfStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ifStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterIfStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitIfStatement(this);
		}
	}

	public final IfStatementContext ifStatement() throws RecognitionException {
		IfStatementContext _localctx = new IfStatementContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_ifStatement);
		int _la;
		try {
			int _alt;
			setState(623);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,48,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(595);
				match(If);
				setState(596);
				parenthesis();
				setState(597);
				statement();
				setState(601);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,44,_ctx);
				while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(598);
						elseIfStatement();
						}
						} 
					}
					setState(603);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,44,_ctx);
				}
				setState(605);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,45,_ctx) ) {
				case 1:
					{
					setState(604);
					elseStatement();
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(607);
				match(If);
				setState(608);
				parenthesis();
				setState(609);
				match(Colon);
				setState(610);
				innerStatementList();
				setState(614);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==ElseIf) {
					{
					{
					setState(611);
					elseIfColonStatement();
					}
					}
					setState(616);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(618);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Else) {
					{
					setState(617);
					elseColonStatement();
					}
				}

				setState(620);
				match(EndIf);
				setState(621);
				match(SemiColon);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ElseIfStatementContext extends ParserRuleContext {
		public TerminalNode ElseIf() { return getToken(PHPParser.ElseIf, 0); }
		public ParenthesisContext parenthesis() {
			return getRuleContext(ParenthesisContext.class,0);
		}
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public ElseIfStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elseIfStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterElseIfStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitElseIfStatement(this);
		}
	}

	public final ElseIfStatementContext elseIfStatement() throws RecognitionException {
		ElseIfStatementContext _localctx = new ElseIfStatementContext(_ctx, getState());
		enterRule(_localctx, 70, RULE_elseIfStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(625);
			match(ElseIf);
			setState(626);
			parenthesis();
			setState(627);
			statement();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ElseIfColonStatementContext extends ParserRuleContext {
		public TerminalNode ElseIf() { return getToken(PHPParser.ElseIf, 0); }
		public ParenthesisContext parenthesis() {
			return getRuleContext(ParenthesisContext.class,0);
		}
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public ElseIfColonStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elseIfColonStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterElseIfColonStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitElseIfColonStatement(this);
		}
	}

	public final ElseIfColonStatementContext elseIfColonStatement() throws RecognitionException {
		ElseIfColonStatementContext _localctx = new ElseIfColonStatementContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_elseIfColonStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(629);
			match(ElseIf);
			setState(630);
			parenthesis();
			setState(631);
			match(Colon);
			setState(632);
			innerStatementList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ElseStatementContext extends ParserRuleContext {
		public TerminalNode Else() { return getToken(PHPParser.Else, 0); }
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public ElseStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elseStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterElseStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitElseStatement(this);
		}
	}

	public final ElseStatementContext elseStatement() throws RecognitionException {
		ElseStatementContext _localctx = new ElseStatementContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_elseStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(634);
			match(Else);
			setState(635);
			statement();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ElseColonStatementContext extends ParserRuleContext {
		public TerminalNode Else() { return getToken(PHPParser.Else, 0); }
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public ElseColonStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elseColonStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterElseColonStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitElseColonStatement(this);
		}
	}

	public final ElseColonStatementContext elseColonStatement() throws RecognitionException {
		ElseColonStatementContext _localctx = new ElseColonStatementContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_elseColonStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(637);
			match(Else);
			setState(638);
			match(Colon);
			setState(639);
			innerStatementList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class WhileStatementContext extends ParserRuleContext {
		public TerminalNode While() { return getToken(PHPParser.While, 0); }
		public ParenthesisContext parenthesis() {
			return getRuleContext(ParenthesisContext.class,0);
		}
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public TerminalNode EndWhile() { return getToken(PHPParser.EndWhile, 0); }
		public WhileStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whileStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterWhileStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitWhileStatement(this);
		}
	}

	public final WhileStatementContext whileStatement() throws RecognitionException {
		WhileStatementContext _localctx = new WhileStatementContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_whileStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(641);
			match(While);
			setState(642);
			parenthesis();
			setState(649);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case HtmlText:
			case HtmlScriptOpen:
			case HtmlStyleOpen:
			case HtmlDtd:
			case HtmlOpen:
			case HtmlClose:
			case HtmlSlashClose:
			case HtmlSlash:
			case HtmlEquals:
			case HtmlStartQuoteString:
			case HtmlStartDoubleQuoteString:
			case HtmlHex:
			case HtmlDecimal:
			case HtmlName:
			case HtmlEndQuoteString:
			case HtmlQuoteString:
			case HtmlEndDoubleQuoteString:
			case HtmlDoubleQuoteString:
			case ScriptText:
			case ScriptClose:
			case StyleBody:
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case Inc:
			case Dec:
			case NamespaceSeparator:
			case Bang:
			case Plus:
			case Minus:
			case Tilde:
			case SuppressWarnings:
			case Dollar:
			case OpenRoundBracket:
			case OpenSquareBracket:
			case OpenCurlyBracket:
			case SemiColon:
			case VarName:
			case Label:
			case Octal:
			case Decimal:
			case Real:
			case Hex:
			case Binary:
			case BackQuoteString:
			case SingleQuoteString:
			case DoubleQuote:
			case StartNowDoc:
			case StartHereDoc:
				{
				setState(643);
				statement();
				}
				break;
			case Colon:
				{
				setState(644);
				match(Colon);
				setState(645);
				innerStatementList();
				setState(646);
				match(EndWhile);
				setState(647);
				match(SemiColon);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DoWhileStatementContext extends ParserRuleContext {
		public TerminalNode Do() { return getToken(PHPParser.Do, 0); }
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public TerminalNode While() { return getToken(PHPParser.While, 0); }
		public ParenthesisContext parenthesis() {
			return getRuleContext(ParenthesisContext.class,0);
		}
		public DoWhileStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_doWhileStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterDoWhileStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitDoWhileStatement(this);
		}
	}

	public final DoWhileStatementContext doWhileStatement() throws RecognitionException {
		DoWhileStatementContext _localctx = new DoWhileStatementContext(_ctx, getState());
		enterRule(_localctx, 80, RULE_doWhileStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(651);
			match(Do);
			setState(652);
			statement();
			setState(653);
			match(While);
			setState(654);
			parenthesis();
			setState(655);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ForStatementContext extends ParserRuleContext {
		public TerminalNode For() { return getToken(PHPParser.For, 0); }
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public TerminalNode EndFor() { return getToken(PHPParser.EndFor, 0); }
		public ForInitContext forInit() {
			return getRuleContext(ForInitContext.class,0);
		}
		public ExpressionListContext expressionList() {
			return getRuleContext(ExpressionListContext.class,0);
		}
		public ForUpdateContext forUpdate() {
			return getRuleContext(ForUpdateContext.class,0);
		}
		public ForStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_forStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterForStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitForStatement(this);
		}
	}

	public final ForStatementContext forStatement() throws RecognitionException {
		ForStatementContext _localctx = new ForStatementContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_forStatement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(657);
			match(For);
			setState(658);
			match(OpenRoundBracket);
			setState(660);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
				{
				setState(659);
				forInit();
				}
			}

			setState(662);
			match(SemiColon);
			setState(664);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
				{
				setState(663);
				expressionList();
				}
			}

			setState(666);
			match(SemiColon);
			setState(668);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
				{
				setState(667);
				forUpdate();
				}
			}

			setState(670);
			match(CloseRoundBracket);
			setState(677);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case HtmlText:
			case HtmlScriptOpen:
			case HtmlStyleOpen:
			case HtmlDtd:
			case HtmlOpen:
			case HtmlClose:
			case HtmlSlashClose:
			case HtmlSlash:
			case HtmlEquals:
			case HtmlStartQuoteString:
			case HtmlStartDoubleQuoteString:
			case HtmlHex:
			case HtmlDecimal:
			case HtmlName:
			case HtmlEndQuoteString:
			case HtmlQuoteString:
			case HtmlEndDoubleQuoteString:
			case HtmlDoubleQuoteString:
			case ScriptText:
			case ScriptClose:
			case StyleBody:
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case Inc:
			case Dec:
			case NamespaceSeparator:
			case Bang:
			case Plus:
			case Minus:
			case Tilde:
			case SuppressWarnings:
			case Dollar:
			case OpenRoundBracket:
			case OpenSquareBracket:
			case OpenCurlyBracket:
			case SemiColon:
			case VarName:
			case Label:
			case Octal:
			case Decimal:
			case Real:
			case Hex:
			case Binary:
			case BackQuoteString:
			case SingleQuoteString:
			case DoubleQuote:
			case StartNowDoc:
			case StartHereDoc:
				{
				setState(671);
				statement();
				}
				break;
			case Colon:
				{
				setState(672);
				match(Colon);
				setState(673);
				innerStatementList();
				setState(674);
				match(EndFor);
				setState(675);
				match(SemiColon);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ForInitContext extends ParserRuleContext {
		public ExpressionListContext expressionList() {
			return getRuleContext(ExpressionListContext.class,0);
		}
		public ForInitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_forInit; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterForInit(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitForInit(this);
		}
	}

	public final ForInitContext forInit() throws RecognitionException {
		ForInitContext _localctx = new ForInitContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_forInit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(679);
			expressionList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ForUpdateContext extends ParserRuleContext {
		public ExpressionListContext expressionList() {
			return getRuleContext(ExpressionListContext.class,0);
		}
		public ForUpdateContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_forUpdate; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterForUpdate(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitForUpdate(this);
		}
	}

	public final ForUpdateContext forUpdate() throws RecognitionException {
		ForUpdateContext _localctx = new ForUpdateContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_forUpdate);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(681);
			expressionList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SwitchStatementContext extends ParserRuleContext {
		public TerminalNode Switch() { return getToken(PHPParser.Switch, 0); }
		public ParenthesisContext parenthesis() {
			return getRuleContext(ParenthesisContext.class,0);
		}
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public TerminalNode EndSwitch() { return getToken(PHPParser.EndSwitch, 0); }
		public List<SwitchBlockContext> switchBlock() {
			return getRuleContexts(SwitchBlockContext.class);
		}
		public SwitchBlockContext switchBlock(int i) {
			return getRuleContext(SwitchBlockContext.class,i);
		}
		public SwitchStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_switchStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterSwitchStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitSwitchStatement(this);
		}
	}

	public final SwitchStatementContext switchStatement() throws RecognitionException {
		SwitchStatementContext _localctx = new SwitchStatementContext(_ctx, getState());
		enterRule(_localctx, 88, RULE_switchStatement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(683);
			match(Switch);
			setState(684);
			parenthesis();
			setState(708);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case OpenCurlyBracket:
				{
				setState(685);
				match(OpenCurlyBracket);
				setState(687);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==SemiColon) {
					{
					setState(686);
					match(SemiColon);
					}
				}

				setState(692);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==Case || _la==Default) {
					{
					{
					setState(689);
					switchBlock();
					}
					}
					setState(694);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(695);
				match(CloseCurlyBracket);
				}
				break;
			case Colon:
				{
				setState(696);
				match(Colon);
				setState(698);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==SemiColon) {
					{
					setState(697);
					match(SemiColon);
					}
				}

				setState(703);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==Case || _la==Default) {
					{
					{
					setState(700);
					switchBlock();
					}
					}
					setState(705);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(706);
				match(EndSwitch);
				setState(707);
				match(SemiColon);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SwitchBlockContext extends ParserRuleContext {
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public List<TerminalNode> Case() { return getTokens(PHPParser.Case); }
		public TerminalNode Case(int i) {
			return getToken(PHPParser.Case, i);
		}
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public List<TerminalNode> Default() { return getTokens(PHPParser.Default); }
		public TerminalNode Default(int i) {
			return getToken(PHPParser.Default, i);
		}
		public SwitchBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_switchBlock; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterSwitchBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitSwitchBlock(this);
		}
	}

	public final SwitchBlockContext switchBlock() throws RecognitionException {
		SwitchBlockContext _localctx = new SwitchBlockContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_switchBlock);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(716); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(713);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case Case:
						{
						setState(710);
						match(Case);
						setState(711);
						expression(0);
						}
						break;
					case Default:
						{
						setState(712);
						match(Default);
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(715);
					_la = _input.LA(1);
					if ( !(_la==Colon || _la==SemiColon) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(718); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,60,_ctx);
			} while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER );
			setState(720);
			innerStatementList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BreakStatementContext extends ParserRuleContext {
		public TerminalNode Break() { return getToken(PHPParser.Break, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public BreakStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_breakStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterBreakStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitBreakStatement(this);
		}
	}

	public final BreakStatementContext breakStatement() throws RecognitionException {
		BreakStatementContext _localctx = new BreakStatementContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_breakStatement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(722);
			match(Break);
			setState(724);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
				{
				setState(723);
				expression(0);
				}
			}

			setState(726);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ContinueStatementContext extends ParserRuleContext {
		public TerminalNode Continue() { return getToken(PHPParser.Continue, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public ContinueStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_continueStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterContinueStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitContinueStatement(this);
		}
	}

	public final ContinueStatementContext continueStatement() throws RecognitionException {
		ContinueStatementContext _localctx = new ContinueStatementContext(_ctx, getState());
		enterRule(_localctx, 94, RULE_continueStatement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(728);
			match(Continue);
			setState(730);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
				{
				setState(729);
				expression(0);
				}
			}

			setState(732);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ReturnStatementContext extends ParserRuleContext {
		public TerminalNode Return() { return getToken(PHPParser.Return, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public ReturnStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_returnStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterReturnStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitReturnStatement(this);
		}
	}

	public final ReturnStatementContext returnStatement() throws RecognitionException {
		ReturnStatementContext _localctx = new ReturnStatementContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_returnStatement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(734);
			match(Return);
			setState(736);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
				{
				setState(735);
				expression(0);
				}
			}

			setState(738);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExpressionStatementContext extends ParserRuleContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public ExpressionStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expressionStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterExpressionStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitExpressionStatement(this);
		}
	}

	public final ExpressionStatementContext expressionStatement() throws RecognitionException {
		ExpressionStatementContext _localctx = new ExpressionStatementContext(_ctx, getState());
		enterRule(_localctx, 98, RULE_expressionStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(740);
			expression(0);
			setState(741);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class UnsetStatementContext extends ParserRuleContext {
		public TerminalNode Unset() { return getToken(PHPParser.Unset, 0); }
		public ChainListContext chainList() {
			return getRuleContext(ChainListContext.class,0);
		}
		public UnsetStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_unsetStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterUnsetStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitUnsetStatement(this);
		}
	}

	public final UnsetStatementContext unsetStatement() throws RecognitionException {
		UnsetStatementContext _localctx = new UnsetStatementContext(_ctx, getState());
		enterRule(_localctx, 100, RULE_unsetStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(743);
			match(Unset);
			setState(744);
			match(OpenRoundBracket);
			setState(745);
			chainList();
			setState(746);
			match(CloseRoundBracket);
			setState(747);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ForeachStatementContext extends ParserRuleContext {
		public TerminalNode Foreach() { return getToken(PHPParser.Foreach, 0); }
		public List<ChainContext> chain() {
			return getRuleContexts(ChainContext.class);
		}
		public ChainContext chain(int i) {
			return getRuleContext(ChainContext.class,i);
		}
		public TerminalNode As() { return getToken(PHPParser.As, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode List() { return getToken(PHPParser.List, 0); }
		public AssignmentListContext assignmentList() {
			return getRuleContext(AssignmentListContext.class,0);
		}
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public TerminalNode EndForeach() { return getToken(PHPParser.EndForeach, 0); }
		public ForeachStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_foreachStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterForeachStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitForeachStatement(this);
		}
	}

	public final ForeachStatementContext foreachStatement() throws RecognitionException {
		ForeachStatementContext _localctx = new ForeachStatementContext(_ctx, getState());
		enterRule(_localctx, 102, RULE_foreachStatement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(749);
			match(Foreach);
			setState(788);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,69,_ctx) ) {
			case 1:
				{
				setState(750);
				match(OpenRoundBracket);
				setState(751);
				chain();
				setState(752);
				match(As);
				setState(754);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Ampersand) {
					{
					setState(753);
					match(Ampersand);
					}
				}

				setState(756);
				chain();
				setState(762);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==DoubleArrow) {
					{
					setState(757);
					match(DoubleArrow);
					setState(759);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if (_la==Ampersand) {
						{
						setState(758);
						match(Ampersand);
						}
					}

					setState(761);
					chain();
					}
				}

				setState(764);
				match(CloseRoundBracket);
				}
				break;
			case 2:
				{
				setState(766);
				match(OpenRoundBracket);
				setState(767);
				expression(0);
				setState(768);
				match(As);
				setState(769);
				chain();
				setState(775);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==DoubleArrow) {
					{
					setState(770);
					match(DoubleArrow);
					setState(772);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if (_la==Ampersand) {
						{
						setState(771);
						match(Ampersand);
						}
					}

					setState(774);
					chain();
					}
				}

				setState(777);
				match(CloseRoundBracket);
				}
				break;
			case 3:
				{
				setState(779);
				match(OpenRoundBracket);
				setState(780);
				chain();
				setState(781);
				match(As);
				setState(782);
				match(List);
				setState(783);
				match(OpenRoundBracket);
				setState(784);
				assignmentList();
				setState(785);
				match(CloseRoundBracket);
				setState(786);
				match(CloseRoundBracket);
				}
				break;
			}
			setState(796);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case HtmlText:
			case HtmlScriptOpen:
			case HtmlStyleOpen:
			case HtmlDtd:
			case HtmlOpen:
			case HtmlClose:
			case HtmlSlashClose:
			case HtmlSlash:
			case HtmlEquals:
			case HtmlStartQuoteString:
			case HtmlStartDoubleQuoteString:
			case HtmlHex:
			case HtmlDecimal:
			case HtmlName:
			case HtmlEndQuoteString:
			case HtmlQuoteString:
			case HtmlEndDoubleQuoteString:
			case HtmlDoubleQuoteString:
			case ScriptText:
			case ScriptClose:
			case StyleBody:
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case Inc:
			case Dec:
			case NamespaceSeparator:
			case Bang:
			case Plus:
			case Minus:
			case Tilde:
			case SuppressWarnings:
			case Dollar:
			case OpenRoundBracket:
			case OpenSquareBracket:
			case OpenCurlyBracket:
			case SemiColon:
			case VarName:
			case Label:
			case Octal:
			case Decimal:
			case Real:
			case Hex:
			case Binary:
			case BackQuoteString:
			case SingleQuoteString:
			case DoubleQuote:
			case StartNowDoc:
			case StartHereDoc:
				{
				setState(790);
				statement();
				}
				break;
			case Colon:
				{
				setState(791);
				match(Colon);
				setState(792);
				innerStatementList();
				setState(793);
				match(EndForeach);
				setState(794);
				match(SemiColon);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TryCatchFinallyContext extends ParserRuleContext {
		public TerminalNode Try() { return getToken(PHPParser.Try, 0); }
		public BlockStatementContext blockStatement() {
			return getRuleContext(BlockStatementContext.class,0);
		}
		public FinallyStatementContext finallyStatement() {
			return getRuleContext(FinallyStatementContext.class,0);
		}
		public List<CatchClauseContext> catchClause() {
			return getRuleContexts(CatchClauseContext.class);
		}
		public CatchClauseContext catchClause(int i) {
			return getRuleContext(CatchClauseContext.class,i);
		}
		public TryCatchFinallyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tryCatchFinally; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTryCatchFinally(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTryCatchFinally(this);
		}
	}

	public final TryCatchFinallyContext tryCatchFinally() throws RecognitionException {
		TryCatchFinallyContext _localctx = new TryCatchFinallyContext(_ctx, getState());
		enterRule(_localctx, 104, RULE_tryCatchFinally);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(798);
			match(Try);
			setState(799);
			blockStatement();
			setState(815);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,74,_ctx) ) {
			case 1:
				{
				setState(801); 
				_errHandler.sync(this);
				_alt = 1;
				do {
					switch (_alt) {
					case 1:
						{
						{
						setState(800);
						catchClause();
						}
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(803); 
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,71,_ctx);
				} while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER );
				setState(806);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,72,_ctx) ) {
				case 1:
					{
					setState(805);
					finallyStatement();
					}
					break;
				}
				}
				break;
			case 2:
				{
				setState(811);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==Catch) {
					{
					{
					setState(808);
					catchClause();
					}
					}
					setState(813);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(814);
				finallyStatement();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CatchClauseContext extends ParserRuleContext {
		public TerminalNode Catch() { return getToken(PHPParser.Catch, 0); }
		public QualifiedStaticTypeRefContext qualifiedStaticTypeRef() {
			return getRuleContext(QualifiedStaticTypeRefContext.class,0);
		}
		public TerminalNode VarName() { return getToken(PHPParser.VarName, 0); }
		public BlockStatementContext blockStatement() {
			return getRuleContext(BlockStatementContext.class,0);
		}
		public CatchClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_catchClause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterCatchClause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitCatchClause(this);
		}
	}

	public final CatchClauseContext catchClause() throws RecognitionException {
		CatchClauseContext _localctx = new CatchClauseContext(_ctx, getState());
		enterRule(_localctx, 106, RULE_catchClause);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(817);
			match(Catch);
			setState(818);
			match(OpenRoundBracket);
			setState(819);
			qualifiedStaticTypeRef();
			setState(820);
			match(VarName);
			setState(821);
			match(CloseRoundBracket);
			setState(822);
			blockStatement();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FinallyStatementContext extends ParserRuleContext {
		public TerminalNode Finally() { return getToken(PHPParser.Finally, 0); }
		public BlockStatementContext blockStatement() {
			return getRuleContext(BlockStatementContext.class,0);
		}
		public FinallyStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_finallyStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterFinallyStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitFinallyStatement(this);
		}
	}

	public final FinallyStatementContext finallyStatement() throws RecognitionException {
		FinallyStatementContext _localctx = new FinallyStatementContext(_ctx, getState());
		enterRule(_localctx, 108, RULE_finallyStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(824);
			match(Finally);
			setState(825);
			blockStatement();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ThrowStatementContext extends ParserRuleContext {
		public TerminalNode Throw() { return getToken(PHPParser.Throw, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public ThrowStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_throwStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterThrowStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitThrowStatement(this);
		}
	}

	public final ThrowStatementContext throwStatement() throws RecognitionException {
		ThrowStatementContext _localctx = new ThrowStatementContext(_ctx, getState());
		enterRule(_localctx, 110, RULE_throwStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(827);
			match(Throw);
			setState(828);
			expression(0);
			setState(829);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GotoStatementContext extends ParserRuleContext {
		public TerminalNode Goto() { return getToken(PHPParser.Goto, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public GotoStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_gotoStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterGotoStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitGotoStatement(this);
		}
	}

	public final GotoStatementContext gotoStatement() throws RecognitionException {
		GotoStatementContext _localctx = new GotoStatementContext(_ctx, getState());
		enterRule(_localctx, 112, RULE_gotoStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(831);
			match(Goto);
			setState(832);
			identifier();
			setState(833);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DeclareStatementContext extends ParserRuleContext {
		public TerminalNode Declare() { return getToken(PHPParser.Declare, 0); }
		public DeclareListContext declareList() {
			return getRuleContext(DeclareListContext.class,0);
		}
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public InnerStatementListContext innerStatementList() {
			return getRuleContext(InnerStatementListContext.class,0);
		}
		public TerminalNode EndDeclare() { return getToken(PHPParser.EndDeclare, 0); }
		public DeclareStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_declareStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterDeclareStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitDeclareStatement(this);
		}
	}

	public final DeclareStatementContext declareStatement() throws RecognitionException {
		DeclareStatementContext _localctx = new DeclareStatementContext(_ctx, getState());
		enterRule(_localctx, 114, RULE_declareStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(835);
			match(Declare);
			setState(836);
			match(OpenRoundBracket);
			setState(837);
			declareList();
			setState(838);
			match(CloseRoundBracket);
			setState(845);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case HtmlText:
			case HtmlScriptOpen:
			case HtmlStyleOpen:
			case HtmlDtd:
			case HtmlOpen:
			case HtmlClose:
			case HtmlSlashClose:
			case HtmlSlash:
			case HtmlEquals:
			case HtmlStartQuoteString:
			case HtmlStartDoubleQuoteString:
			case HtmlHex:
			case HtmlDecimal:
			case HtmlName:
			case HtmlEndQuoteString:
			case HtmlQuoteString:
			case HtmlEndDoubleQuoteString:
			case HtmlDoubleQuoteString:
			case ScriptText:
			case ScriptClose:
			case StyleBody:
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case Inc:
			case Dec:
			case NamespaceSeparator:
			case Bang:
			case Plus:
			case Minus:
			case Tilde:
			case SuppressWarnings:
			case Dollar:
			case OpenRoundBracket:
			case OpenSquareBracket:
			case OpenCurlyBracket:
			case SemiColon:
			case VarName:
			case Label:
			case Octal:
			case Decimal:
			case Real:
			case Hex:
			case Binary:
			case BackQuoteString:
			case SingleQuoteString:
			case DoubleQuote:
			case StartNowDoc:
			case StartHereDoc:
				{
				setState(839);
				statement();
				}
				break;
			case Colon:
				{
				setState(840);
				match(Colon);
				setState(841);
				innerStatementList();
				setState(842);
				match(EndDeclare);
				setState(843);
				match(SemiColon);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InlineHtmlContext extends ParserRuleContext {
		public List<HtmlElementContext> htmlElement() {
			return getRuleContexts(HtmlElementContext.class);
		}
		public HtmlElementContext htmlElement(int i) {
			return getRuleContext(HtmlElementContext.class,i);
		}
		public List<ScriptTextPartContext> scriptTextPart() {
			return getRuleContexts(ScriptTextPartContext.class);
		}
		public ScriptTextPartContext scriptTextPart(int i) {
			return getRuleContext(ScriptTextPartContext.class,i);
		}
		public InlineHtmlContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_inlineHtml; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterInlineHtml(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitInlineHtml(this);
		}
	}

	public final InlineHtmlContext inlineHtml() throws RecognitionException {
		InlineHtmlContext _localctx = new InlineHtmlContext(_ctx, getState());
		enterRule(_localctx, 116, RULE_inlineHtml);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(849); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					setState(849);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case HtmlText:
					case HtmlScriptOpen:
					case HtmlStyleOpen:
					case HtmlDtd:
					case HtmlOpen:
					case HtmlClose:
					case HtmlSlashClose:
					case HtmlSlash:
					case HtmlEquals:
					case HtmlStartQuoteString:
					case HtmlStartDoubleQuoteString:
					case HtmlHex:
					case HtmlDecimal:
					case HtmlName:
					case HtmlEndQuoteString:
					case HtmlQuoteString:
					case HtmlEndDoubleQuoteString:
					case HtmlDoubleQuoteString:
					case ScriptClose:
					case StyleBody:
						{
						setState(847);
						htmlElement();
						}
						break;
					case ScriptText:
						{
						setState(848);
						scriptTextPart();
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(851); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
			} while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DeclareListContext extends ParserRuleContext {
		public List<IdentifierInititalizerContext> identifierInititalizer() {
			return getRuleContexts(IdentifierInititalizerContext.class);
		}
		public IdentifierInititalizerContext identifierInititalizer(int i) {
			return getRuleContext(IdentifierInititalizerContext.class,i);
		}
		public DeclareListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_declareList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterDeclareList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitDeclareList(this);
		}
	}

	public final DeclareListContext declareList() throws RecognitionException {
		DeclareListContext _localctx = new DeclareListContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_declareList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(853);
			identifierInititalizer();
			setState(858);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(854);
				match(Comma);
				setState(855);
				identifierInititalizer();
				}
				}
				setState(860);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FormalParameterListContext extends ParserRuleContext {
		public List<FormalParameterContext> formalParameter() {
			return getRuleContexts(FormalParameterContext.class);
		}
		public FormalParameterContext formalParameter(int i) {
			return getRuleContext(FormalParameterContext.class,i);
		}
		public FormalParameterListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_formalParameterList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterFormalParameterList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitFormalParameterList(this);
		}
	}

	public final FormalParameterListContext formalParameterList() throws RecognitionException {
		FormalParameterListContext _localctx = new FormalParameterListContext(_ctx, getState());
		enterRule(_localctx, 120, RULE_formalParameterList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(862);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Ellipsis - 179)) | (1L << (Ampersand - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)))) != 0)) {
				{
				setState(861);
				formalParameter();
				}
			}

			setState(868);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(864);
				match(Comma);
				setState(865);
				formalParameter();
				}
				}
				setState(870);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FormalParameterContext extends ParserRuleContext {
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public VariableInitializerContext variableInitializer() {
			return getRuleContext(VariableInitializerContext.class,0);
		}
		public TypeHintContext typeHint() {
			return getRuleContext(TypeHintContext.class,0);
		}
		public FormalParameterContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_formalParameter; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterFormalParameter(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitFormalParameter(this);
		}
	}

	public final FormalParameterContext formalParameter() throws RecognitionException {
		FormalParameterContext _localctx = new FormalParameterContext(_ctx, getState());
		enterRule(_localctx, 122, RULE_formalParameter);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(871);
			attributes();
			setState(873);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || _la==NamespaceSeparator || _la==Label) {
				{
				setState(872);
				typeHint();
				}
			}

			setState(876);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Ampersand) {
				{
				setState(875);
				match(Ampersand);
				}
			}

			setState(879);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Ellipsis) {
				{
				setState(878);
				match(Ellipsis);
				}
			}

			setState(881);
			variableInitializer();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeHintContext extends ParserRuleContext {
		public QualifiedStaticTypeRefContext qualifiedStaticTypeRef() {
			return getRuleContext(QualifiedStaticTypeRefContext.class,0);
		}
		public TerminalNode Callable() { return getToken(PHPParser.Callable, 0); }
		public PrimitiveTypeContext primitiveType() {
			return getRuleContext(PrimitiveTypeContext.class,0);
		}
		public TypeHintContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeHint; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTypeHint(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTypeHint(this);
		}
	}

	public final TypeHintContext typeHint() throws RecognitionException {
		TypeHintContext _localctx = new TypeHintContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_typeHint);
		try {
			setState(886);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,84,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(883);
				qualifiedStaticTypeRef();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(884);
				match(Callable);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(885);
				primitiveType();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GlobalStatementContext extends ParserRuleContext {
		public TerminalNode Global() { return getToken(PHPParser.Global, 0); }
		public List<GlobalVarContext> globalVar() {
			return getRuleContexts(GlobalVarContext.class);
		}
		public GlobalVarContext globalVar(int i) {
			return getRuleContext(GlobalVarContext.class,i);
		}
		public GlobalStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_globalStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterGlobalStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitGlobalStatement(this);
		}
	}

	public final GlobalStatementContext globalStatement() throws RecognitionException {
		GlobalStatementContext _localctx = new GlobalStatementContext(_ctx, getState());
		enterRule(_localctx, 126, RULE_globalStatement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(888);
			match(Global);
			setState(889);
			globalVar();
			setState(894);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(890);
				match(Comma);
				setState(891);
				globalVar();
				}
				}
				setState(896);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(897);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GlobalVarContext extends ParserRuleContext {
		public TerminalNode VarName() { return getToken(PHPParser.VarName, 0); }
		public TerminalNode Dollar() { return getToken(PHPParser.Dollar, 0); }
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public GlobalVarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_globalVar; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterGlobalVar(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitGlobalVar(this);
		}
	}

	public final GlobalVarContext globalVar() throws RecognitionException {
		GlobalVarContext _localctx = new GlobalVarContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_globalVar);
		try {
			setState(907);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,86,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(899);
				match(VarName);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(900);
				match(Dollar);
				setState(901);
				chain();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(902);
				match(Dollar);
				setState(903);
				match(OpenCurlyBracket);
				setState(904);
				expression(0);
				setState(905);
				match(CloseCurlyBracket);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class EchoStatementContext extends ParserRuleContext {
		public TerminalNode Echo() { return getToken(PHPParser.Echo, 0); }
		public ExpressionListContext expressionList() {
			return getRuleContext(ExpressionListContext.class,0);
		}
		public EchoStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_echoStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterEchoStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitEchoStatement(this);
		}
	}

	public final EchoStatementContext echoStatement() throws RecognitionException {
		EchoStatementContext _localctx = new EchoStatementContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_echoStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(909);
			match(Echo);
			setState(910);
			expressionList();
			setState(911);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StaticVariableStatementContext extends ParserRuleContext {
		public TerminalNode Static() { return getToken(PHPParser.Static, 0); }
		public List<VariableInitializerContext> variableInitializer() {
			return getRuleContexts(VariableInitializerContext.class);
		}
		public VariableInitializerContext variableInitializer(int i) {
			return getRuleContext(VariableInitializerContext.class,i);
		}
		public StaticVariableStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_staticVariableStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterStaticVariableStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitStaticVariableStatement(this);
		}
	}

	public final StaticVariableStatementContext staticVariableStatement() throws RecognitionException {
		StaticVariableStatementContext _localctx = new StaticVariableStatementContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_staticVariableStatement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(913);
			match(Static);
			setState(914);
			variableInitializer();
			setState(919);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(915);
				match(Comma);
				setState(916);
				variableInitializer();
				}
				}
				setState(921);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(922);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ClassStatementContext extends ParserRuleContext {
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public PropertyModifiersContext propertyModifiers() {
			return getRuleContext(PropertyModifiersContext.class,0);
		}
		public List<VariableInitializerContext> variableInitializer() {
			return getRuleContexts(VariableInitializerContext.class);
		}
		public VariableInitializerContext variableInitializer(int i) {
			return getRuleContext(VariableInitializerContext.class,i);
		}
		public TerminalNode Const() { return getToken(PHPParser.Const, 0); }
		public List<IdentifierInititalizerContext> identifierInititalizer() {
			return getRuleContexts(IdentifierInititalizerContext.class);
		}
		public IdentifierInititalizerContext identifierInititalizer(int i) {
			return getRuleContext(IdentifierInititalizerContext.class,i);
		}
		public TerminalNode Function() { return getToken(PHPParser.Function, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public FormalParameterListContext formalParameterList() {
			return getRuleContext(FormalParameterListContext.class,0);
		}
		public MethodBodyContext methodBody() {
			return getRuleContext(MethodBodyContext.class,0);
		}
		public MemberModifiersContext memberModifiers() {
			return getRuleContext(MemberModifiersContext.class,0);
		}
		public TypeParameterListInBracketsContext typeParameterListInBrackets() {
			return getRuleContext(TypeParameterListInBracketsContext.class,0);
		}
		public BaseCtorCallContext baseCtorCall() {
			return getRuleContext(BaseCtorCallContext.class,0);
		}
		public TerminalNode Use() { return getToken(PHPParser.Use, 0); }
		public QualifiedNamespaceNameListContext qualifiedNamespaceNameList() {
			return getRuleContext(QualifiedNamespaceNameListContext.class,0);
		}
		public TraitAdaptationsContext traitAdaptations() {
			return getRuleContext(TraitAdaptationsContext.class,0);
		}
		public ClassStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_classStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterClassStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitClassStatement(this);
		}
	}

	public final ClassStatementContext classStatement() throws RecognitionException {
		ClassStatementContext _localctx = new ClassStatementContext(_ctx, getState());
		enterRule(_localctx, 134, RULE_classStatement);
		int _la;
		try {
			setState(972);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,94,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(924);
				attributes();
				setState(925);
				propertyModifiers();
				setState(926);
				variableInitializer();
				setState(931);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==Comma) {
					{
					{
					setState(927);
					match(Comma);
					setState(928);
					variableInitializer();
					}
					}
					setState(933);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(934);
				match(SemiColon);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(936);
				attributes();
				setState(937);
				match(Const);
				setState(938);
				identifierInititalizer();
				setState(943);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==Comma) {
					{
					{
					setState(939);
					match(Comma);
					setState(940);
					identifierInititalizer();
					}
					}
					setState(945);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(946);
				match(SemiColon);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(948);
				attributes();
				setState(950);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Abstract || ((((_la - 72)) & ~0x3f) == 0 && ((1L << (_la - 72)) & ((1L << (Final - 72)) | (1L << (Private - 72)) | (1L << (Protected - 72)) | (1L << (Public - 72)) | (1L << (Static - 72)))) != 0)) {
					{
					setState(949);
					memberModifiers();
					}
				}

				setState(952);
				match(Function);
				setState(954);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Ampersand) {
					{
					setState(953);
					match(Ampersand);
					}
				}

				setState(956);
				identifier();
				setState(958);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Lgeneric) {
					{
					setState(957);
					typeParameterListInBrackets();
					}
				}

				setState(960);
				match(OpenRoundBracket);
				setState(961);
				formalParameterList();
				setState(962);
				match(CloseRoundBracket);
				setState(964);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Colon) {
					{
					setState(963);
					baseCtorCall();
					}
				}

				setState(966);
				methodBody();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(968);
				match(Use);
				setState(969);
				qualifiedNamespaceNameList();
				setState(970);
				traitAdaptations();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TraitAdaptationsContext extends ParserRuleContext {
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public List<TraitAdaptationStatementContext> traitAdaptationStatement() {
			return getRuleContexts(TraitAdaptationStatementContext.class);
		}
		public TraitAdaptationStatementContext traitAdaptationStatement(int i) {
			return getRuleContext(TraitAdaptationStatementContext.class,i);
		}
		public TraitAdaptationsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_traitAdaptations; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTraitAdaptations(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTraitAdaptations(this);
		}
	}

	public final TraitAdaptationsContext traitAdaptations() throws RecognitionException {
		TraitAdaptationsContext _localctx = new TraitAdaptationsContext(_ctx, getState());
		enterRule(_localctx, 136, RULE_traitAdaptations);
		int _la;
		try {
			setState(983);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case SemiColon:
				enterOuterAlt(_localctx, 1);
				{
				setState(974);
				match(SemiColon);
				}
				break;
			case OpenCurlyBracket:
				enterOuterAlt(_localctx, 2);
				{
				setState(975);
				match(OpenCurlyBracket);
				setState(979);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || _la==NamespaceSeparator || _la==Label) {
					{
					{
					setState(976);
					traitAdaptationStatement();
					}
					}
					setState(981);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(982);
				match(CloseCurlyBracket);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TraitAdaptationStatementContext extends ParserRuleContext {
		public TraitPrecedenceContext traitPrecedence() {
			return getRuleContext(TraitPrecedenceContext.class,0);
		}
		public TraitAliasContext traitAlias() {
			return getRuleContext(TraitAliasContext.class,0);
		}
		public TraitAdaptationStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_traitAdaptationStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTraitAdaptationStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTraitAdaptationStatement(this);
		}
	}

	public final TraitAdaptationStatementContext traitAdaptationStatement() throws RecognitionException {
		TraitAdaptationStatementContext _localctx = new TraitAdaptationStatementContext(_ctx, getState());
		enterRule(_localctx, 138, RULE_traitAdaptationStatement);
		try {
			setState(987);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,97,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(985);
				traitPrecedence();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(986);
				traitAlias();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TraitPrecedenceContext extends ParserRuleContext {
		public QualifiedNamespaceNameContext qualifiedNamespaceName() {
			return getRuleContext(QualifiedNamespaceNameContext.class,0);
		}
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode InsteadOf() { return getToken(PHPParser.InsteadOf, 0); }
		public QualifiedNamespaceNameListContext qualifiedNamespaceNameList() {
			return getRuleContext(QualifiedNamespaceNameListContext.class,0);
		}
		public TraitPrecedenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_traitPrecedence; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTraitPrecedence(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTraitPrecedence(this);
		}
	}

	public final TraitPrecedenceContext traitPrecedence() throws RecognitionException {
		TraitPrecedenceContext _localctx = new TraitPrecedenceContext(_ctx, getState());
		enterRule(_localctx, 140, RULE_traitPrecedence);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(989);
			qualifiedNamespaceName();
			setState(990);
			match(DoubleColon);
			setState(991);
			identifier();
			setState(992);
			match(InsteadOf);
			setState(993);
			qualifiedNamespaceNameList();
			setState(994);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TraitAliasContext extends ParserRuleContext {
		public TraitMethodReferenceContext traitMethodReference() {
			return getRuleContext(TraitMethodReferenceContext.class,0);
		}
		public TerminalNode As() { return getToken(PHPParser.As, 0); }
		public MemberModifierContext memberModifier() {
			return getRuleContext(MemberModifierContext.class,0);
		}
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TraitAliasContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_traitAlias; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTraitAlias(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTraitAlias(this);
		}
	}

	public final TraitAliasContext traitAlias() throws RecognitionException {
		TraitAliasContext _localctx = new TraitAliasContext(_ctx, getState());
		enterRule(_localctx, 142, RULE_traitAlias);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(996);
			traitMethodReference();
			setState(997);
			match(As);
			setState(1003);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,99,_ctx) ) {
			case 1:
				{
				setState(998);
				memberModifier();
				}
				break;
			case 2:
				{
				setState(1000);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,98,_ctx) ) {
				case 1:
					{
					setState(999);
					memberModifier();
					}
					break;
				}
				setState(1002);
				identifier();
				}
				break;
			}
			setState(1005);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TraitMethodReferenceContext extends ParserRuleContext {
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public QualifiedNamespaceNameContext qualifiedNamespaceName() {
			return getRuleContext(QualifiedNamespaceNameContext.class,0);
		}
		public TraitMethodReferenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_traitMethodReference; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTraitMethodReference(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTraitMethodReference(this);
		}
	}

	public final TraitMethodReferenceContext traitMethodReference() throws RecognitionException {
		TraitMethodReferenceContext _localctx = new TraitMethodReferenceContext(_ctx, getState());
		enterRule(_localctx, 144, RULE_traitMethodReference);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1010);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,100,_ctx) ) {
			case 1:
				{
				setState(1007);
				qualifiedNamespaceName();
				setState(1008);
				match(DoubleColon);
				}
				break;
			}
			setState(1012);
			identifier();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BaseCtorCallContext extends ParserRuleContext {
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public ArgumentsContext arguments() {
			return getRuleContext(ArgumentsContext.class,0);
		}
		public BaseCtorCallContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_baseCtorCall; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterBaseCtorCall(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitBaseCtorCall(this);
		}
	}

	public final BaseCtorCallContext baseCtorCall() throws RecognitionException {
		BaseCtorCallContext _localctx = new BaseCtorCallContext(_ctx, getState());
		enterRule(_localctx, 146, RULE_baseCtorCall);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1014);
			match(Colon);
			setState(1015);
			identifier();
			setState(1016);
			arguments();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MethodBodyContext extends ParserRuleContext {
		public BlockStatementContext blockStatement() {
			return getRuleContext(BlockStatementContext.class,0);
		}
		public MethodBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_methodBody; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterMethodBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitMethodBody(this);
		}
	}

	public final MethodBodyContext methodBody() throws RecognitionException {
		MethodBodyContext _localctx = new MethodBodyContext(_ctx, getState());
		enterRule(_localctx, 148, RULE_methodBody);
		try {
			setState(1020);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case SemiColon:
				enterOuterAlt(_localctx, 1);
				{
				setState(1018);
				match(SemiColon);
				}
				break;
			case OpenCurlyBracket:
				enterOuterAlt(_localctx, 2);
				{
				setState(1019);
				blockStatement();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PropertyModifiersContext extends ParserRuleContext {
		public MemberModifiersContext memberModifiers() {
			return getRuleContext(MemberModifiersContext.class,0);
		}
		public TerminalNode Var() { return getToken(PHPParser.Var, 0); }
		public PropertyModifiersContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_propertyModifiers; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterPropertyModifiers(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitPropertyModifiers(this);
		}
	}

	public final PropertyModifiersContext propertyModifiers() throws RecognitionException {
		PropertyModifiersContext _localctx = new PropertyModifiersContext(_ctx, getState());
		enterRule(_localctx, 150, RULE_propertyModifiers);
		try {
			setState(1024);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Abstract:
			case Final:
			case Private:
			case Protected:
			case Public:
			case Static:
				enterOuterAlt(_localctx, 1);
				{
				setState(1022);
				memberModifiers();
				}
				break;
			case Var:
				enterOuterAlt(_localctx, 2);
				{
				setState(1023);
				match(Var);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MemberModifiersContext extends ParserRuleContext {
		public List<MemberModifierContext> memberModifier() {
			return getRuleContexts(MemberModifierContext.class);
		}
		public MemberModifierContext memberModifier(int i) {
			return getRuleContext(MemberModifierContext.class,i);
		}
		public MemberModifiersContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_memberModifiers; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterMemberModifiers(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitMemberModifiers(this);
		}
	}

	public final MemberModifiersContext memberModifiers() throws RecognitionException {
		MemberModifiersContext _localctx = new MemberModifiersContext(_ctx, getState());
		enterRule(_localctx, 152, RULE_memberModifiers);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1027); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(1026);
				memberModifier();
				}
				}
				setState(1029); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==Abstract || ((((_la - 72)) & ~0x3f) == 0 && ((1L << (_la - 72)) & ((1L << (Final - 72)) | (1L << (Private - 72)) | (1L << (Protected - 72)) | (1L << (Public - 72)) | (1L << (Static - 72)))) != 0) );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VariableInitializerContext extends ParserRuleContext {
		public TerminalNode VarName() { return getToken(PHPParser.VarName, 0); }
		public TerminalNode Eq() { return getToken(PHPParser.Eq, 0); }
		public ConstantInititalizerContext constantInititalizer() {
			return getRuleContext(ConstantInititalizerContext.class,0);
		}
		public VariableInitializerContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variableInitializer; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterVariableInitializer(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitVariableInitializer(this);
		}
	}

	public final VariableInitializerContext variableInitializer() throws RecognitionException {
		VariableInitializerContext _localctx = new VariableInitializerContext(_ctx, getState());
		enterRule(_localctx, 154, RULE_variableInitializer);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1031);
			match(VarName);
			setState(1034);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Eq) {
				{
				setState(1032);
				match(Eq);
				setState(1033);
				constantInititalizer();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IdentifierInititalizerContext extends ParserRuleContext {
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode Eq() { return getToken(PHPParser.Eq, 0); }
		public ConstantInititalizerContext constantInititalizer() {
			return getRuleContext(ConstantInititalizerContext.class,0);
		}
		public IdentifierInititalizerContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_identifierInititalizer; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterIdentifierInititalizer(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitIdentifierInititalizer(this);
		}
	}

	public final IdentifierInititalizerContext identifierInititalizer() throws RecognitionException {
		IdentifierInititalizerContext _localctx = new IdentifierInititalizerContext(_ctx, getState());
		enterRule(_localctx, 156, RULE_identifierInititalizer);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1036);
			identifier();
			setState(1037);
			match(Eq);
			setState(1038);
			constantInititalizer();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GlobalConstantDeclarationContext extends ParserRuleContext {
		public AttributesContext attributes() {
			return getRuleContext(AttributesContext.class,0);
		}
		public TerminalNode Const() { return getToken(PHPParser.Const, 0); }
		public List<IdentifierInititalizerContext> identifierInititalizer() {
			return getRuleContexts(IdentifierInititalizerContext.class);
		}
		public IdentifierInititalizerContext identifierInititalizer(int i) {
			return getRuleContext(IdentifierInititalizerContext.class,i);
		}
		public GlobalConstantDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_globalConstantDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterGlobalConstantDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitGlobalConstantDeclaration(this);
		}
	}

	public final GlobalConstantDeclarationContext globalConstantDeclaration() throws RecognitionException {
		GlobalConstantDeclarationContext _localctx = new GlobalConstantDeclarationContext(_ctx, getState());
		enterRule(_localctx, 158, RULE_globalConstantDeclaration);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1040);
			attributes();
			setState(1041);
			match(Const);
			setState(1042);
			identifierInititalizer();
			setState(1047);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(1043);
				match(Comma);
				setState(1044);
				identifierInititalizer();
				}
				}
				setState(1049);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(1050);
			match(SemiColon);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExpressionListContext extends ParserRuleContext {
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public ExpressionListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expressionList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterExpressionList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitExpressionList(this);
		}
	}

	public final ExpressionListContext expressionList() throws RecognitionException {
		ExpressionListContext _localctx = new ExpressionListContext(_ctx, getState());
		enterRule(_localctx, 160, RULE_expressionList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1052);
			expression(0);
			setState(1057);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(1053);
				match(Comma);
				setState(1054);
				expression(0);
				}
				}
				setState(1059);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParenthesisContext extends ParserRuleContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public YieldExpressionContext yieldExpression() {
			return getRuleContext(YieldExpressionContext.class,0);
		}
		public ParenthesisContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parenthesis; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterParenthesis(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitParenthesis(this);
		}
	}

	public final ParenthesisContext parenthesis() throws RecognitionException {
		ParenthesisContext _localctx = new ParenthesisContext(_ctx, getState());
		enterRule(_localctx, 162, RULE_parenthesis);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1060);
			match(OpenRoundBracket);
			setState(1063);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,107,_ctx) ) {
			case 1:
				{
				setState(1061);
				expression(0);
				}
				break;
			case 2:
				{
				setState(1062);
				yieldExpression();
				}
				break;
			}
			setState(1065);
			match(CloseRoundBracket);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExpressionContext extends ParserRuleContext {
		public AndOrExpressionContext andOrExpression() {
			return getRuleContext(AndOrExpressionContext.class,0);
		}
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public TerminalNode QuestionMark() { return getToken(PHPParser.QuestionMark, 0); }
		public TerminalNode LogicalAnd() { return getToken(PHPParser.LogicalAnd, 0); }
		public TerminalNode LogicalXor() { return getToken(PHPParser.LogicalXor, 0); }
		public TerminalNode LogicalOr() { return getToken(PHPParser.LogicalOr, 0); }
		public ExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitExpression(this);
		}
	}

	public final ExpressionContext expression() throws RecognitionException {
		return expression(0);
	}

	private ExpressionContext expression(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExpressionContext _localctx = new ExpressionContext(_ctx, _parentState);
		ExpressionContext _prevctx = _localctx;
		int _startState = 164;
		enterRecursionRule(_localctx, 164, RULE_expression, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(1068);
			andOrExpression(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(1088);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,110,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(1086);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,109,_ctx) ) {
					case 1:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(1070);
						if (!(precpred(_ctx, 4))) throw new FailedPredicateException(this, "precpred(_ctx, 4)");
						setState(1071);
						match(QuestionMark);
						setState(1073);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
							{
							setState(1072);
							expression(0);
							}
						}

						setState(1075);
						match(Colon);
						setState(1076);
						andOrExpression(0);
						}
						break;
					case 2:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(1077);
						if (!(precpred(_ctx, 3))) throw new FailedPredicateException(this, "precpred(_ctx, 3)");
						setState(1078);
						match(LogicalAnd);
						setState(1079);
						andOrExpression(0);
						}
						break;
					case 3:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(1080);
						if (!(precpred(_ctx, 2))) throw new FailedPredicateException(this, "precpred(_ctx, 2)");
						setState(1081);
						match(LogicalXor);
						setState(1082);
						andOrExpression(0);
						}
						break;
					case 4:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(1083);
						if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
						setState(1084);
						match(LogicalOr);
						setState(1085);
						andOrExpression(0);
						}
						break;
					}
					} 
				}
				setState(1090);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,110,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class AndOrExpressionContext extends ParserRuleContext {
		public ComparisonExpressionContext comparisonExpression() {
			return getRuleContext(ComparisonExpressionContext.class,0);
		}
		public AndOrExpressionContext andOrExpression() {
			return getRuleContext(AndOrExpressionContext.class,0);
		}
		public AndOrExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_andOrExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAndOrExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAndOrExpression(this);
		}
	}

	public final AndOrExpressionContext andOrExpression() throws RecognitionException {
		return andOrExpression(0);
	}

	private AndOrExpressionContext andOrExpression(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		AndOrExpressionContext _localctx = new AndOrExpressionContext(_ctx, _parentState);
		AndOrExpressionContext _prevctx = _localctx;
		int _startState = 166;
		enterRecursionRule(_localctx, 166, RULE_andOrExpression, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(1092);
			comparisonExpression(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(1111);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,112,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(1109);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,111,_ctx) ) {
					case 1:
						{
						_localctx = new AndOrExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_andOrExpression);
						setState(1094);
						if (!(precpred(_ctx, 5))) throw new FailedPredicateException(this, "precpred(_ctx, 5)");
						setState(1095);
						match(Ampersand);
						setState(1096);
						comparisonExpression(0);
						}
						break;
					case 2:
						{
						_localctx = new AndOrExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_andOrExpression);
						setState(1097);
						if (!(precpred(_ctx, 4))) throw new FailedPredicateException(this, "precpred(_ctx, 4)");
						setState(1098);
						match(Caret);
						setState(1099);
						comparisonExpression(0);
						}
						break;
					case 3:
						{
						_localctx = new AndOrExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_andOrExpression);
						setState(1100);
						if (!(precpred(_ctx, 3))) throw new FailedPredicateException(this, "precpred(_ctx, 3)");
						setState(1101);
						match(Pipe);
						setState(1102);
						comparisonExpression(0);
						}
						break;
					case 4:
						{
						_localctx = new AndOrExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_andOrExpression);
						setState(1103);
						if (!(precpred(_ctx, 2))) throw new FailedPredicateException(this, "precpred(_ctx, 2)");
						setState(1104);
						match(BooleanAnd);
						setState(1105);
						comparisonExpression(0);
						}
						break;
					case 5:
						{
						_localctx = new AndOrExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_andOrExpression);
						setState(1106);
						if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
						setState(1107);
						match(BooleanOr);
						setState(1108);
						comparisonExpression(0);
						}
						break;
					}
					} 
				}
				setState(1113);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,112,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class ComparisonExpressionContext extends ParserRuleContext {
		public AdditionExpressionContext additionExpression() {
			return getRuleContext(AdditionExpressionContext.class,0);
		}
		public ComparisonExpressionContext comparisonExpression() {
			return getRuleContext(ComparisonExpressionContext.class,0);
		}
		public TerminalNode Less() { return getToken(PHPParser.Less, 0); }
		public TerminalNode Greater() { return getToken(PHPParser.Greater, 0); }
		public TerminalNode IsNotEq() { return getToken(PHPParser.IsNotEq, 0); }
		public ComparisonExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_comparisonExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterComparisonExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitComparisonExpression(this);
		}
	}

	public final ComparisonExpressionContext comparisonExpression() throws RecognitionException {
		return comparisonExpression(0);
	}

	private ComparisonExpressionContext comparisonExpression(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ComparisonExpressionContext _localctx = new ComparisonExpressionContext(_ctx, _parentState);
		ComparisonExpressionContext _prevctx = _localctx;
		int _startState = 168;
		enterRecursionRule(_localctx, 168, RULE_comparisonExpression, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(1115);
			additionExpression(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(1128);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,114,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(1126);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,113,_ctx) ) {
					case 1:
						{
						_localctx = new ComparisonExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_comparisonExpression);
						setState(1117);
						if (!(precpred(_ctx, 3))) throw new FailedPredicateException(this, "precpred(_ctx, 3)");
						setState(1118);
						_la = _input.LA(1);
						if ( !(_la==ShiftLeft || _la==ShiftRight) ) {
						_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(1119);
						additionExpression(0);
						}
						break;
					case 2:
						{
						_localctx = new ComparisonExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_comparisonExpression);
						setState(1120);
						if (!(precpred(_ctx, 2))) throw new FailedPredicateException(this, "precpred(_ctx, 2)");
						setState(1121);
						_la = _input.LA(1);
						if ( !(((((_la - 158)) & ~0x3f) == 0 && ((1L << (_la - 158)) & ((1L << (IsSmallerOrEqual - 158)) | (1L << (IsGreaterOrEqual - 158)) | (1L << (Less - 158)) | (1L << (Greater - 158)))) != 0)) ) {
						_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(1122);
						additionExpression(0);
						}
						break;
					case 3:
						{
						_localctx = new ComparisonExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_comparisonExpression);
						setState(1123);
						if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
						setState(1124);
						_la = _input.LA(1);
						if ( !(((((_la - 154)) & ~0x3f) == 0 && ((1L << (_la - 154)) & ((1L << (IsIdentical - 154)) | (1L << (IsNoidentical - 154)) | (1L << (IsEqual - 154)) | (1L << (IsNotEq - 154)))) != 0)) ) {
						_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(1125);
						additionExpression(0);
						}
						break;
					}
					} 
				}
				setState(1130);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,114,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class AdditionExpressionContext extends ParserRuleContext {
		public MultiplicationExpressionContext multiplicationExpression() {
			return getRuleContext(MultiplicationExpressionContext.class,0);
		}
		public AdditionExpressionContext additionExpression() {
			return getRuleContext(AdditionExpressionContext.class,0);
		}
		public AdditionExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_additionExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAdditionExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAdditionExpression(this);
		}
	}

	public final AdditionExpressionContext additionExpression() throws RecognitionException {
		return additionExpression(0);
	}

	private AdditionExpressionContext additionExpression(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		AdditionExpressionContext _localctx = new AdditionExpressionContext(_ctx, _parentState);
		AdditionExpressionContext _prevctx = _localctx;
		int _startState = 170;
		enterRecursionRule(_localctx, 170, RULE_additionExpression, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(1132);
			multiplicationExpression(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(1139);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,115,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new AdditionExpressionContext(_parentctx, _parentState);
					pushNewRecursionContext(_localctx, _startState, RULE_additionExpression);
					setState(1134);
					if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
					setState(1135);
					_la = _input.LA(1);
					if ( !(((((_la - 187)) & ~0x3f) == 0 && ((1L << (_la - 187)) & ((1L << (Plus - 187)) | (1L << (Minus - 187)) | (1L << (Dot - 187)))) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(1136);
					multiplicationExpression(0);
					}
					} 
				}
				setState(1141);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,115,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class MultiplicationExpressionContext extends ParserRuleContext {
		public NotLeftRecursionExpressionContext notLeftRecursionExpression() {
			return getRuleContext(NotLeftRecursionExpressionContext.class,0);
		}
		public MultiplicationExpressionContext multiplicationExpression() {
			return getRuleContext(MultiplicationExpressionContext.class,0);
		}
		public TerminalNode InstanceOf() { return getToken(PHPParser.InstanceOf, 0); }
		public TypeRefContext typeRef() {
			return getRuleContext(TypeRefContext.class,0);
		}
		public TerminalNode Divide() { return getToken(PHPParser.Divide, 0); }
		public MultiplicationExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_multiplicationExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterMultiplicationExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitMultiplicationExpression(this);
		}
	}

	public final MultiplicationExpressionContext multiplicationExpression() throws RecognitionException {
		return multiplicationExpression(0);
	}

	private MultiplicationExpressionContext multiplicationExpression(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		MultiplicationExpressionContext _localctx = new MultiplicationExpressionContext(_ctx, _parentState);
		MultiplicationExpressionContext _prevctx = _localctx;
		int _startState = 172;
		enterRecursionRule(_localctx, 172, RULE_multiplicationExpression, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1148);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,116,_ctx) ) {
			case 1:
				{
				setState(1143);
				notLeftRecursionExpression();
				}
				break;
			case 2:
				{
				setState(1144);
				notLeftRecursionExpression();
				setState(1145);
				match(Pow);
				setState(1146);
				multiplicationExpression(3);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(1158);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,118,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(1156);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,117,_ctx) ) {
					case 1:
						{
						_localctx = new MultiplicationExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_multiplicationExpression);
						setState(1150);
						if (!(precpred(_ctx, 2))) throw new FailedPredicateException(this, "precpred(_ctx, 2)");
						setState(1151);
						match(InstanceOf);
						setState(1152);
						typeRef();
						}
						break;
					case 2:
						{
						_localctx = new MultiplicationExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_multiplicationExpression);
						setState(1153);
						if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
						setState(1154);
						_la = _input.LA(1);
						if ( !(((((_la - 189)) & ~0x3f) == 0 && ((1L << (_la - 189)) & ((1L << (Asterisk - 189)) | (1L << (Percent - 189)) | (1L << (Divide - 189)))) != 0)) ) {
						_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(1155);
						notLeftRecursionExpression();
						}
						break;
					}
					} 
				}
				setState(1160);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,118,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class NotLeftRecursionExpressionContext extends ParserRuleContext {
		public NotLeftRecursionExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_notLeftRecursionExpression; }
	 
		public NotLeftRecursionExpressionContext() { }
		public void copyFrom(NotLeftRecursionExpressionContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class ChainExpressionContext extends NotLeftRecursionExpressionContext {
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public ChainExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterChainExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitChainExpression(this);
		}
	}
	public static class UnaryOperatorExpressionContext extends NotLeftRecursionExpressionContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public UnaryOperatorExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterUnaryOperatorExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitUnaryOperatorExpression(this);
		}
	}
	public static class SpecialWordExpressionContext extends NotLeftRecursionExpressionContext {
		public TerminalNode Yield() { return getToken(PHPParser.Yield, 0); }
		public TerminalNode List() { return getToken(PHPParser.List, 0); }
		public AssignmentListContext assignmentList() {
			return getRuleContext(AssignmentListContext.class,0);
		}
		public TerminalNode Eq() { return getToken(PHPParser.Eq, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode IsSet() { return getToken(PHPParser.IsSet, 0); }
		public ChainListContext chainList() {
			return getRuleContext(ChainListContext.class,0);
		}
		public TerminalNode Empty() { return getToken(PHPParser.Empty, 0); }
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public TerminalNode Eval() { return getToken(PHPParser.Eval, 0); }
		public TerminalNode Exit() { return getToken(PHPParser.Exit, 0); }
		public ParenthesisContext parenthesis() {
			return getRuleContext(ParenthesisContext.class,0);
		}
		public TerminalNode Include() { return getToken(PHPParser.Include, 0); }
		public TerminalNode IncludeOnce() { return getToken(PHPParser.IncludeOnce, 0); }
		public TerminalNode Require() { return getToken(PHPParser.Require, 0); }
		public TerminalNode RequireOnce() { return getToken(PHPParser.RequireOnce, 0); }
		public SpecialWordExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterSpecialWordExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitSpecialWordExpression(this);
		}
	}
	public static class ArrayCreationExpressionContext extends NotLeftRecursionExpressionContext {
		public TerminalNode Array() { return getToken(PHPParser.Array, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public ArrayItemListContext arrayItemList() {
			return getRuleContext(ArrayItemListContext.class,0);
		}
		public ArrayCreationExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterArrayCreationExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitArrayCreationExpression(this);
		}
	}
	public static class NewExpressionContext extends NotLeftRecursionExpressionContext {
		public NewExprContext newExpr() {
			return getRuleContext(NewExprContext.class,0);
		}
		public NewExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterNewExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitNewExpression(this);
		}
	}
	public static class ParenthesisExpressionContext extends NotLeftRecursionExpressionContext {
		public ParenthesisContext parenthesis() {
			return getRuleContext(ParenthesisContext.class,0);
		}
		public ParenthesisExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterParenthesisExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitParenthesisExpression(this);
		}
	}
	public static class BackQuoteStringExpressionContext extends NotLeftRecursionExpressionContext {
		public TerminalNode BackQuoteString() { return getToken(PHPParser.BackQuoteString, 0); }
		public BackQuoteStringExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterBackQuoteStringExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitBackQuoteStringExpression(this);
		}
	}
	public static class IndexerExpressionContext extends NotLeftRecursionExpressionContext {
		public StringConstantContext stringConstant() {
			return getRuleContext(StringConstantContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public IndexerExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterIndexerExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitIndexerExpression(this);
		}
	}
	public static class ScalarExpressionContext extends NotLeftRecursionExpressionContext {
		public ConstantContext constant() {
			return getRuleContext(ConstantContext.class,0);
		}
		public StringContext string() {
			return getRuleContext(StringContext.class,0);
		}
		public TerminalNode Label() { return getToken(PHPParser.Label, 0); }
		public ScalarExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterScalarExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitScalarExpression(this);
		}
	}
	public static class PrefixIncDecExpressionContext extends NotLeftRecursionExpressionContext {
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public PrefixIncDecExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterPrefixIncDecExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitPrefixIncDecExpression(this);
		}
	}
	public static class PrintExpressionContext extends NotLeftRecursionExpressionContext {
		public TerminalNode Print() { return getToken(PHPParser.Print, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public PrintExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterPrintExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitPrintExpression(this);
		}
	}
	public static class AssignmentExpressionContext extends NotLeftRecursionExpressionContext {
		public List<ChainContext> chain() {
			return getRuleContexts(ChainContext.class);
		}
		public ChainContext chain(int i) {
			return getRuleContext(ChainContext.class,i);
		}
		public AssignmentOperatorContext assignmentOperator() {
			return getRuleContext(AssignmentOperatorContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode Eq() { return getToken(PHPParser.Eq, 0); }
		public NewExprContext newExpr() {
			return getRuleContext(NewExprContext.class,0);
		}
		public AssignmentExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAssignmentExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAssignmentExpression(this);
		}
	}
	public static class PostfixIncDecExpressionContext extends NotLeftRecursionExpressionContext {
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public PostfixIncDecExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterPostfixIncDecExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitPostfixIncDecExpression(this);
		}
	}
	public static class CastExpressionContext extends NotLeftRecursionExpressionContext {
		public CastOperationContext castOperation() {
			return getRuleContext(CastOperationContext.class,0);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public CastExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterCastExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitCastExpression(this);
		}
	}
	public static class LambdaFunctionExpressionContext extends NotLeftRecursionExpressionContext {
		public TerminalNode Function() { return getToken(PHPParser.Function, 0); }
		public FormalParameterListContext formalParameterList() {
			return getRuleContext(FormalParameterListContext.class,0);
		}
		public BlockStatementContext blockStatement() {
			return getRuleContext(BlockStatementContext.class,0);
		}
		public TerminalNode Static() { return getToken(PHPParser.Static, 0); }
		public LambdaFunctionUseVarsContext lambdaFunctionUseVars() {
			return getRuleContext(LambdaFunctionUseVarsContext.class,0);
		}
		public LambdaFunctionExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterLambdaFunctionExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitLambdaFunctionExpression(this);
		}
	}
	public static class CloneExpressionContext extends NotLeftRecursionExpressionContext {
		public TerminalNode Clone() { return getToken(PHPParser.Clone, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public CloneExpressionContext(NotLeftRecursionExpressionContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterCloneExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitCloneExpression(this);
		}
	}

	public final NotLeftRecursionExpressionContext notLeftRecursionExpression() throws RecognitionException {
		NotLeftRecursionExpressionContext _localctx = new NotLeftRecursionExpressionContext(_ctx, getState());
		enterRule(_localctx, 174, RULE_notLeftRecursionExpression);
		int _la;
		try {
			setState(1269);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,128,_ctx) ) {
			case 1:
				_localctx = new CloneExpressionContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(1161);
				match(Clone);
				setState(1162);
				expression(0);
				}
				break;
			case 2:
				_localctx = new NewExpressionContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(1163);
				newExpr();
				}
				break;
			case 3:
				_localctx = new IndexerExpressionContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(1164);
				stringConstant();
				setState(1165);
				match(OpenSquareBracket);
				setState(1166);
				expression(0);
				setState(1167);
				match(CloseSquareBracket);
				}
				break;
			case 4:
				_localctx = new CastExpressionContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(1169);
				match(OpenRoundBracket);
				setState(1170);
				castOperation();
				setState(1171);
				match(CloseRoundBracket);
				setState(1172);
				expression(0);
				}
				break;
			case 5:
				_localctx = new UnaryOperatorExpressionContext(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(1174);
				_la = _input.LA(1);
				if ( !(_la==Tilde || _la==SuppressWarnings) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(1175);
				expression(0);
				}
				break;
			case 6:
				_localctx = new UnaryOperatorExpressionContext(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(1176);
				_la = _input.LA(1);
				if ( !(((((_la - 185)) & ~0x3f) == 0 && ((1L << (_la - 185)) & ((1L << (Bang - 185)) | (1L << (Plus - 185)) | (1L << (Minus - 185)))) != 0)) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(1177);
				expression(0);
				}
				break;
			case 7:
				_localctx = new PrefixIncDecExpressionContext(_localctx);
				enterOuterAlt(_localctx, 7);
				{
				setState(1178);
				_la = _input.LA(1);
				if ( !(_la==Inc || _la==Dec) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(1179);
				chain();
				}
				break;
			case 8:
				_localctx = new PostfixIncDecExpressionContext(_localctx);
				enterOuterAlt(_localctx, 8);
				{
				setState(1180);
				chain();
				setState(1181);
				_la = _input.LA(1);
				if ( !(_la==Inc || _la==Dec) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				}
				break;
			case 9:
				_localctx = new AssignmentExpressionContext(_localctx);
				enterOuterAlt(_localctx, 9);
				{
				setState(1183);
				chain();
				setState(1184);
				assignmentOperator();
				setState(1185);
				expression(0);
				}
				break;
			case 10:
				_localctx = new AssignmentExpressionContext(_localctx);
				enterOuterAlt(_localctx, 10);
				{
				setState(1187);
				chain();
				setState(1188);
				match(Eq);
				setState(1189);
				match(Ampersand);
				setState(1192);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,119,_ctx) ) {
				case 1:
					{
					setState(1190);
					chain();
					}
					break;
				case 2:
					{
					setState(1191);
					newExpr();
					}
					break;
				}
				}
				break;
			case 11:
				_localctx = new PrintExpressionContext(_localctx);
				enterOuterAlt(_localctx, 11);
				{
				setState(1194);
				match(Print);
				setState(1195);
				expression(0);
				}
				break;
			case 12:
				_localctx = new ChainExpressionContext(_localctx);
				enterOuterAlt(_localctx, 12);
				{
				setState(1196);
				chain();
				}
				break;
			case 13:
				_localctx = new ScalarExpressionContext(_localctx);
				enterOuterAlt(_localctx, 13);
				{
				setState(1197);
				constant();
				}
				break;
			case 14:
				_localctx = new ScalarExpressionContext(_localctx);
				enterOuterAlt(_localctx, 14);
				{
				setState(1198);
				string();
				}
				break;
			case 15:
				_localctx = new ScalarExpressionContext(_localctx);
				enterOuterAlt(_localctx, 15);
				{
				setState(1199);
				match(Label);
				}
				break;
			case 16:
				_localctx = new BackQuoteStringExpressionContext(_localctx);
				enterOuterAlt(_localctx, 16);
				{
				setState(1200);
				match(BackQuoteString);
				}
				break;
			case 17:
				_localctx = new ParenthesisExpressionContext(_localctx);
				enterOuterAlt(_localctx, 17);
				{
				setState(1201);
				parenthesis();
				}
				break;
			case 18:
				_localctx = new ArrayCreationExpressionContext(_localctx);
				enterOuterAlt(_localctx, 18);
				{
				setState(1213);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case Array:
					{
					setState(1202);
					match(Array);
					setState(1203);
					match(OpenRoundBracket);
					setState(1205);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Ampersand - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
						{
						setState(1204);
						arrayItemList();
						}
					}

					setState(1207);
					match(CloseRoundBracket);
					}
					break;
				case OpenSquareBracket:
					{
					setState(1208);
					match(OpenSquareBracket);
					setState(1210);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Ampersand - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
						{
						setState(1209);
						arrayItemList();
						}
					}

					setState(1212);
					match(CloseSquareBracket);
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(1219);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,123,_ctx) ) {
				case 1:
					{
					setState(1215);
					match(OpenSquareBracket);
					setState(1216);
					expression(0);
					setState(1217);
					match(CloseSquareBracket);
					}
					break;
				}
				}
				break;
			case 19:
				_localctx = new SpecialWordExpressionContext(_localctx);
				enterOuterAlt(_localctx, 19);
				{
				setState(1221);
				match(Yield);
				}
				break;
			case 20:
				_localctx = new SpecialWordExpressionContext(_localctx);
				enterOuterAlt(_localctx, 20);
				{
				setState(1222);
				match(List);
				setState(1223);
				match(OpenRoundBracket);
				setState(1224);
				assignmentList();
				setState(1225);
				match(CloseRoundBracket);
				setState(1226);
				match(Eq);
				setState(1227);
				expression(0);
				}
				break;
			case 21:
				_localctx = new SpecialWordExpressionContext(_localctx);
				enterOuterAlt(_localctx, 21);
				{
				setState(1229);
				match(IsSet);
				setState(1230);
				match(OpenRoundBracket);
				setState(1231);
				chainList();
				setState(1232);
				match(CloseRoundBracket);
				}
				break;
			case 22:
				_localctx = new SpecialWordExpressionContext(_localctx);
				enterOuterAlt(_localctx, 22);
				{
				setState(1234);
				match(Empty);
				setState(1235);
				match(OpenRoundBracket);
				setState(1236);
				chain();
				setState(1237);
				match(CloseRoundBracket);
				}
				break;
			case 23:
				_localctx = new SpecialWordExpressionContext(_localctx);
				enterOuterAlt(_localctx, 23);
				{
				setState(1239);
				match(Eval);
				setState(1240);
				match(OpenRoundBracket);
				setState(1241);
				expression(0);
				setState(1242);
				match(CloseRoundBracket);
				}
				break;
			case 24:
				_localctx = new SpecialWordExpressionContext(_localctx);
				enterOuterAlt(_localctx, 24);
				{
				setState(1244);
				match(Exit);
				setState(1248);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,124,_ctx) ) {
				case 1:
					{
					setState(1245);
					match(OpenRoundBracket);
					setState(1246);
					match(CloseRoundBracket);
					}
					break;
				case 2:
					{
					setState(1247);
					parenthesis();
					}
					break;
				}
				}
				break;
			case 25:
				_localctx = new SpecialWordExpressionContext(_localctx);
				enterOuterAlt(_localctx, 25);
				{
				setState(1250);
				_la = _input.LA(1);
				if ( !(_la==Include || _la==IncludeOnce) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(1251);
				expression(0);
				}
				break;
			case 26:
				_localctx = new SpecialWordExpressionContext(_localctx);
				enterOuterAlt(_localctx, 26);
				{
				setState(1252);
				_la = _input.LA(1);
				if ( !(_la==Require || _la==RequireOnce) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(1253);
				expression(0);
				}
				break;
			case 27:
				_localctx = new LambdaFunctionExpressionContext(_localctx);
				enterOuterAlt(_localctx, 27);
				{
				setState(1255);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Static) {
					{
					setState(1254);
					match(Static);
					}
				}

				setState(1257);
				match(Function);
				setState(1259);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Ampersand) {
					{
					setState(1258);
					match(Ampersand);
					}
				}

				setState(1261);
				match(OpenRoundBracket);
				setState(1262);
				formalParameterList();
				setState(1263);
				match(CloseRoundBracket);
				setState(1265);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Use) {
					{
					setState(1264);
					lambdaFunctionUseVars();
					}
				}

				setState(1267);
				blockStatement();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NewExprContext extends ParserRuleContext {
		public TerminalNode New() { return getToken(PHPParser.New, 0); }
		public TypeRefContext typeRef() {
			return getRuleContext(TypeRefContext.class,0);
		}
		public ArgumentsContext arguments() {
			return getRuleContext(ArgumentsContext.class,0);
		}
		public NewExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_newExpr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterNewExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitNewExpr(this);
		}
	}

	public final NewExprContext newExpr() throws RecognitionException {
		NewExprContext _localctx = new NewExprContext(_ctx, getState());
		enterRule(_localctx, 176, RULE_newExpr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1271);
			match(New);
			setState(1272);
			typeRef();
			setState(1274);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,129,_ctx) ) {
			case 1:
				{
				setState(1273);
				arguments();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AssignmentOperatorContext extends ParserRuleContext {
		public TerminalNode Eq() { return getToken(PHPParser.Eq, 0); }
		public AssignmentOperatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assignmentOperator; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAssignmentOperator(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAssignmentOperator(this);
		}
	}

	public final AssignmentOperatorContext assignmentOperator() throws RecognitionException {
		AssignmentOperatorContext _localctx = new AssignmentOperatorContext(_ctx, getState());
		enterRule(_localctx, 178, RULE_assignmentOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1276);
			_la = _input.LA(1);
			if ( !(((((_la - 160)) & ~0x3f) == 0 && ((1L << (_la - 160)) & ((1L << (PlusEqual - 160)) | (1L << (MinusEqual - 160)) | (1L << (MulEqual - 160)) | (1L << (PowEqual - 160)) | (1L << (DivEqual - 160)) | (1L << (Concaequal - 160)) | (1L << (ModEqual - 160)) | (1L << (ShiftLeftEqual - 160)) | (1L << (ShiftRightEqual - 160)) | (1L << (AndEqual - 160)) | (1L << (OrEqual - 160)) | (1L << (XorEqual - 160)) | (1L << (Eq - 160)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class YieldExpressionContext extends ParserRuleContext {
		public TerminalNode Yield() { return getToken(PHPParser.Yield, 0); }
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public YieldExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_yieldExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterYieldExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitYieldExpression(this);
		}
	}

	public final YieldExpressionContext yieldExpression() throws RecognitionException {
		YieldExpressionContext _localctx = new YieldExpressionContext(_ctx, getState());
		enterRule(_localctx, 180, RULE_yieldExpression);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1278);
			match(Yield);
			setState(1279);
			expression(0);
			setState(1282);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==DoubleArrow) {
				{
				setState(1280);
				match(DoubleArrow);
				setState(1281);
				expression(0);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArrayItemListContext extends ParserRuleContext {
		public List<ArrayItemContext> arrayItem() {
			return getRuleContexts(ArrayItemContext.class);
		}
		public ArrayItemContext arrayItem(int i) {
			return getRuleContext(ArrayItemContext.class,i);
		}
		public ArrayItemListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arrayItemList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterArrayItemList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitArrayItemList(this);
		}
	}

	public final ArrayItemListContext arrayItemList() throws RecognitionException {
		ArrayItemListContext _localctx = new ArrayItemListContext(_ctx, getState());
		enterRule(_localctx, 182, RULE_arrayItemList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1284);
			arrayItem();
			setState(1289);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,131,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1285);
					match(Comma);
					setState(1286);
					arrayItem();
					}
					} 
				}
				setState(1291);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,131,_ctx);
			}
			setState(1293);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Comma) {
				{
				setState(1292);
				match(Comma);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArrayItemContext extends ParserRuleContext {
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public ArrayItemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arrayItem; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterArrayItem(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitArrayItem(this);
		}
	}

	public final ArrayItemContext arrayItem() throws RecognitionException {
		ArrayItemContext _localctx = new ArrayItemContext(_ctx, getState());
		enterRule(_localctx, 184, RULE_arrayItem);
		int _la;
		try {
			setState(1307);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,135,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1295);
				expression(0);
				setState(1298);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==DoubleArrow) {
					{
					setState(1296);
					match(DoubleArrow);
					setState(1297);
					expression(0);
					}
				}

				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1303);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
					{
					setState(1300);
					expression(0);
					setState(1301);
					match(DoubleArrow);
					}
				}

				setState(1305);
				match(Ampersand);
				setState(1306);
				chain();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LambdaFunctionUseVarsContext extends ParserRuleContext {
		public TerminalNode Use() { return getToken(PHPParser.Use, 0); }
		public List<LambdaFunctionUseVarContext> lambdaFunctionUseVar() {
			return getRuleContexts(LambdaFunctionUseVarContext.class);
		}
		public LambdaFunctionUseVarContext lambdaFunctionUseVar(int i) {
			return getRuleContext(LambdaFunctionUseVarContext.class,i);
		}
		public LambdaFunctionUseVarsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lambdaFunctionUseVars; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterLambdaFunctionUseVars(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitLambdaFunctionUseVars(this);
		}
	}

	public final LambdaFunctionUseVarsContext lambdaFunctionUseVars() throws RecognitionException {
		LambdaFunctionUseVarsContext _localctx = new LambdaFunctionUseVarsContext(_ctx, getState());
		enterRule(_localctx, 186, RULE_lambdaFunctionUseVars);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1309);
			match(Use);
			setState(1310);
			match(OpenRoundBracket);
			setState(1311);
			lambdaFunctionUseVar();
			setState(1316);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(1312);
				match(Comma);
				setState(1313);
				lambdaFunctionUseVar();
				}
				}
				setState(1318);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(1319);
			match(CloseRoundBracket);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LambdaFunctionUseVarContext extends ParserRuleContext {
		public TerminalNode VarName() { return getToken(PHPParser.VarName, 0); }
		public LambdaFunctionUseVarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lambdaFunctionUseVar; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterLambdaFunctionUseVar(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitLambdaFunctionUseVar(this);
		}
	}

	public final LambdaFunctionUseVarContext lambdaFunctionUseVar() throws RecognitionException {
		LambdaFunctionUseVarContext _localctx = new LambdaFunctionUseVarContext(_ctx, getState());
		enterRule(_localctx, 188, RULE_lambdaFunctionUseVar);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1322);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Ampersand) {
				{
				setState(1321);
				match(Ampersand);
				}
			}

			setState(1324);
			match(VarName);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QualifiedStaticTypeRefContext extends ParserRuleContext {
		public QualifiedNamespaceNameContext qualifiedNamespaceName() {
			return getRuleContext(QualifiedNamespaceNameContext.class,0);
		}
		public GenericDynamicArgsContext genericDynamicArgs() {
			return getRuleContext(GenericDynamicArgsContext.class,0);
		}
		public TerminalNode Static() { return getToken(PHPParser.Static, 0); }
		public QualifiedStaticTypeRefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualifiedStaticTypeRef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterQualifiedStaticTypeRef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitQualifiedStaticTypeRef(this);
		}
	}

	public final QualifiedStaticTypeRefContext qualifiedStaticTypeRef() throws RecognitionException {
		QualifiedStaticTypeRefContext _localctx = new QualifiedStaticTypeRefContext(_ctx, getState());
		enterRule(_localctx, 190, RULE_qualifiedStaticTypeRef);
		int _la;
		try {
			setState(1331);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,139,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1326);
				qualifiedNamespaceName();
				setState(1328);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Lgeneric) {
					{
					setState(1327);
					genericDynamicArgs();
					}
				}

				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1330);
				match(Static);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeRefContext extends ParserRuleContext {
		public QualifiedNamespaceNameContext qualifiedNamespaceName() {
			return getRuleContext(QualifiedNamespaceNameContext.class,0);
		}
		public IndirectTypeRefContext indirectTypeRef() {
			return getRuleContext(IndirectTypeRefContext.class,0);
		}
		public GenericDynamicArgsContext genericDynamicArgs() {
			return getRuleContext(GenericDynamicArgsContext.class,0);
		}
		public PrimitiveTypeContext primitiveType() {
			return getRuleContext(PrimitiveTypeContext.class,0);
		}
		public TerminalNode Static() { return getToken(PHPParser.Static, 0); }
		public TypeRefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeRef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterTypeRef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitTypeRef(this);
		}
	}

	public final TypeRefContext typeRef() throws RecognitionException {
		TypeRefContext _localctx = new TypeRefContext(_ctx, getState());
		enterRule(_localctx, 192, RULE_typeRef);
		try {
			setState(1342);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,142,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1335);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,140,_ctx) ) {
				case 1:
					{
					setState(1333);
					qualifiedNamespaceName();
					}
					break;
				case 2:
					{
					setState(1334);
					indirectTypeRef();
					}
					break;
				}
				setState(1338);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,141,_ctx) ) {
				case 1:
					{
					setState(1337);
					genericDynamicArgs();
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1340);
				primitiveType();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1341);
				match(Static);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IndirectTypeRefContext extends ParserRuleContext {
		public ChainBaseContext chainBase() {
			return getRuleContext(ChainBaseContext.class,0);
		}
		public List<KeyedFieldNameContext> keyedFieldName() {
			return getRuleContexts(KeyedFieldNameContext.class);
		}
		public KeyedFieldNameContext keyedFieldName(int i) {
			return getRuleContext(KeyedFieldNameContext.class,i);
		}
		public IndirectTypeRefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_indirectTypeRef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterIndirectTypeRef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitIndirectTypeRef(this);
		}
	}

	public final IndirectTypeRefContext indirectTypeRef() throws RecognitionException {
		IndirectTypeRefContext _localctx = new IndirectTypeRefContext(_ctx, getState());
		enterRule(_localctx, 194, RULE_indirectTypeRef);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1344);
			chainBase();
			setState(1349);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,143,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1345);
					match(ObjectOperator);
					setState(1346);
					keyedFieldName();
					}
					} 
				}
				setState(1351);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,143,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QualifiedNamespaceNameContext extends ParserRuleContext {
		public NamespaceNameListContext namespaceNameList() {
			return getRuleContext(NamespaceNameListContext.class,0);
		}
		public TerminalNode Namespace() { return getToken(PHPParser.Namespace, 0); }
		public QualifiedNamespaceNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualifiedNamespaceName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterQualifiedNamespaceName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitQualifiedNamespaceName(this);
		}
	}

	public final QualifiedNamespaceNameContext qualifiedNamespaceName() throws RecognitionException {
		QualifiedNamespaceNameContext _localctx = new QualifiedNamespaceNameContext(_ctx, getState());
		enterRule(_localctx, 196, RULE_qualifiedNamespaceName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1353);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,144,_ctx) ) {
			case 1:
				{
				setState(1352);
				match(Namespace);
				}
				break;
			}
			setState(1356);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NamespaceSeparator) {
				{
				setState(1355);
				match(NamespaceSeparator);
				}
			}

			setState(1358);
			namespaceNameList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NamespaceNameListContext extends ParserRuleContext {
		public List<IdentifierContext> identifier() {
			return getRuleContexts(IdentifierContext.class);
		}
		public IdentifierContext identifier(int i) {
			return getRuleContext(IdentifierContext.class,i);
		}
		public NamespaceNameListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_namespaceNameList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterNamespaceNameList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitNamespaceNameList(this);
		}
	}

	public final NamespaceNameListContext namespaceNameList() throws RecognitionException {
		NamespaceNameListContext _localctx = new NamespaceNameListContext(_ctx, getState());
		enterRule(_localctx, 198, RULE_namespaceNameList);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1360);
			identifier();
			setState(1365);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,146,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1361);
					match(NamespaceSeparator);
					setState(1362);
					identifier();
					}
					} 
				}
				setState(1367);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,146,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QualifiedNamespaceNameListContext extends ParserRuleContext {
		public List<QualifiedNamespaceNameContext> qualifiedNamespaceName() {
			return getRuleContexts(QualifiedNamespaceNameContext.class);
		}
		public QualifiedNamespaceNameContext qualifiedNamespaceName(int i) {
			return getRuleContext(QualifiedNamespaceNameContext.class,i);
		}
		public QualifiedNamespaceNameListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualifiedNamespaceNameList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterQualifiedNamespaceNameList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitQualifiedNamespaceNameList(this);
		}
	}

	public final QualifiedNamespaceNameListContext qualifiedNamespaceNameList() throws RecognitionException {
		QualifiedNamespaceNameListContext _localctx = new QualifiedNamespaceNameListContext(_ctx, getState());
		enterRule(_localctx, 200, RULE_qualifiedNamespaceNameList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1368);
			qualifiedNamespaceName();
			setState(1373);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(1369);
				match(Comma);
				setState(1370);
				qualifiedNamespaceName();
				}
				}
				setState(1375);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArgumentsContext extends ParserRuleContext {
		public List<ActualArgumentContext> actualArgument() {
			return getRuleContexts(ActualArgumentContext.class);
		}
		public ActualArgumentContext actualArgument(int i) {
			return getRuleContext(ActualArgumentContext.class,i);
		}
		public YieldExpressionContext yieldExpression() {
			return getRuleContext(YieldExpressionContext.class,0);
		}
		public ArgumentsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arguments; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterArguments(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitArguments(this);
		}
	}

	public final ArgumentsContext arguments() throws RecognitionException {
		ArgumentsContext _localctx = new ArgumentsContext(_ctx, getState());
		enterRule(_localctx, 202, RULE_arguments);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1376);
			match(OpenRoundBracket);
			setState(1386);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,149,_ctx) ) {
			case 1:
				{
				setState(1377);
				actualArgument();
				setState(1382);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==Comma) {
					{
					{
					setState(1378);
					match(Comma);
					setState(1379);
					actualArgument();
					}
					}
					setState(1384);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
				break;
			case 2:
				{
				setState(1385);
				yieldExpression();
				}
				break;
			}
			setState(1388);
			match(CloseRoundBracket);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ActualArgumentContext extends ParserRuleContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public ActualArgumentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_actualArgument; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterActualArgument(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitActualArgument(this);
		}
	}

	public final ActualArgumentContext actualArgument() throws RecognitionException {
		ActualArgumentContext _localctx = new ActualArgumentContext(_ctx, getState());
		enterRule(_localctx, 204, RULE_actualArgument);
		int _la;
		try {
			setState(1396);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case Inc:
			case Dec:
			case NamespaceSeparator:
			case Ellipsis:
			case Bang:
			case Plus:
			case Minus:
			case Tilde:
			case SuppressWarnings:
			case Dollar:
			case OpenRoundBracket:
			case OpenSquareBracket:
			case VarName:
			case Label:
			case Octal:
			case Decimal:
			case Real:
			case Hex:
			case Binary:
			case BackQuoteString:
			case SingleQuoteString:
			case DoubleQuote:
			case StartNowDoc:
			case StartHereDoc:
				enterOuterAlt(_localctx, 1);
				{
				setState(1391);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Ellipsis) {
					{
					setState(1390);
					match(Ellipsis);
					}
				}

				setState(1393);
				expression(0);
				}
				break;
			case Ampersand:
				enterOuterAlt(_localctx, 2);
				{
				setState(1394);
				match(Ampersand);
				setState(1395);
				chain();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstantInititalizerContext extends ParserRuleContext {
		public ConstantContext constant() {
			return getRuleContext(ConstantContext.class,0);
		}
		public StringContext string() {
			return getRuleContext(StringContext.class,0);
		}
		public TerminalNode Array() { return getToken(PHPParser.Array, 0); }
		public ConstantArrayItemListContext constantArrayItemList() {
			return getRuleContext(ConstantArrayItemListContext.class,0);
		}
		public ConstantInititalizerContext constantInititalizer() {
			return getRuleContext(ConstantInititalizerContext.class,0);
		}
		public ConstantInititalizerContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constantInititalizer; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterConstantInititalizer(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitConstantInititalizer(this);
		}
	}

	public final ConstantInititalizerContext constantInititalizer() throws RecognitionException {
		ConstantInititalizerContext _localctx = new ConstantInititalizerContext(_ctx, getState());
		enterRule(_localctx, 206, RULE_constantInititalizer);
		int _la;
		try {
			setState(1419);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,156,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1398);
				constant();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1399);
				string();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1400);
				match(Array);
				setState(1401);
				match(OpenRoundBracket);
				setState(1406);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Dollar - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
					{
					setState(1402);
					constantArrayItemList();
					setState(1404);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if (_la==Comma) {
						{
						setState(1403);
						match(Comma);
						}
					}

					}
				}

				setState(1408);
				match(CloseRoundBracket);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1409);
				match(OpenSquareBracket);
				setState(1414);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Dollar - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
					{
					setState(1410);
					constantArrayItemList();
					setState(1412);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if (_la==Comma) {
						{
						setState(1411);
						match(Comma);
						}
					}

					}
				}

				setState(1416);
				match(CloseSquareBracket);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1417);
				_la = _input.LA(1);
				if ( !(_la==Plus || _la==Minus) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(1418);
				constantInititalizer();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstantArrayItemListContext extends ParserRuleContext {
		public List<ConstantArrayItemContext> constantArrayItem() {
			return getRuleContexts(ConstantArrayItemContext.class);
		}
		public ConstantArrayItemContext constantArrayItem(int i) {
			return getRuleContext(ConstantArrayItemContext.class,i);
		}
		public ConstantArrayItemListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constantArrayItemList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterConstantArrayItemList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitConstantArrayItemList(this);
		}
	}

	public final ConstantArrayItemListContext constantArrayItemList() throws RecognitionException {
		ConstantArrayItemListContext _localctx = new ConstantArrayItemListContext(_ctx, getState());
		enterRule(_localctx, 208, RULE_constantArrayItemList);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1421);
			constantArrayItem();
			setState(1426);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,157,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1422);
					match(Comma);
					setState(1423);
					constantArrayItem();
					}
					} 
				}
				setState(1428);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,157,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstantArrayItemContext extends ParserRuleContext {
		public List<ConstantInititalizerContext> constantInititalizer() {
			return getRuleContexts(ConstantInititalizerContext.class);
		}
		public ConstantInititalizerContext constantInititalizer(int i) {
			return getRuleContext(ConstantInititalizerContext.class,i);
		}
		public ConstantArrayItemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constantArrayItem; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterConstantArrayItem(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitConstantArrayItem(this);
		}
	}

	public final ConstantArrayItemContext constantArrayItem() throws RecognitionException {
		ConstantArrayItemContext _localctx = new ConstantArrayItemContext(_ctx, getState());
		enterRule(_localctx, 210, RULE_constantArrayItem);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1429);
			constantInititalizer();
			setState(1432);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==DoubleArrow) {
				{
				setState(1430);
				match(DoubleArrow);
				setState(1431);
				constantInititalizer();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstantContext extends ParserRuleContext {
		public TerminalNode Null() { return getToken(PHPParser.Null, 0); }
		public LiteralConstantContext literalConstant() {
			return getRuleContext(LiteralConstantContext.class,0);
		}
		public MagicConstantContext magicConstant() {
			return getRuleContext(MagicConstantContext.class,0);
		}
		public ClassConstantContext classConstant() {
			return getRuleContext(ClassConstantContext.class,0);
		}
		public QualifiedNamespaceNameContext qualifiedNamespaceName() {
			return getRuleContext(QualifiedNamespaceNameContext.class,0);
		}
		public ConstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constant; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterConstant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitConstant(this);
		}
	}

	public final ConstantContext constant() throws RecognitionException {
		ConstantContext _localctx = new ConstantContext(_ctx, getState());
		enterRule(_localctx, 212, RULE_constant);
		try {
			setState(1439);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,159,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1434);
				match(Null);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1435);
				literalConstant();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1436);
				magicConstant();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1437);
				classConstant();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1438);
				qualifiedNamespaceName();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LiteralConstantContext extends ParserRuleContext {
		public TerminalNode Real() { return getToken(PHPParser.Real, 0); }
		public TerminalNode BooleanConstant() { return getToken(PHPParser.BooleanConstant, 0); }
		public NumericConstantContext numericConstant() {
			return getRuleContext(NumericConstantContext.class,0);
		}
		public StringConstantContext stringConstant() {
			return getRuleContext(StringConstantContext.class,0);
		}
		public LiteralConstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_literalConstant; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterLiteralConstant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitLiteralConstant(this);
		}
	}

	public final LiteralConstantContext literalConstant() throws RecognitionException {
		LiteralConstantContext _localctx = new LiteralConstantContext(_ctx, getState());
		enterRule(_localctx, 214, RULE_literalConstant);
		try {
			setState(1445);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Real:
				enterOuterAlt(_localctx, 1);
				{
				setState(1441);
				match(Real);
				}
				break;
			case BooleanConstant:
				enterOuterAlt(_localctx, 2);
				{
				setState(1442);
				match(BooleanConstant);
				}
				break;
			case Octal:
			case Decimal:
			case Hex:
			case Binary:
				enterOuterAlt(_localctx, 3);
				{
				setState(1443);
				numericConstant();
				}
				break;
			case Label:
				enterOuterAlt(_localctx, 4);
				{
				setState(1444);
				stringConstant();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NumericConstantContext extends ParserRuleContext {
		public TerminalNode Octal() { return getToken(PHPParser.Octal, 0); }
		public TerminalNode Decimal() { return getToken(PHPParser.Decimal, 0); }
		public TerminalNode Hex() { return getToken(PHPParser.Hex, 0); }
		public TerminalNode Binary() { return getToken(PHPParser.Binary, 0); }
		public NumericConstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_numericConstant; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterNumericConstant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitNumericConstant(this);
		}
	}

	public final NumericConstantContext numericConstant() throws RecognitionException {
		NumericConstantContext _localctx = new NumericConstantContext(_ctx, getState());
		enterRule(_localctx, 216, RULE_numericConstant);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1447);
			_la = _input.LA(1);
			if ( !(((((_la - 211)) & ~0x3f) == 0 && ((1L << (_la - 211)) & ((1L << (Octal - 211)) | (1L << (Decimal - 211)) | (1L << (Hex - 211)) | (1L << (Binary - 211)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ClassConstantContext extends ParserRuleContext {
		public TerminalNode Class() { return getToken(PHPParser.Class, 0); }
		public TerminalNode Parent_() { return getToken(PHPParser.Parent_, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode Constructor() { return getToken(PHPParser.Constructor, 0); }
		public TerminalNode Get() { return getToken(PHPParser.Get, 0); }
		public TerminalNode Set() { return getToken(PHPParser.Set, 0); }
		public QualifiedStaticTypeRefContext qualifiedStaticTypeRef() {
			return getRuleContext(QualifiedStaticTypeRefContext.class,0);
		}
		public KeyedVariableContext keyedVariable() {
			return getRuleContext(KeyedVariableContext.class,0);
		}
		public ClassConstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_classConstant; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterClassConstant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitClassConstant(this);
		}
	}

	public final ClassConstantContext classConstant() throws RecognitionException {
		ClassConstantContext _localctx = new ClassConstantContext(_ctx, getState());
		enterRule(_localctx, 218, RULE_classConstant);
		int _la;
		try {
			setState(1464);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,163,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1449);
				_la = _input.LA(1);
				if ( !(_la==Class || _la==Parent_) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(1450);
				match(DoubleColon);
				setState(1455);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,161,_ctx) ) {
				case 1:
					{
					setState(1451);
					identifier();
					}
					break;
				case 2:
					{
					setState(1452);
					match(Constructor);
					}
					break;
				case 3:
					{
					setState(1453);
					match(Get);
					}
					break;
				case 4:
					{
					setState(1454);
					match(Set);
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1459);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case Abstract:
				case Array:
				case As:
				case BinaryCast:
				case BoolType:
				case BooleanConstant:
				case Break:
				case Callable:
				case Case:
				case Catch:
				case Class:
				case Clone:
				case Const:
				case Continue:
				case Declare:
				case Default:
				case Do:
				case DoubleCast:
				case DoubleType:
				case Echo:
				case Else:
				case ElseIf:
				case Empty:
				case EndDeclare:
				case EndFor:
				case EndForeach:
				case EndIf:
				case EndSwitch:
				case EndWhile:
				case Eval:
				case Exit:
				case Extends:
				case Final:
				case Finally:
				case FloatCast:
				case For:
				case Foreach:
				case Function:
				case Global:
				case Goto:
				case If:
				case Implements:
				case Import:
				case Include:
				case IncludeOnce:
				case InstanceOf:
				case InsteadOf:
				case Int8Cast:
				case Int16Cast:
				case Int64Type:
				case IntType:
				case Interface:
				case IsSet:
				case List:
				case LogicalAnd:
				case LogicalOr:
				case LogicalXor:
				case Namespace:
				case New:
				case Null:
				case ObjectType:
				case Parent_:
				case Partial:
				case Print:
				case Private:
				case Protected:
				case Public:
				case Require:
				case RequireOnce:
				case Resource:
				case Return:
				case Static:
				case StringType:
				case Switch:
				case Throw:
				case Trait:
				case Try:
				case Typeof:
				case UintCast:
				case UnicodeCast:
				case Unset:
				case Use:
				case Var:
				case While:
				case Yield:
				case Get:
				case Set:
				case Call:
				case CallStatic:
				case Constructor:
				case Destruct:
				case Wakeup:
				case Sleep:
				case Autoload:
				case IsSet__:
				case Unset__:
				case ToString__:
				case Invoke:
				case SetState:
				case Clone__:
				case DebugInfo:
				case Namespace__:
				case Class__:
				case Traic__:
				case Function__:
				case Method__:
				case Line__:
				case File__:
				case Dir__:
				case NamespaceSeparator:
				case Label:
					{
					setState(1457);
					qualifiedStaticTypeRef();
					}
					break;
				case Dollar:
				case VarName:
					{
					setState(1458);
					keyedVariable();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(1461);
				match(DoubleColon);
				setState(1462);
				identifier();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StringConstantContext extends ParserRuleContext {
		public TerminalNode Label() { return getToken(PHPParser.Label, 0); }
		public StringConstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stringConstant; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterStringConstant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitStringConstant(this);
		}
	}

	public final StringConstantContext stringConstant() throws RecognitionException {
		StringConstantContext _localctx = new StringConstantContext(_ctx, getState());
		enterRule(_localctx, 220, RULE_stringConstant);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1466);
			match(Label);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StringContext extends ParserRuleContext {
		public TerminalNode StartHereDoc() { return getToken(PHPParser.StartHereDoc, 0); }
		public List<TerminalNode> HereDocText() { return getTokens(PHPParser.HereDocText); }
		public TerminalNode HereDocText(int i) {
			return getToken(PHPParser.HereDocText, i);
		}
		public TerminalNode StartNowDoc() { return getToken(PHPParser.StartNowDoc, 0); }
		public TerminalNode SingleQuoteString() { return getToken(PHPParser.SingleQuoteString, 0); }
		public List<TerminalNode> DoubleQuote() { return getTokens(PHPParser.DoubleQuote); }
		public TerminalNode DoubleQuote(int i) {
			return getToken(PHPParser.DoubleQuote, i);
		}
		public List<InterpolatedStringPartContext> interpolatedStringPart() {
			return getRuleContexts(InterpolatedStringPartContext.class);
		}
		public InterpolatedStringPartContext interpolatedStringPart(int i) {
			return getRuleContext(InterpolatedStringPartContext.class,i);
		}
		public StringContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_string; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterString(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitString(this);
		}
	}

	public final StringContext string() throws RecognitionException {
		StringContext _localctx = new StringContext(_ctx, getState());
		enterRule(_localctx, 222, RULE_string);
		int _la;
		try {
			int _alt;
			setState(1489);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case StartHereDoc:
				enterOuterAlt(_localctx, 1);
				{
				setState(1468);
				match(StartHereDoc);
				setState(1470); 
				_errHandler.sync(this);
				_alt = 1;
				do {
					switch (_alt) {
					case 1:
						{
						{
						setState(1469);
						match(HereDocText);
						}
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(1472); 
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,164,_ctx);
				} while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER );
				}
				break;
			case StartNowDoc:
				enterOuterAlt(_localctx, 2);
				{
				setState(1474);
				match(StartNowDoc);
				setState(1476); 
				_errHandler.sync(this);
				_alt = 1;
				do {
					switch (_alt) {
					case 1:
						{
						{
						setState(1475);
						match(HereDocText);
						}
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(1478); 
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,165,_ctx);
				} while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER );
				}
				break;
			case SingleQuoteString:
				enterOuterAlt(_localctx, 3);
				{
				setState(1480);
				match(SingleQuoteString);
				}
				break;
			case DoubleQuote:
				enterOuterAlt(_localctx, 4);
				{
				setState(1481);
				match(DoubleQuote);
				setState(1485);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (StringPart - 179)))) != 0)) {
					{
					{
					setState(1482);
					interpolatedStringPart();
					}
					}
					setState(1487);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(1488);
				match(DoubleQuote);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InterpolatedStringPartContext extends ParserRuleContext {
		public TerminalNode StringPart() { return getToken(PHPParser.StringPart, 0); }
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public InterpolatedStringPartContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_interpolatedStringPart; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterInterpolatedStringPart(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitInterpolatedStringPart(this);
		}
	}

	public final InterpolatedStringPartContext interpolatedStringPart() throws RecognitionException {
		InterpolatedStringPartContext _localctx = new InterpolatedStringPartContext(_ctx, getState());
		enterRule(_localctx, 224, RULE_interpolatedStringPart);
		try {
			setState(1493);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case StringPart:
				enterOuterAlt(_localctx, 1);
				{
				setState(1491);
				match(StringPart);
				}
				break;
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case NamespaceSeparator:
			case Dollar:
			case OpenRoundBracket:
			case VarName:
			case Label:
				enterOuterAlt(_localctx, 2);
				{
				setState(1492);
				chain();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ChainListContext extends ParserRuleContext {
		public List<ChainContext> chain() {
			return getRuleContexts(ChainContext.class);
		}
		public ChainContext chain(int i) {
			return getRuleContext(ChainContext.class,i);
		}
		public ChainListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_chainList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterChainList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitChainList(this);
		}
	}

	public final ChainListContext chainList() throws RecognitionException {
		ChainListContext _localctx = new ChainListContext(_ctx, getState());
		enterRule(_localctx, 226, RULE_chainList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1495);
			chain();
			setState(1500);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(1496);
				match(Comma);
				setState(1497);
				chain();
				}
				}
				setState(1502);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ChainContext extends ParserRuleContext {
		public ChainBaseContext chainBase() {
			return getRuleContext(ChainBaseContext.class,0);
		}
		public FunctionCallContext functionCall() {
			return getRuleContext(FunctionCallContext.class,0);
		}
		public NewExprContext newExpr() {
			return getRuleContext(NewExprContext.class,0);
		}
		public List<MemberAccessContext> memberAccess() {
			return getRuleContexts(MemberAccessContext.class);
		}
		public MemberAccessContext memberAccess(int i) {
			return getRuleContext(MemberAccessContext.class,i);
		}
		public ChainContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_chain; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterChain(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitChain(this);
		}
	}

	public final ChainContext chain() throws RecognitionException {
		ChainContext _localctx = new ChainContext(_ctx, getState());
		enterRule(_localctx, 228, RULE_chain);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1509);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,170,_ctx) ) {
			case 1:
				{
				setState(1503);
				chainBase();
				}
				break;
			case 2:
				{
				setState(1504);
				functionCall();
				}
				break;
			case 3:
				{
				setState(1505);
				match(OpenRoundBracket);
				setState(1506);
				newExpr();
				setState(1507);
				match(CloseRoundBracket);
				}
				break;
			}
			setState(1514);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,171,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1511);
					memberAccess();
					}
					} 
				}
				setState(1516);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,171,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MemberAccessContext extends ParserRuleContext {
		public KeyedFieldNameContext keyedFieldName() {
			return getRuleContext(KeyedFieldNameContext.class,0);
		}
		public ActualArgumentsContext actualArguments() {
			return getRuleContext(ActualArgumentsContext.class,0);
		}
		public MemberAccessContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_memberAccess; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterMemberAccess(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitMemberAccess(this);
		}
	}

	public final MemberAccessContext memberAccess() throws RecognitionException {
		MemberAccessContext _localctx = new MemberAccessContext(_ctx, getState());
		enterRule(_localctx, 230, RULE_memberAccess);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1517);
			match(ObjectOperator);
			setState(1518);
			keyedFieldName();
			setState(1520);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,172,_ctx) ) {
			case 1:
				{
				setState(1519);
				actualArguments();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FunctionCallContext extends ParserRuleContext {
		public FunctionCallNameContext functionCallName() {
			return getRuleContext(FunctionCallNameContext.class,0);
		}
		public ActualArgumentsContext actualArguments() {
			return getRuleContext(ActualArgumentsContext.class,0);
		}
		public FunctionCallContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_functionCall; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterFunctionCall(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitFunctionCall(this);
		}
	}

	public final FunctionCallContext functionCall() throws RecognitionException {
		FunctionCallContext _localctx = new FunctionCallContext(_ctx, getState());
		enterRule(_localctx, 232, RULE_functionCall);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1522);
			functionCallName();
			setState(1523);
			actualArguments();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FunctionCallNameContext extends ParserRuleContext {
		public QualifiedNamespaceNameContext qualifiedNamespaceName() {
			return getRuleContext(QualifiedNamespaceNameContext.class,0);
		}
		public ClassConstantContext classConstant() {
			return getRuleContext(ClassConstantContext.class,0);
		}
		public ChainBaseContext chainBase() {
			return getRuleContext(ChainBaseContext.class,0);
		}
		public FunctionCallNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_functionCallName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterFunctionCallName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitFunctionCallName(this);
		}
	}

	public final FunctionCallNameContext functionCallName() throws RecognitionException {
		FunctionCallNameContext _localctx = new FunctionCallNameContext(_ctx, getState());
		enterRule(_localctx, 234, RULE_functionCallName);
		try {
			setState(1528);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,173,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1525);
				qualifiedNamespaceName();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1526);
				classConstant();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(1527);
				chainBase();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ActualArgumentsContext extends ParserRuleContext {
		public ArgumentsContext arguments() {
			return getRuleContext(ArgumentsContext.class,0);
		}
		public GenericDynamicArgsContext genericDynamicArgs() {
			return getRuleContext(GenericDynamicArgsContext.class,0);
		}
		public List<SquareCurlyExpressionContext> squareCurlyExpression() {
			return getRuleContexts(SquareCurlyExpressionContext.class);
		}
		public SquareCurlyExpressionContext squareCurlyExpression(int i) {
			return getRuleContext(SquareCurlyExpressionContext.class,i);
		}
		public ActualArgumentsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_actualArguments; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterActualArguments(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitActualArguments(this);
		}
	}

	public final ActualArgumentsContext actualArguments() throws RecognitionException {
		ActualArgumentsContext _localctx = new ActualArgumentsContext(_ctx, getState());
		enterRule(_localctx, 236, RULE_actualArguments);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1531);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Lgeneric) {
				{
				setState(1530);
				genericDynamicArgs();
				}
			}

			setState(1533);
			arguments();
			setState(1537);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,175,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1534);
					squareCurlyExpression();
					}
					} 
				}
				setState(1539);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,175,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ChainBaseContext extends ParserRuleContext {
		public List<KeyedVariableContext> keyedVariable() {
			return getRuleContexts(KeyedVariableContext.class);
		}
		public KeyedVariableContext keyedVariable(int i) {
			return getRuleContext(KeyedVariableContext.class,i);
		}
		public QualifiedStaticTypeRefContext qualifiedStaticTypeRef() {
			return getRuleContext(QualifiedStaticTypeRefContext.class,0);
		}
		public ChainBaseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_chainBase; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterChainBase(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitChainBase(this);
		}
	}

	public final ChainBaseContext chainBase() throws RecognitionException {
		ChainBaseContext _localctx = new ChainBaseContext(_ctx, getState());
		enterRule(_localctx, 238, RULE_chainBase);
		try {
			setState(1549);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Dollar:
			case VarName:
				enterOuterAlt(_localctx, 1);
				{
				setState(1540);
				keyedVariable();
				setState(1543);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,176,_ctx) ) {
				case 1:
					{
					setState(1541);
					match(DoubleColon);
					setState(1542);
					keyedVariable();
					}
					break;
				}
				}
				break;
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case NamespaceSeparator:
			case Label:
				enterOuterAlt(_localctx, 2);
				{
				setState(1545);
				qualifiedStaticTypeRef();
				setState(1546);
				match(DoubleColon);
				setState(1547);
				keyedVariable();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class KeyedFieldNameContext extends ParserRuleContext {
		public KeyedSimpleFieldNameContext keyedSimpleFieldName() {
			return getRuleContext(KeyedSimpleFieldNameContext.class,0);
		}
		public KeyedVariableContext keyedVariable() {
			return getRuleContext(KeyedVariableContext.class,0);
		}
		public KeyedFieldNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_keyedFieldName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterKeyedFieldName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitKeyedFieldName(this);
		}
	}

	public final KeyedFieldNameContext keyedFieldName() throws RecognitionException {
		KeyedFieldNameContext _localctx = new KeyedFieldNameContext(_ctx, getState());
		enterRule(_localctx, 240, RULE_keyedFieldName);
		try {
			setState(1553);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case OpenCurlyBracket:
			case Label:
				enterOuterAlt(_localctx, 1);
				{
				setState(1551);
				keyedSimpleFieldName();
				}
				break;
			case Dollar:
			case VarName:
				enterOuterAlt(_localctx, 2);
				{
				setState(1552);
				keyedVariable();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class KeyedSimpleFieldNameContext extends ParserRuleContext {
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<SquareCurlyExpressionContext> squareCurlyExpression() {
			return getRuleContexts(SquareCurlyExpressionContext.class);
		}
		public SquareCurlyExpressionContext squareCurlyExpression(int i) {
			return getRuleContext(SquareCurlyExpressionContext.class,i);
		}
		public KeyedSimpleFieldNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_keyedSimpleFieldName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterKeyedSimpleFieldName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitKeyedSimpleFieldName(this);
		}
	}

	public final KeyedSimpleFieldNameContext keyedSimpleFieldName() throws RecognitionException {
		KeyedSimpleFieldNameContext _localctx = new KeyedSimpleFieldNameContext(_ctx, getState());
		enterRule(_localctx, 242, RULE_keyedSimpleFieldName);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1560);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Abstract:
			case Array:
			case As:
			case BinaryCast:
			case BoolType:
			case BooleanConstant:
			case Break:
			case Callable:
			case Case:
			case Catch:
			case Class:
			case Clone:
			case Const:
			case Continue:
			case Declare:
			case Default:
			case Do:
			case DoubleCast:
			case DoubleType:
			case Echo:
			case Else:
			case ElseIf:
			case Empty:
			case EndDeclare:
			case EndFor:
			case EndForeach:
			case EndIf:
			case EndSwitch:
			case EndWhile:
			case Eval:
			case Exit:
			case Extends:
			case Final:
			case Finally:
			case FloatCast:
			case For:
			case Foreach:
			case Function:
			case Global:
			case Goto:
			case If:
			case Implements:
			case Import:
			case Include:
			case IncludeOnce:
			case InstanceOf:
			case InsteadOf:
			case Int8Cast:
			case Int16Cast:
			case Int64Type:
			case IntType:
			case Interface:
			case IsSet:
			case List:
			case LogicalAnd:
			case LogicalOr:
			case LogicalXor:
			case Namespace:
			case New:
			case Null:
			case ObjectType:
			case Parent_:
			case Partial:
			case Print:
			case Private:
			case Protected:
			case Public:
			case Require:
			case RequireOnce:
			case Resource:
			case Return:
			case Static:
			case StringType:
			case Switch:
			case Throw:
			case Trait:
			case Try:
			case Typeof:
			case UintCast:
			case UnicodeCast:
			case Unset:
			case Use:
			case Var:
			case While:
			case Yield:
			case Get:
			case Set:
			case Call:
			case CallStatic:
			case Constructor:
			case Destruct:
			case Wakeup:
			case Sleep:
			case Autoload:
			case IsSet__:
			case Unset__:
			case ToString__:
			case Invoke:
			case SetState:
			case Clone__:
			case DebugInfo:
			case Namespace__:
			case Class__:
			case Traic__:
			case Function__:
			case Method__:
			case Line__:
			case File__:
			case Dir__:
			case Label:
				{
				setState(1555);
				identifier();
				}
				break;
			case OpenCurlyBracket:
				{
				setState(1556);
				match(OpenCurlyBracket);
				setState(1557);
				expression(0);
				setState(1558);
				match(CloseCurlyBracket);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(1565);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,180,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1562);
					squareCurlyExpression();
					}
					} 
				}
				setState(1567);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,180,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class KeyedVariableContext extends ParserRuleContext {
		public TerminalNode VarName() { return getToken(PHPParser.VarName, 0); }
		public List<TerminalNode> Dollar() { return getTokens(PHPParser.Dollar); }
		public TerminalNode Dollar(int i) {
			return getToken(PHPParser.Dollar, i);
		}
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public List<SquareCurlyExpressionContext> squareCurlyExpression() {
			return getRuleContexts(SquareCurlyExpressionContext.class);
		}
		public SquareCurlyExpressionContext squareCurlyExpression(int i) {
			return getRuleContext(SquareCurlyExpressionContext.class,i);
		}
		public KeyedVariableContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_keyedVariable; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterKeyedVariable(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitKeyedVariable(this);
		}
	}

	public final KeyedVariableContext keyedVariable() throws RecognitionException {
		KeyedVariableContext _localctx = new KeyedVariableContext(_ctx, getState());
		enterRule(_localctx, 244, RULE_keyedVariable);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(1571);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,181,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1568);
					match(Dollar);
					}
					} 
				}
				setState(1573);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,181,_ctx);
			}
			setState(1580);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case VarName:
				{
				setState(1574);
				match(VarName);
				}
				break;
			case Dollar:
				{
				setState(1575);
				match(Dollar);
				setState(1576);
				match(OpenCurlyBracket);
				setState(1577);
				expression(0);
				setState(1578);
				match(CloseCurlyBracket);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(1585);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,183,_ctx);
			while ( _alt!=2 && _alt!= ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(1582);
					squareCurlyExpression();
					}
					} 
				}
				setState(1587);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,183,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SquareCurlyExpressionContext extends ParserRuleContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode OpenCurlyBracket() { return getToken(PHPParser.OpenCurlyBracket, 0); }
		public SquareCurlyExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_squareCurlyExpression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterSquareCurlyExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitSquareCurlyExpression(this);
		}
	}

	public final SquareCurlyExpressionContext squareCurlyExpression() throws RecognitionException {
		SquareCurlyExpressionContext _localctx = new SquareCurlyExpressionContext(_ctx, getState());
		enterRule(_localctx, 246, RULE_squareCurlyExpression);
		int _la;
		try {
			setState(1597);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case OpenSquareBracket:
				enterOuterAlt(_localctx, 1);
				{
				setState(1588);
				match(OpenSquareBracket);
				setState(1590);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)) | (1L << (Inc - 104)) | (1L << (Dec - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Bang - 179)) | (1L << (Plus - 179)) | (1L << (Minus - 179)) | (1L << (Tilde - 179)) | (1L << (SuppressWarnings - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (OpenSquareBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)) | (1L << (Octal - 179)) | (1L << (Decimal - 179)) | (1L << (Real - 179)) | (1L << (Hex - 179)) | (1L << (Binary - 179)) | (1L << (BackQuoteString - 179)) | (1L << (SingleQuoteString - 179)) | (1L << (DoubleQuote - 179)) | (1L << (StartNowDoc - 179)) | (1L << (StartHereDoc - 179)))) != 0)) {
					{
					setState(1589);
					expression(0);
					}
				}

				setState(1592);
				match(CloseSquareBracket);
				}
				break;
			case OpenCurlyBracket:
				enterOuterAlt(_localctx, 2);
				{
				setState(1593);
				match(OpenCurlyBracket);
				setState(1594);
				expression(0);
				setState(1595);
				match(CloseCurlyBracket);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AssignmentListContext extends ParserRuleContext {
		public List<AssignmentListElementContext> assignmentListElement() {
			return getRuleContexts(AssignmentListElementContext.class);
		}
		public AssignmentListElementContext assignmentListElement(int i) {
			return getRuleContext(AssignmentListElementContext.class,i);
		}
		public AssignmentListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assignmentList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAssignmentList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAssignmentList(this);
		}
	}

	public final AssignmentListContext assignmentList() throws RecognitionException {
		AssignmentListContext _localctx = new AssignmentListContext(_ctx, getState());
		enterRule(_localctx, 248, RULE_assignmentList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1600);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)))) != 0)) {
				{
				setState(1599);
				assignmentListElement();
				}
			}

			setState(1608);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(1602);
				match(Comma);
				setState(1604);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || ((((_la - 179)) & ~0x3f) == 0 && ((1L << (_la - 179)) & ((1L << (NamespaceSeparator - 179)) | (1L << (Dollar - 179)) | (1L << (OpenRoundBracket - 179)) | (1L << (VarName - 179)) | (1L << (Label - 179)))) != 0)) {
					{
					setState(1603);
					assignmentListElement();
					}
				}

				}
				}
				setState(1610);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AssignmentListElementContext extends ParserRuleContext {
		public ChainContext chain() {
			return getRuleContext(ChainContext.class,0);
		}
		public TerminalNode List() { return getToken(PHPParser.List, 0); }
		public AssignmentListContext assignmentList() {
			return getRuleContext(AssignmentListContext.class,0);
		}
		public AssignmentListElementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assignmentListElement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterAssignmentListElement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitAssignmentListElement(this);
		}
	}

	public final AssignmentListElementContext assignmentListElement() throws RecognitionException {
		AssignmentListElementContext _localctx = new AssignmentListElementContext(_ctx, getState());
		enterRule(_localctx, 250, RULE_assignmentListElement);
		try {
			setState(1617);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,189,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1611);
				chain();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1612);
				match(List);
				setState(1613);
				match(OpenRoundBracket);
				setState(1614);
				assignmentList();
				setState(1615);
				match(CloseRoundBracket);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ModifierContext extends ParserRuleContext {
		public TerminalNode Abstract() { return getToken(PHPParser.Abstract, 0); }
		public TerminalNode Final() { return getToken(PHPParser.Final, 0); }
		public ModifierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_modifier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterModifier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitModifier(this);
		}
	}

	public final ModifierContext modifier() throws RecognitionException {
		ModifierContext _localctx = new ModifierContext(_ctx, getState());
		enterRule(_localctx, 252, RULE_modifier);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1619);
			_la = _input.LA(1);
			if ( !(_la==Abstract || _la==Final) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IdentifierContext extends ParserRuleContext {
		public TerminalNode Label() { return getToken(PHPParser.Label, 0); }
		public TerminalNode Abstract() { return getToken(PHPParser.Abstract, 0); }
		public TerminalNode Array() { return getToken(PHPParser.Array, 0); }
		public TerminalNode As() { return getToken(PHPParser.As, 0); }
		public TerminalNode BinaryCast() { return getToken(PHPParser.BinaryCast, 0); }
		public TerminalNode BoolType() { return getToken(PHPParser.BoolType, 0); }
		public TerminalNode BooleanConstant() { return getToken(PHPParser.BooleanConstant, 0); }
		public TerminalNode Break() { return getToken(PHPParser.Break, 0); }
		public TerminalNode Callable() { return getToken(PHPParser.Callable, 0); }
		public TerminalNode Case() { return getToken(PHPParser.Case, 0); }
		public TerminalNode Catch() { return getToken(PHPParser.Catch, 0); }
		public TerminalNode Class() { return getToken(PHPParser.Class, 0); }
		public TerminalNode Clone() { return getToken(PHPParser.Clone, 0); }
		public TerminalNode Const() { return getToken(PHPParser.Const, 0); }
		public TerminalNode Continue() { return getToken(PHPParser.Continue, 0); }
		public TerminalNode Declare() { return getToken(PHPParser.Declare, 0); }
		public TerminalNode Default() { return getToken(PHPParser.Default, 0); }
		public TerminalNode Do() { return getToken(PHPParser.Do, 0); }
		public TerminalNode DoubleCast() { return getToken(PHPParser.DoubleCast, 0); }
		public TerminalNode DoubleType() { return getToken(PHPParser.DoubleType, 0); }
		public TerminalNode Echo() { return getToken(PHPParser.Echo, 0); }
		public TerminalNode Else() { return getToken(PHPParser.Else, 0); }
		public TerminalNode ElseIf() { return getToken(PHPParser.ElseIf, 0); }
		public TerminalNode Empty() { return getToken(PHPParser.Empty, 0); }
		public TerminalNode EndDeclare() { return getToken(PHPParser.EndDeclare, 0); }
		public TerminalNode EndFor() { return getToken(PHPParser.EndFor, 0); }
		public TerminalNode EndForeach() { return getToken(PHPParser.EndForeach, 0); }
		public TerminalNode EndIf() { return getToken(PHPParser.EndIf, 0); }
		public TerminalNode EndSwitch() { return getToken(PHPParser.EndSwitch, 0); }
		public TerminalNode EndWhile() { return getToken(PHPParser.EndWhile, 0); }
		public TerminalNode Eval() { return getToken(PHPParser.Eval, 0); }
		public TerminalNode Exit() { return getToken(PHPParser.Exit, 0); }
		public TerminalNode Extends() { return getToken(PHPParser.Extends, 0); }
		public TerminalNode Final() { return getToken(PHPParser.Final, 0); }
		public TerminalNode Finally() { return getToken(PHPParser.Finally, 0); }
		public TerminalNode FloatCast() { return getToken(PHPParser.FloatCast, 0); }
		public TerminalNode For() { return getToken(PHPParser.For, 0); }
		public TerminalNode Foreach() { return getToken(PHPParser.Foreach, 0); }
		public TerminalNode Function() { return getToken(PHPParser.Function, 0); }
		public TerminalNode Global() { return getToken(PHPParser.Global, 0); }
		public TerminalNode Goto() { return getToken(PHPParser.Goto, 0); }
		public TerminalNode If() { return getToken(PHPParser.If, 0); }
		public TerminalNode Implements() { return getToken(PHPParser.Implements, 0); }
		public TerminalNode Import() { return getToken(PHPParser.Import, 0); }
		public TerminalNode Include() { return getToken(PHPParser.Include, 0); }
		public TerminalNode IncludeOnce() { return getToken(PHPParser.IncludeOnce, 0); }
		public TerminalNode InstanceOf() { return getToken(PHPParser.InstanceOf, 0); }
		public TerminalNode InsteadOf() { return getToken(PHPParser.InsteadOf, 0); }
		public TerminalNode Int16Cast() { return getToken(PHPParser.Int16Cast, 0); }
		public TerminalNode Int64Type() { return getToken(PHPParser.Int64Type, 0); }
		public TerminalNode Int8Cast() { return getToken(PHPParser.Int8Cast, 0); }
		public TerminalNode Interface() { return getToken(PHPParser.Interface, 0); }
		public TerminalNode IntType() { return getToken(PHPParser.IntType, 0); }
		public TerminalNode IsSet() { return getToken(PHPParser.IsSet, 0); }
		public TerminalNode List() { return getToken(PHPParser.List, 0); }
		public TerminalNode LogicalAnd() { return getToken(PHPParser.LogicalAnd, 0); }
		public TerminalNode LogicalOr() { return getToken(PHPParser.LogicalOr, 0); }
		public TerminalNode LogicalXor() { return getToken(PHPParser.LogicalXor, 0); }
		public TerminalNode Namespace() { return getToken(PHPParser.Namespace, 0); }
		public TerminalNode New() { return getToken(PHPParser.New, 0); }
		public TerminalNode Null() { return getToken(PHPParser.Null, 0); }
		public TerminalNode ObjectType() { return getToken(PHPParser.ObjectType, 0); }
		public TerminalNode Parent_() { return getToken(PHPParser.Parent_, 0); }
		public TerminalNode Partial() { return getToken(PHPParser.Partial, 0); }
		public TerminalNode Print() { return getToken(PHPParser.Print, 0); }
		public TerminalNode Private() { return getToken(PHPParser.Private, 0); }
		public TerminalNode Protected() { return getToken(PHPParser.Protected, 0); }
		public TerminalNode Public() { return getToken(PHPParser.Public, 0); }
		public TerminalNode Require() { return getToken(PHPParser.Require, 0); }
		public TerminalNode RequireOnce() { return getToken(PHPParser.RequireOnce, 0); }
		public TerminalNode Resource() { return getToken(PHPParser.Resource, 0); }
		public TerminalNode Return() { return getToken(PHPParser.Return, 0); }
		public TerminalNode Static() { return getToken(PHPParser.Static, 0); }
		public TerminalNode StringType() { return getToken(PHPParser.StringType, 0); }
		public TerminalNode Switch() { return getToken(PHPParser.Switch, 0); }
		public TerminalNode Throw() { return getToken(PHPParser.Throw, 0); }
		public TerminalNode Trait() { return getToken(PHPParser.Trait, 0); }
		public TerminalNode Try() { return getToken(PHPParser.Try, 0); }
		public TerminalNode Typeof() { return getToken(PHPParser.Typeof, 0); }
		public TerminalNode UintCast() { return getToken(PHPParser.UintCast, 0); }
		public TerminalNode UnicodeCast() { return getToken(PHPParser.UnicodeCast, 0); }
		public TerminalNode Unset() { return getToken(PHPParser.Unset, 0); }
		public TerminalNode Use() { return getToken(PHPParser.Use, 0); }
		public TerminalNode Var() { return getToken(PHPParser.Var, 0); }
		public TerminalNode While() { return getToken(PHPParser.While, 0); }
		public TerminalNode Yield() { return getToken(PHPParser.Yield, 0); }
		public TerminalNode Get() { return getToken(PHPParser.Get, 0); }
		public TerminalNode Set() { return getToken(PHPParser.Set, 0); }
		public TerminalNode Call() { return getToken(PHPParser.Call, 0); }
		public TerminalNode CallStatic() { return getToken(PHPParser.CallStatic, 0); }
		public TerminalNode Constructor() { return getToken(PHPParser.Constructor, 0); }
		public TerminalNode Destruct() { return getToken(PHPParser.Destruct, 0); }
		public TerminalNode Wakeup() { return getToken(PHPParser.Wakeup, 0); }
		public TerminalNode Sleep() { return getToken(PHPParser.Sleep, 0); }
		public TerminalNode Autoload() { return getToken(PHPParser.Autoload, 0); }
		public TerminalNode IsSet__() { return getToken(PHPParser.IsSet__, 0); }
		public TerminalNode Unset__() { return getToken(PHPParser.Unset__, 0); }
		public TerminalNode ToString__() { return getToken(PHPParser.ToString__, 0); }
		public TerminalNode Invoke() { return getToken(PHPParser.Invoke, 0); }
		public TerminalNode SetState() { return getToken(PHPParser.SetState, 0); }
		public TerminalNode Clone__() { return getToken(PHPParser.Clone__, 0); }
		public TerminalNode DebugInfo() { return getToken(PHPParser.DebugInfo, 0); }
		public TerminalNode Namespace__() { return getToken(PHPParser.Namespace__, 0); }
		public TerminalNode Class__() { return getToken(PHPParser.Class__, 0); }
		public TerminalNode Traic__() { return getToken(PHPParser.Traic__, 0); }
		public TerminalNode Function__() { return getToken(PHPParser.Function__, 0); }
		public TerminalNode Method__() { return getToken(PHPParser.Method__, 0); }
		public TerminalNode Line__() { return getToken(PHPParser.Line__, 0); }
		public TerminalNode File__() { return getToken(PHPParser.File__, 0); }
		public TerminalNode Dir__() { return getToken(PHPParser.Dir__, 0); }
		public IdentifierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_identifier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterIdentifier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitIdentifier(this);
		}
	}

	public final IdentifierContext identifier() throws RecognitionException {
		IdentifierContext _localctx = new IdentifierContext(_ctx, getState());
		enterRule(_localctx, 254, RULE_identifier);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1621);
			_la = _input.LA(1);
			if ( !(((((_la - 40)) & ~0x3f) == 0 && ((1L << (_la - 40)) & ((1L << (Abstract - 40)) | (1L << (Array - 40)) | (1L << (As - 40)) | (1L << (BinaryCast - 40)) | (1L << (BoolType - 40)) | (1L << (BooleanConstant - 40)) | (1L << (Break - 40)) | (1L << (Callable - 40)) | (1L << (Case - 40)) | (1L << (Catch - 40)) | (1L << (Class - 40)) | (1L << (Clone - 40)) | (1L << (Const - 40)) | (1L << (Continue - 40)) | (1L << (Declare - 40)) | (1L << (Default - 40)) | (1L << (Do - 40)) | (1L << (DoubleCast - 40)) | (1L << (DoubleType - 40)) | (1L << (Echo - 40)) | (1L << (Else - 40)) | (1L << (ElseIf - 40)) | (1L << (Empty - 40)) | (1L << (EndDeclare - 40)) | (1L << (EndFor - 40)) | (1L << (EndForeach - 40)) | (1L << (EndIf - 40)) | (1L << (EndSwitch - 40)) | (1L << (EndWhile - 40)) | (1L << (Eval - 40)) | (1L << (Exit - 40)) | (1L << (Extends - 40)) | (1L << (Final - 40)) | (1L << (Finally - 40)) | (1L << (FloatCast - 40)) | (1L << (For - 40)) | (1L << (Foreach - 40)) | (1L << (Function - 40)) | (1L << (Global - 40)) | (1L << (Goto - 40)) | (1L << (If - 40)) | (1L << (Implements - 40)) | (1L << (Import - 40)) | (1L << (Include - 40)) | (1L << (IncludeOnce - 40)) | (1L << (InstanceOf - 40)) | (1L << (InsteadOf - 40)) | (1L << (Int8Cast - 40)) | (1L << (Int16Cast - 40)) | (1L << (Int64Type - 40)) | (1L << (IntType - 40)) | (1L << (Interface - 40)) | (1L << (IsSet - 40)) | (1L << (List - 40)) | (1L << (LogicalAnd - 40)) | (1L << (LogicalOr - 40)) | (1L << (LogicalXor - 40)) | (1L << (Namespace - 40)) | (1L << (New - 40)) | (1L << (Null - 40)) | (1L << (ObjectType - 40)) | (1L << (Parent_ - 40)) | (1L << (Partial - 40)) | (1L << (Print - 40)))) != 0) || ((((_la - 104)) & ~0x3f) == 0 && ((1L << (_la - 104)) & ((1L << (Private - 104)) | (1L << (Protected - 104)) | (1L << (Public - 104)) | (1L << (Require - 104)) | (1L << (RequireOnce - 104)) | (1L << (Resource - 104)) | (1L << (Return - 104)) | (1L << (Static - 104)) | (1L << (StringType - 104)) | (1L << (Switch - 104)) | (1L << (Throw - 104)) | (1L << (Trait - 104)) | (1L << (Try - 104)) | (1L << (Typeof - 104)) | (1L << (UintCast - 104)) | (1L << (UnicodeCast - 104)) | (1L << (Unset - 104)) | (1L << (Use - 104)) | (1L << (Var - 104)) | (1L << (While - 104)) | (1L << (Yield - 104)) | (1L << (Get - 104)) | (1L << (Set - 104)) | (1L << (Call - 104)) | (1L << (CallStatic - 104)) | (1L << (Constructor - 104)) | (1L << (Destruct - 104)) | (1L << (Wakeup - 104)) | (1L << (Sleep - 104)) | (1L << (Autoload - 104)) | (1L << (IsSet__ - 104)) | (1L << (Unset__ - 104)) | (1L << (ToString__ - 104)) | (1L << (Invoke - 104)) | (1L << (SetState - 104)) | (1L << (Clone__ - 104)) | (1L << (DebugInfo - 104)) | (1L << (Namespace__ - 104)) | (1L << (Class__ - 104)) | (1L << (Traic__ - 104)) | (1L << (Function__ - 104)) | (1L << (Method__ - 104)) | (1L << (Line__ - 104)) | (1L << (File__ - 104)) | (1L << (Dir__ - 104)))) != 0) || _la==Label) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MemberModifierContext extends ParserRuleContext {
		public TerminalNode Public() { return getToken(PHPParser.Public, 0); }
		public TerminalNode Protected() { return getToken(PHPParser.Protected, 0); }
		public TerminalNode Private() { return getToken(PHPParser.Private, 0); }
		public TerminalNode Static() { return getToken(PHPParser.Static, 0); }
		public TerminalNode Abstract() { return getToken(PHPParser.Abstract, 0); }
		public TerminalNode Final() { return getToken(PHPParser.Final, 0); }
		public MemberModifierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_memberModifier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterMemberModifier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitMemberModifier(this);
		}
	}

	public final MemberModifierContext memberModifier() throws RecognitionException {
		MemberModifierContext _localctx = new MemberModifierContext(_ctx, getState());
		enterRule(_localctx, 256, RULE_memberModifier);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1623);
			_la = _input.LA(1);
			if ( !(_la==Abstract || ((((_la - 72)) & ~0x3f) == 0 && ((1L << (_la - 72)) & ((1L << (Final - 72)) | (1L << (Private - 72)) | (1L << (Protected - 72)) | (1L << (Public - 72)) | (1L << (Static - 72)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MagicConstantContext extends ParserRuleContext {
		public TerminalNode Namespace__() { return getToken(PHPParser.Namespace__, 0); }
		public TerminalNode Class__() { return getToken(PHPParser.Class__, 0); }
		public TerminalNode Traic__() { return getToken(PHPParser.Traic__, 0); }
		public TerminalNode Function__() { return getToken(PHPParser.Function__, 0); }
		public TerminalNode Method__() { return getToken(PHPParser.Method__, 0); }
		public TerminalNode Line__() { return getToken(PHPParser.Line__, 0); }
		public TerminalNode File__() { return getToken(PHPParser.File__, 0); }
		public TerminalNode Dir__() { return getToken(PHPParser.Dir__, 0); }
		public MagicConstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_magicConstant; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterMagicConstant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitMagicConstant(this);
		}
	}

	public final MagicConstantContext magicConstant() throws RecognitionException {
		MagicConstantContext _localctx = new MagicConstantContext(_ctx, getState());
		enterRule(_localctx, 258, RULE_magicConstant);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1625);
			_la = _input.LA(1);
			if ( !(((((_la - 141)) & ~0x3f) == 0 && ((1L << (_la - 141)) & ((1L << (Namespace__ - 141)) | (1L << (Class__ - 141)) | (1L << (Traic__ - 141)) | (1L << (Function__ - 141)) | (1L << (Method__ - 141)) | (1L << (Line__ - 141)) | (1L << (File__ - 141)) | (1L << (Dir__ - 141)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MagicMethodContext extends ParserRuleContext {
		public TerminalNode Get() { return getToken(PHPParser.Get, 0); }
		public TerminalNode Set() { return getToken(PHPParser.Set, 0); }
		public TerminalNode Call() { return getToken(PHPParser.Call, 0); }
		public TerminalNode CallStatic() { return getToken(PHPParser.CallStatic, 0); }
		public TerminalNode Constructor() { return getToken(PHPParser.Constructor, 0); }
		public TerminalNode Destruct() { return getToken(PHPParser.Destruct, 0); }
		public TerminalNode Wakeup() { return getToken(PHPParser.Wakeup, 0); }
		public TerminalNode Sleep() { return getToken(PHPParser.Sleep, 0); }
		public TerminalNode Autoload() { return getToken(PHPParser.Autoload, 0); }
		public TerminalNode IsSet__() { return getToken(PHPParser.IsSet__, 0); }
		public TerminalNode Unset__() { return getToken(PHPParser.Unset__, 0); }
		public TerminalNode ToString__() { return getToken(PHPParser.ToString__, 0); }
		public TerminalNode Invoke() { return getToken(PHPParser.Invoke, 0); }
		public TerminalNode SetState() { return getToken(PHPParser.SetState, 0); }
		public TerminalNode Clone__() { return getToken(PHPParser.Clone__, 0); }
		public TerminalNode DebugInfo() { return getToken(PHPParser.DebugInfo, 0); }
		public MagicMethodContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_magicMethod; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterMagicMethod(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitMagicMethod(this);
		}
	}

	public final MagicMethodContext magicMethod() throws RecognitionException {
		MagicMethodContext _localctx = new MagicMethodContext(_ctx, getState());
		enterRule(_localctx, 260, RULE_magicMethod);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1627);
			_la = _input.LA(1);
			if ( !(((((_la - 125)) & ~0x3f) == 0 && ((1L << (_la - 125)) & ((1L << (Get - 125)) | (1L << (Set - 125)) | (1L << (Call - 125)) | (1L << (CallStatic - 125)) | (1L << (Constructor - 125)) | (1L << (Destruct - 125)) | (1L << (Wakeup - 125)) | (1L << (Sleep - 125)) | (1L << (Autoload - 125)) | (1L << (IsSet__ - 125)) | (1L << (Unset__ - 125)) | (1L << (ToString__ - 125)) | (1L << (Invoke - 125)) | (1L << (SetState - 125)) | (1L << (Clone__ - 125)) | (1L << (DebugInfo - 125)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PrimitiveTypeContext extends ParserRuleContext {
		public TerminalNode BoolType() { return getToken(PHPParser.BoolType, 0); }
		public TerminalNode IntType() { return getToken(PHPParser.IntType, 0); }
		public TerminalNode Int64Type() { return getToken(PHPParser.Int64Type, 0); }
		public TerminalNode DoubleType() { return getToken(PHPParser.DoubleType, 0); }
		public TerminalNode StringType() { return getToken(PHPParser.StringType, 0); }
		public TerminalNode Resource() { return getToken(PHPParser.Resource, 0); }
		public TerminalNode ObjectType() { return getToken(PHPParser.ObjectType, 0); }
		public TerminalNode Array() { return getToken(PHPParser.Array, 0); }
		public PrimitiveTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_primitiveType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterPrimitiveType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitPrimitiveType(this);
		}
	}

	public final PrimitiveTypeContext primitiveType() throws RecognitionException {
		PrimitiveTypeContext _localctx = new PrimitiveTypeContext(_ctx, getState());
		enterRule(_localctx, 262, RULE_primitiveType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1629);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << Array) | (1L << BoolType) | (1L << DoubleType))) != 0) || ((((_la - 89)) & ~0x3f) == 0 && ((1L << (_la - 89)) & ((1L << (Int64Type - 89)) | (1L << (IntType - 89)) | (1L << (ObjectType - 89)) | (1L << (Resource - 89)) | (1L << (StringType - 89)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CastOperationContext extends ParserRuleContext {
		public TerminalNode BoolType() { return getToken(PHPParser.BoolType, 0); }
		public TerminalNode Int8Cast() { return getToken(PHPParser.Int8Cast, 0); }
		public TerminalNode Int16Cast() { return getToken(PHPParser.Int16Cast, 0); }
		public TerminalNode IntType() { return getToken(PHPParser.IntType, 0); }
		public TerminalNode Int64Type() { return getToken(PHPParser.Int64Type, 0); }
		public TerminalNode UintCast() { return getToken(PHPParser.UintCast, 0); }
		public TerminalNode DoubleCast() { return getToken(PHPParser.DoubleCast, 0); }
		public TerminalNode DoubleType() { return getToken(PHPParser.DoubleType, 0); }
		public TerminalNode FloatCast() { return getToken(PHPParser.FloatCast, 0); }
		public TerminalNode StringType() { return getToken(PHPParser.StringType, 0); }
		public TerminalNode BinaryCast() { return getToken(PHPParser.BinaryCast, 0); }
		public TerminalNode UnicodeCast() { return getToken(PHPParser.UnicodeCast, 0); }
		public TerminalNode Array() { return getToken(PHPParser.Array, 0); }
		public TerminalNode ObjectType() { return getToken(PHPParser.ObjectType, 0); }
		public TerminalNode Resource() { return getToken(PHPParser.Resource, 0); }
		public TerminalNode Unset() { return getToken(PHPParser.Unset, 0); }
		public CastOperationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_castOperation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).enterCastOperation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PHPParserListener ) ((PHPParserListener)listener).exitCastOperation(this);
		}
	}

	public final CastOperationContext castOperation() throws RecognitionException {
		CastOperationContext _localctx = new CastOperationContext(_ctx, getState());
		enterRule(_localctx, 264, RULE_castOperation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1631);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << Array) | (1L << BinaryCast) | (1L << BoolType) | (1L << DoubleCast) | (1L << DoubleType))) != 0) || ((((_la - 74)) & ~0x3f) == 0 && ((1L << (_la - 74)) & ((1L << (FloatCast - 74)) | (1L << (Int8Cast - 74)) | (1L << (Int16Cast - 74)) | (1L << (Int64Type - 74)) | (1L << (IntType - 74)) | (1L << (ObjectType - 74)) | (1L << (Resource - 74)) | (1L << (StringType - 74)) | (1L << (UintCast - 74)) | (1L << (UnicodeCast - 74)) | (1L << (Unset - 74)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 82:
			return expression_sempred((ExpressionContext)_localctx, predIndex);
		case 83:
			return andOrExpression_sempred((AndOrExpressionContext)_localctx, predIndex);
		case 84:
			return comparisonExpression_sempred((ComparisonExpressionContext)_localctx, predIndex);
		case 85:
			return additionExpression_sempred((AdditionExpressionContext)_localctx, predIndex);
		case 86:
			return multiplicationExpression_sempred((MultiplicationExpressionContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean expression_sempred(ExpressionContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 4);
		case 1:
			return precpred(_ctx, 3);
		case 2:
			return precpred(_ctx, 2);
		case 3:
			return precpred(_ctx, 1);
		}
		return true;
	}
	private boolean andOrExpression_sempred(AndOrExpressionContext _localctx, int predIndex) {
		switch (predIndex) {
		case 4:
			return precpred(_ctx, 5);
		case 5:
			return precpred(_ctx, 4);
		case 6:
			return precpred(_ctx, 3);
		case 7:
			return precpred(_ctx, 2);
		case 8:
			return precpred(_ctx, 1);
		}
		return true;
	}
	private boolean comparisonExpression_sempred(ComparisonExpressionContext _localctx, int predIndex) {
		switch (predIndex) {
		case 9:
			return precpred(_ctx, 3);
		case 10:
			return precpred(_ctx, 2);
		case 11:
			return precpred(_ctx, 1);
		}
		return true;
	}
	private boolean additionExpression_sempred(AdditionExpressionContext _localctx, int predIndex) {
		switch (predIndex) {
		case 12:
			return precpred(_ctx, 1);
		}
		return true;
	}
	private boolean multiplicationExpression_sempred(MultiplicationExpressionContext _localctx, int predIndex) {
		switch (predIndex) {
		case 13:
			return precpred(_ctx, 2);
		case 14:
			return precpred(_ctx, 1);
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\u00e5\u0664\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t="+
		"\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\4I"+
		"\tI\4J\tJ\4K\tK\4L\tL\4M\tM\4N\tN\4O\tO\4P\tP\4Q\tQ\4R\tR\4S\tS\4T\tT"+
		"\4U\tU\4V\tV\4W\tW\4X\tX\4Y\tY\4Z\tZ\4[\t[\4\\\t\\\4]\t]\4^\t^\4_\t_\4"+
		"`\t`\4a\ta\4b\tb\4c\tc\4d\td\4e\te\4f\tf\4g\tg\4h\th\4i\ti\4j\tj\4k\t"+
		"k\4l\tl\4m\tm\4n\tn\4o\to\4p\tp\4q\tq\4r\tr\4s\ts\4t\tt\4u\tu\4v\tv\4"+
		"w\tw\4x\tx\4y\ty\4z\tz\4{\t{\4|\t|\4}\t}\4~\t~\4\177\t\177\4\u0080\t\u0080"+
		"\4\u0081\t\u0081\4\u0082\t\u0082\4\u0083\t\u0083\4\u0084\t\u0084\4\u0085"+
		"\t\u0085\4\u0086\t\u0086\3\2\5\2\u010e\n\2\3\2\7\2\u0111\n\2\f\2\16\2"+
		"\u0114\13\2\3\2\3\2\3\3\3\3\3\3\5\3\u011b\n\3\3\4\3\4\3\5\6\5\u0120\n"+
		"\5\r\5\16\5\u0121\3\6\7\6\u0125\n\6\f\6\16\6\u0128\13\6\3\6\6\6\u012b"+
		"\n\6\r\6\16\6\u012c\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5"+
		"\b\u013b\n\b\3\t\3\t\5\t\u013f\n\t\3\t\3\t\3\t\3\n\5\n\u0145\n\n\3\n\3"+
		"\n\3\n\5\n\u014a\n\n\3\n\7\n\u014d\n\n\f\n\16\n\u0150\13\n\3\13\3\13\3"+
		"\13\5\13\u0155\n\13\3\f\3\f\5\f\u0159\n\f\3\f\3\f\7\f\u015d\n\f\f\f\16"+
		"\f\u0160\13\f\3\f\3\f\3\f\3\f\5\f\u0166\n\f\3\r\3\r\3\r\3\r\3\r\5\r\u016d"+
		"\n\r\3\16\3\16\3\16\5\16\u0172\n\16\3\16\3\16\5\16\u0176\n\16\3\16\3\16"+
		"\3\16\3\16\3\16\3\17\3\17\5\17\u017f\n\17\3\17\5\17\u0182\n\17\3\17\5"+
		"\17\u0185\n\17\3\17\3\17\3\17\5\17\u018a\n\17\3\17\3\17\5\17\u018e\n\17"+
		"\3\17\3\17\5\17\u0192\n\17\3\17\3\17\3\17\5\17\u0197\n\17\3\17\3\17\5"+
		"\17\u019b\n\17\5\17\u019d\n\17\3\17\3\17\7\17\u01a1\n\17\f\17\16\17\u01a4"+
		"\13\17\3\17\3\17\3\20\3\20\3\21\3\21\3\21\7\21\u01ad\n\21\f\21\16\21\u01b0"+
		"\13\21\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22"+
		"\3\22\5\22\u01c0\n\22\3\23\3\23\3\23\7\23\u01c5\n\23\f\23\16\23\u01c8"+
		"\13\23\3\24\3\24\3\24\7\24\u01cd\n\24\f\24\16\24\u01d0\13\24\3\25\3\25"+
		"\3\25\3\26\3\26\3\26\3\26\3\26\5\26\u01da\n\26\3\27\3\27\3\27\3\27\7\27"+
		"\u01e0\n\27\f\27\16\27\u01e3\13\27\3\27\3\27\3\30\7\30\u01e8\n\30\f\30"+
		"\16\30\u01eb\13\30\3\31\3\31\3\31\3\31\5\31\u01f1\n\31\3\31\3\31\3\31"+
		"\7\31\u01f6\n\31\f\31\16\31\u01f9\13\31\3\31\3\31\3\32\3\32\3\32\3\32"+
		"\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32"+
		"\5\32\u020f\n\32\3\33\3\33\3\33\7\33\u0214\n\33\f\33\16\33\u0217\13\33"+
		"\3\34\3\34\3\34\7\34\u021c\n\34\f\34\16\34\u021f\13\34\3\35\3\35\3\35"+
		"\3\35\3\36\7\36\u0226\n\36\f\36\16\36\u0229\13\36\3\37\3\37\3\37\5\37"+
		"\u022e\n\37\3 \3 \5 \u0232\n \3!\3!\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3"+
		"\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\""+
		"\5\"\u0250\n\"\3#\3#\3#\3#\3$\3$\3$\3$\7$\u025a\n$\f$\16$\u025d\13$\3"+
		"$\5$\u0260\n$\3$\3$\3$\3$\3$\7$\u0267\n$\f$\16$\u026a\13$\3$\5$\u026d"+
		"\n$\3$\3$\3$\5$\u0272\n$\3%\3%\3%\3%\3&\3&\3&\3&\3&\3\'\3\'\3\'\3(\3("+
		"\3(\3(\3)\3)\3)\3)\3)\3)\3)\3)\5)\u028c\n)\3*\3*\3*\3*\3*\3*\3+\3+\3+"+
		"\5+\u0297\n+\3+\3+\5+\u029b\n+\3+\3+\5+\u029f\n+\3+\3+\3+\3+\3+\3+\3+"+
		"\5+\u02a8\n+\3,\3,\3-\3-\3.\3.\3.\3.\5.\u02b2\n.\3.\7.\u02b5\n.\f.\16"+
		".\u02b8\13.\3.\3.\3.\5.\u02bd\n.\3.\7.\u02c0\n.\f.\16.\u02c3\13.\3.\3"+
		".\5.\u02c7\n.\3/\3/\3/\5/\u02cc\n/\3/\6/\u02cf\n/\r/\16/\u02d0\3/\3/\3"+
		"\60\3\60\5\60\u02d7\n\60\3\60\3\60\3\61\3\61\5\61\u02dd\n\61\3\61\3\61"+
		"\3\62\3\62\5\62\u02e3\n\62\3\62\3\62\3\63\3\63\3\63\3\64\3\64\3\64\3\64"+
		"\3\64\3\64\3\65\3\65\3\65\3\65\3\65\5\65\u02f5\n\65\3\65\3\65\3\65\5\65"+
		"\u02fa\n\65\3\65\5\65\u02fd\n\65\3\65\3\65\3\65\3\65\3\65\3\65\3\65\3"+
		"\65\5\65\u0307\n\65\3\65\5\65\u030a\n\65\3\65\3\65\3\65\3\65\3\65\3\65"+
		"\3\65\3\65\3\65\3\65\3\65\5\65\u0317\n\65\3\65\3\65\3\65\3\65\3\65\3\65"+
		"\5\65\u031f\n\65\3\66\3\66\3\66\6\66\u0324\n\66\r\66\16\66\u0325\3\66"+
		"\5\66\u0329\n\66\3\66\7\66\u032c\n\66\f\66\16\66\u032f\13\66\3\66\5\66"+
		"\u0332\n\66\3\67\3\67\3\67\3\67\3\67\3\67\3\67\38\38\38\39\39\39\39\3"+
		":\3:\3:\3:\3;\3;\3;\3;\3;\3;\3;\3;\3;\3;\5;\u0350\n;\3<\3<\6<\u0354\n"+
		"<\r<\16<\u0355\3=\3=\3=\7=\u035b\n=\f=\16=\u035e\13=\3>\5>\u0361\n>\3"+
		">\3>\7>\u0365\n>\f>\16>\u0368\13>\3?\3?\5?\u036c\n?\3?\5?\u036f\n?\3?"+
		"\5?\u0372\n?\3?\3?\3@\3@\3@\5@\u0379\n@\3A\3A\3A\3A\7A\u037f\nA\fA\16"+
		"A\u0382\13A\3A\3A\3B\3B\3B\3B\3B\3B\3B\3B\5B\u038e\nB\3C\3C\3C\3C\3D\3"+
		"D\3D\3D\7D\u0398\nD\fD\16D\u039b\13D\3D\3D\3E\3E\3E\3E\3E\7E\u03a4\nE"+
		"\fE\16E\u03a7\13E\3E\3E\3E\3E\3E\3E\3E\7E\u03b0\nE\fE\16E\u03b3\13E\3"+
		"E\3E\3E\3E\5E\u03b9\nE\3E\3E\5E\u03bd\nE\3E\3E\5E\u03c1\nE\3E\3E\3E\3"+
		"E\5E\u03c7\nE\3E\3E\3E\3E\3E\3E\5E\u03cf\nE\3F\3F\3F\7F\u03d4\nF\fF\16"+
		"F\u03d7\13F\3F\5F\u03da\nF\3G\3G\5G\u03de\nG\3H\3H\3H\3H\3H\3H\3H\3I\3"+
		"I\3I\3I\5I\u03eb\nI\3I\5I\u03ee\nI\3I\3I\3J\3J\3J\5J\u03f5\nJ\3J\3J\3"+
		"K\3K\3K\3K\3L\3L\5L\u03ff\nL\3M\3M\5M\u0403\nM\3N\6N\u0406\nN\rN\16N\u0407"+
		"\3O\3O\3O\5O\u040d\nO\3P\3P\3P\3P\3Q\3Q\3Q\3Q\3Q\7Q\u0418\nQ\fQ\16Q\u041b"+
		"\13Q\3Q\3Q\3R\3R\3R\7R\u0422\nR\fR\16R\u0425\13R\3S\3S\3S\5S\u042a\nS"+
		"\3S\3S\3T\3T\3T\3T\3T\3T\5T\u0434\nT\3T\3T\3T\3T\3T\3T\3T\3T\3T\3T\3T"+
		"\7T\u0441\nT\fT\16T\u0444\13T\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3"+
		"U\3U\3U\3U\3U\7U\u0458\nU\fU\16U\u045b\13U\3V\3V\3V\3V\3V\3V\3V\3V\3V"+
		"\3V\3V\3V\7V\u0469\nV\fV\16V\u046c\13V\3W\3W\3W\3W\3W\3W\7W\u0474\nW\f"+
		"W\16W\u0477\13W\3X\3X\3X\3X\3X\3X\5X\u047f\nX\3X\3X\3X\3X\3X\3X\7X\u0487"+
		"\nX\fX\16X\u048a\13X\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3"+
		"Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\5Y\u04ab\nY\3Y\3Y\3Y\3Y\3"+
		"Y\3Y\3Y\3Y\3Y\3Y\3Y\5Y\u04b8\nY\3Y\3Y\3Y\5Y\u04bd\nY\3Y\5Y\u04c0\nY\3"+
		"Y\3Y\3Y\3Y\5Y\u04c6\nY\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3"+
		"Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\3Y\5Y\u04e3\nY\3Y\3Y\3Y\3Y\3Y\5Y\u04ea"+
		"\nY\3Y\3Y\5Y\u04ee\nY\3Y\3Y\3Y\3Y\5Y\u04f4\nY\3Y\3Y\5Y\u04f8\nY\3Z\3Z"+
		"\3Z\5Z\u04fd\nZ\3[\3[\3\\\3\\\3\\\3\\\5\\\u0505\n\\\3]\3]\3]\7]\u050a"+
		"\n]\f]\16]\u050d\13]\3]\5]\u0510\n]\3^\3^\3^\5^\u0515\n^\3^\3^\3^\5^\u051a"+
		"\n^\3^\3^\5^\u051e\n^\3_\3_\3_\3_\3_\7_\u0525\n_\f_\16_\u0528\13_\3_\3"+
		"_\3`\5`\u052d\n`\3`\3`\3a\3a\5a\u0533\na\3a\5a\u0536\na\3b\3b\5b\u053a"+
		"\nb\3b\5b\u053d\nb\3b\3b\5b\u0541\nb\3c\3c\3c\7c\u0546\nc\fc\16c\u0549"+
		"\13c\3d\5d\u054c\nd\3d\5d\u054f\nd\3d\3d\3e\3e\3e\7e\u0556\ne\fe\16e\u0559"+
		"\13e\3f\3f\3f\7f\u055e\nf\ff\16f\u0561\13f\3g\3g\3g\3g\7g\u0567\ng\fg"+
		"\16g\u056a\13g\3g\5g\u056d\ng\3g\3g\3h\5h\u0572\nh\3h\3h\3h\5h\u0577\n"+
		"h\3i\3i\3i\3i\3i\3i\5i\u057f\ni\5i\u0581\ni\3i\3i\3i\3i\5i\u0587\ni\5"+
		"i\u0589\ni\3i\3i\3i\5i\u058e\ni\3j\3j\3j\7j\u0593\nj\fj\16j\u0596\13j"+
		"\3k\3k\3k\5k\u059b\nk\3l\3l\3l\3l\3l\5l\u05a2\nl\3m\3m\3m\3m\5m\u05a8"+
		"\nm\3n\3n\3o\3o\3o\3o\3o\3o\5o\u05b2\no\3o\3o\5o\u05b6\no\3o\3o\3o\5o"+
		"\u05bb\no\3p\3p\3q\3q\6q\u05c1\nq\rq\16q\u05c2\3q\3q\6q\u05c7\nq\rq\16"+
		"q\u05c8\3q\3q\3q\7q\u05ce\nq\fq\16q\u05d1\13q\3q\5q\u05d4\nq\3r\3r\5r"+
		"\u05d8\nr\3s\3s\3s\7s\u05dd\ns\fs\16s\u05e0\13s\3t\3t\3t\3t\3t\3t\5t\u05e8"+
		"\nt\3t\7t\u05eb\nt\ft\16t\u05ee\13t\3u\3u\3u\5u\u05f3\nu\3v\3v\3v\3w\3"+
		"w\3w\5w\u05fb\nw\3x\5x\u05fe\nx\3x\3x\7x\u0602\nx\fx\16x\u0605\13x\3y"+
		"\3y\3y\5y\u060a\ny\3y\3y\3y\3y\5y\u0610\ny\3z\3z\5z\u0614\nz\3{\3{\3{"+
		"\3{\3{\5{\u061b\n{\3{\7{\u061e\n{\f{\16{\u0621\13{\3|\7|\u0624\n|\f|\16"+
		"|\u0627\13|\3|\3|\3|\3|\3|\3|\5|\u062f\n|\3|\7|\u0632\n|\f|\16|\u0635"+
		"\13|\3}\3}\5}\u0639\n}\3}\3}\3}\3}\3}\5}\u0640\n}\3~\5~\u0643\n~\3~\3"+
		"~\5~\u0647\n~\7~\u0649\n~\f~\16~\u064c\13~\3\177\3\177\3\177\3\177\3\177"+
		"\3\177\5\177\u0654\n\177\3\u0080\3\u0080\3\u0081\3\u0081\3\u0082\3\u0082"+
		"\3\u0083\3\u0083\3\u0084\3\u0084\3\u0085\3\u0085\3\u0086\3\u0086\3\u0086"+
		"\2\7\u00a6\u00a8\u00aa\u00ac\u00ae\u0087\2\4\6\b\n\f\16\20\22\24\26\30"+
		"\32\34\36 \"$&(*,.\60\62\64\668:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~\u0080"+
		"\u0082\u0084\u0086\u0088\u008a\u008c\u008e\u0090\u0092\u0094\u0096\u0098"+
		"\u009a\u009c\u009e\u00a0\u00a2\u00a4\u00a6\u00a8\u00aa\u00ac\u00ae\u00b0"+
		"\u00b2\u00b4\u00b6\u00b8\u00ba\u00bc\u00be\u00c0\u00c2\u00c4\u00c6\u00c8"+
		"\u00ca\u00cc\u00ce\u00d0\u00d2\u00d4\u00d6\u00d8\u00da\u00dc\u00de\u00e0"+
		"\u00e2\u00e4\u00e6\u00e8\u00ea\u00ec\u00ee\u00f0\u00f2\u00f4\u00f6\u00f8"+
		"\u00fa\u00fc\u00fe\u0100\u0102\u0104\u0106\u0108\u010a\2\33\13\2\4\4\6"+
		"\7\t\n\16\25\27\27\32\33\36\37\"\"$$\4\2\66\66OO\4\2\64\64uu\3\2\u00ce"+
		"\u00cf\3\2\u00b1\u00b2\4\2\u00a0\u00a1\u00b7\u00b8\3\2\u009c\u009f\4\2"+
		"\u00bd\u00be\u00c5\u00c5\3\2\u00bf\u00c1\3\2\u00c2\u00c3\4\2\u00bb\u00bb"+
		"\u00bd\u00be\3\2\u009a\u009b\3\2UV\3\2mn\5\2\u00a2\u00a4\u00a6\u00ae\u00d0"+
		"\u00d0\3\2\u00bd\u00be\4\2\u00d5\u00d6\u00d8\u00d9\4\2\64\64gg\4\2**J"+
		"J\4\2*\u0096\u00d4\u00d4\6\2**JJjlqq\3\2\u008f\u0096\3\2\177\u008e\t\2"+
		"++..<<[\\ffoorr\13\2++-.;<LLY\\ffoorrxz\2\u06f2\2\u010d\3\2\2\2\4\u011a"+
		"\3\2\2\2\6\u011c\3\2\2\2\b\u011f\3\2\2\2\n\u0126\3\2\2\2\f\u012e\3\2\2"+
		"\2\16\u013a\3\2\2\2\20\u013c\3\2\2\2\22\u0144\3\2\2\2\24\u0151\3\2\2\2"+
		"\26\u0156\3\2\2\2\30\u016c\3\2\2\2\32\u016e\3\2\2\2\34\u017c\3\2\2\2\36"+
		"\u01a7\3\2\2\2 \u01a9\3\2\2\2\"\u01bf\3\2\2\2$\u01c1\3\2\2\2&\u01c9\3"+
		"\2\2\2(\u01d1\3\2\2\2*\u01d4\3\2\2\2,\u01db\3\2\2\2.\u01e9\3\2\2\2\60"+
		"\u01ec\3\2\2\2\62\u020e\3\2\2\2\64\u0210\3\2\2\2\66\u0218\3\2\2\28\u0220"+
		"\3\2\2\2:\u0227\3\2\2\2<\u022d\3\2\2\2>\u0231\3\2\2\2@\u0233\3\2\2\2B"+
		"\u024f\3\2\2\2D\u0251\3\2\2\2F\u0271\3\2\2\2H\u0273\3\2\2\2J\u0277\3\2"+
		"\2\2L\u027c\3\2\2\2N\u027f\3\2\2\2P\u0283\3\2\2\2R\u028d\3\2\2\2T\u0293"+
		"\3\2\2\2V\u02a9\3\2\2\2X\u02ab\3\2\2\2Z\u02ad\3\2\2\2\\\u02ce\3\2\2\2"+
		"^\u02d4\3\2\2\2`\u02da\3\2\2\2b\u02e0\3\2\2\2d\u02e6\3\2\2\2f\u02e9\3"+
		"\2\2\2h\u02ef\3\2\2\2j\u0320\3\2\2\2l\u0333\3\2\2\2n\u033a\3\2\2\2p\u033d"+
		"\3\2\2\2r\u0341\3\2\2\2t\u0345\3\2\2\2v\u0353\3\2\2\2x\u0357\3\2\2\2z"+
		"\u0360\3\2\2\2|\u0369\3\2\2\2~\u0378\3\2\2\2\u0080\u037a\3\2\2\2\u0082"+
		"\u038d\3\2\2\2\u0084\u038f\3\2\2\2\u0086\u0393\3\2\2\2\u0088\u03ce\3\2"+
		"\2\2\u008a\u03d9\3\2\2\2\u008c\u03dd\3\2\2\2\u008e\u03df\3\2\2\2\u0090"+
		"\u03e6\3\2\2\2\u0092\u03f4\3\2\2\2\u0094\u03f8\3\2\2\2\u0096\u03fe\3\2"+
		"\2\2\u0098\u0402\3\2\2\2\u009a\u0405\3\2\2\2\u009c\u0409\3\2\2\2\u009e"+
		"\u040e\3\2\2\2\u00a0\u0412\3\2\2\2\u00a2\u041e\3\2\2\2\u00a4\u0426\3\2"+
		"\2\2\u00a6\u042d\3\2\2\2\u00a8\u0445\3\2\2\2\u00aa\u045c\3\2\2\2\u00ac"+
		"\u046d\3\2\2\2\u00ae\u047e\3\2\2\2\u00b0\u04f7\3\2\2\2\u00b2\u04f9\3\2"+
		"\2\2\u00b4\u04fe\3\2\2\2\u00b6\u0500\3\2\2\2\u00b8\u0506\3\2\2\2\u00ba"+
		"\u051d\3\2\2\2\u00bc\u051f\3\2\2\2\u00be\u052c\3\2\2\2\u00c0\u0535\3\2"+
		"\2\2\u00c2\u0540\3\2\2\2\u00c4\u0542\3\2\2\2\u00c6\u054b\3\2\2\2\u00c8"+
		"\u0552\3\2\2\2\u00ca\u055a\3\2\2\2\u00cc\u0562\3\2\2\2\u00ce\u0576\3\2"+
		"\2\2\u00d0\u058d\3\2\2\2\u00d2\u058f\3\2\2\2\u00d4\u0597\3\2\2\2\u00d6"+
		"\u05a1\3\2\2\2\u00d8\u05a7\3\2\2\2\u00da\u05a9\3\2\2\2\u00dc\u05ba\3\2"+
		"\2\2\u00de\u05bc\3\2\2\2\u00e0\u05d3\3\2\2\2\u00e2\u05d7\3\2\2\2\u00e4"+
		"\u05d9\3\2\2\2\u00e6\u05e7\3\2\2\2\u00e8\u05ef\3\2\2\2\u00ea\u05f4\3\2"+
		"\2\2\u00ec\u05fa\3\2\2\2\u00ee\u05fd\3\2\2\2\u00f0\u060f\3\2\2\2\u00f2"+
		"\u0613\3\2\2\2\u00f4\u061a\3\2\2\2\u00f6\u0625\3\2\2\2\u00f8\u063f\3\2"+
		"\2\2\u00fa\u0642\3\2\2\2\u00fc\u0653\3\2\2\2\u00fe\u0655\3\2\2\2\u0100"+
		"\u0657\3\2\2\2\u0102\u0659\3\2\2\2\u0104\u065b\3\2\2\2\u0106\u065d\3\2"+
		"\2\2\u0108\u065f\3\2\2\2\u010a\u0661\3\2\2\2\u010c\u010e\7\13\2\2\u010d"+
		"\u010c\3\2\2\2\u010d\u010e\3\2\2\2\u010e\u0112\3\2\2\2\u010f\u0111\5\4"+
		"\3\2\u0110\u010f\3\2\2\2\u0111\u0114\3\2\2\2\u0112\u0110\3\2\2\2\u0112"+
		"\u0113\3\2\2\2\u0113\u0115\3\2\2\2\u0114\u0112\3\2\2\2\u0115\u0116\7\2"+
		"\2\3\u0116\3\3\2\2\2\u0117\u011b\5\6\4\2\u0118\u011b\5\n\6\2\u0119\u011b"+
		"\5\b\5\2\u011a\u0117\3\2\2\2\u011a\u0118\3\2\2\2\u011a\u0119\3\2\2\2\u011b"+
		"\5\3\2\2\2\u011c\u011d\t\2\2\2\u011d\7\3\2\2\2\u011e\u0120\7!\2\2\u011f"+
		"\u011e\3\2\2\2\u0120\u0121\3\2\2\2\u0121\u011f\3\2\2\2\u0121\u0122\3\2"+
		"\2\2\u0122\t\3\2\2\2\u0123\u0125\5\f\7\2\u0124\u0123\3\2\2\2\u0125\u0128"+
		"\3\2\2\2\u0126\u0124\3\2\2\2\u0126\u0127\3\2\2\2\u0127\u012a\3\2\2\2\u0128"+
		"\u0126\3\2\2\2\u0129\u012b\5\16\b\2\u012a\u0129\3\2\2\2\u012b\u012c\3"+
		"\2\2\2\u012c\u012a\3\2\2\2\u012c\u012d\3\2\2\2\u012d\13\3\2\2\2\u012e"+
		"\u012f\7T\2\2\u012f\u0130\7c\2\2\u0130\u0131\5\u00c8e\2\u0131\u0132\7"+
		"\u00cf\2\2\u0132\r\3\2\2\2\u0133\u013b\5@!\2\u0134\u013b\5B\"\2\u0135"+
		"\u013b\5\20\t\2\u0136\u013b\5\26\f\2\u0137\u013b\5\32\16\2\u0138\u013b"+
		"\5\34\17\2\u0139\u013b\5\u00a0Q\2\u013a\u0133\3\2\2\2\u013a\u0134\3\2"+
		"\2\2\u013a\u0135\3\2\2\2\u013a\u0136\3\2\2\2\u013a\u0137\3\2\2\2\u013a"+
		"\u0138\3\2\2\2\u013a\u0139\3\2\2\2\u013b\17\3\2\2\2\u013c\u013e\7{\2\2"+
		"\u013d\u013f\t\3\2\2\u013e\u013d\3\2\2\2\u013e\u013f\3\2\2\2\u013f\u0140"+
		"\3\2\2\2\u0140\u0141\5\22\n\2\u0141\u0142\7\u00cf\2\2\u0142\21\3\2\2\2"+
		"\u0143\u0145\7\u00b5\2\2\u0144\u0143\3\2\2\2\u0144\u0145\3\2\2\2\u0145"+
		"\u0146\3\2\2\2\u0146\u014e\5\24\13\2\u0147\u0149\7\u00cd\2\2\u0148\u014a"+
		"\7\u00b5\2\2\u0149\u0148\3\2\2\2\u0149\u014a\3\2\2\2\u014a\u014b\3\2\2"+
		"\2\u014b\u014d\5\24\13\2\u014c\u0147\3\2\2\2\u014d\u0150\3\2\2\2\u014e"+
		"\u014c\3\2\2\2\u014e\u014f\3\2\2\2\u014f\23\3\2\2\2\u0150\u014e\3\2\2"+
		"\2\u0151\u0154\5\u00c8e\2\u0152\u0153\7,\2\2\u0153\u0155\5\u0100\u0081"+
		"\2\u0154\u0152\3\2\2\2\u0154\u0155\3\2\2\2\u0155\25\3\2\2\2\u0156\u0165"+
		"\7c\2\2\u0157\u0159\5\u00c8e\2\u0158\u0157\3\2\2\2\u0158\u0159\3\2\2\2"+
		"\u0159\u015a\3\2\2\2\u015a\u015e\7\u00cb\2\2\u015b\u015d\5\30\r\2\u015c"+
		"\u015b\3\2\2\2\u015d\u0160\3\2\2\2\u015e\u015c\3\2\2\2\u015e\u015f\3\2"+
		"\2\2\u015f\u0161\3\2\2\2\u0160\u015e\3\2\2\2\u0161\u0166\7\u00cc\2\2\u0162"+
		"\u0163\5\u00c8e\2\u0163\u0164\7\u00cf\2\2\u0164\u0166\3\2\2\2\u0165\u0158"+
		"\3\2\2\2\u0165\u0162\3\2\2\2\u0166\27\3\2\2\2\u0167\u016d\5B\"\2\u0168"+
		"\u016d\5\20\t\2\u0169\u016d\5\32\16\2\u016a\u016d\5\34\17\2\u016b\u016d"+
		"\5\u00a0Q\2\u016c\u0167\3\2\2\2\u016c\u0168\3\2\2\2\u016c\u0169\3\2\2"+
		"\2\u016c\u016a\3\2\2\2\u016c\u016b\3\2\2\2\u016d\31\3\2\2\2\u016e\u016f"+
		"\5.\30\2\u016f\u0171\7O\2\2\u0170\u0172\7\u00b9\2\2\u0171\u0170\3\2\2"+
		"\2\u0171\u0172\3\2\2\2\u0172\u0173\3\2\2\2\u0173\u0175\5\u0100\u0081\2"+
		"\u0174\u0176\5\"\22\2\u0175\u0174\3\2\2\2\u0175\u0176\3\2\2\2\u0176\u0177"+
		"\3\2\2\2\u0177\u0178\7\u00c7\2\2\u0178\u0179\5z>\2\u0179\u017a\7\u00c8"+
		"\2\2\u017a\u017b\5D#\2\u017b\33\3\2\2\2\u017c\u017e\5.\30\2\u017d\u017f"+
		"\7j\2\2\u017e\u017d\3\2\2\2\u017e\u017f\3\2\2\2\u017f\u0181\3\2\2\2\u0180"+
		"\u0182\5\u00fe\u0080\2\u0181\u0180\3\2\2\2\u0181\u0182\3\2\2\2\u0182\u0184"+
		"\3\2\2\2\u0183\u0185\7h\2\2\u0184\u0183\3\2\2\2\u0184\u0185\3\2\2\2\u0185"+
		"\u019c\3\2\2\2\u0186\u0187\5\36\20\2\u0187\u0189\5\u0100\u0081\2\u0188"+
		"\u018a\5\"\22\2\u0189\u0188\3\2\2\2\u0189\u018a\3\2\2\2\u018a\u018d\3"+
		"\2\2\2\u018b\u018c\7I\2\2\u018c\u018e\5\u00c0a\2\u018d\u018b\3\2\2\2\u018d"+
		"\u018e\3\2\2\2\u018e\u0191\3\2\2\2\u018f\u0190\7S\2\2\u0190\u0192\5 \21"+
		"\2\u0191\u018f\3\2\2\2\u0191\u0192\3\2\2\2\u0192\u019d\3\2\2\2\u0193\u0194"+
		"\7]\2\2\u0194\u0196\5\u0100\u0081\2\u0195\u0197\5\"\22\2\u0196\u0195\3"+
		"\2\2\2\u0196\u0197\3\2\2\2\u0197\u019a\3\2\2\2\u0198\u0199\7I\2\2\u0199"+
		"\u019b\5 \21\2\u019a\u0198\3\2\2\2\u019a\u019b\3\2\2\2\u019b\u019d\3\2"+
		"\2\2\u019c\u0186\3\2\2\2\u019c\u0193\3\2\2\2\u019d\u019e\3\2\2\2\u019e"+
		"\u01a2\7\u00cb\2\2\u019f\u01a1\5\u0088E\2\u01a0\u019f\3\2\2\2\u01a1\u01a4"+
		"\3\2\2\2\u01a2\u01a0\3\2\2\2\u01a2\u01a3\3\2\2\2\u01a3\u01a5\3\2\2\2\u01a4"+
		"\u01a2\3\2\2\2\u01a5\u01a6\7\u00cc\2\2\u01a6\35\3\2\2\2\u01a7\u01a8\t"+
		"\4\2\2\u01a8\37\3\2\2\2\u01a9\u01ae\5\u00c0a\2\u01aa\u01ab\7\u00cd\2\2"+
		"\u01ab\u01ad\5\u00c0a\2\u01ac\u01aa\3\2\2\2\u01ad\u01b0\3\2\2\2\u01ae"+
		"\u01ac\3\2\2\2\u01ae\u01af\3\2\2\2\u01af!\3\2\2\2\u01b0\u01ae\3\2\2\2"+
		"\u01b1\u01b2\7\u0097\2\2\u01b2\u01b3\5$\23\2\u01b3\u01b4\7\u0098\2\2\u01b4"+
		"\u01c0\3\2\2\2\u01b5\u01b6\7\u0097\2\2\u01b6\u01b7\5&\24\2\u01b7\u01b8"+
		"\7\u0098\2\2\u01b8\u01c0\3\2\2\2\u01b9\u01ba\7\u0097\2\2\u01ba\u01bb\5"+
		"$\23\2\u01bb\u01bc\7\u00cd\2\2\u01bc\u01bd\5&\24\2\u01bd\u01be\7\u0098"+
		"\2\2\u01be\u01c0\3\2\2\2\u01bf\u01b1\3\2\2\2\u01bf\u01b5\3\2\2\2\u01bf"+
		"\u01b9\3\2\2\2\u01c0#\3\2\2\2\u01c1\u01c6\5(\25\2\u01c2\u01c3\7\u00cd"+
		"\2\2\u01c3\u01c5\5(\25\2\u01c4\u01c2\3\2\2\2\u01c5\u01c8\3\2\2\2\u01c6"+
		"\u01c4\3\2\2\2\u01c6\u01c7\3\2\2\2\u01c7%\3\2\2\2\u01c8\u01c6\3\2\2\2"+
		"\u01c9\u01ce\5*\26\2\u01ca\u01cb\7\u00cd\2\2\u01cb\u01cd\5*\26\2\u01cc"+
		"\u01ca\3\2\2\2\u01cd\u01d0\3\2\2\2\u01ce\u01cc\3\2\2\2\u01ce\u01cf\3\2"+
		"\2\2\u01cf\'\3\2\2\2\u01d0\u01ce\3\2\2\2\u01d1\u01d2\5.\30\2\u01d2\u01d3"+
		"\5\u0100\u0081\2\u01d3)\3\2\2\2\u01d4\u01d5\5.\30\2\u01d5\u01d6\5\u0100"+
		"\u0081\2\u01d6\u01d9\7\u00d0\2\2\u01d7\u01da\5\u00c0a\2\u01d8\u01da\5"+
		"\u0108\u0085\2\u01d9\u01d7\3\2\2\2\u01d9\u01d8\3\2\2\2\u01da+\3\2\2\2"+
		"\u01db\u01dc\7\u0097\2\2\u01dc\u01e1\5\u00c2b\2\u01dd\u01de\7\u00cd\2"+
		"\2\u01de\u01e0\5\u00c2b\2\u01df\u01dd\3\2\2\2\u01e0\u01e3\3\2\2\2\u01e1"+
		"\u01df\3\2\2\2\u01e1\u01e2\3\2\2\2\u01e2\u01e4\3\2\2\2\u01e3\u01e1\3\2"+
		"\2\2\u01e4\u01e5\7\u0098\2\2\u01e5-\3\2\2\2\u01e6\u01e8\5\60\31\2\u01e7"+
		"\u01e6\3\2\2\2\u01e8\u01eb\3\2\2\2\u01e9\u01e7\3\2\2\2\u01e9\u01ea\3\2"+
		"\2\2\u01ea/\3\2\2\2\u01eb\u01e9\3\2\2\2\u01ec\u01f0\7\u00c9\2\2\u01ed"+
		"\u01ee\5\u0100\u0081\2\u01ee\u01ef\7\u00ce\2\2\u01ef\u01f1\3\2\2\2\u01f0"+
		"\u01ed\3\2\2\2\u01f0\u01f1\3\2\2\2\u01f1\u01f2\3\2\2\2\u01f2\u01f7\5\62"+
		"\32\2\u01f3\u01f4\7\u00cd\2\2\u01f4\u01f6\5\62\32\2\u01f5\u01f3\3\2\2"+
		"\2\u01f6\u01f9\3\2\2\2\u01f7\u01f5\3\2\2\2\u01f7\u01f8\3\2\2\2\u01f8\u01fa"+
		"\3\2\2\2\u01f9\u01f7\3\2\2\2\u01fa\u01fb\7\u00ca\2\2\u01fb\61\3\2\2\2"+
		"\u01fc\u020f\5\u00c6d\2\u01fd\u01fe\5\u00c6d\2\u01fe\u01ff\7\u00c7\2\2"+
		"\u01ff\u0200\5\64\33\2\u0200\u0201\7\u00c8\2\2\u0201\u020f\3\2\2\2\u0202"+
		"\u0203\5\u00c6d\2\u0203\u0204\7\u00c7\2\2\u0204\u0205\5\66\34\2\u0205"+
		"\u0206\7\u00c8\2\2\u0206\u020f\3\2\2\2\u0207\u0208\5\u00c6d\2\u0208\u0209"+
		"\7\u00c7\2\2\u0209\u020a\5\64\33\2\u020a\u020b\7\u00cd\2\2\u020b\u020c"+
		"\5\66\34\2\u020c\u020d\7\u00c8\2\2\u020d\u020f\3\2\2\2\u020e\u01fc\3\2"+
		"\2\2\u020e\u01fd\3\2\2\2\u020e\u0202\3\2\2\2\u020e\u0207\3\2\2\2\u020f"+
		"\63\3\2\2\2\u0210\u0215\5\u00a6T\2\u0211\u0212\7\u00cd\2\2\u0212\u0214"+
		"\5\u00a6T\2\u0213\u0211\3\2\2\2\u0214\u0217\3\2\2\2\u0215\u0213\3\2\2"+
		"\2\u0215\u0216\3\2\2\2\u0216\65\3\2\2\2\u0217\u0215\3\2\2\2\u0218\u021d"+
		"\58\35\2\u0219\u021a\7\u00cd\2\2\u021a\u021c\58\35\2\u021b\u0219\3\2\2"+
		"\2\u021c\u021f\3\2\2\2\u021d\u021b\3\2\2\2\u021d\u021e\3\2\2\2\u021e\67"+
		"\3\2\2\2\u021f\u021d\3\2\2\2\u0220\u0221\7\u00d3\2\2\u0221\u0222\7\u0099"+
		"\2\2\u0222\u0223\5\u00a6T\2\u02239\3\2\2\2\u0224\u0226\5<\37\2\u0225\u0224"+
		"\3\2\2\2\u0226\u0229\3\2\2\2\u0227\u0225\3\2\2\2\u0227\u0228\3\2\2\2\u0228"+
		";\3\2\2\2\u0229\u0227\3\2\2\2\u022a\u022e\5> \2\u022b\u022e\5\32\16\2"+
		"\u022c\u022e\5\34\17\2\u022d\u022a\3\2\2\2\u022d\u022b\3\2\2\2\u022d\u022c"+
		"\3\2\2\2\u022e=\3\2\2\2\u022f\u0232\5B\"\2\u0230\u0232\5@!\2\u0231\u022f"+
		"\3\2\2\2\u0231\u0230\3\2\2\2\u0232?\3\2\2\2\u0233\u0234\7\u00cf\2\2\u0234"+
		"A\3\2\2\2\u0235\u0236\5\u0100\u0081\2\u0236\u0237\7\u00ce\2\2\u0237\u0250"+
		"\3\2\2\2\u0238\u0250\5D#\2\u0239\u0250\5F$\2\u023a\u0250\5P)\2\u023b\u0250"+
		"\5R*\2\u023c\u0250\5T+\2\u023d\u0250\5Z.\2\u023e\u0250\5^\60\2\u023f\u0250"+
		"\5`\61\2\u0240\u0250\5b\62\2\u0241\u0242\5\u00b6\\\2\u0242\u0243\7\u00cf"+
		"\2\2\u0243\u0250\3\2\2\2\u0244\u0250\5\u0080A\2\u0245\u0250\5\u0086D\2"+
		"\u0246\u0250\5\u0084C\2\u0247\u0250\5d\63\2\u0248\u0250\5f\64\2\u0249"+
		"\u0250\5h\65\2\u024a\u0250\5j\66\2\u024b\u0250\5p9\2\u024c\u0250\5r:\2"+
		"\u024d\u0250\5t;\2\u024e\u0250\5v<\2\u024f\u0235\3\2\2\2\u024f\u0238\3"+
		"\2\2\2\u024f\u0239\3\2\2\2\u024f\u023a\3\2\2\2\u024f\u023b\3\2\2\2\u024f"+
		"\u023c\3\2\2\2\u024f\u023d\3\2\2\2\u024f\u023e\3\2\2\2\u024f\u023f\3\2"+
		"\2\2\u024f\u0240\3\2\2\2\u024f\u0241\3\2\2\2\u024f\u0244\3\2\2\2\u024f"+
		"\u0245\3\2\2\2\u024f\u0246\3\2\2\2\u024f\u0247\3\2\2\2\u024f\u0248\3\2"+
		"\2\2\u024f\u0249\3\2\2\2\u024f\u024a\3\2\2\2\u024f\u024b\3\2\2\2\u024f"+
		"\u024c\3\2\2\2\u024f\u024d\3\2\2\2\u024f\u024e\3\2\2\2\u0250C\3\2\2\2"+
		"\u0251\u0252\7\u00cb\2\2\u0252\u0253\5:\36\2\u0253\u0254\7\u00cc\2\2\u0254"+
		"E\3\2\2\2\u0255\u0256\7R\2\2\u0256\u0257\5\u00a4S\2\u0257\u025b\5> \2"+
		"\u0258\u025a\5H%\2\u0259\u0258\3\2\2\2\u025a\u025d\3\2\2\2\u025b\u0259"+
		"\3\2\2\2\u025b\u025c\3\2\2\2\u025c\u025f\3\2\2\2\u025d\u025b\3\2\2\2\u025e"+
		"\u0260\5L\'\2\u025f\u025e\3\2\2\2\u025f\u0260\3\2\2\2\u0260\u0272\3\2"+
		"\2\2\u0261\u0262\7R\2\2\u0262\u0263\5\u00a4S\2\u0263\u0264\7\u00ce\2\2"+
		"\u0264\u0268\5:\36\2\u0265\u0267\5J&\2\u0266\u0265\3\2\2\2\u0267\u026a"+
		"\3\2\2\2\u0268\u0266\3\2\2\2\u0268\u0269\3\2\2\2\u0269\u026c\3\2\2\2\u026a"+
		"\u0268\3\2\2\2\u026b\u026d\5N(\2\u026c\u026b\3\2\2\2\u026c\u026d\3\2\2"+
		"\2\u026d\u026e\3\2\2\2\u026e\u026f\7D\2\2\u026f\u0270\7\u00cf\2\2\u0270"+
		"\u0272\3\2\2\2\u0271\u0255\3\2\2\2\u0271\u0261\3\2\2\2\u0272G\3\2\2\2"+
		"\u0273\u0274\7?\2\2\u0274\u0275\5\u00a4S\2\u0275\u0276\5> \2\u0276I\3"+
		"\2\2\2\u0277\u0278\7?\2\2\u0278\u0279\5\u00a4S\2\u0279\u027a\7\u00ce\2"+
		"\2\u027a\u027b\5:\36\2\u027bK\3\2\2\2\u027c\u027d\7>\2\2\u027d\u027e\5"+
		"> \2\u027eM\3\2\2\2\u027f\u0280\7>\2\2\u0280\u0281\7\u00ce\2\2\u0281\u0282"+
		"\5:\36\2\u0282O\3\2\2\2\u0283\u0284\7}\2\2\u0284\u028b\5\u00a4S\2\u0285"+
		"\u028c\5> \2\u0286\u0287\7\u00ce\2\2\u0287\u0288\5:\36\2\u0288\u0289\7"+
		"F\2\2\u0289\u028a\7\u00cf\2\2\u028a\u028c\3\2\2\2\u028b\u0285\3\2\2\2"+
		"\u028b\u0286\3\2\2\2\u028cQ\3\2\2\2\u028d\u028e\7:\2\2\u028e\u028f\5>"+
		" \2\u028f\u0290\7}\2\2\u0290\u0291\5\u00a4S\2\u0291\u0292\7\u00cf\2\2"+
		"\u0292S\3\2\2\2\u0293\u0294\7M\2\2\u0294\u0296\7\u00c7\2\2\u0295\u0297"+
		"\5V,\2\u0296\u0295\3\2\2\2\u0296\u0297\3\2\2\2\u0297\u0298\3\2\2\2\u0298"+
		"\u029a\7\u00cf\2\2\u0299\u029b\5\u00a2R\2\u029a\u0299\3\2\2\2\u029a\u029b"+
		"\3\2\2\2\u029b\u029c\3\2\2\2\u029c\u029e\7\u00cf\2\2\u029d\u029f\5X-\2"+
		"\u029e\u029d\3\2\2\2\u029e\u029f\3\2\2\2\u029f\u02a0\3\2\2\2\u02a0\u02a7"+
		"\7\u00c8\2\2\u02a1\u02a8\5> \2\u02a2\u02a3\7\u00ce\2\2\u02a3\u02a4\5:"+
		"\36\2\u02a4\u02a5\7B\2\2\u02a5\u02a6\7\u00cf\2\2\u02a6\u02a8\3\2\2\2\u02a7"+
		"\u02a1\3\2\2\2\u02a7\u02a2\3\2\2\2\u02a8U\3\2\2\2\u02a9\u02aa\5\u00a2"+
		"R\2\u02aaW\3\2\2\2\u02ab\u02ac\5\u00a2R\2\u02acY\3\2\2\2\u02ad\u02ae\7"+
		"s\2\2\u02ae\u02c6\5\u00a4S\2\u02af\u02b1\7\u00cb\2\2\u02b0\u02b2\7\u00cf"+
		"\2\2\u02b1\u02b0\3\2\2\2\u02b1\u02b2\3\2\2\2\u02b2\u02b6\3\2\2\2\u02b3"+
		"\u02b5\5\\/\2\u02b4\u02b3\3\2\2\2\u02b5\u02b8\3\2\2\2\u02b6\u02b4\3\2"+
		"\2\2\u02b6\u02b7\3\2\2\2\u02b7\u02b9\3\2\2\2\u02b8\u02b6\3\2\2\2\u02b9"+
		"\u02c7\7\u00cc\2\2\u02ba\u02bc\7\u00ce\2\2\u02bb\u02bd\7\u00cf\2\2\u02bc"+
		"\u02bb\3\2\2\2\u02bc\u02bd\3\2\2\2\u02bd\u02c1\3\2\2\2\u02be\u02c0\5\\"+
		"/\2\u02bf\u02be\3\2\2\2\u02c0\u02c3\3\2\2\2\u02c1\u02bf\3\2\2\2\u02c1"+
		"\u02c2\3\2\2\2\u02c2\u02c4\3\2\2\2\u02c3\u02c1\3\2\2\2\u02c4\u02c5\7E"+
		"\2\2\u02c5\u02c7\7\u00cf\2\2\u02c6\u02af\3\2\2\2\u02c6\u02ba\3\2\2\2\u02c7"+
		"[\3\2\2\2\u02c8\u02c9\7\62\2\2\u02c9\u02cc\5\u00a6T\2\u02ca\u02cc\79\2"+
		"\2\u02cb\u02c8\3\2\2\2\u02cb\u02ca\3\2\2\2\u02cc\u02cd\3\2\2\2\u02cd\u02cf"+
		"\t\5\2\2\u02ce\u02cb\3\2\2\2\u02cf\u02d0\3\2\2\2\u02d0\u02ce\3\2\2\2\u02d0"+
		"\u02d1\3\2\2\2\u02d1\u02d2\3\2\2\2\u02d2\u02d3\5:\36\2\u02d3]\3\2\2\2"+
		"\u02d4\u02d6\7\60\2\2\u02d5\u02d7\5\u00a6T\2\u02d6\u02d5\3\2\2\2\u02d6"+
		"\u02d7\3\2\2\2\u02d7\u02d8\3\2\2\2\u02d8\u02d9\7\u00cf\2\2\u02d9_\3\2"+
		"\2\2\u02da\u02dc\7\67\2\2\u02db\u02dd\5\u00a6T\2\u02dc\u02db\3\2\2\2\u02dc"+
		"\u02dd\3\2\2\2\u02dd\u02de\3\2\2\2\u02de\u02df\7\u00cf\2\2\u02dfa\3\2"+
		"\2\2\u02e0\u02e2\7p\2\2\u02e1\u02e3\5\u00a6T\2\u02e2\u02e1\3\2\2\2\u02e2"+
		"\u02e3\3\2\2\2\u02e3\u02e4\3\2\2\2\u02e4\u02e5\7\u00cf\2\2\u02e5c\3\2"+
		"\2\2\u02e6\u02e7\5\u00a6T\2\u02e7\u02e8\7\u00cf\2\2\u02e8e\3\2\2\2\u02e9"+
		"\u02ea\7z\2\2\u02ea\u02eb\7\u00c7\2\2\u02eb\u02ec\5\u00e4s\2\u02ec\u02ed"+
		"\7\u00c8\2\2\u02ed\u02ee\7\u00cf\2\2\u02eeg\3\2\2\2\u02ef\u0316\7N\2\2"+
		"\u02f0\u02f1\7\u00c7\2\2\u02f1\u02f2\5\u00e6t\2\u02f2\u02f4\7,\2\2\u02f3"+
		"\u02f5\7\u00b9\2\2\u02f4\u02f3\3\2\2\2\u02f4\u02f5\3\2\2\2\u02f5\u02f6"+
		"\3\2\2\2\u02f6\u02fc\5\u00e6t\2\u02f7\u02f9\7\u0099\2\2\u02f8\u02fa\7"+
		"\u00b9\2\2\u02f9\u02f8\3\2\2\2\u02f9\u02fa\3\2\2\2\u02fa\u02fb\3\2\2\2"+
		"\u02fb\u02fd\5\u00e6t\2\u02fc\u02f7\3\2\2\2\u02fc\u02fd\3\2\2\2\u02fd"+
		"\u02fe\3\2\2\2\u02fe\u02ff\7\u00c8\2\2\u02ff\u0317\3\2\2\2\u0300\u0301"+
		"\7\u00c7\2\2\u0301\u0302\5\u00a6T\2\u0302\u0303\7,\2\2\u0303\u0309\5\u00e6"+
		"t\2\u0304\u0306\7\u0099\2\2\u0305\u0307\7\u00b9\2\2\u0306\u0305\3\2\2"+
		"\2\u0306\u0307\3\2\2\2\u0307\u0308\3\2\2\2\u0308\u030a\5\u00e6t\2\u0309"+
		"\u0304\3\2\2\2\u0309\u030a\3\2\2\2\u030a\u030b\3\2\2\2\u030b\u030c\7\u00c8"+
		"\2\2\u030c\u0317\3\2\2\2\u030d\u030e\7\u00c7\2\2\u030e\u030f\5\u00e6t"+
		"\2\u030f\u0310\7,\2\2\u0310\u0311\7_\2\2\u0311\u0312\7\u00c7\2\2\u0312"+
		"\u0313\5\u00fa~\2\u0313\u0314\7\u00c8\2\2\u0314\u0315\7\u00c8\2\2\u0315"+
		"\u0317\3\2\2\2\u0316\u02f0\3\2\2\2\u0316\u0300\3\2\2\2\u0316\u030d\3\2"+
		"\2\2\u0317\u031e\3\2\2\2\u0318\u031f\5> \2\u0319\u031a\7\u00ce\2\2\u031a"+
		"\u031b\5:\36\2\u031b\u031c\7C\2\2\u031c\u031d\7\u00cf\2\2\u031d\u031f"+
		"\3\2\2\2\u031e\u0318\3\2\2\2\u031e\u0319\3\2\2\2\u031fi\3\2\2\2\u0320"+
		"\u0321\7v\2\2\u0321\u0331\5D#\2\u0322\u0324\5l\67\2\u0323\u0322\3\2\2"+
		"\2\u0324\u0325\3\2\2\2\u0325\u0323\3\2\2\2\u0325\u0326\3\2\2\2\u0326\u0328"+
		"\3\2\2\2\u0327\u0329\5n8\2\u0328\u0327\3\2\2\2\u0328\u0329\3\2\2\2\u0329"+
		"\u0332\3\2\2\2\u032a\u032c\5l\67\2\u032b\u032a\3\2\2\2\u032c\u032f\3\2"+
		"\2\2\u032d\u032b\3\2\2\2\u032d\u032e\3\2\2\2\u032e\u0330\3\2\2\2\u032f"+
		"\u032d\3\2\2\2\u0330\u0332\5n8\2\u0331\u0323\3\2\2\2\u0331\u032d\3\2\2"+
		"\2\u0332k\3\2\2\2\u0333\u0334\7\63\2\2\u0334\u0335\7\u00c7\2\2\u0335\u0336"+
		"\5\u00c0a\2\u0336\u0337\7\u00d3\2\2\u0337\u0338\7\u00c8\2\2\u0338\u0339"+
		"\5D#\2\u0339m\3\2\2\2\u033a\u033b\7K\2\2\u033b\u033c\5D#\2\u033co\3\2"+
		"\2\2\u033d\u033e\7t\2\2\u033e\u033f\5\u00a6T\2\u033f\u0340\7\u00cf\2\2"+
		"\u0340q\3\2\2\2\u0341\u0342\7Q\2\2\u0342\u0343\5\u0100\u0081\2\u0343\u0344"+
		"\7\u00cf\2\2\u0344s\3\2\2\2\u0345\u0346\78\2\2\u0346\u0347\7\u00c7\2\2"+
		"\u0347\u0348\5x=\2\u0348\u034f\7\u00c8\2\2\u0349\u0350\5> \2\u034a\u034b"+
		"\7\u00ce\2\2\u034b\u034c\5:\36\2\u034c\u034d\7A\2\2\u034d\u034e\7\u00cf"+
		"\2\2\u034e\u0350\3\2\2\2\u034f\u0349\3\2\2\2\u034f\u034a\3\2\2\2\u0350"+
		"u\3\2\2\2\u0351\u0354\5\6\4\2\u0352\u0354\5\b\5\2\u0353\u0351\3\2\2\2"+
		"\u0353\u0352\3\2\2\2\u0354\u0355\3\2\2\2\u0355\u0353\3\2\2\2\u0355\u0356"+
		"\3\2\2\2\u0356w\3\2\2\2\u0357\u035c\5\u009eP\2\u0358\u0359\7\u00cd\2\2"+
		"\u0359\u035b\5\u009eP\2\u035a\u0358\3\2\2\2\u035b\u035e\3\2\2\2\u035c"+
		"\u035a\3\2\2\2\u035c\u035d\3\2\2\2\u035dy\3\2\2\2\u035e\u035c\3\2\2\2"+
		"\u035f\u0361\5|?\2\u0360\u035f\3\2\2\2\u0360\u0361\3\2\2\2\u0361\u0366"+
		"\3\2\2\2\u0362\u0363\7\u00cd\2\2\u0363\u0365\5|?\2\u0364\u0362\3\2\2\2"+
		"\u0365\u0368\3\2\2\2\u0366\u0364\3\2\2\2\u0366\u0367\3\2\2\2\u0367{\3"+
		"\2\2\2\u0368\u0366\3\2\2\2\u0369\u036b\5.\30\2\u036a\u036c\5~@\2\u036b"+
		"\u036a\3\2\2\2\u036b\u036c\3\2\2\2\u036c\u036e\3\2\2\2\u036d\u036f\7\u00b9"+
		"\2\2\u036e\u036d\3\2\2\2\u036e\u036f\3\2\2\2\u036f\u0371\3\2\2\2\u0370"+
		"\u0372\7\u00b6\2\2\u0371\u0370\3\2\2\2\u0371\u0372\3\2\2\2\u0372\u0373"+
		"\3\2\2\2\u0373\u0374\5\u009cO\2\u0374}\3\2\2\2\u0375\u0379\5\u00c0a\2"+
		"\u0376\u0379\7\61\2\2\u0377\u0379\5\u0108\u0085\2\u0378\u0375\3\2\2\2"+
		"\u0378\u0376\3\2\2\2\u0378\u0377\3\2\2\2\u0379\177\3\2\2\2\u037a\u037b"+
		"\7P\2\2\u037b\u0380\5\u0082B\2\u037c\u037d\7\u00cd\2\2\u037d\u037f\5\u0082"+
		"B\2\u037e\u037c\3\2\2\2\u037f\u0382\3\2\2\2\u0380\u037e\3\2\2\2\u0380"+
		"\u0381\3\2\2\2\u0381\u0383\3\2\2\2\u0382\u0380\3\2\2\2\u0383\u0384\7\u00cf"+
		"\2\2\u0384\u0081\3\2\2\2\u0385\u038e\7\u00d3\2\2\u0386\u0387\7\u00c4\2"+
		"\2\u0387\u038e\5\u00e6t\2\u0388\u0389\7\u00c4\2\2\u0389\u038a\7\u00cb"+
		"\2\2\u038a\u038b\5\u00a6T\2\u038b\u038c\7\u00cc\2\2\u038c\u038e\3\2\2"+
		"\2\u038d\u0385\3\2\2\2\u038d\u0386\3\2\2\2\u038d\u0388\3\2\2\2\u038e\u0083"+
		"\3\2\2\2\u038f\u0390\7=\2\2\u0390\u0391\5\u00a2R\2\u0391\u0392\7\u00cf"+
		"\2\2\u0392\u0085\3\2\2\2\u0393\u0394\7q\2\2\u0394\u0399\5\u009cO\2\u0395"+
		"\u0396\7\u00cd\2\2\u0396\u0398\5\u009cO\2\u0397\u0395\3\2\2\2\u0398\u039b"+
		"\3\2\2\2\u0399\u0397\3\2\2\2\u0399\u039a\3\2\2\2\u039a\u039c\3\2\2\2\u039b"+
		"\u0399\3\2\2\2\u039c\u039d\7\u00cf\2\2\u039d\u0087\3\2\2\2\u039e\u039f"+
		"\5.\30\2\u039f\u03a0\5\u0098M\2\u03a0\u03a5\5\u009cO\2\u03a1\u03a2\7\u00cd"+
		"\2\2\u03a2\u03a4\5\u009cO\2\u03a3\u03a1\3\2\2\2\u03a4\u03a7\3\2\2\2\u03a5"+
		"\u03a3\3\2\2\2\u03a5\u03a6\3\2\2\2\u03a6\u03a8\3\2\2\2\u03a7\u03a5\3\2"+
		"\2\2\u03a8\u03a9\7\u00cf\2\2\u03a9\u03cf\3\2\2\2\u03aa\u03ab\5.\30\2\u03ab"+
		"\u03ac\7\66\2\2\u03ac\u03b1\5\u009eP\2\u03ad\u03ae\7\u00cd\2\2\u03ae\u03b0"+
		"\5\u009eP\2\u03af\u03ad\3\2\2\2\u03b0\u03b3\3\2\2\2\u03b1\u03af\3\2\2"+
		"\2\u03b1\u03b2\3\2\2\2\u03b2\u03b4\3\2\2\2\u03b3\u03b1\3\2\2\2\u03b4\u03b5"+
		"\7\u00cf\2\2\u03b5\u03cf\3\2\2\2\u03b6\u03b8\5.\30\2\u03b7\u03b9\5\u009a"+
		"N\2\u03b8\u03b7\3\2\2\2\u03b8\u03b9\3\2\2\2\u03b9\u03ba\3\2\2\2\u03ba"+
		"\u03bc\7O\2\2\u03bb\u03bd\7\u00b9\2\2\u03bc\u03bb\3\2\2\2\u03bc\u03bd"+
		"\3\2\2\2\u03bd\u03be\3\2\2\2\u03be\u03c0\5\u0100\u0081\2\u03bf\u03c1\5"+
		"\"\22\2\u03c0\u03bf\3\2\2\2\u03c0\u03c1\3\2\2\2\u03c1\u03c2\3\2\2\2\u03c2"+
		"\u03c3\7\u00c7\2\2\u03c3\u03c4\5z>\2\u03c4\u03c6\7\u00c8\2\2\u03c5\u03c7"+
		"\5\u0094K\2\u03c6\u03c5\3\2\2\2\u03c6\u03c7\3\2\2\2\u03c7\u03c8\3\2\2"+
		"\2\u03c8\u03c9\5\u0096L\2\u03c9\u03cf\3\2\2\2\u03ca\u03cb\7{\2\2\u03cb"+
		"\u03cc\5\u00caf\2\u03cc\u03cd\5\u008aF\2\u03cd\u03cf\3\2\2\2\u03ce\u039e"+
		"\3\2\2\2\u03ce\u03aa\3\2\2\2\u03ce\u03b6\3\2\2\2\u03ce\u03ca\3\2\2\2\u03cf"+
		"\u0089\3\2\2\2\u03d0\u03da\7\u00cf\2\2\u03d1\u03d5\7\u00cb\2\2\u03d2\u03d4"+
		"\5\u008cG\2\u03d3\u03d2\3\2\2\2\u03d4\u03d7\3\2\2\2\u03d5\u03d3\3\2\2"+
		"\2\u03d5\u03d6\3\2\2\2\u03d6\u03d8\3\2\2\2\u03d7\u03d5\3\2\2\2\u03d8\u03da"+
		"\7\u00cc\2\2\u03d9\u03d0\3\2\2\2\u03d9\u03d1\3\2\2\2\u03da\u008b\3\2\2"+
		"\2\u03db\u03de\5\u008eH\2\u03dc\u03de\5\u0090I\2\u03dd\u03db\3\2\2\2\u03dd"+
		"\u03dc\3\2\2\2\u03de\u008d\3\2\2\2\u03df\u03e0\5\u00c6d\2\u03e0\u03e1"+
		"\7\u00b3\2\2\u03e1\u03e2\5\u0100\u0081\2\u03e2\u03e3\7X\2\2\u03e3\u03e4"+
		"\5\u00caf\2\u03e4\u03e5\7\u00cf\2\2\u03e5\u008f\3\2\2\2\u03e6\u03e7\5"+
		"\u0092J\2\u03e7\u03ed\7,\2\2\u03e8\u03ee\5\u0102\u0082\2\u03e9\u03eb\5"+
		"\u0102\u0082\2\u03ea\u03e9\3\2\2\2\u03ea\u03eb\3\2\2\2\u03eb\u03ec\3\2"+
		"\2\2\u03ec\u03ee\5\u0100\u0081\2\u03ed\u03e8\3\2\2\2\u03ed\u03ea\3\2\2"+
		"\2\u03ee\u03ef\3\2\2\2\u03ef\u03f0\7\u00cf\2\2\u03f0\u0091\3\2\2\2\u03f1"+
		"\u03f2\5\u00c6d\2\u03f2\u03f3\7\u00b3\2\2\u03f3\u03f5\3\2\2\2\u03f4\u03f1"+
		"\3\2\2\2\u03f4\u03f5\3\2\2\2\u03f5\u03f6\3\2\2\2\u03f6\u03f7\5\u0100\u0081"+
		"\2\u03f7\u0093\3\2\2\2\u03f8\u03f9\7\u00ce\2\2\u03f9\u03fa\5\u0100\u0081"+
		"\2\u03fa\u03fb\5\u00ccg\2\u03fb\u0095\3\2\2\2\u03fc\u03ff\7\u00cf\2\2"+
		"\u03fd\u03ff\5D#\2\u03fe\u03fc\3\2\2\2\u03fe\u03fd\3\2\2\2\u03ff\u0097"+
		"\3\2\2\2\u0400\u0403\5\u009aN\2\u0401\u0403\7|\2\2\u0402\u0400\3\2\2\2"+
		"\u0402\u0401\3\2\2\2\u0403\u0099\3\2\2\2\u0404\u0406\5\u0102\u0082\2\u0405"+
		"\u0404\3\2\2\2\u0406\u0407\3\2\2\2\u0407\u0405\3\2\2\2\u0407\u0408\3\2"+
		"\2\2\u0408\u009b\3\2\2\2\u0409\u040c\7\u00d3\2\2\u040a\u040b\7\u00d0\2"+
		"\2\u040b\u040d\5\u00d0i\2\u040c\u040a\3\2\2\2\u040c\u040d\3\2\2\2\u040d"+
		"\u009d\3\2\2\2\u040e\u040f\5\u0100\u0081\2\u040f\u0410\7\u00d0\2\2\u0410"+
		"\u0411\5\u00d0i\2\u0411\u009f\3\2\2\2\u0412\u0413\5.\30\2\u0413\u0414"+
		"\7\66\2\2\u0414\u0419\5\u009eP\2\u0415\u0416\7\u00cd\2\2\u0416\u0418\5"+
		"\u009eP\2\u0417\u0415\3\2\2\2\u0418\u041b\3\2\2\2\u0419\u0417\3\2\2\2"+
		"\u0419\u041a\3\2\2\2\u041a\u041c\3\2\2\2\u041b\u0419\3\2\2\2\u041c\u041d"+
		"\7\u00cf\2\2\u041d\u00a1\3\2\2\2\u041e\u0423\5\u00a6T\2\u041f\u0420\7"+
		"\u00cd\2\2\u0420\u0422\5\u00a6T\2\u0421\u041f\3\2\2\2\u0422\u0425\3\2"+
		"\2\2\u0423\u0421\3\2\2\2\u0423\u0424\3\2\2\2\u0424\u00a3\3\2\2\2\u0425"+
		"\u0423\3\2\2\2\u0426\u0429\7\u00c7\2\2\u0427\u042a\5\u00a6T\2\u0428\u042a"+
		"\5\u00b6\\\2\u0429\u0427\3\2\2\2\u0429\u0428\3\2\2\2\u042a\u042b\3\2\2"+
		"\2\u042b\u042c\7\u00c8\2\2\u042c\u00a5\3\2\2\2\u042d\u042e\bT\1\2\u042e"+
		"\u042f\5\u00a8U\2\u042f\u0442\3\2\2\2\u0430\u0431\f\6\2\2\u0431\u0433"+
		"\7\u00c6\2\2\u0432\u0434\5\u00a6T\2\u0433\u0432\3\2\2\2\u0433\u0434\3"+
		"\2\2\2\u0434\u0435\3\2\2\2\u0435\u0436\7\u00ce\2\2\u0436\u0441\5\u00a8"+
		"U\2\u0437\u0438\f\5\2\2\u0438\u0439\7`\2\2\u0439\u0441\5\u00a8U\2\u043a"+
		"\u043b\f\4\2\2\u043b\u043c\7b\2\2\u043c\u0441\5\u00a8U\2\u043d\u043e\f"+
		"\3\2\2\u043e\u043f\7a\2\2\u043f\u0441\5\u00a8U\2\u0440\u0430\3\2\2\2\u0440"+
		"\u0437\3\2\2\2\u0440\u043a\3\2\2\2\u0440\u043d\3\2\2\2\u0441\u0444\3\2"+
		"\2\2\u0442\u0440\3\2\2\2\u0442\u0443\3\2\2\2\u0443\u00a7\3\2\2\2\u0444"+
		"\u0442\3\2\2\2\u0445\u0446\bU\1\2\u0446\u0447\5\u00aaV\2\u0447\u0459\3"+
		"\2\2\2\u0448\u0449\f\7\2\2\u0449\u044a\7\u00b9\2\2\u044a\u0458\5\u00aa"+
		"V\2\u044b\u044c\f\6\2\2\u044c\u044d\7\u00bc\2\2\u044d\u0458\5\u00aaV\2"+
		"\u044e\u044f\f\5\2\2\u044f\u0450\7\u00ba\2\2\u0450\u0458\5\u00aaV\2\u0451"+
		"\u0452\f\4\2\2\u0452\u0453\7\u00b0\2\2\u0453\u0458\5\u00aaV\2\u0454\u0455"+
		"\f\3\2\2\u0455\u0456\7\u00af\2\2\u0456\u0458\5\u00aaV\2\u0457\u0448\3"+
		"\2\2\2\u0457\u044b\3\2\2\2\u0457\u044e\3\2\2\2\u0457\u0451\3\2\2\2\u0457"+
		"\u0454\3\2\2\2\u0458\u045b\3\2\2\2\u0459\u0457\3\2\2\2\u0459\u045a\3\2"+
		"\2\2\u045a\u00a9\3\2\2\2\u045b\u0459\3\2\2\2\u045c\u045d\bV\1\2\u045d"+
		"\u045e\5\u00acW\2\u045e\u046a\3\2\2\2\u045f\u0460\f\5\2\2\u0460\u0461"+
		"\t\6\2\2\u0461\u0469\5\u00acW\2\u0462\u0463\f\4\2\2\u0463\u0464\t\7\2"+
		"\2\u0464\u0469\5\u00acW\2\u0465\u0466\f\3\2\2\u0466\u0467\t\b\2\2\u0467"+
		"\u0469\5\u00acW\2\u0468\u045f\3\2\2\2\u0468\u0462\3\2\2\2\u0468\u0465"+
		"\3\2\2\2\u0469\u046c\3\2\2\2\u046a\u0468\3\2\2\2\u046a\u046b\3\2\2\2\u046b"+
		"\u00ab\3\2\2\2\u046c\u046a\3\2\2\2\u046d\u046e\bW\1\2\u046e\u046f\5\u00ae"+
		"X\2\u046f\u0475\3\2\2\2\u0470\u0471\f\3\2\2\u0471\u0472\t\t\2\2\u0472"+
		"\u0474\5\u00aeX\2\u0473\u0470\3\2\2\2\u0474\u0477\3\2\2\2\u0475\u0473"+
		"\3\2\2\2\u0475\u0476\3\2\2\2\u0476\u00ad\3\2\2\2\u0477\u0475\3\2\2\2\u0478"+
		"\u0479\bX\1\2\u0479\u047f\5\u00b0Y\2\u047a\u047b\5\u00b0Y\2\u047b\u047c"+
		"\7\u00a5\2\2\u047c\u047d\5\u00aeX\5\u047d\u047f\3\2\2\2\u047e\u0478\3"+
		"\2\2\2\u047e\u047a\3\2\2\2\u047f\u0488\3\2\2\2\u0480\u0481\f\4\2\2\u0481"+
		"\u0482\7W\2\2\u0482\u0487\5\u00c2b\2\u0483\u0484\f\3\2\2\u0484\u0485\t"+
		"\n\2\2\u0485\u0487\5\u00b0Y\2\u0486\u0480\3\2\2\2\u0486\u0483\3\2\2\2"+
		"\u0487\u048a\3\2\2\2\u0488\u0486\3\2\2\2\u0488\u0489\3\2\2\2\u0489\u00af"+
		"\3\2\2\2\u048a\u0488\3\2\2\2\u048b\u048c\7\65\2\2\u048c\u04f8\5\u00a6"+
		"T\2\u048d\u04f8\5\u00b2Z\2\u048e\u048f\5\u00dep\2\u048f\u0490\7\u00c9"+
		"\2\2\u0490\u0491\5\u00a6T\2\u0491\u0492\7\u00ca\2\2\u0492\u04f8\3\2\2"+
		"\2\u0493\u0494\7\u00c7\2\2\u0494\u0495\5\u010a\u0086\2\u0495\u0496\7\u00c8"+
		"\2\2\u0496\u0497\5\u00a6T\2\u0497\u04f8\3\2\2\2\u0498\u0499\t\13\2\2\u0499"+
		"\u04f8\5\u00a6T\2\u049a\u049b\t\f\2\2\u049b\u04f8\5\u00a6T\2\u049c\u049d"+
		"\t\r\2\2\u049d\u04f8\5\u00e6t\2\u049e\u049f\5\u00e6t\2\u049f\u04a0\t\r"+
		"\2\2\u04a0\u04f8\3\2\2\2\u04a1\u04a2\5\u00e6t\2\u04a2\u04a3\5\u00b4[\2"+
		"\u04a3\u04a4\5\u00a6T\2\u04a4\u04f8\3\2\2\2\u04a5\u04a6\5\u00e6t\2\u04a6"+
		"\u04a7\7\u00d0\2\2\u04a7\u04aa\7\u00b9\2\2\u04a8\u04ab\5\u00e6t\2\u04a9"+
		"\u04ab\5\u00b2Z\2\u04aa\u04a8\3\2\2\2\u04aa\u04a9\3\2\2\2\u04ab\u04f8"+
		"\3\2\2\2\u04ac\u04ad\7i\2\2\u04ad\u04f8\5\u00a6T\2\u04ae\u04f8\5\u00e6"+
		"t\2\u04af\u04f8\5\u00d6l\2\u04b0\u04f8\5\u00e0q\2\u04b1\u04f8\7\u00d4"+
		"\2\2\u04b2\u04f8\7\u00da\2\2\u04b3\u04f8\5\u00a4S\2\u04b4\u04b5\7+\2\2"+
		"\u04b5\u04b7\7\u00c7\2\2\u04b6\u04b8\5\u00b8]\2\u04b7\u04b6\3\2\2\2\u04b7"+
		"\u04b8\3\2\2\2\u04b8\u04b9\3\2\2\2\u04b9\u04c0\7\u00c8\2\2\u04ba\u04bc"+
		"\7\u00c9\2\2\u04bb\u04bd\5\u00b8]\2\u04bc\u04bb\3\2\2\2\u04bc\u04bd\3"+
		"\2\2\2\u04bd\u04be\3\2\2\2\u04be\u04c0\7\u00ca\2\2\u04bf\u04b4\3\2\2\2"+
		"\u04bf\u04ba\3\2\2\2\u04c0\u04c5\3\2\2\2\u04c1\u04c2\7\u00c9\2\2\u04c2"+
		"\u04c3\5\u00a6T\2\u04c3\u04c4\7\u00ca\2\2\u04c4\u04c6\3\2\2\2\u04c5\u04c1"+
		"\3\2\2\2\u04c5\u04c6\3\2\2\2\u04c6\u04f8\3\2\2\2\u04c7\u04f8\7~\2\2\u04c8"+
		"\u04c9\7_\2\2\u04c9\u04ca\7\u00c7\2\2\u04ca\u04cb\5\u00fa~\2\u04cb\u04cc"+
		"\7\u00c8\2\2\u04cc\u04cd\7\u00d0\2\2\u04cd\u04ce\5\u00a6T\2\u04ce\u04f8"+
		"\3\2\2\2\u04cf\u04d0\7^\2\2\u04d0\u04d1\7\u00c7\2\2\u04d1\u04d2\5\u00e4"+
		"s\2\u04d2\u04d3\7\u00c8\2\2\u04d3\u04f8\3\2\2\2\u04d4\u04d5\7@\2\2\u04d5"+
		"\u04d6\7\u00c7\2\2\u04d6\u04d7\5\u00e6t\2\u04d7\u04d8\7\u00c8\2\2\u04d8"+
		"\u04f8\3\2\2\2\u04d9\u04da\7G\2\2\u04da\u04db\7\u00c7\2\2\u04db\u04dc"+
		"\5\u00a6T\2\u04dc\u04dd\7\u00c8\2\2\u04dd\u04f8\3\2\2\2\u04de\u04e2\7"+
		"H\2\2\u04df\u04e0\7\u00c7\2\2\u04e0\u04e3\7\u00c8\2\2\u04e1\u04e3\5\u00a4"+
		"S\2\u04e2\u04df\3\2\2\2\u04e2\u04e1\3\2\2\2\u04e2\u04e3\3\2\2\2\u04e3"+
		"\u04f8\3\2\2\2\u04e4\u04e5\t\16\2\2\u04e5\u04f8\5\u00a6T\2\u04e6\u04e7"+
		"\t\17\2\2\u04e7\u04f8\5\u00a6T\2\u04e8\u04ea\7q\2\2\u04e9\u04e8\3\2\2"+
		"\2\u04e9\u04ea\3\2\2\2\u04ea\u04eb\3\2\2\2\u04eb\u04ed\7O\2\2\u04ec\u04ee"+
		"\7\u00b9\2\2\u04ed\u04ec\3\2\2\2\u04ed\u04ee\3\2\2\2\u04ee\u04ef\3\2\2"+
		"\2\u04ef\u04f0\7\u00c7\2\2\u04f0\u04f1\5z>\2\u04f1\u04f3\7\u00c8\2\2\u04f2"+
		"\u04f4\5\u00bc_\2\u04f3\u04f2\3\2\2\2\u04f3\u04f4\3\2\2\2\u04f4\u04f5"+
		"\3\2\2\2\u04f5\u04f6\5D#\2\u04f6\u04f8\3\2\2\2\u04f7\u048b\3\2\2\2\u04f7"+
		"\u048d\3\2\2\2\u04f7\u048e\3\2\2\2\u04f7\u0493\3\2\2\2\u04f7\u0498\3\2"+
		"\2\2\u04f7\u049a\3\2\2\2\u04f7\u049c\3\2\2\2\u04f7\u049e\3\2\2\2\u04f7"+
		"\u04a1\3\2\2\2\u04f7\u04a5\3\2\2\2\u04f7\u04ac\3\2\2\2\u04f7\u04ae\3\2"+
		"\2\2\u04f7\u04af\3\2\2\2\u04f7\u04b0\3\2\2\2\u04f7\u04b1\3\2\2\2\u04f7"+
		"\u04b2\3\2\2\2\u04f7\u04b3\3\2\2\2\u04f7\u04bf\3\2\2\2\u04f7\u04c7\3\2"+
		"\2\2\u04f7\u04c8\3\2\2\2\u04f7\u04cf\3\2\2\2\u04f7\u04d4\3\2\2\2\u04f7"+
		"\u04d9\3\2\2\2\u04f7\u04de\3\2\2\2\u04f7\u04e4\3\2\2\2\u04f7\u04e6\3\2"+
		"\2\2\u04f7\u04e9\3\2\2\2\u04f8\u00b1\3\2\2\2\u04f9\u04fa\7d\2\2\u04fa"+
		"\u04fc\5\u00c2b\2\u04fb\u04fd\5\u00ccg\2\u04fc\u04fb\3\2\2\2\u04fc\u04fd"+
		"\3\2\2\2\u04fd\u00b3\3\2\2\2\u04fe\u04ff\t\20\2\2\u04ff\u00b5\3\2\2\2"+
		"\u0500\u0501\7~\2\2\u0501\u0504\5\u00a6T\2\u0502\u0503\7\u0099\2\2\u0503"+
		"\u0505\5\u00a6T\2\u0504\u0502\3\2\2\2\u0504\u0505\3\2\2\2\u0505\u00b7"+
		"\3\2\2\2\u0506\u050b\5\u00ba^\2\u0507\u0508\7\u00cd\2\2\u0508\u050a\5"+
		"\u00ba^\2\u0509\u0507\3\2\2\2\u050a\u050d\3\2\2\2\u050b\u0509\3\2\2\2"+
		"\u050b\u050c\3\2\2\2\u050c\u050f\3\2\2\2\u050d\u050b\3\2\2\2\u050e\u0510"+
		"\7\u00cd\2\2\u050f\u050e\3\2\2\2\u050f\u0510\3\2\2\2\u0510\u00b9\3\2\2"+
		"\2\u0511\u0514\5\u00a6T\2\u0512\u0513\7\u0099\2\2\u0513\u0515\5\u00a6"+
		"T\2\u0514\u0512\3\2\2\2\u0514\u0515\3\2\2\2\u0515\u051e\3\2\2\2\u0516"+
		"\u0517\5\u00a6T\2\u0517\u0518\7\u0099\2\2\u0518\u051a\3\2\2\2\u0519\u0516"+
		"\3\2\2\2\u0519\u051a\3\2\2\2\u051a\u051b\3\2\2\2\u051b\u051c\7\u00b9\2"+
		"\2\u051c\u051e\5\u00e6t\2\u051d\u0511\3\2\2\2\u051d\u0519\3\2\2\2\u051e"+
		"\u00bb\3\2\2\2\u051f\u0520\7{\2\2\u0520\u0521\7\u00c7\2\2\u0521\u0526"+
		"\5\u00be`\2\u0522\u0523\7\u00cd\2\2\u0523\u0525\5\u00be`\2\u0524\u0522"+
		"\3\2\2\2\u0525\u0528\3\2\2\2\u0526\u0524\3\2\2\2\u0526\u0527\3\2\2\2\u0527"+
		"\u0529\3\2\2\2\u0528\u0526\3\2\2\2\u0529\u052a\7\u00c8\2\2\u052a\u00bd"+
		"\3\2\2\2\u052b\u052d\7\u00b9\2\2\u052c\u052b\3\2\2\2\u052c\u052d\3\2\2"+
		"\2\u052d\u052e\3\2\2\2\u052e\u052f\7\u00d3\2\2\u052f\u00bf\3\2\2\2\u0530"+
		"\u0532\5\u00c6d\2\u0531\u0533\5,\27\2\u0532\u0531\3\2\2\2\u0532\u0533"+
		"\3\2\2\2\u0533\u0536\3\2\2\2\u0534\u0536\7q\2\2\u0535\u0530\3\2\2\2\u0535"+
		"\u0534\3\2\2\2\u0536\u00c1\3\2\2\2\u0537\u053a\5\u00c6d\2\u0538\u053a"+
		"\5\u00c4c\2\u0539\u0537\3\2\2\2\u0539\u0538\3\2\2\2\u053a\u053c\3\2\2"+
		"\2\u053b\u053d\5,\27\2\u053c\u053b\3\2\2\2\u053c\u053d\3\2\2\2\u053d\u0541"+
		"\3\2\2\2\u053e\u0541\5\u0108\u0085\2\u053f\u0541\7q\2\2\u0540\u0539\3"+
		"\2\2\2\u0540\u053e\3\2\2\2\u0540\u053f\3\2\2\2\u0541\u00c3\3\2\2\2\u0542"+
		"\u0547\5\u00f0y\2\u0543\u0544\7\u00b4\2\2\u0544\u0546\5\u00f2z\2\u0545"+
		"\u0543\3\2\2\2\u0546\u0549\3\2\2\2\u0547\u0545\3\2\2\2\u0547\u0548\3\2"+
		"\2\2\u0548\u00c5\3\2\2\2\u0549\u0547\3\2\2\2\u054a\u054c\7c\2\2\u054b"+
		"\u054a\3\2\2\2\u054b\u054c\3\2\2\2\u054c\u054e\3\2\2\2\u054d\u054f\7\u00b5"+
		"\2\2\u054e\u054d\3\2\2\2\u054e\u054f\3\2\2\2\u054f\u0550\3\2\2\2\u0550"+
		"\u0551\5\u00c8e\2\u0551\u00c7\3\2\2\2\u0552\u0557\5\u0100\u0081\2\u0553"+
		"\u0554\7\u00b5\2\2\u0554\u0556\5\u0100\u0081\2\u0555\u0553\3\2\2\2\u0556"+
		"\u0559\3\2\2\2\u0557\u0555\3\2\2\2\u0557\u0558\3\2\2\2\u0558\u00c9\3\2"+
		"\2\2\u0559\u0557\3\2\2\2\u055a\u055f\5\u00c6d\2\u055b\u055c\7\u00cd\2"+
		"\2\u055c\u055e\5\u00c6d\2\u055d\u055b\3\2\2\2\u055e\u0561\3\2\2\2\u055f"+
		"\u055d\3\2\2\2\u055f\u0560\3\2\2\2\u0560\u00cb\3\2\2\2\u0561\u055f\3\2"+
		"\2\2\u0562\u056c\7\u00c7\2\2\u0563\u0568\5\u00ceh\2\u0564\u0565\7\u00cd"+
		"\2\2\u0565\u0567\5\u00ceh\2\u0566\u0564\3\2\2\2\u0567\u056a\3\2\2\2\u0568"+
		"\u0566\3\2\2\2\u0568\u0569\3\2\2\2\u0569\u056d\3\2\2\2\u056a\u0568\3\2"+
		"\2\2\u056b\u056d\5\u00b6\\\2\u056c\u0563\3\2\2\2\u056c\u056b\3\2\2\2\u056c"+
		"\u056d\3\2\2\2\u056d\u056e\3\2\2\2\u056e\u056f\7\u00c8\2\2\u056f\u00cd"+
		"\3\2\2\2\u0570\u0572\7\u00b6\2\2\u0571\u0570\3\2\2\2\u0571\u0572\3\2\2"+
		"\2\u0572\u0573\3\2\2\2\u0573\u0577\5\u00a6T\2\u0574\u0575\7\u00b9\2\2"+
		"\u0575\u0577\5\u00e6t\2\u0576\u0571\3\2\2\2\u0576\u0574\3\2\2\2\u0577"+
		"\u00cf\3\2\2\2\u0578\u058e\5\u00d6l\2\u0579\u058e\5\u00e0q\2\u057a\u057b"+
		"\7+\2\2\u057b\u0580\7\u00c7\2\2\u057c\u057e\5\u00d2j\2\u057d\u057f\7\u00cd"+
		"\2\2\u057e\u057d\3\2\2\2\u057e\u057f\3\2\2\2\u057f\u0581\3\2\2\2\u0580"+
		"\u057c\3\2\2\2\u0580\u0581\3\2\2\2\u0581\u0582\3\2\2\2\u0582\u058e\7\u00c8"+
		"\2\2\u0583\u0588\7\u00c9\2\2\u0584\u0586\5\u00d2j\2\u0585\u0587\7\u00cd"+
		"\2\2\u0586\u0585\3\2\2\2\u0586\u0587\3\2\2\2\u0587\u0589\3\2\2\2\u0588"+
		"\u0584\3\2\2\2\u0588\u0589\3\2\2\2\u0589\u058a\3\2\2\2\u058a\u058e\7\u00ca"+
		"\2\2\u058b\u058c\t\21\2\2\u058c\u058e\5\u00d0i\2\u058d\u0578\3\2\2\2\u058d"+
		"\u0579\3\2\2\2\u058d\u057a\3\2\2\2\u058d\u0583\3\2\2\2\u058d\u058b\3\2"+
		"\2\2\u058e\u00d1\3\2\2\2\u058f\u0594\5\u00d4k\2\u0590\u0591\7\u00cd\2"+
		"\2\u0591\u0593\5\u00d4k\2\u0592\u0590\3\2\2\2\u0593\u0596\3\2\2\2\u0594"+
		"\u0592\3\2\2\2\u0594\u0595\3\2\2\2\u0595\u00d3\3\2\2\2\u0596\u0594\3\2"+
		"\2\2\u0597\u059a\5\u00d0i\2\u0598\u0599\7\u0099\2\2\u0599\u059b\5\u00d0"+
		"i\2\u059a\u0598\3\2\2\2\u059a\u059b\3\2\2\2\u059b\u00d5\3\2\2\2\u059c"+
		"\u05a2\7e\2\2\u059d\u05a2\5\u00d8m\2\u059e\u05a2\5\u0104\u0083\2\u059f"+
		"\u05a2\5\u00dco\2\u05a0\u05a2\5\u00c6d\2\u05a1\u059c\3\2\2\2\u05a1\u059d"+
		"\3\2\2\2\u05a1\u059e\3\2\2\2\u05a1\u059f\3\2\2\2\u05a1\u05a0\3\2\2\2\u05a2"+
		"\u00d7\3\2\2\2\u05a3\u05a8\7\u00d7\2\2\u05a4\u05a8\7/\2\2\u05a5\u05a8"+
		"\5\u00dan\2\u05a6\u05a8\5\u00dep\2\u05a7\u05a3\3\2\2\2\u05a7\u05a4\3\2"+
		"\2\2\u05a7\u05a5\3\2\2\2\u05a7\u05a6\3\2\2\2\u05a8\u00d9\3\2\2\2\u05a9"+
		"\u05aa\t\22\2\2\u05aa\u00db\3\2\2\2\u05ab\u05ac\t\23\2\2\u05ac\u05b1\7"+
		"\u00b3\2\2\u05ad\u05b2\5\u0100\u0081\2\u05ae\u05b2\7\u0083\2\2\u05af\u05b2"+
		"\7\177\2\2\u05b0\u05b2\7\u0080\2\2\u05b1\u05ad\3\2\2\2\u05b1\u05ae\3\2"+
		"\2\2\u05b1\u05af\3\2\2\2\u05b1\u05b0\3\2\2\2\u05b2\u05bb\3\2\2\2\u05b3"+
		"\u05b6\5\u00c0a\2\u05b4\u05b6\5\u00f6|\2\u05b5\u05b3\3\2\2\2\u05b5\u05b4"+
		"\3\2\2\2\u05b6\u05b7\3\2\2\2\u05b7\u05b8\7\u00b3\2\2\u05b8\u05b9\5\u0100"+
		"\u0081\2\u05b9\u05bb\3\2\2\2\u05ba\u05ab\3\2\2\2\u05ba\u05b5\3\2\2\2\u05bb"+
		"\u00dd\3\2\2\2\u05bc\u05bd\7\u00d4\2\2\u05bd\u00df\3\2\2\2\u05be\u05c0"+
		"\7\u00de\2\2\u05bf\u05c1\7\u00e5\2\2\u05c0\u05bf\3\2\2\2\u05c1\u05c2\3"+
		"\2\2\2\u05c2\u05c0\3\2\2\2\u05c2\u05c3\3\2\2\2\u05c3\u05d4\3\2\2\2\u05c4"+
		"\u05c6\7\u00dd\2\2\u05c5\u05c7\7\u00e5\2\2\u05c6\u05c5\3\2\2\2\u05c7\u05c8"+
		"\3\2\2\2\u05c8\u05c6\3\2\2\2\u05c8\u05c9\3\2\2\2\u05c9\u05d4\3\2\2\2\u05ca"+
		"\u05d4\7\u00db\2\2\u05cb\u05cf\7\u00dc\2\2\u05cc\u05ce\5\u00e2r\2\u05cd"+
		"\u05cc\3\2\2\2\u05ce\u05d1\3\2\2\2\u05cf\u05cd\3\2\2\2\u05cf\u05d0\3\2"+
		"\2\2\u05d0\u05d2\3\2\2\2\u05d1\u05cf\3\2\2\2\u05d2\u05d4\7\u00dc\2\2\u05d3"+
		"\u05be\3\2\2\2\u05d3\u05c4\3\2\2\2\u05d3\u05ca\3\2\2\2\u05d3\u05cb\3\2"+
		"\2\2\u05d4\u00e1\3\2\2\2\u05d5\u05d8\7\u00e1\2\2\u05d6\u05d8\5\u00e6t"+
		"\2\u05d7\u05d5\3\2\2\2\u05d7\u05d6\3\2\2\2\u05d8\u00e3\3\2\2\2\u05d9\u05de"+
		"\5\u00e6t\2\u05da\u05db\7\u00cd\2\2\u05db\u05dd\5\u00e6t\2\u05dc\u05da"+
		"\3\2\2\2\u05dd\u05e0\3\2\2\2\u05de\u05dc\3\2\2\2\u05de\u05df\3\2\2\2\u05df"+
		"\u00e5\3\2\2\2\u05e0\u05de\3\2\2\2\u05e1\u05e8\5\u00f0y\2\u05e2\u05e8"+
		"\5\u00eav\2\u05e3\u05e4\7\u00c7\2\2\u05e4\u05e5\5\u00b2Z\2\u05e5\u05e6"+
		"\7\u00c8\2\2\u05e6\u05e8\3\2\2\2\u05e7\u05e1\3\2\2\2\u05e7\u05e2\3\2\2"+
		"\2\u05e7\u05e3\3\2\2\2\u05e8\u05ec\3\2\2\2\u05e9\u05eb\5\u00e8u\2\u05ea"+
		"\u05e9\3\2\2\2\u05eb\u05ee\3\2\2\2\u05ec\u05ea\3\2\2\2\u05ec\u05ed\3\2"+
		"\2\2\u05ed\u00e7\3\2\2\2\u05ee\u05ec\3\2\2\2\u05ef\u05f0\7\u00b4\2\2\u05f0"+
		"\u05f2\5\u00f2z\2\u05f1\u05f3\5\u00eex\2\u05f2\u05f1\3\2\2\2\u05f2\u05f3"+
		"\3\2\2\2\u05f3\u00e9\3\2\2\2\u05f4\u05f5\5\u00ecw\2\u05f5\u05f6\5\u00ee"+
		"x\2\u05f6\u00eb\3\2\2\2\u05f7\u05fb\5\u00c6d\2\u05f8\u05fb\5\u00dco\2"+
		"\u05f9\u05fb\5\u00f0y\2\u05fa\u05f7\3\2\2\2\u05fa\u05f8\3\2\2\2\u05fa"+
		"\u05f9\3\2\2\2\u05fb\u00ed\3\2\2\2\u05fc\u05fe\5,\27\2\u05fd\u05fc\3\2"+
		"\2\2\u05fd\u05fe\3\2\2\2\u05fe\u05ff\3\2\2\2\u05ff\u0603\5\u00ccg\2\u0600"+
		"\u0602\5\u00f8}\2\u0601\u0600\3\2\2\2\u0602\u0605\3\2\2\2\u0603\u0601"+
		"\3\2\2\2\u0603\u0604\3\2\2\2\u0604\u00ef\3\2\2\2\u0605\u0603\3\2\2\2\u0606"+
		"\u0609\5\u00f6|\2\u0607\u0608\7\u00b3\2\2\u0608\u060a\5\u00f6|\2\u0609"+
		"\u0607\3\2\2\2\u0609\u060a\3\2\2\2\u060a\u0610\3\2\2\2\u060b\u060c\5\u00c0"+
		"a\2\u060c\u060d\7\u00b3\2\2\u060d\u060e\5\u00f6|\2\u060e\u0610\3\2\2\2"+
		"\u060f\u0606\3\2\2\2\u060f\u060b\3\2\2\2\u0610\u00f1\3\2\2\2\u0611\u0614"+
		"\5\u00f4{\2\u0612\u0614\5\u00f6|\2\u0613\u0611\3\2\2\2\u0613\u0612\3\2"+
		"\2\2\u0614\u00f3\3\2\2\2\u0615\u061b\5\u0100\u0081\2\u0616\u0617\7\u00cb"+
		"\2\2\u0617\u0618\5\u00a6T\2\u0618\u0619\7\u00cc\2\2\u0619\u061b\3\2\2"+
		"\2\u061a\u0615\3\2\2\2\u061a\u0616\3\2\2\2\u061b\u061f\3\2\2\2\u061c\u061e"+
		"\5\u00f8}\2\u061d\u061c\3\2\2\2\u061e\u0621\3\2\2\2\u061f\u061d\3\2\2"+
		"\2\u061f\u0620\3\2\2\2\u0620\u00f5\3\2\2\2\u0621\u061f\3\2\2\2\u0622\u0624"+
		"\7\u00c4\2\2\u0623\u0622\3\2\2\2\u0624\u0627\3\2\2\2\u0625\u0623\3\2\2"+
		"\2\u0625\u0626\3\2\2\2\u0626\u062e\3\2\2\2\u0627\u0625\3\2\2\2\u0628\u062f"+
		"\7\u00d3\2\2\u0629\u062a\7\u00c4\2\2\u062a\u062b\7\u00cb\2\2\u062b\u062c"+
		"\5\u00a6T\2\u062c\u062d\7\u00cc\2\2\u062d\u062f\3\2\2\2\u062e\u0628\3"+
		"\2\2\2\u062e\u0629\3\2\2\2\u062f\u0633\3\2\2\2\u0630\u0632\5\u00f8}\2"+
		"\u0631\u0630\3\2\2\2\u0632\u0635\3\2\2\2\u0633\u0631\3\2\2\2\u0633\u0634"+
		"\3\2\2\2\u0634\u00f7\3\2\2\2\u0635\u0633\3\2\2\2\u0636\u0638\7\u00c9\2"+
		"\2\u0637\u0639\5\u00a6T\2\u0638\u0637\3\2\2\2\u0638\u0639\3\2\2\2\u0639"+
		"\u063a\3\2\2\2\u063a\u0640\7\u00ca\2\2\u063b\u063c\7\u00cb\2\2\u063c\u063d"+
		"\5\u00a6T\2\u063d\u063e\7\u00cc\2\2\u063e\u0640\3\2\2\2\u063f\u0636\3"+
		"\2\2\2\u063f\u063b\3\2\2\2\u0640\u00f9\3\2\2\2\u0641\u0643\5\u00fc\177"+
		"\2\u0642\u0641\3\2\2\2\u0642\u0643\3\2\2\2\u0643\u064a\3\2\2\2\u0644\u0646"+
		"\7\u00cd\2\2\u0645\u0647\5\u00fc\177\2\u0646\u0645\3\2\2\2\u0646\u0647"+
		"\3\2\2\2\u0647\u0649\3\2\2\2\u0648\u0644\3\2\2\2\u0649\u064c\3\2\2\2\u064a"+
		"\u0648\3\2\2\2\u064a\u064b\3\2\2\2\u064b\u00fb\3\2\2\2\u064c\u064a\3\2"+
		"\2\2\u064d\u0654\5\u00e6t\2\u064e\u064f\7_\2\2\u064f\u0650\7\u00c7\2\2"+
		"\u0650\u0651\5\u00fa~\2\u0651\u0652\7\u00c8\2\2\u0652\u0654\3\2\2\2\u0653"+
		"\u064d\3\2\2\2\u0653\u064e\3\2\2\2\u0654\u00fd\3\2\2\2\u0655\u0656\t\24"+
		"\2\2\u0656\u00ff\3\2\2\2\u0657\u0658\t\25\2\2\u0658\u0101\3\2\2\2\u0659"+
		"\u065a\t\26\2\2\u065a\u0103\3\2\2\2\u065b\u065c\t\27\2\2\u065c\u0105\3"+
		"\2\2\2\u065d\u065e\t\30\2\2\u065e\u0107\3\2\2\2\u065f\u0660\t\31\2\2\u0660"+
		"\u0109\3\2\2\2\u0661\u0662\t\32\2\2\u0662\u010b\3\2\2\2\u00c0\u010d\u0112"+
		"\u011a\u0121\u0126\u012c\u013a\u013e\u0144\u0149\u014e\u0154\u0158\u015e"+
		"\u0165\u016c\u0171\u0175\u017e\u0181\u0184\u0189\u018d\u0191\u0196\u019a"+
		"\u019c\u01a2\u01ae\u01bf\u01c6\u01ce\u01d9\u01e1\u01e9\u01f0\u01f7\u020e"+
		"\u0215\u021d\u0227\u022d\u0231\u024f\u025b\u025f\u0268\u026c\u0271\u028b"+
		"\u0296\u029a\u029e\u02a7\u02b1\u02b6\u02bc\u02c1\u02c6\u02cb\u02d0\u02d6"+
		"\u02dc\u02e2\u02f4\u02f9\u02fc\u0306\u0309\u0316\u031e\u0325\u0328\u032d"+
		"\u0331\u034f\u0353\u0355\u035c\u0360\u0366\u036b\u036e\u0371\u0378\u0380"+
		"\u038d\u0399\u03a5\u03b1\u03b8\u03bc\u03c0\u03c6\u03ce\u03d5\u03d9\u03dd"+
		"\u03ea\u03ed\u03f4\u03fe\u0402\u0407\u040c\u0419\u0423\u0429\u0433\u0440"+
		"\u0442\u0457\u0459\u0468\u046a\u0475\u047e\u0486\u0488\u04aa\u04b7\u04bc"+
		"\u04bf\u04c5\u04e2\u04e9\u04ed\u04f3\u04f7\u04fc\u0504\u050b\u050f\u0514"+
		"\u0519\u051d\u0526\u052c\u0532\u0535\u0539\u053c\u0540\u0547\u054b\u054e"+
		"\u0557\u055f\u0568\u056c\u0571\u0576\u057e\u0580\u0586\u0588\u058d\u0594"+
		"\u059a\u05a1\u05a7\u05b1\u05b5\u05ba\u05c2\u05c8\u05cf\u05d3\u05d7\u05de"+
		"\u05e7\u05ec\u05f2\u05fa\u05fd\u0603\u0609\u060f\u0613\u061a\u061f\u0625"+
		"\u062e\u0633\u0638\u063f\u0642\u0646\u064a\u0653";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}