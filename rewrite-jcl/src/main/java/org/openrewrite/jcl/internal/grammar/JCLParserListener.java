// Generated from java-escape by ANTLR 4.11.1
package org.openrewrite.jcl.internal.grammar;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link JCLParser}.
 */
public interface JCLParserListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link JCLParser#compilationUnit}.
	 * @param ctx the parse tree
	 */
	void enterCompilationUnit(JCLParser.CompilationUnitContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#compilationUnit}.
	 * @param ctx the parse tree
	 */
	void exitCompilationUnit(JCLParser.CompilationUnitContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#statement}.
	 * @param ctx the parse tree
	 */
	void enterStatement(JCLParser.StatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#statement}.
	 * @param ctx the parse tree
	 */
	void exitStatement(JCLParser.StatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#jclStatement}.
	 * @param ctx the parse tree
	 */
	void enterJclStatement(JCLParser.JclStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#jclStatement}.
	 * @param ctx the parse tree
	 */
	void exitJclStatement(JCLParser.JclStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#jobStatement}.
	 * @param ctx the parse tree
	 */
	void enterJobStatement(JCLParser.JobStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#jobStatement}.
	 * @param ctx the parse tree
	 */
	void exitJobStatement(JCLParser.JobStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddStatement}.
	 * @param ctx the parse tree
	 */
	void enterDdStatement(JCLParser.DdStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddStatement}.
	 * @param ctx the parse tree
	 */
	void exitDdStatement(JCLParser.DdStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#execStatement}.
	 * @param ctx the parse tree
	 */
	void enterExecStatement(JCLParser.ExecStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#execStatement}.
	 * @param ctx the parse tree
	 */
	void exitExecStatement(JCLParser.ExecStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#outputStatement}.
	 * @param ctx the parse tree
	 */
	void enterOutputStatement(JCLParser.OutputStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#outputStatement}.
	 * @param ctx the parse tree
	 */
	void exitOutputStatement(JCLParser.OutputStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#pendStatement}.
	 * @param ctx the parse tree
	 */
	void enterPendStatement(JCLParser.PendStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#pendStatement}.
	 * @param ctx the parse tree
	 */
	void exitPendStatement(JCLParser.PendStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#procStatement}.
	 * @param ctx the parse tree
	 */
	void enterProcStatement(JCLParser.ProcStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#procStatement}.
	 * @param ctx the parse tree
	 */
	void exitProcStatement(JCLParser.ProcStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#setStatement}.
	 * @param ctx the parse tree
	 */
	void enterSetStatement(JCLParser.SetStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#setStatement}.
	 * @param ctx the parse tree
	 */
	void exitSetStatement(JCLParser.SetStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#xmitStatement}.
	 * @param ctx the parse tree
	 */
	void enterXmitStatement(JCLParser.XmitStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#xmitStatement}.
	 * @param ctx the parse tree
	 */
	void exitXmitStatement(JCLParser.XmitStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#parameter}.
	 * @param ctx the parse tree
	 */
	void enterParameter(JCLParser.ParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#parameter}.
	 * @param ctx the parse tree
	 */
	void exitParameter(JCLParser.ParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#parameterParentheses}.
	 * @param ctx the parse tree
	 */
	void enterParameterParentheses(JCLParser.ParameterParenthesesContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#parameterParentheses}.
	 * @param ctx the parse tree
	 */
	void exitParameterParentheses(JCLParser.ParameterParenthesesContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#parameterAssignment}.
	 * @param ctx the parse tree
	 */
	void enterParameterAssignment(JCLParser.ParameterAssignmentContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#parameterAssignment}.
	 * @param ctx the parse tree
	 */
	void exitParameterAssignment(JCLParser.ParameterAssignmentContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#parameterLiteral}.
	 * @param ctx the parse tree
	 */
	void enterParameterLiteral(JCLParser.ParameterLiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#parameterLiteral}.
	 * @param ctx the parse tree
	 */
	void exitParameterLiteral(JCLParser.ParameterLiteralContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#name}.
	 * @param ctx the parse tree
	 */
	void enterName(JCLParser.NameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#name}.
	 * @param ctx the parse tree
	 */
	void exitName(JCLParser.NameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#unsupportedStatement}.
	 * @param ctx the parse tree
	 */
	void enterUnsupportedStatement(JCLParser.UnsupportedStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#unsupportedStatement}.
	 * @param ctx the parse tree
	 */
	void exitUnsupportedStatement(JCLParser.UnsupportedStatementContext ctx);
}