// Generated from java-escape by ANTLR 4.11.1
package org.openrewrite.jcl.internal.grammar;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link JCLParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface JCLParserVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link JCLParser#compilationUnit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCompilationUnit(JCLParser.CompilationUnitContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatement(JCLParser.StatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#jclStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJclStatement(JCLParser.JclStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#jobStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobStatement(JCLParser.JobStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdStatement(JCLParser.DdStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#execStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExecStatement(JCLParser.ExecStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#outputStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOutputStatement(JCLParser.OutputStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#pendStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPendStatement(JCLParser.PendStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#procStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcStatement(JCLParser.ProcStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#scheduleStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitScheduleStatement(JCLParser.ScheduleStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#setStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSetStatement(JCLParser.SetStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#xmitStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitXmitStatement(JCLParser.XmitStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#parameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParameter(JCLParser.ParameterContext ctx);
}