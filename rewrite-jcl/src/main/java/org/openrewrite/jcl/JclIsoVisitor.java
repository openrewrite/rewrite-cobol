package org.openrewrite.jcl;

import org.openrewrite.jcl.tree.Jcl;

public class JclIsoVisitor<P> extends JclVisitor<P> {

    @Override
    public Jcl.CompilationUnit visitCompilationUnit(Jcl.CompilationUnit compilationUnit, P p) {
        return (Jcl.CompilationUnit) super.visitCompilationUnit(compilationUnit, p);
    }

    @Override
    public Jcl.Assignment visitAssignment(Jcl.Assignment assignment, P p) {
        return (Jcl.Assignment) super.visitAssignment(assignment, p);
    }

    @Override
    public Jcl.Identifier visitIdentifier(Jcl.Identifier identifier, P p) {
        return (Jcl.Identifier) super.visitIdentifier(identifier, p);
    }

    @Override
    public Jcl.DataDefinitionStatement visitDataDefinitionStatement(Jcl.DataDefinitionStatement dataDefinitionStatement, P p) {
        return (Jcl.DataDefinitionStatement) super.visitDataDefinitionStatement(dataDefinitionStatement, p);
    }

    @Override
    public Jcl.ExecStatement visitExecStatement(Jcl.ExecStatement execStatement, P p) {
        return (Jcl.ExecStatement) super.visitExecStatement(execStatement, p);
    }

    @Override
    public Jcl.JclName visitJclName(Jcl.JclName jclName, P p) {
        return (Jcl.JclName) super.visitJclName(jclName, p);
    }

    @Override
    public Jcl.JclStatement visitJclStatement(Jcl.JclStatement jclStatement, P p) {
        return (Jcl.JclStatement) super.visitJclStatement(jclStatement, p);
    }

    @Override
    public Jcl.JobStatement visitJobStatement(Jcl.JobStatement jobStatement, P p) {
        return (Jcl.JobStatement) super.visitJobStatement(jobStatement, p);
    }

    @Override
    public Jcl.Literal visitLiteral(Jcl.Literal literal, P p) {
        return (Jcl.Literal) super.visitLiteral(literal, p);
    }

    @Override
    public Jcl.OutputStatement visitOutputStatement(Jcl.OutputStatement outputStatement, P p) {
        return (Jcl.OutputStatement) super.visitOutputStatement(outputStatement, p);
    }

    @Override
    public <T extends Jcl> Jcl.Parentheses<T> visitParentheses(Jcl.Parentheses<T> parentheses, P p) {
        //noinspection unchecked
        return (Jcl.Parentheses<T>) super.visitParentheses(parentheses, p);
    }

    @Override
    public Jcl.Pend visitPend(Jcl.Pend pend, P p) {
        return (Jcl.Pend) super.visitPend(pend, p);
    }

    @Override
    public Jcl.ProcStatement visitProcStatement(Jcl.ProcStatement procStatement, P p) {
        return (Jcl.ProcStatement) super.visitProcStatement(procStatement, p);
    }
}
