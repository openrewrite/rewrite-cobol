package org.openrewrite.jcl;

import org.openrewrite.jcl.tree.Jcl;

public class JclIsoVisitor<P> extends JclVisitor<P> {

    @Override
    public Jcl.CompilationUnit visitCompilationUnit(Jcl.CompilationUnit compilationUnit, P p) {
        return (Jcl.CompilationUnit) super.visitCompilationUnit(compilationUnit, p);
    }

    @Override
    public Jcl.Identifier visitIdentifier(Jcl.Identifier identifier, P p) {
        return (Jcl.Identifier) super.visitIdentifier(identifier, p);
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
}
