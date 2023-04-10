package org.openrewrite.jcl;

import org.openrewrite.TreeVisitor;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.jcl.tree.Space;

public class JclVisitor<P> extends TreeVisitor<Jcl, P> {

    public Jcl visitCompilationUnit(Jcl.CompilationUnit compilationUnit, P p) {
        Jcl.CompilationUnit c = compilationUnit;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COMPILATION_UNIT_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withStatements(ListUtils.map(c.getStatements(), e -> visitAndCast(e, p)));
        c = c.withEof(visitSpace(c.getEof(), Space.Location.COMPILATION_UNIT_EOF, p));
        return c;
    }

    public Jcl visitIdentifier(Jcl.Identifier identifier, P p) {
        Jcl.Identifier i = identifier;
        i = i.withPrefix(visitSpace(i.getPrefix(), Space.Location.JCL_STATEMENT_PREFIX, p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }

    public Jcl visitJclStatement(Jcl.JclStatement jclStatement, P p) {
        Jcl.JclStatement j = jclStatement;
        j = j.withPrefix(visitSpace(j.getPrefix(), Space.Location.JCL_STATEMENT_PREFIX, p));
        j = j.withMarkers(visitMarkers(j.getMarkers(), p));
        j = j.withName(visitAndCast(j.getName(), p));
        j = j.withStatement(visitAndCast(j.getStatement(), p));
        return j;
    }

    public Jcl visitJobStatement(Jcl.JobStatement jobStatement, P p) {
        Jcl.JobStatement j = jobStatement;
        j = j.withPrefix(visitSpace(j.getPrefix(), Space.Location.JCL_STATEMENT_PREFIX, p));
        j = j.withMarkers(visitMarkers(j.getMarkers(), p));
        j = j.withName(visitAndCast(j.getName(), p));
        return j;
    }

    public Space visitSpace(Space space, Space.Location location, P p) {
        return space;
    }
}
