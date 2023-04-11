package org.openrewrite.jcl;

import org.openrewrite.Cursor;
import org.openrewrite.TreeVisitor;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.jcl.tree.JclLeftPadded;
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

    public Jcl visitAssignment(Jcl.Assignment assignment, P p ) {
        Jcl.Assignment a = assignment;
        a = a.withPrefix(visitSpace(a.getPrefix(), Space.Location.ASSIGNMENT_PREFIX, p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withVariable(visitAndCast(a.getVariable(), p));
        a = a.getPadding().withAssignment(visitLeftPadded(a.getPadding().getAssignment(), JclLeftPadded.Location.ASSIGNMENT, p));
        return a;
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
        j = j.withParameters(ListUtils.map(j.getParameters(), e -> visitAndCast(e, p)));
        return j;
    }

    public Jcl visitLiteral(Jcl.Literal literal, P p) {
        Jcl.Literal l = literal;
        l = l.withPrefix(visitSpace(l.getPrefix(), Space.Location.JCL_STATEMENT_PREFIX, p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public <T> JclLeftPadded<T> visitLeftPadded(@Nullable JclLeftPadded<T> left, JclLeftPadded.Location loc, P p) {
        if (left == null) {
            //noinspection ConstantConditions
            return null;
        }

        setCursor(new Cursor(getCursor(), left));

        Space before = visitSpace(left.getBefore(), loc.getBeforeLocation(), p);
        T t = left.getElement();

        if (t instanceof Jcl) {
            //noinspection unchecked
            t = visitAndCast((Jcl) left.getElement(), p);
        }

        setCursor(getCursor().getParent());
        if (t == null) {
            // If nothing changed leave AST node the same
            if (left.getElement() == null && before == left.getBefore()) {
                return left;
            }
            //noinspection ConstantConditions
            return null;
        }

        return (before == left.getBefore() && t == left.getElement()) ? left : new JclLeftPadded<>(before, t, left.getMarkers());
    }

    public Space visitSpace(Space space, Space.Location location, P p) {
        return space;
    }
}
