package org.openrewrite.jcl;

import org.openrewrite.Cursor;
import org.openrewrite.TreeVisitor;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.tree.*;
import org.openrewrite.marker.Markers;

import java.util.List;

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

    public Jcl visitDataDefinitionStatement(Jcl.DataDefinitionStatement dataDefinitionStatement, P p) {
        Jcl.DataDefinitionStatement d = dataDefinitionStatement;
        d = d.withPrefix(visitSpace(d.getPrefix(), Space.Location.DATA_DEFINITION_STATEMENT_PREFIX, p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withName(visitAndCast(d.getName(), p));
        d = d.getPadding().withParameters(visitContainer(d.getPadding().getParameters(), JclContainer.Location.PARAMETERS, p));
        return d;
    }

    public Jcl visitExecStatement(Jcl.ExecStatement execStatement, P p) {
        Jcl.ExecStatement e = execStatement;
        e = e.withPrefix(visitSpace(e.getPrefix(), Space.Location.EXEC_STATEMENT_PREFIX, p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withName(visitAndCast(e.getName(), p));
        e = e.getPadding().withParameters(visitContainer(e.getPadding().getParameters(), JclContainer.Location.PARAMETERS, p));
        return e;
    }

    public Jcl visitIdentifier(Jcl.Identifier identifier, P p) {
        Jcl.Identifier i = identifier;
        i = i.withPrefix(visitSpace(i.getPrefix(), Space.Location.IDENTIFIER_PREFIX, p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }

    public Jcl visitJclName (Jcl.JclName jclName, P p) {
        Jcl.JclName j = jclName;
        j = j.withPrefix(visitSpace(j.getPrefix(), Space.Location.JCL_NAME_PREFIX, p));
        j = j.withMarkers(visitMarkers(j.getMarkers(), p));
        return j;
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
        j = j.withPrefix(visitSpace(j.getPrefix(), Space.Location.JOB_STATEMENT_PREFIX, p));
        j = j.withMarkers(visitMarkers(j.getMarkers(), p));
        j = j.withName(visitAndCast(j.getName(), p));
        j = j.getPadding().withParameters(visitContainer(j.getPadding().getParameters(), JclContainer.Location.PARAMETERS, p));
        return j;
    }

    public Jcl visitLiteral(Jcl.Literal literal, P p) {
        Jcl.Literal l = literal;
        l = l.withPrefix(visitSpace(l.getPrefix(), Space.Location.LITERAL_PREFIX, p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Jcl visitOutputStatement(Jcl.OutputStatement outputStatement, P p) {
        Jcl.OutputStatement o = outputStatement;
        o = o.withPrefix(visitSpace(o.getPrefix(), Space.Location.OUTPUT_STATEMENT_PREFIX, p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        o = o.withName(visitAndCast(o.getName(), p));
        o = o.getPadding().withParameters(visitContainer(o.getPadding().getParameters(), JclContainer.Location.PARAMETERS, p));
        return o;
    }

    public <T extends Jcl> Jcl visitParentheses(Jcl.Parentheses<T> parentheses, P p) {
        Jcl.Parentheses<T> pa = parentheses;
        pa = pa.withPrefix(visitSpace(pa.getPrefix(), Space.Location.PARENTHESES_PREFIX, p));
        pa = pa.getPadding().withTrees(ListUtils.map(pa.getPadding().getTrees(), t -> visitRightPadded(t, JclRightPadded.Location.PARENTHESES, p)));
        pa = pa.withMarkers(visitMarkers(pa.getMarkers(), p));
        return pa;
    }

    public Jcl visitPend(Jcl.Pend pend, P p) {
        Jcl.Pend pe = pend;
        pe = pe.withPrefix(visitSpace(pe.getPrefix(), Space.Location.PEND_PREFIX, p));
        pe = pe.withMarkers(visitMarkers(pe.getMarkers(), p));
        return pe;
    }

    public Jcl visitProcStatement(Jcl.ProcStatement procStatement, P p) {
        Jcl.ProcStatement pr = procStatement;
        pr = pr.withPrefix(visitSpace(pr.getPrefix(), Space.Location.PROC_STATEMENT_PREFIX, p));
        pr = pr.withMarkers(visitMarkers(pr.getMarkers(), p));
        pr = pr.withName(visitAndCast(pr.getName(), p));
        pr = pr.getPadding().withParameters(visitContainer(pr.getPadding().getParameters(), JclContainer.Location.PARAMETERS, p));
        return pr;
    }

    public Space visitSpace(Space space, Space.Location location, P p) {
        return space;
    }

    public <J2 extends Jcl> JclContainer<J2> visitContainer(@Nullable JclContainer<J2> container,
                                                            JclContainer.Location loc, P p) {
        if (container == null) {
            //noinspection ConstantConditions
            return null;
        }
        setCursor(new Cursor(getCursor(), container));

        Space before = visitSpace(container.getBefore(), loc.getBeforeLocation(), p);
        List<JclRightPadded<J2>> js = ListUtils.map(container.getPadding().getElements(), t -> visitRightPadded(t, loc.getElementLocation(), p));

        setCursor(getCursor().getParent());

        return js == container.getPadding().getElements() && before == container.getBefore() ?
                container :
                JclContainer.build(before, js, container.getMarkers());
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

    public <T> JclRightPadded<T> visitRightPadded(@Nullable JclRightPadded<T> right, JclRightPadded.Location loc, P p) {
        if (right == null) {
            //noinspection ConstantConditions
            return null;
        }

        setCursor(new Cursor(getCursor(), right));

        T t = right.getElement();
        if (t instanceof Jcl) {
            //noinspection unchecked
            t = visitAndCast((Jcl) right.getElement(), p);
        }

        setCursor(getCursor().getParent());
        if (t == null) {
            //noinspection ConstantConditions
            return null;
        }

        Space after = visitSpace(right.getAfter(), loc.getAfterLocation(), p);
        Markers markers = visitMarkers(right.getMarkers(), p);
        return (after == right.getAfter() && t == right.getElement() && markers == right.getMarkers()) ?
                right : new JclRightPadded<>(t, after, markers);
    }
}
