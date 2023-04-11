package org.openrewrite.jcl.internal;

import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.JclVisitor;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.jcl.tree.JclLeftPadded;
import org.openrewrite.jcl.tree.JclRightPadded;
import org.openrewrite.jcl.tree.Space;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.function.UnaryOperator;

public class JclPrinter<P> extends JclVisitor<PrintOutputCapture<P>> {
    public static final UnaryOperator<String> JCL_MARKER_WRAPPER =
            out -> "~~" + out + (out.isEmpty() ? "" : "~~") + ">";

    @Override
    public Jcl.CompilationUnit visitCompilationUnit(Jcl.CompilationUnit cu, PrintOutputCapture<P> p) {
        beforeSyntax(cu, Space.Location.COMPILATION_UNIT_PREFIX, p);
        visit(cu.getStatements(), p);
        afterSyntax(cu, p);
        visitSpace(cu.getEof(), Space.Location.COMPILATION_UNIT_EOF, p);
        return cu;
    }

    @Override
    public Jcl visitAssignment(Jcl.Assignment assignment, PrintOutputCapture<P> p) {
        beforeSyntax(assignment, Space.Location.ASSIGNMENT_PREFIX, p);
        visit(assignment.getVariable(), p);
        visitLeftPadded("=", assignment.getPadding().getAssignment(), JclLeftPadded.Location.ASSIGNMENT, p);
        afterSyntax(assignment, p);
        return assignment;
    }

    @Override
    public Jcl visitIdentifier(Jcl.Identifier identifier, PrintOutputCapture<P> p) {
        beforeSyntax(identifier, Space.Location.IDENTIFIER_PREFIX, p);
        p.append(identifier.getSimpleName());
        afterSyntax(identifier, p);
        return identifier;
    }

    @Override
    public Jcl visitJclStatement(Jcl.JclStatement jclStatement, PrintOutputCapture<P> p) {
        beforeSyntax(jclStatement, Space.Location.JCL_STATEMENT_PREFIX, p);
        p.append("//");
        visit(jclStatement.getName(), p);
        visit(jclStatement.getStatement(), p);
        afterSyntax(jclStatement, p);
        return jclStatement;
    }

    @Override
    public Jcl visitJobStatement(Jcl.JobStatement jobStatement, PrintOutputCapture<P> p) {
        beforeSyntax(jobStatement, Space.Location.JOB_STATEMENT_PREFIX, p);
        visit(jobStatement.getName(), p);
        visit(jobStatement.getParameters(), p);
        afterSyntax(jobStatement, p);
        return jobStatement;
    }

    @Override
    public Jcl visitLiteral(Jcl.Literal literal, PrintOutputCapture<P> p) {
        beforeSyntax(literal, Space.Location.LITERAL_PREFIX, p);
        p.append(literal.getValueSource());
        afterSyntax(literal, p);
        return literal;
    }

    @Override
    public Space visitSpace(Space space, Space.Location location, PrintOutputCapture<P> p) {
        p.append(space.getWhitespace());
        return space;
    }

    protected void visitLeftPadded(@Nullable String prefix, @Nullable JclLeftPadded<? extends Jcl> leftPadded, JclLeftPadded.Location location, PrintOutputCapture<P> p) {
        if (leftPadded != null) {
            beforeSyntax(leftPadded.getBefore(), leftPadded.getMarkers(), location.getBeforeLocation(), p);
            if (prefix != null) {
                p.append(prefix);
            }
            visit(leftPadded.getElement(), p);
            afterSyntax(leftPadded.getMarkers(), p);
        }
    }

    protected void visitRightPadded(@Nullable JclRightPadded<? extends Jcl> rightPadded, JclRightPadded.Location location, @Nullable String suffix, PrintOutputCapture<P> p) {
        if (rightPadded != null) {
            beforeSyntax(Space.EMPTY, rightPadded.getMarkers(), null, p);
            visit(rightPadded.getElement(), p);
            afterSyntax(rightPadded.getMarkers(), p);
            visitSpace(rightPadded.getAfter(), location.getAfterLocation(), p);
            if (suffix != null) {
                p.append(suffix);
            }
        }
    }

    protected void beforeSyntax(Jcl c, Space.Location loc, PrintOutputCapture<P> p) {
        beforeSyntax(c.getPrefix(), c.getMarkers(), loc, p);
    }

    protected void beforeSyntax(Space prefix, Markers markers, @Nullable Space.Location loc, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforePrefix(marker, new Cursor(getCursor(), marker), JCL_MARKER_WRAPPER));
        }
        if (loc != null) {
            visitSpace(prefix, loc, p);
        }
        visitMarkers(markers, p);
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(getCursor(), marker), JCL_MARKER_WRAPPER));
        }
    }

    protected void afterSyntax(Jcl c, PrintOutputCapture<P> p) {
        afterSyntax(c.getMarkers(), p);
    }

    protected void afterSyntax(Markers markers, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(getCursor(), marker), JCL_MARKER_WRAPPER));
        }
    }
}
