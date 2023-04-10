package org.openrewrite.jcl.internal;

import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.JclVisitor;
import org.openrewrite.jcl.tree.Jcl;
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
    public Jcl visitJclStatement(Jcl.JclStatement jclStatement, PrintOutputCapture<P> p) {
        beforeSyntax(jclStatement, Space.Location.COMPILATION_UNIT_PREFIX, p);
        p.append("//");
        visit(jclStatement.getName(), p);
        visit(jclStatement.getStatement(), p);
        afterSyntax(jclStatement, p);
        return jclStatement;
    }

    @Override
    public Jcl visitJobStatement(Jcl.JobStatement jobStatement, PrintOutputCapture<P> p) {
        beforeSyntax(jobStatement, Space.Location.COMPILATION_UNIT_PREFIX, p);
        visit(jobStatement.getName(), p);
        afterSyntax(jobStatement, p);
        return jobStatement;
    }

    @Override
    public Jcl visitIdentifier(Jcl.Identifier identifier, PrintOutputCapture<P> p) {
        beforeSyntax(identifier, Space.Location.IDENTIFIER_PREFIX, p);
        p.append(identifier.getSimpleName());
        afterSyntax(identifier, p);
        return identifier;
    }

    @Override
    public Space visitSpace(Space space, Space.Location location, PrintOutputCapture<P> p) {
        p.append(space.getWhitespace());
        return space;
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
