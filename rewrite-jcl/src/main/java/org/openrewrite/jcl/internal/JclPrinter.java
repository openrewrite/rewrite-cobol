package org.openrewrite.jcl.internal;

import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.JclVisitor;
import org.openrewrite.jcl.tree.*;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.List;
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
    public Jcl visitDataDefinitionStatement(Jcl.DataDefinitionStatement dataDefinitionStatement, PrintOutputCapture<P> p) {
        beforeSyntax(dataDefinitionStatement, Space.Location.DATA_DEFINITION_STATEMENT_PREFIX, p);
        visit(dataDefinitionStatement.getName(), p);
        visitContainer("", dataDefinitionStatement.getPadding().getParameters(), JclContainer.Location.PARAMETERS, ",", "", p);
        afterSyntax(dataDefinitionStatement, p);
        return dataDefinitionStatement;
    }

    @Override
    public Jcl visitExecStatement(Jcl.ExecStatement execStatement, PrintOutputCapture<P> p) {
        beforeSyntax(execStatement, Space.Location.EXEC_STATEMENT_PREFIX, p);
        visit(execStatement.getName(), p);
        visitContainer("", execStatement.getPadding().getParameters(), JclContainer.Location.PARAMETERS, ",", "", p);
        afterSyntax(execStatement, p);
        return execStatement;
    }

    @Override
    public Jcl visitIdentifier(Jcl.Identifier identifier, PrintOutputCapture<P> p) {
        beforeSyntax(identifier, Space.Location.IDENTIFIER_PREFIX, p);
        p.append(identifier.getSimpleName());
        afterSyntax(identifier, p);
        return identifier;
    }

    @Override
    public Jcl visitJclName(Jcl.JclName jclName, PrintOutputCapture<P> p) {
        beforeSyntax(jclName, Space.Location.JCL_NAME_PREFIX, p);
        visit(jclName.getName(), p);
        visit(jclName.getParentheses(), p);
        afterSyntax(jclName, p);
        return jclName;
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
        visitContainer("", jobStatement.getPadding().getParameters(), JclContainer.Location.PARAMETERS, ",", "", p);
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
    public Jcl visitOutputStatement(Jcl.OutputStatement outputStatement, PrintOutputCapture<P> p) {
        beforeSyntax(outputStatement, Space.Location.OUTPUT_STATEMENT_PREFIX, p);
        visit(outputStatement.getName(), p);
        visitContainer("", outputStatement.getPadding().getParameters(), JclContainer.Location.PARAMETERS, ",", "", p);
        afterSyntax(outputStatement, p);
        return outputStatement;
    }

    @Override
    public <T extends Jcl> Jcl visitParentheses(Jcl.Parentheses<T> parentheses, PrintOutputCapture<P> p) {
        beforeSyntax(parentheses, Space.Location.PARENTHESES_PREFIX, p);
        p.append("(");
        if (parentheses.omitFirstParam()) {
            p.append(",");
        }
        visitRightPadded(parentheses.getPadding().getTrees(), JclRightPadded.Location.PARENTHESES, ",", p);
        p.append(")");
        afterSyntax(parentheses, p);
        return parentheses;
    }

    @Override
    public Jcl visitPend(Jcl.Pend pend, PrintOutputCapture<P> p) {
        beforeSyntax(pend, Space.Location.PEND_PREFIX, p);
        visit(pend.getName(), p);
        afterSyntax(pend, p);
        return pend;
    }

    @Override
    public Jcl visitProcStatement(Jcl.ProcStatement procStatement, PrintOutputCapture<P> p) {
        beforeSyntax(procStatement, Space.Location.PROC_STATEMENT_PREFIX, p);
        visit(procStatement.getName(), p);
        visitContainer("", procStatement.getPadding().getParameters(), JclContainer.Location.PARAMETERS, ",", "", p);
        afterSyntax(procStatement, p);
        return procStatement;
    }

    @Override
    public Jcl visitSetStatement(Jcl.SetStatement setStatement, PrintOutputCapture<P> p) {
        beforeSyntax(setStatement, Space.Location.SET_STATEMENT_PREFIX, p);
        visit(setStatement.getName(), p);
        visitContainer("", setStatement.getPadding().getParameters(), JclContainer.Location.PARAMETERS, ",", "", p);
        afterSyntax(setStatement, p);
        return setStatement;
    }

    @Override
    public Space visitSpace(Space space, Space.Location location, PrintOutputCapture<P> p) {
        p.append(space.getWhitespace());
        return space;
    }

    protected void visitContainer(String before, @Nullable JclContainer<? extends Jcl> container, JclContainer.Location location,
                                  String suffixBetween, @Nullable String after, PrintOutputCapture<P> p) {
        if (container == null) {
            return;
        }
        beforeSyntax(container.getBefore(), container.getMarkers(), location.getBeforeLocation(), p);
        p.append(before);
        visitRightPadded(container.getPadding().getElements(), location.getElementLocation(), suffixBetween, p);
        afterSyntax(container.getMarkers(), p);
        p.append(after == null ? "" : after);
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

    protected void visitRightPadded(List<? extends JclRightPadded<? extends Jcl>> nodes, JclRightPadded.Location location, String suffixBetween, PrintOutputCapture<P> p) {
        for (int i = 0; i < nodes.size(); i++) {
            JclRightPadded<? extends Jcl> node = nodes.get(i);
            visit(node.getElement(), p);
            visitSpace(node.getAfter(), location.getAfterLocation(), p);
            visitMarkers(node.getMarkers(), p);
            if (i < nodes.size() - 1) {
                p.append(suffixBetween);
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
