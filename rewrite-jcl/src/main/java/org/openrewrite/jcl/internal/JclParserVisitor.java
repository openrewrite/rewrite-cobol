package org.openrewrite.jcl.internal;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.openrewrite.FileAttributes;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.internal.grammar.JCLParser;
import org.openrewrite.jcl.internal.grammar.JCLParserBaseVisitor;
import org.openrewrite.jcl.tree.*;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static java.util.Collections.emptyList;
import static org.openrewrite.Tree.randomId;
import static org.openrewrite.jcl.tree.Space.EMPTY;

public class JclParserVisitor extends JCLParserBaseVisitor<Jcl> {

    private final Path path;
    private final @Nullable FileAttributes fileAttributes;
    private final String source;
    private final Charset charset;
    private final boolean charsetBomMarked;

    private int cursor = 0;

    public JclParserVisitor(Path path,
                            @Nullable FileAttributes fileAttributes,
                            String source,
                            Charset charset,
                            boolean charsetBomMarked) {
        this.path = path;
        this.fileAttributes = fileAttributes;
        this.source = source;
        this.charset = charset;
        this.charsetBomMarked = charsetBomMarked;
    }

    public <T> T visit(@Nullable ParseTree... trees) {
        for (ParseTree tree : trees) {
            if (tree != null) {
                //noinspection unchecked
                return (T) visit(tree);
            }
        }
        throw new IllegalStateException("Expected one of the supplied trees to be non-null");
    }

    public <T> T visitNullable(@Nullable ParseTree tree) {
        if (tree == null) {
            //noinspection ConstantConditions
            return null;
        }
        //noinspection unchecked
        return (T) super.visit(tree);
    }

    @Override
    public Jcl.CompilationUnit visitCompilationUnit(JCLParser.CompilationUnitContext ctx) {
        List<Statement> statements = new ArrayList<>(ctx.statement().size());
        for (JCLParser.StatementContext statement : ctx.statement()) {
            statements.add((Statement) visitStatement(statement));
        }

        return new Jcl.CompilationUnit(
                randomId(),
                path,
                fileAttributes,
                EMPTY,
                Markers.EMPTY,
                charset.name(),
                charsetBomMarked,
                null,
                statements,
                Space.build(source.substring(cursor))
        );
    }

    @Override
    public Jcl visitJclStatement(JCLParser.JclStatementContext ctx) {
        skip("//");
        return new Jcl.JclStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                createIdentifier(ctx.JCL_STATEMENT().getText().substring(2)),
                visitJobStatement(ctx.jobStatement())
        );
    }

    @Override
    public Jcl visitJobStatement(JCLParser.JobStatementContext ctx) {
        return new Jcl.JobStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                createIdentifier(ctx.JOB().getText()),
                ctx.parameter().isEmpty() ? null : JclContainer.build(EMPTY, convertAll(ctx.parameter(), commaDelim, t -> EMPTY), Markers.EMPTY)
        );
    }

    @Override
    public Jcl visitParameter(JCLParser.ParameterContext ctx) {
        return super.visitParameter(ctx);
    }

    @Override
    public Jcl visitParameterAssignment(JCLParser.ParameterAssignmentContext ctx) {
        return new Jcl.Assignment(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.PARAMETER(), ctx.NAME_FIELD()),
                padLeft(sourceBefore("="), (Expression) visit(ctx.parameter()))
        );
    }

    @Override
    public Jcl visitParameterLiteral(JCLParser.ParameterLiteralContext ctx) {
        Space prefix = whitespace();
        String value = ctx.getText();
        String sourceText = source.substring(ctx.start.getStartIndex(), ctx.stop.getStopIndex() + 1);
        skip(sourceText);
        return new Jcl.Literal(
                randomId(),
                prefix,
                Markers.EMPTY,
                value,
                sourceText
        );
    }

    @Override
    public Jcl visitParameterParentheses(JCLParser.ParameterParenthesesContext ctx) {
        Space prefix = whitespace();
        Boolean omitFirstParam = null;
        skip("(");
        List<JclRightPadded<Jcl>> padded = new ArrayList<>(ctx.parameter().size());
        for (int i = 0; i < ctx.parameter().size(); i++) {
            if (i == 0) {
                int saveCursor = cursor;
                whitespace();
                if (source.startsWith(",", cursor)) {
                    omitFirstParam = true;
                    skip(",");
                } else {
                    cursor = saveCursor;
                }
            }
            Jcl tree = visit(ctx.parameter().get(i));
            padded.add(padRight(tree, i < ctx.parameter().size() - 1 ?
                    sourceBefore(",") : sourceBefore(")"))
            );
        }
        return new Jcl.Parentheses<>(
                randomId(),
                prefix,
                Markers.EMPTY,
                padded,
                omitFirstParam
        );
    }

    @Override
    public Jcl visitTerminal(TerminalNode node) {
        return createIdentifier(node.getText());
    }

    private Jcl.Identifier createIdentifier(@Nullable String name) {
        Space prefix = whitespace();
        skip(name);
        return new Jcl.Identifier(
                randomId(),
                prefix,
                Markers.EMPTY,
                name
        );
    }

    private <C extends Jcl, T extends ParseTree> List<C> convertAll(List<T> trees) {
        //noinspection unchecked
        return convertAll(trees, t -> (C) visit(t));
    }

    private <C, T extends ParseTree> List<C> convertAll(List<T> trees, Function<T, C> convert) {
        List<C> converted = new ArrayList<>(trees.size());
        for (T tree : trees) {
            converted.add(convert.apply(tree));
        }
        return converted;
    }

    private final Function<ParseTree, Space> commaDelim = ignored -> sourceBefore(",");
    private final Function<ParseTree, Space> noDelim = ignored -> EMPTY;

    private <J2 extends Jcl> List<JclRightPadded<J2>> convertAll(List<JCLParser.ParameterContext> elements,
                                                                                      Function<ParseTree, Space> innerSuffix,
                                                                                      Function<ParseTree, Space> suffix) {
        if (elements.isEmpty()) {
            return emptyList();
        }

        List<JclRightPadded<J2>> converted = new ArrayList<>(elements.size());
        for (int i = 0; i < elements.size(); i++) {
            ParseTree element = elements.get(i);
            //noinspection unchecked
            J2 j = (J2) visit(element);
            Space after = i == elements.size() - 1 ? suffix.apply(element) : innerSuffix.apply(element);
            JclRightPadded<J2> rightPadded = padRight(j, after);
            converted.add(rightPadded);
        }
        return converted.isEmpty() ? emptyList() : converted;
    }

    private void skip(@Nullable String token) {
        if (token != null && source.startsWith(token, cursor)) {
            cursor += token.length();
        }
    }

    private Space whitespace() {
        int endIndex = indexOfNextNonWhitespace(cursor, source);
        String prefix = source.substring(cursor, endIndex);
        cursor += prefix.length();
        return Space.build(prefix);
    }

    // TODO: update ... this does not work 100% for JCL.
    // NOTE: this may require revisions to work similar to COBOL when introducing continuations...
    private Space sourceBefore(String untilDelim) {
        Space prefix = whitespace();
        skip(untilDelim);
        return Space.build(prefix.getWhitespace());
    }

    // TODO: update ... this does not work for JCL.
    public static int indexOfNextNonWhitespace(int cursor, String source) {
        boolean inMultiLineComment = false;
        boolean inSingleLineComment = false;

        int delimIndex = cursor;
        for (; delimIndex < source.length(); delimIndex++) {
            if (inSingleLineComment) {
                if (source.charAt(delimIndex) == '\n') {
                    inSingleLineComment = false;
                }
            } else {
                if (source.length() > delimIndex + 1) {
                    switch (source.substring(delimIndex, delimIndex + 2)) {
                        case "//":
                            inSingleLineComment = true;
                            delimIndex++;
                            continue;
                        case "/*":
                            inMultiLineComment = true;
                            delimIndex++;
                            continue;
                        case "*/":
                            inMultiLineComment = false;
                            delimIndex++;
                            continue;
                    }
                }
            }
            if (!inMultiLineComment && !inSingleLineComment) {
                if (!Character.isWhitespace(source.substring(delimIndex, delimIndex + 1).charAt(0))) {
                    break; // found it!
                }
            }
        }
        return delimIndex;
    }

    private <T> JclLeftPadded<T> padLeft(Space left, T tree) {
        return new JclLeftPadded<>(left, tree, Markers.EMPTY);
    }

    @SuppressWarnings("SameParameterValue")
    private <T> JclRightPadded<T> padRight(T tree, @Nullable Space right) {
        return new JclRightPadded<>(tree, right == null ? EMPTY : right, Markers.EMPTY);
    }
}
