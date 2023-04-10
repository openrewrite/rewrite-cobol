package org.openrewrite.jcl.internal;

import org.antlr.v4.runtime.tree.ParseTree;
import org.openrewrite.FileAttributes;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.internal.grammar.JCLParser;
import org.openrewrite.jcl.internal.grammar.JCLParserBaseVisitor;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.jcl.tree.Space;
import org.openrewrite.jcl.tree.Statement;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static org.openrewrite.Tree.randomId;
import static org.openrewrite.internal.StringUtils.indexOfNextNonWhitespace;
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
                convertAll(ctx.parameter())
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

    private void skip(@Nullable String token) {
        if (token != null && source.startsWith(token, cursor)) {
            cursor += token.length();
        }
    }

    private Space whitespace() {
        int endIndex = indexOfNextNonWhitespace(cursor, source); // TODO: apply whitespace rules in JCL.
        String prefix = source.substring(cursor, endIndex);
        cursor += prefix.length();
        return Space.build(prefix);
    }
}
