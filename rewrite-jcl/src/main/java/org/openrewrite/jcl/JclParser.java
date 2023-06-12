/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.jcl;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;
import org.antlr.v4.runtime.*;
import org.openrewrite.*;
import org.openrewrite.Parser;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.internal.JclParserVisitor;
import org.openrewrite.jcl.internal.grammar.JCLLexer;
import org.openrewrite.jcl.internal.grammar.JCLParser;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

public class JclParser implements Parser {

    @Override
    public Stream<SourceFile> parseInputs(Iterable<Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
        ParsingExecutionContextView pctx = ParsingExecutionContextView.view(ctx);
        ParsingEventListener parsingListener = pctx.getParsingListener();
        Stream<Input> accepted = acceptedInputs(sourceFiles);

        return accepted
                .map(sourceFile -> {
                    Timer.Builder timer = Timer.builder("rewrite.parse")
                            .description("The time spent parsing a JCL file")
                            .tag("file.type", "JCL");
                    Timer.Sample sample = Timer.start();
                    Path path = sourceFile.getRelativePath(relativeTo);
                    try {
                        EncodingDetectingInputStream is = sourceFile.getSource(ctx);
                        String sourceStr = is.readFully();

                        JCLParser parser = new JCLParser(new CommonTokenStream(new JCLLexer(
                                CharStreams.fromString(sourceStr))));

                        parser.removeErrorListeners();
                        parser.addErrorListener(new ForwardingErrorListener(sourceFile.getPath(), ctx));

                        Jcl.CompilationUnit cu = new JclParserVisitor(
                                path,
                                sourceFile.getFileAttributes(),
                                sourceStr,
                                is.getCharset(),
                                is.isCharsetBomMarked()
                        ).visitCompilationUnit(parser.compilationUnit());

                        sample.stop(MetricsHelper.successTags(timer).register(Metrics.globalRegistry));
                        parsingListener.parsed(sourceFile, cu);
                        return cu;
                    } catch (Throwable t) {
                        sample.stop(MetricsHelper.errorTags(timer, t).register(Metrics.globalRegistry));
                        return ParseError.build(this, sourceFile, relativeTo, pctx, t);
                    }
                });
    }

    @Override
    public boolean accept(Path path) {
        return path.toString().toLowerCase().endsWith(".jcl");
    }

    @Override
    public Path sourcePathFromSourceText(Path prefix, String sourceCode) {
        return prefix.resolve("file.jcl");
    }

    private static class ForwardingErrorListener extends BaseErrorListener {
        private final Path sourcePath;
        private final ExecutionContext ctx;

        private ForwardingErrorListener(Path sourcePath, ExecutionContext ctx) {
            this.sourcePath = sourcePath;
            this.ctx = ctx;
        }

        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                                int line, int charPositionInLine, String msg, RecognitionException e) {
            ctx.getOnError().accept(new JclParsingException(sourcePath,
                    String.format("Syntax error in %s at line %d:%d %s.", sourcePath, line, charPositionInLine, msg), e));
        }
    }

    public static JclParser.Builder builder() {
        return new JclParser.Builder();
    }

    public static class Builder extends org.openrewrite.Parser.Builder {
        public Builder() {
            super(Jcl.CompilationUnit.class);
        }

        @Override
        public JclParser build() {
            return new JclParser();
        }

        @Override
        public String getDslName() {
            return "jcl";
        }
    }
}
