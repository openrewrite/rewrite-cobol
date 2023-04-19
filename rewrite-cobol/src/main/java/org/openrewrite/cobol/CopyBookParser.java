package org.openrewrite.cobol;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Parser;
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.internal.CobolPreprocessorParserVisitor;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorLexer;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;
import org.openrewrite.text.PlainText;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.nio.file.Path;
import java.util.*;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static org.openrewrite.Tree.randomId;

/**
 * Read preprocessed COBOL and execute preprocessor commands.
 */
public class CopyBookParser implements Parser<CobolPreprocessor.CopyBook> {
    public static final List<String> COPYBOOK_FILE_EXTENSIONS = Collections.singletonList(".cpy");

    private final CobolDialect cobolDialect;

    public CopyBookParser(CobolDialect cobolDialect) {
        this.cobolDialect = cobolDialect;
    }

    @Override
    public List<CobolPreprocessor.CopyBook> parseInputs(Iterable<Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
        ParsingExecutionContextView pctx = ParsingExecutionContextView.view(ctx);
        ParsingEventListener parsingListener = pctx.getParsingListener();
        return acceptedInputs(sourceFiles).stream()
                .map(sourceFile -> {
                    Timer.Builder timer = Timer.builder("rewrite.parse")
                            .description("The time spent parsing a COBOL file")
                            .tag("file.type", "COBOL");
                    Timer.Sample sample = Timer.start();
                    try {
                        EncodingDetectingInputStream is = sourceFile.getSource(ctx);
                        String sourceStr = is.readFully();

                        PlainText plainText = new PlainText(
                                randomId(),
                                sourceFile.getPath(),
                                Markers.EMPTY,
                                is.getCharset().name(),
                                is.isCharsetBomMarked(),
                                null,
                                null,
                                sourceStr,
                                emptyList()
                        );

                        String prepareSource = new CobolLineReader().readLines(sourceStr, cobolDialect);
                        org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser parser =
                                new org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser(
                                        new CommonTokenStream(new CobolPreprocessorLexer(CharStreams.fromString(prepareSource))));

                        CobolPreprocessorParserVisitor parserVisitor = new CobolPreprocessorParserVisitor(
                                sourceFile.getRelativePath(relativeTo),
                                sourceFile.getFileAttributes(),
                                sourceStr,
                                is.getCharset(),
                                is.isCharsetBomMarked(),
                                cobolDialect
                        );

                        CobolPreprocessor.CompilationUnit preprocessedCU = parserVisitor.visitStartRule(parser.startRule());
                        CobolPreprocessor parsedCopySource = preprocessedCU.getCobols().get(0);

                        CobolPreprocessor.CopyBook copyBook = new CobolPreprocessor.CopyBook(
                                randomId(),
                                Space.EMPTY,
                                Markers.EMPTY,
                                plainText.getSourcePath(),
                                null,
                                plainText.getCharsetName(),
                                plainText.isCharsetBomMarked(),
                                null,
                                parsedCopySource,
                                preprocessedCU.getEof()
                        );

                        sample.stop(MetricsHelper.successTags(timer).register(Metrics.globalRegistry));
                        parsingListener.parsed(sourceFile, preprocessedCU);
                        return copyBook;
                    } catch (Throwable t) {
                        sample.stop(MetricsHelper.errorTags(timer, t).register(Metrics.globalRegistry));
                        pctx.parseFailure(sourceFile, relativeTo, this, t);
                        ctx.getOnError().accept(t);
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(toList());
    }

    @Override
    public List<CobolPreprocessor.CopyBook> parse(String... sources) {
        return parse(new InMemoryExecutionContext(), sources);
    }

    @Override
    public boolean accept(Path path) {
        String s = path.toString().toLowerCase();
        for (String COBOL_FILE_EXTENSION : COPYBOOK_FILE_EXTENSIONS) {
            if (s.endsWith(COBOL_FILE_EXTENSION)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public Path sourcePathFromSourceText(Path prefix, String sourceCode) {
        return prefix.resolve("file.CPY");
    }

    public static CopyBookParser.Builder builder() {
        return new CopyBookParser.Builder();
    }

    public static class Builder extends Parser.Builder {

        private CobolDialect cobolDialect = CobolDialect.ibmAnsi85();

        public Builder() {
            super(Cobol.CompilationUnit.class);
        }

        @Override
        public CopyBookParser build() {
            return new CopyBookParser(cobolDialect);
        }

        public Builder setCobolDialect(CobolDialect cobolDialect) {
            this.cobolDialect = cobolDialect;
            return this;
        }

        @Override
        public String getDslName() {
            return "copyBook";
        }
    }
}
