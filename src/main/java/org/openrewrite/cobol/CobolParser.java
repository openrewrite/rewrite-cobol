/*
 * Copyright 2022 the original author or authors.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * https://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openrewrite.cobol;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;
import org.antlr.v4.runtime.*;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Parser;
import org.openrewrite.cobol.internal.CobolParserVisitor;
import org.openrewrite.cobol.internal.grammar.CobolLexer;
import org.openrewrite.cobol.proprocessor.*;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.io.File;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

public class CobolParser implements Parser<Cobol.CompilationUnit> {
    private static final List<String> COBOL_FILE_EXTENSIONS = Arrays.asList(".cbl", ".cpy");

    private final CobolParserVisitor.CobolDialect cobolDialect;
    private final CobolDialect preprocessorDialect;
    private final CobolPreprocessor.CobolSourceFormatEnum sourceFormat;

    public CobolParser(CobolParserVisitor.CobolDialect cobolDialect,
                       CobolDialect preprocessorDialect,
                       CobolPreprocessor.CobolSourceFormatEnum sourceFormat) {

        this.cobolDialect = cobolDialect;
        this.preprocessorDialect = preprocessorDialect;
        this.sourceFormat = sourceFormat;
    }

    @Override
    public List<Cobol.CompilationUnit> parseInputs(Iterable<Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
        ParsingEventListener parsingListener = ParsingExecutionContextView.view(ctx).getParsingListener();
        return acceptedInputs(sourceFiles).stream()
                .map(sourceFile -> {
                    Timer.Builder timer = Timer.builder("rewrite.parse")
                            .description("The time spent parsing a COBOL file")
                            .tag("file.type", "COBOL");
                    Timer.Sample sample = Timer.start();
                    try {
                        EncodingDetectingInputStream is = sourceFile.getSource();
                        String sourceStr = is.readFully();

                        String processedCobol = preprocessCobol(sourceStr, is.getCharset());
                        org.openrewrite.cobol.internal.grammar.CobolParser parser =
                                new org.openrewrite.cobol.internal.grammar.CobolParser(
                                        new CommonTokenStream(new CobolLexer(CharStreams.fromString(processedCobol))));

                        Cobol.CompilationUnit compilationUnit = new CobolParserVisitor(
                                sourceFile.getRelativePath(relativeTo),
                                sourceFile.getFileAttributes(),
                                sourceStr,
                                is.getCharset(),
                                is.isCharsetBomMarked(),
                                cobolDialect
                        ).visitStartRule(parser.startRule());

                        sample.stop(MetricsHelper.successTags(timer).register(Metrics.globalRegistry));
                        parsingListener.parsed(sourceFile, compilationUnit);
                        return compilationUnit;
                    } catch (Throwable t) {
                        sample.stop(MetricsHelper.errorTags(timer, t).register(Metrics.globalRegistry));
                        ctx.getOnError().accept(new IllegalStateException(sourceFile.getPath() + " " + t.getMessage(), t));
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(toList());
    }

    private String preprocessCobol(String source, Charset encoding) {
        CobolPreprocessor cobolPreprocessor = new CobolPreprocessor(
                new CobolCommentEntriesMarker(),
                new CobolDocumentParser(),
                new CobolInlineCommentEntriesNormalizer(),
                new CobolLineIndicatorProcessor(),
                new CobolLineReader()
        );

        // TODO: set dialect and format through configuration.
        CobolParserParams params = new CobolParserParams(
                encoding,
                emptyList(),
                getCobolFileExtensions(),
                getCopyBooks(),
                preprocessorDialect,
                sourceFormat,
                true
        );

        return cobolPreprocessor.process(source, params);
    }

    /**
     * There may be A LOT of copy books in a COBOL codebase, but we do not know how the parser is provided the copy books.
     * We also do not know how they are detected or what types of conventions exist.
     *
     * Temporarily hardcoded to implement COPY/REPLACE, but this SHOULD be auto-detected / configurable.
     * A temp ADHOC solution may be an auto-detected style that contains all the relevant info.
     */
    private List<File> getCopyBooks() {
        String userDir = System.getProperty("user.dir");
        String copyBooks = "/src/test/resources/gov/nist/copybooks";
        return Arrays.stream(Objects.requireNonNull(Paths.get(userDir + copyBooks).toFile().listFiles())).collect(toList());
    }

    private List<String> getCobolFileExtensions() {
        return singletonList("CPY");
    }

    @Override
    public List<Cobol.CompilationUnit> parse(String... sources) {
        return parse(new InMemoryExecutionContext(), sources);
    }

    @Override
    public boolean accept(Path path) {
        String s = path.toString().toLowerCase();
        for (String COBOL_FILE_EXTENSION : COBOL_FILE_EXTENSIONS) {
            if (s.endsWith(COBOL_FILE_EXTENSION)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public Path sourcePathFromSourceText(Path prefix, String sourceCode) {
        return prefix.resolve("file.CBL");
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
            ctx.getOnError().accept(new CobolParsingException(sourcePath,
                    String.format("Syntax error in %s at line %d:%d %s.", sourcePath, line, charPositionInLine, msg), e));
        }
    }
}
