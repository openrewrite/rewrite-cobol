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
import java.util.List;
import java.util.Objects;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

public class CobolIbmAnsi85Parser implements CobolParser {

    // TODO: unify the dialect objects.
    private final CobolParserVisitor.CobolDialect cobolDialect = CobolParserVisitor.CobolDialect.IBM_ANSI_85;
    private final CobolDialect preprocessorDialect = CobolDialect.ANSI85;
    private final CobolPreprocessor.CobolSourceFormatEnum sourceFormat = CobolPreprocessor.CobolSourceFormatEnum.FIXED;

    // TODO: discovery ... there are a lot of unknowns about copy books. There may be MANY of them so some form of
    // auto-detection is ideal. Short-term, this will likely become a "Style" for simple auto-configuration.

    @Nullable
    private List<File> copyBookDirectories;

    @Nullable
    private List<String> copyBookExtensions;

    @Nullable
    private List<File> copyBookFiles;

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

    /**
     * TODO: ... explain
     */
    private String preprocessCobol(String source, Charset encoding) {
        CobolPreprocessor cobolPreprocessor = new CobolPreprocessor(
                new CobolCommentEntriesMarker(),
                new CobolDocumentParser(),
                new CobolInlineCommentEntriesNormalizer(),
                new CobolLineIndicatorProcessor(),
                new CobolLineReader()
        );

        CobolParserParams params = new CobolParserParams(
                encoding,
                emptyList(),
                emptyList(),
                emptyList(),
                preprocessorDialect,
                sourceFormat,
                true
        );

        return cobolPreprocessor.process(source, params);
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
