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
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.internal.CobolPreprocessorParserVisitor;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorLexer;
import org.openrewrite.cobol.markers.Replace;
import org.openrewrite.cobol.markers.ReplaceAdditiveType;
import org.openrewrite.cobol.markers.ReplaceReductiveType;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;
import org.openrewrite.text.PlainText;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.*;
import java.util.*;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static org.openrewrite.Tree.randomId;

/**
 * Read preprocessed COBOL and execute preprocessor commands.
 */
public class CobolPreprocessorParser implements Parser<CobolPreprocessor.CompilationUnit> {
    public static final List<String> COPYBOOK_FILE_EXTENSIONS = Collections.singletonList(".cpy");
    public static final List<String> COBOL_FILE_EXTENSIONS = Collections.singletonList(".cbl");

    private final CobolDialect cobolDialect;
    private final List<CobolPreprocessor.CopyBook> copyBooks;
    private final boolean enableCopy;
    private final boolean enableReplace;

    // Lazily loaded sets of the original source objects.
    private Set<CobolPreprocessor.CopyStatement> copyStatements = null;
    private Set<CobolPreprocessor.ReplaceByStatement> replaceRules = null;
    private Set<CobolPreprocessor.ReplaceOffStatement> replaceOffs = null;
    private Set<Replace> replaces = null;
    private Set<ReplaceAdditiveType> replaceAdditiveTypes = null;
    private Set<ReplaceReductiveType> replaceReductiveTypes = null;

    public CobolPreprocessorParser(CobolDialect cobolDialect,
                                   List<CobolPreprocessor.CopyBook> copyBooks,
                                   boolean enableCopy,
                                   boolean enableReplace) {
        this.cobolDialect = cobolDialect;
        this.copyBooks = copyBooks;
        this.enableCopy = enableCopy;
        this.enableReplace = enableReplace;
    }

    @Override
    public List<CobolPreprocessor.CompilationUnit> parseInputs(Iterable<org.openrewrite.Parser.Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
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

                        if (enableCopy) {
                            PreprocessCopyVisitor<ExecutionContext> copyPhase = new PreprocessCopyVisitor<>(copyBooks);

                            // CU after copy includes the copied source.
                            preprocessedCU = (CobolPreprocessor.CompilationUnit) copyPhase.visit(preprocessedCU, new InMemoryExecutionContext());
                        }

                        if (enableReplace) {
                            PreprocessReplaceVisitor<ExecutionContext> replacePhase = new PreprocessReplaceVisitor<>();

                            // CU after replace has replaced words in each ReplaceArea based on the ReplaceClause.
                            preprocessedCU = (CobolPreprocessor.CompilationUnit) replacePhase.visit(preprocessedCU, new InMemoryExecutionContext());
                        }

                        assert preprocessedCU != null;

                        sample.stop(MetricsHelper.successTags(timer).register(Metrics.globalRegistry));
                        parsingListener.parsed(sourceFile, preprocessedCU);
                        return preprocessedCU;
                    } catch (Throwable t) {
                        sample.stop(MetricsHelper.errorTags(timer, t).register(Metrics.globalRegistry));
                        ctx.getOnError().accept(new IllegalStateException(sourceFile.getPath() + " " + t.getMessage(), t));
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(toList());
    }

    public static List<CobolPreprocessor.CopyBook> parseCopyBooks(
            List<Parser.Input> inputs,
            CobolDialect cobolDialect,
            ExecutionContext ctx) {
        List<CobolPreprocessor.CopyBook> copyBooks = new ArrayList<>();
        for(Parser.Input input : inputs) {
            PlainText plainText;
            try(EncodingDetectingInputStream is = input.getSource(ctx)) {
                plainText = new PlainText(
                        randomId(),
                        input.getPath(),
                        Markers.EMPTY,
                        is.getCharset().name(),
                        is.isCharsetBomMarked(),
                        null,
                        null,
                        is.readFully()
                );
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

            String preprocessedCobol = new CobolLineReader().readLines(plainText.getText(), cobolDialect);
            org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser parser =
                    new org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser(
                            new CommonTokenStream(new CobolPreprocessorLexer(CharStreams.fromString(preprocessedCobol))));

            //noinspection ConstantConditions
            CobolPreprocessorParserVisitor parserVisitor = new CobolPreprocessorParserVisitor(
                    plainText.getSourcePath(),
                    plainText.getFileAttributes(),
                    plainText.getText(),
                    plainText.getCharset(),
                    plainText.isCharsetBomMarked(),
                    cobolDialect
            );

            CobolPreprocessor.CompilationUnit cu = parserVisitor.visitStartRule(parser.startRule());
            CobolPreprocessor parsedCopySource = cu.getCobols().get(0);

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
                    cu.getEof()
            );

            copyBooks.add(copyBook);
        }

        return copyBooks;
    }

    public static List<CobolPreprocessor.CopyBook> parseCopyBooks(
            List<Path> paths,
            @Nullable Path relativeTo,
            CobolDialect cobolDialect,
            ExecutionContext ctx) {
        List<Parser.Input> inputs = new ArrayList<>(paths.size());

        for (Path path : paths) {
            inputs.add(new Parser.Input(
                    relativeTo == null ? path : relativeTo.relativize(path),
                    () -> {
                        try {
                            return Files.newInputStream(path);
                        } catch (IOException e) {
                            throw new UncheckedIOException(e);
                        }
                    }
            ));
        }

        return parseCopyBooks(inputs, cobolDialect, ctx);
    }

    @Override
    public List<CobolPreprocessor.CompilationUnit> parse(String... sources) {
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
    public Parser<CobolPreprocessor.CompilationUnit> reset() {
        this.copyStatements = null;
        this.replaceRules = null;
        this.replaceOffs = null;
        this.replaces = null;
        this.replaceAdditiveTypes = null;
        this.replaceReductiveTypes = null;
        return this;
    }

    @Override
    public Path sourcePathFromSourceText(Path prefix, String sourceCode) {
        return prefix.resolve("file.CBL");
    }

    public static CobolPreprocessorParser.Builder builder() {
        return new CobolPreprocessorParser.Builder();
    }

    public static class Builder extends org.openrewrite.Parser.Builder {

        private CobolDialect cobolDialect = CobolDialect.ibmAnsi85();
        private List<CobolPreprocessor.CopyBook> copyBooks = emptyList();
        private boolean enableCopy;
        private boolean enableReplace;

        public Builder() {
            super(Cobol.CompilationUnit.class);
        }

        @Override
        public CobolPreprocessorParser build() {
            return new CobolPreprocessorParser(
                    cobolDialect,
                    copyBooks,
                    enableCopy,
                    enableReplace
            );
        }

        public Builder setCobolDialect(CobolDialect cobolDialect) {
            this.cobolDialect = cobolDialect;
            return this;
        }

        public Builder setCopyBooks(List<CobolPreprocessor.CopyBook> copyBooks) {
            this.copyBooks = copyBooks;
            return this;
        }

        public Builder setEnableCopy(boolean enableCopy) {
            this.enableCopy = enableCopy;
            return this;
        }

        public Builder setEnableReplace(boolean enableReplace) {
            this.enableReplace = enableReplace;
            return this;
        }

        @Override
        public String getDslName() {
            return "preprocessCobol";
        }
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

    public Set<CobolPreprocessor.CopyStatement> getCopyStatements(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (copyStatements == null) {
            getOriginalSources(cu);
        }
        return copyStatements;
    }

    public Set<CobolPreprocessor.ReplaceByStatement> getReplaceByStatements(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaceRules == null) {
            getOriginalSources(cu);
        }
        return replaceRules;
    }

    public Set<CobolPreprocessor.ReplaceOffStatement> getReplaceOffStatements(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaceOffs == null) {
            getOriginalSources(cu);
        }
        return replaceOffs;
    }

    public Set<Replace> getReplaces(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaces == null) {
            getOriginalSources(cu);
        }
        return replaces;
    }

    public Set<ReplaceAdditiveType> getReplaceAdditiveTypes(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaceAdditiveTypes == null) {
            getOriginalSources(cu);
        }
        return replaceAdditiveTypes;
    }

    public Set<ReplaceReductiveType> getReplaceReductiveTypes(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaceReductiveTypes == null) {
            getOriginalSources(cu);
        }
        return replaceReductiveTypes;
    }

    public void getOriginalSources(@Nullable CobolPreprocessor.CompilationUnit cu) {
        this.copyStatements = new HashSet<>();
        this.replaceRules = new HashSet<>();
        this.replaceOffs = new HashSet<>();
        this.replaces = new HashSet<>();
        this.replaceAdditiveTypes = new HashSet<>();
        this.replaceReductiveTypes = new HashSet<>();

        CobolPreprocessorIsoVisitor<ExecutionContext> visitor = new CobolPreprocessorIsoVisitor<ExecutionContext>() {
            @Override
            public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement,
                                                                      ExecutionContext executionContext) {
                copyStatements.add(copyStatement);
                return super.visitCopyStatement(copyStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.ReplaceByStatement visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement,
                                                                                ExecutionContext executionContext) {
                replaceRules.add(replaceByStatement);
                return super.visitReplaceByStatement(replaceByStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.ReplaceOffStatement visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement,
                                                                                  ExecutionContext executionContext) {
                replaceOffs.add(replaceOffStatement);
                return super.visitReplaceOffStatement(replaceOffStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
                Optional<Replace> replace = word.getMarkers().findFirst(Replace.class);
                replace.ifPresent(it -> replaces.add(it));

                Optional<ReplaceAdditiveType> replaceAdditiveType = word.getMarkers().findFirst(ReplaceAdditiveType.class);
                replaceAdditiveType.ifPresent(it -> replaceAdditiveTypes.add(it));

                Optional<ReplaceReductiveType> replaceReductiveType = word.getMarkers().findFirst(ReplaceReductiveType.class);
                replaceReductiveType.ifPresent(it -> replaceReductiveTypes.add(it));

                return super.visitWord(word, executionContext);
            }
        };

        visitor.visit(cu, new InMemoryExecutionContext());

    }
}
