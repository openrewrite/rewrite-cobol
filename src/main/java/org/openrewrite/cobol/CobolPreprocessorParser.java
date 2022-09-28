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
import org.openrewrite.cobol.internal.IbmAnsi85;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorLexer;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;
import org.openrewrite.text.PlainText;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static java.util.stream.Collectors.toList;
import static org.openrewrite.Tree.randomId;

/**
 * Read preprocessed COBOL and execute preprocessor commands.
 */
public class CobolPreprocessorParser implements Parser<CobolPreprocessor.CompilationUnit> {
    private static final List<String> COBOL_FILE_EXTENSIONS = Arrays.asList(".cbl", ".cpy");

    private final CobolDialect cobolDialect;
    private final boolean enableCopy;
    private final boolean enableReplace;

    // Lazily loaded maps of the original source objects.
    private Set<CobolPreprocessor.CopyStatement> copyStatements = null;
    private Set<CobolPreprocessor.ReplaceByStatement> replaceRules = null;
    private Set<CobolPreprocessor.ReplaceOffStatement> replaceOffs = null;
    private Set<Replace> replaces = null;
    private Set<ReplaceReductiveType> replaceReductiveTypes = null;

    public CobolPreprocessorParser(CobolDialect cobolDialect,
                                   boolean enableCopy,
                                   boolean enableReplace) {
        this.cobolDialect = cobolDialect;
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
                            List<Path> copyBookPaths = getCopyBooks();
                            List<CobolPreprocessor.CopyBook> copyBooks = parseCopyBooks(copyBookPaths, relativeTo);
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

    public List<CobolPreprocessor.CopyBook> parseCopyBooks(List<Path> paths, @Nullable Path relativeTo) {
        List<CobolPreprocessor.CopyBook> copyBooks = new ArrayList<>();

        for (Path path : paths) {
            PlainText plainText;
            try (InputStream inputStream = Files.newInputStream(path)) {
                EncodingDetectingInputStream is = new EncodingDetectingInputStream(inputStream);
                plainText = new PlainText(
                        randomId(),
                        path,
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
                    relativeTo == null ? plainText.getSourcePath() : plainText.getSourcePath().relativize(relativeTo),
                    plainText.getFileAttributes(),
                    plainText.getText(),
                    plainText.getCharset(),
                    plainText.isCharsetBomMarked(),
                    cobolDialect
            );

            CobolPreprocessor.CompilationUnit cu = parserVisitor.visitStartRule(parser.startRule());
            CobolPreprocessor parsedCopySource = cu.getCobols().get(0);

            // TODO: How should the SourceFile and CopyBook be linked together?
            CobolPreprocessor.CopyBook copyBook = new CobolPreprocessor.CopyBook(
                    randomId(),
                    Space.EMPTY,
                    Markers.EMPTY,
                    path,
                    parsedCopySource,
                    cu.getEof()
            );

            copyBooks.add(copyBook);
        }

        return copyBooks;
    }

    /**
     * There may be A LOT of copy books in a COBOL codebase, but we do not know how the parser is provided the copy books.
     * We also do not know how they are detected or what types of conventions exist.
     *
     * Temporarily hardcoded to implement COPY/REPLACE, but this SHOULD be auto-detected / configurable.
     * A temp ADHOC solution may be an auto-detected style that contains all the relevant info.
     */
    private List<Path> getCopyBooks() {
        String userDir = System.getProperty("user.dir");
        String copyBooks = "/src/test/resources/gov/nist/copybooks";
        return Arrays.stream(Objects.requireNonNull(Paths.get(userDir + copyBooks).toFile().listFiles()))
                .filter(it -> !it.isDirectory())
                .map(File::toPath)
                .collect(toList());
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

        CobolDialect cobolDialect;
        boolean enableCopy;
        boolean enableReplace;

        public Builder() {
            super(Cobol.CompilationUnit.class);
        }

        @Override
        public CobolPreprocessorParser build() {
            return new CobolPreprocessorParser(
                    cobolDialect == null ? new IbmAnsi85() : cobolDialect,
                    enableCopy,
                    enableReplace);
        }

        public Builder setCobolDialect(CobolDialect cobolDialect) {
            this.cobolDialect = cobolDialect;
            return this;
        }

        public Builder enableCopy() {
            this.enableCopy = true;
            return this;
        }

        public Builder enableReplace() {
            this.enableReplace = true;
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

    // TODO clean up.
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
        this.replaceReductiveTypes = new HashSet<>();

        CobolPreprocessorIsoVisitor<ExecutionContext> visitor = new CobolPreprocessorIsoVisitor<ExecutionContext>() {
            @Override
            public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement,
                                                                      ExecutionContext executionContext) {
                copyStatements.add(copyStatement);
                return super.visitCopyStatement(copyStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.ReplaceByStatement visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement, ExecutionContext executionContext) {
                replaceRules.add(replaceByStatement);
                return super.visitReplaceByStatement(replaceByStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.ReplaceOffStatement visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, ExecutionContext executionContext) {
                replaceOffs.add(replaceOffStatement);
                return super.visitReplaceOffStatement(replaceOffStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
                Optional<Replace> replace = word.getMarkers().findFirst(Replace.class);
                replace.ifPresent(it -> replaces.add(it));

                Optional<ReplaceReductiveType> replaceReductiveType = word.getMarkers().findFirst(ReplaceReductiveType.class);
                replaceReductiveType.ifPresent(it -> replaceReductiveTypes.add(it));

                return super.visitWord(word, executionContext);
            }
        };

        visitor.visit(cu, new InMemoryExecutionContext());

    }
}
