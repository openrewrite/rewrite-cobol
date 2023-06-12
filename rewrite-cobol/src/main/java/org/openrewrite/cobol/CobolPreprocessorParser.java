/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.openrewrite.*;
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.internal.CobolPreprocessorParserVisitor;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorLexer;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.cobol.tree.Replacement;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

import static java.util.Collections.emptyList;

/**
 * Read preprocessed COBOL and execute preprocessor commands.
 */
public class CobolPreprocessorParser implements Parser {
    public static final List<String> COPYBOOK_FILE_EXTENSIONS = Collections.singletonList(".cpy");
    private static final List<String> COBOL_FILE_EXTENSIONS = Collections.singletonList(".cbl");

    private final CobolDialect cobolDialect;
    private final boolean enableCopy;
    private final boolean enableReplace;

    private List<SourceFile> copyBooks;

    // Lazily loaded sets of the original source objects.
    private Set<CobolPreprocessor.CopyStatement> copyStatements = null;
    private Set<CobolPreprocessor.ReplaceByStatement> replaceRules = null;
    private Set<CobolPreprocessor.ReplaceOffStatement> replaceOffs = null;
    private Set<Replacement> replaces = null;
    private Set<Replacement> replaceAdditiveTypes = null;
    private Set<Replacement> replaceReductiveTypes = null;

    public CobolPreprocessorParser(CobolDialect cobolDialect,
                                   List<SourceFile> copyBooks,
                                   boolean enableCopy,
                                   boolean enableReplace) {
        this.cobolDialect = cobolDialect;
        this.copyBooks = copyBooks;
        this.enableCopy = enableCopy;
        this.enableReplace = enableReplace;
    }

    @Override
    public Stream<SourceFile> parseInputs(Iterable<org.openrewrite.Parser.Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
        ParsingExecutionContextView pctx = ParsingExecutionContextView.view(ctx);
        ParsingEventListener parsingListener = pctx.getParsingListener();
        return acceptedInputs(sourceFiles)
                .map(sourceFile -> {
                    Timer.Builder timer = Timer.builder("rewrite.parse")
                            .description("The time spent parsing a COBOL file")
                            .tag("file.type", "COBOL");
                    Timer.Sample sample = Timer.start();
                    try {
                        EncodingDetectingInputStream is = sourceFile.getSource(ctx);
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
                        ctx.getOnError().accept(t);
                        return ParseError.build(this, sourceFile, relativeTo, ctx, t);
                    }
                })
                .filter(Objects::nonNull);
    }

    public void setCopyBooks(List<SourceFile> copyBooks) {
        this.copyBooks = copyBooks;
    }

    @Override
    public Stream<SourceFile> parse(String... sources) {
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
    public Parser reset() {
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
        private List<SourceFile> copyBooks = emptyList();
        private boolean enableCopy = true;
        private boolean enableReplace = true;

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

        public Builder setCopyBooks(List<SourceFile> copyBooks) {
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

    public Set<Replacement> getReplaces(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaces == null) {
            getOriginalSources(cu);
        }
        return replaces;
    }

    public Set<Replacement> getReplaceAdditiveTypes(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaceAdditiveTypes == null) {
            getOriginalSources(cu);
        }
        return replaceAdditiveTypes;
    }

    public Set<Replacement> getReplaceReductiveTypes(@Nullable CobolPreprocessor.CompilationUnit cu) {
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
                Replacement replacement = word.getCobolWord().getReplacement();
                if (replacement != null) {
                    switch (replacement.getType()) {
                        case EQUAL:
                            replaces.add(replacement);
                            break;
                        case ADDITIVE:
                            replaceAdditiveTypes.add(replacement);
                            break;
                        case REDUCTIVE:
                            replaceReductiveTypes.add(replacement);
                            break;
                    }
                }

                return super.visitWord(word, executionContext);
            }
        };

        visitor.visit(cu, new InMemoryExecutionContext());

    }
}
