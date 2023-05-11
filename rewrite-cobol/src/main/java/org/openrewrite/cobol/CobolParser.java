/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;
import org.antlr.v4.runtime.*;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Parser;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.internal.CobolParserVisitor;
import org.openrewrite.cobol.internal.CobolPreprocessorOutputSourcePrinter;
import org.openrewrite.cobol.internal.grammar.CobolLexer;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.cobol.tree.CobolSourceFile;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

public class CobolParser implements Parser<CobolSourceFile> {
    public static final List<String> COPYBOOK_FILE_EXTENSIONS = Collections.singletonList(".cpy");
    public static final List<String> COBOL_FILE_EXTENSIONS = singletonList(".cbl");

    private final CobolDialect cobolDialect;
    private final List<CobolPreprocessor.CopyBook> copyBooks;
    private final boolean enableCopy;
    private final boolean enableReplace;

    public CobolParser(CobolDialect cobolDialect,
                       List<CobolPreprocessor.CopyBook> copyBooks,
                       boolean enableCopy,
                       boolean enableReplace) {
        this.cobolDialect = cobolDialect;
        this.copyBooks = copyBooks;
        this.enableCopy = enableCopy;
        this.enableReplace = enableReplace;
    }

    @Override
    public Stream<CobolSourceFile> parseInputs(Iterable<Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
        ParsingExecutionContextView pctx = ParsingExecutionContextView.view(ctx);
        ParsingEventListener parsingListener = pctx.getParsingListener();
        List<Input> accepted = acceptedInputs(sourceFiles);
        List<Input> copyBookInputs = new ArrayList<>();
        List<Input> cobolInputs = new ArrayList<>();

        for (Input input : accepted) {
            for (String cobolFileExtension : COBOL_FILE_EXTENSIONS) {
                if (input.getPath().getFileName().toString().toLowerCase().endsWith(cobolFileExtension)) {
                    cobolInputs.add(input);
                }
            }

            for (String copyBookFileExtension : COPYBOOK_FILE_EXTENSIONS) {
                if (input.getPath().getFileName().toString().toLowerCase().endsWith(copyBookFileExtension)) {
                    copyBookInputs.add(input);
                }
            }
        }

        CopyBookParser copyBookParser = new CopyBookParser(cobolDialect);
        List<CobolPreprocessor.CopyBook> copyBooks = copyBookParser.parseInputs(copyBookInputs, relativeTo, ctx).collect(toList());

        CobolPreprocessorParser cobolPreprocessorParser = CobolPreprocessorParser.builder()
                .setCobolDialect(cobolDialect)
                .setEnableCopy(enableCopy)
                .setEnableReplace(enableReplace)
                .build();
        cobolPreprocessorParser.setCopyBooks(!this.copyBooks.isEmpty() ? this.copyBooks : copyBooks);

        Stream<CobolSourceFile> sources;
        sources = cobolInputs.stream()
                .map(sourceFile -> {
                    Timer.Builder timer = Timer.builder("rewrite.parse")
                            .description("The time spent parsing a COBOL file")
                            .tag("file.type", "COBOL");
                    Timer.Sample sample = Timer.start();
                    try {
                        EncodingDetectingInputStream is = sourceFile.getSource(ctx);

                        CobolPreprocessor.CompilationUnit preprocessedCU = cobolPreprocessorParser.parseInputs(singletonList(sourceFile), relativeTo, ctx).findFirst().get();

                        // Print processed code to parse COBOL.
                        PrintOutputCapture<ExecutionContext> cobolParserOutput = new PrintOutputCapture<>(new InMemoryExecutionContext());
                        CobolPreprocessorOutputSourcePrinter<ExecutionContext> printWithoutColumns = new CobolPreprocessorOutputSourcePrinter<>(cobolDialect, false);
                        printWithoutColumns.visit(preprocessedCU, cobolParserOutput);

                        org.openrewrite.cobol.internal.grammar.CobolParser parser =
                                new org.openrewrite.cobol.internal.grammar.CobolParser(
                                        new CommonTokenStream(new CobolLexer(CharStreams.fromString(cobolParserOutput.getOut()))));

                        // Print the pre-processed code to parse COBOL.
                        PrintOutputCapture<ExecutionContext> sourceOutput = new PrintOutputCapture<>(new InMemoryExecutionContext());
                        CobolPreprocessorOutputSourcePrinter<ExecutionContext> printWithColumns = new CobolPreprocessorOutputSourcePrinter<>(cobolDialect, true);
                        printWithColumns.visit(preprocessedCU, sourceOutput);

                        Cobol.CompilationUnit compilationUnit = new CobolParserVisitor(
                                sourceFile.getRelativePath(relativeTo),
                                sourceFile.getFileAttributes(),
                                sourceOutput.getOut(),
                                is.getCharset(),
                                is.isCharsetBomMarked(),
                                cobolDialect,
                                cobolPreprocessorParser.getCopyStatements(preprocessedCU),
                                cobolPreprocessorParser.getReplaceByStatements(preprocessedCU),
                                cobolPreprocessorParser.getReplaceOffStatements(preprocessedCU),
                                cobolPreprocessorParser.getReplaces(preprocessedCU),
                                cobolPreprocessorParser.getReplaceAdditiveTypes(preprocessedCU),
                                cobolPreprocessorParser.getReplaceReductiveTypes(preprocessedCU)
                        ).visitStartRule(parser.startRule());

                        sample.stop(MetricsHelper.successTags(timer).register(Metrics.globalRegistry));
                        parsingListener.parsed(sourceFile, compilationUnit);
                        return (CobolSourceFile) compilationUnit;
                    } catch (Throwable t) {
                        sample.stop(MetricsHelper.errorTags(timer, t).register(Metrics.globalRegistry));
                        pctx.parseFailure(sourceFile, relativeTo, this, t);
                        pctx.getOnError().accept(t);
                        return null;
                    }
                })
                .filter(Objects::nonNull);

        return Stream.concat(sources, copyBooks.stream());
    }

    @Override
    public Stream<CobolSourceFile> parse(String... sources) {
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
        for (String COPYBOOK_FILE_EXTENSION : COPYBOOK_FILE_EXTENSIONS) {
            if (s.endsWith(COPYBOOK_FILE_EXTENSION)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public Path sourcePathFromSourceText(Path prefix, String sourceCode) {
        return prefix.resolve("file.CBL");
    }

    public static CobolParser.Builder builder() {
        return new CobolParser.Builder();
    }

    public static class Builder extends org.openrewrite.Parser.Builder {

        private CobolDialect cobolDialect = CobolDialect.ibmAnsi85();
        private List<CobolPreprocessor.CopyBook> copyBooks = emptyList();
        private boolean enableCopy = true;
        private boolean enableReplace = true;

        public Builder() {
            super(Cobol.CompilationUnit.class);
        }

        @Override
        public CobolParser build() {
            return new CobolParser(
                    cobolDialect,
                    copyBooks,
                    enableCopy,
                    enableReplace);
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
            return "cobol";
        }
    }
}
