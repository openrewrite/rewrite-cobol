/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree;

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.*;
import org.openrewrite.cobol.internal.CobolPrinter;
import org.openrewrite.cobol.internal.IbmAnsi85;
import org.openrewrite.internal.StringUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Consumer;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;

public class ParserAssertions {
    private ParserAssertions() {
    }

    static void customizeExecutionContext(ExecutionContext ctx) {
    }

    public static SourceSpecs cobol(@Nullable String before) {
        return cobol(before, s -> {
        });
    }

    public static SourceSpecs cobol(@Nullable String before, Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        SourceSpec<Cobol.CompilationUnit> cobol = new SourceSpec<>(
                Cobol.CompilationUnit.class, null,
                CobolParser.builder()
                        .setEnableCopy(false)
                        .setEnableReplace(false),
                before,
                SourceSpec.EachResult.noop,
                ParserAssertions::customizeExecutionContext);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobol(@Nullable String before, @Nullable String after) {
        return cobol(before, after, s -> {
        });
    }

    public static SourceSpecs cobol(@Nullable String before, @Nullable String after,
                                    Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        SourceSpec<Cobol.CompilationUnit> cobol =
                new SourceSpec<>(Cobol.CompilationUnit.class,
                        null,
                        CobolParser.builder()
                                .setEnableCopy(false)
                                .setEnableReplace(false),
                        before,
                        SourceSpec.EachResult.noop,
                        ParserAssertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobolCopy(@Nullable String before) {
        return cobolCopy(before, s -> {
        });
    }

    public static SourceSpecs cobolCopy(@Nullable String before, Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        List<CobolPreprocessor.CopyBook> copyBooks = getCopyBookSources();

        SourceSpec<Cobol.CompilationUnit> cobol = new SourceSpec<>(
                Cobol.CompilationUnit.class, null,
                CobolParser.builder()
                        .setCopyBooks(copyBooks),
                before,
                SourceSpec.EachResult.noop,
                ParserAssertions::customizeExecutionContext);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobolCopy(@Nullable String before, String expectedLst) {
        return cobolCopy(before, expectedLst, s -> {
        });
    }

    public static SourceSpecs cobolCopy(@Nullable String before, String expectedLst,
                                    Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        List<CobolPreprocessor.CopyBook> copyBooks = getCopyBookSources();

        SourceSpec<Cobol.CompilationUnit> cobol = new SourceSpec<>(
                Cobol.CompilationUnit.class, null,
                CobolParser.builder()
                        .setCopyBooks(copyBooks),
                before,
                SourceSpec.EachResult.noop,
                ParserAssertions::customizeExecutionContext);
        spec
                .andThen(isFullyParsed())
                .andThen(isExpectedLst(expectedLst))
                .accept(cobol);
        return cobol;
    }

    public static List<CobolPreprocessor.CopyBook> getCopyBookSources() {
        ResourceParser resourceParser = new ResourceParser(Paths.get("").toAbsolutePath(), emptyList(), emptyList());

        try {
            List<Path> paths = resourceParser.getResourcesByExtension(emptyList(), singletonList(".cpy"));
            return CopyBookParser.builder().build().parse(paths, null, new InMemoryExecutionContext());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static void acceptSpec(Consumer<SourceSpec<Cobol.CompilationUnit>> spec, SourceSpec<Cobol.CompilationUnit> cobol) {
        Consumer<Cobol.CompilationUnit> userSuppliedAfterRecipe = cobol.getAfterRecipe();
        cobol.afterRecipe(userSuppliedAfterRecipe::accept);
        isFullyParsed().andThen(spec).accept(cobol);
    }

    public static Consumer<SourceSpec<Cobol.CompilationUnit>> isFullyParsed() {
        return spec -> spec.afterRecipe(cu -> new CobolIsoVisitor<Integer>() {
            @Override
            public Space visitSpace(Space space, Space.Location loc, Integer integer) {
                String whitespace = space.getWhitespace().trim();
                if (!(IbmAnsi85.getInstance().getSeparators().contains(whitespace + " ") || whitespace.isEmpty())) {
                    return space.withWhitespace("(~~>" + whitespace + "<~~)");
                }
                return super.visitSpace(space, loc, integer);
            }
        }.visit(cu, 0));
    }

    public static Consumer<SourceSpec<Cobol.CompilationUnit>> isExpectedLst(String expectedLst) {
        return spec -> spec.afterRecipe(cu -> {
            CobolPrinter<ExecutionContext> printer = new CobolPrinter<>(false, false);
            PrintOutputCapture<ExecutionContext> outputCapture = new PrintOutputCapture<>(new InMemoryExecutionContext());
            printer.visit(cu, outputCapture);
            assertThat(StringUtils.trimIndentPreserveCRLF(outputCapture.getOut())).isEqualTo(expectedLst);
        });
    }
}
