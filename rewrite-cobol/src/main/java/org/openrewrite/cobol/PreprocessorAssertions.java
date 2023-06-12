/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol;

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.SourceFile;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;

public class PreprocessorAssertions {
    private PreprocessorAssertions() {
    }

    static void customizeExecutionContext(ExecutionContext ctx) {
    }

    public static SourceSpecs cobolPreprocess(@Nullable String before) {
        return cobolPreprocess(before, s -> {
        });
    }

    public static SourceSpecs cobolPreprocess(@Nullable String before, Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        SourceSpec<CobolPreprocessor.CompilationUnit> cobol = new SourceSpec<>(
                CobolPreprocessor.CompilationUnit.class, null,
                CobolPreprocessorParser.builder()
                        .setEnableCopy(false)
                        .setEnableReplace(false),
                before,
                SourceSpec.EachResult.noop,
                PreprocessorAssertions::customizeExecutionContext);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocess(@Nullable String before, @Nullable String after) {
        return cobolPreprocess(before, after, s -> {
        });
    }

    public static SourceSpecs cobolPreprocess(@Nullable String before, @Nullable String after,
                                              Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        SourceSpec<CobolPreprocessor.CompilationUnit> cobol = new SourceSpec<>(CobolPreprocessor.CompilationUnit.class, null,
                CobolPreprocessorParser.builder()
                        .setEnableCopy(false)
                        .setEnableReplace(false),
                before,
                SourceSpec.EachResult.noop,
                PreprocessorAssertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before) {
        return cobolPreprocessorCopy(before, s -> {
        });
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        List<SourceFile> copyBooks = getCopyBookSources();

        SourceSpec<CobolPreprocessor.CompilationUnit> cobol = new SourceSpec<>(CobolPreprocessor.CompilationUnit.class, null,
                CobolPreprocessorParser.builder()
                        .setCopyBooks(copyBooks),
                before,
                SourceSpec.EachResult.noop,
                PreprocessorAssertions::customizeExecutionContext);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, @Nullable String after) {
        return cobolPreprocessorCopy(before, after, s -> {
        });
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, @Nullable String after,
                                                    Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        List<SourceFile> copyBooks = getCopyBookSources();

        SourceSpec<CobolPreprocessor.CompilationUnit> cobol = new SourceSpec<>(CobolPreprocessor.CompilationUnit.class, null,
                CobolPreprocessorParser.builder()
                        .setCopyBooks(copyBooks),
                before,
                SourceSpec.EachResult.noop,
                PreprocessorAssertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, cobol);
        return cobol;
    }

    private static void acceptSpec(Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec, SourceSpec<CobolPreprocessor.CompilationUnit> cobol) {
        Consumer<CobolPreprocessor.CompilationUnit> userSuppliedAfterRecipe = cobol.getAfterRecipe();
        cobol.afterRecipe(userSuppliedAfterRecipe::accept);
        spec.accept(cobol);
    }

    private static List<SourceFile> getCopyBookSources() {
        ResourceParser resourceParser = new ResourceParser(Paths.get("").toAbsolutePath(), emptyList(), emptyList());

        try {
            List<Path> paths = resourceParser.getResourcesByExtension(emptyList(), singletonList(".cpy"));
            return CopyBookParser.builder().build().parse(paths, null, new InMemoryExecutionContext()).collect(Collectors.toList());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
