/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree;

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.cobol.*;
import org.openrewrite.cobol.internal.IbmAnsi85;
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

public class PreprocessorParserAssertions {
    private PreprocessorParserAssertions() {
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
                PreprocessorParserAssertions::customizeExecutionContext);
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
                PreprocessorParserAssertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before) {
        return cobolPreprocessorCopy(before, s -> {
        });
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        List<CobolPreprocessor.CopyBook> copyBooks = getCopyBookSources();

        SourceSpec<CobolPreprocessor.CompilationUnit> cobol = new SourceSpec<>(CobolPreprocessor.CompilationUnit.class, null,
                CobolPreprocessorParser.builder()
                        .setCopyBooks(copyBooks),
                before,
                SourceSpec.EachResult.noop,
                PreprocessorParserAssertions::customizeExecutionContext);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, @Nullable String after) {
        return cobolPreprocessorCopy(before, after, s -> {
        });
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, @Nullable String after,
                                                    Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        List<CobolPreprocessor.CopyBook> copyBooks = getCopyBookSources();

        SourceSpec<CobolPreprocessor.CompilationUnit> cobol = new SourceSpec<>(CobolPreprocessor.CompilationUnit.class, null,
                CobolPreprocessorParser.builder()
                        .setCopyBooks(copyBooks),
                before,
                SourceSpec.EachResult.noop,
                PreprocessorParserAssertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, cobol);
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

    private static void acceptSpec(Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec, SourceSpec<CobolPreprocessor.CompilationUnit> cobol) {
        Consumer<CobolPreprocessor.CompilationUnit> userSuppliedAfterRecipe = cobol.getAfterRecipe();
        cobol.afterRecipe(userSuppliedAfterRecipe::accept);
        isFullyParsed().andThen(spec).accept(cobol);
    }

    public static Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> isFullyParsed() {
        return spec -> spec.afterRecipe(cu -> new CobolPreprocessorIsoVisitor<Integer>() {
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
}
