/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol;

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.SourceFile;
import org.openrewrite.cobol.internal.CobolPrinter;
import org.openrewrite.cobol.internal.IbmAnsi85;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.cobol.tree.Space;
import org.openrewrite.internal.StringUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;

public class Assertions {
    private Assertions() {
    }

    static void customizeExecutionContext(ExecutionContext ctx) {
    }

    public static SourceSpecs preprocessor(@Nullable String before) {
        return preprocessor(before, false);
    }

    public static SourceSpecs preprocessor(@Nullable String before, boolean enableCopyAndReplace) {
        return preprocessor(before, s -> {}, enableCopyAndReplace);
    }

    public static SourceSpecs preprocessor(@Nullable String before, Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        return preprocessor(before, spec, false);
    }

    public static SourceSpecs preprocessor(@Nullable String before, Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec, boolean enableCopyAndReplace) {
        CobolPreprocessorParser.Builder builder;
        if (enableCopyAndReplace) {
            List<SourceFile> copyBooks = getCopyBookSources();
            builder = CobolPreprocessorParser.builder()
                    .setCopyBooks(copyBooks);
        } else {
            builder = CobolPreprocessorParser.builder()
                    .setEnableCopy(false)
                    .setEnableReplace(false);
        }

        SourceSpec<CobolPreprocessor.CompilationUnit> cobol = new SourceSpec<>(
                CobolPreprocessor.CompilationUnit.class, null,
                builder,
                before,
                SourceSpec.EachResult.noop,
                Assertions::customizeExecutionContext);
        acceptPreprocessorSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs preprocessor(@Nullable String before, @Nullable String after) {
        return preprocessor(before, after, false);
    }

    public static SourceSpecs preprocessor(@Nullable String before, @Nullable String after, boolean enableCopyAndReplace) {
        return preprocessor(before, after, s -> {}, enableCopyAndReplace);
    }

    public static SourceSpecs preprocessor(@Nullable String before, @Nullable String after,
                                           Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        return preprocessor(before, after, spec, false);
    }

    public static SourceSpecs preprocessor(@Nullable String before, @Nullable String after,
                                           Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec, boolean enableCopyAndReplace) {
        CobolPreprocessorParser.Builder builder;
        if (enableCopyAndReplace) {
            List<SourceFile> copyBooks = getCopyBookSources();
            builder = CobolPreprocessorParser.builder()
                    .setCopyBooks(copyBooks);
        } else {
            builder = CobolPreprocessorParser.builder()
                    .setEnableCopy(false)
                    .setEnableReplace(false);
        }

        SourceSpec<CobolPreprocessor.CompilationUnit> cobol = new SourceSpec<>(CobolPreprocessor.CompilationUnit.class, null,
                builder,
                before,
                SourceSpec.EachResult.noop,
                Assertions::customizeExecutionContext).after(s -> after);
        acceptPreprocessorSpec(spec, cobol);
        return cobol;
    }

    private static void acceptPreprocessorSpec(Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec, SourceSpec<CobolPreprocessor.CompilationUnit> cobol) {
        Consumer<CobolPreprocessor.CompilationUnit> userSuppliedAfterRecipe = cobol.getAfterRecipe();
        cobol.afterRecipe(userSuppliedAfterRecipe::accept);
        isPreprocessorFullyParsed().andThen(spec).accept(cobol);
    }

    public static Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> isPreprocessorFullyParsed() {
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

    public static SourceSpecs cobol(@Nullable String before) {
        return cobol(before, false);
    }

    public static SourceSpecs cobol(@Nullable String before, boolean enableCopyAndReplace) {
        return cobol(before, s -> {
        }, enableCopyAndReplace);
    }

    public static SourceSpecs cobol(@Nullable String before, Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        return cobol(before, spec, false);
    }

    public static SourceSpecs cobol(@Nullable String before, Consumer<SourceSpec<Cobol.CompilationUnit>> spec, boolean enableCopyAndReplace) {
        CobolParser.Builder builder;
        if (enableCopyAndReplace) {
            List<SourceFile> copyBooks = getCopyBookSources();
            builder = CobolParser.builder()
                    .setCopyBooks(copyBooks);
        } else {
            builder = CobolParser.builder()
                    .setEnableCopy(false)
                    .setEnableReplace(false);
        }

        SourceSpec<Cobol.CompilationUnit> cobol = new SourceSpec<>(
                Cobol.CompilationUnit.class, null,
                builder,
                before,
                SourceSpec.EachResult.noop,
                Assertions::customizeExecutionContext);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobol(@Nullable String before, @Nullable String after) {
        return cobol(before, after, false);
    }

    public static SourceSpecs cobol(@Nullable String before, @Nullable String after, boolean enableCopyAndReplace) {
        return cobol(before, after, s -> {
        }, enableCopyAndReplace);
    }

    public static SourceSpecs cobol(@Nullable String before, @Nullable String after,
                                    Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        return cobol(before, after, spec, false);
    }

    public static SourceSpecs cobol(@Nullable String before, @Nullable String after,
                                    Consumer<SourceSpec<Cobol.CompilationUnit>> spec, boolean enableCopyAndReplace) {
        CobolParser.Builder builder;
        if (enableCopyAndReplace) {
            List<SourceFile> copyBooks = getCopyBookSources();
            builder = CobolParser.builder()
                    .setCopyBooks(copyBooks);
        } else {
            builder = CobolParser.builder()
                    .setEnableCopy(false)
                    .setEnableReplace(false);
        }

        SourceSpec<Cobol.CompilationUnit> cobol =
                new SourceSpec<>(Cobol.CompilationUnit.class,
                        null,
                        builder,
                        before,
                        SourceSpec.EachResult.noop,
                        Assertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, cobol);
        return cobol;
    }

    public static SourceSpecs cobolPostProcess(@Nullable String before, @Nullable String after) {
        return cobolPostProcess(before, after, false);
    }

    public static SourceSpecs cobolPostProcess(@Nullable String before, @Nullable String after, boolean enableCopyAndReplace) {
        return cobolPostProcess(before, after, s -> {
        }, enableCopyAndReplace);
    }

    public static SourceSpecs cobolPostProcess(@Nullable String before, @Nullable String after,
                                    Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        return cobolPostProcess(before, after, spec, false);
    }

    public static SourceSpecs cobolPostProcess(@Nullable String before, @Nullable String after,
                                    Consumer<SourceSpec<Cobol.CompilationUnit>> spec, boolean enableCopyAndReplace) {
        CobolParser.Builder builder;
        if (enableCopyAndReplace) {
            List<SourceFile> copyBooks = getCopyBookSources();
            builder = CobolParser.builder()
                    .setCopyBooks(copyBooks);
        } else {
            builder = CobolParser.builder()
                    .setEnableCopy(false)
                    .setEnableReplace(false);
        }

        SourceSpec<Cobol.CompilationUnit> cobol = new SourceSpec<>(
                Cobol.CompilationUnit.class, null,
                builder,
                before,
                SourceSpec.EachResult.noop,
                Assertions::customizeExecutionContext);
        isFullyParsed().andThen(isPostProcessedLst(after)).andThen(spec).accept(cobol);
        return cobol;
    }

    private static void acceptSpec(Consumer<SourceSpec<Cobol.CompilationUnit>> spec, SourceSpec<Cobol.CompilationUnit> cobol) {
        Consumer<Cobol.CompilationUnit> userSuppliedAfterRecipe = cobol.getAfterRecipe();
        cobol.afterRecipe(userSuppliedAfterRecipe::accept);
        isFullyParsed().andThen(spec).accept(cobol);
    }

    public static Consumer<SourceSpec<Cobol.CompilationUnit>> isFullyParsed() {
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

    public static Consumer<SourceSpec<Cobol.CompilationUnit>> isPostProcessedLst(@Nullable String expectedLst) {
        return spec -> spec.afterRecipe(cu -> {
            CobolPrinter<ExecutionContext> printer = new CobolPrinter<>(false, false);
            PrintOutputCapture<ExecutionContext> outputCapture = new PrintOutputCapture<>(new InMemoryExecutionContext());
            printer.visit(cu, outputCapture);
            assert trimTrailingSpaces(StringUtils.trimIndentPreserveCRLF(outputCapture.getOut())).equals(expectedLst);
        });
    }

    public static String trimTrailingSpaces(String input) {
        StringBuilder result = new StringBuilder();
        String[] lines = input.split("\\r?\\n");
        for (String line : lines) {
            String trimmedLine = line.replaceAll("\\s+$", "");
            result.append(trimmedLine).append("\n");
        }
        return result.toString();
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
