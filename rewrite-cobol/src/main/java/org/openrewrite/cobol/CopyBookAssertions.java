package org.openrewrite.cobol;

import org.openrewrite.ExecutionContext;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.util.function.Consumer;

public class CopyBookAssertions {
    private CopyBookAssertions() {
    }

    static void customizeExecutionContext(ExecutionContext ctx) {
    }

    public static SourceSpecs copyBook(@Nullable String before) {
        return copyBook(before, s -> {
        });
    }

    public static SourceSpecs copyBook(@Nullable String before, Consumer<SourceSpec<CobolPreprocessor.CopyBook>> spec) {
        SourceSpec<CobolPreprocessor.CopyBook> copyBook = new SourceSpec<>(CobolPreprocessor.CopyBook.class, null,
                CopyBookParser.builder(),
                before,
                SourceSpec.EachResult.noop,
                CopyBookAssertions::customizeExecutionContext);
        acceptSpec(spec, copyBook);
        return copyBook;
    }

    public static SourceSpecs copyBook(@Nullable String before, @Nullable String after) {
        return copyBook(before, after, s -> {
        });
    }

    public static SourceSpecs copyBook(@Nullable String before, @Nullable String after,
                                                    Consumer<SourceSpec<CobolPreprocessor.CopyBook>> spec) {
        SourceSpec<CobolPreprocessor.CopyBook> copyBook = new SourceSpec<>(CobolPreprocessor.CopyBook.class, null,
                CopyBookParser.builder(),
                before,
                SourceSpec.EachResult.noop,
                CopyBookAssertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, copyBook);
        return copyBook;
    }

    private static void acceptSpec(Consumer<SourceSpec<CobolPreprocessor.CopyBook>> spec, SourceSpec<CobolPreprocessor.CopyBook> cobol) {
        Consumer<CobolPreprocessor.CopyBook> userSuppliedAfterRecipe = cobol.getAfterRecipe();
        cobol.afterRecipe(userSuppliedAfterRecipe::accept);
        spec.accept(cobol);
    }
}
