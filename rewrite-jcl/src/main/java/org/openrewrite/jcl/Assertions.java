/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.jcl;

import org.openrewrite.ExecutionContext;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.util.function.Consumer;

public class Assertions {
    private Assertions() {
    }

    static void customizeExecutionContext(ExecutionContext ctx) {
    }

    public static SourceSpecs jcl(@Nullable String before) {
        return jcl(before, s -> {
        });
    }

    public static SourceSpecs jcl(@Nullable String before, Consumer<SourceSpec<Jcl.CompilationUnit>> spec) {
        SourceSpec<Jcl.CompilationUnit> jcl = new SourceSpec<>(
                Jcl.CompilationUnit.class, null, JclParser.builder(), before,
                SourceSpec.EachResult.noop,
                Assertions::customizeExecutionContext);
        acceptSpec(spec, jcl);
        return jcl;
    }

    public static SourceSpecs jcl(@Nullable String before, @Nullable String after) {
        return jcl(before, after, s -> {
        });
    }

    public static SourceSpecs jcl(@Nullable String before, @Nullable String after,
                                    Consumer<SourceSpec<Jcl.CompilationUnit>> spec) {
        SourceSpec<Jcl.CompilationUnit> jcl = new SourceSpec<>(
                Jcl.CompilationUnit.class, null, JclParser.builder(), before,
                        SourceSpec.EachResult.noop,
                        Assertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, jcl);
        return jcl;
    }

    private static void acceptSpec(Consumer<SourceSpec<Jcl.CompilationUnit>> spec, SourceSpec<Jcl.CompilationUnit> kotlin) {
        Consumer<Jcl.CompilationUnit> userSuppliedAfterRecipe = kotlin.getAfterRecipe();
        kotlin.afterRecipe(userSuppliedAfterRecipe::accept);
        spec.accept(kotlin);
    }
}
