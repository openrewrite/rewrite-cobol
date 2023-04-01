package org.openrewrite.jcl;

import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.util.function.Consumer;

public class Assertions {
    private Assertions() {
    }

    public static SourceSpecs jcl(@Nullable String before) {
        return jcl(before, s -> {
        });
    }

    public static SourceSpecs jcl(@Nullable String before, Consumer<SourceSpec<Jcl.CompilationUnit>> spec) {
        SourceSpec<Jcl.CompilationUnit> jcl =
                new SourceSpec<>(Jcl.CompilationUnit.class,
                        null,
                        JclParser.builder(),
                        before,
                        null);
        spec.accept(jcl);
        return jcl;
    }

    public static SourceSpecs jcl(@Nullable String before, @Nullable String after) {
        return jcl(before, after, s -> {
        });
    }

    public static SourceSpecs jcl(@Nullable String before, @Nullable String after,
                                    Consumer<SourceSpec<Jcl.CompilationUnit>> spec) {
        SourceSpec<Jcl.CompilationUnit> jcl =
                new SourceSpec<>(Jcl.CompilationUnit.class,
                        null,
                        JclParser.builder(),
                        before,
                        s -> after);
        spec.accept(jcl);
        return jcl;
    }
}
