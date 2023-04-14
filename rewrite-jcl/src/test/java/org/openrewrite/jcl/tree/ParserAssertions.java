package org.openrewrite.jcl.tree;

import org.openrewrite.ExecutionContext;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.JclIsoVisitor;
import org.openrewrite.jcl.JclParser;
import org.openrewrite.jcl.JclVisitor;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.util.function.Consumer;

import static org.assertj.core.api.Assertions.assertThat;

public class ParserAssertions {

    private ParserAssertions() {
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
                ParserAssertions::customizeExecutionContext);
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
                ParserAssertions::customizeExecutionContext).after(s -> after);
        acceptSpec(spec, jcl);
        return jcl;
    }

    private static void acceptSpec(Consumer<SourceSpec<Jcl.CompilationUnit>> spec, SourceSpec<Jcl.CompilationUnit> jcl) {
        Consumer<Jcl.CompilationUnit> userSuppliedAfterRecipe = jcl.getAfterRecipe();
        jcl.afterRecipe(userSuppliedAfterRecipe::accept);
        isFullyParsed().andThen(spec).accept(jcl);
    }

    public static Consumer<SourceSpec<Jcl.CompilationUnit>> isFullyParsed() {
        return spec -> spec.afterRecipe(cu -> new JclIsoVisitor<Integer>() {
            @Override
            public Space visitSpace(Space space, Space.Location loc, Integer integer) {
                assertThat(space.getWhitespace().trim()).isEmpty();
                return super.visitSpace(space, loc, integer);
            }
        }.visit(cu, 0));
    }
}
