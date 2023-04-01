package org.openrewrite.jcl;

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Parser;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.tree.Jcl;

import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

public class JclParser implements Parser<Jcl.CompilationUnit> {

    public JclParser() {
    }

    @Override
    public List<Jcl.CompilationUnit> parseInputs(Iterable<Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
        return Collections.emptyList();
    }

    @Override
    public List<Jcl.CompilationUnit> parse(String... sources) {
        return parse(new InMemoryExecutionContext(), sources);
    }

    @Override
    public boolean accept(Path path) {
        return path.toString().toLowerCase().endsWith(".jcl");
    }

    @Override
    public Path sourcePathFromSourceText(Path prefix, String sourceCode) {
        return prefix.resolve("file.jcl");
    }

    public static JclParser.Builder builder() {
        return new JclParser.Builder();
    }

    public static class Builder extends org.openrewrite.Parser.Builder {
        public Builder() {
            super(Jcl.CompilationUnit.class);
        }

        @Override
        public JclParser build() {
            return new JclParser();
        }

        @Override
        public String getDslName() {
            return "jcl";
        }
    }
}
