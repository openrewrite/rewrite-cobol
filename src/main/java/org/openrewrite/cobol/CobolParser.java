package org.openrewrite.cobol;

import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Parser;
import org.openrewrite.cobol.tree.Cobol;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public interface CobolParser extends Parser<Cobol.CompilationUnit> {
    List<String> COBOL_FILE_EXTENSIONS = Arrays.asList(".cbl", ".cpy");

    @Override
    default List<Cobol.CompilationUnit> parse(String... sources) {
        return parse(new InMemoryExecutionContext(), sources);
    }

    @Override
    default boolean accept(Path path) {
        String s = path.toString().toLowerCase();
        for (String COBOL_FILE_EXTENSION : COBOL_FILE_EXTENSIONS) {
            if (s.endsWith(COBOL_FILE_EXTENSION)) {
                return true;
            }
        }
        return false;
    }

    @Override
    default Path sourcePathFromSourceText(Path prefix, String sourceCode) {
        return prefix.resolve("file.CBL");
    }
}
