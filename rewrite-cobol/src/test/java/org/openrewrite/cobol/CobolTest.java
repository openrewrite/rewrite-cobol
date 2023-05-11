/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol;

import io.github.classgraph.ClassGraph;
import io.github.classgraph.ScanResult;
import org.openrewrite.ExecutionContext;
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.tree.Space;
import org.openrewrite.internal.StringUtils;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

import java.util.List;
import java.util.Optional;

import static org.openrewrite.test.RewriteTest.toRecipe;

public class CobolTest implements RewriteTest {

    private static final CobolDialect DIALECT = CobolDialect.ibmAnsi85();
    private List<String> nistResourcePaths = null;

    @Override
    public void defaults(RecipeSpec spec) {
        spec.recipe(toRecipe(() -> new CobolIsoVisitor<>() {
            @Override
            public Space visitSpace(Space space, Space.Location location, ExecutionContext ctx) {
                String whitespace = space.getWhitespace().trim();
                if (!(whitespace.isEmpty() || DIALECT.getSeparators().contains(whitespace + " "))) {
                    return space.withWhitespace("(~~>${space.whitespace}<~~)");
                }
                return space;
            }
        }));
    }

    private List<String> getNistResourcePaths() {
        if (nistResourcePaths == null) {
            try (ScanResult scanResult = new ClassGraph().acceptPaths("/gov/nist").scan()) {
                nistResourcePaths = scanResult.getAllResources().getPaths();
            }
        }
        return nistResourcePaths;
    }

    public String getNistResource(String sourceName) {
        Optional<String> source = getNistResourcePaths().stream()
                .filter(it -> it.endsWith(sourceName))
                .findFirst();

        assert source.isPresent();
        return StringUtils.readFully(getClass().getClassLoader().getResourceAsStream(source.get()));
    }
}
