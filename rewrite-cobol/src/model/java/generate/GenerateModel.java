/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package generate;

import lombok.RequiredArgsConstructor;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Result;
import org.openrewrite.java.JavaParser;
import org.openrewrite.java.tree.J;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Objects.requireNonNull;

@RequiredArgsConstructor
public class GenerateModel {
    static final ExecutionContext ctx = new InMemoryExecutionContext(Throwable::printStackTrace);

    static JavaParser jp() {
        return JavaParser.fromJavaVersion()
                .classpath(JavaParser.runtimeClasspath())
                .logCompilationWarningsAndErrors(true)
                .build();
    }

    final List<J.ClassDeclaration> modelClasses;

    public static void main(String[] args) {
        new GenerateModel(jp()
                .parse(
                        Arrays.asList(
                                Paths.get("src/model/java/model/Cobol.java"),
                                Paths.get("src/model/java/model/Statement.java")
                        ),
                        null,
                        ctx
                )
                .map(J.CompilationUnit.class::cast)
                .findFirst()
                .get()
                .getClasses()
                .get(0)
                .getBody()
                .getStatements()
                .stream()
                .filter(J.ClassDeclaration.class::isInstance)
                .map(J.ClassDeclaration.class::cast)
                .collect(Collectors.toList())).generate();
    }

    public void generate() {
        List<Result> results = new ArrayList<>();

        Path cobolTreePath = Paths.get("src/main/java/org/openrewrite/cobol/tree/Cobol.java");

        List<Path> deps = Arrays.asList(
                Paths.get("src/main/java/org/openrewrite/cobol/CobolVisitor.java"),
                Paths.get("src/main/java/org/openrewrite/cobol/CobolIsoVisitor.java"),
                Paths.get("src/main/java/org/openrewrite/cobol/tree/CobolContainer.java"),
                Paths.get("src/main/java/org/openrewrite/cobol/tree/CobolLeftPadded.java"),
                Paths.get("src/main/java/org/openrewrite/cobol/tree/CobolRightPadded.java")
        );

//        results.addAll(new WriteModel(modelClasses).run(Collections.singletonList(jp().parse(
//                ListUtils.concat(cobolTreePath, deps), null, ctx).get(0)), ctx));
//        results.addAll(new WriteVisitorMethods(modelClasses).run(jp().parse(
//                Arrays.asList(
//                        Paths.get("src/main/java/org/openrewrite/cobol/CobolVisitor.java"),
//                        Paths.get("src/main/java/org/openrewrite/cobol/CobolIsoVisitor.java")
//                ),
//                null,
//                ctx
//        ), ctx));
//        results.addAll(new WritePrinter(modelClasses).run(jp().parse(
//                Collections.singletonList(
//                        Paths.get("src/main/java/org/openrewrite/cobol/internal/CobolPrinter.java")
//                ),
//                null,
//                ctx
//        ), ctx));
//
//        writeResults(results);
//
//        // TODO Unable to add accessors in the first phase due to some bug in JavaTemplate.
//        writeResults(new WritePaddingAccessors().run(jp().parse(Collections.singletonList(cobolTreePath), null, ctx), ctx));
    }

    private void writeResults(List<Result> results) {
        for (Result result : results) {
            try {
                Files.write(requireNonNull(result.getAfter()).getSourcePath(),
                        result.getAfter().printAllAsBytes());
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
    }
}
