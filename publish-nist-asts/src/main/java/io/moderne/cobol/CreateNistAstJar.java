package io.moderne.cobol;

import io.github.classgraph.ClassGraph;
import io.github.classgraph.ScanResult;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Parser;
import org.openrewrite.cobol.CobolParser;
import org.openrewrite.cobol.tree.CobolPreprocessor;

import java.util.ArrayList;
import java.util.List;

public class CreateNistAstJar {
    public static void main(String[] args) {
        List<CobolPreprocessor.CopyBook> copybooks = new ArrayList<>();
        List<Parser.Input> sources = new ArrayList();
        try(ScanResult scanResult = new ClassGraph()
                .acceptPaths("gov/nist")
                .scan()) {
            //TODO populate into copybooks and sources
        }
        CobolParser cp = CobolParser.builder()
                .setCopyBooks(copybooks)
                .build();
        ExecutionContext ctx = new InMemoryExecutionContext(t -> { throw new RuntimeException(t); });
        cp.parseInputs(sources, null, ctx);
        // TODO add moderne-ast-write dependency, write out ast files somewhere
    }
}
