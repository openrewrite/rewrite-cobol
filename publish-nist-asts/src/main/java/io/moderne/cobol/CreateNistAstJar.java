/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package io.moderne.cobol;

import io.github.classgraph.*;
import io.moderne.serialization.TreeSerializer;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolParser;
import org.openrewrite.cobol.tree.CobolSourceFile;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class CreateNistAstJar {
    public static void main(String[] args) {
        if (args.length == 0) {
            throw new IllegalArgumentException("Must supply the path to write the ASTs to as the first program argument");
        }
        List<Parser.Input> rawCopybooks = new ArrayList<>();
        List<Parser.Input> sources = new ArrayList<>();

        System.out.println("Reading cobol files from runtime classpath resources");
        Instant start = Instant.now();
        try (ScanResult scanResult = new ClassGraph()
                .acceptPaths("gov/nist")
                .scan()) {
            for (String copybookExt : CobolParser.COPYBOOK_FILE_EXTENSIONS) {
                try (ResourceList resources = scanResult.getResourcesWithExtension(copybookExt)) {
                    resources.forEach(resource -> rawCopybooks.add(asInput(resource)));
                }
            }
            for (String cobolExt : CobolParser.COBOL_FILE_EXTENSIONS) {
                try (ResourceList resources = scanResult.getResourcesWithExtension(cobolExt)) {
                    resources.forEach(resource -> sources.add(asInput(resource)));
                }
            }
        }
        System.out.println("Found " + rawCopybooks.size() + " copybooks and " + sources.size() + " cobol sources in " + prettyPrint(Duration.between(start, Instant.now())));
        ExecutionContext ctx = new InMemoryExecutionContext(t -> {
            // There is exactly one cobol file in the NIST suite which fails to parse
//            throw new RuntimeException(t);
        });

        CobolParser cp = CobolParser.builder()
                .build();
        sources.addAll(rawCopybooks);
        System.out.println("Parsing " + sources.size() + " COBOL sources");
        start = Instant.now();
        List<CobolSourceFile> cus =  cp.parseInputs(sources, null, ctx).collect(Collectors.toList());
        System.out.println("Parsed sources into " + cus.size() + " Compilation Units in " +
                prettyPrint(Duration.between(start, Instant.now())));

        Path destination = Paths.get(args[0]);
        System.out.println("Writing AST file to " + destination);
        //noinspection ResultOfMethodCallIgnored
        destination.getParent().toFile().mkdirs();
        List<SourceFile> sourceFiles = new ArrayList<>(cus.size());
        sourceFiles.addAll(cus);
        try (OutputStream outputStream = Files.newOutputStream(destination)) {
            start = Instant.now();
            new TreeSerializer().write(sourceFiles, outputStream);
            System.out.println("Wrote ASTs in " + prettyPrint(Duration.between(start, Instant.now())));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static Parser.Input asInput(Resource resource) {
        byte[] bytes;
        try (CloseableByteBuffer buf = resource.readCloseable()) {
            bytes = buf.getByteBuffer().array();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return new Parser.Input(
                Paths.get(resource.getPath()), () -> new ByteArrayInputStream(bytes));
    }

    private static String prettyPrint(Duration duration) {
        StringBuilder result = new StringBuilder();
        if (duration.toHours() > 0) {
            result.append(duration.toHours()).append("h ");
        }
        if (duration.toMinutesPart() > 0) {
            result.append(duration.toMinutesPart()).append("m ");
        }
        result.append(duration.toSecondsPart()).append("s");
        return result.toString();
    }
}
