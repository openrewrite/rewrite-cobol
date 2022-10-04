package org.openrewrite.cobol;

import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.tree.CobolPreprocessor;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;

public class CobolResourceParser {
    private static final Set<String> DEFAULT_IGNORED_DIRECTORIES = new HashSet<>(Arrays.asList("build", "bin", "target", "out", ".gradle", ".idea", ".project", "node_modules", ".git", ".metadata", ".DS_Store"));

    private final Path baseDir;
    private final Collection<PathMatcher> exclusions;
    private final Collection<Path> excludedDirectories;

    public CobolResourceParser(Path baseDir,
                               Collection<PathMatcher> exclusions,
                               Collection<Path> excludedDirectories) {
        this.baseDir = baseDir;
        this.exclusions = exclusions;
        this.excludedDirectories = excludedDirectories;
    }

    public List<CobolPreprocessor.CopyBook> parseCopyBooks(Collection<Path> alreadyParsed, CobolDialect cobolDialect, List<String> fileExtensions) throws IOException {

        List<Path> copyBooks = new ArrayList<>();
        Files.walkFileTree(baseDir, Collections.emptySet(), 16, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                if (isExcluded(dir) || isIgnoredDirectory(baseDir, dir) || excludedDirectories.contains(dir)) {
                    return FileVisitResult.SKIP_SUBTREE;
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                if (!attrs.isOther() && !attrs.isSymbolicLink() &&
                        !alreadyParsed.contains(file) && !isExcluded(file) &&
                        fileExtensions.stream().anyMatch(it -> file.getFileName().toString().toLowerCase().endsWith(it))) {
                    copyBooks.add(file);
                }
                return FileVisitResult.CONTINUE;
            }
        });

        return CobolPreprocessorParser.parseCopyBooks(copyBooks, null, cobolDialect);
    }

    private boolean isIgnoredDirectory(Path searchDir, Path path) {
        for (Path pathSegment : searchDir.relativize(path)) {
            if (DEFAULT_IGNORED_DIRECTORIES.contains(pathSegment.toString())) {
                return true;
            }
        }
        return false;
    }

    private boolean isExcluded(Path path) {
        if (!exclusions.isEmpty()) {
            for (PathMatcher excluded : exclusions) {
                if (excluded.matches(baseDir.relativize(path))) {
                    return true;
                }
            }
        }
        return false;
    }
}
