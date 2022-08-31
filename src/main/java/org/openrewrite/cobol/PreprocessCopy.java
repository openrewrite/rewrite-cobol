package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Option;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser;
import org.openrewrite.cobol.proprocessor.*;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.lang.Nullable;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessCopy extends Recipe {

    @Option(displayName = "",
            description = ".",
            example = "")
    List<String> directories;

    @Option(displayName = "",
            description = ".",
            example = "")
    List<String> filePaths;

    @Option(displayName = "",
            description = ".",
            example = "")
    List<String> extensions;

    @Override
    public String getDisplayName() {
        return "TODO";
    }

    @Override
    public String getDescription() {
        return "TODO";
    }

    @Override
    public @Nullable Duration getEstimatedEffortPerOccurrence() {
        return Duration.ofMinutes(1_000_000);
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new AddCopyBookSource(directories, filePaths, extensions);
    }

    private static class AddCopyBookSource extends CobolPreprocessorIsoVisitor<ExecutionContext> {

        private final CobolWordCopyBookFinder cobolWordCopyBookFinder = new CobolWordCopyBookFinder();
        private final List<Path> directoryPaths;
        private final List<Path> filePaths;
        private final List<String> extensions;

        private AddCopyBookSource(List<String> directoryPaths,
                                  List<String> filePaths,
                                  List<String> extensions) {
            this.directoryPaths = directoryPaths.stream().map(Paths::get).collect(Collectors.toList());
            this.filePaths = filePaths.stream().map(Paths::get).collect(Collectors.toList());
            this.extensions = extensions;
        }

        @Override
        public CobolPreprocessor.CopySource visitCopySource(CobolPreprocessor.CopySource copySource, ExecutionContext executionContext) {
            CobolPreprocessor.CopySource c = super.visitCopySource(copySource, executionContext);
            Path copyBook = cobolWordCopyBookFinder.findCopyBook(copySource);
            if (copyBook != null) {
                String source = getSource(copyBook);
                String result;
                try {
                    // TEMP for POC
                    CobolParserParams params = new CobolParserParams(
                            StandardCharsets.UTF_8,
                            emptyList(),
                            getCobolFileExtensions(),
                            getCopyBooks(),
                            CobolDialect.ANSI85,
                            org.openrewrite.cobol.proprocessor.CobolPreprocessor.CobolSourceFormatEnum.FIXED,
                            true
                    );
                    result = new org.openrewrite.cobol.proprocessor.CobolPreprocessor(
                            new CobolCommentEntriesMarker(),
                            new CobolDocumentParser(),
                            new CobolInlineCommentEntriesNormalizer(),
                            new CobolLineIndicatorProcessor(),
                            new CobolLineReader()
                    ).process(copyBook.toFile(), params);
                } catch (IOException e) {
                    result = null;
                }

                if (result != null) {
                    c = c.withSource(source);
                }
            }
            return c;
        }

        // TEMP for POC
        private List<File> getCopyBooks() {
            String userDir = System.getProperty("user.dir");
            String copyBooks = "/src/test/resources/gov/nist/copybooks";
            return Arrays.stream(Objects.requireNonNull(Paths.get(userDir + copyBooks).toFile().listFiles())).collect(toList());
        }

        // TEMP for POC
        private List<String> getCobolFileExtensions() {
            return singletonList("CPY");
        }

        private String getSource(Path copyBook) {
            String source = "";
            try (InputStream inputStream = Files.newInputStream(copyBook)) {
                EncodingDetectingInputStream is = new EncodingDetectingInputStream(inputStream);
                source = is.readFully();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            return source;
        }

        // TODO: create our own or add license credit.
        class CobolWordCopyBookFinder {

            @Nullable
            public Path findCopyBook(CobolPreprocessor.CopySource copySource) {
                for (Path copyBookPath : filePaths) {
                    if (isMatchingCopyBook(copyBookPath.toFile(), copySource)) {
                        return copyBookPath;
                    }
                }

                for (Path copyBookDirectory : directoryPaths) {
                    Path validCopyBook = findCopyBookInDirectory(copyBookDirectory, copySource);
                    if (validCopyBook != null) {
                        return validCopyBook;
                    }
                }

                return null;
            }

            @Nullable
            protected Path findCopyBookInDirectory(Path copyBookDirectory, CobolPreprocessor.CopySource copySource) {
                File[] files = copyBookDirectory.toFile().listFiles();
                if (files != null) {
                    for (File copyBookCandidate : files) {
                        if (isMatchingCopyBook(copyBookCandidate, copySource)) {
                            return copyBookCandidate.toPath();
                        }
                    }
                }

                return null;
            }

            protected boolean isMatchingCopyBook(File copyBookCandidate, CobolPreprocessor.CopySource copySource) {
                String copyBookIdentifier = copySource.getName().getWord();

                if (!extensions.isEmpty()) {
                    for (String copyBookExtension : extensions) {
                        if (isMatchingCopyBookWithExtension(copyBookCandidate, copyBookIdentifier, copyBookExtension)) {
                            return true;
                        }
                    }

                    return false;
                } else {
                    return isMatchingCopyBookWithoutExtension(copyBookCandidate, copyBookIdentifier);
                }
            }

            protected boolean isMatchingCopyBookWithExtension(File copyBookCandidate,
                                                              String copyBookIdentifier,
                                                              @Nullable String copyBookExtension) {
                String copyBookFilename = copyBookExtension == null || copyBookExtension.isEmpty() ? copyBookIdentifier
                        : copyBookIdentifier + "." + copyBookExtension;
                String copyBookCandidateName = copyBookCandidate.getName();
                return copyBookFilename.equalsIgnoreCase(copyBookCandidateName);
            }

            protected boolean isMatchingCopyBookWithoutExtension(File copyBookCandidate,
                                                                 String copyBookIdentifier) {
                String copyBookCandidateBaseName = FilenameUtils.getBaseName(copyBookCandidate.getName());
                return copyBookCandidateBaseName != null && copyBookCandidateBaseName.equalsIgnoreCase(copyBookIdentifier);
            }
        }
    }
}
