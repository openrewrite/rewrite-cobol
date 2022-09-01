package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.openrewrite.Parser;
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.internal.CobolPreprocessorParserVisitor;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorLexer;
import org.openrewrite.cobol.proprocessor.*;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.cobol.tree.Space;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static org.openrewrite.Tree.randomId;
import static org.openrewrite.cobol.proprocessor.CobolDialect.ANSI85;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessCopyVisitor<P> extends CobolPreprocessorIsoVisitor<P> {

    Parser.Input sourceFile;

    @Nullable
    Path relativeTo;

    CobolWordCopyBookFinder cobolWordCopyBookFinder = new CobolWordCopyBookFinder();
    List<Path> directoryPaths;
    List<Path> filePaths;
    List<String> extensions;
    CobolDialect cobolDialect;

    public PreprocessCopyVisitor(Parser.Input sourceFile,
                                 @Nullable Path relativeTo,
                                 List<String> directoryPaths,
                                 List<String> filePaths,
                                 List<String> extensions,
                                 CobolDialect cobolDialect) {
        this.sourceFile = sourceFile;
        this.relativeTo = relativeTo;
        this.directoryPaths = directoryPaths.stream().map(Paths::get).collect(Collectors.toList());
        this.filePaths = filePaths.stream().map(Paths::get).collect(Collectors.toList());
        this.extensions = extensions;
        this.cobolDialect = cobolDialect;
    }

    @Override
    public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, P p) {
        CobolPreprocessor.CopyStatement c = super.visitCopyStatement(copyStatement, p);
        Path copyBookPath = cobolWordCopyBookFinder.findCopyBook(c.getCopySource());
        if (copyBookPath != null) {
            String source = getSource(copyBookPath);
            String result;

            try {
                // TEMP for POC, using proleap preprocessor to produce malformed COBOL source.
                CobolParserParams params = new CobolParserParams(
                        StandardCharsets.UTF_8,
                        emptyList(),
                        getCobolFileExtensions(),
                        getCopyBooks(),
                        ANSI85,
                        org.openrewrite.cobol.proprocessor.CobolPreprocessor.CobolSourceFormatEnum.FIXED,
                        true
                );
                // Convert the copyBook into a valid source for the CobolPreproccessor.g4
                result = new org.openrewrite.cobol.proprocessor.CobolPreprocessor(
                        new CobolCommentEntriesMarker(),
                        new CobolDocumentParser(),
                        new CobolInlineCommentEntriesNormalizer(),
                        new CobolLineIndicatorProcessor(),
                        new CobolLineReader()
                ).process(copyBookPath.toFile(), params);
            } catch (IOException e) {
                result = null;
            }

            if (result != null) {
                org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser parser =
                        new org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser(
                                new CommonTokenStream(new CobolPreprocessorLexer(CharStreams.fromString(result))));

                CobolPreprocessorParserVisitor parserVisitor = new CobolPreprocessorParserVisitor(
                        sourceFile.getRelativePath(relativeTo),
                        sourceFile.getFileAttributes(),
                        source,
                        sourceFile.getSource().getCharset(),
                        sourceFile.getSource().isCharsetBomMarked(),
                        cobolDialect
                );
                CobolPreprocessor.CompilationUnit cu = parserVisitor.visitStartRule(parser.startRule());
                CobolPreprocessor parsedCopySource = cu.getCobols().get(0);

                CobolPreprocessor.CopyBook copyBook = new CobolPreprocessor.CopyBook(
                        randomId(),
                        Space.EMPTY,
                        Markers.EMPTY,
                        copyBookPath,
                        parsedCopySource,
                        cu.getEof()
                );
                c = c.withCopyBook(copyBook);
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

    /*
     * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
     * All rights reserved.
     *
     * This software may be modified and distributed under the terms
     * of the MIT license. See the LICENSE file for details.
     */
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
