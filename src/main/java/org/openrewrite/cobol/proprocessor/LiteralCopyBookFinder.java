/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser;
import org.openrewrite.internal.lang.Nullable;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LiteralCopyBookFinder {

    @Nullable
    public File findCopyBook(CobolParserParams params, CobolPreprocessorParser.LiteralContext ctx) {
        if (params.getCopyBookFiles() != null) {
            for (File copyBookFile : params.getCopyBookFiles()) {
                if (isMatchingCopyBook(copyBookFile, null, ctx)) {
                    return copyBookFile;
                }
            }
        }

        if (params.getCopyBookDirectories() != null) {
            for (File copyBookDirectory : params.getCopyBookDirectories()) {
                File validCopyBook = findCopyBookInDirectory(copyBookDirectory, ctx);

                if (validCopyBook != null) {
                    return validCopyBook;
                }
            }
        }

        return null;
    }

    @Nullable
    protected File findCopyBookInDirectory(File copyBooksDirectory, CobolPreprocessorParser.LiteralContext ctx) {
        try (Stream<Path> paths = Files.walk(copyBooksDirectory.toPath())) {
            for (File copyBookCandidate : paths.map(Path::toFile).collect(Collectors.toList())) {
                if (isMatchingCopyBook(copyBookCandidate, copyBooksDirectory, ctx)) {
                    return copyBookCandidate;
                }
            }
        } catch (IOException ignored) {
        }

        return null;
    }

    protected boolean isMatchingCopyBook(File copyBookCandidate, @Nullable File cobolCopyDir,
                                         CobolPreprocessorParser.LiteralContext ctx) {
        String copyBookIdentifier = PreprocessorStringUtils.trimQuotes(ctx.getText()).replace("\\", "/");
        boolean result;

        if (cobolCopyDir == null) {
            result = isMatchingCopyBookRelative(copyBookCandidate, copyBookIdentifier);
        } else {
            result = isMatchingCopyBookAbsolute(copyBookCandidate, cobolCopyDir, copyBookIdentifier);
        }

        return result;
    }

    protected boolean isMatchingCopyBookAbsolute(File copyBookCandidate, File cobolCopyDir,
                                                 String copyBookIdentifier) {
        Path copyBookCandidateAbsolutePath = Paths.get(copyBookCandidate.getAbsolutePath()).normalize();
        Path copyBookIdentifierAbsolutePath = Paths.get(cobolCopyDir.getAbsolutePath(), copyBookIdentifier)
                .normalize();
        return copyBookIdentifierAbsolutePath.toString()
                .equalsIgnoreCase(copyBookCandidateAbsolutePath.toString());
    }

    protected boolean isMatchingCopyBookRelative(File copyBookCandidate, String copyBookIdentifier) {
        Path copyBookCandidateAbsolutePath = Paths.get(copyBookCandidate.getAbsolutePath()).normalize();
        Path copyBookIdentifierRelativePath;

        if (copyBookIdentifier.startsWith("/") || copyBookIdentifier.startsWith("./")
                || copyBookIdentifier.startsWith("\\") || copyBookIdentifier.startsWith(".\\")) {
            copyBookIdentifierRelativePath = Paths.get(copyBookIdentifier).normalize();
        } else {
            copyBookIdentifierRelativePath = Paths.get("/" + copyBookIdentifier).normalize();
        }

        return copyBookCandidateAbsolutePath.toString().toLowerCase()
                .endsWith(copyBookIdentifierRelativePath.toString().toLowerCase());
    }
}
