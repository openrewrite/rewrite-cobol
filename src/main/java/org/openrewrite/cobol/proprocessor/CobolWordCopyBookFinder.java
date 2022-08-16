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

public class CobolWordCopyBookFinder {

    @Nullable
    public File findCopyBook(CobolParserParams params, CobolPreprocessorParser.CobolWordContext ctx) {
        if (params.getCopyBookFiles() != null) {
            for (File copyBookFile : params.getCopyBookFiles()) {
                if (isMatchingCopyBook(copyBookFile, params, ctx)) {
                    return copyBookFile;
                }
            }
        }

        if (params.getCopyBookDirectories() != null) {
            for (File copyBookDirectory : params.getCopyBookDirectories()) {
                File validCopyBook = findCopyBookInDirectory(copyBookDirectory, params, ctx);

                if (validCopyBook != null) {
                    return validCopyBook;
                }
            }
        }

        return null;
    }

    @Nullable
    protected File findCopyBookInDirectory(File copyBooksDirectory, CobolParserParams params,
                                           CobolPreprocessorParser.CobolWordContext ctx) {
        if (copyBooksDirectory.listFiles() != null) {
            //noinspection ConstantConditions
            for (File copyBookCandidate : copyBooksDirectory.listFiles()) {
                if (isMatchingCopyBook(copyBookCandidate, params, ctx)) {
                    return copyBookCandidate;
                }
            }
        }

        return null;
    }

    protected boolean isMatchingCopyBook(File copyBookCandidate, CobolParserParams params,
                                         CobolPreprocessorParser.CobolWordContext ctx) {
        String copyBookIdentifier = ctx.getText();

        if (params.getCopyBookExtensions() != null) {
            for (String copyBookExtension : params.getCopyBookExtensions()) {
                if (isMatchingCopyBookWithExtension(copyBookCandidate, copyBookIdentifier, copyBookExtension)) {
                    return true;
                }
            }

            return false;
        } else {
            return isMatchingCopyBookWithoutExtension(copyBookCandidate, copyBookIdentifier);
        }
    }

    protected boolean isMatchingCopyBookWithExtension(File copyBookCandidate, String copyBookIdentifier,
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
