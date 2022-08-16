/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser;
import org.openrewrite.internal.lang.Nullable;

import java.util.Arrays;
import java.util.List;

/**
 * A replacement context that defines, which replaceables should be replaced by
 * which replacements.
 */
public class CobolDocumentContext {

    private CobolReplacementMapping[] currentReplaceableReplacements;

    private StringBuffer outputBuffer = new StringBuffer();

    public String read() {
        return outputBuffer.toString();
    }

    /**
     * Replaces replaceables with replacements.
     */
    public void replaceReplaceablesByReplacements(BufferedTokenStream tokens) {
        if (currentReplaceableReplacements != null) {
            Arrays.sort(currentReplaceableReplacements);

            for (CobolReplacementMapping replaceableReplacement : currentReplaceableReplacements) {
                String currentOutput = outputBuffer.toString();
                String replacedOutput = replaceableReplacement.replace(currentOutput, tokens);

                outputBuffer = new StringBuffer();
                outputBuffer.append(replacedOutput);
            }
        }
    }

    public void storeReplaceablesAndReplacements(@Nullable List<CobolPreprocessorParser.ReplaceClauseContext> replaceClauses) {
        if (replaceClauses == null) {
            currentReplaceableReplacements = null;
        } else {
            int length = replaceClauses.size();
            currentReplaceableReplacements = new CobolReplacementMapping[length];

            int i = 0;

            for (CobolPreprocessorParser.ReplaceClauseContext replaceClause : replaceClauses) {
                CobolReplacementMapping replaceableReplacement = new CobolReplacementMapping();

                replaceableReplacement.replaceable = replaceClause.replaceable();
                replaceableReplacement.replacement = replaceClause.replacement();

                currentReplaceableReplacements[i] = replaceableReplacement;
                i++;
            }
        }
    }

    public void write(String text) {
        outputBuffer.append(text);
    }
}
