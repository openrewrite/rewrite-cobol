/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.internal;

import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.tree.*;

/**
 * Print the post-processed COBOL AST.
 */
public class CobolPrinter<P> extends CobolSourcePrinter<P> {

    private final boolean printColumns;
    private final boolean printOriginalSource;
    private Replacement additiveReplacement = null;

    public CobolPrinter(boolean printColumns,
                        boolean printOriginalSource) {
        super(printColumns);
        this.printColumns = printColumns;
        this.printOriginalSource = printOriginalSource;
    }

    @Override
    public Cobol visitWord(Cobol.Word word, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitWord(word, p);
        }

        if (printColumns && word.getLines() != null) {
            for (CobolLine cobolLine : word.getLines()) {
                visitMarkers(cobolLine.getMarkers(), p);
                cobolLine.printCobolLine(this, getCursor(), p);
            }
        }

        if (additiveReplacement == null && word.getReplacement() != null && word.getReplacement().getType() == Replacement.Type.ADDITIVE) {
            additiveReplacement = word.getReplacement();
        } else if (additiveReplacement != null && word.getReplacement() != null && word.getReplacement().getType() == Replacement.Type.ADDITIVE && additiveReplacement.getId() != word.getReplacement().getId()) {
            additiveReplacement = word.getReplacement();
            p.append("\n");
        } else if (additiveReplacement != null && word.getReplacement() == null) {
            additiveReplacement = null;
            p.append("\n");
        }

        if (word.getSequenceArea() != null) {
            word.getSequenceArea().printColumnArea(this, getCursor(), printColumns, p);
        }
        if (word.getIndicatorArea() != null) {
            word.getIndicatorArea().printColumnArea(this, getCursor(), printColumns, p);
        }

        if (word.getContinuation() != null && printColumns) {
            word.getContinuation().printContinuation(this, getCursor(), word, printColumns, p);
        } else {
            beforeSyntax(word, Space.Location.WORD_PREFIX, p);
            p.append(word.getWord());
        }

        if (word.getCommentArea() != null && !word.getCommentArea().isAdded()) {
            word.getCommentArea().printColumnArea(this, getCursor(), printColumns, p);
        }

        afterSyntax(word, p);
        return word;
    }
}
