/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.internal;

import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.tree.*;

/**
 * Print the original preprocessed COBOL.
 * Note: All the logic to print column areas exists in visitWord.
 */
public class CobolPreprocessorPrinter<P> extends CobolPreprocessorSourcePrinter<P> {

    private final CobolPrinter<P> cobolPrinter;
    private final boolean printOriginalSource;

    public CobolPreprocessorPrinter(boolean printOriginalSource,
                                    boolean printColumns) {
        super(printColumns);
        this.cobolPrinter = new CobolPrinter<>(printColumns, printOriginalSource);
        this.printOriginalSource = printOriginalSource;
    }

    @Override
    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
        beforeSyntax(copyBook, Space.Location.COPY_BOOK_PREFIX, p);
        visit(copyBook.getAst(), p);
        afterSyntax(copyBook, p);
        return copyBook;
    }

    @Override
    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitCopyStatement(copyStatement, p);
        }
        if (copyStatement.getCopyBook() != null) {
            beforeSyntax(copyStatement, Space.Location.COPY_STATEMENT_PREFIX, p);
            visit(copyStatement.getCopyBook(), p);
            if (!p.getOut().endsWith("\n")) {
                p.append("\n");
            }
            afterSyntax(copyStatement, p);
        }
        return copyStatement;
    }

    @Override
    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitWord(word, p);
        }

        cobolPrinter.visitWord(word.getCobolWord(), p);

        afterSyntax(word, p);
        return word;
    }
}
