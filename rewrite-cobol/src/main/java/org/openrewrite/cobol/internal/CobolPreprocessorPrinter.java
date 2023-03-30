/*
 * Copyright 2022 the original author or authors.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * https://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openrewrite.cobol.internal;

import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.tree.*;

/**
 * Print the original preprocessed COBOL.
 * Note: All the logic to print column areas exists in visitWord.
 */
public class CobolPreprocessorPrinter<P> extends CobolPreprocessorSourcePrinter<P> {

    private final boolean printOriginalSource;
    private final boolean printColumns;
    private Replacement additiveReplacement = null;

    public CobolPreprocessorPrinter(boolean printOriginalSource,
                                    boolean printColumns) {
        super(printColumns);
        this.printOriginalSource = printOriginalSource;
        this.printColumns = printColumns;
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

        beforeSyntax(word, Space.Location.WORD_PREFIX, p);
        p.append(word.getWord());

        if (word.getCommentArea() != null && !word.getCommentArea().isAdded()) {
            word.getCommentArea().printColumnArea(this, getCursor(), printColumns, p);
        }

        afterSyntax(word, p);
        return word;
    }
}
