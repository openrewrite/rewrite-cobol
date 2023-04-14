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
