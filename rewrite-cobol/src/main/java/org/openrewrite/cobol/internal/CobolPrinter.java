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
import org.openrewrite.cobol.markers.Continuation;
import org.openrewrite.cobol.markers.Lines;
import org.openrewrite.cobol.tree.*;

import java.util.Optional;

/**
 * Print the post-processed COBOL AST.
 */
public class CobolPrinter<P> extends CobolSourcePrinter<P> {

    private final boolean printColumns;
    private final boolean printOriginalSource;
    private Cobol.Preprocessor.Replacement additiveReplacement = null;

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

        Optional<Lines> lines = word.getMarkers().findFirst(Lines.class);
        lines.ifPresent(value -> visitLines(value, p));

        if (printColumns && word.getLines() != null) {
            for (CobolLine cobolLine : word.getLines()) {
                visitMarkers(cobolLine.getMarkers(), p);
                cobolLine.printCobolLine(this, getCursor(), p);
            }
        }

        if (additiveReplacement == null && word.getReplacement() != null && word.getReplacement().getType() == Cobol.Preprocessor.Replacement.Type.ADDITIVE) {
            additiveReplacement = word.getReplacement();
        } else if (additiveReplacement != null && word.getReplacement() != null && word.getReplacement().getType() == Cobol.Preprocessor.Replacement.Type.ADDITIVE && additiveReplacement.getId() != word.getReplacement().getId()) {
            additiveReplacement = word.getReplacement();
            p.append("\n");
        } else if (additiveReplacement != null && word.getReplacement() == null) {
            additiveReplacement = null;
            p.append("\n");
        }

        Optional<Continuation> continuation = word.getMarkers().findFirst(Continuation.class);
        if (continuation.isPresent() && printColumns) {
            visitContinuation(word, continuation.get(), p);
        } else {
            visit(word.getSequenceArea(), p);
            visit(word.getIndicatorArea(), p);

            beforeSyntax(word, Space.Location.WORD_PREFIX, p);
            p.append(word.getWord());

            if (word.getCommentArea() != null && !word.getCommentArea().isAdded()) {
                visit(word.getCommentArea(), p);
            }
        }

        afterSyntax(word, p);
        return word;
    }
}
