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

import java.util.Optional;

/**
 * TODO: explain.
 */
public class CobolPostPreprocessorPrinter<P> extends CobolPreprocessorPrinter<P> {

    boolean printWithColumnAreas;

    public CobolPostPreprocessorPrinter(boolean printWithColumnAreas) {
        this.printWithColumnAreas = printWithColumnAreas;
    }

    @Override
    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
        visitSpace(copyBook.getPrefix(), p);
        visitMarkers(copyBook.getMarkers(), p);
        visit(copyBook.getAst(), p);
        visit(copyBook.getEof(), p);
        return copyBook;
    }

    @Override
    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        // TODO: Fix Me. Add markers from COPY and the source name to the CopyBook AST to print correct end of line.
        visitSpace(copyStatement.getPrefix(), p);
        visitMarkers(copyStatement.getMarkers(), p);
        visit(copyStatement.getCopyBook(), p);
        return copyStatement;
    }

    @Override
    public CobolPreprocessor visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, PrintOutputCapture<P> p) {
        if (replaceArea.getCobols() != null) {
            for (CobolPreprocessor cobol : replaceArea.getCobols()) {
                visit(cobol, p);
            }
        }
        return replaceArea;
    }

    @Override
    public CobolPreprocessor visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, PrintOutputCapture<P> p) {
        // Do not print.
        return replaceOffStatement;
    }

    @Override
    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        if (printWithColumnAreas) {
            super.visitWord(word, p);
        } else {
            // print applicable empty lines.
            visitSpace(word.getPrefix(), p);
            visitMarkers(word.getMarkers(), p);
            p.append(word.getWord());
            Optional<CommentArea> commentArea = word.getMarkers().findFirst(CommentArea.class);
            commentArea.ifPresent(area -> visitSpace(area.getPrefix(), p));
            commentArea.ifPresent(area -> visitSpace(area.getEndOfLine(), p));
        }
        return word;
    }
}
