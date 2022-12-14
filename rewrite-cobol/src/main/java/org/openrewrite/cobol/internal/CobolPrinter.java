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
import org.openrewrite.marker.SearchResult;

import java.util.Optional;

/**
 * Print the post-processed COBOL AST.
 */
public class CobolPrinter<P> extends CobolSourcePrinter<P> {

    private final boolean printColumns;
    private final boolean printOriginalSource;

    public CobolPrinter(boolean printColumns,
                        boolean printCopiedSource,
                        boolean printOriginalSource) {
        super(printColumns, printCopiedSource);
        this.printColumns = printColumns;
        this.printOriginalSource = printOriginalSource;
    }

    @Override
    public Cobol visitWord(Cobol.Word word, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitWord(word, p);
        }

        Optional<SearchResult> searchResultOptional = word.getMarkers().getMarkers().stream()
                .filter(m -> m instanceof SearchResult)
                .map(m -> (SearchResult) m).findFirst();
        SearchResult searchResult = searchResultOptional.orElse(null);

        Optional<Lines> lines = word.getMarkers().findFirst(Lines.class);
        lines.ifPresent(value -> visitLines(value, p));

        Optional<Continuation> continuation = word.getMarkers().findFirst(Continuation.class);
        if (continuation.isPresent() && printColumns) {
            visitContinuation(word, continuation.get(), searchResult, p);
        } else {
            Optional<SequenceArea> sequenceArea = word.getMarkers().findFirst(SequenceArea.class);
            sequenceArea.ifPresent(it -> visitSequenceArea(it, p));

            Optional<IndicatorArea> indicatorArea = word.getMarkers().findFirst(IndicatorArea.class);
            indicatorArea.ifPresent(it -> visitIndicatorArea(it, searchResult, p));

            visitSpace(word.getPrefix(), p);
            p.append(word.getWord());

            Optional<CommentArea> commentArea = word.getMarkers().findFirst(CommentArea.class);
            if (commentArea.isPresent() && !commentArea.get().isAdded()) {
                commentArea.ifPresent(it -> visitCommentArea(it, p));
            }
        }

        return word;
    }
}
