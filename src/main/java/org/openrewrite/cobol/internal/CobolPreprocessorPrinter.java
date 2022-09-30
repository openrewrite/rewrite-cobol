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
import org.openrewrite.marker.Markers;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Print the original preprocessed COBOL.
 * Note: All the logic to print column areas exists in visitWord.
 */
public class CobolPreprocessorPrinter<P> extends CobolPreprocessorSourcePrinter<P> {

    private final boolean printOriginalSource;
    private final boolean printColumns;

    public CobolPreprocessorPrinter(boolean printOriginalSource,
                                    boolean printColumns) {
        super(printColumns);
        this.printOriginalSource = printOriginalSource;
        this.printColumns = printColumns;
    }

    @Override
    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
        visit(copyBook.getAst(), p);
        return copyBook;
    }

    @Override
    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitCopyStatement(copyStatement, p);
        }
        if (copyStatement.getCopyBook() != null) {
            visit(copyStatement.getCopyBook(), p);
            if (!p.getOut().endsWith("\n")) {
                p.append("\n");
            }
        }
        return copyStatement;
    }

    @Override
    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitWord(word, p);
        }

        Optional<Lines> lines = word.getMarkers().findFirst(Lines.class);
        lines.ifPresent(value -> visitLines(value, p));

        Optional<Continuation> continuation = word.getMarkers().findFirst(Continuation.class);
        if (continuation.isPresent() && printColumns) {
            visitContinuation(word, continuation.get(), p);
        } else {
            Optional<SequenceArea> sequenceArea = word.getMarkers().findFirst(SequenceArea.class);
            sequenceArea.ifPresent(it -> visitSequenceArea(it, p));

            Optional<IndicatorArea> indicatorArea = word.getMarkers().findFirst(IndicatorArea.class);
            indicatorArea.ifPresent(it -> visitIndicatorArea(it, p));

            visitSpace(word.getPrefix(), p);
            p.append(word.getWord());

            Optional<CommentArea> commentArea = word.getMarkers().findFirst(CommentArea.class);
            commentArea.ifPresent(it -> visitCommentArea(it, p));
        }

        return word;
    }

    public void visitContinuation(CobolPreprocessor.Word word, Continuation continuation, PrintOutputCapture<P> p) {
        if (continuation.getContinuations().containsKey(0)) {
            Markers markers = continuation.getContinuations().get(0);
            Optional<SequenceArea> sequenceArea = markers.findFirst(SequenceArea.class);
            sequenceArea.ifPresent(it -> visitSequenceArea(it, p));

            Optional<IndicatorArea> indicatorArea = markers.findFirst(IndicatorArea.class);
            indicatorArea.ifPresent(it -> visitIndicatorArea(it, p));
        }

        visitSpace(word.getPrefix(), p);

        char[] charArray = word.getWord().toCharArray();
        for (int i = 0; i < charArray.length; i++) {
            if (i != 0 && continuation.getContinuations().containsKey(i)) {
                Markers markers = continuation.getContinuations().get(i);
                Optional<CommentArea> commentArea = markers.findFirst(CommentArea.class);
                commentArea.ifPresent(it -> visitCommentArea(it, p));

                Optional<SequenceArea> sequenceArea = markers.findFirst(SequenceArea.class);
                sequenceArea.ifPresent(it -> visitSequenceArea(it, p));

                Optional<IndicatorArea> indicatorArea = markers.findFirst(IndicatorArea.class);
                indicatorArea.ifPresent(it -> visitIndicatorArea(it, p));
            }
            char c = charArray[i];
            p.append(c);
        }

        List<Markers> lastMarkers = continuation.getContinuations().entrySet().stream()
                .filter(it -> it.getKey() > word.getWord().length())
                .map(Map.Entry::getValue)
                .collect(Collectors.toList());

        if (!lastMarkers.isEmpty()) {
            Markers markers = lastMarkers.get(0);
            Optional<CommentArea> commentArea = markers.findFirst(CommentArea.class);
            commentArea.ifPresent(it -> visitCommentArea(it, p));
        }
    }
}
