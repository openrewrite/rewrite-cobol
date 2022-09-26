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
 * Print the post-processed COBOL AST.
 */
public class CobolPrinter<P> extends CobolSourcePrinter<P> {

    private final boolean printOriginalSource;

    public CobolPrinter(boolean printOriginalSource) {
        this.printOriginalSource = printOriginalSource;
    }

    @Override
    public Cobol visitWord(Cobol.Word word, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitWord(word, p);
        }

        Optional<Lines> lines = word.getMarkers().findFirst(Lines.class);
        if (lines.isPresent()) {
            for (Lines.Line line : lines.get().getLines()) {
                if (line.isCopiedSource()) {
                    continue;
                }

                if (line.getSequenceArea() != null) {
                    p.append(line.getSequenceArea().getSequence());
                }
                if (line.getIndicatorArea() != null) {
                    p.append(line.getIndicatorArea().getIndicator());
                    p.append(line.getIndicatorArea().getContinuationPrefix());
                }
                p.append(line.getContent());
                if (line.getCommentArea() != null) {
                    visitSpace(line.getCommentArea().getPrefix(), p);
                    p.append(line.getCommentArea().getComment());
                    visitSpace(line.getCommentArea().getEndOfLine(), p);
                }
            }
        }

        Optional<Continuation> continuation = word.getMarkers().findFirst(Continuation.class);
        if (continuation.isPresent()) {
            if (continuation.get().getContinuations().containsKey(0)) {
                Markers markers = continuation.get().getContinuations().get(0);
                Optional<SequenceArea> sequenceArea = markers.findFirst(SequenceArea.class);
                sequenceArea.ifPresent(it -> p.append(it.getSequence()));

                Optional<IndicatorArea> indicatorArea = markers.findFirst(IndicatorArea.class);
                indicatorArea.ifPresent(it -> p.append(it.getIndicator()));
                indicatorArea.ifPresent(it -> p.append(it.getContinuationPrefix()));
            }

            visitSpace(word.getPrefix(), p);
            char[] charArray = word.getWord().toCharArray();
            for (int i = 0; i < charArray.length; i++) {
                if (i != 0 && continuation.get().getContinuations().containsKey(i)) {
                    Markers markers = continuation.get().getContinuations().get(i);
                    Optional<CommentArea> commentArea = markers.findFirst(CommentArea.class);
                    commentArea.ifPresent(it -> visitSpace(it.getPrefix(), p));
                    commentArea.ifPresent(it -> p.append(it.getComment()));
                    commentArea.ifPresent(it -> visitSpace(it.getEndOfLine(), p));

                    Optional<SequenceArea> sequenceArea = markers.findFirst(SequenceArea.class);
                    sequenceArea.ifPresent(it -> p.append(it.getSequence()));

                    Optional<IndicatorArea> indicatorArea = markers.findFirst(IndicatorArea.class);
                    indicatorArea.ifPresent(it -> p.append(it.getIndicator()));
                    indicatorArea.ifPresent(it -> p.append(it.getContinuationPrefix()));
                }
                char c = charArray[i];
                p.append(c);
            }

            List<Markers> lastMarkers = continuation.get().getContinuations().entrySet().stream()
                    .filter(it -> it.getKey() > word.getWord().length())
                    .map(Map.Entry::getValue)
                    .collect(Collectors.toList());

            if (!lastMarkers.isEmpty()) {
                Markers markers = lastMarkers.get(0);
                Optional<CommentArea> commentArea = markers.findFirst(CommentArea.class);
                commentArea.ifPresent(it -> visitSpace(it.getPrefix(), p));
                commentArea.ifPresent(it -> p.append(it.getComment()));
                commentArea.ifPresent(it -> visitSpace(it.getEndOfLine(), p));
            }
        } else {
            Optional<SequenceArea> sequenceArea = word.getMarkers().findFirst(SequenceArea.class);
            sequenceArea.ifPresent(it -> p.append(it.getSequence()));

            Optional<IndicatorArea> indicatorArea = word.getMarkers().findFirst(IndicatorArea.class);
            indicatorArea.ifPresent(it -> p.append(it.getIndicator()));
            indicatorArea.ifPresent(it -> p.append(it.getContinuationPrefix()));

            visitSpace(word.getPrefix(), p);
            p.append(word.getWord());

            Optional<CommentArea> commentArea = word.getMarkers().findFirst(CommentArea.class);
            if (commentArea.isPresent() && !commentArea.get().isAdded()) {
                commentArea.ifPresent(it -> visitSpace(it.getPrefix(), p));
                commentArea.ifPresent(it -> p.append(it.getComment()));
                commentArea.ifPresent(it -> visitSpace(it.getEndOfLine(), p));
            }
        }

        return word;
    }
}
