package org.openrewrite.cobol.format;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Incubating;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.CobolPrinterUtils;
import org.openrewrite.cobol.search.FindWords;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

@Incubating(since = "0.0")
@EqualsAndHashCode(callSuper = true)
@Value
public class RemoveWords extends CobolIsoVisitor<ExecutionContext> {

    List<Cobol.Word> removeWords;

    public RemoveWords(Cobol tree) {
        this.removeWords = FindWords.find(tree);
    }

    public RemoveWords(List<Cobol.Word> removeWords) {
        this.removeWords = removeWords;
    }

    @Override
    public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
        Cobol.Word w = super.visitWord(word, executionContext);
        if (removeWords.contains(w) && !w.getWord().trim().isEmpty()) {
            Replace replace = null;
            Copy copy = null;
            Continuation continuation = null;

            for (Marker marker : w.getMarkers().getMarkers()) {
                if (marker instanceof Copy) {
                    copy = (Copy) marker;
                } else if (marker instanceof Replace) {
                    replace = (Replace) marker;
                } else if (marker instanceof Continuation) {
                    continuation = (Continuation) marker;
                }
            }

            if (copy != null || replace != null) {
                // The NIST test does not provide examples of these types of transformation, so it hasn't been implemented yet.
                throw new UnsupportedOperationException("RemoveWords does not support changes on copied sources or replaced words.");
            }

            if (continuation != null) {
                Map<Integer, Markers> continuations = new HashMap<>();
                AtomicBoolean changed = new AtomicBoolean(false);
                for (Map.Entry<Integer, Markers> entry : continuation.getContinuations().entrySet()) {
                    Markers newMarkers = entry.getValue().withMarkers(ListUtils.map(entry.getValue().getMarkers(), it -> {
                        if (it instanceof IndicatorArea && "-".equals(((IndicatorArea) it).getIndicator())) {
                            it = ((IndicatorArea) it).withIndicator(" ");
                            changed.set(true);
                        }
                        return it;
                    }));
                    continuations.put(entry.getKey(), newMarkers);
                }

                if (changed.get()) {
                    continuation = continuation.withContinuations(continuations);
                    w = w.withMarkers(w.getMarkers().removeByType(Continuation.class).add(continuation));
                }
            }
            w = w.withWord(CobolPrinterUtils.fillArea(' ', w.getWord().length()));
        }
        return w;
    }
}
