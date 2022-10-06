package org.openrewrite.cobol.cleanup;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.CobolPrinterUtils;
import org.openrewrite.cobol.internal.CobolPrinter;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.Continuation;
import org.openrewrite.cobol.tree.IndicatorArea;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.marker.Markers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

@EqualsAndHashCode(callSuper = true)
@Value
public class RemoveWords extends CobolIsoVisitor<ExecutionContext> {

    List<Cobol.Word> removeWords;

    public RemoveWords(List<Cobol.Word> removeWords) {
        this.removeWords = removeWords;
    }

    @Override
    public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
        Cobol.Word w = super.visitWord(word, executionContext);
        if (removeWords.contains(w) && !w.getWord().trim().isEmpty()) {
            Continuation continuation = w.getMarkers().findFirst(Continuation.class).orElse(null);
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

    private static class EraseContinuationIndicators {
        public static Continuation change(Continuation continuation) {
            for (Map.Entry<Integer, Markers> entry : continuation.getContinuations().entrySet()) {
                IndicatorArea indicatorArea = entry.getValue().findFirst(IndicatorArea.class).orElse(null);
                if (indicatorArea != null && "-".equals(indicatorArea.getIndicator())) {
                    indicatorArea = indicatorArea.withIndicator(" ");
                }
            }

            return continuation;
        }
    }
}
