package org.openrewrite.cobol.format;

import lombok.EqualsAndHashCode;
import lombok.Value;
import lombok.experimental.NonFinal;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Incubating;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.markers.SequenceArea;
import org.openrewrite.marker.Marker;

import java.util.*;
import java.util.stream.Collectors;

@Incubating(since = "0.0")
@EqualsAndHashCode(callSuper = true)
@Value
public class ShiftSequenceAreas extends CobolIsoVisitor<ExecutionContext> {

    LinkedList<SequenceArea> originalSequenceAreas;
    Cobol.Word startAfter;

    @NonFinal
    Cobol.Word previousWord = null;

    @NonFinal
    boolean startShift = false;

    public ShiftSequenceAreas(List<Cobol.Word> originalWords,
                              Cobol.Word startAfter) {

        this.originalSequenceAreas = originalWords.stream()
                .flatMap(it ->
                        it.getMarkers().getMarkers().stream()
                                .filter(m -> m instanceof SequenceArea)
                                .map(m -> (SequenceArea) m))
                .collect(Collectors.toCollection(LinkedList::new));
        this.startAfter = startAfter;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <M extends Marker> M visitSequenceArea(SequenceArea sequenceArea, ExecutionContext executionContext) {
        if (startShift) {
            originalSequenceAreas.add(sequenceArea);
            return (M) originalSequenceAreas.removeFirst();
        }
        return (M) sequenceArea;
    }

    @Override
    public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
        Cobol.Word w = super.visitWord(word, executionContext);

        if (previousWord == startAfter) {
            startShift = true;
        }

        previousWord = w;
        return w;
    }
}
