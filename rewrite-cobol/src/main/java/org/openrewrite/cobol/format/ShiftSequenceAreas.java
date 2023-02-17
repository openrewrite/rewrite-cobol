package org.openrewrite.cobol.format;

import lombok.EqualsAndHashCode;
import lombok.Value;
import lombok.experimental.NonFinal;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Incubating;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;

import java.util.*;
import java.util.stream.Collectors;

@Incubating(since = "0.0")
@EqualsAndHashCode(callSuper = true)
@Value
public class ShiftSequenceAreas extends CobolIsoVisitor<ExecutionContext> {

    LinkedList<Cobol.SequenceArea> originalSequenceAreas;
    Cobol.Word startAfter;

    @NonFinal
    Cobol.Word previousWord = null;

    @NonFinal
    boolean startShift = false;

    public ShiftSequenceAreas(List<Cobol.Word> originalWords,
                              Cobol.Word startAfter) {

        this.originalSequenceAreas = originalWords.stream()
                .map(Cobol.Word::getSequenceArea)
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(LinkedList::new));
        this.startAfter = startAfter;
    }

    @Override
    public Cobol visitSequenceArea(Cobol.SequenceArea sequenceArea, ExecutionContext executionContext) {
        if (startShift) {
            originalSequenceAreas.add(sequenceArea);
            return originalSequenceAreas.removeFirst();
        }
        return sequenceArea;
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
