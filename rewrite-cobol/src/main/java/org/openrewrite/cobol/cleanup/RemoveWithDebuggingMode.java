package org.openrewrite.cobol.cleanup;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.format.RemoveWords;
import org.openrewrite.cobol.format.ShiftSequenceAreas;
import org.openrewrite.cobol.markers.*;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Set;

@EqualsAndHashCode(callSuper = true)
@Value
public class RemoveWithDebuggingMode extends Recipe {

    @Override
    public String getDisplayName() {
        return "Remove with debugging mode";
    }

    @Override
    public String getDescription() {
        return "Remove debugging mode from SOURCE-COMPUTER paragraphs.";
    }

    @Override
    public Set<String> getTags() {
        return Collections.singleton("RSPEC-4057");
    }

    @Override
    public Duration getEstimatedEffortPerOccurrence() {
        return Duration.ofMinutes(1_000_000);
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new CobolIsoVisitor<ExecutionContext>() {

            @Nullable
            private Cobol.Word endWord = null;

            @Override
            public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
                Cobol.Word w = super.visitWord(word, executionContext);
                if (endWord != null) {
                    Cursor parent = getCursor().getParent();
                    // This covers an unlikely case, and requires the cursor to have access to the CU.
                    // Removes whitespace from the EOF on the CU if the SourceComputerDefinition is the last COBOL.
                    if (parent != null && parent.getValue() instanceof Cobol.CompilationUnit && ((Cobol.CompilationUnit) parent.getValue()).getEof() == word) {
                        w = w.withPrefix(Space.EMPTY);
                    }
                    endWord = null;
                }
                return w;
            }

            @Override
            public Cobol.SourceComputerDefinition visitSourceComputerDefinition(Cobol.SourceComputerDefinition sourceComputerDefinition,
                                                                                ExecutionContext executionContext) {
                Cobol.SourceComputerDefinition s = super.visitSourceComputerDefinition(sourceComputerDefinition, executionContext);

                if (s.getDebuggingMode() != null) {
                    // Do not change copied or replaced code until the transformations are understood.
                    boolean isSafe = s.getDebuggingMode().stream().anyMatch(it ->
                            it.getMarkers().getMarkers().stream().noneMatch(m ->
                                    m instanceof Copy ||
                                            m instanceof Replace ||
                                            m instanceof ReplaceAdditiveType ||
                                            m instanceof ReplaceReductiveType)
                    );

                    if (isSafe && !s.getDebuggingMode().isEmpty()) {
                        CommentArea commentArea = s.getComputerName().getMarkers().findFirst(CommentArea.class).orElse(null);
                        if (commentArea != null && commentArea.getPrefix().getWhitespace().length() > 0) {
                            List<Cobol.Word> originalWords = s.getDebuggingMode();

                            Markers newMarkers = s.getDot().getMarkers()
                                    .removeByType(SequenceArea.class)
                                    .removeByType(IndicatorArea.class)
                                    .removeByType(CommentArea.class);

                            commentArea = commentArea.withPrefix(
                                    commentArea.getPrefix().withWhitespace(
                                            commentArea.getPrefix().getWhitespace().substring(1)));
                            newMarkers = newMarkers.addIfAbsent(commentArea);

                            s = s.withDot(s.getDot().withMarkers(newMarkers));
                            s = s.withComputerName(s.getComputerName().withMarkers(
                                    s.getComputerName().getMarkers().removeByType(CommentArea.class)));

                            Cobol.Word startWord = s.getComputerName();
                            doAfterVisit(new ShiftSequenceAreas(originalWords, startWord));

                            endWord = s.getDot();
                            s = s.withDebuggingMode(null);
                        } else {
                            // Interim safe replace until we have auto-formatting.
                            s = new RemoveWords(s.getDebuggingMode()).visitSourceComputerDefinition(s, executionContext);
                        }
                    }
                }
                return s;
            }
        };
    }
}

