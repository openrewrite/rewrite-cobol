package org.openrewrite.cobol.cleanup;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.format.RemoveWords;
import org.openrewrite.cobol.format.ShiftSequenceAreas;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.lang.Nullable;

import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Set;

@EqualsAndHashCode(callSuper = true)
@Value
public class RemoveWithDebuggingMode extends Recipe {

    @Option(displayName = "Update sequence areas",
            description = "When set to `true` the existing sequence are updated to preserve ordering. " +
                    "This is default to false, and is used to prevent large diffs since COBOL has a line limit of 999k.",
            example = "true",
            required = false)
    @Nullable
    Boolean updateSequenceAreas;

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
                    boolean isSupported = s.getDebuggingMode().stream().noneMatch(it ->
                            it.getCopyStatement() != null || it.getReplacement() != null);

                    if (isSupported) {
                        if (s.getComputerName().getCommentArea() != null && !s.getComputerName().getCommentArea().getPrefix().getWhitespace().isEmpty()) {
                            List<Cobol.Word> originalWords = s.getDebuggingMode();
                            CommentArea commentArea = s.getComputerName().getCommentArea();
                            commentArea = commentArea.withPrefix(
                                    commentArea.getPrefix().withWhitespace(
                                            commentArea.getPrefix().getWhitespace().substring(1)));
                            s = s.withDot(s.getDot().withCommentArea(commentArea));
                            s = s.withComputerName(s.getComputerName().withCommentArea(null));

                            Cobol.Word startWord = s.getComputerName();
                            if (Boolean.TRUE.equals(updateSequenceAreas)) {
                                doAfterVisit(new ShiftSequenceAreas(originalWords, startWord));
                            }

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

