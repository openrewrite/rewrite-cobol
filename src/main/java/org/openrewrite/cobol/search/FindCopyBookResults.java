package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.Copy;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindCopyBookResults extends Recipe {

    @Option(displayName = "Copy book name",
            description = "The copy book name to search for.",
            example = "KP008")
    String copyBookName;

    @Override
    public String getDisplayName() {
        return "Find COBOL copy by copy book name.";
    }

    @Override
    public String getDescription() {
        return "Search for a copy book by name and find the produced COBOL code after preprocessing.";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getSingleSourceApplicableTest() {
        return new UsesCopyBook(copyBookName);
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new MarkCopyBook(copyBookName);
    }

    private static class MarkCopyBook extends CobolIsoVisitor<ExecutionContext> {
        private final String bookName;

        public MarkCopyBook(String bookName) {
            this.bookName = bookName;
        }

        @Override
        public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
            Cobol.Word w = super.visitWord(word, executionContext);
            Copy copy = w.getMarkers().findFirst(Copy.class).orElse(null);
            if (copy != null && bookName.equals(copy.getOriginalStatement().getCopySource().getName().getWord())) {
                boolean isMarked = w.getMarkers().getMarkers().stream()
                        .anyMatch(it -> it instanceof SearchResult && ((SearchResult) it).getType() == SearchResult.Type.COPIED_SOURCE);

                if (!isMarked) {
                    w = w.withMarkers(w.getMarkers().addIfAbsent(new SearchResult(Tree.randomId(), SearchResult.Type.COPIED_SOURCE, null)));
                }
            }
            return w;
        }
    }
}
