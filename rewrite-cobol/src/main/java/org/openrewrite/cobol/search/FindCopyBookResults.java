package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.Copy;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.SearchResult;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindCopyBookResults extends Recipe {

    @Option(displayName = "Copy book name",
            description = "The copy book name to search for.",
            example = "KP008",
            required = false)
    @Nullable
    String copyBookName;

    @Override
    public String getDisplayName() {
        return "Find COBOL copy by copy book name";
    }

    @Override
    public String getDescription() {
        return "Search for a copy book by name and find the produced COBOL code after preprocessing. The recipe will mark all copied sources if a book name is not provided.";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getSingleSourceApplicableTest() {
        return copyBookName == null ? null : new UsesCopyBook(copyBookName);
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new MarkCopyBook(copyBookName);
    }

    private static class MarkCopyBook extends CobolIsoVisitor<ExecutionContext> {
        private final String bookName;

        public MarkCopyBook(@Nullable String bookName) {
            this.bookName = bookName;
        }

        @Override
        public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
            Cobol.Word w = super.visitWord(word, executionContext);
            Copy copy = w.getMarkers().findFirst(Copy.class).orElse(null);
            if (copy != null && (bookName == null || bookName.equals(copy.getOriginalStatement().getCopySource().getName().getWord()))) {
                boolean isMarked = w.getMarkers().getMarkers().stream()
                        .anyMatch(it -> it instanceof CobolSearchResult && ((CobolSearchResult) it).getType() == CobolSearchResult.Type.COPIED_SOURCE);

                if (!isMarked) {
                    SearchResult.found(w);
                }
            }
            return w;
        }
    }
}
