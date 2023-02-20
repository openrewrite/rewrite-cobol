package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Option;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.SearchResult;

import java.util.*;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindCopyBookReferences extends Recipe {

    @Option(displayName = "Copy book name",
            description = "The copy book name to search for. If not provided, all copy statements will be returned.",
            example = "KP008",
            required = false)
    @Nullable
    String copyBookName;

    @Override
    public String getDisplayName() {
        return "Find references to a copy book by name";
    }

    @Override
    public String getDescription() {
        return "Find all copy statements with the copybook name.";
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
        private final Map<UUID, UUID> copyIds = new HashMap<>();

        public MarkCopyBook(@Nullable String bookName) {
            this.bookName = bookName;
        }

        @Override
        public Cobol.Preprocessor.CopyStatement visitCopyStatement(Cobol.Preprocessor.CopyStatement copyStatement, ExecutionContext executionContext) {
            Cobol.Preprocessor.CopyStatement c = super.visitCopyStatement(copyStatement, executionContext);
            if (!copyIds.containsKey(copyStatement.getId())) {
                if (bookName == null || bookName.equals(c.getCopySource().getName().getWord())) {
                    c = c.withCopySource(c.getCopySource().withName(SearchResult.found(c.getCopySource().getName(), null)));
                    copyIds.put(copyStatement.getId(), c.getId());
                }
            } else {
                return c.withId(copyIds.get(copyStatement.getId()));
            }
            return c;
        }
    }
}
