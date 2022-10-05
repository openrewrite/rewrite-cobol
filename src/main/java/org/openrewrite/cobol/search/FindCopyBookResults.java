package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Option;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;

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
        return new CobolIsoVisitor<ExecutionContext>() {
            @Override
            public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
                return super.visitWord(word, executionContext);
            }
        };
    }
}
