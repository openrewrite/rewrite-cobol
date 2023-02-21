package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.marker.SearchResult;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindAnyCopyStatement extends Recipe {

    @Override
    public String getDisplayName() {
        return "Find any copy statement";
    }

    @Override
    public String getDescription() {
        return "Find any copy statement";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new CobolIsoVisitor<ExecutionContext>() {
            @Override
            public Cobol.Preprocessor.CopyStatement visitCopyStatement(Cobol.Preprocessor.CopyStatement copyStatement, ExecutionContext executionContext) {
                return SearchResult.found(copyStatement, null);
            }
        };
    }
}
