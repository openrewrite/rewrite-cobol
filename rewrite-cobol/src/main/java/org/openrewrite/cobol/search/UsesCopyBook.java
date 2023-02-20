package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.SearchResult;

import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Value
public class UsesCopyBook extends CobolIsoVisitor<ExecutionContext> {

    @Nullable
    String copyBookName;

    public UsesCopyBook(@Nullable String copyBookName) {
        this.copyBookName = copyBookName;
    }

    @Override
    public Cobol.CompilationUnit visitCompilationUnit(Cobol.CompilationUnit compilationUnit, ExecutionContext executionContext) {
        Cobol.CompilationUnit cu = compilationUnit;
        if (FindCopySource.find(cu, copyBookName) != null) {
            cu = SearchResult.found(cu);
        }
        return cu;
    }

    private static class FindCopySource extends CobolIsoVisitor<ExecutionContext> {

        @Nullable
        public static Cobol.Preprocessor.CopySource find(Cobol cobol, @Nullable String bookName) {
            CobolIsoVisitor<List<Cobol.Preprocessor.CopySource>> visitor = new CobolIsoVisitor<List<Cobol.Preprocessor.CopySource>>() {
                @Override
                public Cobol.Word visitWord(Cobol.Word word, List<Cobol.Preprocessor.CopySource> copySources) {
                    Cobol.Word w = super.visitWord(word, copySources);
                    if (copySources.isEmpty()) {
                        if (w.getCopyStatement() != null && (bookName == null || bookName.equals(w.getCopyStatement().getCopySource().getName().getWord()))) {
                            copySources.add(w.getCopyStatement().getCopySource());
                        }
                    }
                    return w;
                }
            };

            List<Cobol.Preprocessor.CopySource> result = new ArrayList<>(1);
            visitor.visit(cobol, result);
            return result.isEmpty() ? null : result.get(0);
        }
    }
}
