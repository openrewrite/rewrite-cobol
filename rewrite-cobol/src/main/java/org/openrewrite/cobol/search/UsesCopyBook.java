package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.markers.Copy;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.internal.lang.Nullable;

import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Value
public class UsesCopyBook extends CobolIsoVisitor<ExecutionContext> {

    String copyBookName;

    public UsesCopyBook(String copyBookName) {
        this.copyBookName = copyBookName;
    }

    @Override
    public Cobol.CompilationUnit visitCompilationUnit(Cobol.CompilationUnit compilationUnit, ExecutionContext executionContext) {
        Cobol.CompilationUnit cu = compilationUnit;
        if (FindCopyBook.find(cu, copyBookName) != null) {
            cu = cu.withMarkers(cu.getMarkers().searchResult());
        }
        return cu;
    }

    private static class FindCopySource extends CobolIsoVisitor<ExecutionContext> {

        @Nullable
        public static CobolPreprocessor.CopySource find(Cobol cobol, String bookName) {
            CobolIsoVisitor<List<CobolPreprocessor.CopySource>> visitor = new CobolIsoVisitor<List<CobolPreprocessor.CopySource>>() {
                @Override
                public Cobol.Word visitWord(Cobol.Word word, List<CobolPreprocessor.CopySource> copySources) {
                    Cobol.Word w = super.visitWord(word, copySources);
                    if (copySources.isEmpty()) {
                        Copy copy = w.getMarkers().findFirst(Copy.class).orElse(null);
                        if (copy != null && bookName.equals(copy.getOriginalStatement().getCopySource().getName().getWord())) {
                            copySources.add(copy.getOriginalStatement().getCopySource());
                        }
                    }
                    return w;
                }
            };

            List<CobolPreprocessor.CopySource> result = new ArrayList<>(1);
            visitor.visit(cobol, result);
            return result.isEmpty() ? null : result.get(0);
        }
    }

    private static class FindCopyBook extends CobolIsoVisitor<ExecutionContext> {

        @Nullable
        public static CobolPreprocessor.CopyBook find(Cobol cobol, String bookName) {
            CobolIsoVisitor<List<CobolPreprocessor.CopyBook>> visitor = new CobolIsoVisitor<List<CobolPreprocessor.CopyBook>>() {
                @Override
                public Cobol.Word visitWord(Cobol.Word word, List<CobolPreprocessor.CopyBook> copyBooks) {
                    Cobol.Word w = super.visitWord(word, copyBooks);
                    if (copyBooks.isEmpty()) {
                        Copy copy = w.getMarkers().findFirst(Copy.class).orElse(null);
                        if (copy != null &&
                                bookName.equals(copy.getOriginalStatement().getCopySource().getName().getWord()) &&
                                copy.getOriginalStatement().getCopyBook() != null) {

                            copyBooks.add(copy.getOriginalStatement().getCopyBook());
                        }
                    }
                    return w;
                }
            };

            List<CobolPreprocessor.CopyBook> result = new ArrayList<>(1);
            visitor.visit(cobol, result);
            return result.isEmpty() ? null : result.get(0);
        }
    }
}
