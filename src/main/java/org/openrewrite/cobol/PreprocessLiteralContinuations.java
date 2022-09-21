package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.cobol.tree.Continuation;
import org.openrewrite.cobol.tree.IndicatorArea;

import java.util.*;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessLiteralContinuations extends Recipe {

    @Override
    public String getDisplayName() {
        return "";
    }

    @Override
    public String getDescription() {
        return "";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new CobolPreprocessorIsoVisitor<ExecutionContext>() {
            @Override
            public CobolPreprocessor.CompilationUnit visitCompilationUnit(CobolPreprocessor.CompilationUnit compilationUnit, ExecutionContext executionContext) {
                CobolPreprocessor.CompilationUnit cu = super.visitCompilationUnit(compilationUnit, executionContext);

                Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords = ContinuedLines.find(cu);
                return cu;
            }
        };
    }


    private static class ContinuedLines extends CobolPreprocessorIsoVisitor<Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>>> {
        public static Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> find(CobolPreprocessor.CompilationUnit compilationUnit) {
            CobolPreprocessorIsoVisitor<Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>>> visitor =
                    new CobolPreprocessorIsoVisitor<Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>>>() {
                        private CobolPreprocessor.Word previousWord = null;
                        private CobolPreprocessor.Word currentKey = null;
                        private boolean inContinuation = false;

                        @Override
                        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords) {
                            Optional<Continuation> continuedLines = word.getMarkers().findFirst(Continuation.class);
                            if (continuedLines.isPresent()) {
                                System.out.println(word.getWord());
                            }
                            Optional<IndicatorArea> indicatorArea = word.getMarkers().findFirst(IndicatorArea.class);
                            if (indicatorArea.isPresent() && "-".equals(indicatorArea.get().getIndicator())) {
                                if (!inContinuation) {
                                    currentKey = previousWord;
                                    inContinuation = true;
                                }

                                List<CobolPreprocessor.Word> continued = continuedWords.computeIfAbsent(currentKey, k -> new ArrayList<>());
                                continued.add(word);
                            } else {
                                if (inContinuation) {
                                    inContinuation = false;
                                }
                                System.out.println(word.getWord());
                                previousWord = word;
                            }

                            return super.visitWord(word, continuedWords);
                        }
            };

            Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords = new HashMap<>();
            visitor.visit(compilationUnit, continuedWords);
            return continuedWords;
        }
    }
}
