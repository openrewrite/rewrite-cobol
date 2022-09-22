package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.tree.*;

import java.util.*;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessWordContinuations extends Recipe {

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
                        // The previous 2 values are required because the CobolProcessor.g4 considers `-` a token.
                        private CobolPreprocessor.Word previous2 = null;
                        private CobolPreprocessor.Word previous = null;

                        private CobolPreprocessor.Word currentKey = null;
                        private boolean inContinuation = false;

                        @Override
                        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords) {
                            Optional<IndicatorArea> indicatorArea = word.getMarkers().findFirst(IndicatorArea.class);
                            if (indicatorArea.isPresent() && "-".equals(indicatorArea.get().getIndicator())) {
                                // Special case, because CobolPreprocessor creates a Word for `-` characters.
                                if (previous != null && "-".equals(previous.getWord()) &&
                                    // The continued `-` is a part of the previous word. I.E. CONT-B.
                                    // Otherwise, the `-` is something like `VALUE -10`.
                                    previous.getPrefix() == Space.EMPTY && previous2 != null) {
                                    currentKey = previous2;
                                    inContinuation = true;
                                    List<CobolPreprocessor.Word> words = continuedWords.computeIfAbsent(currentKey, key -> new ArrayList<>());
                                    words.add(previous);
                                    words.add(word);
                                } else {
                                    currentKey = previous;
                                    inContinuation = true;
                                    List<CobolPreprocessor.Word> words = continuedWords.computeIfAbsent(currentKey, key -> new ArrayList<>());
                                    words.add(word);
                                }
                            } else if (inContinuation) {
                                if (word.getPrefix() != Space.EMPTY || ".".equals(word.getWord())) {
                                    inContinuation = false;
                                } else {
                                    List<CobolPreprocessor.Word> words = continuedWords.computeIfAbsent(currentKey, key -> new ArrayList<>());
                                    words.add(word);
                                }
                            }

                            previous2 = previous;
                            previous = word;
                            return super.visitWord(word, continuedWords);
                        }
            };

            Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> orderedWords = new LinkedHashMap<>();
            visitor.visit(compilationUnit, orderedWords);
            return orderedWords;
        }
    }
}
