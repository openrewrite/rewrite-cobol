package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Tree;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.*;

import static org.openrewrite.Tree.randomId;

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
                MergeKeywords mergeKeywords = new MergeKeywords(continuedWords);
                cu = (CobolPreprocessor.CompilationUnit) mergeKeywords.visitNonNull(cu, executionContext);

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
                                // A continuation is immediately continued again.
                                if (inContinuation) {
                                    List<CobolPreprocessor.Word> words = continuedWords.computeIfAbsent(currentKey, key -> new ArrayList<>());
                                    words.add(word);
                                }

                                // Special case, because CobolPreprocessor creates a Word for `-` characters.
                                else if (previous != null && "-".equals(previous.getWord()) &&
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

            Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> orderedWords = new HashMap<>();
            visitor.visit(compilationUnit, orderedWords);

            return orderedWords;
        }
    }

    private static class MergeKeywords extends CobolPreprocessorIsoVisitor<ExecutionContext> {
        private final Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords;
        private Map<CobolPreprocessor.Word, CobolPreprocessor.Word> replacements = null;

        public MergeKeywords(Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords) {
            this.continuedWords = continuedWords;
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
            CobolPreprocessor.Word w = super.visitWord(word, executionContext);

            if (continuedWords.containsKey(w)) {
                Map<CobolPreprocessor.Word, CobolPreprocessor.Word> result = getReplacements(w, continuedWords.get(w));
                if (result.containsKey(w)) {
                    w = result.get(w);
                    result.remove(w);
                    this.replacements = result;
                }
            } else if (replacements != null && replacements.containsKey(w)){
                return replacements.get(w);
            }

            return w;
        }

        private Map<CobolPreprocessor.Word, CobolPreprocessor.Word> getReplacements(CobolPreprocessor.Word key, List<CobolPreprocessor.Word> values) {
            Map<CobolPreprocessor.Word, CobolPreprocessor.Word> result = new HashMap<>();
            String concatenateWord = concatenateWords(key, values);

            Map<Integer, Markers> continuations = new HashMap<>();
            Continuation continuedLines = new Continuation(randomId(), continuations);

            CobolPreprocessor.Word newKey = key.withMarkers(key.getMarkers().addIfAbsent(continuedLines));
            List<Marker> continuation = new ArrayList<>(2);

            Optional<SequenceArea> sequenceAreaOptional = newKey.getMarkers().findFirst(SequenceArea.class);
            sequenceAreaOptional.ifPresent(continuation::add);
            newKey = newKey.withMarkers(newKey.getMarkers().removeByType(SequenceArea.class));

            Optional<IndicatorArea> indicatorAreaOptional = newKey.getMarkers().findFirst(IndicatorArea.class);
            indicatorAreaOptional.ifPresent(continuation::add);
            newKey = newKey.withMarkers(newKey.getMarkers().removeByType(IndicatorArea.class));

            int pos = 0;
            if (!continuation.isEmpty()) {
                continuations.put(pos, Markers.build(continuation));
            }

            newKey = newKey.withWord(concatenateWord);
            pos += key.getWord().length();

            boolean isSpecialCase = false;
            if ("-".equals(values.get(0).getWord()) && !key.getMarkers().findFirst(CommentArea.class).isPresent() && values.get(0).getMarkers().findFirst(CommentArea.class).isPresent()) {
                isSpecialCase = true;
                pos++;
            }
            int firstPos = pos;

            newKey = newKey.withMarkers(newKey.getMarkers().removeByType(CommentArea.class));
            result.put(key, newKey);

            for (int i = isSpecialCase ? 1 : 0; i < values.size(); i++) {
                CobolPreprocessor.Word value = values.get(i);
                continuation = new ArrayList<>(3);

                // Add the CommentArea from the original word to align the columns.
                Optional<CommentArea> commentAreaOptional;
                if (pos == firstPos && !isSpecialCase) {
                    commentAreaOptional = key.getMarkers().findFirst(CommentArea.class);
                } else {
                    commentAreaOptional = values.get(i - 1).getMarkers().findFirst(CommentArea.class);
                }
                commentAreaOptional.ifPresent(continuation::add);

                sequenceAreaOptional = value.getMarkers().findFirst(SequenceArea.class);
                sequenceAreaOptional.ifPresent(continuation::add);

                indicatorAreaOptional = value.getMarkers().findFirst(IndicatorArea.class);
                // Move the prefix from the concatenated value to the indicator marker.
                if (indicatorAreaOptional.isPresent()) {
                    IndicatorArea indicatorArea = indicatorAreaOptional.get();
                    indicatorArea = indicatorArea.withContinuationPrefix(value.getPrefix().getWhitespace());
                    continuation.add(indicatorArea);
                }

                if (!continuation.isEmpty()) {
                    continuations.put(pos, Markers.build(continuation));
                }
                pos += value.getWord().length();
                if (isSpecialCase) {
                    isSpecialCase = false;
                }
            }

            Optional<CommentArea> commentAreaOptional = values.get(values.size() - 1).getMarkers().findFirst(CommentArea.class);
            if (commentAreaOptional.isPresent()) {
                continuation = new ArrayList<>(1);
                CommentArea commentArea = commentAreaOptional.get();
                continuation.add(commentArea);
                continuations.put(pos + 1, Markers.build(continuation));
            }
            values.forEach(k -> result.put(k, null));

            return result;
        }

        private String concatenateWords(CobolPreprocessor.Word key, List<CobolPreprocessor.Word> values) {
            StringBuilder word = new StringBuilder();
            word.append(key.getWord());
            values.forEach(w -> word.append(w.getWord()));
            return word.toString();
        }
    }
}
