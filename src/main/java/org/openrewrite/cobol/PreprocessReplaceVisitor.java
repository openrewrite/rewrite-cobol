package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.cobol.tree.Space;
import org.openrewrite.internal.ListUtils;

import java.util.*;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessReplaceVisitor<P> extends CobolPreprocessorIsoVisitor<P> {

    @Override
    public CobolPreprocessor.ReplaceArea visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, P p) {
        CobolPreprocessor.ReplaceArea r = super.visitReplaceArea(replaceArea, p);

        Map<List<String>, List<String>> replacements = getReplacements(replaceArea.getReplaceByStatement());
        for (Map.Entry<List<String>, List<String>> entry : replacements.entrySet()) {
            List<List<CobolPreprocessor.Word>> replaceWords = new ArrayList<>();
            FindReplaceableAreasVisitor findReplaceableAreasVisitor = new FindReplaceableAreasVisitor(entry.getKey());
            ListUtils.map(r.getCobols(), it -> findReplaceableAreasVisitor.visit(it, replaceWords, getCursor()));

            if (!replaceWords.isEmpty()) {
                ReplaceVisitor replaceVisitor = new ReplaceVisitor(replaceWords, entry.getValue());
                r = r.withCobols(ListUtils.map(r.getCobols(), it -> replaceVisitor.visit(it, new InMemoryExecutionContext(), getCursor())));
            }
        }

        return r;
    }

    private static class FindReplaceableAreasVisitor extends CobolPreprocessorIsoVisitor<List<List<CobolPreprocessor.Word>>> {
        private final List<String> from;
        private final List<CobolPreprocessor.Word> replacements;

        boolean inMatch = false;
        private int fromPos = 0;

        public FindReplaceableAreasVisitor(List<String> from) {
            this.from = from;
            this.replacements = new ArrayList<>();
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, List<List<CobolPreprocessor.Word>> words) {
            if (!inMatch && word.getWord().equals(from.get(0))) {
                // Reset match.
                fromPos = 0;
                replacements.add(word);

                if (from.size() == 1) {
                    words.add(new ArrayList<>(replacements));
                    replacements.clear();
                } else {
                    inMatch = true;
                    fromPos++;
                }
            } else if (inMatch) {
                if (word.getWord().equals(from.get(fromPos))) {
                    replacements.add(word);
                    if (from.size() - 1 == fromPos) {
                        words.add(new ArrayList<>(replacements));

                        inMatch = false;
                        fromPos = 0;
                        replacements.clear();
                    } else {
                        fromPos++;
                    }
                } else {
                    inMatch = false;
                    fromPos = 0;
                }
            }

            return super.visitWord(word, words);
        }
    }

    private static class ReplaceVisitor extends CobolPreprocessorIsoVisitor<ExecutionContext> {
        private final List<List<CobolPreprocessor.Word>> from;
        private final List<String> to;
        private final ReplacementType replacementType;

        private List<CobolPreprocessor.Word> current;
        private int pos = 0;
        boolean inMatch = false;

        public ReplaceVisitor(List<List<CobolPreprocessor.Word>> from,
                              List<String> to) {
            this.from = from;
            this.to = to;
            this.replacementType = init();
        }

        @Override
        public CobolPreprocessor.CopyBook visitCopyBook(CobolPreprocessor.CopyBook copyBook, ExecutionContext executionContext) {
            copyBook = copyBook.withAst(visit(copyBook.getAst(), executionContext));
            return copyBook;
        }

        @Override
        public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, ExecutionContext executionContext) {
            if (copyStatement.getCopyBook() != null ) {
                copyStatement = copyStatement.withCopyBook(visitCopyBook(copyStatement.getCopyBook(), executionContext));
            }
            return copyStatement;
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
            CobolPreprocessor.Word finalWord = word;
            // TODO:
            //  - the preserved words may not be the same length and cause column miss-alignments.
            //  - This is especially important for continuations.
            if (ReplacementType.SINGLE_WORD == replacementType) {
                if (from.stream().anyMatch(it -> it.contains(finalWord))) {
                    if (to.get(0).isEmpty()) {
                        // The word isn't removed from the AST to preserve markers.
                        word = word.withPrefix(Space.EMPTY);
                        word = word.withWord("");
                    } else {
                        word = word.withWord(to.get(0));
                    }
                }
            } else if (ReplacementType.EQUAL == replacementType) {
                if (!inMatch) {
                    Optional<List<CobolPreprocessor.Word>> firstMatch = from.stream().filter(it -> it.get(0) == finalWord).findAny();
                    if (firstMatch.isPresent()) {
                        inMatch = true;
                        current = firstMatch.get();
                        pos = 0;

                        if (!current.get(pos).getWord().equals(to.get(pos))) {
                            word = word.withWord(to.get(pos));
                        }
                        pos++;
                    }
                } else {
                    boolean isSame = current.get(pos).getWord().equals(word.getWord());
                    if (isSame) {
                        if (!current.get(pos).getWord().equals(to.get(pos))) {
                            word = word.withWord(to.get(pos));
                        }

                        if (current.size() - 1 == pos) {
                            inMatch = false;
                            current = null;
                            pos = 0;
                        } else {
                            pos++;
                        }
                    } else {
                        throw new IllegalStateException("Fix me, this should not have happened.");
                    }
                }
            } else {
                // ReplacementType.ADDITIVE and ReplacementType.REDUCTIVE do not exist in the NIST test, but might be possible in COBOL.
                throw new UnsupportedOperationException("Unsupported ReplacementType detected: " + replacementType.name());
            }

            return super.visitWord(word, executionContext);
        }

        private ReplacementType init() {
            for (List<CobolPreprocessor.Word> words : from) {
                if (words.size() == 1 && to.size() == 1) {
                    return ReplacementType.SINGLE_WORD;
                } else if (!words.isEmpty() && words.size() == to.size()) {
                    return ReplacementType.EQUAL;
                } else if (words.size() < to.size()) {
                    return ReplacementType.ADDITIVE;
                } else if (words.size() > from.size()) {
                    return ReplacementType.REDUCTIVE;
                }
            }
            return ReplacementType.UNKNOWN;
        }

        enum ReplacementType {
            SINGLE_WORD, REDUCTIVE, ADDITIVE, EQUAL, UNKNOWN
        }
    }

    private Map<List<String>, List<String>> getReplacements(CobolPreprocessor.ReplaceByStatement replaceByStatement) {
        Map<List<String>, List<String>> replacements = new HashMap<>();
        for (CobolPreprocessor.ReplaceClause clause : replaceByStatement.getClauses()) {
            List<String> replaceable = resolveReplace(clause.getReplaceable());
            List<String> replacement = resolveReplace(clause.getReplacement());
            if (!replaceable.isEmpty()) {
                replacements.put(replaceable, replacement);
            }
        }
        return replacements;
    }

    private List<String> resolveReplace(CobolPreprocessor cobolPreprocessor) {
        List<String> words = new ArrayList<>();
        CobolPreprocessorWordVisitor wordVisitor = new CobolPreprocessorWordVisitor();

        if (cobolPreprocessor instanceof CobolPreprocessor.PseudoText) {
            CobolPreprocessor.PseudoText pseudoText = (CobolPreprocessor.PseudoText) cobolPreprocessor;
            if (pseudoText.getCharData() != null) {
                wordVisitor.visit(pseudoText.getCharData(), words);
            } else {
                words.add("");
            }
        } else if (cobolPreprocessor instanceof CobolPreprocessor.CharDataLine) {
            throw new UnsupportedOperationException("Implement me.");
        } else if (cobolPreprocessor instanceof CobolPreprocessor.Word) {
            throw new UnsupportedOperationException("Implement me.");
        } else {
            throw new UnsupportedOperationException("Implement me.");
        }
        return words;
    }

    private static class CobolPreprocessorWordVisitor extends CobolPreprocessorIsoVisitor<List<String>> {
        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, List<String> words) {
            words.add(word.getWord());
            return super.visitWord(word, words);
        }
    }
}
