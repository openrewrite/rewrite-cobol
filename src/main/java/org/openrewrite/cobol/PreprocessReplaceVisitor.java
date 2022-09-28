package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.cobol.tree.Replace;
import org.openrewrite.cobol.tree.ReplaceTypeReductive;
import org.openrewrite.internal.ListUtils;

import java.util.*;
import java.util.stream.Collectors;

import static org.openrewrite.Tree.randomId;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessReplaceVisitor<P> extends CobolPreprocessorIsoVisitor<P> {

    @Override
    public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, P p) {
        CobolPreprocessor.CopyStatement c = super.visitCopyStatement(copyStatement, p);

        if (c.getCopyBook() != null) {
            List<CobolPreprocessor.ReplacingPhrase> phrases = c.getCobols().stream()
                    .filter(is -> is instanceof CobolPreprocessor.ReplacingPhrase)
                    .map(it -> (CobolPreprocessor.ReplacingPhrase) it)
                    .collect(Collectors.toList());

            if (!phrases.isEmpty()) {
                Map<List<String>, List<String>> replacements = new HashMap<>();
                phrases.forEach(it -> replacements.putAll(getReplacings(it)));

                for (Map.Entry<List<String>, List<String>> entry : replacements.entrySet()) {
                    List<List<CobolPreprocessor.Word>> replaceWords = new ArrayList<>();
                    FindReplaceableAreasVisitor findReplaceableAreasVisitor = new FindReplaceableAreasVisitor(entry.getKey());

                    //noinspection ConstantConditions
                    CobolPreprocessor preprocessor = c.getCopyBook().getAst();
                    findReplaceableAreasVisitor.visit(preprocessor, replaceWords);

                    if (!replaceWords.isEmpty()) {
                        ReplaceVisitor replaceVisitor = new ReplaceVisitor(replaceWords, entry.getValue());
                        preprocessor = replaceVisitor.visit(preprocessor, new InMemoryExecutionContext(), getCursor());
                        c = c.withCopyBook(c.getCopyBook().withAst(preprocessor));
                    }
                }
            }
        }
        return c;
    }

    @Override
    public CobolPreprocessor.ReplaceArea visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, P p) {
        CobolPreprocessor.ReplaceArea r = super.visitReplaceArea(replaceArea, p);

        // Unknown:
        // The CobolPreprocessor grammar does not allow a `replaceArea` in a `replaceArea`.
        // However, a `replaceArea` may contain a `copyStatement`, and the `copyStatement` may contain a `replaceArea`.
        // So, it might be possible for multiple replacements rules to be applied in a replaceArea.
        Map<List<String>, List<String>> replacements = getReplacements(replaceArea.getReplaceByStatement());
        for (Map.Entry<List<String>, List<String>> entry : replacements.entrySet()) {
            if (entry.getKey().isEmpty()) {
                System.out.println();
            } else if (entry.getValue().isEmpty()) {
                System.out.println();
            }
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
                    replacements.clear();
                    fromPos = 0;
                }
            }

            return super.visitWord(word, words);
        }
    }

    private static class ReplaceVisitor extends CobolPreprocessorIsoVisitor<ExecutionContext> {
        // A replacement rule may match multiple sets of words, but will be changed to 1 output.
        private final List<List<CobolPreprocessor.Word>> from;
        private final List<String> to;
        private final ReplacementType replacementType;

        private List<CobolPreprocessor.Word> current;
        private List<Replace> reductiveReplaces;
        private int fromPos = 0;
        private int toPos = 0;
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
            // Detection of the first word using `contains` may be reduced from O(n) rather than O(1) with a set.
            if (ReplacementType.SINGLE_WORD == replacementType) {
                if (from.stream().anyMatch(it -> it.contains(finalWord))) {
                    boolean isEmpty = to.get(0).isEmpty();
                    Replace replace = new Replace(randomId(), word, isEmpty);
                    word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                    word = word.withWord(to.get(fromPos));
                }
            } else if (ReplacementType.EQUAL == replacementType) {
                if (!inMatch) {
                    Optional<List<CobolPreprocessor.Word>> firstMatch = from.stream().filter(it -> it.get(0) == finalWord).findAny();
                    if (firstMatch.isPresent()) {
                        inMatch = true;
                        current = firstMatch.get();
                        fromPos = 0;
                        toPos = 0;

                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        if (!current.get(fromPos).getWord().equals(to.get(toPos))) {
                            boolean isEmpty = to.get(toPos).isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);
                            word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                            word = word.withWord(to.get(toPos));
                        }
                        fromPos++;
                        toPos++;
                    }
                } else {
                    boolean isSame = current.get(fromPos).getWord().equals(word.getWord());
                    if (isSame) {
                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        if (!current.get(fromPos).getWord().equals(to.get(toPos))) {
                            boolean isEmpty = to.get(toPos).isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);
                            word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                            word = word.withWord(to.get(toPos));
                        }

                        if (current.size() - 1 == fromPos) {
                            inMatch = false;
                            current = null;
                            fromPos = 0;
                            toPos = 0;
                        } else {
                            fromPos++;
                            toPos++;
                        }
                    } else {
                        throw new IllegalStateException("Fix me, this should not have happened.");
                    }
                }
            } else if (ReplacementType.REDUCTIVE == replacementType) {
                if (!inMatch) {
                    Optional<List<CobolPreprocessor.Word>> firstMatch = from.stream().filter(it -> it.get(0) == finalWord).findAny();
                    if (firstMatch.isPresent()) {
                        inMatch = true;
                        current = firstMatch.get();
                        fromPos = 0;
                        toPos = 0;

                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        if (!current.get(fromPos).getWord().equals(to.get(toPos))) {
                            boolean isEmpty = to.get(toPos).isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);
                            if (isEmpty) {
                                reductiveReplaces.add(replace);
                                ReplaceTypeReductive replaceTypeReductive = new ReplaceTypeReductive(randomId(), reductiveReplaces);
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replaceTypeReductive));
                            } else {
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                            }
                            word = word.withWord(to.get(toPos));
                        }
                        fromPos++;
                        toPos++;
                    }
                } else {
                    boolean isSame = current.get(fromPos).getWord().equals(word.getWord());
                    if (isSame) {
                        if (toPos >= to.size()) {
                            Replace replace = new Replace(randomId(), word, true);
                            reductiveReplaces.add(replace);
                            ReplaceTypeReductive replaceTypeReductive = new ReplaceTypeReductive(randomId(), reductiveReplaces);
                            word = word.withMarkers(word.getMarkers().addIfAbsent(replaceTypeReductive));
                            word = word.withWord("");
                        }

                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        else if (!current.get(fromPos).getWord().equals(to.get(toPos))) {
                            boolean isEmpty = to.get(toPos).isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);
                            if (isEmpty) {
                                reductiveReplaces.add(replace);
                                ReplaceTypeReductive replaceTypeReductive = new ReplaceTypeReductive(randomId(), reductiveReplaces);
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replaceTypeReductive));
                            } else {
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                            }
                            word = word.withWord(to.get(toPos));
                        }

                        if (current.size() - 1 == fromPos) {
                            inMatch = false;
                            current = null;
                            fromPos = 0;
                            toPos = 0;
                            reductiveReplaces = new ArrayList<>();
                        } else {
                            fromPos++;
                            toPos++;
                        }
                    } else {
                        throw new IllegalStateException("Fix me, this should not have happened.");
                    }
                }
            } else {
                // ReplacementType.ADDITIVE do not exist in the NIST test, but might be possible in COBOL.
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
                    reductiveReplaces = new ArrayList<>();
                    return ReplacementType.REDUCTIVE;
                }
            }
            return ReplacementType.UNKNOWN;
        }

        public enum ReplacementType {
            // Single word changes are isolated for simplicity. I.E. PIC => PICTURE.
            SINGLE_WORD,
            // A multi-word replacement of equal size. I.E. MOVE "*" AO WRK-XN-00001. => MOVE "*" TO WRK-XN-00001.
            EQUAL,
            // A reduction of words. I.E. PERFORM FAIL. => ""
            REDUCTIVE,
            // An addition of words. I.E. TO => PERFORM FAIL.
            ADDITIVE,
            UNKNOWN
        }
    }

    // Collect ReplaceClauses from CopyStatement Replacing.
    @SuppressWarnings("SpellCheckingInspection")
    private Map<List<String>, List<String>> getReplacings(CobolPreprocessor.ReplacingPhrase replacingPhrase) {
        Map<List<String>, List<String>> replacements = new HashMap<>();
        for (CobolPreprocessor.ReplaceClause clause : replacingPhrase.getClauses()) {
            List<String> replaceable = resolveReplace(clause.getReplaceable());
            List<String> replacement = resolveReplace(clause.getReplacement());
            if (clause.getReplacement() instanceof CobolPreprocessor.CharDataLine) {
                return replacements;
            }
            if (!replaceable.isEmpty()) {
                replacements.put(replaceable, replacement);
            }
        }
        return replacements;
    }

    // Collect ReplaceClauses from ReplaceByStatement.
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
            CobolPreprocessor.CharDataLine charDataLine = (CobolPreprocessor.CharDataLine) cobolPreprocessor;
            wordVisitor.visit(charDataLine, words);
        } else if (cobolPreprocessor instanceof CobolPreprocessor.Word) {
            CobolPreprocessor.Word word = (CobolPreprocessor.Word) cobolPreprocessor;
            wordVisitor.visit(word, words);
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
