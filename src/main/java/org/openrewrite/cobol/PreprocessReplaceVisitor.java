package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.marker.Markers;

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
                Map<List<CobolPreprocessor.Word>, List<CobolPreprocessor.Word>> replacements = new LinkedHashMap<>();
                phrases.forEach(it -> replacements.putAll(getReplacings(it)));

                for (Map.Entry<List<CobolPreprocessor.Word>, List<CobolPreprocessor.Word>> entry : replacements.entrySet()) {
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
        Map<List<CobolPreprocessor.Word>, List<CobolPreprocessor.Word>> replacements = getReplacements(replaceArea.getReplaceByStatement());
        for (Map.Entry<List<CobolPreprocessor.Word>, List<CobolPreprocessor.Word>> entry : replacements.entrySet()) {
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
        private final List<CobolPreprocessor.Word> from;
        private final List<CobolPreprocessor.Word> replacements;

        boolean inMatch = false;
        private int fromPos = 0;

        public FindReplaceableAreasVisitor(List<CobolPreprocessor.Word> from) {
            this.from = from;
            this.replacements = new ArrayList<>();
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, List<List<CobolPreprocessor.Word>> words) {
            if (!inMatch && word.getWord().equals(from.get(0).getWord())) {
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
                if (word.getWord().equals(from.get(fromPos).getWord())) {
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
        private final Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> from;
        private final List<CobolPreprocessor.Word> to;
        private final ReplacementType replacementType;

        private List<CobolPreprocessor.Word> current;
        private List<Replace> reductiveReplaces;
        private int fromPos = 0;
        private int toPos = 0;
        boolean inMatch = false;

        public ReplaceVisitor(List<List<CobolPreprocessor.Word>> from,
                              List<CobolPreprocessor.Word> to) {
            this.from = new IdentityHashMap<>(from.size());
            from.forEach(it -> this.from.put(it.get(0), it));

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
            if (ReplacementType.SINGLE_WORD == replacementType) {
                if (from.containsKey(word)) {
                    CobolPreprocessor.Word toWord = to.get(toPos);
                    boolean isEmpty = toWord.getWord().isEmpty();
                    Replace replace = new Replace(randomId(), word, isEmpty);
                    word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                    word = word.withWord(toWord.getWord());
                }
            } else if (ReplacementType.EQUAL == replacementType) {
                if (!inMatch) {
                    if (from.containsKey(word)) {
                        inMatch = true;
                        current = from.get(word);
                        fromPos = 0;
                        toPos = 0;

                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        if (!current.get(fromPos).getWord().equals(to.get(toPos).getWord())) {
                            CobolPreprocessor.Word toWord = to.get(toPos);
                            boolean isEmpty = toWord.getWord().isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);
                            word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                            word = word.withWord(toWord.getWord());
                        }
                        fromPos++;
                        toPos++;
                    }
                } else {
                    boolean isSame = current.get(fromPos).getWord().equals(word.getWord());
                    if (isSame) {
                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        if (!current.get(fromPos).getWord().equals(to.get(toPos).getWord())) {
                            CobolPreprocessor.Word toWord = to.get(toPos);
                            boolean isEmpty = toWord.getWord().isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);
                            word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                            word = word.withWord(toWord.getWord());
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
                    if (from.containsKey(word)) {
                        inMatch = true;
                        current = from.get(word);
                        fromPos = 0;
                        toPos = 0;

                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        if (!current.get(fromPos).getWord().equals(to.get(toPos).getWord())) {
                            CobolPreprocessor.Word toWord = to.get(toPos);
                            boolean isEmpty = toWord.getWord().isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);
                            if (isEmpty) {
                                reductiveReplaces.add(replace);
                                ReplaceReductiveType replaceReductiveType = new ReplaceReductiveType(randomId(), reductiveReplaces);
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replaceReductiveType));
                                word = word.withWord(CobolPrinterUtils.fillArea(' ', word.getWord().length()));
                            } else {
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                                word = word.withWord(toWord.getWord());
                            }

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
                            ReplaceReductiveType replaceReductiveType = new ReplaceReductiveType(randomId(), reductiveReplaces);
                            word = word.withMarkers(word.getMarkers().addIfAbsent(replaceReductiveType));
                            word = word.withWord(CobolPrinterUtils.fillArea(' ', word.getWord().length()));
                        }

                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        else if (!current.get(fromPos).getWord().equals(to.get(toPos).getWord())) {
                            CobolPreprocessor.Word toWord = to.get(toPos);
                            boolean isEmpty = toWord.getWord().isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);
                            if (isEmpty) {
                                reductiveReplaces.add(replace);
                                ReplaceReductiveType replaceReductiveType = new ReplaceReductiveType(randomId(), reductiveReplaces);
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replaceReductiveType));
                                word = word.withWord(CobolPrinterUtils.fillArea(' ', word.getWord().length()));
                            } else {
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                                word = word.withWord(toWord.getWord());
                            }

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
            } else if (ReplacementType.ADDITIVE == replacementType) {
                if (!inMatch) {
                    if (from.containsKey(word)) {
                        inMatch = true;
                        current = from.get(word);
                        fromPos = 0;
                        toPos = 0;

                        // Marks the changed word. Unknown: Should all the words be marked instead??
                        if (!current.get(fromPos).getWord().equals(to.get(toPos).getWord())) {
                            CobolPreprocessor.Word toWord = to.get(toPos);
                            boolean isEmpty = toWord.getWord().isEmpty();
                            Replace replace = new Replace(randomId(), word, isEmpty);

                            word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                            word = word.withWord(toWord.getWord());
                        }
                        fromPos++;
                        toPos++;
                    }
                } else {
                    if (fromPos < current.size()) {
                        boolean isSame = current.get(fromPos).getWord().equals(word.getWord());
                        if (isSame) {
                            // Marks the changed word. Unknown: Should all the words be marked instead??
                            if (!current.get(fromPos).getWord().equals(to.get(toPos).getWord())) {
                                CobolPreprocessor.Word toWord = to.get(toPos);
                                boolean isEmpty = toWord.getWord().isEmpty();
                                Replace replace = new Replace(randomId(), word, isEmpty);
                                word = word.withMarkers(word.getMarkers().addIfAbsent(replace));
                                if (word.getPrefix().isEmpty() && !toWord.getPrefix().isEmpty()) {
                                    // Add the prefix of toWord so that words are separated correctly.
                                    word = word.withPrefix(toWord.getPrefix());
                                }
                                word = word.withWord(toWord.getWord());
                            }
                            fromPos++;
                            toPos++;
                        } else {
                            throw new IllegalStateException("Fix me, this should not have happened.");
                        }
                    } else {
                        int difference = to.size() - current.size();

                        List<Replace> additiveReplaces = new ArrayList<>(difference);
                        for (int i = 0; i < difference; i++) {
                            int cur = toPos + i;
                            CobolPreprocessor.Word toWord = to.get(cur);
                            CobolPreprocessor.Word addedWord = new CobolPreprocessor.Word(
                                    randomId(),
                                    toWord.getPrefix(),
                                    Markers.EMPTY,
                                    toWord.getWord()
                            );

                            Replace replace = new Replace(randomId(), addedWord, false);
                            additiveReplaces.add(replace);
                        }
                        ReplaceAdditiveType replaceAdditiveType = new ReplaceAdditiveType(randomId(), additiveReplaces);
                        word = word.withMarkers(word.getMarkers().addIfAbsent(replaceAdditiveType));

                        inMatch = false;
                        current = null;
                        fromPos = 0;
                        toPos = 0;
                    }
                }
            }

            return super.visitWord(word, executionContext);
        }

        private ReplacementType init() {
            for (List<CobolPreprocessor.Word> words : from.values()) {
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
    private Map<List<CobolPreprocessor.Word>, List<CobolPreprocessor.Word>> getReplacings(CobolPreprocessor.ReplacingPhrase replacingPhrase) {
        // The order of matched MUST be retained for sequential replacements to work.
        Map<List<CobolPreprocessor.Word>, List<CobolPreprocessor.Word>> replacements = new LinkedHashMap<>();
        for (CobolPreprocessor.ReplaceClause clause : replacingPhrase.getClauses()) {
            List<CobolPreprocessor.Word> replaceable = resolveReplacementRule(clause.getReplaceable());
            List<CobolPreprocessor.Word> replacement = resolveReplacement(clause);
            if (!replaceable.isEmpty()) {
                replacements.put(replaceable, replacement);
            }
        }
        return replacements;
    }

    // Collect ReplaceClauses from ReplaceByStatement.
    private Map<List<CobolPreprocessor.Word>, List<CobolPreprocessor.Word>> getReplacements(CobolPreprocessor.ReplaceByStatement replaceByStatement) {
        // The order of matched MUST be retained for sequential replacements to work.
        Map<List<CobolPreprocessor.Word>, List<CobolPreprocessor.Word>> replacements = new LinkedHashMap<>();
        for (CobolPreprocessor.ReplaceClause clause : replaceByStatement.getClauses()) {
            List<CobolPreprocessor.Word> replaceable = resolveReplacementRule(clause.getReplaceable());
            List<CobolPreprocessor.Word> replacement = resolveReplacement(clause);
            if (!replaceable.isEmpty()) {
                replacements.put(replaceable, replacement);
            }
        }
        return replacements;
    }

    /**
     * A replacement in a {@link org.openrewrite.cobol.tree.CobolPreprocessor.ReplaceClause} contains trailing elements
     * that need to be a part of the replacement.
     *
     * @param replaceClause The original ReplaceClause.
     */
    private List<CobolPreprocessor.Word> resolveReplacement(CobolPreprocessor.ReplaceClause replaceClause) {
        List<CobolPreprocessor.Word> words = new ArrayList<>(resolveReplacementRule(replaceClause.getReplacement()));
        if (replaceClause.getSubscript() != null) {
            replaceClause.getSubscript().forEach(s -> words.addAll(resolveReplacementRule(s)));
        }
        if (replaceClause.getDirectoryPhrases() != null) {
            replaceClause.getDirectoryPhrases().forEach(s -> words.addAll(resolveReplacementRule(s)));
        }
        if (replaceClause.getFamilyPhrase() != null) {
            words.addAll(resolveReplacementRule(replaceClause.getFamilyPhrase()));
        }
        return words;
    }

    /**
     * Generic resolve method to interpret the CobolPreprocessor object and generate the replacement rule.
     */
    private List<CobolPreprocessor.Word> resolveReplacementRule(CobolPreprocessor cobolPreprocessor) {
        List<CobolPreprocessor.Word> words = new ArrayList<>();
        CobolPreprocessorWordVisitor wordVisitor = new CobolPreprocessorWordVisitor();

        if (cobolPreprocessor instanceof CobolPreprocessor.PseudoText) {
            CobolPreprocessor.PseudoText pseudoText = (CobolPreprocessor.PseudoText) cobolPreprocessor;
            if (pseudoText.getCharData() != null) {
                wordVisitor.visit(pseudoText.getCharData(), words);
            } else {
                words.add(new CobolPreprocessor.Word(
                        randomId(),
                        Space.EMPTY,
                        Markers.EMPTY,
                        ""
                ));
            }
        } else if (cobolPreprocessor instanceof CobolPreprocessor.CharDataLine) {
            CobolPreprocessor.CharDataLine charDataLine = (CobolPreprocessor.CharDataLine) cobolPreprocessor;
            wordVisitor.visit(charDataLine, words);
        } else if (cobolPreprocessor instanceof CobolPreprocessor.Word) {
            CobolPreprocessor.Word word = (CobolPreprocessor.Word) cobolPreprocessor;
            wordVisitor.visit(word, words);
        } else if (cobolPreprocessor instanceof CobolPreprocessor.DirectoryPhrase) {
            CobolPreprocessor.DirectoryPhrase directoryPhrase = (CobolPreprocessor.DirectoryPhrase) cobolPreprocessor;
            wordVisitor.visit(directoryPhrase, words);
        } else if (cobolPreprocessor instanceof CobolPreprocessor.FamilyPhrase) {
            CobolPreprocessor.FamilyPhrase familyPhrase = (CobolPreprocessor.FamilyPhrase) cobolPreprocessor;
            wordVisitor.visit(familyPhrase, words);
        } else {
            throw new UnsupportedOperationException("Implement me.");
        }
        return words;
    }

    private static class CobolPreprocessorWordVisitor extends CobolPreprocessorIsoVisitor<List<CobolPreprocessor.Word>> {
        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, List<CobolPreprocessor.Word> words) {
            words.add(word);
            return super.visitWord(word, words);
        }
    }
}
