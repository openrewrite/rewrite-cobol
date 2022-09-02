package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolPreprocessorPrinter;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.StringUtils;

import java.util.HashMap;
import java.util.Map;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessReplaceVisitor<P> extends CobolPreprocessorIsoVisitor<P> {

    @Override
    public CobolPreprocessor.ReplaceArea visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, P p) {
        CobolPreprocessor.ReplaceArea r = super.visitReplaceArea(replaceArea, p);

        Map<String, String> replacements = getReplacements(replaceArea.getReplaceByStatement());
        ReplaceVisitor replaceVisitor = new ReplaceVisitor(replacements);
        r = r.withCobols(ListUtils.map(r.getCobols(), it -> replaceVisitor.visit(it, new InMemoryExecutionContext(), getCursor())));
        return r;
    }

    private static class ReplaceVisitor extends CobolPreprocessorIsoVisitor<ExecutionContext> {
        private final Map<String, String> replacements;

        public ReplaceVisitor(Map<String, String> replacements) {
            this.replacements = replacements;
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
            if (replacements.containsKey(word.getWord().trim())) {
                if (replacements.get(word.getWord()).isEmpty()) {
                    //noinspection ConstantConditions
                    return null;
                }
                return word.withWord(replacements.get(word.getWord()));
            }
            return super.visitWord(word, executionContext);
        }
    }

    private Map<String, String> getReplacements(CobolPreprocessor.ReplaceByStatement replaceByStatement) {
        Map<String, String> replacements = new HashMap<>();
        for (CobolPreprocessor.ReplaceClause clause : replaceByStatement.getClauses()) {
            String replaceable = resolveReplace(clause.getReplaceable());
            String replacement = resolveReplace(clause.getReplacement());
            if (!StringUtils.isBlank(replaceable)) {
                replacements.put(replaceable, replacement);
            }
        }
        return replacements;
    }

    private String resolveReplace(CobolPreprocessor cobolPreprocessor) {
        String result = "";
        PrintOutputCapture<ExecutionContext> output = new PrintOutputCapture<>(new InMemoryExecutionContext());
        CobolPreprocessorWordPrinter<ExecutionContext> printer = new CobolPreprocessorWordPrinter<>();

        if (cobolPreprocessor instanceof CobolPreprocessor.PseudoText) {
            CobolPreprocessor.PseudoText pseudoText = (CobolPreprocessor.PseudoText) cobolPreprocessor;
            if (pseudoText.getCharData() != null) {
                printer.visit(pseudoText.getCharData(), output);
                result = output.getOut();
            }
        } else if (cobolPreprocessor instanceof CobolPreprocessor.CharDataLine) {
            throw new UnsupportedOperationException("Implement me.");
        } else if (cobolPreprocessor instanceof CobolPreprocessor.Word) {
            throw new UnsupportedOperationException("Implement me.");
        } else {
            throw new UnsupportedOperationException("Implement me.");
        }
        return result.trim();
    }

    private static class CobolPreprocessorWordPrinter<P> extends CobolPreprocessorPrinter<P> {

        @Override
        public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
            visit(copyBook.getAst(), p);
            return copyBook;
        }

        @Override
        public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
            visit(copyStatement.getCopyBook(), p);
            return copyStatement;
        }

        @Override
        public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
            p.append(" ");
            p.append(word.getWord());
            return word;
        }
    }
}
