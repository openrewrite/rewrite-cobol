package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
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
        // Refactor this could be done dynamically through visitWord.
        Map<String, String> replacements = getReplacements(replaceArea.getReplaceByStatement());
        ReplaceVisitor replaceVisitor = new ReplaceVisitor(replacements);
        r = r.withCobols(ListUtils.map(r.getCobols(), it -> replaceVisitor.visit(it, new InMemoryExecutionContext())));
        return r;
    }

    private static class ReplaceVisitor extends CobolPreprocessorIsoVisitor<ExecutionContext> {
        private final Map<String, String> replacements;

        public ReplaceVisitor(Map<String, String> replacements) {
            this.replacements = replacements;
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
            if (replacements.containsKey(word.getWord())) {
                return word.withWord(replacements.get(word.getWord()));
            }
            return super.visitWord(word, executionContext);
        }
    }

    private Map<String, String> getReplacements(CobolPreprocessor.ReplaceByStatement replaceByStatement) {
        // TODO: initially works for single words ... modify to match AST for multi word rules.
        Map<String, String> replacements = new HashMap<>();
        for (CobolPreprocessor.ReplaceClause clause : replaceByStatement.getClauses()) {
            String replaceable = resolveReplace(clause.getReplaceable());
            String replacement = resolveReplace(clause.getReplacement());
            if (!StringUtils.isBlank(replaceable) && !StringUtils.isBlank(replacement)) {
                replacements.put(replaceable, replacement);
            }
        }

        return replacements;
    }

    private String resolveReplace(CobolPreprocessor cobolPreprocessor) {
        String result = "";
        if (cobolPreprocessor instanceof CobolPreprocessor.PseudoText) {
            CobolPreprocessor.PseudoText pseudoText = (CobolPreprocessor.PseudoText) cobolPreprocessor;
            if (pseudoText.getCharData() != null) {
                result = pseudoText.getCharData().print(getCursor());
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
}
