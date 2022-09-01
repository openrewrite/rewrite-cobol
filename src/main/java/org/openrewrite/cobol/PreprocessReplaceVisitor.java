package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.cobol.tree.CobolPreprocessor;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessReplaceVisitor<P> extends CobolPreprocessorIsoVisitor<P> {

    @Override
    public CobolPreprocessor.ReplaceArea visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, P p) {
        // Set replacement rules from ReplaceStatement.
        // visit replaceArea.getCobols()
        return super.visitReplaceArea(replaceArea, p);
    }
}
