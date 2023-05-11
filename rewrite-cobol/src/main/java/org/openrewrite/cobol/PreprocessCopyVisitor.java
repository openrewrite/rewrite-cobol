/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.Tree;
import org.openrewrite.cobol.tree.CobolPreprocessor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessCopyVisitor<P> extends CobolPreprocessorIsoVisitor<P> {

    Map<String, CobolPreprocessor.CopyBook> copyBooks = new HashMap<>();

    public PreprocessCopyVisitor(List<CobolPreprocessor.CopyBook> copyBooks) {
        copyBooks.forEach(it -> {
            // Note: this implementation ASSUMES CopyBooks are resolved by FileName and will require changes.
            String fileName = it.getSourcePath().getFileName().toString();
            this.copyBooks.putIfAbsent(fileName.substring(0, fileName.indexOf(".")), it);
        });
    }

    @Override
    public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, P p) {
        CobolPreprocessor.CopyStatement c = super.visitCopyStatement(copyStatement, p);

        if (copyBooks.containsKey(copyStatement.getCopySource().getName().getCobolWord().getWord())) {
            c = c.withCopyBook(copyBooks.get(copyStatement.getCopySource().getName().getCobolWord().getWord()).withId(Tree.randomId()));
        }
        return c;
    }
}
