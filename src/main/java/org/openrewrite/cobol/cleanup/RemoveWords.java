package org.openrewrite.cobol.cleanup;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.CobolPrinterUtils;
import org.openrewrite.cobol.tree.Cobol;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Value
public class RemoveWords extends CobolIsoVisitor<ExecutionContext> {

    List<Cobol.Word> removeWords;

    public RemoveWords(List<Cobol.Word> removeWords) {
        this.removeWords = removeWords;
    }

    @Override
    public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
        Cobol.Word w = super.visitWord(word, executionContext);
        if (removeWords.contains(w) && !w.getWord().trim().isEmpty()) {
            w = w.withWord(CobolPrinterUtils.fillArea(' ', w.getWord().length()));
        }
        return w;
    }
}
