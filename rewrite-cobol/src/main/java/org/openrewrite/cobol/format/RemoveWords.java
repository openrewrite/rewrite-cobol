/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.format;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Incubating;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.CobolPrinterUtils;
import org.openrewrite.cobol.search.FindWords;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

@Incubating(since = "0.0")
@EqualsAndHashCode(callSuper = true)
@Value
public class RemoveWords extends CobolIsoVisitor<ExecutionContext> {

    List<Cobol.Word> removeWords;

    public RemoveWords(Cobol tree) {
        this.removeWords = FindWords.find(tree);
    }

    public RemoveWords(List<Cobol.Word> removeWords) {
        this.removeWords = removeWords;
    }

    @Override
    public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
        Cobol.Word w = super.visitWord(word, executionContext);
        if (removeWords.contains(w) && !w.getWord().trim().isEmpty()) {
            if (word.getCopyStatement() != null || word.getReplacement() != null) {
                // The NIST test does not provide examples of these types of transformation, so it hasn't been implemented yet.
                throw new UnsupportedOperationException("RemoveWords does not support changes on copied sources or replaced words.");
            }

            if (word.getContinuation() != null) {
                org.openrewrite.cobol.tree.Continuation continuation = word.getContinuation();
                Map<Integer, List<ColumnArea>> continuations = new HashMap<>(continuation.getContinuations().size());
                AtomicBoolean changed = new AtomicBoolean(false);
                for (Map.Entry<Integer, List<ColumnArea>> entry : continuation.getContinuations().entrySet()) {
                    List<ColumnArea> columnAreas = ListUtils.map(entry.getValue(), it -> {
                        if (it instanceof IndicatorArea && "-".equals(((IndicatorArea) it).getIndicator())) {
                            it = ((IndicatorArea) it).withIndicator(" ");
                            changed.set(true);
                        }
                        return it;
                    });
                    continuations.put(entry.getKey(), columnAreas);
                }

                if (changed.get()) {
                    continuation = continuation.withContinuations(continuations);
                    w = w.withContinuation(continuation);
                }
            }
            w = w.withWord(CobolPrinterUtils.fillArea(' ', w.getWord().length()));
        }
        return w;
    }
}
