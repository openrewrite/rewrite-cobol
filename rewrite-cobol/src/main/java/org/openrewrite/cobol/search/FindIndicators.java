package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.marker.SearchResult;

import static org.openrewrite.Tree.randomId;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindIndicators extends Recipe {

    @Option(displayName = "Indicator character",
            description = "Indicator to search for.",
            example = "D")
    String indicator;

    @Override
    public String getDisplayName() {
        return "Find indicators";
    }

    @Override
    public String getDescription() {
        return "Find matching indicators. Currently, this recipe will not mark indicators on CopyBook code.";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new AddSearchResult(indicator);
    }

    private static class AddSearchResult extends CobolIsoVisitor<ExecutionContext> {
        private final String indicator;

        public AddSearchResult(String indicator) {
            this.indicator = indicator;
        }


        @Override
        public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
            if (word.getIndicatorArea() != null && word.getIndicatorArea().getIndicator().equals(indicator)) {
                word = word.withIndicatorArea(
                        word.getIndicatorArea().withMarkers(
                                word.getIndicatorArea().getMarkers().addIfAbsent(new SearchResult(randomId(), null))));
            }
            return word;
        }
    }
}
