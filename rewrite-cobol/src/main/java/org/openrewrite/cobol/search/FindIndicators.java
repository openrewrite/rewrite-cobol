package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.marker.SearchResult;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindIndicators extends Recipe {

    @Option(displayName = "Indicator character",
            description = "Indicator to search for.",
            example = "Y")
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
        public Cobol visitIndicatorArea(Cobol.IndicatorArea indicatorArea, ExecutionContext executionContext) {
            if (indicator.equals(indicatorArea.getIndicator())) {
                return SearchResult.found(indicatorArea);
            }

            return indicatorArea;
        }
    }
}
