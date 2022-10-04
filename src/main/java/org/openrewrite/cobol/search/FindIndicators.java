package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CopyStatement;
import org.openrewrite.cobol.tree.IndicatorArea;
import org.openrewrite.marker.Marker;

import java.util.List;

import static org.openrewrite.Tree.randomId;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindIndicators extends Recipe {

    @Option(displayName = "Indicator character",
            description = "A list of indicators to search for.",
            example = "D")
    List<String> indicators;

    @Override
    public String getDisplayName() {
        return "Find indicators";
    }

    @Override
    public String getDescription() {
        return "Find all matching indicators.";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new AddSearchResult(indicators);
    }

    private static class AddSearchResult extends CobolIsoVisitor<ExecutionContext> {
        private final List<String> indicators;
        private final SearchResult searchResult = new SearchResult(randomId(), SearchResult.Type.INDICATOR_AREA, null);
        public AddSearchResult(List<String> indicators) {
            this.indicators = indicators;
        }

        @Override
        public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
            Cobol.Word w = super.visitWord(word, executionContext);
            IndicatorArea indicatorArea = w.getMarkers().findFirst(IndicatorArea.class).orElse(null);
            if (indicatorArea != null && indicators.contains(indicatorArea.getIndicator())) {
                SearchResult result = w.getMarkers().findFirst(SearchResult.class).orElse(null);
                if (result == null) {
                    w = w.withMarkers(w.getMarkers().addIfAbsent(searchResult));
                }
            }

            CopyStatement copyStatement = w.getMarkers().findFirst(CopyStatement.class).orElse(null);
            if (copyStatement != null) {
                indicatorArea = copyStatement.getOriginalStatement().getMarkers().findFirst(IndicatorArea.class).orElse(null);
                if (indicatorArea != null && indicators.contains(indicatorArea.getIndicator())) {
                    SearchResult copySearchResult = copyStatement.getOriginalStatement().getMarkers().findFirst(SearchResult.class).orElse(null);
                    if (copySearchResult == null) {
                        List<Marker> copyStatementMarkers = copyStatement.getOriginalStatement().getMarkers().getMarkers();
                        copyStatementMarkers.add(searchResult);
                    }
                }
            }
            return w;
        }
    }
}
