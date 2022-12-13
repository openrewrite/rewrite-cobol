package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.CobolPreprocessorIsoVisitor;
import org.openrewrite.cobol.markers.*;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;

import static org.openrewrite.Tree.randomId;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindIndicators extends Recipe {

    @Option(displayName = "Indicator character",
            description = "A list of indicators to search for.",
            example = "D")
    String indicator;

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
        return new AddSearchResult(indicator);
    }

    private static class AddSearchResult extends CobolIsoVisitor<ExecutionContext> {
        private final SearchResult searchResult = new SearchResult(randomId(), SearchResult.Type.INDICATOR_AREA, null);
        private final PreprocessorSearchVisitor preprocessorSearchVisitor;
        private final String indicator;

        public AddSearchResult(String indicator) {
            this.indicator = indicator;
            this.preprocessorSearchVisitor = new PreprocessorSearchVisitor(indicator, searchResult);
        }

        @Override
        public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
            Cobol.Word w = super.visitWord(word, executionContext);

            IndicatorArea indicatorArea = w.getMarkers().findFirst(IndicatorArea.class).orElse(null);
            if (indicatorArea != null && indicator.equals(indicatorArea.getIndicator())) {
                SearchResult result = w.getMarkers().findFirst(SearchResult.class).orElse(null);
                if (result == null) {
                    w = w.withMarkers(w.getMarkers().addIfAbsent(searchResult));
                }
            }

            w = w.withMarkers(w.getMarkers().withMarkers(ListUtils.map(w.getMarkers().getMarkers(), it -> {
                if (it instanceof Copy) {
                    Copy copy = (Copy) it;
                    return preprocessorSearchVisitor.visitCopy(copy, executionContext);
                } else if (it instanceof Replace) {
                    Replace replace = (Replace) it;
                    return preprocessorSearchVisitor.visitReplace(replace, executionContext);
                } else if (it instanceof ReplaceAdditiveType) {
                    ReplaceAdditiveType replaceAdditiveType = (ReplaceAdditiveType) it;
                    return preprocessorSearchVisitor.visitReplaceAdditiveType(replaceAdditiveType, executionContext);
                } else if (it instanceof ReplaceReductiveType) {
                    ReplaceReductiveType replaceReductiveType = (ReplaceReductiveType) it;
                    return preprocessorSearchVisitor.visitReplaceReductiveType(replaceReductiveType, executionContext);
                }
                return it;
            })));

            return w;
        }
    }

    private static class PreprocessorSearchVisitor extends CobolPreprocessorIsoVisitor<ExecutionContext> {
        private final String indicator;
        private final SearchResult searchResult;

        public PreprocessorSearchVisitor(String indicator,
                                         SearchResult searchResult) {
            this.indicator = indicator;
            this.searchResult = searchResult;
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
            CobolPreprocessor.Word w = super.visitWord(word, executionContext);

            IndicatorArea indicatorArea = w.getMarkers().findFirst(IndicatorArea.class).orElse(null);
            if (indicatorArea != null && indicator.equals(indicatorArea.getIndicator())) {
                SearchResult result = w.getMarkers().findFirst(SearchResult.class).orElse(null);
                if (result == null) {
                    w = w.withMarkers(w.getMarkers().addIfAbsent(searchResult));
                }
            }
            return w;
        }
    }
}
