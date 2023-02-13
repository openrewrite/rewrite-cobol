package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.CobolPreprocessorIsoVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.marker.SearchResult;

import java.util.UUID;

import static org.openrewrite.Tree.randomId;

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
        return "Find all matching indicators.";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new AddSearchResult(indicator);
    }

    private static class AddSearchResult extends CobolIsoVisitor<ExecutionContext> {
        private final PreprocessorSearchVisitor preprocessorSearchVisitor;
        private final String indicator;

        public AddSearchResult(String indicator) {
            this.indicator = indicator;
            this.preprocessorSearchVisitor = new PreprocessorSearchVisitor(indicator);
        }

        @Override
        public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
            Cobol.Word w = super.visitWord(word, executionContext);

            IndicatorArea indicatorArea = w.getMarkers().findFirst(IndicatorArea.class).orElse(null);
            if (indicatorArea != null && indicator.equals(indicatorArea.getIndicator())) {
                SearchResult result = indicatorArea.getMarkers().findAll(SearchResult.class).stream()
                        .filter(it -> SearchResultKey.INDICATOR_AREA.equals(it.getDescription()))
                        .findFirst()
                        .orElse(null);
                if (result == null) {
                    indicatorArea = indicatorArea.withMarkers(indicatorArea.getMarkers().addIfAbsent(
                            new SearchResult(randomId(), SearchResultKey.INDICATOR_AREA)
                    ));
                    w = w.withMarkers(w.getMarkers().addIfAbsent(indicatorArea));
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

        public PreprocessorSearchVisitor(String indicator) {
            this.indicator = indicator;
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
            CobolPreprocessor.Word w = super.visitWord(word, executionContext);

            IndicatorArea indicatorArea = w.getMarkers().findFirst(IndicatorArea.class).orElse(null);
            if (indicatorArea != null && indicator.equals(indicatorArea.getIndicator())) {
                SearchResult result = indicatorArea.getMarkers().findAll(SearchResult.class).stream()
                        .filter(it -> SearchResultKey.INDICATOR_AREA.equals(it.getDescription()))
                        .findFirst()
                        .orElse(null);
                if (result == null) {
                    indicatorArea = indicatorArea.withMarkers(indicatorArea.getMarkers().addIfAbsent(
                            new SearchResult(randomId(), SearchResultKey.INDICATOR_AREA)
                    ));
                    w = w.withMarkers(w.getMarkers().addIfAbsent(indicatorArea));
                }
            }
            return w;
        }
    }
}
