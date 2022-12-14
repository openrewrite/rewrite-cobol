package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.CobolPreprocessorIsoVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.marker.SearchResult;

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
        private final CobolSearchResult searchResult = new CobolSearchResult(randomId(), CobolSearchResult.Type.INDICATOR_AREA, "Found indicator area.");
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
                CobolSearchResult result = w.getMarkers().findAll(CobolSearchResult.class).stream()
                        .filter(it -> it.getType() == CobolSearchResult.Type.INDICATOR_AREA)
                        .findFirst().orElse(null);
                if (result == null) {
                    SearchResult.found(w);
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
        private final CobolSearchResult searchResult;

        public PreprocessorSearchVisitor(String indicator,
                                         CobolSearchResult searchResult) {
            this.indicator = indicator;
            this.searchResult = searchResult;
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
            CobolPreprocessor.Word w = super.visitWord(word, executionContext);

            IndicatorArea indicatorArea = w.getMarkers().findFirst(IndicatorArea.class).orElse(null);
            if (indicatorArea != null && indicator.equals(indicatorArea.getIndicator())) {
                SearchResult.found(w);
            }
            return w;
        }
    }
}
