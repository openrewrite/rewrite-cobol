package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.CobolPreprocessorIsoVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
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
        public Cobol visitIndicatorArea(Cobol.IndicatorArea indicatorArea, ExecutionContext executionContext) {
            if (indicator.equals(indicatorArea.getIndicator())) {
                return SearchResult.found(indicatorArea);
            }

            return indicatorArea;
        }

        @Override
        public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
            Cobol.Word w = super.visitWord(word, executionContext);
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
        public CobolPreprocessor visitIndicatorArea(CobolPreprocessor.IndicatorArea indicatorArea, ExecutionContext executionContext) {
            if (indicator.equals(indicatorArea.getIndicator())) {
                return SearchResult.found(indicatorArea);
            }

            return indicatorArea;
        }
    }
}
