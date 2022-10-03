package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Option;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.IndicatorArea;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindIndicators extends Recipe {

    @Option(displayName = "Indicator character",
            description = "A list of indicators to search for.",
            example = "D")
    List<String> indicator;

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
        return new CobolIsoVisitor<ExecutionContext>() {
            @Override
            public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
                IndicatorArea indicatorArea = word.getMarkers().findFirst(IndicatorArea.class).orElse(null);
                if (indicatorArea != null) {
                    String id = indicatorArea.getIndicator();
                }
                return super.visitWord(word, executionContext);
            }
        };
    }
}
