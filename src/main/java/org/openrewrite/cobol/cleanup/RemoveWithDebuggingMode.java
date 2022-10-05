package org.openrewrite.cobol.cleanup;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;

import java.time.Duration;
import java.util.Collections;
import java.util.Set;

@EqualsAndHashCode(callSuper = true)
@Value
public class RemoveWithDebuggingMode extends Recipe {

    @Override
    public String getDisplayName() {
        return "Remove with debugging mode";
    }

    @Override
    public String getDescription() {
        return "Remove debugging mode from SOURCE-COMPUTER paragraphs.";
    }

    @Override
    public Set<String> getTags() {
        return Collections.singleton("RSPEC-2057");
    }

    @Override
    public Duration getEstimatedEffortPerOccurrence() {
        return Duration.ofMinutes(1_000_000);
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new CobolIsoVisitor<ExecutionContext>() {
            @Override
            public Cobol.SourceComputerDefinition visitSourceComputerDefinition(Cobol.SourceComputerDefinition sourceComputerDefinition,
                                                                                ExecutionContext executionContext) {
                Cobol.SourceComputerDefinition s = super.visitSourceComputerDefinition(sourceComputerDefinition, executionContext);
                if (s.getDebuggingMode() != null && !s.getDebuggingMode().isEmpty()) {
                    s = new RemoveWords(s.getDebuggingMode()).visitSourceComputerDefinition(s, executionContext);
                }
                return s;
            }
        };
    }
}

