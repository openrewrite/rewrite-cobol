package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * Indicator areas are pre-compile flags that determine how to handle the current line.
 * Space is used to preserve whitespace from certain types of continuation lines that would otherwise remove whitespace.
 *
 * I.E. String literal:
 *   |000000| | Some COBOL "String literal ...             |
 *   |000001|-|    "is continued with prefixed whitespace."|
 *
 * Exact output is "String literal ...             is continued with prefixed whitespace."
 * So, the indicator preserves the `    "`.
 */
@With
@Value
public class IndicatorArea implements Marker {
    UUID id;
    String indicator;

    @Nullable
    String continuationPrefix;

    public String getContinuationPrefix() {
        return continuationPrefix == null ? "" : continuationPrefix;
    }
}
