package org.openrewrite.cobol.tree;

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
 *   |000000| | Some COBOL "string literal ...             |
 *   |000001|-|    "is continued with prefixed whitespace."|
 */
@With
@Value
public class IndicatorArea implements Marker {
    UUID id;
    String indicator;

    @Nullable
    Space continuationWhitespace;
}
