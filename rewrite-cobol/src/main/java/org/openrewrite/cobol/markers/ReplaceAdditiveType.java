package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

/**
 * This marker represents words that were added by a {@link org.openrewrite.cobol.tree.CobolPreprocessor.ReplaceClause}
 * that did not exist in the original source code.
 */
@Deprecated
@With
@Value
public class ReplaceAdditiveType implements Marker {
    UUID id;
    List<Replace> additionalWords;
}