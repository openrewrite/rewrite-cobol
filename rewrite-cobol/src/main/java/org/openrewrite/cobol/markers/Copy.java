package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * Mark the results of a copied source with the original source code.
 */
@Deprecated
@Value
@With
public class Copy implements Marker {
    UUID id;
    CobolPreprocessor.CopyStatement originalStatement;
}
