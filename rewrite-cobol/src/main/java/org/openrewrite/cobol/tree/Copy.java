package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * Mark the results of a copied source with the original source code.
 */
@Value
@With
public class Copy implements Marker {
    UUID id;
    CobolPreprocessor.CopyStatement originalStatement;
}
