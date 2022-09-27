package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * ReplaceBy rule from the original source code.
 */
@With
@Value
public class ReplaceBy implements Marker {
    UUID id;
    CobolPreprocessor.ReplaceByStatement statement;
}