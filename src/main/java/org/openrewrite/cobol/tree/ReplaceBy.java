package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * TODO:
 */
@With
@Value
public class ReplaceBy implements Marker {
    UUID id;
    CobolPreprocessor.ReplaceByStatement statement;
}