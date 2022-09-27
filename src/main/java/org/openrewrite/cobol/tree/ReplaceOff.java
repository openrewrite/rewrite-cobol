package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * This marker preserves the {@link org.openrewrite.cobol.tree.CobolPreprocessor.ReplaceOffStatement} from the source code.
 */
@With
@Value
public class ReplaceOff implements Marker {
    UUID id;
    CobolPreprocessor.ReplaceOffStatement replaceOff;
}