package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * A {@link org.openrewrite.cobol.tree.CobolPreprocessor.ReplaceOffStatement} from the source code.
 */
@With
@Value
public class ReplaceOff implements Marker {
    UUID id;
    CobolPreprocessor.ReplaceOffStatement replaceOff;
}