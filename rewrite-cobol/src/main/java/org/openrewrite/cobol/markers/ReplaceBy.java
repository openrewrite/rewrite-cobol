package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * A {@link org.openrewrite.cobol.tree.CobolPreprocessor.ReplaceByStatement} from the original source code.
 */
@Deprecated
@With
@Value
public class ReplaceBy implements Marker {
    UUID id;
    CobolPreprocessor.ReplaceByStatement statement;
}