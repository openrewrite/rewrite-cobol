package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * This marker preserves the original word before the execution of a {@link org.openrewrite.cobol.tree.CobolPreprocessor.ReplaceClause}.
 */
@Deprecated
@With
@Value
public class Replace implements Marker {
    UUID id;
    // Saves the original word to preserve the markers and prefix.
    CobolPreprocessor.Word originalWord;
    boolean replacedWithEmpty;
}