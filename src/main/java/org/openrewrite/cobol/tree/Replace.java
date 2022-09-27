package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * This marker preserved the {@link org.openrewrite.cobol.tree.CobolPreprocessor.ReplaceClause} from the original source code.
 */
@With
@Value
public class Replace implements Marker {
    UUID id;
    // Saves the original word to preserve the markers and prefix.
    CobolPreprocessor.Word originalWord;
    boolean replacedWithEmpty;
}