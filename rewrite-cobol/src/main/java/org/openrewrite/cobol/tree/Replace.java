package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * This marker preserves the original word before the execution of a {@link org.openrewrite.cobol.tree.CobolPreprocessor.ReplaceClause}.
 */
@With
@Value
public class Replace implements Marker {
    UUID id;
    // Saves the original word to preserve the markers and prefix.
    CobolPreprocessor.Word originalWord;
    boolean replacedWithEmpty;
}