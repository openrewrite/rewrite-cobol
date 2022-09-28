package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

/**
 * This marker represents a REDUCTIVE {@link org.openrewrite.cobol.PreprocessReplaceVisitor.ReplaceVisitor.ReplacementType}.
 * A REDUCTIVE change will cause words to be set to empty, and are not printed by
 * {@link org.openrewrite.cobol.internal.CobolPreprocessorOutputPrinter}.
 *
 * There may be multiple removed words that get associated to the next visible word, so a unique Marker is created.
 *
 * I.E. PERFORM FAIL. => ""
 * Before:
 *  |000001| | PERFORM FAIL .            nextWord|
 *
 * After:
 *  |000001| |                           nextWord|
 */
@With
@Value
public class ReplaceReductiveType implements Marker {
    UUID id;
    List<Replace> originalWords;
}