package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.cobol.internal.CobolPreprocessorOutputSourcePrinter;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

/**
 * This marker represents a REDUCTIVE {@link org.openrewrite.cobol.PreprocessReplaceVisitor.ReplaceVisitor.ReplacementType}.
 * A REDUCTIVE change will cause words to be set to replaced by whitespace, and are not printed by
 * {@link CobolPreprocessorOutputSourcePrinter}.
 * <p>
 * There may be multiple removed words that get associated to the next visible word, so a unique Marker is created.
 * <p>
 * I.E. PERFORM FAIL. => ""
 * Before:
 *  |000001| | PERFORM FAIL .            nextWord|
 * <p>
 * After:
 *  |000001| |                           nextWord|
 */
@With
@Value
public class ReplaceReductiveType implements Marker {
    UUID id;
    List<Replace> originalWords;
}