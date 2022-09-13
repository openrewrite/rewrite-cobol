package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * CommentAreas are optional areas that exist because the COBOL parser does not read passed a specified index (based on dialect).
 */
@With
@Value
public class CommentArea implements Marker {
    UUID id;
    Space prefix;
    String comment;
    Space endOfLine;
}
