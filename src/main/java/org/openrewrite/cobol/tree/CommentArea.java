package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * TODO: explain commentArea.
 * Note: this should be revisited with Sam. The endOfLine may technically be considered whitespace in the AST,
 * but was added to the marker to simplify debugging.
 */
@With
@Value
public class CommentArea implements Marker {
    UUID id;
    Space prefix;
    String comment;
    Space endOfLine;
}
