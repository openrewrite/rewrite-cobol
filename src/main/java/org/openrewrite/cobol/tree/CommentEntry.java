package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

@With
@Value
public class CommentEntry implements Marker {
    UUID id;
    Space prefix;
    String comment;
}