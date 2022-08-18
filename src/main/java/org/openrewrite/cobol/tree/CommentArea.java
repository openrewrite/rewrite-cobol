package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

@With
@Value
public class CommentArea implements Marker {
    UUID id;
    Space prefix;
    String comment;
}
