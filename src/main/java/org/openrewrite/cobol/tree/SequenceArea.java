package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

@With
@Value
public class SequenceArea implements Marker {
    UUID id;
    String sequence;
}