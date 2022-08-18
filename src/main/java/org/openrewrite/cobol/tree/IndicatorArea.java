package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

@With
@Value
public class IndicatorArea implements Marker {
    UUID id;
    String indicator;
}
