package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * TODO:
 */
@With
@Value
public class ReplaceOff implements Marker {
    UUID id;
}