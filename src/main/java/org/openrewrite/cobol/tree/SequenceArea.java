package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * TODO: explain.
 */
@With
@Value
public class SequenceArea implements Marker {
    UUID id;
    Space whitespace;
    String sequence;
}