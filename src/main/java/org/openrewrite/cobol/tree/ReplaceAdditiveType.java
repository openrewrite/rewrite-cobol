package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

@With
@Value
public class ReplaceAdditiveType implements Marker {
    UUID id;
    List<Replace> additionalWords;
}