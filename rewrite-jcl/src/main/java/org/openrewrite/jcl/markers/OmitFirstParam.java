package org.openrewrite.jcl.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

@With
@Value
public class OmitFirstParam implements Marker {
    UUID id;
}
