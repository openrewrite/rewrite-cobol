package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * TODO: info on COBOL indicator areas.
 */
@With
@Value
public class IndicatorArea implements Marker {
    UUID id;
    // TODO: clean up; separate whitespace from the indicator.
    // Indicators may contain the space between the indicator and whitespace that is truncated by the preprocessor.
    String indicator;
}
