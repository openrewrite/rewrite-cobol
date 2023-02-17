package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

/**
 * There may be one or more comments and/or empty lines between any token via whitespace.
 * The Lines Marker preserves the column areas for each of the lines that come before a COBOL word.
 * <p>
 * Line comments are indicated with a `*` (depends on dialect) in the indicator areas.
 */
@With
@Value
public class Lines implements Marker {
    UUID id;
    List<Line> lines;

    @With
    @Value
    public static class Line {
        UUID id;
        SequenceArea sequenceArea;
        IndicatorArea indicatorArea;
        String content;

        @Nullable
        CommentArea commentArea;

        boolean isCopiedSource;
    }
}
