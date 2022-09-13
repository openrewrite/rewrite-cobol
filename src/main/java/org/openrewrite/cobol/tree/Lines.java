package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

/**
 * TODO: explain lines.
 * Examples: completely empty lines, blank lines with sequence areas, indicators and comment areas.
 * A line comment indicated with `*`.
 * There may be 1-M lines between ANY token via whitespace.
 */
@With
@Value
public class Lines implements Marker {
    UUID id;
    List<Line> lines;

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
