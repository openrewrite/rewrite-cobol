package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;

import java.util.List;
import java.util.UUID;

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
    }
}
