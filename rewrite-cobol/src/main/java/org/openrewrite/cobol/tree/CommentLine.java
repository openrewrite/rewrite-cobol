package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolSourcePrinter;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.function.UnaryOperator;

@Value
public class CommentLine implements CobolLine, Comment {

    @With
    Markers markers;

    @Nullable
    @With
    SequenceArea sequenceArea;

    @With
    IndicatorArea indicatorArea;

    @With
    String contentArea;

    @Nullable
    @With
    CommentArea commentArea;

    @With
    boolean isCopiedSource;

    private static final UnaryOperator<String> COBOL_MARKER_WRAPPER =
            out -> "~~" + out + (out.isEmpty() ? "" : "~~") + ">";

    @Override
    public <P> void printCobolLine(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(cursor, this), COBOL_MARKER_WRAPPER));
        }

        if (!isCopiedSource) {
            if (sequenceArea != null) {
                sequenceArea.printColumnArea(sourcePrinter, cursor, true, p);
            }
            indicatorArea.printColumnArea(sourcePrinter, cursor, true, p);
            p.append(contentArea);
            if (commentArea != null) {
                commentArea.printColumnArea(sourcePrinter, cursor, true, p);
            }
        }

        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, this), COBOL_MARKER_WRAPPER));
        }
    }
}
