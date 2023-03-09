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
    Cobol.ColumnArea.SequenceArea sequenceArea;

    @With
    Cobol.ColumnArea.IndicatorArea indicatorArea;

    @With
    String contentArea;

    @Nullable
    @With
    Cobol.ColumnArea.CommentArea commentArea;

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
            sourcePrinter.visit(sequenceArea, p);
            sourcePrinter.visit(indicatorArea, p);
            p.append(contentArea);
            sourcePrinter.visit(commentArea, p);
        }

        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, this), COBOL_MARKER_WRAPPER));
        }
    }
}
