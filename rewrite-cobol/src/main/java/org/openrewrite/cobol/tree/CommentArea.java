package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolPreprocessorSourcePrinter;
import org.openrewrite.cobol.internal.CobolSourcePrinter;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import static org.openrewrite.cobol.internal.CobolSourcePrinter.COBOL_MARKER_WRAPPER;

@Value
@With
public class CommentArea implements ColumnArea {

    Space prefix;
    Markers markers;
    String comment;
    Space endOfLine;
    boolean isAdded;

    @Override
    public <P> void printColumnArea(CobolPreprocessorSourcePrinter<P> sourcePrinter, Cursor cursor, boolean printColumns, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }

        sourcePrinter.visitSpace(prefix, Space.Location.COMMENT_AREA_PREFIX, p);
        if (printColumns) {
            p.append(comment);
        }
        sourcePrinter.visitSpace(endOfLine, Space.Location.COMMENT_AREA_EOL, p);

        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }
    }

    @Override
    public <P> void printColumnArea(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, boolean printColumns, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }

        sourcePrinter.visitSpace(prefix, Space.Location.COMMENT_AREA_PREFIX, p);
        if (printColumns) {
            p.append(comment);
        }
        sourcePrinter.visitSpace(endOfLine, Space.Location.COMMENT_AREA_EOL, p);

        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }
    }
}
