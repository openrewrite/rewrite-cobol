/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
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
public class SequenceArea implements ColumnArea {

    Markers markers;
    String sequence;

    @Override
    public <P> void printColumnArea(CobolPreprocessorSourcePrinter<P> sourcePrinter, Cursor cursor, boolean printColumns, PrintOutputCapture<P> p) {
        //noinspection DuplicatedCode
        if (printColumns) {
            for (Marker marker : markers.getMarkers()) {
                p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
            }

            p.out.append(sequence);

            for (Marker marker : markers.getMarkers()) {
                p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
            }
        }
    }

    @Override
    public <P> void printColumnArea(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, boolean printColumns, PrintOutputCapture<P> p) {
        //noinspection DuplicatedCode
        if (printColumns) {
            for (Marker marker : markers.getMarkers()) {
                p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
            }

            p.out.append(sequence);

            for (Marker marker : markers.getMarkers()) {
                p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
            }
        }
    }
}
