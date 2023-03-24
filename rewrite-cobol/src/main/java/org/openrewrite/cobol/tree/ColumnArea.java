package org.openrewrite.cobol.tree;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolSourcePrinter;
import org.openrewrite.marker.Markers;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, property = "@c")
public interface ColumnArea {
    Markers getMarkers();

    <C extends ColumnArea> C withMarkers(Markers markers);

    <P> void printColumnArea(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, boolean printColumns, PrintOutputCapture<P> p);
}
