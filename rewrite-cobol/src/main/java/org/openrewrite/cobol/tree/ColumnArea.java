/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolPreprocessorSourcePrinter;
import org.openrewrite.cobol.internal.CobolSourcePrinter;
import org.openrewrite.marker.Markers;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, property = "@c")
public interface ColumnArea {
    Markers getMarkers();

    <C extends ColumnArea> C withMarkers(Markers markers);

    <P> void printColumnArea(CobolPreprocessorSourcePrinter<P> sourcePrinter, Cursor cursor, boolean printColumns, PrintOutputCapture<P> p);

    <P> void printColumnArea(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, boolean printColumns, PrintOutputCapture<P> p);
}
