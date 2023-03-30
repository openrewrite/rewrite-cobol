package org.openrewrite.cobol.tree;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import lombok.Value;
import lombok.With;
import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolPreprocessorSourcePrinter;
import org.openrewrite.cobol.internal.CobolSourcePrinter;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.openrewrite.cobol.internal.CobolSourcePrinter.COBOL_MARKER_WRAPPER;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, property = "@c")
@Value
@With
public class Continuation {

    Markers markers;
    Map<Integer, List<ColumnArea>> continuations;

    public <P> void printContinuation(CobolPreprocessorSourcePrinter<P> sourcePrinter, Cursor cursor, CobolPreprocessor.Word word, boolean printColumns, PrintOutputCapture<P> p) {
        for (Marker marker : getMarkers().getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }

        if (continuations.containsKey(0)) {
            for (ColumnArea columnArea : continuations.get(0)) {
                if (columnArea instanceof SequenceArea) {
                    columnArea.printColumnArea(sourcePrinter, cursor, printColumns, p);
                } else if (columnArea instanceof IndicatorArea) {
                    columnArea.printColumnArea(sourcePrinter, cursor, printColumns, p);
                }
            }
        }

        sourcePrinter.visitSpace(word.getPrefix(), Space.Location.CONTINUATION_PREFIX, p);

        char[] charArray = word.getWord().toCharArray();
        for (int i = 0; i < charArray.length; i++) {
            if (i != 0 && continuations.containsKey(i)) {
                for (ColumnArea columnArea : continuations.get(i)) {
                    columnArea.printColumnArea(sourcePrinter, cursor, printColumns, p);
                }
            }
            char c = charArray[i];
            p.append(c);
        }

        List<List<ColumnArea>> lastColumnAreas = continuations.entrySet().stream()
                .filter(it -> it.getKey() > word.getWord().length())
                .map(Map.Entry::getValue)
                .collect(Collectors.toList());

        if (!lastColumnAreas.isEmpty()) {
            List<ColumnArea> columnAreas = lastColumnAreas.get(0);
            for (ColumnArea columnArea : columnAreas) {
                if (columnArea instanceof CommentArea) {
                    columnArea.printColumnArea(sourcePrinter, cursor, printColumns, p);
                }
            }
        }

        for (Marker marker : getMarkers().getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }
    }

    public <P> void printContinuation(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, Cobol.Word word, boolean printColumns, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }

        if (continuations.containsKey(0)) {
            for (ColumnArea columnArea : continuations.get(0)) {
                if (columnArea instanceof SequenceArea) {
                    columnArea.printColumnArea(sourcePrinter, cursor, printColumns, p);
                } else if (columnArea instanceof IndicatorArea) {
                    columnArea.printColumnArea(sourcePrinter, cursor, printColumns, p);
                }
            }
        }

        sourcePrinter.visitSpace(word.getPrefix(), Space.Location.CONTINUATION_PREFIX, p);

        char[] charArray = word.getWord().toCharArray();
        for (int i = 0; i < charArray.length; i++) {
            if (i != 0 && continuations.containsKey(i)) {
                for (ColumnArea columnArea : continuations.get(i)) {
                    columnArea.printColumnArea(sourcePrinter, cursor, printColumns, p);
                }
            }
            char c = charArray[i];
            p.append(c);
        }

        List<List<ColumnArea>> lastColumnAreas = continuations.entrySet().stream()
                .filter(it -> it.getKey() > word.getWord().length())
                .map(Map.Entry::getValue)
                .collect(Collectors.toList());

        if (!lastColumnAreas.isEmpty()) {
            List<ColumnArea> columnAreas = lastColumnAreas.get(0);
            for (ColumnArea columnArea : columnAreas) {
                if (columnArea instanceof CommentArea) {
                    columnArea.printColumnArea(sourcePrinter, cursor, printColumns, p);
                }
            }
        }

        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }
    }
}
