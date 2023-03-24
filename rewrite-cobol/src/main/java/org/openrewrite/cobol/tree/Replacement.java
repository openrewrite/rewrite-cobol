package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolSourcePrinter;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.List;

import static org.openrewrite.cobol.internal.CobolSourcePrinter.COBOL_MARKER_WRAPPER;

@Value
@With
public class Replacement {

    Markers markers;
    List<OriginalWord> originalWords;
    Type type;

    boolean isCopiedSource;

    @Value
    @With
    public static class OriginalWord {
        Cobol.Word original;
        boolean replacedWithEmpty;
    }

    public enum Type {
        ADDITIVE,
        REDUCTIVE,
        EQUAL
    }

    public <P> void printReplacement(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }

        if (type == Type.EQUAL) {
            OriginalWord originalWord = originalWords.get(0);
            sourcePrinter.visit(originalWord.getOriginal(), p);
        } else if (type == Type.REDUCTIVE && !isCopiedSource) {
            for (OriginalWord originalWord : originalWords) {
                sourcePrinter.visit(originalWord.getOriginal(), p);
            }
        }

        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(cursor, marker), COBOL_MARKER_WRAPPER));
        }
    }
}
