package org.openrewrite.cobol.tree;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolSourcePrinter;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

/**
 * There may be one or more comments and/or empty lines between any token via whitespace.
 * The Lines Marker preserves the column areas for each of the lines that come before a COBOL word.
 * <p>
 * Line comments are indicated with a `*` (depends on dialect) in the indicator areas.
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, property = "@c")
public interface CobolLine {

    Markers getMarkers();
    <C extends CobolLine> C withMarkers(Markers markers);

    @Nullable
    Cobol.ColumnArea.SequenceArea getSequenceArea();
    <C extends CobolLine> C withSequenceArea(Cobol.ColumnArea.SequenceArea sequenceArea);

    Cobol.ColumnArea.IndicatorArea getIndicatorArea();
    <C extends CobolLine> C withIndicatorArea(Cobol.ColumnArea.IndicatorArea indicatorArea);

    String getContentArea();
    <C extends CobolLine> C withContentArea(String contentArea);

    @Nullable
    Cobol.ColumnArea.CommentArea getCommentArea();
    <C extends CobolLine> C withCommentArea(Cobol.ColumnArea.CommentArea commentArea);

    boolean isCopiedSource();
    <C extends CobolLine> C withCopiedSource(boolean isCopiedSource);

    <P> void printCobolLine(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, PrintOutputCapture<P> p);
}
