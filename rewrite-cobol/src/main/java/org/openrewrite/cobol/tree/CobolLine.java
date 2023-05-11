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
    SequenceArea getSequenceArea();
    <C extends CobolLine> C withSequenceArea(SequenceArea sequenceArea);

    IndicatorArea getIndicatorArea();
    <C extends CobolLine> C withIndicatorArea(IndicatorArea indicatorArea);

    @Nullable
    CommentArea getCommentArea();
    <C extends CobolLine> C withCommentArea(CommentArea commentArea);

    boolean isCopiedSource();
    <C extends CobolLine> C withCopiedSource(boolean isCopiedSource);

    <P> void printCobolLine(CobolPreprocessorSourcePrinter<P> sourcePrinter, Cursor cursor, PrintOutputCapture<P> p);
    <P> void printCobolLine(CobolSourcePrinter<P> sourcePrinter, Cursor cursor, PrintOutputCapture<P> p);
}
