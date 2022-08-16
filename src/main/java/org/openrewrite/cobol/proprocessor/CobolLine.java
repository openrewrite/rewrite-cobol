/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import lombok.Value;
import lombok.experimental.NonFinal;
import org.openrewrite.internal.lang.Nullable;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

@Value
public class CobolLine {

    public static CobolLine copyCobolLineWithContentArea(String contentArea, CobolLine line) {
        return new CobolLine(line.sequenceArea, line.sequenceAreaOriginal, line.indicatorArea,
                line.indicatorAreaOriginal, extractContentAreaA(contentArea), line.contentAreaAOriginal,
                extractContentAreaB(contentArea), line.contentAreaBOriginal, line.commentArea, line.commentAreaOriginal,
                line.format, line.dialect, line.number, line.type, line.predecessor, line.successor);
    }

    public static CobolLine copyCobolLineWithIndicatorAndContentArea(String indicatorArea,
                                                                     String contentArea, CobolLine line) {
        return new CobolLine(line.sequenceArea, line.sequenceAreaOriginal, indicatorArea, line.indicatorAreaOriginal,
                extractContentAreaA(contentArea), line.contentAreaAOriginal, extractContentAreaB(contentArea),
                line.contentAreaBOriginal, line.commentArea, line.commentAreaOriginal, line.format, line.dialect,
                line.number, line.type, line.predecessor, line.successor);
    }

    public static CobolLine copyCobolLineWithIndicatorArea(String indicatorArea, CobolLine line) {
        return new CobolLine(line.sequenceArea, line.sequenceAreaOriginal, indicatorArea, line.indicatorAreaOriginal,
                line.contentAreaA, line.contentAreaAOriginal, line.contentAreaB, line.contentAreaBOriginal,
                line.commentArea, line.commentAreaOriginal, line.format, line.dialect, line.number, line.type,
                line.predecessor, line.successor);
    }

    public static String createBlankSequenceArea(CobolPreprocessor.CobolSourceFormatEnum format) {
        return CobolPreprocessor.CobolSourceFormatEnum.TANDEM.equals(format) ? "" : IntStream.range(0, 6).mapToObj(i -> CobolPreprocessor.WS).collect(Collectors.joining(""));
    }

    static String extractContentAreaA(String contentArea) {
        return contentArea.length() > 4 ? contentArea.substring(0, 4) : contentArea;
    }

    static String extractContentAreaB(String contentArea) {
        return contentArea.length() > 4 ? contentArea.substring(4) : "";
    }

    public static CobolLine newCobolLine(String sequenceArea, String indicatorArea,
                                         String contentAreaA, String contentAreaB, String commentArea,
                                         CobolPreprocessor.CobolSourceFormatEnum format, CobolDialect dialect, int number,
                                         CobolLineTypeEnum type) {
        return new CobolLine(sequenceArea, sequenceArea, indicatorArea, indicatorArea, contentAreaA, contentAreaA,
                contentAreaB, contentAreaB, commentArea, commentArea, format, dialect, number, type, null, null);
    }

    String commentArea;

    String commentAreaOriginal;

    String contentAreaA;

    String contentAreaAOriginal;

    String contentAreaB;

    String contentAreaBOriginal;

    CobolDialect dialect;

    CobolPreprocessor.CobolSourceFormatEnum format;

    String indicatorArea;

    String indicatorAreaOriginal;

    int number;

    @NonFinal
    CobolLine predecessor;

    String sequenceArea;

    String sequenceAreaOriginal;

    @NonFinal
    CobolLine successor;

    CobolLineTypeEnum type;

    CobolLine(String sequenceArea, String sequenceAreaOriginal, String indicatorArea,
              String indicatorAreaOriginal, String contentAreaA, String contentAreaAOriginal,
              String contentAreaB, String contentAreaBOriginal, String commentArea,
              String commentAreaOriginal, CobolPreprocessor.CobolSourceFormatEnum format, CobolDialect dialect,
              int number, CobolLineTypeEnum type, @Nullable CobolLine predecessor, @Nullable CobolLine successor) {

        this.sequenceArea = sequenceArea;
        this.indicatorArea = indicatorArea;
        this.contentAreaA = contentAreaA;
        this.contentAreaB = contentAreaB;
        this.commentArea = commentArea;

        this.sequenceAreaOriginal = sequenceAreaOriginal;
        this.indicatorAreaOriginal = indicatorAreaOriginal;
        this.contentAreaAOriginal = contentAreaAOriginal;
        this.contentAreaBOriginal = contentAreaBOriginal;
        this.commentAreaOriginal = commentAreaOriginal;

        this.format = format;
        this.dialect = dialect;
        this.number = number;
        this.type = type;

        setPredecessor(predecessor);
        setSuccessor(successor);
    }

    public String getBlankSequenceArea() {
        return createBlankSequenceArea(format);
    }

    public String getContentArea() {
        return contentAreaA + contentAreaB;
    }

    public String getContentAreaOriginal() {
        return contentAreaAOriginal + contentAreaBOriginal;
    }

    public String serialize() {
        return sequenceArea + indicatorArea + contentAreaA + contentAreaB + commentArea;
    }

    public void setPredecessor(@Nullable CobolLine predecessor) {
        this.predecessor = predecessor;

        if (predecessor != null) {
            predecessor.successor = this;
        }
    }

    public void setSuccessor(@Nullable CobolLine successor) {
        this.successor = successor;

        if (successor != null) {
            successor.predecessor = this;
        }
    }

    @Override
    public String toString() {
        return serialize();
    }
}
