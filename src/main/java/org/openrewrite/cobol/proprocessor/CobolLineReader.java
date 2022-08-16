/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openrewrite.cobol.proprocessor.CobolLineTypeEnum.NORMAL;

public class CobolLineReader {

    public static CobolLineTypeEnum determineType(String indicatorArea) {
        CobolLineTypeEnum result;

        switch (indicatorArea) {
            case CobolPreprocessor.CHAR_D:
            case CobolPreprocessor.CHAR_D_:
                result = CobolLineTypeEnum.DEBUG;
                break;
            case CobolPreprocessor.CHAR_MINUS:
                result = CobolLineTypeEnum.CONTINUATION;
                break;
            case CobolPreprocessor.CHAR_ASTERISK:
            case CobolPreprocessor.CHAR_SLASH:
                result = CobolLineTypeEnum.COMMENT;
                break;
            case CobolPreprocessor.CHAR_DOLLAR_SIGN:
                result = CobolLineTypeEnum.COMPILER_DIRECTIVE;
                break;
            case CobolPreprocessor.WS:
            default:
                result = NORMAL;
                break;
        }

        return result;
    }

    public CobolLine parseLine(String line, int lineNumber, CobolParserParams params) {
        CobolPreprocessor.CobolSourceFormatEnum format = params.getFormat();
        Pattern pattern = format.getPattern();
        Matcher matcher = pattern.matcher(line);

        CobolLine result;

        if (matcher.matches()) {
            String sequenceAreaGroup = matcher.group(1);
            String indicatorAreaGroup = matcher.group(2);
            String contentAreaAGroup = matcher.group(3);
            String contentAreaBGroup = matcher.group(4);
            String commentAreaGroup = matcher.group(5);

            String sequenceArea = sequenceAreaGroup != null ? sequenceAreaGroup : "";
            String indicatorArea = indicatorAreaGroup != null ? indicatorAreaGroup : " ";
            String contentAreaA = contentAreaAGroup != null ? contentAreaAGroup : "";
            String contentAreaB = contentAreaBGroup != null ? contentAreaBGroup : "";
            String commentArea = commentAreaGroup != null ? commentAreaGroup : "";

            CobolLineTypeEnum type = determineType(indicatorArea);

            result = CobolLine.newCobolLine(sequenceArea, indicatorArea, contentAreaA, contentAreaB, commentArea,
                    params.getFormat(), params.getDialect(), lineNumber, type);
        } else {
            result = CobolLine.newCobolLine("", "", line, "", "",
                    params.getFormat(), params.getDialect(), lineNumber, NORMAL);
        }
        return result;
    }

    public List<CobolLine> processLines(String lines, CobolParserParams params) {
        Scanner scanner = new Scanner(lines);
        List<CobolLine> result = new ArrayList<>();

        String currentLine;
        CobolLine lastCobolLine = null;
        int lineNumber = 0;

        while (scanner.hasNextLine()) {
            currentLine = scanner.nextLine();

            CobolLine currentCobolLine = parseLine(currentLine, lineNumber, params);
            currentCobolLine.setPredecessor(lastCobolLine);
            result.add(currentCobolLine);

            lineNumber++;
            lastCobolLine = currentCobolLine;
        }

        scanner.close();
        return result;
    }
}
