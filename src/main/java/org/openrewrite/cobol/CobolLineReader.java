package org.openrewrite.cobol;

import org.openrewrite.cobol.internal.CobolDialect;

import java.util.Scanner;

public class CobolLineReader {

    public static String readLines(String source, CobolDialect cobolDialect) {
        int indicatorArea = cobolDialect.getColumns().getIndicatorArea();
        int contentAreaAStart = cobolDialect.getColumns().getContentArea();
        int contentAreaBEnd = cobolDialect.getColumns().getOtherArea();

        StringBuilder processedSource = new StringBuilder();

        int cursor = 0;
        Scanner scanner = new Scanner(source);
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (!"*".equals(line.substring(indicatorArea, contentAreaAStart))) {
                processedSource.append(line, contentAreaAStart, contentAreaBEnd);
                cursor += line.length();

                String endOfLine = source.substring(cursor).startsWith("\r\n") ? "\r\n" :
                        source.substring(cursor).startsWith("\n") ? "\n" : null;
                if (endOfLine != null) {
                    cursor += endOfLine.length();
                    processedSource.append(endOfLine);
                }
            }
        }
        return processedSource.toString();
    }
}
