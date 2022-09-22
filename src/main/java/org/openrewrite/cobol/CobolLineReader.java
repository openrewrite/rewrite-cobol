package org.openrewrite.cobol;

import org.openrewrite.cobol.internal.CobolDialect;

import java.util.Scanner;

/**
 * Trim COBOL column areas, remove comments, mark comment entries, and concatenate String literals continued on new lines.
 */
public class CobolLineReader {

    private static final String EMPTY_STRING = "";

    public static String readLines(String source, CobolDialect cobolDialect) {
        int indicatorArea = cobolDialect.getColumns().getIndicatorArea();
        int contentAreaAStart = cobolDialect.getColumns().getContentArea();
        int contentAreaBEnd = cobolDialect.getColumns().getOtherArea();

        StringBuilder processedSource = new StringBuilder();

        int previousNewLineLength = 0;
        int cursor = 0;
        Scanner scanner = new Scanner(source);
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();

            String indicator = line.substring(indicatorArea, contentAreaAStart);
            String contentArea = line.substring(contentAreaAStart, contentAreaBEnd);
            boolean isValidText = !"*".equals(indicator) && !(" ".equals(indicator) && contentArea.trim().isEmpty());
            if (isValidText) {
                if ("-".equals(indicator)) {
                    // Trim the leading whitespace required for continuation indicators.
                    String trimmedContentArea = trimLeadingWhitespace(contentArea);
                    if (trimmedContentArea.startsWith("\"") || trimmedContentArea.startsWith("'")) {
                        // Remove the previous newline character to concatenate the literal.
                        processedSource.delete(processedSource.length() - previousNewLineLength, processedSource.length());
                        // Remove the leading delimiter.
                        trimmedContentArea = trimLeadingChar(trimmedContentArea);

                        processedSource.append(trimmedContentArea);
                    } else {
                        processedSource.append(contentArea);
                    }
                } else {
                    processedSource.append(contentArea);
                }
            }

            cursor += line.length();
            String endOfLine = source.substring(cursor).startsWith("\r\n") ? "\r\n" :
                    source.substring(cursor).startsWith("\n") ? "\n" : null;
            previousNewLineLength = endOfLine == null ? 0 : endOfLine.length();

            if (endOfLine != null) {
                cursor += endOfLine.length();
                if (isValidText) {
                    processedSource.append(endOfLine);
                }
            }
        }
        return processedSource.toString();
    }

    private static String trimLeadingChar(String contentArea) {
        return contentArea.substring(1);
    }

    private static String trimLeadingWhitespace(String contentarea) {
        return contentarea.replaceAll("^\\s+", EMPTY_STRING);
    }
}
