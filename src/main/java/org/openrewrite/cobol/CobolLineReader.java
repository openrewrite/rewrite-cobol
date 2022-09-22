package org.openrewrite.cobol;

import org.openrewrite.cobol.internal.CobolDialect;

import java.util.Scanner;

/**
 * Trim COBOL column areas, remove comments, mark comment entries, and concatenate String literals continued on new lines.
 */
public class CobolLineReader {

    private static final String EMPTY_STRING = "";
    private static final String[] triggersStart = new String[]{"AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
            "DATE-COMPILED.", "SECURITY.", "REMARKS."};

    private static final String[] triggersEnd = new String[]{"PROGRAM-ID.", "AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
            "DATE-COMPILED.", "SECURITY.", "ENVIRONMENT", "DATA.", "PROCEDURE."};

    private boolean inCommentEntry = false;

    public String readLines(String source, CobolDialect cobolDialect) {
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
            boolean isValidText = !(" ".equals(indicator) && contentArea.trim().isEmpty());

            if (inCommentEntry) {
                if (startsWithTrigger(contentArea, triggersEnd)) {
                    inCommentEntry = false;
                } else {
                    processedSource.append("*>CE ");
                }
            }

            if (startsWithTrigger(contentArea, triggersStart)) {
                inCommentEntry = true;
            }

            if (!inCommentEntry && "*".equals(indicator)) {
                processedSource.append("*> ");
            }

            if (isValidText) {
                String trimmedContentArea = trimLeadingWhitespace(contentArea);
                if ("-".equals(indicator) && trimmedContentArea.startsWith("\"") || trimmedContentArea.startsWith("'")) {
                    // Remove the previous newline character to concatenate the literal.
                    processedSource.delete(processedSource.length() - previousNewLineLength, processedSource.length());
                    // Remove the leading delimiter.
                    trimmedContentArea = trimLeadingChar(trimmedContentArea);

                    processedSource.append(trimmedContentArea);
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
                if (inCommentEntry || isValidText) {
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

    // TODO: add method to extract the first String at the start of a line based on the end condition being WS.
    // Change the String[] to a Set of Strings.
    private static boolean startsWithTrigger(String line, String[] triggers) {
        String contentAreaUpperCase = line.toUpperCase();

        boolean result = false;

        for (String trigger : triggers) {
            boolean containsTrigger = contentAreaUpperCase.trim().startsWith(trigger);

            if (containsTrigger) {
                result = true;
                break;
            }
        }

        return result;
    }
}
