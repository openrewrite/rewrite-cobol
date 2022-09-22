package org.openrewrite.cobol;

import org.openrewrite.cobol.internal.CobolDialect;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;

/**
 * Trim COBOL column areas, remove comments, mark comment entries, and concatenate String literals continued on new lines.
 */
public class CobolLineReader {

    private static final String EMPTY_STRING = "";
    private static final Set<String> triggersStart = new HashSet<>(Arrays.asList("AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
            "DATE-COMPILED.", "SECURITY.", "REMARKS."));
//    private static final String[] triggersStart = new String[]{"AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
//            "DATE-COMPILED.", "SECURITY.", "REMARKS."};

    private static final Set<String> triggersEnd = new HashSet<>(Arrays.asList("PROGRAM-ID.", "AUTHOR.", "INSTALLATION.",
            "DATE-WRITTEN.", "DATE-COMPILED.", "SECURITY.", "ENVIRONMENT", "DATA.", "PROCEDURE."));
//    private static final String[] triggersEnd = new String[]{"PROGRAM-ID.", "AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
//            "DATE-COMPILED.", "SECURITY.", "ENVIRONMENT", "DATA.", "PROCEDURE."};

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
                    // Mark the comment entry.
                    processedSource.append("*>CE ");
                }
            }

            // Comment entries are a specific type of comment that occur in the Identification Division.
            // Each comment entry needs to be marked uniquely to be recognized by the grammar.
            if (startsWithTrigger(contentArea, triggersStart)) {
                inCommentEntry = true;
                String firstWords = getFirstWords(contentArea);
                // Comment entries in older COBOL dialects may start in line with the paragraph start.
                if (!contentArea.substring(firstWords.length()).trim().isEmpty()) {
                    contentArea = firstWords + " *>CE " + contentArea.substring(firstWords.length());
                }
            }

            if (!inCommentEntry && "*".equals(indicator)) {
                // Mark in-line comments.
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

    private static String getFirstWords(String line) {
        int i = 0;
        char[] charArray = line.toCharArray();
        for (; i < charArray.length; i++) {
            char c = charArray[i];
            if (Character.isWhitespace(c)) {
                break;
            }
        }
        return line.substring(0, i);
    }

    private static boolean startsWithTrigger(String line, Set<String> triggers) {
        String firstWordsUpper = getFirstWords(line).toUpperCase();
        return triggers.contains(firstWordsUpper);
    }
}
