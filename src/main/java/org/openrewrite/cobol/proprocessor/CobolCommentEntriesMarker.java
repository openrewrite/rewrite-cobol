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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CobolCommentEntriesMarker {

    Pattern commentEntryTriggerLinePattern;

    boolean foundCommentEntryTriggerInPreviousLine = false;

    boolean isInCommentEntry = false;

    String[] triggersEnd = new String[]{"PROGRAM-ID.", "AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
            "DATE-COMPILED.", "SECURITY.", "ENVIRONMENT", "DATA.", "PROCEDURE."};

    String[] triggersStart = new String[]{"AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
            "DATE-COMPILED.", "SECURITY.", "REMARKS."};

    public CobolCommentEntriesMarker() {
        String commentEntryTriggerLineFormat = "([ \\t]*)(" + String.join("|", triggersStart) + ")(.+)";
        commentEntryTriggerLinePattern = Pattern.compile(commentEntryTriggerLineFormat, Pattern.CASE_INSENSITIVE);
    }

    CobolLine buildMultiLineCommentEntryLine(CobolLine line) {
        return CobolLine.copyCobolLineWithIndicatorArea(CobolPreprocessor.COMMENT_ENTRY_TAG + CobolPreprocessor.WS,
                line);
    }

    /**
     * Escapes in a given line a potential comment entry.
     */
    CobolLine escapeCommentEntry(CobolLine line) {
        CobolLine result;

        Matcher matcher = commentEntryTriggerLinePattern.matcher(line.getContentArea());

        if (matcher.matches()) {
            String whitespace = matcher.group(1);
            String trigger = matcher.group(2);
            String commentEntry = matcher.group(3);
            String newContentArea = whitespace + trigger + CobolPreprocessor.WS
                    + CobolPreprocessor.COMMENT_ENTRY_TAG + commentEntry;

            result = CobolLine.copyCobolLineWithContentArea(newContentArea, line);
        } else {
            result = line;
        }

        return result;
    }

    boolean isInCommentEntry(CobolLine line, boolean isContentAreaAEmpty,
                                       boolean isInOsvsCommentEntry) {
        return CobolLineTypeEnum.COMMENT.equals(line.getType()) || isContentAreaAEmpty
                || isInOsvsCommentEntry;
    }

    /**
     * OSVS: The comment-entry can be contained in either area A or area B of the
     * comment-entry lines. However, the next occurrence in area A of any one of the
     * following COBOL words or phrases terminates the comment-entry and begin the
     * next paragraph or division.
     */
    boolean isInOsvsCommentEntry(CobolLine line) {
        return CobolDialect.OSVS.equals(line.getDialect()) && !startsWithTrigger(line, triggersEnd);
    }

    public CobolLine processLine(CobolLine line) {
        CobolLine result;

        if (line.getFormat().isCommentEntryMultiLine()) {
            result = processMultiLineCommentEntry(line);
        } else {
            result = processSingleLineCommentEntry(line);
        }

        return result;
    }

    public List<CobolLine> processLines(List<CobolLine> lines) {
        List<CobolLine> result = new ArrayList<>();

        for (CobolLine line : lines) {
            CobolLine processedLine = processLine(line);
            result.add(processedLine);
        }

        return result;
    }

    /**
     * If the Compiler directive SOURCEFORMAT is specified as or defaulted to FIXED,
     * the comment-entry can be contained on one or more lines but is restricted to
     * area B of those lines; the next line commencing in area A begins the next
     * non-comment entry.
     */
    CobolLine processMultiLineCommentEntry(CobolLine line) {
        boolean foundCommentEntryTriggerInCurrentLine = startsWithTrigger(line, triggersStart);
        CobolLine result;

        if (foundCommentEntryTriggerInCurrentLine) {
            result = escapeCommentEntry(line);
        } else if (foundCommentEntryTriggerInPreviousLine || isInCommentEntry) {
            boolean isContentAreaAEmpty = line.getContentAreaA().trim().isEmpty();
            boolean isInOsvsCommentEntry = isInOsvsCommentEntry(line);

            isInCommentEntry = isInCommentEntry(line, isContentAreaAEmpty, isInOsvsCommentEntry);

            if (isInCommentEntry) {
                result = buildMultiLineCommentEntryLine(line);
            } else {
                result = line;
            }
        } else {
            result = line;
        }

        foundCommentEntryTriggerInPreviousLine = foundCommentEntryTriggerInCurrentLine;

        return result;
    }

    CobolLine processSingleLineCommentEntry(CobolLine line) {
        boolean foundCommentEntryTriggerInCurrentLine = startsWithTrigger(line, triggersStart);
        CobolLine result;

        if (foundCommentEntryTriggerInCurrentLine) {
            result = escapeCommentEntry(line);
        } else {
            result = line;
        }

        return result;
    }

    /**
     * Checks, whether given line starts with a trigger keyword indicating a comment
     * entry.
     */
    boolean startsWithTrigger(CobolLine line, String[] triggers) {
        String contentAreaUpperCase = line.getContentArea().toUpperCase();

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
