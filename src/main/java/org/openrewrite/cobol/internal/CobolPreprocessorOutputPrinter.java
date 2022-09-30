/*
 * Copyright 2022 the original author or authors.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * https://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openrewrite.cobol.internal;

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.openrewrite.cobol.internal.CobolGrammarToken.COMMENT_ENTRY;

/**
 * Print the post processed COBOL AST with comments that act like a `JavaTemplate`.
 * The comments are used to link AST elements that contain the original source code to the COBOL AST.
 *
 * Each key will be added as a comment that is formatted based on the current {@link CobolDialect}.
 *
 * `printWithColumnAreas`:
 *      true: Print as source code with modifications to distinguish changes during preprocessing like COPY and REPLACE.
 *      false: Print as parser input for the CobolParserVisitor.
 */
public class CobolPreprocessorOutputPrinter<P> extends CobolPreprocessorPrinter<P> {
    // The keys may be replaced by UUIDs for uniqueness, but are human-readable until COBOL is understood.

    // START keys mark the line where whitespace is added until the end of the content area.
    // STOP keys mark when the exit of a template area.
    // UUID keys link the CobolPreprocessor AST to the CobolParser.
    public static final String COPY_START_KEY = "__COPY_START__";
    public static final String COPY_STOP_KEY = "__COPY_STOP__";
    public static final String COPY_UUID_KEY = "__COPY_UUID__";

    public static final String REPLACE_START_KEY = "__REPLACE_START__";
    public static final String REPLACE_STOP_KEY = "__REPLACE_STOP__";
    public static final String REPLACE_UUID_KEY = "__REPLACE_UUID__";

    public static final String UUID_KEY = "__UUID__";

    // The ReplaceBy template is fully parsed between the start and stop and do not require a unique UUID.
    public static final String REPLACE_BY_START_KEY = "__REPLACE_BY_START__";
    public static final String REPLACE_BY_STOP_KEY = "__REPLACE_BY_STOP__";

    // The ReplaceOff template is fully parsed between the start and stop and do not require a unique UUID.
    public static final String REPLACE_OFF_START_KEY = "__REPLACE_OFF_START__";
    public static final String REPLACE_OFF_STOP_KEY = "__REPLACE_OFF_STOP__";

    // The ReplaceTypeReductive template is fully parsed between the start and stop and do not require a unique UUID.
    public static final String REPLACE_TYPE_REDUCTIVE_START_KEY = "__REPLACE_TYPE_START_REDUCTIVE__";
    public static final String REPLACE_TYPE_REDUCTIVE_STOP_KEY = "__REPLACE_TYPE_STOP_REDUCTIVE__";

    public static final String REPLACE_TYPE_ADDITIVE_KEY = "__REPLACE_TYPE_ADDITIVE__";

    private final CobolDialect cobolDialect;
    private final boolean printColumns;

    // Lazily initialized Strings that are generated once with constraints based on the dialect.
    private String dialectSequenceArea = null;
    private String uuidEndOfLine = null;

    // CopyStatement comments.
    private String copyStartComment = null;
    private String copyStopComment = null;
    private String copyUuidComment = null;

    // Words that have been replaced by replace rules.
    private String replaceStartComment = null;
    private String replaceStopComment = null;
    private String replaceUuidComment = null;

    // Generic UUID comment.
    private String uuidComment = null;

    // ReplaceByStatement comments.
    private String replaceByStartComment = null;
    private String replaceByStopComment = null;

    // ReplaceOff comments.
    private String replaceOffStartComment = null;
    private String replaceOffStopComment = null;

    // ReplaceTypeReductive comments.
    private String replaceTypeReductiveStartComment = null;
    private String replaceTypeReductiveStopComment = null;

    // Represents whitespace used to keep the original AST and the processed AST aligned in the column area.
    private String replaceTypeAdditiveComment = null;

    // Lines prefixed with an unknown indicator are commented out during printing until we know more about them.
    private boolean inUnknownIndicator = false;

    private ReplaceReductiveType replaceReductiveType = null;
    private final CobolPreprocessorPrinter<ExecutionContext> statementPrinter = new CobolPreprocessorPrinter<>(false);

    public CobolPreprocessorOutputPrinter(CobolDialect cobolDialect,
                                          boolean printColumns) {
        super(true);

        this.cobolDialect = cobolDialect;
        this.printColumns = printColumns;
    }

    @Override
    public CobolPreprocessor visitCommentEntry(CobolPreprocessor.CommentEntry commentEntry, PrintOutputCapture<P> p) {
        if (printColumns) {
            super.visitCommentEntry(commentEntry, p);
        } else {
            visitSpace(commentEntry.getPrefix(), p);
            visitMarkers(commentEntry.getMarkers(), p);
            for (CobolPreprocessor.Word comment : commentEntry.getComments()) {
                p.append(COMMENT_ENTRY);
                visit(comment, p);
            }
        }

        return commentEntry;
    }

    @Override
    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
        visitSpace(copyBook.getPrefix(), p);
        visitMarkers(copyBook.getMarkers(), p);
        visit(copyBook.getAst(), p);
        visit(copyBook.getEof(), p);
        return copyBook;
    }

    @Override
    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        visitSpace(copyStatement.getPrefix(), p);
        visitMarkers(copyStatement.getMarkers(), p);

        if (printColumns) {
            if (copyStatement.getCopyBook() != null) {
                copyTemplate(copyStatement, p);
            }
        } else {
            visit(copyStatement.getCopyBook(), p);
        }

        return copyStatement;
    }

    private void copyTemplate(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        // Printing the COPY statement will add comments that work similar to JavaTemplate.
        // Comments are added before and after the template to provide context about which AST elements
        // are a product of a COPY statement.

        /*
         *  Before:
         *      |000001| |Some COBOL tokens|      COPY STATEMENT.           |
         *
         *  After:
         *      |      |*|__COPY_START______________________________________|=> Trigger search for end of line to detect whitespace added from the template.
         *      |000001| |Some COBOL tokens|                                |=> The index + 1 is the position of `|`.
         *      |      |*|__UUID____________________________________________|=> Detect the UUID section of the template.
         *      |      |*|263cd588-bdea-4c06-8ba1-177e515bded2              |=> UUID to the CopyStatement; a UUID will fit in the column area, but the copy statement might not.
         *      |~~~~~~| |Print the COPIED source AST. ~~~~~~~~~~~~~~~~~~~~~|=> Print the COPIED AST, which includes new column areas.
         *      |      |*|__COPY_END________________________________________|
         *      |      |*|33                                                |=> # of spaces added to align the column areas.
         *      |      | |[WS for tokens  ]|[WS for COPY        ]|=> White space is conditionally printer based on where the copy statement ends to ensure columns are aligned.
         *
         *  Alignment:
         *      |      | | COPY STATEMENT.                                  |=> Requires whitespace to replace the statement for correct alignment.
         *      |      | |                                   COPY STATEMENT.|=> The next line does not require any whitespace.
         */

        // Print markers like Lines, SequenceArea, and Indicator if the line starts with COPY.
        visit(copyStatement.getWord(), p);

        // Remove the prefix of and the word COPY, because the statement is replaced by the CopyBook.
        p.out.delete(p.getOut().length() - copyStatement.getWord().getWord().length() -
                copyStatement.getWord().getPrefix().getWhitespace().length(), p.getOut().length());

        // Save the current index to ensure the text that follows the COPY will be aligned correctly.
        int curIndex = getCurrentIndex(p.getOut());

        addStartKey(getCopyStartComment(), curIndex, p);
        addUuidKey(getCopyUuidKey(), copyStatement.getId(), p);

        // Print copied source.
        visit(copyStatement.getCopyBook(), p);
        if (!p.getOut().endsWith("\n")) {
            // Add a new line character if the copied source does not end with one already.
            p.append("\n");
        }

        addStopComment(getCopyStopComment(), copyStatement, curIndex, p);
    }

    @Override
    public CobolPreprocessor visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, PrintOutputCapture<P> p) {
        if (printColumns) {
            replaceByTemplate(replaceArea, p);
        }

        if (replaceArea.getCobols() != null) {
            for (CobolPreprocessor cobol : replaceArea.getCobols()) {
                visit(cobol, p);
            }
        }
        visit(replaceArea.getReplaceOffStatement(), p);
        return replaceArea;
    }

    private void replaceByTemplate(CobolPreprocessor.ReplaceArea replaceArea, PrintOutputCapture<P> p) {
        CobolPreprocessor.ReplaceByStatement replaceByStatement = replaceArea.getReplaceByStatement();

        // Save the current index to ensure the text that follows the ReplaceByStatement will be aligned correctly.
        int curIndex = getCurrentIndex(p.getOut());

        addStartKey(getReplaceByStartComment(), curIndex, p);
        addUuidKey(getUuidComment(), replaceByStatement.getId(), p);
        addStopComment(getReplaceByStopComment(), replaceArea.getReplaceByStatement(), curIndex, p);
    }

    @Override
    public CobolPreprocessor visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, PrintOutputCapture<P> p) {
        if (printColumns) {
            replaceOffTemplate(replaceOffStatement, p);
        }
        return replaceOffStatement;
    }

    private void replaceOffTemplate(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, PrintOutputCapture<P> p) {
        // Save the current index to ensure the text that follows the ReplaceOffStatement will be aligned correctly.
        int curIndex = getCurrentIndex(p.getOut());

        addStartKey(getReplaceOffStartComment(), curIndex, p);
        addUuidKey(getUuidComment(), replaceOffStatement.getId(), p);
        addStopComment(getReplaceOffStopComment(), replaceOffStatement, curIndex, p);
    }

    @Override
    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        if (!printColumns) {
            // Do not print words on lines with an unknown indicator until we know how to handle them.
            // Note: Unknown indicators are treated as comments via source code in CobolParserVisitor#isCommentIndicator.
            Optional<IndicatorArea> maybeUnknown = word.getMarkers().findFirst(IndicatorArea.class);
            if (maybeUnknown.isPresent()) {
                String indicator = maybeUnknown.get().getIndicator();
                if ("G".equals(indicator) || "J".equals(indicator) || "P".equals(indicator)) {
                    // TODO: add a logger and log a warning or any form of visibility.
                    inUnknownIndicator = true;
                }
            }

            if (!inUnknownIndicator) {
                Optional<ReplaceAdditiveType> replaceAdditiveTypeOptional = word.getMarkers().findFirst(ReplaceAdditiveType.class);
                if (replaceAdditiveTypeOptional.isPresent()) {
                    ReplaceAdditiveType replaceAdditiveType = replaceAdditiveTypeOptional.get();
                    for (Replace additionalWord : replaceAdditiveType.getAdditionalWords()) {
                        visit(additionalWord.getOriginalWord(), p);
                    }
                }

                visitSpace(word.getPrefix(), p);
                visitMarkers(word.getMarkers(), p);
                p.append(word.getWord());

                Optional<CommentArea> commentArea = word.getMarkers().findFirst(CommentArea.class);
                commentArea.ifPresent(area -> visitSpace(area.getPrefix(), p));
                commentArea.ifPresent(area -> visitSpace(area.getEndOfLine(), p));
            }

            if (inUnknownIndicator && word.getMarkers().findFirst(CommentArea.class).isPresent()) {
                inUnknownIndicator = false;
            }
            return word;
        }

        // Beware all who enter.
        // Replace contains many special cases due to changes in column alignment after a replacement.
        Optional<Replace> replaceOptional = word.getMarkers().findFirst(Replace.class);
        Optional<ReplaceReductiveType> replaceTypeReductiveOptional = word.getMarkers().findFirst(ReplaceReductiveType.class);
        if (replaceOptional.isPresent()) {
            Optional<Lines> replaceLines = replaceOptional.get().getOriginalWord().getMarkers().findFirst(Lines.class);
            replaceLines.ifPresent(lines -> visitLines(lines, p));

            replaceTemplate(word, p, replaceOptional.get());
        } else if (replaceTypeReductiveOptional.isPresent()) {
            if (replaceReductiveType == null) {
                replaceReductiveType = replaceTypeReductiveOptional.get();
            }
        } else if (replaceReductiveType != null) {
            reductiveTemplate(p);

            super.visitWord(word, p);
            replaceReductiveType = null;
        } else {
            super.visitWord(word, p);
        }
        return word;
    }

    private void reductiveTemplate(PrintOutputCapture<P> p) {
        // Print the markers from the original words and replace the original words with whitespace.
        for (Replace replace : replaceReductiveType.getOriginalWords()) {
            CobolPreprocessor.Word originalWord = replace.getOriginalWord();
            Optional<Continuation> continuation = originalWord.getMarkers().findFirst(Continuation.class);
            if (continuation.isPresent()) {
                throw new UnsupportedOperationException("Implement continuation lines for a reductive replacement.");
            }

            Optional<SequenceArea> sequenceArea = originalWord.getMarkers().findFirst(SequenceArea.class);
            sequenceArea.ifPresent(it -> visitSequenceArea(it, p));

            Optional<IndicatorArea> indicatorArea = originalWord.getMarkers().findFirst(IndicatorArea.class);
            indicatorArea.ifPresent(it -> visitIndicatorArea(it, p));

            visitSpace(originalWord.getPrefix(), p);

            String replaceWithWhitespace = generateWhitespace(originalWord.getWord().length());
            p.append(replaceWithWhitespace);

            Optional<CommentArea> commentArea = originalWord.getMarkers().findFirst(CommentArea.class);
            commentArea.ifPresent(it -> visitCommentArea(it, p));
        }

        // Save the current index to ensure the text that follows the REPLACE will be aligned correctly.
        int curIndex = getCurrentIndex(p.getOut());
        if (curIndex == -1) {
            throw new UnsupportedOperationException("Unknown case: Detected a ReplaceTypeReductive at the start of the source code.");
        }

        // Fill the remaining line with whitespace to align the column areas.
        // Reductive changes require printing the original column areas with the original word replaced by whitespace.
        // The last replaced word might end the line, so the current index might be greater than (CommentArea) or
        // equal to the end of the content area.
        int contentEnd = cobolDialect.getColumns().getOtherArea();
        int untilEndOfLine = curIndex >= contentEnd ? 0 : cobolDialect.getColumns().getOtherArea() - curIndex;
        String whitespace = generateWhitespace(untilEndOfLine) + "\n";
        p.append(whitespace);

        // Add Start key.
        p.append(getReplaceTypeReductiveStartComment());

        // Add UUID key.
        addUuidKey(getUuidComment(), replaceReductiveType.getId(), p);

        // Add Stop key.
        p.append(getReplaceTypeReductiveStopComment());

        // Add whitespace until the next token will be aligned with the column area.
        String afterStop = getColumnAlignmentAfterStop(curIndex);
        p.append(afterStop);
        p.append(StringUtils.repeat(" ", curIndex >= contentEnd ? 0 : curIndex));
    }

    private void replaceTemplate(CobolPreprocessor.Word word, PrintOutputCapture<P> p, Replace replace) {
        // Save the current index to ensure the text that follows the REPLACE will be aligned correctly.
        int curIndex = getCurrentIndex(p.getOut());
        if (curIndex == -1) {
            throw new UnsupportedOperationException("Unknown case: Detected a Replace at the start of the source code.");
        }

        boolean isLongerWord = word.getWord().length() > replace.getOriginalWord().getWord().length();
        String replacedWord = isLongerWord ? " " + word.getWord() : word.getWord();

        boolean isLiteral = word.getWord().startsWith("\"") || word.getWord().startsWith("'");
        int contentAreaLength = cobolDialect.getColumns().getOtherArea() - cobolDialect.getColumns().getContentArea();
        boolean isContinuedLiteral = isLiteral && (curIndex + replacedWord.length()) > contentAreaLength;

        // Add Start key.
        int insertIndex = getInsertIndex(p.getOut());
        p.out.insert(insertIndex, getReplaceStartComment());

        if (isLongerWord && !isContinuedLiteral) {
            insertIndex = getInsertIndex(p.getOut());
            p.out.insert(insertIndex, getReplaceTypeAdditiveComment());
        }

        if (curIndex == 0) {
            Optional<SequenceArea> sequenceArea = word.getMarkers().findFirst(SequenceArea.class);
            sequenceArea.ifPresent(it -> visitSequenceArea(it, p));

            Optional<IndicatorArea> indicatorArea = word.getMarkers().findFirst(IndicatorArea.class);
            indicatorArea.ifPresent(it -> visitIndicatorArea(it, p));
        }

        // Fill in the rest of the content area with whitespace.
        int untilEndOfLine =  cobolDialect.getColumns().getOtherArea() - (curIndex == 0 ? cobolDialect.getColumns().getContentArea() : curIndex);
        String whitespace = generateWhitespace(untilEndOfLine) + "\n";
        p.append(whitespace);

        // Add UUID key.
        addUuidKey(getReplaceUuidComment(), replace.getId(), p);

        // Add Stop key.
        p.append(getReplaceStopComment());

        // Additive replacement like PIC to PICTURE.
        if (isLongerWord) {
            if (isContinuedLiteral) {
                int numberOfSpaces = (curIndex == 0 ? cobolDialect.getColumns().getContentArea() : curIndex);
                String afterStop = getColumnAlignmentAfterStop(numberOfSpaces);
                p.append(afterStop);
                p.append(StringUtils.repeat(" ", numberOfSpaces));

                // The current word must be a literal.
                /*
                 *  I.E. "Z" replaced by """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                 *  Before:
                 *      |036800| |    MOVE "Z"| TO WRK-XN-00322.         |SM2084.2
                 *
                 *  After:
                 *      |      | |REPLACE_START__________________________|
                 *      |036800| |    MOVE """"""""""""""""""""""""""""""|
                 *      |      |-|"""""""""""""""""""""""""""""""""""""""|
                 *      |      |-|""""""""""""| TO WRK-XN-00322.         |SM2084.2
                 */

                // Predetermine the length of the end of the literal to align the column areas with the next token.
                String dialectSequenceArea = getDialectSequenceArea();
                int originalLength = replace.getOriginalWord().getPrefix().getWhitespace().length() + replace.getOriginalWord().getWord().length();
                int endPos = replacedWord.length() - curIndex + getDialectSequenceArea().length() + 1 - originalLength;
                String end = dialectSequenceArea + "-" + replacedWord.substring(endPos);
                replacedWord = replacedWord.substring(0, replacedWord.length() - end.length());

                // Split the rest of the literal into continuable parts.
                int remainder = replacedWord.length() % contentAreaLength;
                int size = replacedWord.length() / contentAreaLength + (remainder == 0 ? 0 : 1);
                List<String> parts = new ArrayList<>(size);
                int total = replacedWord.length();
                for (int i = 0; i < size; i++) {
                    if (total - contentAreaLength >= 0) {
                        String part = replacedWord.substring(total - contentAreaLength, total);
                        parts.add(part);
                        total -= part.length();
                    } else {
                        if (total != remainder) {
                            throw new IllegalStateException("Unexpected remained calculating replacement end position.");
                        }
                        parts.add(replacedWord.substring(0, remainder));
                    }
                }

                for (int i = parts.size() - 1; i >= 0; i--) {
                    String part = parts.get(i);
                    if (i != parts.size() - 1) {
                        p.append(getDialectSequenceArea());
                        p.append("-");
                    }
                    p.append(part);
                    if (i == parts.size() - 1 && part.length() < contentAreaLength) {
                        // // Total area to be filled - existing characters.
                        untilEndOfLine = (getDialectSequenceArea().length() + 1 + contentAreaLength) - (curIndex + part.length());
                        whitespace = generateWhitespace(untilEndOfLine);
                        p.append(whitespace);
                    }
                    p.append("\n");
                }
                p.append(end);
            } else {
                int difference = word.getWord().length() - replace.getOriginalWord().getWord().length();
                int alignColumn = curIndex == 0 ? cobolDialect.getColumns().getContentArea() : curIndex;
                int total = alignColumn + word.getPrefix().getWhitespace().length() + difference;
                if (total > cobolDialect.getColumns().getOtherArea()) {
                    throw new UnsupportedOperationException("The position of the replaced word exceeds the column area.");
                }
                String prefix = generateWhitespace(total);
                p.append(prefix);
                p.append(word.getWord());
            }
        } else {
            PrintOutputCapture<ExecutionContext> outputCapture = new PrintOutputCapture<>(new InMemoryExecutionContext());
            statementPrinter.visit(replace.getOriginalWord(), outputCapture);

            String statement = outputCapture.getOut();
            boolean isEndOfLine = statement.endsWith("\n");
            boolean isCRLF = statement.endsWith("\r\n");

            int totalChars = statement.length() + curIndex - cobolDialect.getColumns().getContentArea() - (isEndOfLine ? (isCRLF ? 2 : 1) : 0);

            int numberOfSpaces;
            if (!isEndOfLine && totalChars > contentAreaLength) {
                String replacement = replace.getOriginalWord().print(getCursor());
                numberOfSpaces = getCurrentIndex(replacement);
            } else {
                numberOfSpaces = (curIndex == 0 ? cobolDialect.getColumns().getContentArea() : curIndex);
            }

            String afterStop = getColumnAlignmentAfterStop(numberOfSpaces);
            p.append(afterStop);
            p.append(StringUtils.repeat(" ", numberOfSpaces));

            /*  The original word is <= the length of the replaced word.
             *  To retain column alignment, the prefix is shifted left with whitespace equal to the difference between the original word and the replaced word.
             *
             *  I.E. PICTURE replaced by PIC.
             *  Before:
             *      |000001| | firstWord PICTURE secondWord         |
             *  After:
             *      |000001| | firstWord     PIC secondWord         |
             */
            int difference = replace.getOriginalWord().getWord().length() - word.getWord().length();
            // The difference exceeds the content area.
            if (curIndex + difference > cobolDialect.getColumns().getOtherArea()) {
                String fullWord = replace.getOriginalWord().print(getCursor());
                int lastIndex = fullWord.lastIndexOf("\n");
                fullWord = fullWord.substring(lastIndex + 1);

                if (fullWord.length() - word.getWord().length() < curIndex) {
                    p.out.delete(p.getOut().length() - (curIndex - (fullWord.length() - word.getWord().length())), p.getOut().length());
                } else {
                    p.append(StringUtils.repeat(" ", fullWord.length() - word.getWord().length() - curIndex));
                }

                p.append(word.getWord());
            } else {
                String additionalPrefix = StringUtils.repeat(" ", difference);
                p.append(additionalPrefix);
                visitSpace(word.getPrefix(), p);
                p.append(word.getWord());
            }
        }

        Optional<CommentArea> commentArea = word.getMarkers().findFirst(CommentArea.class);
        commentArea.ifPresent(it -> visitCommentArea(it, p));
    }


    /**
     * Return the index to insert a Template START comment at.
     */
    private int getInsertIndex(String output) {
        int insertIndex = output.lastIndexOf("\n");
        return insertIndex == -1 ? 0 : insertIndex + 1;
    }

    /**
     * Return the index position of the current line.
     */
    private int getCurrentIndex(String output) {
        int index = output.lastIndexOf("\n");
        return index == -1 ? index : output.substring(index + 1).length();
    }

    private String generateWhitespace(int count) {
        if (count < 0) {
            throw new IllegalStateException("Negative index detected.");
        }
        return StringUtils.repeat(" ", count);
    }

    /**
     * Add a templates START comment.
     * @param startComment start comment for the template type.
     * @param curIndex the position of the current index.
     */
    private void addStartKey(String startComment, int curIndex, PrintOutputCapture<P> p) {
        if (curIndex == -1) {
            throw new UnsupportedOperationException("Negative index detected for: " + startComment);
        }

        int insertIndex = getInsertIndex(p.getOut());
        p.out.insert(insertIndex, startComment);

        // Fill the remaining line with whitespace to align the column areas.
        int untilEndOfLine = cobolDialect.getColumns().getOtherArea() - curIndex;
        String whitespace = generateWhitespace(untilEndOfLine) + "\n";
        p.append(whitespace);
    }

    /**
     * Add a templates UUID comment.
     * @param uuidComment uuid comment for the template type.
     * @param uuid uuid from an AST element to retrieve.
     */
    private void addUuidKey(String uuidComment, UUID uuid, PrintOutputCapture<P> p) {
        p.append(uuidComment);
        String replaceUuidLine = getDialectSequenceArea() + "*" + uuid + getUuidEndOfLine();
        p.append(replaceUuidLine);
    }

    /**
     * Add a template STOP comment and align the whitespace for the next Word.
     * @param stopComment STOP comment for the template type.
     * @param statement current preprocessor element being processed.
     * @param curIndex the cursor position before the current statement.
     */
    private void addStopComment(String stopComment, CobolPreprocessor statement, int curIndex, PrintOutputCapture<P> p) {
        p.append(stopComment);

        PrintOutputCapture<ExecutionContext> outputCapture = new PrintOutputCapture<>(new InMemoryExecutionContext());
        statementPrinter.visit(statement, outputCapture);

        String copy = outputCapture.getOut();
        boolean isEndOfLine = copy.endsWith("\n");
        boolean isCRLF = copy.endsWith("\r\n");

        int totalChars = copy.length() + curIndex - cobolDialect.getColumns().getContentArea() - (isEndOfLine ? (isCRLF ? 2 : 1) : 0);
        int contentAreaLength = cobolDialect.getColumns().getOtherArea() - cobolDialect.getColumns().getContentArea();

        int numberOfSpaces;
        if (!isEndOfLine && totalChars > contentAreaLength) {
            throw new UnsupportedOperationException("Recalculate prefix.");
        } else {
            numberOfSpaces = isEndOfLine ? 0 : copy.length() + curIndex;
        }

        String afterStop = getColumnAlignmentAfterStop(numberOfSpaces);
        p.append(afterStop);
        p.append(StringUtils.repeat(" ", numberOfSpaces));
    }

    /**
     * Calculate the whitespace required to align the column area based on the position of the previous word.
     * Then return a Template Comment with the calculated information.
     */
    private String getColumnAlignmentAfterStop(int lengthOfPrefix) {
        if (lengthOfPrefix > 0 && lengthOfPrefix - cobolDialect.getColumns().getContentArea() < 0) {
            throw new IllegalStateException("Negative index detected.");
        }

        int startOfContentArea = cobolDialect.getColumns().getContentArea();
        int endOfContentArea = cobolDialect.getColumns().getOtherArea();

        int prefixLength = lengthOfPrefix == 0 || lengthOfPrefix >= endOfContentArea ? 0 : (lengthOfPrefix - startOfContentArea);
        prefixLength = prefixLength == endOfContentArea - startOfContentArea ? 0 : prefixLength;

        String alignmentKey = getDialectSequenceArea() + "*" + prefixLength;
        String whitespace = generateWhitespace(endOfContentArea - alignmentKey.length());
        return alignmentKey + whitespace + "\n";
    }

    /**
     * Generate a comment based on the Key and the length of the ContentArea in the COBOL dialect.
     */
    private String getTemplateComment(String key) {
        String start = getDialectSequenceArea() + "*" + key;
        return start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
    }

    /**
     * Lazily load the START comment of a {@link CopyStatement} template.
     */
    public String getCopyStartComment() {
        if (copyStartComment == null) {
            copyStartComment = getTemplateComment(COPY_START_KEY);
        }
        return copyStartComment;
    }

    /**
     * Lazily load the STOP comment of a {@link CopyStatement} template.
     */
    public String getCopyStopComment() {
        if (copyStopComment == null) {
            copyStopComment = getTemplateComment(COPY_STOP_KEY);
        }
        return copyStopComment;
    }

    /**
     * Lazily load the UUID comment of a {@link CopyStatement} template.
     */
    public String getCopyUuidKey() {
        if (copyUuidComment == null) {
            copyUuidComment = getTemplateComment(COPY_UUID_KEY);
        }
        return copyUuidComment;
    }

    /**
     * Lazily load the START key of a {@link Replace} template.
     */
    public String getReplaceStartComment() {
        if (replaceStartComment == null) {
            replaceStartComment = getTemplateComment(REPLACE_START_KEY);
        }
        return replaceStartComment;
    }

    /**
     * Lazily load the STOP comment of a {@link Replace} template.
     */
    public String getReplaceStopComment() {
        if (replaceStopComment == null) {
            replaceStopComment = getTemplateComment(REPLACE_STOP_KEY);
        }
        return replaceStopComment;
    }

    /**
     * Lazily load the UUID comment of a {@link Replace} template.
     */
    public String getReplaceUuidComment() {
        if (replaceUuidComment == null) {
            replaceUuidComment = getTemplateComment(REPLACE_UUID_KEY);
        }
        return replaceUuidComment;
    }

    /**
     * Lazily load the START comment of a {@link ReplaceBy} template.
     */
    public String getReplaceByStartComment() {
        if (replaceByStartComment == null) {
            replaceByStartComment = getTemplateComment(REPLACE_BY_START_KEY);
        }
        return replaceByStartComment;
    }

    /**
     * Lazily load the STOP comment of a {@link ReplaceBy} template.
     */
    public String getReplaceByStopComment() {
        if (replaceByStopComment == null) {
            replaceByStopComment = getTemplateComment(REPLACE_BY_STOP_KEY);
        }
        return replaceByStopComment;
    }

    /**
     * Lazily load the {@link ReplaceAdditiveWhitespace} comment of a {@link Replace} template.
     */
    public String getReplaceTypeAdditiveComment() {
        if (replaceTypeAdditiveComment == null) {
            replaceTypeAdditiveComment = getTemplateComment(REPLACE_TYPE_ADDITIVE_KEY);
        }
        return replaceTypeAdditiveComment;
    }

    /**
     * Lazily load the START comment of a {@link ReplaceOff} template.
     */
    public String getReplaceOffStartComment() {
        if (replaceOffStartComment == null) {
            replaceOffStartComment = getTemplateComment(REPLACE_OFF_START_KEY);
        }
        return replaceOffStartComment;
    }

    /**
     * Lazily load the STOP comment of a {@link ReplaceOff} template.
     */
    public String getReplaceOffStopComment() {
        if (replaceOffStopComment == null) {
            replaceOffStopComment = getTemplateComment(REPLACE_OFF_STOP_KEY);
        }
        return replaceOffStopComment;
    }

    /**
     * Lazily load the START comment of a {@link ReplaceReductiveType} template.
     */
    public String getReplaceTypeReductiveStartComment() {
        if (replaceTypeReductiveStartComment == null) {
            replaceTypeReductiveStartComment = getTemplateComment(REPLACE_TYPE_REDUCTIVE_START_KEY);
        }
        return replaceTypeReductiveStartComment;
    }

    /**
     * Lazily load the STOP comment of a {@link ReplaceReductiveType} template.
     */
    public String getReplaceTypeReductiveStopComment() {
        if (replaceTypeReductiveStopComment == null) {
            replaceTypeReductiveStopComment = getTemplateComment(REPLACE_TYPE_REDUCTIVE_STOP_KEY);
        }
        return replaceTypeReductiveStopComment;
    }

    /**
     * Lazily loaded the generic UUID comment for templates that are immediately fully parsed from START to STOP.
     */
    public String getUuidComment() {
        if (uuidComment == null) {
            uuidComment = getTemplateComment(UUID_KEY);
        }
        return uuidComment;
    }

    private String getUuidEndOfLine() {
        if (uuidEndOfLine == null) {
            uuidEndOfLine = StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - cobolDialect.getColumns().getContentArea() - 36) + "\n";
        }
        return uuidEndOfLine;
    }

    /**
     * Lazily load a String to fill the SequenceArea with whitespace based on the {@link CobolDialect}.
     */
    private String getDialectSequenceArea() {
        if (dialectSequenceArea == null) {
            dialectSequenceArea = StringUtils.repeat(" ", cobolDialect.getColumns().getContentArea() - 1);
        }
        return dialectSequenceArea;
    }
}
