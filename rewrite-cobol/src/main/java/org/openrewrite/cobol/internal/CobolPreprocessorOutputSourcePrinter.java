/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.internal;

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.StringUtils;
import org.openrewrite.internal.lang.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.openrewrite.cobol.CobolPrinterUtils.*;
import static org.openrewrite.cobol.CobolStringUtils.isSubstituteCharacter;
import static org.openrewrite.cobol.internal.CobolGrammarToken.COMMENT_ENTRY;

/**
 * Print the post processed COBOL AST with comments that act like a `JavaTemplate`.
 * The comments are used to link AST elements that contain the original source code to the COBOL AST.
 * <p>
 * Each key will be added as a comment that is formatted based on the current {@link CobolDialect}.
 * <p>
 * `printWithColumnAreas`:
 *      true: Print as source code with modifications to distinguish changes during preprocessing like COPY and REPLACE.
 *      false: Print as parser input for the CobolParserVisitor.
 */
public class CobolPreprocessorOutputSourcePrinter<P> extends CobolPreprocessorSourcePrinter<P> {
    public static final String COPY_START_KEY = "__COPY_START__";
    public static final String COPY_STOP_KEY = "__COPY_STOP__";
    public static final String COPY_UUID_KEY = "__COPY_UUID__";

    public static final String REPLACE_START_KEY = "__REPLACE_START__";
    public static final String REPLACE_STOP_KEY = "__REPLACE_STOP__";
    public static final String REPLACE_UUID_KEY = "__REPLACE_UUID__";

    public static final String UUID_KEY = "__UUID__";

    public static final String REPLACE_BY_START_KEY = "__REPLACE_BY_START__";
    public static final String REPLACE_BY_STOP_KEY = "__REPLACE_BY_STOP__";

    public static final String REPLACE_OFF_START_KEY = "__REPLACE_OFF_START__";
    public static final String REPLACE_OFF_STOP_KEY = "__REPLACE_OFF_STOP__";

    public static final String REPLACE_TYPE_ADDITIVE_START_KEY = "__REPLACE_TYPE_ADDITIVE_START__";
    public static final String REPLACE_TYPE_ADDITIVE_STOP_KEY = "__REPLACE_TYPE_ADDITIVE_STOP__";
    public static final String REPLACE_ADD_WORD_START_KEY = "__REPLACE_ADD_WORD_START__";
    public static final String REPLACE_ADD_WORD_STOP_KEY = "__REPLACE_ADD_WORD_STOP__";

    public static final String REPLACE_TYPE_REDUCTIVE_START_KEY = "__REPLACE_TYPE_REDUCTIVE_START__";
    public static final String REPLACE_TYPE_REDUCTIVE_STOP_KEY = "__REPLACE_TYPE_REDUCTIVE_STOP__";

    public static final String REPLACE_ADDED_WHITESPACE_KEY = "__REPLACE_ADDED_WHITESPACE__";

    private final CobolSourcePrinter<P> cobolSourcePrinter;
    private final CobolDialect cobolDialect;
    private final boolean printColumns;

    // Lazily initialized Strings that are generated once with constraints based on the dialect.
    private String dialectSequenceArea = null;
    private String uuidEndOfLine = null;

    private String copyStartComment = null;
    private String copyStopComment = null;
    private String copyUuidComment = null;

    private String replaceStartComment = null;
    private String replaceStopComment = null;
    private String replaceUuidComment = null;

    private String uuidComment = null;

    private String replaceByStartComment = null;
    private String replaceByStopComment = null;

    private String replaceOffStartComment = null;
    private String replaceOffStopComment = null;

    private String replaceTypeAdditiveStartComment = null;
    private String replaceTypeAdditiveStopComment = null;
    private String replaceAddWordStartComment = null;
    private String replaceAddWordStopComment = null;

    private String replaceTypeReductiveStartComment = null;
    private String replaceTypeReductiveStopComment = null;
    private Replacement replaceReductiveType = null;

    private String replaceAddedWhitespaceComment = null;

    private final CobolPreprocessorSourcePrinter<ExecutionContext> statementPrinter = new CobolPreprocessorSourcePrinter<>(false);
    private final CobolSourcePrinter<ExecutionContext> cobolStatementPrinter = new CobolSourcePrinter<>(false);

    private boolean inUnknownIndicator = false;

    public CobolPreprocessorOutputSourcePrinter(CobolDialect cobolDialect,
                                                boolean printColumns) {
        super(true);
        this.cobolSourcePrinter = new CobolSourcePrinter<>(printColumns);
        this.cobolDialect = cobolDialect;
        this.printColumns = printColumns;
    }

    @Override
    public CobolPreprocessor visitCommentEntry(CobolPreprocessor.CommentEntry commentEntry, PrintOutputCapture<P> p) {
        if (printColumns) {
            super.visitCommentEntry(commentEntry, p);
        } else {
            visitSpace(commentEntry.getPrefix(), Space.Location.COMMENT_ENTRY_PREFIX, p);
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
        visitSpace(copyBook.getPrefix(), Space.Location.COPY_BOOK_PREFIX, p);
        visitMarkers(copyBook.getMarkers(), p);
        visit(copyBook.getAst(), p);
        visit(copyBook.getEof(), p);
        return copyBook;
    }

    @Override
    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        visitSpace(copyStatement.getPrefix(), Space.Location.COPY_STATEMENT_PREFIX, p);
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
        p.out.delete(p.getOut().length() - copyStatement.getWord().getCobolWord().getWord().length() -
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
            if (word.getCobolWord().getIndicatorArea() != null) {
                String indicator = word.getCobolWord().getIndicatorArea().getIndicator();
                if ("G".equals(indicator) || "J".equals(indicator) || "P".equals(indicator)) {
                    // TODO: add a form of visibility for an unrecognized indicator.
                    inUnknownIndicator = true;
                }
            }

            if (!inUnknownIndicator) {
                if (word.getCobolWord().getReplacement() != null && word.getCobolWord().getReplacement().getType() == Replacement.Type.ADDITIVE) {
                    Replacement replaceAdditiveType = word.getCobolWord().getReplacement();
                    for (Replacement.OriginalWord additionalWord : replaceAdditiveType.getOriginalWords()) {
                        cobolSourcePrinter.visit(additionalWord.getOriginal(), p);
                    }
                    int curIndex = getCurrentIndex(p.getOut());
                    int contentEnd = cobolDialect.getColumns().getOtherArea();
                    int untilEndOfLine = curIndex >= contentEnd ? 0 : cobolDialect.getColumns().getOtherArea() - curIndex;
                    String whitespace = generateWhitespace(untilEndOfLine) + "\n";
                    p.append(whitespace);
                }

                visitSpace(word.getPrefix(), Space.Location.WORD_PREFIX, p);
                visitMarkers(word.getMarkers(), p);
                p.append(word.getCobolWord().getWord());

                if (word.getCobolWord().getCommentArea() != null) {
                    visitSpace(word.getCobolWord().getCommentArea().getPrefix(), Space.Location.COMMENT_AREA_PREFIX, p);
                    visitSpace(word.getCobolWord().getCommentArea().getEndOfLine(), Space.Location.COMMENT_AREA_EOL, p);
                }
            }

            if (inUnknownIndicator && word.getCobolWord().getCommentArea() != null) {
                inUnknownIndicator = false;
            }
            return word;
        }

        // Beware all who enter.
        // Replace contains many special cases due to changes in column alignment after a replacement.
        Replacement replacement = word.getCobolWord().getReplacement();
        if (replacement != null && replacement.getType() == Replacement.Type.EQUAL) {
            List<CobolLine> lines = replacement.getOriginalWords().get(0).getOriginal().getLines();
            if (lines != null) {
                ListUtils.map(lines, l -> cobolSourcePrinter.visitLine(l, p));
            }

            replaceTemplate(word, p, replacement);
        } else if (replacement != null && replacement.getType() == Replacement.Type.REDUCTIVE) {
            if (replaceReductiveType == null) {
                replaceReductiveType = replacement;
            }
        } else if (replaceReductiveType != null) {
            replaceReductiveTemplate(p);

            super.visitWord(word, p);
            replaceReductiveType = null;
        } else if (replacement != null && replacement.getType() == Replacement.Type.ADDITIVE) {
            replaceAdditiveTemplate(replacement, p);
            super.visitWord(word, p);
        } else if (!isSubstituteCharacter(word.getCobolWord().getWord())) {
            super.visitWord(word, p);
        }
        return word;
    }

    private void replaceAdditiveTemplate(Replacement replaceAdditiveType, PrintOutputCapture<P> p) {
        // Fill the remaining line with whitespace to align the column areas.
        int curIndex = getCurrentIndex(p.getOut());
        int contentEnd = cobolDialect.getColumns().getOtherArea();
        int untilEndOfLine = curIndex >= contentEnd ? 0 : cobolDialect.getColumns().getOtherArea() - curIndex;
        String whitespace = generateWhitespace(untilEndOfLine) + "\n";
        p.append(whitespace);

        int startKeyIndex = getCurrentIndex(p.getOut());
        addStartKey(getReplaceTypeAdditiveStartComment(), startKeyIndex, p);
        addUuidKey(getUuidComment(), replaceAdditiveType.getId(), p);
        for (Replacement.OriginalWord additionalWord : replaceAdditiveType.getOriginalWords()) {
            p.append(getReplaceAddWordStartComment());
            String addedWord = getDialectSequenceArea() + " " + additionalWord.getOriginal().getPrefix().getWhitespace() + additionalWord.getOriginal().getWord();
            p.append(addedWord);
            int addedIndex = getCurrentIndex(p.getOut());
            untilEndOfLine = addedIndex >= contentEnd ? 0 : cobolDialect.getColumns().getOtherArea() - addedIndex;
            String addWhitespace = generateWhitespace(untilEndOfLine) + "\n";
            p.append(addWhitespace);
            p.append(getReplaceAddWordStopComment());
        }
        addStopComment(getReplaceTypeAdditiveStopComment(), null, curIndex, p);
    }

    private void replaceReductiveTemplate(PrintOutputCapture<P> p) {
        // Print the markers from the original words and replace the original words with whitespace.
        for (Replacement.OriginalWord replace : replaceReductiveType.getOriginalWords()) {
            Cobol.Word originalWord = replace.getOriginal();
            if (originalWord.getContinuation() != null) {
                throw new UnsupportedOperationException("Implement continuation lines for a reductive replacement.");
            }

            if (originalWord.getSequenceArea() != null) {
                originalWord.getSequenceArea().printColumnArea(this, getCursor(), printColumns, p);
            }

            if (originalWord.getIndicatorArea() != null) {
                originalWord.getIndicatorArea().printColumnArea(this, getCursor(), printColumns, p);
            }

            visitSpace(originalWord.getPrefix(), Space.Location.WORD_PREFIX, p);

            String replaceWithWhitespace = generateWhitespace(originalWord.getWord().length());
            p.append(replaceWithWhitespace);

            if (originalWord.getCommentArea() != null) {
                originalWord.getCommentArea().printColumnArea(this, getCursor(), printColumns, p);
            }
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

    private void replaceTemplate(CobolPreprocessor.Word word, PrintOutputCapture<P> p, Replacement replacement) {
        // Save the current index to ensure the text that follows the REPLACE will be aligned correctly.
        int curIndex = getCurrentIndex(p.getOut());
        if (curIndex == -1) {
            throw new UnsupportedOperationException("Unknown case: Detected a Replace at the start of the source code.");
        }

        Cobol.Word originalWord = replacement.getOriginalWords().get(0).getOriginal();
        boolean isLongerWord = word.getCobolWord().getWord().length() > originalWord.getWord().length();
        String replacedWord = isLongerWord ? " " + word.getCobolWord().getWord() : word.getCobolWord().getWord();

        boolean isLiteral = word.getCobolWord().getWord().startsWith("\"") || word.getCobolWord().getWord().startsWith("'");
        int contentAreaLength = getContentAreaLength(cobolDialect);
        boolean isContinuedLiteral = isLiteral && (curIndex + replacedWord.length()) > contentAreaLength;

        // Add Start key.
        int insertIndex = getInsertIndex(p.getOut());
        p.out.insert(insertIndex, getReplaceStartComment());

        if (isLongerWord && !isContinuedLiteral) {
            insertIndex = getInsertIndex(p.getOut());
            p.out.insert(insertIndex, getReplaceAddedWhitespaceComment());
        }

        if (curIndex == 0) {
            if (originalWord.getSequenceArea() != null) {
                originalWord.getSequenceArea().printColumnArea(this, getCursor(), printColumns, p);
            }
            if (originalWord.getIndicatorArea() != null) {
                originalWord.getIndicatorArea().printColumnArea(this, getCursor(), printColumns, p);
            }
        }

        // Fill in the rest of the content area with whitespace.
        int untilEndOfLine =  cobolDialect.getColumns().getOtherArea() - (curIndex == 0 ? cobolDialect.getColumns().getContentArea() : curIndex);
        String whitespace = generateWhitespace(untilEndOfLine) + "\n";
        p.append(whitespace);

        // Add UUID key.
        addUuidKey(getReplaceUuidComment(), replacement.getId(), p);

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
                int originalLength = originalWord.getPrefix().getWhitespace().length() + originalWord.getWord().length();
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
                int difference = word.getCobolWord().getWord().length() - originalWord.getWord().length();
                int alignColumn = curIndex == 0 ? cobolDialect.getColumns().getContentArea() : curIndex;
                int total = alignColumn + word.getPrefix().getWhitespace().length() - difference;
                if (total > cobolDialect.getColumns().getOtherArea()) {
                    throw new UnsupportedOperationException("The position of the replaced word exceeds the column area.");
                }
                String prefix = generateWhitespace(total);
                p.append(prefix);
                p.append(word.getCobolWord().getWord());
            }
        } else {
            PrintOutputCapture<ExecutionContext> outputCapture = new PrintOutputCapture<>(new InMemoryExecutionContext());
            cobolStatementPrinter.visit(originalWord, outputCapture);

            String statement = outputCapture.getOut();
            boolean isEndOfLine = statement.endsWith("\n");
            boolean isCRLF = statement.endsWith("\r\n");

            int totalChars = statement.length() + curIndex - cobolDialect.getColumns().getContentArea() - (isEndOfLine ? (isCRLF ? 2 : 1) : 0);

            int numberOfSpaces;
            if (!isEndOfLine && totalChars > contentAreaLength) {
                String replaced = word.print(getCursor());
                numberOfSpaces = getCurrentIndex(replaced);
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
            int difference = originalWord.getWord().length() - word.getCobolWord().getWord().length();
            // The difference exceeds the content area.
            if (curIndex + difference > cobolDialect.getColumns().getOtherArea()) {
                String fullWord = word.print(getCursor());
                int lastIndex = fullWord.lastIndexOf("\n");
                fullWord = fullWord.substring(lastIndex + 1);

                if (fullWord.length() - word.getCobolWord().getWord().length() < curIndex) {
                    p.out.delete(p.getOut().length() - (curIndex - (fullWord.length() - word.getCobolWord().getWord().length())), p.getOut().length());
                } else {
                    p.append(StringUtils.repeat(" ", fullWord.length() - word.getCobolWord().getWord().length() - curIndex));
                }

                p.append(word.getCobolWord().getWord());
            } else {
                String additionalPrefix = StringUtils.repeat(" ", difference);
                p.append(additionalPrefix);
                visitSpace(word.getPrefix(), Space.Location.WORD_PREFIX, p);
                p.append(word.getCobolWord().getWord());
            }
        }

        if (word.getCobolWord().getCommentArea() != null) {
            word.getCobolWord().getCommentArea().printColumnArea(this, getCursor(), printColumns, p);
        }
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
    private void addStopComment(String stopComment, @Nullable CobolPreprocessor statement, int curIndex, PrintOutputCapture<P> p) {
        p.append(stopComment);

        PrintOutputCapture<ExecutionContext> outputCapture = new PrintOutputCapture<>(new InMemoryExecutionContext());
        statementPrinter.visit(statement, outputCapture);

        String output = outputCapture.getOut();
        boolean isEndOfLine = output.endsWith("\n");
        boolean isCRLF = output.endsWith("\r\n");

        int totalChars = output.length() + curIndex - cobolDialect.getColumns().getContentArea() - (isEndOfLine ? (isCRLF ? 2 : 1) : 0);
        int contentAreaLength = getContentAreaLength(cobolDialect);

        int numberOfSpaces;
        if (!isEndOfLine && totalChars > contentAreaLength) {
            throw new UnsupportedOperationException("Recalculate prefix.");
        } else {
            numberOfSpaces = isEndOfLine ? 0 : output.length() + curIndex;
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

    public String getCopyStartComment() {
        if (copyStartComment == null) {
            copyStartComment = getTemplateComment(COPY_START_KEY);
        }
        return copyStartComment;
    }

    public String getCopyStopComment() {
        if (copyStopComment == null) {
            copyStopComment = getTemplateComment(COPY_STOP_KEY);
        }
        return copyStopComment;
    }

    public String getCopyUuidKey() {
        if (copyUuidComment == null) {
            copyUuidComment = getTemplateComment(COPY_UUID_KEY);
        }
        return copyUuidComment;
    }

    public String getReplaceStartComment() {
        if (replaceStartComment == null) {
            replaceStartComment = getTemplateComment(REPLACE_START_KEY);
        }
        return replaceStartComment;
    }

    public String getReplaceStopComment() {
        if (replaceStopComment == null) {
            replaceStopComment = getTemplateComment(REPLACE_STOP_KEY);
        }
        return replaceStopComment;
    }

    public String getReplaceUuidComment() {
        if (replaceUuidComment == null) {
            replaceUuidComment = getTemplateComment(REPLACE_UUID_KEY);
        }
        return replaceUuidComment;
    }

    public String getReplaceByStartComment() {
        if (replaceByStartComment == null) {
            replaceByStartComment = getTemplateComment(REPLACE_BY_START_KEY);
        }
        return replaceByStartComment;
    }

    public String getReplaceByStopComment() {
        if (replaceByStopComment == null) {
            replaceByStopComment = getTemplateComment(REPLACE_BY_STOP_KEY);
        }
        return replaceByStopComment;
    }

    public String getReplaceAddedWhitespaceComment() {
        if (replaceAddedWhitespaceComment == null) {
            replaceAddedWhitespaceComment = getTemplateComment(REPLACE_ADDED_WHITESPACE_KEY);
        }
        return replaceAddedWhitespaceComment;
    }

    public String getReplaceOffStartComment() {
        if (replaceOffStartComment == null) {
            replaceOffStartComment = getTemplateComment(REPLACE_OFF_START_KEY);
        }
        return replaceOffStartComment;
    }

    public String getReplaceOffStopComment() {
        if (replaceOffStopComment == null) {
            replaceOffStopComment = getTemplateComment(REPLACE_OFF_STOP_KEY);
        }
        return replaceOffStopComment;
    }

    public String getReplaceAddWordStartComment() {
        if (replaceAddWordStartComment == null) {
            replaceAddWordStartComment = getTemplateComment(REPLACE_ADD_WORD_START_KEY);
        }
        return replaceAddWordStartComment;
    }

    public String getReplaceAddWordStopComment() {
        if (replaceAddWordStopComment == null) {
            replaceAddWordStopComment = getTemplateComment(REPLACE_ADD_WORD_STOP_KEY);
        }
        return replaceAddWordStopComment;
    }

    public String getReplaceTypeAdditiveStartComment() {
        if (replaceTypeAdditiveStartComment == null) {
            replaceTypeAdditiveStartComment = getTemplateComment(REPLACE_TYPE_ADDITIVE_START_KEY);
        }
        return replaceTypeAdditiveStartComment;
    }

    public String getReplaceTypeAdditiveStopComment() {
        if (replaceTypeAdditiveStopComment == null) {
            replaceTypeAdditiveStopComment = getTemplateComment(REPLACE_TYPE_ADDITIVE_STOP_KEY);
        }
        return replaceTypeAdditiveStopComment;
    }

    public String getReplaceTypeReductiveStartComment() {
        if (replaceTypeReductiveStartComment == null) {
            replaceTypeReductiveStartComment = getTemplateComment(REPLACE_TYPE_REDUCTIVE_START_KEY);
        }
        return replaceTypeReductiveStartComment;
    }

    public String getReplaceTypeReductiveStopComment() {
        if (replaceTypeReductiveStopComment == null) {
            replaceTypeReductiveStopComment = getTemplateComment(REPLACE_TYPE_REDUCTIVE_STOP_KEY);
        }
        return replaceTypeReductiveStopComment;
    }

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

    private String getDialectSequenceArea() {
        if (dialectSequenceArea == null) {
            dialectSequenceArea = StringUtils.repeat(" ", cobolDialect.getColumns().getContentArea() - 1);
        }
        return dialectSequenceArea;
    }
}
