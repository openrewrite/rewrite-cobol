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

import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.StringUtils;

import java.util.Optional;

/**
 * Print the processed COBOL code.
 *
 * `printWithColumnAreas`:
 *      true: Print as source code with modifications to distinguish changes during preprocessing like COPY and REPLACE.
 *      false: Print as parser input for the CobolParserVisitor.
 */
public class CobolPreprocessorOutputPrinter<P> extends CobolPreprocessorPrinter<P> {

    // START keys mark the line where whitespace is added until the end of the content area.
    public static final String COPY_START_KEY = "__COPY_START__";
    public static final String REPLACE_RULE_START_KEY = "__REPLACE_RULE_START__";
    public static final String REPLACE_OFF_START_KEY = "__REPLACE_OFF_START__";
    public static final String REPLACE_BY_START_KEY = "__REPLACE_BY_START__";

    // END keys mark when the exit of a template area.
    public static final String COPY_STOP_KEY = "__COPY_STOP__";
    public static final String REPLACE_RULE_STOP_KEY = "__REPLACE_RULE_STOP__";
    public static final String REPLACE_OFF_STOP_KEY = "__REPLACE_OFF_STOP__";
    public static final String REPLACE_BY_STOP_KEY = "__REPLACE_BY_END__";

    // Link the CobolPreprocessor AST UUID to the CobolParser.
    public static final String UUID_KEY = "__UUID__";

    private final CobolDialect cobolDialect;
    private final boolean printWithColumnAreas;

    // Lazily initialized Strings that are generated once with constraints based on the dialect.
    private String dialectSequenceArea = null;
    private String copyStartComment = null;
    private String copyStopComment = null;
    private String replaceRuleStartComment = null;
    private String replaceRuleStopComment = null;
    private String replaceOffStartComment = null;
    private String replaceOffStopComment = null;
    private String replaceByStartComment = null;
    private String replaceByStopComment = null;
    private String uuidComment = null;


    public CobolPreprocessorOutputPrinter(CobolDialect cobolDialect,
                                          boolean printWithColumnAreas) {
        this.cobolDialect = cobolDialect;
        this.printWithColumnAreas = printWithColumnAreas;
    }

    @Override
    public CobolPreprocessor visitCommentEntry(CobolPreprocessor.CommentEntry commentEntry, PrintOutputCapture<P> p) {
        if (printWithColumnAreas) {
            super.visitCommentEntry(commentEntry, p);
        } else {
            visitSpace(commentEntry.getPrefix(), p);
            visitMarkers(commentEntry.getMarkers(), p);
            for (CobolPreprocessor.Word comment : commentEntry.getComments()) {
                p.append("*>CE ");
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

        if (printWithColumnAreas) {
            if (copyStatement.getCopyBook() != null) {
                // Print markers like Lines, SequenceArea, and Indicator if the line starts with COPY.
                visit(copyStatement.getWord(), p);

                // Remove the prefix of and the word COPY, because the statement is replaced by the CopyBook.
                p.out.delete(p.getOut().length() - copyStatement.getWord().getWord().length() -
                        copyStatement.getWord().getPrefix().getWhitespace().length(), p.getOut().length());

                // Save the current index to ensure the text that follows the COPY will be aligned correctly.
                int curIndex = getCurrentIndex(p.getOut());
                if (curIndex != -1) {
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
                    int insertIndex = p.getOut().lastIndexOf("\n");
                    insertIndex = insertIndex == -1 ? 0 : insertIndex + 1;

                    p.out.insert(insertIndex, getCopyStartComment());
                    p.append(StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - curIndex));
                    p.append("\n");

                    p.append(getUuidKey());
                    String copyUuid = getDialectSequenceArea() + "*" + copyStatement.getId();
                    String copyUuidLine = copyUuid + StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - copyUuid.length()) + "\n";
                    p.append(copyUuidLine);

                    visit(copyStatement.getCopyBook(), p);

                    p.append(getCopyStopComment());

                    // Add whitespace until the next token will be aligned with the column area.
                    String copy = copyStatement.print(getCursor());
                    int numberOfSpaces = copy.endsWith("\n") ? 0 : copy.length() + curIndex;

                    String spacesCount = getDialectSequenceArea() + "*" + (numberOfSpaces == 0 ? 0 : (numberOfSpaces - cobolDialect.getColumns().getContentArea()));
                    String spacesCountLine = spacesCount + StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - spacesCount.length()) + "\n";
                    p.append(spacesCountLine);

                    p.append(StringUtils.repeat(" ", numberOfSpaces));
                }
            }
        } else {
            visit(copyStatement.getCopyBook(), p);
        }

        return copyStatement;
    }

    @Override
    public CobolPreprocessor visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, PrintOutputCapture<P> p) {
        // TODO: note ... assess: due to the way COBOL works, it looks like the same template pattern will apply to EVERY Preprocessor AST that needs to be linked to the CobolParser.
        if (printWithColumnAreas) {

            // Save the current index to ensure the text that follows the REPLACE will be aligned correctly.
            int curIndex = getCurrentIndex(p.getOut());
            if (curIndex != -1) {
                int insertIndex = p.getOut().lastIndexOf("\n");
                insertIndex = insertIndex == -1 ? 0 : insertIndex + 1;

                p.out.insert(insertIndex, getReplaceRuleStartComment());
                p.append(StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - curIndex));
                p.append("\n");

                p.append(getUuidKey());
                String copyUuid = getDialectSequenceArea() + "*" + replaceArea.getReplaceByStatement().getId();
                String copyUuidLine = copyUuid + StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - copyUuid.length()) + "\n";
                p.append(copyUuidLine);

                p.append(getReplaceRuleStopComment());

                // Add whitespace until the next token will be aligned with the column area.
                String statement = replaceArea.getReplaceByStatement().print(getCursor());
                int numberOfSpaces = statement.endsWith("\n") ? 0 : statement.length() + curIndex;

                String spacesCount = getDialectSequenceArea() + "*" + (numberOfSpaces == 0 ? 0 : (numberOfSpaces - cobolDialect.getColumns().getContentArea()));
                String spacesCountLine = spacesCount + StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - spacesCount.length()) + "\n";
                p.append(spacesCountLine);

                p.append(StringUtils.repeat(" ", numberOfSpaces));
            }
        }

        if (replaceArea.getCobols() != null) {
            for (CobolPreprocessor cobol : replaceArea.getCobols()) {
                visit(cobol, p);
            }
        }
        visit(replaceArea.getReplaceOffStatement(), p);
        return replaceArea;
    }

    @Override
    public CobolPreprocessor visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, PrintOutputCapture<P> p) {
        // TODO: note ... assess: due to the way COBOL works, it looks like the same template pattern will apply to EVERY Preprocessor AST that needs to be linked to the CobolParser.
        if (printWithColumnAreas) {

            // Save the current index to ensure the text that follows the REPLACE will be aligned correctly.
            int curIndex = getCurrentIndex(p.getOut());
            if (curIndex != -1) {
                int insertIndex = p.getOut().lastIndexOf("\n");
                insertIndex = insertIndex == -1 ? 0 : insertIndex + 1;

                p.out.insert(insertIndex, getReplaceOffStartComment());
                p.append(StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - curIndex));
                p.append("\n");

                p.append(getUuidKey());
                String copyUuid = getDialectSequenceArea() + "*" + replaceOffStatement.getId();
                String copyUuidLine = copyUuid + StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - copyUuid.length()) + "\n";
                p.append(copyUuidLine);

                p.append(getReplaceOffStopComment());

                // Add whitespace until the next token will be aligned with the column area.
                String statement = replaceOffStatement.print(getCursor());
                int numberOfSpaces = statement.endsWith("\n") ? 0 : statement.length() + curIndex;

                String spacesCount = getDialectSequenceArea() + "*" + (numberOfSpaces == 0 ? 0 : (numberOfSpaces - cobolDialect.getColumns().getContentArea()));
                String spacesCountLine = spacesCount + StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - spacesCount.length()) + "\n";
                p.append(spacesCountLine);

                p.append(StringUtils.repeat(" ", numberOfSpaces));
            }
        }
        return replaceOffStatement;
    }

    @Override
    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        if (printWithColumnAreas) {
            super.visitWord(word, p);
        } else {
            visitSpace(word.getPrefix(), p);
            visitMarkers(word.getMarkers(), p);
            p.append(word.getWord());

            Optional<CommentArea> commentArea = word.getMarkers().findFirst(CommentArea.class);
            commentArea.ifPresent(area -> visitSpace(area.getPrefix(), p));
            commentArea.ifPresent(area -> visitSpace(area.getEndOfLine(), p));
        }
        return word;
    }

    /**
     * TODO:
     */
    private String getCopyStartComment() {
        if (copyStartComment == null) {
            String start = getDialectSequenceArea() + "*" + COPY_START_KEY;
            copyStartComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return copyStartComment;
    }

    /**
     * TODO:
     */
    private String getCopyStopComment() {
        if (copyStopComment == null) {
            String start = getDialectSequenceArea() + "*" + COPY_STOP_KEY;
            copyStopComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return copyStopComment;
    }

    /**
     * TODO:
     */
    private String getReplaceRuleStartComment() {
        if (replaceRuleStartComment == null) {
            String start = getDialectSequenceArea() + "*" + REPLACE_RULE_START_KEY;
            replaceRuleStartComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return replaceRuleStartComment;
    }

    /**
     * TODO:
     */
    private String getReplaceRuleStopComment() {
        if (replaceRuleStopComment == null) {
            String start = getDialectSequenceArea() + "*" + REPLACE_RULE_STOP_KEY;
            replaceRuleStopComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return replaceRuleStopComment;
    }

    /**
     * TODO:
     */
    private String getReplaceByStartComment() {
        if (replaceByStartComment == null) {
            String start = getDialectSequenceArea() + "*" + REPLACE_BY_START_KEY;
            replaceByStartComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return replaceByStartComment;
    }

    /**
     * TODO:
     */
    private String getReplaceByStopComment() {
        if (replaceByStopComment == null) {
            String start = getDialectSequenceArea() + "*" + REPLACE_BY_STOP_KEY;
            replaceByStopComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return replaceByStopComment;
    }

    /**
     * TODO:
     */
    private String getReplaceOffStartComment() {
        if (replaceOffStartComment == null) {
            String start = getDialectSequenceArea() + "*" + REPLACE_OFF_START_KEY;
            replaceOffStartComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return replaceOffStartComment;
    }

    /**
     * TODO:
     */
    private String getReplaceOffStopComment() {
        if (replaceOffStopComment == null) {
            String start = getDialectSequenceArea() + "*" + REPLACE_OFF_STOP_KEY;
            replaceOffStopComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return replaceOffStopComment;
    }

    /**
     * TODO:
     */
    private String getUuidKey() {
        if (uuidComment == null) {
            String start = getDialectSequenceArea() + "*" + UUID_KEY;
            uuidComment = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return uuidComment;
    }

    /**
     * TODO:
     */
    private String getDialectSequenceArea() {
        if (dialectSequenceArea == null) {
            dialectSequenceArea = StringUtils.repeat(" ", cobolDialect.getColumns().getContentArea() - 1);
        }
        return dialectSequenceArea;
    }

    /**
     * TODO:
     */
    private int getCurrentIndex(String output) {
        int index = output.lastIndexOf("\n");
        if (index >= 0) {
            index = output.substring(index + 1).length();
        }
        return index;
    }
}
