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
 * TODO: explain.
 */
public class CobolPostPreprocessorPrinter<P> extends CobolPreprocessorPrinter<P> {

    public static final String COPY_START = "__COPY_START__";
    public static final String COPY_END = "__COPY_END__";
    public static final String COPY_UUID = "__UUID__";
    private final CobolDialect cobolDialect;
    private final boolean printWithColumnAreas;
    private String sequenceArea = null;
    private String copyStartLine = null;
    private String copyEndLine = null;
    private String uuidLine = null;


    public CobolPostPreprocessorPrinter(CobolDialect cobolDialect,
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
                int curIndex = getIndex(p.getOut());
                if (curIndex != -1) {
                    // Printing the COPY statement will add comments that work similar to JavaTemplate.
                    // Comments are added before and after the template to provide context about which AST elements
                    // are a product of a COPY statement.

                    // TODO: FIX: Comments from the end of the COPY SOURCE should not be associated to the next AST element. OR distinguish a trailing comment in the Marker.
                    // TODO: FIX: refactor how CommentArea is created to associate whitespace at the end of a line, and CommentArea to the next AST element.

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

                    p.out.insert(insertIndex, getCopyStartLine());
                    p.append(StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - curIndex));
                    p.append("\n");

                    p.append(getUuidLine());
                    String copyUuid = getSequenceArea() + "*" + copyStatement.getId();
                    String copyUuidLine = copyUuid + StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - copyUuid.length()) + "\n";
                    p.append(copyUuidLine);

                    visit(copyStatement.getCopyBook(), p);

                    p.append(getCopyEndLine());

                    // Add whitespace until the next token will be aligned with the column area.
                    String copy = copyStatement.print(getCursor());
                    int numberOfSpaces = copy.endsWith("\n") ? 0 : copy.length() + curIndex;

                    String spacesCount = getSequenceArea() + "*" + (numberOfSpaces == 0 ? 0 : (numberOfSpaces - cobolDialect.getColumns().getContentArea()));
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
        if (replaceArea.getCobols() != null) {
            for (CobolPreprocessor cobol : replaceArea.getCobols()) {
                visit(cobol, p);
            }
        }
        return replaceArea;
    }

    @Override
    public CobolPreprocessor visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, PrintOutputCapture<P> p) {
        // Do not print.
        return replaceOffStatement;
    }

    @Override
    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        if (printWithColumnAreas) {
            super.visitWord(word, p);
        } else {
            // print applicable empty lines.
            visitSpace(word.getPrefix(), p);
            visitMarkers(word.getMarkers(), p);
            p.append(word.getWord());
            Optional<CommentArea> commentArea = word.getMarkers().findFirst(CommentArea.class);
            commentArea.ifPresent(area -> visitSpace(area.getPrefix(), p));
            commentArea.ifPresent(area -> visitSpace(area.getEndOfLine(), p));
        }
        return word;
    }

    private String getCopyStartLine() {
        if (copyStartLine == null) {
            String start = getSequenceArea() + "*" + COPY_START;
            copyStartLine = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return copyStartLine;
    }

    private String getCopyEndLine() {
        if (copyEndLine == null) {
            String start = getSequenceArea() + "*" + COPY_END;
            copyEndLine = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return copyEndLine;
    }

    private String getUuidLine() {
        if (uuidLine == null) {
            String start = getSequenceArea() + "*" + COPY_UUID;
            uuidLine = start + StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - start.length()) + "\n";
        }
        return uuidLine;
    }

    private String getSequenceArea() {
        if (sequenceArea == null) {
            sequenceArea = StringUtils.repeat(" ", cobolDialect.getColumns().getContentArea() - 1);
        }
        return sequenceArea;
    }

    private int getIndex(String output) {
        int index = output.lastIndexOf("\n");
        if (index >= 0) {
            index = output.substring(index + 1).length();
        }
        return index;
    }
}
