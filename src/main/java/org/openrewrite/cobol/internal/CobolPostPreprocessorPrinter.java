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
    public static final String COPY_UUID = "UUID: ";
    private final CobolDialect cobolDialect;
    private final boolean printWithColumnAreas;

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
                // TODO: Clean up. This is a POC to test linking the processed COBOL to the CobolParserVisitor.

                // Print all comments that might exist before the word COPY.
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
                     *      |000001| |Some COBOL tokens|                               .|=> The index + 1 is the position of `|`.
                     *      |      |*|__COPY_START______________________________________|
                     *      |      |*|UUID: 263cd588-bdea-4c06-8ba1-177e515bded2        |=> UUID to the CopyStatement; a UUID will fit in the column area, but the copy statement might not.
                     *      |~~~~~~| |Print the COPIED source AST. ~~~~~~~~~~~~~~~~~~~~~|=> Print the COPIED AST, which includes new column areas.
                     *      |      |*|__COPY_END________________________________________|
                     *      |      | |[WS for tokens  ]|[WS for COPY        ]|=> White space is conditionally printer based on where the copy statement ends to ensure columns are aligned.
                     *
                     *  Alignment:
                     *      |      | | COPY STATEMENT.                                  |=> Requires whitespace to replace the statement for correct alignment.
                     *      |      | |                                   COPY STATEMENT.|=> The next line does not require any whitespace.
                     */

                    p.append(StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - 1 - curIndex));
                    p.append("\n");

                    StringBuilder templateStart = new StringBuilder();
                    String sequenceArea = StringUtils.repeat(" ", cobolDialect.getColumns().getContentArea() - 1);

                    String copyStartId = sequenceArea + "*" + COPY_START;
                    templateStart.append(copyStartId);
                    templateStart.append(StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - copyStartId.length()));
                    templateStart.append("\n");

                    String bookId = sequenceArea + "*UUID: " + copyStatement.getId();
                    templateStart.append(bookId);
                    templateStart.append(StringUtils.repeat(" ", cobolDialect.getColumns().getOtherArea() - bookId.length()));
                    templateStart.append("\n");

                    // Insert template identifier at the previous line.
                    p.append(templateStart.toString());

                    visit(copyStatement.getCopyBook(), p);

                    String copyEndId = sequenceArea + "*" + COPY_END;
                    String templateEnd = copyEndId +
                            StringUtils.repeat("_", cobolDialect.getColumns().getOtherArea() - copyEndId.length()) +
                            "\n";

                    p.append(templateEnd);

                    // Add whitespace until the next token will be aligned with the column area.
                    String copy = copyStatement.print(getCursor());
                    int whitespace = copy.endsWith("\n") ? 0 : copy.length() + curIndex + 1;
                    p.append(StringUtils.repeat(" ", whitespace));
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

    private int getIndex(String output) {
        int index = output.lastIndexOf("\n");
        if (index >= 0) {
            index = output.substring(index + 1).length() - 1;
        }
        return index;
    }
}
