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

/**
 * Print the original preprocessed COBOL.
 * Note: All the logic to print column areas exists in visitWord.
 */
public class CobolPreprocessorPrinter<P> extends CobolPreprocessorSourcePrinter<P> {

    private final boolean printOriginalSource;

    public CobolPreprocessorPrinter(boolean printOriginalSource,
                                    boolean printColumns) {
        super(printColumns);
        this.printOriginalSource = printOriginalSource;
    }

    @Override
    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
        beforeSyntax(copyBook, Space.Location.COPY_BOOK_PREFIX, p);
        visit(copyBook.getAst(), p);
        afterSyntax(copyBook, p);
        return copyBook;
    }

    @Override
    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitCopyStatement(copyStatement, p);
        }
        if (copyStatement.getCopyBook() != null) {
            beforeSyntax(copyStatement, Space.Location.COPY_STATEMENT_PREFIX, p);
            visit(copyStatement.getCopyBook(), p);
            if (!p.getOut().endsWith("\n")) {
                p.append("\n");
            }
            afterSyntax(copyStatement, p);
        }
        return copyStatement;
    }

    @Override
    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        if (printOriginalSource) {
            return super.visitWord(word, p);
        }

        cobolVisitor.visitWord(word.getCobolWord(), p);

        afterSyntax(word, p);
        return word;
    }
}
