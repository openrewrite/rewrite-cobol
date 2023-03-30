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

import org.openrewrite.Cursor;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.CobolPreprocessorVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.StringUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.*;
import java.util.function.UnaryOperator;

/**
 * Print the original preprocessed COBOL.
 * Note: All the logic to print column areas exists in visitWord.
 */
public class CobolPreprocessorSourcePrinter<P> extends CobolPreprocessorVisitor<PrintOutputCapture<P>> {

    private final CobolSourcePrinter<P> cobolSourcePrinter;
    private final boolean printColumns;
    private int originalReplaceLength;

    private final Collection<String> printedCopyStatements;
    private final Collection<String> printedReductiveReplaces;

    public CobolPreprocessorSourcePrinter(boolean printColumns) {
        this.cobolSourcePrinter = new CobolSourcePrinter<>(printColumns);
        this.printColumns = printColumns;
        this.printedCopyStatements = new HashSet<>();
        this.printedReductiveReplaces = new HashSet<>();
    }

    @Override
    public CobolPreprocessor visitCharData(CobolPreprocessor.CharData charData, PrintOutputCapture<P> p) {
        beforeSyntax(charData, Space.Location.CHAR_DATA_PREFIX, p);
        visit(charData.getCobols(), p);
        afterSyntax(charData, p);
        return charData;
    }

    @Override
    public CobolPreprocessor visitCharDataLine(CobolPreprocessor.CharDataLine charDataLine, PrintOutputCapture<P> p) {
        beforeSyntax(charDataLine, Space.Location.CHAR_DATA_LINE_PREFIX, p);
        visit(charDataLine.getWords(), p);
        afterSyntax(charDataLine, p);
        return charDataLine;
    }

    @Override
    public CobolPreprocessor visitCharDataSql(CobolPreprocessor.CharDataSql charDataSql, PrintOutputCapture<P> p) {
        beforeSyntax(charDataSql, Space.Location.CHAR_DATA_SQL_PREFIX, p);
        visit(charDataSql.getCobols(), p);
        afterSyntax(charDataSql, p);
        return charDataSql;
    }

    @Override
    public CobolPreprocessor visitCommentEntry(CobolPreprocessor.CommentEntry commentEntry, PrintOutputCapture<P> p) {
        beforeSyntax(commentEntry, Space.Location.COMMENT_ENTRY_PREFIX, p);
        visit(commentEntry.getComments(), p);
        afterSyntax(commentEntry, p);
        return commentEntry;
    }

    @Override
    public CobolPreprocessor visitCompilationUnit(CobolPreprocessor.CompilationUnit compilationUnit, PrintOutputCapture<P> p) {
        beforeSyntax(compilationUnit, Space.Location.PREPROCESSOR_COMPILATION_UNIT_PREFIX, p);
        visit(compilationUnit.getCobols(), p);
        visit(compilationUnit.getEof(), p);
        afterSyntax(compilationUnit, p);
        return compilationUnit;
    }

    @Override
    public CobolPreprocessor visitCompilerOption(CobolPreprocessor.CompilerOption compilerOption, PrintOutputCapture<P> p) {
        beforeSyntax(compilerOption, Space.Location.COMPILER_OPTION_PREFIX, p);
        visit(compilerOption.getCobols(), p);
        afterSyntax(compilerOption, p);
        return compilerOption;
    }

    @Override
    public CobolPreprocessor visitCompilerOptions(CobolPreprocessor.CompilerOptions compilerOptions, PrintOutputCapture<P> p) {
        beforeSyntax(compilerOptions, Space.Location.COMPILER_OPTIONS_PREFIX, p);
        visit(compilerOptions.getWord(), p);
        visit(compilerOptions.getCobols(), p);
        afterSyntax(compilerOptions, p);
        return compilerOptions;
    }

    @Override
    public CobolPreprocessor visitCompilerXOpts(CobolPreprocessor.CompilerXOpts compilerXOpts, PrintOutputCapture<P> p) {
        beforeSyntax(compilerXOpts, Space.Location.COMPILER_XOPTS_PREFIX, p);
        visit(compilerXOpts.getWord(), p);
        visit(compilerXOpts.getLeftParen(), p);
        visit(compilerXOpts.getCompilerOptions(), p);
        visit(compilerXOpts.getRightParen(), p);
        afterSyntax(compilerXOpts, p);
        return compilerXOpts;
    }

    @Override
    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
        // The CopyBook is not printed as a part of the original source.
        return copyBook;
    }

    @Override
    public CobolPreprocessor visitCopySource(CobolPreprocessor.CopySource copySource, PrintOutputCapture<P> p) {
        beforeSyntax(copySource, Space.Location.COPY_SOURCE_PREFIX, p);
        visit(copySource.getName(), p);
        visit(copySource.getWord(), p);
        visit(copySource.getCopyLibrary(), p);
        afterSyntax(copySource, p);
        return copySource;
    }

    @Override
    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        beforeSyntax(copyStatement, Space.Location.COPY_STATEMENT_PREFIX, p);
        visit(copyStatement.getWord(), p);
        visit(copyStatement.getCopySource(), p);
        visit(copyStatement.getCobols(), p);
        visit(copyStatement.getDot(), p);
        afterSyntax(copyStatement, p);
        return copyStatement;
    }

    @Override
    public CobolPreprocessor visitDirectoryPhrase(CobolPreprocessor.DirectoryPhrase directoryPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(directoryPhrase, Space.Location.DIRECTORY_PHRASE_PREFIX, p);
        visit(directoryPhrase.getWord(), p);
        visit(directoryPhrase.getName(), p);
        afterSyntax(directoryPhrase, p);
        return directoryPhrase;
    }

    @Override
    public CobolPreprocessor visitEjectStatement(CobolPreprocessor.EjectStatement ejectStatement, PrintOutputCapture<P> p) {
        beforeSyntax(ejectStatement, Space.Location.EJECT_STATEMENT_PREFIX, p);
        visit(ejectStatement.getWord(), p);
        visit(ejectStatement.getDot(), p);
        afterSyntax(ejectStatement, p);
        return ejectStatement;
    }

    @Override
    public CobolPreprocessor visitExecStatement(CobolPreprocessor.ExecStatement execStatement, PrintOutputCapture<P> p) {
        beforeSyntax(execStatement, Space.Location.EXEC_STATEMENT_PREFIX, p);
        visit(execStatement.getWords(), p);
        visit(execStatement.getCobol(), p);
        visit(execStatement.getEndExec(), p);
        visit(execStatement.getDot(), p);
        afterSyntax(execStatement, p);
        return execStatement;
    }

    @Override
    public CobolPreprocessor visitFamilyPhrase(CobolPreprocessor.FamilyPhrase familyPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(familyPhrase, Space.Location.FAMILY_PHRASE_PREFIX, p);
        visit(familyPhrase.getWord(), p);
        visit(familyPhrase.getName(), p);
        afterSyntax(familyPhrase, p);
        return familyPhrase;
    }

    @Override
    public CobolPreprocessor visitPseudoText(CobolPreprocessor.PseudoText pseudoText, PrintOutputCapture<P> p) {
        beforeSyntax(pseudoText, Space.Location.PSEUDO_TEXT_PREFIX, p);
        visit(pseudoText.getDoubleEqualOpen(), p);
        visit(pseudoText.getCharData(), p);
        visit(pseudoText.getDoubleEqualClose(), p);
        afterSyntax(pseudoText, p);
        return pseudoText;
    }

    @Override
    public CobolPreprocessor visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, PrintOutputCapture<P> p) {
        beforeSyntax(replaceArea, Space.Location.REPLACE_AREA_PREFIX, p);
        visit(replaceArea.getReplaceByStatement(), p);
        visit(replaceArea.getCobols(), p);
        visit(replaceArea.getReplaceOffStatement(), p);
        afterSyntax(replaceArea, p);
        return replaceArea;
    }

    @Override
    public CobolPreprocessor visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement, PrintOutputCapture<P> p) {
        beforeSyntax(replaceByStatement, Space.Location.REPLACE_BY_STATEMENT_PREFIX, p);
        visit(replaceByStatement.getWord(), p);
        visit(replaceByStatement.getClauses(), p);
        visit(replaceByStatement.getDot(), p);
        afterSyntax(replaceByStatement, p);
        return replaceByStatement;
    }

    @Override
    public CobolPreprocessor visitReplaceClause(CobolPreprocessor.ReplaceClause replaceClause, PrintOutputCapture<P> p) {
        beforeSyntax(replaceClause, Space.Location.REPLACE_CLAUSE_PREFIX, p);
        visit(replaceClause.getReplaceable(), p);
        visit(replaceClause.getBy(), p);
        visit(replaceClause.getReplacement(), p);
        visit(replaceClause.getSubscript(), p);
        visit(replaceClause.getDirectoryPhrases(), p);
        visit(replaceClause.getFamilyPhrase(), p);
        afterSyntax(replaceClause, p);
        return replaceClause;
    }

    @Override
    public CobolPreprocessor visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, PrintOutputCapture<P> p) {
        beforeSyntax(replaceOffStatement, Space.Location.REPLACE_OFF_STATEMENT_PREFIX, p);
        visit(replaceOffStatement.getWords(), p);
        visit(replaceOffStatement.getDot(), p);
        afterSyntax(replaceOffStatement, p);
        return replaceOffStatement;
    }

    @Override
    public CobolPreprocessor visitReplacingPhrase(CobolPreprocessor.ReplacingPhrase replacingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(replacingPhrase, Space.Location.REPLACING_PHRASE_PREFIX, p);
        visit(replacingPhrase.getWord(), p);
        visit(replacingPhrase.getClauses(), p);
        afterSyntax(replacingPhrase, p);
        return replacingPhrase;
    }

    @Override
    public CobolPreprocessor visitSkipStatement(CobolPreprocessor.SkipStatement skipStatement, PrintOutputCapture<P> p) {
        beforeSyntax(skipStatement, Space.Location.SKIP_STATEMENT_PREFIX, p);
        visit(skipStatement.getWord(), p);
        visit(skipStatement.getDot(), p);
        afterSyntax(skipStatement, p);
        return skipStatement;
    }

    @Override
    public Space visitSpace(Space space, Space.Location location, PrintOutputCapture<P> p) {
        p.append(space.getWhitespace());
        return space;
    }

    @Override
    public CobolPreprocessor visitTitleStatement(CobolPreprocessor.TitleStatement titleStatement, PrintOutputCapture<P> p) {
        beforeSyntax(titleStatement, Space.Location.TITLE_STATEMENT_PREFIX, p);
        visit(titleStatement.getFirst(), p);
        visit(titleStatement.getSecond(), p);
        visit(titleStatement.getDot(), p);
        afterSyntax(titleStatement, p);
        return titleStatement;
    }

    @Override
    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        visit(word.getReplaceByStatement(), p);
        visit(word.getReplaceOffStatement(), p);

        if (word.getReplacement() != null && word.getCopyStatement() == null) {
            if (word.getReplacement().getType() == Replacement.Type.EQUAL) {
                Replacement.OriginalWord originalWord = word.getReplacement().getOriginalWords().get(0);
                cobolSourcePrinter.visitWord(originalWord.getOriginal(), p);
                if (originalWord.isReplacedWithEmpty()) {
                    originalReplaceLength = word.getPrefix().getWhitespace().length() - word.getWord().length();
                } else {
                    originalReplaceLength = 0;
                    return word;
                }
            } else if (word.getReplacement().getType() == Replacement.Type.REDUCTIVE && !word.getReplacement().isCopiedSource()) {
                if (printedReductiveReplaces.add(word.getReplacement().getId().toString())) {
                    for (Replacement.OriginalWord originalWord : word.getReplacement().getOriginalWords()) {
                        cobolSourcePrinter.visitWord(originalWord.getOriginal(), p);
                    }
                    if (word.getSequenceArea() != null) {
                        word.getSequenceArea().printColumnArea(this, getCursor(), printColumns, p);
                    }
                    if (word.getIndicatorArea() != null) {
                        word.getIndicatorArea().printColumnArea(this, getCursor(), printColumns, p);
                    }
                    beforeSyntax(word, Space.Location.WORD_PREFIX, p);
                    p.append(word.getWord());
                    return word;
                }
            }
        }

        // The COBOL word is a product of a copy statement.
        if (word.getCopyStatement() != null) {
            // Print the original Copy Statement in place of the first word from the copied source.
            if (printedCopyStatements.add(word.getCopyStatement().getId().toString())) {
                visit(word.getCopyStatement(), p);
            }

            // Do not print the AST for the copied source.
            return word;
        }

        if (printColumns) {
            if (word.getLines() != null) {
                for (CobolLine cobolLine : word.getLines()) {
                    visitMarkers(cobolLine.getMarkers(), p);
                    cobolLine.printCobolLine(this, getCursor(), p);
                }
            }
        }

        if (word.getContinuation() != null) {
            word.getContinuation().printContinuation(this, getCursor(), word, printColumns, p);
        } else {
            if (word.getSequenceArea() != null) {
                word.getSequenceArea().printColumnArea(this, getCursor(), printColumns, p);
            }

            if (word.getIndicatorArea() != null) {
                word.getIndicatorArea().printColumnArea(this, getCursor(), printColumns, p);
            }

            if (word.getReplacement() != null &&
                    word.getReplacement().getType() == Replacement.Type.EQUAL &&
                    word.getReplacement().getOriginalWords().get(0).isReplacedWithEmpty()) {
                p.append(StringUtils.repeat(" ", word.getPrefix().getWhitespace().length() - originalReplaceLength));
                originalReplaceLength = 0;
            } else {
                beforeSyntax(word, Space.Location.WORD_PREFIX, p);
            }

            p.append(word.getWord());

            if (word.getCommentArea() != null && !word.getCommentArea().isAdded()) {
                word.getCommentArea().printColumnArea(this, getCursor(), printColumns, p);
            }
        }

        afterSyntax(word, p);
        return word;
    }

    private static final UnaryOperator<String> COBOL_MARKER_WRAPPER =
            out -> "~~" + out + (out.isEmpty() ? "" : "~~") + ">";

    protected void beforeSyntax(CobolPreprocessor c, Space.Location loc, PrintOutputCapture<P> p) {
        beforeSyntax(c.getPrefix(), c.getMarkers(), loc, p);
    }

    protected void beforeSyntax(Space prefix, Markers markers, @Nullable Space.Location loc, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforePrefix(marker, new Cursor(getCursor(), marker), COBOL_MARKER_WRAPPER));
        }
        if (loc != null) {
            visitSpace(prefix, loc, p);
        }
        visitMarkers(markers, p);
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(getCursor(), marker), COBOL_MARKER_WRAPPER));
        }
    }

    protected void afterSyntax(CobolPreprocessor c, PrintOutputCapture<P> p) {
        afterSyntax(c.getMarkers(), p);
    }

    protected void afterSyntax(Markers markers, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(getCursor(), marker), COBOL_MARKER_WRAPPER));
        }
    }
}
