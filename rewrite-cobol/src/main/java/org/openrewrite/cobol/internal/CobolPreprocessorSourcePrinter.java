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
import org.openrewrite.marker.SearchResult;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 * Print the original preprocessed COBOL.
 * Note: All the logic to print column areas exists in visitWord.
 */
public class CobolPreprocessorSourcePrinter<P> extends CobolPreprocessorVisitor<PrintOutputCapture<P>> {

    private final boolean printColumns;
    private int originalReplaceLength;

    public CobolPreprocessorSourcePrinter(boolean printColumns) {
        this.printColumns = printColumns;
    }

    public CobolPreprocessor visitCharData(CobolPreprocessor.CharData charData, PrintOutputCapture<P> p) {
        beforeSyntax(charData, Space.Location.CHAR_DATA_PREFIX, p);
        visit(charData.getCobols(), p);
        afterSyntax(charData, p);
        return charData;
    }

    public CobolPreprocessor visitCharDataLine(CobolPreprocessor.CharDataLine charDataLine, PrintOutputCapture<P> p) {
        beforeSyntax(charDataLine, Space.Location.CHAR_DATA_LINE_PREFIX, p);
        visit(charDataLine.getWords(), p);
        afterSyntax(charDataLine, p);
        return charDataLine;
    }

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

    public CobolPreprocessor visitCompilationUnit(CobolPreprocessor.CompilationUnit compilationUnit, PrintOutputCapture<P> p) {
        beforeSyntax(compilationUnit, Space.Location.PREPROCESSOR_COMPILATION_UNIT_PREFIX, p);
        visit(compilationUnit.getCobols(), p);
        visit(compilationUnit.getEof(), p);
        afterSyntax(compilationUnit, p);
        return compilationUnit;
    }

    public CobolPreprocessor visitCompilerOption(CobolPreprocessor.CompilerOption compilerOption, PrintOutputCapture<P> p) {
        beforeSyntax(compilerOption, Space.Location.COMPILER_OPTION_PREFIX, p);
        visit(compilerOption.getCobols(), p);
        afterSyntax(compilerOption, p);
        return compilerOption;
    }

    public CobolPreprocessor visitCompilerOptions(CobolPreprocessor.CompilerOptions compilerOptions, PrintOutputCapture<P> p) {
        beforeSyntax(compilerOptions, Space.Location.COMPILER_OPTIONS_PREFIX, p);
        visit(compilerOptions.getWord(), p);
        visit(compilerOptions.getCobols(), p);
        afterSyntax(compilerOptions, p);
        return compilerOptions;
    }

    public CobolPreprocessor visitCompilerXOpts(CobolPreprocessor.CompilerXOpts compilerXOpts, PrintOutputCapture<P> p) {
        beforeSyntax(compilerXOpts, Space.Location.COMPILER_XOPTS_PREFIX, p);
        visit(compilerXOpts.getWord(), p);
        visit(compilerXOpts.getLeftParen(), p);
        visit(compilerXOpts.getCompilerOptions(), p);
        visit(compilerXOpts.getRightParen(), p);
        afterSyntax(compilerXOpts, p);
        return compilerXOpts;
    }

    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
        // The CopyBook is not printed as a part of the original source.
        return copyBook;
    }

    public CobolPreprocessor visitCopySource(CobolPreprocessor.CopySource copySource, PrintOutputCapture<P> p) {
        beforeSyntax(copySource, Space.Location.COPY_SOURCE_PREFIX, p);
        visit(copySource.getName(), p);
        visit(copySource.getWord(), p);
        visit(copySource.getCopyLibrary(), p);
        afterSyntax(copySource, p);
        return copySource;
    }

    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        beforeSyntax(copyStatement, Space.Location.COPY_STATEMENT_PREFIX, p);
        visit(copyStatement.getWord(), p);
        visit(copyStatement.getCopySource(), p);
        visit(copyStatement.getCobols(), p);
        visit(copyStatement.getDot(), p);
        afterSyntax(copyStatement, p);
        return copyStatement;
    }

    public CobolPreprocessor visitDirectoryPhrase(CobolPreprocessor.DirectoryPhrase directoryPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(directoryPhrase, Space.Location.DIRECTORY_PHRASE_PREFIX, p);
        visit(directoryPhrase.getWord(), p);
        visit(directoryPhrase.getName(), p);
        afterSyntax(directoryPhrase, p);
        return directoryPhrase;
    }

    public CobolPreprocessor visitEjectStatement(CobolPreprocessor.EjectStatement ejectStatement, PrintOutputCapture<P> p) {
        beforeSyntax(ejectStatement, Space.Location.EJECT_STATEMENT_PREFIX, p);
        visit(ejectStatement.getWord(), p);
        visit(ejectStatement.getDot(), p);
        afterSyntax(ejectStatement, p);
        return ejectStatement;
    }

    public CobolPreprocessor visitExecStatement(CobolPreprocessor.ExecStatement execStatement, PrintOutputCapture<P> p) {
        beforeSyntax(execStatement, Space.Location.EXEC_STATEMENT_PREFIX, p);
        visit(execStatement.getWords(), p);
        visit(execStatement.getCobol(), p);
        visit(execStatement.getEndExec(), p);
        visit(execStatement.getDot(), p);
        afterSyntax(execStatement, p);
        return execStatement;
    }

    public CobolPreprocessor visitFamilyPhrase(CobolPreprocessor.FamilyPhrase familyPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(familyPhrase, Space.Location.FAMILY_PHRASE_PREFIX, p);
        visit(familyPhrase.getWord(), p);
        visit(familyPhrase.getName(), p);
        afterSyntax(familyPhrase, p);
        return familyPhrase;
    }

    public CobolPreprocessor visitIndicatorArea(CobolPreprocessor.IndicatorArea indicatorArea, PrintOutputCapture<P> p) {
        if (printColumns) {
            beforeSyntax(indicatorArea, Space.Location.INDICATOR_AREA_PREFIX, p);
            p.out.append(indicatorArea.getIndicator());
            afterSyntax(indicatorArea, p);
        }
        p.out.append(indicatorArea.getContinuationPrefix());
        return indicatorArea;
    }

    public CobolPreprocessor visitPseudoText(CobolPreprocessor.PseudoText pseudoText, PrintOutputCapture<P> p) {
        beforeSyntax(pseudoText, Space.Location.PSEUDO_TEXT_PREFIX, p);
        visit(pseudoText.getDoubleEqualOpen(), p);
        visit(pseudoText.getCharData(), p);
        visit(pseudoText.getDoubleEqualClose(), p);
        afterSyntax(pseudoText, p);
        return pseudoText;
    }

    public CobolPreprocessor visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, PrintOutputCapture<P> p) {
        beforeSyntax(replaceArea, Space.Location.REPLACE_AREA_PREFIX, p);
        visit(replaceArea.getReplaceByStatement(), p);
        visit(replaceArea.getCobols(), p);
        visit(replaceArea.getReplaceOffStatement(), p);
        afterSyntax(replaceArea, p);
        return replaceArea;
    }

    public CobolPreprocessor visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement, PrintOutputCapture<P> p) {
        beforeSyntax(replaceByStatement, Space.Location.REPLACE_BY_STATEMENT_PREFIX, p);
        visit(replaceByStatement.getWord(), p);
        visit(replaceByStatement.getClauses(), p);
        visit(replaceByStatement.getDot(), p);
        afterSyntax(replaceByStatement, p);
        return replaceByStatement;
    }

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

    public CobolPreprocessor visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, PrintOutputCapture<P> p) {
        beforeSyntax(replaceOffStatement, Space.Location.REPLACE_OFF_STATEMENT_PREFIX, p);
        visit(replaceOffStatement.getWords(), p);
        visit(replaceOffStatement.getDot(), p);
        afterSyntax(replaceOffStatement, p);
        return replaceOffStatement;
    }

    public CobolPreprocessor visitReplacingPhrase(CobolPreprocessor.ReplacingPhrase replacingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(replacingPhrase, Space.Location.REPLACING_PHRASE_PREFIX, p);
        visit(replacingPhrase.getWord(), p);
        visit(replacingPhrase.getClauses(), p);
        afterSyntax(replacingPhrase, p);
        return replacingPhrase;
    }

    public CobolPreprocessor visitSkipStatement(CobolPreprocessor.SkipStatement skipStatement, PrintOutputCapture<P> p) {
        beforeSyntax(skipStatement, Space.Location.SKIP_STATEMENT_PREFIX, p);
        visit(skipStatement.getWord(), p);
        visit(skipStatement.getDot(), p);
        afterSyntax(skipStatement, p);
        return skipStatement;
    }

    public Space visitSpace(Space space, Space.Location location, PrintOutputCapture<P> p) {
        p.append(space.getWhitespace());
        return space;
    }

    public CobolPreprocessor visitTitleStatement(CobolPreprocessor.TitleStatement titleStatement, PrintOutputCapture<P> p) {
        beforeSyntax(titleStatement, Space.Location.TITLE_STATEMENT_PREFIX, p);
        visit(titleStatement.getFirst(), p);
        visit(titleStatement.getSecond(), p);
        visit(titleStatement.getDot(), p);
        afterSyntax(titleStatement, p);
        return titleStatement;
    }

    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        // Column area markers.
        SequenceArea sequenceArea = null;
        CommentArea commentArea = null;

        // CobolPreprocessor markers.
        ReplaceBy replaceBy = null;
        ReplaceOff replaceOff = null;
        Replace replace = null;

        Lines lines = null;
        Continuation continuation = null;

        for (Marker marker : word.getMarkers().getMarkers()) {
            if (marker instanceof SequenceArea) {
                sequenceArea = (SequenceArea) marker;
            } else if (marker instanceof CommentArea) {
                commentArea = (CommentArea) marker;
            } else if (marker instanceof ReplaceBy) {
                replaceBy = (ReplaceBy) marker;
            } else if (marker instanceof ReplaceOff) {
                replaceOff = (ReplaceOff) marker;
            } else if (marker instanceof Replace) {
                replace = (Replace) marker;
            } else if (marker instanceof Lines) {
                lines = (Lines) marker;
            } else if (marker instanceof Continuation) {
                continuation = (Continuation) marker;
            }
        }

        if (replaceBy != null) {
            visit(replaceBy.getStatement(), p);
        }

        if (replaceOff != null) {
            visit(replaceOff.getReplaceOff(), p);
        }

        if (replace != null) {
            // Print the original copy
            visit(replace.getOriginalWord(), p);

            if (replace.isReplacedWithEmpty()) {
                originalReplaceLength = word.getPrefix().getWhitespace().length() + word.getWord().length();
            } else {
                originalReplaceLength = 0;
                return word;
            }
        }

        if (lines != null) {
            visitLines(lines, p);
        }

        if (continuation != null) {
            visitContinuation(word, continuation, p);
        } else {
            if (sequenceArea != null) {
                visitSequenceArea(sequenceArea, p);
            }

            visit(word.getIndicatorArea(), p);

            if (replace != null && replace.isReplacedWithEmpty()) {
                p.append(StringUtils.repeat(" ", word.getPrefix().getWhitespace().length() - originalReplaceLength));
                originalReplaceLength = 0;
            } else {
                beforeSyntax(word, Space.Location.WORD_PREFIX, p);
            }
            p.append(word.getWord());

            if (commentArea != null) {
                visitCommentArea(commentArea, p);
            }
        }

        afterSyntax(word, p);
        return word;
    }

    public void visitContinuation(CobolPreprocessor.Word word, Continuation continuation, PrintOutputCapture<P> p) {
        visitContinuation(word, continuation, null, p);
    }

    public void visitContinuation(CobolPreprocessor.Word word, Continuation continuation, @Nullable SearchResult searchResult, PrintOutputCapture<P> p) {
        if (continuation.getContinuations().containsKey(0)) {
            Markers markers = continuation.getContinuations().get(0);
            Optional<SequenceArea> sequenceArea = markers.findFirst(SequenceArea.class);
            sequenceArea.ifPresent(it -> visitSequenceArea(it, p));

            Optional<IndicatorArea> indicatorArea = markers.findFirst(IndicatorArea.class);
            indicatorArea.ifPresent(it -> visitIndicatorArea(it, p));
        }

        visitSpace(word.getPrefix(), Space.Location.CONTINUATION_PREFIX, p);

        char[] charArray = word.getWord().toCharArray();
        for (int i = 0; i < charArray.length; i++) {
            if (i != 0 && continuation.getContinuations().containsKey(i)) {
                Markers markers = continuation.getContinuations().get(i);
                Optional<CommentArea> commentArea = markers.findFirst(CommentArea.class);
                commentArea.ifPresent(it -> visitCommentArea(it, p));

                Optional<SequenceArea> sequenceArea = markers.findFirst(SequenceArea.class);
                sequenceArea.ifPresent(it -> visitSequenceArea(it, p));

                Optional<IndicatorArea> indicatorArea = markers.findFirst(IndicatorArea.class);
                indicatorArea.ifPresent(it -> visitIndicatorArea(it, p));
            }
            char c = charArray[i];
            p.append(c);
        }

        List<Markers> lastMarkers = continuation.getContinuations().entrySet().stream()
                .filter(it -> it.getKey() > word.getWord().length())
                .map(Map.Entry::getValue)
                .collect(Collectors.toList());

        if (!lastMarkers.isEmpty()) {
            Markers markers = lastMarkers.get(0);
            Optional<CommentArea> commentArea = markers.findFirst(CommentArea.class);
            commentArea.ifPresent(it -> visitCommentArea(it, p));
        }
    }

    public void visitLines(Lines lines, PrintOutputCapture<P> p) {
        if (printColumns) {
            for (Lines.Line line : lines.getLines()) {
                if (line.isCopiedSource()) {
                    continue;
                }

                if (line.getSequenceArea() != null) {
                    visitSequenceArea(line.getSequenceArea(), p);
                }

                if (line.getIndicatorArea() != null) {
                    visitIndicatorArea(line.getIndicatorArea(), p);
                }

                p.append(line.getContent());
                if (line.getCommentArea() != null) {
                    visitCommentArea(line.getCommentArea(), p);
                }
            }
        }
    }

    public void visitSequenceArea(SequenceArea sequenceArea, PrintOutputCapture<P> p) {
        if (printColumns) {
            p.append(sequenceArea.getSequence());
        }
    }

    public void visitIndicatorArea(IndicatorArea indicatorArea, PrintOutputCapture<P> p) {
        if (printColumns) {
            p.append(indicatorArea.getIndicator());
        }

        p.append(indicatorArea.getContinuationPrefix());
    }

    public void visitCommentArea(CommentArea commentArea, PrintOutputCapture<P> p) {
        visitSpace(commentArea.getPrefix(), Space.Location.COMMENT_AREA_PREFIX, p);
        if (printColumns) {
            p.append(commentArea.getComment());
        }
        visitSpace(commentArea.getEndOfLine(), Space.Location.COMMENT_AREA_EOL, p);
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
