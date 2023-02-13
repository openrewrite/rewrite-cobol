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
import org.openrewrite.cobol.CobolPreprocessorVisitor;
import org.openrewrite.cobol.search.SearchResultKey;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.StringUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;
import org.openrewrite.marker.SearchResult;

import java.util.List;
import java.util.Map;
import java.util.Optional;
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
        visitSpace(charData.getPrefix(), p);
        visitMarkers(charData.getMarkers(), p);
        visit(charData.getCobols(), p);
        return charData;
    }

    public CobolPreprocessor visitCharDataLine(CobolPreprocessor.CharDataLine charDataLine, PrintOutputCapture<P> p) {
        visitSpace(charDataLine.getPrefix(), p);
        visitMarkers(charDataLine.getMarkers(), p);
        visit(charDataLine.getWords(), p);
        return charDataLine;
    }

    public CobolPreprocessor visitCharDataSql(CobolPreprocessor.CharDataSql charDataSql, PrintOutputCapture<P> p) {
        visitSpace(charDataSql.getPrefix(), p);
        visitMarkers(charDataSql.getMarkers(), p);
        visit(charDataSql.getCobols(), p);
        return charDataSql;
    }

    @Override
    public CobolPreprocessor visitCommentEntry(CobolPreprocessor.CommentEntry commentEntry, PrintOutputCapture<P> p) {
        visitSpace(commentEntry.getPrefix(), p);
        visitMarkers(commentEntry.getMarkers(), p);
        visit(commentEntry.getComments(), p);
        return commentEntry;
    }

    public CobolPreprocessor visitCompilationUnit(CobolPreprocessor.CompilationUnit compilationUnit, PrintOutputCapture<P> p) {
        visitSpace(compilationUnit.getPrefix(), p);
        visitMarkers(compilationUnit.getMarkers(), p);
        visit(compilationUnit.getCobols(), p);
        visit(compilationUnit.getEof(), p);
        return compilationUnit;
    }

    public CobolPreprocessor visitCompilerOption(CobolPreprocessor.CompilerOption compilerOption, PrintOutputCapture<P> p) {
        visitSpace(compilerOption.getPrefix(), p);
        visitMarkers(compilerOption.getMarkers(), p);
        visit(compilerOption.getCobols(), p);
        return compilerOption;
    }

    public CobolPreprocessor visitCompilerOptions(CobolPreprocessor.CompilerOptions compilerOptions, PrintOutputCapture<P> p) {
        visitSpace(compilerOptions.getPrefix(), p);
        visitMarkers(compilerOptions.getMarkers(), p);
        visit(compilerOptions.getWord(), p);
        visit(compilerOptions.getCobols(), p);
        return compilerOptions;
    }

    public CobolPreprocessor visitCompilerXOpts(CobolPreprocessor.CompilerXOpts compilerXOpts, PrintOutputCapture<P> p) {
        visitSpace(compilerXOpts.getPrefix(), p);
        visitMarkers(compilerXOpts.getMarkers(), p);
        visit(compilerXOpts.getWord(), p);
        visit(compilerXOpts.getLeftParen(), p);
        visit(compilerXOpts.getCompilerOptions(), p);
        visit(compilerXOpts.getRightParen(), p);
        return compilerXOpts;
    }

    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, PrintOutputCapture<P> p) {
        // The CopyBook is not printed as a part of the original source.
        return copyBook;
    }

    public CobolPreprocessor visitCopySource(CobolPreprocessor.CopySource copySource, PrintOutputCapture<P> p) {
        visitSpace(copySource.getPrefix(), p);
        visitMarkers(copySource.getMarkers(), p);
        visit(copySource.getName(), p);
        visit(copySource.getWord(), p);
        visit(copySource.getCopyLibrary(), p);
        return copySource;
    }

    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        visitSpace(copyStatement.getPrefix(), p);
        visitMarkers(copyStatement.getMarkers(), p);
        visit(copyStatement.getWord(), p);
        visit(copyStatement.getCopySource(), p);
        visit(copyStatement.getCobols(), p);
        visit(copyStatement.getDot(), p);
        return copyStatement;
    }

    public CobolPreprocessor visitDirectoryPhrase(CobolPreprocessor.DirectoryPhrase directoryPhrase, PrintOutputCapture<P> p) {
        visitSpace(directoryPhrase.getPrefix(), p);
        visitMarkers(directoryPhrase.getMarkers(), p);
        visit(directoryPhrase.getWord(), p);
        visit(directoryPhrase.getName(), p);
        return directoryPhrase;
    }

    public CobolPreprocessor visitEjectStatement(CobolPreprocessor.EjectStatement ejectStatement, PrintOutputCapture<P> p) {
        visitSpace(ejectStatement.getPrefix(), p);
        visitMarkers(ejectStatement.getMarkers(), p);
        visit(ejectStatement.getWord(), p);
        visit(ejectStatement.getDot(), p);
        return ejectStatement;
    }

    public CobolPreprocessor visitExecStatement(CobolPreprocessor.ExecStatement execStatement, PrintOutputCapture<P> p) {
        visitSpace(execStatement.getPrefix(), p);
        visitMarkers(execStatement.getMarkers(), p);
        visit(execStatement.getWords(), p);
        visit(execStatement.getCobol(), p);
        visit(execStatement.getEndExec(), p);
        visit(execStatement.getDot(), p);
        return execStatement;
    }

    public CobolPreprocessor visitFamilyPhrase(CobolPreprocessor.FamilyPhrase familyPhrase, PrintOutputCapture<P> p) {
        visitSpace(familyPhrase.getPrefix(), p);
        visitMarkers(familyPhrase.getMarkers(), p);
        visit(familyPhrase.getWord(), p);
        visit(familyPhrase.getName(), p);
        return familyPhrase;
    }

    public CobolPreprocessor visitPseudoText(CobolPreprocessor.PseudoText pseudoText, PrintOutputCapture<P> p) {
        visitSpace(pseudoText.getPrefix(), p);
        visitMarkers(pseudoText.getMarkers(), p);
        visit(pseudoText.getDoubleEqualOpen(), p);
        visit(pseudoText.getCharData(), p);
        visit(pseudoText.getDoubleEqualClose(), p);
        return pseudoText;
    }

    public CobolPreprocessor visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, PrintOutputCapture<P> p) {
        visitSpace(replaceArea.getPrefix(), p);
        visitMarkers(replaceArea.getMarkers(), p);
        visit(replaceArea.getReplaceByStatement(), p);
        visit(replaceArea.getCobols(), p);
        visit(replaceArea.getReplaceOffStatement(), p);
        return replaceArea;
    }

    public CobolPreprocessor visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement, PrintOutputCapture<P> p) {
        visitSpace(replaceByStatement.getPrefix(), p);
        visitMarkers(replaceByStatement.getMarkers(), p);
        visit(replaceByStatement.getWord(), p);
        visit(replaceByStatement.getClauses(), p);
        visit(replaceByStatement.getDot(), p);
        return replaceByStatement;
    }

    public CobolPreprocessor visitReplaceClause(CobolPreprocessor.ReplaceClause replaceClause, PrintOutputCapture<P> p) {
        visitSpace(replaceClause.getPrefix(), p);
        visitMarkers(replaceClause.getMarkers(), p);
        visit(replaceClause.getReplaceable(), p);
        visit(replaceClause.getBy(), p);
        visit(replaceClause.getReplacement(), p);
        visit(replaceClause.getSubscript(), p);
        visit(replaceClause.getDirectoryPhrases(), p);
        visit(replaceClause.getFamilyPhrase(), p);
        return replaceClause;
    }

    public CobolPreprocessor visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, PrintOutputCapture<P> p) {
        visitSpace(replaceOffStatement.getPrefix(), p);
        visitMarkers(replaceOffStatement.getMarkers(), p);
        visit(replaceOffStatement.getWords(), p);
        visit(replaceOffStatement.getDot(), p);
        return replaceOffStatement;
    }

    public CobolPreprocessor visitReplacingPhrase(CobolPreprocessor.ReplacingPhrase replacingPhrase, PrintOutputCapture<P> p) {
        visitSpace(replacingPhrase.getPrefix(), p);
        visitMarkers(replacingPhrase.getMarkers(), p);
        visit(replacingPhrase.getWord(), p);
        visit(replacingPhrase.getClauses(), p);
        return replacingPhrase;
    }

    public CobolPreprocessor visitSkipStatement(CobolPreprocessor.SkipStatement skipStatement, PrintOutputCapture<P> p) {
        visitSpace(skipStatement.getPrefix(), p);
        visitMarkers(skipStatement.getMarkers(), p);
        visit(skipStatement.getWord(), p);
        visit(skipStatement.getDot(), p);
        return skipStatement;
    }

    public Space visitSpace(Space space, PrintOutputCapture<P> p) {
        p.append(space.getWhitespace());
        return space;
    }

    public CobolPreprocessor visitTitleStatement(CobolPreprocessor.TitleStatement titleStatement, PrintOutputCapture<P> p) {
        visitSpace(titleStatement.getPrefix(), p);
        visitMarkers(titleStatement.getMarkers(), p);
        visit(titleStatement.getFirst(), p);
        visit(titleStatement.getSecond(), p);
        visit(titleStatement.getDot(), p);
        return titleStatement;
    }

    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        // Column area markers.
        SequenceArea sequenceArea = null;
        IndicatorArea indicatorArea = null;
        CommentArea commentArea = null;

        // CobolPreprocessor markers.
        ReplaceBy replaceBy = null;
        ReplaceOff replaceOff = null;
        Replace replace = null;

        Lines lines = null;
        Continuation continuation = null;

        // Search markers.
        SearchResult indicatorSearch = null;

        for (Marker marker : word.getMarkers().getMarkers()) {
            if (marker instanceof SequenceArea) {
                sequenceArea = (SequenceArea) marker;
            } else if (marker instanceof IndicatorArea) {
                indicatorArea = (IndicatorArea) marker;
            } else if (marker instanceof CommentArea) {
                commentArea = (CommentArea) marker;
            } else if (marker instanceof SearchResult) {
                SearchResult m = (SearchResult) marker;
                if (SearchResultKey.INDICATOR_AREA.equals(m.getDescription())) {
                    indicatorSearch = m;
                }
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

            if (indicatorArea != null) {
                visitIndicatorArea(indicatorArea, p);
            }

            if (replace != null && replace.isReplacedWithEmpty()) {
                p.append(StringUtils.repeat(" ", word.getPrefix().getWhitespace().length() - originalReplaceLength));
                originalReplaceLength = 0;
            } else {
                visitSpace(word.getPrefix(), p);
            }
            p.append(word.getWord());

            if (commentArea != null) {
                visitCommentArea(commentArea, p);
            }
        }

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

        visitSpace(word.getPrefix(), p);

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
        visitMarkers(indicatorArea.getMarkers(), p);
        if (printColumns) {
            p.append(indicatorArea.getIndicator());
        }

        p.append(indicatorArea.getContinuationPrefix());
    }

    public void visitCommentArea(CommentArea commentArea, PrintOutputCapture<P> p) {
        visitSpace(commentArea.getPrefix(), p);
        if (printColumns) {
            p.append(commentArea.getComment());
        }
        visitSpace(commentArea.getEndOfLine(), p);
    }
}
