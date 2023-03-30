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
package org.openrewrite.cobol;

import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.lang.Nullable;

public class CobolPreprocessorVisitor<P> extends TreeVisitor<CobolPreprocessor, P> {

    public CobolPreprocessor visitCharData(CobolPreprocessor.CharData charData, P p) {
        CobolPreprocessor.CharData c = charData;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.CHAR_DATA_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCobols(ListUtils.map(c.getCobols(), it -> (CobolPreprocessor.CharDataLine) visit(it, p)));
        return c;
    }

    public CobolPreprocessor visitCharDataLine(CobolPreprocessor.CharDataLine charDataLine, P p) {
        CobolPreprocessor.CharDataLine c = charDataLine;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.CHAR_DATA_LINE_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withWords(ListUtils.map(c.getWords(), it -> visit(it, p)));
        return c;
    }

    public CobolPreprocessor visitCharDataSql(CobolPreprocessor.CharDataSql charDataSql, P p) {
        CobolPreprocessor.CharDataSql c = charDataSql;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.CHAR_DATA_SQL_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCobols(ListUtils.map(c.getCobols(), it -> visit(it, p)));
        return c;
    }

    public CobolPreprocessor visitCommentEntry(CobolPreprocessor.CommentEntry commentEntry, P p) {
        CobolPreprocessor.CommentEntry c = commentEntry;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COMMENT_ENTRY_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withComments(ListUtils.map(c.getComments(), it -> (CobolPreprocessor.Word) visit(it, p)));
        return c;
    }

    public CobolPreprocessor visitCompilationUnit(CobolPreprocessor.CompilationUnit compilationUnit, P p) {
        CobolPreprocessor.CompilationUnit c = compilationUnit;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.PREPROCESSOR_COMPILATION_UNIT_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCobols(ListUtils.map(c.getCobols(), it -> visit(it, p)));
        c = c.withEof((CobolPreprocessor.Word) visit(c.getEof(), p));
        return c;
    }

    public CobolPreprocessor visitCompilerOption(CobolPreprocessor.CompilerOption compilerOption, P p) {
        CobolPreprocessor.CompilerOption c = compilerOption;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COMPILER_OPTION_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCobols(ListUtils.map(c.getCobols(), it -> visit(it, p)));
        return c;
    }

    public CobolPreprocessor visitCompilerOptions(CobolPreprocessor.CompilerOptions compilerOptions, P p) {
        CobolPreprocessor.CompilerOptions c = compilerOptions;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COMPILER_OPTIONS_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCobols(ListUtils.map(c.getCobols(), it -> visit(it, p)));
        return c;
    }

    public CobolPreprocessor visitCompilerXOpts(CobolPreprocessor.CompilerXOpts compilerXOpts, P p) {
        CobolPreprocessor.CompilerXOpts c = compilerXOpts;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COMPILER_XOPTS_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withWord((CobolPreprocessor.Word) visit(c.getWord(), p));
        c = c.withLeftParen((CobolPreprocessor.Word) visit(c.getLeftParen(), p));
        c = c.withCompilerOptions(ListUtils.map(c.getCompilerOptions(), it -> visit(it, p)));
        c = c.withRightParen((CobolPreprocessor.Word) visit(c.getRightParen(), p));
        return c;
    }

    public CobolPreprocessor visitCopyBook(CobolPreprocessor.CopyBook copyBook, P p) {
        CobolPreprocessor.CopyBook c = copyBook;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COPY_BOOK_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withAst(visit(c.getAst(), p));
        c = c.withEof((CobolPreprocessor.Word) visit(c.getEof(), p));
        return c;
    }

    public CobolPreprocessor visitCopySource(CobolPreprocessor.CopySource copySource, P p) {
        CobolPreprocessor.CopySource c = copySource;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COPY_SOURCE_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withName((CobolPreprocessor.Word) visit(c.getName(), p));
        c = c.withWord((CobolPreprocessor.Word) visit(c.getWord(), p));
        c = c.withCopyLibrary((CobolPreprocessor.Word) visit(c.getCopyLibrary(), p));
        return c;
    }

    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, P p) {
        CobolPreprocessor.CopyStatement c = copyStatement;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COPY_STATEMENT_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withWord((CobolPreprocessor.Word) visit(c.getWord(), p));
        c = c.withCopySource((CobolPreprocessor.CopySource) visit(c.getCopySource(), p));
        c = c.withCobols(ListUtils.map(c.getCobols(), it -> visit(it, p)));
        c = c.withDot((CobolPreprocessor.Word) visit(c.getDot(), p));
        c = c.withCopyBook((CobolPreprocessor.CopyBook) visit(c.getCopyBook(), p));
        return c;
    }

    public CobolPreprocessor visitDirectoryPhrase(CobolPreprocessor.DirectoryPhrase directoryPhrase, P p) {
        CobolPreprocessor.DirectoryPhrase d = directoryPhrase;
        d = d.withPrefix(visitSpace(d.getPrefix(), Space.Location.DIRECTORY_PHRASE_PREFIX, p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withWord((CobolPreprocessor.Word) visit(d.getWord(), p));
        d = d.withName((CobolPreprocessor.Word) visit(d.getName(), p));
        return d;
    }

    public CobolPreprocessor visitEjectStatement(CobolPreprocessor.EjectStatement ejectStatement, P p) {
        CobolPreprocessor.EjectStatement e = ejectStatement;
        e = e.withPrefix(visitSpace(e.getPrefix(), Space.Location.EJECT_STATEMENT_PREFIX, p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withWord((CobolPreprocessor.Word) visit(e.getWord(), p));
        e = e.withDot((CobolPreprocessor.Word) visit(e.getDot(), p));
        return e;
    }

    public CobolPreprocessor visitExecStatement(CobolPreprocessor.ExecStatement execStatement, P p) {
        CobolPreprocessor.ExecStatement e = execStatement;
        e = e.withPrefix(visitSpace(e.getPrefix(), Space.Location.EXEC_STATEMENT_PREFIX, p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withWords(ListUtils.map(e.getWords(), it -> (CobolPreprocessor.Word) visit(it, p)));
        e = e.withCobol(visit(e.getCobol(), p));
        e = e.withEndExec((CobolPreprocessor.Word) visit(e.getEndExec(), p));
        e = e.withDot((CobolPreprocessor.Word) visit(e.getDot(), p));
        return e;
    }

    public CobolPreprocessor visitFamilyPhrase(CobolPreprocessor.FamilyPhrase familyPhrase, P p) {
        CobolPreprocessor.FamilyPhrase f = familyPhrase;
        f = f.withPrefix(visitSpace(f.getPrefix(), Space.Location.FAMILY_PHRASE_PREFIX, p));
        f = f.withMarkers(visitMarkers(f.getMarkers(), p));
        f = f.withWord((CobolPreprocessor.Word) visit(f.getWord(), p));
        f = f.withName((CobolPreprocessor.Word) visit(f.getName(), p));
        return f;
    }

    public CobolPreprocessor visitPseudoText(CobolPreprocessor.PseudoText pseudoText, P p) {
        CobolPreprocessor.PseudoText pp = pseudoText;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), Space.Location.PSEUDO_TEXT_PREFIX, p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withDoubleEqualOpen((CobolPreprocessor.Word) visit(pp.getDoubleEqualOpen(), p));
        pp = pp.withCharData((CobolPreprocessor.CharData) visit(pp.getCharData(), p));
        pp = pp.withDoubleEqualClose((CobolPreprocessor.Word) visit(pp.getDoubleEqualClose(), p));
        return pp;
    }

    public CobolPreprocessor visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, P p) {
        CobolPreprocessor.ReplaceArea r = replaceArea;
        r = r.withPrefix(visitSpace(r.getPrefix(), Space.Location.REPLACE_AREA_PREFIX, p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withReplaceByStatement((CobolPreprocessor.ReplaceByStatement) visit(r.getReplaceByStatement(), p));
        r = r.withCobols(ListUtils.map(r.getCobols(), it -> visit(it, p)));
        r = r.withReplaceOffStatement((CobolPreprocessor.ReplaceOffStatement) visit(r.getReplaceOffStatement(), p));
        return r;
    }

    public CobolPreprocessor visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement, P p) {
        CobolPreprocessor.ReplaceByStatement r = replaceByStatement;
        r = r.withPrefix(visitSpace(r.getPrefix(), Space.Location.REPLACE_BY_STATEMENT_PREFIX, p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withWord((CobolPreprocessor.Word) visit(r.getWord(), p));
        r = r.withClauses(ListUtils.map(r.getClauses(), it -> (CobolPreprocessor.ReplaceClause) visit(it, p)));
        r = r.withDot((CobolPreprocessor.Word) visit(r.getDot(), p));
        return r;
    }

    public CobolPreprocessor visitReplaceClause(CobolPreprocessor.ReplaceClause replaceClause, P p) {
        CobolPreprocessor.ReplaceClause r = replaceClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), Space.Location.REPLACE_CLAUSE_PREFIX, p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withReplaceable(visit(r.getReplaceable(), p));
        r = r.withBy((CobolPreprocessor.Word) visit(r.getBy(), p));
        r = r.withReplacement(visit(r.getReplacement(), p));
        r = r.withSubscript(ListUtils.map(r.getSubscript(), it -> visit(it, p)));
        r = r.withDirectoryPhrases(ListUtils.map(r.getDirectoryPhrases(), it -> (CobolPreprocessor.DirectoryPhrase) visit(it, p)));
        r = r.withFamilyPhrase((CobolPreprocessor.FamilyPhrase) visit(r.getFamilyPhrase(), p));
        return r;
    }

    public CobolPreprocessor visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, P p) {
        CobolPreprocessor.ReplaceOffStatement r = replaceOffStatement;
        r = r.withPrefix(visitSpace(r.getPrefix(), Space.Location.REPLACE_OFF_STATEMENT_PREFIX, p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withWords(ListUtils.map(r.getWords(), it -> (CobolPreprocessor.Word) visit(it, p)));
        r = r.withDot((CobolPreprocessor.Word) visit(r.getDot(), p));
        return r;
    }

    public CobolPreprocessor visitReplacingPhrase(CobolPreprocessor.ReplacingPhrase replacingPhrase, P p) {
        CobolPreprocessor.ReplacingPhrase r = replacingPhrase;
        r = r.withPrefix(visitSpace(r.getPrefix(), Space.Location.REPLACING_PHRASE_PREFIX, p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withWord((CobolPreprocessor.Word) visit(r.getWord(), p));
        r = r.withClauses(ListUtils.map(r.getClauses(), it -> (CobolPreprocessor.ReplaceClause) visit(it, p)));
        return r;
    }

    public CobolPreprocessor visitSkipStatement(CobolPreprocessor.SkipStatement skipStatement, P p) {
        CobolPreprocessor.SkipStatement s = skipStatement;
        s = s.withPrefix(visitSpace(s.getPrefix(), Space.Location.SKIP_STATEMENT_PREFIX, p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withWord((CobolPreprocessor.Word) visit(s.getWord(), p));
        s = s.withDot((CobolPreprocessor.Word) visit(s.getDot(), p));
        return s;
    }

    public CobolPreprocessor visitTitleStatement(CobolPreprocessor.TitleStatement titleStatement, P p) {
        CobolPreprocessor.TitleStatement t = titleStatement;
        t = t.withPrefix(visitSpace(t.getPrefix(), Space.Location.TITLE_STATEMENT_PREFIX, p));
        t = t.withMarkers(visitMarkers(t.getMarkers(), p));
        t = t.withFirst((CobolPreprocessor.Word) visit(t.getFirst(), p));
        t = t.withSecond((CobolPreprocessor.Word) visit(t.getSecond(), p));
        t = t.withDot((CobolPreprocessor.Word) visit(t.getDot(), p));
        return t;
    }

    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, P p) {
        CobolPreprocessor.Word w = word;
        w = w.withPrefix(visitSpace(w.getPrefix(), Space.Location.WORD_PREFIX, p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));

        // Column areas.
        w = w.withCommentArea(visitCommentArea(w.getCommentArea(), p));
        w = w.withIndicatorArea(visitIndicatorArea(w.getIndicatorArea(), p));
        w = w.withSequenceArea(visitSequenceArea(w.getSequenceArea(), p));

        w = w.withContinuation(visitContinuation(w.getContinuation(), p));
        w = w.withLines(ListUtils.map(w.getLines(), it -> visitLine(it, p)));

        // Preprocessed COBOL preservation.
        w = w.withCopyStatement((CobolPreprocessor.CopyStatement) visit(w.getCopyStatement(), p));
        w = w.withReplaceByStatement((CobolPreprocessor.ReplaceByStatement) visit(w.getReplaceByStatement(), p));
        w = w.withReplaceOffStatement((CobolPreprocessor.ReplaceOffStatement) visit(w.getReplaceOffStatement(), p));
        return w;
    }

    public CobolLine visitLine(CobolLine line, P p) {
        CobolLine l = line;
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        SequenceArea sequenceArea = visitSequenceArea(l.getSequenceArea(), p);
        if (sequenceArea != null) {
            l = l.withSequenceArea(sequenceArea);
        }
        IndicatorArea indicatorArea = visitIndicatorArea(l.getIndicatorArea(), p);
        if (indicatorArea != null) {
            l = l.withIndicatorArea(indicatorArea);
        }
        CommentArea commentArea = visitCommentArea(l.getCommentArea(), p);
        if (commentArea != null) {
            l = l.withCommentArea(commentArea);
        }
        return l;
    }

    @Nullable
    public Continuation visitContinuation(@Nullable Continuation continuation, P p) {
        if (continuation == null) {
            return null;
        }
        org.openrewrite.cobol.tree.Continuation c = continuation;
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    @Nullable
    public CommentArea visitCommentArea(@Nullable CommentArea commentArea, P p) {
        if (commentArea == null) {
            return null;
        }
        CommentArea c = commentArea;
        c = c.withMarkers(visitMarkers(commentArea.getMarkers(), p));
        return c;
    }

    @Nullable
    public IndicatorArea visitIndicatorArea(@Nullable IndicatorArea indicatorArea, P p) {
        if (indicatorArea == null) {
            return null;
        }
        IndicatorArea i = indicatorArea;
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }
    @Nullable
    public SequenceArea visitSequenceArea(@Nullable SequenceArea sequenceArea, P p) {
        if (sequenceArea == null) {
            return null;
        }
        SequenceArea s = sequenceArea;
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Space visitSpace(Space space, Space.Location location, P p) {
        return space;
    }
}
