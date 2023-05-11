/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree;

import lombok.*;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolPreprocessorVisitor;
import org.openrewrite.cobol.internal.CobolPreprocessorPrinter;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

public interface CobolPreprocessor extends Tree {

    @SuppressWarnings("unchecked")
    @Override
    default <R extends Tree, P> R accept(TreeVisitor<R, P> v, P p) {
        return (R) acceptCobolPreprocessor(v.adapt(CobolPreprocessorVisitor.class), p);
    }

    @Nullable
    default <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
        return v.defaultValue(this, p);
    }

    @Override
    default <P> boolean isAcceptable(TreeVisitor<?, P> v, P p) {
        return v.isAdaptableTo(CobolPreprocessorVisitor.class);
    }

    Space getPrefix();

    <P extends CobolPreprocessor> P withPrefix(Space prefix);

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CompilationUnit implements CobolPreprocessor, SourceFile {

        @EqualsAndHashCode.Include
        UUID id;

        Path sourcePath;

        @Nullable
        FileAttributes fileAttributes;

        Space prefix;
        Markers markers;

        @Nullable // for backwards compatibility
        @With(AccessLevel.PRIVATE)
        String charsetName;

        boolean charsetBomMarked;

        @Nullable
        Checksum checksum;

        @Override
        public Charset getCharset() {
            return charsetName == null ? StandardCharsets.UTF_8 : Charset.forName(charsetName);
        }

        @SuppressWarnings("unchecked")
        @Override
        public SourceFile withCharset(Charset charset) {
            return withCharsetName(charset.name());
        }

        List<CobolPreprocessor> cobols;

        Word eof;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCompilationUnit(this, p);
        }

        @Override
        public <P> TreeVisitor<?, PrintOutputCapture<P>> printer(Cursor cursor) {
            return new CobolPreprocessorPrinter<>(true, true);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CharData implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        List<CharDataLine> cobols;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCharData(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CharDataLine implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        List<CobolPreprocessor> words;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCharDataLine(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CharDataSql implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        List<CobolPreprocessor> cobols;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCharDataSql(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CommentEntry implements CobolPreprocessor, Comment {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Word> comments;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCommentEntry(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CompilerOption implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        List<CobolPreprocessor> cobols;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCompilerOption(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CompilerOptions implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;

        List<CobolPreprocessor> cobols;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCompilerOptions(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CompilerXOpts implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;
        Word leftParen;
        List<CobolPreprocessor> compilerOptions;
        Word rightParen;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCompilerXOpts(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CopyBook implements CobolPreprocessor, CobolSourceFile {

        UUID id;
        Space prefix;
        Markers markers;

        // ... verbose for quality assurance.
        Path sourcePath;

        @Nullable
        FileAttributes fileAttributes;

        @Nullable // for backwards compatibility
        @With(AccessLevel.PRIVATE)
        String charsetName;

        boolean charsetBomMarked;

        @Nullable
        Checksum checksum;

        @Override
        public Charset getCharset() {
            return charsetName == null ? StandardCharsets.UTF_8 : Charset.forName(charsetName);
        }

        @SuppressWarnings("unchecked")
        @Override
        public SourceFile withCharset(Charset charset) {
            return withCharsetName(charset.name());
        }

        CobolPreprocessor ast;
        CobolPreprocessor.Word eof;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCopyBook(this, p);
        }

        @Override
        public <P> TreeVisitor<?, PrintOutputCapture<P>> printer(Cursor cursor) {
            return new CobolPreprocessorPrinter<>(true, true);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CopySource implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word name;

        @Nullable
        Word word;

        @Nullable
        Word copyLibrary;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCopySource(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CopyStatement implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;
        CopySource copySource;

        List<CobolPreprocessor> cobols;

        Word dot;

        // TODO: temp POC clean up.
        @Nullable
        CopyBook copyBook;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCopyStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DirectoryPhrase implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;
        Word name;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitDirectoryPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EjectStatement implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;

        @Nullable
        Word dot;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitEjectStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ExecStatement implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        List<Word> words;

        CobolPreprocessor cobol;

        Word endExec;

        @Nullable
        Word dot;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitExecStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class FamilyPhrase implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;
        Word name;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitFamilyPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PseudoText implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word doubleEqualOpen;

        @Nullable
        CharData charData;

        Word doubleEqualClose;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitPseudoText(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReplaceArea implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        ReplaceByStatement replaceByStatement;

        @Nullable
        List<CobolPreprocessor> cobols;

        @Nullable
        ReplaceOffStatement replaceOffStatement;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitReplaceArea(this, p);
        }
    }

    /**
     * Define the {@link ReplaceClause}s in a {@link ReplaceArea}.
     */
    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReplaceByStatement implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;
        List<ReplaceClause> clauses;
        Word dot;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitReplaceByStatement(this, p);
        }
    }

    /**
     * A ReplaceClause is a rule to change COBOL words that is applied to code in either a {@link CopyBook} or {@link ReplaceArea}.
     */
    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReplaceClause implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        CobolPreprocessor replaceable;
        Word by;
        CobolPreprocessor replacement;

        @Nullable
        List<CobolPreprocessor> subscript;

        @Nullable
        List<DirectoryPhrase> directoryPhrases;

        @Nullable
        FamilyPhrase familyPhrase;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitReplaceClause(this, p);
        }
    }

    /**
     * A ReplaceOffStatement is a part of preprocessing and marks the end of a {@link ReplaceArea}.
     */
    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReplaceOffStatement implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        List<Word> words;

        Word dot;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitReplaceOffStatement(this, p);
        }
    }

    /**
     * Define the {@link ReplaceClause}s in a {@link ReplacingPhrase}.
     */
    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReplacingPhrase implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;
        List<ReplaceClause> clauses;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitReplacingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SkipStatement implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;

        @Nullable
        Word dot;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitSkipStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class TitleStatement implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word first;
        Word second;

        @Nullable
        Word dot;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitTitleStatement(this, p);
        }
    }

    @SuppressWarnings("unchecked")
    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    class Word implements CobolPreprocessor {

        @EqualsAndHashCode.Include
        @With
        UUID id;

        Space prefix;
        Markers markers;

        @With
        Cobol.Word cobolWord;

        public Space getPrefix() {
            return cobolWord.getPrefix();
        }

        public Word withPrefix(Space prefix) {
            return cobolWord.getPrefix() == prefix ? this : withCobolWord(cobolWord.withPrefix(prefix));
        }

        public Markers getMarkers() {
            return cobolWord.getMarkers();
        }

        public Word withMarkers(Markers markers) {
            return cobolWord.getMarkers() == markers ? this : withCobolWord(cobolWord.withMarkers(markers));
        }

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitWord(this, p);
        }
    }
}
