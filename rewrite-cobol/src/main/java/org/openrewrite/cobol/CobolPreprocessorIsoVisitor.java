/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol;

import org.openrewrite.cobol.tree.CobolPreprocessor;

public class CobolPreprocessorIsoVisitor<P> extends CobolPreprocessorVisitor<P> {

    public CobolPreprocessorIsoVisitor() {
        super(new CobolVisitor<>());
    }

    @Override
    public CobolPreprocessor.CharData visitCharData(CobolPreprocessor.CharData charData, P p) {
        return (CobolPreprocessor.CharData) super.visitCharData(charData, p);
    }

    @Override
    public CobolPreprocessor.CharDataLine visitCharDataLine(CobolPreprocessor.CharDataLine charDataLine, P p) {
        return (CobolPreprocessor.CharDataLine) super.visitCharDataLine(charDataLine, p);
    }

    @Override
    public CobolPreprocessor.CharDataSql visitCharDataSql(CobolPreprocessor.CharDataSql charDataSql, P p) {
        return (CobolPreprocessor.CharDataSql) super.visitCharDataSql(charDataSql, p);
    }

    @Override
    public CobolPreprocessor.CommentEntry visitCommentEntry(CobolPreprocessor.CommentEntry commentEntry, P p) {
        return (CobolPreprocessor.CommentEntry) super.visitCommentEntry(commentEntry, p);
    }

    @Override
    public CobolPreprocessor.CompilationUnit visitCompilationUnit(CobolPreprocessor.CompilationUnit compilationUnit, P p) {
        return (CobolPreprocessor.CompilationUnit) super.visitCompilationUnit(compilationUnit, p);
    }

    @Override
    public CobolPreprocessor.CompilerOption visitCompilerOption(CobolPreprocessor.CompilerOption compilerOption, P p) {
        return (CobolPreprocessor.CompilerOption) super.visitCompilerOption(compilerOption, p);
    }

    @Override
    public CobolPreprocessor.CompilerOptions visitCompilerOptions(CobolPreprocessor.CompilerOptions compilerOptions, P p) {
        return (CobolPreprocessor.CompilerOptions) super.visitCompilerOptions(compilerOptions, p);
    }

    @Override
    public CobolPreprocessor.CompilerXOpts visitCompilerXOpts(CobolPreprocessor.CompilerXOpts compilerXOpts, P p) {
        return (CobolPreprocessor.CompilerXOpts) super.visitCompilerXOpts(compilerXOpts, p);
    }

    @Override
    public CobolPreprocessor.CopyBook visitCopyBook(CobolPreprocessor.CopyBook copyBook, P p) {
        return (CobolPreprocessor.CopyBook) super.visitCopyBook(copyBook, p);
    }

    @Override
    public CobolPreprocessor.CopySource visitCopySource(CobolPreprocessor.CopySource copySource, P p) {
        return (CobolPreprocessor.CopySource) super.visitCopySource(copySource, p);
    }

    @Override
    public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, P p) {
        return (CobolPreprocessor.CopyStatement) super.visitCopyStatement(copyStatement, p);
    }

    @Override
    public CobolPreprocessor.DirectoryPhrase visitDirectoryPhrase(CobolPreprocessor.DirectoryPhrase directoryPhrase, P p) {
        return (CobolPreprocessor.DirectoryPhrase) super.visitDirectoryPhrase(directoryPhrase, p);
    }

    @Override
    public CobolPreprocessor.EjectStatement visitEjectStatement(CobolPreprocessor.EjectStatement ejectStatement, P p) {
        return (CobolPreprocessor.EjectStatement) super.visitEjectStatement(ejectStatement, p);
    }

    @Override
    public CobolPreprocessor.ExecStatement visitExecStatement(CobolPreprocessor.ExecStatement execStatement, P p) {
        return (CobolPreprocessor.ExecStatement) super.visitExecStatement(execStatement, p);
    }

    @Override
    public CobolPreprocessor.FamilyPhrase visitFamilyPhrase(CobolPreprocessor.FamilyPhrase familyPhrase, P p) {
        return (CobolPreprocessor.FamilyPhrase) super.visitFamilyPhrase(familyPhrase, p);
    }

    @Override
    public CobolPreprocessor.PseudoText visitPseudoText(CobolPreprocessor.PseudoText pseudoText, P p) {
        return (CobolPreprocessor.PseudoText) super.visitPseudoText(pseudoText, p);
    }

    @Override
    public CobolPreprocessor.ReplaceArea visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, P p) {
        return (CobolPreprocessor.ReplaceArea) super.visitReplaceArea(replaceArea, p);
    }

    @Override
    public CobolPreprocessor.ReplaceByStatement visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement, P p) {
        return (CobolPreprocessor.ReplaceByStatement) super.visitReplaceByStatement(replaceByStatement, p);
    }

    @Override
    public CobolPreprocessor.ReplaceClause visitReplaceClause(CobolPreprocessor.ReplaceClause replaceClause, P p) {
        return (CobolPreprocessor.ReplaceClause) super.visitReplaceClause(replaceClause, p);
    }

    @Override
    public CobolPreprocessor.ReplaceOffStatement visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, P p) {
        return (CobolPreprocessor.ReplaceOffStatement) super.visitReplaceOffStatement(replaceOffStatement, p);
    }

    @Override
    public CobolPreprocessor.ReplacingPhrase visitReplacingPhrase(CobolPreprocessor.ReplacingPhrase replacingPhrase, P p) {
        return (CobolPreprocessor.ReplacingPhrase) super.visitReplacingPhrase(replacingPhrase, p);
    }

    @Override
    public CobolPreprocessor.SkipStatement visitSkipStatement(CobolPreprocessor.SkipStatement skipStatement, P p) {
        return (CobolPreprocessor.SkipStatement) super.visitSkipStatement(skipStatement, p);
    }

    @Override
    public CobolPreprocessor.TitleStatement visitTitleStatement(CobolPreprocessor.TitleStatement titleStatement, P p) {
        return (CobolPreprocessor.TitleStatement) super.visitTitleStatement(titleStatement, p);
    }

    @Override
    public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, P p) {
        return (CobolPreprocessor.Word) super.visitWord(word, p);
    }
}
