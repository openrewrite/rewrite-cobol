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

import org.openrewrite.cobol.tree.CobolPreprocessor;

public class CobolPreprocessorIsoVisitor<P> extends CobolPreprocessorVisitor<P> {

    public CobolPreprocessor.CharData visitCharData(CobolPreprocessor.CharData charData, P p) {
        return (CobolPreprocessor.CharData) super.visitCharData(charData, p);
    }

    public CobolPreprocessor.CharDataLine visitCharDataLine(CobolPreprocessor.CharDataLine charDataLine, P p) {
        return (CobolPreprocessor.CharDataLine) super.visitCharDataLine(charDataLine, p);
    }

    public CobolPreprocessor.CharDataSql visitCharDataSql(CobolPreprocessor.CharDataSql charDataSql, P p) {
        return (CobolPreprocessor.CharDataSql) super.visitCharDataSql(charDataSql, p);
    }

    public CobolPreprocessor.CompilationUnit visitCompilationUnit(CobolPreprocessor.CompilationUnit compilationUnit, P p) {
        return (CobolPreprocessor.CompilationUnit) super.visitCompilationUnit(compilationUnit, p);
    }

    public CobolPreprocessor.CompilerOption visitCompilerOption(CobolPreprocessor.CompilerOption compilerOption, P p) {
        return (CobolPreprocessor.CompilerOption) super.visitCompilerOption(compilerOption, p);
    }

    public CobolPreprocessor.CompilerOptions visitCompilerOptions(CobolPreprocessor.CompilerOptions compilerOptions, P p) {
        return (CobolPreprocessor.CompilerOptions) super.visitCompilerOptions(compilerOptions, p);
    }

    public CobolPreprocessor.CompilerXOpts visitCompilerXOpts(CobolPreprocessor.CompilerXOpts compilerXOpts, P p) {
        return (CobolPreprocessor.CompilerXOpts) super.visitCompilerXOpts(compilerXOpts, p);
    }

    @Override
    public CobolPreprocessor.CopyBook visitCopyBook(CobolPreprocessor.CopyBook copyBook, P p) {
        return (CobolPreprocessor.CopyBook) super.visitCopyBook(copyBook, p);
    }

    public CobolPreprocessor.CopySource visitCopySource(CobolPreprocessor.CopySource copySource, P p) {
        return (CobolPreprocessor.CopySource) super.visitCopySource(copySource, p);
    }

    public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, P p) {
        return (CobolPreprocessor.CopyStatement) super.visitCopyStatement(copyStatement, p);
    }

    public CobolPreprocessor.DirectoryPhrase visitDirectoryPhrase(CobolPreprocessor.DirectoryPhrase directoryPhrase, P p) {
        return (CobolPreprocessor.DirectoryPhrase) super.visitDirectoryPhrase(directoryPhrase, p);
    }

    public CobolPreprocessor.EjectStatement visitEjectStatement(CobolPreprocessor.EjectStatement ejectStatement, P p) {
        return (CobolPreprocessor.EjectStatement) super.visitEjectStatement(ejectStatement, p);
    }

    public CobolPreprocessor.ExecStatement visitExecStatement(CobolPreprocessor.ExecStatement execStatement, P p) {
        return (CobolPreprocessor.ExecStatement) super.visitExecStatement(execStatement, p);
    }

    public CobolPreprocessor.FamilyPhrase visitFamilyPhrase(CobolPreprocessor.FamilyPhrase familyPhrase, P p) {
        return (CobolPreprocessor.FamilyPhrase) super.visitFamilyPhrase(familyPhrase, p);
    }

    public CobolPreprocessor.PseudoText visitPseudoText(CobolPreprocessor.PseudoText pseudoText, P p) {
        return (CobolPreprocessor.PseudoText) super.visitPseudoText(pseudoText, p);
    }

    public CobolPreprocessor.ReplaceArea visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, P p) {
        return (CobolPreprocessor.ReplaceArea) super.visitReplaceArea(replaceArea, p);
    }

    public CobolPreprocessor.ReplaceByStatement visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement, P p) {
        return (CobolPreprocessor.ReplaceByStatement) super.visitReplaceByStatement(replaceByStatement, p);
    }

    public CobolPreprocessor.ReplaceClause visitReplaceClause(CobolPreprocessor.ReplaceClause replaceClause, P p) {
        return (CobolPreprocessor.ReplaceClause) super.visitReplaceClause(replaceClause, p);
    }

    public CobolPreprocessor.ReplaceOffStatement visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, P p) {
        return (CobolPreprocessor.ReplaceOffStatement) super.visitReplaceOffStatement(replaceOffStatement, p);
    }

    public CobolPreprocessor.ReplacingPhrase visitReplacingPhrase(CobolPreprocessor.ReplacingPhrase replacingPhrase, P p) {
        return (CobolPreprocessor.ReplacingPhrase) super.visitReplacingPhrase(replacingPhrase, p);
    }

    public CobolPreprocessor.SkipStatement visitSkipStatement(CobolPreprocessor.SkipStatement skipStatement, P p) {
        return (CobolPreprocessor.SkipStatement) super.visitSkipStatement(skipStatement, p);
    }

    public CobolPreprocessor.TitleStatement visitTitleStatement(CobolPreprocessor.TitleStatement titleStatement, P p) {
        return (CobolPreprocessor.TitleStatement) super.visitTitleStatement(titleStatement, p);
    }

    public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, P p) {
        return (CobolPreprocessor.Word) super.visitWord(word, p);
    }
}
