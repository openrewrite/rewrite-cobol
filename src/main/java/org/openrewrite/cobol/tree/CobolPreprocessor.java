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
        return v instanceof CobolPreprocessorVisitor ? (R) acceptCobolPreprocessor((CobolPreprocessorVisitor<P>) v, p) : v.defaultValue(this, p);
    }

    @Nullable
    default <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
        return v.defaultValue(this, p);
    }

    @Override
    default <P> boolean isAcceptable(TreeVisitor<?, P> v, P p) {
        return v instanceof CobolPreprocessorVisitor;
    }

    Space getPrefix();

    <P extends CobolPreprocessor> P withPrefix(Space prefix);

    <P extends CobolPreprocessor> P withMarkers(Markers markers);

    Markers getMarkers();

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CharData implements CobolPreprocessor {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        List<Cobol> cobols;

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

        List<Word> words;

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

        List<Cobol> cobols;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCharDataSql(this, p);
        }
    }

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

        List<Cobol> cobols;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitCompilationUnit(this, p);
        }

        @Override
        public <P> TreeVisitor<?, PrintOutputCapture<P>> printer(Cursor cursor) {
            return new CobolPreprocessorPrinter<>();
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
    class CopySource implements CobolPreprocessor {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        CobolPreprocessor cobolPreprocessor;

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
        Name name;

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
        Name name;

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
        CobolPreprocessor charData;

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

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReplaceClause implements CobolPreprocessor {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;


        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitReplaceClause(this, p);
        }
    }

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

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReplacingPhrase implements CobolPreprocessor {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Word word;
        List<CobolPreprocessor> cobols;

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

        List<Word> words;

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

        List<Word> words;

        @Nullable
        Word dot;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitTitleStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Word implements CobolPreprocessor {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        String word;

        @Override
        public <P> CobolPreprocessor acceptCobolPreprocessor(CobolPreprocessorVisitor<P> v, P p) {
            return v.visitWord(this, p);
        }
    }
}
