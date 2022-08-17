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
import lombok.experimental.FieldDefaults;
import lombok.experimental.NonFinal;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolVisitor;
import org.openrewrite.cobol.internal.CobolPrinter;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.lang.ref.WeakReference;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

public interface Cobol extends Tree {

    @SuppressWarnings("unchecked")
    @Override
    default <R extends Tree, P> R accept(TreeVisitor<R, P> v, P p) {
        return v instanceof CobolVisitor ? (R) acceptCobol((CobolVisitor<P>) v, p) : v.defaultValue(this, p);
    }

    @Nullable
    default <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
        return v.defaultValue(this, p);
    }

    @Override
    default <P> boolean isAcceptable(TreeVisitor<?, P> v, P p) {
        return v instanceof CobolVisitor;
    }

    Space getPrefix();

    <P extends Cobol> P withPrefix(Space prefix);

    <P extends Cobol> P withMarkers(Markers markers);

    Markers getMarkers();

    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class CompilationUnit implements Cobol, SourceFile {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Path sourcePath;

        @With
        @Getter
        @Nullable FileAttributes fileAttributes;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @Nullable // for backwards compatibility
        @With(AccessLevel.PRIVATE)
        String charsetName;

        @With
        @Getter
        boolean charsetBomMarked;

        @With
        @Getter
        @Nullable Checksum checksum;

        @Override
        public Charset getCharset() {
            return charsetName == null ? StandardCharsets.UTF_8 : Charset.forName(charsetName);
        }

        @Override
        public SourceFile withCharset(Charset charset) {
            return withCharsetName(charset.name());
        }

        List<CobolRightPadded<ProgramUnit>> programUnits;

        public List<ProgramUnit> getProgramUnits() {
            return CobolRightPadded.getElements(programUnits);
        }

        public CompilationUnit withProgramUnits(List<ProgramUnit> body) {
            return getPadding().withProgramUnits(CobolRightPadded.withElements(this.programUnits, body));
        }

        @With
        @Getter
        String eof;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCompilationUnit(this, p);
        }

        @Override
        public <P> TreeVisitor<?, PrintOutputCapture<P>> printer(Cursor cursor) {
            return new CobolPrinter<>();
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final CompilationUnit t;

            public List<CobolRightPadded<ProgramUnit>> getProgramUnits() {
                return t.programUnits;
            }

            public CompilationUnit withProgramUnits(List<CobolRightPadded<ProgramUnit>> programUnits) {
                return t.programUnits == programUnits ? t : new CompilationUnit(t.id, t.sourcePath, t.fileAttributes, t.prefix, t.markers, t.charsetName, t.charsetBomMarked, t.checksum, programUnits, t.eof);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Abbreviation implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        CobolWord not;

        @Nullable
        RelationalOperator relationalOperator;

        @Nullable
        CobolWord leftParen;

        Cobol arithmeticExpression;

        @Nullable
        Cobol abbreviation;

        @Nullable
        CobolWord rightParen;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAbbreviation(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Accept implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord accept;
        Identifier identifier;
        Cobol operation;

        @Nullable
        StatementPhrase onExceptionClause;

        @Nullable
        StatementPhrase notOnExceptionClause;

        @Nullable
        CobolWord endAccept;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAccept(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AcceptFromDateStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAcceptFromDateStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AcceptFromEscapeKeyStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAcceptFromEscapeKeyStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AcceptFromMnemonicStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord from;

        Identifier mnemonicName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAcceptFromMnemonicStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AcceptMessageCountStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAcceptMessageCountStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AccessModeClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord type;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAccessModeClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Add implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord add;
        Cobol operation;

        @Nullable
        StatementPhrase onSizeError;

        @Nullable
        CobolWord endAdd;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAdd(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class AddTo implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        List<Name> from;

        @Nullable
        CobolContainer<Name> to;

        @Nullable
        CobolContainer<Name> giving;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAddTo(this, p);
        }

        public List<Name> getTo() {
            return to.getElements();
        }

        public AddTo withTo(List<Name> to) {
            return getPadding().withTo(this.to.getPadding().withElements(CobolRightPadded.withElements(
                    this.to.getPadding().getElements(), to)));
        }

        public List<Name> getGiving() {
            return giving.getElements();
        }

        public AddTo withGiving(List<Name> giving) {
            return getPadding().withGiving(this.giving.getPadding().withElements(CobolRightPadded.withElements(
                    this.giving.getPadding().getElements(), giving)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final AddTo t;

            @Nullable
            public CobolContainer<Name> getTo() {
                return t.to;
            }

            public AddTo withTo(@Nullable CobolContainer<Name> to) {
                return t.to == to ? t : new AddTo(t.padding, t.id, t.prefix, t.markers, t.from, to, t.giving);
            }

            @Nullable
            public CobolContainer<Name> getGiving() {
                return t.giving;
            }

            public AddTo withGiving(@Nullable CobolContainer<Name> giving) {
                return t.giving == giving ? t : new AddTo(t.padding, t.id, t.prefix, t.markers, t.from, t.to, giving);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AlphabetAlso implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        List<Literal> literals;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAlphabetAlso(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AlphabetClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord alphabet;
        Name name;

        @Nullable
        List<Cobol> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAlphabetClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AlphabetLiteral implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Literal literal;

        @Nullable
        AlphabetThrough alphabetThrough;

        @Nullable
        List<AlphabetAlso> alphabetAlso;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAlphabetLiteral(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AlphabetThrough implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Literal literal;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAlphabetThrough(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class AlteredGoTo implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord words;

        CobolLeftPadded<CobolWord> dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAlteredGoTo(this, p);
        }

        public CobolWord getDot() {
            return dot.getElement();
        }

        public AlteredGoTo withDot(CobolWord dot) {
            //noinspection ConstantConditions
            return getPadding().withDot(CobolLeftPadded.withElement(this.dot, dot));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final AlteredGoTo t;

            public CobolLeftPadded<CobolWord> getDot() {
                return t.dot;
            }

            public AlteredGoTo withDot(CobolLeftPadded<CobolWord> dot) {
                return t.dot == dot ? t : new AlteredGoTo(t.padding, t.id, t.prefix, t.markers, t.words, dot);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AlternateRecordKeyClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> alternateWords;
        QualifiedDataName qualifiedDataName;

        @Nullable
        PasswordClause passwordClause;

        List<CobolWord> duplicates;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAlternateRecordKeyClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AlterProceedTo implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        ProcedureName from;
        List<CobolWord> words;
        ProcedureName to;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAlterProceedTo(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AlterStatement implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        List<AlterProceedTo> alterProceedTo;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAlterStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AndOrCondition implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord logicalOperator;

        @Nullable
        CombinableCondition combinableCondition;

        @Nullable
        List<Cobol> abbreviations;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAndOrCondition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Argument implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        Cobol first;

        @Nullable
        CobolWord integerLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitArgument(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ArithmeticExpression implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        MultDivs multDivs;

        @Nullable
        List<PlusMinus> plusMinuses;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitArithmeticExpression(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class AssignClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitAssignClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class BlockContainsClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> firstWords;
        CobolWord integerLiteral;

        @Nullable
        BlockContainsTo blockContainsTo;

        @Nullable
        CobolWord lastWords;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitBlockContainsClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class BlockContainsTo implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord to;
        CobolWord integerLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitBlockContainsTo(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Call implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord call;
        Name identifier;

        @Nullable
        CallPhrase callUsingPhrase;

        @Nullable
        CallGivingPhrase callGivingPhrase;

        @Nullable
        StatementPhrase onOverflowPhrase;

        @Nullable
        StatementPhrase onExceptionClause;

        @Nullable
        StatementPhrase notOnExceptionClause;

        @Nullable
        CobolWord endCall;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCall(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CallBy implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCallBy(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CallGivingPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCallGivingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CallPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Cobol> parameters;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCallPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Cancel implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord cancel;
        List<CancelCall> cancelCalls;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCancel(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CancelCall implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        Name libraryName;

        @Nullable
        List<CobolWord> by;

        @Nullable
        Identifier identifier;

        @Nullable
        Literal literal;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCancelCall(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ChannelClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Literal literal;

        @Nullable
        CobolWord is;

        Identifier mnemonicName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitChannelClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ClassClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord clazz;
        CobolWord className;
        List<CobolWord> words;
        List<ClassClauseThrough> throughs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitClassClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ClassClauseThrough implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;
        Space prefix;
        Markers markers;
        Name from;
        @Nullable
        CobolWord through;
        @Nullable
        Name to;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitClassClauseThrough(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ClassCondition implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;

        @Nullable
        List<CobolWord> words;

        @Nullable
        Cobol type;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitClassCondition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Close implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord close;
        List<CloseFile> closeFiles;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitClose(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CloseFile implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name fileName;

        @Nullable
        Cobol closeStatement;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCloseFile(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ClosePortFileIOStatement implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Cobol> closePortFileIOUsing;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitClosePortFileIOStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ClosePortFileIOUsingAssociatedData implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord associatedData;
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitClosePortFileIOUsingAssociatedData(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ClosePortFileIOUsingAssociatedDataLength implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitClosePortFileIOUsingAssociatedDataLength(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ClosePortFileIOUsingCloseDisposition implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitClosePortFileIOUsingCloseDisposition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CloseReelUnitStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCloseReelUnitStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CloseRelativeStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCloseRelativeStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CobolWord implements Literal, Identifier {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        String word;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCobolWord(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CodeSetClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord alphabetName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCodeSetClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CollatingSequenceAlphabet implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Identifier alphabetName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCollatingSequenceAlphabet(this, p);
        }
    }


    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class CollatingSequenceClause implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        List<CobolWord> words;

        CobolContainer<Identifier> alphabetName;

        @Getter
        @Nullable
        @With
        CollatingSequenceAlphabet alphanumeric;

        @Getter
        @Nullable
        @With
        CollatingSequenceAlphabet national;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCollatingSequenceClause(this, p);
        }

        public List<Identifier> getAlphabetName() {
            return alphabetName.getElements();
        }

        public CollatingSequenceClause withAlphabetName(List<Identifier> alphabetName) {
            return getPadding().withAlphabetName(this.alphabetName.getPadding().withElements(CobolRightPadded.withElements(
                    this.alphabetName.getPadding().getElements(), alphabetName)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final CollatingSequenceClause t;

            public CobolContainer<Identifier> getAlphabetName() {
                return t.alphabetName;
            }

            public CollatingSequenceClause withAlphabetName(CobolContainer<Identifier> alphabetName) {
                return t.alphabetName == alphabetName ? t : new CollatingSequenceClause(t.padding, t.id, t.prefix, t.markers, t.words, alphabetName, t.alphanumeric, t.national);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CombinableCondition implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        CobolWord not;

        Cobol simpleCondition;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCombinableCondition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CommitmentControlClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord fileName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCommitmentControlClause(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class CommunicationDescriptionEntryFormat1 implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord cd;

        @Getter
        @With
        CobolWord name;

        @Getter
        @With
        List<CobolWord> words;

        CobolContainer<Cobol> inputs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCommunicationDescriptionEntryFormat1(this, p);
        }

        public List<Cobol> getInputs() {
            return inputs.getElements();
        }

        public CommunicationDescriptionEntryFormat1 withInputs(List<Cobol> inputs) {
            return getPadding().withInputs(this.inputs.getPadding().withElements(CobolRightPadded.withElements(
                    this.inputs.getPadding().getElements(), inputs)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final CommunicationDescriptionEntryFormat1 t;

            public CobolContainer<Cobol> getInputs() {
                return t.inputs;
            }

            public CommunicationDescriptionEntryFormat1 withInputs(CobolContainer<Cobol> inputs) {
                return t.inputs == inputs ? t : new CommunicationDescriptionEntryFormat1(t.padding, t.id, t.prefix, t.markers, t.cd, t.name, t.words, inputs);
            }
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class CommunicationDescriptionEntryFormat2 implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord cd;

        @Getter
        @With
        CobolWord name;

        @Getter
        @With
        List<CobolWord> words;

        CobolContainer<Cobol> outputs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCommunicationDescriptionEntryFormat2(this, p);
        }

        public List<Cobol> getOutputs() {
            return outputs.getElements();
        }

        public CommunicationDescriptionEntryFormat2 withOutputs(List<Cobol> outputs) {
            return getPadding().withOutputs(this.outputs.getPadding().withElements(CobolRightPadded.withElements(
                    this.outputs.getPadding().getElements(), outputs)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final CommunicationDescriptionEntryFormat2 t;

            public CobolContainer<Cobol> getOutputs() {
                return t.outputs;
            }

            public CommunicationDescriptionEntryFormat2 withOutputs(CobolContainer<Cobol> outputs) {
                return t.outputs == outputs ? t : new CommunicationDescriptionEntryFormat2(t.padding, t.id, t.prefix, t.markers, t.cd, t.name, t.words, outputs);
            }
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class CommunicationDescriptionEntryFormat3 implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord cd;

        @Getter
        @With
        CobolWord name;

        @Getter
        @With
        List<CobolWord> words;

        CobolContainer<Cobol> initialIOs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCommunicationDescriptionEntryFormat3(this, p);
        }

        public List<Cobol> getInitialIOs() {
            return initialIOs.getElements();
        }

        public CommunicationDescriptionEntryFormat3 withInitialIOs(List<Cobol> initialIOs) {
            return getPadding().withInitialIOs(this.initialIOs.getPadding().withElements(CobolRightPadded.withElements(
                    this.initialIOs.getPadding().getElements(), initialIOs)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final CommunicationDescriptionEntryFormat3 t;

            public CobolContainer<Cobol> getInitialIOs() {
                return t.initialIOs;
            }

            public CommunicationDescriptionEntryFormat3 withInitialIOs(CobolContainer<Cobol> initialIOs) {
                return t.initialIOs == initialIOs ? t : new CommunicationDescriptionEntryFormat3(t.padding, t.id, t.prefix, t.markers, t.cd, t.name, t.words, initialIOs);
            }
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class CommunicationSection implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        List<CobolWord> words;

        CobolContainer<Cobol> entries;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCommunicationSection(this, p);
        }

        public List<Cobol> getEntries() {
            return entries.getElements();
        }

        public CommunicationSection withEntries(List<Cobol> entries) {
            return getPadding().withEntries(this.entries.getPadding().withElements(CobolRightPadded.withElements(
                    this.entries.getPadding().getElements(), entries)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final CommunicationSection t;

            public CobolContainer<Cobol> getEntries() {
                return t.entries;
            }

            public CommunicationSection withEntries(CobolContainer<Cobol> entries) {
                return t.entries == entries ? t : new CommunicationSection(t.padding, t.id, t.prefix, t.markers, t.words, entries);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Compute implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord compute;
        List<Roundable> roundables;
        CobolWord equalWord;
        ArithmeticExpression arithmeticExpression;
        StatementPhrase onSizeErrorPhrase;
        StatementPhrase notOnSizeErrorPhrase;

        @Nullable
        CobolWord endCompute;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCompute(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Condition implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CombinableCondition combinableCondition;
        List<AndOrCondition> andOrConditions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCondition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ConditionNameReference implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;

        @Nullable
        List<InData> inDatas;

        @Nullable
        InFile inFile;

        @Nullable
        List<Parenthesized> references;

        @Nullable
        List<InMnemonic> inMnemonics;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitConditionNameReference(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @AllArgsConstructor
    class ConditionNameSubscriptReference implements Cobol {
        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord leftParen;

        @Getter
        @With
        List<Cobol> subscripts;

        @Getter
        @With
        CobolWord rightParen;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitConditionNameSubscriptReference(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ConfigurationSection implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;
        List<Cobol> paragraphs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitConfigurationSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Continue implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitContinue(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class CurrencyClause implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        List<CobolWord> words;

        @Getter
        @With
        Literal literal;

        @Nullable
        CobolLeftPadded<CobolWord> pictureSymbol;

        @Getter
        @Nullable
        @With
        Literal pictureSymbolLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitCurrencyClause(this, p);
        }

        @Nullable
        public CobolWord getPictureSymbol() {
            return pictureSymbol == null ? null : pictureSymbol.getElement();
        }

        public CurrencyClause withPictureSymbol(@Nullable CobolWord pictureSymbol) {
            if (pictureSymbol == null) {
                return this.pictureSymbol == null ? this : new CurrencyClause(id, prefix, markers, words, literal, null, pictureSymbolLiteral);
            }
            return getPadding().withPictureSymbol(CobolLeftPadded.withElement(this.pictureSymbol, pictureSymbol));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final CurrencyClause t;

            @Nullable
            public CobolLeftPadded<CobolWord> getPictureSymbol() {
                return t.pictureSymbol;
            }

            public CurrencyClause withPictureSymbol(@Nullable CobolLeftPadded<CobolWord> pictureSymbol) {
                return t.pictureSymbol == pictureSymbol ? t : new CurrencyClause(t.padding, t.id, t.prefix, t.markers, t.words, t.literal, pictureSymbol, t.pictureSymbolLiteral);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataAlignedClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord aligned;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataAlignedClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataBaseSection implements DataDivisionSection {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;
        List<DataBaseSectionEntry> entries;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataBaseSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataBaseSectionEntry implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord db;
        Literal from;
        CobolWord invoke;
        Literal to;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataBaseSectionEntry(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataBlankWhenZeroClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataBlankWhenZeroClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataCommonOwnLocalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataCommonOwnLocalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataDescriptionEntry implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        List<CobolWord> words;

        @Nullable
        CobolWord name;

        @Nullable
        List<Cobol> clauses;

        @Nullable
        CobolWord dot;
        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataDescriptionEntry(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataDivision implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;
        List<DataDivisionSection> sections;
        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataDivision(this, p);
        }
    }


    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataExternalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> redefines;
        Literal literal;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataExternalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataGlobalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataGlobalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataIntegerStringClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord redefines;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataIntegerStringClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataJustifiedClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataJustifiedClause(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class DataOccursClause implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord occurs;

        @Getter
        @With
        Name name;

        @Getter
        @Nullable
        @With
        DataOccursTo dataOccursTo;

        @Getter
        @Nullable
        @With
        CobolWord times;

        @Getter
        @Nullable
        @With
        DataOccursDepending dataOccursDepending;

        @Nullable
        CobolContainer<Cobol> sortIndexed;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataOccursClause(this, p);
        }

        public List<Cobol> getSortIndexed() {
            return sortIndexed.getElements();
        }

        public DataOccursClause withSortIndexed(List<Cobol> sortIndexed) {
            return getPadding().withSortIndexed(this.sortIndexed.getPadding().withElements(CobolRightPadded.withElements(
                    this.sortIndexed.getPadding().getElements(), sortIndexed)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final DataOccursClause t;

            @Nullable
            public CobolContainer<Cobol> getSortIndexed() {
                return t.sortIndexed;
            }

            public DataOccursClause withSortIndexed(@Nullable CobolContainer<Cobol> sortIndexed) {
                return t.sortIndexed == sortIndexed ? t : new DataOccursClause(t.padding, t.id, t.prefix, t.markers, t.occurs, t.name, t.dataOccursTo, t.times, t.dataOccursDepending, sortIndexed);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataOccursDepending implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataOccursDepending(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataOccursIndexed implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<CobolWord> indexNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataOccursIndexed(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataOccursSort implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<QualifiedDataName> qualifiedDataNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataOccursSort(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataOccursTo implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord to;
        CobolWord integerLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataOccursTo(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataPictureClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Cobol> pictures;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataPictureClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataReceivedByClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataReceivedByClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataRecordAreaClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataRecordAreaClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataRecordsClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Name> dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataRecordsClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataRedefinesClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord redefines;
        CobolWord dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataRedefinesClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataRenamesClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord renames;

        QualifiedDataName fromName;

        @Nullable
        CobolWord through;

        @Nullable
        QualifiedDataName toName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataRenamesClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataSignClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataSignClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataSynchronizedClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataSynchronizedClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataThreadLocalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataThreadLocalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataTypeClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        Parenthesized parenthesized;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataTypeClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataTypeDefClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataTypeDefClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataUsageClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataUsageClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataUsingClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataUsingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataValueClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Cobol> cobols;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataValueClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DataWithLowerBoundsClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDataWithLowerBoundsClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DecimalPointClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDecimalPointClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DefaultComputationalSignClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDefaultComputationalSignClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DefaultDisplaySignClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDefaultDisplaySignClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Delete implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord delete;
        Name fileName;

        @Nullable
        CobolWord record;

        @Nullable
        StatementPhrase invalidKey;

        @Nullable
        StatementPhrase notInvalidKey;

        @Nullable
        CobolWord endDelete;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDelete(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DestinationCountClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDestinationCountClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DestinationTableClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> firstWords;
        CobolWord integerLiteral;
        List<CobolWord> secondWords;
        List<CobolWord> indexNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDestinationTableClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Disable implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord disable;
        List<CobolWord> type;
        Name cdName;

        @Nullable
        CobolWord with;

        CobolWord key;
        Name keyName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDisable(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Display implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord display;
        List<Name> operands;

        @Nullable
        DisplayAt displayAt;

        @Nullable
        DisplayUpon displayUpon;

        @Nullable
        CobolWord displayWith;

        @Nullable
        StatementPhrase onExceptionClause;

        @Nullable
        StatementPhrase notOnExceptionClause;

        @Nullable
        CobolWord endDisplay;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDisplay(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DisplayAt implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord at;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDisplayAt(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DisplayUpon implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord upon;
        CobolWord name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDisplayUpon(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Divide implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord divide;
        Name name;
        Cobol action;

        @Nullable
        DivideRemainder divideRemainder;

        @Nullable
        StatementPhrase onSizeErrorPhrase;

        @Nullable
        StatementPhrase notOnSizeErrorPhrase;

        @Nullable
        CobolWord endDivide;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDivide(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DivideGiving implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;
        Name name;

        @Nullable
        DivideGivingPhrase divideGivingPhrase;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDivideGiving(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DivideGivingPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord giving;
        List<Roundable> roundable;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDivideGivingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DivideInto implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord into;
        List<Roundable> roundable;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDivideInto(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class DivideRemainder implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord remainder;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitDivideRemainder(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Enable implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord enable;
        List<CobolWord> type;
        Name cdName;

        @Nullable
        CobolWord with;

        CobolWord key;
        Name keyName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEnable(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EndKeyClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEndKeyClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EndProgram implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name programName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEndProgram(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class Entry implements Statement {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord entry;

        @Getter
        @With
        Literal literal;

        @Nullable
        CobolContainer<Identifier> identifiers;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEntry(this, p);
        }

        public List<Identifier> getIdentifiers() {
            return identifiers.getElements();
        }

        public Entry withIdentifiers(List<Identifier> identifiers) {
            return getPadding().withIdentifiers(this.identifiers.getPadding().withElements(CobolRightPadded.withElements(
                    this.identifiers.getPadding().getElements(), identifiers)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final Entry t;

            @Nullable
            public CobolContainer<Identifier> getIdentifiers() {
                return t.identifiers;
            }

            public Entry withIdentifiers(@Nullable CobolContainer<Identifier> identifiers) {
                return t.identifiers == identifiers ? t : new Entry(t.padding, t.id, t.prefix, t.markers, t.entry, t.literal, identifiers);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EnvironmentDivision implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;
        List<Cobol> body;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEnvironmentDivision(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Evaluate implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord evaluate;
        Cobol select;

        @Nullable
        List<EvaluateAlso> alsoSelect;

        @Nullable
        List<EvaluateWhenPhrase> whenPhrase;

        @Nullable
        StatementPhrase whenOther;

        @Nullable
        CobolWord endPhrase;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEvaluate(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EvaluateAlso implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord also;
        Cobol select;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEvaluateAlso(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EvaluateAlsoCondition implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord also;

        EvaluateCondition condition;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEvaluateAlsoCondition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EvaluateCondition implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        List<CobolWord> words;

        @Nullable
        Cobol condition;

        @Nullable
        EvaluateThrough evaluateThrough;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEvaluateCondition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EvaluateThrough implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord through;
        Cobol value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEvaluateThrough(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EvaluateValueThrough implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        CobolWord not;

        Cobol value;

        @Nullable
        EvaluateThrough evaluateThrough;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEvaluateValueThrough(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EvaluateWhen implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord when;
        EvaluateCondition condition;

        @Nullable
        List<EvaluateAlsoCondition> alsoCondition;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEvaluateWhen(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class EvaluateWhenPhrase implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<EvaluateWhenPhrase> whens;

        @Nullable
        List<Statement> statements;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitEvaluateWhenPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ExecCicsStatement implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> execCicsLines;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitExecCicsStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ExecSqlImsStatement implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> execSqlLmsLines;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitExecSqlImsStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ExecSqlStatement implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> execSqlLines;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitExecSqlStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Exhibit implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Identifier> operands;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitExhibit(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Exit implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitExit(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ExternalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitExternalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class FileControlEntry implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Cobol selectClause;

        @Nullable
        List<Cobol> controlClauses;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitFileControlEntry(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class FileControlParagraph implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;

        Markers markers;

        CobolWord fileControl;

        @Nullable
        List<Cobol> controlEntries;

        @Nullable
        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitFileControlParagraph(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class FileDescriptionEntry implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        CobolWord name;

        @Nullable
        List<Cobol> clauses;

        CobolWord dot;
        @Nullable
        List<DataDescriptionEntry> dataDescriptions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitFileDescriptionEntry(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class FileSection implements DataDivisionSection {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;
        List<FileDescriptionEntry> fileDescriptionEntry;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitFileSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class FileStatusClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<QualifiedDataName> qualifiedDataNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitFileStatusClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class FunctionCall implements Identifier {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord function;
        CobolWord functionName;
        List<Parenthesized> arguments;

        @Nullable
        ReferenceModifier referenceModifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitFunctionCall(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Generate implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord generate;
        QualifiedDataName reportName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitGenerate(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class GlobalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitGlobalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class GoBack implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord goBack;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitGoBack(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class GoTo implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        Cobol statement;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitGoTo(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class GoToDependingOnStatement implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        List<ProcedureName> procedureNames;

        List<CobolWord> words;

        @Nullable
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitGoToDependingOnStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class IdentificationDivision implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        ProgramIdParagraph programIdParagraph;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitIdentificationDivision(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class If implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;

        Markers markers;

        CobolWord word;

        Condition condition;

        IfThen ifThen;

        @Nullable
        IfElse ifElse;

        @Nullable
        CobolWord endIf;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitIf(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class IfElse implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;

        @Nullable
        List<CobolWord> nextSentence;

        @Nullable
        List<Statement> statements;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitIfElse(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class IfThen implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        CobolWord word;

        @Nullable
        List<CobolWord> nextSentence;

        @Nullable
        List<Statement> statements;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitIfThen(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InData implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInData(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InFile implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInFile(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Initialize implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord initialize;
        List<Identifier> identifiers;

        @Nullable
        InitializeReplacingPhrase initializeReplacingPhrase;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInitialize(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InitializeReplacingBy implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInitializeReplacingBy(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InitializeReplacingPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord replacing;
        List<InitializeReplacingBy> initializeReplacingBy;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInitializeReplacingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Initiate implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord initiate;
        List<QualifiedDataName> reportNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInitiate(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InLibrary implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInLibrary(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InMnemonic implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInMnemonic(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InputOutputSection implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        List<Cobol> paragraphs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInputOutputSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InSection implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Inspect implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord inspect;
        Identifier identifier;
        Cobol phrase;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspect(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectAllLeading implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;

        @Nullable
        List<InspectBeforeAfter> inspections;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectAllLeading(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class InspectAllLeadings implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord word;

        CobolContainer<InspectAllLeading> leadings;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectAllLeadings(this, p);
        }

        public List<Cobol.InspectAllLeading> getLeadings() {
            return leadings.getElements();
        }

        public InspectAllLeadings withLeadings(List<Cobol.InspectAllLeading> leadings) {
            return getPadding().withLeadings(this.leadings.getPadding().withElements(CobolRightPadded.withElements(
                    this.leadings.getPadding().getElements(), leadings)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final InspectAllLeadings t;

            public CobolContainer<Cobol.InspectAllLeading> getLeadings() {
                return t.leadings;
            }

            public InspectAllLeadings withLeadings(CobolContainer<Cobol.InspectAllLeading> leadings) {
                return t.leadings == leadings ? t : new InspectAllLeadings(t.padding, t.id, t.prefix, t.markers, t.word, leadings);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectBeforeAfter implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectBeforeAfter(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectBy implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord by;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectBy(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectCharacters implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord character;

        @Nullable
        List<InspectBeforeAfter> inspections;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectCharacters(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectConvertingPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord converting;
        Name identifier;
        InspectTo inspectTo;

        @Nullable
        List<InspectBeforeAfter> inspections;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectConvertingPhrase(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class InspectFor implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        Identifier identifier;

        @Getter
        @With
        CobolWord word;

        CobolContainer<Cobol> inspects;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectFor(this, p);
        }

        public List<Cobol> getInspects() {
            return inspects.getElements();
        }

        public InspectFor withInspects(List<Cobol> inspects) {
            return getPadding().withInspects(this.inspects.getPadding().withElements(CobolRightPadded.withElements(
                    this.inspects.getPadding().getElements(), inspects)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final InspectFor t;

            public CobolContainer<Cobol> getInspects() {
                return t.inspects;
            }

            public InspectFor withInspects(CobolContainer<Cobol> inspects) {
                return t.inspects == inspects ? t : new InspectFor(t.padding, t.id, t.prefix, t.markers, t.identifier, t.word, inspects);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectReplacingAllLeading implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name identifier;
        InspectBy inspectBy;

        @Nullable
        List<InspectBeforeAfter> inspections;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectReplacingAllLeading(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectReplacingAllLeadings implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;
        List<InspectReplacingAllLeading> inspections;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectReplacingAllLeadings(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectReplacingCharacters implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;
        InspectBy inspectBy;

        @Nullable
        List<InspectBeforeAfter> inspections;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectReplacingCharacters(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class InspectReplacingPhrase implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord word;

        CobolContainer<Cobol> inspections;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectReplacingPhrase(this, p);
        }

        public List<Cobol> getInspections() {
            return inspections.getElements();
        }

        public InspectReplacingPhrase withInspections(List<Cobol> inspections) {
            return getPadding().withInspections(this.inspections.getPadding().withElements(CobolRightPadded.withElements(
                    this.inspections.getPadding().getElements(), inspections)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final InspectReplacingPhrase t;

            public CobolContainer<Cobol> getInspections() {
                return t.inspections;
            }

            public InspectReplacingPhrase withInspections(CobolContainer<Cobol> inspections) {
                return t.inspections == inspections ? t : new InspectReplacingPhrase(t.padding, t.id, t.prefix, t.markers, t.word, inspections);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectTallyingPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord tallying;
        List<InspectFor> inspectFors;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectTallyingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectTallyingReplacingPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord tallying;
        List<InspectFor> inspectFors;
        List<InspectReplacingPhrase> replacingPhrases;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectTallyingReplacingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InspectTo implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord to;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInspectTo(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class InTable implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitInTable(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class IoControlParagraph implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord iOControl;

        @Getter
        @With
        CobolWord dot;

        @Getter
        @Nullable
        @With
        CobolWord fileName;

        @Getter
        @Nullable
        @With
        CobolWord fileNameDot;

        @Nullable
        CobolContainer<Cobol> clauses;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitIoControlParagraph(this, p);
        }

        public List<Cobol> getClauses() {
            return clauses.getElements();
        }

        public IoControlParagraph withClauses(List<Cobol> clauses) {
            return getPadding().withClauses(this.clauses.getPadding().withElements(CobolRightPadded.withElements(
                    this.clauses.getPadding().getElements(), clauses)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final IoControlParagraph t;

            @Nullable
            public CobolContainer<Cobol> getClauses() {
                return t.clauses;
            }

            public IoControlParagraph withClauses(@Nullable CobolContainer<Cobol> clauses) {
                return t.clauses == clauses ? t : new IoControlParagraph(t.padding, t.id, t.prefix, t.markers, t.iOControl, t.dot, t.fileName, t.fileNameDot, clauses);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LabelRecordsClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        List<CobolWord> dataNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLabelRecordsClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryAttributeClauseFormat1 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryAttributeClauseFormat1(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryAttributeClauseFormat2 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord attribute;

        @Nullable
        LibraryAttributeFunction libraryAttributeFunction;

        List<CobolWord> words;

        @Nullable
        LibraryAttributeParameter libraryAttributeParameter;

        @Nullable
        LibraryAttributeTitle libraryAttributeTitle;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryAttributeClauseFormat2(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryAttributeFunction implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name literal;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryAttributeFunction(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryAttributeParameter implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name literal;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryAttributeParameter(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryAttributeTitle implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name literal;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryAttributeTitle(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryDescriptionEntryFormat1 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord ld;

        CobolWord libraryName;

        CobolWord export;

        @Nullable
        LibraryAttributeClauseFormat1 libraryAttributeClauseFormat1;

        @Nullable
        LibraryEntryProcedureClauseFormat1 libraryEntryProcedureClauseFormat1;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryDescriptionEntryFormat1(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class LibraryDescriptionEntryFormat2 implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord lb;

        @Getter
        @With
        CobolWord libraryName;

        @Getter
        @With
        CobolWord export;

        @Getter
        @Nullable
        @With
        LibraryIsGlobalClause libraryIsGlobalClause;

        @Getter
        @Nullable
        @With
        LibraryIsCommonClause libraryIsCommonClause;

        @Nullable
        CobolContainer<Cobol> clauseFormats;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryDescriptionEntryFormat2(this, p);
        }

        public List<Cobol> getClauseFormats() {
            return clauseFormats.getElements();
        }

        public LibraryDescriptionEntryFormat2 withClauseFormats(List<Cobol> clauseFormats) {
            return getPadding().withClauseFormats(this.clauseFormats.getPadding().withElements(CobolRightPadded.withElements(
                    this.clauseFormats.getPadding().getElements(), clauseFormats)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final LibraryDescriptionEntryFormat2 t;

            @Nullable
            public CobolContainer<Cobol> getClauseFormats() {
                return t.clauseFormats;
            }

            public LibraryDescriptionEntryFormat2 withClauseFormats(@Nullable CobolContainer<Cobol> clauseFormats) {
                return t.clauseFormats == clauseFormats ? t : new LibraryDescriptionEntryFormat2(t.padding, t.id, t.prefix, t.markers, t.lb, t.libraryName, t.export, t.libraryIsGlobalClause, t.libraryIsCommonClause, clauseFormats);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryEntryProcedureClauseFormat1 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord entryProcedure;
        CobolWord programName;

        @Nullable
        LibraryEntryProcedureForClause libraryEntryProcedureForClause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryEntryProcedureClauseFormat1(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryEntryProcedureClauseFormat2 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord entryProcedure;
        CobolWord programName;

        @Nullable
        LibraryEntryProcedureForClause libraryEntryProcedureForClause;

        @Nullable
        LibraryEntryProcedureWithClause libraryEntryProcedureWithClause;

        @Nullable
        LibraryEntryProcedureUsingClause libraryEntryProcedureUsingClause;

        @Nullable
        LibraryEntryProcedureGivingClause libraryEntryProcedureGivingClause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryEntryProcedureClauseFormat2(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryEntryProcedureForClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;
        Name literal;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryEntryProcedureForClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryEntryProcedureGivingClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord giving;
        CobolWord dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryEntryProcedureGivingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryEntryProcedureUsingClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord using;
        List<CobolWord> names;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryEntryProcedureUsingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryEntryProcedureWithClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord with;
        List<CobolWord> names;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryEntryProcedureWithClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryIsCommonClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryIsCommonClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LibraryIsGlobalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLibraryIsGlobalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LinageClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Nullable
        CobolWord lines;

        @Nullable
        List<Cobol> linageAt;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLinageClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LinageFootingAt implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLinageFootingAt(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LinageLinesAtBottom implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLinageLinesAtBottom(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LinageLinesAtTop implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLinageLinesAtTop(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LinkageSection implements DataDivisionSection {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;

        @Nullable
        List<DataDescriptionEntry> dataDescriptions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLinkageSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class LocalStorageSection implements DataDivisionSection {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;

        @Nullable
        CobolWord localData;

        @Nullable
        Name localName;

        @Nullable
        CobolWord dot2;
        List<DataDescriptionEntry> dataDescriptions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitLocalStorageSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Merge implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name fileName;
        List<MergeOnKeyClause> mergeOnKeyClause;

        @Nullable
        MergeCollatingSequencePhrase mergeCollatingSequencePhrase;

        List<MergeUsing> mergeUsing;

        @Nullable
        MergeOutputProcedurePhrase mergeOutputProcedurePhrase;

        List<MergeGivingPhrase> mergeGivingPhrase;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMerge(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Mergeable implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMergeable(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MergeCollatingSequencePhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Name> name;

        @Nullable
        Mergeable mergeCollatingAlphanumeric;

        @Nullable
        Mergeable mergeCollatingNational;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMergeCollatingSequencePhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MergeGiving implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;

        @Nullable
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMergeGiving(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MergeGivingPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        List<MergeGiving> mergeGiving;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMergeGivingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MergeOnKeyClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<QualifiedDataName> qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMergeOnKeyClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MergeOutputProcedurePhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        ProcedureName procedureName;

        @Nullable
        MergeOutputThrough mergeOutputThrough;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMergeOutputProcedurePhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MergeOutputThrough implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        ProcedureName procedureName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMergeOutputThrough(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MergeUsing implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        List<Name> fileNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMergeUsing(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MessageCountClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMessageCountClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MessageDateClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMessageDateClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MessageTimeClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMessageTimeClause(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class MoveCorrespondingToStatement implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord words;

        @Getter
        @With
        Identifier moveCorrespondingToSendingArea;

        CobolContainer<Identifier> to;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMoveCorrespondingToStatement(this, p);
        }

        public List<Identifier> getTo() {
            return to.getElements();
        }

        public MoveCorrespondingToStatement withTo(List<Identifier> to) {
            return getPadding().withTo(this.to.getPadding().withElements(CobolRightPadded.withElements(
                    this.to.getPadding().getElements(), to)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final MoveCorrespondingToStatement t;

            public CobolContainer<Identifier> getTo() {
                return t.to;
            }

            public MoveCorrespondingToStatement withTo(CobolContainer<Identifier> to) {
                return t.to == to ? t : new MoveCorrespondingToStatement(t.padding, t.id, t.prefix, t.markers, t.words, t.moveCorrespondingToSendingArea, to);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MoveStatement implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Cobol moveToStatement;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMoveStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MoveToStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name from;
        CobolWord to;
        List<Identifier> names;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMoveToStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MultDiv implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Powers powers;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMultDiv(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MultDivs implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Powers powers;

        @Nullable
        List<MultDiv> multDivs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMultDivs(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MultipleFileClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Cobol> filePositions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMultipleFileClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MultipleFilePosition implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord fileName;

        @Nullable
        CobolWord position;

        @Nullable
        CobolWord integerLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMultipleFilePosition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Multiply implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name multiplicand;
        CobolWord by;
        Cobol multiply;

        @Nullable
        StatementPhrase onSizeErrorPhrase;

        @Nullable
        StatementPhrase notOnSizeErrorPhrase;

        @Nullable
        CobolWord endMultiply;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMultiply(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class MultiplyGiving implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        Name operand;

        CobolContainer<Roundable> result;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMultiplyGiving(this, p);
        }

        public List<Cobol.Roundable> getResult() {
            return result.getElements();
        }

        public MultiplyGiving withResult(List<Cobol.Roundable> result) {
            return getPadding().withResult(this.result.getPadding().withElements(CobolRightPadded.withElements(
                    this.result.getPadding().getElements(), result)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final MultiplyGiving t;

            public CobolContainer<Cobol.Roundable> getResult() {
                return t.result;
            }

            public MultiplyGiving withResult(CobolContainer<Cobol.Roundable> result) {
                return t.result == result ? t : new MultiplyGiving(t.padding, t.id, t.prefix, t.markers, t.operand, result);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class MultiplyRegular implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Roundable> operand;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitMultiplyRegular(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class NextSentence implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitNextSentence(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ObjectComputer implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        ObjectComputerDefinition computer;

        @Nullable
        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitObjectComputer(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ObjectComputerDefinition implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord computerName;
        List<Cobol> specifications;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitObjectComputerDefinition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class OdtClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Identifier mnemonicName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitOdtClause(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class Open implements Statement {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord words;

        CobolContainer<Cobol> open;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitOpen(this, p);
        }

        public List<Cobol> getOpen() {
            return open.getElements();
        }

        public Open withOpen(List<Cobol> open) {
            return getPadding().withOpen(this.open.getPadding().withElements(CobolRightPadded.withElements(
                    this.open.getPadding().getElements(), open)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final Open t;

            public CobolContainer<Cobol> getOpen() {
                return t.open;
            }

            public Open withOpen(CobolContainer<Cobol> open) {
                return t.open == open ? t : new Open(t.padding, t.id, t.prefix, t.markers, t.words, open);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Openable implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name fileName;

        @Nullable
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitOpenable(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class OpenInputOutputStatement implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        List<Openable> openInput;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitOpenInputOutputStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class OpenIOExtendStatement implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        List<Name> fileNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitOpenIOExtendStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class OrganizationClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitOrganizationClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PaddingCharacterClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPaddingCharacterClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Paragraph implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name paragraphName;

        @Nullable
        CobolWord dot;

        @Nullable
        AlteredGoTo alteredGoTo;

        @Nullable
        List<Sentence> sentences;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitParagraph(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Paragraphs implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Sentence> sentences;
        List<Paragraph> paragraphs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitParagraphs(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Parenthesized implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;
        Space prefix;
        Markers markers;
        CobolWord leftParen;
        List<Cobol> contents;
        CobolWord rightParen;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitParenthesized(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PasswordClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPasswordClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Perform implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Cobol statement;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerform(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Performable implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Cobol expression;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformable(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformFrom implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Cobol from;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformFrom(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformInlineStatement implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        Cobol performType;

        List<Statement> statements;

        CobolWord words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformInlineStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformProcedureStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        ProcedureName procedureName;

        @Nullable
        CobolWord words;

        @Nullable
        ProcedureName throughProcedure;

        @Nullable
        Cobol performType;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformProcedureStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformTestClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformTestClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformTimes implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name value;
        CobolWord words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformTimes(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformUntil implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        PerformTestClause performTestClause;

        CobolWord words;
        Condition condition;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformUntil(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformVarying implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Cobol first;

        @Nullable
        Cobol second;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformVarying(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformVaryingClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        PerformVaryingPhrase performVaryingPhrase;
        List<Performable> performAfter;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformVaryingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PerformVaryingPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;
        PerformFrom from;
        Performable by;
        PerformUntil until;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPerformVaryingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Picture implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        Parenthesized Parenthesized;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPicture(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PictureString implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Picture> pictures;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPictureString(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class PlusMinus implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        MultDivs multDivs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPlusMinus(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Power implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        CobolWord power;

        Cobol expression;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPower(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Powers implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord plusMinusChar;
        Cobol expression;

        @Nullable
        List<Power> powers;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPowers(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDeclarative implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        ProcedureSectionHeader procedureSectionHeader;
        CobolWord dot;
        UseStatement useStatement;
        CobolWord dot2;
        Paragraphs paragraphs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDeclarative(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDeclaratives implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord declaratives;
        CobolWord dot;
        List<ProcedureDeclarative> procedureDeclarative;
        List<CobolWord> endDeclaratives;
        CobolWord dot2;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDeclaratives(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDivision implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        ProcedureDivisionUsingClause procedureDivisionUsingClause;

        @Nullable
        ProcedureDivisionGivingClause procedureDivisionGivingClause;

        CobolWord dot;

        @Nullable
        ProcedureDeclaratives procedureDeclaratives;

        ProcedureDivisionBody body;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDivision(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDivisionBody implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Paragraphs paragraphs;

        @Nullable
        List<ProcedureSection> procedureSection;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDivisionBody(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDivisionByReference implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        CobolWord words;

        Name reference;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDivisionByReference(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDivisionByReferencePhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        List<CobolWord> words;

        List<ProcedureDivisionByReference> procedureDivisionByReference;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDivisionByReferencePhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDivisionByValuePhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        List<Name> phrases;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDivisionByValuePhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDivisionGivingClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDivisionGivingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureDivisionUsingClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        List<Cobol> procedureDivisionUsingParameter;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureDivisionUsingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureName implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name paragraphName;

        @Nullable
        InSection inSection;

        @Nullable
        Name sectionName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureName(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureSection implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        ProcedureSectionHeader procedureSectionHeader;
        CobolWord dot;
        Paragraphs paragraphs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProcedureSectionHeader implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name sectionName;
        CobolWord section;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProcedureSectionHeader(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProgramIdParagraph implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord programId;
        CobolWord dot;
        Name programName;

        @Nullable
        List<CobolWord> programAttributes;

        @Nullable
        CobolWord dot2;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProgramIdParagraph(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ProgramLibrarySection implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        List<Cobol> libraryDescriptionEntries;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProgramLibrarySection(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class ProgramUnit implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        IdentificationDivision identificationDivision;

        @Getter
        @Nullable
        @With
        EnvironmentDivision environmentDivision;

        @Getter
        @Nullable
        @With
        DataDivision dataDivision;

        @Getter
        @Nullable
        @With
        ProcedureDivision procedureDivision;

        @Getter
        @Nullable
        @With
        List<ProgramUnit> programUnits;

        @Nullable
        CobolRightPadded<EndProgram> endProgram;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitProgramUnit(this, p);
        }

        @Nullable
        public Cobol.EndProgram getEndProgram() {
            return endProgram == null ? null : endProgram.getElement();
        }

        public ProgramUnit withEndProgram(@Nullable Cobol.EndProgram endProgram) {
            if (endProgram == null) {
                return this.endProgram == null ? this : new ProgramUnit(id, prefix, markers, identificationDivision, environmentDivision, dataDivision, procedureDivision, programUnits, null);
            }
            return getPadding().withEndProgram(CobolRightPadded.withElement(this.endProgram, endProgram));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final ProgramUnit t;

            @Nullable
            public CobolRightPadded<Cobol.EndProgram> getEndProgram() {
                return t.endProgram;
            }

            public ProgramUnit withEndProgram(@Nullable CobolRightPadded<Cobol.EndProgram> endProgram) {
                return t.endProgram == endProgram ? t : new ProgramUnit(t.padding, t.id, t.prefix, t.markers, t.identificationDivision, t.environmentDivision, t.dataDivision, t.procedureDivision, t.programUnits, endProgram);
            }
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class Purge implements Statement {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        CobolContainer<Name> names;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitPurge(this, p);
        }

        public List<Name> getNames() {
            return names.getElements();
        }

        public Purge withNames(List<Name> names) {
            return getPadding().withNames(this.names.getPadding().withElements(CobolRightPadded.withElements(
                    this.names.getPadding().getElements(), names)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final Purge t;

            public CobolContainer<Name> getNames() {
                return t.names;
            }

            public Purge withNames(CobolContainer<Name> names) {
                return t.names == names ? t : new Purge(t.padding, t.id, t.prefix, t.markers, names);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class QualifiedDataName implements Cobol, Identifier {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Cobol dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitQualifiedDataName(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class QualifiedDataNameFormat1 implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;

        @Nullable
        List<Cobol> qualifiedInData;

        @Nullable
        InFile inFile;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitQualifiedDataNameFormat1(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class QualifiedDataNameFormat2 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name paragraphName;
        InSection inSection;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitQualifiedDataNameFormat2(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class QualifiedDataNameFormat3 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name textName;
        InLibrary inLibrary;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitQualifiedDataNameFormat3(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class QualifiedDataNameFormat4 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord linageCounter;
        InFile inFile;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitQualifiedDataNameFormat4(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class QualifiedInData implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Cobol in;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitQualifiedInData(this, p);
        }
    }


    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Read implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name fileName;
        List<CobolWord> nextRecord;

        @Nullable
        ReadInto readInto;

        @Nullable
        ReadWith readWith;

        @Nullable
        ReadKey readKey;

        @Nullable
        StatementPhrase invalidKeyPhrase;

        @Nullable
        StatementPhrase notInvalidKeyPhrase;

        @Nullable
        StatementPhrase atEndPhrase;

        @Nullable
        StatementPhrase notAtEndPhrase;

        CobolWord endRead;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRead(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReadInto implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReadInto(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReadKey implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReadKey(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReadWith implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReadWith(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Receivable implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReceivable(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Receive implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord receive;
        Cobol fromOrInto;

        @Nullable
        StatementPhrase onExceptionClause;

        @Nullable
        StatementPhrase notOnExceptionClause;

        CobolWord endReceive;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReceive(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReceiveFrom implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        CobolWord dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReceiveFrom(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class ReceiveFromStatement implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord dataName;

        @Getter
        @With
        CobolWord from;

        @Getter
        @With
        ReceiveFrom receiveFrom;

        CobolContainer<Cobol> beforeWithThreadSizeStatus;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReceiveFromStatement(this, p);
        }

        public List<Cobol> getBeforeWithThreadSizeStatus() {
            return beforeWithThreadSizeStatus.getElements();
        }

        public ReceiveFromStatement withBeforeWithThreadSizeStatus(List<Cobol> beforeWithThreadSizeStatus) {
            return getPadding().withBeforeWithThreadSizeStatus(this.beforeWithThreadSizeStatus.getPadding().withElements(CobolRightPadded.withElements(
                    this.beforeWithThreadSizeStatus.getPadding().getElements(), beforeWithThreadSizeStatus)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final ReceiveFromStatement t;

            public CobolContainer<Cobol> getBeforeWithThreadSizeStatus() {
                return t.beforeWithThreadSizeStatus;
            }

            public ReceiveFromStatement withBeforeWithThreadSizeStatus(CobolContainer<Cobol> beforeWithThreadSizeStatus) {
                return t.beforeWithThreadSizeStatus == beforeWithThreadSizeStatus ? t : new ReceiveFromStatement(t.padding, t.id, t.prefix, t.markers, t.dataName, t.from, t.receiveFrom, beforeWithThreadSizeStatus);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReceiveIntoStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord cdName;
        List<CobolWord> words;
        Identifier identifier;

        @Nullable
        StatementPhrase receiveNoData;

        @Nullable
        StatementPhrase receiveWithData;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReceiveIntoStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RecordContainsClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord record;
        Cobol clause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRecordContainsClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RecordContainsClauseFormat1 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        CobolWord contains;

        CobolWord integerLiteral;

        @Nullable
        CobolWord characters;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRecordContainsClauseFormat1(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RecordContainsClauseFormat2 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Cobol> fromClause;
        List<Cobol> qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRecordContainsClauseFormat2(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RecordContainsClauseFormat3 implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        CobolWord contains;

        CobolWord integerLiteral;
        RecordContainsTo recordContainsTo;


        @Nullable
        CobolWord characters;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRecordContainsClauseFormat3(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RecordContainsTo implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord to;
        CobolWord integerLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRecordContainsTo(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RecordDelimiterClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRecordDelimiterClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RecordingModeClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord mode;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRecordingModeClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RecordKeyClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> recordWords;
        QualifiedDataName qualifiedDataName;

        @Nullable
        PasswordClause passwordClause;

        List<CobolWord> duplicates;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRecordKeyClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReferenceModifier implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord leftParen;
        ArithmeticExpression characterPosition;
        CobolWord colon;

        @Nullable
        ArithmeticExpression length;

        CobolWord rightParen;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReferenceModifier(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RelationalOperator implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRelationalOperator(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RelationArithmeticComparison implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        ArithmeticExpression arithmeticExpressionA;
        RelationalOperator relationalOperator;
        ArithmeticExpression arithmeticExpressionB;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRelationArithmeticComparison(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RelationCombinedComparison implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        ArithmeticExpression arithmeticExpression;
        RelationalOperator relationalOperator;
        Parenthesized combinedCondition;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRelationCombinedComparison(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RelationCombinedCondition implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Cobol> relationalArithmeticExpressions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRelationCombinedCondition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RelationSignCondition implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        ArithmeticExpression arithmeticExpression;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRelationSignCondition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RelativeKeyClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRelativeKeyClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Release implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord release;
        QualifiedDataName recordName;

        @Nullable
        CobolWord from;

        @Nullable
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRelease(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Name> reportName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportDescription implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        ReportDescriptionEntry reportDescriptionEntry;
        List<Cobol> groupDescriptionEntries;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportDescription(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportDescriptionEntry implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord rd;
        QualifiedDataName qualifiedDataName;

        @Nullable
        ReportDescriptionGlobalClause reportDescriptionGlobalClause;

        @Nullable
        ReportDescriptionGlobalClause reportDescriptionPageLimitClause;

        @Nullable
        ReportDescriptionGlobalClause reportDescriptionHeadingClause;

        @Nullable
        ReportDescriptionGlobalClause reportDescriptionFirstDetailClause;

        @Nullable
        ReportDescriptionGlobalClause reportDescriptionLastDetailClause;

        @Nullable
        ReportDescriptionGlobalClause reportDescriptionFootingClause;

        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportDescriptionEntry(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportDescriptionFirstDetailClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportDescriptionFirstDetailClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportDescriptionFootingClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportDescriptionFootingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportDescriptionGlobalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportDescriptionGlobalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportDescriptionHeadingClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportDescriptionHeadingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportDescriptionLastDetailClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportDescriptionLastDetailClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportDescriptionPageLimitClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> firstWords;
        Name integerLiteral;

        @Nullable
        CobolWord secondWords;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportDescriptionPageLimitClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupBlankWhenZeroClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupBlankWhenZeroClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupColumnNumberClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupColumnNumberClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupDescriptionEntryFormat1 implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord integerLiteral;
        CobolWord dataName;

        @Nullable
        ReportGroupLineNumberClause groupLineNumberClause;

        @Nullable
        ReportGroupNextGroupClause groupNextGroupClause;

        ReportGroupTypeClause groupTypeClause;

        @Nullable
        ReportGroupUsageClause groupUsageClause;

        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupDescriptionEntryFormat1(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupDescriptionEntryFormat2 implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord integerLiteral;

        @Nullable
        CobolWord dataName;

        @Nullable
        ReportGroupLineNumberClause reportGroupLineNumberClause;

        ReportGroupUsageClause groupUsageClause;

        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupDescriptionEntryFormat2(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class ReportGroupDescriptionEntryFormat3 implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        CobolWord integerLiteral;

        @Getter
        @Nullable
        @With
        CobolWord dataName;

        @Nullable
        CobolContainer<Cobol> clauses;

        @Getter
        @With
        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupDescriptionEntryFormat3(this, p);
        }

        public List<Cobol> getClauses() {
            return clauses.getElements();
        }

        public ReportGroupDescriptionEntryFormat3 withClauses(List<Cobol> clauses) {
            return getPadding().withClauses(this.clauses.getPadding().withElements(CobolRightPadded.withElements(
                    this.clauses.getPadding().getElements(), clauses)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final ReportGroupDescriptionEntryFormat3 t;

            @Nullable
            public CobolContainer<Cobol> getClauses() {
                return t.clauses;
            }

            public ReportGroupDescriptionEntryFormat3 withClauses(@Nullable CobolContainer<Cobol> clauses) {
                return t.clauses == clauses ? t : new ReportGroupDescriptionEntryFormat3(t.padding, t.id, t.prefix, t.markers, t.integerLiteral, t.dataName, clauses, t.dot);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupIndicateClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupIndicateClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupJustifiedClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupJustifiedClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupLineNumberClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Cobol clause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupLineNumberClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupLineNumberNextPage implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord integerLiteral;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupLineNumberNextPage(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupLineNumberPlus implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord plus;
        CobolWord integerLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupLineNumberPlus(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupNextGroupClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Cobol clause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupNextGroupClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupNextGroupNextPage implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> nextPage;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupNextGroupNextPage(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupNextGroupPlus implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord plus;
        CobolWord integerLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupNextGroupPlus(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupPictureClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        PictureString pictureString;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupPictureClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupResetClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupResetClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupSignClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupSignClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupSourceClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupSourceClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupSumClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        List<Cobol> identifiers;

        @Nullable
        CobolWord upon;

        List<Cobol> dataNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupSumClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupTypeClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Cobol type;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupTypeClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupTypeControlFooting implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupTypeControlFooting(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupTypeControlHeading implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        Name dataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupTypeControlHeading(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupTypeDetail implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupTypeDetail(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupTypePageFooting implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupTypePageFooting(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupTypePageHeading implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupTypePageHeading(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupTypeReportFooting implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupTypeReportFooting(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupTypeReportHeading implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupTypeReportHeading(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupUsageClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupUsageClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportGroupValueClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name literal;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportGroupValueClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportName implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportName(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReportSection implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<Cobol> descriptions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReportSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RerunClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord rerun;

        @Nullable
        CobolWord on;

        @Nullable
        CobolWord name;

        CobolWord every;

        Cobol action;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRerunClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RerunEveryClock implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord integerLiteral;

        @Nullable
        CobolWord clockUnits;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRerunEveryClock(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RerunEveryOf implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> records;
        CobolWord fileName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRerunEveryOf(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RerunEveryRecords implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord integerLiteral;
        CobolWord records;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRerunEveryRecords(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReserveClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Cobol> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReserveClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReserveNetworkClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReserveNetworkClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Return implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        Name fileName;

        @Nullable
        CobolWord record;

        @Nullable
        ReturnInto into;

        StatementPhrase atEndPhrase;

        @Nullable
        StatementPhrase notAtEndPhrase;

        @Nullable
        CobolWord endReturn;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReturn(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ReturnInto implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord into;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitReturnInto(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Rewrite implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord rewrite;

        @Nullable
        QualifiedDataName recordName;

        @Nullable
        StatementPhrase invalidKeyPhrase;

        @Nullable
        StatementPhrase notInvalidKeyPhrase;

        @Nullable
        CobolWord endRewrite;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRewrite(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class RewriteFrom implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord from;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRewriteFrom(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Roundable implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Identifier identifier;

        @Nullable
        CobolWord rounded;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitRoundable(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SameClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<CobolWord> fileNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSameClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionAutoClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord auto;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionAutoClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionBackgroundColorClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord background;

        CobolWord is;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionBackgroundColorClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionBellClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord bell;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionBellClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionBlankClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionBlankClause(this, p);
        }
    }


    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionBlankWhenZeroClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionBlankWhenZeroClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionBlinkClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord blink;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionBlinkClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionColumnClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionColumnClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionControlClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionControlClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionEntry implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;

        @Nullable
        CobolWord name;

        List<Cobol> clauses;
        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionEntry(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionEraseClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionEraseClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionForegroundColorClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionForegroundColorClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionFromClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord from;
        Name value;

        @Nullable
        ScreenDescriptionToClause screenDescriptionToClause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionFromClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionFullClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionFullClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionGridClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionGridClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionJustifiedClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionJustifiedClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionLightClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord light;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionLightClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionLineClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionLineClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionPictureClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        PictureString pictureString;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionPictureClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionPromptClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Nullable
        ScreenDescriptionPromptOccursClause screenDescriptionPromptOccursClause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionPromptClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionPromptOccursClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord occurs;
        CobolWord integer;

        @Nullable
        CobolWord times;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionPromptOccursClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionRequiredClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord required;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionRequiredClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionReverseVideoClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionReverseVideoClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionSecureClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionSecureClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionSignClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionSignClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionSizeClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionSizeClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionToClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord to;
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionToClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionUnderlineClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord underline;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionUnderlineClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionUsageClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionUsageClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionUsingClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord using;
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionUsingClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionValueClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionValueClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenDescriptionZeroFillClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenDescriptionZeroFillClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ScreenSection implements DataDivisionSection {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;
        List<ScreenDescriptionEntry> descriptions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitScreenSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Search implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        QualifiedDataName qualifiedDataName;

        @Nullable
        SearchVarying searchVarying;

        @Nullable
        StatementPhrase atEndPhrase;

        List<SearchWhen> searchWhen;

        @Nullable
        CobolWord endSearch;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSearch(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SearchVarying implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord varying;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSearchVarying(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SearchWhen implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord when;
        Condition condition;
        List<CobolWord> nextSentence;
        List<Statement> statements;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSearchWhen(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SelectClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord fileName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSelectClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Send implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord send;
        Cobol statement;

        @Nullable
        StatementPhrase onExceptionClause;

        @Nullable
        StatementPhrase notOnExceptionClause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSend(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SendAdvancingLines implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;
        CobolWord lines;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSendAdvancingLines(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SendPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        Cobol target;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSendPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SendStatementSync implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;

        @Nullable
        SendPhrase sendFromPhrase;

        @Nullable
        SendPhrase sendWithPhrase;

        @Nullable
        SendPhrase sendReplacingPhrase;

        @Nullable
        SendPhrase sendAdvancingPhrase;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSendStatementSync(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Sentence implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Statement> statements;
        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSentence(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Set implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord set;

        @Nullable
        List<SetTo> to;

        @Nullable
        SetUpDown upDown;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSet(this, p);
        }
    }

    @ToString
    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class SetTo implements Cobol {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @Getter
        @EqualsAndHashCode.Include
        @With
        UUID id;

        @Getter
        @With
        Space prefix;

        @Getter
        @With
        Markers markers;

        @Getter
        @With
        List<Identifier> to;

        CobolContainer<Name> values;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSetTo(this, p);
        }

        public List<Name> getValues() {
            return values.getElements();
        }

        public SetTo withValues(List<Name> values) {
            return getPadding().withValues(this.values.getPadding().withElements(CobolRightPadded.withElements(
                    this.values.getPadding().getElements(), values)));
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final SetTo t;

            public CobolContainer<Name> getValues() {
                return t.values;
            }

            public SetTo withValues(CobolContainer<Name> values) {
                return t.values == values ? t : new SetTo(t.padding, t.id, t.prefix, t.markers, t.to, values);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SetUpDown implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Identifier> to;
        List<CobolWord> operation;
        Name value;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSetUpDown(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Sort implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord sort;
        CobolWord fileName;
        List<Sortable> sortOnKeyClause;

        @Nullable
        CobolWord sortDuplicatesPhrase;

        SortCollatingSequencePhrase sortCollatingSequencePhrase;
        SortProcedurePhrase sortInputProcedurePhrase;
        List<Sortable> sortUsing;

        @Nullable
        SortProcedurePhrase sortOutputProcedurePhrase;

        List<Sortable> sortGiving;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSort(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Sortable implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<CobolWord> names;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSortable(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SortCollatingSequencePhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<CobolWord> alphabetNames;

        @Nullable
        Sortable sortCollatingAlphanumeric;

        @Nullable
        Sortable sortCollatingNational;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSortCollatingSequencePhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SortGiving implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord fileName;
        List<CobolWord> words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSortGiving(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SortProcedurePhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord procedureName;

        @Nullable
        Sortable sortInputThrough;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSortProcedurePhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SourceComputer implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;

        @Nullable
        SourceComputerDefinition computer;

        @Nullable
        CobolWord dot;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSourceComputer(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SourceComputerDefinition implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord computerName;

        @Nullable
        List<CobolWord> debuggingMode;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSourceComputerDefinition(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SpecialNames implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord words;
        CobolWord dot;

        @Nullable
        List<Cobol> clauses;

        @Nullable
        CobolWord dot2;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSpecialNames(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Start implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord start;
        CobolWord fileName;

        @Nullable
        StartKey startKey;

        @Nullable
        StatementPhrase invalidKeyPhrase;

        @Nullable
        StatementPhrase notInvalidKeyPhrase;

        @Nullable
        CobolWord endStart;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStart(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StartKey implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStartKey(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StatementPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> phrase;
        List<Statement> statement;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStatementPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StatusKeyClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStatusKeyClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Stop implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Cobol statement;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStop(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StringDelimitedByPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> word;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStringDelimitedByPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StopStatementGiving implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStopStatementGiving(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StringForPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord word;
        Name identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStringForPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StringIntoPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord into;
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStringIntoPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StringSendingPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Cobol> sendings;
        Cobol phrase;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStringSendingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StringStatement implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord string;
        List<Cobol> stringSendingPhrases;
        StringIntoPhrase stringIntoPhrase;

        @Nullable
        StringWithPointerPhrase stringWithPointerPhrase;

        @Nullable
        StatementPhrase onOverflowPhrase;

        @Nullable
        StatementPhrase notOnOverflowPhrase;

        @Nullable
        CobolWord endString;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStringStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class StringWithPointerPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitStringWithPointerPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Subscript implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        Cobol first;

        @Nullable
        CobolWord integerLiteral;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSubscript(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Subtract implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord substract;
        Cobol operation;

        @Nullable
        StatementPhrase onSizeErrorPhrase;

        @Nullable
        StatementPhrase notOnSizeErrorPhrase;

        @Nullable
        CobolWord endSubtract;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSubtract(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SubtractCorrespondingStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord corresponding;
        QualifiedDataName qualifiedDataName;
        CobolWord giving;
        SubtractMinuendCorresponding subtractMinuendCorresponding;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSubtractCorrespondingStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SubtractFromGivingStatement implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Name> subtractSubtrahend;
        CobolWord from;
        Name subtractMinuendGiving;
        CobolWord giving;
        List<Roundable> subtractGiving;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSubtractFromGivingStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SubtractFromStatement implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<Name> subtractSubtrahend;
        CobolWord from;
        List<Roundable> subtractMinuend;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSubtractFromStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SubtractMinuendCorresponding implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        QualifiedDataName qualifiedDataName;

        @Nullable
        CobolWord rounded;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSubtractMinuendCorresponding(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SymbolicCharacter implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> symbols;

        @Nullable
        CobolWord words;

        List<CobolWord> literals;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSymbolicCharacter(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SymbolicCharactersClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<SymbolicCharacter> symbols;

        @Nullable
        CobolWord inAlphabet;

        @Nullable
        Identifier alphabetName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSymbolicCharactersClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SymbolicDestinationClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSymbolicDestinationClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SymbolicQueueClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSymbolicQueueClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SymbolicSourceClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSymbolicSourceClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SymbolicSubQueueClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSymbolicSubQueueClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class SymbolicTerminalClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitSymbolicTerminalClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class TableCall implements Identifier {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        QualifiedDataName qualifiedDataName;
        List<Parenthesized> subscripts;

        @Nullable
        ReferenceModifier referenceModifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitTableCall(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Terminate implements Statement {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord terminate;
        QualifiedDataName reportName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitTerminate(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class TextLengthClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dataDescName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitTextLengthClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnString implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord unstring;
        UnstringSendingPhrase unstringSendingPhrase;
        UnstringIntoPhrase unstringIntoPhrase;

        @Nullable
        UnstringWithPointerPhrase unstringWithPointerPhrase;

        @Nullable
        UnstringTallyingPhrase unstringTallyingPhrase;

        @Nullable
        StatementPhrase onOverflowPhrase;

        @Nullable
        StatementPhrase notOnOverflowPhrase;

        @Nullable
        CobolWord endUnstring;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnString(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringCountIn implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringCountIn(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringDelimitedByPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringDelimitedByPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringDelimiterIn implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Identifier identifier;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringDelimiterIn(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringInto implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Identifier identifier;

        @Nullable
        UnstringDelimiterIn unstringDelimiterIn;

        @Nullable
        UnstringCountIn unstringCountIn;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringInto(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringIntoPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord into;
        List<UnstringInto> unstringIntos;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringIntoPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringOrAllPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringOrAllPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringSendingPhrase implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Identifier identifier;

        @Nullable
        UnstringDelimitedByPhrase unstringDelimitedByPhrase;

        @Nullable
        List<UnstringOrAllPhrase> unstringOrAllPhrases;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringSendingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringTallyingPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringTallyingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UnstringWithPointerPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        QualifiedDataName qualifiedDataName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUnstringWithPointerPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UseAfterClause implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        UseAfterOn useAfterOn;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUseAfterClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UseAfterOn implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        List<CobolWord> afterOn;

        @Nullable
        List<Name> fileNames;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUseAfterOn(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UseDebugClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        List<UseDebugOn> useDebugs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUseDebugClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UseDebugOn implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        List<CobolWord> words;

        @Nullable
        Name name;

        @Nullable
        ProcedureName procedureName;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUseDebugOn(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class UseStatement implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord use;
        Cobol clause;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitUseStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ValuedObjectComputerClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Type type;
        List<CobolWord> words;

        @Nullable
        Cobol value;

        @Nullable
        CobolWord units;

        public enum Type {
            Memory,
            Disk,
            SegmentLimit,
            CharacterSet
        }

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitValuedObjectComputerClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ValueOfClause implements Cobol {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> valueOf;
        List<ValuePair> valuePairs;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitValueOfClause(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class ValuePair implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord systemName;

        @Nullable
        CobolWord is;

        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitValuePair(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class WorkingStorageSection implements DataDivisionSection {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        CobolWord dot;
        List<DataDescriptionEntry> dataDescriptions;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitWorkingStorageSection(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Write implements Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord write;
        QualifiedDataName recordName;

        @Nullable
        WriteFromPhrase writeFromPhrase;

        @Nullable
        WriteAdvancingPhrase writeAdvancingPhrase;

        @Nullable
        StatementPhrase writeAtEndOfPagePhrase;

        @Nullable
        StatementPhrase writeNotAtEndOfPagePhrase;

        @Nullable
        StatementPhrase invalidKeyPhrase;

        @Nullable
        StatementPhrase notInvalidKeyPhrase;

        @Nullable
        CobolWord endWrite;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitWrite(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class WriteAdvancingLines implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;
        CobolWord words;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitWriteAdvancingLines(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class WriteAdvancingMnemonic implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitWriteAdvancingMnemonic(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class WriteAdvancingPage implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord page;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitWriteAdvancingPage(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class WriteAdvancingPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        List<CobolWord> words;
        Cobol writeBy;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitWriteAdvancingPhrase(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class WriteFromPhrase implements Cobol {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;
        CobolWord from;
        Name name;

        @Override
        public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {
            return v.visitWriteFromPhrase(this, p);
        }
    }
}
