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

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.openrewrite.FileAttributes;
import org.openrewrite.cobol.internal.grammar.CobolBaseVisitor;
import org.openrewrite.cobol.internal.grammar.CobolParser;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Collections.*;
import static org.openrewrite.Tree.randomId;
import static org.openrewrite.cobol.tree.Space.format;
import static org.openrewrite.internal.StringUtils.indexOfNextNonWhitespace;

public class CobolParserVisitor extends CobolBaseVisitor<Object> {
    private final Path path;

    @Nullable
    private final FileAttributes fileAttributes;

    private final String source;
    private final Charset charset;
    private final boolean charsetBomMarked;

    private int cursor = 0;

    public CobolParserVisitor(Path path, @Nullable FileAttributes fileAttributes,
                              String source, Charset charset, boolean charsetBomMarked) {
        this.path = path;
        this.fileAttributes = fileAttributes;
        this.source = source;
        this.charset = charset;
        this.charsetBomMarked = charsetBomMarked;
    }

    public <T> T visit(@Nullable ParseTree... trees) {
        for (ParseTree tree : trees) {
            if (tree != null) {
                //noinspection unchecked
                return (T) visit(tree);
            }
        }
        throw new IllegalStateException("Expected one of the supplied trees to be non-null");
    }

    public <T> T visitNullable(@Nullable ParseTree tree) {
        if (tree == null) {
            //noinspection ConstantConditions
            return null;
        }
        //noinspection unchecked
        return (T) super.visit(tree);
    }

    @Override
    public Cobol.CompilationUnit visitStartRule(CobolParser.StartRuleContext ctx) {
        return visitCompilationUnit(ctx.compilationUnit());
    }

    @Override
    public Cobol.CompilationUnit visitCompilationUnit(CobolParser.CompilationUnitContext ctx) {
        Space prefix = whitespace();
        List<CobolRightPadded<Cobol.ProgramUnit>> programUnits = new ArrayList<>(ctx.programUnit().size());
        for (CobolParser.ProgramUnitContext pu : ctx.programUnit()) {
            programUnits.add(CobolRightPadded.build(visitProgramUnit(pu)));
        }
        return new Cobol.CompilationUnit(
                randomId(),
                path,
                fileAttributes,
                prefix,
                Markers.EMPTY,
                charset.name(),
                charsetBomMarked,
                null,
                programUnits,
                source.substring(cursor)
        );
    }

    @Override
    public Object visitAbbreviation(CobolParser.AbbreviationContext ctx) {
        return new Cobol.Abbreviation(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.NOT()),
                visitNullable(ctx.relationalOperator()),
                visitNullable(ctx.LPARENCHAR()),
                (Cobol) visit(ctx.arithmeticExpression()),
                visitNullable(ctx.abbreviation()),
                visitNullable(ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitAcceptFromDateStatement(CobolParser.AcceptFromDateStatementContext ctx) {
        return new Cobol.AcceptFromDateStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FROM(), ctx.DATE(), ctx.YYYYMMDD(), ctx.DAY(), ctx.YYYYDDD(), ctx.DAY_OF_WEEK(), ctx.TIME(), ctx.TIMER(), ctx.TODAYS_DATE(), ctx.MMDDYYYY(), ctx.TODAYS_NAME(), ctx.YEAR())
        );
    }

    @Override
    public Object visitAcceptFromEscapeKeyStatement(CobolParser.AcceptFromEscapeKeyStatementContext ctx) {
        return new Cobol.AcceptFromEscapeKeyStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FROM(), ctx.ESCAPE(), ctx.KEY())
        );
    }

    @Override
    public Object visitAcceptFromMnemonicStatement(CobolParser.AcceptFromMnemonicStatementContext ctx) {
        return new Cobol.AcceptFromMnemonicStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FROM()),
                (Identifier) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Object visitAcceptMessageCountStatement(CobolParser.AcceptMessageCountStatementContext ctx) {
        return new Cobol.AcceptMessageCountStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.MESSAGE(), ctx.COUNT())
        );
    }

    @Override
    public Object visitAcceptStatement(CobolParser.AcceptStatementContext ctx) {
        return new Cobol.Accept(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ACCEPT()),
                (Identifier) visit(ctx.identifier()),
                ctx.acceptFromDateStatement() != null ? (Cobol) visit(ctx.acceptFromDateStatement()) :
                        ctx.acceptFromEscapeKeyStatement() != null ? (Cobol) visit(ctx.acceptFromEscapeKeyStatement()) :
                                ctx.acceptFromMnemonicStatement() != null ? (Cobol) visit(ctx.acceptFromMnemonicStatement()) :
                                        ctx.acceptMessageCountStatement() != null ? (Cobol) visit(ctx.acceptMessageCountStatement()) : null,
                visitNullable(ctx.onExceptionClause()),
                visitNullable(ctx.notOnExceptionClause()),
                visitNullable(ctx.END_ACCEPT())
        );
    }

    @Override
    public Object visitAccessModeClause(CobolParser.AccessModeClauseContext ctx) {
        return new Cobol.AccessModeClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ACCESS(), ctx.MODE()),
                visit(ctx.IS(), ctx.SEQUENTIAL(), ctx.RANDOM(), ctx.DYNAMIC(), ctx.EXCLUSIVE())
        );
    }

    @Override
    public Cobol.Add visitAddStatement(CobolParser.AddStatementContext ctx) {
        return new Cobol.Add(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ADD()),
                visit(ctx.addToStatement(), ctx.addToGivingStatement(), ctx.addCorrespondingStatement()),
                ctx.onSizeErrorPhrase() == null ? visitNullable(ctx.notOnSizeErrorPhrase()) :
                        visitNullable(ctx.onSizeErrorPhrase()),
                visitNullable(ctx.END_ADD())
        );
    }

    @Override
    public Cobol.AddTo visitAddToGivingStatement(CobolParser.AddToGivingStatementContext ctx) {
        return new Cobol.AddTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllContainer(ctx.addFrom()),
                convertAllContainer(padLeft(ctx.TO()), ctx.addToGiving()),
                convertAllContainer(padLeft(ctx.GIVING()), ctx.addGiving())
        );
    }

    @Override
    public Cobol.AddTo visitAddToStatement(CobolParser.AddToStatementContext ctx) {
        return new Cobol.AddTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllContainer(ctx.addFrom()),
                convertAllContainer(padLeft(ctx.TO()), ctx.addTo()),
                null
        );
    }

    @Override
    public Cobol.AlphabetAlso visitAlphabetAlso(CobolParser.AlphabetAlsoContext ctx) {
        return new Cobol.AlphabetAlso(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ALSO()),
                convertAllContainer(ctx.literal())
        );
    }

    @Override
    public Cobol.AlphabetClause visitAlphabetClauseFormat1(CobolParser.AlphabetClauseFormat1Context ctx) {
        return new Cobol.AlphabetClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ALPHABET()),
                (Name) visit(ctx.alphabetName()),
                convertAllList(emptyList(), singletonList(ctx.FOR()), singletonList(ctx.ALPHANUMERIC()), singletonList(ctx.IS()),
                        singletonList(ctx.EBCDIC()), singletonList(ctx.ASCII()), singletonList(ctx.STANDARD_1()),
                        singletonList(ctx.STANDARD_2()), singletonList(ctx.NATIVE()),
                        singletonList(ctx.cobolWord()), ctx.alphabetLiterals())
        );
    }

    @Override
    public Cobol.AlphabetClause visitAlphabetClauseFormat2(CobolParser.AlphabetClauseFormat2Context ctx) {
        return new Cobol.AlphabetClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ALPHABET()),
                (Name) visit(ctx.alphabetName()),
                convertAllList(emptyList(), singletonList(ctx.FOR()), singletonList(ctx.NATIONAL()), singletonList(ctx.IS()),
                        singletonList(ctx.NATIVE()), singletonList(ctx.CCSVERSION()), singletonList(ctx.literal()))
        );
    }

    @Override
    public Cobol.AlphabetLiteral visitAlphabetLiterals(CobolParser.AlphabetLiteralsContext ctx) {
        return new Cobol.AlphabetLiteral(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Literal) visit(ctx.literal()),
                visitNullable(ctx.alphabetThrough()),
                ctx.alphabetAlso() == null ? null : convertAllContainer(ctx.alphabetAlso())
        );
    }

    @Override
    public Cobol.AlphabetThrough visitAlphabetThrough(CobolParser.AlphabetThroughContext ctx) {
        return new Cobol.AlphabetThrough(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.THROUGH(), ctx.THRU()),
                (Literal) visit(ctx.literal())
        );
    }

    @Override
    public Cobol.AlterProceedTo visitAlterProceedTo(CobolParser.AlterProceedToContext ctx) {
        return new Cobol.AlterProceedTo(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.ProcedureName) visit(ctx.procedureName(0)),
                words(ctx.TO(0), ctx.PROCEED(), ctx.PROCEED() != null ? ctx.TO(1) : null),
                (Cobol.ProcedureName) visit(ctx.procedureName(1))
        );
    }

    @Override
    public Cobol.AlterStatement visitAlterStatement(CobolParser.AlterStatementContext ctx) {
        return new Cobol.AlterStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ALTER()),
                convertAll(ctx.alterProceedTo())
        );
    }

    @Override
    public Object visitAlternateRecordKeyClause(CobolParser.AlternateRecordKeyClauseContext ctx) {
        return new Cobol.AlternateRecordKeyClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ALTERNATE(), ctx.RECORD(), ctx.KEY(), ctx.IS()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                visitNullable(ctx.passwordClause()),
                words(ctx.WITH(), ctx.DUPLICATES())
        );
    }

    @Override
    public Object visitAndOrCondition(CobolParser.AndOrConditionContext ctx) {
        return new Cobol.AndOrCondition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.AND(), ctx.OR()),
                (Cobol.CombinableCondition) visit(ctx.combinableCondition()),
                convertAllContainer(ctx.abbreviation())
        );
    }

    @Override
    public Object visitArgument(CobolParser.ArgumentContext ctx) {
        return new Cobol.Argument(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.literal(), ctx.identifier(), ctx.qualifiedDataName(), ctx.indexName(), ctx.arithmeticExpression()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitArithmeticExpression(CobolParser.ArithmeticExpressionContext ctx) {
        return new Cobol.ArithmeticExpression(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.MultDivs) visit(ctx.multDivs()),
                convertAllContainer(ctx.plusMinus())
        );
    }

    @Override
    public Object visitAssignClause(CobolParser.AssignClauseContext ctx) {
        return new Cobol.AssignClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ASSIGN(), ctx.TO(), ctx.DISK(), ctx.DISPLAY(), ctx.KEYBOARD(), ctx.PORT(), ctx.PRINTER(),
                        ctx.READER(), ctx.REMOTE(), ctx.TAPE(), ctx.VIRTUAL(), ctx.DYNAMIC(), ctx.EXTERNAL()),
                ctx.assignmentName() != null ? visitNullable(ctx.assignmentName()) :
                        ctx.literal() != null ? visitNullable(ctx.literal()) : null
        );
    }

    @Override
    public Cobol.StatementPhrase visitAtEndPhrase(CobolParser.AtEndPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.AT(), ctx.END()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitBasis(CobolParser.BasisContext ctx) {
        if (ctx.arithmeticExpression() != null) {
            return new Cobol.Parenthesized(
                    randomId(),
                    whitespace(),
                    Markers.EMPTY,
                    (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                    singletonList((Cobol) visit(ctx.arithmeticExpression())),
                    (Cobol.CobolWord) visit(ctx.RPARENCHAR())
            );
        } else {
            return visit(ctx.identifier(), ctx.literal());
        }
    }

    @Override
    public Object visitBlockContainsClause(CobolParser.BlockContainsClauseContext ctx) {
        return new Cobol.BlockContainsClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BLOCK(), ctx.CONTAINS()),
                visitNullable(ctx.integerLiteral()),
                visitNullable(ctx.blockContainsTo()),
                ctx.RECORDS() != null ? (Cobol.CobolWord) visit(ctx.RECORDS()) :
                        ctx.CHARACTERS() != null ? (Cobol.CobolWord)visit(ctx.CHARACTERS()) : null
        );
    }

    @Override
    public Object visitBlockContainsTo(CobolParser.BlockContainsToContext ctx) {
        return new Cobol.BlockContainsTo(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.TO()),
                (Cobol.CobolWord) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitCallByContent(CobolParser.CallByContentContext ctx) {
        return new Cobol.CallBy(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ADDRESS(), ctx.LENGTH(), ctx.OF(), ctx.OMITTED()),
                ctx.identifier() != null ? (Name) visit(ctx.identifier()) :
                        ctx.literal() != null ? (Name) visit(ctx.literal()) : null
        );
    }

    @Override
    public Object visitCallByContentPhrase(CobolParser.CallByContentPhraseContext ctx) {
        return new Cobol.CallPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BY(), ctx.CONTENT()),
                convertAllContainer(ctx.callByContent())
        );
    }

    @Override
    public Object visitCallByReference(CobolParser.CallByReferenceContext ctx) {
        return new Cobol.CallBy(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ADDRESS(), ctx.OF(), ctx.INTEGER(), ctx.STRING(), ctx.OMITTED()),
                ctx.identifier() != null ? (Name) visit(ctx.identifier()) :
                        ctx.literal() != null ? (Name) visit(ctx.literal()) :
                                ctx.fileName() != null ? (Name) visit(ctx.fileName()) : null
        );
    }

    @Override
    public Object visitCallByReferencePhrase(CobolParser.CallByReferencePhraseContext ctx) {
        return new Cobol.CallPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BY(), ctx.REFERENCE()),
                convertAllContainer(ctx.callByReference())
        );
    }

    @Override
    public Object visitCallByValue(CobolParser.CallByValueContext ctx) {
        return new Cobol.CallBy(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ADDRESS(), ctx.LENGTH(), ctx.OF()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitCallByValuePhrase(CobolParser.CallByValuePhraseContext ctx) {
        return new Cobol.CallPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BY(), ctx.VALUE()),
                convertAllContainer(ctx.callByValue())
        );
    }

    @Override
    public Object visitCallGivingPhrase(CobolParser.CallGivingPhraseContext ctx) {
        return new Cobol.CallGivingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.GIVING(), ctx.RETURNING()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitCallStatement(CobolParser.CallStatementContext ctx) {
        return new Cobol.Call(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.CALL()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.callUsingPhrase()),
                visitNullable(ctx.callGivingPhrase()),
                visitNullable(ctx.onOverflowPhrase()),
                visitNullable(ctx.onExceptionClause()),
                visitNullable(ctx.notOnExceptionClause()),
                visitNullable(ctx.END_CALL())
        );
    }

    @Override
    public Object visitCallUsingPhrase(CobolParser.CallUsingPhraseContext ctx) {
        return new Cobol.CallPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.USING()),
                convertAllContainer(ctx.callUsingParameter())
        );
    }

    @Override
    public Object visitCancelCall(CobolParser.CancelCallContext ctx) {
        return new Cobol.CancelCall(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.libraryName()),
                words(ctx.BYTITLE(), ctx.BYFUNCTION()),
                visitNullable(ctx.identifier()),
                visitNullable(ctx.literal())
        );
    }

    @Override
    public Object visitCancelStatement(CobolParser.CancelStatementContext ctx) {
        return new Cobol.Cancel(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CANCEL()),
                convertAllContainer(ctx.cancelCall())
        );
    }

    @Override
    public Cobol.ChannelClause visitChannelClause(CobolParser.ChannelClauseContext ctx) {
        return new Cobol.ChannelClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CHANNEL()),
                (Literal) visit(ctx.integerLiteral()),
                visitNullable(ctx.IS()),
                (Identifier) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Cobol.ValuedObjectComputerClause visitCharacterSetClause(CobolParser.CharacterSetClauseContext ctx) {
        return new Cobol.ValuedObjectComputerClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                Cobol.ValuedObjectComputerClause.Type.CharacterSet,
                words(ctx.CHARACTER(), ctx.SET(), ctx.DOT_FS()),
                null,
                null
        );
    }

    @Override
    public Object visitClassClause(CobolParser.ClassClauseContext ctx) {
        return new Cobol.ClassClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CLASS()),
                visitNullable(ctx.className()),
                words(ctx.FOR(), ctx.ALPHANUMERIC(), ctx.NATIONAL(), ctx.IS()),
                convertAllContainer(ctx.classClauseThrough())
        );
    }

    @Override
    public Object visitClassClauseThrough(CobolParser.ClassClauseThroughContext ctx) {
        return new Cobol.ClassClauseThrough(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.classClauseFrom()),
                ctx.THROUGH() != null ? (Cobol.CobolWord) visit(ctx.THROUGH()) :
                        ctx.THRU() != null ? (Cobol.CobolWord) visit(ctx.THRU()) : null,
                visitNullable(ctx.classClauseTo())
        );
    }

    @Override
    public Object visitClassCondition(CobolParser.ClassConditionContext ctx) {
        return new Cobol.ClassCondition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.identifier()),
                words(ctx.IS(), ctx.NOT()),
                visit(ctx.NUMERIC(), ctx.ALPHABETIC(), ctx.ALPHABETIC_LOWER(), ctx.ALPHABETIC_UPPER(), ctx.DBCS(), ctx.KANJI(), ctx.className())
        );
    }

    @Override
    public Object visitCloseFile(CobolParser.CloseFileContext ctx) {
        return new Cobol.CloseFile(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.fileName()),
                ctx.closeReelUnitStatement() != null || ctx.closeRelativeStatement() != null || ctx.closePortFileIOStatement() != null ?
                        visit(ctx.closeReelUnitStatement(), ctx.closeRelativeStatement(), ctx.closePortFileIOStatement()) : null
        );
    }

    @Override
    public Object visitClosePortFileIOStatement(CobolParser.ClosePortFileIOStatementContext ctx) {
        return new Cobol.ClosePortFileIOStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.NO(), ctx.WAIT(), ctx.USING()),
                convertAllContainer(ctx.closePortFileIOUsing())
        );
    }

    @Override
    public Object visitClosePortFileIOUsingAssociatedData(CobolParser.ClosePortFileIOUsingAssociatedDataContext ctx) {
        return new Cobol.ClosePortFileIOUsingAssociatedData(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ASSOCIATED_DATA()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitClosePortFileIOUsingAssociatedDataLength(CobolParser.ClosePortFileIOUsingAssociatedDataLengthContext ctx) {
        return new Cobol.ClosePortFileIOUsingAssociatedDataLength(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ASSOCIATED_DATA_LENGTH(), ctx.OF()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitClosePortFileIOUsingCloseDisposition(CobolParser.ClosePortFileIOUsingCloseDispositionContext ctx) {
        return new Cobol.ClosePortFileIOUsingCloseDisposition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.CLOSE_DISPOSITION(), ctx.OF(), ctx.ABORT(), ctx.ORDERLY())
        );
    }

    @Override
    public Object visitCloseReelUnitStatement(CobolParser.CloseReelUnitStatementContext ctx) {
        return new Cobol.CloseReelUnitStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.REEL(), ctx.UNIT(), ctx.FOR(), ctx.REMOVAL(), ctx.WITH(), ctx.NO(), ctx.REWIND(), ctx.LOCK())
        );
    }

    @Override
    public Object visitCloseRelativeStatement(CobolParser.CloseRelativeStatementContext ctx) {
        return new Cobol.CloseRelativeStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.NO(), ctx.REWIND(), ctx.LOCK())
        );
    }

    @Override
    public Object visitCloseStatement(CobolParser.CloseStatementContext ctx) {
        return new Cobol.Close(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CLOSE()),
                convertAllContainer(ctx.closeFile())
        );
    }

    @Override
    public Object visitCodeSetClause(CobolParser.CodeSetClauseContext ctx) {
        return new Cobol.CodeSetClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.CODE_SET(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.alphabetName())
        );
    }

    @Override
    public Cobol.CollatingSequenceClause visitCollatingSequenceClause(CobolParser.CollatingSequenceClauseContext ctx) {
        return new Cobol.CollatingSequenceClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PROGRAM(), ctx.COLLATING(), ctx.SEQUENCE()),
                convertAllContainer(padLeft(ctx.IS()), ctx.alphabetName()),
                visitNullable(ctx.collatingSequenceClauseAlphanumeric()),
                visitNullable(ctx.collatingSequenceClauseNational())
        );
    }

    @Override
    public Cobol.CollatingSequenceAlphabet visitCollatingSequenceClauseAlphanumeric(CobolParser.CollatingSequenceClauseAlphanumericContext ctx) {
        return new Cobol.CollatingSequenceAlphabet(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FOR(), ctx.ALPHANUMERIC(), ctx.IS()),
                (Identifier) visit(ctx.alphabetName())
        );
    }

    @Override
    public Object visitCollatingSequenceClauseNational(CobolParser.CollatingSequenceClauseNationalContext ctx) {
        return new Cobol.CollatingSequenceAlphabet(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FOR(), ctx.NATIONAL(), ctx.IS()),
                (Identifier) visit(ctx.alphabetName())
        );
    }

    @Override
    public Object visitCombinableCondition(CobolParser.CombinableConditionContext ctx) {
        return new Cobol.CombinableCondition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.NOT()),
                (Cobol) visit(ctx.simpleCondition())
        );
    }

    @Override
    public Object visitCommitmentControlClause(CobolParser.CommitmentControlClauseContext ctx) {
        return new Cobol.CommitmentControlClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.COMMITMENT(), ctx.CONTROL(), ctx.FOR()),
                (Cobol.CobolWord) visit(ctx.fileName())
        );
    }

    @Override
    public Object visitCommunicationDescriptionEntryFormat1(CobolParser.CommunicationDescriptionEntryFormat1Context ctx) {
        return new Cobol.CommunicationDescriptionEntryFormat1(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CD()),
                visitNullable(ctx.cdName()),
                words(ctx.FOR(), ctx.INITIAL(), ctx.INPUT()),
                convertAllContainer(ctx.symbolicQueueClause(),
                        ctx.symbolicSubQueueClause(),
                        ctx.messageDateClause(),
                        ctx.messageTimeClause(),
                        ctx.symbolicSourceClause(),
                        ctx.textLengthClause(),
                        ctx.endKeyClause(),
                        ctx.statusKeyClause(),
                        ctx.messageCountClause(),
                        ctx.dataDescName()).withLastSpace(sourceBefore("."))
        );
    }

    @Override
    public Object visitCommunicationDescriptionEntryFormat2(CobolParser.CommunicationDescriptionEntryFormat2Context ctx) {
        return new Cobol.CommunicationDescriptionEntryFormat2(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CD()),
                visitNullable(ctx.cdName()),
                words(ctx.FOR(), ctx.OUTPUT()),
                convertAllContainer(ctx.destinationCountClause(),
                        ctx.textLengthClause(),
                        ctx.statusKeyClause(),
                        ctx.destinationTableClause(),
                        ctx.errorKeyClause(),
                        ctx.symbolicDestinationClause()).withLastSpace(sourceBefore("."))
        );
    }

    @Override
    public Object visitCommunicationDescriptionEntryFormat3(CobolParser.CommunicationDescriptionEntryFormat3Context ctx) {
        return new Cobol.CommunicationDescriptionEntryFormat3(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CD()),
                visitNullable(ctx.cdName()),
                words(ctx.FOR(), ctx.INITIAL(), ctx.I_O()),
                convertAllContainer(ctx.messageDateClause(),
                        ctx.messageTimeClause(),
                        ctx.symbolicTerminalClause(),
                        ctx.textLengthClause(),
                        ctx.endKeyClause(),
                        ctx.statusKeyClause(),
                        ctx.dataDescName()).withLastSpace(sourceBefore("."))
        );
    }

    @Override
    public Object visitCommunicationSection(CobolParser.CommunicationSectionContext ctx) {
        return new Cobol.CommunicationSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.COMMUNICATION(), ctx.SECTION(), ctx.DOT_FS()),
                convertAllContainer(ctx.communicationDescriptionEntry(), ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Object visitComputeStatement(CobolParser.ComputeStatementContext ctx) {
        return new Cobol.Compute(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.COMPUTE()),
                convertAllContainer(ctx.computeStore()),
                words(ctx.EQUALCHAR(), ctx.EQUAL()),
                (Cobol.ArithmeticExpression) visit(ctx.arithmeticExpression()),
                visitNullable(ctx.onSizeErrorPhrase()),
                visitNullable(ctx.notOnSizeErrorPhrase()),
                visitNullable(ctx.END_COMPUTE())
        );
    }

    @Override
    public Object visitCondition(CobolParser.ConditionContext ctx) {
        return new Cobol.Condition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CombinableCondition) visit(ctx.combinableCondition()),
                convertAllContainer(ctx.andOrCondition())
        );
    }

    @Override
    public Cobol.ConditionNameReference visitConditionNameReference(CobolParser.ConditionNameReferenceContext ctx) {
        return new Cobol.ConditionNameReference(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.conditionName()),
                convertAllContainer(ctx.inData()),
                visitNullable(ctx.inFile()),
                convertAllContainer(ctx.conditionNameSubscriptReference()),
                convertAllContainer(ctx.inMnemonic())
        );
    }

    @Override
    public Cobol.ConditionNameSubscriptReference visitConditionNameSubscriptReference(CobolParser.ConditionNameSubscriptReferenceContext ctx) {
        return new Cobol.ConditionNameSubscriptReference(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                convertAllPrefixedList(singletonList(","), ctx.subscript()),
                (Cobol.CobolWord) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Cobol.ConfigurationSection visitConfigurationSection(CobolParser.ConfigurationSectionContext ctx) {
        return new Cobol.ConfigurationSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.CONFIGURATION(), ctx.SECTION()),
                convertAllContainer(sourceBefore("."), ctx.configurationSectionParagraph())
        );
    }

    @Override
    public Object visitContinueStatement(CobolParser.ContinueStatementContext ctx) {
        return new Cobol.Continue(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CONTINUE())
        );
    }

    @Override
    public Cobol.CurrencyClause visitCurrencySignClause(CobolParser.CurrencySignClauseContext ctx) {
        return new Cobol.CurrencyClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.CURRENCY(), ctx.SIGN(), ctx.IS()),
                (Literal) visit(ctx.literal(0)),
                ctx.literal().size() > 1 ? padLeft(whitespace(), words(ctx.WITH(), ctx.PICTURE(), ctx.SYMBOL())) : null,
                ctx.literal().size() > 1 ? (Literal) visit(ctx.literal(1)) : null
        );
    }

    @Override
    public Object visitDataAlignedClause(CobolParser.DataAlignedClauseContext ctx) {
        return new Cobol.DataAlignedClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ALIGNED())
        );
    }

    @Override
    public Object visitDataBaseSection(CobolParser.DataBaseSectionContext ctx) {
        return new Cobol.DataBaseSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DATA_BASE(), ctx.SECTION()),
                convertAllContainer(sourceBefore("."), ctx.dataBaseSectionEntry())
        );
    }

    @Override
    public Object visitDataBaseSectionEntry(CobolParser.DataBaseSectionEntryContext ctx) {
        return new Cobol.DataBaseSectionEntry(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                (Literal) visit(ctx.literal(0)),
                (Cobol.CobolWord) visit(ctx.INVOKE()),
                (Literal) visit(ctx.literal(1))
        );
    }

    @Override
    public Object visitDataBlankWhenZeroClause(CobolParser.DataBlankWhenZeroClauseContext ctx) {
        return new Cobol.DataBlankWhenZeroClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BLANK(), ctx.WHEN(), ctx.ZERO(), ctx.ZEROS(), ctx.ZEROES())
        );
    }

    @Override
    public Object visitDataCommonOwnLocalClause(CobolParser.DataCommonOwnLocalClauseContext ctx) {
        return new Cobol.DataCommonOwnLocalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.COMMON(), ctx.OWN(), ctx.LOCAL())
        );
    }

    @Override
    public Cobol.DataDescriptionEntry visitDataDescriptionEntryFormat1(CobolParser.DataDescriptionEntryFormat1Context ctx) {
        return new Cobol.DataDescriptionEntry(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.LEVEL_NUMBER_77(), ctx.INTEGERLITERAL()),
                ctx.dataName() != null ? (Cobol.CobolWord) visit(ctx.dataName()) :
                        ctx.FILLER() != null ? (Cobol.CobolWord) visit(ctx.FILLER()) : null,
                convertAllContainer(ctx.dataDescriptionEntryFormat1Clause()).withLastSpace(sourceBefore("."))
        );
    }

    @Override
    public Cobol.DataDivision visitDataDivision(CobolParser.DataDivisionContext ctx) {
        return new Cobol.DataDivision(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DATA(), ctx.DIVISION()),
                convertAllContainer(sourceBefore("."), ctx.dataDivisionSection())
        );
    }

    @Override
    public Object visitDataExternalClause(CobolParser.DataExternalClauseContext ctx) {
        return new Cobol.DataExternalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.EXTERNAL(), ctx.BY()),
                visitNullable(ctx.literal())
        );
    }

    @Override
    public Object visitDataGlobalClause(CobolParser.DataGlobalClauseContext ctx) {
        return new Cobol.DataGlobalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.GLOBAL())
        );
    }

    @Override
    public Object visitDataIntegerStringClause(CobolParser.DataIntegerStringClauseContext ctx) {
        return new Cobol.DataIntegerStringClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.INTEGER(), ctx.STRING())
        );
    }

    @Override
    public Object visitDataJustifiedClause(CobolParser.DataJustifiedClauseContext ctx) {
        return new Cobol.DataJustifiedClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.JUSTIFIED(), ctx.JUST(), ctx.RIGHT())
        );
    }

    @Override
    public Object visitDataOccursClause(CobolParser.DataOccursClauseContext ctx) {
        return new Cobol.DataOccursClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.OCCURS()),
                visit(ctx.identifier(), ctx.integerLiteral()),
                visitNullable(ctx.dataOccursTo()),
                visitNullable(ctx.TIMES()),
                visitNullable(ctx.dataOccursDepending()),
                convertAllContainer(ctx.dataOccursSort(), ctx.dataOccursIndexed())
        );
    }

    @Override
    public Object visitDataOccursDepending(CobolParser.DataOccursDependingContext ctx) {
        return new Cobol.DataOccursDepending(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DEPENDING(), ctx.ON()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitDataOccursIndexed(CobolParser.DataOccursIndexedContext ctx) {
        return new Cobol.DataOccursIndexed(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.INDEXED(), ctx.BY(), ctx.LOCAL()),
                convertAllContainer(ctx.indexName())
        );
    }

    @Override
    public Object visitDataOccursSort(CobolParser.DataOccursSortContext ctx) {
        return new Cobol.DataOccursSort(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ASCENDING(), ctx.DESCENDING(), ctx.KEY(), ctx.IS()),
                convertAllContainer(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitDataOccursTo(CobolParser.DataOccursToContext ctx) {
        return new Cobol.DataOccursTo(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.TO()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.DataPictureClause visitDataPictureClause(CobolParser.DataPictureClauseContext ctx) {
        return new Cobol.DataPictureClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PICTURE(), ctx.PIC(), ctx.IS()),
                convertAllContainer(ctx.pictureString().picture())
        );
    }

    @Override
    public Object visitDataReceivedByClause(CobolParser.DataReceivedByClauseContext ctx) {
        return new Cobol.DataReceivedByClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RECEIVED(), ctx.BY(), ctx.CONTENT(), ctx.REFERENCE(), ctx.REF())
        );
    }

    @Override
    public Object visitDataRecordAreaClause(CobolParser.DataRecordAreaClauseContext ctx) {
        return new Cobol.DataRecordAreaClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RECORD(), ctx.AREA())
        );
    }

    @Override
    public Object visitDataRecordsClause(CobolParser.DataRecordsClauseContext ctx) {
        return new Cobol.DataRecordsClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DATA(), ctx.RECORD(), ctx.IS(), ctx.RECORDS(), ctx.ARE()),
                convertAllContainer(ctx.dataName())
        );
    }

    @Override
    public Object visitDataRedefinesClause(CobolParser.DataRedefinesClauseContext ctx) {
        return new Cobol.DataRedefinesClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.REDEFINES()),
                (Cobol.CobolWord) visit(ctx.dataName())
        );
    }

    @Override
    public Object visitDataRenamesClause(CobolParser.DataRenamesClauseContext ctx) {
        return new Cobol.DataRenamesClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.RENAMES()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName(0)),
                ctx.THROUGH() != null ? (Cobol.CobolWord) visit(ctx.THROUGH()) :
                        ctx.THRU() != null ? (Cobol.CobolWord) visit(ctx.THRU()) : null,
                ctx.qualifiedDataName().size() == 1 ? null : (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName(1))
        );
    }

    @Override
    public Object visitDataSignClause(CobolParser.DataSignClauseContext ctx) {
        return new Cobol.DataSignClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SIGN(), ctx.IS(), ctx.LEADING(), ctx.TRAILING(), ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Object visitDataSynchronizedClause(CobolParser.DataSynchronizedClauseContext ctx) {
        return new Cobol.DataSignClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SYNCHRONIZED(), ctx.SYNC(), ctx.LEFT(), ctx.RIGHT())
        );
    }

    @Override
    public Object visitDataThreadLocalClause(CobolParser.DataThreadLocalClauseContext ctx) {
        return new Cobol.DataThreadLocalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.THREAD_LOCAL())
        );
    }

    @Override
    public Object visitDataTypeClause(CobolParser.DataTypeClauseContext ctx) {
        return new Cobol.DataTypeClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.TYPE(), ctx.IS(), ctx.SHORT_DATE(), ctx.LONG_DATE(), ctx.NUMERIC_DATE(),
                        ctx.NUMERIC_TIME(), ctx.LONG_TIME(), ctx.CLOB(), ctx.BLOB(), ctx.DBCLOB()),
                ctx.integerLiteral() == null ? null : new Cobol.Parenthesized(
                        randomId(),
                        whitespace(),
                        Markers.EMPTY,
                        (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                        singletonList((Cobol) visit(ctx.integerLiteral())),
                        (Cobol.CobolWord) visit(ctx.RPARENCHAR())
                )
        );
    }

    @Override
    public Object visitDataTypeDefClause(CobolParser.DataTypeDefClauseContext ctx) {
        return new Cobol.DataTypeDefClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.TYPEDEF())
        );
    }

    @Override
    public Object visitDataUsageClause(CobolParser.DataUsageClauseContext ctx) {
        return new Cobol.DataUsageClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.USAGE(), ctx.IS(), ctx.BINARY(), ctx.TRUNCATED(), ctx.EXTENDED(), ctx.BIT(),
                        ctx.COMP(), ctx.COMP_1(), ctx.COMP_2(), ctx.COMP_3(), ctx.COMP_4(), ctx.COMP_5(),
                        ctx.COMPUTATIONAL(), ctx.COMPUTATIONAL_1(), ctx.COMPUTATIONAL_2(), ctx.COMPUTATIONAL_3(), ctx.COMPUTATIONAL_4(), ctx.COMPUTATIONAL_5(),
                        ctx.CONTROL_POINT(), ctx.DATE(), ctx.DISPLAY(), ctx.DISPLAY_1(), ctx.DOUBLE(), ctx.EVENT(), ctx.FUNCTION_POINTER(), ctx.INDEX(), ctx.KANJI(), ctx.LOCK(),
                        ctx.NATIONAL(), ctx.PACKED_DECIMAL(), ctx.POINTER(), ctx.PROCEDURE_POINTER(), ctx.REAL(), ctx.SQL(), ctx.TASK())
        );
    }

    @Override
    public Object visitDataUsingClause(CobolParser.DataUsingClauseContext ctx) {
        return new Cobol.DataUsingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.USING(), ctx.LANGUAGE(), ctx.CONVENTION(), ctx.OF()),
                visit(ctx.cobolWord(), ctx.dataName())
        );
    }

    @Override
    public Object visitDataValueClause(CobolParser.DataValueClauseContext ctx) {
        return new Cobol.DataValueClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.VALUE(), ctx.VALUES(), ctx.IS(), ctx.ARE()),
                convertAllPrefixedList(singletonList(","), ctx.dataValueInterval())
        );
    }

    @Override
    public Object visitDataWithLowerBoundsClause(CobolParser.DataWithLowerBoundsClauseContext ctx) {
        return new Cobol.DecimalPointClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.LOWER(), ctx.BOUNDS())
        );
    }

    @Override
    public Cobol.DecimalPointClause visitDecimalPointClause(CobolParser.DecimalPointClauseContext ctx) {
        return new Cobol.DecimalPointClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DECIMAL_POINT(), ctx.IS(), ctx.COMMA())
        );
    }

    @Override
    public Cobol.DefaultComputationalSignClause visitDefaultComputationalSignClause(CobolParser.DefaultComputationalSignClauseContext ctx) {
        return new Cobol.DefaultComputationalSignClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DEFAULT(), ctx.COMPUTATIONAL(), ctx.COMP(), ctx.SIGN(), ctx.IS(),
                        ctx.LEADING(), ctx.TRAILING(), ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Cobol.DefaultDisplaySignClause visitDefaultDisplaySignClause(CobolParser.DefaultDisplaySignClauseContext ctx) {
        return new Cobol.DefaultDisplaySignClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DEFAULT_DISPLAY(), ctx.SIGN(), ctx.IS(), ctx.LEADING(), ctx.TRAILING(),
                        ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Object visitDeleteStatement(CobolParser.DeleteStatementContext ctx) {
        return new Cobol.Delete(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.DELETE()),
                (Name) visit(ctx.fileName()),
                visitNullable(ctx.RECORD()),
                (Cobol.StatementPhrase) visit(ctx.invalidKeyPhrase()),
                (Cobol.StatementPhrase) visit(ctx.notInvalidKeyPhrase()),
                visitNullable(ctx.END_DELETE())
        );
    }

    @Override
    public Object visitDestinationCountClause(CobolParser.DestinationCountClauseContext ctx) {
        return new Cobol.DestinationCountClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DESTINATION(), ctx.COUNT(), ctx.IS()),
                visitNullable(ctx.dataDescName())
        );
    }

    @Override
    public Object visitDestinationTableClause(CobolParser.DestinationTableClauseContext ctx) {
        return new Cobol.DestinationTableClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DESTINATION(), ctx.TABLE(), ctx.OCCURS()),
                visitNullable(ctx.integerLiteral()),
                words(ctx.TIMES(), ctx.INDEXED(), ctx.BY()),
                convertAllContainer(ctx.indexName())
        );
    }

    @Override
    public Object visitDisableStatement(CobolParser.DisableStatementContext ctx) {
        return new Cobol.Disable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.DISABLE()),
                words(ctx.INPUT(), ctx.I_O(), ctx.TERMINAL(), ctx.OUTPUT()),
                (Name) visit(ctx.cdName()),
                visitNullable(ctx.WITH()),
                (Cobol.CobolWord) visit(ctx.KEY()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Cobol.ValuedObjectComputerClause visitDiskSizeClause(CobolParser.DiskSizeClauseContext ctx) {
        return new Cobol.ValuedObjectComputerClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                Cobol.ValuedObjectComputerClause.Type.Disk,
                words(ctx.DISK(), ctx.SIZE()),
                ctx.integerLiteral() != null ? (Cobol) visit(ctx.integerLiteral()) :
                        ctx.cobolWord() != null ? (Cobol) visit(ctx.cobolWord()) : null,
                ctx.WORDS() != null ? (Cobol.CobolWord) visit(ctx.WORDS()) :
                        ctx.MODULES() != null ? (Cobol.CobolWord) visit(ctx.MODULES()) : null
        );
    }

    @Override
    public Object visitDisplayAt(CobolParser.DisplayAtContext ctx) {
        return new Cobol.DisplayAt(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.AT()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Cobol.Display visitDisplayStatement(CobolParser.DisplayStatementContext ctx) {
        return new Cobol.Display(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.DISPLAY()),
                convertAll(ctx.displayOperand()),
                visitNullable(ctx.displayAt()),
                visitNullable(ctx.displayUpon()),
                visitNullable(ctx.displayWith()),
                visitNullable(ctx.onExceptionClause()),
                visitNullable(ctx.notOnExceptionClause()),
                visitNullable(ctx.END_DISPLAY())
        );
    }

    @Override
    public Object visitDisplayUpon(CobolParser.DisplayUponContext ctx) {
        return new Cobol.DisplayUpon(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.UPON()),
                visit(ctx.mnemonicName(), ctx.environmentName())
        );
    }

    @Override
    public Object visitDisplayWith(CobolParser.DisplayWithContext ctx) {
        return words(ctx.WITH(), ctx.NO(), ctx.ADVANCING());
    }

    @Override
    public Object visitDivideByGivingStatement(CobolParser.DivideByGivingStatementContext ctx) {
        return new Cobol.DivideGiving(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.BY()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.divideGivingPhrase())
        );
    }

    @Override
    public Object visitDivideGivingPhrase(CobolParser.DivideGivingPhraseContext ctx) {
        return new Cobol.DivideGivingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.GIVING()),
                convertAllContainer(ctx.divideGiving())
        );
    }

    @Override
    public Object visitDivideIntoGivingStatement(CobolParser.DivideIntoGivingStatementContext ctx) {
        return new Cobol.DivideGiving(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INTO()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.divideGivingPhrase())
        );
    }

    @Override
    public Object visitDivideIntoStatement(CobolParser.DivideIntoStatementContext ctx) {
        return new Cobol.DivideInto(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INTO()),
                convertAllContainer(ctx.divideInto())
        );
    }

    @Override
    public Object visitDivideRemainder(CobolParser.DivideRemainderContext ctx) {
        return new Cobol.DivideRemainder(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.REMAINDER()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitDivideStatement(CobolParser.DivideStatementContext ctx) {
        return new Cobol.Divide(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.DIVIDE()),
                visit(ctx.identifier(), ctx.literal()),
                visit(ctx.divideIntoStatement(), ctx.divideIntoGivingStatement(), ctx.divideByGivingStatement()),
                visitNullable(ctx.divideRemainder()),
                visitNullable(ctx.onSizeErrorPhrase()),
                visitNullable(ctx.notOnSizeErrorPhrase()),
                visitNullable(ctx.END_DIVIDE())
        );
    }

    @Override
    public Object visitEnableStatement(CobolParser.EnableStatementContext ctx) {
        return new Cobol.Enable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ENABLE()),
                words(ctx.INPUT(), ctx.I_O(), ctx.TERMINAL(), ctx.OUTPUT()),
                (Name) visit(ctx.cdName()),
                visitNullable(ctx.WITH()),
                (Cobol.CobolWord) visit(ctx.KEY()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitEndKeyClause(CobolParser.EndKeyClauseContext ctx) {
        return new Cobol.EndKeyClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.END(), ctx.KEY(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Cobol.EndProgram visitEndProgramStatement(CobolParser.EndProgramStatementContext ctx) {
        return new Cobol.EndProgram(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.END(), ctx.PROGRAM()),
                (Name) visit(ctx.programName())
        );
    }

    @Override
    public Object visitEntryStatement(CobolParser.EntryStatementContext ctx) {
        return new Cobol.Entry(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ENTRY()),
                (Literal) visit(ctx.literal()),
                convertAllContainer(padLeft(ctx.USING()), ctx.identifier())
        );
    }

    @Override
    public Cobol.EnvironmentDivision visitEnvironmentDivision(CobolParser.EnvironmentDivisionContext ctx) {
        return new Cobol.EnvironmentDivision(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ENVIRONMENT(), ctx.DIVISION()),
                convertAllContainer(sourceBefore("."), ctx.environmentDivisionBody())
        );
    }

    @Override
    public Object visitEvaluateAlsoCondition(CobolParser.EvaluateAlsoConditionContext ctx) {
        return new Cobol.EvaluateAlsoCondition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ALSO()),
                (Cobol.EvaluateCondition) visit(ctx.evaluateCondition())
        );
    }

    @Override
    public Object visitEvaluateAlsoSelect(CobolParser.EvaluateAlsoSelectContext ctx) {
        return new Cobol.EvaluateAlso(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ALSO()),
                (Cobol) visit(ctx.evaluateSelect())
        );
    }

    @Override
    public Object visitEvaluateCondition(CobolParser.EvaluateConditionContext ctx) {
        return new Cobol.EvaluateCondition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ANY(), ctx.NOT()),
                ctx.ANY() != null ? null : visit(ctx.evaluateValue(), ctx.condition(), ctx.booleanLiteral()),
                visitNullable(ctx.evaluateThrough())
        );
    }

    @Override
    public Object visitEvaluateStatement(CobolParser.EvaluateStatementContext ctx) {
        return new Cobol.Evaluate(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.EVALUATE()),
                (Cobol) visit(ctx.evaluateSelect()),
                convertAllContainer(ctx.evaluateAlsoSelect()),
                convertAllContainer(ctx.evaluateWhenPhrase()),
                visitNullable(ctx.evaluateWhenOther()),
                visitNullable(ctx.END_EVALUATE())
        );
    }

    @Override
    public Object visitEvaluateThrough(CobolParser.EvaluateThroughContext ctx) {
        return new Cobol.EvaluateThrough(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.THROUGH(), ctx.THRU()),
                (Cobol) visit(ctx.evaluateValue())
        );
    }

    @Override
    public Object visitEvaluateWhen(CobolParser.EvaluateWhenContext ctx) {
        return new Cobol.EvaluateWhen(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.WHEN()),
                (Cobol.EvaluateCondition) visit(ctx.evaluateCondition()),
                convertAllContainer(ctx.evaluateAlsoCondition())
        );
    }

    @Override
    public Object visitEvaluateWhenOther(CobolParser.EvaluateWhenOtherContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WHEN(), ctx.OTHER()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitEvaluateWhenPhrase(CobolParser.EvaluateWhenPhraseContext ctx) {
        return new Cobol.EvaluateWhenPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.evaluateWhen()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitExecCicsStatement(CobolParser.ExecCicsStatementContext ctx) {
        return new Cobol.ExecCicsStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.EXECCICSLINE())
        );
    }

    @Override
    public Object visitExecSqlImsStatement(CobolParser.ExecSqlImsStatementContext ctx) {
        return new Cobol.ExecSqlImsStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.EXECSQLIMSLINE())
        );
    }

    @Override
    public Object visitExecSqlStatement(CobolParser.ExecSqlStatementContext ctx) {
        return new Cobol.ExecSqlStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.EXECSQLLINE())
        );
    }

    @Override
    public Cobol.Exhibit visitExhibitStatement(CobolParser.ExhibitStatementContext ctx) {
        return new Cobol.Exhibit(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.EXHIBIT(), ctx.NAMED(), ctx.CHANGED()),
                convertAllContainer(ctx.exhibitOperand())
        );
    }

    @Override
    public Object visitExitStatement(CobolParser.ExitStatementContext ctx) {
        return new Cobol.Exit(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.EXIT(), ctx.PROGRAM())
        );
    }

    @Override
    public Object visitExternalClause(CobolParser.ExternalClauseContext ctx) {
        return new Cobol.ExternalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.EXTERNAL())
        );
    }

    @Override
    public Object visitFileControlEntry(CobolParser.FileControlEntryContext ctx) {
        return new Cobol.FileControlEntry(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol) visit(ctx.selectClause()),
                convertAllContainer(ctx.fileControlClause())
        );
    }

    @Override
    public Object visitFileControlParagraph(CobolParser.FileControlParagraphContext ctx) {
        return new Cobol.FileControlParagraph(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FILE_CONTROL()),
                convertAllList(singletonList("."), ctx.fileControlEntry()),
                (Cobol.CobolWord) visit(ctx.DOT_FS().get(ctx.DOT_FS().size() - 1))
        );
    }

    @Override
    public Cobol.FileDescriptionEntry visitFileDescriptionEntry(CobolParser.FileDescriptionEntryContext ctx) {
        return new Cobol.FileDescriptionEntry(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.FD(), ctx.SD()),
                (Cobol.CobolWord) visit(ctx.fileName()),
                convertAllList(singletonList("."), ctx.fileDescriptionEntryClause()),
                convertAllContainer(sourceBefore("."), ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Cobol.FileSection visitFileSection(CobolParser.FileSectionContext ctx) {
        return new Cobol.FileSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FILE(), ctx.SECTION()),
                convertAllContainer(sourceBefore("."), ctx.fileDescriptionEntry())
        );
    }

    @Override
    public Object visitFileStatusClause(CobolParser.FileStatusClauseContext ctx) {
        return new Cobol.FileStatusClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FILE(), ctx.STATUS(), ctx.IS()),
                convertAllContainer(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.FunctionCall visitFunctionCall(CobolParser.FunctionCallContext ctx) {
        return new Cobol.FunctionCall(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FUNCTION()),
                (Cobol.CobolWord) visit(ctx.functionName()),
                convertAllContainer(ctx.functionCallArguments()),
                visitNullable(ctx.referenceModifier())
        );
    }

    @Override
    public Object visitFunctionCallArguments(CobolParser.FunctionCallArgumentsContext ctx) {
        return new Cobol.Parenthesized(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                convertAllPrefixedList(Collections.singletonList(","), ctx.argument()),
                (Cobol.CobolWord) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitGenerateStatement(CobolParser.GenerateStatementContext ctx) {
        return new Cobol.Generate(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.GENERATE()),
                (Cobol.QualifiedDataName) visit(ctx.reportName())
        );
    }

    @Override
    public Object visitGlobalClause(CobolParser.GlobalClauseContext ctx) {
        return new Cobol.GlobalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.GLOBAL())
        );
    }

    @Override
    public Object visitGoToDependingOnStatement(CobolParser.GoToDependingOnStatementContext ctx) {
        return new Cobol.GoToDependingOnStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.procedureName()),
                words(ctx.MORE_LABELS(), ctx.DEPENDING(), ctx.ON()),
                visitNullable(ctx.identifier())
        );
    }

    @Override
    public Object visitGoToStatement(CobolParser.GoToStatementContext ctx) {
        return new Cobol.GoTo(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.GO(), ctx.TO()),
                visit(ctx.goToStatementSimple(), ctx.goToDependingOnStatement())
        );
    }

    @Override
    public Object visitGobackStatement(CobolParser.GobackStatementContext ctx) {
        return new Cobol.GoBack(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.GOBACK())
        );
    }

    @Override
    public Cobol visitIdentificationDivision(CobolParser.IdentificationDivisionContext ctx) {
        if (ctx.identificationDivisionBody() != null && ctx.identificationDivisionBody().size() != 0) {
            throw new UnsupportedOperationException("Implement me");
        }
        return new Cobol.IdentificationDivision(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IDENTIFICATION(), ctx.ID(), ctx.DIVISION(), ctx.DOT_FS()),
                (Cobol.ProgramIdParagraph) visit(ctx.programIdParagraph())
        );
    }

    @Override
    public Object visitIfElse(CobolParser.IfElseContext ctx) {
        return new Cobol.IfElse(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ELSE()),
                words(ctx.NEXT(), ctx.SENTENCE()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitIfStatement(CobolParser.IfStatementContext ctx) {
        return new Cobol.If(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.IF()),
                (Cobol.Condition) visit(ctx.condition()),
                (Cobol.IfThen) visit(ctx.ifThen()),
                visitNullable(ctx.ifElse()),
                visitNullable(ctx.END_IF())
        );
    }

    @Override
    public Object visitIfThen(CobolParser.IfThenContext ctx) {
        return new Cobol.IfThen(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.THEN()),
                words(ctx.NEXT(), ctx.SENTENCE()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitInData(CobolParser.InDataContext ctx) {
        return new Cobol.InData(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.dataName())
        );
    }

    @Override
    public Object visitInFile(CobolParser.InFileContext ctx) {
        return new Cobol.InFile(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.fileName())
        );
    }

    @Override
    public Object visitInLibrary(CobolParser.InLibraryContext ctx) {
        return new Cobol.InLibrary(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.libraryName())
        );
    }

    @Override
    public Object visitInMnemonic(CobolParser.InMnemonicContext ctx) {
        return new Cobol.InMnemonic(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Cobol.InSection visitInSection(CobolParser.InSectionContext ctx) {
        return new Cobol.InSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.sectionName())
        );
    }

    @Override
    public Object visitInTable(CobolParser.InTableContext ctx) {
        return new Cobol.InLibrary(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.tableCall())
        );
    }

    @Override
    public Object visitInitializeReplacingBy(CobolParser.InitializeReplacingByContext ctx) {
        return new Cobol.InitializeReplacingBy(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ALPHABETIC(), ctx.ALPHANUMERIC(), ctx.ALPHANUMERIC_EDITED(),
                        ctx.NATIONAL(), ctx.NATIONAL_EDITED(), ctx.NUMERIC(), ctx.NATIONAL_EDITED(),
                        ctx.DBCS(), ctx.EGCS(), ctx.DATA(), ctx.BY()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitInitializeReplacingPhrase(CobolParser.InitializeReplacingPhraseContext ctx) {
        return new Cobol.InitializeReplacingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.REPLACING()),
                convertAllContainer(ctx.initializeReplacingBy())
        );
    }

    @Override
    public Object visitInitializeStatement(CobolParser.InitializeStatementContext ctx) {
        return new Cobol.Initialize(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INITIALIZE()),
                convertAllContainer(ctx.identifier()),
                visitNullable(ctx.initializeReplacingPhrase())
        );
    }

    @Override
    public Object visitInitiateStatement(CobolParser.InitiateStatementContext ctx) {
        return new Cobol.Initiate(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INITIATE()),
                convertAllContainer(ctx.reportName())
        );
    }

    @Override
    public Object visitInputOutputSection(CobolParser.InputOutputSectionContext ctx) {
        return new Cobol.InputOutputSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.INPUT_OUTPUT(), ctx.SECTION(), ctx.DOT_FS()),
                convertAllContainer(ctx.inputOutputSectionParagraph())
        );
    }

    @Override
    public Object visitInspectAllLeading(CobolParser.InspectAllLeadingContext ctx) {
        return new Cobol.InspectAllLeading(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                convertAllContainer(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectAllLeadings(CobolParser.InspectAllLeadingsContext ctx) {
        return new Cobol.InspectAllLeading(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.ALL(), ctx.LEADING()),
                convertAllContainer(ctx.inspectAllLeading())
        );
    }

    @Override
    public Object visitInspectBeforeAfter(CobolParser.InspectBeforeAfterContext ctx) {
        return new Cobol.InspectBeforeAfter(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BEFORE(), ctx.AFTER(), ctx.INITIAL()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitInspectBy(CobolParser.InspectByContext ctx) {
        return new Cobol.InspectBy(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.BY()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitInspectCharacters(CobolParser.InspectCharactersContext ctx) {
        return new Cobol.InspectCharacters(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.CHARACTER(), ctx.CHARACTERS()),
                convertAllContainer(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectConvertingPhrase(CobolParser.InspectConvertingPhraseContext ctx) {
        return new Cobol.InspectConvertingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.CONVERTING()),
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.InspectTo) visit(ctx.inspectTo()),
                convertAllContainer(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectFor(CobolParser.InspectForContext ctx) {
        return new Cobol.InspectFor(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Identifier) visit(ctx.identifier()),
                (Cobol.CobolWord) visit(ctx.FOR()),
                convertAllContainer(ctx.inspectCharacters(), ctx.inspectAllLeadings())
        );
    }

    @Override
    public Object visitInspectReplacingAllLeading(CobolParser.InspectReplacingAllLeadingContext ctx) {
        return new Cobol.InspectReplacingAllLeading(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.InspectBy) visit(ctx.inspectBy()),
                convertAllContainer(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectReplacingAllLeadings(CobolParser.InspectReplacingAllLeadingsContext ctx) {
        return new Cobol.InspectReplacingAllLeadings(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.ALL(), ctx.LEADING(), ctx.FIRST()),
                convertAllContainer(ctx.inspectReplacingAllLeading())
        );
    }

    @Override
    public Object visitInspectReplacingCharacters(CobolParser.InspectReplacingCharactersContext ctx) {
        return new Cobol.InspectReplacingCharacters(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.CHARACTER(), ctx.CHARACTERS()),
                (Cobol.InspectBy) visit(ctx.inspectBy()),
                convertAllContainer(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectReplacingPhrase(CobolParser.InspectReplacingPhraseContext ctx) {
        return new Cobol.InspectReplacingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.REPLACING()),
                convertAllContainer(ctx.inspectReplacingCharacters(), ctx.inspectReplacingAllLeadings())
        );
    }

    @Override
    public Object visitInspectStatement(CobolParser.InspectStatementContext ctx) {
        return new Cobol.Inspect(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INSPECT()),
                (Identifier) visit(ctx.identifier()),
                visit(ctx.inspectTallyingPhrase(), ctx.inspectReplacingPhrase(),
                        ctx.inspectTallyingReplacingPhrase(), ctx.inspectConvertingPhrase())
        );
    }

    @Override
    public Object visitInspectTallyingPhrase(CobolParser.InspectTallyingPhraseContext ctx) {
        return new Cobol.InspectTallyingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.TALLYING()),
                convertAllContainer(ctx.inspectFor())
        );
    }

    @Override
    public Object visitInspectTallyingReplacingPhrase(CobolParser.InspectTallyingReplacingPhraseContext ctx) {
        return new Cobol.InspectTallyingReplacingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.TALLYING()),
                convertAllContainer(ctx.inspectFor()),
                convertAllContainer(ctx.inspectReplacingPhrase())
        );
    }

    @Override
    public Object visitInspectTo(CobolParser.InspectToContext ctx) {
        return new Cobol.InspectTo(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.TO()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Cobol.StatementPhrase visitInvalidKeyPhrase(CobolParser.InvalidKeyPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.INVALID(), ctx.KEY()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitIoControlParagraph(CobolParser.IoControlParagraphContext ctx) {
        return new Cobol.IoControlParagraph(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.I_O_CONTROL()),
                (Cobol.CobolWord) visit(ctx.DOT_FS(0)),
                visitNullable(ctx.fileName()),
                ctx.fileName() == null ? null : (Cobol.CobolWord) visit(ctx.DOT_FS(1)),
                convertAllContainer(ctx.ioControlClause()).withLastSpace(sourceBefore("."))
        );
    }

    @Override
    public Object visitLabelRecordsClause(CobolParser.LabelRecordsClauseContext ctx) {
        return new Cobol.LabelRecordsClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LABEL(), ctx.RECORD(), ctx.IS(), ctx.RECORDS(), ctx.ARE(), ctx.OMITTED(), ctx.STANDARD()),
                convertAllContainer(ctx.dataName())
        );
    }

    @Override
    public Object visitLibraryAttributeClauseFormat1(CobolParser.LibraryAttributeClauseFormat1Context ctx) {
        return new Cobol.LibraryAttributeClauseFormat1(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ATTRIBUTE(), ctx.SHARING(), ctx.IS(), ctx.DONTCARE(), ctx.PRIVATE(), ctx.SHAREDBYRUNUNIT(), ctx.SHAREDBYALL())
        );
    }

    @Override
    public Object visitLibraryAttributeClauseFormat2(CobolParser.LibraryAttributeClauseFormat2Context ctx) {
        return new Cobol.LibraryAttributeClauseFormat2(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ATTRIBUTE()),
                visitNullable(ctx.libraryAttributeFunction()),
                words(ctx.LIBACCESS(), ctx.IS(), ctx.BYFUNCTION(), ctx.BYTITLE()),
                visitNullable(ctx.libraryAttributeParameter()),
                visitNullable(ctx.libraryAttributeTitle())
        );
    }

    @Override
    public Object visitLibraryAttributeFunction(CobolParser.LibraryAttributeFunctionContext ctx) {
        return new Cobol.LibraryAttributeFunction(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FUNCTIONNAME(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitLibraryAttributeParameter(CobolParser.LibraryAttributeParameterContext ctx) {
        return new Cobol.LibraryAttributeParameter(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LIBPARAMETER(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitLibraryAttributeTitle(CobolParser.LibraryAttributeTitleContext ctx) {
        return new Cobol.LibraryAttributeTitle(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.TITLE(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitLibraryDescriptionEntryFormat1(CobolParser.LibraryDescriptionEntryFormat1Context ctx) {
        return new Cobol.LibraryDescriptionEntryFormat1(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.LD()),
                (Cobol.CobolWord) visit(ctx.libraryName()),
                (Cobol.CobolWord) visit(ctx.EXPORT()),
                visitNullable(ctx.libraryAttributeClauseFormat1()),
                visitNullable(ctx.libraryEntryProcedureClauseFormat1())
        );
    }

    @Override
    public Object visitLibraryDescriptionEntryFormat2(CobolParser.LibraryDescriptionEntryFormat2Context ctx) {
        return new Cobol.LibraryDescriptionEntryFormat2(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.LB()),
                (Cobol.CobolWord) visit(ctx.libraryName()),
                (Cobol.CobolWord) visit(ctx.IMPORT()),
                visitNullable(ctx.libraryIsGlobalClause()),
                visitNullable(ctx.libraryIsCommonClause()),
                convertAllContainer(ctx.libraryAttributeClauseFormat2(), ctx.libraryEntryProcedureClauseFormat2())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureClauseFormat1(CobolParser.LibraryEntryProcedureClauseFormat1Context ctx) {
        return new Cobol.LibraryEntryProcedureClauseFormat1(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ENTRY_PROCEDURE()),
                (Cobol.CobolWord) visit(ctx.programName()),
                visitNullable(ctx.libraryEntryProcedureForClause())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureClauseFormat2(CobolParser.LibraryEntryProcedureClauseFormat2Context ctx) {
        return new Cobol.LibraryEntryProcedureClauseFormat2(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ENTRY_PROCEDURE()),
                (Cobol.CobolWord) visit(ctx.programName()),
                visitNullable(ctx.libraryEntryProcedureForClause()),
                visitNullable(ctx.libraryEntryProcedureWithClause()),
                visitNullable(ctx.libraryEntryProcedureUsingClause()),
                visitNullable(ctx.libraryEntryProcedureGivingClause())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureForClause(CobolParser.LibraryEntryProcedureForClauseContext ctx) {
        return new Cobol.LibraryEntryProcedureForClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FOR()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureGivingClause(CobolParser.LibraryEntryProcedureGivingClauseContext ctx) {
        return new Cobol.LibraryEntryProcedureGivingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.GIVING()),
                (Cobol.CobolWord) visit(ctx.dataName())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureUsingClause(CobolParser.LibraryEntryProcedureUsingClauseContext ctx) {
        return new Cobol.LibraryEntryProcedureUsingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.USING()),
                convertAllContainer(ctx.libraryEntryProcedureUsingName())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureWithClause(CobolParser.LibraryEntryProcedureWithClauseContext ctx) {
        return new Cobol.LibraryEntryProcedureWithClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.WITH()),
                convertAllContainer(ctx.libraryEntryProcedureWithName())
        );
    }

    @Override
    public Object visitLibraryIsCommonClause(CobolParser.LibraryIsCommonClauseContext ctx) {
        return new Cobol.LibraryIsCommonClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.COMMON())
        );
    }

    @Override
    public Object visitLibraryIsGlobalClause(CobolParser.LibraryIsGlobalClauseContext ctx) {
        return new Cobol.LibraryIsGlobalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.GLOBAL())
        );
    }

    @Override
    public Object visitLinageClause(CobolParser.LinageClauseContext ctx) {
        return new Cobol.LinageClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LINAGE(), ctx.IS()),
                visit(ctx.dataName(), ctx.integerLiteral()),
                visitNullable(ctx.LINES()),
                convertAllContainer(ctx.linageAt())
        );
    }

    @Override
    public Object visitLinageFootingAt(CobolParser.LinageFootingAtContext ctx) {
        return new Cobol.LinageFootingAt(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.FOOTING(), ctx.AT()),
                visit(ctx.dataName(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitLinageLinesAtBottom(CobolParser.LinageLinesAtBottomContext ctx) {
        return new Cobol.LinageFootingAt(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LINES(), ctx.AT(), ctx.BOTTOM()),
                visit(ctx.dataName(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitLinageLinesAtTop(CobolParser.LinageLinesAtTopContext ctx) {
        return new Cobol.LinageFootingAt(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LINES(), ctx.AT(), ctx.TOP()),
                visit(ctx.dataName(), ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.LinkageSection visitLinkageSection(CobolParser.LinkageSectionContext ctx) {
        return new Cobol.LinkageSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LINKAGE(), ctx.SECTION()),
                convertAllContainer(sourceBefore("."), ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Cobol.LocalStorageSection visitLocalStorageSection(CobolParser.LocalStorageSectionContext ctx) {
        return new Cobol.LocalStorageSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LOCAL_STORAGE(), ctx.SECTION()),
                visitNullable(ctx.LD()),
                visitNullable(ctx.localName()),
                convertAllContainer(sourceBefore("."), ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Cobol.ValuedObjectComputerClause visitMemorySizeClause(CobolParser.MemorySizeClauseContext ctx) {
        return new Cobol.ValuedObjectComputerClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                Cobol.ValuedObjectComputerClause.Type.Memory,
                words(ctx.MEMORY(), ctx.SIZE()),
                (Cobol) visit(ctx.integerLiteral() == null ? ctx.cobolWord() : ctx.integerLiteral()),
                ctx.WORDS() != null ? (Cobol.CobolWord) visit(ctx.WORDS()) :
                        ctx.CHARACTERS() != null ? (Cobol.CobolWord) visit(ctx.CHARACTERS()) :
                                ctx.MODULES() != null ? (Cobol.CobolWord) visit(ctx.MODULES()) : null
        );
    }

    @Override
    public Cobol.Mergeable visitMergeCollatingAlphanumeric(CobolParser.MergeCollatingAlphanumericContext ctx) {
        return new Cobol.Mergeable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FOR(), ctx.ALPHANUMERIC(), ctx.IS()),
                (Name) visit(ctx.alphabetName())
        );
    }

    @Override
    public Cobol.Mergeable visitMergeCollatingNational(CobolParser.MergeCollatingNationalContext ctx) {
        return new Cobol.Mergeable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FOR(), ctx.NATIONAL(), ctx.IS()),
                (Name) visit(ctx.alphabetName())
        );
    }

    @Override
    public Cobol.MergeCollatingSequencePhrase visitMergeCollatingSequencePhrase(CobolParser.MergeCollatingSequencePhraseContext ctx) {
        return new Cobol.MergeCollatingSequencePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.COLLATING(), ctx.SEQUENCE(), ctx.IS()),
                convertAllContainer(ctx.alphabetName()),
                visitNullable(ctx.mergeCollatingAlphanumeric()),
                visitNullable(ctx.mergeCollatingNational())
        );
    }

    @Override
    public Cobol.MergeGiving visitMergeGiving(CobolParser.MergeGivingContext ctx) {
        return new Cobol.MergeGiving(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.fileName()),
                words(ctx.LOCK(), ctx.SAVE(), ctx.NO(), ctx.REWIND(), ctx.CRUNCH(), ctx.RELEASE(), ctx.WITH(), ctx.REMOVE(), ctx.CRUNCH())
        );
    }

    @Override
    public Cobol.MergeGivingPhrase visitMergeGivingPhrase(CobolParser.MergeGivingPhraseContext ctx) {
        return new Cobol.MergeGivingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.GIVING()),
                convertAllContainer(ctx.mergeGiving())
        );
    }

    @Override
    public Cobol.MergeOnKeyClause visitMergeOnKeyClause(CobolParser.MergeOnKeyClauseContext ctx) {
        return new Cobol.MergeOnKeyClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ON(), ctx.ASCENDING(), ctx.DESCENDING(), ctx.KEY()),
                convertAllContainer(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.MergeOutputProcedurePhrase visitMergeOutputProcedurePhrase(CobolParser.MergeOutputProcedurePhraseContext ctx) {
        return new Cobol.MergeOutputProcedurePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.OUTPUT(), ctx.PROCEDURE(), ctx.IS()),
                visitProcedureName(ctx.procedureName()),
                visitNullable(ctx.mergeOutputThrough())
        );
    }

    @Override
    public Cobol.MergeOutputThrough visitMergeOutputThrough(CobolParser.MergeOutputThroughContext ctx) {
        return new Cobol.MergeOutputThrough(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.THROUGH(), ctx.THRU()),
                visitProcedureName(ctx.procedureName())
        );
    }

    @Override
    public Cobol.Merge visitMergeStatement(CobolParser.MergeStatementContext ctx) {
        return new Cobol.Merge(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.MERGE()),
                (Name) visit(ctx.fileName()),
                convertAllContainer(ctx.mergeOnKeyClause()),
                visitNullable(ctx.mergeCollatingSequencePhrase()),
                convertAllContainer(ctx.mergeUsing()),
                visitNullable(ctx.mergeOutputProcedurePhrase()),
                convertAllContainer(ctx.mergeGivingPhrase())
        );
    }

    @Override
    public Cobol.MergeUsing visitMergeUsing(CobolParser.MergeUsingContext ctx) {
        return new Cobol.MergeUsing(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.USING()),
                convertAllContainer(ctx.fileName())
        );
    }

    @Override
    public Object visitMessageCountClause(CobolParser.MessageCountClauseContext ctx) {
        return new Cobol.MessageCountClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.MESSAGE(), ctx.COUNT(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitMessageDateClause(CobolParser.MessageDateClauseContext ctx) {
        return new Cobol.MessageDateClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.MESSAGE(), ctx.DATE(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitMessageTimeClause(CobolParser.MessageTimeClauseContext ctx) {
        return new Cobol.MessageTimeClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.MESSAGE(), ctx.TIME(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Cobol.MoveCorrespondingToStatement visitMoveCorrespondingToStatement(CobolParser.MoveCorrespondingToStatementContext ctx) {
        return new Cobol.MoveCorrespondingToStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.CORRESPONDING(), ctx.CORR()),
                (Identifier) visit(ctx.moveCorrespondingToSendingArea()),
                convertAllContainer(padLeft(ctx.TO()), ctx.identifier())
        );
    }

    @Override
    public Cobol.MoveStatement visitMoveStatement(CobolParser.MoveStatementContext ctx) {
        return new Cobol.MoveStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.MOVE(), ctx.ALL()),
                visit(ctx.moveCorrespondingToStatement(), ctx.moveToStatement())
        );
    }

    @Override
    public Cobol.MoveToStatement visitMoveToStatement(CobolParser.MoveToStatementContext ctx) {
        return new Cobol.MoveToStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.moveToSendingArea()),
                convertAllContainer(padLeft(ctx.TO()), ctx.identifier())
        );
    }

    @Override
    public Object visitMultDiv(CobolParser.MultDivContext ctx) {
        return new Cobol.MultDiv(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.ASTERISKCHAR(), ctx.SLASHCHAR()),
                (Cobol.Powers) visit(ctx.powers())
        );
    }

    @Override
    public Object visitMultDivs(CobolParser.MultDivsContext ctx) {
        return new Cobol.MultDivs(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.Powers) visit(ctx.powers()),
                convertAllContainer(ctx.multDiv())
        );
    }

    @Override
    public Object visitMultipleFileClause(CobolParser.MultipleFileClauseContext ctx) {
        return new Cobol.MultipleFileClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.MULTIPLE(), ctx.FILE(), ctx.TAPE(), ctx.CONTAINS()),
                convertAllContainer(ctx.multipleFilePosition())
        );
    }

    @Override
    public Object visitMultipleFilePosition(CobolParser.MultipleFilePositionContext ctx) {
        return new Cobol.MultipleFilePosition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.fileName()),
                visitNullable(ctx.POSITION()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.MultiplyGiving visitMultiplyGiving(CobolParser.MultiplyGivingContext ctx) {
        return new Cobol.MultiplyGiving(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.multiplyGivingOperand()),
                convertAllContainer(padLeft(ctx.GIVING()), ctx.multiplyGivingResult())
        );
    }

    @Override
    public Cobol.MultiplyRegular visitMultiplyRegular(CobolParser.MultiplyRegularContext ctx) {
        return new Cobol.MultiplyRegular(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.multiplyRegularOperand())
        );
    }

    @Override
    public Cobol.Multiply visitMultiplyStatement(CobolParser.MultiplyStatementContext ctx) {
        return new Cobol.Multiply(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.MULTIPLY()),
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.CobolWord) visit(ctx.BY()),
                visit(ctx.multiplyRegular(), ctx.multiplyGiving()),
                visitNullable(ctx.onSizeErrorPhrase()),
                visitNullable(ctx.notOnSizeErrorPhrase()),
                visitNullable(ctx.END_MULTIPLY())
        );
    }

    @Override
    public Cobol.NextSentence visitNextSentenceStatement(CobolParser.NextSentenceStatementContext ctx) {
        return new Cobol.NextSentence(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NEXT(), ctx.SENTENCE())
            );
    }

    @Override
    public Cobol.StatementPhrase visitNotAtEndPhrase(CobolParser.NotAtEndPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NOT(), ctx.AT(), ctx.END()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Cobol.StatementPhrase visitNotInvalidKeyPhrase(CobolParser.NotInvalidKeyPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NOT(), ctx.INVALID(), ctx.KEY()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitNotOnExceptionClause(CobolParser.NotOnExceptionClauseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NOT(), ctx.ON(), ctx.EXCEPTION()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Cobol.StatementPhrase visitNotOnOverflowPhrase(CobolParser.NotOnOverflowPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NOT(), ctx.ON(), ctx.OVERFLOW()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Cobol.StatementPhrase visitNotOnSizeErrorPhrase(CobolParser.NotOnSizeErrorPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NOT(), ctx.ON(), ctx.SIZE(), ctx.ERROR()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Cobol.ObjectComputer visitObjectComputerParagraph(CobolParser.ObjectComputerParagraphContext ctx) {
        return new Cobol.ObjectComputer(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.OBJECT_COMPUTER(), ctx.DOT_FS(0)),
                ctx.computerName() == null ? null :
                        new Cobol.ObjectComputerDefinition(
                                randomId(),
                                whitespace(),
                                Markers.EMPTY,
                                (Cobol.CobolWord) visit(ctx.computerName()),
                                convertAllContainer(ctx.objectComputerClause())
                        ),
                ctx.DOT_FS().size() == 1 ? null : (Cobol.CobolWord) visit(ctx.DOT_FS(1))
        );
    }

    @Override
    public Cobol.OdtClause visitOdtClause(CobolParser.OdtClauseContext ctx) {
        return new Cobol.OdtClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ODT(), ctx.IS()),
                (Identifier) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Object visitOnExceptionClause(CobolParser.OnExceptionClauseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ON(), ctx.EXCEPTION()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Cobol.StatementPhrase visitOnOverflowPhrase(CobolParser.OnOverflowPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ON(), ctx.OVERFLOW()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitOnSizeErrorPhrase(CobolParser.OnSizeErrorPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ON(), ctx.SIZE(), ctx.ERROR()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Cobol.OpenIOExtendStatement visitOpenExtendStatement(CobolParser.OpenExtendStatementContext ctx) {
        return new Cobol.OpenIOExtendStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.EXTEND()),
                convertAllContainer(ctx.fileName())
        );
    }

    @Override
    public Cobol.OpenIOExtendStatement visitOpenIOStatement(CobolParser.OpenIOStatementContext ctx) {
        return new Cobol.OpenIOExtendStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.I_O()),
                convertAllContainer(ctx.fileName())
        );
    }

    @Override
    public Cobol.Openable visitOpenInput(CobolParser.OpenInputContext ctx) {
        return new Cobol.Openable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.fileName()),
                words(ctx.REVERSED(), ctx.WITH(), ctx.NO(), ctx.REWIND())
        );
    }

    @Override
    public Cobol.OpenInputOutputStatement visitOpenInputStatement(CobolParser.OpenInputStatementContext ctx) {
        return new Cobol.OpenInputOutputStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INPUT()),
                convertAllContainer(ctx.openInput())
        );
    }

    @Override
    public Cobol.Openable visitOpenOutput(CobolParser.OpenOutputContext ctx) {
        return new Cobol.Openable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.fileName()),
                words(ctx.WITH(), ctx.NO(), ctx.REWIND())
        );
    }

    @Override
    public Cobol.OpenInputOutputStatement visitOpenOutputStatement(CobolParser.OpenOutputStatementContext ctx) {
        return new Cobol.OpenInputOutputStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.OUTPUT()),
                convertAllContainer(ctx.openOutput())
        );
    }

    @Override
    public Cobol.Open visitOpenStatement(CobolParser.OpenStatementContext ctx) {
        return new Cobol.Open(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.OPEN()),
                convertAllContainer(ctx.openInputStatement(), ctx.openOutputStatement(), ctx.openIOStatement(),
                        ctx.openExtendStatement())
        );
    }

    @Override
    public Object visitOrganizationClause(CobolParser.OrganizationClauseContext ctx) {
        return new Cobol.OrganizationClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ORGANIZATION(), ctx.IS(), ctx.LINE(), ctx.RECORD(), ctx.BINARY(),
                        ctx.SEQUENTIAL(), ctx.RELATIVE(), ctx.INDEXED())
        );
    }

    @Override
    public Object visitPaddingCharacterClause(CobolParser.PaddingCharacterClauseContext ctx) {
        return new Cobol.PaddingCharacterClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PADDING(), ctx.CHARACTER(), ctx.IS()),
                visit(ctx.qualifiedDataName(), ctx.literal())
        );
    }

    @Override
    public Object visitParagraph(CobolParser.ParagraphContext ctx) {
        return new Cobol.Paragraph(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.paragraphName()),
                visitNullable(ctx.DOT_FS()),
                visitNullable(ctx.alteredGoTo()),
                convertAllContainer(ctx.sentence())
        );
    }

    @Override
    public Cobol.Paragraphs visitParagraphs(CobolParser.ParagraphsContext ctx) {
        return new Cobol.Paragraphs(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.sentence()),
                convertAllContainer(ctx.paragraph())
        );
    }

    @Override
    public Object visitPasswordClause(CobolParser.PasswordClauseContext ctx) {
        return new Cobol.PasswordClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PASSWORD(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataName())
        );
    }

    @Override
    public Cobol.Performable visitPerformAfter(CobolParser.PerformAfterContext ctx) {
        return new Cobol.Performable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.AFTER()),
                (Cobol) visit(ctx.performVaryingPhrase())
        );
    }

    @Override
    public Cobol.Performable visitPerformBy(CobolParser.PerformByContext ctx) {
        return new Cobol.Performable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.BY()),
                visit(ctx.identifier(), ctx.literal(), ctx.arithmeticExpression())
        );
    }

    @Override
    public Cobol.Performable visitPerformFrom(CobolParser.PerformFromContext ctx) {
        return new Cobol.Performable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FROM()),
                visit(ctx.identifier(), ctx.literal(), ctx.arithmeticExpression())
        );
    }

    @Override
    public Cobol.PerformInlineStatement visitPerformInlineStatement(CobolParser.PerformInlineStatementContext ctx) {
        return new Cobol.PerformInlineStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol) visit(ctx.performType()),
                convertAllContainer(ctx.statement()),
                (Cobol.CobolWord) visit(ctx.END_PERFORM())
        );
    }

    @Override
    public Cobol.PerformProcedureStatement visitPerformProcedureStatement(CobolParser.PerformProcedureStatementContext ctx) {
        return new Cobol.PerformProcedureStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.ProcedureName) visit(ctx.procedureName(0)),
                ctx.THROUGH() != null ? (Cobol.CobolWord) visit(ctx.THROUGH()) :
                        ctx.THRU() != null ? (Cobol.CobolWord) visit(ctx.THRU()) : null,
                (ctx.procedureName().size() > 1) ? (Cobol.ProcedureName) visit(ctx.procedureName(1)) : null,
                visitNullable(ctx.performType())
        );
    }

    @Override
    public Cobol.Perform visitPerformStatement(CobolParser.PerformStatementContext ctx) {
        return new Cobol.Perform(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.PERFORM()),
                visit(ctx.performInlineStatement(), ctx.performProcedureStatement())
        );
    }

    @Override
    public Cobol.PerformTestClause visitPerformTestClause(CobolParser.PerformTestClauseContext ctx) {
        return new Cobol.PerformTestClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.TEST(), ctx.BEFORE(), ctx.AFTER())
        );
    }

    @Override
    public Cobol.PerformTimes visitPerformTimes(CobolParser.PerformTimesContext ctx) {
        return new Cobol.PerformTimes(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.integerLiteral()),
                (Cobol.CobolWord) visit(ctx.TIMES())
        );
    }

    @Override
    public Cobol.PerformUntil visitPerformUntil(CobolParser.PerformUntilContext ctx) {
        return new Cobol.PerformUntil(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.performTestClause()),
                (Cobol.CobolWord) visit(ctx.UNTIL()),
                (Cobol.Condition) visit(ctx.condition())
        );
    }

    @Override
    public Cobol.PerformVarying visitPerformVarying(CobolParser.PerformVaryingContext ctx) {
        if (ctx.performVaryingClause().getRuleIndex() == ctx.getRuleIndex()) {
            return new Cobol.PerformVarying(
                    randomId(),
                    whitespace(),
                    Markers.EMPTY,
                    (Cobol) visit(ctx.performVaryingClause()),
                    visitNullable(ctx.performTestClause())
            );
        }
        return new Cobol.PerformVarying(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol) visit(ctx.performTestClause()),
                (Cobol) visit(ctx.performVaryingClause())
        );
    }

    @Override
    public Cobol.PerformVaryingClause visitPerformVaryingClause(CobolParser.PerformVaryingClauseContext ctx) {
        return new Cobol.PerformVaryingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.VARYING()),
                (Cobol.PerformVaryingPhrase) visit(ctx.performVaryingPhrase()),
                convertAllContainer(ctx.performAfter())
        );
    }

    @Override
    public Cobol.PerformVaryingPhrase visitPerformVaryingPhrase(CobolParser.PerformVaryingPhraseContext ctx) {
        return new Cobol.PerformVaryingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.PerformFrom) visit(ctx.performFrom()),
                (Cobol.Performable) visit(ctx.performBy()),
                (Cobol.PerformUntil) visit(ctx.performUntil())
        );
    }

    @Override
    public Cobol.Picture visitPicture(CobolParser.PictureContext ctx) {
        return new Cobol.Picture(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.pictureChars()),
                visitNullable(ctx.pictureCardinality())
        );
    }

    @Override
    public Object visitPictureCardinality(CobolParser.PictureCardinalityContext ctx) {
        return new Cobol.Parenthesized(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                singletonList((Cobol) visit(ctx.integerLiteral())),
                (Cobol.CobolWord) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitPictureString(CobolParser.PictureStringContext ctx) {
        return new Cobol.PictureString(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.picture())
        );
    }

    @Override
    public Object visitPlusMinus(CobolParser.PlusMinusContext ctx) {
        return new Cobol.PlusMinus(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.PLUSCHAR(), ctx.MINUSCHAR()),
                (Cobol.MultDivs) visit(ctx.multDivs())
        );
    }

    @Override
    public Object visitPower(CobolParser.PowerContext ctx) {
        return new Cobol.Power(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.DOUBLEASTERISKCHAR()),
                (Cobol) visit(ctx.basis())
        );
    }

    @Override
    public Object visitPowers(CobolParser.PowersContext ctx) {
        return new Cobol.Powers(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                ctx.PLUSCHAR() != null ? (Cobol.CobolWord) visit(ctx.PLUSCHAR()) :
                        ctx.MINUSCHAR() != null ? (Cobol.CobolWord) visit(ctx.MINUSCHAR()) : null,
                (Cobol) visit(ctx.basis()),
                convertAllContainer(ctx.power())
        );
    }

    @Override
    public Object visitProcedureDeclarative(CobolParser.ProcedureDeclarativeContext ctx) {
        return new Cobol.ProcedureDeclarative(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.ProcedureSectionHeader) visit(ctx.procedureSectionHeader()),
                padLeft(sourceBefore("."), (Cobol.UseStatement) visit(ctx.useStatement())),
                padLeft(sourceBefore("."), (Cobol.Paragraphs) visit(ctx.paragraphs()))
        );
    }

    @Override
    public Object visitProcedureDeclaratives(CobolParser.ProcedureDeclarativesContext ctx) {
        return new Cobol.ProcedureDeclaratives(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.DECLARATIVES(0)),
                convertAllContainer(sourceBefore("."), ctx.procedureDeclarative()),
                words(ctx.END(), ctx.DECLARATIVES(1)),
                (Cobol.CobolWord) visit(ctx.DOT_FS(1))
        );
    }

    @Override
    public Cobol.ProcedureDivision visitProcedureDivision(CobolParser.ProcedureDivisionContext ctx) {
        return new Cobol.ProcedureDivision(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PROCEDURE(), ctx.DIVISION()),
                visitNullable(ctx.procedureDivisionUsingClause()),
                visitNullable(ctx.procedureDivisionGivingClause()),
                (Cobol.CobolWord) visit(ctx.DOT_FS()),
                visitNullable(ctx.procedureDeclaratives()),
                padLeft(sourceBefore(""), (Cobol.ProcedureDivisionBody) visit(ctx.procedureDivisionBody()))
        );
    }

    @Override
    public Cobol.ProcedureDivisionBody visitProcedureDivisionBody(CobolParser.ProcedureDivisionBodyContext ctx) {
        return new Cobol.ProcedureDivisionBody(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.Paragraphs) visit(ctx.paragraphs()),
                convertAllContainer(ctx.procedureSection())
        );
    }

    @Override
    public Cobol.ProcedureDivisionByReference visitProcedureDivisionByReference(CobolParser.ProcedureDivisionByReferenceContext ctx) {
        if(ctx.ANY() == null) {
            return new Cobol.ProcedureDivisionByReference(
                    randomId(),
                    whitespace(),
                    Markers.EMPTY,
                    visitNullable(ctx.OPTIONAL()),
                    (ctx.identifier() == null) ? (Name) visit(ctx.fileName()) : (Name) visit(ctx.identifier())
            );
        } else {
            return new Cobol.ProcedureDivisionByReference(
                    randomId(),
                    whitespace(),
                    Markers.EMPTY,
                    (Cobol.CobolWord) visit(ctx.ANY()),
                    null);
        }
    }

    @Override
    public Cobol.ProcedureDivisionByReferencePhrase visitProcedureDivisionByReferencePhrase(CobolParser.ProcedureDivisionByReferencePhraseContext ctx) {
        return new Cobol.ProcedureDivisionByReferencePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BY(), ctx.REFERENCE()),
                convertAll(ctx.procedureDivisionByReference())
        );
    }

    @Override
    public Cobol.ProcedureDivisionByValuePhrase visitProcedureDivisionByValuePhrase(CobolParser.ProcedureDivisionByValuePhraseContext ctx) {
        return new Cobol.ProcedureDivisionByValuePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BY(), ctx.VALUE()),
                convertAll(ctx.procedureDivisionByValue())
        );
    }

    @Override
    public Object visitProcedureDivisionGivingClause(CobolParser.ProcedureDivisionGivingClauseContext ctx) {
        return new Cobol.ProcedureDivisionGivingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.GIVING(), ctx.RETURNING()),
                (Name) visit(ctx.dataName())
        );
    }

    @Override
    public Cobol.ProcedureDivisionUsingClause visitProcedureDivisionUsingClause(CobolParser.ProcedureDivisionUsingClauseContext ctx) {
        return new Cobol.ProcedureDivisionUsingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.USING(), ctx.CHAINING()),
                convertAll(ctx.procedureDivisionUsingParameter())
        );
    }

    @Override
    public Cobol.ProcedureName visitProcedureName(CobolParser.ProcedureNameContext ctx) {
        return new Cobol.ProcedureName(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.paragraphName()),
                visitNullable(ctx.inSection()),
                visitNullable(ctx.sectionName())
        );
    }

    @Override
    public Object visitProcedureSection(CobolParser.ProcedureSectionContext ctx) {
        return new Cobol.ProcedureSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.ProcedureSectionHeader) visit(ctx.procedureSectionHeader()),
                (Cobol.CobolWord) visit(ctx.DOT_FS()),
                (Cobol.Paragraphs) visit(ctx.paragraphs())
        );
    }

    @Override
    public Object visitProcedureSectionHeader(CobolParser.ProcedureSectionHeaderContext ctx) {
        return new Cobol.ProcedureSectionHeader(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.sectionName()),
                (Cobol.CobolWord) visit(ctx.SECTION()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.ProgramIdParagraph visitProgramIdParagraph(CobolParser.ProgramIdParagraphContext ctx) {
        return new Cobol.ProgramIdParagraph(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.PROGRAM_ID()),
                padLeft(sourceBefore("."), (Name) visit(ctx.programName())),
                words(ctx.IS(), ctx.COMMON(), ctx.INITIAL(), ctx.LIBRARY(), ctx.DEFINITION(), ctx.RECURSIVE(), ctx.PROGRAM()),
                ctx.DOT_FS().size() == 1 ? null : visitNullable(ctx.DOT_FS(1))
        );
    }

    @Override
    public Object visitProgramLibrarySection(CobolParser.ProgramLibrarySectionContext ctx) {
        return new Cobol.ProgramLibrarySection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PROGRAM_LIBRARY(), ctx.SECTION(), ctx.DOT_FS()),
                convertAllContainer(ctx.libraryDescriptionEntry())
        );
    }

    @Override
    public Cobol.ProgramUnit visitProgramUnit(CobolParser.ProgramUnitContext ctx) {
        return new Cobol.ProgramUnit(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.IdentificationDivision) visitIdentificationDivision(ctx.identificationDivision()),
                visitNullable(ctx.environmentDivision()),
                visitNullable(ctx.dataDivision()),
                visitNullable(ctx.procedureDivision()),
                convertAllContainer(ctx.programUnit()),
                ctx.endProgramStatement() == null ? null : padRight(visitEndProgramStatement(ctx.endProgramStatement()),
                        sourceBefore("."))
        );
    }

    @Override
    public Cobol.Purge visitPurgeStatement(CobolParser.PurgeStatementContext ctx) {
        return new Cobol.Purge(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(padLeft(ctx.PURGE()), ctx.cdName())
        );
    }

    @Override
    public Object visitQualifiedDataName(CobolParser.QualifiedDataNameContext ctx) {
        return new Cobol.QualifiedDataName(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.qualifiedDataNameFormat1(), ctx.qualifiedDataNameFormat1(), ctx.qualifiedDataNameFormat3(), ctx.qualifiedDataNameFormat4())
        );
    }

    @Override
    public Object visitQualifiedDataNameFormat1(CobolParser.QualifiedDataNameFormat1Context ctx) {
        return new Cobol.QualifiedDataNameFormat1(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.dataName(), ctx.conditionName()),
                convertAllContainer(ctx.qualifiedInData()),
                visitNullable(ctx.inFile())
        );
    }

    @Override
    public Object visitQualifiedDataNameFormat2(CobolParser.QualifiedDataNameFormat2Context ctx) {
        return new Cobol.QualifiedDataNameFormat2(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.paragraphName()),
                (Cobol.InSection) visit(ctx.inSection())
        );
    }

    @Override
    public Object visitQualifiedDataNameFormat3(CobolParser.QualifiedDataNameFormat3Context ctx) {
        return new Cobol.QualifiedDataNameFormat3(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.textName()),
                (Cobol.InLibrary) visit(ctx.inLibrary())
        );
    }

    @Override
    public Object visitQualifiedDataNameFormat4(CobolParser.QualifiedDataNameFormat4Context ctx) {
        return new Cobol.QualifiedDataNameFormat4(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.LINAGE_COUNTER()),
                (Cobol.InFile) visit(ctx.inFile())
        );
    }

    @Override
    public Object visitQualifiedInData(CobolParser.QualifiedInDataContext ctx) {
        return new Cobol.QualifiedDataName(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.inData(), ctx.inTable())
        );
    }

    @Override
    public Cobol.ReadInto visitReadInto(CobolParser.ReadIntoContext ctx) {
        return new Cobol.ReadInto(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INTO()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Cobol.ReadKey visitReadKey(CobolParser.ReadKeyContext ctx) {
        return new Cobol.ReadKey(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.KEY(), ctx.IS()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.Read visitReadStatement(CobolParser.ReadStatementContext ctx) {
        return new Cobol.Read(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.READ()),
                (Name) visit(ctx.fileName()),
                words(ctx.NEXT(), ctx.RECORD()),
                visitNullable(ctx.readInto()),
                visitNullable(ctx.readWith()),
                visitNullable(ctx.readKey()),
                visitNullable(ctx.invalidKeyPhrase()),
                visitNullable(ctx.notInvalidKeyPhrase()),
                visitNullable(ctx.atEndPhrase()),
                visitNullable(ctx.notAtEndPhrase()),
                visitNullable(ctx.END_READ())
        );
    }

    @Override
    public Cobol.ReadWith visitReadWith(CobolParser.ReadWithContext ctx) {
        return new Cobol.ReadWith(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.KEPT(), ctx.NO(), ctx.LOCK(), ctx.WAIT())
        );
    }

    @Override
    public Cobol.Receivable visitReceiveBefore(CobolParser.ReceiveBeforeContext ctx) {
        return new Cobol.Receivable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BEFORE(), ctx.TIME()),
                visit(ctx.numericLiteral(), ctx.identifier())
        );
    }

    @Override
    public Cobol.ReceiveFrom visitReceiveFrom(CobolParser.ReceiveFromContext ctx) {
        return new Cobol.ReceiveFrom(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LAST(), ctx.ANY(), ctx.THREAD()),
                visitNullable(ctx.dataName())
        );
    }

    @Override
    public Cobol.ReceiveFromStatement visitReceiveFromStatement(CobolParser.ReceiveFromStatementContext ctx) {
        return new Cobol.ReceiveFromStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.dataName()),
                (Cobol.CobolWord) visit(ctx.FROM()),
                (Cobol.ReceiveFrom) visit(ctx.receiveFrom()),
                convertAllContainer(ctx.receiveBefore(), ctx.receiveWith(), ctx.receiveThread(), ctx.receiveSize(), ctx.receiveStatus())
        );
    }

    @Override
    public Cobol.ReceiveIntoStatement visitReceiveIntoStatement(CobolParser.ReceiveIntoStatementContext ctx) {
        return new Cobol.ReceiveIntoStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.cdName()),
                words(ctx.MESSAGE(), ctx.SEGMENT(), ctx.INTO()),
                (Identifier) visit(ctx.identifier()),
                visitNullable(ctx.receiveNoData()),
                visitNullable(ctx.receiveWithData())
        );
    }

    @Override
    public Cobol.StatementPhrase visitReceiveNoData(CobolParser.ReceiveNoDataContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NO(), ctx.DATA()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Cobol.Receivable visitReceiveSize(CobolParser.ReceiveSizeContext ctx) {
        return new Cobol.Receivable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SIZE(), ctx.IN()),
                visit(ctx.numericLiteral(), ctx.identifier())
        );
    }

    @Override
    public Cobol.Receive visitReceiveStatement(CobolParser.ReceiveStatementContext ctx) {
        return new Cobol.Receive(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.RECEIVE()),
                visit(ctx.receiveFromStatement(), ctx.receiveIntoStatement()),
                visitNullable(ctx.onExceptionClause()),
                visitNullable(ctx.notOnExceptionClause()),
                visitNullable(ctx.END_RECEIVE())
        );
    }

    @Override
    public Cobol.Receivable visitReceiveStatus(CobolParser.ReceiveStatusContext ctx) {
        return new Cobol.Receivable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.STATUS(), ctx.IN()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Cobol.Receivable visitReceiveThread(CobolParser.ReceiveThreadContext ctx) {
        return new Cobol.Receivable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.THREAD(), ctx.IN()),
                (Name) visit(ctx.dataName())
        );
    }

    @Override
    public Cobol.CobolWord visitReceiveWith(CobolParser.ReceiveWithContext ctx) {
        return words(ctx.WITH(), ctx.NO(), ctx.WAIT());
    }

    @Override
    public Cobol.StatementPhrase visitReceiveWithData(CobolParser.ReceiveWithDataContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.DATA()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitRecordContainsClause(CobolParser.RecordContainsClauseContext ctx) {
        return new Cobol.RecordContainsClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.RECORD()),
                visit(ctx.recordContainsClauseFormat1(), ctx.recordContainsClauseFormat2(), ctx.recordContainsClauseFormat3())
        );
    }

    @Override
    public Object visitRecordContainsClauseFormat1(CobolParser.RecordContainsClauseFormat1Context ctx) {
        return new Cobol.RecordContainsClauseFormat1(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.CONTAINS()),
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                visitNullable(ctx.CHARACTERS())
        );
    }

    @Override
    public Object visitRecordContainsClauseFormat2(CobolParser.RecordContainsClauseFormat2Context ctx) {
        return new Cobol.RecordContainsClauseFormat2(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.VARYING(), ctx.IN(), ctx.SIZE()),
                convertAllList(emptyList(), singletonList(ctx.FROM()), singletonList(ctx.integerLiteral()), singletonList(ctx.recordContainsTo()), singletonList(ctx.CHARACTERS())),
                convertAllList(emptyList(), singletonList(ctx.DEPENDING()), singletonList(ctx.ON()), singletonList(ctx.qualifiedDataName()))
        );
    }

    @Override
    public Object visitRecordContainsClauseFormat3(CobolParser.RecordContainsClauseFormat3Context ctx) {
        return new Cobol.RecordContainsClauseFormat3(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.CONTAINS()),
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                (Cobol.RecordContainsTo) visit(ctx.recordContainsTo()),
                visitNullable(ctx.CHARACTERS())
        );
    }

    @Override
    public Object visitRecordContainsTo(CobolParser.RecordContainsToContext ctx) {
        return new Cobol.RecordContainsTo(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.TO()),
                (Cobol.CobolWord) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitRecordDelimiterClause(CobolParser.RecordDelimiterClauseContext ctx) {
        return new Cobol.RecordDelimiterClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RECORD(), ctx.DELIMITER(), ctx.IS(), ctx.STANDARD_1(), ctx.IMPLICIT()),
                visitNullable(ctx.assignmentName())
        );
    }

    @Override
    public Object visitRecordKeyClause(CobolParser.RecordKeyClauseContext ctx) {
        return new Cobol.RecordKeyClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RECORD(), ctx.KEY(), ctx.IS()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                visitNullable(ctx.passwordClause()),
                words(ctx.WITH(), ctx.DUPLICATES())
        );
    }

    @Override
    public Object visitRecordingModeClause(CobolParser.RecordingModeClauseContext ctx) {
        return new Cobol.RecordingModeClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RECORDING(), ctx.MODE(), ctx.IS()),
                visitNullable(ctx.modeStatement())
        );
    }

    @Override
    public Object visitReferenceModifier(CobolParser.ReferenceModifierContext ctx) {
        return new Cobol.ReferenceModifier(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                (Cobol.ArithmeticExpression) visit(ctx.characterPosition()),
                (Cobol.CobolWord) visit(ctx.COLONCHAR()),
                visitNullable(ctx.length()),
                (Cobol.CobolWord) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitRelationArithmeticComparison(CobolParser.RelationArithmeticComparisonContext ctx) {
        return new Cobol.RelationArithmeticComparison(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.ArithmeticExpression) visit(ctx.arithmeticExpression().get(0)),
                (Cobol.RelationalOperator) visit(ctx.relationalOperator()),
                (Cobol.ArithmeticExpression) visit(ctx.arithmeticExpression().get(1))
        );
    }

    @Override
    public Object visitRelationCombinedComparison(CobolParser.RelationCombinedComparisonContext ctx) {
        return new Cobol.RelationCombinedComparison(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.ArithmeticExpression) visit(ctx.arithmeticExpression()),
                (Cobol.RelationalOperator) visit(ctx.relationalOperator()),
                new Cobol.Parenthesized(
                        randomId(),
                        whitespace(),
                        Markers.EMPTY,
                        (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                        singletonList((Cobol) visit(ctx.relationCombinedCondition())),
                        (Cobol.CobolWord) visit(ctx.RPARENCHAR())
                )
        );
    }

    @Override
    public Object visitRelationCombinedCondition(CobolParser.RelationCombinedConditionContext ctx) {
        return new Cobol.RelationCombinedCondition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllPrefixedList(Arrays.asList("AND", "OR"), ctx.arithmeticExpression())
        );
    }

    @Override
    public Object visitRelationSignCondition(CobolParser.RelationSignConditionContext ctx) {
        return new Cobol.RelationSignCondition(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.ArithmeticExpression) visit(ctx.arithmeticExpression()),
                words(ctx.IS(), ctx.NOT(), ctx.POSITIVE(), ctx.NEGATIVE(), ctx.ZERO())
        );
    }

    @Override
    public Object visitRelationalOperator(CobolParser.RelationalOperatorContext ctx) {
        return new Cobol.RelationalOperator(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.ARE(), ctx.NOT(),
                        ctx.GREATER(), ctx.LESS(), ctx.THAN(), ctx.OR(), ctx.EQUAL(), ctx.TO(),
                        ctx.MORETHANCHAR(), ctx.LESSTHANCHAR(), ctx.EQUALCHAR(), ctx.NOTEQUALCHAR(),
                        ctx.MORETHANOREQUAL(), ctx.LESSTHANOREQUAL()
                )
        );
    }

    @Override
    public Object visitRelativeKeyClause(CobolParser.RelativeKeyClauseContext ctx) {
        return new Cobol.RelativeKeyClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RELATIVE(), ctx.KEY(), ctx.IS()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.Release visitReleaseStatement(CobolParser.ReleaseStatementContext ctx) {
        return new Cobol.Release(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.RELEASE()),
                (Cobol.QualifiedDataName) visit(ctx.recordName()),
                visitNullable(ctx.FROM()),
                visitNullable(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitReportClause(CobolParser.ReportClauseContext ctx) {
        return new Cobol.ReportClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.REPORT(), ctx.IS(), ctx.REPORTS(), ctx.ARE()),
                convertAllContainer(ctx.reportName())
        );
    }

    @Override
    public Object visitReportDescription(CobolParser.ReportDescriptionContext ctx) {
        return new Cobol.ReportDescription(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.ReportDescriptionEntry) visit(ctx.reportDescriptionEntry()),
                convertAllContainer(ctx.reportGroupDescriptionEntry())
        );
    }

    @Override
    public Object visitReportDescriptionEntry(CobolParser.ReportDescriptionEntryContext ctx) {
        return new Cobol.ReportDescriptionEntry(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.RD()),
                (Cobol.QualifiedDataName) visit(ctx.reportName()),
                visitNullable(ctx.reportDescriptionGlobalClause()),
                visitNullable(ctx.reportDescriptionPageLimitClause()),
                visitNullable(ctx.reportDescriptionHeadingClause()),
                visitNullable(ctx.reportDescriptionFirstDetailClause()),
                visitNullable(ctx.reportDescriptionLastDetailClause()),
                visitNullable(ctx.reportDescriptionFootingClause()),
                (Cobol.CobolWord) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitReportDescriptionFirstDetailClause(CobolParser.ReportDescriptionFirstDetailClauseContext ctx) {
        return new Cobol.ReportDescriptionFirstDetailClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FIRST(), ctx.DETAIL()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportDescriptionFootingClause(CobolParser.ReportDescriptionFootingClauseContext ctx) {
        return new Cobol.ReportDescriptionFootingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FOOTING()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportDescriptionGlobalClause(CobolParser.ReportDescriptionGlobalClauseContext ctx) {
        return new Cobol.ReportDescriptionGlobalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.IS(), ctx.GLOBAL())
        );
    }

    @Override
    public Object visitReportDescriptionHeadingClause(CobolParser.ReportDescriptionHeadingClauseContext ctx) {
        return new Cobol.ReportDescriptionHeadingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.HEADING()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportDescriptionLastDetailClause(CobolParser.ReportDescriptionLastDetailClauseContext ctx) {
        return new Cobol.ReportDescriptionLastDetailClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LAST(), ctx.DETAIL()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportDescriptionPageLimitClause(CobolParser.ReportDescriptionPageLimitClauseContext ctx) {
        return new Cobol.ReportDescriptionPageLimitClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PAGE(), ctx.LIMIT(), ctx.IS(), ctx.LIMITS(), ctx.ARE()),
                (Name) visit(ctx.integerLiteral()),
                visit(ctx.LINE(), ctx.LINES())
        );
    }

    @Override
    public Object visitReportGroupBlankWhenZeroClause(CobolParser.ReportGroupBlankWhenZeroClauseContext ctx) {
        return new Cobol.ReportGroupBlankWhenZeroClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BLANK(), ctx.WHEN(), ctx.ZERO())
        );
    }

    @Override
    public Object visitReportGroupColumnNumberClause(CobolParser.ReportGroupColumnNumberClauseContext ctx) {
        return new Cobol.ReportGroupColumnNumberClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.COLUMN(), ctx.NUMBER(), ctx.IS()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportGroupDescriptionEntryFormat1(CobolParser.ReportGroupDescriptionEntryFormat1Context ctx) {
        return new Cobol.ReportGroupDescriptionEntryFormat1(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                (Cobol.CobolWord) visit(ctx.dataName()),
                visitNullable(ctx.reportGroupLineNumberClause()),
                visitNullable(ctx.reportGroupNextGroupClause()),
                (Cobol.ReportGroupTypeClause) visit(ctx.reportGroupTypeClause()),
                visitNullable(ctx.reportGroupUsageClause()),
                (Cobol.CobolWord) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitReportGroupDescriptionEntryFormat2(CobolParser.ReportGroupDescriptionEntryFormat2Context ctx) {
        return new Cobol.ReportGroupDescriptionEntryFormat2(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                visitNullable(ctx.dataName()),
                visitNullable(ctx.reportGroupLineNumberClause()),
                (Cobol.ReportGroupUsageClause) visit(ctx.reportGroupUsageClause()),
                (Cobol.CobolWord) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitReportGroupDescriptionEntryFormat3(CobolParser.ReportGroupDescriptionEntryFormat3Context ctx) {
        return new Cobol.ReportGroupDescriptionEntryFormat3(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                visitNullable(ctx.dataName()),
                convertAllContainer(ctx.reportGroupPictureClause(),
                        ctx.reportGroupUsageClause(),
                        ctx.reportGroupSignClause(),
                        ctx.reportGroupJustifiedClause(),
                        ctx.reportGroupBlankWhenZeroClause(),
                        ctx.reportGroupLineNumberClause(),
                        ctx.reportGroupColumnNumberClause(),
                        ctx.reportGroupSourceClause(),
                        ctx.reportGroupValueClause(),
                        ctx.reportGroupSumClause(),
                        ctx.reportGroupResetClause(),
                        ctx.reportGroupIndicateClause()),
                (Cobol.CobolWord) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitReportGroupIndicateClause(CobolParser.ReportGroupIndicateClauseContext ctx) {
        return new Cobol.ReportGroupIndicateClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.GROUP(), ctx.INDICATE())
        );
    }

    @Override
    public Object visitReportGroupJustifiedClause(CobolParser.ReportGroupJustifiedClauseContext ctx) {
        return new Cobol.ReportGroupJustifiedClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.JUSTIFIED(), ctx.JUST(), ctx.RIGHT())
        );
    }

    @Override
    public Object visitReportGroupLineNumberClause(CobolParser.ReportGroupLineNumberClauseContext ctx) {
        return new Cobol.ReportGroupLineNumberClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LINE(), ctx.NUMBER(), ctx.IS()),
                visit(ctx.reportGroupLineNumberNextPage(), ctx.reportGroupLineNumberPlus())
        );
    }

    @Override
    public Object visitReportGroupLineNumberNextPage(CobolParser.ReportGroupLineNumberNextPageContext ctx) {
        return new Cobol.ReportGroupLineNumberNextPage(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                words(ctx.ON(), ctx.NEXT(), ctx.PAGE())
        );
    }

    @Override
    public Object visitReportGroupLineNumberPlus(CobolParser.ReportGroupLineNumberPlusContext ctx) {
        return new Cobol.ReportGroupLineNumberPlus(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.PLUS()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportGroupNextGroupClause(CobolParser.ReportGroupNextGroupClauseContext ctx) {
        return new Cobol.ReportGroupNextGroupClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NEXT(), ctx.GROUP(), ctx.IS()),
                visit(ctx.integerLiteral(), ctx.reportGroupNextGroupNextPage(), ctx.reportGroupNextGroupPlus())
        );
    }

    @Override
    public Object visitReportGroupNextGroupNextPage(CobolParser.ReportGroupNextGroupNextPageContext ctx) {
        return new Cobol.ReportGroupNextGroupNextPage(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NEXT(), ctx.PAGE())
        );
    }

    @Override
    public Object visitReportGroupNextGroupPlus(CobolParser.ReportGroupNextGroupPlusContext ctx) {
        return new Cobol.ReportGroupNextGroupPlus(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.PLUS()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportGroupPictureClause(CobolParser.ReportGroupPictureClauseContext ctx) {
        return new Cobol.ReportGroupPictureClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PICTURE(), ctx.PIC(), ctx.IS()),
                (Cobol.PictureString) visit(ctx.pictureString())
        );
    }

    @Override
    public Object visitReportGroupResetClause(CobolParser.ReportGroupResetClauseContext ctx) {
        return new Cobol.ReportGroupResetClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RESET(), ctx.ON(), ctx.FINAL()),
                visitNullable(ctx.dataName())
        );
    }

    @Override
    public Object visitReportGroupSignClause(CobolParser.ReportGroupSignClauseContext ctx) {
        return new Cobol.ReportGroupSignClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SIGN(), ctx.IS(), ctx.LEADING(), ctx.TRAILING(), ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Object visitReportGroupSourceClause(CobolParser.ReportGroupSourceClauseContext ctx) {
        return new Cobol.ReportGroupSourceClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SOURCE(), ctx.IS()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitReportGroupSumClause(CobolParser.ReportGroupSumClauseContext ctx) {
        return new Cobol.ReportGroupSumClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.SUM()),
                convertAllList(singletonList(","), ctx.identifier()),
                visitNullable(ctx.UPON()),
                convertAllPrefixedList(singletonList(","), ctx.dataName())
        );
    }

    @Override
    public Object visitReportGroupTypeClause(CobolParser.ReportGroupTypeClauseContext ctx) {
        return new Cobol.ReportGroupTypeClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.TYPE(), ctx.IS()),
                visit(ctx.reportGroupTypeReportHeading(),
                        ctx.reportGroupTypeReportFooting(),
                        ctx.reportGroupTypePageHeading(),
                        ctx.reportGroupTypePageFooting(),
                        ctx.reportGroupTypeControlHeading(),
                        ctx.reportGroupTypeControlFooting(),
                        ctx.reportGroupTypeDetail())
        );
    }

    @Override
    public Object visitReportGroupTypeControlFooting(CobolParser.ReportGroupTypeControlFootingContext ctx) {
        return new Cobol.ReportGroupTypeControlFooting(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.CONTROL(), ctx.FOOTING(), ctx.CF(), ctx.FINAL()),
                visitNullable(ctx.dataName())
        );
    }

    @Override
    public Object visitReportGroupTypeControlHeading(CobolParser.ReportGroupTypeControlHeadingContext ctx) {
        return new Cobol.ReportGroupTypeControlHeading(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.CONTROL(), ctx.HEADING(), ctx.CH(), ctx.FINAL()),
                visitNullable(ctx.dataName())
        );
    }

    @Override
    public Object visitReportGroupTypeDetail(CobolParser.ReportGroupTypeDetailContext ctx) {
        return new Cobol.ReportGroupTypeDetail(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DETAIL(), ctx.DE())
        );
    }

    @Override
    public Object visitReportGroupTypePageFooting(CobolParser.ReportGroupTypePageFootingContext ctx) {
        return new Cobol.ReportGroupTypePageFooting(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PAGE(), ctx.FOOTING(), ctx.PF())
        );
    }

    @Override
    public Object visitReportGroupTypePageHeading(CobolParser.ReportGroupTypePageHeadingContext ctx) {
        return new Cobol.ReportGroupTypePageHeading(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PAGE(), ctx.HEADING(), ctx.PH())
        );
    }

    @Override
    public Object visitReportGroupTypeReportFooting(CobolParser.ReportGroupTypeReportFootingContext ctx) {
        return new Cobol.ReportGroupTypeReportFooting(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.REPORT(), ctx.FOOTING(), ctx.RF())
        );
    }

    @Override
    public Object visitReportGroupTypeReportHeading(CobolParser.ReportGroupTypeReportHeadingContext ctx) {
        return new Cobol.ReportGroupTypeReportHeading(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.REPORT(), ctx.HEADING(), ctx.RH())
        );
    }

    @Override
    public Object visitReportGroupUsageClause(CobolParser.ReportGroupUsageClauseContext ctx) {
        return new Cobol.ReportGroupUsageClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.USAGE(), ctx.IS(), ctx.DISPLAY(), ctx.DISPLAY_1())
        );
    }

    @Override
    public Object visitReportGroupValueClause(CobolParser.ReportGroupValueClauseContext ctx) {
        return new Cobol.ReportGroupValueClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.VALUE(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitReportSection(CobolParser.ReportSectionContext ctx) {
        return new Cobol.ReportSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.REPORT(), ctx.SECTION(), ctx.DOT_FS()),
                convertAllContainer(ctx.reportDescription())
        );
    }

    @Override
    public Object visitRerunClause(CobolParser.RerunClauseContext ctx) {
        return new Cobol.RerunClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.RERUN()),
                visitNullable(ctx.ON()),
                visit(ctx.assignmentName(), ctx.fileName()),
                (Cobol.CobolWord) visit(ctx.EVERY()),
                visit(ctx.rerunEveryRecords(), ctx.rerunEveryOf(), ctx.rerunEveryClock())
        );
    }

    @Override
    public Object visitRerunEveryClock(CobolParser.RerunEveryClockContext ctx) {
        return new Cobol.RerunEveryClock(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                visitNullable(ctx.CLOCK_UNITS())
        );
    }

    @Override
    public Object visitRerunEveryOf(CobolParser.RerunEveryOfContext ctx) {
        return new Cobol.RerunEveryOf(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.END(), ctx.OF().size() == 1 ? null : ctx.OF(0), ctx.REEL(), ctx.UNIT(), ctx.OF(ctx.OF().size() == 1 ? 0 : 1)),
                (Cobol.CobolWord) visit(ctx.fileName())
        );
    }

    @Override
    public Object visitRerunEveryRecords(CobolParser.RerunEveryRecordsContext ctx) {
        return new Cobol.RerunEveryRecords(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                (Cobol.CobolWord) visit(ctx.RECORDS())
        );
    }

    @Override
    public Object visitReserveClause(CobolParser.ReserveClauseContext ctx) {
        return new Cobol.ReserveClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllList(emptyList(), singletonList(ctx.RESERVE()), singletonList(ctx.NO()),
                        singletonList(ctx.integerLiteral()), singletonList(ctx.ALTERNATE()),
                        singletonList(ctx.AREA()), singletonList(ctx.AREAS()))
        );
    }

    @Override
    public Cobol.ReserveNetworkClause visitReserveNetworkClause(CobolParser.ReserveNetworkClauseContext ctx) {
        return new Cobol.ReserveNetworkClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RESERVE(), ctx.WORDS(), ctx.LIST(), ctx.IS(), ctx.NETWORK(), ctx.CAPABLE())
        );
    }

    @Override
    public Cobol.ReturnInto visitReturnInto(CobolParser.ReturnIntoContext ctx) {
        return new Cobol.ReturnInto(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INTO()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.Return visitReturnStatement(CobolParser.ReturnStatementContext ctx) {
        return new Cobol.Return(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.RETURN()),
                (Name) visit(ctx.fileName()),
                visitNullable(ctx.RECORD()),
                visitNullable(ctx.returnInto()),
                (Cobol.StatementPhrase) visit(ctx.atEndPhrase()),
                (Cobol.StatementPhrase) visit(ctx.notAtEndPhrase()),
                visitNullable(ctx.END_RETURN())
        );
    }

    @Override
    public Object visitRewriteFrom(CobolParser.RewriteFromContext ctx) {
        return new Cobol.RewriteFrom(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FROM()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitRewriteStatement(CobolParser.RewriteStatementContext ctx) {
        return new Cobol.Rewrite(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.REWRITE()),
                visitNullable(ctx.recordName()),
                visitNullable(ctx.invalidKeyPhrase()),
                visitNullable(ctx.notInvalidKeyPhrase()),
                visitNullable(ctx.END_REWRITE())
        );
    }

    @Override
    public Cobol.Roundable visitRoundable(CobolParser.RoundableContext ctx) {
        return new Cobol.Roundable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Identifier) visit(ctx.identifier()),
                visitNullable(ctx.ROUNDED())
        );
    }

    @Override
    public Object visitSameClause(CobolParser.SameClauseContext ctx) {
        return new Cobol.SameClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SAME(), ctx.RECORD(), ctx.SORT(), ctx.SORT_MERGE(), ctx.AREA(), ctx.FOR()),
                convertAllContainer(ctx.fileName())
        );
    }

    @Override
    public Object visitScreenDescriptionAutoClause(CobolParser.ScreenDescriptionAutoClauseContext ctx) {
        return new Cobol.ScreenDescriptionAutoClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.AUTO(), ctx.AUTO_SKIP())
        );
    }

    @Override
    public Object visitScreenDescriptionBackgroundColorClause(CobolParser.ScreenDescriptionBackgroundColorClauseContext ctx) {
        return new Cobol.ScreenDescriptionBackgroundColorClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.BACKGROUND_COLOR(), ctx.BACKGROUND_COLOUR()),
                visitNullable(ctx.IS()),
                visit(ctx.identifier(), ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionBellClause(CobolParser.ScreenDescriptionBellClauseContext ctx) {
        return new Cobol.ScreenDescriptionBellClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BELL(), ctx.BEEP())
        );
    }

    @Override
    public Object visitScreenDescriptionBlankClause(CobolParser.ScreenDescriptionBlankClauseContext ctx) {
        return new Cobol.ScreenDescriptionBlankClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BLANK(), ctx.SCREEN(), ctx.LINE())
        );
    }

    @Override
    public Object visitScreenDescriptionBlankWhenZeroClause(CobolParser.ScreenDescriptionBlankWhenZeroClauseContext ctx) {
        return new Cobol.ScreenDescriptionBlankWhenZeroClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BLANK(), ctx.WHEN(), ctx.ZERO())
        );
    }

    @Override
    public Object visitScreenDescriptionBlinkClause(CobolParser.ScreenDescriptionBlinkClauseContext ctx) {
        return new Cobol.ScreenDescriptionBlinkClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.BLINK())
        );
    }

    @Override
    public Object visitScreenDescriptionColumnClause(CobolParser.ScreenDescriptionColumnClauseContext ctx) {
        return new Cobol.ScreenDescriptionColumnClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.COLUMN(), ctx.COL(), ctx.NUMBER(), ctx.IS(), ctx.PLUS(), ctx.PLUSCHAR(), ctx.MINUSCHAR()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitScreenDescriptionControlClause(CobolParser.ScreenDescriptionControlClauseContext ctx) {
        return new Cobol.ScreenDescriptionControlClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.CONTROL(), ctx.IS()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionEntry(CobolParser.ScreenDescriptionEntryContext ctx) {
        return new Cobol.ScreenDescriptionEntry(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.INTEGERLITERAL()),
                ctx.FILLER() != null ? visitNullable(ctx.FILLER()) :
                        ctx.screenName() != null ? visitNullable(ctx.screenName()) : null,
                convertAllContainer(ctx.screenDescriptionBlankClause(),
                        ctx.screenDescriptionAutoClause(),
                        ctx.screenDescriptionBellClause(),
                        ctx.screenDescriptionBlinkClause(),
                        ctx.screenDescriptionBlankWhenZeroClause(),
                        ctx.screenDescriptionBackgroundColorClause(),
                        ctx.screenDescriptionEraseClause(),
                        ctx.screenDescriptionLightClause(),
                        ctx.screenDescriptionGridClause(),
                        ctx.screenDescriptionReverseVideoClause(),
                        ctx.screenDescriptionUnderlineClause(),
                        ctx.screenDescriptionSizeClause(),
                        ctx.screenDescriptionLineClause(),
                        ctx.screenDescriptionColumnClause(),
                        ctx.screenDescriptionForegroundColorClause(),
                        ctx.screenDescriptionControlClause(),
                        ctx.screenDescriptionValueClause(),
                        ctx.screenDescriptionPictureClause(),
                        ctx.screenDescriptionFromClause(),
                        ctx.screenDescriptionUsingClause(),
                        ctx.screenDescriptionUsageClause(),
                        ctx.screenDescriptionJustifiedClause(),
                        ctx.screenDescriptionSignClause(),
                        ctx.screenDescriptionSecureClause(),
                        ctx.screenDescriptionRequiredClause(),
                        ctx.screenDescriptionPromptClause(),
                        ctx.screenDescriptionFullClause(),
                        ctx.screenDescriptionZeroFillClause())
                        .withLastSpace(sourceBefore("."))
        );
    }

    @Override
    public Object visitScreenDescriptionEraseClause(CobolParser.ScreenDescriptionEraseClauseContext ctx) {
        return new Cobol.ScreenDescriptionEraseClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ERASE(), ctx.EOL(), ctx.EOS())
        );
    }

    @Override
    public Object visitScreenDescriptionForegroundColorClause(CobolParser.ScreenDescriptionForegroundColorClauseContext ctx) {
        return new Cobol.ScreenDescriptionForegroundColorClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FOREGROUND_COLOR(), ctx.FOREGROUND_COLOUR(), ctx.IS()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitScreenDescriptionFromClause(CobolParser.ScreenDescriptionFromClauseContext ctx) {
        return new Cobol.ScreenDescriptionFromClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FROM()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.screenDescriptionToClause())
        );
    }

    @Override
    public Object visitScreenDescriptionFullClause(CobolParser.ScreenDescriptionFullClauseContext ctx) {
        return new Cobol.ScreenDescriptionFullClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FULL(), ctx.LENGTH_CHECK())
        );
    }

    @Override
    public Object visitScreenDescriptionGridClause(CobolParser.ScreenDescriptionGridClauseContext ctx) {
        return new Cobol.ScreenDescriptionGridClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.GRID(), ctx.LEFTLINE(), ctx.OVERLINE())
        );
    }

    @Override
    public Object visitScreenDescriptionJustifiedClause(CobolParser.ScreenDescriptionJustifiedClauseContext ctx) {
        return new Cobol.ScreenDescriptionJustifiedClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.JUSTIFIED(), ctx.JUST(), ctx.RIGHT())
        );
    }

    @Override
    public Object visitScreenDescriptionLightClause(CobolParser.ScreenDescriptionLightClauseContext ctx) {
        return new Cobol.ScreenDescriptionLightClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.HIGHLIGHT(), ctx.LOWLIGHT())
        );
    }

    @Override
    public Object visitScreenDescriptionLineClause(CobolParser.ScreenDescriptionLineClauseContext ctx) {
        return new Cobol.ScreenDescriptionLineClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.LINE(), ctx.NUMBER(), ctx.IS(), ctx.PLUS(), ctx.PLUSCHAR(), ctx.MINUSCHAR()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitScreenDescriptionPictureClause(CobolParser.ScreenDescriptionPictureClauseContext ctx) {
        return new Cobol.ScreenDescriptionPictureClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PICTURE(), ctx.PIC(), ctx.IS()),
                (Cobol.PictureString) visit(ctx.pictureString())
        );
    }

    @Override
    public Object visitScreenDescriptionPromptClause(CobolParser.ScreenDescriptionPromptClauseContext ctx) {
        return new Cobol.ScreenDescriptionPromptClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.PROMPT(), ctx.CHARACTER(), ctx.IS()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.screenDescriptionPromptOccursClause())
        );
    }

    @Override
    public Object visitScreenDescriptionPromptOccursClause(CobolParser.ScreenDescriptionPromptOccursClauseContext ctx) {
        return new Cobol.ScreenDescriptionPromptOccursClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.OCCURS()),
                (Cobol.CobolWord) visit(ctx.integerLiteral()),
                visitNullable(ctx.TIMES())
        );
    }

    @Override
    public Object visitScreenDescriptionRequiredClause(CobolParser.ScreenDescriptionRequiredClauseContext ctx) {
        return new Cobol.ScreenDescriptionRequiredClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.REQUIRED(), ctx.EMPTY_CHECK())
        );
    }

    @Override
    public Object visitScreenDescriptionReverseVideoClause(CobolParser.ScreenDescriptionReverseVideoClauseContext ctx) {
        return new Cobol.ScreenDescriptionReverseVideoClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.REVERSE_VIDEO())
        );
    }

    @Override
    public Object visitScreenDescriptionSignClause(CobolParser.ScreenDescriptionSignClauseContext ctx) {
        return new Cobol.ScreenDescriptionSignClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SIGN(), ctx.IS(), ctx.LEADING(), ctx.TRAILING(), ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Object visitScreenDescriptionSizeClause(CobolParser.ScreenDescriptionSizeClauseContext ctx) {
        return new Cobol.ScreenDescriptionSizeClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SIZE(), ctx.IS()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionToClause(CobolParser.ScreenDescriptionToClauseContext ctx) {
        return new Cobol.ScreenDescriptionToClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.TO()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionUnderlineClause(CobolParser.ScreenDescriptionUnderlineClauseContext ctx) {
        return new Cobol.ScreenDescriptionUnderlineClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.UNDERLINE())
        );
    }

    @Override
    public Object visitScreenDescriptionUsageClause(CobolParser.ScreenDescriptionUsageClauseContext ctx) {
        return new Cobol.ScreenDescriptionUsageClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.USAGE(), ctx.IS(), ctx.DISPLAY(), ctx.DISPLAY_1())
        );
    }

    @Override
    public Object visitScreenDescriptionUsingClause(CobolParser.ScreenDescriptionUsingClauseContext ctx) {
        return new Cobol.ScreenDescriptionUsingClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.USING()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionValueClause(CobolParser.ScreenDescriptionValueClauseContext ctx) {
        return new Cobol.ScreenDescriptionValueClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.VALUE(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitScreenDescriptionZeroFillClause(CobolParser.ScreenDescriptionZeroFillClauseContext ctx) {
        return new Cobol.ScreenDescriptionZeroFillClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.ZERO_FILL())
        );
    }

    @Override
    public Object visitScreenSection(CobolParser.ScreenSectionContext ctx) {
        return new Cobol.ScreenSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SCREEN(), ctx.SECTION()),
                convertAllContainer(sourceBefore("."), ctx.screenDescriptionEntry())
        );
    }

    @Override
    public Cobol.Search visitSearchStatement(CobolParser.SearchStatementContext ctx) {
        return new Cobol.Search(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SEARCH(), ctx.ALL()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                visitNullable(ctx.searchVarying()),
                visitNullable(ctx.atEndPhrase()),
                convertAllContainer(ctx.searchWhen()),
                visitNullable(ctx.END_SEARCH())
        );
    }

    @Override
    public Cobol.SearchVarying visitSearchVarying(CobolParser.SearchVaryingContext ctx) {
        return new Cobol.SearchVarying(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.VARYING()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.SearchWhen visitSearchWhen(CobolParser.SearchWhenContext ctx) {
        return new Cobol.SearchWhen(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.WHEN()),
                (Cobol.Condition) visit(ctx.condition()),
                words(ctx.NEXT(), ctx.SENTENCE()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Cobol.ValuedObjectComputerClause visitSegmentLimitClause(CobolParser.SegmentLimitClauseContext ctx) {
        return new Cobol.ValuedObjectComputerClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                Cobol.ValuedObjectComputerClause.Type.SegmentLimit,
                words(ctx.SEGMENT_LIMIT(), ctx.IS()),
                (Cobol) visit(ctx.integerLiteral()),
                null
        );
    }

    @Override
    public Object visitSelectClause(CobolParser.SelectClauseContext ctx) {
        return new Cobol.SelectClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SELECT(), ctx.OPTIONAL()),
                (Cobol.CobolWord) visit(ctx.fileName())
        );
    }

    @Override
    public Cobol.SendAdvancingLines visitSendAdvancingLines(CobolParser.SendAdvancingLinesContext ctx) {
        return new Cobol.SendAdvancingLines(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.CobolWord) visit(ctx.LINE(), ctx.LINES())
        );
    }

    @Override
    public Cobol.SendPhrase visitSendAdvancingPhrase(CobolParser.SendAdvancingPhraseContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BEFORE(), ctx.AFTER(), ctx.ADVANCING()),
                visit(ctx.sendAdvancingPage(), ctx.sendAdvancingLines(), ctx.sendAdvancingMnemonic())
        );
    }

    @Override
    public Cobol.SendPhrase visitSendFromPhrase(CobolParser.SendFromPhraseContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FROM()),
                (Cobol) visit(ctx.identifier())
        );
    }

    @Override
    public Cobol.SendPhrase visitSendReplacingPhrase(CobolParser.SendReplacingPhraseContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.REPLACING(), ctx.LINE()),
                null
        );
    }

    @Override
    public Cobol.Send visitSendStatement(CobolParser.SendStatementContext ctx) {
        return new Cobol.Send(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.SEND()),
                visit(ctx.sendStatementSync(), ctx.sendStatementAsync()),
                visitNullable(ctx.onExceptionClause()),
                visitNullable(ctx.notOnExceptionClause())
        );
    }

    @Override
    public Cobol.SendPhrase visitSendStatementAsync(CobolParser.SendStatementAsyncContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.TO(), ctx.TOP(), ctx.BOTTOM()),
                (Cobol) visit(ctx.identifier())
        );
    }

    @Override
    public Cobol.SendStatementSync visitSendStatementSync(CobolParser.SendStatementSyncContext ctx) {
        return new Cobol.SendStatementSync(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.sendFromPhrase()),
                visitNullable(ctx.sendWithPhrase()),
                visitNullable(ctx.sendReplacingPhrase()),
                visitNullable(ctx.sendAdvancingPhrase())
        );
    }

    @Override
    public Cobol.SendPhrase visitSendWithPhrase(CobolParser.SendWithPhraseContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.EGI(), ctx.EMI(), ctx.ESI()),
                visitNullable(ctx.identifier())
        );
    }

    @Override
    public Cobol.Sentence visitSentence(CobolParser.SentenceContext ctx) {
        return new Cobol.Sentence(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAll(ctx.statement()),
                (Cobol.CobolWord) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Cobol.Set visitSetStatement(CobolParser.SetStatementContext ctx) {
        return new Cobol.Set(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.SET()),
                convertAllContainer(ctx.setToStatement()),
                visitNullable(ctx.setUpDownByStatement())
        );
    }

    @Override
    public Cobol.SetTo visitSetToStatement(CobolParser.SetToStatementContext ctx) {
        return new Cobol.SetTo(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.setTo()),
                convertAllContainer(padLeft(ctx.TO()), ctx.setToValue())
        );
    }

    @Override
    public Cobol.SetUpDown visitSetUpDownByStatement(CobolParser.SetUpDownByStatementContext ctx) {
        return new Cobol.SetUpDown(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.setTo()),
                words(ctx.DOWN(), ctx.UP(), ctx.BY()),
                (Name) visit(ctx.setByValue())
        );
    }

    @Override
    public Object visitSimpleCondition(CobolParser.SimpleConditionContext ctx) {
        return ctx.condition() != null ? new Cobol.Parenthesized(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                singletonList((Cobol) visit(ctx.condition())),
                (Cobol.CobolWord) visit(ctx.RPARENCHAR())
        ) : visit(ctx.relationCondition(), ctx.classCondition(), ctx.conditionNameReference());
    }

    @Override
    public Cobol.SortProcedurePhrase visitSortCollatingAlphanumeric(CobolParser.SortCollatingAlphanumericContext ctx) {
        return new Cobol.SortProcedurePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FOR(), ctx.ALPHANUMERIC(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.alphabetName()),
                null
        );
    }

    @Override
    public Cobol.SortProcedurePhrase visitSortCollatingNational(CobolParser.SortCollatingNationalContext ctx) {
        return new Cobol.SortProcedurePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FOR(), ctx.NATIONAL(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.alphabetName()),
                null
        );
    }

    @Override
    public Cobol.SortCollatingSequencePhrase visitSortCollatingSequencePhrase(CobolParser.SortCollatingSequencePhraseContext ctx) {
        return new Cobol.SortCollatingSequencePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.COLLATING(), ctx.SEQUENCE(), ctx.IS()),
                convertAllContainer(ctx.alphabetName()),
                visitNullable(ctx.sortCollatingAlphanumeric()),
                visitNullable(ctx.sortCollatingNational())
        );
    }

    @Override
    public Cobol.Sortable visitSortDuplicatesPhrase(CobolParser.SortDuplicatesPhraseContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.DUPLICATES(), ctx.IN(), ctx.ORDER()),
                convertAllContainer(Collections.emptyList())
        );
    }

    @Override
    public Cobol.SortGiving visitSortGiving(CobolParser.SortGivingContext ctx) {
        return new Cobol.SortGiving(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.fileName()),
                words(ctx.LOCK(), ctx.SAVE(), ctx.NO(), ctx.REWIND(), ctx.RELEASE(), ctx.WITH(), ctx.REMOVE(), ctx.CRUNCH())
        );
    }

    @Override
    public Cobol.Sortable visitSortGivingPhrase(CobolParser.SortGivingPhraseContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.GIVING()),
                convertAllContainer(ctx.sortGiving())
        );
    }

    @Override
    public Cobol.SortProcedurePhrase visitSortInputProcedurePhrase(CobolParser.SortInputProcedurePhraseContext ctx) {
        return new Cobol.SortProcedurePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.INPUT(), ctx.PROCEDURE(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.procedureName()),
                null
        );
    }

    @Override
    public Cobol.Sortable visitSortInputThrough(CobolParser.SortInputThroughContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.THROUGH(), ctx.THRU()),
                convertAllContainer(Collections.singletonList(ctx.procedureName()))
        );
    }

    @Override
    public Cobol.Sortable visitSortOnKeyClause(CobolParser.SortOnKeyClauseContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ON(), ctx.ASCENDING(), ctx.DESCENDING(), ctx.KEY()),
                convertAllContainer(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.SortProcedurePhrase visitSortOutputProcedurePhrase(CobolParser.SortOutputProcedurePhraseContext ctx) {
        return new Cobol.SortProcedurePhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.OUTPUT(), ctx.PROCEDURE(), ctx.IS()),
                visitNullable(ctx.procedureName()),
                visitNullable(ctx.sortOutputThrough())
        );
    }

    @Override
    public Cobol.Sortable visitSortOutputThrough(CobolParser.SortOutputThroughContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.THROUGH(), ctx.THRU()),
                convertAllContainer(Collections.singletonList(ctx.procedureName()))
        );
    }

    @Override
    public Cobol.Sort visitSortStatement(CobolParser.SortStatementContext ctx) {
        return new Cobol.Sort(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.SORT()),
                visitNullable(ctx.fileName()),
                convertAllContainer(ctx.sortOnKeyClause()),
                visitNullable(ctx.sortDuplicatesPhrase()),
                visitNullable(ctx.sortCollatingSequencePhrase()),
                visitNullable(ctx.sortInputProcedurePhrase()),
                convertAllContainer(ctx.sortUsing()),
                visitNullable(ctx.sortOutputProcedurePhrase()),
                convertAllContainer(ctx.sortGivingPhrase())
        );
    }

    @Override
    public Cobol.Sortable visitSortUsing(CobolParser.SortUsingContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.USING()),
                convertAllContainer(ctx.fileName())
        );
    }

    @Override
    public Cobol.SourceComputer visitSourceComputerParagraph(CobolParser.SourceComputerParagraphContext ctx) {
        return new Cobol.SourceComputer(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SOURCE_COMPUTER(), ctx.DOT_FS(0)),
                ctx.computerName() == null ? null : new Cobol.SourceComputerDefinition(
                        randomId(),
                        whitespace(),
                        Markers.EMPTY,
                        (Cobol.CobolWord) visit(ctx.computerName()),
                        words(ctx.WITH(), ctx.DEBUGGING(), ctx.MODE())
                ),
                ctx.DOT_FS().size() == 1 ? null : (Cobol.CobolWord) visit(ctx.DOT_FS(1))
        );
    }

    @Override
    public Cobol.SpecialNames visitSpecialNamesParagraph(CobolParser.SpecialNamesParagraphContext ctx) {
        return new Cobol.SpecialNames(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.SPECIAL_NAMES()),
                ctx.specialNameClause() == null ?
                        convertAllContainer(sourceBefore("."), emptyList()) :
                        convertAllContainer(sourceBefore("."), ctx.specialNameClause())
                                .withLastSpace(sourceBefore("."))
        );
    }

    @Override
    public Object visitStartKey(CobolParser.StartKeyContext ctx) {
        return new Cobol.StartKey(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.KEY(), ctx.IS(),
                        ctx.NOT(), ctx.GREATER(), ctx.LESS(), ctx.THAN(), ctx.OR(), ctx.EQUAL(), ctx.TO(),
                        ctx.MORETHANCHAR(), ctx.LESSTHANCHAR(), ctx.MORETHANOREQUAL(), ctx.EQUALCHAR()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitStartStatement(CobolParser.StartStatementContext ctx) {
        return new Cobol.Start(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.START()),
                (Cobol.CobolWord) visit(ctx.fileName()),
                visitNullable(ctx.startKey()),
                visitNullable(ctx.invalidKeyPhrase()),
                visitNullable(ctx.notInvalidKeyPhrase()),
                visitNullable(ctx.END_START())
        );
    }

    @Override
    public Object visitStatusKeyClause(CobolParser.StatusKeyClauseContext ctx) {
        return new Cobol.StatusKeyClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.STATUS(), ctx.KEY(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Cobol.Stop visitStopStatement(CobolParser.StopStatementContext ctx) {
        return new Cobol.Stop(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.STOP(), ctx.RUN()),
                ctx.literal() != null ? (Cobol) visit(ctx.literal()) :
                        ctx.stopStatementGiving() != null ? (Cobol) visit(ctx.stopStatementGiving()) : null
        );
    }

    @Override
    public Object visitStopStatementGiving(CobolParser.StopStatementGivingContext ctx) {
        return new Cobol.StopStatementGiving(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.RUN(), ctx.GIVING(), ctx.RETURNING()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitStringDelimitedByPhrase(CobolParser.StringDelimitedByPhraseContext ctx) {
        return new Cobol.StringDelimitedByPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DELIMITED(), ctx.BY()),
                visit(ctx.SIZE(), ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitStringForPhrase(CobolParser.StringForPhraseContext ctx) {
        return new Cobol.StringForPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.FOR()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitStringIntoPhrase(CobolParser.StringIntoPhraseContext ctx) {
        return new Cobol.StringIntoPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INTO()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitStringSendingPhrase(CobolParser.StringSendingPhraseContext ctx) {
        return new Cobol.StringSendingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllPrefixedList(singletonList(","), ctx.stringSending()),
                visit(ctx.stringDelimitedByPhrase(), ctx.stringForPhrase())
        );
    }

    @Override
    public Object visitStringStatement(CobolParser.StringStatementContext ctx) {
        return new Cobol.StringStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.STRING()),
                convertAllContainer(ctx.stringSendingPhrase()),
                (Cobol.StringIntoPhrase) visit(ctx.stringIntoPhrase()),
                visitNullable(ctx.stringWithPointerPhrase()),
                visitNullable(ctx.onOverflowPhrase()),
                visitNullable(ctx.notOnOverflowPhrase()),
                visitNullable(ctx.END_STRING())
        );
    }

    @Override
    public Object visitStringWithPointerPhrase(CobolParser.StringWithPointerPhraseContext ctx) {
        return new Cobol.StringWithPointerPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.POINTER()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.Subscript visitSubscript(CobolParser.SubscriptContext ctx) {
        return new Cobol.Subscript(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.ALL(), ctx.qualifiedDataName(), ctx.indexName(), ctx.arithmeticExpression()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitSubtractCorrespondingStatement(CobolParser.SubtractCorrespondingStatementContext ctx) {
        return new Cobol.SubtractCorrespondingStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.CORRESPONDING(), ctx.CORR()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                (Cobol.CobolWord) visit(ctx.FROM()),
                (Cobol.SubtractMinuendCorresponding) visit(ctx.subtractMinuendCorresponding())
        );
    }

    @Override
    public Object visitSubtractFromGivingStatement(CobolParser.SubtractFromGivingStatementContext ctx) {
        return new Cobol.SubtractFromGivingStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.subtractSubtrahend()),
                (Cobol.CobolWord) visit(ctx.FROM()),
                (Name) visit(ctx.subtractMinuendGiving()),
                (Cobol.CobolWord) visit(ctx.GIVING()),
                convertAllContainer(ctx.subtractGiving())
        );
    }

    @Override
    public Object visitSubtractFromStatement(CobolParser.SubtractFromStatementContext ctx) {
        return new Cobol.SubtractFromStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.subtractSubtrahend()),
                (Cobol.CobolWord) visit(ctx.FROM()),
                convertAllContainer(ctx.subtractMinuend())
        );
    }

    @Override
    public Object visitSubtractMinuendCorresponding(CobolParser.SubtractMinuendCorrespondingContext ctx) {
        return new Cobol.SubtractMinuendCorresponding(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                visitNullable(ctx.ROUNDED())
        );
    }

    @Override
    public Object visitSubtractStatement(CobolParser.SubtractStatementContext ctx) {
        return new Cobol.Subtract(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.SUBTRACT()),
                visit(ctx.subtractFromStatement(), ctx.subtractFromGivingStatement(), ctx.subtractCorrespondingStatement()),
                visitNullable(ctx.onSizeErrorPhrase()),
                visitNullable(ctx.notOnSizeErrorPhrase()),
                visitNullable(ctx.END_SUBTRACT())
        );
    }

    @Override
    public Cobol.SymbolicCharacter visitSymbolicCharacters(CobolParser.SymbolicCharactersContext ctx) {
        return new Cobol.SymbolicCharacter(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                convertAllContainer(ctx.symbolicCharacter()),
                ctx.IS() != null ? (Cobol.CobolWord) visit(ctx.IS()) :
                        ctx.ARE() != null ? (Cobol.CobolWord) visit(ctx.ARE()) : null,
                convertAllContainer(ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.SymbolicCharactersClause visitSymbolicCharactersClause(CobolParser.SymbolicCharactersClauseContext ctx) {
        return new Cobol.SymbolicCharactersClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SYMBOLIC(), ctx.CHARACTERS(), ctx.FOR(), ctx.ALPHANUMERIC(), ctx.NATIONAL()),
                convertAllContainer(ctx.symbolicCharacters()),
                visitNullable(ctx.IN()),
                visitNullable(ctx.alphabetName())
        );
    }

    @Override
    public Object visitSymbolicDestinationClause(CobolParser.SymbolicDestinationClauseContext ctx) {
        return new Cobol.SymbolicDestinationClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SYMBOLIC(), ctx.DESTINATION(), ctx.IS()),
                visitNullable(ctx.dataDescName())
        );
    }

    @Override
    public Object visitSymbolicQueueClause(CobolParser.SymbolicQueueClauseContext ctx) {
        return new Cobol.SymbolicQueueClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SYMBOLIC(), ctx.QUEUE(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitSymbolicSourceClause(CobolParser.SymbolicSourceClauseContext ctx) {
        return new Cobol.SymbolicSourceClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SYMBOLIC(), ctx.SOURCE(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitSymbolicSubQueueClause(CobolParser.SymbolicSubQueueClauseContext ctx) {
        return new Cobol.SymbolicSubQueueClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SYMBOLIC(), ctx.SUB_QUEUE_1(), ctx.SUB_QUEUE_2(), ctx.SUB_QUEUE_3(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitSymbolicTerminalClause(CobolParser.SymbolicTerminalClauseContext ctx) {
        return new Cobol.SymbolicTerminalClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.SYMBOLIC(), ctx.TERMINAL(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Cobol.TableCall visitTableCall(CobolParser.TableCallContext ctx) {
        return new Cobol.TableCall(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                convertAllContainer(ctx.tableCallSubscripts()),
                visitNullable(ctx.referenceModifier())
        );
    }

    @Override
    public Cobol.Parenthesized visitTableCallSubscripts(CobolParser.TableCallSubscriptsContext ctx) {
        return new Cobol.Parenthesized(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.LPARENCHAR()),
                convertAllPrefixedList(Collections.singletonList(","), ctx.subscript()),
                (Cobol.CobolWord) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Cobol.CobolWord visitTerminal(TerminalNode node) {
        Space prefix = whitespace();
        Markers markers = continuationMarker(node.getText());
        return new Cobol.CobolWord(
                randomId(),
                prefix,
                markers,
                node.getText()
        );
    }

    @Nullable
    private Markers continuationMarker(String text) {
        // Check if a NONNUMERIC token is continued on the next line.
        if (!text.isEmpty() && (text.charAt(0) == '\'' || text.charAt(0) == '"')) {
            List<CobolRightPadded<Integer>> continuations = null;
            char delimiter = text.charAt(0);
            int continuationPos = 0;
            int matchedText = 0;
            while (matchedText < text.length()) {
                int endLinePos = source.substring(cursor).indexOf("\n");
                if (endLinePos == -1) {
                    break;
                }

                String current = source.substring(cursor);
                String start = current.substring(0, endLinePos);
                continuationPos += endLinePos;

                // Increment past the starting delimiter to find the closing delimiter.
                int endPos = current.substring(1).indexOf(delimiter) + 2;
                if (start.length() > endPos) {
                    break;
                }
                String marker = current.substring(start.length(), endPos);
                cursor += start.length() + marker.length();
                matchedText += start.length();
                if (continuations == null) {
                    continuations = new ArrayList<>();
                }
                continuations.add(CobolRightPadded.build(Integer.valueOf(continuationPos)).withAfter(Space.build(marker, emptyList())));
            }
            if (continuations != null && !continuations.isEmpty()) {
                return Markers.build(singletonList(new Continuation(randomId(), CobolContainer.build(continuations))));
            }
        }
        cursor += text.length();
        return Markers.EMPTY;
    }

    @Override
    public Object visitTerminateStatement(CobolParser.TerminateStatementContext ctx) {
        return new Cobol.Terminate(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.TERMINATE()),
                (Cobol.QualifiedDataName) visit(ctx.reportName())
        );
    }

    @Override
    public Object visitTextLengthClause(CobolParser.TextLengthClauseContext ctx) {
        return new Cobol.TextLengthClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.TEXT(), ctx.LENGTH(), ctx.IS()),
                (Cobol.CobolWord) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitUnstringCountIn(CobolParser.UnstringCountInContext ctx) {
        return new Cobol.UnstringCountIn(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.COUNT(), ctx.IN()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitUnstringDelimitedByPhrase(CobolParser.UnstringDelimitedByPhraseContext ctx) {
        return new Cobol.UnstringDelimitedByPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DELIMITED(), ctx.BY(), ctx.ALL()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitUnstringDelimiterIn(CobolParser.UnstringDelimiterInContext ctx) {
        return new Cobol.UnstringDelimiterIn(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.DELIMITER(), ctx.IN()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitUnstringInto(CobolParser.UnstringIntoContext ctx) {
        return new Cobol.UnstringInto(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Identifier) visit(ctx.identifier()),
                visitNullable(ctx.unstringDelimiterIn()),
                visitNullable(ctx.unstringCountIn())
        );
    }

    @Override
    public Object visitUnstringIntoPhrase(CobolParser.UnstringIntoPhraseContext ctx) {
        return new Cobol.UnstringIntoPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.INTO()),
                convertAllContainer(ctx.unstringInto())
        );
    }

    @Override
    public Object visitUnstringOrAllPhrase(CobolParser.UnstringOrAllPhraseContext ctx) {
        return new Cobol.UnstringOrAllPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.OR(), ctx.ALL()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitUnstringSendingPhrase(CobolParser.UnstringSendingPhraseContext ctx) {
        return new Cobol.UnstringSendingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Identifier) visit(ctx.identifier()),
                visitNullable(ctx.unstringDelimitedByPhrase()),
                convertAllContainer(ctx.unstringOrAllPhrase())
        );
    }

    @Override
    public Object visitUnstringStatement(CobolParser.UnstringStatementContext ctx) {
        return new Cobol.UnString(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.UNSTRING()),
                (Cobol.UnstringSendingPhrase) visit(ctx.unstringSendingPhrase()),
                (Cobol.UnstringIntoPhrase) visit(ctx.unstringIntoPhrase()),
                visitNullable(ctx.unstringWithPointerPhrase()),
                visitNullable(ctx.unstringTallyingPhrase()),
                visitNullable(ctx.onOverflowPhrase()),
                visitNullable(ctx.notOnOverflowPhrase()),
                visitNullable(ctx.END_UNSTRING())
        );
    }

    @Override
    public Object visitUnstringTallyingPhrase(CobolParser.UnstringTallyingPhraseContext ctx) {
        return new Cobol.UnstringTallyingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.TALLYING(), ctx.IN()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitUnstringWithPointerPhrase(CobolParser.UnstringWithPointerPhraseContext ctx) {
        return new Cobol.UnstringWithPointerPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WITH(), ctx.POINTER()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitUseAfterClause(CobolParser.UseAfterClauseContext ctx) {
        return new Cobol.UseAfterClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.GLOBAL(), ctx.AFTER(), ctx.STANDARD(), ctx.EXCEPTION(), ctx.ERROR(), ctx.PROCEDURE(), ctx.ON()),
                (Cobol.UseAfterOn) visit(ctx.useAfterOn())
        );
    }

    @Override
    public Object visitUseAfterOn(CobolParser.UseAfterOnContext ctx) {
        return new Cobol.UseAfterOn(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.INPUT(), ctx.OUTPUT(), ctx.I_O(), ctx.EXTEND()),
                convertAllContainer(ctx.fileName())
        );
    }

    @Override
    public Object visitUseDebugClause(CobolParser.UseDebugClauseContext ctx) {
        return new Cobol.UseDebugClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.FOR(), ctx.DEBUGGING(), ctx.ON()),
                convertAllContainer(ctx.useDebugOn())
        );
    }

    @Override
    public Object visitUseDebugOn(CobolParser.UseDebugOnContext ctx) {
        return new Cobol.UseDebugClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.ALL(), ctx.PROCEDURES(), ctx.REFERENCES(), ctx.OF()),
                ctx.PROCEDURES() != null ? null : visit(ctx.identifier(), ctx.procedureName(), ctx.fileName())
        );
    }

    @Override
    public Object visitUseStatement(CobolParser.UseStatementContext ctx) {
        return new Cobol.UseStatement(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.USE()),
                visit(ctx.useAfterClause(), ctx.useDebugClause())
        );
    }

    @Override
    public Object visitValueOfClause(CobolParser.ValueOfClauseContext ctx) {
        return new Cobol.ValueOfClause(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.VALUE(), ctx.OF()),
                convertAllContainer(ctx.valuePair())
        );
    }

    @Override
    public Object visitValuePair(CobolParser.ValuePairContext ctx) {
        return new Cobol.ValuePair(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.systemName()),
                visitNullable(ctx.IS()),
                visit(ctx.qualifiedDataName(), ctx.literal())
        );
    }

    @Override
    public Cobol.WorkingStorageSection visitWorkingStorageSection(CobolParser.WorkingStorageSectionContext ctx) {
        return new Cobol.WorkingStorageSection(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.WORKING_STORAGE(), ctx.SECTION()),
                convertAllContainer(sourceBefore("."), ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Object visitWriteAdvancingLines(CobolParser.WriteAdvancingLinesContext ctx) {
        return new Cobol.WriteAdvancingLines(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                ctx.LINE() != null ? (Cobol.CobolWord) visit(ctx.LINE()) :
                        ctx.LINES() != null ? (Cobol.CobolWord) visit(ctx.LINES()) : null
        );
    }

    @Override
    public Object visitWriteAdvancingMnemonic(CobolParser.WriteAdvancingMnemonicContext ctx) {
        return new Cobol.WriteAdvancingMnemonic(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Name) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Object visitWriteAdvancingPage(CobolParser.WriteAdvancingPageContext ctx) {
        return new Cobol.WriteAdvancingPage(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.PAGE())
        );
    }

    @Override
    public Object visitWriteAdvancingPhrase(CobolParser.WriteAdvancingPhraseContext ctx) {
        return new Cobol.WriteAdvancingPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.BEFORE(), ctx.AFTER(), ctx.ADVANCING()),
                visit(ctx.writeAdvancingPage(), ctx.writeAdvancingLines(), ctx.writeAdvancingMnemonic())
        );
    }

    @Override
    public Object visitWriteAtEndOfPagePhrase(CobolParser.WriteAtEndOfPagePhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.AT(), ctx.END_OF_PAGE(), ctx.EOP()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitWriteFromPhrase(CobolParser.WriteFromPhraseContext ctx) {
        return new Cobol.WriteFromPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                visitNullable(ctx.FROM()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitWriteNotAtEndOfPagePhrase(CobolParser.WriteNotAtEndOfPagePhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                words(ctx.NOT(), ctx.AT(), ctx.END_OF_PAGE(), ctx.EOP()),
                convertAllContainer(ctx.statement())
        );
    }

    @Override
    public Object visitWriteStatement(CobolParser.WriteStatementContext ctx) {
        return new Cobol.Write(
                randomId(),
                whitespace(),
                Markers.EMPTY,
                (Cobol.CobolWord) visit(ctx.WRITE()),
                (Cobol.QualifiedDataName) visit(ctx.recordName()),
                visitNullable(ctx.writeFromPhrase()),
                visitNullable(ctx.writeAdvancingPhrase()),
                visitNullable(ctx.writeAtEndOfPagePhrase()),
                visitNullable(ctx.writeNotAtEndOfPagePhrase()),
                visitNullable(ctx.invalidKeyPhrase()),
                visitNullable(ctx.notInvalidKeyPhrase()),
                visitNullable(ctx.END_WRITE())
        );
    }

    private Space whitespace() {
        String prefix = source.substring(cursor, indexOfNextNonWhitespace(cursor, source));
        cursor += prefix.length();
        return format(prefix);
    }

    private @Nullable Cobol.CobolWord words(TerminalNode... wordNodes) {
        Space prefix = null;
        StringBuilder words = new StringBuilder();
        for (TerminalNode wordNode : wordNodes) {
            if (wordNode != null) {
                if (prefix == null) {
                    prefix = whitespace();
                }
                words.append(sourceBefore(wordNode.getText()).getWhitespace());
                words.append(wordNode.getText());
            }
        }

        if (words.toString().isEmpty()) {
            return null;
        }

        return new Cobol.CobolWord(
                randomId(),
                prefix,
                Markers.EMPTY,
                words.toString()
        );
    }


    private <C, T extends ParseTree> List<C> convertAll(List<T> trees, Function<T, C> convert) {
        List<C> converted = new ArrayList<>(trees.size());
        for (T tree : trees) {
            converted.add(convert.apply(tree));
        }
        return converted;
    }

    private <C extends Cobol, T extends ParseTree> List<C> convertAll(List<T> trees) {
        //noinspection unchecked
        return convertAll(trees, t -> (C) visit(t));
    }

    private <C extends Cobol, T extends ParseTree> CobolContainer<C> convertAllContainer(@Nullable CobolLeftPadded<String> preposition, List<T> trees) {
        return this.<C, T>convertAllContainer(trees, () -> Space.EMPTY).withPreposition(preposition);
    }

    private <C extends Cobol, T extends ParseTree> CobolContainer<C> convertAllContainer(Space before, List<T> trees) {
        return this.<C, T>convertAllContainer(trees, () -> Space.EMPTY).withBefore(before);
    }

    private <C extends Cobol, T extends ParseTree> CobolContainer<C> convertAllContainer(List<T> trees) {
        return convertAllContainer(trees, () -> Space.EMPTY);
    }

    @SafeVarargs
    private final CobolContainer<Cobol> convertAllContainer(List<? extends ParserRuleContext>... trees) {
        return convertAllContainer(Arrays.stream(trees)
                        .filter(Objects::nonNull)
                        .flatMap(Collection::stream)
                        .sorted(Comparator.comparingInt(it -> it.start.getStartIndex()))
                        .collect(Collectors.toList()),
                () -> Space.EMPTY);
    }

    private <C extends Cobol, T extends ParseTree> CobolContainer<C> convertAllContainer(List<T> trees, Supplier<Space> sourceBefore) {
        //noinspection unchecked
        return CobolContainer.build(convertAll(trees, t -> padRight((C) visit(t), sourceBefore.get())));
    }

    @SafeVarargs
    private final List<Cobol> convertAllList(List<String> delimiters, List<? extends ParseTree>... trees) {
        return Arrays.stream(trees)
                .flatMap(Collection::stream)
                .filter(Objects::nonNull)
                .flatMap(it -> {
                    int saveCursor = cursor;
                    Space prefix = whitespace();
                    Cobol.CobolWord cw = null;
                    for (String delimiter : delimiters) {
                        if (source.substring(cursor).startsWith(delimiter)) {
                            cw = new Cobol.CobolWord(
                                    randomId(),
                                    prefix,
                                    Markers.EMPTY,
                                    delimiter
                            );
                            cursor += delimiter.length(); // skip the delimiter
                            break;
                        }
                    }

                    if (cw == null) {
                        cursor = saveCursor;
                    }

                    Cobol cobol = (Cobol) visit(it);
                    return cw == null ? Stream.of(cobol) : Stream.of(cw, cobol);
                })
                .collect(Collectors.toList());
    }

    @SafeVarargs
    private final List<Cobol> convertAllPrefixedList(List<String> delimiters, List<? extends ParseTree>... trees) {
        return Arrays.stream(trees)
                .flatMap(Collection::stream)
                .filter(Objects::nonNull)
                .flatMap(it -> {
                    Cobol cobol = (Cobol) visit(it);
                    int saveCursor = cursor;
                    Space prefix = whitespace();
                    for (String delimiter : delimiters) {
                        if (source.substring(cursor).startsWith(delimiter)) {
                            Cobol.CobolWord comma = new Cobol.CobolWord(
                                    randomId(),
                                    prefix,
                                    Markers.EMPTY,
                                    delimiter
                            );
                            cursor += delimiter.length(); // skip the delimiter
                            return Stream.of(cobol, comma);
                        }
                    }
                    cursor = saveCursor;
                    return Stream.of(cobol);
                })
                .collect(Collectors.toList());
    }

    private <T> CobolLeftPadded<T> padLeft(Space left, T tree) {
        return new CobolLeftPadded<>(left, tree, Markers.EMPTY);
    }

    // TODO: fix me. The tree should be visited instead of using text.
    private CobolLeftPadded<String> padLeft(@Nullable ParseTree pt) {
        if (pt == null) {
            //noinspection ConstantConditions
            return null;
        }
        return padLeft(sourceBefore(pt.getText()), pt.getText());
    }

    private <T> CobolRightPadded<T> padRight(T tree, Space right) {
        return new CobolRightPadded<>(tree, right, Markers.EMPTY);
    }

    private int positionOfNext(String untilDelim, @Nullable Character stop) {
        int delimIndex = cursor;
        for (; delimIndex < source.length() - untilDelim.length() + 1; delimIndex++) {
            if (stop != null && source.charAt(delimIndex) == stop)
                return -1; // reached stop word before finding the delimiter

            if (source.startsWith(untilDelim, delimIndex)) {
                break; // found it!
            }
        }

        return delimIndex > source.length() - untilDelim.length() ? -1 : delimIndex;
    }

    private Space prefix(ParserRuleContext ctx) {
        return prefix(ctx.getStart());
    }

    private Space prefix(Token token) {
        int start = token.getStartIndex();
        if (start < cursor) {
            return format("");
        }
        String prefix = source.substring(cursor, start);
        cursor = start;
        return format(prefix);
    }

    private String skip(String string) {
        cursor += string.length();
        return string;
    }

    private Space sourceBefore(String untilDelim) {
        return sourceBefore(untilDelim, null);
    }

    /**
     * @return Source from <code>cursor</code> to next occurrence of <code>untilDelim</code>,
     * and if not found in the remaining source, the empty String. If <code>stop</code> is reached before
     * <code>untilDelim</code> return the empty String.
     */
    private Space sourceBefore(String untilDelim, @Nullable Character stop) {
        int delimIndex = positionOfNext(untilDelim, stop);
        if (delimIndex < 0) {
            return Space.EMPTY; // unable to find this delimiter
        }

        String prefix = source.substring(cursor, delimIndex);
        cursor += prefix.length() + untilDelim.length(); // advance past the delimiter
        return format(prefix);
    }
}
