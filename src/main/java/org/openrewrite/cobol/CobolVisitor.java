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

import org.openrewrite.Cursor;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.util.List;

public class CobolVisitor<P> extends TreeVisitor<Cobol, P> {

    public Cobol visitAbbreviation(Cobol.Abbreviation abbreviation, P p) {
        Cobol.Abbreviation a = abbreviation;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withRelationalOperator((Cobol.RelationalOperator) visit(a.getRelationalOperator(), p));
        a = a.withArithmeticExpression(visit(a.getArithmeticExpression(), p));
        a = a.withAbbreviation(visit(a.getAbbreviation(), p));
        return a;
    }

    public Cobol visitAccept(Cobol.Accept acceptStatement, P p) {
        Cobol.Accept a = acceptStatement;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withIdentifier((Identifier) visit(a.getIdentifier(), p));
        a = a.withOperation(visit(a.getOperation(), p));
        a = a.withOnExceptionClause((Cobol.StatementPhrase) visit(a.getOnExceptionClause(), p));
        a = a.withNotOnExceptionClause((Cobol.StatementPhrase) visit(a.getNotOnExceptionClause(), p));
        a = a.withEndAccept((Cobol.CobolWord) visit(a.getEndAccept(), p));
        return a;
    }

    public Cobol visitAcceptFromDateStatement(Cobol.AcceptFromDateStatement acceptFromDateStatement, P p) {
        Cobol.AcceptFromDateStatement a = acceptFromDateStatement;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        return a;
    }

    public Cobol visitAcceptFromEscapeKeyStatement(Cobol.AcceptFromEscapeKeyStatement acceptFromEscapeKeyStatement, P p) {
        Cobol.AcceptFromEscapeKeyStatement a = acceptFromEscapeKeyStatement;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        return a;
    }

    public Cobol visitAcceptFromMnemonicStatement(Cobol.AcceptFromMnemonicStatement acceptFromMnemonicStatement, P p) {
        Cobol.AcceptFromMnemonicStatement a = acceptFromMnemonicStatement;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withMnemonicName((Identifier) visit(a.getMnemonicName(), p));
        return a;
    }

    public Cobol visitAcceptMessageCountStatement(Cobol.AcceptMessageCountStatement acceptMessageCountStatement, P p) {
        Cobol.AcceptMessageCountStatement a = acceptMessageCountStatement;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        return a;
    }

    public Cobol visitAccessModeClause(Cobol.AccessModeClause accessModeClause, P p) {
        Cobol.AccessModeClause a = accessModeClause;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        return a;
    }

    public Cobol visitAdd(Cobol.Add add, P p) {
        Cobol.Add a = add;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withOperation(visit(a.getOperation(), p));
        a = a.withOnSizeError((Cobol.StatementPhrase) visit(a.getOnSizeError(), p));
        a = a.withEndAdd((Cobol.CobolWord) visit(a.getEndAdd(), p));
        return a;
    }

    public Cobol visitAddTo(Cobol.AddTo addTo, P p) {
        Cobol.AddTo a = addTo;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withFrom(ListUtils.map(a.getFrom(), t -> (Name) visit(t, p)));
        a = a.getPadding().withTo(visitContainer(a.getPadding().getTo(), p));
        a = a.getPadding().withGiving(visitContainer(a.getPadding().getGiving(), p));
        return a;
    }

    public Cobol visitAlphabetAlso(Cobol.AlphabetAlso alphabetAlso, P p) {
        Cobol.AlphabetAlso a = alphabetAlso;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withLiterals(ListUtils.map(a.getLiterals(), t -> (Literal) visit(t, p)));
        return a;
    }

    public Cobol visitAlphabetClause(Cobol.AlphabetClause alphabetClause, P p) {
        Cobol.AlphabetClause a = alphabetClause;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withAlphabet((Cobol.CobolWord) visit(a.getAlphabet(), p));
        a = a.withName((Name) visit(a.getName(), p));
        a = a.withWords(ListUtils.map(a.getWords(), it -> visit(it, p)));
        return a;
    }

    public Cobol visitAlphabetLiteral(Cobol.AlphabetLiteral alphabetLiteral, P p) {
        Cobol.AlphabetLiteral a = alphabetLiteral;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withLiteral((Literal) visit(a.getLiteral(), p));
        a = a.withAlphabetThrough((Cobol.AlphabetThrough) visit(a.getAlphabetThrough(), p));
        a = a.withAlphabetAlso(ListUtils.map(a.getAlphabetAlso(), t -> (Cobol.AlphabetAlso) visit(t, p)));
        return a;
    }

    public Cobol visitAlphabetThrough(Cobol.AlphabetThrough alphabetThrough, P p) {
        Cobol.AlphabetThrough a = alphabetThrough;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withLiteral((Literal) visit(a.getLiteral(), p));
        return a;
    }

    public Cobol visitAlterProceedTo(Cobol.AlterProceedTo alterProceedTo, P p) {
        Cobol.AlterProceedTo a = alterProceedTo;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withFrom((Cobol.ProcedureName) visit(a.getFrom(), p));
        a = a.withTo((Cobol.ProcedureName) visit(a.getTo(), p));
        return a;
    }

    public Cobol visitAlterStatement(Cobol.AlterStatement alterStatement, P p) {
        Cobol.AlterStatement a = alterStatement;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withAlterProceedTo(ListUtils.map(a.getAlterProceedTo(), t -> (Cobol.AlterProceedTo) visit(t, p)));
        return a;
    }

    public Cobol visitAlteredGoTo(Cobol.AlteredGoTo alteredGoTo, P p) {
        Cobol.AlteredGoTo a = alteredGoTo;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.getPadding().withDot(visitLeftPadded(a.getPadding().getDot(), p));
        return a;
    }

    public Cobol visitAlternateRecordKeyClause(Cobol.AlternateRecordKeyClause alternateRecordKeyClause, P p) {
        Cobol.AlternateRecordKeyClause a = alternateRecordKeyClause;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withQualifiedDataName((Cobol.QualifiedDataName) visit(a.getQualifiedDataName(), p));
        a = a.withPasswordClause((Cobol.PasswordClause) visit(a.getPasswordClause(), p));
        return a;
    }

    public Cobol visitAndOrCondition(Cobol.AndOrCondition andOrCondition, P p) {
        Cobol.AndOrCondition a = andOrCondition;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withCombinableCondition((Cobol.CombinableCondition) visit(a.getCombinableCondition(), p));
        a = a.withAbbreviations(ListUtils.map(a.getAbbreviations(), t -> visit(t, p)));
        return a;
    }

    public Cobol visitArgument(Cobol.Argument argument, P p) {
        Cobol.Argument a = argument;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withFirst(visit(a.getFirst(), p));
        a = a.withIntegerLiteral((Cobol.CobolWord) visit(a.getIntegerLiteral(), p));
        return a;
    }

    public Cobol visitArithmeticExpression(Cobol.ArithmeticExpression arithmeticExpression, P p) {
        Cobol.ArithmeticExpression a = arithmeticExpression;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        a = a.withMultDivs((Cobol.MultDivs) visit(a.getMultDivs(), p));
        a = a.withPlusMinuses(ListUtils.map(a.getPlusMinuses(), t -> (Cobol.PlusMinus) visit(t, p)));
        return a;
    }

    public Cobol visitAssignClause(Cobol.AssignClause assignClause, P p) {
        Cobol.AssignClause a = assignClause;
        a = a.withPrefix(visitSpace(a.getPrefix(), p));
        a = a.withMarkers(visitMarkers(a.getMarkers(), p));
        return a;
    }

    public Cobol visitBlockContainsClause(Cobol.BlockContainsClause blockContainsClause, P p) {
        Cobol.BlockContainsClause b = blockContainsClause;
        b = b.withPrefix(visitSpace(b.getPrefix(), p));
        b = b.withMarkers(visitMarkers(b.getMarkers(), p));
        b = b.withIntegerLiteral((Cobol.CobolWord) visit(b.getIntegerLiteral(), p));
        b = b.withBlockContainsTo((Cobol.BlockContainsTo) visit(b.getBlockContainsTo(), p));
        return b;
    }

    public Cobol visitBlockContainsTo(Cobol.BlockContainsTo blockContainsTo, P p) {
        Cobol.BlockContainsTo b = blockContainsTo;
        b = b.withPrefix(visitSpace(b.getPrefix(), p));
        b = b.withMarkers(visitMarkers(b.getMarkers(), p));
        b = b.withIntegerLiteral((Cobol.CobolWord) visit(b.getIntegerLiteral(), p));
        return b;
    }

    public Cobol visitCall(Cobol.Call call, P p) {
        Cobol.Call c = call;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitCallBy(Cobol.CallBy callBy, P p) {
        Cobol.CallBy c = callBy;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitCallGivingPhrase(Cobol.CallGivingPhrase callGivingPhrase, P p) {
        Cobol.CallGivingPhrase c = callGivingPhrase;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitCallPhrase(Cobol.CallPhrase callPhrase, P p) {
        Cobol.CallPhrase c = callPhrase;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withParameters(ListUtils.map(c.getParameters(), t -> visit(t, p)));
        return c;
    }

    public Cobol visitCancel(Cobol.Cancel cancel, P p) {
        Cobol.Cancel c = cancel;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCancelCalls(ListUtils.map(c.getCancelCalls(), t -> (Cobol.CancelCall) visit(t, p)));
        return c;
    }

    public Cobol visitCancelCall(Cobol.CancelCall cancelCall, P p) {
        Cobol.CancelCall c = cancelCall;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withIdentifier((Identifier) visit(c.getIdentifier(), p));
        c = c.withLiteral((Literal) visit(c.getLiteral(), p));
        return c;
    }

    public Cobol visitChannelClause(Cobol.ChannelClause channelClause, P p) {
        Cobol.ChannelClause c = channelClause;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withLiteral((Literal) visit(c.getLiteral(), p));
        c = c.withIs((Cobol.CobolWord) visit(c.getIs(), p));
        c = c.withMnemonicName((Identifier) visit(c.getMnemonicName(), p));
        return c;
    }

    public Cobol visitClassClause(Cobol.ClassClause classClause, P p) {
        Cobol.ClassClause c = classClause;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withClassName((Cobol.CobolWord) visit(c.getClassName(), p));
        c = c.withThroughs(ListUtils.map(c.getThroughs(), t -> (Cobol.ClassClauseThrough) visit(t, p)));
        return c;
    }

    public Cobol visitClassClauseThrough(Cobol.ClassClauseThrough classClauseThrough, P p) {
        Cobol.ClassClauseThrough c = classClauseThrough;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withFrom((Name) visit(c.getFrom(), p));
        c = c.withThrough((Cobol.CobolWord) visit(c.getThrough(), p));
        c = c.withFrom((Name) visit(c.getTo(), p));
        return c;
    }

    public Cobol visitClassCondition(Cobol.ClassCondition classCondition, P p) {
        Cobol.ClassCondition c = classCondition;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitClose(Cobol.Close close, P p) {
        Cobol.Close c = close;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCloseFiles(ListUtils.map(c.getCloseFiles(), t -> (Cobol.CloseFile) visit(t, p)));
        return c;
    }

    public Cobol visitCloseFile(Cobol.CloseFile closeFile, P p) {
        Cobol.CloseFile c = closeFile;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCloseStatement(visit(c.getCloseStatement(), p));
        return c;
    }

    public Cobol visitClosePortFileIOStatement(Cobol.ClosePortFileIOStatement closePortFileIOStatement, P p) {
        Cobol.ClosePortFileIOStatement c = closePortFileIOStatement;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withClosePortFileIOUsing(ListUtils.map(c.getClosePortFileIOUsing(), t -> visit(t, p)));
        return c;
    }

    public Cobol visitClosePortFileIOUsingAssociatedData(Cobol.ClosePortFileIOUsingAssociatedData closePortFileIOUsingAssociatedData, P p) {
        Cobol.ClosePortFileIOUsingAssociatedData c = closePortFileIOUsingAssociatedData;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withIdentifier((Identifier) visit(c.getIdentifier(), p));
        return c;
    }

    public Cobol visitClosePortFileIOUsingAssociatedDataLength(Cobol.ClosePortFileIOUsingAssociatedDataLength closePortFileIOUsingAssociatedDataLength, P p) {
        Cobol.ClosePortFileIOUsingAssociatedDataLength c = closePortFileIOUsingAssociatedDataLength;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withIdentifier((Identifier) visit(c.getIdentifier(), p));
        return c;
    }

    public Cobol visitClosePortFileIOUsingCloseDisposition(Cobol.ClosePortFileIOUsingCloseDisposition closePortFileIOUsingCloseDisposition, P p) {
        Cobol.ClosePortFileIOUsingCloseDisposition c = closePortFileIOUsingCloseDisposition;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitCloseReelUnitStatement(Cobol.CloseReelUnitStatement closeReelUnitStatement, P p) {
        Cobol.CloseReelUnitStatement c = closeReelUnitStatement;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitCloseRelativeStatement(Cobol.CloseRelativeStatement closeRelativeStatement, P p) {
        Cobol.CloseRelativeStatement c = closeRelativeStatement;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitCobolWord(Cobol.CobolWord cobolWord, P p) {
        Cobol.CobolWord c = cobolWord;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitCodeSetClause(Cobol.CodeSetClause codeSetClause, P p) {
        Cobol.CodeSetClause c = codeSetClause;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withAlphabetName((Cobol.CobolWord) visit(c.getAlphabetName(), p));
        return c;
    }

    public Cobol visitCollatingSequenceAlphabet(Cobol.CollatingSequenceAlphabet collatingSequenceAlphabet, P p) {
        Cobol.CollatingSequenceAlphabet c = collatingSequenceAlphabet;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withAlphabetName((Identifier) visit(c.getAlphabetName(), p));
        return c;
    }

    public Cobol visitCollatingSequenceClause(Cobol.CollatingSequenceClause collatingSequenceClause, P p) {
        Cobol.CollatingSequenceClause c = collatingSequenceClause;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.getPadding().withAlphabetName(visitContainer(c.getPadding().getAlphabetName(), p));
        c = c.withAlphanumeric((Cobol.CollatingSequenceAlphabet) visit(c.getAlphanumeric(), p));
        c = c.withNational((Cobol.CollatingSequenceAlphabet) visit(c.getNational(), p));
        return c;
    }

    public Cobol visitCombinableCondition(Cobol.CombinableCondition combinableCondition, P p) {
        Cobol.CombinableCondition c = combinableCondition;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withSimpleCondition(visit(c.getSimpleCondition(), p));
        return c;
    }

    public Cobol visitCommitmentControlClause(Cobol.CommitmentControlClause commitmentControlClause, P p) {
        Cobol.CommitmentControlClause c = commitmentControlClause;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withFileName((Cobol.CobolWord) visit(c.getFileName(), p));
        return c;
    }

    public Cobol visitCommunicationDescriptionEntryFormat1(Cobol.CommunicationDescriptionEntryFormat1 communicationDescriptionEntryFormat1, P p) {
        Cobol.CommunicationDescriptionEntryFormat1 c = communicationDescriptionEntryFormat1;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCd((Cobol.CobolWord) visit(c.getCd(), p));
        c = c.withName((Cobol.CobolWord) visit(c.getName(), p));
        c = c.withWords(ListUtils.map(c.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        c = c.withInputs(ListUtils.map(c.getInputs(), it -> visit(it, p)));
        c = c.withDot((Cobol.CobolWord) visit(c.getDot(), p));
        return c;
    }

    public Cobol visitCommunicationDescriptionEntryFormat2(Cobol.CommunicationDescriptionEntryFormat2 communicationDescriptionEntryFormat2, P p) {
        Cobol.CommunicationDescriptionEntryFormat2 c = communicationDescriptionEntryFormat2;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCd((Cobol.CobolWord) visit(c.getCd(), p));
        c = c.withName((Cobol.CobolWord) visit(c.getName(), p));
        c = c.withWords(ListUtils.map(c.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        c = c.withOutputs(ListUtils.map(c.getOutputs(), it -> visit(it, p)));
        c = c.withDot((Cobol.CobolWord) visit(c.getDot(), p));
        return c;
    }

    public Cobol visitCommunicationDescriptionEntryFormat3(Cobol.CommunicationDescriptionEntryFormat3 communicationDescriptionEntryFormat3, P p) {
        Cobol.CommunicationDescriptionEntryFormat3 c = communicationDescriptionEntryFormat3;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCd((Cobol.CobolWord) visit(c.getCd(), p));
        c = c.withName((Cobol.CobolWord) visit(c.getName(), p));
        c = c.withWords(ListUtils.map(c.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        c = c.withInitialIOs(ListUtils.map(c.getInitialIOs(), it -> visit(it, p)));
        c = c.withDot((Cobol.CobolWord) visit(c.getDot(), p));
        return c;
    }

    public Cobol visitCommunicationSection(Cobol.CommunicationSection communicationSection, P p) {
        Cobol.CommunicationSection c = communicationSection;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withWords(ListUtils.map(c.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        c = c.withDot((Cobol.CobolWord) visit(c.getDot(), p));
        c = c.withEntries(ListUtils.map(c.getEntries(), it -> visit(it, p)));
        return c;
    }

    public Cobol visitCompilationUnit(Cobol.CompilationUnit compilationUnit, P p) {
        Cobol.CompilationUnit d = compilationUnit;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.getPadding().withProgramUnits(ListUtils.map(d.getPadding().getProgramUnits(), it -> visitRightPadded(it, p)));
        return d;
    }

    public Cobol visitCompute(Cobol.Compute compute, P p) {
        Cobol.Compute c = compute;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withRoundables(ListUtils.map(c.getRoundables(), t -> (Cobol.Roundable) visit(t, p)));
        c = c.withArithmeticExpression((Cobol.ArithmeticExpression) visit(c.getArithmeticExpression(), p));
        c = c.withOnSizeErrorPhrase((Cobol.StatementPhrase) visit(c.getOnSizeErrorPhrase(), p));
        c = c.withNotOnSizeErrorPhrase((Cobol.StatementPhrase) visit(c.getNotOnSizeErrorPhrase(), p));
        return c;
    }

    public Cobol visitCondition(Cobol.Condition condition, P p) {
        Cobol.Condition c = condition;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withCombinableCondition((Cobol.CombinableCondition) visit(c.getCombinableCondition(), p));
        c = c.withAndOrConditions(ListUtils.map(c.getAndOrConditions(), t -> (Cobol.AndOrCondition) visit(t, p)));
        return c;
    }

    public Cobol visitConditionNameReference(Cobol.ConditionNameReference conditionNameReference, P p) {
        Cobol.ConditionNameReference c = conditionNameReference;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withInDatas(ListUtils.map(c.getInDatas(), t -> (Cobol.InData) visit(t, p)));
        c = c.withInFile((Cobol.InFile) visit(c.getInFile(), p));
        c = c.withReferences(ListUtils.map(c.getReferences(), t -> (Cobol.Parenthesized) visit(t, p)));
        c = c.withInMnemonics(ListUtils.map(c.getInMnemonics(), t -> (Cobol.InMnemonic) visit(t, p)));
        return c;
    }

    public Cobol visitConditionNameSubscriptReference(Cobol.ConditionNameSubscriptReference conditionNameSubscriptReference, P p) {
        Cobol.ConditionNameSubscriptReference c = conditionNameSubscriptReference;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withSubscripts(ListUtils.map(c.getSubscripts(), it -> visit(it, p)));
        return c;
    }

    public Cobol visitConfigurationSection(Cobol.ConfigurationSection configurationSection, P p) {
        Cobol.ConfigurationSection c = configurationSection;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withWords(ListUtils.map(c.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        c = c.withDot((Cobol.CobolWord) visit(c.getDot(), p));
        c = c.withParagraphs(ListUtils.map(c.getParagraphs(), it -> visit(it, p)));
        return c;
    }

    public Cobol visitContinue(Cobol.Continue continuez, P p) {
        Cobol.Continue c = continuez;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Cobol visitCurrencyClause(Cobol.CurrencyClause currencyClause, P p) {
        Cobol.CurrencyClause c = currencyClause;
        c = c.withPrefix(visitSpace(c.getPrefix(), p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        c = c.withLiteral((Literal) visit(c.getLiteral(), p));
        if (c.getPadding().getPictureSymbol() != null) {
            c = c.getPadding().withPictureSymbol(visitLeftPadded(c.getPadding().getPictureSymbol(), p));
        }
        c = c.withPictureSymbolLiteral((Literal) visit(c.getPictureSymbolLiteral(), p));
        return c;
    }

    public Cobol visitDataAlignedClause(Cobol.DataAlignedClause dataAlignedClause, P p) {
        Cobol.DataAlignedClause d = dataAlignedClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataBaseSection(Cobol.DataBaseSection dataBaseSection, P p) {
        Cobol.DataBaseSection d = dataBaseSection;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withWords(ListUtils.map(d.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        d = d.withDot((Cobol.CobolWord) visit(d.getDot(), p));
        d = d.withEntries(ListUtils.map(d.getEntries(), it -> (Cobol.DataBaseSectionEntry) visit(it, p)));
        return d;
    }

    public Cobol visitDataBaseSectionEntry(Cobol.DataBaseSectionEntry dataBaseSectionEntry, P p) {
        Cobol.DataBaseSectionEntry d = dataBaseSectionEntry;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withFrom((Literal) visit(d.getFrom(), p));
        d = d.withTo((Literal) visit(d.getTo(), p));
        return d;
    }

    public Cobol visitDataBlankWhenZeroClause(Cobol.DataBlankWhenZeroClause dataBlankWhenZeroClause, P p) {
        Cobol.DataBlankWhenZeroClause d = dataBlankWhenZeroClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataCommonOwnLocalClause(Cobol.DataCommonOwnLocalClause dataCommonOwnLocalClause, P p) {
        Cobol.DataCommonOwnLocalClause d = dataCommonOwnLocalClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataDescriptionEntry(Cobol.DataDescriptionEntry dataDescriptionEntry, P p) {
        Cobol.DataDescriptionEntry d = dataDescriptionEntry;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withWords(ListUtils.map(d.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        d = d.withName((Cobol.CobolWord) visit(d.getName(), p));
        d = d.withClauses(ListUtils.map(d.getClauses(), it -> visit(it, p)));
        d = d.withDot((Cobol.CobolWord) visit(d.getDot(), p));
        return d;
    }

    public Cobol visitDataDivision(Cobol.DataDivision dataDivision, P p) {
        Cobol.DataDivision d = dataDivision;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withWords(ListUtils.map(d.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        d = d.withDot((Cobol.CobolWord) visit(d.getDot(), p));
        d = d.withSections(ListUtils.map(d.getSections(), it -> (DataDivisionSection) visit(it, p)));
        return d;
    }

    public Cobol visitDataExternalClause(Cobol.DataExternalClause dataExternalClause, P p) {
        Cobol.DataExternalClause d = dataExternalClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataGlobalClause(Cobol.DataGlobalClause dataGlobalClause, P p) {
        Cobol.DataGlobalClause d = dataGlobalClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataIntegerStringClause(Cobol.DataIntegerStringClause dataIntegerStringClause, P p) {
        Cobol.DataIntegerStringClause d = dataIntegerStringClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataJustifiedClause(Cobol.DataJustifiedClause dataJustifiedClause, P p) {
        Cobol.DataJustifiedClause d = dataJustifiedClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataOccursClause(Cobol.DataOccursClause dataOccursClause, P p) {
        Cobol.DataOccursClause d = dataOccursClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withDataOccursTo((Cobol.DataOccursTo) visit(d.getDataOccursTo(), p));
        d = d.withDataOccursDepending((Cobol.DataOccursDepending) visit(d.getDataOccursDepending(), p));
        d = d.getPadding().withSortIndexed(visitContainer(d.getPadding().getSortIndexed(), p));
        return d;
    }

    public Cobol visitDataOccursDepending(Cobol.DataOccursDepending dataOccursDepending, P p) {
        Cobol.DataOccursDepending d = dataOccursDepending;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withQualifiedDataName((Cobol.QualifiedDataName) visit(d.getQualifiedDataName(), p));
        return d;
    }

    public Cobol visitDataOccursIndexed(Cobol.DataOccursIndexed dataOccursIndexed, P p) {
        Cobol.DataOccursIndexed d = dataOccursIndexed;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withIndexNames(ListUtils.map(d.getIndexNames(), t -> (Cobol.CobolWord) visit(t, p)));
        return d;
    }

    public Cobol visitDataOccursSort(Cobol.DataOccursSort dataOccursSort, P p) {
        Cobol.DataOccursSort d = dataOccursSort;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withQualifiedDataNames(ListUtils.map(d.getQualifiedDataNames(), t -> (Cobol.QualifiedDataName) visit(t, p)));
        return d;
    }

    public Cobol visitDataOccursTo(Cobol.DataOccursTo dataOccursTo, P p) {
        Cobol.DataOccursTo d = dataOccursTo;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withIntegerLiteral((Cobol.CobolWord) visit(d.getIntegerLiteral(), p));
        return d;
    }

    public Cobol visitDataPictureClause(Cobol.DataPictureClause dataPictureClause, P p) {
        Cobol.DataPictureClause d = dataPictureClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withWords(ListUtils.map(d.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        d = d.withPictures(ListUtils.map(d.getPictures(), it -> visit(it, p)));
        return d;
    }

    public Cobol visitDataReceivedByClause(Cobol.DataReceivedByClause dataReceivedByClause, P p) {
        Cobol.DataReceivedByClause d = dataReceivedByClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataRecordAreaClause(Cobol.DataRecordAreaClause dataRecordAreaClause, P p) {
        Cobol.DataRecordAreaClause d = dataRecordAreaClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataRecordsClause(Cobol.DataRecordsClause dataRecordsClause, P p) {
        Cobol.DataRecordsClause d = dataRecordsClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withDataName(ListUtils.map(d.getDataName(), t -> (Name) visit(t, p)));
        return d;
    }

    public Cobol visitDataRedefinesClause(Cobol.DataRedefinesClause dataRedefinesClause, P p) {
        Cobol.DataRedefinesClause d = dataRedefinesClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withDataName((Cobol.CobolWord) visit(d.getDataName(), p));
        return d;
    }

    public Cobol visitDataRenamesClause(Cobol.DataRenamesClause dataRenamesClause, P p) {
        Cobol.DataRenamesClause d = dataRenamesClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withFromName((Cobol.QualifiedDataName) visit(d.getFromName(), p));
        d = d.withToName((Cobol.QualifiedDataName) visit(d.getToName(), p));
        return d;
    }

    public Cobol visitDataSignClause(Cobol.DataSignClause dataSignClause, P p) {
        Cobol.DataSignClause d = dataSignClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataSynchronizedClause(Cobol.DataSynchronizedClause dataSynchronizedClause, P p) {
        Cobol.DataSynchronizedClause d = dataSynchronizedClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataThreadLocalClause(Cobol.DataThreadLocalClause dataThreadLocalClause, P p) {
        Cobol.DataThreadLocalClause d = dataThreadLocalClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataTypeClause(Cobol.DataTypeClause dataTypeClause, P p) {
        Cobol.DataTypeClause d = dataTypeClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withParenthesized((Cobol.Parenthesized) visit(d.getParenthesized(), p));
        return d;
    }

    public Cobol visitDataTypeDefClause(Cobol.DataTypeDefClause dataTypeDefClause, P p) {
        Cobol.DataTypeDefClause d = dataTypeDefClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataUsageClause(Cobol.DataUsageClause dataUsageClause, P p) {
        Cobol.DataUsageClause d = dataUsageClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataUsingClause(Cobol.DataUsingClause dataUsingClause, P p) {
        Cobol.DataUsingClause d = dataUsingClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDataValueClause(Cobol.DataValueClause dataValueClause, P p) {
        Cobol.DataValueClause d = dataValueClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withCobols(ListUtils.map(d.getCobols(), t -> visit(t, p)));
        return d;
    }

    public Cobol visitDataWithLowerBoundsClause(Cobol.DataWithLowerBoundsClause dataWithLowerBoundsClause, P p) {
        Cobol.DataWithLowerBoundsClause d = dataWithLowerBoundsClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDecimalPointClause(Cobol.DecimalPointClause decimalPointClause, P p) {
        Cobol.DecimalPointClause d = decimalPointClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDefaultComputationalSignClause(Cobol.DefaultComputationalSignClause defaultComputationalSignClause, P p) {
        Cobol.DefaultComputationalSignClause d = defaultComputationalSignClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDefaultDisplaySignClause(Cobol.DefaultDisplaySignClause defaultDisplaySignClause, P p) {
        Cobol.DefaultDisplaySignClause d = defaultDisplaySignClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDelete(Cobol.Delete delete, P p) {
        Cobol.Delete d = delete;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withInvalidKey((Cobol.StatementPhrase) visit(d.getInvalidKey(), p));
        d = d.withNotInvalidKey((Cobol.StatementPhrase) visit(d.getNotInvalidKey(), p));
        d = d.withEndDelete((Cobol.CobolWord) visit(d.getEndDelete(), p));
        return d;
    }

    public Cobol visitDestinationCountClause(Cobol.DestinationCountClause destinationCountClause, P p) {
        Cobol.DestinationCountClause d = destinationCountClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withDataDescName((Cobol.CobolWord) visit(d.getDataDescName(), p));
        return d;
    }

    public Cobol visitDestinationTableClause(Cobol.DestinationTableClause destinationTableClause, P p) {
        Cobol.DestinationTableClause d = destinationTableClause;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withIntegerLiteral((Cobol.CobolWord) visit(d.getIntegerLiteral(), p));
        d = d.withIndexNames(ListUtils.map(d.getIndexNames(), t -> (Cobol.CobolWord) visit(t, p)));
        return d;
    }

    public Cobol visitDisable(Cobol.Disable disable, P p) {
        Cobol.Disable d = disable;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitDisplay(Cobol.Display display, P p) {
        Cobol.Display d = display;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withOperands(ListUtils.map(d.getOperands(), t -> (Name) visit(t, p)));
        return d;
    }

    public Cobol visitDisplayAt(Cobol.DisplayAt displayAt, P p) {
        Cobol.DisplayAt d = displayAt;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withAt((Cobol.CobolWord) visit(d.getAt(), p));
        return d;
    }

    public Cobol visitDisplayUpon(Cobol.DisplayUpon displayUpon, P p) {
        Cobol.DisplayUpon d = displayUpon;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withUpon((Cobol.CobolWord) visit(d.getUpon(), p));
        d = d.withName((Cobol.CobolWord) visit(d.getName(), p));
        return d;
    }

    public Cobol visitDivide(Cobol.Divide divide, P p) {
        Cobol.Divide d = divide;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withAction(visit(d.getAction(), p));
        d = d.withOnSizeErrorPhrase((Cobol.StatementPhrase) visit(d.getOnSizeErrorPhrase(), p));
        d = d.withNotOnSizeErrorPhrase((Cobol.StatementPhrase) visit(d.getNotOnSizeErrorPhrase(), p));
        d = d.withEndDivide((Cobol.CobolWord) visit(d.getEndDivide(), p));
        return d;
    }

    public Cobol visitDivideGiving(Cobol.DivideGiving divideGiving, P p) {
        Cobol.DivideGiving d = divideGiving;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withDivideGivingPhrase((Cobol.DivideGivingPhrase) visit(d.getDivideGivingPhrase(), p));
        return d;
    }

    public Cobol visitDivideGivingPhrase(Cobol.DivideGivingPhrase divideGivingPhrase, P p) {
        Cobol.DivideGivingPhrase d = divideGivingPhrase;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withRoundable(ListUtils.map(d.getRoundable(), t -> (Cobol.Roundable) visit(t, p)));
        return d;
    }

    public Cobol visitDivideInto(Cobol.DivideInto divideInto, P p) {
        Cobol.DivideInto d = divideInto;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        d = d.withRoundable(ListUtils.map(d.getRoundable(), t -> (Cobol.Roundable) visit(t, p)));
        return d;
    }

    public Cobol visitDivideRemainder(Cobol.DivideRemainder divideRemainder, P p) {
        Cobol.DivideRemainder d = divideRemainder;
        d = d.withPrefix(visitSpace(d.getPrefix(), p));
        d = d.withMarkers(visitMarkers(d.getMarkers(), p));
        return d;
    }

    public Cobol visitEnable(Cobol.Enable enable, P p) {
        Cobol.Enable e = enable;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        return e;
    }

    public Cobol visitEndKeyClause(Cobol.EndKeyClause endKeyClause, P p) {
        Cobol.EndKeyClause e = endKeyClause;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withWords(ListUtils.map(e.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        e = e.withName((Cobol.CobolWord) visit(e.getName(), p));
        return e;
    }

    public Cobol visitEndProgram(Cobol.EndProgram endProgram, P p) {
        Cobol.EndProgram e = endProgram;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        return e;
    }

    public Cobol.Entry visitEntry(Cobol.Entry entry, P p) {
        Cobol.Entry e = entry;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        return e;
    }

    public Cobol visitEnvironmentDivision(Cobol.EnvironmentDivision environmentDivision, P p) {
        Cobol.EnvironmentDivision e = environmentDivision;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withWords(ListUtils.map(e.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        e = e.withDot((Cobol.CobolWord) visit(e.getDot(), p));
        e = e.withBody(ListUtils.map(e.getBody(), it -> visit(it, p)));
        return e;
    }

    public Cobol visitEvaluate(Cobol.Evaluate evaluate, P p) {
        Cobol.Evaluate e = evaluate;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withSelect(visit(e.getSelect(), p));
        e = e.withAlsoSelect(ListUtils.map(e.getAlsoSelect(), t -> (Cobol.EvaluateAlso) visit(t, p)));
        e = e.withWhenPhrase(ListUtils.map(e.getWhenPhrase(), t -> (Cobol.EvaluateWhenPhrase) visit(t, p)));
        e = e.withWhenOther((Cobol.StatementPhrase) visit(e.getWhenOther(), p));
        e = e.withEndPhrase((Cobol.CobolWord) visit(e.getEndPhrase(), p));
        return e;
    }

    public Cobol visitEvaluateAlso(Cobol.EvaluateAlso evaluateAlso, P p) {
        Cobol.EvaluateAlso e = evaluateAlso;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withSelect(visit(e.getSelect(), p));
        return e;
    }

    public Cobol visitEvaluateAlsoCondition(Cobol.EvaluateAlsoCondition evaluateAlsoCondition, P p) {
        Cobol.EvaluateAlsoCondition e = evaluateAlsoCondition;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withCondition((Cobol.EvaluateCondition) visit(e.getCondition(), p));
        return e;
    }

    public Cobol visitEvaluateCondition(Cobol.EvaluateCondition evaluateCondition, P p) {
        Cobol.EvaluateCondition e = evaluateCondition;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withCondition(visit(e.getCondition(), p));
        e = e.withEvaluateThrough((Cobol.EvaluateThrough) visit(e.getEvaluateThrough(), p));
        return e;
    }

    public Cobol visitEvaluateThrough(Cobol.EvaluateThrough evaluateThrough, P p) {
        Cobol.EvaluateThrough e = evaluateThrough;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withValue(visit(e.getValue(), p));
        return e;
    }

    public Cobol visitEvaluateValueThrough(Cobol.EvaluateValueThrough evaluateValueThrough, P p) {
        Cobol.EvaluateValueThrough e = evaluateValueThrough;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withValue(visit(e.getValue(), p));
        e = e.withEvaluateThrough((Cobol.EvaluateThrough) visit(e.getEvaluateThrough(), p));
        return e;
    }

    public Cobol visitEvaluateWhen(Cobol.EvaluateWhen evaluateWhen, P p) {
        Cobol.EvaluateWhen e = evaluateWhen;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withCondition((Cobol.EvaluateCondition) visit(e.getCondition(), p));
        e = e.withAlsoCondition(ListUtils.map(e.getAlsoCondition(), t -> (Cobol.EvaluateAlsoCondition) visit(t, p)));
        return e;
    }

    public Cobol visitEvaluateWhenPhrase(Cobol.EvaluateWhenPhrase evaluateWhenPhrase, P p) {
        Cobol.EvaluateWhenPhrase e = evaluateWhenPhrase;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withWhens(ListUtils.map(e.getWhens(), t -> (Cobol.EvaluateWhen) visit(t, p)));
        e = e.withStatements(ListUtils.map(e.getStatements(), t -> (Statement) visit(t, p)));
        return e;
    }

    public Cobol visitExecCicsStatement(Cobol.ExecCicsStatement execCicsStatement, P p) {
        Cobol.ExecCicsStatement e = execCicsStatement;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withExecCicsLines(ListUtils.map(e.getExecCicsLines(), t -> (Cobol.CobolWord) visit(t, p)));
        return e;
    }

    public Cobol visitExecSqlImsStatement(Cobol.ExecSqlImsStatement execSqlImsStatement, P p) {
        Cobol.ExecSqlImsStatement e = execSqlImsStatement;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withExecSqlLmsLines(ListUtils.map(e.getExecSqlLmsLines(), t -> (Cobol.CobolWord) visit(t, p)));
        return e;
    }

    public Cobol visitExecSqlStatement(Cobol.ExecSqlStatement execSqlStatement, P p) {
        Cobol.ExecSqlStatement e = execSqlStatement;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withExecSqlLines(ListUtils.map(e.getExecSqlLines(), t -> (Cobol.CobolWord) visit(t, p)));
        return e;
    }

    public Cobol visitExhibit(Cobol.Exhibit exhibit, P p) {
        Cobol.Exhibit e = exhibit;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        e = e.withOperands(ListUtils.map(e.getOperands(), t -> (Identifier) visit(t, p)));
        return e;
    }

    public Cobol visitExit(Cobol.Exit exit, P p) {
        Cobol.Exit e = exit;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        return e;
    }

    public Cobol visitExternalClause(Cobol.ExternalClause externalClause, P p) {
        Cobol.ExternalClause e = externalClause;
        e = e.withPrefix(visitSpace(e.getPrefix(), p));
        e = e.withMarkers(visitMarkers(e.getMarkers(), p));
        return e;
    }

    public Cobol visitFileControlEntry(Cobol.FileControlEntry fileControlEntry, P p) {
        Cobol.FileControlEntry f = fileControlEntry;
        f = f.withPrefix(visitSpace(f.getPrefix(), p));
        f = f.withMarkers(visitMarkers(f.getMarkers(), p));
        f = f.withSelectClause(visit(f.getSelectClause(), p));
        f = f.withControlClauses(ListUtils.map(f.getControlClauses(), t -> visit(t, p)));
        return f;
    }

    public Cobol visitFileControlParagraph(Cobol.FileControlParagraph fileControlParagraph, P p) {
        Cobol.FileControlParagraph f = fileControlParagraph;
        f = f.withPrefix(visitSpace(f.getPrefix(), p));
        f = f.withMarkers(visitMarkers(f.getMarkers(), p));
        f = f.withControlEntries(ListUtils.map(f.getControlEntries(), t -> visit(t, p)));
        f = f.withDot((Cobol.CobolWord) visit(f.getDot(), p));
        return f;
    }

    public Cobol visitFileDescriptionEntry(Cobol.FileDescriptionEntry fileDescriptionEntry, P p) {
        Cobol.FileDescriptionEntry f = fileDescriptionEntry;
        f = f.withPrefix(visitSpace(f.getPrefix(), p));
        f = f.withMarkers(visitMarkers(f.getMarkers(), p));
        f = f.withWords((Cobol.CobolWord) visit(f.getWords(), p));
        f = f.withName((Cobol.CobolWord) visit(f.getName(), p));
        f = f.withClauses(ListUtils.map(f.getClauses(), it -> visit(it, p)));
        f = f.withDot((Cobol.CobolWord) visit(f.getDot(), p));
        f = f.withDataDescriptions(ListUtils.map(f.getDataDescriptions(), it -> (Cobol.DataDescriptionEntry) visit(it, p)));
        return f;
    }

    public Cobol visitFileSection(Cobol.FileSection fileSection, P p) {
        Cobol.FileSection f = fileSection;
        f = f.withPrefix(visitSpace(f.getPrefix(), p));
        f = f.withMarkers(visitMarkers(f.getMarkers(), p));
        f = f.withWords(ListUtils.map(f.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        f = f.withDot((Cobol.CobolWord) visit(f.getDot(), p));
        f = f.withFileDescriptionEntry(ListUtils.map(f.getFileDescriptionEntry(), it -> (Cobol.FileDescriptionEntry) visit(it, p)));
        return f;
    }

    public Cobol visitFileStatusClause(Cobol.FileStatusClause fileStatusClause, P p) {
        Cobol.FileStatusClause f = fileStatusClause;
        f = f.withPrefix(visitSpace(f.getPrefix(), p));
        f = f.withMarkers(visitMarkers(f.getMarkers(), p));
        f = f.withQualifiedDataNames(ListUtils.map(f.getQualifiedDataNames(), t -> (Cobol.QualifiedDataName) visit(t, p)));
        return f;
    }

    public Cobol visitFunctionCall(Cobol.FunctionCall functionCall, P p) {
        Cobol.FunctionCall f = functionCall;
        f = f.withPrefix(visitSpace(f.getPrefix(), p));
        f = f.withMarkers(visitMarkers(f.getMarkers(), p));
        f = f.withFunctionName((Cobol.CobolWord) visit(f.getFunctionName(), p));
        f = f.withArguments(ListUtils.map(f.getArguments(), t -> (Cobol.Parenthesized) visit(t, p)));
        f = f.withReferenceModifier((Cobol.ReferenceModifier) visit(f.getReferenceModifier(), p));
        return f;
    }

    public Cobol visitGenerate(Cobol.Generate generate, P p) {
        Cobol.Generate g = generate;
        g = g.withPrefix(visitSpace(g.getPrefix(), p));
        g = g.withMarkers(visitMarkers(g.getMarkers(), p));
        g = g.withReportName((Cobol.QualifiedDataName) visit(g.getReportName(), p));
        return g;
    }

    public Cobol visitGlobalClause(Cobol.GlobalClause globalClause, P p) {
        Cobol.GlobalClause g = globalClause;
        g = g.withPrefix(visitSpace(g.getPrefix(), p));
        g = g.withMarkers(visitMarkers(g.getMarkers(), p));
        return g;
    }

    public Cobol visitGoBack(Cobol.GoBack goBack, P p) {
        Cobol.GoBack g = goBack;
        g = g.withPrefix(visitSpace(g.getPrefix(), p));
        g = g.withMarkers(visitMarkers(g.getMarkers(), p));
        g = g.withGoBack((Cobol.CobolWord) visit(g.getGoBack(), p));
        return g;
    }

    public Cobol visitGoTo(Cobol.GoTo _goTo, P p) {
        Cobol.GoTo g = _goTo;
        g = g.withPrefix(visitSpace(g.getPrefix(), p));
        g = g.withMarkers(visitMarkers(g.getMarkers(), p));
        g = g.withStatement(visit(g.getStatement(), p));
        return g;
    }

    public Cobol visitGoToDependingOnStatement(Cobol.GoToDependingOnStatement goToDependingOnStatement, P p) {
        Cobol.GoToDependingOnStatement g = goToDependingOnStatement;
        g = g.withPrefix(visitSpace(g.getPrefix(), p));
        g = g.withMarkers(visitMarkers(g.getMarkers(), p));
        g = g.withProcedureNames(ListUtils.map(g.getProcedureNames(), t -> (Cobol.ProcedureName) visit(t, p)));
        return g;
    }

    public Cobol visitIdentificationDivision(Cobol.IdentificationDivision identificationDivision, P p) {
        Cobol.IdentificationDivision i = identificationDivision;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withWords(ListUtils.map(i.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        i = i.withProgramIdParagraph((Cobol.ProgramIdParagraph) visit(i.getProgramIdParagraph(), p));
        return i;
    }

    public Cobol visitIf(Cobol.If _if, P p) {
        Cobol.If i = _if;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withCondition((Cobol.Condition) visit(i.getCondition(), p));
        i = i.withIfThen((Cobol.IfThen) visit(i.getIfThen(), p));
        i = i.withIfThen((Cobol.IfThen) visit(i.getIfThen(), p));
        i = i.withEndIf((Cobol.CobolWord) visit(i.getEndIf(), p));
        return i;
    }

    public Cobol visitIfElse(Cobol.IfElse ifElse, P p) {
        Cobol.IfElse i = ifElse;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withStatements(ListUtils.map(i.getStatements(), t -> (Statement) visit(t, p)));
        return i;
    }

    public Cobol visitIfThen(Cobol.IfThen ifThen, P p) {
        Cobol.IfThen i = ifThen;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withStatements(ListUtils.map(i.getStatements(), t -> (Statement) visit(t, p)));
        return i;
    }

    public Cobol visitInData(Cobol.InData inData, P p) {
        Cobol.InData i = inData;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }

    public Cobol visitInFile(Cobol.InFile inFile, P p) {
        Cobol.InFile i = inFile;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }

    public Cobol visitInLibrary(Cobol.InLibrary inLibrary, P p) {
        Cobol.InLibrary i = inLibrary;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }

    public Cobol visitInMnemonic(Cobol.InMnemonic inMnemonic, P p) {
        Cobol.InMnemonic i = inMnemonic;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }

    public Cobol visitInSection(Cobol.InSection inSection, P p) {
        Cobol.InSection i = inSection;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }

    public Cobol visitInTable(Cobol.InTable inTable, P p) {
        Cobol.InTable i = inTable;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        return i;
    }

    public Cobol visitInitialize(Cobol.Initialize initialize, P p) {
        Cobol.Initialize i = initialize;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withIdentifiers(ListUtils.map(i.getIdentifiers(), t -> (Identifier) visit(t, p)));
        i = i.withInitializeReplacingPhrase((Cobol.InitializeReplacingPhrase) visit(i.getInitializeReplacingPhrase(), p));
        return i;
    }

    public Cobol visitInitializeReplacingBy(Cobol.InitializeReplacingBy initializeReplacingBy, P p) {
        Cobol.InitializeReplacingBy i = initializeReplacingBy;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withIdentifier((Cobol.CobolWord) visit(i.getIdentifier(), p));
        return i;
    }

    public Cobol visitInitializeReplacingPhrase(Cobol.InitializeReplacingPhrase initializeReplacingPhrase, P p) {
        Cobol.InitializeReplacingPhrase i = initializeReplacingPhrase;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withInitializeReplacingBy(ListUtils.map(i.getInitializeReplacingBy(), t -> (Cobol.InitializeReplacingBy) visit(t, p)));
        return i;
    }

    public Cobol visitInitiate(Cobol.Initiate initiate, P p) {
        Cobol.Initiate i = initiate;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withReportNames(ListUtils.map(i.getReportNames(), t -> (Cobol.QualifiedDataName) visit(t, p)));
        return i;
    }

    public Cobol visitInputOutputSection(Cobol.InputOutputSection inputOutputSection, P p) {
        Cobol.InputOutputSection i = inputOutputSection;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withParagraphs(ListUtils.map(i.getParagraphs(), t -> visit(t, p)));
        return i;
    }

    public Cobol visitInspect(Cobol.Inspect inspect, P p) {
        Cobol.Inspect i = inspect;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withPhrase(visit(i.getPhrase(), p));
        return i;
    }

    public Cobol visitInspectAllLeading(Cobol.InspectAllLeading inspectAllLeading, P p) {
        Cobol.InspectAllLeading i = inspectAllLeading;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withWord((Cobol.CobolWord) visit(i.getWord(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), t -> (Cobol.InspectBeforeAfter) visit(t, p)));
        return i;
    }

    public Cobol visitInspectAllLeadings(Cobol.InspectAllLeadings inspectAllLeadings, P p) {
        Cobol.InspectAllLeadings i = inspectAllLeadings;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.getPadding().withLeadings(visitContainer(i.getPadding().getLeadings(), p));
        return i;
    }

    public Cobol visitInspectBeforeAfter(Cobol.InspectBeforeAfter inspectBeforeAfter, P p) {
        Cobol.InspectBeforeAfter i = inspectBeforeAfter;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withIdentifier((Name) visit(i.getIdentifier(), p));
        return i;
    }

    public Cobol visitInspectBy(Cobol.InspectBy inspectBy, P p) {
        Cobol.InspectBy i = inspectBy;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withIdentifier((Name) visit(i.getIdentifier(), p));
        return i;
    }

    public Cobol visitInspectCharacters(Cobol.InspectCharacters inspectCharacters, P p) {
        Cobol.InspectCharacters i = inspectCharacters;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), t -> (Cobol.InspectBeforeAfter) visit(t, p)));
        return i;
    }

    public Cobol visitInspectConvertingPhrase(Cobol.InspectConvertingPhrase inspectConvertingPhrase, P p) {
        Cobol.InspectConvertingPhrase i = inspectConvertingPhrase;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withIdentifier((Name) visit(i.getIdentifier(), p));
        i = i.withInspectTo((Cobol.InspectTo) visit(i.getInspectTo(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), t -> (Cobol.InspectBeforeAfter) visit(t, p)));
        return i;
    }

    public Cobol visitInspectFor(Cobol.InspectFor inspectFor, P p) {
        Cobol.InspectFor i = inspectFor;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.getPadding().withInspects(visitContainer(i.getPadding().getInspects(), p));
        return i;
    }

    public Cobol visitInspectReplacingAllLeading(Cobol.InspectReplacingAllLeading inspectReplacingAllLeading, P p) {
        Cobol.InspectReplacingAllLeading i = inspectReplacingAllLeading;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withIdentifier((Name) visit(i.getIdentifier(), p));
        i = i.withInspectBy((Cobol.InspectBy) visit(i.getInspectBy(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), t -> (Cobol.InspectBeforeAfter) visit(t, p)));
        return i;
    }

    public Cobol visitInspectReplacingAllLeadings(Cobol.InspectReplacingAllLeadings inspectReplacingAllLeadings, P p) {
        Cobol.InspectReplacingAllLeadings i = inspectReplacingAllLeadings;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), t -> (Cobol.InspectReplacingAllLeading) visit(t, p)));
        return i;
    }

    public Cobol visitInspectReplacingCharacters(Cobol.InspectReplacingCharacters inspectReplacingCharacters, P p) {
        Cobol.InspectReplacingCharacters i = inspectReplacingCharacters;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withInspectBy((Cobol.InspectBy) visit(i.getInspectBy(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), t -> (Cobol.InspectBeforeAfter) visit(t, p)));
        return i;
    }

    public Cobol visitInspectReplacingPhrase(Cobol.InspectReplacingPhrase inspectReplacingPhrase, P p) {
        Cobol.InspectReplacingPhrase i = inspectReplacingPhrase;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.getPadding().withInspections(visitContainer(i.getPadding().getInspections(), p));
        return i;
    }

    public Cobol visitInspectTallyingPhrase(Cobol.InspectTallyingPhrase inspectTallyingPhrase, P p) {
        Cobol.InspectTallyingPhrase i = inspectTallyingPhrase;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withInspectFors(ListUtils.map(i.getInspectFors(), t -> (Cobol.InspectFor) visit(t, p)));
        return i;
    }

    public Cobol visitInspectTallyingReplacingPhrase(Cobol.InspectTallyingReplacingPhrase inspectTallyingReplacingPhrase, P p) {
        Cobol.InspectTallyingReplacingPhrase i = inspectTallyingReplacingPhrase;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withInspectFors(ListUtils.map(i.getInspectFors(), t -> (Cobol.InspectFor) visit(t, p)));
        i = i.withReplacingPhrases(ListUtils.map(i.getReplacingPhrases(), t -> (Cobol.InspectReplacingPhrase) visit(t, p)));
        return i;
    }

    public Cobol visitInspectTo(Cobol.InspectTo inspectTo, P p) {
        Cobol.InspectTo i = inspectTo;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withIdentifier((Name) visit(i.getIdentifier(), p));
        return i;
    }

    public Cobol visitIoControlParagraph(Cobol.IoControlParagraph ioControlParagraph, P p) {
        Cobol.IoControlParagraph i = ioControlParagraph;
        i = i.withPrefix(visitSpace(i.getPrefix(), p));
        i = i.withMarkers(visitMarkers(i.getMarkers(), p));
        i = i.withDot((Cobol.CobolWord) visit(i.getDot(), p));
        i = i.withFileName((Cobol.CobolWord) visit(i.getFileName(), p));
        i = i.getPadding().withClauses(visitContainer(i.getPadding().getClauses(), p));
        return i;
    }

    public Cobol visitLabelRecordsClause(Cobol.LabelRecordsClause labelRecordsClause, P p) {
        Cobol.LabelRecordsClause l = labelRecordsClause;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withDataNames(ListUtils.map(l.getDataNames(), t -> (Cobol.CobolWord) visit(t, p)));
        return l;
    }

    public Cobol visitLibraryAttributeClauseFormat1(Cobol.LibraryAttributeClauseFormat1 libraryAttributeClauseFormat1, P p) {
        Cobol.LibraryAttributeClauseFormat1 l = libraryAttributeClauseFormat1;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLibraryAttributeClauseFormat2(Cobol.LibraryAttributeClauseFormat2 libraryAttributeClauseFormat2, P p) {
        Cobol.LibraryAttributeClauseFormat2 l = libraryAttributeClauseFormat2;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withLibraryAttributeFunction((Cobol.LibraryAttributeFunction) visit(l.getLibraryAttributeFunction(), p));
        l = l.withLibraryAttributeParameter((Cobol.LibraryAttributeParameter) visit(l.getLibraryAttributeParameter(), p));
        l = l.withLibraryAttributeTitle((Cobol.LibraryAttributeTitle) visit(l.getLibraryAttributeTitle(), p));
        return l;
    }

    public Cobol visitLibraryAttributeFunction(Cobol.LibraryAttributeFunction libraryAttributeFunction, P p) {
        Cobol.LibraryAttributeFunction l = libraryAttributeFunction;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLibraryAttributeParameter(Cobol.LibraryAttributeParameter libraryAttributeParameter, P p) {
        Cobol.LibraryAttributeParameter l = libraryAttributeParameter;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLibraryAttributeTitle(Cobol.LibraryAttributeTitle libraryAttributeTitle, P p) {
        Cobol.LibraryAttributeTitle l = libraryAttributeTitle;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLibraryDescriptionEntryFormat1(Cobol.LibraryDescriptionEntryFormat1 libraryDescriptionEntryFormat1, P p) {
        Cobol.LibraryDescriptionEntryFormat1 l = libraryDescriptionEntryFormat1;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withLibraryName((Cobol.CobolWord) visit(l.getLibraryName(), p));
        l = l.withLibraryAttributeClauseFormat1((Cobol.LibraryAttributeClauseFormat1) visit(l.getLibraryAttributeClauseFormat1(), p));
        l = l.withLibraryEntryProcedureClauseFormat1((Cobol.LibraryEntryProcedureClauseFormat1) visit(l.getLibraryEntryProcedureClauseFormat1(), p));
        return l;
    }

    public Cobol visitLibraryDescriptionEntryFormat2(Cobol.LibraryDescriptionEntryFormat2 libraryDescriptionEntryFormat2, P p) {
        Cobol.LibraryDescriptionEntryFormat2 l = libraryDescriptionEntryFormat2;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withLibraryName((Cobol.CobolWord) visit(l.getLibraryName(), p));
        l = l.withLibraryIsGlobalClause((Cobol.LibraryIsGlobalClause) visit(l.getLibraryIsGlobalClause(), p));
        l = l.withLibraryIsCommonClause((Cobol.LibraryIsCommonClause) visit(l.getLibraryIsCommonClause(), p));
        l = l.getPadding().withClauseFormats(visitContainer(l.getPadding().getClauseFormats(), p));
        return l;
    }

    public Cobol visitLibraryEntryProcedureClauseFormat1(Cobol.LibraryEntryProcedureClauseFormat1 libraryEntryProcedureClauseFormat1, P p) {
        Cobol.LibraryEntryProcedureClauseFormat1 l = libraryEntryProcedureClauseFormat1;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withProgramName((Cobol.CobolWord) visit(l.getProgramName(), p));
        l = l.withLibraryEntryProcedureForClause((Cobol.LibraryEntryProcedureForClause) visit(l.getLibraryEntryProcedureForClause(), p));
        return l;
    }

    public Cobol visitLibraryEntryProcedureClauseFormat2(Cobol.LibraryEntryProcedureClauseFormat2 libraryEntryProcedureClauseFormat2, P p) {
        Cobol.LibraryEntryProcedureClauseFormat2 l = libraryEntryProcedureClauseFormat2;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withProgramName((Cobol.CobolWord) visit(l.getProgramName(), p));
        l = l.withLibraryEntryProcedureForClause((Cobol.LibraryEntryProcedureForClause) visit(l.getLibraryEntryProcedureForClause(), p));
        l = l.withLibraryEntryProcedureWithClause((Cobol.LibraryEntryProcedureWithClause) visit(l.getLibraryEntryProcedureWithClause(), p));
        l = l.withLibraryEntryProcedureUsingClause((Cobol.LibraryEntryProcedureUsingClause) visit(l.getLibraryEntryProcedureUsingClause(), p));
        l = l.withLibraryEntryProcedureGivingClause((Cobol.LibraryEntryProcedureGivingClause) visit(l.getLibraryEntryProcedureGivingClause(), p));
        return l;
    }

    public Cobol visitLibraryEntryProcedureForClause(Cobol.LibraryEntryProcedureForClause libraryEntryProcedureForClause, P p) {
        Cobol.LibraryEntryProcedureForClause l = libraryEntryProcedureForClause;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLibraryEntryProcedureGivingClause(Cobol.LibraryEntryProcedureGivingClause libraryEntryProcedureGivingClause, P p) {
        Cobol.LibraryEntryProcedureGivingClause l = libraryEntryProcedureGivingClause;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withDataName((Cobol.CobolWord) visit(l.getDataName(), p));
        return l;
    }

    public Cobol visitLibraryEntryProcedureUsingClause(Cobol.LibraryEntryProcedureUsingClause libraryEntryProcedureUsingClause, P p) {
        Cobol.LibraryEntryProcedureUsingClause l = libraryEntryProcedureUsingClause;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withNames(ListUtils.map(l.getNames(), t -> (Cobol.CobolWord) visit(t, p)));
        return l;
    }

    public Cobol visitLibraryEntryProcedureWithClause(Cobol.LibraryEntryProcedureWithClause libraryEntryProcedureWithClause, P p) {
        Cobol.LibraryEntryProcedureWithClause l = libraryEntryProcedureWithClause;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withNames(ListUtils.map(l.getNames(), t -> (Cobol.CobolWord) visit(t, p)));
        return l;
    }

    public Cobol visitLibraryIsCommonClause(Cobol.LibraryIsCommonClause libraryIsCommonClause, P p) {
        Cobol.LibraryIsCommonClause l = libraryIsCommonClause;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLibraryIsGlobalClause(Cobol.LibraryIsGlobalClause libraryIsGlobalClause, P p) {
        Cobol.LibraryIsGlobalClause l = libraryIsGlobalClause;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLinageClause(Cobol.LinageClause linageClause, P p) {
        Cobol.LinageClause l = linageClause;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withLinageAt(ListUtils.map(l.getLinageAt(), t -> visit(t, p)));
        return l;
    }

    public Cobol visitLinageFootingAt(Cobol.LinageFootingAt linageFootingAt, P p) {
        Cobol.LinageFootingAt l = linageFootingAt;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLinageLinesAtBottom(Cobol.LinageLinesAtBottom linageLinesAtBottom, P p) {
        Cobol.LinageLinesAtBottom l = linageLinesAtBottom;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLinageLinesAtTop(Cobol.LinageLinesAtTop linageLinesAtTop, P p) {
        Cobol.LinageLinesAtTop l = linageLinesAtTop;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        return l;
    }

    public Cobol visitLinkageSection(Cobol.LinkageSection linkageSection, P p) {
        Cobol.LinkageSection l = linkageSection;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withWords(ListUtils.map(l.getWords(), t -> (Cobol.CobolWord) visit(t, p)));
        l = l.withDot((Cobol.CobolWord) visit(l.getDot(), p));
        l = l.withDataDescriptions(ListUtils.map(l.getDataDescriptions(), t -> (Cobol.DataDescriptionEntry) visit(t, p)));
        return l;
    }

    public Cobol visitLocalStorageSection(Cobol.LocalStorageSection localStorageSection, P p) {
        Cobol.LocalStorageSection l = localStorageSection;
        l = l.withPrefix(visitSpace(l.getPrefix(), p));
        l = l.withMarkers(visitMarkers(l.getMarkers(), p));
        l = l.withWords(ListUtils.map(l.getWords(), t -> (Cobol.CobolWord) visit(t, p)));
        l = l.withDot((Cobol.CobolWord) visit(l.getDot(), p));
        l = l.withLocalName((Name) visit(l.getLocalName(), p));
        l = l.withDot2((Cobol.CobolWord) visit(l.getDot2(), p));
        l = l.withDataDescriptions(ListUtils.map(l.getDataDescriptions(), t -> (Cobol.DataDescriptionEntry) visit(t, p)));
        return l;
    }

    public Cobol visitMerge(Cobol.Merge merge, P p) {
        Cobol.Merge m = merge;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withMergeOnKeyClause(ListUtils.map(m.getMergeOnKeyClause(), t -> (Cobol.MergeOnKeyClause) visit(t, p)));
        m = m.withMergeCollatingSequencePhrase((Cobol.MergeCollatingSequencePhrase) visit(m.getMergeCollatingSequencePhrase(), p));
        m = m.withMergeUsing(ListUtils.map(m.getMergeUsing(), t -> (Cobol.MergeUsing) visit(t, p)));
        m = m.withMergeOutputProcedurePhrase((Cobol.MergeOutputProcedurePhrase) visit(m.getMergeOutputProcedurePhrase(), p));
        m = m.withMergeGivingPhrase(ListUtils.map(m.getMergeGivingPhrase(), t -> (Cobol.MergeGivingPhrase) visit(t, p)));
        return m;
    }

    public Cobol visitMergeCollatingSequencePhrase(Cobol.MergeCollatingSequencePhrase mergeCollatingSequencePhrase, P p) {
        Cobol.MergeCollatingSequencePhrase m = mergeCollatingSequencePhrase;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withMergeCollatingAlphanumeric((Cobol.Mergeable) visit(m.getMergeCollatingAlphanumeric(), p));
        m = m.withMergeCollatingNational((Cobol.Mergeable) visit(m.getMergeCollatingNational(), p));
        return m;
    }

    public Cobol visitMergeGiving(Cobol.MergeGiving mergeGiving, P p) {
        Cobol.MergeGiving m = mergeGiving;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        return m;
    }

    public Cobol visitMergeGivingPhrase(Cobol.MergeGivingPhrase mergeGivingPhrase, P p) {
        Cobol.MergeGivingPhrase m = mergeGivingPhrase;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withMergeGiving(ListUtils.map(m.getMergeGiving(), t -> (Cobol.MergeGiving) visit(t, p)));
        return m;
    }

    public Cobol visitMergeOnKeyClause(Cobol.MergeOnKeyClause mergeOnKeyClause, P p) {
        Cobol.MergeOnKeyClause m = mergeOnKeyClause;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withQualifiedDataName(ListUtils.map(m.getQualifiedDataName(), t -> (Cobol.QualifiedDataName) visit(t, p)));
        return m;
    }

    public Cobol visitMergeOutputProcedurePhrase(Cobol.MergeOutputProcedurePhrase mergeOutputProcedurePhrase, P p) {
        Cobol.MergeOutputProcedurePhrase m = mergeOutputProcedurePhrase;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withProcedureName((Cobol.ProcedureName) visit(m.getProcedureName(), p));
        m = m.withMergeOutputThrough((Cobol.MergeOutputThrough) visit(m.getMergeOutputThrough(), p));
        return m;
    }

    public @Nullable Cobol visitMergeOutputThrough(Cobol.MergeOutputThrough mergeOutputThrough, P p) {
        Cobol.MergeOutputThrough m = mergeOutputThrough;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withProcedureName((Cobol.ProcedureName) visit(m.getProcedureName(), p));
        return m;
    }

    public @Nullable Cobol visitMergeUsing(Cobol.MergeUsing mergeUsing, P p) {
        Cobol.MergeUsing m = mergeUsing;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withFileNames(ListUtils.map(m.getFileNames(), t -> (Name) visit(t, p)));
        return m;
    }

    public Cobol visitMergeable(Cobol.Mergeable mergeable, P p) {
        Cobol.Mergeable m = mergeable;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        return m;
    }

    public Cobol visitMessageCountClause(Cobol.MessageCountClause messageCountClause, P p) {
        Cobol.MessageCountClause m = messageCountClause;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withDataDescName((Cobol.CobolWord) visit(m.getDataDescName(), p));
        return m;
    }

    public Cobol visitMessageDateClause(Cobol.MessageDateClause messageDateClause, P p) {
        Cobol.MessageDateClause m = messageDateClause;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withDataDescName((Cobol.CobolWord) visit(m.getDataDescName(), p));
        return m;
    }

    public Cobol visitMessageTimeClause(Cobol.MessageTimeClause messageTimeClause, P p) {
        Cobol.MessageTimeClause m = messageTimeClause;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withDataDescName((Cobol.CobolWord) visit(m.getDataDescName(), p));
        return m;
    }

    public Cobol visitMoveCorrespondingToStatement(Cobol.MoveCorrespondingToStatement moveCorrespondingToStatement, P p) {
        Cobol.MoveCorrespondingToStatement m = moveCorrespondingToStatement;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withMoveCorrespondingToSendingArea((Identifier) visit(m.getMoveCorrespondingToSendingArea(), p));
        m = m.getPadding().withTo(visitContainer(m.getPadding().getTo(), p));
        return m;
    }

    public Cobol visitMoveStatement(Cobol.MoveStatement moveStatement, P p) {
        Cobol.MoveStatement m = moveStatement;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withMoveToStatement(visit(m.getMoveToStatement(), p));
        return m;
    }

    public Cobol visitMoveToStatement(Cobol.MoveToStatement moveToStatement, P p) {
        Cobol.MoveToStatement m = moveToStatement;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withFrom((Name) visit(m.getFrom(), p));
        m = m.withTo((Cobol.CobolWord) visit(m.getTo(), p));
        m = m.withNames(ListUtils.map(m.getNames(), it -> (Identifier) visit(it, p)));
        return m;
    }

    public Cobol visitMultDiv(Cobol.MultDiv multDiv, P p) {
        Cobol.MultDiv m = multDiv;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withPowers((Cobol.Powers) visit(m.getPowers(), p));
        return m;
    }

    public Cobol visitMultDivs(Cobol.MultDivs multDivs, P p) {
        Cobol.MultDivs m = multDivs;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withPowers((Cobol.Powers) visit(m.getPowers(), p));
        m = m.withMultDivs(ListUtils.map(m.getMultDivs(), t -> (Cobol.MultDiv) visit(t, p)));
        return m;
    }

    public Cobol visitMultipleFileClause(Cobol.MultipleFileClause multipleFileClause, P p) {
        Cobol.MultipleFileClause m = multipleFileClause;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withFilePositions(ListUtils.map(m.getFilePositions(), t -> visit(t, p)));
        return m;
    }

    public Cobol visitMultipleFilePosition(Cobol.MultipleFilePosition multipleFilePosition, P p) {
        Cobol.MultipleFilePosition m = multipleFilePosition;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withFileName((Cobol.CobolWord) visit(m.getFileName(), p));
        m = m.withIntegerLiteral((Cobol.CobolWord) visit(m.getIntegerLiteral(), p));
        return m;
    }

    public Cobol visitMultiply(Cobol.Multiply multiply, P p) {
        Cobol.Multiply m = multiply;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withMultiply(visit(m.getMultiply(), p));
        m = m.withOnSizeErrorPhrase((Cobol.StatementPhrase) visit(m.getOnSizeErrorPhrase(), p));
        m = m.withNotOnSizeErrorPhrase((Cobol.StatementPhrase) visit(m.getNotOnSizeErrorPhrase(), p));
        return m;
    }

    public Cobol visitMultiplyGiving(Cobol.MultiplyGiving multiplyGiving, P p) {
        Cobol.MultiplyGiving m = multiplyGiving;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.getPadding().withResult(visitContainer(m.getPadding().getResult(), p));
        return m;
    }

    public Cobol visitMultiplyRegular(Cobol.MultiplyRegular multiplyRegular, P p) {
        Cobol.MultiplyRegular m = multiplyRegular;
        m = m.withPrefix(visitSpace(m.getPrefix(), p));
        m = m.withMarkers(visitMarkers(m.getMarkers(), p));
        m = m.withOperand(ListUtils.map(m.getOperand(), t -> (Cobol.Roundable) visit(t, p)));
        return m;
    }

    public Cobol visitNextSentence(Cobol.NextSentence nextSentence, P p) {
        Cobol.NextSentence n = nextSentence;
        n = n.withPrefix(visitSpace(n.getPrefix(), p));
        n = n.withMarkers(visitMarkers(n.getMarkers(), p));
        return n;
    }

    public Cobol visitObjectComputer(Cobol.ObjectComputer objectComputer, P p) {
        Cobol.ObjectComputer o = objectComputer;
        o = o.withPrefix(visitSpace(o.getPrefix(), p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        o = o.withWords(ListUtils.map(o.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        o = o.withComputer((Cobol.ObjectComputerDefinition) visit(o.getComputer(), p));
        o = o.withDot((Cobol.CobolWord) visit(o.getDot(), p));
        return o;
    }

    public Cobol visitObjectComputerDefinition(Cobol.ObjectComputerDefinition objectComputerDefinition, P p) {
        Cobol.ObjectComputerDefinition o = objectComputerDefinition;
        o = o.withPrefix(visitSpace(o.getPrefix(), p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        o = o.withSpecifications(ListUtils.map(o.getSpecifications(), t -> visit(t, p)));
        return o;
    }

    public Cobol visitOdtClause(Cobol.OdtClause odtClause, P p) {
        Cobol.OdtClause o = odtClause;
        o = o.withPrefix(visitSpace(o.getPrefix(), p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        o = o.withMnemonicName((Identifier) visit(o.getMnemonicName(), p));
        return o;
    }

    public Cobol visitOpen(Cobol.Open open, P p) {
        Cobol.Open o = open;
        o = o.withPrefix(visitSpace(o.getPrefix(), p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        o = o.getPadding().withOpen(visitContainer(o.getPadding().getOpen(), p));
        return o;
    }

    public Cobol visitOpenIOExtendStatement(Cobol.OpenIOExtendStatement openIOExtendStatement, P p) {
        Cobol.OpenIOExtendStatement o = openIOExtendStatement;
        o = o.withPrefix(visitSpace(o.getPrefix(), p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        o = o.withFileNames(ListUtils.map(o.getFileNames(), t -> (Name) visit(t, p)));
        return o;
    }

    public Cobol visitOpenInputOutputStatement(Cobol.OpenInputOutputStatement openInputOutputStatement, P p) {
        Cobol.OpenInputOutputStatement o = openInputOutputStatement;
        o = o.withPrefix(visitSpace(o.getPrefix(), p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        o = o.withOpenInput(ListUtils.map(o.getOpenInput(), t -> (Cobol.Openable) visit(t, p)));
        return o;
    }

    public Cobol visitOpenable(Cobol.Openable openable, P p) {
        Cobol.Openable o = openable;
        o = o.withPrefix(visitSpace(o.getPrefix(), p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        return o;
    }

    public Cobol visitOrganizationClause(Cobol.OrganizationClause organizationClause, P p) {
        Cobol.OrganizationClause o = organizationClause;
        o = o.withPrefix(visitSpace(o.getPrefix(), p));
        o = o.withMarkers(visitMarkers(o.getMarkers(), p));
        return o;
    }

    public Cobol visitPaddingCharacterClause(Cobol.PaddingCharacterClause paddingCharacterClause, P p) {
        Cobol.PaddingCharacterClause pp = paddingCharacterClause;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        return pp;
    }

    public Cobol visitParagraph(Cobol.Paragraph paragraph, P p) {
        Cobol.Paragraph pp = paragraph;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withSentences(ListUtils.map(pp.getSentences(), t -> (Cobol.Sentence) visit(t, p)));
        return pp;
    }

    public Cobol visitParagraphs(Cobol.Paragraphs paragraphs, P p) {
        Cobol.Paragraphs pp = paragraphs;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withSentences(ListUtils.map(pp.getSentences(), t -> (Cobol.Sentence) visit(t, p)));
        pp = pp.withParagraphs(ListUtils.map(pp.getParagraphs(), t -> (Cobol.Paragraph) visit(t, p)));
        return pp;
    }

    public Cobol visitParenthesized(Cobol.Parenthesized parenthesized, P p) {
        Cobol.Parenthesized pp = parenthesized;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withContents(ListUtils.map(pp.getContents(), it -> visit(it, p)));
        return pp;
    }

    public Cobol visitPasswordClause(Cobol.PasswordClause passwordClause, P p) {
        Cobol.PasswordClause pp = passwordClause;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withDataName((Cobol.CobolWord) visit(pp.getDataName(), p));
        return pp;
    }

    public Cobol visitPerform(Cobol.Perform perform, P p) {
        Cobol.Perform pp = perform;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withStatement(visit(pp.getStatement(), p));
        return pp;
    }

    public Cobol visitPerformFrom(Cobol.PerformFrom performFrom, P p) {
        Cobol.PerformFrom pp = performFrom;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withFrom(visit(pp.getFrom(), p));
        return pp;
    }

    public Cobol visitPerformInlineStatement(Cobol.PerformInlineStatement performInlineStatement, P p) {
        Cobol.PerformInlineStatement pp = performInlineStatement;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withPerformType(visit(pp.getPerformType(), p));
        pp = pp.withStatements(ListUtils.map(pp.getStatements(), t -> (Statement) visit(t, p)));
        return pp;
    }

    public Cobol visitPerformProcedureStatement(Cobol.PerformProcedureStatement performProcedureStatement, P p) {
        Cobol.PerformProcedureStatement pp = performProcedureStatement;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withProcedureName((Cobol.ProcedureName) visit(pp.getProcedureName(), p));
        pp = pp.withThroughProcedure((Cobol.ProcedureName) visit(pp.getThroughProcedure(), p));
        pp = pp.withPerformType(visit(pp.getPerformType(), p));
        return pp;
    }

    public Cobol visitPerformTestClause(Cobol.PerformTestClause performTestClause, P p) {
        Cobol.PerformTestClause pp = performTestClause;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        return pp;
    }

    public Cobol visitPerformTimes(Cobol.PerformTimes performTimes, P p) {
        Cobol.PerformTimes pp = performTimes;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        return pp;
    }

    public Cobol visitPerformUntil(Cobol.PerformUntil performUntil, P p) {
        Cobol.PerformUntil pp = performUntil;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withPerformTestClause((Cobol.PerformTestClause) visit(pp.getPerformTestClause(), p));
        pp = pp.withCondition((Cobol.Condition) visit(pp.getCondition(), p));
        return pp;
    }

    public Cobol visitPerformVarying(Cobol.PerformVarying performVarying, P p) {
        Cobol.PerformVarying pp = performVarying;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withFirst(visit(pp.getFirst(), p));
        pp = pp.withSecond(visit(pp.getSecond(), p));
        return pp;
    }

    public Cobol visitPerformVaryingClause(Cobol.PerformVaryingClause performVaryingClause, P p) {
        Cobol.PerformVaryingClause pp = performVaryingClause;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withPerformVaryingPhrase((Cobol.PerformVaryingPhrase) visit(pp.getPerformVaryingPhrase(), p));
        pp = pp.withPerformAfter(ListUtils.map(pp.getPerformAfter(), t -> (Cobol.Performable) visit(t, p)));
        return pp;
    }

    public Cobol visitPerformVaryingPhrase(Cobol.PerformVaryingPhrase performVaryingPhrase, P p) {
        Cobol.PerformVaryingPhrase pp = performVaryingPhrase;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withFrom((Cobol.PerformFrom) visit(pp.getFrom(), p));
        pp = pp.withBy((Cobol.Performable) visit(pp.getBy(), p));
        pp = pp.withUntil((Cobol.PerformUntil) visit(pp.getUntil(), p));
        return pp;
    }

    public Cobol visitPerformable(Cobol.Performable performable, P p) {
        Cobol.Performable pp = performable;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withExpression(visit(pp.getExpression(), p));
        return pp;
    }

    public Cobol visitPicture(Cobol.Picture picture, P p) {
        Cobol.Picture pp = picture;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withWords(ListUtils.map(pp.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        pp = pp.withParenthesized((Cobol.Parenthesized) visit(pp.getParenthesized(), p));
        return pp;
    }

    public Cobol visitPictureString(Cobol.PictureString pictureString, P p) {
        Cobol.PictureString pp = pictureString;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withPictures(ListUtils.map(pp.getPictures(), t -> (Cobol.Picture) visit(t, p)));
        return pp;
    }

    public Cobol visitPlusMinus(Cobol.PlusMinus plusMinus, P p) {
        Cobol.PlusMinus pp = plusMinus;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withMultDivs((Cobol.MultDivs) visit(pp.getMultDivs(), p));
        return pp;
    }

    public Cobol visitPower(Cobol.Power power, P p) {
        Cobol.Power pp = power;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withExpression(visit(pp.getExpression(), p));
        return pp;
    }

    public Cobol visitPowers(Cobol.Powers powers, P p) {
        Cobol.Powers pp = powers;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withExpression(visit(pp.getExpression(), p));
        pp = pp.withPowers(ListUtils.map(pp.getPowers(), t -> (Cobol.Power) visit(t, p)));
        return pp;
    }

    public Cobol visitProcedureDeclarative(Cobol.ProcedureDeclarative procedureDeclarative, P p) {
        Cobol.ProcedureDeclarative pp = procedureDeclarative;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withProcedureSectionHeader((Cobol.ProcedureSectionHeader) visit(pp.getProcedureSectionHeader(), p));
        pp = pp.withDot((Cobol.CobolWord) visit(pp.getDot(), p));
        pp = pp.withUseStatement((Cobol.UseStatement) visit(pp.getUseStatement(), p));
        pp = pp.withDot2((Cobol.CobolWord) visit(pp.getDot2(), p));
        pp = pp.withParagraphs((Cobol.Paragraphs) visit(pp.getParagraphs(), p));
        return pp;
    }

    public Cobol visitProcedureDeclaratives(Cobol.ProcedureDeclaratives procedureDeclaratives, P p) {
        Cobol.ProcedureDeclaratives pp = procedureDeclaratives;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withDeclaratives((Cobol.CobolWord) visit(pp.getDeclaratives(), p));
        pp = pp.withDot((Cobol.CobolWord) visit(pp.getDot(), p));
        pp = pp.withProcedureDeclarative(ListUtils.map(pp.getProcedureDeclarative(), it -> (Cobol.ProcedureDeclarative) visit(it, p)));
        pp = pp.withEndDeclaratives(ListUtils.map(pp.getEndDeclaratives(), it -> (Cobol.CobolWord) visit(it, p)));
        pp = pp.withDot2((Cobol.CobolWord) visit(pp.getDot2(), p));
        return pp;
    }

    public Cobol visitProcedureDivision(Cobol.ProcedureDivision procedureDivision, P p) {
        Cobol.ProcedureDivision pp = procedureDivision;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withWords(ListUtils.map(pp.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        pp = pp.withProcedureDivisionUsingClause((Cobol.ProcedureDivisionUsingClause) visit(pp.getProcedureDivisionUsingClause(), p));
        pp = pp.withProcedureDivisionGivingClause((Cobol.ProcedureDivisionGivingClause) visit(pp.getProcedureDivisionGivingClause(), p));
        pp = pp.withDot((Cobol.CobolWord) visit(pp.getDot(), p));
        pp = pp.withProcedureDeclaratives((Cobol.ProcedureDeclaratives) visit(pp.getProcedureDeclaratives(), p));
        pp = pp.withBody((Cobol.ProcedureDivisionBody) visit(pp.getBody(), p));
        return pp;
    }

    public Cobol visitProcedureDivisionBody(Cobol.ProcedureDivisionBody procedureDivisionBody, P p) {
        Cobol.ProcedureDivisionBody pp = procedureDivisionBody;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withParagraphs((Cobol.Paragraphs) visit(pp.getParagraphs(), p));
        return pp;
    }

    public Cobol visitProcedureDivisionByReference(Cobol.ProcedureDivisionByReference procedureDivisionByReference, P p) {
        Cobol.ProcedureDivisionByReference pp = procedureDivisionByReference;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        return pp;
    }

    public Cobol visitProcedureDivisionByReferencePhrase(Cobol.ProcedureDivisionByReferencePhrase procedureDivisionByReferencePhrase, P p) {
        Cobol.ProcedureDivisionByReferencePhrase pp = procedureDivisionByReferencePhrase;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withProcedureDivisionByReference(ListUtils.map(pp.getProcedureDivisionByReference(), t -> (Cobol.ProcedureDivisionByReference) visit(t, p)));
        return pp;
    }

    public Cobol visitProcedureDivisionByValuePhrase(Cobol.ProcedureDivisionByValuePhrase procedureDivisionByValuePhrase, P p) {
        Cobol.ProcedureDivisionByValuePhrase pp = procedureDivisionByValuePhrase;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withPhrases(ListUtils.map(pp.getPhrases(), t -> (Name) visit(t, p)));
        return pp;
    }

    public Cobol visitProcedureDivisionGivingClause(Cobol.ProcedureDivisionGivingClause procedureDivisionGivingClause, P p) {
        Cobol.ProcedureDivisionGivingClause pp = procedureDivisionGivingClause;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        return pp;
    }

    public Cobol visitProcedureDivisionUsingClause(Cobol.ProcedureDivisionUsingClause procedureDivisionUsingClause, P p) {
        Cobol.ProcedureDivisionUsingClause pp = procedureDivisionUsingClause;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withProcedureDivisionUsingParameter(ListUtils.map(pp.getProcedureDivisionUsingParameter(), t -> visit(t, p)));
        return pp;
    }

    public Cobol visitProcedureName(Cobol.ProcedureName procedureName, P p) {
        Cobol.ProcedureName pp = procedureName;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withInSection((Cobol.InSection) visit(pp.getInSection(), p));
        return pp;
    }

    public Cobol visitProcedureSection(Cobol.ProcedureSection procedureSection, P p) {
        Cobol.ProcedureSection pp = procedureSection;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withProcedureSectionHeader((Cobol.ProcedureSectionHeader) visit(pp.getProcedureSectionHeader(), p));
        pp = pp.withParagraphs((Cobol.Paragraphs) visit(pp.getParagraphs(), p));
        return pp;
    }

    public Cobol visitProcedureSectionHeader(Cobol.ProcedureSectionHeader procedureSectionHeader, P p) {
        Cobol.ProcedureSectionHeader pp = procedureSectionHeader;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        return pp;
    }

    public Cobol visitProgramIdParagraph(Cobol.ProgramIdParagraph programIdParagraph, P p) {
        Cobol.ProgramIdParagraph pp = programIdParagraph;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withProgramId((Cobol.CobolWord) visit(pp.getProgramId(), p));
        pp = pp.withDot((Cobol.CobolWord) visit(pp.getDot(), p));
        pp = pp.withProgramName((Name) visit(pp.getProgramName(), p));
        pp = pp.withProgramAttributes(ListUtils.map(pp.getProgramAttributes(), it -> (Cobol.CobolWord) visit(it, p)));
        pp = pp.withDot2((Cobol.CobolWord) visit(pp.getDot2(), p));
        return pp;
    }

    public Cobol visitProgramLibrarySection(Cobol.ProgramLibrarySection programLibrarySection, P p) {
        Cobol.ProgramLibrarySection pp = programLibrarySection;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withLibraryDescriptionEntries(ListUtils.map(pp.getLibraryDescriptionEntries(), t -> visit(t, p)));
        return pp;
    }

    public Cobol visitProgramUnit(Cobol.ProgramUnit programUnit, P p) {
        Cobol.ProgramUnit pp = programUnit;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.withIdentificationDivision((Cobol.IdentificationDivision) visit(pp.getIdentificationDivision(), p));
        pp = pp.withEnvironmentDivision((Cobol.EnvironmentDivision) visit(pp.getEnvironmentDivision(), p));
        pp = pp.withDataDivision((Cobol.DataDivision) visit(pp.getDataDivision(), p));
        pp = pp.withProcedureDivision((Cobol.ProcedureDivision) visit(pp.getProcedureDivision(), p));
        pp = pp.withProgramUnits(ListUtils.map(pp.getProgramUnits(), t -> (Cobol.ProgramUnit) visit(t, p)));
        if (pp.getPadding().getEndProgram() != null) {
            pp = pp.getPadding().withEndProgram(visitRightPadded(pp.getPadding().getEndProgram(), p));
        }
        return pp;
    }

    public Cobol visitPurge(Cobol.Purge purge, P p) {
        Cobol.Purge pp = purge;
        pp = pp.withPrefix(visitSpace(pp.getPrefix(), p));
        pp = pp.withMarkers(visitMarkers(pp.getMarkers(), p));
        pp = pp.getPadding().withNames(visitContainer(pp.getPadding().getNames(), p));
        return pp;
    }

    public Cobol visitQualifiedDataName(Cobol.QualifiedDataName qualifiedDataName, P p) {
        Cobol.QualifiedDataName q = qualifiedDataName;
        q = q.withPrefix(visitSpace(q.getPrefix(), p));
        q = q.withMarkers(visitMarkers(q.getMarkers(), p));
        q = q.withDataName(visit(q.getDataName(), p));
        return q;
    }

    public Cobol visitQualifiedDataNameFormat1(Cobol.QualifiedDataNameFormat1 qualifiedDataNameFormat1, P p) {
        Cobol.QualifiedDataNameFormat1 q = qualifiedDataNameFormat1;
        q = q.withPrefix(visitSpace(q.getPrefix(), p));
        q = q.withMarkers(visitMarkers(q.getMarkers(), p));
        q = q.withQualifiedInData(ListUtils.map(q.getQualifiedInData(), t -> visit(t, p)));
        q = q.withInFile((Cobol.InFile) visit(q.getInFile(), p));
        return q;
    }

    public Cobol visitQualifiedDataNameFormat2(Cobol.QualifiedDataNameFormat2 qualifiedDataNameFormat2, P p) {
        Cobol.QualifiedDataNameFormat2 q = qualifiedDataNameFormat2;
        q = q.withPrefix(visitSpace(q.getPrefix(), p));
        q = q.withMarkers(visitMarkers(q.getMarkers(), p));
        q = q.withInSection((Cobol.InSection) visit(q.getInSection(), p));
        return q;
    }

    public Cobol visitQualifiedDataNameFormat3(Cobol.QualifiedDataNameFormat3 qualifiedDataNameFormat3, P p) {
        Cobol.QualifiedDataNameFormat3 q = qualifiedDataNameFormat3;
        q = q.withPrefix(visitSpace(q.getPrefix(), p));
        q = q.withMarkers(visitMarkers(q.getMarkers(), p));
        q = q.withInLibrary((Cobol.InLibrary) visit(q.getInLibrary(), p));
        return q;
    }

    public Cobol visitQualifiedDataNameFormat4(Cobol.QualifiedDataNameFormat4 qualifiedDataNameFormat4, P p) {
        Cobol.QualifiedDataNameFormat4 q = qualifiedDataNameFormat4;
        q = q.withPrefix(visitSpace(q.getPrefix(), p));
        q = q.withMarkers(visitMarkers(q.getMarkers(), p));
        q = q.withInFile((Cobol.InFile) visit(q.getInFile(), p));
        return q;
    }

    public Cobol visitQualifiedInData(Cobol.QualifiedInData qualifiedInData, P p) {
        Cobol.QualifiedInData q = qualifiedInData;
        q = q.withPrefix(visitSpace(q.getPrefix(), p));
        q = q.withMarkers(visitMarkers(q.getMarkers(), p));
        q = q.withIn(visit(q.getIn(), p));
        return q;
    }

    public Cobol visitRead(Cobol.Read read, P p) {
        Cobol.Read r = read;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withReadInto((Cobol.ReadInto) visit(r.getReadInto(), p));
        r = r.withReadWith((Cobol.ReadWith) visit(r.getReadWith(), p));
        r = r.withReadKey((Cobol.ReadKey) visit(r.getReadKey(), p));
        r = r.withInvalidKeyPhrase((Cobol.StatementPhrase) visit(r.getInvalidKeyPhrase(), p));
        r = r.withNotInvalidKeyPhrase((Cobol.StatementPhrase) visit(r.getNotInvalidKeyPhrase(), p));
        r = r.withAtEndPhrase((Cobol.StatementPhrase) visit(r.getAtEndPhrase(), p));
        r = r.withNotAtEndPhrase((Cobol.StatementPhrase) visit(r.getNotAtEndPhrase(), p));
        return r;
    }

    public Cobol visitReadInto(Cobol.ReadInto readInto, P p) {
        Cobol.ReadInto r = readInto;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIdentifier((Identifier) visit(r.getIdentifier(), p));
        return r;
    }

    public Cobol visitReadKey(Cobol.ReadKey readKey, P p) {
        Cobol.ReadKey r = readKey;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withQualifiedDataName((Cobol.QualifiedDataName) visit(r.getQualifiedDataName(), p));
        return r;
    }

    public Cobol visitReadWith(Cobol.ReadWith readWith, P p) {
        Cobol.ReadWith r = readWith;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReceivable(Cobol.Receivable receivable, P p) {
        Cobol.Receivable r = receivable;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReceive(Cobol.Receive receive, P p) {
        Cobol.Receive r = receive;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withFromOrInto(visit(r.getFromOrInto(), p));
        r = r.withOnExceptionClause((Cobol.StatementPhrase) visit(r.getOnExceptionClause(), p));
        r = r.withNotOnExceptionClause((Cobol.StatementPhrase) visit(r.getNotOnExceptionClause(), p));
        return r;
    }

    public Cobol visitReceiveFrom(Cobol.ReceiveFrom receiveFrom, P p) {
        Cobol.ReceiveFrom r = receiveFrom;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withDataName((Cobol.CobolWord) visit(r.getDataName(), p));
        return r;
    }

    public Cobol visitReceiveFromStatement(Cobol.ReceiveFromStatement receiveFromStatement, P p) {
        Cobol.ReceiveFromStatement r = receiveFromStatement;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withDataName((Cobol.CobolWord) visit(r.getDataName(), p));
        r = r.withReceiveFrom((Cobol.ReceiveFrom) visit(r.getReceiveFrom(), p));
        r = r.getPadding().withBeforeWithThreadSizeStatus(visitContainer(r.getPadding().getBeforeWithThreadSizeStatus(), p));
        return r;
    }

    public Cobol visitReceiveIntoStatement(Cobol.ReceiveIntoStatement receiveIntoStatement, P p) {
        Cobol.ReceiveIntoStatement r = receiveIntoStatement;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withCdName((Cobol.CobolWord) visit(r.getCdName(), p));
        r = r.withReceiveNoData((Cobol.StatementPhrase) visit(r.getReceiveNoData(), p));
        r = r.withReceiveWithData((Cobol.StatementPhrase) visit(r.getReceiveWithData(), p));
        return r;
    }

    public Cobol visitRecordContainsClause(Cobol.RecordContainsClause recordContainsClause, P p) {
        Cobol.RecordContainsClause r = recordContainsClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withClause(visit(r.getClause(), p));
        return r;
    }

    public Cobol visitRecordContainsClauseFormat1(Cobol.RecordContainsClauseFormat1 recordContainsClauseFormat1, P p) {
        Cobol.RecordContainsClauseFormat1 r = recordContainsClauseFormat1;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        return r;
    }

    public Cobol visitRecordContainsClauseFormat2(Cobol.RecordContainsClauseFormat2 recordContainsClauseFormat2, P p) {
        Cobol.RecordContainsClauseFormat2 r = recordContainsClauseFormat2;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withFromClause(ListUtils.map(r.getFromClause(), t -> visit(t, p)));
        r = r.withQualifiedDataName(ListUtils.map(r.getQualifiedDataName(), t -> visit(t, p)));
        return r;
    }

    public Cobol visitRecordContainsClauseFormat3(Cobol.RecordContainsClauseFormat3 recordContainsClauseFormat3, P p) {
        Cobol.RecordContainsClauseFormat3 r = recordContainsClauseFormat3;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        r = r.withRecordContainsTo((Cobol.RecordContainsTo) visit(r.getRecordContainsTo(), p));
        return r;
    }

    public Cobol visitRecordContainsTo(Cobol.RecordContainsTo recordContainsTo, P p) {
        Cobol.RecordContainsTo r = recordContainsTo;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        return r;
    }

    public Cobol visitRecordDelimiterClause(Cobol.RecordDelimiterClause recordDelimiterClause, P p) {
        Cobol.RecordDelimiterClause r = recordDelimiterClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitRecordKeyClause(Cobol.RecordKeyClause recordKeyClause, P p) {
        Cobol.RecordKeyClause r = recordKeyClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withQualifiedDataName((Cobol.QualifiedDataName) visit(r.getQualifiedDataName(), p));
        r = r.withPasswordClause((Cobol.PasswordClause) visit(r.getPasswordClause(), p));
        return r;
    }

    public Cobol visitRecordingModeClause(Cobol.RecordingModeClause recordingModeClause, P p) {
        Cobol.RecordingModeClause r = recordingModeClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withMode((Cobol.CobolWord) visit(r.getMode(), p));
        return r;
    }

    public Cobol visitReferenceModifier(Cobol.ReferenceModifier referenceModifier, P p) {
        Cobol.ReferenceModifier r = referenceModifier;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withCharacterPosition((Cobol.ArithmeticExpression) visit(r.getCharacterPosition(), p));
        r = r.withLength((Cobol.ArithmeticExpression) visit(r.getLength(), p));
        return r;
    }

    public Cobol visitRelationArithmeticComparison(Cobol.RelationArithmeticComparison relationArithmeticComparison, P p) {
        Cobol.RelationArithmeticComparison r = relationArithmeticComparison;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withArithmeticExpressionA((Cobol.ArithmeticExpression) visit(r.getArithmeticExpressionA(), p));
        r = r.withRelationalOperator((Cobol.RelationalOperator) visit(r.getRelationalOperator(), p));
        r = r.withArithmeticExpressionB((Cobol.ArithmeticExpression) visit(r.getArithmeticExpressionB(), p));
        return r;
    }

    public Cobol visitRelationCombinedComparison(Cobol.RelationCombinedComparison relationCombinedComparison, P p) {
        Cobol.RelationCombinedComparison r = relationCombinedComparison;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withArithmeticExpression((Cobol.ArithmeticExpression) visit(r.getArithmeticExpression(), p));
        r = r.withRelationalOperator((Cobol.RelationalOperator) visit(r.getRelationalOperator(), p));
        r = r.withCombinedCondition((Cobol.Parenthesized) visit(r.getCombinedCondition(), p));
        return r;
    }

    public Cobol visitRelationCombinedCondition(Cobol.RelationCombinedCondition relationCombinedCondition, P p) {
        Cobol.RelationCombinedCondition r = relationCombinedCondition;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withRelationalArithmeticExpressions(ListUtils.map(r.getRelationalArithmeticExpressions(), it -> visit(it, p)));
        return r;
    }

    public Cobol visitRelationSignCondition(Cobol.RelationSignCondition relationSignCondition, P p) {
        Cobol.RelationSignCondition r = relationSignCondition;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withArithmeticExpression((Cobol.ArithmeticExpression) visit(r.getArithmeticExpression(), p));
        return r;
    }

    public Cobol visitRelationalOperator(Cobol.RelationalOperator relationalOperator, P p) {
        Cobol.RelationalOperator r = relationalOperator;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitRelativeKeyClause(Cobol.RelativeKeyClause relativeKeyClause, P p) {
        Cobol.RelativeKeyClause r = relativeKeyClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withQualifiedDataName((Cobol.QualifiedDataName) visit(r.getQualifiedDataName(), p));
        return r;
    }

    public Cobol visitRelease(Cobol.Release release, P p) {
        Cobol.Release r = release;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withRecordName((Cobol.QualifiedDataName) visit(r.getRecordName(), p));
        r = r.withQualifiedDataName((Cobol.QualifiedDataName) visit(r.getQualifiedDataName(), p));
        return r;
    }

    public Cobol visitReportClause(Cobol.ReportClause reportClause, P p) {
        Cobol.ReportClause r = reportClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withReportName(ListUtils.map(r.getReportName(), t -> (Name) visit(t, p)));
        return r;
    }

    public Cobol visitReportDescription(Cobol.ReportDescription reportDescription, P p) {
        Cobol.ReportDescription r = reportDescription;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withReportDescriptionEntry((Cobol.ReportDescriptionEntry) visit(r.getReportDescriptionEntry(), p));
        r = r.withGroupDescriptionEntries(ListUtils.map(r.getGroupDescriptionEntries(), t -> visit(t, p)));
        return r;
    }

    public Cobol visitReportDescriptionEntry(Cobol.ReportDescriptionEntry reportDescriptionEntry, P p) {
        Cobol.ReportDescriptionEntry r = reportDescriptionEntry;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withQualifiedDataName((Cobol.QualifiedDataName) visit(r.getQualifiedDataName(), p));
        r = r.withReportDescriptionGlobalClause((Cobol.ReportDescriptionGlobalClause) visit(r.getReportDescriptionGlobalClause(), p));
        r = r.withReportDescriptionPageLimitClause((Cobol.ReportDescriptionGlobalClause) visit(r.getReportDescriptionPageLimitClause(), p));
        r = r.withReportDescriptionHeadingClause((Cobol.ReportDescriptionGlobalClause) visit(r.getReportDescriptionHeadingClause(), p));
        r = r.withReportDescriptionFirstDetailClause((Cobol.ReportDescriptionGlobalClause) visit(r.getReportDescriptionFirstDetailClause(), p));
        r = r.withReportDescriptionLastDetailClause((Cobol.ReportDescriptionGlobalClause) visit(r.getReportDescriptionLastDetailClause(), p));
        r = r.withReportDescriptionFootingClause((Cobol.ReportDescriptionGlobalClause) visit(r.getReportDescriptionFootingClause(), p));
        r = r.withDot((Cobol.CobolWord) visit(r.getDot(), p));
        return r;
    }

    public Cobol visitReportDescriptionFirstDetailClause(Cobol.ReportDescriptionFirstDetailClause reportDescriptionFirstDetailClause, P p) {
        Cobol.ReportDescriptionFirstDetailClause r = reportDescriptionFirstDetailClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportDescriptionFootingClause(Cobol.ReportDescriptionFootingClause reportDescriptionFootingClause, P p) {
        Cobol.ReportDescriptionFootingClause r = reportDescriptionFootingClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportDescriptionGlobalClause(Cobol.ReportDescriptionGlobalClause reportDescriptionGlobalClause, P p) {
        Cobol.ReportDescriptionGlobalClause r = reportDescriptionGlobalClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportDescriptionHeadingClause(Cobol.ReportDescriptionHeadingClause reportDescriptionHeadingClause, P p) {
        Cobol.ReportDescriptionHeadingClause r = reportDescriptionHeadingClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportDescriptionLastDetailClause(Cobol.ReportDescriptionLastDetailClause reportDescriptionLastDetailClause, P p) {
        Cobol.ReportDescriptionLastDetailClause r = reportDescriptionLastDetailClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportDescriptionPageLimitClause(Cobol.ReportDescriptionPageLimitClause reportDescriptionPageLimitClause, P p) {
        Cobol.ReportDescriptionPageLimitClause r = reportDescriptionPageLimitClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupBlankWhenZeroClause(Cobol.ReportGroupBlankWhenZeroClause reportGroupBlankWhenZeroClause, P p) {
        Cobol.ReportGroupBlankWhenZeroClause r = reportGroupBlankWhenZeroClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupColumnNumberClause(Cobol.ReportGroupColumnNumberClause reportGroupColumnNumberClause, P p) {
        Cobol.ReportGroupColumnNumberClause r = reportGroupColumnNumberClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupDescriptionEntryFormat1(Cobol.ReportGroupDescriptionEntryFormat1 reportGroupDescriptionEntryFormat1, P p) {
        Cobol.ReportGroupDescriptionEntryFormat1 r = reportGroupDescriptionEntryFormat1;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        r = r.withDataName((Cobol.CobolWord) visit(r.getDataName(), p));
        r = r.withGroupLineNumberClause((Cobol.ReportGroupLineNumberClause) visit(r.getGroupLineNumberClause(), p));
        r = r.withGroupNextGroupClause((Cobol.ReportGroupNextGroupClause) visit(r.getGroupNextGroupClause(), p));
        r = r.withGroupTypeClause((Cobol.ReportGroupTypeClause) visit(r.getGroupTypeClause(), p));
        r = r.withGroupUsageClause((Cobol.ReportGroupUsageClause) visit(r.getGroupUsageClause(), p));
        r = r.withDot((Cobol.CobolWord) visit(r.getDot(), p));
        return r;
    }

    public Cobol visitReportGroupDescriptionEntryFormat2(Cobol.ReportGroupDescriptionEntryFormat2 reportGroupDescriptionEntryFormat2, P p) {
        Cobol.ReportGroupDescriptionEntryFormat2 r = reportGroupDescriptionEntryFormat2;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        r = r.withDataName((Cobol.CobolWord) visit(r.getDataName(), p));
        r = r.withReportGroupLineNumberClause((Cobol.ReportGroupLineNumberClause) visit(r.getReportGroupLineNumberClause(), p));
        r = r.withGroupUsageClause((Cobol.ReportGroupUsageClause) visit(r.getGroupUsageClause(), p));
        r = r.withDot((Cobol.CobolWord) visit(r.getDot(), p));
        return r;
    }

    public Cobol visitReportGroupDescriptionEntryFormat3(Cobol.ReportGroupDescriptionEntryFormat3 reportGroupDescriptionEntryFormat3, P p) {
        Cobol.ReportGroupDescriptionEntryFormat3 r = reportGroupDescriptionEntryFormat3;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        r = r.withDataName((Cobol.CobolWord) visit(r.getDataName(), p));
        r = r.getPadding().withClauses(visitContainer(r.getPadding().getClauses(), p));
        r = r.withDot((Cobol.CobolWord) visit(r.getDot(), p));
        return r;
    }

    public Cobol visitReportGroupIndicateClause(Cobol.ReportGroupIndicateClause reportGroupIndicateClause, P p) {
        Cobol.ReportGroupIndicateClause r = reportGroupIndicateClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupJustifiedClause(Cobol.ReportGroupJustifiedClause reportGroupJustifiedClause, P p) {
        Cobol.ReportGroupJustifiedClause r = reportGroupJustifiedClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupLineNumberClause(Cobol.ReportGroupLineNumberClause reportGroupLineNumberClause, P p) {
        Cobol.ReportGroupLineNumberClause r = reportGroupLineNumberClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withClause(visit(r.getClause(), p));
        return r;
    }

    public Cobol visitReportGroupLineNumberNextPage(Cobol.ReportGroupLineNumberNextPage reportGroupLineNumberNextPage, P p) {
        Cobol.ReportGroupLineNumberNextPage r = reportGroupLineNumberNextPage;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        return r;
    }

    public Cobol visitReportGroupLineNumberPlus(Cobol.ReportGroupLineNumberPlus reportGroupLineNumberPlus, P p) {
        Cobol.ReportGroupLineNumberPlus r = reportGroupLineNumberPlus;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        return r;
    }

    public Cobol visitReportGroupNextGroupClause(Cobol.ReportGroupNextGroupClause reportGroupNextGroupClause, P p) {
        Cobol.ReportGroupNextGroupClause r = reportGroupNextGroupClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withClause(visit(r.getClause(), p));
        return r;
    }

    public Cobol visitReportGroupNextGroupNextPage(Cobol.ReportGroupNextGroupNextPage reportGroupNextGroupNextPage, P p) {
        Cobol.ReportGroupNextGroupNextPage r = reportGroupNextGroupNextPage;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupNextGroupPlus(Cobol.ReportGroupNextGroupPlus reportGroupNextGroupPlus, P p) {
        Cobol.ReportGroupNextGroupPlus r = reportGroupNextGroupPlus;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        return r;
    }

    public Cobol visitReportGroupPictureClause(Cobol.ReportGroupPictureClause reportGroupPictureClause, P p) {
        Cobol.ReportGroupPictureClause r = reportGroupPictureClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withPictureString((Cobol.PictureString) visit(r.getPictureString(), p));
        return r;
    }

    public Cobol visitReportGroupResetClause(Cobol.ReportGroupResetClause reportGroupResetClause, P p) {
        Cobol.ReportGroupResetClause r = reportGroupResetClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupSignClause(Cobol.ReportGroupSignClause reportGroupSignClause, P p) {
        Cobol.ReportGroupSignClause r = reportGroupSignClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupSourceClause(Cobol.ReportGroupSourceClause reportGroupSourceClause, P p) {
        Cobol.ReportGroupSourceClause r = reportGroupSourceClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupSumClause(Cobol.ReportGroupSumClause reportGroupSumClause, P p) {
        Cobol.ReportGroupSumClause r = reportGroupSumClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIdentifiers(ListUtils.map(r.getIdentifiers(), t -> visit(t, p)));
        r = r.withDataNames(ListUtils.map(r.getDataNames(), t -> visit(t, p)));
        return r;
    }

    public Cobol visitReportGroupTypeClause(Cobol.ReportGroupTypeClause reportGroupTypeClause, P p) {
        Cobol.ReportGroupTypeClause r = reportGroupTypeClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withType(visit(r.getType(), p));
        return r;
    }

    public Cobol visitReportGroupTypeControlFooting(Cobol.ReportGroupTypeControlFooting reportGroupTypeControlFooting, P p) {
        Cobol.ReportGroupTypeControlFooting r = reportGroupTypeControlFooting;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupTypeControlHeading(Cobol.ReportGroupTypeControlHeading reportGroupTypeControlHeading, P p) {
        Cobol.ReportGroupTypeControlHeading r = reportGroupTypeControlHeading;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupTypeDetail(Cobol.ReportGroupTypeDetail reportGroupTypeDetail, P p) {
        Cobol.ReportGroupTypeDetail r = reportGroupTypeDetail;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupTypePageFooting(Cobol.ReportGroupTypePageFooting reportGroupTypePageFooting, P p) {
        Cobol.ReportGroupTypePageFooting r = reportGroupTypePageFooting;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupTypeReportFooting(Cobol.ReportGroupTypeReportFooting reportGroupTypeReportFooting, P p) {
        Cobol.ReportGroupTypeReportFooting r = reportGroupTypeReportFooting;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupTypePageHeading(Cobol.ReportGroupTypePageHeading reportGroupTypePageHeading, P p) {
        Cobol.ReportGroupTypePageHeading r = reportGroupTypePageHeading;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withWords(ListUtils.map(r.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        return r;
    }

    public Cobol visitReportGroupTypeReportHeading(Cobol.ReportGroupTypeReportHeading reportGroupTypeReportHeading, P p) {
        Cobol.ReportGroupTypeReportHeading r = reportGroupTypeReportHeading;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupUsageClause(Cobol.ReportGroupUsageClause reportGroupUsageClause, P p) {
        Cobol.ReportGroupUsageClause r = reportGroupUsageClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportGroupValueClause(Cobol.ReportGroupValueClause reportGroupValueClause, P p) {
        Cobol.ReportGroupValueClause r = reportGroupValueClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReportName(Cobol.ReportName reportName, P p) {
        Cobol.ReportName r = reportName;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withQualifiedDataName((Cobol.QualifiedDataName) visit(r.getQualifiedDataName(), p));
        return r;
    }

    public Cobol visitReportSection(Cobol.ReportSection reportSection, P p) {
        Cobol.ReportSection r = reportSection;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withDescriptions(ListUtils.map(r.getDescriptions(), t -> visit(t, p)));
        return r;
    }

    public Cobol visitRerunClause(Cobol.RerunClause rerunClause, P p) {
        Cobol.RerunClause r = rerunClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withName((Cobol.CobolWord) visit(r.getName(), p));
        r = r.withAction(visit(r.getAction(), p));
        return r;
    }

    public Cobol visitRerunEveryClock(Cobol.RerunEveryClock rerunEveryClock, P p) {
        Cobol.RerunEveryClock r = rerunEveryClock;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        return r;
    }

    public Cobol visitRerunEveryOf(Cobol.RerunEveryOf rerunEveryOf, P p) {
        Cobol.RerunEveryOf r = rerunEveryOf;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withFileName((Cobol.CobolWord) visit(r.getFileName(), p));
        return r;
    }

    public Cobol visitRerunEveryRecords(Cobol.RerunEveryRecords rerunEveryRecords, P p) {
        Cobol.RerunEveryRecords r = rerunEveryRecords;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIntegerLiteral((Cobol.CobolWord) visit(r.getIntegerLiteral(), p));
        return r;
    }

    public Cobol visitReserveClause(Cobol.ReserveClause reserveClause, P p) {
        Cobol.ReserveClause r = reserveClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withWords(ListUtils.map(r.getWords(), t -> visit(t, p)));
        return r;
    }

    public Cobol visitReserveNetworkClause(Cobol.ReserveNetworkClause reserveNetworkClause, P p) {
        Cobol.ReserveNetworkClause r = reserveNetworkClause;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitReturn(Cobol.Return r, P p) {
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withFileName((Name) visit(r.getFileName(), p));
        r = r.withInto((Cobol.ReturnInto) visit(r.getInto(), p));
        r = r.withAtEndPhrase((Cobol.StatementPhrase) visit(r.getAtEndPhrase(), p));
        r = r.withNotAtEndPhrase((Cobol.StatementPhrase) visit(r.getNotAtEndPhrase(), p));
        return r;
    }

    public Cobol visitReturnInto(Cobol.ReturnInto returnInto, P p) {
        Cobol.ReturnInto r = returnInto;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withQualifiedDataName((Cobol.QualifiedDataName) visit(r.getQualifiedDataName(), p));
        return r;
    }

    public Cobol visitRewrite(Cobol.Rewrite rewrite, P p) {
        Cobol.Rewrite r = rewrite;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withRecordName((Cobol.QualifiedDataName) visit(r.getRecordName(), p));
        r = r.withInvalidKeyPhrase((Cobol.StatementPhrase) visit(r.getInvalidKeyPhrase(), p));
        r = r.withNotInvalidKeyPhrase((Cobol.StatementPhrase) visit(r.getNotInvalidKeyPhrase(), p));
        r = r.withEndRewrite((Cobol.CobolWord) visit(r.getEndRewrite(), p));
        return r;
    }

    public Cobol visitRewriteFrom(Cobol.RewriteFrom rewriteFrom, P p) {
        Cobol.RewriteFrom r = rewriteFrom;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        return r;
    }

    public Cobol visitRoundable(Cobol.Roundable roundable, P p) {
        Cobol.Roundable r = roundable;
        r = r.withPrefix(visitSpace(r.getPrefix(), p));
        r = r.withMarkers(visitMarkers(r.getMarkers(), p));
        r = r.withIdentifier((Identifier) visit(r.getIdentifier(), p));
        r = r.withRounded((Cobol.CobolWord) visit(r.getRounded(), p));
        return r;
    }

    public Cobol visitSameClause(Cobol.SameClause sameClause, P p) {
        Cobol.SameClause s = sameClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withFileNames(ListUtils.map(s.getFileNames(), t -> (Cobol.CobolWord) visit(t, p)));
        return s;
    }

    public Cobol visitScreenDescriptionAutoClause(Cobol.ScreenDescriptionAutoClause screenDescriptionAutoClause, P p) {
        Cobol.ScreenDescriptionAutoClause s = screenDescriptionAutoClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionBackgroundColorClause(Cobol.ScreenDescriptionBackgroundColorClause screenDescriptionBackgroundColorClause, P p) {
        Cobol.ScreenDescriptionBackgroundColorClause s = screenDescriptionBackgroundColorClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionBellClause(Cobol.ScreenDescriptionBellClause screenDescriptionBellClause, P p) {
        Cobol.ScreenDescriptionBellClause s = screenDescriptionBellClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionBlankClause(Cobol.ScreenDescriptionBlankClause screenDescriptionBlankClause, P p) {
        Cobol.ScreenDescriptionBlankClause s = screenDescriptionBlankClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionBlankWhenZeroClause(Cobol.ScreenDescriptionBlankWhenZeroClause screenDescriptionBlankWhenZeroClause, P p) {
        Cobol.ScreenDescriptionBlankWhenZeroClause s = screenDescriptionBlankWhenZeroClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionBlinkClause(Cobol.ScreenDescriptionBlinkClause screenDescriptionBlinkClause, P p) {
        Cobol.ScreenDescriptionBlinkClause s = screenDescriptionBlinkClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionColumnClause(Cobol.ScreenDescriptionColumnClause screenDescriptionColumnClause, P p) {
        Cobol.ScreenDescriptionColumnClause s = screenDescriptionColumnClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionControlClause(Cobol.ScreenDescriptionControlClause screenDescriptionControlClause, P p) {
        Cobol.ScreenDescriptionControlClause s = screenDescriptionControlClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withValue((Name) visit(s.getValue(), p));
        return s;
    }

    public Cobol visitScreenDescriptionEntry(Cobol.ScreenDescriptionEntry screenDescriptionEntry, P p) {
        Cobol.ScreenDescriptionEntry s = screenDescriptionEntry;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withWords((Cobol.CobolWord) visit(s.getWords(), p));
        s = s.withName((Cobol.CobolWord) visit(s.getName(), p));
        s = s.withClauses(ListUtils.map(s.getClauses(), it -> visit(it, p)));
        s = s.withDot((Cobol.CobolWord) visit(s.getDot(), p));
        return s;
    }

    public Cobol visitScreenDescriptionEraseClause(Cobol.ScreenDescriptionEraseClause screenDescriptionEraseClause, P p) {
        Cobol.ScreenDescriptionEraseClause s = screenDescriptionEraseClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionForegroundColorClause(Cobol.ScreenDescriptionForegroundColorClause screenDescriptionForegroundColorClause, P p) {
        Cobol.ScreenDescriptionForegroundColorClause s = screenDescriptionForegroundColorClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionFromClause(Cobol.ScreenDescriptionFromClause screenDescriptionFromClause, P p) {
        Cobol.ScreenDescriptionFromClause s = screenDescriptionFromClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withScreenDescriptionToClause((Cobol.ScreenDescriptionToClause) visit(s.getScreenDescriptionToClause(), p));
        return s;
    }

    public Cobol visitScreenDescriptionFullClause(Cobol.ScreenDescriptionFullClause screenDescriptionFullClause, P p) {
        Cobol.ScreenDescriptionFullClause s = screenDescriptionFullClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionGridClause(Cobol.ScreenDescriptionGridClause screenDescriptionGridClause, P p) {
        Cobol.ScreenDescriptionGridClause s = screenDescriptionGridClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionJustifiedClause(Cobol.ScreenDescriptionJustifiedClause screenDescriptionJustifiedClause, P p) {
        Cobol.ScreenDescriptionJustifiedClause s = screenDescriptionJustifiedClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionLightClause(Cobol.ScreenDescriptionLightClause screenDescriptionLightClause, P p) {
        Cobol.ScreenDescriptionLightClause s = screenDescriptionLightClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionLineClause(Cobol.ScreenDescriptionLineClause screenDescriptionLineClause, P p) {
        Cobol.ScreenDescriptionLineClause s = screenDescriptionLineClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionPictureClause(Cobol.ScreenDescriptionPictureClause screenDescriptionPictureClause, P p) {
        Cobol.ScreenDescriptionPictureClause s = screenDescriptionPictureClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionPromptClause(Cobol.ScreenDescriptionPromptClause screenDescriptionPromptClause, P p) {
        Cobol.ScreenDescriptionPromptClause s = screenDescriptionPromptClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withScreenDescriptionPromptOccursClause((Cobol.ScreenDescriptionPromptOccursClause) visit(s.getScreenDescriptionPromptOccursClause(), p));
        return s;
    }

    public Cobol visitScreenDescriptionPromptOccursClause(Cobol.ScreenDescriptionPromptOccursClause screenDescriptionPromptOccursClause, P p) {
        Cobol.ScreenDescriptionPromptOccursClause s = screenDescriptionPromptOccursClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withInteger((Cobol.CobolWord) visit(s.getInteger(), p));
        return s;
    }

    public Cobol visitScreenDescriptionRequiredClause(Cobol.ScreenDescriptionRequiredClause screenDescriptionRequiredClause, P p) {
        Cobol.ScreenDescriptionRequiredClause s = screenDescriptionRequiredClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionReverseVideoClause(Cobol.ScreenDescriptionReverseVideoClause screenDescriptionReverseVideoClause, P p) {
        Cobol.ScreenDescriptionReverseVideoClause s = screenDescriptionReverseVideoClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionSecureClause(Cobol.ScreenDescriptionSecureClause screenDescriptionSecureClause, P p) {
        Cobol.ScreenDescriptionSecureClause s = screenDescriptionSecureClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionSignClause(Cobol.ScreenDescriptionSignClause screenDescriptionSignClause, P p) {
        Cobol.ScreenDescriptionSignClause s = screenDescriptionSignClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionSizeClause(Cobol.ScreenDescriptionSizeClause screenDescriptionSizeClause, P p) {
        Cobol.ScreenDescriptionSizeClause s = screenDescriptionSizeClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withValue((Name) visit(s.getValue(), p));
        return s;
    }

    public Cobol visitScreenDescriptionToClause(Cobol.ScreenDescriptionToClause screenDescriptionToClause, P p) {
        Cobol.ScreenDescriptionToClause s = screenDescriptionToClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withIdentifier((Identifier) visit(s.getIdentifier(), p));
        return s;
    }

    public Cobol visitScreenDescriptionUnderlineClause(Cobol.ScreenDescriptionUnderlineClause screenDescriptionUnderlineClause, P p) {
        Cobol.ScreenDescriptionUnderlineClause s = screenDescriptionUnderlineClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionUsageClause(Cobol.ScreenDescriptionUsageClause screenDescriptionUsageClause, P p) {
        Cobol.ScreenDescriptionUsageClause s = screenDescriptionUsageClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionUsingClause(Cobol.ScreenDescriptionUsingClause screenDescriptionUsingClause, P p) {
        Cobol.ScreenDescriptionUsingClause s = screenDescriptionUsingClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withIdentifier((Identifier) visit(s.getIdentifier(), p));
        return s;
    }

    public Cobol visitScreenDescriptionValueClause(Cobol.ScreenDescriptionValueClause screenDescriptionValueClause, P p) {
        Cobol.ScreenDescriptionValueClause s = screenDescriptionValueClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenDescriptionZeroFillClause(Cobol.ScreenDescriptionZeroFillClause screenDescriptionZeroFillClause, P p) {
        Cobol.ScreenDescriptionZeroFillClause s = screenDescriptionZeroFillClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitScreenSection(Cobol.ScreenSection screenSection, P p) {
        Cobol.ScreenSection s = screenSection;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withWords(ListUtils.map(s.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        s = s.withDot((Cobol.CobolWord) visit(s.getDot(), p));
        s = s.withDescriptions(ListUtils.map(s.getDescriptions(), it -> (Cobol.ScreenDescriptionEntry) visit(it, p)));
        return s;
    }

    public Cobol visitSearch(Cobol.Search search, P p) {
        Cobol.Search s = search;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withQualifiedDataName((Cobol.QualifiedDataName) visit(s.getQualifiedDataName(), p));
        s = s.withSearchVarying((Cobol.SearchVarying) visit(s.getSearchVarying(), p));
        s = s.withAtEndPhrase((Cobol.StatementPhrase) visit(s.getAtEndPhrase(), p));
        s = s.withSearchWhen(ListUtils.map(s.getSearchWhen(), t -> (Cobol.SearchWhen) visit(t, p)));
        return s;
    }

    public Cobol visitSearchVarying(Cobol.SearchVarying searchVarying, P p) {
        Cobol.SearchVarying s = searchVarying;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withQualifiedDataName((Cobol.QualifiedDataName) visit(s.getQualifiedDataName(), p));
        return s;
    }

    public Cobol visitSearchWhen(Cobol.SearchWhen searchWhen, P p) {
        Cobol.SearchWhen s = searchWhen;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withCondition((Cobol.Condition) visit(s.getCondition(), p));
        s = s.withStatements(ListUtils.map(s.getStatements(), t -> (Statement) visit(t, p)));
        return s;
    }

    public Cobol visitSelectClause(Cobol.SelectClause selectClause, P p) {
        Cobol.SelectClause s = selectClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withFileName((Cobol.CobolWord) visit(s.getFileName(), p));
        return s;
    }

    public Cobol visitSend(Cobol.Send send, P p) {
        Cobol.Send s = send;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withStatement(visit(s.getStatement(), p));
        s = s.withOnExceptionClause((Cobol.StatementPhrase) visit(s.getOnExceptionClause(), p));
        s = s.withNotOnExceptionClause((Cobol.StatementPhrase) visit(s.getNotOnExceptionClause(), p));
        return s;
    }

    public Cobol visitSendAdvancingLines(Cobol.SendAdvancingLines sendAdvancingLines, P p) {
        Cobol.SendAdvancingLines s = sendAdvancingLines;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        return s;
    }

    public Cobol visitSendPhrase(Cobol.SendPhrase sendPhrase, P p) {
        Cobol.SendPhrase s = sendPhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withTarget(visit(s.getTarget(), p));
        return s;
    }

    public Cobol visitSendStatementSync(Cobol.SendStatementSync sendStatementSync, P p) {
        Cobol.SendStatementSync s = sendStatementSync;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withSendFromPhrase((Cobol.SendPhrase) visit(s.getSendFromPhrase(), p));
        s = s.withSendWithPhrase((Cobol.SendPhrase) visit(s.getSendWithPhrase(), p));
        s = s.withSendReplacingPhrase((Cobol.SendPhrase) visit(s.getSendReplacingPhrase(), p));
        s = s.withSendAdvancingPhrase((Cobol.SendPhrase) visit(s.getSendAdvancingPhrase(), p));
        return s;
    }

    public Cobol visitSentence(Cobol.Sentence sentence, P p) {
        Cobol.Sentence s = sentence;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withStatements(ListUtils.map(s.getStatements(), t -> (Statement) visit(t, p)));
        return s;
    }

    public Cobol visitSet(Cobol.Set set, P p) {
        Cobol.Set s = set;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withTo(ListUtils.map(s.getTo(), t -> (Cobol.SetTo) visit(t, p)));
        s = s.withUpDown((Cobol.SetUpDown) visit(s.getUpDown(), p));
        return s;
    }

    public Cobol visitSetTo(Cobol.SetTo setTo, P p) {
        Cobol.SetTo s = setTo;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withTo(ListUtils.map(s.getTo(), t -> (Identifier) visit(t, p)));
        s = s.getPadding().withValues(visitContainer(s.getPadding().getValues(), p));
        return s;
    }

    public Cobol visitSetUpDown(Cobol.SetUpDown setUpDown, P p) {
        Cobol.SetUpDown s = setUpDown;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withTo(ListUtils.map(s.getTo(), t -> (Identifier) visit(t, p)));
        s = s.withOperation(ListUtils.map(s.getOperation(), it -> (Cobol.CobolWord) visit(it, p)));
        s = s.withValue((Cobol.CobolWord) visit(s.getValue(), p));
        return s;
    }

    public Cobol visitSort(Cobol.Sort sort, P p) {
        Cobol.Sort s = sort;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withFileName((Cobol.CobolWord) visit(s.getFileName(), p));
        s = s.withSortOnKeyClause(ListUtils.map(s.getSortOnKeyClause(), t -> (Cobol.Sortable) visit(t, p)));
        s = s.withSortDuplicatesPhrase((Cobol.CobolWord) visit(s.getSortDuplicatesPhrase(), p));
        s = s.withSortCollatingSequencePhrase((Cobol.SortCollatingSequencePhrase) visit(s.getSortCollatingSequencePhrase(), p));
        s = s.withSortInputProcedurePhrase((Cobol.SortProcedurePhrase) visit(s.getSortInputProcedurePhrase(), p));
        s = s.withSortUsing(ListUtils.map(s.getSortUsing(), t -> (Cobol.Sortable) visit(t, p)));
        s = s.withSortOutputProcedurePhrase((Cobol.SortProcedurePhrase) visit(s.getSortOutputProcedurePhrase(), p));
        s = s.withSortGiving(ListUtils.map(s.getSortGiving(), t -> (Cobol.Sortable) visit(t, p)));
        return s;
    }

    public Cobol visitSortCollatingSequencePhrase(Cobol.SortCollatingSequencePhrase sortCollatingSequencePhrase, P p) {
        Cobol.SortCollatingSequencePhrase s = sortCollatingSequencePhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withAlphabetNames(ListUtils.map(s.getAlphabetNames(), t -> (Cobol.CobolWord) visit(t, p)));
        s = s.withSortCollatingAlphanumeric((Cobol.Sortable) visit(s.getSortCollatingAlphanumeric(), p));
        s = s.withSortCollatingNational((Cobol.Sortable) visit(s.getSortCollatingNational(), p));
        return s;
    }

    public Cobol visitSortGiving(Cobol.SortGiving sortGiving, P p) {
        Cobol.SortGiving s = sortGiving;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withFileName((Cobol.CobolWord) visit(s.getFileName(), p));
        return s;
    }

    public Cobol visitSortProcedurePhrase(Cobol.SortProcedurePhrase sortProcedurePhrase, P p) {
        Cobol.SortProcedurePhrase s = sortProcedurePhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withProcedureName((Cobol.CobolWord) visit(s.getProcedureName(), p));
        s = s.withSortInputThrough((Cobol.Sortable) visit(s.getSortInputThrough(), p));
        return s;
    }

    public Cobol visitSortable(Cobol.Sortable sortable, P p) {
        Cobol.Sortable s = sortable;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withWords(ListUtils.map(s.getWords(), t -> (Cobol.CobolWord) visit(t, p)));
        s = s.withNames(ListUtils.map(s.getNames(), t -> visit(t, p)));
        return s;
    }

    public Cobol visitSourceComputer(Cobol.SourceComputer sourceComputer, P p) {
        Cobol.SourceComputer s = sourceComputer;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withWords(ListUtils.map(s.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        s = s.withComputer((Cobol.SourceComputerDefinition) visit(s.getComputer(), p));
        s = s.withDot((Cobol.CobolWord) visit(s.getDot(), p));
        return s;
    }

    public Cobol visitSourceComputerDefinition(Cobol.SourceComputerDefinition sourceComputerDefinition, P p) {
        Cobol.SourceComputerDefinition s = sourceComputerDefinition;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withComputerName((Cobol.CobolWord) visit(s.getComputerName(), p));
        s = s.withDebuggingMode(ListUtils.map(s.getDebuggingMode(), it -> (Cobol.CobolWord) visit(it, p)));
        return s;
    }

    public Space visitSpace(Space space, P p) {
        return space;
    }

    public Cobol visitSpecialNames(Cobol.SpecialNames specialNames, P p) {
        Cobol.SpecialNames s = specialNames;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withWords((Cobol.CobolWord) visit(s.getWords(), p));
        s = s.withDot((Cobol.CobolWord) visit(s.getDot(), p));
        s = s.withClauses(ListUtils.map(s.getClauses(), it -> visit(it, p)));
        s = s.withDot2((Cobol.CobolWord) visit(s.getDot2(), p));
        return s;
    }

    public Cobol visitStart(Cobol.Start start, P p) {
        Cobol.Start s = start;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withFileName((Cobol.CobolWord) visit(s.getFileName(), p));
        s = s.withStartKey((Cobol.StartKey) visit(s.getStartKey(), p));
        s = s.withInvalidKeyPhrase((Cobol.StatementPhrase) visit(s.getInvalidKeyPhrase(), p));
        s = s.withNotInvalidKeyPhrase((Cobol.StatementPhrase) visit(s.getNotInvalidKeyPhrase(), p));
        s = s.withEndStart((Cobol.CobolWord) visit(s.getEndStart(), p));
        return s;
    }

    public Cobol visitStartKey(Cobol.StartKey startKey, P p) {
        Cobol.StartKey s = startKey;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withQualifiedDataName((Cobol.QualifiedDataName) visit(s.getQualifiedDataName(), p));
        return s;
    }

    public Cobol visitStatementPhrase(Cobol.StatementPhrase statementPhrase, P p) {
        Cobol.StatementPhrase s = statementPhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withStatement(ListUtils.map(s.getStatement(), t -> (Statement) visit(t, p)));
        return s;
    }

    public Cobol visitStatusKeyClause(Cobol.StatusKeyClause statusKeyClause, P p) {
        Cobol.StatusKeyClause s = statusKeyClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withWords(ListUtils.map(s.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        s = s.withName((Cobol.CobolWord) visit(s.getName(), p));
        return s;
    }

    public Cobol visitStop(Cobol.Stop stop, P p) {
        Cobol.Stop s = stop;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withStatement(visit(s.getStatement(), p));
        return s;
    }

    public Cobol visitStopStatementGiving(Cobol.StopStatementGiving stopStatementGiving, P p) {
        Cobol.StopStatementGiving s = stopStatementGiving;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withWords(ListUtils.map(s.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        return s;
    }

    public Cobol visitStringDelimitedByPhrase(Cobol.StringDelimitedByPhrase stringDelimitedByPhrase, P p) {
        Cobol.StringDelimitedByPhrase s = stringDelimitedByPhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withIdentifier((Cobol.CobolWord) visit(s.getIdentifier(), p));
        return s;
    }

    public Cobol visitStringForPhrase(Cobol.StringForPhrase stringForPhrase, P p) {
        Cobol.StringForPhrase s = stringForPhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withIdentifier((Cobol.CobolWord) visit(s.getIdentifier(), p));
        return s;
    }

    public Cobol visitStringIntoPhrase(Cobol.StringIntoPhrase stringIntoPhrase, P p) {
        Cobol.StringIntoPhrase s = stringIntoPhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withIdentifier((Identifier) visit(s.getIdentifier(), p));
        return s;
    }

    public Cobol visitStringSendingPhrase(Cobol.StringSendingPhrase stringSendingPhrase, P p) {
        Cobol.StringSendingPhrase s = stringSendingPhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withSendings(ListUtils.map(s.getSendings(), t -> visit(t, p)));
        s = s.withPhrase(visit(s.getPhrase(), p));
        return s;
    }

    public Cobol visitStringStatement(Cobol.StringStatement stringStatement, P p) {
        Cobol.StringStatement s = stringStatement;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withString((Cobol.CobolWord) visit(s.getString(), p));
        s = s.withStringSendingPhrases(ListUtils.map(s.getStringSendingPhrases(), t -> visit(t, p)));
        s = s.withStringIntoPhrase((Cobol.StringIntoPhrase) visit(s.getStringIntoPhrase(), p));
        s = s.withStringWithPointerPhrase((Cobol.StringWithPointerPhrase) visit(s.getStringWithPointerPhrase(), p));
        s = s.withOnOverflowPhrase((Cobol.StatementPhrase) visit(s.getOnOverflowPhrase(), p));
        s = s.withNotOnOverflowPhrase((Cobol.StatementPhrase) visit(s.getNotOnOverflowPhrase(), p));
        s = s.withEndString((Cobol.CobolWord) visit(s.getEndString(), p));
        return s;
    }

    public Cobol visitStringWithPointerPhrase(Cobol.StringWithPointerPhrase stringWithPointerPhrase, P p) {
        Cobol.StringWithPointerPhrase s = stringWithPointerPhrase;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withQualifiedDataName((Cobol.QualifiedDataName) visit(s.getQualifiedDataName(), p));
        return s;
    }

    public Cobol visitSubscript(Cobol.Subscript subscript, P p) {
        Cobol.Subscript s = subscript;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withFirst(visit(s.getFirst(), p));
        s = s.withIntegerLiteral((Cobol.CobolWord) visit(s.getIntegerLiteral(), p));
        return s;
    }

    public Cobol visitSubtract(Cobol.Subtract subtract, P p) {
        Cobol.Subtract s = subtract;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withSubstract((Cobol.CobolWord) visit(s.getSubstract(), p));
        s = s.withOperation(visit(s.getOperation(), p));
        s = s.withOnSizeErrorPhrase((Cobol.StatementPhrase) visit(s.getOnSizeErrorPhrase(), p));
        s = s.withNotOnSizeErrorPhrase((Cobol.StatementPhrase) visit(s.getNotOnSizeErrorPhrase(), p));
        s = s.withEndSubtract((Cobol.CobolWord) visit(s.getEndSubtract(), p));
        return s;
    }

    public Cobol visitSubtractCorrespondingStatement(Cobol.SubtractCorrespondingStatement subtractCorrespondingStatement, P p) {
        Cobol.SubtractCorrespondingStatement s = subtractCorrespondingStatement;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withCorresponding((Cobol.CobolWord) visit(s.getCorresponding(), p));
        s = s.withQualifiedDataName((Cobol.QualifiedDataName) visit(s.getQualifiedDataName(), p));
        s = s.withGiving((Cobol.CobolWord) visit(s.getGiving(), p));
        s = s.withSubtractMinuendCorresponding((Cobol.SubtractMinuendCorresponding) visit(s.getSubtractMinuendCorresponding(), p));
        return s;
    }

    public Cobol visitSubtractFromGivingStatement(Cobol.SubtractFromGivingStatement subtractFromGivingStatement, P p) {
        Cobol.SubtractFromGivingStatement s = subtractFromGivingStatement;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withSubtractSubtrahend(ListUtils.map(s.getSubtractSubtrahend(), t -> (Name) visit(t, p)));
        s = s.withFrom((Cobol.CobolWord) visit(s.getFrom(), p));
        s = s.withSubtractMinuendGiving((Name) visit(s.getSubtractMinuendGiving(), p));
        s = s.withGiving((Cobol.CobolWord) visit(s.getGiving(), p));
        s = s.withSubtractGiving(ListUtils.map(s.getSubtractGiving(), t -> (Cobol.Roundable) visit(t, p)));
        return s;
    }

    public Cobol visitSubtractFromStatement(Cobol.SubtractFromStatement subtractFromStatement, P p) {
        Cobol.SubtractFromStatement s = subtractFromStatement;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withSubtractSubtrahend(ListUtils.map(s.getSubtractSubtrahend(), t -> (Name) visit(t, p)));
        s = s.withFrom((Cobol.CobolWord) visit(s.getFrom(), p));
        s = s.withSubtractMinuend(ListUtils.map(s.getSubtractMinuend(), t -> (Cobol.Roundable) visit(t, p)));
        return s;
    }

    public Cobol visitSubtractMinuendCorresponding(Cobol.SubtractMinuendCorresponding subtractMinuendCorresponding, P p) {
        Cobol.SubtractMinuendCorresponding s = subtractMinuendCorresponding;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withQualifiedDataName((Cobol.QualifiedDataName) visit(s.getQualifiedDataName(), p));
        s = s.withRounded((Cobol.CobolWord) visit(s.getRounded(), p));
        return s;
    }

    public Cobol visitSymbolicCharacter(Cobol.SymbolicCharacter symbolicCharacter, P p) {
        Cobol.SymbolicCharacter s = symbolicCharacter;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withSymbols(ListUtils.map(s.getSymbols(), t -> (Cobol.CobolWord) visit(t, p)));
        s = s.withLiterals(ListUtils.map(s.getLiterals(), t -> (Cobol.CobolWord) visit(t, p)));
        return s;
    }

    public Cobol visitSymbolicCharactersClause(Cobol.SymbolicCharactersClause symbolicCharactersClause, P p) {
        Cobol.SymbolicCharactersClause s = symbolicCharactersClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withSymbols(ListUtils.map(s.getSymbols(), t -> (Cobol.SymbolicCharacter) visit(t, p)));
        s = s.withInAlphabet((Cobol.CobolWord) visit(s.getInAlphabet(), p));
        s = s.withAlphabetName((Identifier) visit(s.getAlphabetName(), p));
        return s;
    }

    public Cobol visitSymbolicDestinationClause(Cobol.SymbolicDestinationClause symbolicDestinationClause, P p) {
        Cobol.SymbolicDestinationClause s = symbolicDestinationClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withDataDescName((Cobol.CobolWord) visit(s.getDataDescName(), p));
        return s;
    }

    public Cobol visitSymbolicQueueClause(Cobol.SymbolicQueueClause symbolicQueueClause, P p) {
        Cobol.SymbolicQueueClause s = symbolicQueueClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withDataDescName((Cobol.CobolWord) visit(s.getDataDescName(), p));
        return s;
    }

    public Cobol visitSymbolicSourceClause(Cobol.SymbolicSourceClause symbolicSourceClause, P p) {
        Cobol.SymbolicSourceClause s = symbolicSourceClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withDataDescName((Cobol.CobolWord) visit(s.getDataDescName(), p));
        return s;
    }

    public Cobol visitSymbolicSubQueueClause(Cobol.SymbolicSubQueueClause symbolicSubQueueClause, P p) {
        Cobol.SymbolicSubQueueClause s = symbolicSubQueueClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withDataDescName((Cobol.CobolWord) visit(s.getDataDescName(), p));
        return s;
    }

    public Cobol visitSymbolicTerminalClause(Cobol.SymbolicTerminalClause symbolicTerminalClause, P p) {
        Cobol.SymbolicTerminalClause s = symbolicTerminalClause;
        s = s.withPrefix(visitSpace(s.getPrefix(), p));
        s = s.withMarkers(visitMarkers(s.getMarkers(), p));
        s = s.withDataDescName((Cobol.CobolWord) visit(s.getDataDescName(), p));
        return s;
    }

    public Cobol visitTableCall(Cobol.TableCall tableCall, P p) {
        Cobol.TableCall t = tableCall;
        t = t.withPrefix(visitSpace(t.getPrefix(), p));
        t = t.withMarkers(visitMarkers(t.getMarkers(), p));
        t = t.withQualifiedDataName((Cobol.QualifiedDataName) visit(t.getQualifiedDataName(), p));
        t = t.withSubscripts(ListUtils.map(t.getSubscripts(), it -> (Cobol.Parenthesized) visit(it, p)));
        t = t.withReferenceModifier((Cobol.ReferenceModifier) visit(t.getReferenceModifier(), p));
        return t;
    }

    public Cobol visitTerminate(Cobol.Terminate terminate, P p) {
        Cobol.Terminate t = terminate;
        t = t.withPrefix(visitSpace(t.getPrefix(), p));
        t = t.withMarkers(visitMarkers(t.getMarkers(), p));
        t = t.withTerminate((Cobol.CobolWord) visit(t.getTerminate(), p));
        t = t.withReportName((Cobol.QualifiedDataName) visit(t.getReportName(), p));
        return t;
    }

    public Cobol visitTextLengthClause(Cobol.TextLengthClause textLengthClause, P p) {
        Cobol.TextLengthClause t = textLengthClause;
        t = t.withPrefix(visitSpace(t.getPrefix(), p));
        t = t.withMarkers(visitMarkers(t.getMarkers(), p));
        t = t.withDataDescName((Cobol.CobolWord) visit(t.getDataDescName(), p));
        return t;
    }

    public Cobol visitUnString(Cobol.UnString unString, P p) {
        Cobol.UnString u = unString;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withUnstringSendingPhrase((Cobol.UnstringSendingPhrase) visit(u.getUnstringSendingPhrase(), p));
        u = u.withUnstringIntoPhrase((Cobol.UnstringIntoPhrase) visit(u.getUnstringIntoPhrase(), p));
        u = u.withUnstringWithPointerPhrase((Cobol.UnstringWithPointerPhrase) visit(u.getUnstringWithPointerPhrase(), p));
        u = u.withUnstringTallyingPhrase((Cobol.UnstringTallyingPhrase) visit(u.getUnstringTallyingPhrase(), p));
        u = u.withOnOverflowPhrase((Cobol.StatementPhrase) visit(u.getOnOverflowPhrase(), p));
        u = u.withNotOnOverflowPhrase((Cobol.StatementPhrase) visit(u.getNotOnOverflowPhrase(), p));
        u = u.withEndUnstring((Cobol.CobolWord) visit(u.getEndUnstring(), p));
        return u;
    }

    public Cobol visitUnstringCountIn(Cobol.UnstringCountIn unstringCountIn, P p) {
        Cobol.UnstringCountIn u = unstringCountIn;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withIdentifier((Identifier) visit(u.getIdentifier(), p));
        return u;
    }

    public Cobol visitUnstringDelimitedByPhrase(Cobol.UnstringDelimitedByPhrase unstringDelimitedByPhrase, P p) {
        Cobol.UnstringDelimitedByPhrase u = unstringDelimitedByPhrase;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        return u;
    }

    public Cobol visitUnstringDelimiterIn(Cobol.UnstringDelimiterIn unstringDelimiterIn, P p) {
        Cobol.UnstringDelimiterIn u = unstringDelimiterIn;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withIdentifier((Identifier) visit(u.getIdentifier(), p));
        return u;
    }

    public Cobol visitUnstringInto(Cobol.UnstringInto unstringInto, P p) {
        Cobol.UnstringInto u = unstringInto;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withIdentifier((Identifier) visit(u.getIdentifier(), p));
        u = u.withUnstringDelimiterIn((Cobol.UnstringDelimiterIn) visit(u.getUnstringDelimiterIn(), p));
        u = u.withUnstringCountIn((Cobol.UnstringCountIn) visit(u.getUnstringCountIn(), p));
        return u;
    }

    public Cobol visitUnstringIntoPhrase(Cobol.UnstringIntoPhrase unstringIntoPhrase, P p) {
        Cobol.UnstringIntoPhrase u = unstringIntoPhrase;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withUnstringIntos(ListUtils.map(u.getUnstringIntos(), t -> (Cobol.UnstringInto) visit(t, p)));
        return u;
    }

    public Cobol visitUnstringOrAllPhrase(Cobol.UnstringOrAllPhrase unstringOrAllPhrase, P p) {
        Cobol.UnstringOrAllPhrase u = unstringOrAllPhrase;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        return u;
    }

    public Cobol visitUnstringSendingPhrase(Cobol.UnstringSendingPhrase unstringSendingPhrase, P p) {
        Cobol.UnstringSendingPhrase u = unstringSendingPhrase;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withIdentifier((Identifier) visit(u.getIdentifier(), p));
        u = u.withUnstringDelimitedByPhrase((Cobol.UnstringDelimitedByPhrase) visit(u.getUnstringDelimitedByPhrase(), p));
        u = u.withUnstringOrAllPhrases(ListUtils.map(u.getUnstringOrAllPhrases(), t -> (Cobol.UnstringOrAllPhrase) visit(t, p)));
        return u;
    }

    public Cobol visitUnstringTallyingPhrase(Cobol.UnstringTallyingPhrase unstringTallyingPhrase, P p) {
        Cobol.UnstringTallyingPhrase u = unstringTallyingPhrase;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withQualifiedDataName((Cobol.QualifiedDataName) visit(u.getQualifiedDataName(), p));
        return u;
    }

    public Cobol visitUnstringWithPointerPhrase(Cobol.UnstringWithPointerPhrase unstringWithPointerPhrase, P p) {
        Cobol.UnstringWithPointerPhrase u = unstringWithPointerPhrase;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withQualifiedDataName((Cobol.QualifiedDataName) visit(u.getQualifiedDataName(), p));
        return u;
    }

    public Cobol visitUseAfterClause(Cobol.UseAfterClause useAfterClause, P p) {
        Cobol.UseAfterClause u = useAfterClause;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withUseAfterOn((Cobol.UseAfterOn) visit(u.getUseAfterOn(), p));
        return u;
    }

    public Cobol visitUseAfterOn(Cobol.UseAfterOn useAfterOn, P p) {
        Cobol.UseAfterOn u = useAfterOn;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withFileNames(ListUtils.map(u.getFileNames(), t -> (Name) visit(t, p)));
        return u;
    }

    public Cobol visitUseDebugClause(Cobol.UseDebugClause useDebugClause, P p) {
        Cobol.UseDebugClause u = useDebugClause;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withUseDebugs(ListUtils.map(u.getUseDebugs(), t -> (Cobol.UseDebugOn) visit(t, p)));
        return u;
    }

    public Cobol visitUseDebugOn(Cobol.UseDebugOn useDebugOn, P p) {
        Cobol.UseDebugOn u = useDebugOn;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withProcedureName((Cobol.ProcedureName) visit(u.getProcedureName(), p));
        return u;
    }

    public Cobol visitUseStatement(Cobol.UseStatement useStatement, P p) {
        Cobol.UseStatement u = useStatement;
        u = u.withPrefix(visitSpace(u.getPrefix(), p));
        u = u.withMarkers(visitMarkers(u.getMarkers(), p));
        u = u.withClause(visit(u.getClause(), p));
        return u;
    }

    public Cobol visitValueOfClause(Cobol.ValueOfClause valueOfClause, P p) {
        Cobol.ValueOfClause v = valueOfClause;
        v = v.withPrefix(visitSpace(v.getPrefix(), p));
        v = v.withMarkers(visitMarkers(v.getMarkers(), p));
        v = v.withValueOf(ListUtils.map(v.getValueOf(), t -> (Cobol.CobolWord) visit(t, p)));
        v = v.withValuePairs(ListUtils.map(v.getValuePairs(), t -> (Cobol.ValuePair) visit(t, p)));
        return v;
    }

    public Cobol visitValuePair(Cobol.ValuePair valuePair, P p) {
        Cobol.ValuePair v = valuePair;
        v = v.withPrefix(visitSpace(v.getPrefix(), p));
        v = v.withMarkers(visitMarkers(v.getMarkers(), p));
        v = v.withSystemName((Cobol.CobolWord) visit(v.getSystemName(), p));
        return v;
    }

    public Cobol visitValuedObjectComputerClause(Cobol.ValuedObjectComputerClause valuedObjectComputerClause, P p) {
        Cobol.ValuedObjectComputerClause v = valuedObjectComputerClause;
        v = v.withPrefix(visitSpace(v.getPrefix(), p));
        v = v.withMarkers(visitMarkers(v.getMarkers(), p));
        v = v.withWords(ListUtils.map(v.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        v = v.withValue(visit(v.getValue(), p));
        v = v.withUnits((Cobol.CobolWord) visit(v.getUnits(), p));
        return v;
    }

    public Cobol visitWorkingStorageSection(Cobol.WorkingStorageSection workingStorageSection, P p) {
        Cobol.WorkingStorageSection w = workingStorageSection;
        w = w.withPrefix(visitSpace(w.getPrefix(), p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));
        w = w.withWords(ListUtils.map(w.getWords(), it -> (Cobol.CobolWord) visit(it, p)));
        w = w.withDot((Cobol.CobolWord) visit(w.getDot(), p));
        w = w.withDataDescriptions(ListUtils.map(w.getDataDescriptions(), it -> (Cobol.DataDescriptionEntry) visit(it, p)));
        return w;
    }

    public Cobol visitWrite(Cobol.Write write, P p) {
        Cobol.Write w = write;
        w = w.withPrefix(visitSpace(w.getPrefix(), p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));
        w = w.withRecordName((Cobol.QualifiedDataName) visit(w.getRecordName(), p));
        w = w.withWriteFromPhrase((Cobol.WriteFromPhrase) visit(w.getWriteFromPhrase(), p));
        w = w.withWriteAdvancingPhrase((Cobol.WriteAdvancingPhrase) visit(w.getWriteAdvancingPhrase(), p));
        w = w.withWriteAtEndOfPagePhrase((Cobol.StatementPhrase) visit(w.getWriteAtEndOfPagePhrase(), p));
        w = w.withWriteNotAtEndOfPagePhrase((Cobol.StatementPhrase) visit(w.getWriteNotAtEndOfPagePhrase(), p));
        w = w.withInvalidKeyPhrase((Cobol.StatementPhrase) visit(w.getInvalidKeyPhrase(), p));
        w = w.withNotInvalidKeyPhrase((Cobol.StatementPhrase) visit(w.getNotInvalidKeyPhrase(), p));
        w = w.withEndWrite((Cobol.CobolWord) visit(w.getEndWrite(), p));
        return w;
    }

    public Cobol visitWriteAdvancingLines(Cobol.WriteAdvancingLines writeAdvancingLines, P p) {
        Cobol.WriteAdvancingLines w = writeAdvancingLines;
        w = w.withPrefix(visitSpace(w.getPrefix(), p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));
        return w;
    }

    public Cobol visitWriteAdvancingMnemonic(Cobol.WriteAdvancingMnemonic writeAdvancingMnemonic, P p) {
        Cobol.WriteAdvancingMnemonic w = writeAdvancingMnemonic;
        w = w.withPrefix(visitSpace(w.getPrefix(), p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));
        return w;
    }

    public Cobol visitWriteAdvancingPage(Cobol.WriteAdvancingPage writeAdvancingPage, P p) {
        Cobol.WriteAdvancingPage w = writeAdvancingPage;
        w = w.withPrefix(visitSpace(w.getPrefix(), p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));
        return w;
    }

    public Cobol visitWriteAdvancingPhrase(Cobol.WriteAdvancingPhrase writeAdvancingPhrase, P p) {
        Cobol.WriteAdvancingPhrase w = writeAdvancingPhrase;
        w = w.withPrefix(visitSpace(w.getPrefix(), p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));
        return w;
    }

    public Cobol visitWriteFromPhrase(Cobol.WriteFromPhrase writeFromPhrase, P p) {
        Cobol.WriteFromPhrase w = writeFromPhrase;
        w = w.withPrefix(visitSpace(w.getPrefix(), p));
        w = w.withMarkers(visitMarkers(w.getMarkers(), p));
        return w;
    }


    public <P2 extends Cobol> CobolContainer<P2> visitContainer(@Nullable CobolContainer<P2> container, P p) {
        if (container == null) {
            //noinspection ConstantConditions
            return null;
        }

        setCursor(new Cursor(getCursor(), container));

        Space before = visitSpace(container.getBefore(), p);
        CobolLeftPadded<String> preposition = visitLeftPadded(container.getPreposition(), p);
        List<CobolRightPadded<P2>> ps = ListUtils.map(container.getPadding().getElements(), t -> visitRightPadded(t, p));
        Markers markers = visitMarkers(container.getMarkers(), p);

        setCursor(getCursor().getParent());

        return (ps == container.getPadding().getElements() && before == container.getBefore() && preposition == container.getPreposition() &&
                markers == container.getMarkers()) ?
                container :
                CobolContainer.build(before, preposition, ps, markers);
    }


    public <T> CobolLeftPadded<T> visitLeftPadded(@Nullable CobolLeftPadded<T> left, P p) {
        if (left == null) {
            //noinspection ConstantConditions
            return null;
        }

        setCursor(new Cursor(getCursor(), left));

        Space before = visitSpace(left.getBefore(), p);
        T t = left.getElement();

        if (t instanceof Cobol) {
            //noinspection unchecked
            t = visitAndCast((Cobol) left.getElement(), p);
        }

        setCursor(getCursor().getParent());
        if (t == null) {
            //noinspection ConstantConditions
            return null;
        }

        return (before == left.getBefore() && t == left.getElement()) ? left : new CobolLeftPadded<>(before, t, left.getMarkers());
    }


    @SuppressWarnings("ConstantConditions")
    public <T> CobolRightPadded<T> visitRightPadded(@Nullable CobolRightPadded<T> right, P p) {
        if (right == null) {
            //noinspection ConstantConditions
            return null;
        }

        setCursor(new Cursor(getCursor(), right));

        T t = right.getElement();
        if (t instanceof Cobol) {
            //noinspection unchecked
            t = (T) visit((Cobol) right.getElement(), p);
        }

        setCursor(getCursor().getParent());
        if (t == null) {
            //noinspection ConstantConditions
            return null;
        }
        Space after = visitSpace(right.getAfter(), p);
        return (after == right.getAfter() && t == right.getElement()) ? right : new CobolRightPadded<>(t, after, right.getMarkers());
    }

}