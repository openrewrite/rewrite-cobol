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

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.CobolVisitor;
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

import static org.openrewrite.cobol.CobolPrinterUtils.fillArea;

/**
 * Print the original COBOL source code.
 *
 * `printOriginalSource`:
 *      true: Print as the original source code before preprocessing commands like COPY and REPLACE.
 *      false: Print the post-processed AST.
 *
 * Note: All the logic to print column areas exists in visitWord.
 */
public class CobolSourcePrinter<P> extends CobolVisitor<PrintOutputCapture<P>> {

    private final CobolPreprocessorSourcePrinter<ExecutionContext> printer = new CobolPreprocessorSourcePrinter<>(true);
    private int originalReplaceLength;
    private final boolean printColumns;
    private final boolean printCopiedSource;

    @Nullable
    private String copyUuid = null;

    public CobolSourcePrinter(boolean printColumns,
                              boolean printCopiedSource) {
        this.printColumns = printColumns;
        this.printCopiedSource = printCopiedSource;
    }

    public Cobol visitAbbreviation(Cobol.Abbreviation abbreviation, PrintOutputCapture<P> p) {
        visitSpace(abbreviation.getPrefix(), Space.Location.ABBREVIATION_PREFIX, p);
        visitMarkers(abbreviation.getMarkers(), p);
        visit(abbreviation.getNot(), p);
        visit(abbreviation.getRelationalOperator(), p);
        if (abbreviation.getLeftParen() != null) {
            visit(abbreviation.getLeftParen(), p);
        }
        visit(abbreviation.getArithmeticExpression(), p);
        visit(abbreviation.getAbbreviation(), p);
        if (abbreviation.getRightParen() != null) {
            visit(abbreviation.getRightParen(), p);
        }
        return abbreviation;
    }

    public Cobol visitAccept(Cobol.Accept accept, PrintOutputCapture<P> p) {
        visitSpace(accept.getPrefix(), Space.Location.ACCEPT_PREFIX, p);
        visitMarkers(accept.getMarkers(), p);
        visit(accept.getAccept(), p);
        visit(accept.getIdentifier(), p);
        visit(accept.getOperation(), p);
        visit(accept.getOnExceptionClause(), p);
        visit(accept.getNotOnExceptionClause(), p);
        visit(accept.getEndAccept(), p);
        return accept;
    }

    public Cobol visitAcceptFromDateStatement(Cobol.AcceptFromDateStatement acceptFromDateStatement, PrintOutputCapture<P> p) {
        visitSpace(acceptFromDateStatement.getPrefix(), Space.Location.ACCEPT_FROM_DATE_STATEMENT_PREFIX, p);
        visitMarkers(acceptFromDateStatement.getMarkers(), p);
        visit(acceptFromDateStatement.getWords(), p);
        return acceptFromDateStatement;
    }

    public Cobol visitAcceptFromEscapeKeyStatement(Cobol.AcceptFromEscapeKeyStatement acceptFromEscapeKeyStatement, PrintOutputCapture<P> p) {
        visitSpace(acceptFromEscapeKeyStatement.getPrefix(), Space.Location.ACCEPT_FROM_ESCAPE_KEY_STATEMENT_PREFIX, p);
        visitMarkers(acceptFromEscapeKeyStatement.getMarkers(), p);
        visit(acceptFromEscapeKeyStatement.getWords(), p);
        return acceptFromEscapeKeyStatement;
    }

    public Cobol visitAcceptFromMnemonicStatement(Cobol.AcceptFromMnemonicStatement acceptFromMnemonicStatement, PrintOutputCapture<P> p) {
        visitSpace(acceptFromMnemonicStatement.getPrefix(), Space.Location.ACCEPT_FROM_MNEMONIC_STATEMENT_PREFIX, p);
        visitMarkers(acceptFromMnemonicStatement.getMarkers(), p);
        visit(acceptFromMnemonicStatement.getFrom(), p);
        visit(acceptFromMnemonicStatement.getMnemonicName(), p);
        return acceptFromMnemonicStatement;
    }

    public Cobol visitAcceptMessageCountStatement(Cobol.AcceptMessageCountStatement acceptMessageCountStatement, PrintOutputCapture<P> p) {
        visitSpace(acceptMessageCountStatement.getPrefix(), Space.Location.ACCEPT_MESSAGE_COUNT_STATEMENT_PREFIX, p);
        visitMarkers(acceptMessageCountStatement.getMarkers(), p);
        visit(acceptMessageCountStatement.getWords(), p);
        return acceptMessageCountStatement;
    }

    public Cobol visitAccessModeClause(Cobol.AccessModeClause accessModeClause, PrintOutputCapture<P> p) {
        visitSpace(accessModeClause.getPrefix(), Space.Location.ACCESS_MODE_CLAUSE_PREFIX, p);
        visitMarkers(accessModeClause.getMarkers(), p);
        visit(accessModeClause.getWords(), p);
        visit(accessModeClause.getType(), p);
        return accessModeClause;
    }

    public Cobol visitAdd(Cobol.Add add, PrintOutputCapture<P> p) {
        visitSpace(add.getPrefix(), Space.Location.ADD_PREFIX, p);
        visitMarkers(add.getMarkers(), p);
        visit(add.getAdd(), p);
        visit(add.getOperation(), p);
        visit(add.getOnSizeError(), p);
        visit(add.getNotOnSizeError(), p);
        visit(add.getEndAdd(), p);
        return add;
    }

    public Cobol visitAddCorresponding(Cobol.AddCorresponding addCorresponding, PrintOutputCapture<P> p) {
        visitSpace(addCorresponding.getPrefix(), Space.Location.ADD_CORRESPONDING_PREFIX, p);
        visitMarkers(addCorresponding.getMarkers(), p);
        visit(addCorresponding.getCorresponding(), p);
        visit(addCorresponding.getIdentifier(), p);
        visit(addCorresponding.getTo(), p);
        visit(addCorresponding.getRoundable(), p);
        return addCorresponding;
    }

    public Cobol visitAddTo(Cobol.AddTo addTo, PrintOutputCapture<P> p) {
        visitSpace(addTo.getPrefix(), Space.Location.ADD_TO_PREFIX, p);
        visitMarkers(addTo.getMarkers(), p);
        visit(addTo.getFrom(), p);
        visit(addTo.getTo(), p);
        visit(addTo.getRoundables(), p);
        return addTo;
    }

    public Cobol visitAddToGiving(Cobol.AddToGiving addToGiving, PrintOutputCapture<P> p) {
        visitSpace(addToGiving.getPrefix(), Space.Location.ADD_TO_GIVING_PREFIX, p);
        visitMarkers(addToGiving.getMarkers(), p);
        visit(addToGiving.getFrom(), p);
        visit(addToGiving.getTo(), p);
        visit(addToGiving.getNames(), p);
        visit(addToGiving.getGiving(), p);
        visit(addToGiving.getRoundables(), p);
        return addToGiving;
    }

    public Cobol visitAlphabetAlso(Cobol.AlphabetAlso alphabetAlso, PrintOutputCapture<P> p) {
        visitSpace(alphabetAlso.getPrefix(), Space.Location.ALPHABET_ALSO_PREFIX, p);
        visitMarkers(alphabetAlso.getMarkers(), p);
        visit(alphabetAlso.getWord(), p);
        visit(alphabetAlso.getLiterals(), p);
        return alphabetAlso;
    }

    public Cobol visitAlphabetClause(Cobol.AlphabetClause alphabetClause, PrintOutputCapture<P> p) {
        visitSpace(alphabetClause.getPrefix(), Space.Location.ALPHABET_CLAUSE_PREFIX, p);
        visitMarkers(alphabetClause.getMarkers(), p);
        visit(alphabetClause.getAlphabet(), p);
        visit(alphabetClause.getName(), p);
        visit(alphabetClause.getWords(), p);
        return alphabetClause;
    }

    public Cobol visitAlphabetLiteral(Cobol.AlphabetLiteral alphabetLiteral, PrintOutputCapture<P> p) {
        visitSpace(alphabetLiteral.getPrefix(), Space.Location.ALPHABET_LITERAL_PREFIX, p);
        visitMarkers(alphabetLiteral.getMarkers(), p);
        visit(alphabetLiteral.getLiteral(), p);
        visit(alphabetLiteral.getAlphabetThrough(), p);
        visit(alphabetLiteral.getAlphabetAlso(), p);
        return alphabetLiteral;
    }

    public Cobol visitAlphabetThrough(Cobol.AlphabetThrough alphabetThrough, PrintOutputCapture<P> p) {
        visitSpace(alphabetThrough.getPrefix(), Space.Location.ALPHABET_THROUGH_PREFIX, p);
        visitMarkers(alphabetThrough.getMarkers(), p);
        visit(alphabetThrough.getWord(), p);
        visit(alphabetThrough.getLiteral(), p);
        return alphabetThrough;
    }

    public Cobol visitAlterProceedTo(Cobol.AlterProceedTo alterProceedTo, PrintOutputCapture<P> p) {
        visitSpace(alterProceedTo.getPrefix(), Space.Location.ALTER_PROCEED_TO_PREFIX, p);
        visitMarkers(alterProceedTo.getMarkers(), p);
        visit(alterProceedTo.getFrom(), p);
        visit(alterProceedTo.getWords(), p);
        visit(alterProceedTo.getTo(), p);
        return alterProceedTo;
    }

    public Cobol visitAlterStatement(Cobol.AlterStatement alterStatement, PrintOutputCapture<P> p) {
        visitSpace(alterStatement.getPrefix(), Space.Location.ALTER_STATEMENT_PREFIX, p);
        visitMarkers(alterStatement.getMarkers(), p);
        visit(alterStatement.getWord(), p);
        visit(alterStatement.getAlterProceedTo(), p);
        return alterStatement;
    }

    public Cobol visitAlteredGoTo(Cobol.AlteredGoTo alteredGoTo, PrintOutputCapture<P> p) {
        visitSpace(alteredGoTo.getPrefix(), Space.Location.ALTERED_GO_TO_PREFIX, p);
        visitMarkers(alteredGoTo.getMarkers(), p);
        visit(alteredGoTo.getWords(), p);
        visit(alteredGoTo.getDot(), p);
        return alteredGoTo;
    }

    public Cobol visitAlternateRecordKeyClause(Cobol.AlternateRecordKeyClause alternateRecordKeyClause, PrintOutputCapture<P> p) {
        visitSpace(alternateRecordKeyClause.getPrefix(), Space.Location.ALTERNATE_RECORD_KEY_CLAUSE_PREFIX, p);
        visitMarkers(alternateRecordKeyClause.getMarkers(), p);
        visit(alternateRecordKeyClause.getAlternateWords(), p);
        visit(alternateRecordKeyClause.getQualifiedDataName(), p);
        visit(alternateRecordKeyClause.getPasswordClause(), p);
        visit(alternateRecordKeyClause.getDuplicates(), p);
        return alternateRecordKeyClause;
    }

    public Cobol visitAndOrCondition(Cobol.AndOrCondition andOrCondition, PrintOutputCapture<P> p) {
        visitSpace(andOrCondition.getPrefix(), Space.Location.AND_OR_CONDITION_PREFIX, p);
        visitMarkers(andOrCondition.getMarkers(), p);
        visit(andOrCondition.getLogicalOperator(), p);
        visit(andOrCondition.getCombinableCondition(), p);
        visit(andOrCondition.getAbbreviations(), p);
        return andOrCondition;
    }

    public Cobol visitArgument(Cobol.Argument argument, PrintOutputCapture<P> p) {
        visitSpace(argument.getPrefix(), Space.Location.ARGUMENT_PREFIX, p);
        visitMarkers(argument.getMarkers(), p);
        visit(argument.getFirst(), p);
        visit(argument.getIntegerLiteral(), p);
        return argument;
    }

    public Cobol visitArithmeticExpression(Cobol.ArithmeticExpression arithmeticExpression, PrintOutputCapture<P> p) {
        visitSpace(arithmeticExpression.getPrefix(), Space.Location.ARITHMETIC_EXPRESSION_PREFIX, p);
        visitMarkers(arithmeticExpression.getMarkers(), p);
        visit(arithmeticExpression.getMultDivs(), p);
        visit(arithmeticExpression.getPlusMinuses(), p);
        return arithmeticExpression;
    }

    public Cobol visitAssignClause(Cobol.AssignClause assignClause, PrintOutputCapture<P> p) {
        visitSpace(assignClause.getPrefix(), Space.Location.ASSIGN_CLAUSE_PREFIX, p);
        visitMarkers(assignClause.getMarkers(), p);
        visit(assignClause.getWords(), p);
        visit(assignClause.getName(), p);
        return assignClause;
    }

    public Cobol visitBlockContainsClause(Cobol.BlockContainsClause blockContainsClause, PrintOutputCapture<P> p) {
        visitSpace(blockContainsClause.getPrefix(), Space.Location.BLOCK_CONTAINS_PREFIX, p);
        visitMarkers(blockContainsClause.getMarkers(), p);
        visit(blockContainsClause.getFirstWords(), p);
        visit(blockContainsClause.getIntegerLiteral(), p);
        visit(blockContainsClause.getBlockContainsTo(), p);
        visit(blockContainsClause.getLastWord(), p);
        return blockContainsClause;
    }

    public Cobol visitBlockContainsTo(Cobol.BlockContainsTo blockContainsTo, PrintOutputCapture<P> p) {
        visitSpace(blockContainsTo.getPrefix(), Space.Location.BLOCK_CONTAINS_TO_PREFIX, p);
        visitMarkers(blockContainsTo.getMarkers(), p);
        visit(blockContainsTo.getTo(), p);
        visit(blockContainsTo.getIntegerLiteral(), p);
        return blockContainsTo;
    }

    public Cobol visitCall(Cobol.Call call, PrintOutputCapture<P> p) {
        visitSpace(call.getPrefix(), Space.Location.CALL_PREFIX, p);
        visitMarkers(call.getMarkers(), p);
        visit(call.getCall(), p);
        visit(call.getIdentifier(), p);
        visit(call.getCallUsingPhrase(), p);
        visit(call.getCallGivingPhrase(), p);
        visit(call.getOnOverflowPhrase(), p);
        visit(call.getOnExceptionClause(), p);
        visit(call.getNotOnExceptionClause(), p);
        visit(call.getEndCall(), p);
        return call;
    }

    public Cobol visitCallBy(Cobol.CallBy callBy, PrintOutputCapture<P> p) {
        visitSpace(callBy.getPrefix(), Space.Location.CALL_BY_PREFIX, p);
        visitMarkers(callBy.getMarkers(), p);
        visit(callBy.getWords(), p);
        visit(callBy.getIdentifier(), p);
        return callBy;
    }

    public Cobol visitCallGivingPhrase(Cobol.CallGivingPhrase callGivingPhrase, PrintOutputCapture<P> p) {
        visitSpace(callGivingPhrase.getPrefix(), Space.Location.CALL_GIVING_PHRASE_PREFIX, p);
        visitMarkers(callGivingPhrase.getMarkers(), p);
        visit(callGivingPhrase.getWord(), p);
        visit(callGivingPhrase.getIdentifier(), p);
        return callGivingPhrase;
    }

    public Cobol visitCallPhrase(Cobol.CallPhrase callPhrase, PrintOutputCapture<P> p) {
        visitSpace(callPhrase.getPrefix(), Space.Location.CALL_PHRASE_PREFIX, p);
        visitMarkers(callPhrase.getMarkers(), p);
        visit(callPhrase.getWords(), p);
        visit(callPhrase.getParameters(), p);
        return callPhrase;
    }

    public Cobol visitCancel(Cobol.Cancel cancel, PrintOutputCapture<P> p) {
        visitSpace(cancel.getPrefix(), Space.Location.CANCEL_PREFIX, p);
        visitMarkers(cancel.getMarkers(), p);
        visit(cancel.getCancel(), p);
        visit(cancel.getCancelCalls(), p);
        return cancel;
    }

    public Cobol visitCancelCall(Cobol.CancelCall cancelCall, PrintOutputCapture<P> p) {
        visitSpace(cancelCall.getPrefix(), Space.Location.CANCEL_CALL_PREFIX, p);
        visitMarkers(cancelCall.getMarkers(), p);
        visit(cancelCall.getLibraryName(), p);
        visit(cancelCall.getBy(), p);
        visit(cancelCall.getIdentifier(), p);
        visit(cancelCall.getLiteral(), p);
        return cancelCall;
    }

    public Cobol visitChannelClause(Cobol.ChannelClause channelClause, PrintOutputCapture<P> p) {
        visitSpace(channelClause.getPrefix(), Space.Location.CHANNEL_CLAUSE_PREFIX, p);
        visitMarkers(channelClause.getMarkers(), p);
        visit(channelClause.getWord(), p);
        visit(channelClause.getLiteral(), p);
        visit(channelClause.getIs(), p);
        visit(channelClause.getMnemonicName(), p);
        return channelClause;
    }

    public Cobol visitClassClause(Cobol.ClassClause classClause, PrintOutputCapture<P> p) {
        visitSpace(classClause.getPrefix(), Space.Location.CLASS_CLAUSE_PREFIX, p);
        visitMarkers(classClause.getMarkers(), p);
        visit(classClause.getClazz(), p);
        visit(classClause.getClassName(), p);
        visit(classClause.getWords(), p);
        visit(classClause.getThroughs(), p);
        return classClause;
    }

    public Cobol visitClassClauseThrough(Cobol.ClassClauseThrough classClauseThrough, PrintOutputCapture<P> p) {
        visitSpace(classClauseThrough.getPrefix(), Space.Location.CLASS_CLAUSE_THROUGH_PREFIX, p);
        visitMarkers(classClauseThrough.getMarkers(), p);
        visit(classClauseThrough.getFrom(), p);
        visit(classClauseThrough.getThrough(), p);
        visit(classClauseThrough.getTo(), p);
        return classClauseThrough;
    }

    public Cobol visitClassCondition(Cobol.ClassCondition classCondition, PrintOutputCapture<P> p) {
        visitSpace(classCondition.getPrefix(), Space.Location.CLASS_CONDITION_PREFIX, p);
        visitMarkers(classCondition.getMarkers(), p);
        visit(classCondition.getName(), p);
        visit(classCondition.getWords(), p);
        visit(classCondition.getType(), p);
        return classCondition;
    }

    public Cobol visitClose(Cobol.Close close, PrintOutputCapture<P> p) {
        visitSpace(close.getPrefix(), Space.Location.CLOSE_PREFIX, p);
        visitMarkers(close.getMarkers(), p);
        visit(close.getClose(), p);
        visit(close.getCloseFiles(), p);
        return close;
    }

    public Cobol visitCloseFile(Cobol.CloseFile closeFile, PrintOutputCapture<P> p) {
        visitSpace(closeFile.getPrefix(), Space.Location.CLOSE_FILE_PREFIX, p);
        visitMarkers(closeFile.getMarkers(), p);
        visit(closeFile.getFileName(), p);
        visit(closeFile.getCloseStatement(), p);
        return closeFile;
    }

    public Cobol visitClosePortFileIOStatement(Cobol.ClosePortFileIOStatement closePortFileIOStatement, PrintOutputCapture<P> p) {
        visitSpace(closePortFileIOStatement.getPrefix(), Space.Location.CLOSE_PORT_FILE_IO_STATEMENT_PREFIX, p);
        visitMarkers(closePortFileIOStatement.getMarkers(), p);
        visit(closePortFileIOStatement.getWords(), p);
        visit(closePortFileIOStatement.getClosePortFileIOUsing(), p);
        return closePortFileIOStatement;
    }

    public Cobol visitClosePortFileIOUsingAssociatedData(Cobol.ClosePortFileIOUsingAssociatedData closePortFileIOUsingAssociatedData, PrintOutputCapture<P> p) {
        visitSpace(closePortFileIOUsingAssociatedData.getPrefix(), Space.Location.CLOSE_PORT_FILE_IO_USING_ASSOCIATED_DATA_PREFIX, p);
        visitMarkers(closePortFileIOUsingAssociatedData.getMarkers(), p);
        visit(closePortFileIOUsingAssociatedData.getAssociatedData(), p);
        visit(closePortFileIOUsingAssociatedData.getIdentifier(), p);
        return closePortFileIOUsingAssociatedData;
    }

    public Cobol visitClosePortFileIOUsingAssociatedDataLength(Cobol.ClosePortFileIOUsingAssociatedDataLength closePortFileIOUsingAssociatedDataLength, PrintOutputCapture<P> p) {
        visitSpace(closePortFileIOUsingAssociatedDataLength.getPrefix(), Space.Location.CLOSE_PORT_FILE_IO_USING_ASSOCIATED_DATA_LENGTH_PREFIX, p);
        visitMarkers(closePortFileIOUsingAssociatedDataLength.getMarkers(), p);
        visit(closePortFileIOUsingAssociatedDataLength.getWords(), p);
        visit(closePortFileIOUsingAssociatedDataLength.getIdentifier(), p);
        return closePortFileIOUsingAssociatedDataLength;
    }

    public Cobol visitClosePortFileIOUsingCloseDisposition(Cobol.ClosePortFileIOUsingCloseDisposition closePortFileIOUsingCloseDisposition, PrintOutputCapture<P> p) {
        visitSpace(closePortFileIOUsingCloseDisposition.getPrefix(), Space.Location.CLOSE_PORT_FILE_IO_USING_CLOSE_DISPOSITION_PREFIX, p);
        visitMarkers(closePortFileIOUsingCloseDisposition.getMarkers(), p);
        visit(closePortFileIOUsingCloseDisposition.getWords(), p);
        return closePortFileIOUsingCloseDisposition;
    }

    public Cobol visitCloseReelUnitStatement(Cobol.CloseReelUnitStatement closeReelUnitStatement, PrintOutputCapture<P> p) {
        visitSpace(closeReelUnitStatement.getPrefix(), Space.Location.CLOSE_REEL_UNIT_STATEMENT_PREFIX, p);
        visitMarkers(closeReelUnitStatement.getMarkers(), p);
        visit(closeReelUnitStatement.getWords(), p);
        return closeReelUnitStatement;
    }

    public Cobol visitCloseRelativeStatement(Cobol.CloseRelativeStatement closeRelativeStatement, PrintOutputCapture<P> p) {
        visitSpace(closeRelativeStatement.getPrefix(), Space.Location.CLOSE_RELATIVE_STATEMENT_PREFIX, p);
        visitMarkers(closeRelativeStatement.getMarkers(), p);
        visit(closeRelativeStatement.getWords(), p);
        return closeRelativeStatement;
    }

    public Cobol visitCodeSetClause(Cobol.CodeSetClause codeSetClause, PrintOutputCapture<P> p) {
        visitSpace(codeSetClause.getPrefix(), Space.Location.CLOSE_SET_CLAUSE_PREFIX, p);
        visitMarkers(codeSetClause.getMarkers(), p);
        visit(codeSetClause.getWords(), p);
        visit(codeSetClause.getAlphabetName(), p);
        return codeSetClause;
    }

    public Cobol visitCollatingSequenceAlphabet(Cobol.CollatingSequenceAlphabet collatingSequenceAlphabet, PrintOutputCapture<P> p) {
        visitSpace(collatingSequenceAlphabet.getPrefix(), Space.Location.COLLATING_SEQUENCE_ALPHABET_PREFIX, p);
        visitMarkers(collatingSequenceAlphabet.getMarkers(), p);
        visit(collatingSequenceAlphabet.getWords(), p);
        visit(collatingSequenceAlphabet.getAlphabetName(), p);
        return collatingSequenceAlphabet;
    }

    public Cobol visitCollatingSequenceClause(Cobol.CollatingSequenceClause collatingSequenceClause, PrintOutputCapture<P> p) {
        visitSpace(collatingSequenceClause.getPrefix(), Space.Location.COLLATING_SEQUENCE_CLAUSE_PREFIX, p);
        visitMarkers(collatingSequenceClause.getMarkers(), p);
        visit(collatingSequenceClause.getWords(), p);
        visit(collatingSequenceClause.getIs(), p);
        visit(collatingSequenceClause.getAlphabetName(), p);
        visit(collatingSequenceClause.getAlphanumeric(), p);
        visit(collatingSequenceClause.getNational(), p);
        return collatingSequenceClause;
    }

    public Cobol visitCombinableCondition(Cobol.CombinableCondition combinableCondition, PrintOutputCapture<P> p) {
        visitSpace(combinableCondition.getPrefix(), Space.Location.COMBINABLE_CONDITION_PREFIX, p);
        visitMarkers(combinableCondition.getMarkers(), p);
        visit(combinableCondition.getNot(), p);
        visit(combinableCondition.getSimpleCondition(), p);
        return combinableCondition;
    }

    public Cobol visitCommentEntry(Cobol.CommentEntry commentEntry, PrintOutputCapture<P> p) {
        visitSpace(commentEntry.getPrefix(), Space.Location.COMMENT_ENTRY_PREFIX, p);
        visitMarkers(commentEntry.getMarkers(), p);
        visit(commentEntry.getComments(), p);
        return commentEntry;
    }

    public Cobol visitCommitmentControlClause(Cobol.CommitmentControlClause commitmentControlClause, PrintOutputCapture<P> p) {
        visitSpace(commitmentControlClause.getPrefix(), Space.Location.COMMITMENT_CONTROL_PREFIX, p);
        visitMarkers(commitmentControlClause.getMarkers(), p);
        visit(commitmentControlClause.getWords(), p);
        visit(commitmentControlClause.getFileName(), p);
        return commitmentControlClause;
    }

    public Cobol visitCommunicationDescriptionEntryFormat1(Cobol.CommunicationDescriptionEntryFormat1 communicationDescriptionEntryFormat1, PrintOutputCapture<P> p) {
        visitSpace(communicationDescriptionEntryFormat1.getPrefix(), Space.Location.COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_1_PREFIX, p);
        visitMarkers(communicationDescriptionEntryFormat1.getMarkers(), p);
        visit(communicationDescriptionEntryFormat1.getCd(), p);
        visit(communicationDescriptionEntryFormat1.getName(), p);
        visit(communicationDescriptionEntryFormat1.getWords(), p);
        visit(communicationDescriptionEntryFormat1.getInputs(), p);
        visit(communicationDescriptionEntryFormat1.getDot(), p);
        return communicationDescriptionEntryFormat1;
    }

    public Cobol visitCommunicationDescriptionEntryFormat2(Cobol.CommunicationDescriptionEntryFormat2 communicationDescriptionEntryFormat2, PrintOutputCapture<P> p) {
        visitSpace(communicationDescriptionEntryFormat2.getPrefix(), Space.Location.COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_2_PREFIX, p);
        visitMarkers(communicationDescriptionEntryFormat2.getMarkers(), p);
        visit(communicationDescriptionEntryFormat2.getCd(), p);
        visit(communicationDescriptionEntryFormat2.getName(), p);
        visit(communicationDescriptionEntryFormat2.getWords(), p);
        visit(communicationDescriptionEntryFormat2.getOutputs(), p);
        visit(communicationDescriptionEntryFormat2.getDot(), p);
        return communicationDescriptionEntryFormat2;
    }

    public Cobol visitCommunicationDescriptionEntryFormat3(Cobol.CommunicationDescriptionEntryFormat3 communicationDescriptionEntryFormat3, PrintOutputCapture<P> p) {
        visitSpace(communicationDescriptionEntryFormat3.getPrefix(), Space.Location.COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_3_PREFIX, p);
        visitMarkers(communicationDescriptionEntryFormat3.getMarkers(), p);
        visit(communicationDescriptionEntryFormat3.getCd(), p);
        visit(communicationDescriptionEntryFormat3.getName(), p);
        visit(communicationDescriptionEntryFormat3.getWords(), p);
        visit(communicationDescriptionEntryFormat3.getInitialIOs(), p);
        visit(communicationDescriptionEntryFormat3.getDot(), p);
        return communicationDescriptionEntryFormat3;
    }

    public Cobol visitCommunicationSection(Cobol.CommunicationSection communicationSection, PrintOutputCapture<P> p) {
        visitSpace(communicationSection.getPrefix(), Space.Location.COMMUNICATION_SECTION_PREFIX, p);
        visitMarkers(communicationSection.getMarkers(), p);
        visit(communicationSection.getWords(), p);
        visit(communicationSection.getDot(), p);
        visit(communicationSection.getEntries(), p);
        return communicationSection;
    }

    @Override
    public Cobol visitCompilationUnit(Cobol.CompilationUnit compilationUnit, PrintOutputCapture<P> p) {
        visitSpace(compilationUnit.getPrefix(), Space.Location.COMPILATION_UNIT_PREFIX, p);
        visitMarkers(compilationUnit.getMarkers(), p);
        visit(compilationUnit.getProgramUnits(), p);
        visit(compilationUnit.getEof(), p);
        return compilationUnit;
    }

    public Cobol visitCompute(Cobol.Compute compute, PrintOutputCapture<P> p) {
        visitSpace(compute.getPrefix(), Space.Location.COMPUTE_PREFIX, p);
        visitMarkers(compute.getMarkers(), p);
        visit(compute.getCompute(), p);
        visit(compute.getRoundables(), p);
        visit(compute.getEqualWord(), p);
        visit(compute.getArithmeticExpression(), p);
        visit(compute.getOnSizeErrorPhrase(), p);
        visit(compute.getNotOnSizeErrorPhrase(), p);
        visit(compute.getEndCompute(), p);
        return compute;
    }

    @Override
    public Cobol visitCondition(Cobol.Condition condition, PrintOutputCapture<P> p) {
        visitSpace(condition.getPrefix(), Space.Location.CONDITION_PREFIX, p);
        visitMarkers(condition.getMarkers(), p);
        visit(condition.getCombinableCondition(), p);
        visit(condition.getAndOrConditions(), p);
        return condition;
    }

    public Cobol visitConditionNameReference(Cobol.ConditionNameReference conditionNameReference, PrintOutputCapture<P> p) {
        visitSpace(conditionNameReference.getPrefix(), Space.Location.CONDITION_NAME_REFERENCE_PREFIX, p);
        visitMarkers(conditionNameReference.getMarkers(), p);
        visit(conditionNameReference.getName(), p);
        visit(conditionNameReference.getInDatas(), p);
        visit(conditionNameReference.getInFile(), p);
        visit(conditionNameReference.getReferences(), p);
        visit(conditionNameReference.getInMnemonics(), p);
        return conditionNameReference;
    }

    public Cobol visitConditionNameSubscriptReference(Cobol.ConditionNameSubscriptReference conditionNameSubscriptReference, PrintOutputCapture<P> p) {
        visitSpace(conditionNameSubscriptReference.getPrefix(), Space.Location.CONDITION_NAME_SUBSCRIPT_REFERENCE_PREFIX, p);
        visitMarkers(conditionNameSubscriptReference.getMarkers(), p);
        visit(conditionNameSubscriptReference.getLeftParen(), p);
        visit(conditionNameSubscriptReference.getSubscripts(), p);
        visit(conditionNameSubscriptReference.getRightParen(), p);
        return conditionNameSubscriptReference;
    }

    public Cobol visitConfigurationSection(Cobol.ConfigurationSection configurationSection, PrintOutputCapture<P> p) {
        visitSpace(configurationSection.getPrefix(), Space.Location.CONFIGURATION_SECTION_PREFIX, p);
        visitMarkers(configurationSection.getMarkers(), p);
        visit(configurationSection.getWords(), p);
        visit(configurationSection.getDot(), p);
        visit(configurationSection.getParagraphs(), p);
        return configurationSection;
    }

    public Cobol visitContinue(Cobol.Continue continuez, PrintOutputCapture<P> p) {
        visitSpace(continuez.getPrefix(), Space.Location.CONTINUE_PREFIX, p);
        visitMarkers(continuez.getMarkers(), p);
        visit(continuez.getWord(), p);
        return continuez;
    }

    public Cobol visitCurrencyClause(Cobol.CurrencyClause currencyClause, PrintOutputCapture<P> p) {
        visitSpace(currencyClause.getPrefix(), Space.Location.CURRENCY_CLAUSE_PREFIX, p);
        visitMarkers(currencyClause.getMarkers(), p);
        visit(currencyClause.getWords(), p);
        visit(currencyClause.getLiteral(), p);
        visit(currencyClause.getPictureSymbols(), p);
        visit(currencyClause.getPictureSymbolLiteral(), p);
        return currencyClause;
    }

    public Cobol visitDataAlignedClause(Cobol.DataAlignedClause dataAlignedClause, PrintOutputCapture<P> p) {
        visitSpace(dataAlignedClause.getPrefix(), Space.Location.DATA_ALIGNED_CLAUSE_PREFIX, p);
        visitMarkers(dataAlignedClause.getMarkers(), p);
        visit(dataAlignedClause.getAligned(), p);
        return dataAlignedClause;
    }

    public Cobol visitDataBaseSection(Cobol.DataBaseSection dataBaseSection, PrintOutputCapture<P> p) {
        visitSpace(dataBaseSection.getPrefix(), Space.Location.DATA_BASE_SECTION_PREFIX, p);
        visitMarkers(dataBaseSection.getMarkers(), p);
        visit(dataBaseSection.getWords(), p);
        visit(dataBaseSection.getDot(), p);
        visit(dataBaseSection.getEntries(), p);
        return dataBaseSection;
    }

    public Cobol visitDataBaseSectionEntry(Cobol.DataBaseSectionEntry dataBaseSectionEntry, PrintOutputCapture<P> p) {
        visitSpace(dataBaseSectionEntry.getPrefix(), Space.Location.DATA_BASE_SECTION_ENTRY_PREFIX, p);
        visitMarkers(dataBaseSectionEntry.getMarkers(), p);
        visit(dataBaseSectionEntry.getDb(), p);
        visit(dataBaseSectionEntry.getFrom(), p);
        visit(dataBaseSectionEntry.getInvoke(), p);
        visit(dataBaseSectionEntry.getTo(), p);
        return dataBaseSectionEntry;
    }

    public Cobol visitDataBlankWhenZeroClause(Cobol.DataBlankWhenZeroClause dataBlankWhenZeroClause, PrintOutputCapture<P> p) {
        visitSpace(dataBlankWhenZeroClause.getPrefix(), Space.Location.DATA_BLANK_WHEN_ZERO_CLAUSE_PREFIX, p);
        visitMarkers(dataBlankWhenZeroClause.getMarkers(), p);
        visit(dataBlankWhenZeroClause.getWords(), p);
        return dataBlankWhenZeroClause;
    }

    public Cobol visitDataCommonOwnLocalClause(Cobol.DataCommonOwnLocalClause dataCommonOwnLocalClause, PrintOutputCapture<P> p) {
        visitSpace(dataCommonOwnLocalClause.getPrefix(), Space.Location.DATA_COMMON_OWN_LOCAL_CLAUSE_PREFIX, p);
        visitMarkers(dataCommonOwnLocalClause.getMarkers(), p);
        visit(dataCommonOwnLocalClause.getWord(), p);
        return dataCommonOwnLocalClause;
    }

    public Cobol visitDataDescriptionEntry(Cobol.DataDescriptionEntry dataDescriptionEntry, PrintOutputCapture<P> p) {
        visitSpace(dataDescriptionEntry.getPrefix(), Space.Location.DATA_DESCRIPTION_ENTRY_PREFIX, p);
        visitMarkers(dataDescriptionEntry.getMarkers(), p);
        visit(dataDescriptionEntry.getWords(), p);
        visit(dataDescriptionEntry.getName(), p);
        visit(dataDescriptionEntry.getClauses(), p);
        visit(dataDescriptionEntry.getDot(), p);
        return dataDescriptionEntry;
    }

    public Cobol visitDataDivision(Cobol.DataDivision dataDivision, PrintOutputCapture<P> p) {
        visitSpace(dataDivision.getPrefix(), Space.Location.DATA_DIVISION_PREFIX, p);
        visitMarkers(dataDivision.getMarkers(), p);
        visit(dataDivision.getWords(), p);
        visit(dataDivision.getDot(), p);
        visit(dataDivision.getSections(), p);
        return dataDivision;
    }

    public Cobol visitDataExternalClause(Cobol.DataExternalClause dataExternalClause, PrintOutputCapture<P> p) {
        visitSpace(dataExternalClause.getPrefix(), Space.Location.DATA_EXTERNAL_CLAUSE_PREFIX, p);
        visitMarkers(dataExternalClause.getMarkers(), p);
        visit(dataExternalClause.getRedefines(), p);
        return dataExternalClause;
    }

    public Cobol visitDataGlobalClause(Cobol.DataGlobalClause dataGlobalClause, PrintOutputCapture<P> p) {
        visitSpace(dataGlobalClause.getPrefix(), Space.Location.DATA_GLOBAL_CLAUSE_PREFIX, p);
        visitMarkers(dataGlobalClause.getMarkers(), p);
        visit(dataGlobalClause.getWords(), p);
        return dataGlobalClause;
    }

    public Cobol visitDataIntegerStringClause(Cobol.DataIntegerStringClause dataIntegerStringClause, PrintOutputCapture<P> p) {
        visitSpace(dataIntegerStringClause.getPrefix(), Space.Location.DATA_INTEGER_STRING_CLAUSE_PREFIX, p);
        visitMarkers(dataIntegerStringClause.getMarkers(), p);
        visit(dataIntegerStringClause.getWord(), p);
        return dataIntegerStringClause;
    }

    public Cobol visitDataJustifiedClause(Cobol.DataJustifiedClause dataJustifiedClause, PrintOutputCapture<P> p) {
        visitSpace(dataJustifiedClause.getPrefix(), Space.Location.DATA_JUSTIFIED_CLAUSE_PREFIX, p);
        visitMarkers(dataJustifiedClause.getMarkers(), p);
        visit(dataJustifiedClause.getWords(), p);
        return dataJustifiedClause;
    }

    public Cobol visitDataOccursClause(Cobol.DataOccursClause dataOccursClause, PrintOutputCapture<P> p) {
        visitSpace(dataOccursClause.getPrefix(), Space.Location.DATA_OCCURS_CLAUSE_PREFIX, p);
        visitMarkers(dataOccursClause.getMarkers(), p);
        visit(dataOccursClause.getOccurs(), p);
        visit(dataOccursClause.getName(), p);
        visit(dataOccursClause.getDataOccursTo(), p);
        visit(dataOccursClause.getTimes(), p);
        visit(dataOccursClause.getDataOccursDepending(), p);
        visit(dataOccursClause.getSortIndexed(), p);
        return dataOccursClause;
    }

    public Cobol visitDataOccursDepending(Cobol.DataOccursDepending dataOccursDepending, PrintOutputCapture<P> p) {
        visitSpace(dataOccursDepending.getPrefix(), Space.Location.DATA_OCCURS_DEPENDING_PREFIX, p);
        visitMarkers(dataOccursDepending.getMarkers(), p);
        visit(dataOccursDepending.getWords(), p);
        visit(dataOccursDepending.getQualifiedDataName(), p);
        return dataOccursDepending;
    }

    public Cobol visitDataOccursIndexed(Cobol.DataOccursIndexed dataOccursIndexed, PrintOutputCapture<P> p) {
        visitSpace(dataOccursIndexed.getPrefix(), Space.Location.DATA_OCCURS_INDEXED_PREFIX, p);
        visitMarkers(dataOccursIndexed.getMarkers(), p);
        visit(dataOccursIndexed.getWords(), p);
        visit(dataOccursIndexed.getIndexNames(), p);
        return dataOccursIndexed;
    }

    public Cobol visitDataOccursSort(Cobol.DataOccursSort dataOccursSort, PrintOutputCapture<P> p) {
        visitSpace(dataOccursSort.getPrefix(), Space.Location.DATA_OCCURS_SORT_PREFIX, p);
        visitMarkers(dataOccursSort.getMarkers(), p);
        visit(dataOccursSort.getWords(), p);
        visit(dataOccursSort.getQualifiedDataNames(), p);
        return dataOccursSort;
    }

    public Cobol visitDataOccursTo(Cobol.DataOccursTo dataOccursTo, PrintOutputCapture<P> p) {
        visitSpace(dataOccursTo.getPrefix(), Space.Location.DATA_OCCURS_TO_PREFIX, p);
        visitMarkers(dataOccursTo.getMarkers(), p);
        visit(dataOccursTo.getTo(), p);
        visit(dataOccursTo.getIntegerLiteral(), p);
        return dataOccursTo;
    }

    public Cobol visitDataPictureClause(Cobol.DataPictureClause dataPictureClause, PrintOutputCapture<P> p) {
        visitSpace(dataPictureClause.getPrefix(), Space.Location.DATA_PICTURE_CLAUSE_PREFIX, p);
        visitMarkers(dataPictureClause.getMarkers(), p);
        visit(dataPictureClause.getWords(), p);
        visit(dataPictureClause.getPictures(), p);
        return dataPictureClause;
    }

    public Cobol visitDataReceivedByClause(Cobol.DataReceivedByClause dataReceivedByClause, PrintOutputCapture<P> p) {
        visitSpace(dataReceivedByClause.getPrefix(), Space.Location.DATA_RECEIVED_BY_CLAUSE_PREFIX, p);
        visitMarkers(dataReceivedByClause.getMarkers(), p);
        visit(dataReceivedByClause.getWords(), p);
        return dataReceivedByClause;
    }

    public Cobol visitDataRecordAreaClause(Cobol.DataRecordAreaClause dataRecordAreaClause, PrintOutputCapture<P> p) {
        visitSpace(dataRecordAreaClause.getPrefix(), Space.Location.DATA_RECORD_AREA_CLAUSE_PREFIX, p);
        visitMarkers(dataRecordAreaClause.getMarkers(), p);
        visit(dataRecordAreaClause.getWords(), p);
        return dataRecordAreaClause;
    }

    public Cobol visitDataRecordsClause(Cobol.DataRecordsClause dataRecordsClause, PrintOutputCapture<P> p) {
        visitSpace(dataRecordsClause.getPrefix(), Space.Location.DATA_RECORDS_CLAUSE_PREFIX, p);
        visitMarkers(dataRecordsClause.getMarkers(), p);
        visit(dataRecordsClause.getWords(), p);
        visit(dataRecordsClause.getDataName(), p);
        return dataRecordsClause;
    }

    public Cobol visitDataRedefinesClause(Cobol.DataRedefinesClause dataRedefinesClause, PrintOutputCapture<P> p) {
        visitSpace(dataRedefinesClause.getPrefix(), Space.Location.DATA_REDEFINES_CLAUSE_PREFIX, p);
        visitMarkers(dataRedefinesClause.getMarkers(), p);
        visit(dataRedefinesClause.getRedefines(), p);
        visit(dataRedefinesClause.getDataName(), p);
        return dataRedefinesClause;
    }

    public Cobol visitDataRenamesClause(Cobol.DataRenamesClause dataRenamesClause, PrintOutputCapture<P> p) {
        visitSpace(dataRenamesClause.getPrefix(), Space.Location.DATA_RENAMES_CLAUSE_PREFIX, p);
        visitMarkers(dataRenamesClause.getMarkers(), p);
        visit(dataRenamesClause.getRenames(), p);
        visit(dataRenamesClause.getFromName(), p);
        visit(dataRenamesClause.getThrough(), p);
        visit(dataRenamesClause.getToName(), p);
        return dataRenamesClause;
    }

    public Cobol visitDataSignClause(Cobol.DataSignClause dataSignClause, PrintOutputCapture<P> p) {
        visitSpace(dataSignClause.getPrefix(), Space.Location.DATA_SIGN_CLAUSE_PREFIX, p);
        visitMarkers(dataSignClause.getMarkers(), p);
        visit(dataSignClause.getWords(), p);
        return dataSignClause;
    }

    public Cobol visitDataSynchronizedClause(Cobol.DataSynchronizedClause dataSynchronizedClause, PrintOutputCapture<P> p) {
        visitSpace(dataSynchronizedClause.getPrefix(), Space.Location.DATA_SYNCHRONIZED_CLAUSE_PREFIX, p);
        visitMarkers(dataSynchronizedClause.getMarkers(), p);
        visit(dataSynchronizedClause.getWords(), p);
        return dataSynchronizedClause;
    }

    public Cobol visitDataThreadLocalClause(Cobol.DataThreadLocalClause dataThreadLocalClause, PrintOutputCapture<P> p) {
        visitSpace(dataThreadLocalClause.getPrefix(), Space.Location.DATA_THREAD_LOCAL_CLAUSE_PREFIX, p);
        visitMarkers(dataThreadLocalClause.getMarkers(), p);
        visit(dataThreadLocalClause.getWords(), p);
        return dataThreadLocalClause;
    }

    public Cobol visitDataTypeClause(Cobol.DataTypeClause dataTypeClause, PrintOutputCapture<P> p) {
        visitSpace(dataTypeClause.getPrefix(), Space.Location.DATA_TYPE_CLAUSE_PREFIX, p);
        visitMarkers(dataTypeClause.getMarkers(), p);
        visit(dataTypeClause.getWords(), p);
        visit(dataTypeClause.getParenthesized(), p);
        return dataTypeClause;
    }

    public Cobol visitDataTypeDefClause(Cobol.DataTypeDefClause dataTypeDefClause, PrintOutputCapture<P> p) {
        visitSpace(dataTypeDefClause.getPrefix(), Space.Location.DATA_TYPE_DEF_CLAUSE_PREFIX, p);
        visitMarkers(dataTypeDefClause.getMarkers(), p);
        visit(dataTypeDefClause.getWords(), p);
        return dataTypeDefClause;
    }

    public Cobol visitDataUsageClause(Cobol.DataUsageClause dataUsageClause, PrintOutputCapture<P> p) {
        visitSpace(dataUsageClause.getPrefix(), Space.Location.DATA_USAGE_CLAUSE_PREFIX, p);
        visitMarkers(dataUsageClause.getMarkers(), p);
        visit(dataUsageClause.getWords(), p);
        return dataUsageClause;
    }

    public Cobol visitDataUsingClause(Cobol.DataUsingClause dataUsingClause, PrintOutputCapture<P> p) {
        visitSpace(dataUsingClause.getPrefix(), Space.Location.DATA_USING_CLAUSE_PREFIX, p);
        visitMarkers(dataUsingClause.getMarkers(), p);
        visit(dataUsingClause.getWords(), p);
        return dataUsingClause;
    }

    public Cobol visitDataValueClause(Cobol.DataValueClause dataValueClause, PrintOutputCapture<P> p) {
        visitSpace(dataValueClause.getPrefix(), Space.Location.DATA_VALUE_CLAUSE_PREFIX, p);
        visitMarkers(dataValueClause.getMarkers(), p);
        visit(dataValueClause.getWords(), p);
        visit(dataValueClause.getCobols(), p);
        return dataValueClause;
    }

    public Cobol visitDataValueInterval(Cobol.DataValueInterval dataValueInterval, PrintOutputCapture<P> p) {
        visitSpace(dataValueInterval.getPrefix(), Space.Location.DATA_VALUE_INTERVAL_PREFIX, p);
        visitMarkers(dataValueInterval.getMarkers(), p);
        visit(dataValueInterval.getFrom(), p);
        visit(dataValueInterval.getTo(), p);
        return dataValueInterval;
    }

    public Cobol visitDataValueIntervalTo(Cobol.DataValueIntervalTo dataValueIntervalTo, PrintOutputCapture<P> p) {
        visitSpace(dataValueIntervalTo.getPrefix(), Space.Location.DATA_VALUE_INTERVAL_TO_PREFIX, p);
        visitMarkers(dataValueIntervalTo.getMarkers(), p);
        visit(dataValueIntervalTo.getThrough(), p);
        visit(dataValueIntervalTo.getLiteral(), p);
        return dataValueIntervalTo;
    }

    public Cobol visitDataWithLowerBoundsClause(Cobol.DataWithLowerBoundsClause dataWithLowerBoundsClause, PrintOutputCapture<P> p) {
        visitSpace(dataWithLowerBoundsClause.getPrefix(), Space.Location.DATA_WITH_LOWER_BOUNDS_CLAUSE_PREFIX, p);
        visitMarkers(dataWithLowerBoundsClause.getMarkers(), p);
        visit(dataWithLowerBoundsClause.getWords(), p);
        return dataWithLowerBoundsClause;
    }

    public Cobol visitDecimalPointClause(Cobol.DecimalPointClause decimalPointClause, PrintOutputCapture<P> p) {
        visitSpace(decimalPointClause.getPrefix(), Space.Location.DECIMAL_POINT_CLAUSE_PREFIX, p);
        visitMarkers(decimalPointClause.getMarkers(), p);
        visit(decimalPointClause.getWords(), p);
        return decimalPointClause;
    }

    public Cobol visitDefaultComputationalSignClause(Cobol.DefaultComputationalSignClause defaultComputationalSignClause, PrintOutputCapture<P> p) {
        visitSpace(defaultComputationalSignClause.getPrefix(), Space.Location.DEFAULT_COMPUTATIONAL_SIGN_CLAUSE_PREFIX, p);
        visitMarkers(defaultComputationalSignClause.getMarkers(), p);
        visit(defaultComputationalSignClause.getWords(), p);
        return defaultComputationalSignClause;
    }

    public Cobol visitDefaultDisplaySignClause(Cobol.DefaultDisplaySignClause defaultDisplaySignClause, PrintOutputCapture<P> p) {
        visitSpace(defaultDisplaySignClause.getPrefix(), Space.Location.DEFAULT_DISPLAY_SIGN_CLAUSE_PREFIX, p);
        visitMarkers(defaultDisplaySignClause.getMarkers(), p);
        visit(defaultDisplaySignClause.getWords(), p);
        return defaultDisplaySignClause;
    }

    public Cobol visitDelete(Cobol.Delete delete, PrintOutputCapture<P> p) {
        visitSpace(delete.getPrefix(), Space.Location.DELETE_PREFIX, p);
        visitMarkers(delete.getMarkers(), p);
        visit(delete.getDelete(), p);
        visit(delete.getFileName(), p);
        visit(delete.getRecord(), p);
        visit(delete.getInvalidKey(), p);
        visit(delete.getNotInvalidKey(), p);
        visit(delete.getEndDelete(), p);
        return delete;
    }

    public Cobol visitDestinationCountClause(Cobol.DestinationCountClause destinationCountClause, PrintOutputCapture<P> p) {
        visitSpace(destinationCountClause.getPrefix(), Space.Location.DESTINATION_COUNT_CLAUSE_PREFIX, p);
        visitMarkers(destinationCountClause.getMarkers(), p);
        visit(destinationCountClause.getWords(), p);
        visit(destinationCountClause.getDataDescName(), p);
        return destinationCountClause;
    }

    public Cobol visitDestinationTableClause(Cobol.DestinationTableClause destinationTableClause, PrintOutputCapture<P> p) {
        visitSpace(destinationTableClause.getPrefix(), Space.Location.DESTINATION_TABLE_CLAUSE_PREFIX, p);
        visitMarkers(destinationTableClause.getMarkers(), p);
        visit(destinationTableClause.getFirstWords(), p);
        visit(destinationTableClause.getIntegerLiteral(), p);
        visit(destinationTableClause.getSecondWords(), p);
        visit(destinationTableClause.getIndexNames(), p);
        return destinationTableClause;
    }

    public Cobol visitDisable(Cobol.Disable disable, PrintOutputCapture<P> p) {
        visitSpace(disable.getPrefix(), Space.Location.DISABLE_PREFIX, p);
        visitMarkers(disable.getMarkers(), p);
        visit(disable.getDisable(), p);
        visit(disable.getTypes(), p);
        visit(disable.getCdName(), p);
        visit(disable.getWith(), p);
        visit(disable.getKey(), p);
        visit(disable.getKeyName(), p);
        return disable;
    }

    public Cobol visitDisplay(Cobol.Display display, PrintOutputCapture<P> p) {
        visitSpace(display.getPrefix(), Space.Location.DISPLAY_PREFIX, p);
        visitMarkers(display.getMarkers(), p);
        visit(display.getDisplay(), p);
        visit(display.getOperands(), p);
        visit(display.getDisplayAt(), p);
        visit(display.getDisplayUpon(), p);
        visit(display.getDisplayWith(), p);
        visit(display.getOnExceptionClause(), p);
        visit(display.getNotOnExceptionClause(), p);
        visit(display.getEndDisplay(), p);
        return display;
    }

    public Cobol visitDisplayAt(Cobol.DisplayAt displayAt, PrintOutputCapture<P> p) {
        visitSpace(displayAt.getPrefix(), Space.Location.DISPLAY_AT_PREFIX, p);
        visitMarkers(displayAt.getMarkers(), p);
        visit(displayAt.getAt(), p);
        visit(displayAt.getName(), p);
        return displayAt;
    }

    public Cobol visitDisplayUpon(Cobol.DisplayUpon displayUpon, PrintOutputCapture<P> p) {
        visitSpace(displayUpon.getPrefix(), Space.Location.DISPLAY_UPON_PREFIX, p);
        visitMarkers(displayUpon.getMarkers(), p);
        visit(displayUpon.getUpon(), p);
        visit(displayUpon.getName(), p);
        return displayUpon;
    }

    public Cobol visitDivide(Cobol.Divide divide, PrintOutputCapture<P> p) {
        visitSpace(divide.getPrefix(), Space.Location.DIVIDE_PREFIX, p);
        visitMarkers(divide.getMarkers(), p);
        visit(divide.getDivide(), p);
        visit(divide.getName(), p);
        visit(divide.getAction(), p);
        visit(divide.getDivideRemainder(), p);
        visit(divide.getOnSizeErrorPhrase(), p);
        visit(divide.getNotOnSizeErrorPhrase(), p);
        visit(divide.getEndDivide(), p);
        return divide;
    }

    public Cobol visitDivideGiving(Cobol.DivideGiving divideGiving, PrintOutputCapture<P> p) {
        visitSpace(divideGiving.getPrefix(), Space.Location.DIVIDE_GIVING_PREFIX, p);
        visitMarkers(divideGiving.getMarkers(), p);
        visit(divideGiving.getWord(), p);
        visit(divideGiving.getName(), p);
        visit(divideGiving.getDivideGivingPhrase(), p);
        return divideGiving;
    }

    public Cobol visitDivideGivingPhrase(Cobol.DivideGivingPhrase divideGivingPhrase, PrintOutputCapture<P> p) {
        visitSpace(divideGivingPhrase.getPrefix(), Space.Location.DIVIDE_GIVING_PHRASE_PREFIX, p);
        visitMarkers(divideGivingPhrase.getMarkers(), p);
        visit(divideGivingPhrase.getGiving(), p);
        visit(divideGivingPhrase.getRoundables(), p);
        return divideGivingPhrase;
    }

    public Cobol visitDivideInto(Cobol.DivideInto divideInto, PrintOutputCapture<P> p) {
        visitSpace(divideInto.getPrefix(), Space.Location.DIVIDE_INTO_PREFIX, p);
        visitMarkers(divideInto.getMarkers(), p);
        visit(divideInto.getInto(), p);
        visit(divideInto.getRoundables(), p);
        return divideInto;
    }

    public Cobol visitDivideRemainder(Cobol.DivideRemainder divideRemainder, PrintOutputCapture<P> p) {
        visitSpace(divideRemainder.getPrefix(), Space.Location.DIVIDE_REMAINDER_PREFIX, p);
        visitMarkers(divideRemainder.getMarkers(), p);
        visit(divideRemainder.getRemainder(), p);
        visit(divideRemainder.getName(), p);
        return divideRemainder;
    }

    public Cobol visitEnable(Cobol.Enable enable, PrintOutputCapture<P> p) {
        visitSpace(enable.getPrefix(), Space.Location.ENABLE_PREFIX, p);
        visitMarkers(enable.getMarkers(), p);
        visit(enable.getEnable(), p);
        visit(enable.getTypes(), p);
        visit(enable.getCdName(), p);
        visit(enable.getWith(), p);
        visit(enable.getKey(), p);
        visit(enable.getKeyName(), p);
        return enable;
    }

    public Cobol visitEndKeyClause(Cobol.EndKeyClause endKeyClause, PrintOutputCapture<P> p) {
        visitSpace(endKeyClause.getPrefix(), Space.Location.END_KEY_CLAUSE_PREFIX, p);
        visitMarkers(endKeyClause.getMarkers(), p);
        visit(endKeyClause.getWords(), p);
        visit(endKeyClause.getName(), p);
        return endKeyClause;
    }

    public Cobol visitEndProgram(Cobol.EndProgram endProgram, PrintOutputCapture<P> p) {
        visitSpace(endProgram.getPrefix(), Space.Location.END_PROGRAM_PREFIX, p);
        visitMarkers(endProgram.getMarkers(), p);
        visit(endProgram.getWords(), p);
        visit(endProgram.getProgramName(), p);
        visit(endProgram.getDot(), p);
        return endProgram;
    }

    @Override
    public Cobol.Entry visitEntry(Cobol.Entry entry, PrintOutputCapture<P> p) {
        visitSpace(entry.getPrefix(), Space.Location.ENTRY_PREFIX, p);
        visitMarkers(entry.getMarkers(), p);
        visit(entry.getEntry(), p);
        visit(entry.getLiteral(), p);
        visit(entry.getUsing(), p);
        visit(entry.getIdentifiers(), p);
        return entry;
    }

    public Cobol visitEnvironmentDivision(Cobol.EnvironmentDivision environmentDivision, PrintOutputCapture<P> p) {
        visitSpace(environmentDivision.getPrefix(), Space.Location.ENVIRONMENT_DIVISION_PREFIX, p);
        visitMarkers(environmentDivision.getMarkers(), p);
        visit(environmentDivision.getWords(), p);
        visit(environmentDivision.getDot(), p);
        visit(environmentDivision.getBody(), p);
        return environmentDivision;
    }

    public Cobol visitEvaluate(Cobol.Evaluate evaluate, PrintOutputCapture<P> p) {
        visitSpace(evaluate.getPrefix(), Space.Location.EVALUATE_PREFIX, p);
        visitMarkers(evaluate.getMarkers(), p);
        visit(evaluate.getEvaluate(), p);
        visit(evaluate.getSelect(), p);
        visit(evaluate.getAlsoSelect(), p);
        visit(evaluate.getWhenPhrase(), p);
        visit(evaluate.getWhenOther(), p);
        visit(evaluate.getEndPhrase(), p);
        return evaluate;
    }

    public Cobol visitEvaluateAlso(Cobol.EvaluateAlso evaluateAlso, PrintOutputCapture<P> p) {
        visitSpace(evaluateAlso.getPrefix(), Space.Location.EVALUATE_ALSO_PREFIX, p);
        visitMarkers(evaluateAlso.getMarkers(), p);
        visit(evaluateAlso.getAlso(), p);
        visit(evaluateAlso.getSelect(), p);
        return evaluateAlso;
    }

    public Cobol visitEnvironmentSwitchNameClause(Cobol.EnvironmentSwitchNameClause environmentSwitchNameClause, PrintOutputCapture<P> p) {
        visitSpace(environmentSwitchNameClause.getPrefix(), Space.Location.ENVIRONMENT_SWITCH_NAME_CLAUSE_PREFIX, p);
        visitMarkers(environmentSwitchNameClause.getMarkers(), p);
        visit(environmentSwitchNameClause.getEnvironmentName(), p);
        visit(environmentSwitchNameClause.getIs(), p);
        visit(environmentSwitchNameClause.getMnemonicName(), p);
        visit(environmentSwitchNameClause.getEnvironmentSwitchNameSpecialNamesStatusPhrase(), p);
        return environmentSwitchNameClause;
    }

    public Cobol visitEnvironmentSwitchNameSpecialNamesStatusPhrase(Cobol.EnvironmentSwitchNameSpecialNamesStatusPhrase environmentSwitchNameSpecialNamesStatusPhrase, PrintOutputCapture<P> p) {
        visitSpace(environmentSwitchNameSpecialNamesStatusPhrase.getPrefix(), Space.Location.ENVIRONMENT_SWITCH_NAME_SPECIAL_NAMES_STATUS_PHRASE_PREFIX, p);
        visitMarkers(environmentSwitchNameSpecialNamesStatusPhrase.getMarkers(), p);
        visit(environmentSwitchNameSpecialNamesStatusPhrase.getCobols(), p);
        return environmentSwitchNameSpecialNamesStatusPhrase;
    }

    public Cobol visitErrorKeyClause(Cobol.ErrorKeyClause errorKeyClause, PrintOutputCapture<P> p) {
        visitSpace(errorKeyClause.getPrefix(), Space.Location.ERROR_KEY_CLAUSE_PREFIX, p);
        visitMarkers(errorKeyClause.getMarkers(), p);
        visit(errorKeyClause.getWords(), p);
        visit(errorKeyClause.getName(), p);
        return errorKeyClause;
    }

    public Cobol visitEvaluateAlsoCondition(Cobol.EvaluateAlsoCondition evaluateAlsoCondition, PrintOutputCapture<P> p) {
        visitSpace(evaluateAlsoCondition.getPrefix(), Space.Location.EVALUATE_ALSO_CONDITION_PREFIX, p);
        visitMarkers(evaluateAlsoCondition.getMarkers(), p);
        visit(evaluateAlsoCondition.getAlso(), p);
        visit(evaluateAlsoCondition.getCondition(), p);
        return evaluateAlsoCondition;
    }

    public Cobol visitEvaluateCondition(Cobol.EvaluateCondition evaluateCondition, PrintOutputCapture<P> p) {
        visitSpace(evaluateCondition.getPrefix(), Space.Location.EVALUATE_CONDITION_PREFIX, p);
        visitMarkers(evaluateCondition.getMarkers(), p);
        visit(evaluateCondition.getWords(), p);
        visit(evaluateCondition.getCondition(), p);
        visit(evaluateCondition.getEvaluateThrough(), p);
        return evaluateCondition;
    }

    public Cobol visitEvaluateThrough(Cobol.EvaluateThrough evaluateThrough, PrintOutputCapture<P> p) {
        visitSpace(evaluateThrough.getPrefix(), Space.Location.EVALUATE_THROUGH_PREFIX, p);
        visitMarkers(evaluateThrough.getMarkers(), p);
        visit(evaluateThrough.getThrough(), p);
        visit(evaluateThrough.getValue(), p);
        return evaluateThrough;
    }

    public Cobol visitEvaluateValueThrough(Cobol.EvaluateValueThrough evaluateValueThrough, PrintOutputCapture<P> p) {
        visitSpace(evaluateValueThrough.getPrefix(), Space.Location.EVALUATE_VALUE_THROUGH_PREFIX, p);
        visitMarkers(evaluateValueThrough.getMarkers(), p);
        visit(evaluateValueThrough.getNot(), p);
        visit(evaluateValueThrough.getValue(), p);
        visit(evaluateValueThrough.getEvaluateThrough(), p);
        return evaluateValueThrough;
    }

    public Cobol visitEvaluateWhen(Cobol.EvaluateWhen evaluateWhen, PrintOutputCapture<P> p) {
        visitSpace(evaluateWhen.getPrefix(), Space.Location.EVALUATE_WHEN_PREFIX, p);
        visitMarkers(evaluateWhen.getMarkers(), p);
        visit(evaluateWhen.getWhen(), p);
        visit(evaluateWhen.getCondition(), p);
        visit(evaluateWhen.getAlsoCondition(), p);
        return evaluateWhen;
    }

    public Cobol visitEvaluateWhenPhrase(Cobol.EvaluateWhenPhrase evaluateWhenPhrase, PrintOutputCapture<P> p) {
        visitSpace(evaluateWhenPhrase.getPrefix(), Space.Location.EVALUATE_WHEN_PHRASE_PREFIX, p);
        visitMarkers(evaluateWhenPhrase.getMarkers(), p);
        visit(evaluateWhenPhrase.getWhens(), p);
        visit(evaluateWhenPhrase.getStatements(), p);
        return evaluateWhenPhrase;
    }

    public Cobol visitExecCicsStatement(Cobol.ExecCicsStatement execCicsStatement, PrintOutputCapture<P> p) {
        visitSpace(execCicsStatement.getPrefix(), Space.Location.EXEC_CICS_STATEMENT_PREFIX, p);
        visitMarkers(execCicsStatement.getMarkers(), p);
        visit(execCicsStatement.getExecCicsLines(), p);
        return execCicsStatement;
    }

    public Cobol visitExecSqlImsStatement(Cobol.ExecSqlImsStatement execSqlImsStatement, PrintOutputCapture<P> p) {
        visitSpace(execSqlImsStatement.getPrefix(), Space.Location.EXEC_SQL_IMS_STATEMENT_PREFIX, p);
        visitMarkers(execSqlImsStatement.getMarkers(), p);
        visit(execSqlImsStatement.getExecSqlLmsLines(), p);
        return execSqlImsStatement;
    }

    public Cobol visitExecSqlStatement(Cobol.ExecSqlStatement execSqlStatement, PrintOutputCapture<P> p) {
        visitSpace(execSqlStatement.getPrefix(), Space.Location.EXEC_SQL_STATEMENT_PREFIX, p);
        visitMarkers(execSqlStatement.getMarkers(), p);
        visit(execSqlStatement.getExecSqlLines(), p);
        return execSqlStatement;
    }

    public Cobol visitExhibit(Cobol.Exhibit exhibit, PrintOutputCapture<P> p) {
        visitSpace(exhibit.getPrefix(), Space.Location.EXHIBIT_PREFIX, p);
        visitMarkers(exhibit.getMarkers(), p);
        visit(exhibit.getWords(), p);
        visit(exhibit.getOperands(), p);
        return exhibit;
    }

    public Cobol visitExit(Cobol.Exit exit, PrintOutputCapture<P> p) {
        visitSpace(exit.getPrefix(), Space.Location.EXIT_PREFIX, p);
        visitMarkers(exit.getMarkers(), p);
        visit(exit.getWords(), p);
        return exit;
    }

    public Cobol visitExternalClause(Cobol.ExternalClause externalClause, PrintOutputCapture<P> p) {
        visitSpace(externalClause.getPrefix(), Space.Location.EXTERNAL_CLAUSE_PREFIX, p);
        visitMarkers(externalClause.getMarkers(), p);
        visit(externalClause.getWords(), p);
        return externalClause;
    }

    public Cobol visitFigurativeConstant(Cobol.FigurativeConstant figurativeConstant, PrintOutputCapture<P> p) {
        visitSpace(figurativeConstant.getPrefix(), Space.Location.FIGURATIVE_CONSTANT_PREFIX, p);
        visitMarkers(figurativeConstant.getMarkers(), p);
        visit(figurativeConstant.getWord(), p);
        visit(figurativeConstant.getLiteral(), p);
        return figurativeConstant;
    }

    public Cobol visitFileControlEntry(Cobol.FileControlEntry fileControlEntry, PrintOutputCapture<P> p) {
        visitSpace(fileControlEntry.getPrefix(), Space.Location.FILE_CONTROL_ENTRY_PREFIX, p);
        visitMarkers(fileControlEntry.getMarkers(), p);
        visit(fileControlEntry.getSelectClause(), p);
        visit(fileControlEntry.getControlClauses(), p);
        return fileControlEntry;
    }

    public Cobol visitFileControlParagraph(Cobol.FileControlParagraph fileControlParagraph, PrintOutputCapture<P> p) {
        visitSpace(fileControlParagraph.getPrefix(), Space.Location.FILE_CONTROL_PARAGRAPH_PREFIX, p);
        visitMarkers(fileControlParagraph.getMarkers(), p);
        visit(fileControlParagraph.getFileControl(), p);
        visit(fileControlParagraph.getControlEntries(), p);
        return fileControlParagraph;
    }

    public Cobol visitFileDescriptionEntry(Cobol.FileDescriptionEntry fileDescriptionEntry, PrintOutputCapture<P> p) {
        visitSpace(fileDescriptionEntry.getPrefix(), Space.Location.FILE_DESCRIPTION_ENTRY_PREFIX, p);
        visitMarkers(fileDescriptionEntry.getMarkers(), p);
        visit(fileDescriptionEntry.getWord(), p);
        visit(fileDescriptionEntry.getName(), p);
        visit(fileDescriptionEntry.getClauses(), p);
        visit(fileDescriptionEntry.getDataDescriptions(), p);
        return fileDescriptionEntry;
    }

    public Cobol visitFileSection(Cobol.FileSection fileSection, PrintOutputCapture<P> p) {
        visitSpace(fileSection.getPrefix(), Space.Location.FILE_SECTION_PREFIX, p);
        visitMarkers(fileSection.getMarkers(), p);
        visit(fileSection.getWords(), p);
        visit(fileSection.getDot(), p);
        visit(fileSection.getFileDescriptionEntry(), p);
        return fileSection;
    }

    public Cobol visitFileStatusClause(Cobol.FileStatusClause fileStatusClause, PrintOutputCapture<P> p) {
        visitSpace(fileStatusClause.getPrefix(), Space.Location.FILE_STATUS_CLAUSE_PREFIX, p);
        visitMarkers(fileStatusClause.getMarkers(), p);
        visit(fileStatusClause.getWords(), p);
        visit(fileStatusClause.getQualifiedDataNames(), p);
        return fileStatusClause;
    }

    public Cobol visitFunctionCall(Cobol.FunctionCall functionCall, PrintOutputCapture<P> p) {
        visitSpace(functionCall.getPrefix(), Space.Location.FUNCTION_CALL_PREFIX, p);
        visitMarkers(functionCall.getMarkers(), p);
        visit(functionCall.getFunction(), p);
        visit(functionCall.getFunctionName(), p);
        visit(functionCall.getArguments(), p);
        visit(functionCall.getReferenceModifier(), p);
        return functionCall;
    }

    public Cobol visitGenerate(Cobol.Generate generate, PrintOutputCapture<P> p) {
        visitSpace(generate.getPrefix(), Space.Location.GENERATE_PREFIX, p);
        visitMarkers(generate.getMarkers(), p);
        visit(generate.getGenerate(), p);
        visit(generate.getReportName(), p);
        return generate;
    }

    public Cobol visitGlobalClause(Cobol.GlobalClause globalClause, PrintOutputCapture<P> p) {
        visitSpace(globalClause.getPrefix(), Space.Location.GLOBAL_CLAUSE_PREFIX, p);
        visitMarkers(globalClause.getMarkers(), p);
        visit(globalClause.getWords(), p);
        return globalClause;
    }

    public Cobol visitGoBack(Cobol.GoBack goBack, PrintOutputCapture<P> p) {
        visitSpace(goBack.getPrefix(), Space.Location.GO_BACK_PREFIX, p);
        visitMarkers(goBack.getMarkers(), p);
        visit(goBack.getGoBack(), p);
        return goBack;
    }

    public Cobol visitGoTo(Cobol.GoTo _goTo, PrintOutputCapture<P> p) {
        visitSpace(_goTo.getPrefix(), Space.Location.GO_TO_PREFIX, p);
        visitMarkers(_goTo.getMarkers(), p);
        visit(_goTo.getWords(), p);
        visit(_goTo.getStatement(), p);
        return _goTo;
    }

    public Cobol visitGoToDependingOnStatement(Cobol.GoToDependingOnStatement goToDependingOnStatement, PrintOutputCapture<P> p) {
        visitSpace(goToDependingOnStatement.getPrefix(), Space.Location.GO_TO_DEPENDING_ON_STATEMENT_PREFIX, p);
        visitMarkers(goToDependingOnStatement.getMarkers(), p);
        visit(goToDependingOnStatement.getProcedureNames(), p);
        visit(goToDependingOnStatement.getWords(), p);
        visit(goToDependingOnStatement.getIdentifier(), p);
        return goToDependingOnStatement;
    }

    public Cobol visitIdentificationDivision(Cobol.IdentificationDivision identificationDivision, PrintOutputCapture<P> p) {
        visitSpace(identificationDivision.getPrefix(), Space.Location.IDENTIFICATION_DIVISION_PREFIX, p);
        visitMarkers(identificationDivision.getMarkers(), p);
        visit(identificationDivision.getWords(), p);
        visit(identificationDivision.getProgramIdParagraph(), p);
        visit(identificationDivision.getParagraphs(), p);
        return identificationDivision;
    }

    public Cobol visitIdentificationDivisionParagraph(Cobol.IdentificationDivisionParagraph identificationDivisionParagraph, PrintOutputCapture<P> p) {
        visitSpace(identificationDivisionParagraph.getPrefix(), Space.Location.IDENTIFICATION_DIVISION_PARAGRAPH_PREFIX, p);
        visitMarkers(identificationDivisionParagraph.getMarkers(), p);
        visit(identificationDivisionParagraph.getWord(), p);
        visit(identificationDivisionParagraph.getDot(), p);
        visit(identificationDivisionParagraph.getCommentEntry(), p);
        visit(identificationDivisionParagraph.getWords(), p);
        visit(identificationDivisionParagraph.getDot2(), p);
        return identificationDivisionParagraph;
    }

    public Cobol visitIf(Cobol.If _if, PrintOutputCapture<P> p) {
        visitSpace(_if.getPrefix(), Space.Location.IF_PREFIX, p);
        visitMarkers(_if.getMarkers(), p);
        visit(_if.getWord(), p);
        visit(_if.getCondition(), p);
        visit(_if.getIfThen(), p);
        visit(_if.getIfElse(), p);
        visit(_if.getEndIf(), p);
        return _if;
    }

    public Cobol visitIfElse(Cobol.IfElse ifElse, PrintOutputCapture<P> p) {
        visitSpace(ifElse.getPrefix(), Space.Location.IF_ELSE_PREFIX, p);
        visitMarkers(ifElse.getMarkers(), p);
        visit(ifElse.getWord(), p);
        visit(ifElse.getNextSentences(), p);
        visit(ifElse.getStatements(), p);
        return ifElse;
    }

    public Cobol visitIfThen(Cobol.IfThen ifThen, PrintOutputCapture<P> p) {
        visitSpace(ifThen.getPrefix(), Space.Location.IF_THEN_PREFIX, p);
        visitMarkers(ifThen.getMarkers(), p);
        visit(ifThen.getWord(), p);
        visit(ifThen.getNextSentences(), p);
        visit(ifThen.getStatements(), p);
        return ifThen;
    }

    public Cobol visitInData(Cobol.InData inData, PrintOutputCapture<P> p) {
        visitSpace(inData.getPrefix(), Space.Location.IN_DATA_PREFIX, p);
        visitMarkers(inData.getMarkers(), p);
        visit(inData.getWord(), p);
        visit(inData.getName(), p);
        return inData;
    }

    public Cobol visitInFile(Cobol.InFile inFile, PrintOutputCapture<P> p) {
        visitSpace(inFile.getPrefix(), Space.Location.IN_FILE_PREFIX, p);
        visitMarkers(inFile.getMarkers(), p);
        visit(inFile.getWord(), p);
        visit(inFile.getName(), p);
        return inFile;
    }

    public Cobol visitInLibrary(Cobol.InLibrary inLibrary, PrintOutputCapture<P> p) {
        visitSpace(inLibrary.getPrefix(), Space.Location.IN_LIBRARY_PREFIX, p);
        visitMarkers(inLibrary.getMarkers(), p);
        visit(inLibrary.getWord(), p);
        visit(inLibrary.getName(), p);
        return inLibrary;
    }

    public Cobol visitInMnemonic(Cobol.InMnemonic inMnemonic, PrintOutputCapture<P> p) {
        visitSpace(inMnemonic.getPrefix(), Space.Location.IN_MNEMONIC_PREFIX, p);
        visitMarkers(inMnemonic.getMarkers(), p);
        visit(inMnemonic.getWord(), p);
        visit(inMnemonic.getName(), p);
        return inMnemonic;
    }

    public Cobol visitInSection(Cobol.InSection inSection, PrintOutputCapture<P> p) {
        visitSpace(inSection.getPrefix(), Space.Location.IN_SECTION_PREFIX, p);
        visitMarkers(inSection.getMarkers(), p);
        visit(inSection.getWord(), p);
        visit(inSection.getName(), p);
        return inSection;
    }

    public Cobol visitInTable(Cobol.InTable inTable, PrintOutputCapture<P> p) {
        visitSpace(inTable.getPrefix(), Space.Location.IN_TABLE_PREFIX, p);
        visitMarkers(inTable.getMarkers(), p);
        visit(inTable.getWord(), p);
        return inTable;
    }

    public Cobol visitInitialize(Cobol.Initialize initialize, PrintOutputCapture<P> p) {
        visitSpace(initialize.getPrefix(), Space.Location.INITIALIZE_PREFIX, p);
        visitMarkers(initialize.getMarkers(), p);
        visit(initialize.getInitialize(), p);
        visit(initialize.getIdentifiers(), p);
        visit(initialize.getInitializeReplacingPhrase(), p);
        return initialize;
    }

    public Cobol visitInitializeReplacingBy(Cobol.InitializeReplacingBy initializeReplacingBy, PrintOutputCapture<P> p) {
        visitSpace(initializeReplacingBy.getPrefix(), Space.Location.INITIALIZE_REPLACING_BY_PREFIX, p);
        visitMarkers(initializeReplacingBy.getMarkers(), p);
        visit(initializeReplacingBy.getWords(), p);
        visit(initializeReplacingBy.getIdentifier(), p);
        return initializeReplacingBy;
    }

    public Cobol visitInitializeReplacingPhrase(Cobol.InitializeReplacingPhrase initializeReplacingPhrase, PrintOutputCapture<P> p) {
        visitSpace(initializeReplacingPhrase.getPrefix(), Space.Location.INITIALIZE_REPLACING_PHRASE_PREFIX, p);
        visitMarkers(initializeReplacingPhrase.getMarkers(), p);
        visit(initializeReplacingPhrase.getReplacing(), p);
        visit(initializeReplacingPhrase.getInitializeReplacingBy(), p);
        return initializeReplacingPhrase;
    }

    public Cobol visitInitiate(Cobol.Initiate initiate, PrintOutputCapture<P> p) {
        visitSpace(initiate.getPrefix(), Space.Location.INITIATE_PREFIX, p);
        visitMarkers(initiate.getMarkers(), p);
        visit(initiate.getInitiate(), p);
        visit(initiate.getReportNames(), p);
        return initiate;
    }

    public Cobol visitInputOutputSection(Cobol.InputOutputSection inputOutputSection, PrintOutputCapture<P> p) {
        visitSpace(inputOutputSection.getPrefix(), Space.Location.INPUT_OUTPUT_SECTION_PREFIX, p);
        visitMarkers(inputOutputSection.getMarkers(), p);
        visit(inputOutputSection.getWords(), p);
        visit(inputOutputSection.getParagraphs(), p);
        return inputOutputSection;
    }

    public Cobol visitInspect(Cobol.Inspect inspect, PrintOutputCapture<P> p) {
        visitSpace(inspect.getPrefix(), Space.Location.INSPECT_PREFIX, p);
        visitMarkers(inspect.getMarkers(), p);
        visit(inspect.getInspect(), p);
        visit(inspect.getIdentifier(), p);
        visit(inspect.getPhrase(), p);
        return inspect;
    }

    public Cobol visitInspectAllLeading(Cobol.InspectAllLeading inspectAllLeading, PrintOutputCapture<P> p) {
        visitSpace(inspectAllLeading.getPrefix(), Space.Location.INSPECT_ALL_LEADING_PREFIX, p);
        visitMarkers(inspectAllLeading.getMarkers(), p);
        visit(inspectAllLeading.getName(), p);
        visit(inspectAllLeading.getInspections(), p);
        return inspectAllLeading;
    }

    public Cobol visitInspectAllLeadings(Cobol.InspectAllLeadings inspectAllLeadings, PrintOutputCapture<P> p) {
        visitSpace(inspectAllLeadings.getPrefix(), Space.Location.INSPECT_ALL_LEADINGS_PREFIX, p);
        visitMarkers(inspectAllLeadings.getMarkers(), p);
        visit(inspectAllLeadings.getWord(), p);
        visit(inspectAllLeadings.getLeadings(), p);
        return inspectAllLeadings;
    }

    public Cobol visitInspectBeforeAfter(Cobol.InspectBeforeAfter inspectBeforeAfter, PrintOutputCapture<P> p) {
        visitSpace(inspectBeforeAfter.getPrefix(), Space.Location.INSPECT_BEFORE_AFTER_PREFIX, p);
        visitMarkers(inspectBeforeAfter.getMarkers(), p);
        visit(inspectBeforeAfter.getWords(), p);
        visit(inspectBeforeAfter.getIdentifier(), p);
        return inspectBeforeAfter;
    }

    public Cobol visitInspectBy(Cobol.InspectBy inspectBy, PrintOutputCapture<P> p) {
        visitSpace(inspectBy.getPrefix(), Space.Location.INSPECT_BY_PREFIX, p);
        visitMarkers(inspectBy.getMarkers(), p);
        visit(inspectBy.getBy(), p);
        visit(inspectBy.getIdentifier(), p);
        return inspectBy;
    }

    public Cobol visitInspectCharacters(Cobol.InspectCharacters inspectCharacters, PrintOutputCapture<P> p) {
        visitSpace(inspectCharacters.getPrefix(), Space.Location.INSPECT_CHARACTERS_PREFIX, p);
        visitMarkers(inspectCharacters.getMarkers(), p);
        visit(inspectCharacters.getCharacter(), p);
        visit(inspectCharacters.getInspections(), p);
        return inspectCharacters;
    }

    public Cobol visitInspectConvertingPhrase(Cobol.InspectConvertingPhrase inspectConvertingPhrase, PrintOutputCapture<P> p) {
        visitSpace(inspectConvertingPhrase.getPrefix(), Space.Location.INSPECT_CONVERTING_PHRASE_PREFIX, p);
        visitMarkers(inspectConvertingPhrase.getMarkers(), p);
        visit(inspectConvertingPhrase.getConverting(), p);
        visit(inspectConvertingPhrase.getIdentifier(), p);
        visit(inspectConvertingPhrase.getInspectTo(), p);
        visit(inspectConvertingPhrase.getInspections(), p);
        return inspectConvertingPhrase;
    }

    public Cobol visitInspectFor(Cobol.InspectFor inspectFor, PrintOutputCapture<P> p) {
        visitSpace(inspectFor.getPrefix(), Space.Location.INSPECT_FOR_PREFIX, p);
        visitMarkers(inspectFor.getMarkers(), p);
        visit(inspectFor.getIdentifier(), p);
        visit(inspectFor.getWord(), p);
        visit(inspectFor.getInspects(), p);
        return inspectFor;
    }

    public Cobol visitInspectReplacingAllLeading(Cobol.InspectReplacingAllLeading inspectReplacingAllLeading, PrintOutputCapture<P> p) {
        visitSpace(inspectReplacingAllLeading.getPrefix(), Space.Location.INSPECT_REPLACING_ALL_LEADING_PREFIX, p);
        visitMarkers(inspectReplacingAllLeading.getMarkers(), p);
        visit(inspectReplacingAllLeading.getIdentifier(), p);
        visit(inspectReplacingAllLeading.getInspectBy(), p);
        visit(inspectReplacingAllLeading.getInspections(), p);
        return inspectReplacingAllLeading;
    }

    public Cobol visitInspectReplacingAllLeadings(Cobol.InspectReplacingAllLeadings inspectReplacingAllLeadings, PrintOutputCapture<P> p) {
        visitSpace(inspectReplacingAllLeadings.getPrefix(), Space.Location.INSPECT_REPLACING_ALL_LEADINGS_PREFIX, p);
        visitMarkers(inspectReplacingAllLeadings.getMarkers(), p);
        visit(inspectReplacingAllLeadings.getWord(), p);
        visit(inspectReplacingAllLeadings.getInspections(), p);
        return inspectReplacingAllLeadings;
    }

    public Cobol visitInspectReplacingCharacters(Cobol.InspectReplacingCharacters inspectReplacingCharacters, PrintOutputCapture<P> p) {
        visitSpace(inspectReplacingCharacters.getPrefix(), Space.Location.INSPECT_REPLACING_CHARACTERS_PREFIX, p);
        visitMarkers(inspectReplacingCharacters.getMarkers(), p);
        visit(inspectReplacingCharacters.getWord(), p);
        visit(inspectReplacingCharacters.getInspectBy(), p);
        visit(inspectReplacingCharacters.getInspections(), p);
        return inspectReplacingCharacters;
    }

    public Cobol visitInspectReplacingPhrase(Cobol.InspectReplacingPhrase inspectReplacingPhrase, PrintOutputCapture<P> p) {
        visitSpace(inspectReplacingPhrase.getPrefix(), Space.Location.INSPECT_REPLACING_PHRASE_PREFIX, p);
        visitMarkers(inspectReplacingPhrase.getMarkers(), p);
        visit(inspectReplacingPhrase.getWord(), p);
        visit(inspectReplacingPhrase.getInspections(), p);
        return inspectReplacingPhrase;
    }

    public Cobol visitInspectTallyingPhrase(Cobol.InspectTallyingPhrase inspectTallyingPhrase, PrintOutputCapture<P> p) {
        visitSpace(inspectTallyingPhrase.getPrefix(), Space.Location.INSPECT_TALLYING_PHRASE_PREFIX, p);
        visitMarkers(inspectTallyingPhrase.getMarkers(), p);
        visit(inspectTallyingPhrase.getTallying(), p);
        visit(inspectTallyingPhrase.getInspectFors(), p);
        return inspectTallyingPhrase;
    }

    public Cobol visitInspectTallyingReplacingPhrase(Cobol.InspectTallyingReplacingPhrase inspectTallyingReplacingPhrase, PrintOutputCapture<P> p) {
        visitSpace(inspectTallyingReplacingPhrase.getPrefix(), Space.Location.INSPECT_TALLYING_REPLACING_PHRASE_PREFIX, p);
        visitMarkers(inspectTallyingReplacingPhrase.getMarkers(), p);
        visit(inspectTallyingReplacingPhrase.getTallying(), p);
        visit(inspectTallyingReplacingPhrase.getInspectFors(), p);
        visit(inspectTallyingReplacingPhrase.getReplacingPhrases(), p);
        return inspectTallyingReplacingPhrase;
    }

    public Cobol visitInspectTo(Cobol.InspectTo inspectTo, PrintOutputCapture<P> p) {
        visitSpace(inspectTo.getPrefix(), Space.Location.INSPECT_TO_PREFIX, p);
        visitMarkers(inspectTo.getMarkers(), p);
        visit(inspectTo.getTo(), p);
        visit(inspectTo.getIdentifier(), p);
        return inspectTo;
    }

    public Cobol visitIoControlParagraph(Cobol.IoControlParagraph ioControlParagraph, PrintOutputCapture<P> p) {
        visitSpace(ioControlParagraph.getPrefix(), Space.Location.IO_CONTROL_PARAGRAPH_PREFIX, p);
        visitMarkers(ioControlParagraph.getMarkers(), p);
        visit(ioControlParagraph.getIOControl(), p);
        visit(ioControlParagraph.getDot(), p);
        visit(ioControlParagraph.getFileName(), p);
        visit(ioControlParagraph.getFileNameDot(), p);
        visit(ioControlParagraph.getClauses(), p);
        visit(ioControlParagraph.getDot2(), p);
        return ioControlParagraph;
    }

    public Cobol visitLabelRecordsClause(Cobol.LabelRecordsClause labelRecordsClause, PrintOutputCapture<P> p) {
        visitSpace(labelRecordsClause.getPrefix(), Space.Location.LABEL_RECORDS_CLAUSE_PREFIX, p);
        visitMarkers(labelRecordsClause.getMarkers(), p);
        visit(labelRecordsClause.getWords(), p);
        visit(labelRecordsClause.getDataNames(), p);
        return labelRecordsClause;
    }

    public Cobol visitLibraryAttributeClauseFormat1(Cobol.LibraryAttributeClauseFormat1 libraryAttributeClauseFormat1, PrintOutputCapture<P> p) {
        visitSpace(libraryAttributeClauseFormat1.getPrefix(), Space.Location.LIBRARY_ATTRIBUTE_CLAUSE_1_PREFIX, p);
        visitMarkers(libraryAttributeClauseFormat1.getMarkers(), p);
        visit(libraryAttributeClauseFormat1.getWords(), p);
        return libraryAttributeClauseFormat1;
    }

    public Cobol visitLibraryAttributeClauseFormat2(Cobol.LibraryAttributeClauseFormat2 libraryAttributeClauseFormat2, PrintOutputCapture<P> p) {
        visitSpace(libraryAttributeClauseFormat2.getPrefix(), Space.Location.LIBRARY_ATTRIBUTE_CLAUSE_2_PREFIX, p);
        visitMarkers(libraryAttributeClauseFormat2.getMarkers(), p);
        visit(libraryAttributeClauseFormat2.getAttribute(), p);
        visit(libraryAttributeClauseFormat2.getLibraryAttributeFunction(), p);
        visit(libraryAttributeClauseFormat2.getWords(), p);
        visit(libraryAttributeClauseFormat2.getLibraryAttributeParameter(), p);
        visit(libraryAttributeClauseFormat2.getLibraryAttributeTitle(), p);
        return libraryAttributeClauseFormat2;
    }

    public Cobol visitLibraryAttributeFunction(Cobol.LibraryAttributeFunction libraryAttributeFunction, PrintOutputCapture<P> p) {
        visitSpace(libraryAttributeFunction.getPrefix(), Space.Location.LIBRARY_ATTRIBUTE_FUNCTION_PREFIX, p);
        visitMarkers(libraryAttributeFunction.getMarkers(), p);
        visit(libraryAttributeFunction.getWords(), p);
        visit(libraryAttributeFunction.getLiteral(), p);
        return libraryAttributeFunction;
    }

    public Cobol visitLibraryAttributeParameter(Cobol.LibraryAttributeParameter libraryAttributeParameter, PrintOutputCapture<P> p) {
        visitSpace(libraryAttributeParameter.getPrefix(), Space.Location.LIBRARY_ATTRIBUTE_PARAMETER_PREFIX, p);
        visitMarkers(libraryAttributeParameter.getMarkers(), p);
        visit(libraryAttributeParameter.getWords(), p);
        visit(libraryAttributeParameter.getLiteral(), p);
        return libraryAttributeParameter;
    }

    public Cobol visitLibraryAttributeTitle(Cobol.LibraryAttributeTitle libraryAttributeTitle, PrintOutputCapture<P> p) {
        visitSpace(libraryAttributeTitle.getPrefix(), Space.Location.LIBRARY_ATTRIBUTE_TITLE_PREFIX, p);
        visitMarkers(libraryAttributeTitle.getMarkers(), p);
        visit(libraryAttributeTitle.getWords(), p);
        visit(libraryAttributeTitle.getLiteral(), p);
        return libraryAttributeTitle;
    }

    public Cobol visitLibraryDescriptionEntryFormat1(Cobol.LibraryDescriptionEntryFormat1 libraryDescriptionEntryFormat1, PrintOutputCapture<P> p) {
        visitSpace(libraryDescriptionEntryFormat1.getPrefix(), Space.Location.LIBRARY_DESCRIPTION_ENTRY_FORMAT_1_PREFIX, p);
        visitMarkers(libraryDescriptionEntryFormat1.getMarkers(), p);
        visit(libraryDescriptionEntryFormat1.getLd(), p);
        visit(libraryDescriptionEntryFormat1.getLibraryName(), p);
        visit(libraryDescriptionEntryFormat1.getExport(), p);
        visit(libraryDescriptionEntryFormat1.getLibraryAttributeClauseFormat1(), p);
        visit(libraryDescriptionEntryFormat1.getLibraryEntryProcedureClauseFormat1(), p);
        return libraryDescriptionEntryFormat1;
    }

    public Cobol visitLibraryDescriptionEntryFormat2(Cobol.LibraryDescriptionEntryFormat2 libraryDescriptionEntryFormat2, PrintOutputCapture<P> p) {
        visitSpace(libraryDescriptionEntryFormat2.getPrefix(), Space.Location.LIBRARY_DESCRIPTION_ENTRY_FORMAT_2_PREFIX, p);
        visitMarkers(libraryDescriptionEntryFormat2.getMarkers(), p);
        visit(libraryDescriptionEntryFormat2.getLb(), p);
        visit(libraryDescriptionEntryFormat2.getLibraryName(), p);
        visit(libraryDescriptionEntryFormat2.getExport(), p);
        visit(libraryDescriptionEntryFormat2.getLibraryIsGlobalClause(), p);
        visit(libraryDescriptionEntryFormat2.getLibraryIsCommonClause(), p);
        visit(libraryDescriptionEntryFormat2.getClauseFormats(), p);
        return libraryDescriptionEntryFormat2;
    }

    public Cobol visitLibraryEntryProcedureClauseFormat1(Cobol.LibraryEntryProcedureClauseFormat1 libraryEntryProcedureClauseFormat1, PrintOutputCapture<P> p) {
        visitSpace(libraryEntryProcedureClauseFormat1.getPrefix(), Space.Location.LIBRARY_ENTRY_PROCEDURE_CLAUSE_FORMAT_1_PREFIX, p);
        visitMarkers(libraryEntryProcedureClauseFormat1.getMarkers(), p);
        visit(libraryEntryProcedureClauseFormat1.getEntryProcedure(), p);
        visit(libraryEntryProcedureClauseFormat1.getProgramName(), p);
        visit(libraryEntryProcedureClauseFormat1.getLibraryEntryProcedureForClause(), p);
        return libraryEntryProcedureClauseFormat1;
    }

    public Cobol visitLibraryEntryProcedureClauseFormat2(Cobol.LibraryEntryProcedureClauseFormat2 libraryEntryProcedureClauseFormat2, PrintOutputCapture<P> p) {
        visitSpace(libraryEntryProcedureClauseFormat2.getPrefix(), Space.Location.LIBRARY_ENTRY_PROCEDURE_CLAUSE_FORMAT_2_PREFIX, p);
        visitMarkers(libraryEntryProcedureClauseFormat2.getMarkers(), p);
        visit(libraryEntryProcedureClauseFormat2.getEntryProcedure(), p);
        visit(libraryEntryProcedureClauseFormat2.getProgramName(), p);
        visit(libraryEntryProcedureClauseFormat2.getLibraryEntryProcedureForClause(), p);
        visit(libraryEntryProcedureClauseFormat2.getLibraryEntryProcedureWithClause(), p);
        visit(libraryEntryProcedureClauseFormat2.getLibraryEntryProcedureUsingClause(), p);
        visit(libraryEntryProcedureClauseFormat2.getLibraryEntryProcedureGivingClause(), p);
        return libraryEntryProcedureClauseFormat2;
    }

    public Cobol visitLibraryEntryProcedureForClause(Cobol.LibraryEntryProcedureForClause libraryEntryProcedureForClause, PrintOutputCapture<P> p) {
        visitSpace(libraryEntryProcedureForClause.getPrefix(), Space.Location.LIBRARY_ENTRY_PROCEDURE_FOR_CLAUSE_PREFIX, p);
        visitMarkers(libraryEntryProcedureForClause.getMarkers(), p);
        visit(libraryEntryProcedureForClause.getWord(), p);
        visit(libraryEntryProcedureForClause.getLiteral(), p);
        return libraryEntryProcedureForClause;
    }

    public Cobol visitLibraryEntryProcedureGivingClause(Cobol.LibraryEntryProcedureGivingClause libraryEntryProcedureGivingClause, PrintOutputCapture<P> p) {
        visitSpace(libraryEntryProcedureGivingClause.getPrefix(), Space.Location.LIBRARY_ENTRY_PROCEDURE_GIVING_CLAUSE_PREFIX, p);
        visitMarkers(libraryEntryProcedureGivingClause.getMarkers(), p);
        visit(libraryEntryProcedureGivingClause.getGiving(), p);
        visit(libraryEntryProcedureGivingClause.getDataName(), p);
        return libraryEntryProcedureGivingClause;
    }

    public Cobol visitLibraryEntryProcedureUsingClause(Cobol.LibraryEntryProcedureUsingClause libraryEntryProcedureUsingClause, PrintOutputCapture<P> p) {
        visitSpace(libraryEntryProcedureUsingClause.getPrefix(), Space.Location.LIBRARY_ENTRY_PROCEDURE_USING_CLAUSE_PREFIX, p);
        visitMarkers(libraryEntryProcedureUsingClause.getMarkers(), p);
        visit(libraryEntryProcedureUsingClause.getUsing(), p);
        visit(libraryEntryProcedureUsingClause.getNames(), p);
        return libraryEntryProcedureUsingClause;
    }

    public Cobol visitLibraryEntryProcedureWithClause(Cobol.LibraryEntryProcedureWithClause libraryEntryProcedureWithClause, PrintOutputCapture<P> p) {
        visitSpace(libraryEntryProcedureWithClause.getPrefix(), Space.Location.LIBRARY_ENTRY_PROCEDURE_WITH_CLAUSE_PREFIX, p);
        visitMarkers(libraryEntryProcedureWithClause.getMarkers(), p);
        visit(libraryEntryProcedureWithClause.getWith(), p);
        visit(libraryEntryProcedureWithClause.getNames(), p);
        return libraryEntryProcedureWithClause;
    }

    public Cobol visitLibraryIsCommonClause(Cobol.LibraryIsCommonClause libraryIsCommonClause, PrintOutputCapture<P> p) {
        visitSpace(libraryIsCommonClause.getPrefix(), Space.Location.LIBRARY_IS_COMMON_CLAUSE_PREFIX, p);
        visitMarkers(libraryIsCommonClause.getMarkers(), p);
        visit(libraryIsCommonClause.getWords(), p);
        return libraryIsCommonClause;
    }

    public Cobol visitLibraryIsGlobalClause(Cobol.LibraryIsGlobalClause libraryIsGlobalClause, PrintOutputCapture<P> p) {
        visitSpace(libraryIsGlobalClause.getPrefix(), Space.Location.LIBRARY_IS_GLOBAL_CLAUSE_PREFIX, p);
        visitMarkers(libraryIsGlobalClause.getMarkers(), p);
        visit(libraryIsGlobalClause.getWords(), p);
        return libraryIsGlobalClause;
    }

    public Cobol visitLinageClause(Cobol.LinageClause linageClause, PrintOutputCapture<P> p) {
        visitSpace(linageClause.getPrefix(), Space.Location.LINAGE_CLAUSE_PREFIX, p);
        visitMarkers(linageClause.getMarkers(), p);
        visit(linageClause.getWords(), p);
        visit(linageClause.getName(), p);
        visit(linageClause.getLine(), p);
        visit(linageClause.getLinageAt(), p);
        return linageClause;
    }

    public Cobol visitLinageFootingAt(Cobol.LinageFootingAt linageFootingAt, PrintOutputCapture<P> p) {
        visitSpace(linageFootingAt.getPrefix(), Space.Location.LINAGE_FOOTING_AT_PREFIX, p);
        visitMarkers(linageFootingAt.getMarkers(), p);
        visit(linageFootingAt.getWords(), p);
        visit(linageFootingAt.getName(), p);
        return linageFootingAt;
    }

    public Cobol visitLinageLinesAtBottom(Cobol.LinageLinesAtBottom linageLinesAtBottom, PrintOutputCapture<P> p) {
        visitSpace(linageLinesAtBottom.getPrefix(), Space.Location.LINAGE_LINES_AT_BOTTOM_PREFIX, p);
        visitMarkers(linageLinesAtBottom.getMarkers(), p);
        visit(linageLinesAtBottom.getWords(), p);
        visit(linageLinesAtBottom.getName(), p);
        return linageLinesAtBottom;
    }

    public Cobol visitLinageLinesAtTop(Cobol.LinageLinesAtTop linageLinesAtTop, PrintOutputCapture<P> p) {
        visitSpace(linageLinesAtTop.getPrefix(), Space.Location.LINAGE_LINES_AT_TOP_PREFIX, p);
        visitMarkers(linageLinesAtTop.getMarkers(), p);
        visit(linageLinesAtTop.getWords(), p);
        visit(linageLinesAtTop.getName(), p);
        return linageLinesAtTop;
    }

    public Cobol visitLinkageSection(Cobol.LinkageSection linkageSection, PrintOutputCapture<P> p) {
        visitSpace(linkageSection.getPrefix(), Space.Location.LINKAGE_SECTION_PREFIX, p);
        visitMarkers(linkageSection.getMarkers(), p);
        visit(linkageSection.getWords(), p);
        visit(linkageSection.getDot(), p);
        visit(linkageSection.getDataDescriptions(), p);
        return linkageSection;
    }

    public Cobol visitLocalStorageSection(Cobol.LocalStorageSection localStorageSection, PrintOutputCapture<P> p) {
        visitSpace(localStorageSection.getPrefix(), Space.Location.LOCAL_STORAGE_SECTION_PREFIX, p);
        visitMarkers(localStorageSection.getMarkers(), p);
        visit(localStorageSection.getWords(), p);
        visit(localStorageSection.getDot(), p);
        visit(localStorageSection.getLocalName(), p);
        visit(localStorageSection.getLocalData(), p);
        visit(localStorageSection.getDataDescriptions(), p);
        return localStorageSection;
    }

    public Cobol visitMerge(Cobol.Merge merge, PrintOutputCapture<P> p) {
        visitSpace(merge.getPrefix(), Space.Location.MERGE_PREFIX, p);
        visitMarkers(merge.getMarkers(), p);
        visit(merge.getWord(), p);
        visit(merge.getFileName(), p);
        visit(merge.getMergeOnKeyClause(), p);
        visit(merge.getMergeCollatingSequencePhrase(), p);
        visit(merge.getMergeUsing(), p);
        visit(merge.getMergeOutputProcedurePhrase(), p);
        visit(merge.getMergeGivingPhrase(), p);
        return merge;
    }

    public Cobol visitMergeCollatingSequencePhrase(Cobol.MergeCollatingSequencePhrase mergeCollatingSequencePhrase, PrintOutputCapture<P> p) {
        visitSpace(mergeCollatingSequencePhrase.getPrefix(), Space.Location.MERGE_COLLATING_SEQUENCE_PHRASE_PREFIX, p);
        visitMarkers(mergeCollatingSequencePhrase.getMarkers(), p);
        visit(mergeCollatingSequencePhrase.getWords(), p);
        visit(mergeCollatingSequencePhrase.getName(), p);
        visit(mergeCollatingSequencePhrase.getMergeCollatingAlphanumeric(), p);
        visit(mergeCollatingSequencePhrase.getMergeCollatingNational(), p);
        return mergeCollatingSequencePhrase;
    }

    public Cobol visitMergeGiving(Cobol.MergeGiving mergeGiving, PrintOutputCapture<P> p) {
        visitSpace(mergeGiving.getPrefix(), Space.Location.MERGE_GIVING_PREFIX, p);
        visitMarkers(mergeGiving.getMarkers(), p);
        visit(mergeGiving.getName(), p);
        visit(mergeGiving.getWords(), p);
        return mergeGiving;
    }

    public Cobol visitMergeGivingPhrase(Cobol.MergeGivingPhrase mergeGivingPhrase, PrintOutputCapture<P> p) {
        visitSpace(mergeGivingPhrase.getPrefix(), Space.Location.MERGE_GIVING_PHRASE_PREFIX, p);
        visitMarkers(mergeGivingPhrase.getMarkers(), p);
        visit(mergeGivingPhrase.getWord(), p);
        visit(mergeGivingPhrase.getMergeGiving(), p);
        return mergeGivingPhrase;
    }

    public Cobol visitMergeOnKeyClause(Cobol.MergeOnKeyClause mergeOnKeyClause, PrintOutputCapture<P> p) {
        visitSpace(mergeOnKeyClause.getPrefix(), Space.Location.MERGE_ON_KEY_CLAUSE_PREFIX, p);
        visitMarkers(mergeOnKeyClause.getMarkers(), p);
        visit(mergeOnKeyClause.getWords(), p);
        visit(mergeOnKeyClause.getQualifiedDataName(), p);
        return mergeOnKeyClause;
    }

    public Cobol visitMergeOutputProcedurePhrase(Cobol.MergeOutputProcedurePhrase mergeOutputProcedurePhrase, PrintOutputCapture<P> p) {
        visitSpace(mergeOutputProcedurePhrase.getPrefix(), Space.Location.MERGE_OUTPUT_PROCEDURE_PHRASE_PREFIX, p);
        visitMarkers(mergeOutputProcedurePhrase.getMarkers(), p);
        visit(mergeOutputProcedurePhrase.getWords(), p);
        visit(mergeOutputProcedurePhrase.getProcedureName(), p);
        visit(mergeOutputProcedurePhrase.getMergeOutputThrough(), p);
        return mergeOutputProcedurePhrase;
    }

    @Override
    public @Nullable Cobol visitMergeOutputThrough(Cobol.MergeOutputThrough mergeOutputThrough, PrintOutputCapture<P> p) {
        visitSpace(mergeOutputThrough.getPrefix(), Space.Location.MERGE_OUTPUT_THROUGH_PREFIX, p);
        visitMarkers(mergeOutputThrough.getMarkers(), p);
        visit(mergeOutputThrough.getWord(), p);
        return mergeOutputThrough;
    }

    @Override
    public Cobol visitMergeUsing(Cobol.MergeUsing mergeUsing, PrintOutputCapture<P> p) {
        visitSpace(mergeUsing.getPrefix(), Space.Location.MERGE_USING_PREFIX, p);
        visitMarkers(mergeUsing.getMarkers(), p);
        visit(mergeUsing.getWord(), p);
        visit(mergeUsing.getFileNames(), p);
        return mergeUsing;
    }

    public Cobol visitMergeable(Cobol.Mergeable mergeable, PrintOutputCapture<P> p) {
        visitSpace(mergeable.getPrefix(), Space.Location.MERGEABLE_PREFIX, p);
        visitMarkers(mergeable.getMarkers(), p);
        visit(mergeable.getWords(), p);
        visit(mergeable.getName(), p);
        return mergeable;
    }

    public Cobol visitMessageCountClause(Cobol.MessageCountClause messageCountClause, PrintOutputCapture<P> p) {
        visitSpace(messageCountClause.getPrefix(), Space.Location.MESSAGE_COUNT_CLAUSE_PREFIX, p);
        visitMarkers(messageCountClause.getMarkers(), p);
        visit(messageCountClause.getWords(), p);
        visit(messageCountClause.getDataDescName(), p);
        return messageCountClause;
    }

    public Cobol visitMessageDateClause(Cobol.MessageDateClause messageDateClause, PrintOutputCapture<P> p) {
        visitSpace(messageDateClause.getPrefix(), Space.Location.MESSAGE_DATA_CLAUSE_PREFIX, p);
        visitMarkers(messageDateClause.getMarkers(), p);
        visit(messageDateClause.getWords(), p);
        visit(messageDateClause.getDataDescName(), p);
        return messageDateClause;
    }

    public Cobol visitMessageTimeClause(Cobol.MessageTimeClause messageTimeClause, PrintOutputCapture<P> p) {
        visitSpace(messageTimeClause.getPrefix(), Space.Location.MESSAGE_TIME_CLAUSE_PREFIX, p);
        visitMarkers(messageTimeClause.getMarkers(), p);
        visit(messageTimeClause.getWords(), p);
        visit(messageTimeClause.getDataDescName(), p);
        return messageTimeClause;
    }

    public Cobol visitMoveCorrespondingToStatement(Cobol.MoveCorrespondingToStatement moveCorrespondingToStatement, PrintOutputCapture<P> p) {
        visitSpace(moveCorrespondingToStatement.getPrefix(), Space.Location.MOVE_CORRESPONDING_TO_STATEMENT_PREFIX, p);
        visitMarkers(moveCorrespondingToStatement.getMarkers(), p);
        visit(moveCorrespondingToStatement.getWord(), p);
        visit(moveCorrespondingToStatement.getMoveCorrespondingToSendingArea(), p);
        visit(moveCorrespondingToStatement.getTo(), p);
        visit(moveCorrespondingToStatement.getIdentifiers(), p);
        return moveCorrespondingToStatement;
    }

    public Cobol visitMoveStatement(Cobol.MoveStatement moveStatement, PrintOutputCapture<P> p) {
        visitSpace(moveStatement.getPrefix(), Space.Location.MOVE_STATEMENT_PREFIX, p);
        visitMarkers(moveStatement.getMarkers(), p);
        visit(moveStatement.getWords(), p);
        visit(moveStatement.getMoveToStatement(), p);
        return moveStatement;
    }

    public Cobol visitMoveToStatement(Cobol.MoveToStatement moveToStatement, PrintOutputCapture<P> p) {
        visitSpace(moveToStatement.getPrefix(), Space.Location.MOVE_TO_STATEMENT_PREFIX, p);
        visitMarkers(moveToStatement.getMarkers(), p);
        visit(moveToStatement.getFrom(), p);
        visit(moveToStatement.getTo(), p);
        visit(moveToStatement.getNames(), p);
        return moveToStatement;
    }

    public Cobol visitMultDiv(Cobol.MultDiv multDiv, PrintOutputCapture<P> p) {
        visitSpace(multDiv.getPrefix(), Space.Location.MULT_DIV_PREFIX, p);
        visitMarkers(multDiv.getMarkers(), p);
        visit(multDiv.getWord(), p);
        visit(multDiv.getPowers(), p);
        return multDiv;
    }

    public Cobol visitMultDivs(Cobol.MultDivs multDivs, PrintOutputCapture<P> p) {
        visitSpace(multDivs.getPrefix(), Space.Location.MULT_DIVS_PREFIX, p);
        visitMarkers(multDivs.getMarkers(), p);
        visit(multDivs.getPowers(), p);
        visit(multDivs.getMultDivs(), p);
        return multDivs;
    }

    public Cobol visitMultipleFileClause(Cobol.MultipleFileClause multipleFileClause, PrintOutputCapture<P> p) {
        visitSpace(multipleFileClause.getPrefix(), Space.Location.MULTIPLE_FILE_CLAUSE_PREFIX, p);
        visitMarkers(multipleFileClause.getMarkers(), p);
        visit(multipleFileClause.getWords(), p);
        visit(multipleFileClause.getFilePositions(), p);
        return multipleFileClause;
    }

    public Cobol visitMultipleFilePosition(Cobol.MultipleFilePosition multipleFilePosition, PrintOutputCapture<P> p) {
        visitSpace(multipleFilePosition.getPrefix(), Space.Location.MULTIPLE_FILE_POSITION_PREFIX, p);
        visitMarkers(multipleFilePosition.getMarkers(), p);
        visit(multipleFilePosition.getFileName(), p);
        visit(multipleFilePosition.getPosition(), p);
        visit(multipleFilePosition.getIntegerLiteral(), p);
        return multipleFilePosition;
    }

    public Cobol visitMultiply(Cobol.Multiply multiply, PrintOutputCapture<P> p) {
        visitSpace(multiply.getPrefix(), Space.Location.MULTIPLY_PREFIX, p);
        visitMarkers(multiply.getMarkers(), p);
        visit(multiply.getWord(), p);
        visit(multiply.getMultiplicand(), p);
        visit(multiply.getBy(), p);
        visit(multiply.getMultiply(), p);
        visit(multiply.getOnSizeErrorPhrase(), p);
        visit(multiply.getNotOnSizeErrorPhrase(), p);
        visit(multiply.getEndMultiply(), p);
        return multiply;
    }

    public Cobol visitMultiplyGiving(Cobol.MultiplyGiving multiplyGiving, PrintOutputCapture<P> p) {
        visitSpace(multiplyGiving.getPrefix(), Space.Location.MULTIPLY_GIVING_PREFIX, p);
        visitMarkers(multiplyGiving.getMarkers(), p);
        visit(multiplyGiving.getOperand(), p);
        visit(multiplyGiving.getGiving(), p);
        visit(multiplyGiving.getResult(), p);
        return multiplyGiving;
    }

    public Cobol visitMultiplyRegular(Cobol.MultiplyRegular multiplyRegular, PrintOutputCapture<P> p) {
        visitSpace(multiplyRegular.getPrefix(), Space.Location.MULTIPLY_REGULAR_PREFIX, p);
        visitMarkers(multiplyRegular.getMarkers(), p);
        visit(multiplyRegular.getOperand(), p);
        return multiplyRegular;
    }

    public Cobol visitNextSentence(Cobol.NextSentence nextSentence, PrintOutputCapture<P> p) {
        visitSpace(nextSentence.getPrefix(), Space.Location.NEXT_SENTENCE_PREFIX, p);
        visitMarkers(nextSentence.getMarkers(), p);
        visit(nextSentence.getWords(), p);
        return nextSentence;
    }

    public Cobol visitObjectComputer(Cobol.ObjectComputer objectComputer, PrintOutputCapture<P> p) {
        visitSpace(objectComputer.getPrefix(), Space.Location.OBJECT_COMPUTER_PREFIX, p);
        visitMarkers(objectComputer.getMarkers(), p);
        visit(objectComputer.getWords(), p);
        visit(objectComputer.getComputer(), p);
        return objectComputer;
    }

    public Cobol visitObjectComputerDefinition(Cobol.ObjectComputerDefinition objectComputerDefinition, PrintOutputCapture<P> p) {
        visitSpace(objectComputerDefinition.getPrefix(), Space.Location.OBJECT_COMPUTER_DEFINITION_PREFIX, p);
        visitMarkers(objectComputerDefinition.getMarkers(), p);
        visit(objectComputerDefinition.getComputerName(), p);
        visit(objectComputerDefinition.getSpecifications(), p);
        visit(objectComputerDefinition.getDot(), p);
        return objectComputerDefinition;
    }

    public Cobol visitOdtClause(Cobol.OdtClause odtClause, PrintOutputCapture<P> p) {
        visitSpace(odtClause.getPrefix(), Space.Location.ODT_CLAUSE_PREFIX, p);
        visitMarkers(odtClause.getMarkers(), p);
        visit(odtClause.getWords(), p);
        visit(odtClause.getMnemonicName(), p);
        return odtClause;
    }

    public Cobol visitOpen(Cobol.Open open, PrintOutputCapture<P> p) {
        visitSpace(open.getPrefix(), Space.Location.OPEN_PREFIX, p);
        visitMarkers(open.getMarkers(), p);
        visit(open.getWord(), p);
        visit(open.getOpen(), p);
        return open;
    }

    public Cobol visitOpenIOExtendStatement(Cobol.OpenIOExtendStatement openIOExtendStatement, PrintOutputCapture<P> p) {
        visitSpace(openIOExtendStatement.getPrefix(), Space.Location.OPEN_IO_EXTEND_STATEMENT_PREFIX, p);
        visitMarkers(openIOExtendStatement.getMarkers(), p);
        visit(openIOExtendStatement.getWord(), p);
        visit(openIOExtendStatement.getFileNames(), p);
        return openIOExtendStatement;
    }

    public Cobol visitOpenInputOutputStatement(Cobol.OpenInputOutputStatement openInputOutputStatement, PrintOutputCapture<P> p) {
        visitSpace(openInputOutputStatement.getPrefix(), Space.Location.OPEN_INPUT_OUTPUT_STATEMENT_PREFIX, p);
        visitMarkers(openInputOutputStatement.getMarkers(), p);
        visit(openInputOutputStatement.getWord(), p);
        visit(openInputOutputStatement.getOpenInput(), p);
        return openInputOutputStatement;
    }

    public Cobol visitOpenable(Cobol.Openable openable, PrintOutputCapture<P> p) {
        visitSpace(openable.getPrefix(), Space.Location.OPENABLE_PREFIX, p);
        visitMarkers(openable.getMarkers(), p);
        visit(openable.getFileName(), p);
        visit(openable.getWords(), p);
        return openable;
    }

    public Cobol visitOrganizationClause(Cobol.OrganizationClause organizationClause, PrintOutputCapture<P> p) {
        visitSpace(organizationClause.getPrefix(), Space.Location.ORGANIZATION_CLAUSE_PREFIX, p);
        visitMarkers(organizationClause.getMarkers(), p);
        visit(organizationClause.getWords(), p);
        return organizationClause;
    }

    public Cobol visitPaddingCharacterClause(Cobol.PaddingCharacterClause paddingCharacterClause, PrintOutputCapture<P> p) {
        visitSpace(paddingCharacterClause.getPrefix(), Space.Location.PADDING_CHARACTER_CLAUSE_PREFIX, p);
        visitMarkers(paddingCharacterClause.getMarkers(), p);
        visit(paddingCharacterClause.getWords(), p);
        visit(paddingCharacterClause.getName(), p);
        return paddingCharacterClause;
    }

    public Cobol visitParagraph(Cobol.Paragraph paragraph, PrintOutputCapture<P> p) {
        visitSpace(paragraph.getPrefix(), Space.Location.PARAGRAPH_PREFIX, p);
        visitMarkers(paragraph.getMarkers(), p);
        visit(paragraph.getParagraphName(), p);
        visit(paragraph.getDot(), p);
        visit(paragraph.getAlteredGoTo(), p);
        visit(paragraph.getSentences(), p);
        return paragraph;
    }

    public Cobol visitParagraphs(Cobol.Paragraphs paragraphs, PrintOutputCapture<P> p) {
        visitSpace(paragraphs.getPrefix(), Space.Location.PARAGRAPHS_PREFIX, p);
        visitMarkers(paragraphs.getMarkers(), p);
        visit(paragraphs.getSentences(), p);
        visit(paragraphs.getParagraphs(), p);
        return paragraphs;
    }

    public Cobol visitParenthesized(Cobol.Parenthesized parenthesized, PrintOutputCapture<P> p) {
        visitSpace(parenthesized.getPrefix(), Space.Location.PARENTHESIZED_PREFIX, p);
        visitMarkers(parenthesized.getMarkers(), p);
        visit(parenthesized.getLeftParen(), p);
        visit(parenthesized.getContents(), p);
        visit(parenthesized.getRightParen(), p);
        return parenthesized;
    }

    public Cobol visitPasswordClause(Cobol.PasswordClause passwordClause, PrintOutputCapture<P> p) {
        visitSpace(passwordClause.getPrefix(), Space.Location.PASSWORD_CLAUSE_PREFIX, p);
        visitMarkers(passwordClause.getMarkers(), p);
        visit(passwordClause.getWords(), p);
        visit(passwordClause.getDataName(), p);
        return passwordClause;
    }

    public Cobol visitPerform(Cobol.Perform perform, PrintOutputCapture<P> p) {
        visitSpace(perform.getPrefix(), Space.Location.PERFORM_PREFIX, p);
        visitMarkers(perform.getMarkers(), p);
        visit(perform.getWord(), p);
        visit(perform.getStatement(), p);
        return perform;
    }

    public Cobol visitPerformInlineStatement(Cobol.PerformInlineStatement performInlineStatement, PrintOutputCapture<P> p) {
        visitSpace(performInlineStatement.getPrefix(), Space.Location.PERFORM_IN_LINE_STATEMENT_PREFIX, p);
        visitMarkers(performInlineStatement.getMarkers(), p);
        visit(performInlineStatement.getPerformType(), p);
        visit(performInlineStatement.getStatements(), p);
        visit(performInlineStatement.getWord(), p);
        return performInlineStatement;
    }

    public Cobol visitPerformProcedureStatement(Cobol.PerformProcedureStatement performProcedureStatement, PrintOutputCapture<P> p) {
        visitSpace(performProcedureStatement.getPrefix(), Space.Location.PERFORM_PROCEDURE_STATEMENT_PREFIX, p);
        visitMarkers(performProcedureStatement.getMarkers(), p);
        visit(performProcedureStatement.getProcedureName(), p);
        visit(performProcedureStatement.getWord(), p);
        visit(performProcedureStatement.getThroughProcedure(), p);
        visit(performProcedureStatement.getPerformType(), p);
        return performProcedureStatement;
    }

    public Cobol visitPerformTestClause(Cobol.PerformTestClause performTestClause, PrintOutputCapture<P> p) {
        visitSpace(performTestClause.getPrefix(), Space.Location.PERFORM_TEST_CLAUSE_PREFIX, p);
        visitMarkers(performTestClause.getMarkers(), p);
        visit(performTestClause.getWords(), p);
        return performTestClause;
    }

    public Cobol visitPerformTimes(Cobol.PerformTimes performTimes, PrintOutputCapture<P> p) {
        visitSpace(performTimes.getPrefix(), Space.Location.PERFORM_TIMES_PREFIX, p);
        visitMarkers(performTimes.getMarkers(), p);
        visit(performTimes.getValue(), p);
        visit(performTimes.getWord(), p);
        return performTimes;
    }

    public Cobol visitPerformUntil(Cobol.PerformUntil performUntil, PrintOutputCapture<P> p) {
        visitSpace(performUntil.getPrefix(), Space.Location.PERFORM_UNTIL_PREFIX, p);
        visitMarkers(performUntil.getMarkers(), p);
        visit(performUntil.getPerformTestClause(), p);
        visit(performUntil.getWord(), p);
        visit(performUntil.getCondition(), p);
        return performUntil;
    }

    public Cobol visitPerformVarying(Cobol.PerformVarying performVarying, PrintOutputCapture<P> p) {
        visitSpace(performVarying.getPrefix(), Space.Location.PERFORM_VARYING_PREFIX, p);
        visitMarkers(performVarying.getMarkers(), p);
        visit(performVarying.getCobols(), p);
        return performVarying;
    }

    public Cobol visitPerformVaryingClause(Cobol.PerformVaryingClause performVaryingClause, PrintOutputCapture<P> p) {
        visitSpace(performVaryingClause.getPrefix(), Space.Location.PERFORM_VARYING_CLAUSE_PREFIX, p);
        visitMarkers(performVaryingClause.getMarkers(), p);
        visit(performVaryingClause.getWord(), p);
        visit(performVaryingClause.getPerformVaryingPhrase(), p);
        visit(performVaryingClause.getPerformAfter(), p);
        return performVaryingClause;
    }

    public Cobol visitPerformVaryingPhrase(Cobol.PerformVaryingPhrase performVaryingPhrase, PrintOutputCapture<P> p) {
        visitSpace(performVaryingPhrase.getPrefix(), Space.Location.PERFORM_VARYING_PHRASE_PREFIX, p);
        visitMarkers(performVaryingPhrase.getMarkers(), p);
        visit(performVaryingPhrase.getName(), p);
        visit(performVaryingPhrase.getFrom(), p);
        visit(performVaryingPhrase.getBy(), p);
        visit(performVaryingPhrase.getUntil(), p);
        return performVaryingPhrase;
    }

    public Cobol visitPerformable(Cobol.Performable performable, PrintOutputCapture<P> p) {
        visitSpace(performable.getPrefix(), Space.Location.PERFORMABLE_PREFIX, p);
        visitMarkers(performable.getMarkers(), p);
        visit(performable.getWord(), p);
        visit(performable.getExpression(), p);
        return performable;
    }

    public Cobol visitPicture(Cobol.Picture picture, PrintOutputCapture<P> p) {
        visitSpace(picture.getPrefix(), Space.Location.PICTURE_PREFIX, p);
        visitMarkers(picture.getMarkers(), p);
        visit(picture.getWords(), p);
        visit(picture.getParenthesized(), p);
        return picture;
    }

    public Cobol visitPictureString(Cobol.PictureString pictureString, PrintOutputCapture<P> p) {
        visitSpace(pictureString.getPrefix(), Space.Location.PICTURE_STRING_PREFIX, p);
        visitMarkers(pictureString.getMarkers(), p);
        visit(pictureString.getPictures(), p);
        return pictureString;
    }

    public Cobol visitPlusMinus(Cobol.PlusMinus plusMinus, PrintOutputCapture<P> p) {
        visitSpace(plusMinus.getPrefix(), Space.Location.PLUS_MINUS_PREFIX, p);
        visitMarkers(plusMinus.getMarkers(), p);
        visit(plusMinus.getWord(), p);
        visit(plusMinus.getMultDivs(), p);
        return plusMinus;
    }

    public Cobol visitPower(Cobol.Power power, PrintOutputCapture<P> p) {
        visitSpace(power.getPrefix(), Space.Location.POWER_PREFIX, p);
        visitMarkers(power.getMarkers(), p);
        visit(power.getPower(), p);
        visit(power.getExpression(), p);
        return power;
    }

    public Cobol visitPowers(Cobol.Powers powers, PrintOutputCapture<P> p) {
        visitSpace(powers.getPrefix(), Space.Location.POWERS_PREFIX, p);
        visitMarkers(powers.getMarkers(), p);
        visit(powers.getPlusMinusChar(), p);
        visit(powers.getExpression(), p);
        visit(powers.getPowers(), p);
        return powers;
    }

    public Cobol visitProcedureDeclarative(Cobol.ProcedureDeclarative procedureDeclarative, PrintOutputCapture<P> p) {
        visitSpace(procedureDeclarative.getPrefix(), Space.Location.PROCEDURE_DECLARATIVE_PREFIX, p);
        visitMarkers(procedureDeclarative.getMarkers(), p);
        visit(procedureDeclarative.getProcedureSectionHeader(), p);
        visit(procedureDeclarative.getDot(), p);
        visit(procedureDeclarative.getUseStatement(), p);
        visit(procedureDeclarative.getDot2(), p);
        visit(procedureDeclarative.getParagraphs(), p);
        return procedureDeclarative;
    }

    public Cobol visitProcedureDeclaratives(Cobol.ProcedureDeclaratives procedureDeclaratives, PrintOutputCapture<P> p) {
        visitSpace(procedureDeclaratives.getPrefix(), Space.Location.PROCEDURE_DECLARATIVES_PREFIX, p);
        visitMarkers(procedureDeclaratives.getMarkers(), p);
        visit(procedureDeclaratives.getDeclaratives(), p);
        visit(procedureDeclaratives.getDot(), p);
        visit(procedureDeclaratives.getProcedureDeclarative(), p);
        visit(procedureDeclaratives.getEndDeclaratives(), p);
        visit(procedureDeclaratives.getDot2(), p);
        return procedureDeclaratives;
    }

    public Cobol visitProcedureDivision(Cobol.ProcedureDivision procedureDivision, PrintOutputCapture<P> p) {
        visitSpace(procedureDivision.getPrefix(), Space.Location.PROCEDURE_DIVISION_PREFIX, p);
        visitMarkers(procedureDivision.getMarkers(), p);
        visit(procedureDivision.getWords(), p);
        visit(procedureDivision.getProcedureDivisionUsingClause(), p);
        visit(procedureDivision.getProcedureDivisionGivingClause(), p);
        visit(procedureDivision.getDot(), p);
        visit(procedureDivision.getProcedureDeclaratives(), p);
        visit(procedureDivision.getBody(), p);
        return procedureDivision;
    }

    public Cobol visitProcedureDivisionBody(Cobol.ProcedureDivisionBody procedureDivisionBody, PrintOutputCapture<P> p) {
        visitSpace(procedureDivisionBody.getPrefix(), Space.Location.PROCEDURE_DIVISION_BODY_PREFIX, p);
        visitMarkers(procedureDivisionBody.getMarkers(), p);
        visit(procedureDivisionBody.getParagraphs(), p);
        visit(procedureDivisionBody.getProcedureSection(), p);
        return procedureDivisionBody;
    }

    public Cobol visitProcedureDivisionByReference(Cobol.ProcedureDivisionByReference procedureDivisionByReference, PrintOutputCapture<P> p) {
        visitSpace(procedureDivisionByReference.getPrefix(), Space.Location.PROCEDURE_DIVISION_BY_REFERENCE_PREFIX, p);
        visitMarkers(procedureDivisionByReference.getMarkers(), p);
        visit(procedureDivisionByReference.getWord(), p);
        visit(procedureDivisionByReference.getReference(), p);
        return procedureDivisionByReference;
    }

    public Cobol visitProcedureDivisionByReferencePhrase(Cobol.ProcedureDivisionByReferencePhrase procedureDivisionByReferencePhrase, PrintOutputCapture<P> p) {
        visitSpace(procedureDivisionByReferencePhrase.getPrefix(), Space.Location.PROCEDURE_DIVISION_BY_REFERENCE_PHRASE_PREFIX, p);
        visitMarkers(procedureDivisionByReferencePhrase.getMarkers(), p);
        visit(procedureDivisionByReferencePhrase.getWords(), p);
        visit(procedureDivisionByReferencePhrase.getProcedureDivisionByReference(), p);
        return procedureDivisionByReferencePhrase;
    }

    public Cobol visitProcedureDivisionByValuePhrase(Cobol.ProcedureDivisionByValuePhrase procedureDivisionByValuePhrase, PrintOutputCapture<P> p) {
        visitSpace(procedureDivisionByValuePhrase.getPrefix(), Space.Location.PROCEDURE_DIVISION_BY_VALUE_PHRASE_PREFIX, p);
        visitMarkers(procedureDivisionByValuePhrase.getMarkers(), p);
        visit(procedureDivisionByValuePhrase.getWords(), p);
        visit(procedureDivisionByValuePhrase.getPhrases(), p);
        return procedureDivisionByValuePhrase;
    }

    public Cobol visitProcedureDivisionGivingClause(Cobol.ProcedureDivisionGivingClause procedureDivisionGivingClause, PrintOutputCapture<P> p) {
        visitSpace(procedureDivisionGivingClause.getPrefix(), Space.Location.PROCEDURE_DIVISION_GIVING_CLAUSE_PREFIX, p);
        visitMarkers(procedureDivisionGivingClause.getMarkers(), p);
        visit(procedureDivisionGivingClause.getWord(), p);
        visit(procedureDivisionGivingClause.getDataName(), p);
        return procedureDivisionGivingClause;
    }

    @SuppressWarnings("NullableProblems")
    @Nullable
    public Cobol visitProcedureDivisionUsingClause(@Nullable Cobol.ProcedureDivisionUsingClause procedureDivisionUsingClause, PrintOutputCapture<P> p) {
        if (procedureDivisionUsingClause == null) {
            return null;
        }
        visitSpace(procedureDivisionUsingClause.getPrefix(), Space.Location.PROCEDURE_DIVISION_USING_CLAUSE_PREFIX, p);
        visitMarkers(procedureDivisionUsingClause.getMarkers(), p);
        visit(procedureDivisionUsingClause.getWord(), p);
        visit(procedureDivisionUsingClause.getProcedureDivisionUsingParameter(), p);
        return procedureDivisionUsingClause;
    }

    public Cobol visitProcedureName(Cobol.ProcedureName procedureName, PrintOutputCapture<P> p) {
        visitSpace(procedureName.getPrefix(), Space.Location.PROCEDURE_NAME_PREFIX, p);
        visitMarkers(procedureName.getMarkers(), p);
        visit(procedureName.getParagraphName(), p);
        visit(procedureName.getInSection(), p);
        visit(procedureName.getSectionName(), p);
        return procedureName;
    }

    public Cobol visitProcedureSection(Cobol.ProcedureSection procedureSection, PrintOutputCapture<P> p) {
        visitSpace(procedureSection.getPrefix(), Space.Location.PROCEDURE_SECTION_PREFIX, p);
        visitMarkers(procedureSection.getMarkers(), p);
        visit(procedureSection.getProcedureSectionHeader(), p);
        visit(procedureSection.getDot(), p);
        visit(procedureSection.getParagraphs(), p);
        return procedureSection;
    }

    public Cobol visitProcedureSectionHeader(Cobol.ProcedureSectionHeader procedureSectionHeader, PrintOutputCapture<P> p) {
        visitSpace(procedureSectionHeader.getPrefix(), Space.Location.PROCEDURE_SECTION_HEADER_PREFIX, p);
        visitMarkers(procedureSectionHeader.getMarkers(), p);
        visit(procedureSectionHeader.getSectionName(), p);
        visit(procedureSectionHeader.getSection(), p);
        visit(procedureSectionHeader.getIdentifier(), p);
        return procedureSectionHeader;
    }

    public Cobol visitProgramIdParagraph(Cobol.ProgramIdParagraph programIdParagraph, PrintOutputCapture<P> p) {
        visitSpace(programIdParagraph.getPrefix(), Space.Location.PROGRAM_ID_PARAGRAPH_PREFIX, p);
        visitMarkers(programIdParagraph.getMarkers(), p);
        visit(programIdParagraph.getProgramId(), p);
        visit(programIdParagraph.getDot(), p);
        visit(programIdParagraph.getProgramName(), p);
        visit(programIdParagraph.getProgramAttributes(), p);
        visit(programIdParagraph.getDot2(), p);
        return programIdParagraph;
    }

    public Cobol visitProgramLibrarySection(Cobol.ProgramLibrarySection programLibrarySection, PrintOutputCapture<P> p) {
        visitSpace(programLibrarySection.getPrefix(), Space.Location.PROGRAM_LIBRARY_SECTION_PREFIX, p);
        visitMarkers(programLibrarySection.getMarkers(), p);
        visit(programLibrarySection.getWords(), p);
        visit(programLibrarySection.getLibraryDescriptionEntries(), p);
        return programLibrarySection;
    }

    public Cobol visitProgramUnit(Cobol.ProgramUnit programUnit, PrintOutputCapture<P> p) {
        visitSpace(programUnit.getPrefix(), Space.Location.PROGRAM_UNIT_PREFIX, p);
        visitMarkers(programUnit.getMarkers(), p);
        visit(programUnit.getIdentificationDivision(), p);
        visit(programUnit.getEnvironmentDivision(), p);
        visit(programUnit.getDataDivision(), p);
        visit(programUnit.getProcedureDivision(), p);
        visit(programUnit.getProgramUnits(), p);
        visit(programUnit.getEndProgram(), p);
        return programUnit;
    }

    public Cobol visitPurge(Cobol.Purge purge, PrintOutputCapture<P> p) {
        visitSpace(purge.getPrefix(), Space.Location.PURGE_PREFIX, p);
        visitMarkers(purge.getMarkers(), p);
        visit(purge.getPurge(), p);
        visit(purge.getNames(), p);
        return purge;
    }

    public Cobol visitQualifiedDataName(Cobol.QualifiedDataName qualifiedDataName, PrintOutputCapture<P> p) {
        visitSpace(qualifiedDataName.getPrefix(), Space.Location.QUALIFIED_DATA_NAME_PREFIX, p);
        visitMarkers(qualifiedDataName.getMarkers(), p);
        visit(qualifiedDataName.getDataName(), p);
        return qualifiedDataName;
    }

    public Cobol visitQualifiedDataNameFormat1(Cobol.QualifiedDataNameFormat1 qualifiedDataNameFormat1, PrintOutputCapture<P> p) {
        visitSpace(qualifiedDataNameFormat1.getPrefix(), Space.Location.QUALIFIED_DATA_NAME_FORMAT_1_PREFIX, p);
        visitMarkers(qualifiedDataNameFormat1.getMarkers(), p);
        visit(qualifiedDataNameFormat1.getName(), p);
        visit(qualifiedDataNameFormat1.getQualifiedInData(), p);
        visit(qualifiedDataNameFormat1.getInFile(), p);
        return qualifiedDataNameFormat1;
    }

    public Cobol visitQualifiedDataNameFormat2(Cobol.QualifiedDataNameFormat2 qualifiedDataNameFormat2, PrintOutputCapture<P> p) {
        visitSpace(qualifiedDataNameFormat2.getPrefix(), Space.Location.QUALIFIED_DATA_NAME_FORMAT_2_PREFIX, p);
        visitMarkers(qualifiedDataNameFormat2.getMarkers(), p);
        visit(qualifiedDataNameFormat2.getParagraphName(), p);
        visit(qualifiedDataNameFormat2.getInSection(), p);
        return qualifiedDataNameFormat2;
    }

    public Cobol visitQualifiedDataNameFormat3(Cobol.QualifiedDataNameFormat3 qualifiedDataNameFormat3, PrintOutputCapture<P> p) {
        visitSpace(qualifiedDataNameFormat3.getPrefix(), Space.Location.QUALIFIED_DATA_NAME_FORMAT_3_PREFIX, p);
        visitMarkers(qualifiedDataNameFormat3.getMarkers(), p);
        visit(qualifiedDataNameFormat3.getTextName(), p);
        visit(qualifiedDataNameFormat3.getInLibrary(), p);
        return qualifiedDataNameFormat3;
    }

    public Cobol visitQualifiedDataNameFormat4(Cobol.QualifiedDataNameFormat4 qualifiedDataNameFormat4, PrintOutputCapture<P> p) {
        visitSpace(qualifiedDataNameFormat4.getPrefix(), Space.Location.QUALIFIED_DATA_NAME_FORMAT_4_PREFIX, p);
        visitMarkers(qualifiedDataNameFormat4.getMarkers(), p);
        visit(qualifiedDataNameFormat4.getLinageCounter(), p);
        visit(qualifiedDataNameFormat4.getInFile(), p);
        return qualifiedDataNameFormat4;
    }

    public Cobol visitQualifiedInData(Cobol.QualifiedInData qualifiedInData, PrintOutputCapture<P> p) {
        visitSpace(qualifiedInData.getPrefix(), Space.Location.QUALIFIED_IN_DATA_PREFIX, p);
        visitMarkers(qualifiedInData.getMarkers(), p);
        visit(qualifiedInData.getIn(), p);
        return qualifiedInData;
    }

    public Cobol visitRead(Cobol.Read read, PrintOutputCapture<P> p) {
        visitSpace(read.getPrefix(), Space.Location.READ_PREFIX, p);
        visitMarkers(read.getMarkers(), p);
        visit(read.getWord(), p);
        visit(read.getFileName(), p);
        visit(read.getNextRecord(), p);
        visit(read.getReadInto(), p);
        visit(read.getReadWith(), p);
        visit(read.getReadKey(), p);
        visit(read.getInvalidKeyPhrase(), p);
        visit(read.getNotInvalidKeyPhrase(), p);
        visit(read.getAtEndPhrase(), p);
        visit(read.getNotAtEndPhrase(), p);
        visit(read.getEndRead(), p);
        return read;
    }

    public Cobol visitReadInto(Cobol.ReadInto readInto, PrintOutputCapture<P> p) {
        visitSpace(readInto.getPrefix(), Space.Location.READ_INTO_PREFIX, p);
        visitMarkers(readInto.getMarkers(), p);
        visit(readInto.getWord(), p);
        visit(readInto.getIdentifier(), p);
        return readInto;
    }

    public Cobol visitReadKey(Cobol.ReadKey readKey, PrintOutputCapture<P> p) {
        visitSpace(readKey.getPrefix(), Space.Location.READ_KEY_PREFIX, p);
        visitMarkers(readKey.getMarkers(), p);
        visit(readKey.getWords(), p);
        visit(readKey.getQualifiedDataName(), p);
        return readKey;
    }

    public Cobol visitReadWith(Cobol.ReadWith readWith, PrintOutputCapture<P> p) {
        visitSpace(readWith.getPrefix(), Space.Location.READ_WITH_PREFIX, p);
        visitMarkers(readWith.getMarkers(), p);
        visit(readWith.getWords(), p);
        return readWith;
    }

    public Cobol visitReceivable(Cobol.Receivable receivable, PrintOutputCapture<P> p) {
        visitSpace(receivable.getPrefix(), Space.Location.RECEIVABLE_PREFIX, p);
        visitMarkers(receivable.getMarkers(), p);
        visit(receivable.getWords(), p);
        visit(receivable.getValue(), p);
        return receivable;
    }

    public Cobol visitReceiveWith(Cobol.ReceiveWith receiveWith, PrintOutputCapture<P> p) {
        visitSpace(receiveWith.getPrefix(), Space.Location.RECEIVE_WITH_PREFIX, p);
        visitMarkers(receiveWith.getMarkers(), p);
        visit(receiveWith.getWords(), p);
        return receiveWith;
    }

    public Cobol visitReceive(Cobol.Receive receive, PrintOutputCapture<P> p) {
        visitSpace(receive.getPrefix(), Space.Location.RECEIVE_PREFIX, p);
        visitMarkers(receive.getMarkers(), p);
        visit(receive.getReceive(), p);
        visit(receive.getFromOrInto(), p);
        visit(receive.getOnExceptionClause(), p);
        visit(receive.getNotOnExceptionClause(), p);
        visit(receive.getEndReceive(), p);
        return receive;
    }

    public Cobol visitReceiveFrom(Cobol.ReceiveFrom receiveFrom, PrintOutputCapture<P> p) {
        visitSpace(receiveFrom.getPrefix(), Space.Location.RECEIVE_FROM_PREFIX, p);
        visitMarkers(receiveFrom.getMarkers(), p);
        visit(receiveFrom.getWords(), p);
        visit(receiveFrom.getDataName(), p);
        return receiveFrom;
    }

    public Cobol visitReceiveFromStatement(Cobol.ReceiveFromStatement receiveFromStatement, PrintOutputCapture<P> p) {
        visitSpace(receiveFromStatement.getPrefix(), Space.Location.RECEIVE_FROM_STATEMENT_PREFIX, p);
        visitMarkers(receiveFromStatement.getMarkers(), p);
        visit(receiveFromStatement.getDataName(), p);
        visit(receiveFromStatement.getFrom(), p);
        visit(receiveFromStatement.getReceiveFrom(), p);
        visit(receiveFromStatement.getBeforeWithThreadSizeStatus(), p);
        return receiveFromStatement;
    }

    public Cobol visitReceiveIntoStatement(Cobol.ReceiveIntoStatement receiveIntoStatement, PrintOutputCapture<P> p) {
        visitSpace(receiveIntoStatement.getPrefix(), Space.Location.RECEIVE_INTO_STATEMENT_PREFIX, p);
        visitMarkers(receiveIntoStatement.getMarkers(), p);
        visit(receiveIntoStatement.getCdName(), p);
        visit(receiveIntoStatement.getWords(), p);
        visit(receiveIntoStatement.getIdentifier(), p);
        visit(receiveIntoStatement.getReceiveNoData(), p);
        visit(receiveIntoStatement.getReceiveWithData(), p);
        return receiveIntoStatement;
    }

    public Cobol visitRecordContainsClause(Cobol.RecordContainsClause recordContainsClause, PrintOutputCapture<P> p) {
        visitSpace(recordContainsClause.getPrefix(), Space.Location.RECORD_CONTAINS_CLAUSE_PREFIX, p);
        visitMarkers(recordContainsClause.getMarkers(), p);
        visit(recordContainsClause.getRecord(), p);
        visit(recordContainsClause.getClause(), p);
        return recordContainsClause;
    }

    public Cobol visitRecordContainsClauseFormat1(Cobol.RecordContainsClauseFormat1 recordContainsClauseFormat1, PrintOutputCapture<P> p) {
        visitSpace(recordContainsClauseFormat1.getPrefix(), Space.Location.RECORD_CONTAINS_CLAUSE_FORMAT_1_PREFIX, p);
        visitMarkers(recordContainsClauseFormat1.getMarkers(), p);
        visit(recordContainsClauseFormat1.getContains(), p);
        visit(recordContainsClauseFormat1.getIntegerLiteral(), p);
        visit(recordContainsClauseFormat1.getCharacters(), p);
        return recordContainsClauseFormat1;
    }

    public Cobol visitRecordContainsClauseFormat2(Cobol.RecordContainsClauseFormat2 recordContainsClauseFormat2, PrintOutputCapture<P> p) {
        visitSpace(recordContainsClauseFormat2.getPrefix(), Space.Location.RECORD_CONTAINS_CLAUSE_FORMAT_2_PREFIX, p);
        visitMarkers(recordContainsClauseFormat2.getMarkers(), p);
        visit(recordContainsClauseFormat2.getWords(), p);
        visit(recordContainsClauseFormat2.getFromClause(), p);
        visit(recordContainsClauseFormat2.getQualifiedDataName(), p);
        return recordContainsClauseFormat2;
    }

    public Cobol visitRecordContainsClauseFormat3(Cobol.RecordContainsClauseFormat3 recordContainsClauseFormat3, PrintOutputCapture<P> p) {
        visitSpace(recordContainsClauseFormat3.getPrefix(), Space.Location.RECORD_CONTAINS_CLAUSE_FORMAT_3_PREFIX, p);
        visitMarkers(recordContainsClauseFormat3.getMarkers(), p);
        visit(recordContainsClauseFormat3.getContains(), p);
        visit(recordContainsClauseFormat3.getIntegerLiteral(), p);
        visit(recordContainsClauseFormat3.getRecordContainsTo(), p);
        visit(recordContainsClauseFormat3.getCharacters(), p);
        return recordContainsClauseFormat3;
    }

    public Cobol visitRecordContainsTo(Cobol.RecordContainsTo recordContainsTo, PrintOutputCapture<P> p) {
        visitSpace(recordContainsTo.getPrefix(), Space.Location.RECORD_CONTAINS_TO_PREFIX, p);
        visitMarkers(recordContainsTo.getMarkers(), p);
        visit(recordContainsTo.getTo(), p);
        visit(recordContainsTo.getIntegerLiteral(), p);
        return recordContainsTo;
    }

    public Cobol visitRecordDelimiterClause(Cobol.RecordDelimiterClause recordDelimiterClause, PrintOutputCapture<P> p) {
        visitSpace(recordDelimiterClause.getPrefix(), Space.Location.RECORD_DELIMITER_CLAUSE_PREFIX, p);
        visitMarkers(recordDelimiterClause.getMarkers(), p);
        visit(recordDelimiterClause.getWords(), p);
        visit(recordDelimiterClause.getName(), p);
        return recordDelimiterClause;
    }

    public Cobol visitRecordKeyClause(Cobol.RecordKeyClause recordKeyClause, PrintOutputCapture<P> p) {
        visitSpace(recordKeyClause.getPrefix(), Space.Location.RECORD_KEY_CLAUSE_PREFIX, p);
        visitMarkers(recordKeyClause.getMarkers(), p);
        visit(recordKeyClause.getRecordWords(), p);
        visit(recordKeyClause.getQualifiedDataName(), p);
        visit(recordKeyClause.getPasswordClause(), p);
        visit(recordKeyClause.getDuplicates(), p);
        return recordKeyClause;
    }

    public Cobol visitRecordingModeClause(Cobol.RecordingModeClause recordingModeClause, PrintOutputCapture<P> p) {
        visitSpace(recordingModeClause.getPrefix(), Space.Location.RECORDING_MODE_CLAUSE_PREFIX, p);
        visitMarkers(recordingModeClause.getMarkers(), p);
        visit(recordingModeClause.getWords(), p);
        visit(recordingModeClause.getMode(), p);
        return recordingModeClause;
    }

    public Cobol visitReferenceModifier(Cobol.ReferenceModifier referenceModifier, PrintOutputCapture<P> p) {
        visitSpace(referenceModifier.getPrefix(), Space.Location.REFERENCE_MODIFIER_PREFIX, p);
        visitMarkers(referenceModifier.getMarkers(), p);
        visit(referenceModifier.getLeftParen(), p);
        visit(referenceModifier.getCharacterPosition(), p);
        visit(referenceModifier.getColon(), p);
        visit(referenceModifier.getLength(), p);
        visit(referenceModifier.getRightParen(), p);
        return referenceModifier;
    }

    public Cobol visitRelationArithmeticComparison(Cobol.RelationArithmeticComparison relationArithmeticComparison, PrintOutputCapture<P> p) {
        visitSpace(relationArithmeticComparison.getPrefix(), Space.Location.RELATION_ARITHMETIC_COMPARISON_PREFIX, p);
        visitMarkers(relationArithmeticComparison.getMarkers(), p);
        visit(relationArithmeticComparison.getArithmeticExpressionA(), p);
        visit(relationArithmeticComparison.getRelationalOperator(), p);
        visit(relationArithmeticComparison.getArithmeticExpressionB(), p);
        return relationArithmeticComparison;
    }

    public Cobol visitRelationCombinedComparison(Cobol.RelationCombinedComparison relationCombinedComparison, PrintOutputCapture<P> p) {
        visitSpace(relationCombinedComparison.getPrefix(), Space.Location.RELATION_COMBINED_COMPARISON_PREFIX, p);
        visitMarkers(relationCombinedComparison.getMarkers(), p);
        visit(relationCombinedComparison.getArithmeticExpression(), p);
        visit(relationCombinedComparison.getRelationalOperator(), p);
        visit(relationCombinedComparison.getCombinedCondition(), p);
        return relationCombinedComparison;
    }

    public Cobol visitRelationCombinedCondition(Cobol.RelationCombinedCondition relationCombinedCondition, PrintOutputCapture<P> p) {
        visitSpace(relationCombinedCondition.getPrefix(), Space.Location.RELATION_COMBINED_CONDITION_PREFIX, p);
        visitMarkers(relationCombinedCondition.getMarkers(), p);
        visit(relationCombinedCondition.getRelationalArithmeticExpressions(), p);
        return relationCombinedCondition;
    }

    public Cobol visitRelationSignCondition(Cobol.RelationSignCondition relationSignCondition, PrintOutputCapture<P> p) {
        visitSpace(relationSignCondition.getPrefix(), Space.Location.RELATION_SIGN_CONDITION_PREFIX, p);
        visitMarkers(relationSignCondition.getMarkers(), p);
        visit(relationSignCondition.getArithmeticExpression(), p);
        visit(relationSignCondition.getWords(), p);
        return relationSignCondition;
    }

    public Cobol visitRelationalOperator(Cobol.RelationalOperator relationalOperator, PrintOutputCapture<P> p) {
        visitSpace(relationalOperator.getPrefix(), Space.Location.RELATIONAL_OPERATOR_PREFIX, p);
        visitMarkers(relationalOperator.getMarkers(), p);
        visit(relationalOperator.getWords(), p);
        return relationalOperator;
    }

    public Cobol visitRelativeKeyClause(Cobol.RelativeKeyClause relativeKeyClause, PrintOutputCapture<P> p) {
        visitSpace(relativeKeyClause.getPrefix(), Space.Location.RELATIVE_KEY_CLAUSE_PREFIX, p);
        visitMarkers(relativeKeyClause.getMarkers(), p);
        visit(relativeKeyClause.getWords(), p);
        visit(relativeKeyClause.getQualifiedDataName(), p);
        return relativeKeyClause;
    }

    public Cobol visitRelease(Cobol.Release release, PrintOutputCapture<P> p) {
        visitSpace(release.getPrefix(), Space.Location.RELEASE_PREFIX, p);
        visitMarkers(release.getMarkers(), p);
        visit(release.getRelease(), p);
        visit(release.getRecordName(), p);
        visit(release.getFrom(), p);
        visit(release.getQualifiedDataName(), p);
        return release;
    }

    public Cobol visitReportClause(Cobol.ReportClause reportClause, PrintOutputCapture<P> p) {
        visitSpace(reportClause.getPrefix(), Space.Location.REPORT_CLAUSE_PREFIX, p);
        visitMarkers(reportClause.getMarkers(), p);
        visit(reportClause.getWords(), p);
        visit(reportClause.getReportName(), p);
        return reportClause;
    }

    public Cobol visitReportDescription(Cobol.ReportDescription reportDescription, PrintOutputCapture<P> p) {
        visitSpace(reportDescription.getPrefix(), Space.Location.REPORT_DESCRIPTION_PREFIX, p);
        visitMarkers(reportDescription.getMarkers(), p);
        visit(reportDescription.getReportDescriptionEntry(), p);
        visit(reportDescription.getGroupDescriptionEntries(), p);
        return reportDescription;
    }

    public Cobol visitReportDescriptionEntry(Cobol.ReportDescriptionEntry reportDescriptionEntry, PrintOutputCapture<P> p) {
        visitSpace(reportDescriptionEntry.getPrefix(), Space.Location.REPORT_DESCRIPTION_ENTRY_PREFIX, p);
        visitMarkers(reportDescriptionEntry.getMarkers(), p);
        visit(reportDescriptionEntry.getRd(), p);
        visit(reportDescriptionEntry.getQualifiedDataName(), p);
        visit(reportDescriptionEntry.getReportDescriptionGlobalClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionPageLimitClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionHeadingClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionFirstDetailClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionLastDetailClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionFootingClause(), p);
        visit(reportDescriptionEntry.getDot(), p);
        return reportDescriptionEntry;
    }

    public Cobol visitReportDescriptionFirstDetailClause(Cobol.ReportDescriptionFirstDetailClause reportDescriptionFirstDetailClause, PrintOutputCapture<P> p) {
        visitSpace(reportDescriptionFirstDetailClause.getPrefix(), Space.Location.REPORT_DESCRIPTION_FIRST_DETAIL_CLAUSE_PREFIX, p);
        visitMarkers(reportDescriptionFirstDetailClause.getMarkers(), p);
        visit(reportDescriptionFirstDetailClause.getWords(), p);
        visit(reportDescriptionFirstDetailClause.getDataName(), p);
        return reportDescriptionFirstDetailClause;
    }

    public Cobol visitReportDescriptionFootingClause(Cobol.ReportDescriptionFootingClause reportDescriptionFootingClause, PrintOutputCapture<P> p) {
        visitSpace(reportDescriptionFootingClause.getPrefix(), Space.Location.REPORT_DESCRIPTION_FOOTING_CLAUSE_PREFIX, p);
        visitMarkers(reportDescriptionFootingClause.getMarkers(), p);
        visit(reportDescriptionFootingClause.getWord(), p);
        visit(reportDescriptionFootingClause.getDataName(), p);
        return reportDescriptionFootingClause;
    }

    public Cobol visitReportDescriptionGlobalClause(Cobol.ReportDescriptionGlobalClause reportDescriptionGlobalClause, PrintOutputCapture<P> p) {
        visitSpace(reportDescriptionGlobalClause.getPrefix(), Space.Location.REPORT_DESCRIPTION_GLOBAL_CLAUSE_PREFIX, p);
        visitMarkers(reportDescriptionGlobalClause.getMarkers(), p);
        visit(reportDescriptionGlobalClause.getWords(), p);
        return reportDescriptionGlobalClause;
    }

    public Cobol visitReportDescriptionHeadingClause(Cobol.ReportDescriptionHeadingClause reportDescriptionHeadingClause, PrintOutputCapture<P> p) {
        visitSpace(reportDescriptionHeadingClause.getPrefix(), Space.Location.REPORT_DESCRIPTION_HEADING_CLAUSE_PREFIX, p);
        visitMarkers(reportDescriptionHeadingClause.getMarkers(), p);
        visit(reportDescriptionHeadingClause.getWord(), p);
        visit(reportDescriptionHeadingClause.getDataName(), p);
        return reportDescriptionHeadingClause;
    }

    public Cobol visitReportDescriptionLastDetailClause(Cobol.ReportDescriptionLastDetailClause reportDescriptionLastDetailClause, PrintOutputCapture<P> p) {
        visitSpace(reportDescriptionLastDetailClause.getPrefix(), Space.Location.REPORT_DESCRIPTION_LAST_DETAIL_CLAUSE_PREFIX, p);
        visitMarkers(reportDescriptionLastDetailClause.getMarkers(), p);
        visit(reportDescriptionLastDetailClause.getWords(), p);
        visit(reportDescriptionLastDetailClause.getDataName(), p);
        return reportDescriptionLastDetailClause;
    }

    public Cobol visitReportDescriptionPageLimitClause(Cobol.ReportDescriptionPageLimitClause reportDescriptionPageLimitClause, PrintOutputCapture<P> p) {
        visitSpace(reportDescriptionPageLimitClause.getPrefix(), Space.Location.REPORT_DESCRIPTION_PAGE_LIMIT_CLAUSE_PREFIX, p);
        visitMarkers(reportDescriptionPageLimitClause.getMarkers(), p);
        visit(reportDescriptionPageLimitClause.getFirstWords(), p);
        visit(reportDescriptionPageLimitClause.getIntegerLiteral(), p);
        visit(reportDescriptionPageLimitClause.getSecondWords(), p);
        return reportDescriptionPageLimitClause;
    }

    public Cobol visitReportGroupBlankWhenZeroClause(Cobol.ReportGroupBlankWhenZeroClause reportGroupBlankWhenZeroClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupBlankWhenZeroClause.getPrefix(), Space.Location.REPORT_GROUP_BLANK_WHEN_ZERO_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupBlankWhenZeroClause.getMarkers(), p);
        visit(reportGroupBlankWhenZeroClause.getWords(), p);
        return reportGroupBlankWhenZeroClause;
    }

    public Cobol visitReportGroupColumnNumberClause(Cobol.ReportGroupColumnNumberClause reportGroupColumnNumberClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupColumnNumberClause.getPrefix(), Space.Location.REPORT_GROUP_COLUMN_NUMBER_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupColumnNumberClause.getMarkers(), p);
        visit(reportGroupColumnNumberClause.getWords(), p);
        visit(reportGroupColumnNumberClause.getDataName(), p);
        return reportGroupColumnNumberClause;
    }

    public Cobol visitReportGroupDescriptionEntryFormat1(Cobol.ReportGroupDescriptionEntryFormat1 reportGroupDescriptionEntryFormat1, PrintOutputCapture<P> p) {
        visitSpace(reportGroupDescriptionEntryFormat1.getPrefix(), Space.Location.REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_1_PREFIX, p);
        visitMarkers(reportGroupDescriptionEntryFormat1.getMarkers(), p);
        visit(reportGroupDescriptionEntryFormat1.getIntegerLiteral(), p);
        visit(reportGroupDescriptionEntryFormat1.getDataName(), p);
        visit(reportGroupDescriptionEntryFormat1.getGroupLineNumberClause(), p);
        visit(reportGroupDescriptionEntryFormat1.getGroupNextGroupClause(), p);
        visit(reportGroupDescriptionEntryFormat1.getGroupTypeClause(), p);
        visit(reportGroupDescriptionEntryFormat1.getGroupUsageClause(), p);
        visit(reportGroupDescriptionEntryFormat1.getDot(), p);
        return reportGroupDescriptionEntryFormat1;
    }

    public Cobol visitReportGroupDescriptionEntryFormat2(Cobol.ReportGroupDescriptionEntryFormat2 reportGroupDescriptionEntryFormat2, PrintOutputCapture<P> p) {
        visitSpace(reportGroupDescriptionEntryFormat2.getPrefix(), Space.Location.REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_2_PREFIX, p);
        visitMarkers(reportGroupDescriptionEntryFormat2.getMarkers(), p);
        visit(reportGroupDescriptionEntryFormat2.getIntegerLiteral(), p);
        visit(reportGroupDescriptionEntryFormat2.getDataName(), p);
        visit(reportGroupDescriptionEntryFormat2.getReportGroupLineNumberClause(), p);
        visit(reportGroupDescriptionEntryFormat2.getGroupUsageClause(), p);
        visit(reportGroupDescriptionEntryFormat2.getDot(), p);
        return reportGroupDescriptionEntryFormat2;
    }

    public Cobol visitReportGroupDescriptionEntryFormat3(Cobol.ReportGroupDescriptionEntryFormat3 reportGroupDescriptionEntryFormat3, PrintOutputCapture<P> p) {
        visitSpace(reportGroupDescriptionEntryFormat3.getPrefix(), Space.Location.REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_3_PREFIX, p);
        visitMarkers(reportGroupDescriptionEntryFormat3.getMarkers(), p);
        visit(reportGroupDescriptionEntryFormat3.getIntegerLiteral(), p);
        visit(reportGroupDescriptionEntryFormat3.getDataName(), p);
        visit(reportGroupDescriptionEntryFormat3.getClauses(), p);
        visit(reportGroupDescriptionEntryFormat3.getDot(), p);
        return reportGroupDescriptionEntryFormat3;
    }

    public Cobol visitReportGroupIndicateClause(Cobol.ReportGroupIndicateClause reportGroupIndicateClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupIndicateClause.getPrefix(), Space.Location.REPORT_GROUP_INDICATOR_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupIndicateClause.getMarkers(), p);
        visit(reportGroupIndicateClause.getWords(), p);
        return reportGroupIndicateClause;
    }

    public Cobol visitReportGroupJustifiedClause(Cobol.ReportGroupJustifiedClause reportGroupJustifiedClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupJustifiedClause.getPrefix(), Space.Location.REPORT_GROUP_JUSTIFIED_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupJustifiedClause.getMarkers(), p);
        visit(reportGroupJustifiedClause.getWords(), p);
        return reportGroupJustifiedClause;
    }

    public Cobol visitReportGroupLineNumberClause(Cobol.ReportGroupLineNumberClause reportGroupLineNumberClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupLineNumberClause.getPrefix(), Space.Location.REPORT_GROUP_LINE_NUMBER_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupLineNumberClause.getMarkers(), p);
        visit(reportGroupLineNumberClause.getWords(), p);
        visit(reportGroupLineNumberClause.getClause(), p);
        return reportGroupLineNumberClause;
    }

    public Cobol visitReportGroupLineNumberNextPage(Cobol.ReportGroupLineNumberNextPage reportGroupLineNumberNextPage, PrintOutputCapture<P> p) {
        visitSpace(reportGroupLineNumberNextPage.getPrefix(), Space.Location.REPORT_GROUP_LINE_NUMBER_NEXT_PAGE_PREFIX, p);
        visitMarkers(reportGroupLineNumberNextPage.getMarkers(), p);
        visit(reportGroupLineNumberNextPage.getIntegerLiteral(), p);
        visit(reportGroupLineNumberNextPage.getWords(), p);
        return reportGroupLineNumberNextPage;
    }

    public Cobol visitReportGroupLineNumberPlus(Cobol.ReportGroupLineNumberPlus reportGroupLineNumberPlus, PrintOutputCapture<P> p) {
        visitSpace(reportGroupLineNumberPlus.getPrefix(), Space.Location.REPORT_GROUP_LINE_NUMBER_PLUS_PREFIX, p);
        visitMarkers(reportGroupLineNumberPlus.getMarkers(), p);
        visit(reportGroupLineNumberPlus.getPlus(), p);
        visit(reportGroupLineNumberPlus.getIntegerLiteral(), p);
        return reportGroupLineNumberPlus;
    }

    public Cobol visitReportGroupNextGroupClause(Cobol.ReportGroupNextGroupClause reportGroupNextGroupClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupNextGroupClause.getPrefix(), Space.Location.REPORT_GROUP_NEXT_GROUP_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupNextGroupClause.getMarkers(), p);
        visit(reportGroupNextGroupClause.getWords(), p);
        visit(reportGroupNextGroupClause.getClause(), p);
        return reportGroupNextGroupClause;
    }

    public Cobol visitReportGroupNextGroupNextPage(Cobol.ReportGroupNextGroupNextPage reportGroupNextGroupNextPage, PrintOutputCapture<P> p) {
        visitSpace(reportGroupNextGroupNextPage.getPrefix(), Space.Location.REPORT_GROUP_NEXT_GROUP_NEXT_PAGE_PREFIX, p);
        visitMarkers(reportGroupNextGroupNextPage.getMarkers(), p);
        visit(reportGroupNextGroupNextPage.getNextPage(), p);
        return reportGroupNextGroupNextPage;
    }

    public Cobol visitReportGroupNextGroupPlus(Cobol.ReportGroupNextGroupPlus reportGroupNextGroupPlus, PrintOutputCapture<P> p) {
        visitSpace(reportGroupNextGroupPlus.getPrefix(), Space.Location.REPORT_GROUP_NEXT_GROUP_PLUS_PREFIX, p);
        visitMarkers(reportGroupNextGroupPlus.getMarkers(), p);
        visit(reportGroupNextGroupPlus.getPlus(), p);
        visit(reportGroupNextGroupPlus.getIntegerLiteral(), p);
        return reportGroupNextGroupPlus;
    }

    public Cobol visitReportGroupPictureClause(Cobol.ReportGroupPictureClause reportGroupPictureClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupPictureClause.getPrefix(), Space.Location.REPORT_GROUP_PICTURE_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupPictureClause.getMarkers(), p);
        visit(reportGroupPictureClause.getWords(), p);
        visit(reportGroupPictureClause.getPictureString(), p);
        return reportGroupPictureClause;
    }

    public Cobol visitReportGroupResetClause(Cobol.ReportGroupResetClause reportGroupResetClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupResetClause.getPrefix(), Space.Location.REPORT_GROUP_RESET_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupResetClause.getMarkers(), p);
        visit(reportGroupResetClause.getWords(), p);
        return reportGroupResetClause;
    }

    public Cobol visitReportGroupSignClause(Cobol.ReportGroupSignClause reportGroupSignClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupSignClause.getPrefix(), Space.Location.REPORT_GROUP_SIGN_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupSignClause.getMarkers(), p);
        visit(reportGroupSignClause.getWords(), p);
        return reportGroupSignClause;
    }

    public Cobol visitReportGroupSourceClause(Cobol.ReportGroupSourceClause reportGroupSourceClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupSourceClause.getPrefix(), Space.Location.REPORT_GROUP_SOURCE_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupSourceClause.getMarkers(), p);
        visit(reportGroupSourceClause.getWords(), p);
        visit(reportGroupSourceClause.getIdentifier(), p);
        return reportGroupSourceClause;
    }

    public Cobol visitReportGroupSumClause(Cobol.ReportGroupSumClause reportGroupSumClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupSumClause.getPrefix(), Space.Location.REPORT_GROUP_SUM_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupSumClause.getMarkers(), p);
        visit(reportGroupSumClause.getCobols(), p);
        return reportGroupSumClause;
    }

    public Cobol visitReportGroupTypeClause(Cobol.ReportGroupTypeClause reportGroupTypeClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupTypeClause.getPrefix(), Space.Location.REPORT_GROUP_TYPE_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupTypeClause.getMarkers(), p);
        visit(reportGroupTypeClause.getWords(), p);
        visit(reportGroupTypeClause.getType(), p);
        return reportGroupTypeClause;
    }

    public Cobol visitReportGroupTypeControlFooting(Cobol.ReportGroupTypeControlFooting reportGroupTypeControlFooting, PrintOutputCapture<P> p) {
        visitSpace(reportGroupTypeControlFooting.getPrefix(), Space.Location.REPORT_GROUP_TYPE_CONTROL_FOOTING_PREFIX, p);
        visitMarkers(reportGroupTypeControlFooting.getMarkers(), p);
        visit(reportGroupTypeControlFooting.getWords(), p);
        return reportGroupTypeControlFooting;
    }

    public Cobol visitReportGroupTypeControlHeading(Cobol.ReportGroupTypeControlHeading reportGroupTypeControlHeading, PrintOutputCapture<P> p) {
        visitSpace(reportGroupTypeControlHeading.getPrefix(), Space.Location.REPORT_GROUP_TYPE_CONTROL_HEADING_PREFIX, p);
        visitMarkers(reportGroupTypeControlHeading.getMarkers(), p);
        visit(reportGroupTypeControlHeading.getWords(), p);
        return reportGroupTypeControlHeading;
    }

    public Cobol visitReportGroupTypeDetail(Cobol.ReportGroupTypeDetail reportGroupTypeDetail, PrintOutputCapture<P> p) {
        visitSpace(reportGroupTypeDetail.getPrefix(), Space.Location.REPORT_GROUP_TYPE_DETAIL_PREFIX, p);
        visitMarkers(reportGroupTypeDetail.getMarkers(), p);
        visit(reportGroupTypeDetail.getWords(), p);
        return reportGroupTypeDetail;
    }

    public Cobol visitReportGroupTypePageFooting(Cobol.ReportGroupTypePageFooting reportGroupTypePageFooting, PrintOutputCapture<P> p) {
        visitSpace(reportGroupTypePageFooting.getPrefix(), Space.Location.REPORT_GROUP_TYPE_PAGE_FOOTING_PREFIX, p);
        visitMarkers(reportGroupTypePageFooting.getMarkers(), p);
        visit(reportGroupTypePageFooting.getWords(), p);
        return reportGroupTypePageFooting;
    }

    public Cobol visitReportGroupTypePageHeading(Cobol.ReportGroupTypePageHeading reportGroupTypePageHeading, PrintOutputCapture<P> p) {
        visitSpace(reportGroupTypePageHeading.getPrefix(), Space.Location.REPORT_GROUP_TYPE_REPORT_FOOTING_PREFIX, p);
        visitMarkers(reportGroupTypePageHeading.getMarkers(), p);
        visit(reportGroupTypePageHeading.getWords(), p);
        return reportGroupTypePageHeading;
    }

    public Cobol visitReportGroupTypeReportFooting(Cobol.ReportGroupTypeReportFooting reportGroupTypeReportFooting, PrintOutputCapture<P> p) {
        visitSpace(reportGroupTypeReportFooting.getPrefix(), Space.Location.REPORT_GROUP_TYPE_PAGE_HEADING_PREFIX, p);
        visitMarkers(reportGroupTypeReportFooting.getMarkers(), p);
        visit(reportGroupTypeReportFooting.getWords(), p);
        return reportGroupTypeReportFooting;
    }

    public Cobol visitReportGroupTypeReportHeading(Cobol.ReportGroupTypeReportHeading reportGroupTypeReportHeading, PrintOutputCapture<P> p) {
        visitSpace(reportGroupTypeReportHeading.getPrefix(), Space.Location.REPORT_GROUP_TYPE_REPORT_HEADING_PREFIX, p);
        visitMarkers(reportGroupTypeReportHeading.getMarkers(), p);
        visit(reportGroupTypeReportHeading.getWords(), p);
        return reportGroupTypeReportHeading;
    }

    public Cobol visitReportGroupUsageClause(Cobol.ReportGroupUsageClause reportGroupUsageClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupUsageClause.getPrefix(), Space.Location.REPORT_GROUP_USAGE_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupUsageClause.getMarkers(), p);
        visit(reportGroupUsageClause.getWords(), p);
        return reportGroupUsageClause;
    }

    public Cobol visitReportGroupValueClause(Cobol.ReportGroupValueClause reportGroupValueClause, PrintOutputCapture<P> p) {
        visitSpace(reportGroupValueClause.getPrefix(), Space.Location.REPORT_GROUP_VALUE_CLAUSE_PREFIX, p);
        visitMarkers(reportGroupValueClause.getMarkers(), p);
        visit(reportGroupValueClause.getWords(), p);
        visit(reportGroupValueClause.getLiteral(), p);
        return reportGroupValueClause;
    }

    public Cobol visitReportName(Cobol.ReportName reportName, PrintOutputCapture<P> p) {
        visitSpace(reportName.getPrefix(), Space.Location.REPORT_NAME_PREFIX, p);
        visitMarkers(reportName.getMarkers(), p);
        visit(reportName.getQualifiedDataName(), p);
        return reportName;
    }

    public Cobol visitReportSection(Cobol.ReportSection reportSection, PrintOutputCapture<P> p) {
        visitSpace(reportSection.getPrefix(), Space.Location.REPORT_SECTION_PREFIX, p);
        visitMarkers(reportSection.getMarkers(), p);
        visit(reportSection.getWords(), p);
        visit(reportSection.getDescriptions(), p);
        return reportSection;
    }

    public Cobol visitRerunClause(Cobol.RerunClause rerunClause, PrintOutputCapture<P> p) {
        visitSpace(rerunClause.getPrefix(), Space.Location.RERUN_CLAUSE_PREFIX, p);
        visitMarkers(rerunClause.getMarkers(), p);
        visit(rerunClause.getRerun(), p);
        visit(rerunClause.getOn(), p);
        visit(rerunClause.getName(), p);
        visit(rerunClause.getEvery(), p);
        visit(rerunClause.getAction(), p);
        return rerunClause;
    }

    public Cobol visitRerunEveryClock(Cobol.RerunEveryClock rerunEveryClock, PrintOutputCapture<P> p) {
        visitSpace(rerunEveryClock.getPrefix(), Space.Location.RERUN_EVERY_CLOCK_PREFIX, p);
        visitMarkers(rerunEveryClock.getMarkers(), p);
        visit(rerunEveryClock.getIntegerLiteral(), p);
        visit(rerunEveryClock.getClockUnits(), p);
        return rerunEveryClock;
    }

    public Cobol visitRerunEveryOf(Cobol.RerunEveryOf rerunEveryOf, PrintOutputCapture<P> p) {
        visitSpace(rerunEveryOf.getPrefix(), Space.Location.RERUN_EVERY_OF_PREFIX, p);
        visitMarkers(rerunEveryOf.getMarkers(), p);
        visit(rerunEveryOf.getRecords(), p);
        visit(rerunEveryOf.getFileName(), p);
        return rerunEveryOf;
    }

    public Cobol visitRerunEveryRecords(Cobol.RerunEveryRecords rerunEveryRecords, PrintOutputCapture<P> p) {
        visitSpace(rerunEveryRecords.getPrefix(), Space.Location.RERUN_EVERY_RECORDS_PREFIX, p);
        visitMarkers(rerunEveryRecords.getMarkers(), p);
        visit(rerunEveryRecords.getIntegerLiteral(), p);
        visit(rerunEveryRecords.getRecords(), p);
        return rerunEveryRecords;
    }

    public Cobol visitReserveClause(Cobol.ReserveClause reserveClause, PrintOutputCapture<P> p) {
        visitSpace(reserveClause.getPrefix(), Space.Location.RERUN_RESERVE_CLAUSE_PREFIX, p);
        visitMarkers(reserveClause.getMarkers(), p);
        visit(reserveClause.getWords(), p);
        return reserveClause;
    }

    public Cobol visitReserveNetworkClause(Cobol.ReserveNetworkClause reserveNetworkClause, PrintOutputCapture<P> p) {
        visitSpace(reserveNetworkClause.getPrefix(), Space.Location.RESERVE_NETWORK_CLAUSE_PREFIX, p);
        visitMarkers(reserveNetworkClause.getMarkers(), p);
        visit(reserveNetworkClause.getWords(), p);
        return reserveNetworkClause;
    }

    @Override
    public Cobol visitReturn(Cobol.Return r, PrintOutputCapture<P> p) {
        visitSpace(r.getPrefix(), Space.Location.RETURN_PREFIX, p);
        visitMarkers(r.getMarkers(), p);
        visit(r.getWord(), p);
        visit(r.getFileName(), p);
        visit(r.getRecord(), p);
        visit(r.getInto(), p);
        visit(r.getAtEndPhrase(), p);
        visit(r.getNotAtEndPhrase(), p);
        visit(r.getEndReturn(), p);
        return r;
    }

    @Override
    public Cobol visitReturnInto(Cobol.ReturnInto r, PrintOutputCapture<P> p) {
        visitSpace(r.getPrefix(), Space.Location.RETURN_INTO_PREFIX, p);
        visitMarkers(r.getMarkers(), p);
        visit(r.getInto(), p);
        visit(r.getQualifiedDataName(), p);
        return r;
    }

    public Cobol visitRewrite(Cobol.Rewrite rewrite, PrintOutputCapture<P> p) {
        visitSpace(rewrite.getPrefix(), Space.Location.REWRITE_PREFIX, p);
        visitMarkers(rewrite.getMarkers(), p);
        visit(rewrite.getRewrite(), p);
        visit(rewrite.getRecordName(), p);
        visit(rewrite.getRewriteFrom(), p);
        visit(rewrite.getInvalidKeyPhrase(), p);
        visit(rewrite.getNotInvalidKeyPhrase(), p);
        visit(rewrite.getEndRewrite(), p);
        return rewrite;
    }

    public Cobol visitRewriteFrom(Cobol.RewriteFrom rewriteFrom, PrintOutputCapture<P> p) {
        visitSpace(rewriteFrom.getPrefix(), Space.Location.REWRITE_FROM_PREFIX, p);
        visitMarkers(rewriteFrom.getMarkers(), p);
        visit(rewriteFrom.getFrom(), p);
        visit(rewriteFrom.getIdentifier(), p);
        return rewriteFrom;
    }

    public Cobol visitRoundable(Cobol.Roundable roundable, PrintOutputCapture<P> p) {
        visitSpace(roundable.getPrefix(), Space.Location.ROUNDABLE_PREFIX, p);
        visitMarkers(roundable.getMarkers(), p);
        visit(roundable.getIdentifier(), p);
        visit(roundable.getRounded(), p);
        return roundable;
    }

    public Cobol visitSameClause(Cobol.SameClause sameClause, PrintOutputCapture<P> p) {
        visitSpace(sameClause.getPrefix(), Space.Location.SAME_CLAUSE_PREFIX, p);
        visitMarkers(sameClause.getMarkers(), p);
        visit(sameClause.getWords(), p);
        visit(sameClause.getFileNames(), p);
        return sameClause;
    }

    public Cobol visitScreenDescriptionAutoClause(Cobol.ScreenDescriptionAutoClause screenDescriptionAutoClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionAutoClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_AUTO_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionAutoClause.getMarkers(), p);
        visit(screenDescriptionAutoClause.getAuto(), p);
        return screenDescriptionAutoClause;
    }

    public Cobol visitScreenDescriptionBackgroundColorClause(Cobol.ScreenDescriptionBackgroundColorClause screenDescriptionBackgroundColorClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionBackgroundColorClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_BACKGROUND_COLOR_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionBackgroundColorClause.getMarkers(), p);
        visit(screenDescriptionBackgroundColorClause.getBackground(), p);
        visit(screenDescriptionBackgroundColorClause.getIs(), p);
        visit(screenDescriptionBackgroundColorClause.getValue(), p);
        return screenDescriptionBackgroundColorClause;
    }

    public Cobol visitScreenDescriptionBellClause(Cobol.ScreenDescriptionBellClause screenDescriptionBellClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionBellClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_BELL_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionBellClause.getMarkers(), p);
        visit(screenDescriptionBellClause.getBell(), p);
        return screenDescriptionBellClause;
    }

    public Cobol visitScreenDescriptionBlankClause(Cobol.ScreenDescriptionBlankClause screenDescriptionBlankClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionBlankClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_BLANK_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionBlankClause.getMarkers(), p);
        visit(screenDescriptionBlankClause.getWords(), p);
        return screenDescriptionBlankClause;
    }

    public Cobol visitScreenDescriptionBlankWhenZeroClause(Cobol.ScreenDescriptionBlankWhenZeroClause screenDescriptionBlankWhenZeroClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionBlankWhenZeroClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_BLANK_WHEN_ZERO_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionBlankWhenZeroClause.getMarkers(), p);
        visit(screenDescriptionBlankWhenZeroClause.getWords(), p);
        return screenDescriptionBlankWhenZeroClause;
    }

    public Cobol visitScreenDescriptionBlinkClause(Cobol.ScreenDescriptionBlinkClause screenDescriptionBlinkClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionBlinkClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_BLINK_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionBlinkClause.getMarkers(), p);
        visit(screenDescriptionBlinkClause.getBlink(), p);
        return screenDescriptionBlinkClause;
    }

    public Cobol visitScreenDescriptionColumnClause(Cobol.ScreenDescriptionColumnClause screenDescriptionColumnClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionColumnClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_COLUMN_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionColumnClause.getMarkers(), p);
        visit(screenDescriptionColumnClause.getWords(), p);
        visit(screenDescriptionColumnClause.getValue(), p);
        return screenDescriptionColumnClause;
    }

    public Cobol visitScreenDescriptionControlClause(Cobol.ScreenDescriptionControlClause screenDescriptionControlClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionControlClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_CONTROL_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionControlClause.getMarkers(), p);
        visit(screenDescriptionControlClause.getWords(), p);
        visit(screenDescriptionControlClause.getValue(), p);
        return screenDescriptionControlClause;
    }

    public Cobol visitScreenDescriptionEntry(Cobol.ScreenDescriptionEntry screenDescriptionEntry, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionEntry.getPrefix(), Space.Location.SCREEN_DESCRIPTION_ENTRY_PREFIX, p);
        visitMarkers(screenDescriptionEntry.getMarkers(), p);
        visit(screenDescriptionEntry.getWord(), p);
        visit(screenDescriptionEntry.getName(), p);
        visit(screenDescriptionEntry.getClauses(), p);
        visit(screenDescriptionEntry.getDot(), p);
        return screenDescriptionEntry;
    }

    public Cobol visitScreenDescriptionEraseClause(Cobol.ScreenDescriptionEraseClause screenDescriptionEraseClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionEraseClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_ERASE_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionEraseClause.getMarkers(), p);
        visit(screenDescriptionEraseClause.getWords(), p);
        return screenDescriptionEraseClause;
    }

    public Cobol visitScreenDescriptionForegroundColorClause(Cobol.ScreenDescriptionForegroundColorClause screenDescriptionForegroundColorClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionForegroundColorClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_FOREGROUND_COLOR_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionForegroundColorClause.getMarkers(), p);
        visit(screenDescriptionForegroundColorClause.getWords(), p);
        visit(screenDescriptionForegroundColorClause.getValue(), p);
        return screenDescriptionForegroundColorClause;
    }

    public Cobol visitScreenDescriptionFromClause(Cobol.ScreenDescriptionFromClause screenDescriptionFromClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionFromClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_FROM_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionFromClause.getMarkers(), p);
        visit(screenDescriptionFromClause.getFrom(), p);
        visit(screenDescriptionFromClause.getValue(), p);
        visit(screenDescriptionFromClause.getScreenDescriptionToClause(), p);
        return screenDescriptionFromClause;
    }

    public Cobol visitScreenDescriptionFullClause(Cobol.ScreenDescriptionFullClause screenDescriptionFullClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionFullClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_FULL_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionFullClause.getMarkers(), p);
        visit(screenDescriptionFullClause.getWord(), p);
        return screenDescriptionFullClause;
    }

    public Cobol visitScreenDescriptionGridClause(Cobol.ScreenDescriptionGridClause screenDescriptionGridClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionGridClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_GRID_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionGridClause.getMarkers(), p);
        visit(screenDescriptionGridClause.getWord(), p);
        return screenDescriptionGridClause;
    }

    public Cobol visitScreenDescriptionJustifiedClause(Cobol.ScreenDescriptionJustifiedClause screenDescriptionJustifiedClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionJustifiedClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_JUSTIFIED_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionJustifiedClause.getMarkers(), p);
        visit(screenDescriptionJustifiedClause.getWords(), p);
        return screenDescriptionJustifiedClause;
    }

    public Cobol visitScreenDescriptionLightClause(Cobol.ScreenDescriptionLightClause screenDescriptionLightClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionLightClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_LIGHT_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionLightClause.getMarkers(), p);
        visit(screenDescriptionLightClause.getLight(), p);
        return screenDescriptionLightClause;
    }

    public Cobol visitScreenDescriptionLineClause(Cobol.ScreenDescriptionLineClause screenDescriptionLineClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionLineClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_LINE_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionLineClause.getMarkers(), p);
        visit(screenDescriptionLineClause.getWords(), p);
        visit(screenDescriptionLineClause.getValue(), p);
        return screenDescriptionLineClause;
    }

    public Cobol visitScreenDescriptionPictureClause(Cobol.ScreenDescriptionPictureClause screenDescriptionPictureClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionPictureClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_PICTURE_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionPictureClause.getMarkers(), p);
        visit(screenDescriptionPictureClause.getWords(), p);
        visit(screenDescriptionPictureClause.getPictureString(), p);
        return screenDescriptionPictureClause;
    }

    public Cobol visitScreenDescriptionPromptClause(Cobol.ScreenDescriptionPromptClause screenDescriptionPromptClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionPromptClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_PROMPT_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionPromptClause.getMarkers(), p);
        visit(screenDescriptionPromptClause.getWords(), p);
        visit(screenDescriptionPromptClause.getName(), p);
        visit(screenDescriptionPromptClause.getScreenDescriptionPromptOccursClause(), p);
        return screenDescriptionPromptClause;
    }

    public Cobol visitScreenDescriptionPromptOccursClause(Cobol.ScreenDescriptionPromptOccursClause screenDescriptionPromptOccursClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionPromptOccursClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_PROMPT_OCCURS_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionPromptOccursClause.getMarkers(), p);
        visit(screenDescriptionPromptOccursClause.getOccurs(), p);
        visit(screenDescriptionPromptOccursClause.getInteger(), p);
        visit(screenDescriptionPromptOccursClause.getTimes(), p);
        return screenDescriptionPromptOccursClause;
    }

    public Cobol visitScreenDescriptionRequiredClause(Cobol.ScreenDescriptionRequiredClause screenDescriptionRequiredClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionRequiredClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_REQUIRED_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionRequiredClause.getMarkers(), p);
        visit(screenDescriptionRequiredClause.getRequired(), p);
        return screenDescriptionRequiredClause;
    }

    public Cobol visitScreenDescriptionReverseVideoClause(Cobol.ScreenDescriptionReverseVideoClause screenDescriptionReverseVideoClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionReverseVideoClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_REVERSE_VIDEO_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionReverseVideoClause.getMarkers(), p);
        visit(screenDescriptionReverseVideoClause.getWord(), p);
        return screenDescriptionReverseVideoClause;
    }

    public Cobol visitScreenDescriptionSecureClause(Cobol.ScreenDescriptionSecureClause screenDescriptionSecureClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionSecureClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_SECURE_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionSecureClause.getMarkers(), p);
        visit(screenDescriptionSecureClause.getWord(), p);
        return screenDescriptionSecureClause;
    }

    public Cobol visitScreenDescriptionSignClause(Cobol.ScreenDescriptionSignClause screenDescriptionSignClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionSignClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_SIGN_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionSignClause.getMarkers(), p);
        visit(screenDescriptionSignClause.getWords(), p);
        return screenDescriptionSignClause;
    }

    public Cobol visitScreenDescriptionSizeClause(Cobol.ScreenDescriptionSizeClause screenDescriptionSizeClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionSizeClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_SIZE_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionSizeClause.getMarkers(), p);
        visit(screenDescriptionSizeClause.getWords(), p);
        visit(screenDescriptionSizeClause.getValue(), p);
        return screenDescriptionSizeClause;
    }

    public Cobol visitScreenDescriptionToClause(Cobol.ScreenDescriptionToClause screenDescriptionToClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionToClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_TO_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionToClause.getMarkers(), p);
        visit(screenDescriptionToClause.getTo(), p);
        visit(screenDescriptionToClause.getIdentifier(), p);
        return screenDescriptionToClause;
    }

    public Cobol visitScreenDescriptionUnderlineClause(Cobol.ScreenDescriptionUnderlineClause screenDescriptionUnderlineClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionUnderlineClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_UNDERLINE_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionUnderlineClause.getMarkers(), p);
        visit(screenDescriptionUnderlineClause.getUnderline(), p);
        return screenDescriptionUnderlineClause;
    }

    public Cobol visitScreenDescriptionUsageClause(Cobol.ScreenDescriptionUsageClause screenDescriptionUsageClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionUsageClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_USAGE_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionUsageClause.getMarkers(), p);
        visit(screenDescriptionUsageClause.getWords(), p);
        return screenDescriptionUsageClause;
    }

    public Cobol visitScreenDescriptionUsingClause(Cobol.ScreenDescriptionUsingClause screenDescriptionUsingClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionUsingClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_USING_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionUsingClause.getMarkers(), p);
        visit(screenDescriptionUsingClause.getUsing(), p);
        visit(screenDescriptionUsingClause.getIdentifier(), p);
        return screenDescriptionUsingClause;
    }

    public Cobol visitScreenDescriptionValueClause(Cobol.ScreenDescriptionValueClause screenDescriptionValueClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionValueClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_VALUE_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionValueClause.getMarkers(), p);
        visit(screenDescriptionValueClause.getWords(), p);
        visit(screenDescriptionValueClause.getValue(), p);
        return screenDescriptionValueClause;
    }

    public Cobol visitScreenDescriptionZeroFillClause(Cobol.ScreenDescriptionZeroFillClause screenDescriptionZeroFillClause, PrintOutputCapture<P> p) {
        visitSpace(screenDescriptionZeroFillClause.getPrefix(), Space.Location.SCREEN_DESCRIPTION_ZERO_FILL_CLAUSE_PREFIX, p);
        visitMarkers(screenDescriptionZeroFillClause.getMarkers(), p);
        visit(screenDescriptionZeroFillClause.getWord(), p);
        return screenDescriptionZeroFillClause;
    }

    public Cobol visitScreenSection(Cobol.ScreenSection screenSection, PrintOutputCapture<P> p) {
        visitSpace(screenSection.getPrefix(), Space.Location.SCREEN_SECTION_PREFIX, p);
        visitMarkers(screenSection.getMarkers(), p);
        visit(screenSection.getWords(), p);
        visit(screenSection.getDot(), p);
        visit(screenSection.getDescriptions(), p);
        return screenSection;
    }

    @Override
    public Cobol visitSearch(Cobol.Search s, PrintOutputCapture<P> p) {
        visitSpace(s.getPrefix(), Space.Location.SEARCH_PREFIX, p);
        visitMarkers(s.getMarkers(), p);
        visit(s.getWords(), p);
        visit(s.getQualifiedDataName(), p);
        visit(s.getSearchVarying(), p);
        visit(s.getAtEndPhrase(), p);
        visit(s.getSearchWhen(), p);
        visit(s.getEndSearch(), p);
        return s;
    }

    @Override
    public Cobol visitSearchVarying(Cobol.SearchVarying s, PrintOutputCapture<P> p) {
        visitSpace(s.getPrefix(), Space.Location.SEARCH_VARYING_PREFIX, p);
        visitMarkers(s.getMarkers(), p);
        visit(s.getVarying(), p);
        visit(s.getQualifiedDataName(), p);
        return s;
    }

    @Override
    public Cobol visitSearchWhen(Cobol.SearchWhen s, PrintOutputCapture<P> p) {
        visitSpace(s.getPrefix(), Space.Location.SEARCH_WHEN_PREFIX, p);
        visitMarkers(s.getMarkers(), p);
        visit(s.getWhen(), p);
        visit(s.getCondition(), p);
        visit(s.getNextSentence(), p);
        visit(s.getStatements(), p);
        return s;
    }

    public Cobol visitSelectClause(Cobol.SelectClause selectClause, PrintOutputCapture<P> p) {
        visitSpace(selectClause.getPrefix(), Space.Location.SEARCH_CLAUSE_PREFIX, p);
        visitMarkers(selectClause.getMarkers(), p);
        visit(selectClause.getWords(), p);
        visit(selectClause.getFileName(), p);
        return selectClause;
    }

    @Override
    public Cobol visitSend(Cobol.Send s, PrintOutputCapture<P> p) {
        visitSpace(s.getPrefix(), Space.Location.SEND_PREFIX, p);
        visitMarkers(s.getMarkers(), p);
        visit(s.getSend(), p);
        visit(s.getStatement(), p);
        visit(s.getOnExceptionClause(), p);
        visit(s.getNotOnExceptionClause(), p);
        return s;
    }

    public Cobol visitSendAdvancingLines(Cobol.SendAdvancingLines s, PrintOutputCapture<P> p) {
        visitSpace(s.getPrefix(), Space.Location.SEND_ADVANCING_LINES_PREFIX, p);
        visitMarkers(s.getMarkers(), p);
        visit(s.getName(), p);
        visit(s.getLines(), p);
        return s;
    }

    @Override
    public Cobol visitSendPhrase(Cobol.SendPhrase s, PrintOutputCapture<P> p) {
        visitSpace(s.getPrefix(), Space.Location.SEND_PHRASE_PREFIX, p);
        visitMarkers(s.getMarkers(), p);
        visit(s.getWords(), p);
        visit(s.getTarget(), p);
        return s;
    }

    @Override
    public Cobol visitSendStatementSync(Cobol.SendStatementSync s, PrintOutputCapture<P> p) {
        visitSpace(s.getPrefix(), Space.Location.SEND_STATEMENT_SYNC_PREFIX, p);
        visitMarkers(s.getMarkers(), p);
        visit(s.getName(), p);
        visit(s.getSendFromPhrase(), p);
        visit(s.getSendWithPhrase(), p);
        visit(s.getSendReplacingPhrase(), p);
        visit(s.getSendAdvancingPhrase(), p);
        return s;
    }

    public Cobol visitSentence(Cobol.Sentence sentence, PrintOutputCapture<P> p) {
        visitSpace(sentence.getPrefix(), Space.Location.SENTENCE_PREFIX, p);
        visitMarkers(sentence.getMarkers(), p);
        visit(sentence.getStatements(), p);
        visit(sentence.getDot(), p);
        return sentence;
    }

    public Cobol visitSet(Cobol.Set set, PrintOutputCapture<P> p) {
        visitSpace(set.getPrefix(), Space.Location.SET_PREFIX, p);
        visitMarkers(set.getMarkers(), p);
        visit(set.getSet(), p);
        visit(set.getTo(), p);
        visit(set.getUpDown(), p);
        return set;
    }

    public Cobol visitSetTo(Cobol.SetTo setTo, PrintOutputCapture<P> p) {
        visitSpace(setTo.getPrefix(), Space.Location.SET_TO_PREFIX, p);
        visitMarkers(setTo.getMarkers(), p);
        visit(setTo.getIdentifiers(), p);
        visit(setTo.getTo(), p);
        visit(setTo.getValues(), p);
        return setTo;
    }

    public Cobol visitSetUpDown(Cobol.SetUpDown setUpDown, PrintOutputCapture<P> p) {
        visitSpace(setUpDown.getPrefix(), Space.Location.SET_UP_DOWN_PREFIX, p);
        visitMarkers(setUpDown.getMarkers(), p);
        visit(setUpDown.getTo(), p);
        visit(setUpDown.getOperation(), p);
        visit(setUpDown.getValue(), p);
        return setUpDown;
    }

    public Cobol visitSort(Cobol.Sort sort, PrintOutputCapture<P> p) {
        visitSpace(sort.getPrefix(), Space.Location.SORT_PREFIX, p);
        visitMarkers(sort.getMarkers(), p);
        visit(sort.getSort(), p);
        visit(sort.getFileName(), p);
        visit(sort.getSortOnKeyClause(), p);
        visit(sort.getSortDuplicatesPhrase(), p);
        visit(sort.getSortCollatingSequencePhrase(), p);
        visit(sort.getSortInputProcedurePhrase(), p);
        visit(sort.getSortUsing(), p);
        visit(sort.getSortOutputProcedurePhrase(), p);
        visit(sort.getSortGiving(), p);
        return sort;
    }

    public Cobol visitSortCollatingSequencePhrase(Cobol.SortCollatingSequencePhrase sortCollatingSequencePhrase, PrintOutputCapture<P> p) {
        visitSpace(sortCollatingSequencePhrase.getPrefix(), Space.Location.SORT_COLLATING_SEQUENCE_PHRASE_PREFIX, p);
        visitMarkers(sortCollatingSequencePhrase.getMarkers(), p);
        visit(sortCollatingSequencePhrase.getWords(), p);
        visit(sortCollatingSequencePhrase.getAlphabetNames(), p);
        visit(sortCollatingSequencePhrase.getSortCollatingAlphanumeric(), p);
        visit(sortCollatingSequencePhrase.getSortCollatingNational(), p);
        return sortCollatingSequencePhrase;
    }

    public Cobol visitSortGiving(Cobol.SortGiving sortGiving, PrintOutputCapture<P> p) {
        visitSpace(sortGiving.getPrefix(), Space.Location.SORT_GIVING_PREFIX, p);
        visitMarkers(sortGiving.getMarkers(), p);
        visit(sortGiving.getFileName(), p);
        visit(sortGiving.getWords(), p);
        return sortGiving;
    }

    public Cobol visitSortProcedurePhrase(Cobol.SortProcedurePhrase sortProcedurePhrase, PrintOutputCapture<P> p) {
        visitSpace(sortProcedurePhrase.getPrefix(), Space.Location.SORT_PROCEDURE_PHRASE_PREFIX, p);
        visitMarkers(sortProcedurePhrase.getMarkers(), p);
        visit(sortProcedurePhrase.getWords(), p);
        visit(sortProcedurePhrase.getProcedureName(), p);
        visit(sortProcedurePhrase.getSortInputThrough(), p);
        return sortProcedurePhrase;
    }

    public Cobol visitSortable(Cobol.Sortable sortable, PrintOutputCapture<P> p) {
        visitSpace(sortable.getPrefix(), Space.Location.SORTABLE_PREFIX, p);
        visitMarkers(sortable.getMarkers(), p);
        visit(sortable.getWords(), p);
        visit(sortable.getNames(), p);
        return sortable;
    }

    public Cobol visitSourceComputer(Cobol.SourceComputer sourceComputer, PrintOutputCapture<P> p) {
        visitSpace(sourceComputer.getPrefix(), Space.Location.SOURCE_COMPUTER_PREFIX, p);
        visitMarkers(sourceComputer.getMarkers(), p);
        visit(sourceComputer.getWords(), p);
        visit(sourceComputer.getComputer(), p);
        return sourceComputer;
    }

    public Cobol visitSourceComputerDefinition(Cobol.SourceComputerDefinition sourceComputerDefinition, PrintOutputCapture<P> p) {
        visitSpace(sourceComputerDefinition.getPrefix(), Space.Location.SOURCE_COMPUTER_DEFINITION_PREFIX, p);
        visitMarkers(sourceComputerDefinition.getMarkers(), p);
        visit(sourceComputerDefinition.getComputerName(), p);
        visit(sourceComputerDefinition.getDebuggingMode(), p);
        visit(sourceComputerDefinition.getDot(), p);
        return sourceComputerDefinition;
    }

    @Override
    public Space visitSpace(Space space, Space.Location location, PrintOutputCapture<P> p) {
        p.append(space.getWhitespace());
        return space;
    }

    public Cobol visitSpecialNames(Cobol.SpecialNames specialNames, PrintOutputCapture<P> p) {
        visitSpace(specialNames.getPrefix(), Space.Location.SPECIAL_NAMES_PREFIX, p);
        visitMarkers(specialNames.getMarkers(), p);
        visit(specialNames.getWord(), p);
        visit(specialNames.getDot(), p);
        visit(specialNames.getClauses(), p);
        visit(specialNames.getDot2(), p);
        return specialNames;
    }

    public Cobol visitStart(Cobol.Start start, PrintOutputCapture<P> p) {
        visitSpace(start.getPrefix(), Space.Location.START_PREFIX, p);
        visitMarkers(start.getMarkers(), p);
        visit(start.getStart(), p);
        visit(start.getFileName(), p);
        visit(start.getStartKey(), p);
        visit(start.getInvalidKeyPhrase(), p);
        visit(start.getNotInvalidKeyPhrase(), p);
        visit(start.getEndStart(), p);
        return start;
    }

    public Cobol visitStartKey(Cobol.StartKey startKey, PrintOutputCapture<P> p) {
        visitSpace(startKey.getPrefix(), Space.Location.START_KEY_PREFIX, p);
        visitMarkers(startKey.getMarkers(), p);
        visit(startKey.getWords(), p);
        visit(startKey.getQualifiedDataName(), p);
        return startKey;
    }

    public Cobol visitStatementPhrase(Cobol.StatementPhrase statementPhrase, PrintOutputCapture<P> p) {
        visitSpace(statementPhrase.getPrefix(), Space.Location.STATEMENT_PHRASE_PREFIX, p);
        visitMarkers(statementPhrase.getMarkers(), p);
        visit(statementPhrase.getPhrases(), p);
        visit(statementPhrase.getStatements(), p);
        return statementPhrase;
    }

    public Cobol visitStatusKeyClause(Cobol.StatusKeyClause statusKeyClause, PrintOutputCapture<P> p) {
        visitSpace(statusKeyClause.getPrefix(), Space.Location.STATUS_KEY_CLAUSE_PREFIX, p);
        visitMarkers(statusKeyClause.getMarkers(), p);
        visit(statusKeyClause.getWords(), p);
        visit(statusKeyClause.getName(), p);
        return statusKeyClause;
    }

    public Cobol visitStop(Cobol.Stop stop, PrintOutputCapture<P> p) {
        visitSpace(stop.getPrefix(), Space.Location.STOP_PREFIX, p);
        visitMarkers(stop.getMarkers(), p);
        visit(stop.getWords(), p);
        visit(stop.getStatement(), p);
        return stop;
    }

    public Cobol visitStopStatementGiving(Cobol.StopStatementGiving stopStatementGiving, PrintOutputCapture<P> p) {
        visitSpace(stopStatementGiving.getPrefix(), Space.Location.STOP_STATEMENT_GIVING_PREFIX, p);
        visitMarkers(stopStatementGiving.getMarkers(), p);
        visit(stopStatementGiving.getWords(), p);
        visit(stopStatementGiving.getName(), p);
        return stopStatementGiving;
    }

    public Cobol visitStringDelimitedByPhrase(Cobol.StringDelimitedByPhrase stringDelimitedByPhrase, PrintOutputCapture<P> p) {
        visitSpace(stringDelimitedByPhrase.getPrefix(), Space.Location.STRING_DELIMITED_BY_PHRASE_PREFIX, p);
        visitMarkers(stringDelimitedByPhrase.getMarkers(), p);
        visit(stringDelimitedByPhrase.getWords(), p);
        visit(stringDelimitedByPhrase.getIdentifier(), p);
        return stringDelimitedByPhrase;
    }

    public Cobol visitStringForPhrase(Cobol.StringForPhrase stringForPhrase, PrintOutputCapture<P> p) {
        visitSpace(stringForPhrase.getPrefix(), Space.Location.STRING_FOR_PHRASE_PREFIX, p);
        visitMarkers(stringForPhrase.getMarkers(), p);
        visit(stringForPhrase.getWord(), p);
        visit(stringForPhrase.getIdentifier(), p);
        return stringForPhrase;
    }

    public Cobol visitStringIntoPhrase(Cobol.StringIntoPhrase stringIntoPhrase, PrintOutputCapture<P> p) {
        visitSpace(stringIntoPhrase.getPrefix(), Space.Location.STRING_INTO_PHRASE_PREFIX, p);
        visitMarkers(stringIntoPhrase.getMarkers(), p);
        visit(stringIntoPhrase.getInto(), p);
        visit(stringIntoPhrase.getIdentifier(), p);
        return stringIntoPhrase;
    }

    public Cobol visitStringSendingPhrase(Cobol.StringSendingPhrase stringSendingPhrase, PrintOutputCapture<P> p) {
        visitSpace(stringSendingPhrase.getPrefix(), Space.Location.STRING_SENDING_PHRASE_PREFIX, p);
        visitMarkers(stringSendingPhrase.getMarkers(), p);
        visit(stringSendingPhrase.getSendings(), p);
        visit(stringSendingPhrase.getPhrase(), p);
        return stringSendingPhrase;
    }

    public Cobol visitStringStatement(Cobol.StringStatement stringStatement, PrintOutputCapture<P> p) {
        visitSpace(stringStatement.getPrefix(), Space.Location.STRING_STATEMENT_PREFIX, p);
        visitMarkers(stringStatement.getMarkers(), p);
        visit(stringStatement.getString(), p);
        visit(stringStatement.getStringSendingPhrases(), p);
        visit(stringStatement.getStringIntoPhrase(), p);
        visit(stringStatement.getStringWithPointerPhrase(), p);
        visit(stringStatement.getOnOverflowPhrase(), p);
        visit(stringStatement.getNotOnOverflowPhrase(), p);
        visit(stringStatement.getEndString(), p);
        return stringStatement;
    }

    public Cobol visitStringWithPointerPhrase(Cobol.StringWithPointerPhrase stringWithPointerPhrase, PrintOutputCapture<P> p) {
        visitSpace(stringWithPointerPhrase.getPrefix(), Space.Location.STRING_WITH_POINTER_PHRASE_PREFIX, p);
        visitMarkers(stringWithPointerPhrase.getMarkers(), p);
        visit(stringWithPointerPhrase.getWords(), p);
        visit(stringWithPointerPhrase.getQualifiedDataName(), p);
        return stringWithPointerPhrase;
    }

    public Cobol visitSubscript(Cobol.Subscript subscript, PrintOutputCapture<P> p) {
        visitSpace(subscript.getPrefix(), Space.Location.SUBSCRIPT_PREFIX, p);
        visitMarkers(subscript.getMarkers(), p);
        visit(subscript.getFirst(), p);
        visit(subscript.getSecond(), p);
        return subscript;
    }

    public Cobol visitSubtract(Cobol.Subtract subtract, PrintOutputCapture<P> p) {
        visitSpace(subtract.getPrefix(), Space.Location.SUBTRACT_PREFIX, p);
        visitMarkers(subtract.getMarkers(), p);
        visit(subtract.getSubstract(), p);
        visit(subtract.getOperation(), p);
        visit(subtract.getOnSizeErrorPhrase(), p);
        visit(subtract.getNotOnSizeErrorPhrase(), p);
        visit(subtract.getEndSubtract(), p);
        return subtract;
    }

    public Cobol visitSubtractCorrespondingStatement(Cobol.SubtractCorrespondingStatement subtractCorrespondingStatement, PrintOutputCapture<P> p) {
        visitSpace(subtractCorrespondingStatement.getPrefix(), Space.Location.SUBTRACT_CORRESPONDING_STATEMENT_PREFIX, p);
        visitMarkers(subtractCorrespondingStatement.getMarkers(), p);
        visit(subtractCorrespondingStatement.getCorresponding(), p);
        visit(subtractCorrespondingStatement.getQualifiedDataName(), p);
        visit(subtractCorrespondingStatement.getGiving(), p);
        visit(subtractCorrespondingStatement.getSubtractMinuendCorresponding(), p);
        return subtractCorrespondingStatement;
    }

    public Cobol visitSubtractFromGivingStatement(Cobol.SubtractFromGivingStatement subtractFromGivingStatement, PrintOutputCapture<P> p) {
        visitSpace(subtractFromGivingStatement.getPrefix(), Space.Location.SUBTRACT_FROM_GIVING_STATEMENT_PREFIX, p);
        visitMarkers(subtractFromGivingStatement.getMarkers(), p);
        visit(subtractFromGivingStatement.getSubtractSubtrahend(), p);
        visit(subtractFromGivingStatement.getFrom(), p);
        visit(subtractFromGivingStatement.getSubtractMinuendGiving(), p);
        visit(subtractFromGivingStatement.getGiving(), p);
        visit(subtractFromGivingStatement.getSubtractGiving(), p);
        return subtractFromGivingStatement;
    }

    public Cobol visitSubtractFromStatement(Cobol.SubtractFromStatement subtractFromStatement, PrintOutputCapture<P> p) {
        visitSpace(subtractFromStatement.getPrefix(), Space.Location.SUBTRACT_FROM_STATEMENT_PREFIX, p);
        visitMarkers(subtractFromStatement.getMarkers(), p);
        visit(subtractFromStatement.getSubtractSubtrahend(), p);
        visit(subtractFromStatement.getFrom(), p);
        visit(subtractFromStatement.getSubtractMinuend(), p);
        return subtractFromStatement;
    }

    public Cobol visitSubtractMinuendCorresponding(Cobol.SubtractMinuendCorresponding subtractMinuendCorresponding, PrintOutputCapture<P> p) {
        visitSpace(subtractMinuendCorresponding.getPrefix(), Space.Location.SUBTRACT_MINUEND_CORRESPONDING_PREFIX, p);
        visitMarkers(subtractMinuendCorresponding.getMarkers(), p);
        visit(subtractMinuendCorresponding.getQualifiedDataName(), p);
        visit(subtractMinuendCorresponding.getRounded(), p);
        return subtractMinuendCorresponding;
    }

    public Cobol visitSymbolicCharacter(Cobol.SymbolicCharacter symbolicCharacter, PrintOutputCapture<P> p) {
        visitSpace(symbolicCharacter.getPrefix(), Space.Location.SYMBOLIC_CHARACTER_PREFIX, p);
        visitMarkers(symbolicCharacter.getMarkers(), p);
        visit(symbolicCharacter.getSymbols(), p);
        visit(symbolicCharacter.getWord(), p);
        visit(symbolicCharacter.getLiterals(), p);
        return symbolicCharacter;
    }

    public Cobol visitSymbolicCharactersClause(Cobol.SymbolicCharactersClause symbolicCharactersClause, PrintOutputCapture<P> p) {
        visitSpace(symbolicCharactersClause.getPrefix(), Space.Location.SYMBOLIC_CHARACTERS_CLAUSE_PREFIX, p);
        visitMarkers(symbolicCharactersClause.getMarkers(), p);
        visit(symbolicCharactersClause.getWords(), p);
        visit(symbolicCharactersClause.getSymbols(), p);
        visit(symbolicCharactersClause.getInAlphabet(), p);
        visit(symbolicCharactersClause.getAlphabetName(), p);
        return symbolicCharactersClause;
    }

    public Cobol visitSymbolicDestinationClause(Cobol.SymbolicDestinationClause symbolicDestinationClause, PrintOutputCapture<P> p) {
        visitSpace(symbolicDestinationClause.getPrefix(), Space.Location.SYMBOLIC_DESTINATION_CLAUSE_PREFIX, p);
        visitMarkers(symbolicDestinationClause.getMarkers(), p);
        visit(symbolicDestinationClause.getWords(), p);
        visit(symbolicDestinationClause.getDataDescName(), p);
        return symbolicDestinationClause;
    }

    public Cobol visitSymbolicQueueClause(Cobol.SymbolicQueueClause symbolicQueueClause, PrintOutputCapture<P> p) {
        visitSpace(symbolicQueueClause.getPrefix(), Space.Location.SYMBOLIC_QUEUE_CLAUSE_PREFIX, p);
        visitMarkers(symbolicQueueClause.getMarkers(), p);
        visit(symbolicQueueClause.getWords(), p);
        visit(symbolicQueueClause.getDataDescName(), p);
        return symbolicQueueClause;
    }

    public Cobol visitSymbolicSourceClause(Cobol.SymbolicSourceClause symbolicSourceClause, PrintOutputCapture<P> p) {
        visitSpace(symbolicSourceClause.getPrefix(), Space.Location.SYMBOLIC_SOURCE_CLAUSE_PREFIX, p);
        visitMarkers(symbolicSourceClause.getMarkers(), p);
        visit(symbolicSourceClause.getWords(), p);
        visit(symbolicSourceClause.getDataDescName(), p);
        return symbolicSourceClause;
    }

    public Cobol visitSymbolicSubQueueClause(Cobol.SymbolicSubQueueClause symbolicSubQueueClause, PrintOutputCapture<P> p) {
        visitSpace(symbolicSubQueueClause.getPrefix(), Space.Location.SYMBOLIC_SUB_QUEUE_CLAUSE_PREFIX, p);
        visitMarkers(symbolicSubQueueClause.getMarkers(), p);
        visit(symbolicSubQueueClause.getWords(), p);
        visit(symbolicSubQueueClause.getDataDescName(), p);
        return symbolicSubQueueClause;
    }

    public Cobol visitSymbolicTerminalClause(Cobol.SymbolicTerminalClause symbolicTerminalClause, PrintOutputCapture<P> p) {
        visitSpace(symbolicTerminalClause.getPrefix(), Space.Location.SYMBOLIC_TERMINAL_CLAUSE_PREFIX, p);
        visitMarkers(symbolicTerminalClause.getMarkers(), p);
        visit(symbolicTerminalClause.getWords(), p);
        visit(symbolicTerminalClause.getDataDescName(), p);
        return symbolicTerminalClause;
    }

    public Cobol visitTableCall(Cobol.TableCall tableCall, PrintOutputCapture<P> p) {
        visitSpace(tableCall.getPrefix(), Space.Location.TABLE_CLAUSE_PREFIX, p);
        visitMarkers(tableCall.getMarkers(), p);
        visit(tableCall.getQualifiedDataName(), p);
        visit(tableCall.getSubscripts(), p);
        visit(tableCall.getReferenceModifier(), p);
        return tableCall;
    }

    public Cobol visitTerminate(Cobol.Terminate terminate, PrintOutputCapture<P> p) {
        visitSpace(terminate.getPrefix(), Space.Location.TERMINATE_PREFIX, p);
        visitMarkers(terminate.getMarkers(), p);
        visit(terminate.getTerminate(), p);
        visit(terminate.getReportName(), p);
        return terminate;
    }

    public Cobol visitTextLengthClause(Cobol.TextLengthClause textLengthClause, PrintOutputCapture<P> p) {
        visitSpace(textLengthClause.getPrefix(), Space.Location.TEXT_LENGTH_CLAUSE_PREFIX, p);
        visitMarkers(textLengthClause.getMarkers(), p);
        visit(textLengthClause.getWords(), p);
        visit(textLengthClause.getDataDescName(), p);
        return textLengthClause;
    }

    public Cobol visitUnString(Cobol.UnString unString, PrintOutputCapture<P> p) {
        visitSpace(unString.getPrefix(), Space.Location.UNSTRING_PREFIX, p);
        visitMarkers(unString.getMarkers(), p);
        visit(unString.getUnstring(), p);
        visit(unString.getUnstringSendingPhrase(), p);
        visit(unString.getUnstringIntoPhrase(), p);
        visit(unString.getUnstringWithPointerPhrase(), p);
        visit(unString.getUnstringTallyingPhrase(), p);
        visit(unString.getOnOverflowPhrase(), p);
        visit(unString.getNotOnOverflowPhrase(), p);
        visit(unString.getEndUnstring(), p);
        return unString;
    }

    public Cobol visitUnstringCountIn(Cobol.UnstringCountIn unstringCountIn, PrintOutputCapture<P> p) {
        visitSpace(unstringCountIn.getPrefix(), Space.Location.UNSTRING_COUNT_IN_PREFIX, p);
        visitMarkers(unstringCountIn.getMarkers(), p);
        visit(unstringCountIn.getWords(), p);
        visit(unstringCountIn.getIdentifier(), p);
        return unstringCountIn;
    }

    public Cobol visitUnstringDelimitedByPhrase(Cobol.UnstringDelimitedByPhrase unstringDelimitedByPhrase, PrintOutputCapture<P> p) {
        visitSpace(unstringDelimitedByPhrase.getPrefix(), Space.Location.UNSTRING_DELIMITED_BY_PHRASE_PREFIX, p);
        visitMarkers(unstringDelimitedByPhrase.getMarkers(), p);
        visit(unstringDelimitedByPhrase.getWords(), p);
        visit(unstringDelimitedByPhrase.getName(), p);
        return unstringDelimitedByPhrase;
    }

    public Cobol visitUnstringDelimiterIn(Cobol.UnstringDelimiterIn unstringDelimiterIn, PrintOutputCapture<P> p) {
        visitSpace(unstringDelimiterIn.getPrefix(), Space.Location.UNSTRING_DELIMITED_IN_PREFIX, p);
        visitMarkers(unstringDelimiterIn.getMarkers(), p);
        visit(unstringDelimiterIn.getWords(), p);
        visit(unstringDelimiterIn.getIdentifier(), p);
        return unstringDelimiterIn;
    }

    public Cobol visitUnstringInto(Cobol.UnstringInto unstringInto, PrintOutputCapture<P> p) {
        visitSpace(unstringInto.getPrefix(), Space.Location.UNSTRING_INTO_PREFIX, p);
        visitMarkers(unstringInto.getMarkers(), p);
        visit(unstringInto.getIdentifier(), p);
        visit(unstringInto.getUnstringDelimiterIn(), p);
        visit(unstringInto.getUnstringCountIn(), p);
        return unstringInto;
    }

    public Cobol visitUnstringIntoPhrase(Cobol.UnstringIntoPhrase unstringIntoPhrase, PrintOutputCapture<P> p) {
        visitSpace(unstringIntoPhrase.getPrefix(), Space.Location.UNSTRING_INTO_PHRASE_PREFIX, p);
        visitMarkers(unstringIntoPhrase.getMarkers(), p);
        visit(unstringIntoPhrase.getInto(), p);
        visit(unstringIntoPhrase.getUnstringIntos(), p);
        return unstringIntoPhrase;
    }

    public Cobol visitUnstringOrAllPhrase(Cobol.UnstringOrAllPhrase unstringOrAllPhrase, PrintOutputCapture<P> p) {
        visitSpace(unstringOrAllPhrase.getPrefix(), Space.Location.UNSTRING_OR_ALL_PHRASE_PREFIX, p);
        visitMarkers(unstringOrAllPhrase.getMarkers(), p);
        visit(unstringOrAllPhrase.getWords(), p);
        visit(unstringOrAllPhrase.getName(), p);
        return unstringOrAllPhrase;
    }

    public Cobol visitUnstringSendingPhrase(Cobol.UnstringSendingPhrase unstringSendingPhrase, PrintOutputCapture<P> p) {
        visitSpace(unstringSendingPhrase.getPrefix(), Space.Location.UNSTRING_SENDING_PHRASE_PREFIX, p);
        visitMarkers(unstringSendingPhrase.getMarkers(), p);
        visit(unstringSendingPhrase.getIdentifier(), p);
        visit(unstringSendingPhrase.getUnstringDelimitedByPhrase(), p);
        visit(unstringSendingPhrase.getUnstringOrAllPhrases(), p);
        return unstringSendingPhrase;
    }

    public Cobol visitUnstringTallyingPhrase(Cobol.UnstringTallyingPhrase unstringTallyingPhrase, PrintOutputCapture<P> p) {
        visitSpace(unstringTallyingPhrase.getPrefix(), Space.Location.UNSTRING_TALLYING_PHRASE_PREFIX, p);
        visitMarkers(unstringTallyingPhrase.getMarkers(), p);
        visit(unstringTallyingPhrase.getWords(), p);
        visit(unstringTallyingPhrase.getQualifiedDataName(), p);
        return unstringTallyingPhrase;
    }

    public Cobol visitUnstringWithPointerPhrase(Cobol.UnstringWithPointerPhrase unstringWithPointerPhrase, PrintOutputCapture<P> p) {
        visitSpace(unstringWithPointerPhrase.getPrefix(), Space.Location.UNSTRING_WITH_POINTER_PHRASE_PREFIX, p);
        visitMarkers(unstringWithPointerPhrase.getMarkers(), p);
        visit(unstringWithPointerPhrase.getWords(), p);
        visit(unstringWithPointerPhrase.getQualifiedDataName(), p);
        return unstringWithPointerPhrase;
    }

    public Cobol visitUseAfterClause(Cobol.UseAfterClause useAfterClause, PrintOutputCapture<P> p) {
        visitSpace(useAfterClause.getPrefix(), Space.Location.USE_AFTER_CLAUSE_PREFIX, p);
        visitMarkers(useAfterClause.getMarkers(), p);
        visit(useAfterClause.getWords(), p);
        visit(useAfterClause.getUseAfterOn(), p);
        return useAfterClause;
    }

    public Cobol visitUseAfterOn(Cobol.UseAfterOn useAfterOn, PrintOutputCapture<P> p) {
        visitSpace(useAfterOn.getPrefix(), Space.Location.USE_AFTER_ON_PREFIX, p);
        visitMarkers(useAfterOn.getMarkers(), p);
        visit(useAfterOn.getAfterOn(), p);
        visit(useAfterOn.getFileNames(), p);
        return useAfterOn;
    }

    public Cobol visitUseDebugClause(Cobol.UseDebugClause useDebugClause, PrintOutputCapture<P> p) {
        visitSpace(useDebugClause.getPrefix(), Space.Location.USE_DEBUG_CLAUSE_PREFIX, p);
        visitMarkers(useDebugClause.getMarkers(), p);
        visit(useDebugClause.getWords(), p);
        visit(useDebugClause.getUseDebugs(), p);
        return useDebugClause;
    }

    public Cobol visitUseDebugOn(Cobol.UseDebugOn useDebugOn, PrintOutputCapture<P> p) {
        visitSpace(useDebugOn.getPrefix(), Space.Location.USE_DEBUG_ON_PREFIX, p);
        visitMarkers(useDebugOn.getMarkers(), p);
        visit(useDebugOn.getWords(), p);
        visit(useDebugOn.getName(), p);
        return useDebugOn;
    }

    public Cobol visitUseStatement(Cobol.UseStatement useStatement, PrintOutputCapture<P> p) {
        visitSpace(useStatement.getPrefix(), Space.Location.USE_STATEMENT_PREFIX, p);
        visitMarkers(useStatement.getMarkers(), p);
        visit(useStatement.getUse(), p);
        visit(useStatement.getClause(), p);
        return useStatement;
    }

    public Cobol visitValueOfClause(Cobol.ValueOfClause valueOfClause, PrintOutputCapture<P> p) {
        visitSpace(valueOfClause.getPrefix(), Space.Location.VALUE_OF_CLAUSE_PREFIX, p);
        visitMarkers(valueOfClause.getMarkers(), p);
        visit(valueOfClause.getValueOf(), p);
        visit(valueOfClause.getValuePairs(), p);
        return valueOfClause;
    }

    public Cobol visitValuePair(Cobol.ValuePair valuePair, PrintOutputCapture<P> p) {
        visitSpace(valuePair.getPrefix(), Space.Location.VALUE_PAIR_PREFIX, p);
        visitMarkers(valuePair.getMarkers(), p);
        visit(valuePair.getSystemName(), p);
        visit(valuePair.getIs(), p);
        visit(valuePair.getName(), p);
        return valuePair;
    }

    public Cobol visitValuedObjectComputerClause(Cobol.ValuedObjectComputerClause valuedObjectComputerClause, PrintOutputCapture<P> p) {
        visitSpace(valuedObjectComputerClause.getPrefix(), Space.Location.VALUE_OBJECT_COMPUTER_CLAUSE_PREFIX, p);
        visitMarkers(valuedObjectComputerClause.getMarkers(), p);
        visit(valuedObjectComputerClause.getWords(), p);
        visit(valuedObjectComputerClause.getValue(), p);
        visit(valuedObjectComputerClause.getUnits(), p);
        return valuedObjectComputerClause;
    }

    public Cobol visitWord(Cobol.Word word, PrintOutputCapture<P> p) {
        // Column area markers.
        SequenceArea sequenceArea = null;
        IndicatorArea indicatorArea = null;
        CommentArea commentArea = null;

        // CobolPreprocessor markers.
        ReplaceBy replaceBy = null;
        ReplaceOff replaceOff = null;
        Replace replace = null;
        Copy copy = null;

        Lines lines = null;
        Continuation continuation = null;

        // Search markers.
        SearchResult indicatorSearch = null;
        SearchResult  copyBookSearch = null;

        for (Marker marker : word.getMarkers().getMarkers()) {
            if (marker instanceof SequenceArea) {
                sequenceArea = (SequenceArea) marker;
            } else if (marker instanceof IndicatorArea) {
                indicatorArea = (IndicatorArea) marker;
            } else if (marker instanceof CommentArea) {
                commentArea = (CommentArea) marker;
            } else if (marker instanceof SearchResult) {
                SearchResult m = (SearchResult) marker;
                if (SearchResultKey.COPIED_SOURCE.equals(m.getDescription())) {
                    copyBookSearch = m;
                }
            } else if (marker instanceof ReplaceBy) {
                replaceBy = (ReplaceBy) marker;
            } else if (marker instanceof ReplaceOff) {
                replaceOff = (ReplaceOff) marker;
            } else if (marker instanceof Replace) {
                replace = (Replace) marker;
            } else if (marker instanceof Copy) {
                copy = (Copy) marker;
            } else if (marker instanceof Lines) {
                lines = (Lines) marker;
            } else if (marker instanceof Continuation) {
                continuation = (Continuation) marker;
            }
        }

        if (replaceBy != null) {
            // Print the original replaceBy
            PrintOutputCapture<ExecutionContext> output = new PrintOutputCapture<>(new InMemoryExecutionContext());
            printer.visit(replaceBy.getStatement(), output);
            p.append(output.getOut());
        }

        if (replaceOff != null) {
            // Print the original replaceOff
            PrintOutputCapture<ExecutionContext> output = new PrintOutputCapture<>(new InMemoryExecutionContext());
            printer.visit(replaceOff.getReplaceOff(), output);
            p.append(output.getOut());
        }

        if (replace != null && copy == null) {
            // Print the original replace
            PrintOutputCapture<ExecutionContext> output = new PrintOutputCapture<>(new InMemoryExecutionContext());
            printer.visit(replace.getOriginalWord(), output);
            p.append(output.getOut());

            if (replace.isReplacedWithEmpty()) {
                originalReplaceLength = output.getOut().length();
            } else {
                originalReplaceLength = 0;
                return word;
            }
        }

        // The COBOL word is a product of a copy statement.
        if (copy != null) {
            // Print the original Copy Statement in place of the first word from the copied source.
            if (copyUuid == null || !copyUuid.equals(copy.getOriginalStatement().getId().toString())) {
                // Print the original copy
                PrintOutputCapture<ExecutionContext> output = new PrintOutputCapture<>(new InMemoryExecutionContext());
                printer.visit(copy.getOriginalStatement(), output);
                p.append(output.getOut());
                copyUuid = copy.getOriginalStatement().getId().toString();

                // `printCopiedSource` is for debugging purposed and may be removed after visiting preprocessing markers is more stable.
                if (printCopiedSource || copyBookSearch != null) {
                    if (!p.getOut().endsWith("\n")) {
                        p.append("\n");
                    }

                    output = new PrintOutputCapture<>(new InMemoryExecutionContext());
                    // Printing the CopyBook AST requires a post process printer.
                    CobolPreprocessorPrinter<ExecutionContext> copyBookAstPrinter = new CobolPreprocessorPrinter<>(false, true);
                    copyBookAstPrinter.visit(copy.getOriginalStatement().getCopyBook(), output);

                    String bookName = "   ~~~*>CopyBook " + copy.getOriginalStatement().getCopySource().getName().getWord();

                    // This should eventually be based on the CobolDialect.
                    String start = bookName + " start ";
                    String copiedSourceStart = start + fillArea('*', 72 - start.length()) + "\n";
                    p.append(copiedSourceStart);
                    p.append(output.getOut());

                    if (!p.getOut().endsWith("\n")) {
                        p.append("\n");
                    }

                    String end = bookName + " end ";
                    String copiedSourceEnd = end + fillArea('*', 72 - end.length()) + "\n";
                    p.append(copiedSourceEnd);
                }
            }

            // Do not print the AST for the copied source.
            return word;
        } else if (copyUuid != null) {
            copyUuid = null;
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
                visitSpace(word.getPrefix(), Space.Location.WORD_PREFIX, p);
            }

            p.append(word.getWord());

            if (commentArea != null && !commentArea.isAdded()) {
                visitCommentArea(commentArea, p);
            }
        }

        return word;
    }

    public Cobol visitWorkingStorageSection(Cobol.WorkingStorageSection workingStorageSection, PrintOutputCapture<P> p) {
        visitSpace(workingStorageSection.getPrefix(), Space.Location.WORKING_STORAGE_SECTION_PREFIX, p);
        visitMarkers(workingStorageSection.getMarkers(), p);
        visit(workingStorageSection.getWords(), p);
        visit(workingStorageSection.getDot(), p);
        visit(workingStorageSection.getDataDescriptions(), p);
        return workingStorageSection;
    }

    public Cobol visitWrite(Cobol.Write write, PrintOutputCapture<P> p) {
        visitSpace(write.getPrefix(), Space.Location.WRITE_PREFIX, p);
        visitMarkers(write.getMarkers(), p);
        visit(write.getWrite(), p);
        visit(write.getRecordName(), p);
        visit(write.getWriteFromPhrase(), p);
        visit(write.getWriteAdvancingPhrase(), p);
        visit(write.getWriteAtEndOfPagePhrase(), p);
        visit(write.getWriteNotAtEndOfPagePhrase(), p);
        visit(write.getInvalidKeyPhrase(), p);
        visit(write.getNotInvalidKeyPhrase(), p);
        visit(write.getEndWrite(), p);
        return write;
    }

    public Cobol visitWriteAdvancingLines(Cobol.WriteAdvancingLines writeAdvancingLines, PrintOutputCapture<P> p) {
        visitSpace(writeAdvancingLines.getPrefix(), Space.Location.WRITE_ADVANCING_LINES_PREFIX, p);
        visitMarkers(writeAdvancingLines.getMarkers(), p);
        visit(writeAdvancingLines.getName(), p);
        visit(writeAdvancingLines.getWord(), p);
        return writeAdvancingLines;
    }

    public Cobol visitWriteAdvancingMnemonic(Cobol.WriteAdvancingMnemonic writeAdvancingMnemonic, PrintOutputCapture<P> p) {
        visitSpace(writeAdvancingMnemonic.getPrefix(), Space.Location.WRITE_ADVANCING_MNEMONIC_PREFIX, p);
        visitMarkers(writeAdvancingMnemonic.getMarkers(), p);
        visit(writeAdvancingMnemonic.getName(), p);
        return writeAdvancingMnemonic;
    }

    public Cobol visitWriteAdvancingPage(Cobol.WriteAdvancingPage writeAdvancingPage, PrintOutputCapture<P> p) {
        visitSpace(writeAdvancingPage.getPrefix(), Space.Location.WRITE_ADVANCING_PAGE_PREFIX, p);
        visitMarkers(writeAdvancingPage.getMarkers(), p);
        visit(writeAdvancingPage.getPage(), p);
        return writeAdvancingPage;
    }

    public Cobol visitWriteAdvancingPhrase(Cobol.WriteAdvancingPhrase writeAdvancingPhrase, PrintOutputCapture<P> p) {
        visitSpace(writeAdvancingPhrase.getPrefix(), Space.Location.WRITE_ADVANCING_PHRASE_PREFIX, p);
        visitMarkers(writeAdvancingPhrase.getMarkers(), p);
        visit(writeAdvancingPhrase.getWords(), p);
        visit(writeAdvancingPhrase.getWriteBy(), p);
        return writeAdvancingPhrase;
    }

    public Cobol visitWriteFromPhrase(Cobol.WriteFromPhrase writeFromPhrase, PrintOutputCapture<P> p) {
        visitSpace(writeFromPhrase.getPrefix(), Space.Location.WRITE_FROM_PHRASE_PREFIX, p);
        visitMarkers(writeFromPhrase.getMarkers(), p);
        visit(writeFromPhrase.getFrom(), p);
        visit(writeFromPhrase.getName(), p);
        return writeFromPhrase;
    }

    public void visitContinuation(Cobol.Word word, Continuation continuation, PrintOutputCapture<P> p) {
        visitContinuation(word, continuation, null, p);
    }

    public void visitContinuation(Cobol.Word word, Continuation continuation, @Nullable SearchResult searchResult, PrintOutputCapture<P> p) {
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

    @Override
    public <M extends Marker> M visitLines(Lines lines, PrintOutputCapture<P> p) {
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

        //noinspection unchecked
        return (M) lines;
    }

    @Override
    public <M extends Marker> M visitSequenceArea(SequenceArea sequenceArea, PrintOutputCapture<P> p) {
        if (printColumns) {
            p.append(sequenceArea.getSequence());
        }

        //noinspection unchecked
        return (M) sequenceArea;
    }

    @Override
    public <M extends Marker> M visitIndicatorArea(IndicatorArea indicatorArea, PrintOutputCapture<P> p) {
        visitMarkers(indicatorArea.getMarkers(), p);
        if (printColumns) {
            p.append(indicatorArea.getIndicator());
        }
        p.append(indicatorArea.getContinuationPrefix());

        //noinspection unchecked
        return (M) indicatorArea;
    }

    @Nullable
    @Override
    public <M extends Marker> M visitCommentArea(@Nullable CommentArea commentArea, PrintOutputCapture<P> p) {
        if (commentArea != null) {
            visitSpace(commentArea.getPrefix(), Space.Location.COMMENT_AREA_PREFIX, p);
            if (printColumns) {
                p.append(commentArea.getComment());
            }
            visitSpace(commentArea.getEndOfLine(), Space.Location.COMMENT_AREA_EOL, p);
        }

        //noinspection unchecked
        return (M) commentArea;
    }
}
