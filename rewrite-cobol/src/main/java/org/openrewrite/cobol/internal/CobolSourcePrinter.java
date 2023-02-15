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

import org.openrewrite.Cursor;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.CobolVisitor;
import org.openrewrite.cobol.search.SearchResultKey;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.StringUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;
import org.openrewrite.marker.SearchResult;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import static org.openrewrite.cobol.CobolPrinterUtils.fillArea;

/**
 * Print the original COBOL source code.
 * <p>
 * `printOriginalSource`:
 *      true: Print as the original source code before preprocessing commands like COPY and REPLACE.
 *      false: Print the post-processed AST.
 * <p>
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
        beforeSyntax(abbreviation, Space.Location.ABBREVIATION_PREFIX, p);
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
        afterSyntax(abbreviation, p);
        return abbreviation;
    }

    public Cobol visitAccept(Cobol.Accept accept, PrintOutputCapture<P> p) {
        beforeSyntax(accept, Space.Location.ACCEPT_PREFIX, p);
        visit(accept.getAccept(), p);
        visit(accept.getIdentifier(), p);
        visit(accept.getOperation(), p);
        visit(accept.getOnExceptionClause(), p);
        visit(accept.getNotOnExceptionClause(), p);
        visit(accept.getEndAccept(), p);
        afterSyntax(accept, p);
        return accept;
    }

    public Cobol visitAcceptFromDateStatement(Cobol.AcceptFromDateStatement acceptFromDateStatement, PrintOutputCapture<P> p) {
        beforeSyntax(acceptFromDateStatement, Space.Location.ACCEPT_FROM_DATE_STATEMENT_PREFIX, p);
        visit(acceptFromDateStatement.getWords(), p);
        afterSyntax(acceptFromDateStatement, p);
        return acceptFromDateStatement;
    }

    public Cobol visitAcceptFromEscapeKeyStatement(Cobol.AcceptFromEscapeKeyStatement acceptFromEscapeKeyStatement, PrintOutputCapture<P> p) {
        beforeSyntax(acceptFromEscapeKeyStatement, Space.Location.ACCEPT_FROM_ESCAPE_KEY_STATEMENT_PREFIX, p);
        visit(acceptFromEscapeKeyStatement.getWords(), p);
        afterSyntax(acceptFromEscapeKeyStatement, p);
        return acceptFromEscapeKeyStatement;
    }

    public Cobol visitAcceptFromMnemonicStatement(Cobol.AcceptFromMnemonicStatement acceptFromMnemonicStatement, PrintOutputCapture<P> p) {
        beforeSyntax(acceptFromMnemonicStatement, Space.Location.ACCEPT_FROM_MNEMONIC_STATEMENT_PREFIX, p);
        visit(acceptFromMnemonicStatement.getFrom(), p);
        visit(acceptFromMnemonicStatement.getMnemonicName(), p);
        afterSyntax(acceptFromMnemonicStatement, p);
        return acceptFromMnemonicStatement;
    }

    public Cobol visitAcceptMessageCountStatement(Cobol.AcceptMessageCountStatement acceptMessageCountStatement, PrintOutputCapture<P> p) {
        beforeSyntax(acceptMessageCountStatement, Space.Location.ACCEPT_MESSAGE_COUNT_STATEMENT_PREFIX, p);
        visit(acceptMessageCountStatement.getWords(), p);
        afterSyntax(acceptMessageCountStatement, p);
        return acceptMessageCountStatement;
    }

    public Cobol visitAccessModeClause(Cobol.AccessModeClause accessModeClause, PrintOutputCapture<P> p) {
        beforeSyntax(accessModeClause, Space.Location.ACCESS_MODE_CLAUSE_PREFIX, p);
        visit(accessModeClause.getWords(), p);
        visit(accessModeClause.getType(), p);
        afterSyntax(accessModeClause, p);
        return accessModeClause;
    }

    public Cobol visitAdd(Cobol.Add add, PrintOutputCapture<P> p) {
        beforeSyntax(add, Space.Location.ADD_PREFIX, p);
        visit(add.getAdd(), p);
        visit(add.getOperation(), p);
        visit(add.getOnSizeError(), p);
        visit(add.getNotOnSizeError(), p);
        visit(add.getEndAdd(), p);
        afterSyntax(add, p);
        return add;
    }

    public Cobol visitAddCorresponding(Cobol.AddCorresponding addCorresponding, PrintOutputCapture<P> p) {
        beforeSyntax(addCorresponding, Space.Location.ADD_CORRESPONDING_PREFIX, p);
        visit(addCorresponding.getCorresponding(), p);
        visit(addCorresponding.getIdentifier(), p);
        visit(addCorresponding.getTo(), p);
        visit(addCorresponding.getRoundable(), p);
        afterSyntax(addCorresponding, p);
        return addCorresponding;
    }

    public Cobol visitAddTo(Cobol.AddTo addTo, PrintOutputCapture<P> p) {
        beforeSyntax(addTo, Space.Location.ADD_TO_PREFIX, p);
        visit(addTo.getFrom(), p);
        visit(addTo.getTo(), p);
        visit(addTo.getRoundables(), p);
        afterSyntax(addTo, p);
        return addTo;
    }

    public Cobol visitAddToGiving(Cobol.AddToGiving addToGiving, PrintOutputCapture<P> p) {
        beforeSyntax(addToGiving, Space.Location.ADD_TO_GIVING_PREFIX, p);
        visit(addToGiving.getFrom(), p);
        visit(addToGiving.getTo(), p);
        visit(addToGiving.getNames(), p);
        visit(addToGiving.getGiving(), p);
        visit(addToGiving.getRoundables(), p);
        afterSyntax(addToGiving, p);
        return addToGiving;
    }

    public Cobol visitAlphabetAlso(Cobol.AlphabetAlso alphabetAlso, PrintOutputCapture<P> p) {
        beforeSyntax(alphabetAlso, Space.Location.ALPHABET_ALSO_PREFIX, p);
        visit(alphabetAlso.getWord(), p);
        visit(alphabetAlso.getLiterals(), p);
        afterSyntax(alphabetAlso, p);
        return alphabetAlso;
    }

    public Cobol visitAlphabetClause(Cobol.AlphabetClause alphabetClause, PrintOutputCapture<P> p) {
        beforeSyntax(alphabetClause, Space.Location.ALPHABET_CLAUSE_PREFIX, p);
        visit(alphabetClause.getAlphabet(), p);
        visit(alphabetClause.getName(), p);
        visit(alphabetClause.getWords(), p);
        afterSyntax(alphabetClause, p);
        return alphabetClause;
    }

    public Cobol visitAlphabetLiteral(Cobol.AlphabetLiteral alphabetLiteral, PrintOutputCapture<P> p) {
        beforeSyntax(alphabetLiteral, Space.Location.ALPHABET_LITERAL_PREFIX, p);
        visit(alphabetLiteral.getLiteral(), p);
        visit(alphabetLiteral.getAlphabetThrough(), p);
        visit(alphabetLiteral.getAlphabetAlso(), p);
        afterSyntax(alphabetLiteral, p);
        return alphabetLiteral;
    }

    public Cobol visitAlphabetThrough(Cobol.AlphabetThrough alphabetThrough, PrintOutputCapture<P> p) {
        beforeSyntax(alphabetThrough, Space.Location.ALPHABET_THROUGH_PREFIX, p);
        visit(alphabetThrough.getWord(), p);
        visit(alphabetThrough.getLiteral(), p);
        afterSyntax(alphabetThrough, p);
        return alphabetThrough;
    }

    public Cobol visitAlterProceedTo(Cobol.AlterProceedTo alterProceedTo, PrintOutputCapture<P> p) {
        beforeSyntax(alterProceedTo, Space.Location.ALTER_PROCEED_TO_PREFIX, p);
        visit(alterProceedTo.getFrom(), p);
        visit(alterProceedTo.getWords(), p);
        visit(alterProceedTo.getTo(), p);
        afterSyntax(alterProceedTo, p);
        return alterProceedTo;
    }

    public Cobol visitAlterStatement(Cobol.AlterStatement alterStatement, PrintOutputCapture<P> p) {
        beforeSyntax(alterStatement, Space.Location.ALTER_STATEMENT_PREFIX, p);
        visit(alterStatement.getWord(), p);
        visit(alterStatement.getAlterProceedTo(), p);
        afterSyntax(alterStatement, p);
        return alterStatement;
    }

    public Cobol visitAlteredGoTo(Cobol.AlteredGoTo alteredGoTo, PrintOutputCapture<P> p) {
        beforeSyntax(alteredGoTo, Space.Location.ALTERED_GO_TO_PREFIX, p);
        visit(alteredGoTo.getWords(), p);
        visit(alteredGoTo.getDot(), p);
        afterSyntax(alteredGoTo, p);
        return alteredGoTo;
    }

    public Cobol visitAlternateRecordKeyClause(Cobol.AlternateRecordKeyClause alternateRecordKeyClause, PrintOutputCapture<P> p) {
        beforeSyntax(alternateRecordKeyClause, Space.Location.ALTERNATE_RECORD_KEY_CLAUSE_PREFIX, p);
        visit(alternateRecordKeyClause.getAlternateWords(), p);
        visit(alternateRecordKeyClause.getQualifiedDataName(), p);
        visit(alternateRecordKeyClause.getPasswordClause(), p);
        visit(alternateRecordKeyClause.getDuplicates(), p);
        afterSyntax(alternateRecordKeyClause, p);
        return alternateRecordKeyClause;
    }

    public Cobol visitAndOrCondition(Cobol.AndOrCondition andOrCondition, PrintOutputCapture<P> p) {
        beforeSyntax(andOrCondition, Space.Location.AND_OR_CONDITION_PREFIX, p);
        visit(andOrCondition.getLogicalOperator(), p);
        visit(andOrCondition.getCombinableCondition(), p);
        visit(andOrCondition.getAbbreviations(), p);
        afterSyntax(andOrCondition, p);
        return andOrCondition;
    }

    public Cobol visitArgument(Cobol.Argument argument, PrintOutputCapture<P> p) {
        beforeSyntax(argument, Space.Location.ARGUMENT_PREFIX, p);
        visit(argument.getFirst(), p);
        visit(argument.getIntegerLiteral(), p);
        afterSyntax(argument, p);
        return argument;
    }

    public Cobol visitArithmeticExpression(Cobol.ArithmeticExpression arithmeticExpression, PrintOutputCapture<P> p) {
        beforeSyntax(arithmeticExpression, Space.Location.ARITHMETIC_EXPRESSION_PREFIX, p);
        visit(arithmeticExpression.getMultDivs(), p);
        visit(arithmeticExpression.getPlusMinuses(), p);
        afterSyntax(arithmeticExpression, p);
        return arithmeticExpression;
    }

    public Cobol visitAssignClause(Cobol.AssignClause assignClause, PrintOutputCapture<P> p) {
        beforeSyntax(assignClause, Space.Location.ASSIGN_CLAUSE_PREFIX, p);
        visit(assignClause.getWords(), p);
        visit(assignClause.getName(), p);
        afterSyntax(assignClause, p);
        return assignClause;
    }

    public Cobol visitBlockContainsClause(Cobol.BlockContainsClause blockContainsClause, PrintOutputCapture<P> p) {
        beforeSyntax(blockContainsClause, Space.Location.BLOCK_CONTAINS_PREFIX, p);
        visit(blockContainsClause.getFirstWords(), p);
        visit(blockContainsClause.getIntegerLiteral(), p);
        visit(blockContainsClause.getBlockContainsTo(), p);
        visit(blockContainsClause.getLastWord(), p);
        afterSyntax(blockContainsClause, p);
        return blockContainsClause;
    }

    public Cobol visitBlockContainsTo(Cobol.BlockContainsTo blockContainsTo, PrintOutputCapture<P> p) {
        beforeSyntax(blockContainsTo, Space.Location.BLOCK_CONTAINS_TO_PREFIX, p);
        visit(blockContainsTo.getTo(), p);
        visit(blockContainsTo.getIntegerLiteral(), p);
        afterSyntax(blockContainsTo, p);
        return blockContainsTo;
    }

    public Cobol visitCall(Cobol.Call call, PrintOutputCapture<P> p) {
        beforeSyntax(call, Space.Location.CALL_PREFIX, p);
        visit(call.getCall(), p);
        visit(call.getIdentifier(), p);
        visit(call.getCallUsingPhrase(), p);
        visit(call.getCallGivingPhrase(), p);
        visit(call.getOnOverflowPhrase(), p);
        visit(call.getOnExceptionClause(), p);
        visit(call.getNotOnExceptionClause(), p);
        visit(call.getEndCall(), p);
        afterSyntax(call, p);
        return call;
    }

    public Cobol visitCallBy(Cobol.CallBy callBy, PrintOutputCapture<P> p) {
        beforeSyntax(callBy, Space.Location.CALL_BY_PREFIX, p);
        visit(callBy.getWords(), p);
        visit(callBy.getIdentifier(), p);
        afterSyntax(callBy, p);
        return callBy;
    }

    public Cobol visitCallGivingPhrase(Cobol.CallGivingPhrase callGivingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(callGivingPhrase, Space.Location.CALL_GIVING_PHRASE_PREFIX, p);
        visit(callGivingPhrase.getWord(), p);
        visit(callGivingPhrase.getIdentifier(), p);
        afterSyntax(callGivingPhrase, p);
        return callGivingPhrase;
    }

    public Cobol visitCallPhrase(Cobol.CallPhrase callPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(callPhrase, Space.Location.CALL_PHRASE_PREFIX, p);
        visit(callPhrase.getWords(), p);
        visit(callPhrase.getParameters(), p);
        afterSyntax(callPhrase, p);
        return callPhrase;
    }

    public Cobol visitCancel(Cobol.Cancel cancel, PrintOutputCapture<P> p) {
        beforeSyntax(cancel, Space.Location.CANCEL_PREFIX, p);
        visit(cancel.getCancel(), p);
        visit(cancel.getCancelCalls(), p);
        afterSyntax(cancel, p);
        return cancel;
    }

    public Cobol visitCancelCall(Cobol.CancelCall cancelCall, PrintOutputCapture<P> p) {
        beforeSyntax(cancelCall, Space.Location.CANCEL_CALL_PREFIX, p);
        visit(cancelCall.getLibraryName(), p);
        visit(cancelCall.getBy(), p);
        visit(cancelCall.getIdentifier(), p);
        visit(cancelCall.getLiteral(), p);
        afterSyntax(cancelCall, p);
        return cancelCall;
    }

    public Cobol visitChannelClause(Cobol.ChannelClause channelClause, PrintOutputCapture<P> p) {
        beforeSyntax(channelClause, Space.Location.CHANNEL_CLAUSE_PREFIX, p);
        visit(channelClause.getWord(), p);
        visit(channelClause.getLiteral(), p);
        visit(channelClause.getIs(), p);
        visit(channelClause.getMnemonicName(), p);
        afterSyntax(channelClause, p);
        return channelClause;
    }

    public Cobol visitClassClause(Cobol.ClassClause classClause, PrintOutputCapture<P> p) {
        beforeSyntax(classClause, Space.Location.CLASS_CLAUSE_PREFIX, p);
        visit(classClause.getClazz(), p);
        visit(classClause.getClassName(), p);
        visit(classClause.getWords(), p);
        visit(classClause.getThroughs(), p);
        afterSyntax(classClause, p);
        return classClause;
    }

    public Cobol visitClassClauseThrough(Cobol.ClassClauseThrough classClauseThrough, PrintOutputCapture<P> p) {
        beforeSyntax(classClauseThrough, Space.Location.CLASS_CLAUSE_THROUGH_PREFIX, p);
        visit(classClauseThrough.getFrom(), p);
        visit(classClauseThrough.getThrough(), p);
        visit(classClauseThrough.getTo(), p);
        afterSyntax(classClauseThrough, p);
        return classClauseThrough;
    }

    public Cobol visitClassCondition(Cobol.ClassCondition classCondition, PrintOutputCapture<P> p) {
        beforeSyntax(classCondition, Space.Location.CLASS_CONDITION_PREFIX, p);
        visit(classCondition.getName(), p);
        visit(classCondition.getWords(), p);
        visit(classCondition.getType(), p);
        afterSyntax(classCondition, p);
        return classCondition;
    }

    public Cobol visitClose(Cobol.Close close, PrintOutputCapture<P> p) {
        beforeSyntax(close, Space.Location.CLOSE_PREFIX, p);
        visit(close.getClose(), p);
        visit(close.getCloseFiles(), p);
        afterSyntax(close, p);
        return close;
    }

    public Cobol visitCloseFile(Cobol.CloseFile closeFile, PrintOutputCapture<P> p) {
        beforeSyntax(closeFile, Space.Location.CLOSE_FILE_PREFIX, p);
        visit(closeFile.getFileName(), p);
        visit(closeFile.getCloseStatement(), p);
        afterSyntax(closeFile, p);
        return closeFile;
    }

    public Cobol visitClosePortFileIOStatement(Cobol.ClosePortFileIOStatement closePortFileIOStatement, PrintOutputCapture<P> p) {
        beforeSyntax(closePortFileIOStatement, Space.Location.CLOSE_PORT_FILE_IO_STATEMENT_PREFIX, p);
        visit(closePortFileIOStatement.getWords(), p);
        visit(closePortFileIOStatement.getClosePortFileIOUsing(), p);
        afterSyntax(closePortFileIOStatement, p);
        return closePortFileIOStatement;
    }

    public Cobol visitClosePortFileIOUsingAssociatedData(Cobol.ClosePortFileIOUsingAssociatedData closePortFileIOUsingAssociatedData, PrintOutputCapture<P> p) {
        beforeSyntax(closePortFileIOUsingAssociatedData, Space.Location.CLOSE_PORT_FILE_IO_USING_ASSOCIATED_DATA_PREFIX, p);
        visit(closePortFileIOUsingAssociatedData.getAssociatedData(), p);
        visit(closePortFileIOUsingAssociatedData.getIdentifier(), p);
        afterSyntax(closePortFileIOUsingAssociatedData, p);
        return closePortFileIOUsingAssociatedData;
    }

    public Cobol visitClosePortFileIOUsingAssociatedDataLength(Cobol.ClosePortFileIOUsingAssociatedDataLength closePortFileIOUsingAssociatedDataLength, PrintOutputCapture<P> p) {
        beforeSyntax(closePortFileIOUsingAssociatedDataLength, Space.Location.CLOSE_PORT_FILE_IO_USING_ASSOCIATED_DATA_LENGTH_PREFIX, p);
        visit(closePortFileIOUsingAssociatedDataLength.getWords(), p);
        visit(closePortFileIOUsingAssociatedDataLength.getIdentifier(), p);
        afterSyntax(closePortFileIOUsingAssociatedDataLength, p);
        return closePortFileIOUsingAssociatedDataLength;
    }

    public Cobol visitClosePortFileIOUsingCloseDisposition(Cobol.ClosePortFileIOUsingCloseDisposition closePortFileIOUsingCloseDisposition, PrintOutputCapture<P> p) {
        beforeSyntax(closePortFileIOUsingCloseDisposition, Space.Location.CLOSE_PORT_FILE_IO_USING_CLOSE_DISPOSITION_PREFIX, p);
        visit(closePortFileIOUsingCloseDisposition.getWords(), p);
        afterSyntax(closePortFileIOUsingCloseDisposition, p);
        return closePortFileIOUsingCloseDisposition;
    }

    public Cobol visitCloseReelUnitStatement(Cobol.CloseReelUnitStatement closeReelUnitStatement, PrintOutputCapture<P> p) {
        beforeSyntax(closeReelUnitStatement, Space.Location.CLOSE_REEL_UNIT_STATEMENT_PREFIX, p);
        visit(closeReelUnitStatement.getWords(), p);
        afterSyntax(closeReelUnitStatement, p);
        return closeReelUnitStatement;
    }

    public Cobol visitCloseRelativeStatement(Cobol.CloseRelativeStatement closeRelativeStatement, PrintOutputCapture<P> p) {
        beforeSyntax(closeRelativeStatement, Space.Location.CLOSE_RELATIVE_STATEMENT_PREFIX, p);
        visit(closeRelativeStatement.getWords(), p);
        afterSyntax(closeRelativeStatement, p);
        return closeRelativeStatement;
    }

    public Cobol visitCodeSetClause(Cobol.CodeSetClause codeSetClause, PrintOutputCapture<P> p) {
        beforeSyntax(codeSetClause, Space.Location.CLOSE_SET_CLAUSE_PREFIX, p);
        visit(codeSetClause.getWords(), p);
        visit(codeSetClause.getAlphabetName(), p);
        afterSyntax(codeSetClause, p);
        return codeSetClause;
    }

    public Cobol visitCollatingSequenceAlphabet(Cobol.CollatingSequenceAlphabet collatingSequenceAlphabet, PrintOutputCapture<P> p) {
        beforeSyntax(collatingSequenceAlphabet, Space.Location.COLLATING_SEQUENCE_ALPHABET_PREFIX, p);
        visit(collatingSequenceAlphabet.getWords(), p);
        visit(collatingSequenceAlphabet.getAlphabetName(), p);
        afterSyntax(collatingSequenceAlphabet, p);
        return collatingSequenceAlphabet;
    }

    public Cobol visitCollatingSequenceClause(Cobol.CollatingSequenceClause collatingSequenceClause, PrintOutputCapture<P> p) {
        beforeSyntax(collatingSequenceClause, Space.Location.COLLATING_SEQUENCE_CLAUSE_PREFIX, p);
        visit(collatingSequenceClause.getWords(), p);
        visit(collatingSequenceClause.getIs(), p);
        visit(collatingSequenceClause.getAlphabetName(), p);
        visit(collatingSequenceClause.getAlphanumeric(), p);
        visit(collatingSequenceClause.getNational(), p);
        afterSyntax(collatingSequenceClause, p);
        return collatingSequenceClause;
    }

    public Cobol visitCombinableCondition(Cobol.CombinableCondition combinableCondition, PrintOutputCapture<P> p) {
        beforeSyntax(combinableCondition, Space.Location.COMBINABLE_CONDITION_PREFIX, p);
        visit(combinableCondition.getNot(), p);
        visit(combinableCondition.getSimpleCondition(), p);
        afterSyntax(combinableCondition, p);
        return combinableCondition;
    }

    public Cobol visitCommentEntry(Cobol.CommentEntry commentEntry, PrintOutputCapture<P> p) {
        beforeSyntax(commentEntry, Space.Location.COMMENT_ENTRY_PREFIX, p);
        visit(commentEntry.getComments(), p);
        afterSyntax(commentEntry, p);
        return commentEntry;
    }

    public Cobol visitCommitmentControlClause(Cobol.CommitmentControlClause commitmentControlClause, PrintOutputCapture<P> p) {
        beforeSyntax(commitmentControlClause, Space.Location.COMMITMENT_CONTROL_PREFIX, p);
        visit(commitmentControlClause.getWords(), p);
        visit(commitmentControlClause.getFileName(), p);
        afterSyntax(commitmentControlClause, p);
        return commitmentControlClause;
    }

    public Cobol visitCommunicationDescriptionEntryFormat1(Cobol.CommunicationDescriptionEntryFormat1 communicationDescriptionEntryFormat1, PrintOutputCapture<P> p) {
        beforeSyntax(communicationDescriptionEntryFormat1, Space.Location.COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_1_PREFIX, p);
        visit(communicationDescriptionEntryFormat1.getCd(), p);
        visit(communicationDescriptionEntryFormat1.getName(), p);
        visit(communicationDescriptionEntryFormat1.getWords(), p);
        visit(communicationDescriptionEntryFormat1.getInputs(), p);
        visit(communicationDescriptionEntryFormat1.getDot(), p);
        afterSyntax(communicationDescriptionEntryFormat1, p);
        return communicationDescriptionEntryFormat1;
    }

    public Cobol visitCommunicationDescriptionEntryFormat2(Cobol.CommunicationDescriptionEntryFormat2 communicationDescriptionEntryFormat2, PrintOutputCapture<P> p) {
        beforeSyntax(communicationDescriptionEntryFormat2, Space.Location.COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_2_PREFIX, p);
        visit(communicationDescriptionEntryFormat2.getCd(), p);
        visit(communicationDescriptionEntryFormat2.getName(), p);
        visit(communicationDescriptionEntryFormat2.getWords(), p);
        visit(communicationDescriptionEntryFormat2.getOutputs(), p);
        visit(communicationDescriptionEntryFormat2.getDot(), p);
        afterSyntax(communicationDescriptionEntryFormat2, p);
        return communicationDescriptionEntryFormat2;
    }

    public Cobol visitCommunicationDescriptionEntryFormat3(Cobol.CommunicationDescriptionEntryFormat3 communicationDescriptionEntryFormat3, PrintOutputCapture<P> p) {
        beforeSyntax(communicationDescriptionEntryFormat3, Space.Location.COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_3_PREFIX, p);
        visit(communicationDescriptionEntryFormat3.getCd(), p);
        visit(communicationDescriptionEntryFormat3.getName(), p);
        visit(communicationDescriptionEntryFormat3.getWords(), p);
        visit(communicationDescriptionEntryFormat3.getInitialIOs(), p);
        visit(communicationDescriptionEntryFormat3.getDot(), p);
        afterSyntax(communicationDescriptionEntryFormat3, p);
        return communicationDescriptionEntryFormat3;
    }

    public Cobol visitCommunicationSection(Cobol.CommunicationSection communicationSection, PrintOutputCapture<P> p) {
        beforeSyntax(communicationSection, Space.Location.COMMUNICATION_SECTION_PREFIX, p);
        visit(communicationSection.getWords(), p);
        visit(communicationSection.getDot(), p);
        visit(communicationSection.getEntries(), p);
        afterSyntax(communicationSection, p);
        return communicationSection;
    }

    @Override
    public Cobol visitCompilationUnit(Cobol.CompilationUnit compilationUnit, PrintOutputCapture<P> p) {
        beforeSyntax(compilationUnit, Space.Location.COMPILATION_UNIT_PREFIX, p);
        visit(compilationUnit.getProgramUnits(), p);
        visit(compilationUnit.getEof(), p);
        afterSyntax(compilationUnit, p);
        return compilationUnit;
    }

    public Cobol visitCompute(Cobol.Compute compute, PrintOutputCapture<P> p) {
        beforeSyntax(compute, Space.Location.COMPUTE_PREFIX, p);
        visit(compute.getCompute(), p);
        visit(compute.getRoundables(), p);
        visit(compute.getEqualWord(), p);
        visit(compute.getArithmeticExpression(), p);
        visit(compute.getOnSizeErrorPhrase(), p);
        visit(compute.getNotOnSizeErrorPhrase(), p);
        visit(compute.getEndCompute(), p);
        afterSyntax(compute, p);
        return compute;
    }

    @Override
    public Cobol visitCondition(Cobol.Condition condition, PrintOutputCapture<P> p) {
        beforeSyntax(condition, Space.Location.CONDITION_PREFIX, p);
        visit(condition.getCombinableCondition(), p);
        visit(condition.getAndOrConditions(), p);
        afterSyntax(condition, p);
        return condition;
    }

    public Cobol visitConditionNameReference(Cobol.ConditionNameReference conditionNameReference, PrintOutputCapture<P> p) {
        beforeSyntax(conditionNameReference, Space.Location.CONDITION_NAME_REFERENCE_PREFIX, p);
        visit(conditionNameReference.getName(), p);
        visit(conditionNameReference.getInDatas(), p);
        visit(conditionNameReference.getInFile(), p);
        visit(conditionNameReference.getReferences(), p);
        visit(conditionNameReference.getInMnemonics(), p);
        afterSyntax(conditionNameReference, p);
        return conditionNameReference;
    }

    public Cobol visitConditionNameSubscriptReference(Cobol.ConditionNameSubscriptReference conditionNameSubscriptReference, PrintOutputCapture<P> p) {
        beforeSyntax(conditionNameSubscriptReference, Space.Location.CONDITION_NAME_SUBSCRIPT_REFERENCE_PREFIX, p);
        visit(conditionNameSubscriptReference.getLeftParen(), p);
        visit(conditionNameSubscriptReference.getSubscripts(), p);
        visit(conditionNameSubscriptReference.getRightParen(), p);
        afterSyntax(conditionNameSubscriptReference, p);
        return conditionNameSubscriptReference;
    }

    public Cobol visitConfigurationSection(Cobol.ConfigurationSection configurationSection, PrintOutputCapture<P> p) {
        beforeSyntax(configurationSection, Space.Location.CONFIGURATION_SECTION_PREFIX, p);
        visit(configurationSection.getWords(), p);
        visit(configurationSection.getDot(), p);
        visit(configurationSection.getParagraphs(), p);
        afterSyntax(configurationSection, p);
        return configurationSection;
    }

    public Cobol visitContinue(Cobol.Continue continuez, PrintOutputCapture<P> p) {
        beforeSyntax(continuez, Space.Location.CONTINUE_PREFIX, p);
        visit(continuez.getWord(), p);
        afterSyntax(continuez, p);
        return continuez;
    }

    public Cobol visitCurrencyClause(Cobol.CurrencyClause currencyClause, PrintOutputCapture<P> p) {
        beforeSyntax(currencyClause, Space.Location.CURRENCY_CLAUSE_PREFIX, p);
        visit(currencyClause.getWords(), p);
        visit(currencyClause.getLiteral(), p);
        visit(currencyClause.getPictureSymbols(), p);
        visit(currencyClause.getPictureSymbolLiteral(), p);
        afterSyntax(currencyClause, p);
        return currencyClause;
    }

    public Cobol visitDataAlignedClause(Cobol.DataAlignedClause dataAlignedClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataAlignedClause, Space.Location.DATA_ALIGNED_CLAUSE_PREFIX, p);
        visit(dataAlignedClause.getAligned(), p);
        afterSyntax(dataAlignedClause, p);
        return dataAlignedClause;
    }

    public Cobol visitDataBaseSection(Cobol.DataBaseSection dataBaseSection, PrintOutputCapture<P> p) {
        beforeSyntax(dataBaseSection, Space.Location.DATA_BASE_SECTION_PREFIX, p);
        visit(dataBaseSection.getWords(), p);
        visit(dataBaseSection.getDot(), p);
        visit(dataBaseSection.getEntries(), p);
        afterSyntax(dataBaseSection, p);
        return dataBaseSection;
    }

    public Cobol visitDataBaseSectionEntry(Cobol.DataBaseSectionEntry dataBaseSectionEntry, PrintOutputCapture<P> p) {
        beforeSyntax(dataBaseSectionEntry, Space.Location.DATA_BASE_SECTION_ENTRY_PREFIX, p);
        visit(dataBaseSectionEntry.getDb(), p);
        visit(dataBaseSectionEntry.getFrom(), p);
        visit(dataBaseSectionEntry.getInvoke(), p);
        visit(dataBaseSectionEntry.getTo(), p);
        afterSyntax(dataBaseSectionEntry, p);
        return dataBaseSectionEntry;
    }

    public Cobol visitDataBlankWhenZeroClause(Cobol.DataBlankWhenZeroClause dataBlankWhenZeroClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataBlankWhenZeroClause, Space.Location.DATA_BLANK_WHEN_ZERO_CLAUSE_PREFIX, p);
        visit(dataBlankWhenZeroClause.getWords(), p);
        afterSyntax(dataBlankWhenZeroClause, p);
        return dataBlankWhenZeroClause;
    }

    public Cobol visitDataCommonOwnLocalClause(Cobol.DataCommonOwnLocalClause dataCommonOwnLocalClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataCommonOwnLocalClause, Space.Location.DATA_COMMON_OWN_LOCAL_CLAUSE_PREFIX, p);
        visit(dataCommonOwnLocalClause.getWord(), p);
        afterSyntax(dataCommonOwnLocalClause, p);
        return dataCommonOwnLocalClause;
    }

    public Cobol visitDataDescriptionEntry(Cobol.DataDescriptionEntry dataDescriptionEntry, PrintOutputCapture<P> p) {
        beforeSyntax(dataDescriptionEntry, Space.Location.DATA_DESCRIPTION_ENTRY_PREFIX, p);
        visit(dataDescriptionEntry.getWords(), p);
        visit(dataDescriptionEntry.getName(), p);
        visit(dataDescriptionEntry.getClauses(), p);
        visit(dataDescriptionEntry.getDot(), p);
        afterSyntax(dataDescriptionEntry, p);
        return dataDescriptionEntry;
    }

    public Cobol visitDataDivision(Cobol.DataDivision dataDivision, PrintOutputCapture<P> p) {
        beforeSyntax(dataDivision, Space.Location.DATA_DIVISION_PREFIX, p);
        visit(dataDivision.getWords(), p);
        visit(dataDivision.getDot(), p);
        visit(dataDivision.getSections(), p);
        afterSyntax(dataDivision, p);
        return dataDivision;
    }

    public Cobol visitDataExternalClause(Cobol.DataExternalClause dataExternalClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataExternalClause, Space.Location.DATA_EXTERNAL_CLAUSE_PREFIX, p);
        visit(dataExternalClause.getRedefines(), p);
        afterSyntax(dataExternalClause, p);
        return dataExternalClause;
    }

    public Cobol visitDataGlobalClause(Cobol.DataGlobalClause dataGlobalClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataGlobalClause, Space.Location.DATA_GLOBAL_CLAUSE_PREFIX, p);
        visit(dataGlobalClause.getWords(), p);
        afterSyntax(dataGlobalClause, p);
        return dataGlobalClause;
    }

    public Cobol visitDataIntegerStringClause(Cobol.DataIntegerStringClause dataIntegerStringClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataIntegerStringClause, Space.Location.DATA_INTEGER_STRING_CLAUSE_PREFIX, p);
        visit(dataIntegerStringClause.getWord(), p);
        afterSyntax(dataIntegerStringClause, p);
        return dataIntegerStringClause;
    }

    public Cobol visitDataJustifiedClause(Cobol.DataJustifiedClause dataJustifiedClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataJustifiedClause, Space.Location.DATA_JUSTIFIED_CLAUSE_PREFIX, p);
        visit(dataJustifiedClause.getWords(), p);
        afterSyntax(dataJustifiedClause, p);
        return dataJustifiedClause;
    }

    public Cobol visitDataOccursClause(Cobol.DataOccursClause dataOccursClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataOccursClause, Space.Location.DATA_OCCURS_CLAUSE_PREFIX, p);
        visit(dataOccursClause.getOccurs(), p);
        visit(dataOccursClause.getName(), p);
        visit(dataOccursClause.getDataOccursTo(), p);
        visit(dataOccursClause.getTimes(), p);
        visit(dataOccursClause.getDataOccursDepending(), p);
        visit(dataOccursClause.getSortIndexed(), p);
        afterSyntax(dataOccursClause, p);
        return dataOccursClause;
    }

    public Cobol visitDataOccursDepending(Cobol.DataOccursDepending dataOccursDepending, PrintOutputCapture<P> p) {
        beforeSyntax(dataOccursDepending, Space.Location.DATA_OCCURS_DEPENDING_PREFIX, p);
        visit(dataOccursDepending.getWords(), p);
        visit(dataOccursDepending.getQualifiedDataName(), p);
        afterSyntax(dataOccursDepending, p);
        return dataOccursDepending;
    }

    public Cobol visitDataOccursIndexed(Cobol.DataOccursIndexed dataOccursIndexed, PrintOutputCapture<P> p) {
        beforeSyntax(dataOccursIndexed, Space.Location.DATA_OCCURS_INDEXED_PREFIX, p);
        visit(dataOccursIndexed.getWords(), p);
        visit(dataOccursIndexed.getIndexNames(), p);
        afterSyntax(dataOccursIndexed, p);
        return dataOccursIndexed;
    }

    public Cobol visitDataOccursSort(Cobol.DataOccursSort dataOccursSort, PrintOutputCapture<P> p) {
        beforeSyntax(dataOccursSort, Space.Location.DATA_OCCURS_SORT_PREFIX, p);
        visit(dataOccursSort.getWords(), p);
        visit(dataOccursSort.getQualifiedDataNames(), p);
        afterSyntax(dataOccursSort, p);
        return dataOccursSort;
    }

    public Cobol visitDataOccursTo(Cobol.DataOccursTo dataOccursTo, PrintOutputCapture<P> p) {
        beforeSyntax(dataOccursTo, Space.Location.DATA_OCCURS_TO_PREFIX, p);
        visit(dataOccursTo.getTo(), p);
        visit(dataOccursTo.getIntegerLiteral(), p);
        afterSyntax(dataOccursTo, p);
        return dataOccursTo;
    }

    public Cobol visitDataPictureClause(Cobol.DataPictureClause dataPictureClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataPictureClause, Space.Location.DATA_PICTURE_CLAUSE_PREFIX, p);
        visit(dataPictureClause.getWords(), p);
        visit(dataPictureClause.getPictures(), p);
        afterSyntax(dataPictureClause, p);
        return dataPictureClause;
    }

    public Cobol visitDataReceivedByClause(Cobol.DataReceivedByClause dataReceivedByClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataReceivedByClause, Space.Location.DATA_RECEIVED_BY_CLAUSE_PREFIX, p);
        visit(dataReceivedByClause.getWords(), p);
        afterSyntax(dataReceivedByClause, p);
        return dataReceivedByClause;
    }

    public Cobol visitDataRecordAreaClause(Cobol.DataRecordAreaClause dataRecordAreaClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataRecordAreaClause, Space.Location.DATA_RECORD_AREA_CLAUSE_PREFIX, p);
        visit(dataRecordAreaClause.getWords(), p);
        afterSyntax(dataRecordAreaClause, p);
        return dataRecordAreaClause;
    }

    public Cobol visitDataRecordsClause(Cobol.DataRecordsClause dataRecordsClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataRecordsClause, Space.Location.DATA_RECORDS_CLAUSE_PREFIX, p);
        visit(dataRecordsClause.getWords(), p);
        visit(dataRecordsClause.getDataName(), p);
        afterSyntax(dataRecordsClause, p);
        return dataRecordsClause;
    }

    public Cobol visitDataRedefinesClause(Cobol.DataRedefinesClause dataRedefinesClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataRedefinesClause, Space.Location.DATA_REDEFINES_CLAUSE_PREFIX, p);
        visit(dataRedefinesClause.getRedefines(), p);
        visit(dataRedefinesClause.getDataName(), p);
        afterSyntax(dataRedefinesClause, p);
        return dataRedefinesClause;
    }

    public Cobol visitDataRenamesClause(Cobol.DataRenamesClause dataRenamesClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataRenamesClause, Space.Location.DATA_RENAMES_CLAUSE_PREFIX, p);
        visit(dataRenamesClause.getRenames(), p);
        visit(dataRenamesClause.getFromName(), p);
        visit(dataRenamesClause.getThrough(), p);
        visit(dataRenamesClause.getToName(), p);
        afterSyntax(dataRenamesClause, p);
        return dataRenamesClause;
    }

    public Cobol visitDataSignClause(Cobol.DataSignClause dataSignClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataSignClause, Space.Location.DATA_SIGN_CLAUSE_PREFIX, p);
        visit(dataSignClause.getWords(), p);
        afterSyntax(dataSignClause, p);
        return dataSignClause;
    }

    public Cobol visitDataSynchronizedClause(Cobol.DataSynchronizedClause dataSynchronizedClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataSynchronizedClause, Space.Location.DATA_SYNCHRONIZED_CLAUSE_PREFIX, p);
        visit(dataSynchronizedClause.getWords(), p);
        afterSyntax(dataSynchronizedClause, p);
        return dataSynchronizedClause;
    }

    public Cobol visitDataThreadLocalClause(Cobol.DataThreadLocalClause dataThreadLocalClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataThreadLocalClause, Space.Location.DATA_THREAD_LOCAL_CLAUSE_PREFIX, p);
        visit(dataThreadLocalClause.getWords(), p);
        afterSyntax(dataThreadLocalClause, p);
        return dataThreadLocalClause;
    }

    public Cobol visitDataTypeClause(Cobol.DataTypeClause dataTypeClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataTypeClause, Space.Location.DATA_TYPE_CLAUSE_PREFIX, p);
        visit(dataTypeClause.getWords(), p);
        visit(dataTypeClause.getParenthesized(), p);
        afterSyntax(dataTypeClause, p);
        return dataTypeClause;
    }

    public Cobol visitDataTypeDefClause(Cobol.DataTypeDefClause dataTypeDefClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataTypeDefClause, Space.Location.DATA_TYPE_DEF_CLAUSE_PREFIX, p);
        visit(dataTypeDefClause.getWords(), p);
        afterSyntax(dataTypeDefClause, p);
        return dataTypeDefClause;
    }

    public Cobol visitDataUsageClause(Cobol.DataUsageClause dataUsageClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataUsageClause, Space.Location.DATA_USAGE_CLAUSE_PREFIX, p);
        visit(dataUsageClause.getWords(), p);
        afterSyntax(dataUsageClause, p);
        return dataUsageClause;
    }

    public Cobol visitDataUsingClause(Cobol.DataUsingClause dataUsingClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataUsingClause, Space.Location.DATA_USING_CLAUSE_PREFIX, p);
        visit(dataUsingClause.getWords(), p);
        afterSyntax(dataUsingClause, p);
        return dataUsingClause;
    }

    public Cobol visitDataValueClause(Cobol.DataValueClause dataValueClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataValueClause, Space.Location.DATA_VALUE_CLAUSE_PREFIX, p);
        visit(dataValueClause.getWords(), p);
        visit(dataValueClause.getCobols(), p);
        afterSyntax(dataValueClause, p);
        return dataValueClause;
    }

    public Cobol visitDataValueInterval(Cobol.DataValueInterval dataValueInterval, PrintOutputCapture<P> p) {
        beforeSyntax(dataValueInterval, Space.Location.DATA_VALUE_INTERVAL_PREFIX, p);
        visit(dataValueInterval.getFrom(), p);
        visit(dataValueInterval.getTo(), p);
        afterSyntax(dataValueInterval, p);
        return dataValueInterval;
    }

    public Cobol visitDataValueIntervalTo(Cobol.DataValueIntervalTo dataValueIntervalTo, PrintOutputCapture<P> p) {
        beforeSyntax(dataValueIntervalTo, Space.Location.DATA_VALUE_INTERVAL_TO_PREFIX, p);
        visit(dataValueIntervalTo.getThrough(), p);
        visit(dataValueIntervalTo.getLiteral(), p);
        afterSyntax(dataValueIntervalTo, p);
        return dataValueIntervalTo;
    }

    public Cobol visitDataWithLowerBoundsClause(Cobol.DataWithLowerBoundsClause dataWithLowerBoundsClause, PrintOutputCapture<P> p) {
        beforeSyntax(dataWithLowerBoundsClause, Space.Location.DATA_WITH_LOWER_BOUNDS_CLAUSE_PREFIX, p);
        visit(dataWithLowerBoundsClause.getWords(), p);
        afterSyntax(dataWithLowerBoundsClause, p);
        return dataWithLowerBoundsClause;
    }

    public Cobol visitDecimalPointClause(Cobol.DecimalPointClause decimalPointClause, PrintOutputCapture<P> p) {
        beforeSyntax(decimalPointClause, Space.Location.DECIMAL_POINT_CLAUSE_PREFIX, p);
        visit(decimalPointClause.getWords(), p);
        afterSyntax(decimalPointClause, p);
        return decimalPointClause;
    }

    public Cobol visitDefaultComputationalSignClause(Cobol.DefaultComputationalSignClause defaultComputationalSignClause, PrintOutputCapture<P> p) {
        beforeSyntax(defaultComputationalSignClause, Space.Location.DEFAULT_COMPUTATIONAL_SIGN_CLAUSE_PREFIX, p);
        visit(defaultComputationalSignClause.getWords(), p);
        afterSyntax(defaultComputationalSignClause, p);
        return defaultComputationalSignClause;
    }

    public Cobol visitDefaultDisplaySignClause(Cobol.DefaultDisplaySignClause defaultDisplaySignClause, PrintOutputCapture<P> p) {
        beforeSyntax(defaultDisplaySignClause, Space.Location.DEFAULT_DISPLAY_SIGN_CLAUSE_PREFIX, p);
        visit(defaultDisplaySignClause.getWords(), p);
        afterSyntax(defaultDisplaySignClause, p);
        return defaultDisplaySignClause;
    }

    public Cobol visitDelete(Cobol.Delete delete, PrintOutputCapture<P> p) {
        beforeSyntax(delete, Space.Location.DELETE_PREFIX, p);
        visit(delete.getDelete(), p);
        visit(delete.getFileName(), p);
        visit(delete.getRecord(), p);
        visit(delete.getInvalidKey(), p);
        visit(delete.getNotInvalidKey(), p);
        visit(delete.getEndDelete(), p);
        afterSyntax(delete, p);
        return delete;
    }

    public Cobol visitDestinationCountClause(Cobol.DestinationCountClause destinationCountClause, PrintOutputCapture<P> p) {
        beforeSyntax(destinationCountClause, Space.Location.DESTINATION_COUNT_CLAUSE_PREFIX, p);
        visit(destinationCountClause.getWords(), p);
        visit(destinationCountClause.getDataDescName(), p);
        afterSyntax(destinationCountClause, p);
        return destinationCountClause;
    }

    public Cobol visitDestinationTableClause(Cobol.DestinationTableClause destinationTableClause, PrintOutputCapture<P> p) {
        beforeSyntax(destinationTableClause, Space.Location.DESTINATION_TABLE_CLAUSE_PREFIX, p);
        visit(destinationTableClause.getFirstWords(), p);
        visit(destinationTableClause.getIntegerLiteral(), p);
        visit(destinationTableClause.getSecondWords(), p);
        visit(destinationTableClause.getIndexNames(), p);
        afterSyntax(destinationTableClause, p);
        return destinationTableClause;
    }

    public Cobol visitDisable(Cobol.Disable disable, PrintOutputCapture<P> p) {
        beforeSyntax(disable, Space.Location.DISABLE_PREFIX, p);
        visit(disable.getDisable(), p);
        visit(disable.getTypes(), p);
        visit(disable.getCdName(), p);
        visit(disable.getWith(), p);
        visit(disable.getKey(), p);
        visit(disable.getKeyName(), p);
        afterSyntax(disable, p);
        return disable;
    }

    public Cobol visitDisplay(Cobol.Display display, PrintOutputCapture<P> p) {
        beforeSyntax(display, Space.Location.DISPLAY_PREFIX, p);
        visit(display.getDisplay(), p);
        visit(display.getOperands(), p);
        visit(display.getDisplayAt(), p);
        visit(display.getDisplayUpon(), p);
        visit(display.getDisplayWith(), p);
        visit(display.getOnExceptionClause(), p);
        visit(display.getNotOnExceptionClause(), p);
        visit(display.getEndDisplay(), p);
        afterSyntax(display, p);
        return display;
    }

    public Cobol visitDisplayAt(Cobol.DisplayAt displayAt, PrintOutputCapture<P> p) {
        beforeSyntax(displayAt, Space.Location.DISPLAY_AT_PREFIX, p);
        visit(displayAt.getAt(), p);
        visit(displayAt.getName(), p);
        afterSyntax(displayAt, p);
        return displayAt;
    }

    public Cobol visitDisplayUpon(Cobol.DisplayUpon displayUpon, PrintOutputCapture<P> p) {
        beforeSyntax(displayUpon, Space.Location.DISPLAY_UPON_PREFIX, p);
        visit(displayUpon.getUpon(), p);
        visit(displayUpon.getName(), p);
        afterSyntax(displayUpon, p);
        return displayUpon;
    }

    public Cobol visitDivide(Cobol.Divide divide, PrintOutputCapture<P> p) {
        beforeSyntax(divide, Space.Location.DIVIDE_PREFIX, p);
        visit(divide.getDivide(), p);
        visit(divide.getName(), p);
        visit(divide.getAction(), p);
        visit(divide.getDivideRemainder(), p);
        visit(divide.getOnSizeErrorPhrase(), p);
        visit(divide.getNotOnSizeErrorPhrase(), p);
        visit(divide.getEndDivide(), p);
        afterSyntax(divide, p);
        return divide;
    }

    public Cobol visitDivideGiving(Cobol.DivideGiving divideGiving, PrintOutputCapture<P> p) {
        beforeSyntax(divideGiving, Space.Location.DIVIDE_GIVING_PREFIX, p);
        visit(divideGiving.getWord(), p);
        visit(divideGiving.getName(), p);
        visit(divideGiving.getDivideGivingPhrase(), p);
        afterSyntax(divideGiving, p);
        return divideGiving;
    }

    public Cobol visitDivideGivingPhrase(Cobol.DivideGivingPhrase divideGivingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(divideGivingPhrase, Space.Location.DIVIDE_GIVING_PHRASE_PREFIX, p);
        visit(divideGivingPhrase.getGiving(), p);
        visit(divideGivingPhrase.getRoundables(), p);
        afterSyntax(divideGivingPhrase, p);
        return divideGivingPhrase;
    }

    public Cobol visitDivideInto(Cobol.DivideInto divideInto, PrintOutputCapture<P> p) {
        beforeSyntax(divideInto, Space.Location.DIVIDE_INTO_PREFIX, p);
        visit(divideInto.getInto(), p);
        visit(divideInto.getRoundables(), p);
        afterSyntax(divideInto, p);
        return divideInto;
    }

    public Cobol visitDivideRemainder(Cobol.DivideRemainder divideRemainder, PrintOutputCapture<P> p) {
        beforeSyntax(divideRemainder, Space.Location.DIVIDE_REMAINDER_PREFIX, p);
        visit(divideRemainder.getRemainder(), p);
        visit(divideRemainder.getName(), p);
        afterSyntax(divideRemainder, p);
        return divideRemainder;
    }

    public Cobol visitEnable(Cobol.Enable enable, PrintOutputCapture<P> p) {
        beforeSyntax(enable, Space.Location.ENABLE_PREFIX, p);
        visit(enable.getEnable(), p);
        visit(enable.getTypes(), p);
        visit(enable.getCdName(), p);
        visit(enable.getWith(), p);
        visit(enable.getKey(), p);
        visit(enable.getKeyName(), p);
        afterSyntax(enable, p);
        return enable;
    }

    public Cobol visitEndKeyClause(Cobol.EndKeyClause endKeyClause, PrintOutputCapture<P> p) {
        beforeSyntax(endKeyClause, Space.Location.END_KEY_CLAUSE_PREFIX, p);
        visit(endKeyClause.getWords(), p);
        visit(endKeyClause.getName(), p);
        afterSyntax(endKeyClause, p);
        return endKeyClause;
    }

    public Cobol visitEndProgram(Cobol.EndProgram endProgram, PrintOutputCapture<P> p) {
        beforeSyntax(endProgram, Space.Location.END_PROGRAM_PREFIX, p);
        visit(endProgram.getWords(), p);
        visit(endProgram.getProgramName(), p);
        visit(endProgram.getDot(), p);
        afterSyntax(endProgram, p);
        return endProgram;
    }

    @Override
    public Cobol.Entry visitEntry(Cobol.Entry entry, PrintOutputCapture<P> p) {
        beforeSyntax(entry, Space.Location.ENTRY_PREFIX, p);
        visit(entry.getEntry(), p);
        visit(entry.getLiteral(), p);
        visit(entry.getUsing(), p);
        visit(entry.getIdentifiers(), p);
        afterSyntax(entry, p);
        return entry;
    }

    public Cobol visitEnvironmentDivision(Cobol.EnvironmentDivision environmentDivision, PrintOutputCapture<P> p) {
        beforeSyntax(environmentDivision, Space.Location.ENVIRONMENT_DIVISION_PREFIX, p);
        visit(environmentDivision.getWords(), p);
        visit(environmentDivision.getDot(), p);
        visit(environmentDivision.getBody(), p);
        afterSyntax(environmentDivision, p);
        return environmentDivision;
    }

    public Cobol visitEvaluate(Cobol.Evaluate evaluate, PrintOutputCapture<P> p) {
        beforeSyntax(evaluate, Space.Location.EVALUATE_PREFIX, p);
        visit(evaluate.getEvaluate(), p);
        visit(evaluate.getSelect(), p);
        visit(evaluate.getAlsoSelect(), p);
        visit(evaluate.getWhenPhrase(), p);
        visit(evaluate.getWhenOther(), p);
        visit(evaluate.getEndPhrase(), p);
        afterSyntax(evaluate, p);
        return evaluate;
    }

    public Cobol visitEvaluateAlso(Cobol.EvaluateAlso evaluateAlso, PrintOutputCapture<P> p) {
        beforeSyntax(evaluateAlso, Space.Location.EVALUATE_ALSO_PREFIX, p);
        visit(evaluateAlso.getAlso(), p);
        visit(evaluateAlso.getSelect(), p);
        afterSyntax(evaluateAlso, p);
        return evaluateAlso;
    }

    public Cobol visitEnvironmentSwitchNameClause(Cobol.EnvironmentSwitchNameClause environmentSwitchNameClause, PrintOutputCapture<P> p) {
        beforeSyntax(environmentSwitchNameClause, Space.Location.ENVIRONMENT_SWITCH_NAME_CLAUSE_PREFIX, p);
        visit(environmentSwitchNameClause.getEnvironmentName(), p);
        visit(environmentSwitchNameClause.getIs(), p);
        visit(environmentSwitchNameClause.getMnemonicName(), p);
        visit(environmentSwitchNameClause.getEnvironmentSwitchNameSpecialNamesStatusPhrase(), p);
        afterSyntax(environmentSwitchNameClause, p);
        return environmentSwitchNameClause;
    }

    public Cobol visitEnvironmentSwitchNameSpecialNamesStatusPhrase(Cobol.EnvironmentSwitchNameSpecialNamesStatusPhrase environmentSwitchNameSpecialNamesStatusPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(environmentSwitchNameSpecialNamesStatusPhrase, Space.Location.ENVIRONMENT_SWITCH_NAME_SPECIAL_NAMES_STATUS_PHRASE_PREFIX, p);
        visit(environmentSwitchNameSpecialNamesStatusPhrase.getCobols(), p);
        afterSyntax(environmentSwitchNameSpecialNamesStatusPhrase, p);
        return environmentSwitchNameSpecialNamesStatusPhrase;
    }

    public Cobol visitErrorKeyClause(Cobol.ErrorKeyClause errorKeyClause, PrintOutputCapture<P> p) {
        beforeSyntax(errorKeyClause, Space.Location.ERROR_KEY_CLAUSE_PREFIX, p);
        visit(errorKeyClause.getWords(), p);
        visit(errorKeyClause.getName(), p);
        afterSyntax(errorKeyClause, p);
        return errorKeyClause;
    }

    public Cobol visitEvaluateAlsoCondition(Cobol.EvaluateAlsoCondition evaluateAlsoCondition, PrintOutputCapture<P> p) {
        beforeSyntax(evaluateAlsoCondition, Space.Location.EVALUATE_ALSO_CONDITION_PREFIX, p);
        visit(evaluateAlsoCondition.getAlso(), p);
        visit(evaluateAlsoCondition.getCondition(), p);
        afterSyntax(evaluateAlsoCondition, p);
        return evaluateAlsoCondition;
    }

    public Cobol visitEvaluateCondition(Cobol.EvaluateCondition evaluateCondition, PrintOutputCapture<P> p) {
        beforeSyntax(evaluateCondition, Space.Location.EVALUATE_CONDITION_PREFIX, p);
        visit(evaluateCondition.getWords(), p);
        visit(evaluateCondition.getCondition(), p);
        visit(evaluateCondition.getEvaluateThrough(), p);
        afterSyntax(evaluateCondition, p);
        return evaluateCondition;
    }

    public Cobol visitEvaluateThrough(Cobol.EvaluateThrough evaluateThrough, PrintOutputCapture<P> p) {
        beforeSyntax(evaluateThrough, Space.Location.EVALUATE_THROUGH_PREFIX, p);
        visit(evaluateThrough.getThrough(), p);
        visit(evaluateThrough.getValue(), p);
        afterSyntax(evaluateThrough, p);
        return evaluateThrough;
    }

    public Cobol visitEvaluateValueThrough(Cobol.EvaluateValueThrough evaluateValueThrough, PrintOutputCapture<P> p) {
        beforeSyntax(evaluateValueThrough, Space.Location.EVALUATE_VALUE_THROUGH_PREFIX, p);
        visit(evaluateValueThrough.getNot(), p);
        visit(evaluateValueThrough.getValue(), p);
        visit(evaluateValueThrough.getEvaluateThrough(), p);
        afterSyntax(evaluateValueThrough, p);
        return evaluateValueThrough;
    }

    public Cobol visitEvaluateWhen(Cobol.EvaluateWhen evaluateWhen, PrintOutputCapture<P> p) {
        beforeSyntax(evaluateWhen, Space.Location.EVALUATE_WHEN_PREFIX, p);
        visit(evaluateWhen.getWhen(), p);
        visit(evaluateWhen.getCondition(), p);
        visit(evaluateWhen.getAlsoCondition(), p);
        afterSyntax(evaluateWhen, p);
        return evaluateWhen;
    }

    public Cobol visitEvaluateWhenPhrase(Cobol.EvaluateWhenPhrase evaluateWhenPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(evaluateWhenPhrase, Space.Location.EVALUATE_WHEN_PHRASE_PREFIX, p);
        visit(evaluateWhenPhrase.getWhens(), p);
        visit(evaluateWhenPhrase.getStatements(), p);
        afterSyntax(evaluateWhenPhrase, p);
        return evaluateWhenPhrase;
    }

    public Cobol visitExecCicsStatement(Cobol.ExecCicsStatement execCicsStatement, PrintOutputCapture<P> p) {
        beforeSyntax(execCicsStatement, Space.Location.EXEC_CICS_STATEMENT_PREFIX, p);
        visit(execCicsStatement.getExecCicsLines(), p);
        afterSyntax(execCicsStatement, p);
        return execCicsStatement;
    }

    public Cobol visitExecSqlImsStatement(Cobol.ExecSqlImsStatement execSqlImsStatement, PrintOutputCapture<P> p) {
        beforeSyntax(execSqlImsStatement, Space.Location.EXEC_SQL_IMS_STATEMENT_PREFIX, p);
        visit(execSqlImsStatement.getExecSqlLmsLines(), p);
        afterSyntax(execSqlImsStatement, p);
        return execSqlImsStatement;
    }

    public Cobol visitExecSqlStatement(Cobol.ExecSqlStatement execSqlStatement, PrintOutputCapture<P> p) {
        beforeSyntax(execSqlStatement, Space.Location.EXEC_SQL_STATEMENT_PREFIX, p);
        visit(execSqlStatement.getExecSqlLines(), p);
        afterSyntax(execSqlStatement, p);
        return execSqlStatement;
    }

    public Cobol visitExhibit(Cobol.Exhibit exhibit, PrintOutputCapture<P> p) {
        beforeSyntax(exhibit, Space.Location.EXHIBIT_PREFIX, p);
        visit(exhibit.getWords(), p);
        visit(exhibit.getOperands(), p);
        afterSyntax(exhibit, p);
        return exhibit;
    }

    public Cobol visitExit(Cobol.Exit exit, PrintOutputCapture<P> p) {
        beforeSyntax(exit, Space.Location.EXIT_PREFIX, p);
        visit(exit.getWords(), p);
        afterSyntax(exit, p);
        return exit;
    }

    public Cobol visitExternalClause(Cobol.ExternalClause externalClause, PrintOutputCapture<P> p) {
        beforeSyntax(externalClause, Space.Location.EXTERNAL_CLAUSE_PREFIX, p);
        visit(externalClause.getWords(), p);
        afterSyntax(externalClause, p);
        return externalClause;
    }

    public Cobol visitFigurativeConstant(Cobol.FigurativeConstant figurativeConstant, PrintOutputCapture<P> p) {
        beforeSyntax(figurativeConstant, Space.Location.FIGURATIVE_CONSTANT_PREFIX, p);
        visit(figurativeConstant.getWord(), p);
        visit(figurativeConstant.getLiteral(), p);
        afterSyntax(figurativeConstant, p);
        return figurativeConstant;
    }

    public Cobol visitFileControlEntry(Cobol.FileControlEntry fileControlEntry, PrintOutputCapture<P> p) {
        beforeSyntax(fileControlEntry, Space.Location.FILE_CONTROL_ENTRY_PREFIX, p);
        visit(fileControlEntry.getSelectClause(), p);
        visit(fileControlEntry.getControlClauses(), p);
        afterSyntax(fileControlEntry, p);
        return fileControlEntry;
    }

    public Cobol visitFileControlParagraph(Cobol.FileControlParagraph fileControlParagraph, PrintOutputCapture<P> p) {
        beforeSyntax(fileControlParagraph, Space.Location.FILE_CONTROL_PARAGRAPH_PREFIX, p);
        visit(fileControlParagraph.getFileControl(), p);
        visit(fileControlParagraph.getControlEntries(), p);
        afterSyntax(fileControlParagraph, p);
        return fileControlParagraph;
    }

    public Cobol visitFileDescriptionEntry(Cobol.FileDescriptionEntry fileDescriptionEntry, PrintOutputCapture<P> p) {
        beforeSyntax(fileDescriptionEntry, Space.Location.FILE_DESCRIPTION_ENTRY_PREFIX, p);
        visit(fileDescriptionEntry.getWord(), p);
        visit(fileDescriptionEntry.getName(), p);
        visit(fileDescriptionEntry.getClauses(), p);
        visit(fileDescriptionEntry.getDataDescriptions(), p);
        afterSyntax(fileDescriptionEntry, p);
        return fileDescriptionEntry;
    }

    public Cobol visitFileSection(Cobol.FileSection fileSection, PrintOutputCapture<P> p) {
        beforeSyntax(fileSection, Space.Location.FILE_SECTION_PREFIX, p);
        visit(fileSection.getWords(), p);
        visit(fileSection.getDot(), p);
        visit(fileSection.getFileDescriptionEntry(), p);
        afterSyntax(fileSection, p);
        return fileSection;
    }

    public Cobol visitFileStatusClause(Cobol.FileStatusClause fileStatusClause, PrintOutputCapture<P> p) {
        beforeSyntax(fileStatusClause, Space.Location.FILE_STATUS_CLAUSE_PREFIX, p);
        visit(fileStatusClause.getWords(), p);
        visit(fileStatusClause.getQualifiedDataNames(), p);
        afterSyntax(fileStatusClause, p);
        return fileStatusClause;
    }

    public Cobol visitFunctionCall(Cobol.FunctionCall functionCall, PrintOutputCapture<P> p) {
        beforeSyntax(functionCall, Space.Location.FUNCTION_CALL_PREFIX, p);
        visit(functionCall.getFunction(), p);
        visit(functionCall.getFunctionName(), p);
        visit(functionCall.getArguments(), p);
        visit(functionCall.getReferenceModifier(), p);
        afterSyntax(functionCall, p);
        return functionCall;
    }

    public Cobol visitGenerate(Cobol.Generate generate, PrintOutputCapture<P> p) {
        beforeSyntax(generate, Space.Location.GENERATE_PREFIX, p);
        visit(generate.getGenerate(), p);
        visit(generate.getReportName(), p);
        afterSyntax(generate, p);
        return generate;
    }

    public Cobol visitGlobalClause(Cobol.GlobalClause globalClause, PrintOutputCapture<P> p) {
        beforeSyntax(globalClause, Space.Location.GLOBAL_CLAUSE_PREFIX, p);
        visit(globalClause.getWords(), p);
        afterSyntax(globalClause, p);
        return globalClause;
    }

    public Cobol visitGoBack(Cobol.GoBack goBack, PrintOutputCapture<P> p) {
        beforeSyntax(goBack, Space.Location.GO_BACK_PREFIX, p);
        visit(goBack.getGoBack(), p);
        afterSyntax(goBack, p);
        return goBack;
    }

    public Cobol visitGoTo(Cobol.GoTo _goTo, PrintOutputCapture<P> p) {
        beforeSyntax(_goTo, Space.Location.GO_TO_PREFIX, p);
        visit(_goTo.getWords(), p);
        visit(_goTo.getStatement(), p);
        afterSyntax(_goTo, p);
        return _goTo;
    }

    public Cobol visitGoToDependingOnStatement(Cobol.GoToDependingOnStatement goToDependingOnStatement, PrintOutputCapture<P> p) {
        beforeSyntax(goToDependingOnStatement, Space.Location.GO_TO_DEPENDING_ON_STATEMENT_PREFIX, p);
        visit(goToDependingOnStatement.getProcedureNames(), p);
        visit(goToDependingOnStatement.getWords(), p);
        visit(goToDependingOnStatement.getIdentifier(), p);
        afterSyntax(goToDependingOnStatement, p);
        return goToDependingOnStatement;
    }

    public Cobol visitIdentificationDivision(Cobol.IdentificationDivision identificationDivision, PrintOutputCapture<P> p) {
        beforeSyntax(identificationDivision, Space.Location.IDENTIFICATION_DIVISION_PREFIX, p);
        visit(identificationDivision.getWords(), p);
        visit(identificationDivision.getProgramIdParagraph(), p);
        visit(identificationDivision.getParagraphs(), p);
        afterSyntax(identificationDivision, p);
        return identificationDivision;
    }

    public Cobol visitIdentificationDivisionParagraph(Cobol.IdentificationDivisionParagraph identificationDivisionParagraph, PrintOutputCapture<P> p) {
        beforeSyntax(identificationDivisionParagraph, Space.Location.IDENTIFICATION_DIVISION_PARAGRAPH_PREFIX, p);
        visit(identificationDivisionParagraph.getWord(), p);
        visit(identificationDivisionParagraph.getDot(), p);
        visit(identificationDivisionParagraph.getCommentEntry(), p);
        visit(identificationDivisionParagraph.getWords(), p);
        visit(identificationDivisionParagraph.getDot2(), p);
        afterSyntax(identificationDivisionParagraph, p);
        return identificationDivisionParagraph;
    }

    public Cobol visitIf(Cobol.If _if, PrintOutputCapture<P> p) {
        beforeSyntax(_if, Space.Location.IF_PREFIX, p);
        visit(_if.getWord(), p);
        visit(_if.getCondition(), p);
        visit(_if.getIfThen(), p);
        visit(_if.getIfElse(), p);
        visit(_if.getEndIf(), p);
        afterSyntax(_if, p);
        return _if;
    }

    public Cobol visitIfElse(Cobol.IfElse ifElse, PrintOutputCapture<P> p) {
        beforeSyntax(ifElse, Space.Location.IF_ELSE_PREFIX, p);
        visit(ifElse.getWord(), p);
        visit(ifElse.getNextSentences(), p);
        visit(ifElse.getStatements(), p);
        afterSyntax(ifElse, p);
        return ifElse;
    }

    public Cobol visitIfThen(Cobol.IfThen ifThen, PrintOutputCapture<P> p) {
        beforeSyntax(ifThen, Space.Location.IF_THEN_PREFIX, p);
        visit(ifThen.getWord(), p);
        visit(ifThen.getNextSentences(), p);
        visit(ifThen.getStatements(), p);
        afterSyntax(ifThen, p);
        return ifThen;
    }

    public Cobol visitInData(Cobol.InData inData, PrintOutputCapture<P> p) {
        beforeSyntax(inData, Space.Location.IN_DATA_PREFIX, p);
        visit(inData.getWord(), p);
        visit(inData.getName(), p);
        afterSyntax(inData, p);
        return inData;
    }

    public Cobol visitInFile(Cobol.InFile inFile, PrintOutputCapture<P> p) {
        beforeSyntax(inFile, Space.Location.IN_FILE_PREFIX, p);
        visit(inFile.getWord(), p);
        visit(inFile.getName(), p);
        afterSyntax(inFile, p);
        return inFile;
    }

    public Cobol visitInLibrary(Cobol.InLibrary inLibrary, PrintOutputCapture<P> p) {
        beforeSyntax(inLibrary, Space.Location.IN_LIBRARY_PREFIX, p);
        visit(inLibrary.getWord(), p);
        visit(inLibrary.getName(), p);
        afterSyntax(inLibrary, p);
        return inLibrary;
    }

    public Cobol visitInMnemonic(Cobol.InMnemonic inMnemonic, PrintOutputCapture<P> p) {
        beforeSyntax(inMnemonic, Space.Location.IN_MNEMONIC_PREFIX, p);
        visit(inMnemonic.getWord(), p);
        visit(inMnemonic.getName(), p);
        afterSyntax(inMnemonic, p);
        return inMnemonic;
    }

    public Cobol visitInSection(Cobol.InSection inSection, PrintOutputCapture<P> p) {
        beforeSyntax(inSection, Space.Location.IN_SECTION_PREFIX, p);
        visit(inSection.getWord(), p);
        visit(inSection.getName(), p);
        afterSyntax(inSection, p);
        return inSection;
    }

    public Cobol visitInTable(Cobol.InTable inTable, PrintOutputCapture<P> p) {
        beforeSyntax(inTable, Space.Location.IN_TABLE_PREFIX, p);
        visit(inTable.getWord(), p);
        afterSyntax(inTable, p);
        return inTable;
    }

    public Cobol visitInitialize(Cobol.Initialize initialize, PrintOutputCapture<P> p) {
        beforeSyntax(initialize, Space.Location.INITIALIZE_PREFIX, p);
        visit(initialize.getInitialize(), p);
        visit(initialize.getIdentifiers(), p);
        visit(initialize.getInitializeReplacingPhrase(), p);
        afterSyntax(initialize, p);
        return initialize;
    }

    public Cobol visitInitializeReplacingBy(Cobol.InitializeReplacingBy initializeReplacingBy, PrintOutputCapture<P> p) {
        beforeSyntax(initializeReplacingBy, Space.Location.INITIALIZE_REPLACING_BY_PREFIX, p);
        visit(initializeReplacingBy.getWords(), p);
        visit(initializeReplacingBy.getIdentifier(), p);
        afterSyntax(initializeReplacingBy, p);
        return initializeReplacingBy;
    }

    public Cobol visitInitializeReplacingPhrase(Cobol.InitializeReplacingPhrase initializeReplacingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(initializeReplacingPhrase, Space.Location.INITIALIZE_REPLACING_PHRASE_PREFIX, p);
        visit(initializeReplacingPhrase.getReplacing(), p);
        visit(initializeReplacingPhrase.getInitializeReplacingBy(), p);
        afterSyntax(initializeReplacingPhrase, p);
        return initializeReplacingPhrase;
    }

    public Cobol visitInitiate(Cobol.Initiate initiate, PrintOutputCapture<P> p) {
        beforeSyntax(initiate, Space.Location.INITIATE_PREFIX, p);
        visit(initiate.getInitiate(), p);
        visit(initiate.getReportNames(), p);
        afterSyntax(initiate, p);
        return initiate;
    }

    public Cobol visitInputOutputSection(Cobol.InputOutputSection inputOutputSection, PrintOutputCapture<P> p) {
        beforeSyntax(inputOutputSection, Space.Location.INPUT_OUTPUT_SECTION_PREFIX, p);
        visit(inputOutputSection.getWords(), p);
        visit(inputOutputSection.getParagraphs(), p);
        afterSyntax(inputOutputSection, p);
        return inputOutputSection;
    }

    public Cobol visitInspect(Cobol.Inspect inspect, PrintOutputCapture<P> p) {
        beforeSyntax(inspect, Space.Location.INSPECT_PREFIX, p);
        visit(inspect.getInspect(), p);
        visit(inspect.getIdentifier(), p);
        visit(inspect.getPhrase(), p);
        afterSyntax(inspect, p);
        return inspect;
    }

    public Cobol visitInspectAllLeading(Cobol.InspectAllLeading inspectAllLeading, PrintOutputCapture<P> p) {
        beforeSyntax(inspectAllLeading, Space.Location.INSPECT_ALL_LEADING_PREFIX, p);
        visit(inspectAllLeading.getName(), p);
        visit(inspectAllLeading.getInspections(), p);
        afterSyntax(inspectAllLeading, p);
        return inspectAllLeading;
    }

    public Cobol visitInspectAllLeadings(Cobol.InspectAllLeadings inspectAllLeadings, PrintOutputCapture<P> p) {
        beforeSyntax(inspectAllLeadings, Space.Location.INSPECT_ALL_LEADINGS_PREFIX, p);
        visit(inspectAllLeadings.getWord(), p);
        visit(inspectAllLeadings.getLeadings(), p);
        afterSyntax(inspectAllLeadings, p);
        return inspectAllLeadings;
    }

    public Cobol visitInspectBeforeAfter(Cobol.InspectBeforeAfter inspectBeforeAfter, PrintOutputCapture<P> p) {
        beforeSyntax(inspectBeforeAfter, Space.Location.INSPECT_BEFORE_AFTER_PREFIX, p);
        visit(inspectBeforeAfter.getWords(), p);
        visit(inspectBeforeAfter.getIdentifier(), p);
        afterSyntax(inspectBeforeAfter, p);
        return inspectBeforeAfter;
    }

    public Cobol visitInspectBy(Cobol.InspectBy inspectBy, PrintOutputCapture<P> p) {
        beforeSyntax(inspectBy, Space.Location.INSPECT_BY_PREFIX, p);
        visit(inspectBy.getBy(), p);
        visit(inspectBy.getIdentifier(), p);
        afterSyntax(inspectBy, p);
        return inspectBy;
    }

    public Cobol visitInspectCharacters(Cobol.InspectCharacters inspectCharacters, PrintOutputCapture<P> p) {
        beforeSyntax(inspectCharacters, Space.Location.DATA_WITH_LOWER_BOUNDS_CLAUSE_PREFIX, p);
        visit(inspectCharacters.getCharacter(), p);
        visit(inspectCharacters.getInspections(), p);
        afterSyntax(inspectCharacters, p);
        return inspectCharacters;
    }

    public Cobol visitInspectConvertingPhrase(Cobol.InspectConvertingPhrase inspectConvertingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(inspectConvertingPhrase, Space.Location.INSPECT_CONVERTING_PHRASE_PREFIX, p);
        visit(inspectConvertingPhrase.getConverting(), p);
        visit(inspectConvertingPhrase.getIdentifier(), p);
        visit(inspectConvertingPhrase.getInspectTo(), p);
        visit(inspectConvertingPhrase.getInspections(), p);
        afterSyntax(inspectConvertingPhrase, p);
        return inspectConvertingPhrase;
    }

    public Cobol visitInspectFor(Cobol.InspectFor inspectFor, PrintOutputCapture<P> p) {
        beforeSyntax(inspectFor, Space.Location.INSPECT_FOR_PREFIX, p);
        visit(inspectFor.getIdentifier(), p);
        visit(inspectFor.getWord(), p);
        visit(inspectFor.getInspects(), p);
        afterSyntax(inspectFor, p);
        return inspectFor;
    }

    public Cobol visitInspectReplacingAllLeading(Cobol.InspectReplacingAllLeading inspectReplacingAllLeading, PrintOutputCapture<P> p) {
        beforeSyntax(inspectReplacingAllLeading, Space.Location.INSPECT_REPLACING_ALL_LEADING_PREFIX, p);
        visit(inspectReplacingAllLeading.getIdentifier(), p);
        visit(inspectReplacingAllLeading.getInspectBy(), p);
        visit(inspectReplacingAllLeading.getInspections(), p);
        afterSyntax(inspectReplacingAllLeading, p);
        return inspectReplacingAllLeading;
    }

    public Cobol visitInspectReplacingAllLeadings(Cobol.InspectReplacingAllLeadings inspectReplacingAllLeadings, PrintOutputCapture<P> p) {
        beforeSyntax(inspectReplacingAllLeadings, Space.Location.INSPECT_REPLACING_ALL_LEADINGS_PREFIX, p);
        visit(inspectReplacingAllLeadings.getWord(), p);
        visit(inspectReplacingAllLeadings.getInspections(), p);
        afterSyntax(inspectReplacingAllLeadings, p);
        return inspectReplacingAllLeadings;
    }

    public Cobol visitInspectReplacingCharacters(Cobol.InspectReplacingCharacters inspectReplacingCharacters, PrintOutputCapture<P> p) {
        beforeSyntax(inspectReplacingCharacters, Space.Location.INSPECT_REPLACING_CHARACTERS_PREFIX, p);
        visit(inspectReplacingCharacters.getWord(), p);
        visit(inspectReplacingCharacters.getInspectBy(), p);
        visit(inspectReplacingCharacters.getInspections(), p);
        afterSyntax(inspectReplacingCharacters, p);
        return inspectReplacingCharacters;
    }

    public Cobol visitInspectReplacingPhrase(Cobol.InspectReplacingPhrase inspectReplacingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(inspectReplacingPhrase, Space.Location.INSPECT_REPLACING_PHRASE_PREFIX, p);
        visit(inspectReplacingPhrase.getWord(), p);
        visit(inspectReplacingPhrase.getInspections(), p);
        afterSyntax(inspectReplacingPhrase, p);
        return inspectReplacingPhrase;
    }

    public Cobol visitInspectTallyingPhrase(Cobol.InspectTallyingPhrase inspectTallyingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(inspectTallyingPhrase, Space.Location.INSPECT_TALLYING_PHRASE_PREFIX, p);
        visit(inspectTallyingPhrase.getTallying(), p);
        visit(inspectTallyingPhrase.getInspectFors(), p);
        afterSyntax(inspectTallyingPhrase, p);
        return inspectTallyingPhrase;
    }

    public Cobol visitInspectTallyingReplacingPhrase(Cobol.InspectTallyingReplacingPhrase inspectTallyingReplacingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(inspectTallyingReplacingPhrase, Space.Location.INSPECT_TALLYING_REPLACING_PHRASE_PREFIX, p);
        visit(inspectTallyingReplacingPhrase.getTallying(), p);
        visit(inspectTallyingReplacingPhrase.getInspectFors(), p);
        visit(inspectTallyingReplacingPhrase.getReplacingPhrases(), p);
        afterSyntax(inspectTallyingReplacingPhrase, p);
        return inspectTallyingReplacingPhrase;
    }

    public Cobol visitInspectTo(Cobol.InspectTo inspectTo, PrintOutputCapture<P> p) {
        beforeSyntax(inspectTo, Space.Location.INSPECT_TO_PREFIX, p);
        visit(inspectTo.getTo(), p);
        visit(inspectTo.getIdentifier(), p);
        afterSyntax(inspectTo, p);
        return inspectTo;
    }

    public Cobol visitIoControlParagraph(Cobol.IoControlParagraph ioControlParagraph, PrintOutputCapture<P> p) {
        beforeSyntax(ioControlParagraph, Space.Location.IO_CONTROL_PARAGRAPH_PREFIX, p);
        visit(ioControlParagraph.getIOControl(), p);
        visit(ioControlParagraph.getDot(), p);
        visit(ioControlParagraph.getFileName(), p);
        visit(ioControlParagraph.getFileNameDot(), p);
        visit(ioControlParagraph.getClauses(), p);
        visit(ioControlParagraph.getDot2(), p);
        afterSyntax(ioControlParagraph, p);
        return ioControlParagraph;
    }

    public Cobol visitLabelRecordsClause(Cobol.LabelRecordsClause labelRecordsClause, PrintOutputCapture<P> p) {
        beforeSyntax(labelRecordsClause, Space.Location.LABEL_RECORDS_CLAUSE_PREFIX, p);
        visit(labelRecordsClause.getWords(), p);
        visit(labelRecordsClause.getDataNames(), p);
        afterSyntax(labelRecordsClause, p);
        return labelRecordsClause;
    }

    public Cobol visitLibraryAttributeClauseFormat1(Cobol.LibraryAttributeClauseFormat1 libraryAttributeClauseFormat1, PrintOutputCapture<P> p) {
        beforeSyntax(libraryAttributeClauseFormat1, Space.Location.LIBRARY_ATTRIBUTE_CLAUSE_1_PREFIX, p);
        visit(libraryAttributeClauseFormat1.getWords(), p);
        afterSyntax(libraryAttributeClauseFormat1, p);
        return libraryAttributeClauseFormat1;
    }

    public Cobol visitLibraryAttributeClauseFormat2(Cobol.LibraryAttributeClauseFormat2 libraryAttributeClauseFormat2, PrintOutputCapture<P> p) {
        beforeSyntax(libraryAttributeClauseFormat2, Space.Location.LIBRARY_ATTRIBUTE_CLAUSE_2_PREFIX, p);
        visit(libraryAttributeClauseFormat2.getAttribute(), p);
        visit(libraryAttributeClauseFormat2.getLibraryAttributeFunction(), p);
        visit(libraryAttributeClauseFormat2.getWords(), p);
        visit(libraryAttributeClauseFormat2.getLibraryAttributeParameter(), p);
        visit(libraryAttributeClauseFormat2.getLibraryAttributeTitle(), p);
        afterSyntax(libraryAttributeClauseFormat2, p);
        return libraryAttributeClauseFormat2;
    }

    public Cobol visitLibraryAttributeFunction(Cobol.LibraryAttributeFunction libraryAttributeFunction, PrintOutputCapture<P> p) {
        beforeSyntax(libraryAttributeFunction, Space.Location.LIBRARY_ATTRIBUTE_FUNCTION_PREFIX, p);
        visit(libraryAttributeFunction.getWords(), p);
        visit(libraryAttributeFunction.getLiteral(), p);
        afterSyntax(libraryAttributeFunction, p);
        return libraryAttributeFunction;
    }

    public Cobol visitLibraryAttributeParameter(Cobol.LibraryAttributeParameter libraryAttributeParameter, PrintOutputCapture<P> p) {
        beforeSyntax(libraryAttributeParameter, Space.Location.LIBRARY_ATTRIBUTE_PARAMETER_PREFIX, p);
        visit(libraryAttributeParameter.getWords(), p);
        visit(libraryAttributeParameter.getLiteral(), p);
        afterSyntax(libraryAttributeParameter, p);
        return libraryAttributeParameter;
    }

    public Cobol visitLibraryAttributeTitle(Cobol.LibraryAttributeTitle libraryAttributeTitle, PrintOutputCapture<P> p) {
        beforeSyntax(libraryAttributeTitle, Space.Location.LIBRARY_ATTRIBUTE_TITLE_PREFIX, p);
        visit(libraryAttributeTitle.getWords(), p);
        visit(libraryAttributeTitle.getLiteral(), p);
        afterSyntax(libraryAttributeTitle, p);
        return libraryAttributeTitle;
    }

    public Cobol visitLibraryDescriptionEntryFormat1(Cobol.LibraryDescriptionEntryFormat1 libraryDescriptionEntryFormat1, PrintOutputCapture<P> p) {
        beforeSyntax(libraryDescriptionEntryFormat1, Space.Location.LIBRARY_DESCRIPTION_ENTRY_FORMAT_1_PREFIX, p);
        visit(libraryDescriptionEntryFormat1.getLd(), p);
        visit(libraryDescriptionEntryFormat1.getLibraryName(), p);
        visit(libraryDescriptionEntryFormat1.getExport(), p);
        visit(libraryDescriptionEntryFormat1.getLibraryAttributeClauseFormat1(), p);
        visit(libraryDescriptionEntryFormat1.getLibraryEntryProcedureClauseFormat1(), p);
        afterSyntax(libraryDescriptionEntryFormat1, p);
        return libraryDescriptionEntryFormat1;
    }

    public Cobol visitLibraryDescriptionEntryFormat2(Cobol.LibraryDescriptionEntryFormat2 libraryDescriptionEntryFormat2, PrintOutputCapture<P> p) {
        beforeSyntax(libraryDescriptionEntryFormat2, Space.Location.LIBRARY_DESCRIPTION_ENTRY_FORMAT_2_PREFIX, p);
        visit(libraryDescriptionEntryFormat2.getLb(), p);
        visit(libraryDescriptionEntryFormat2.getLibraryName(), p);
        visit(libraryDescriptionEntryFormat2.getExport(), p);
        visit(libraryDescriptionEntryFormat2.getLibraryIsGlobalClause(), p);
        visit(libraryDescriptionEntryFormat2.getLibraryIsCommonClause(), p);
        visit(libraryDescriptionEntryFormat2.getClauseFormats(), p);
        afterSyntax(libraryDescriptionEntryFormat2, p);
        return libraryDescriptionEntryFormat2;
    }

    public Cobol visitLibraryEntryProcedureClauseFormat1(Cobol.LibraryEntryProcedureClauseFormat1 libraryEntryProcedureClauseFormat1, PrintOutputCapture<P> p) {
        beforeSyntax(libraryEntryProcedureClauseFormat1, Space.Location.LIBRARY_ENTRY_PROCEDURE_CLAUSE_FORMAT_1_PREFIX, p);
        visit(libraryEntryProcedureClauseFormat1.getEntryProcedure(), p);
        visit(libraryEntryProcedureClauseFormat1.getProgramName(), p);
        visit(libraryEntryProcedureClauseFormat1.getLibraryEntryProcedureForClause(), p);
        afterSyntax(libraryEntryProcedureClauseFormat1, p);
        return libraryEntryProcedureClauseFormat1;
    }

    public Cobol visitLibraryEntryProcedureClauseFormat2(Cobol.LibraryEntryProcedureClauseFormat2 libraryEntryProcedureClauseFormat2, PrintOutputCapture<P> p) {
        beforeSyntax(libraryEntryProcedureClauseFormat2, Space.Location.LIBRARY_ENTRY_PROCEDURE_CLAUSE_FORMAT_2_PREFIX, p);
        visit(libraryEntryProcedureClauseFormat2.getEntryProcedure(), p);
        visit(libraryEntryProcedureClauseFormat2.getProgramName(), p);
        visit(libraryEntryProcedureClauseFormat2.getLibraryEntryProcedureForClause(), p);
        visit(libraryEntryProcedureClauseFormat2.getLibraryEntryProcedureWithClause(), p);
        visit(libraryEntryProcedureClauseFormat2.getLibraryEntryProcedureUsingClause(), p);
        visit(libraryEntryProcedureClauseFormat2.getLibraryEntryProcedureGivingClause(), p);
        afterSyntax(libraryEntryProcedureClauseFormat2, p);
        return libraryEntryProcedureClauseFormat2;
    }

    public Cobol visitLibraryEntryProcedureForClause(Cobol.LibraryEntryProcedureForClause libraryEntryProcedureForClause, PrintOutputCapture<P> p) {
        beforeSyntax(libraryEntryProcedureForClause, Space.Location.LIBRARY_ENTRY_PROCEDURE_FOR_CLAUSE_PREFIX, p);
        visit(libraryEntryProcedureForClause.getWord(), p);
        visit(libraryEntryProcedureForClause.getLiteral(), p);
        afterSyntax(libraryEntryProcedureForClause, p);
        return libraryEntryProcedureForClause;
    }

    public Cobol visitLibraryEntryProcedureGivingClause(Cobol.LibraryEntryProcedureGivingClause libraryEntryProcedureGivingClause, PrintOutputCapture<P> p) {
        beforeSyntax(libraryEntryProcedureGivingClause, Space.Location.LIBRARY_ENTRY_PROCEDURE_GIVING_CLAUSE_PREFIX, p);
        visit(libraryEntryProcedureGivingClause.getGiving(), p);
        visit(libraryEntryProcedureGivingClause.getDataName(), p);
        afterSyntax(libraryEntryProcedureGivingClause, p);
        return libraryEntryProcedureGivingClause;
    }

    public Cobol visitLibraryEntryProcedureUsingClause(Cobol.LibraryEntryProcedureUsingClause libraryEntryProcedureUsingClause, PrintOutputCapture<P> p) {
        beforeSyntax(libraryEntryProcedureUsingClause, Space.Location.LIBRARY_ENTRY_PROCEDURE_USING_CLAUSE_PREFIX, p);
        visit(libraryEntryProcedureUsingClause.getUsing(), p);
        visit(libraryEntryProcedureUsingClause.getNames(), p);
        afterSyntax(libraryEntryProcedureUsingClause, p);
        return libraryEntryProcedureUsingClause;
    }

    public Cobol visitLibraryEntryProcedureWithClause(Cobol.LibraryEntryProcedureWithClause libraryEntryProcedureWithClause, PrintOutputCapture<P> p) {
        beforeSyntax(libraryEntryProcedureWithClause, Space.Location.LIBRARY_ENTRY_PROCEDURE_WITH_CLAUSE_PREFIX, p);
        visit(libraryEntryProcedureWithClause.getWith(), p);
        visit(libraryEntryProcedureWithClause.getNames(), p);
        afterSyntax(libraryEntryProcedureWithClause, p);
        return libraryEntryProcedureWithClause;
    }

    public Cobol visitLibraryIsCommonClause(Cobol.LibraryIsCommonClause libraryIsCommonClause, PrintOutputCapture<P> p) {
        beforeSyntax(libraryIsCommonClause, Space.Location.LIBRARY_IS_COMMON_CLAUSE_PREFIX, p);
        visit(libraryIsCommonClause.getWords(), p);
        afterSyntax(libraryIsCommonClause, p);
        return libraryIsCommonClause;
    }

    public Cobol visitLibraryIsGlobalClause(Cobol.LibraryIsGlobalClause libraryIsGlobalClause, PrintOutputCapture<P> p) {
        beforeSyntax(libraryIsGlobalClause, Space.Location.LIBRARY_IS_GLOBAL_CLAUSE_PREFIX, p);
        visit(libraryIsGlobalClause.getWords(), p);
        afterSyntax(libraryIsGlobalClause, p);
        return libraryIsGlobalClause;
    }

    public Cobol visitLinageClause(Cobol.LinageClause linageClause, PrintOutputCapture<P> p) {
        beforeSyntax(linageClause, Space.Location.LINAGE_CLAUSE_PREFIX, p);
        visit(linageClause.getWords(), p);
        visit(linageClause.getName(), p);
        visit(linageClause.getLine(), p);
        visit(linageClause.getLinageAt(), p);
        afterSyntax(linageClause, p);
        return linageClause;
    }

    public Cobol visitLinageFootingAt(Cobol.LinageFootingAt linageFootingAt, PrintOutputCapture<P> p) {
        beforeSyntax(linageFootingAt, Space.Location.LINAGE_FOOTING_AT_PREFIX, p);
        visit(linageFootingAt.getWords(), p);
        visit(linageFootingAt.getName(), p);
        afterSyntax(linageFootingAt, p);
        return linageFootingAt;
    }

    public Cobol visitLinageLinesAtBottom(Cobol.LinageLinesAtBottom linageLinesAtBottom, PrintOutputCapture<P> p) {
        beforeSyntax(linageLinesAtBottom, Space.Location.LINAGE_LINES_AT_BOTTOM_PREFIX, p);
        visit(linageLinesAtBottom.getWords(), p);
        visit(linageLinesAtBottom.getName(), p);
        afterSyntax(linageLinesAtBottom, p);
        return linageLinesAtBottom;
    }

    public Cobol visitLinageLinesAtTop(Cobol.LinageLinesAtTop linageLinesAtTop, PrintOutputCapture<P> p) {
        beforeSyntax(linageLinesAtTop, Space.Location.LINAGE_LINES_AT_TOP_PREFIX, p);
        visit(linageLinesAtTop.getWords(), p);
        visit(linageLinesAtTop.getName(), p);
        afterSyntax(linageLinesAtTop, p);
        return linageLinesAtTop;
    }

    public Cobol visitLinkageSection(Cobol.LinkageSection linkageSection, PrintOutputCapture<P> p) {
        beforeSyntax(linkageSection, Space.Location.LINKAGE_SECTION_PREFIX, p);
        visit(linkageSection.getWords(), p);
        visit(linkageSection.getDot(), p);
        visit(linkageSection.getDataDescriptions(), p);
        afterSyntax(linkageSection, p);
        return linkageSection;
    }

    public Cobol visitLocalStorageSection(Cobol.LocalStorageSection localStorageSection, PrintOutputCapture<P> p) {
        beforeSyntax(localStorageSection, Space.Location.LOCAL_STORAGE_SECTION_PREFIX, p);
        visit(localStorageSection.getWords(), p);
        visit(localStorageSection.getDot(), p);
        visit(localStorageSection.getLocalName(), p);
        visit(localStorageSection.getLocalData(), p);
        visit(localStorageSection.getDataDescriptions(), p);
        afterSyntax(localStorageSection, p);
        return localStorageSection;
    }

    public Cobol visitMerge(Cobol.Merge merge, PrintOutputCapture<P> p) {
        beforeSyntax(merge, Space.Location.MERGE_PREFIX, p);
        visit(merge.getWord(), p);
        visit(merge.getFileName(), p);
        visit(merge.getMergeOnKeyClause(), p);
        visit(merge.getMergeCollatingSequencePhrase(), p);
        visit(merge.getMergeUsing(), p);
        visit(merge.getMergeOutputProcedurePhrase(), p);
        visit(merge.getMergeGivingPhrase(), p);
        afterSyntax(merge, p);
        return merge;
    }

    public Cobol visitMergeCollatingSequencePhrase(Cobol.MergeCollatingSequencePhrase mergeCollatingSequencePhrase, PrintOutputCapture<P> p) {
        beforeSyntax(mergeCollatingSequencePhrase, Space.Location.MERGE_COLLATING_SEQUENCE_PHRASE_PREFIX, p);
        visit(mergeCollatingSequencePhrase.getWords(), p);
        visit(mergeCollatingSequencePhrase.getName(), p);
        visit(mergeCollatingSequencePhrase.getMergeCollatingAlphanumeric(), p);
        visit(mergeCollatingSequencePhrase.getMergeCollatingNational(), p);
        afterSyntax(mergeCollatingSequencePhrase, p);
        return mergeCollatingSequencePhrase;
    }

    public Cobol visitMergeGiving(Cobol.MergeGiving mergeGiving, PrintOutputCapture<P> p) {
        beforeSyntax(mergeGiving, Space.Location.MERGE_GIVING_PREFIX, p);
        visit(mergeGiving.getName(), p);
        visit(mergeGiving.getWords(), p);
        afterSyntax(mergeGiving, p);
        return mergeGiving;
    }

    public Cobol visitMergeGivingPhrase(Cobol.MergeGivingPhrase mergeGivingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(mergeGivingPhrase, Space.Location.MERGE_GIVING_PHRASE_PREFIX, p);
        visit(mergeGivingPhrase.getWord(), p);
        visit(mergeGivingPhrase.getMergeGiving(), p);
        afterSyntax(mergeGivingPhrase, p);
        return mergeGivingPhrase;
    }

    public Cobol visitMergeOnKeyClause(Cobol.MergeOnKeyClause mergeOnKeyClause, PrintOutputCapture<P> p) {
        beforeSyntax(mergeOnKeyClause, Space.Location.MERGE_ON_KEY_CLAUSE_PREFIX, p);
        visit(mergeOnKeyClause.getWords(), p);
        visit(mergeOnKeyClause.getQualifiedDataName(), p);
        afterSyntax(mergeOnKeyClause, p);
        return mergeOnKeyClause;
    }

    public Cobol visitMergeOutputProcedurePhrase(Cobol.MergeOutputProcedurePhrase mergeOutputProcedurePhrase, PrintOutputCapture<P> p) {
        beforeSyntax(mergeOutputProcedurePhrase, Space.Location.MERGE_OUTPUT_PROCEDURE_PHRASE_PREFIX, p);
        visit(mergeOutputProcedurePhrase.getWords(), p);
        visit(mergeOutputProcedurePhrase.getProcedureName(), p);
        visit(mergeOutputProcedurePhrase.getMergeOutputThrough(), p);
        afterSyntax(mergeOutputProcedurePhrase, p);
        return mergeOutputProcedurePhrase;
    }

    @Override
    public @Nullable Cobol visitMergeOutputThrough(Cobol.MergeOutputThrough mergeOutputThrough, PrintOutputCapture<P> p) {
        beforeSyntax(mergeOutputThrough, Space.Location.MERGE_OUTPUT_THROUGH_PREFIX, p);
        visit(mergeOutputThrough.getWord(), p);
        afterSyntax(mergeOutputThrough, p);
        return mergeOutputThrough;
    }

    @Override
    public Cobol visitMergeUsing(Cobol.MergeUsing mergeUsing, PrintOutputCapture<P> p) {
        beforeSyntax(mergeUsing, Space.Location.MERGE_USING_PREFIX, p);
        visit(mergeUsing.getWord(), p);
        visit(mergeUsing.getFileNames(), p);
        afterSyntax(mergeUsing, p);
        return mergeUsing;
    }

    public Cobol visitMergeable(Cobol.Mergeable mergeable, PrintOutputCapture<P> p) {
        beforeSyntax(mergeable, Space.Location.MERGEABLE_PREFIX, p);
        visit(mergeable.getWords(), p);
        visit(mergeable.getName(), p);
        afterSyntax(mergeable, p);
        return mergeable;
    }

    public Cobol visitMessageCountClause(Cobol.MessageCountClause messageCountClause, PrintOutputCapture<P> p) {
        beforeSyntax(messageCountClause, Space.Location.MESSAGE_COUNT_CLAUSE_PREFIX, p);
        visit(messageCountClause.getWords(), p);
        visit(messageCountClause.getDataDescName(), p);
        afterSyntax(messageCountClause, p);
        return messageCountClause;
    }

    public Cobol visitMessageDateClause(Cobol.MessageDateClause messageDateClause, PrintOutputCapture<P> p) {
        beforeSyntax(messageDateClause, Space.Location.MESSAGE_DATA_CLAUSE_PREFIX, p);
        visit(messageDateClause.getWords(), p);
        visit(messageDateClause.getDataDescName(), p);
        afterSyntax(messageDateClause, p);
        return messageDateClause;
    }

    public Cobol visitMessageTimeClause(Cobol.MessageTimeClause messageTimeClause, PrintOutputCapture<P> p) {
        beforeSyntax(messageTimeClause, Space.Location.MESSAGE_TIME_CLAUSE_PREFIX, p);
        visit(messageTimeClause.getWords(), p);
        visit(messageTimeClause.getDataDescName(), p);
        afterSyntax(messageTimeClause, p);
        return messageTimeClause;
    }

    public Cobol visitMoveCorrespondingToStatement(Cobol.MoveCorrespondingToStatement moveCorrespondingToStatement, PrintOutputCapture<P> p) {
        beforeSyntax(moveCorrespondingToStatement, Space.Location.MOVE_CORRESPONDING_TO_STATEMENT_PREFIX, p);
        visit(moveCorrespondingToStatement.getWord(), p);
        visit(moveCorrespondingToStatement.getMoveCorrespondingToSendingArea(), p);
        visit(moveCorrespondingToStatement.getTo(), p);
        visit(moveCorrespondingToStatement.getIdentifiers(), p);
        afterSyntax(moveCorrespondingToStatement, p);
        return moveCorrespondingToStatement;
    }

    public Cobol visitMoveStatement(Cobol.MoveStatement moveStatement, PrintOutputCapture<P> p) {
        beforeSyntax(moveStatement, Space.Location.MOVE_STATEMENT_PREFIX, p);
        visit(moveStatement.getWords(), p);
        visit(moveStatement.getMoveToStatement(), p);
        afterSyntax(moveStatement, p);
        return moveStatement;
    }

    public Cobol visitMoveToStatement(Cobol.MoveToStatement moveToStatement, PrintOutputCapture<P> p) {
        beforeSyntax(moveToStatement, Space.Location.MOVE_TO_STATEMENT_PREFIX, p);
        visit(moveToStatement.getFrom(), p);
        visit(moveToStatement.getTo(), p);
        visit(moveToStatement.getNames(), p);
        afterSyntax(moveToStatement, p);
        return moveToStatement;
    }

    public Cobol visitMultDiv(Cobol.MultDiv multDiv, PrintOutputCapture<P> p) {
        beforeSyntax(multDiv, Space.Location.MULT_DIV_PREFIX, p);
        visit(multDiv.getWord(), p);
        visit(multDiv.getPowers(), p);
        afterSyntax(multDiv, p);
        return multDiv;
    }

    public Cobol visitMultDivs(Cobol.MultDivs multDivs, PrintOutputCapture<P> p) {
        beforeSyntax(multDivs, Space.Location.MULT_DIVS_PREFIX, p);
        visit(multDivs.getPowers(), p);
        visit(multDivs.getMultDivs(), p);
        afterSyntax(multDivs, p);
        return multDivs;
    }

    public Cobol visitMultipleFileClause(Cobol.MultipleFileClause multipleFileClause, PrintOutputCapture<P> p) {
        beforeSyntax(multipleFileClause, Space.Location.MULTIPLE_FILE_CLAUSE_PREFIX, p);
        visit(multipleFileClause.getWords(), p);
        visit(multipleFileClause.getFilePositions(), p);
        afterSyntax(multipleFileClause, p);
        return multipleFileClause;
    }

    public Cobol visitMultipleFilePosition(Cobol.MultipleFilePosition multipleFilePosition, PrintOutputCapture<P> p) {
        beforeSyntax(multipleFilePosition, Space.Location.MULTIPLE_FILE_POSITION_PREFIX, p);
        visit(multipleFilePosition.getFileName(), p);
        visit(multipleFilePosition.getPosition(), p);
        visit(multipleFilePosition.getIntegerLiteral(), p);
        afterSyntax(multipleFilePosition, p);
        return multipleFilePosition;
    }

    public Cobol visitMultiply(Cobol.Multiply multiply, PrintOutputCapture<P> p) {
        beforeSyntax(multiply, Space.Location.MULTIPLY_PREFIX, p);
        visit(multiply.getWord(), p);
        visit(multiply.getMultiplicand(), p);
        visit(multiply.getBy(), p);
        visit(multiply.getMultiply(), p);
        visit(multiply.getOnSizeErrorPhrase(), p);
        visit(multiply.getNotOnSizeErrorPhrase(), p);
        visit(multiply.getEndMultiply(), p);
        afterSyntax(multiply, p);
        return multiply;
    }

    public Cobol visitMultiplyGiving(Cobol.MultiplyGiving multiplyGiving, PrintOutputCapture<P> p) {
        beforeSyntax(multiplyGiving, Space.Location.MULTIPLY_GIVING_PREFIX, p);
        visit(multiplyGiving.getOperand(), p);
        visit(multiplyGiving.getGiving(), p);
        visit(multiplyGiving.getResult(), p);
        afterSyntax(multiplyGiving, p);
        return multiplyGiving;
    }

    public Cobol visitMultiplyRegular(Cobol.MultiplyRegular multiplyRegular, PrintOutputCapture<P> p) {
        beforeSyntax(multiplyRegular, Space.Location.MULTIPLY_REGULAR_PREFIX, p);
        visit(multiplyRegular.getOperand(), p);
        afterSyntax(multiplyRegular, p);
        return multiplyRegular;
    }

    public Cobol visitNextSentence(Cobol.NextSentence nextSentence, PrintOutputCapture<P> p) {
        beforeSyntax(nextSentence, Space.Location.NEXT_SENTENCE_PREFIX, p);
        visit(nextSentence.getWords(), p);
        afterSyntax(nextSentence, p);
        return nextSentence;
    }

    public Cobol visitObjectComputer(Cobol.ObjectComputer objectComputer, PrintOutputCapture<P> p) {
        beforeSyntax(objectComputer, Space.Location.OBJECT_COMPUTER_PREFIX, p);
        visit(objectComputer.getWords(), p);
        visit(objectComputer.getComputer(), p);
        afterSyntax(objectComputer, p);
        return objectComputer;
    }

    public Cobol visitObjectComputerDefinition(Cobol.ObjectComputerDefinition objectComputerDefinition, PrintOutputCapture<P> p) {
        beforeSyntax(objectComputerDefinition, Space.Location.OBJECT_COMPUTER_DEFINITION_PREFIX, p);
        visit(objectComputerDefinition.getComputerName(), p);
        visit(objectComputerDefinition.getSpecifications(), p);
        visit(objectComputerDefinition.getDot(), p);
        afterSyntax(objectComputerDefinition, p);
        return objectComputerDefinition;
    }

    public Cobol visitOdtClause(Cobol.OdtClause odtClause, PrintOutputCapture<P> p) {
        beforeSyntax(odtClause, Space.Location.ODT_CLAUSE_PREFIX, p);
        visit(odtClause.getWords(), p);
        visit(odtClause.getMnemonicName(), p);
        afterSyntax(odtClause, p);
        return odtClause;
    }

    public Cobol visitOpen(Cobol.Open open, PrintOutputCapture<P> p) {
        beforeSyntax(open, Space.Location.OPEN_PREFIX, p);
        visit(open.getWord(), p);
        visit(open.getOpen(), p);
        afterSyntax(open, p);
        return open;
    }

    public Cobol visitOpenIOExtendStatement(Cobol.OpenIOExtendStatement openIOExtendStatement, PrintOutputCapture<P> p) {
        beforeSyntax(openIOExtendStatement, Space.Location.OPEN_IO_EXTEND_STATEMENT_PREFIX, p);
        visit(openIOExtendStatement.getWord(), p);
        visit(openIOExtendStatement.getFileNames(), p);
        afterSyntax(openIOExtendStatement, p);
        return openIOExtendStatement;
    }

    public Cobol visitOpenInputOutputStatement(Cobol.OpenInputOutputStatement openInputOutputStatement, PrintOutputCapture<P> p) {
        beforeSyntax(openInputOutputStatement, Space.Location.OPEN_INPUT_OUTPUT_STATEMENT_PREFIX, p);
        visit(openInputOutputStatement.getWord(), p);
        visit(openInputOutputStatement.getOpenInput(), p);
        afterSyntax(openInputOutputStatement, p);
        return openInputOutputStatement;
    }

    public Cobol visitOpenable(Cobol.Openable openable, PrintOutputCapture<P> p) {
        beforeSyntax(openable, Space.Location.OPENABLE_PREFIX, p);
        visit(openable.getFileName(), p);
        visit(openable.getWords(), p);
        afterSyntax(openable, p);
        return openable;
    }

    public Cobol visitOrganizationClause(Cobol.OrganizationClause organizationClause, PrintOutputCapture<P> p) {
        beforeSyntax(organizationClause, Space.Location.ORGANIZATION_CLAUSE_PREFIX, p);
        visit(organizationClause.getWords(), p);
        afterSyntax(organizationClause, p);
        return organizationClause;
    }

    public Cobol visitPaddingCharacterClause(Cobol.PaddingCharacterClause paddingCharacterClause, PrintOutputCapture<P> p) {
        beforeSyntax(paddingCharacterClause, Space.Location.PADDING_CHARACTER_CLAUSE_PREFIX, p);
        visit(paddingCharacterClause.getWords(), p);
        visit(paddingCharacterClause.getName(), p);
        afterSyntax(paddingCharacterClause, p);
        return paddingCharacterClause;
    }

    public Cobol visitParagraph(Cobol.Paragraph paragraph, PrintOutputCapture<P> p) {
        beforeSyntax(paragraph, Space.Location.PARAGRAPH_PREFIX, p);
        visit(paragraph.getParagraphName(), p);
        visit(paragraph.getDot(), p);
        visit(paragraph.getAlteredGoTo(), p);
        visit(paragraph.getSentences(), p);
        afterSyntax(paragraph, p);
        return paragraph;
    }

    public Cobol visitParagraphs(Cobol.Paragraphs paragraphs, PrintOutputCapture<P> p) {
        beforeSyntax(paragraphs, Space.Location.PARAGRAPHS_PREFIX, p);
        visit(paragraphs.getSentences(), p);
        visit(paragraphs.getParagraphs(), p);
        afterSyntax(paragraphs, p);
        return paragraphs;
    }

    public Cobol visitParenthesized(Cobol.Parenthesized parenthesized, PrintOutputCapture<P> p) {
        beforeSyntax(parenthesized, Space.Location.PARENTHESIZED_PREFIX, p);
        visit(parenthesized.getLeftParen(), p);
        visit(parenthesized.getContents(), p);
        visit(parenthesized.getRightParen(), p);
        afterSyntax(parenthesized, p);
        return parenthesized;
    }

    public Cobol visitPasswordClause(Cobol.PasswordClause passwordClause, PrintOutputCapture<P> p) {
        beforeSyntax(passwordClause, Space.Location.PASSWORD_CLAUSE_PREFIX, p);
        visit(passwordClause.getWords(), p);
        visit(passwordClause.getDataName(), p);
        afterSyntax(passwordClause, p);
        return passwordClause;
    }

    public Cobol visitPerform(Cobol.Perform perform, PrintOutputCapture<P> p) {
        beforeSyntax(perform, Space.Location.PERFORM_PREFIX, p);
        visit(perform.getWord(), p);
        visit(perform.getStatement(), p);
        afterSyntax(perform, p);
        return perform;
    }

    public Cobol visitPerformInlineStatement(Cobol.PerformInlineStatement performInlineStatement, PrintOutputCapture<P> p) {
        beforeSyntax(performInlineStatement, Space.Location.PERFORM_IN_LINE_STATEMENT_PREFIX, p);
        visit(performInlineStatement.getPerformType(), p);
        visit(performInlineStatement.getStatements(), p);
        visit(performInlineStatement.getWord(), p);
        afterSyntax(performInlineStatement, p);
        return performInlineStatement;
    }

    public Cobol visitPerformProcedureStatement(Cobol.PerformProcedureStatement performProcedureStatement, PrintOutputCapture<P> p) {
        beforeSyntax(performProcedureStatement, Space.Location.PERFORM_PROCEDURE_STATEMENT_PREFIX, p);
        visit(performProcedureStatement.getProcedureName(), p);
        visit(performProcedureStatement.getWord(), p);
        visit(performProcedureStatement.getThroughProcedure(), p);
        visit(performProcedureStatement.getPerformType(), p);
        afterSyntax(performProcedureStatement, p);
        return performProcedureStatement;
    }

    public Cobol visitPerformTestClause(Cobol.PerformTestClause performTestClause, PrintOutputCapture<P> p) {
        beforeSyntax(performTestClause, Space.Location.PERFORM_TEST_CLAUSE_PREFIX, p);
        visit(performTestClause.getWords(), p);
        afterSyntax(performTestClause, p);
        return performTestClause;
    }

    public Cobol visitPerformTimes(Cobol.PerformTimes performTimes, PrintOutputCapture<P> p) {
        beforeSyntax(performTimes, Space.Location.PERFORM_TIMES_PREFIX, p);
        visit(performTimes.getValue(), p);
        visit(performTimes.getWord(), p);
        afterSyntax(performTimes, p);
        return performTimes;
    }

    public Cobol visitPerformUntil(Cobol.PerformUntil performUntil, PrintOutputCapture<P> p) {
        beforeSyntax(performUntil, Space.Location.PERFORM_UNTIL_PREFIX, p);
        visit(performUntil.getPerformTestClause(), p);
        visit(performUntil.getWord(), p);
        visit(performUntil.getCondition(), p);
        afterSyntax(performUntil, p);
        return performUntil;
    }

    public Cobol visitPerformVarying(Cobol.PerformVarying performVarying, PrintOutputCapture<P> p) {
        beforeSyntax(performVarying, Space.Location.PERFORM_VARYING_PREFIX, p);
        visit(performVarying.getCobols(), p);
        afterSyntax(performVarying, p);
        return performVarying;
    }

    public Cobol visitPerformVaryingClause(Cobol.PerformVaryingClause performVaryingClause, PrintOutputCapture<P> p) {
        beforeSyntax(performVaryingClause, Space.Location.PERFORM_VARYING_CLAUSE_PREFIX, p);
        visit(performVaryingClause.getWord(), p);
        visit(performVaryingClause.getPerformVaryingPhrase(), p);
        visit(performVaryingClause.getPerformAfter(), p);
        afterSyntax(performVaryingClause, p);
        return performVaryingClause;
    }

    public Cobol visitPerformVaryingPhrase(Cobol.PerformVaryingPhrase performVaryingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(performVaryingPhrase, Space.Location.PERFORM_VARYING_PHRASE_PREFIX, p);
        visit(performVaryingPhrase.getName(), p);
        visit(performVaryingPhrase.getFrom(), p);
        visit(performVaryingPhrase.getBy(), p);
        visit(performVaryingPhrase.getUntil(), p);
        afterSyntax(performVaryingPhrase, p);
        return performVaryingPhrase;
    }

    public Cobol visitPerformable(Cobol.Performable performable, PrintOutputCapture<P> p) {
        beforeSyntax(performable, Space.Location.PERFORMABLE_PREFIX, p);
        visit(performable.getWord(), p);
        visit(performable.getExpression(), p);
        afterSyntax(performable, p);
        return performable;
    }

    public Cobol visitPicture(Cobol.Picture picture, PrintOutputCapture<P> p) {
        beforeSyntax(picture, Space.Location.PICTURE_PREFIX, p);
        visit(picture.getWords(), p);
        visit(picture.getParenthesized(), p);
        afterSyntax(picture, p);
        return picture;
    }

    public Cobol visitPictureString(Cobol.PictureString pictureString, PrintOutputCapture<P> p) {
        beforeSyntax(pictureString, Space.Location.PICTURE_STRING_PREFIX, p);
        visit(pictureString.getPictures(), p);
        afterSyntax(pictureString, p);
        return pictureString;
    }

    public Cobol visitPlusMinus(Cobol.PlusMinus plusMinus, PrintOutputCapture<P> p) {
        beforeSyntax(plusMinus, Space.Location.PLUS_MINUS_PREFIX, p);
        visit(plusMinus.getWord(), p);
        visit(plusMinus.getMultDivs(), p);
        afterSyntax(plusMinus, p);
        return plusMinus;
    }

    public Cobol visitPower(Cobol.Power power, PrintOutputCapture<P> p) {
        beforeSyntax(power, Space.Location.POWER_PREFIX, p);
        visit(power.getPower(), p);
        visit(power.getExpression(), p);
        afterSyntax(power, p);
        return power;
    }

    public Cobol visitPowers(Cobol.Powers powers, PrintOutputCapture<P> p) {
        beforeSyntax(powers, Space.Location.POWERS_PREFIX, p);
        visit(powers.getPlusMinusChar(), p);
        visit(powers.getExpression(), p);
        visit(powers.getPowers(), p);
        afterSyntax(powers, p);
        return powers;
    }

    public Cobol visitProcedureDeclarative(Cobol.ProcedureDeclarative procedureDeclarative, PrintOutputCapture<P> p) {
        beforeSyntax(procedureDeclarative, Space.Location.PROCEDURE_DECLARATIVE_PREFIX, p);
        visit(procedureDeclarative.getProcedureSectionHeader(), p);
        visit(procedureDeclarative.getDot(), p);
        visit(procedureDeclarative.getUseStatement(), p);
        visit(procedureDeclarative.getDot2(), p);
        visit(procedureDeclarative.getParagraphs(), p);
        afterSyntax(procedureDeclarative, p);
        return procedureDeclarative;
    }

    public Cobol visitProcedureDeclaratives(Cobol.ProcedureDeclaratives procedureDeclaratives, PrintOutputCapture<P> p) {
        beforeSyntax(procedureDeclaratives, Space.Location.PROCEDURE_DECLARATIVES_PREFIX, p);
        visit(procedureDeclaratives.getDeclaratives(), p);
        visit(procedureDeclaratives.getDot(), p);
        visit(procedureDeclaratives.getProcedureDeclarative(), p);
        visit(procedureDeclaratives.getEndDeclaratives(), p);
        visit(procedureDeclaratives.getDot2(), p);
        afterSyntax(procedureDeclaratives, p);
        return procedureDeclaratives;
    }

    public Cobol visitProcedureDivision(Cobol.ProcedureDivision procedureDivision, PrintOutputCapture<P> p) {
        beforeSyntax(procedureDivision, Space.Location.PROCEDURE_DIVISION_PREFIX, p);
        visit(procedureDivision.getWords(), p);
        visit(procedureDivision.getProcedureDivisionUsingClause(), p);
        visit(procedureDivision.getProcedureDivisionGivingClause(), p);
        visit(procedureDivision.getDot(), p);
        visit(procedureDivision.getProcedureDeclaratives(), p);
        visit(procedureDivision.getBody(), p);
        afterSyntax(procedureDivision, p);
        return procedureDivision;
    }

    public Cobol visitProcedureDivisionBody(Cobol.ProcedureDivisionBody procedureDivisionBody, PrintOutputCapture<P> p) {
        beforeSyntax(procedureDivisionBody, Space.Location.PROCEDURE_DIVISION_BODY_PREFIX, p);
        visit(procedureDivisionBody.getParagraphs(), p);
        visit(procedureDivisionBody.getProcedureSection(), p);
        afterSyntax(procedureDivisionBody, p);
        return procedureDivisionBody;
    }

    public Cobol visitProcedureDivisionByReference(Cobol.ProcedureDivisionByReference procedureDivisionByReference, PrintOutputCapture<P> p) {
        beforeSyntax(procedureDivisionByReference, Space.Location.PROCEDURE_DIVISION_BY_REFERENCE_PREFIX, p);
        visit(procedureDivisionByReference.getWord(), p);
        visit(procedureDivisionByReference.getReference(), p);
        afterSyntax(procedureDivisionByReference, p);
        return procedureDivisionByReference;
    }

    public Cobol visitProcedureDivisionByReferencePhrase(Cobol.ProcedureDivisionByReferencePhrase procedureDivisionByReferencePhrase, PrintOutputCapture<P> p) {
        beforeSyntax(procedureDivisionByReferencePhrase, Space.Location.PROCEDURE_DIVISION_BY_REFERENCE_PHRASE_PREFIX, p);
        visit(procedureDivisionByReferencePhrase.getWords(), p);
        visit(procedureDivisionByReferencePhrase.getProcedureDivisionByReference(), p);
        afterSyntax(procedureDivisionByReferencePhrase, p);
        return procedureDivisionByReferencePhrase;
    }

    public Cobol visitProcedureDivisionByValuePhrase(Cobol.ProcedureDivisionByValuePhrase procedureDivisionByValuePhrase, PrintOutputCapture<P> p) {
        beforeSyntax(procedureDivisionByValuePhrase, Space.Location.PROCEDURE_DIVISION_BY_VALUE_PHRASE_PREFIX, p);
        visit(procedureDivisionByValuePhrase.getWords(), p);
        visit(procedureDivisionByValuePhrase.getPhrases(), p);
        afterSyntax(procedureDivisionByValuePhrase, p);
        return procedureDivisionByValuePhrase;
    }

    public Cobol visitProcedureDivisionGivingClause(Cobol.ProcedureDivisionGivingClause procedureDivisionGivingClause, PrintOutputCapture<P> p) {
        beforeSyntax(procedureDivisionGivingClause, Space.Location.PROCEDURE_DIVISION_GIVING_CLAUSE_PREFIX, p);
        visit(procedureDivisionGivingClause.getWord(), p);
        visit(procedureDivisionGivingClause.getDataName(), p);
        afterSyntax(procedureDivisionGivingClause, p);
        return procedureDivisionGivingClause;
    }

    @SuppressWarnings("NullableProblems")
    @Nullable
    public Cobol visitProcedureDivisionUsingClause(@Nullable Cobol.ProcedureDivisionUsingClause procedureDivisionUsingClause, PrintOutputCapture<P> p) {
        if (procedureDivisionUsingClause == null) {
            return null;
        }
        beforeSyntax(procedureDivisionUsingClause, Space.Location.PROCEDURE_DIVISION_USING_CLAUSE_PREFIX, p);
        visit(procedureDivisionUsingClause.getWord(), p);
        visit(procedureDivisionUsingClause.getProcedureDivisionUsingParameter(), p);
        afterSyntax(procedureDivisionUsingClause, p);
        return procedureDivisionUsingClause;
    }

    public Cobol visitProcedureName(Cobol.ProcedureName procedureName, PrintOutputCapture<P> p) {
        beforeSyntax(procedureName, Space.Location.PROCEDURE_NAME_PREFIX, p);
        visit(procedureName.getParagraphName(), p);
        visit(procedureName.getInSection(), p);
        visit(procedureName.getSectionName(), p);
        afterSyntax(procedureName, p);
        return procedureName;
    }

    public Cobol visitProcedureSection(Cobol.ProcedureSection procedureSection, PrintOutputCapture<P> p) {
        beforeSyntax(procedureSection, Space.Location.PROCEDURE_SECTION_PREFIX, p);
        visit(procedureSection.getProcedureSectionHeader(), p);
        visit(procedureSection.getDot(), p);
        visit(procedureSection.getParagraphs(), p);
        afterSyntax(procedureSection, p);
        return procedureSection;
    }

    public Cobol visitProcedureSectionHeader(Cobol.ProcedureSectionHeader procedureSectionHeader, PrintOutputCapture<P> p) {
        beforeSyntax(procedureSectionHeader, Space.Location.PROCEDURE_SECTION_HEADER_PREFIX, p);
        visit(procedureSectionHeader.getSectionName(), p);
        visit(procedureSectionHeader.getSection(), p);
        visit(procedureSectionHeader.getIdentifier(), p);
        afterSyntax(procedureSectionHeader, p);
        return procedureSectionHeader;
    }

    public Cobol visitProgramIdParagraph(Cobol.ProgramIdParagraph programIdParagraph, PrintOutputCapture<P> p) {
        beforeSyntax(programIdParagraph, Space.Location.PROGRAM_ID_PARAGRAPH_PREFIX, p);
        visit(programIdParagraph.getProgramId(), p);
        visit(programIdParagraph.getDot(), p);
        visit(programIdParagraph.getProgramName(), p);
        visit(programIdParagraph.getProgramAttributes(), p);
        visit(programIdParagraph.getDot2(), p);
        afterSyntax(programIdParagraph, p);
        return programIdParagraph;
    }

    public Cobol visitProgramLibrarySection(Cobol.ProgramLibrarySection programLibrarySection, PrintOutputCapture<P> p) {
        beforeSyntax(programLibrarySection, Space.Location.PROGRAM_LIBRARY_SECTION_PREFIX, p);
        visit(programLibrarySection.getWords(), p);
        visit(programLibrarySection.getLibraryDescriptionEntries(), p);
        afterSyntax(programLibrarySection, p);
        return programLibrarySection;
    }

    public Cobol visitProgramUnit(Cobol.ProgramUnit programUnit, PrintOutputCapture<P> p) {
        beforeSyntax(programUnit, Space.Location.PROGRAM_UNIT_PREFIX, p);
        visit(programUnit.getIdentificationDivision(), p);
        visit(programUnit.getEnvironmentDivision(), p);
        visit(programUnit.getDataDivision(), p);
        visit(programUnit.getProcedureDivision(), p);
        visit(programUnit.getProgramUnits(), p);
        visit(programUnit.getEndProgram(), p);
        afterSyntax(programUnit, p);
        return programUnit;
    }

    public Cobol visitPurge(Cobol.Purge purge, PrintOutputCapture<P> p) {
        beforeSyntax(purge, Space.Location.PURGE_PREFIX, p);
        visit(purge.getPurge(), p);
        visit(purge.getNames(), p);
        afterSyntax(purge, p);
        return purge;
    }

    public Cobol visitQualifiedDataName(Cobol.QualifiedDataName qualifiedDataName, PrintOutputCapture<P> p) {
        beforeSyntax(qualifiedDataName, Space.Location.QUALIFIED_DATA_NAME_PREFIX, p);
        visit(qualifiedDataName.getDataName(), p);
        afterSyntax(qualifiedDataName, p);
        return qualifiedDataName;
    }

    public Cobol visitQualifiedDataNameFormat1(Cobol.QualifiedDataNameFormat1 qualifiedDataNameFormat1, PrintOutputCapture<P> p) {
        beforeSyntax(qualifiedDataNameFormat1, Space.Location.QUALIFIED_DATA_NAME_FORMAT_1_PREFIX, p);
        visit(qualifiedDataNameFormat1.getName(), p);
        visit(qualifiedDataNameFormat1.getQualifiedInData(), p);
        visit(qualifiedDataNameFormat1.getInFile(), p);
        afterSyntax(qualifiedDataNameFormat1, p);
        return qualifiedDataNameFormat1;
    }

    public Cobol visitQualifiedDataNameFormat2(Cobol.QualifiedDataNameFormat2 qualifiedDataNameFormat2, PrintOutputCapture<P> p) {
        beforeSyntax(qualifiedDataNameFormat2, Space.Location.QUALIFIED_DATA_NAME_FORMAT_2_PREFIX, p);
        visit(qualifiedDataNameFormat2.getParagraphName(), p);
        visit(qualifiedDataNameFormat2.getInSection(), p);
        afterSyntax(qualifiedDataNameFormat2, p);
        return qualifiedDataNameFormat2;
    }

    public Cobol visitQualifiedDataNameFormat3(Cobol.QualifiedDataNameFormat3 qualifiedDataNameFormat3, PrintOutputCapture<P> p) {
        beforeSyntax(qualifiedDataNameFormat3, Space.Location.QUALIFIED_DATA_NAME_FORMAT_3_PREFIX, p);
        visit(qualifiedDataNameFormat3.getTextName(), p);
        visit(qualifiedDataNameFormat3.getInLibrary(), p);
        afterSyntax(qualifiedDataNameFormat3, p);
        return qualifiedDataNameFormat3;
    }

    public Cobol visitQualifiedDataNameFormat4(Cobol.QualifiedDataNameFormat4 qualifiedDataNameFormat4, PrintOutputCapture<P> p) {
        beforeSyntax(qualifiedDataNameFormat4, Space.Location.QUALIFIED_DATA_NAME_FORMAT_4_PREFIX, p);
        visit(qualifiedDataNameFormat4.getLinageCounter(), p);
        visit(qualifiedDataNameFormat4.getInFile(), p);
        afterSyntax(qualifiedDataNameFormat4, p);
        return qualifiedDataNameFormat4;
    }

    public Cobol visitQualifiedInData(Cobol.QualifiedInData qualifiedInData, PrintOutputCapture<P> p) {
        beforeSyntax(qualifiedInData, Space.Location.QUALIFIED_IN_DATA_PREFIX, p);
        visit(qualifiedInData.getIn(), p);
        afterSyntax(qualifiedInData, p);
        return qualifiedInData;
    }

    public Cobol visitRead(Cobol.Read read, PrintOutputCapture<P> p) {
        beforeSyntax(read, Space.Location.READ_PREFIX, p);
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
        afterSyntax(read, p);
        return read;
    }

    public Cobol visitReadInto(Cobol.ReadInto readInto, PrintOutputCapture<P> p) {
        beforeSyntax(readInto, Space.Location.READ_INTO_PREFIX, p);
        visit(readInto.getWord(), p);
        visit(readInto.getIdentifier(), p);
        afterSyntax(readInto, p);
        return readInto;
    }

    public Cobol visitReadKey(Cobol.ReadKey readKey, PrintOutputCapture<P> p) {
        beforeSyntax(readKey, Space.Location.READ_KEY_PREFIX, p);
        visit(readKey.getWords(), p);
        visit(readKey.getQualifiedDataName(), p);
        afterSyntax(readKey, p);
        return readKey;
    }

    public Cobol visitReadWith(Cobol.ReadWith readWith, PrintOutputCapture<P> p) {
        beforeSyntax(readWith, Space.Location.READ_WITH_PREFIX, p);
        visit(readWith.getWords(), p);
        afterSyntax(readWith, p);
        return readWith;
    }

    public Cobol visitReceivable(Cobol.Receivable receivable, PrintOutputCapture<P> p) {
        beforeSyntax(receivable, Space.Location.RECEIVABLE_PREFIX, p);
        visit(receivable.getWords(), p);
        visit(receivable.getValue(), p);
        afterSyntax(receivable, p);
        return receivable;
    }

    public Cobol visitReceiveWith(Cobol.ReceiveWith receiveWith, PrintOutputCapture<P> p) {
        beforeSyntax(receiveWith, Space.Location.RECEIVE_WITH_PREFIX, p);
        visit(receiveWith.getWords(), p);
        afterSyntax(receiveWith, p);
        return receiveWith;
    }

    public Cobol visitReceive(Cobol.Receive receive, PrintOutputCapture<P> p) {
        beforeSyntax(receive, Space.Location.RECEIVE_PREFIX, p);
        visit(receive.getReceive(), p);
        visit(receive.getFromOrInto(), p);
        visit(receive.getOnExceptionClause(), p);
        visit(receive.getNotOnExceptionClause(), p);
        visit(receive.getEndReceive(), p);
        afterSyntax(receive, p);
        return receive;
    }

    public Cobol visitReceiveFrom(Cobol.ReceiveFrom receiveFrom, PrintOutputCapture<P> p) {
        beforeSyntax(receiveFrom, Space.Location.RECEIVE_FROM_PREFIX, p);
        visit(receiveFrom.getWords(), p);
        visit(receiveFrom.getDataName(), p);
        afterSyntax(receiveFrom, p);
        return receiveFrom;
    }

    public Cobol visitReceiveFromStatement(Cobol.ReceiveFromStatement receiveFromStatement, PrintOutputCapture<P> p) {
        beforeSyntax(receiveFromStatement, Space.Location.RECEIVE_FROM_STATEMENT_PREFIX, p);
        visit(receiveFromStatement.getDataName(), p);
        visit(receiveFromStatement.getFrom(), p);
        visit(receiveFromStatement.getReceiveFrom(), p);
        visit(receiveFromStatement.getBeforeWithThreadSizeStatus(), p);
        afterSyntax(receiveFromStatement, p);
        return receiveFromStatement;
    }

    public Cobol visitReceiveIntoStatement(Cobol.ReceiveIntoStatement receiveIntoStatement, PrintOutputCapture<P> p) {
        beforeSyntax(receiveIntoStatement, Space.Location.RECEIVE_INTO_STATEMENT_PREFIX, p);
        visit(receiveIntoStatement.getCdName(), p);
        visit(receiveIntoStatement.getWords(), p);
        visit(receiveIntoStatement.getIdentifier(), p);
        visit(receiveIntoStatement.getReceiveNoData(), p);
        visit(receiveIntoStatement.getReceiveWithData(), p);
        afterSyntax(receiveIntoStatement, p);
        return receiveIntoStatement;
    }

    public Cobol visitRecordContainsClause(Cobol.RecordContainsClause recordContainsClause, PrintOutputCapture<P> p) {
        beforeSyntax(recordContainsClause, Space.Location.RECORD_CONTAINS_CLAUSE_PREFIX, p);
        visit(recordContainsClause.getRecord(), p);
        visit(recordContainsClause.getClause(), p);
        afterSyntax(recordContainsClause, p);
        return recordContainsClause;
    }

    public Cobol visitRecordContainsClauseFormat1(Cobol.RecordContainsClauseFormat1 recordContainsClauseFormat1, PrintOutputCapture<P> p) {
        beforeSyntax(recordContainsClauseFormat1, Space.Location.RECORD_CONTAINS_CLAUSE_FORMAT_1_PREFIX, p);
        visit(recordContainsClauseFormat1.getContains(), p);
        visit(recordContainsClauseFormat1.getIntegerLiteral(), p);
        visit(recordContainsClauseFormat1.getCharacters(), p);
        afterSyntax(recordContainsClauseFormat1, p);
        return recordContainsClauseFormat1;
    }

    public Cobol visitRecordContainsClauseFormat2(Cobol.RecordContainsClauseFormat2 recordContainsClauseFormat2, PrintOutputCapture<P> p) {
        beforeSyntax(recordContainsClauseFormat2, Space.Location.RECORD_CONTAINS_CLAUSE_FORMAT_2_PREFIX, p);
        visit(recordContainsClauseFormat2.getWords(), p);
        visit(recordContainsClauseFormat2.getFromClause(), p);
        visit(recordContainsClauseFormat2.getQualifiedDataName(), p);
        afterSyntax(recordContainsClauseFormat2, p);
        return recordContainsClauseFormat2;
    }

    public Cobol visitRecordContainsClauseFormat3(Cobol.RecordContainsClauseFormat3 recordContainsClauseFormat3, PrintOutputCapture<P> p) {
        beforeSyntax(recordContainsClauseFormat3, Space.Location.RECORD_CONTAINS_CLAUSE_FORMAT_3_PREFIX, p);
        visit(recordContainsClauseFormat3.getContains(), p);
        visit(recordContainsClauseFormat3.getIntegerLiteral(), p);
        visit(recordContainsClauseFormat3.getRecordContainsTo(), p);
        visit(recordContainsClauseFormat3.getCharacters(), p);
        afterSyntax(recordContainsClauseFormat3, p);
        return recordContainsClauseFormat3;
    }

    public Cobol visitRecordContainsTo(Cobol.RecordContainsTo recordContainsTo, PrintOutputCapture<P> p) {
        beforeSyntax(recordContainsTo, Space.Location.RECORD_CONTAINS_TO_PREFIX, p);
        visit(recordContainsTo.getTo(), p);
        visit(recordContainsTo.getIntegerLiteral(), p);
        afterSyntax(recordContainsTo, p);
        return recordContainsTo;
    }

    public Cobol visitRecordDelimiterClause(Cobol.RecordDelimiterClause recordDelimiterClause, PrintOutputCapture<P> p) {
        beforeSyntax(recordDelimiterClause, Space.Location.RECORD_DELIMITER_CLAUSE_PREFIX, p);
        visit(recordDelimiterClause.getWords(), p);
        visit(recordDelimiterClause.getName(), p);
        afterSyntax(recordDelimiterClause, p);
        return recordDelimiterClause;
    }

    public Cobol visitRecordKeyClause(Cobol.RecordKeyClause recordKeyClause, PrintOutputCapture<P> p) {
        beforeSyntax(recordKeyClause, Space.Location.RECORD_KEY_CLAUSE_PREFIX, p);
        visit(recordKeyClause.getRecordWords(), p);
        visit(recordKeyClause.getQualifiedDataName(), p);
        visit(recordKeyClause.getPasswordClause(), p);
        visit(recordKeyClause.getDuplicates(), p);
        afterSyntax(recordKeyClause, p);
        return recordKeyClause;
    }

    public Cobol visitRecordingModeClause(Cobol.RecordingModeClause recordingModeClause, PrintOutputCapture<P> p) {
        beforeSyntax(recordingModeClause, Space.Location.RECORDING_MODE_CLAUSE_PREFIX, p);
        visit(recordingModeClause.getWords(), p);
        visit(recordingModeClause.getMode(), p);
        afterSyntax(recordingModeClause, p);
        return recordingModeClause;
    }

    public Cobol visitReferenceModifier(Cobol.ReferenceModifier referenceModifier, PrintOutputCapture<P> p) {
        beforeSyntax(referenceModifier, Space.Location.REFERENCE_MODIFIER_PREFIX, p);
        visit(referenceModifier.getLeftParen(), p);
        visit(referenceModifier.getCharacterPosition(), p);
        visit(referenceModifier.getColon(), p);
        visit(referenceModifier.getLength(), p);
        visit(referenceModifier.getRightParen(), p);
        afterSyntax(referenceModifier, p);
        return referenceModifier;
    }

    public Cobol visitRelationArithmeticComparison(Cobol.RelationArithmeticComparison relationArithmeticComparison, PrintOutputCapture<P> p) {
        beforeSyntax(relationArithmeticComparison, Space.Location.RELATION_ARITHMETIC_COMPARISON_PREFIX, p);
        visit(relationArithmeticComparison.getArithmeticExpressionA(), p);
        visit(relationArithmeticComparison.getRelationalOperator(), p);
        visit(relationArithmeticComparison.getArithmeticExpressionB(), p);
        afterSyntax(relationArithmeticComparison, p);
        return relationArithmeticComparison;
    }

    public Cobol visitRelationCombinedComparison(Cobol.RelationCombinedComparison relationCombinedComparison, PrintOutputCapture<P> p) {
        beforeSyntax(relationCombinedComparison, Space.Location.RELATION_COMBINED_COMPARISON_PREFIX, p);
        visit(relationCombinedComparison.getArithmeticExpression(), p);
        visit(relationCombinedComparison.getRelationalOperator(), p);
        visit(relationCombinedComparison.getCombinedCondition(), p);
        afterSyntax(relationCombinedComparison, p);
        return relationCombinedComparison;
    }

    public Cobol visitRelationCombinedCondition(Cobol.RelationCombinedCondition relationCombinedCondition, PrintOutputCapture<P> p) {
        beforeSyntax(relationCombinedCondition, Space.Location.RELATION_COMBINED_CONDITION_PREFIX, p);
        visit(relationCombinedCondition.getRelationalArithmeticExpressions(), p);
        afterSyntax(relationCombinedCondition, p);
        return relationCombinedCondition;
    }

    public Cobol visitRelationSignCondition(Cobol.RelationSignCondition relationSignCondition, PrintOutputCapture<P> p) {
        beforeSyntax(relationSignCondition, Space.Location.RELATION_SIGN_CONDITION_PREFIX, p);
        visit(relationSignCondition.getArithmeticExpression(), p);
        visit(relationSignCondition.getWords(), p);
        afterSyntax(relationSignCondition, p);
        return relationSignCondition;
    }

    public Cobol visitRelationalOperator(Cobol.RelationalOperator relationalOperator, PrintOutputCapture<P> p) {
        beforeSyntax(relationalOperator, Space.Location.RELATIONAL_OPERATOR_PREFIX, p);
        visit(relationalOperator.getWords(), p);
        afterSyntax(relationalOperator, p);
        return relationalOperator;
    }

    public Cobol visitRelativeKeyClause(Cobol.RelativeKeyClause relativeKeyClause, PrintOutputCapture<P> p) {
        beforeSyntax(relativeKeyClause, Space.Location.RELATIVE_KEY_CLAUSE_PREFIX, p);
        visit(relativeKeyClause.getWords(), p);
        visit(relativeKeyClause.getQualifiedDataName(), p);
        afterSyntax(relativeKeyClause, p);
        return relativeKeyClause;
    }

    public Cobol visitRelease(Cobol.Release release, PrintOutputCapture<P> p) {
        beforeSyntax(release, Space.Location.RELEASE_PREFIX, p);
        visit(release.getRelease(), p);
        visit(release.getRecordName(), p);
        visit(release.getFrom(), p);
        visit(release.getQualifiedDataName(), p);
        afterSyntax(release, p);
        return release;
    }

    public Cobol visitReportClause(Cobol.ReportClause reportClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportClause, Space.Location.REPORT_CLAUSE_PREFIX, p);
        visit(reportClause.getWords(), p);
        visit(reportClause.getReportName(), p);
        afterSyntax(reportClause, p);
        return reportClause;
    }

    public Cobol visitReportDescription(Cobol.ReportDescription reportDescription, PrintOutputCapture<P> p) {
        beforeSyntax(reportDescription, Space.Location.REPORT_DESCRIPTION_PREFIX, p);
        visit(reportDescription.getReportDescriptionEntry(), p);
        visit(reportDescription.getGroupDescriptionEntries(), p);
        afterSyntax(reportDescription, p);
        return reportDescription;
    }

    public Cobol visitReportDescriptionEntry(Cobol.ReportDescriptionEntry reportDescriptionEntry, PrintOutputCapture<P> p) {
        beforeSyntax(reportDescriptionEntry, Space.Location.REPORT_DESCRIPTION_ENTRY_PREFIX, p);
        visit(reportDescriptionEntry.getRd(), p);
        visit(reportDescriptionEntry.getQualifiedDataName(), p);
        visit(reportDescriptionEntry.getReportDescriptionGlobalClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionPageLimitClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionHeadingClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionFirstDetailClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionLastDetailClause(), p);
        visit(reportDescriptionEntry.getReportDescriptionFootingClause(), p);
        visit(reportDescriptionEntry.getDot(), p);
        afterSyntax(reportDescriptionEntry, p);
        return reportDescriptionEntry;
    }

    public Cobol visitReportDescriptionFirstDetailClause(Cobol.ReportDescriptionFirstDetailClause reportDescriptionFirstDetailClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportDescriptionFirstDetailClause, Space.Location.REPORT_DESCRIPTION_FIRST_DETAIL_CLAUSE_PREFIX, p);
        visit(reportDescriptionFirstDetailClause.getWords(), p);
        visit(reportDescriptionFirstDetailClause.getDataName(), p);
        afterSyntax(reportDescriptionFirstDetailClause, p);
        return reportDescriptionFirstDetailClause;
    }

    public Cobol visitReportDescriptionFootingClause(Cobol.ReportDescriptionFootingClause reportDescriptionFootingClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportDescriptionFootingClause, Space.Location.REPORT_DESCRIPTION_FOOTING_CLAUSE_PREFIX, p);
        visit(reportDescriptionFootingClause.getWord(), p);
        visit(reportDescriptionFootingClause.getDataName(), p);
        afterSyntax(reportDescriptionFootingClause, p);
        return reportDescriptionFootingClause;
    }

    public Cobol visitReportDescriptionGlobalClause(Cobol.ReportDescriptionGlobalClause reportDescriptionGlobalClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportDescriptionGlobalClause, Space.Location.REPORT_DESCRIPTION_GLOBAL_CLAUSE_PREFIX, p);
        visit(reportDescriptionGlobalClause.getWords(), p);
        afterSyntax(reportDescriptionGlobalClause, p);
        return reportDescriptionGlobalClause;
    }

    public Cobol visitReportDescriptionHeadingClause(Cobol.ReportDescriptionHeadingClause reportDescriptionHeadingClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportDescriptionHeadingClause, Space.Location.REPORT_DESCRIPTION_HEADING_CLAUSE_PREFIX, p);
        visit(reportDescriptionHeadingClause.getWord(), p);
        visit(reportDescriptionHeadingClause.getDataName(), p);
        afterSyntax(reportDescriptionHeadingClause, p);
        return reportDescriptionHeadingClause;
    }

    public Cobol visitReportDescriptionLastDetailClause(Cobol.ReportDescriptionLastDetailClause reportDescriptionLastDetailClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportDescriptionLastDetailClause, Space.Location.REPORT_DESCRIPTION_LAST_DETAIL_CLAUSE_PREFIX, p);
        visit(reportDescriptionLastDetailClause.getWords(), p);
        visit(reportDescriptionLastDetailClause.getDataName(), p);
        afterSyntax(reportDescriptionLastDetailClause, p);
        return reportDescriptionLastDetailClause;
    }

    public Cobol visitReportDescriptionPageLimitClause(Cobol.ReportDescriptionPageLimitClause reportDescriptionPageLimitClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportDescriptionPageLimitClause, Space.Location.REPORT_DESCRIPTION_PAGE_LIMIT_CLAUSE_PREFIX, p);
        visit(reportDescriptionPageLimitClause.getFirstWords(), p);
        visit(reportDescriptionPageLimitClause.getIntegerLiteral(), p);
        visit(reportDescriptionPageLimitClause.getSecondWords(), p);
        afterSyntax(reportDescriptionPageLimitClause, p);
        return reportDescriptionPageLimitClause;
    }

    public Cobol visitReportGroupBlankWhenZeroClause(Cobol.ReportGroupBlankWhenZeroClause reportGroupBlankWhenZeroClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupBlankWhenZeroClause, Space.Location.REPORT_GROUP_BLANK_WHEN_ZERO_CLAUSE_PREFIX, p);
        visit(reportGroupBlankWhenZeroClause.getWords(), p);
        afterSyntax(reportGroupBlankWhenZeroClause, p);
        return reportGroupBlankWhenZeroClause;
    }

    public Cobol visitReportGroupColumnNumberClause(Cobol.ReportGroupColumnNumberClause reportGroupColumnNumberClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupColumnNumberClause, Space.Location.REPORT_GROUP_COLUMN_NUMBER_CLAUSE_PREFIX, p);
        visit(reportGroupColumnNumberClause.getWords(), p);
        visit(reportGroupColumnNumberClause.getDataName(), p);
        afterSyntax(reportGroupColumnNumberClause, p);
        return reportGroupColumnNumberClause;
    }

    public Cobol visitReportGroupDescriptionEntryFormat1(Cobol.ReportGroupDescriptionEntryFormat1 reportGroupDescriptionEntryFormat1, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupDescriptionEntryFormat1, Space.Location.REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_1_PREFIX, p);
        visit(reportGroupDescriptionEntryFormat1.getIntegerLiteral(), p);
        visit(reportGroupDescriptionEntryFormat1.getDataName(), p);
        visit(reportGroupDescriptionEntryFormat1.getGroupLineNumberClause(), p);
        visit(reportGroupDescriptionEntryFormat1.getGroupNextGroupClause(), p);
        visit(reportGroupDescriptionEntryFormat1.getGroupTypeClause(), p);
        visit(reportGroupDescriptionEntryFormat1.getGroupUsageClause(), p);
        visit(reportGroupDescriptionEntryFormat1.getDot(), p);
        afterSyntax(reportGroupDescriptionEntryFormat1, p);
        return reportGroupDescriptionEntryFormat1;
    }

    public Cobol visitReportGroupDescriptionEntryFormat2(Cobol.ReportGroupDescriptionEntryFormat2 reportGroupDescriptionEntryFormat2, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupDescriptionEntryFormat2, Space.Location.REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_2_PREFIX, p);
        visit(reportGroupDescriptionEntryFormat2.getIntegerLiteral(), p);
        visit(reportGroupDescriptionEntryFormat2.getDataName(), p);
        visit(reportGroupDescriptionEntryFormat2.getReportGroupLineNumberClause(), p);
        visit(reportGroupDescriptionEntryFormat2.getGroupUsageClause(), p);
        visit(reportGroupDescriptionEntryFormat2.getDot(), p);
        afterSyntax(reportGroupDescriptionEntryFormat2, p);
        return reportGroupDescriptionEntryFormat2;
    }

    public Cobol visitReportGroupDescriptionEntryFormat3(Cobol.ReportGroupDescriptionEntryFormat3 reportGroupDescriptionEntryFormat3, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupDescriptionEntryFormat3, Space.Location.REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_3_PREFIX, p);
        visit(reportGroupDescriptionEntryFormat3.getIntegerLiteral(), p);
        visit(reportGroupDescriptionEntryFormat3.getDataName(), p);
        visit(reportGroupDescriptionEntryFormat3.getClauses(), p);
        visit(reportGroupDescriptionEntryFormat3.getDot(), p);
        afterSyntax(reportGroupDescriptionEntryFormat3, p);
        return reportGroupDescriptionEntryFormat3;
    }

    public Cobol visitReportGroupIndicateClause(Cobol.ReportGroupIndicateClause reportGroupIndicateClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupIndicateClause, Space.Location.REPORT_GROUP_INDICATOR_CLAUSE_PREFIX, p);
        visit(reportGroupIndicateClause.getWords(), p);
        afterSyntax(reportGroupIndicateClause, p);
        return reportGroupIndicateClause;
    }

    public Cobol visitReportGroupJustifiedClause(Cobol.ReportGroupJustifiedClause reportGroupJustifiedClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupJustifiedClause, Space.Location.REPORT_GROUP_JUSTIFIED_CLAUSE_PREFIX, p);
        visit(reportGroupJustifiedClause.getWords(), p);
        afterSyntax(reportGroupJustifiedClause, p);
        return reportGroupJustifiedClause;
    }

    public Cobol visitReportGroupLineNumberClause(Cobol.ReportGroupLineNumberClause reportGroupLineNumberClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupLineNumberClause, Space.Location.REPORT_GROUP_LINE_NUMBER_CLAUSE_PREFIX, p);
        visit(reportGroupLineNumberClause.getWords(), p);
        visit(reportGroupLineNumberClause.getClause(), p);
        afterSyntax(reportGroupLineNumberClause, p);
        return reportGroupLineNumberClause;
    }

    public Cobol visitReportGroupLineNumberNextPage(Cobol.ReportGroupLineNumberNextPage reportGroupLineNumberNextPage, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupLineNumberNextPage, Space.Location.REPORT_GROUP_LINE_NUMBER_NEXT_PAGE_PREFIX, p);
        visit(reportGroupLineNumberNextPage.getIntegerLiteral(), p);
        visit(reportGroupLineNumberNextPage.getWords(), p);
        afterSyntax(reportGroupLineNumberNextPage, p);
        return reportGroupLineNumberNextPage;
    }

    public Cobol visitReportGroupLineNumberPlus(Cobol.ReportGroupLineNumberPlus reportGroupLineNumberPlus, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupLineNumberPlus, Space.Location.REPORT_GROUP_LINE_NUMBER_PLUS_PREFIX, p);
        visit(reportGroupLineNumberPlus.getPlus(), p);
        visit(reportGroupLineNumberPlus.getIntegerLiteral(), p);
        afterSyntax(reportGroupLineNumberPlus, p);
        return reportGroupLineNumberPlus;
    }

    public Cobol visitReportGroupNextGroupClause(Cobol.ReportGroupNextGroupClause reportGroupNextGroupClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupNextGroupClause, Space.Location.REPORT_GROUP_NEXT_GROUP_CLAUSE_PREFIX, p);
        visit(reportGroupNextGroupClause.getWords(), p);
        visit(reportGroupNextGroupClause.getClause(), p);
        afterSyntax(reportGroupNextGroupClause, p);
        return reportGroupNextGroupClause;
    }

    public Cobol visitReportGroupNextGroupNextPage(Cobol.ReportGroupNextGroupNextPage reportGroupNextGroupNextPage, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupNextGroupNextPage, Space.Location.REPORT_GROUP_NEXT_GROUP_NEXT_PAGE_PREFIX, p);
        visit(reportGroupNextGroupNextPage.getNextPage(), p);
        afterSyntax(reportGroupNextGroupNextPage, p);
        return reportGroupNextGroupNextPage;
    }

    public Cobol visitReportGroupNextGroupPlus(Cobol.ReportGroupNextGroupPlus reportGroupNextGroupPlus, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupNextGroupPlus, Space.Location.REPORT_GROUP_NEXT_GROUP_PLUS_PREFIX, p);
        visit(reportGroupNextGroupPlus.getPlus(), p);
        visit(reportGroupNextGroupPlus.getIntegerLiteral(), p);
        afterSyntax(reportGroupNextGroupPlus, p);
        return reportGroupNextGroupPlus;
    }

    public Cobol visitReportGroupPictureClause(Cobol.ReportGroupPictureClause reportGroupPictureClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupPictureClause, Space.Location.REPORT_GROUP_PICTURE_CLAUSE_PREFIX, p);
        visit(reportGroupPictureClause.getWords(), p);
        visit(reportGroupPictureClause.getPictureString(), p);
        afterSyntax(reportGroupPictureClause, p);
        return reportGroupPictureClause;
    }

    public Cobol visitReportGroupResetClause(Cobol.ReportGroupResetClause reportGroupResetClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupResetClause, Space.Location.REPORT_GROUP_RESET_CLAUSE_PREFIX, p);
        visit(reportGroupResetClause.getWords(), p);
        afterSyntax(reportGroupResetClause, p);
        return reportGroupResetClause;
    }

    public Cobol visitReportGroupSignClause(Cobol.ReportGroupSignClause reportGroupSignClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupSignClause, Space.Location.REPORT_GROUP_SIGN_CLAUSE_PREFIX, p);
        visit(reportGroupSignClause.getWords(), p);
        afterSyntax(reportGroupSignClause, p);
        return reportGroupSignClause;
    }

    public Cobol visitReportGroupSourceClause(Cobol.ReportGroupSourceClause reportGroupSourceClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupSourceClause, Space.Location.REPORT_GROUP_SOURCE_CLAUSE_PREFIX, p);
        visit(reportGroupSourceClause.getWords(), p);
        visit(reportGroupSourceClause.getIdentifier(), p);
        afterSyntax(reportGroupSourceClause, p);
        return reportGroupSourceClause;
    }

    public Cobol visitReportGroupSumClause(Cobol.ReportGroupSumClause reportGroupSumClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupSumClause, Space.Location.REPORT_GROUP_SUM_CLAUSE_PREFIX, p);
        visit(reportGroupSumClause.getCobols(), p);
        afterSyntax(reportGroupSumClause, p);
        return reportGroupSumClause;
    }

    public Cobol visitReportGroupTypeClause(Cobol.ReportGroupTypeClause reportGroupTypeClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupTypeClause, Space.Location.REPORT_GROUP_TYPE_CLAUSE_PREFIX, p);
        visit(reportGroupTypeClause.getWords(), p);
        visit(reportGroupTypeClause.getType(), p);
        afterSyntax(reportGroupTypeClause, p);
        return reportGroupTypeClause;
    }

    public Cobol visitReportGroupTypeControlFooting(Cobol.ReportGroupTypeControlFooting reportGroupTypeControlFooting, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupTypeControlFooting, Space.Location.REPORT_GROUP_TYPE_CONTROL_FOOTING_PREFIX, p);
        visit(reportGroupTypeControlFooting.getWords(), p);
        afterSyntax(reportGroupTypeControlFooting, p);
        return reportGroupTypeControlFooting;
    }

    public Cobol visitReportGroupTypeControlHeading(Cobol.ReportGroupTypeControlHeading reportGroupTypeControlHeading, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupTypeControlHeading, Space.Location.REPORT_GROUP_TYPE_CONTROL_HEADING_PREFIX, p);
        visit(reportGroupTypeControlHeading.getWords(), p);
        afterSyntax(reportGroupTypeControlHeading, p);
        return reportGroupTypeControlHeading;
    }

    public Cobol visitReportGroupTypeDetail(Cobol.ReportGroupTypeDetail reportGroupTypeDetail, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupTypeDetail, Space.Location.REPORT_GROUP_TYPE_DETAIL_PREFIX, p);
        visit(reportGroupTypeDetail.getWords(), p);
        afterSyntax(reportGroupTypeDetail, p);
        return reportGroupTypeDetail;
    }

    public Cobol visitReportGroupTypePageFooting(Cobol.ReportGroupTypePageFooting reportGroupTypePageFooting, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupTypePageFooting, Space.Location.REPORT_GROUP_TYPE_PAGE_FOOTING_PREFIX, p);
        visit(reportGroupTypePageFooting.getWords(), p);
        afterSyntax(reportGroupTypePageFooting, p);
        return reportGroupTypePageFooting;
    }

    public Cobol visitReportGroupTypePageHeading(Cobol.ReportGroupTypePageHeading reportGroupTypePageHeading, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupTypePageHeading, Space.Location.REPORT_GROUP_TYPE_REPORT_FOOTING_PREFIX, p);
        visit(reportGroupTypePageHeading.getWords(), p);
        afterSyntax(reportGroupTypePageHeading, p);
        return reportGroupTypePageHeading;
    }

    public Cobol visitReportGroupTypeReportFooting(Cobol.ReportGroupTypeReportFooting reportGroupTypeReportFooting, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupTypeReportFooting, Space.Location.REPORT_GROUP_TYPE_PAGE_HEADING_PREFIX, p);
        visit(reportGroupTypeReportFooting.getWords(), p);
        afterSyntax(reportGroupTypeReportFooting, p);
        return reportGroupTypeReportFooting;
    }

    public Cobol visitReportGroupTypeReportHeading(Cobol.ReportGroupTypeReportHeading reportGroupTypeReportHeading, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupTypeReportHeading, Space.Location.REPORT_GROUP_TYPE_REPORT_HEADING_PREFIX, p);
        visit(reportGroupTypeReportHeading.getWords(), p);
        afterSyntax(reportGroupTypeReportHeading, p);
        return reportGroupTypeReportHeading;
    }

    public Cobol visitReportGroupUsageClause(Cobol.ReportGroupUsageClause reportGroupUsageClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupUsageClause, Space.Location.REPORT_GROUP_USAGE_CLAUSE_PREFIX, p);
        visit(reportGroupUsageClause.getWords(), p);
        afterSyntax(reportGroupUsageClause, p);
        return reportGroupUsageClause;
    }

    public Cobol visitReportGroupValueClause(Cobol.ReportGroupValueClause reportGroupValueClause, PrintOutputCapture<P> p) {
        beforeSyntax(reportGroupValueClause, Space.Location.REPORT_GROUP_VALUE_CLAUSE_PREFIX, p);
        visit(reportGroupValueClause.getWords(), p);
        visit(reportGroupValueClause.getLiteral(), p);
        afterSyntax(reportGroupValueClause, p);
        return reportGroupValueClause;
    }

    public Cobol visitReportName(Cobol.ReportName reportName, PrintOutputCapture<P> p) {
        beforeSyntax(reportName, Space.Location.REPORT_NAME_PREFIX, p);
        visit(reportName.getQualifiedDataName(), p);
        afterSyntax(reportName, p);
        return reportName;
    }

    public Cobol visitReportSection(Cobol.ReportSection reportSection, PrintOutputCapture<P> p) {
        beforeSyntax(reportSection, Space.Location.REPORT_SECTION_PREFIX, p);
        visit(reportSection.getWords(), p);
        visit(reportSection.getDescriptions(), p);
        afterSyntax(reportSection, p);
        return reportSection;
    }

    public Cobol visitRerunClause(Cobol.RerunClause rerunClause, PrintOutputCapture<P> p) {
        beforeSyntax(rerunClause, Space.Location.RERUN_CLAUSE_PREFIX, p);
        visit(rerunClause.getRerun(), p);
        visit(rerunClause.getOn(), p);
        visit(rerunClause.getName(), p);
        visit(rerunClause.getEvery(), p);
        visit(rerunClause.getAction(), p);
        afterSyntax(rerunClause, p);
        return rerunClause;
    }

    public Cobol visitRerunEveryClock(Cobol.RerunEveryClock rerunEveryClock, PrintOutputCapture<P> p) {
        beforeSyntax(rerunEveryClock, Space.Location.RERUN_EVERY_CLOCK_PREFIX, p);
        visit(rerunEveryClock.getIntegerLiteral(), p);
        visit(rerunEveryClock.getClockUnits(), p);
        afterSyntax(rerunEveryClock, p);
        return rerunEveryClock;
    }

    public Cobol visitRerunEveryOf(Cobol.RerunEveryOf rerunEveryOf, PrintOutputCapture<P> p) {
        beforeSyntax(rerunEveryOf, Space.Location.RERUN_EVERY_OF_PREFIX, p);
        visit(rerunEveryOf.getRecords(), p);
        visit(rerunEveryOf.getFileName(), p);
        afterSyntax(rerunEveryOf, p);
        return rerunEveryOf;
    }

    public Cobol visitRerunEveryRecords(Cobol.RerunEveryRecords rerunEveryRecords, PrintOutputCapture<P> p) {
        beforeSyntax(rerunEveryRecords, Space.Location.RERUN_EVERY_RECORDS_PREFIX, p);
        visit(rerunEveryRecords.getIntegerLiteral(), p);
        visit(rerunEveryRecords.getRecords(), p);
        afterSyntax(rerunEveryRecords, p);
        return rerunEveryRecords;
    }

    public Cobol visitReserveClause(Cobol.ReserveClause reserveClause, PrintOutputCapture<P> p) {
        beforeSyntax(reserveClause, Space.Location.RERUN_RESERVE_CLAUSE_PREFIX, p);
        visit(reserveClause.getWords(), p);
        afterSyntax(reserveClause, p);
        return reserveClause;
    }

    public Cobol visitReserveNetworkClause(Cobol.ReserveNetworkClause reserveNetworkClause, PrintOutputCapture<P> p) {
        beforeSyntax(reserveNetworkClause, Space.Location.RESERVE_NETWORK_CLAUSE_PREFIX, p);
        visit(reserveNetworkClause.getWords(), p);
        afterSyntax(reserveNetworkClause, p);
        return reserveNetworkClause;
    }

    @Override
    public Cobol visitReturn(Cobol.Return r, PrintOutputCapture<P> p) {
        beforeSyntax(r, Space.Location.RETURN_PREFIX, p);
        visit(r.getWord(), p);
        visit(r.getFileName(), p);
        visit(r.getRecord(), p);
        visit(r.getInto(), p);
        visit(r.getAtEndPhrase(), p);
        visit(r.getNotAtEndPhrase(), p);
        visit(r.getEndReturn(), p);
        afterSyntax(r, p);
        return r;
    }

    @Override
    public Cobol visitReturnInto(Cobol.ReturnInto r, PrintOutputCapture<P> p) {
        beforeSyntax(r, Space.Location.RETURN_INTO_PREFIX, p);
        visit(r.getInto(), p);
        visit(r.getQualifiedDataName(), p);
        afterSyntax(r, p);
        return r;
    }

    public Cobol visitRewrite(Cobol.Rewrite rewrite, PrintOutputCapture<P> p) {
        beforeSyntax(rewrite, Space.Location.REWRITE_PREFIX, p);
        visit(rewrite.getRewrite(), p);
        visit(rewrite.getRecordName(), p);
        visit(rewrite.getRewriteFrom(), p);
        visit(rewrite.getInvalidKeyPhrase(), p);
        visit(rewrite.getNotInvalidKeyPhrase(), p);
        visit(rewrite.getEndRewrite(), p);
        afterSyntax(rewrite, p);
        return rewrite;
    }

    public Cobol visitRewriteFrom(Cobol.RewriteFrom rewriteFrom, PrintOutputCapture<P> p) {
        beforeSyntax(rewriteFrom, Space.Location.REWRITE_FROM_PREFIX, p);
        visit(rewriteFrom.getFrom(), p);
        visit(rewriteFrom.getIdentifier(), p);
        afterSyntax(rewriteFrom, p);
        return rewriteFrom;
    }

    public Cobol visitRoundable(Cobol.Roundable roundable, PrintOutputCapture<P> p) {
        beforeSyntax(roundable, Space.Location.ROUNDABLE_PREFIX, p);
        visit(roundable.getIdentifier(), p);
        visit(roundable.getRounded(), p);
        afterSyntax(roundable, p);
        return roundable;
    }

    public Cobol visitSameClause(Cobol.SameClause sameClause, PrintOutputCapture<P> p) {
        beforeSyntax(sameClause, Space.Location.SAME_CLAUSE_PREFIX, p);
        visit(sameClause.getWords(), p);
        visit(sameClause.getFileNames(), p);
        afterSyntax(sameClause, p);
        return sameClause;
    }

    public Cobol visitScreenDescriptionAutoClause(Cobol.ScreenDescriptionAutoClause screenDescriptionAutoClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionAutoClause, Space.Location.SCREEN_DESCRIPTION_AUTO_CLAUSE_PREFIX, p);
        visit(screenDescriptionAutoClause.getAuto(), p);
        afterSyntax(screenDescriptionAutoClause, p);
        return screenDescriptionAutoClause;
    }

    public Cobol visitScreenDescriptionBackgroundColorClause(Cobol.ScreenDescriptionBackgroundColorClause screenDescriptionBackgroundColorClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionBackgroundColorClause, Space.Location.SCREEN_DESCRIPTION_BACKGROUND_COLOR_CLAUSE_PREFIX, p);
        visit(screenDescriptionBackgroundColorClause.getBackground(), p);
        visit(screenDescriptionBackgroundColorClause.getIs(), p);
        visit(screenDescriptionBackgroundColorClause.getValue(), p);
        afterSyntax(screenDescriptionBackgroundColorClause, p);
        return screenDescriptionBackgroundColorClause;
    }

    public Cobol visitScreenDescriptionBellClause(Cobol.ScreenDescriptionBellClause screenDescriptionBellClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionBellClause, Space.Location.SCREEN_DESCRIPTION_BELL_CLAUSE_PREFIX, p);
        visit(screenDescriptionBellClause.getBell(), p);
        afterSyntax(screenDescriptionBellClause, p);
        return screenDescriptionBellClause;
    }

    public Cobol visitScreenDescriptionBlankClause(Cobol.ScreenDescriptionBlankClause screenDescriptionBlankClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionBlankClause, Space.Location.SCREEN_DESCRIPTION_BLANK_CLAUSE_PREFIX, p);
        visit(screenDescriptionBlankClause.getWords(), p);
        afterSyntax(screenDescriptionBlankClause, p);
        return screenDescriptionBlankClause;
    }

    public Cobol visitScreenDescriptionBlankWhenZeroClause(Cobol.ScreenDescriptionBlankWhenZeroClause screenDescriptionBlankWhenZeroClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionBlankWhenZeroClause, Space.Location.SCREEN_DESCRIPTION_BLANK_WHEN_ZERO_CLAUSE_PREFIX, p);
        visit(screenDescriptionBlankWhenZeroClause.getWords(), p);
        afterSyntax(screenDescriptionBlankWhenZeroClause, p);
        return screenDescriptionBlankWhenZeroClause;
    }

    public Cobol visitScreenDescriptionBlinkClause(Cobol.ScreenDescriptionBlinkClause screenDescriptionBlinkClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionBlinkClause, Space.Location.SCREEN_DESCRIPTION_BLINK_CLAUSE_PREFIX, p);
        visit(screenDescriptionBlinkClause.getBlink(), p);
        afterSyntax(screenDescriptionBlinkClause, p);
        return screenDescriptionBlinkClause;
    }

    public Cobol visitScreenDescriptionColumnClause(Cobol.ScreenDescriptionColumnClause screenDescriptionColumnClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionColumnClause, Space.Location.SCREEN_DESCRIPTION_COLUMN_CLAUSE_PREFIX, p);
        visit(screenDescriptionColumnClause.getWords(), p);
        visit(screenDescriptionColumnClause.getValue(), p);
        afterSyntax(screenDescriptionColumnClause, p);
        return screenDescriptionColumnClause;
    }

    public Cobol visitScreenDescriptionControlClause(Cobol.ScreenDescriptionControlClause screenDescriptionControlClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionControlClause, Space.Location.SCREEN_DESCRIPTION_CONTROL_CLAUSE_PREFIX, p);
        visit(screenDescriptionControlClause.getWords(), p);
        visit(screenDescriptionControlClause.getValue(), p);
        afterSyntax(screenDescriptionControlClause, p);
        return screenDescriptionControlClause;
    }

    public Cobol visitScreenDescriptionEntry(Cobol.ScreenDescriptionEntry screenDescriptionEntry, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionEntry, Space.Location.SCREEN_DESCRIPTION_ENTRY_PREFIX, p);
        visit(screenDescriptionEntry.getWord(), p);
        visit(screenDescriptionEntry.getName(), p);
        visit(screenDescriptionEntry.getClauses(), p);
        visit(screenDescriptionEntry.getDot(), p);
        afterSyntax(screenDescriptionEntry, p);
        return screenDescriptionEntry;
    }

    public Cobol visitScreenDescriptionEraseClause(Cobol.ScreenDescriptionEraseClause screenDescriptionEraseClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionEraseClause, Space.Location.SCREEN_DESCRIPTION_ERASE_CLAUSE_PREFIX, p);
        visit(screenDescriptionEraseClause.getWords(), p);
        afterSyntax(screenDescriptionEraseClause, p);
        return screenDescriptionEraseClause;
    }

    public Cobol visitScreenDescriptionForegroundColorClause(Cobol.ScreenDescriptionForegroundColorClause screenDescriptionForegroundColorClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionForegroundColorClause, Space.Location.SCREEN_DESCRIPTION_FOREGROUND_COLOR_CLAUSE_PREFIX, p);
        visit(screenDescriptionForegroundColorClause.getWords(), p);
        visit(screenDescriptionForegroundColorClause.getValue(), p);
        afterSyntax(screenDescriptionForegroundColorClause, p);
        return screenDescriptionForegroundColorClause;
    }

    public Cobol visitScreenDescriptionFromClause(Cobol.ScreenDescriptionFromClause screenDescriptionFromClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionFromClause, Space.Location.SCREEN_DESCRIPTION_FROM_CLAUSE_PREFIX, p);
        visit(screenDescriptionFromClause.getFrom(), p);
        visit(screenDescriptionFromClause.getValue(), p);
        visit(screenDescriptionFromClause.getScreenDescriptionToClause(), p);
        afterSyntax(screenDescriptionFromClause, p);
        return screenDescriptionFromClause;
    }

    public Cobol visitScreenDescriptionFullClause(Cobol.ScreenDescriptionFullClause screenDescriptionFullClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionFullClause, Space.Location.SCREEN_DESCRIPTION_FULL_CLAUSE_PREFIX, p);
        visit(screenDescriptionFullClause.getWord(), p);
        afterSyntax(screenDescriptionFullClause, p);
        return screenDescriptionFullClause;
    }

    public Cobol visitScreenDescriptionGridClause(Cobol.ScreenDescriptionGridClause screenDescriptionGridClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionGridClause, Space.Location.SCREEN_DESCRIPTION_GRID_CLAUSE_PREFIX, p);
        visit(screenDescriptionGridClause.getWord(), p);
        afterSyntax(screenDescriptionGridClause, p);
        return screenDescriptionGridClause;
    }

    public Cobol visitScreenDescriptionJustifiedClause(Cobol.ScreenDescriptionJustifiedClause screenDescriptionJustifiedClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionJustifiedClause, Space.Location.SCREEN_DESCRIPTION_JUSTIFIED_CLAUSE_PREFIX, p);
        visit(screenDescriptionJustifiedClause.getWords(), p);
        afterSyntax(screenDescriptionJustifiedClause, p);
        return screenDescriptionJustifiedClause;
    }

    public Cobol visitScreenDescriptionLightClause(Cobol.ScreenDescriptionLightClause screenDescriptionLightClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionLightClause, Space.Location.SCREEN_DESCRIPTION_LIGHT_CLAUSE_PREFIX, p);
        visit(screenDescriptionLightClause.getLight(), p);
        afterSyntax(screenDescriptionLightClause, p);
        return screenDescriptionLightClause;
    }

    public Cobol visitScreenDescriptionLineClause(Cobol.ScreenDescriptionLineClause screenDescriptionLineClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionLineClause, Space.Location.SCREEN_DESCRIPTION_LINE_CLAUSE_PREFIX, p);
        visit(screenDescriptionLineClause.getWords(), p);
        visit(screenDescriptionLineClause.getValue(), p);
        afterSyntax(screenDescriptionLineClause, p);
        return screenDescriptionLineClause;
    }

    public Cobol visitScreenDescriptionPictureClause(Cobol.ScreenDescriptionPictureClause screenDescriptionPictureClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionPictureClause, Space.Location.SCREEN_DESCRIPTION_PICTURE_CLAUSE_PREFIX, p);
        visit(screenDescriptionPictureClause.getWords(), p);
        visit(screenDescriptionPictureClause.getPictureString(), p);
        afterSyntax(screenDescriptionPictureClause, p);
        return screenDescriptionPictureClause;
    }

    public Cobol visitScreenDescriptionPromptClause(Cobol.ScreenDescriptionPromptClause screenDescriptionPromptClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionPromptClause, Space.Location.SCREEN_DESCRIPTION_PROMPT_CLAUSE_PREFIX, p);
        visit(screenDescriptionPromptClause.getWords(), p);
        visit(screenDescriptionPromptClause.getName(), p);
        visit(screenDescriptionPromptClause.getScreenDescriptionPromptOccursClause(), p);
        afterSyntax(screenDescriptionPromptClause, p);
        return screenDescriptionPromptClause;
    }

    public Cobol visitScreenDescriptionPromptOccursClause(Cobol.ScreenDescriptionPromptOccursClause screenDescriptionPromptOccursClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionPromptOccursClause, Space.Location.SCREEN_DESCRIPTION_PROMPT_OCCURS_CLAUSE_PREFIX, p);
        visit(screenDescriptionPromptOccursClause.getOccurs(), p);
        visit(screenDescriptionPromptOccursClause.getInteger(), p);
        visit(screenDescriptionPromptOccursClause.getTimes(), p);
        afterSyntax(screenDescriptionPromptOccursClause, p);
        return screenDescriptionPromptOccursClause;
    }

    public Cobol visitScreenDescriptionRequiredClause(Cobol.ScreenDescriptionRequiredClause screenDescriptionRequiredClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionRequiredClause, Space.Location.SCREEN_DESCRIPTION_REQUIRED_CLAUSE_PREFIX, p);
        visit(screenDescriptionRequiredClause.getRequired(), p);
        afterSyntax(screenDescriptionRequiredClause, p);
        return screenDescriptionRequiredClause;
    }

    public Cobol visitScreenDescriptionReverseVideoClause(Cobol.ScreenDescriptionReverseVideoClause screenDescriptionReverseVideoClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionReverseVideoClause, Space.Location.SCREEN_DESCRIPTION_REVERSE_VIDEO_CLAUSE_PREFIX, p);
        visit(screenDescriptionReverseVideoClause.getWord(), p);
        afterSyntax(screenDescriptionReverseVideoClause, p);
        return screenDescriptionReverseVideoClause;
    }

    public Cobol visitScreenDescriptionSecureClause(Cobol.ScreenDescriptionSecureClause screenDescriptionSecureClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionSecureClause, Space.Location.SCREEN_DESCRIPTION_SECURE_CLAUSE_PREFIX, p);
        visit(screenDescriptionSecureClause.getWord(), p);
        afterSyntax(screenDescriptionSecureClause, p);
        return screenDescriptionSecureClause;
    }

    public Cobol visitScreenDescriptionSignClause(Cobol.ScreenDescriptionSignClause screenDescriptionSignClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionSignClause, Space.Location.SCREEN_DESCRIPTION_SIGN_CLAUSE_PREFIX, p);
        visit(screenDescriptionSignClause.getWords(), p);
        afterSyntax(screenDescriptionSignClause, p);
        return screenDescriptionSignClause;
    }

    public Cobol visitScreenDescriptionSizeClause(Cobol.ScreenDescriptionSizeClause screenDescriptionSizeClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionSizeClause, Space.Location.SCREEN_DESCRIPTION_SIZE_CLAUSE_PREFIX, p);
        visit(screenDescriptionSizeClause.getWords(), p);
        visit(screenDescriptionSizeClause.getValue(), p);
        afterSyntax(screenDescriptionSizeClause, p);
        return screenDescriptionSizeClause;
    }

    public Cobol visitScreenDescriptionToClause(Cobol.ScreenDescriptionToClause screenDescriptionToClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionToClause, Space.Location.SCREEN_DESCRIPTION_TO_CLAUSE_PREFIX, p);
        visit(screenDescriptionToClause.getTo(), p);
        visit(screenDescriptionToClause.getIdentifier(), p);
        afterSyntax(screenDescriptionToClause, p);
        return screenDescriptionToClause;
    }

    public Cobol visitScreenDescriptionUnderlineClause(Cobol.ScreenDescriptionUnderlineClause screenDescriptionUnderlineClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionUnderlineClause, Space.Location.SCREEN_DESCRIPTION_UNDERLINE_CLAUSE_PREFIX, p);
        visit(screenDescriptionUnderlineClause.getUnderline(), p);
        afterSyntax(screenDescriptionUnderlineClause, p);
        return screenDescriptionUnderlineClause;
    }

    public Cobol visitScreenDescriptionUsageClause(Cobol.ScreenDescriptionUsageClause screenDescriptionUsageClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionUsageClause, Space.Location.SCREEN_DESCRIPTION_USAGE_CLAUSE_PREFIX, p);
        visit(screenDescriptionUsageClause.getWords(), p);
        afterSyntax(screenDescriptionUsageClause, p);
        return screenDescriptionUsageClause;
    }

    public Cobol visitScreenDescriptionUsingClause(Cobol.ScreenDescriptionUsingClause screenDescriptionUsingClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionUsingClause, Space.Location.SCREEN_DESCRIPTION_USING_CLAUSE_PREFIX, p);
        visit(screenDescriptionUsingClause.getUsing(), p);
        visit(screenDescriptionUsingClause.getIdentifier(), p);
        afterSyntax(screenDescriptionUsingClause, p);
        return screenDescriptionUsingClause;
    }

    public Cobol visitScreenDescriptionValueClause(Cobol.ScreenDescriptionValueClause screenDescriptionValueClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionValueClause, Space.Location.SCREEN_DESCRIPTION_VALUE_CLAUSE_PREFIX, p);
        visit(screenDescriptionValueClause.getWords(), p);
        visit(screenDescriptionValueClause.getValue(), p);
        afterSyntax(screenDescriptionValueClause, p);
        return screenDescriptionValueClause;
    }

    public Cobol visitScreenDescriptionZeroFillClause(Cobol.ScreenDescriptionZeroFillClause screenDescriptionZeroFillClause, PrintOutputCapture<P> p) {
        beforeSyntax(screenDescriptionZeroFillClause, Space.Location.SCREEN_DESCRIPTION_ZERO_FILL_CLAUSE_PREFIX, p);
        visit(screenDescriptionZeroFillClause.getWord(), p);
        afterSyntax(screenDescriptionZeroFillClause, p);
        return screenDescriptionZeroFillClause;
    }

    public Cobol visitScreenSection(Cobol.ScreenSection screenSection, PrintOutputCapture<P> p) {
        beforeSyntax(screenSection, Space.Location.SCREEN_SECTION_PREFIX, p);
        visit(screenSection.getWords(), p);
        visit(screenSection.getDot(), p);
        visit(screenSection.getDescriptions(), p);
        afterSyntax(screenSection, p);
        return screenSection;
    }

    @Override
    public Cobol visitSearch(Cobol.Search s, PrintOutputCapture<P> p) {
        beforeSyntax(s, Space.Location.SEARCH_PREFIX, p);
        visit(s.getWords(), p);
        visit(s.getQualifiedDataName(), p);
        visit(s.getSearchVarying(), p);
        visit(s.getAtEndPhrase(), p);
        visit(s.getSearchWhen(), p);
        visit(s.getEndSearch(), p);
        afterSyntax(s, p);
        return s;
    }

    @Override
    public Cobol visitSearchVarying(Cobol.SearchVarying s, PrintOutputCapture<P> p) {
        beforeSyntax(s, Space.Location.SEARCH_VARYING_PREFIX, p);
        visit(s.getVarying(), p);
        visit(s.getQualifiedDataName(), p);
        afterSyntax(s, p);
        return s;
    }

    @Override
    public Cobol visitSearchWhen(Cobol.SearchWhen s, PrintOutputCapture<P> p) {
        beforeSyntax(s, Space.Location.SEARCH_WHEN_PREFIX, p);
        visit(s.getWhen(), p);
        visit(s.getCondition(), p);
        visit(s.getNextSentence(), p);
        visit(s.getStatements(), p);
        afterSyntax(s, p);
        return s;
    }

    public Cobol visitSelectClause(Cobol.SelectClause selectClause, PrintOutputCapture<P> p) {
        beforeSyntax(selectClause, Space.Location.SEARCH_CLAUSE_PREFIX, p);
        visit(selectClause.getWords(), p);
        visit(selectClause.getFileName(), p);
        afterSyntax(selectClause, p);
        return selectClause;
    }

    @Override
    public Cobol visitSend(Cobol.Send s, PrintOutputCapture<P> p) {
        beforeSyntax(s, Space.Location.SEND_PREFIX, p);
        visit(s.getSend(), p);
        visit(s.getStatement(), p);
        visit(s.getOnExceptionClause(), p);
        visit(s.getNotOnExceptionClause(), p);
        afterSyntax(s, p);
        return s;
    }

    public Cobol visitSendAdvancingLines(Cobol.SendAdvancingLines s, PrintOutputCapture<P> p) {
        beforeSyntax(s, Space.Location.SEND_ADVANCING_LINES_PREFIX, p);
        visit(s.getName(), p);
        visit(s.getLines(), p);
        afterSyntax(s, p);
        return s;
    }

    @Override
    public Cobol visitSendPhrase(Cobol.SendPhrase s, PrintOutputCapture<P> p) {
        beforeSyntax(s, Space.Location.SEND_PHRASE_PREFIX, p);
        visit(s.getWords(), p);
        visit(s.getTarget(), p);
        afterSyntax(s, p);
        return s;
    }

    @Override
    public Cobol visitSendStatementSync(Cobol.SendStatementSync s, PrintOutputCapture<P> p) {
        beforeSyntax(s, Space.Location.SEND_STATEMENT_SYNC_PREFIX, p);
        visit(s.getName(), p);
        visit(s.getSendFromPhrase(), p);
        visit(s.getSendWithPhrase(), p);
        visit(s.getSendReplacingPhrase(), p);
        visit(s.getSendAdvancingPhrase(), p);
        afterSyntax(s, p);
        return s;
    }

    public Cobol visitSentence(Cobol.Sentence sentence, PrintOutputCapture<P> p) {
        beforeSyntax(sentence, Space.Location.SENTENCE_PREFIX, p);
        visit(sentence.getStatements(), p);
        visit(sentence.getDot(), p);
        afterSyntax(sentence, p);
        return sentence;
    }

    public Cobol visitSet(Cobol.Set set, PrintOutputCapture<P> p) {
        beforeSyntax(set, Space.Location.SET_PREFIX, p);
        visit(set.getSet(), p);
        visit(set.getTo(), p);
        visit(set.getUpDown(), p);
        afterSyntax(set, p);
        return set;
    }

    public Cobol visitSetTo(Cobol.SetTo setTo, PrintOutputCapture<P> p) {
        beforeSyntax(setTo, Space.Location.SET_TO_PREFIX, p);
        visit(setTo.getIdentifiers(), p);
        visit(setTo.getTo(), p);
        visit(setTo.getValues(), p);
        afterSyntax(setTo, p);
        return setTo;
    }

    public Cobol visitSetUpDown(Cobol.SetUpDown setUpDown, PrintOutputCapture<P> p) {
        beforeSyntax(setUpDown, Space.Location.SET_UP_DOWN_PREFIX, p);
        visit(setUpDown.getTo(), p);
        visit(setUpDown.getOperation(), p);
        visit(setUpDown.getValue(), p);
        afterSyntax(setUpDown, p);
        return setUpDown;
    }

    public Cobol visitSort(Cobol.Sort sort, PrintOutputCapture<P> p) {
        beforeSyntax(sort, Space.Location.SORT_PREFIX, p);
        visit(sort.getSort(), p);
        visit(sort.getFileName(), p);
        visit(sort.getSortOnKeyClause(), p);
        visit(sort.getSortDuplicatesPhrase(), p);
        visit(sort.getSortCollatingSequencePhrase(), p);
        visit(sort.getSortInputProcedurePhrase(), p);
        visit(sort.getSortUsing(), p);
        visit(sort.getSortOutputProcedurePhrase(), p);
        visit(sort.getSortGiving(), p);
        afterSyntax(sort, p);
        return sort;
    }

    public Cobol visitSortCollatingSequencePhrase(Cobol.SortCollatingSequencePhrase sortCollatingSequencePhrase, PrintOutputCapture<P> p) {
        beforeSyntax(sortCollatingSequencePhrase, Space.Location.SORT_COLLATING_SEQUENCE_PHRASE_PREFIX, p);
        visit(sortCollatingSequencePhrase.getWords(), p);
        visit(sortCollatingSequencePhrase.getAlphabetNames(), p);
        visit(sortCollatingSequencePhrase.getSortCollatingAlphanumeric(), p);
        visit(sortCollatingSequencePhrase.getSortCollatingNational(), p);
        afterSyntax(sortCollatingSequencePhrase, p);
        return sortCollatingSequencePhrase;
    }

    public Cobol visitSortGiving(Cobol.SortGiving sortGiving, PrintOutputCapture<P> p) {
        beforeSyntax(sortGiving, Space.Location.SORT_GIVING_PREFIX, p);
        visit(sortGiving.getFileName(), p);
        visit(sortGiving.getWords(), p);
        afterSyntax(sortGiving, p);
        return sortGiving;
    }

    public Cobol visitSortProcedurePhrase(Cobol.SortProcedurePhrase sortProcedurePhrase, PrintOutputCapture<P> p) {
        beforeSyntax(sortProcedurePhrase, Space.Location.SORT_PROCEDURE_PHRASE_PREFIX, p);
        visit(sortProcedurePhrase.getWords(), p);
        visit(sortProcedurePhrase.getProcedureName(), p);
        visit(sortProcedurePhrase.getSortInputThrough(), p);
        afterSyntax(sortProcedurePhrase, p);
        return sortProcedurePhrase;
    }

    public Cobol visitSortable(Cobol.Sortable sortable, PrintOutputCapture<P> p) {
        beforeSyntax(sortable, Space.Location.SORTABLE_PREFIX, p);
        visit(sortable.getWords(), p);
        visit(sortable.getNames(), p);
        afterSyntax(sortable, p);
        return sortable;
    }

    public Cobol visitSourceComputer(Cobol.SourceComputer sourceComputer, PrintOutputCapture<P> p) {
        beforeSyntax(sourceComputer, Space.Location.SOURCE_COMPUTER_PREFIX, p);
        visit(sourceComputer.getWords(), p);
        visit(sourceComputer.getComputer(), p);
        afterSyntax(sourceComputer, p);
        return sourceComputer;
    }

    public Cobol visitSourceComputerDefinition(Cobol.SourceComputerDefinition sourceComputerDefinition, PrintOutputCapture<P> p) {
        beforeSyntax(sourceComputerDefinition, Space.Location.SOURCE_COMPUTER_DEFINITION_PREFIX, p);
        visit(sourceComputerDefinition.getComputerName(), p);
        visit(sourceComputerDefinition.getDebuggingMode(), p);
        visit(sourceComputerDefinition.getDot(), p);
        afterSyntax(sourceComputerDefinition, p);
        return sourceComputerDefinition;
    }

    @Override
    public Space visitSpace(Space space, Space.Location location, PrintOutputCapture<P> p) {
        p.append(space.getWhitespace());
        return space;
    }

    public Cobol visitSpecialNames(Cobol.SpecialNames specialNames, PrintOutputCapture<P> p) {
        beforeSyntax(specialNames, Space.Location.SPECIAL_NAMES_PREFIX, p);
        visit(specialNames.getWord(), p);
        visit(specialNames.getDot(), p);
        visit(specialNames.getClauses(), p);
        visit(specialNames.getDot2(), p);
        afterSyntax(specialNames, p);
        return specialNames;
    }

    public Cobol visitStart(Cobol.Start start, PrintOutputCapture<P> p) {
        beforeSyntax(start, Space.Location.START_PREFIX, p);
        visit(start.getStart(), p);
        visit(start.getFileName(), p);
        visit(start.getStartKey(), p);
        visit(start.getInvalidKeyPhrase(), p);
        visit(start.getNotInvalidKeyPhrase(), p);
        visit(start.getEndStart(), p);
        afterSyntax(start, p);
        return start;
    }

    public Cobol visitStartKey(Cobol.StartKey startKey, PrintOutputCapture<P> p) {
        beforeSyntax(startKey, Space.Location.START_KEY_PREFIX, p);
        visit(startKey.getWords(), p);
        visit(startKey.getQualifiedDataName(), p);
        afterSyntax(startKey, p);
        return startKey;
    }

    public Cobol visitStatementPhrase(Cobol.StatementPhrase statementPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(statementPhrase, Space.Location.STATEMENT_PHRASE_PREFIX, p);
        visit(statementPhrase.getPhrases(), p);
        visit(statementPhrase.getStatements(), p);
        afterSyntax(statementPhrase, p);
        return statementPhrase;
    }

    public Cobol visitStatusKeyClause(Cobol.StatusKeyClause statusKeyClause, PrintOutputCapture<P> p) {
        beforeSyntax(statusKeyClause, Space.Location.STATUS_KEY_CLAUSE_PREFIX, p);
        visit(statusKeyClause.getWords(), p);
        visit(statusKeyClause.getName(), p);
        afterSyntax(statusKeyClause, p);
        return statusKeyClause;
    }

    public Cobol visitStop(Cobol.Stop stop, PrintOutputCapture<P> p) {
        beforeSyntax(stop, Space.Location.STOP_PREFIX, p);
        visit(stop.getWords(), p);
        visit(stop.getStatement(), p);
        afterSyntax(stop, p);
        return stop;
    }

    public Cobol visitStopStatementGiving(Cobol.StopStatementGiving stopStatementGiving, PrintOutputCapture<P> p) {
        beforeSyntax(stopStatementGiving, Space.Location.STOP_STATEMENT_GIVING_PREFIX, p);
        visit(stopStatementGiving.getWords(), p);
        visit(stopStatementGiving.getName(), p);
        afterSyntax(stopStatementGiving, p);
        return stopStatementGiving;
    }

    public Cobol visitStringDelimitedByPhrase(Cobol.StringDelimitedByPhrase stringDelimitedByPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(stringDelimitedByPhrase, Space.Location.STRING_DELIMITED_BY_PHRASE_PREFIX, p);
        visit(stringDelimitedByPhrase.getWords(), p);
        visit(stringDelimitedByPhrase.getIdentifier(), p);
        afterSyntax(stringDelimitedByPhrase, p);
        return stringDelimitedByPhrase;
    }

    public Cobol visitStringForPhrase(Cobol.StringForPhrase stringForPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(stringForPhrase, Space.Location.STRING_FOR_PHRASE_PREFIX, p);
        visit(stringForPhrase.getWord(), p);
        visit(stringForPhrase.getIdentifier(), p);
        afterSyntax(stringForPhrase, p);
        return stringForPhrase;
    }

    public Cobol visitStringIntoPhrase(Cobol.StringIntoPhrase stringIntoPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(stringIntoPhrase, Space.Location.STRING_INTO_PHRASE_PREFIX, p);
        visit(stringIntoPhrase.getInto(), p);
        visit(stringIntoPhrase.getIdentifier(), p);
        afterSyntax(stringIntoPhrase, p);
        return stringIntoPhrase;
    }

    public Cobol visitStringSendingPhrase(Cobol.StringSendingPhrase stringSendingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(stringSendingPhrase, Space.Location.STRING_SENDING_PHRASE_PREFIX, p);
        visit(stringSendingPhrase.getSendings(), p);
        visit(stringSendingPhrase.getPhrase(), p);
        afterSyntax(stringSendingPhrase, p);
        return stringSendingPhrase;
    }

    public Cobol visitStringStatement(Cobol.StringStatement stringStatement, PrintOutputCapture<P> p) {
        beforeSyntax(stringStatement, Space.Location.STRING_STATEMENT_PREFIX, p);
        visit(stringStatement.getString(), p);
        visit(stringStatement.getStringSendingPhrases(), p);
        visit(stringStatement.getStringIntoPhrase(), p);
        visit(stringStatement.getStringWithPointerPhrase(), p);
        visit(stringStatement.getOnOverflowPhrase(), p);
        visit(stringStatement.getNotOnOverflowPhrase(), p);
        visit(stringStatement.getEndString(), p);
        afterSyntax(stringStatement, p);
        return stringStatement;
    }

    public Cobol visitStringWithPointerPhrase(Cobol.StringWithPointerPhrase stringWithPointerPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(stringWithPointerPhrase, Space.Location.STRING_WITH_POINTER_PHRASE_PREFIX, p);
        visit(stringWithPointerPhrase.getWords(), p);
        visit(stringWithPointerPhrase.getQualifiedDataName(), p);
        afterSyntax(stringWithPointerPhrase, p);
        return stringWithPointerPhrase;
    }

    public Cobol visitSubscript(Cobol.Subscript subscript, PrintOutputCapture<P> p) {
        beforeSyntax(subscript, Space.Location.SUBSCRIPT_PREFIX, p);
        visit(subscript.getFirst(), p);
        visit(subscript.getSecond(), p);
        afterSyntax(subscript, p);
        return subscript;
    }

    public Cobol visitSubtract(Cobol.Subtract subtract, PrintOutputCapture<P> p) {
        beforeSyntax(subtract, Space.Location.SUBTRACT_PREFIX, p);
        visit (subtract.getSubstract(), p);
        visit(subtract.getOperation(), p);
        visit(subtract.getOnSizeErrorPhrase(), p);
        visit(subtract.getNotOnSizeErrorPhrase(), p);
        visit(subtract.getEndSubtract(), p);
        afterSyntax(subtract, p);
        return subtract;
    }

    public Cobol visitSubtractCorrespondingStatement(Cobol.SubtractCorrespondingStatement subtractCorrespondingStatement, PrintOutputCapture<P> p) {
        beforeSyntax(subtractCorrespondingStatement, Space.Location.SUBTRACT_CORRESPONDING_STATEMENT_PREFIX, p);
        visit(subtractCorrespondingStatement.getCorresponding(), p);
        visit(subtractCorrespondingStatement.getQualifiedDataName(), p);
        visit(subtractCorrespondingStatement.getGiving(), p);
        visit(subtractCorrespondingStatement.getSubtractMinuendCorresponding(), p);
        afterSyntax(subtractCorrespondingStatement, p);
        return subtractCorrespondingStatement;
    }

    public Cobol visitSubtractFromGivingStatement(Cobol.SubtractFromGivingStatement subtractFromGivingStatement, PrintOutputCapture<P> p) {
        beforeSyntax(subtractFromGivingStatement, Space.Location.SUBTRACT_FROM_GIVING_STATEMENT_PREFIX, p);
        visit(subtractFromGivingStatement.getSubtractSubtrahend(), p);
        visit(subtractFromGivingStatement.getFrom(), p);
        visit(subtractFromGivingStatement.getSubtractMinuendGiving(), p);
        visit(subtractFromGivingStatement.getGiving(), p);
        visit(subtractFromGivingStatement.getSubtractGiving(), p);
        afterSyntax(subtractFromGivingStatement, p);
        return subtractFromGivingStatement;
    }

    public Cobol visitSubtractFromStatement(Cobol.SubtractFromStatement subtractFromStatement, PrintOutputCapture<P> p) {
        beforeSyntax(subtractFromStatement, Space.Location.SUBTRACT_FROM_STATEMENT_PREFIX, p);
        visit(subtractFromStatement.getSubtractSubtrahend(), p);
        visit(subtractFromStatement.getFrom(), p);
        visit(subtractFromStatement.getSubtractMinuend(), p);
        afterSyntax(subtractFromStatement, p);
        return subtractFromStatement;
    }

    public Cobol visitSubtractMinuendCorresponding(Cobol.SubtractMinuendCorresponding subtractMinuendCorresponding, PrintOutputCapture<P> p) {
        beforeSyntax(subtractMinuendCorresponding, Space.Location.SUBTRACT_MINUEND_CORRESPONDING_PREFIX, p);
        visit(subtractMinuendCorresponding.getQualifiedDataName(), p);
        visit(subtractMinuendCorresponding.getRounded(), p);
        afterSyntax(subtractMinuendCorresponding, p);
        return subtractMinuendCorresponding;
    }

    public Cobol visitSymbolicCharacter(Cobol.SymbolicCharacter symbolicCharacter, PrintOutputCapture<P> p) {
        beforeSyntax(symbolicCharacter, Space.Location.SYMBOLIC_CHARACTER_PREFIX, p);
        visit(symbolicCharacter.getSymbols(), p);
        visit(symbolicCharacter.getWord(), p);
        visit(symbolicCharacter.getLiterals(), p);
        afterSyntax(symbolicCharacter, p);
        return symbolicCharacter;
    }

    public Cobol visitSymbolicCharactersClause(Cobol.SymbolicCharactersClause symbolicCharactersClause, PrintOutputCapture<P> p) {
        beforeSyntax(symbolicCharactersClause, Space.Location.SYMBOLIC_CHARACTERS_CLAUSE_PREFIX, p);
        visit(symbolicCharactersClause.getWords(), p);
        visit(symbolicCharactersClause.getSymbols(), p);
        visit(symbolicCharactersClause.getInAlphabet(), p);
        visit(symbolicCharactersClause.getAlphabetName(), p);
        afterSyntax(symbolicCharactersClause, p);
        return symbolicCharactersClause;
    }

    public Cobol visitSymbolicDestinationClause(Cobol.SymbolicDestinationClause symbolicDestinationClause, PrintOutputCapture<P> p) {
        beforeSyntax(symbolicDestinationClause, Space.Location.SYMBOLIC_DESTINATION_CLAUSE_PREFIX, p);
        visit(symbolicDestinationClause.getWords(), p);
        visit(symbolicDestinationClause.getDataDescName(), p);
        afterSyntax(symbolicDestinationClause, p);
        return symbolicDestinationClause;
    }

    public Cobol visitSymbolicQueueClause(Cobol.SymbolicQueueClause symbolicQueueClause, PrintOutputCapture<P> p) {
        beforeSyntax(symbolicQueueClause, Space.Location.SYMBOLIC_QUEUE_CLAUSE_PREFIX, p);
        visit(symbolicQueueClause.getWords(), p);
        visit(symbolicQueueClause.getDataDescName(), p);
        afterSyntax(symbolicQueueClause, p);
        return symbolicQueueClause;
    }

    public Cobol visitSymbolicSourceClause(Cobol.SymbolicSourceClause symbolicSourceClause, PrintOutputCapture<P> p) {
        beforeSyntax(symbolicSourceClause, Space.Location.SYMBOLIC_SOURCE_CLAUSE_PREFIX, p);
        visit(symbolicSourceClause.getWords(), p);
        visit(symbolicSourceClause.getDataDescName(), p);
        afterSyntax(symbolicSourceClause, p);
        return symbolicSourceClause;
    }

    public Cobol visitSymbolicSubQueueClause(Cobol.SymbolicSubQueueClause symbolicSubQueueClause, PrintOutputCapture<P> p) {
        beforeSyntax(symbolicSubQueueClause, Space.Location.SYMBOLIC_SUB_QUEUE_CLAUSE_PREFIX, p);
        visit(symbolicSubQueueClause.getWords(), p);
        visit(symbolicSubQueueClause.getDataDescName(), p);
        afterSyntax(symbolicSubQueueClause, p);
        return symbolicSubQueueClause;
    }

    public Cobol visitSymbolicTerminalClause(Cobol.SymbolicTerminalClause symbolicTerminalClause, PrintOutputCapture<P> p) {
        beforeSyntax(symbolicTerminalClause, Space.Location.SYMBOLIC_TERMINAL_CLAUSE_PREFIX, p);
        visit(symbolicTerminalClause.getWords(), p);
        visit(symbolicTerminalClause.getDataDescName(), p);
        afterSyntax(symbolicTerminalClause, p);
        return symbolicTerminalClause;
    }

    public Cobol visitTableCall(Cobol.TableCall tableCall, PrintOutputCapture<P> p) {
        beforeSyntax(tableCall, Space.Location.TABLE_CLAUSE_PREFIX, p);
        visit(tableCall.getQualifiedDataName(), p);
        visit(tableCall.getSubscripts(), p);
        visit(tableCall.getReferenceModifier(), p);
        afterSyntax(tableCall, p);
        return tableCall;
    }

    public Cobol visitTerminate(Cobol.Terminate terminate, PrintOutputCapture<P> p) {
        beforeSyntax(terminate, Space.Location.TERMINATE_PREFIX, p);
        visit(terminate.getTerminate(), p);
        visit(terminate.getReportName(), p);
        afterSyntax(terminate, p);
        return terminate;
    }

    public Cobol visitTextLengthClause(Cobol.TextLengthClause textLengthClause, PrintOutputCapture<P> p) {
        beforeSyntax(textLengthClause, Space.Location.TEXT_LENGTH_CLAUSE_PREFIX, p);
        visit(textLengthClause.getWords(), p);
        visit(textLengthClause.getDataDescName(), p);
        afterSyntax(textLengthClause, p);
        return textLengthClause;
    }

    public Cobol visitUnString(Cobol.UnString unString, PrintOutputCapture<P> p) {
        beforeSyntax(unString, Space.Location.UNSTRING_PREFIX, p);
        visit(unString.getUnstring(), p);
        visit(unString.getUnstringSendingPhrase(), p);
        visit(unString.getUnstringIntoPhrase(), p);
        visit(unString.getUnstringWithPointerPhrase(), p);
        visit(unString.getUnstringTallyingPhrase(), p);
        visit(unString.getOnOverflowPhrase(), p);
        visit(unString.getNotOnOverflowPhrase(), p);
        visit(unString.getEndUnstring(), p);
        afterSyntax(unString, p);
        return unString;
    }

    public Cobol visitUnstringCountIn(Cobol.UnstringCountIn unstringCountIn, PrintOutputCapture<P> p) {
        beforeSyntax(unstringCountIn, Space.Location.UNSTRING_COUNT_IN_PREFIX, p);
        visit(unstringCountIn.getWords(), p);
        visit(unstringCountIn.getIdentifier(), p);
        afterSyntax(unstringCountIn, p);
        return unstringCountIn;
    }

    public Cobol visitUnstringDelimitedByPhrase(Cobol.UnstringDelimitedByPhrase unstringDelimitedByPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(unstringDelimitedByPhrase, Space.Location.UNSTRING_DELIMITED_BY_PHRASE_PREFIX, p);
        visit(unstringDelimitedByPhrase.getWords(), p);
        visit(unstringDelimitedByPhrase.getName(), p);
        afterSyntax(unstringDelimitedByPhrase, p);
        return unstringDelimitedByPhrase;
    }

    public Cobol visitUnstringDelimiterIn(Cobol.UnstringDelimiterIn unstringDelimiterIn, PrintOutputCapture<P> p) {
        beforeSyntax(unstringDelimiterIn, Space.Location.UNSTRING_DELIMITED_IN_PREFIX, p);
        visit(unstringDelimiterIn.getWords(), p);
        visit(unstringDelimiterIn.getIdentifier(), p);
        afterSyntax(unstringDelimiterIn, p);
        return unstringDelimiterIn;
    }

    public Cobol visitUnstringInto(Cobol.UnstringInto unstringInto, PrintOutputCapture<P> p) {
        beforeSyntax(unstringInto, Space.Location.UNSTRING_INTO_PREFIX, p);
        visit(unstringInto.getIdentifier(), p);
        visit(unstringInto.getUnstringDelimiterIn(), p);
        visit(unstringInto.getUnstringCountIn(), p);
        afterSyntax(unstringInto, p);
        return unstringInto;
    }

    public Cobol visitUnstringIntoPhrase(Cobol.UnstringIntoPhrase unstringIntoPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(unstringIntoPhrase, Space.Location.UNSTRING_INTO_PHRASE_PREFIX, p);
        visit(unstringIntoPhrase.getInto(), p);
        visit(unstringIntoPhrase.getUnstringIntos(), p);
        afterSyntax(unstringIntoPhrase, p);
        return unstringIntoPhrase;
    }

    public Cobol visitUnstringOrAllPhrase(Cobol.UnstringOrAllPhrase unstringOrAllPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(unstringOrAllPhrase, Space.Location.UNSTRING_OR_ALL_PHRASE_PREFIX, p);
        visit(unstringOrAllPhrase.getWords(), p);
        visit(unstringOrAllPhrase.getName(), p);
        afterSyntax(unstringOrAllPhrase, p);
        return unstringOrAllPhrase;
    }

    public Cobol visitUnstringSendingPhrase(Cobol.UnstringSendingPhrase unstringSendingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(unstringSendingPhrase, Space.Location.UNSTRING_SENDING_PHRASE_PREFIX, p);
        visit(unstringSendingPhrase.getIdentifier(), p);
        visit(unstringSendingPhrase.getUnstringDelimitedByPhrase(), p);
        visit(unstringSendingPhrase.getUnstringOrAllPhrases(), p);
        afterSyntax(unstringSendingPhrase, p);
        return unstringSendingPhrase;
    }

    public Cobol visitUnstringTallyingPhrase(Cobol.UnstringTallyingPhrase unstringTallyingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(unstringTallyingPhrase, Space.Location.UNSTRING_TALLYING_PHRASE_PREFIX, p);
        visit(unstringTallyingPhrase.getWords(), p);
        visit(unstringTallyingPhrase.getQualifiedDataName(), p);
        afterSyntax(unstringTallyingPhrase, p);
        return unstringTallyingPhrase;
    }

    public Cobol visitUnstringWithPointerPhrase(Cobol.UnstringWithPointerPhrase unstringWithPointerPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(unstringWithPointerPhrase, Space.Location.UNSTRING_WITH_POINTER_PHRASE_PREFIX, p);
        visit(unstringWithPointerPhrase.getWords(), p);
        visit(unstringWithPointerPhrase.getQualifiedDataName(), p);
        afterSyntax(unstringWithPointerPhrase, p);
        return unstringWithPointerPhrase;
    }

    public Cobol visitUseAfterClause(Cobol.UseAfterClause useAfterClause, PrintOutputCapture<P> p) {
        beforeSyntax(useAfterClause, Space.Location.USE_AFTER_CLAUSE_PREFIX, p);
        visit(useAfterClause.getWords(), p);
        visit(useAfterClause.getUseAfterOn(), p);
        afterSyntax(useAfterClause, p);
        return useAfterClause;
    }

    public Cobol visitUseAfterOn(Cobol.UseAfterOn useAfterOn, PrintOutputCapture<P> p) {
        beforeSyntax(useAfterOn, Space.Location.USE_AFTER_ON_PREFIX, p);
        visit(useAfterOn.getAfterOn(), p);
        visit(useAfterOn.getFileNames(), p);
        afterSyntax(useAfterOn, p);
        return useAfterOn;
    }

    public Cobol visitUseDebugClause(Cobol.UseDebugClause useDebugClause, PrintOutputCapture<P> p) {
        beforeSyntax(useDebugClause, Space.Location.USE_DEBUG_CLAUSE_PREFIX, p);
        visit(useDebugClause.getWords(), p);
        visit(useDebugClause.getUseDebugs(), p);
        afterSyntax(useDebugClause, p);
        return useDebugClause;
    }

    public Cobol visitUseDebugOn(Cobol.UseDebugOn useDebugOn, PrintOutputCapture<P> p) {
        beforeSyntax(useDebugOn, Space.Location.USE_DEBUG_ON_PREFIX, p);
        visit(useDebugOn.getWords(), p);
        visit(useDebugOn.getName(), p);
        afterSyntax(useDebugOn, p);
        return useDebugOn;
    }

    public Cobol visitUseStatement(Cobol.UseStatement useStatement, PrintOutputCapture<P> p) {
        beforeSyntax(useStatement, Space.Location.USE_STATEMENT_PREFIX, p);
        visit(useStatement.getUse(), p);
        visit(useStatement.getClause(), p);
        afterSyntax(useStatement, p);
        return useStatement;
    }

    public Cobol visitValueOfClause(Cobol.ValueOfClause valueOfClause, PrintOutputCapture<P> p) {
        beforeSyntax(valueOfClause, Space.Location.VALUE_OF_CLAUSE_PREFIX, p);
        visit(valueOfClause.getValueOf(), p);
        visit(valueOfClause.getValuePairs(), p);
        afterSyntax(valueOfClause, p);
        return valueOfClause;
    }

    public Cobol visitValuePair(Cobol.ValuePair valuePair, PrintOutputCapture<P> p) {
        beforeSyntax(valuePair, Space.Location.VALUE_PAIR_PREFIX, p);
        visit(valuePair.getSystemName(), p);
        visit(valuePair.getIs(), p);
        visit(valuePair.getName(), p);
        afterSyntax(valuePair, p);
        return valuePair;
    }

    public Cobol visitValuedObjectComputerClause(Cobol.ValuedObjectComputerClause valuedObjectComputerClause, PrintOutputCapture<P> p) {
        beforeSyntax(valuedObjectComputerClause, Space.Location.VALUE_OBJECT_COMPUTER_CLAUSE_PREFIX, p);
        visit(valuedObjectComputerClause.getWords(), p);
        visit(valuedObjectComputerClause.getValue(), p);
        visit(valuedObjectComputerClause.getUnits(), p);
        afterSyntax(valuedObjectComputerClause, p);
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
                beforeSyntax(word, Space.Location.WORD_PREFIX, p);
            }

            p.append(word.getWord());

            if (commentArea != null && !commentArea.isAdded()) {
                visitCommentArea(commentArea, p);
            }
        }

        afterSyntax(word, p);
        return word;
    }

    public Cobol visitWorkingStorageSection(Cobol.WorkingStorageSection workingStorageSection, PrintOutputCapture<P> p) {
        beforeSyntax(workingStorageSection, Space.Location.WORKING_STORAGE_SECTION_PREFIX, p);
        visit(workingStorageSection.getWords(), p);
        visit(workingStorageSection.getDot(), p);
        visit(workingStorageSection.getDataDescriptions(), p);
        afterSyntax(workingStorageSection, p);
        return workingStorageSection;
    }

    public Cobol visitWrite(Cobol.Write write, PrintOutputCapture<P> p) {
        beforeSyntax(write, Space.Location.WRITE_PREFIX, p);
        visit(write.getWrite(), p);
        visit(write.getRecordName(), p);
        visit(write.getWriteFromPhrase(), p);
        visit(write.getWriteAdvancingPhrase(), p);
        visit(write.getWriteAtEndOfPagePhrase(), p);
        visit(write.getWriteNotAtEndOfPagePhrase(), p);
        visit(write.getInvalidKeyPhrase(), p);
        visit(write.getNotInvalidKeyPhrase(), p);
        visit(write.getEndWrite(), p);
        afterSyntax(write, p);
        return write;
    }

    public Cobol visitWriteAdvancingLines(Cobol.WriteAdvancingLines writeAdvancingLines, PrintOutputCapture<P> p) {
        beforeSyntax(writeAdvancingLines, Space.Location.WRITE_ADVANCING_LINES_PREFIX, p);
        visit(writeAdvancingLines.getName(), p);
        visit(writeAdvancingLines.getWord(), p);
        afterSyntax(writeAdvancingLines, p);
        return writeAdvancingLines;
    }

    public Cobol visitWriteAdvancingMnemonic(Cobol.WriteAdvancingMnemonic writeAdvancingMnemonic, PrintOutputCapture<P> p) {
        beforeSyntax(writeAdvancingMnemonic, Space.Location.WRITE_ADVANCING_MNEMONIC_PREFIX, p);
        visit(writeAdvancingMnemonic.getName(), p);
        afterSyntax(writeAdvancingMnemonic, p);
        return writeAdvancingMnemonic;
    }

    public Cobol visitWriteAdvancingPage(Cobol.WriteAdvancingPage writeAdvancingPage, PrintOutputCapture<P> p) {
        beforeSyntax(writeAdvancingPage, Space.Location.WRITE_ADVANCING_PAGE_PREFIX, p);
        visit(writeAdvancingPage.getPage(), p);
        afterSyntax(writeAdvancingPage, p);
        return writeAdvancingPage;
    }

    public Cobol visitWriteAdvancingPhrase(Cobol.WriteAdvancingPhrase writeAdvancingPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(writeAdvancingPhrase, Space.Location.WRITE_ADVANCING_PHRASE_PREFIX, p);
        visit(writeAdvancingPhrase.getWords(), p);
        visit(writeAdvancingPhrase.getWriteBy(), p);
        afterSyntax(writeAdvancingPhrase, p);
        return writeAdvancingPhrase;
    }

    public Cobol visitWriteFromPhrase(Cobol.WriteFromPhrase writeFromPhrase, PrintOutputCapture<P> p) {
        beforeSyntax(writeFromPhrase, Space.Location.WRITE_FROM_PHRASE_PREFIX, p);
        visit(writeFromPhrase.getFrom(), p);
        visit(writeFromPhrase.getName(), p);
        afterSyntax(writeFromPhrase, p);
        return writeFromPhrase;
    }

    @Override
    public Markers visitMarkers(Markers markers, PrintOutputCapture<P> p) {
        return markers.withMarkers(ListUtils.map(markers.getMarkers(), marker -> {
            // Do not visit markers that are required for printing the LST.
            if (marker instanceof SequenceArea ||
                    marker instanceof IndicatorArea ||
                    marker instanceof CommentArea ||
                    marker instanceof Continuation ||
                    marker instanceof Lines ||
                    marker instanceof Copy ||
                    marker instanceof Replace ||
                    marker instanceof ReplaceBy ||
                    marker instanceof ReplaceOff ||
                    (marker instanceof SearchResult && SearchResultKey.COPIED_SOURCE.equals(((SearchResult) marker).getDescription()))) {
                return marker;
            }
            return super.visitMarker(marker, p);
        }));
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
        if (printColumns) {
            for (Marker marker : indicatorArea.getMarkers().getMarkers()) {
                p.out.append(p.getMarkerPrinter().beforePrefix(marker, new Cursor(getCursor(), marker), COBOL_MARKER_WRAPPER));
            }
            visitMarkers(indicatorArea.getMarkers(), p);
            for (Marker marker : indicatorArea.getMarkers().getMarkers()) {
                p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(getCursor(), marker), COBOL_MARKER_WRAPPER));
            }
            p.append(indicatorArea.getIndicator());
            afterSyntax(indicatorArea.getMarkers(), p);
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

    private static final UnaryOperator<String> COBOL_MARKER_WRAPPER =
            out -> "~~" + out + (out.isEmpty() ? "" : "~~") + ">";

    protected void beforeSyntax(Cobol c, Space.Location loc, PrintOutputCapture<P> p) {
        beforeSyntax(c.getPrefix(), c.getMarkers(), loc, p);
    }

    protected void beforeSyntax(Space prefix, Markers markers, @Nullable Space.Location loc, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforePrefix(marker, new Cursor(getCursor(), marker), COBOL_MARKER_WRAPPER));
        }
        if (loc != null) {
            visitSpace(prefix, loc, p);
        }
        visitMarkers(markers, p);
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().beforeSyntax(marker, new Cursor(getCursor(), marker), COBOL_MARKER_WRAPPER));
        }
    }

    protected void afterSyntax(Cobol c, PrintOutputCapture<P> p) {
        afterSyntax(c.getMarkers(), p);
    }

    protected void afterSyntax(Markers markers, PrintOutputCapture<P> p) {
        for (Marker marker : markers.getMarkers()) {
            p.out.append(p.getMarkerPrinter().afterSyntax(marker, new Cursor(getCursor(), marker), COBOL_MARKER_WRAPPER));
        }
    }
}
