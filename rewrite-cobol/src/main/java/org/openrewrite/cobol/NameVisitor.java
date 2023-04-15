package org.openrewrite.cobol;

import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.internal.ListUtils;

public class NameVisitor<P> extends CobolIsoVisitor<P> {

    @Override
    public Cobol.CommentEntry visitCommentEntry(Cobol.CommentEntry commentEntry, P p) {
        return commentEntry;
    }

    @Override
    public Cobol.Abbreviation visitAbbreviation(Cobol.Abbreviation abbreviation, P p) {
        Cobol.Abbreviation a = abbreviation;
        a = a.withArithmeticExpression(visitAndCast(a.getArithmeticExpression(), p));
        a = a.withAbbreviation(visitAndCast(a.getAbbreviation(), p));
        return a;
    }

    @Override
    public Cobol.Accept visitAccept(Cobol.Accept accept, P p) {
        Cobol.Accept a = accept;
        a = a.withIdentifier(visitAndCast(a.getIdentifier(), p));
        a = a.withOperation(visitAndCast(a.getOperation(), p));
        return a;
    }

    @Override
    public Cobol.AcceptFromDateStatement visitAcceptFromDateStatement(Cobol.AcceptFromDateStatement acceptFromDateStatement, P p) {
        return acceptFromDateStatement;
    }

    @Override
    public Cobol.AcceptFromEscapeKeyStatement visitAcceptFromEscapeKeyStatement(Cobol.AcceptFromEscapeKeyStatement acceptFromEscapeKeyStatement, P p) {
        return acceptFromEscapeKeyStatement;
    }

    @Override
    public Cobol.AcceptFromMnemonicStatement visitAcceptFromMnemonicStatement(Cobol.AcceptFromMnemonicStatement acceptFromMnemonicStatement, P p) {
        Cobol.AcceptFromMnemonicStatement a = acceptFromMnemonicStatement;
        a = a.withMnemonicName(visitAndCast(a.getMnemonicName(), p));
        return a;
    }

    @Override
    public Cobol.AcceptMessageCountStatement visitAcceptMessageCountStatement(Cobol.AcceptMessageCountStatement acceptMessageCountStatement, P p) {
        return acceptMessageCountStatement;
    }

    @Override
    public Cobol.AccessModeClause visitAccessModeClause(Cobol.AccessModeClause accessModeClause, P p) {
        return accessModeClause;
    }

    @Override
    public Cobol.Add visitAdd(Cobol.Add add, P p) {
        Cobol.Add a = add;
        a = a.withOperation(visitAndCast(a.getOperation(), p));
        a = a.withOnSizeError(visitAndCast(a.getOnSizeError(), p));
        a = a.withNotOnSizeError(visitAndCast(a.getNotOnSizeError(), p));
        return a;
    }

    @Override
    public Cobol.AddCorresponding visitAddCorresponding(Cobol.AddCorresponding addCorresponding, P p) {
        Cobol.AddCorresponding a = addCorresponding;
        a = a.withIdentifier(visitAndCast(a.getIdentifier(), p));
        a = a.withRoundable(visitAndCast(a.getRoundable(), p));
        return addCorresponding;
    }

    @Override
    public Cobol.AddTo visitAddTo(Cobol.AddTo addTo, P p) {
        Cobol.AddTo a = addTo;
        a = a.withFrom(ListUtils.map(a.getFrom(), it -> visitAndCast(it, p)));
        a = a.withRoundables(ListUtils.map(a.getRoundables(), it -> visitAndCast(it, p)));
        return a;
    }

    @Override
    public Cobol.AddToGiving visitAddToGiving(Cobol.AddToGiving addToGiving, P p) {
        Cobol.AddToGiving a = addToGiving;
        a = a.withFrom(ListUtils.map(a.getFrom(), it -> visitAndCast(it, p)));
        a = a.withNames(ListUtils.map(a.getNames(), it -> visitAndCast(it, p)));
        a = a.withRoundables(ListUtils.map(a.getRoundables(), it -> visitAndCast(it, p)));
        return a;
    }

    @Override
    public Cobol.AlphabetAlso visitAlphabetAlso(Cobol.AlphabetAlso alphabetAlso, P p) {
        Cobol.AlphabetAlso a = alphabetAlso;
        a = a.withLiterals(ListUtils.map(a.getLiterals(), it -> visitAndCast(it, p)));
        return a;
    }

    @Override
    public Cobol.AlphabetClause visitAlphabetClause(Cobol.AlphabetClause alphabetClause, P p) {
        Cobol.AlphabetClause a = alphabetClause;
        a = a.withName(visitAndCast(a.getName(), p));
        a = a.withWords(ListUtils.map(a.getWords(), it -> visitAndCast(it, p)));
        return a;
    }

    @Override
    public Cobol.AlphabetLiteral visitAlphabetLiteral(Cobol.AlphabetLiteral alphabetLiteral, P p) {
        Cobol.AlphabetLiteral a = alphabetLiteral;
        a = a.withLiteral(visitAndCast(a.getLiteral(), p));
        a = a.withAlphabetThrough(visitAndCast(a.getAlphabetThrough(), p));
        a = a.withAlphabetAlso(ListUtils.map(a.getAlphabetAlso(), it -> visitAndCast(it, p)));
        return a;
    }

    @Override
    public Cobol.AlphabetThrough visitAlphabetThrough(Cobol.AlphabetThrough alphabetThrough, P p) {
        Cobol.AlphabetThrough a = alphabetThrough;
        a = a.withLiteral(visitAndCast(a.getLiteral(), p));
        return a;
    }

    @Override
    public Cobol.AlteredGoTo visitAlteredGoTo(Cobol.AlteredGoTo alteredGoTo, P p) {
        return alteredGoTo;
    }

    @Override
    public Cobol.AlternateRecordKeyClause visitAlternateRecordKeyClause(Cobol.AlternateRecordKeyClause alternateRecordKeyClause, P p) {
        Cobol.AlternateRecordKeyClause a = alternateRecordKeyClause;
        a = a.withQualifiedDataName(visitAndCast(a.getQualifiedDataName(), p));
        a = a.withPasswordClause(visitAndCast(a.getPasswordClause(), p));
        return a;
    }

    @Override
    public Cobol.AlterProceedTo visitAlterProceedTo(Cobol.AlterProceedTo alterProceedTo, P p) {
        Cobol.AlterProceedTo a = alterProceedTo;
        a = a.withFrom(visitAndCast(a.getFrom(), p));
        a = a.withTo(visitAndCast(a.getTo(), p));
        return a;
    }

    @Override
    public Cobol.AlterStatement visitAlterStatement(Cobol.AlterStatement alterStatement, P p) {
        Cobol.AlterStatement a = alterStatement;
        a = a.withAlterProceedTo(ListUtils.map(a.getAlterProceedTo(), it -> visitAndCast(it, p)));
        return a;
    }

    @Override
    public Cobol.AndOrCondition visitAndOrCondition(Cobol.AndOrCondition andOrCondition, P p) {
        Cobol.AndOrCondition a = andOrCondition;
        a = a.withCombinableCondition(visitAndCast(a.getCombinableCondition(), p));
        a = a.withAbbreviations(ListUtils.map(a.getAbbreviations(), it -> visitAndCast(it, p)));
        return a;
    }

    @Override
    public Cobol.Argument visitArgument(Cobol.Argument argument, P p) {
        Cobol.Argument a = argument;
        a = a.withFirst(visitAndCast(a.getFirst(), p));
        a = a.withIntegerLiteral(visitAndCast(a.getIntegerLiteral(), p));
        return a;
    }

    @Override
    public Cobol.ArithmeticExpression visitArithmeticExpression(Cobol.ArithmeticExpression arithmeticExpression, P p) {
        Cobol.ArithmeticExpression a = arithmeticExpression;
        a = a.withMultDivs(visitAndCast(a.getMultDivs(), p));
        a = a.withPlusMinuses(ListUtils.map(a.getPlusMinuses(), it -> visitAndCast(it, p)));
        return a;
    }

    @Override
    public Cobol.AssignClause visitAssignClause(Cobol.AssignClause assignClause, P p) {
        Cobol.AssignClause a = assignClause;
        a = a.withName(visitAndCast(a.getName(), p));
        return a;
    }

    @Override
    public Cobol.BlockContainsClause visitBlockContainsClause(Cobol.BlockContainsClause blockContainsClause, P p) {
        Cobol.BlockContainsClause b = blockContainsClause;
        b = b.withIntegerLiteral(visitAndCast(b.getIntegerLiteral(), p));
        b = b.withBlockContainsTo(visitAndCast(b.getBlockContainsTo(), p));
        return b;
    }

    @Override
    public Cobol.BlockContainsTo visitBlockContainsTo(Cobol.BlockContainsTo blockContainsTo, P p) {
        Cobol.BlockContainsTo b = blockContainsTo;
        b = b.withIntegerLiteral(visitAndCast(b.getIntegerLiteral(), p));
        return b;
    }

    @Override
    public Cobol.Call visitCall(Cobol.Call call, P p) {
        Cobol.Call c = call;
        c = c.withIdentifier(visitAndCast(c.getIdentifier(), p));
        c = c.withCallUsingPhrase(visitAndCast(c.getCallUsingPhrase(), p));
        c = c.withCallGivingPhrase(visitAndCast(c.getCallGivingPhrase(), p));
        c = c.withOnOverflowPhrase(visitAndCast(c.getOnOverflowPhrase(), p));
        c = c.withOnExceptionClause(visitAndCast(c.getOnExceptionClause(), p));
        c = c.withNotOnExceptionClause(visitAndCast(c.getNotOnExceptionClause(), p));
        return c;
    }

    @Override
    public Cobol.CallBy visitCallBy(Cobol.CallBy callBy, P p) {
        Cobol.CallBy c = callBy;
        c = c.withIdentifier(visitAndCast(c.getIdentifier(), p));
        return c;
    }

    @Override
    public Cobol.CallGivingPhrase visitCallGivingPhrase(Cobol.CallGivingPhrase callGivingPhrase, P p) {
        Cobol.CallGivingPhrase c = callGivingPhrase;
        c = c.withIdentifier(visitAndCast(c.getIdentifier(), p));
        return c;
    }

    @Override
    public Cobol.CallPhrase visitCallPhrase(Cobol.CallPhrase callPhrase, P p) {
        Cobol.CallPhrase c = callPhrase;
        c = c.withParameters(ListUtils.map(c.getParameters(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.Cancel visitCancel(Cobol.Cancel cancel, P p) {
        Cobol.Cancel c = cancel;
        c = c.withCancelCalls(ListUtils.map(c.getCancelCalls(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.CancelCall visitCancelCall(Cobol.CancelCall cancelCall, P p) {
        Cobol.CancelCall c = cancelCall;
        c = c.withLibraryName(visitAndCast(c.getLibraryName(), p));
        c = c.withIdentifier(visitAndCast(c.getIdentifier(), p));
        c = c.withLiteral(visitAndCast(c.getLiteral(), p));
        return c;
    }

    @Override
    public Cobol.ChannelClause visitChannelClause(Cobol.ChannelClause channelClause, P p) {
        Cobol.ChannelClause c = channelClause;
        c = c.withLiteral(visitAndCast(c.getLiteral(), p));
        c = c.withMnemonicName(visitAndCast(c.getMnemonicName(), p));
        return c;
    }

    @Override
    public Cobol.ClassClause visitClassClause(Cobol.ClassClause classClause, P p) {
        Cobol.ClassClause c = classClause;
        c = c.withClassName(visitAndCast(c.getClassName(), p));
        c = c.withThroughs(ListUtils.map(c.getThroughs(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.ClassClauseThrough visitClassClauseThrough(Cobol.ClassClauseThrough classClauseThrough, P p) {
        Cobol.ClassClauseThrough c = classClauseThrough;
        c = c.withFrom(visitAndCast(c.getFrom(), p));
        c = c.withTo(visitAndCast(c.getTo(), p));
        return c;
    }

    @Override
    public Cobol.ClassCondition visitClassCondition(Cobol.ClassCondition classCondition, P p) {
        Cobol.ClassCondition c = classCondition;
        c = c.withName(visitAndCast(c.getName(), p));
        c = c.withType(visitAndCast(c.getType(), p));
        return c;
    }

    @Override
    public Cobol.Close visitClose(Cobol.Close close, P p) {
        Cobol.Close c = close;
        c = c.withCloseFiles(ListUtils.map(c.getCloseFiles(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.CloseFile visitCloseFile(Cobol.CloseFile closeFile, P p) {
        Cobol.CloseFile c = closeFile;
        c = c.withFileName(visitAndCast(c.getFileName(), p));
        c = c.withCloseStatement(visitAndCast(c.getCloseStatement(), p));
        return c;
    }

    @Override
    public Cobol.ClosePortFileIOStatement visitClosePortFileIOStatement(Cobol.ClosePortFileIOStatement closePortFileIOStatement, P p) {
        Cobol.ClosePortFileIOStatement c = closePortFileIOStatement;
        c = c.withClosePortFileIOUsing(ListUtils.map(c.getClosePortFileIOUsing(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.ClosePortFileIOUsingAssociatedData visitClosePortFileIOUsingAssociatedData(Cobol.ClosePortFileIOUsingAssociatedData closePortFileIOUsingAssociatedData, P p) {
        Cobol.ClosePortFileIOUsingAssociatedData c = closePortFileIOUsingAssociatedData;
        c = c.withIdentifier(visitAndCast(c.getIdentifier(), p));
        return c;
    }

    @Override
    public Cobol.ClosePortFileIOUsingAssociatedDataLength visitClosePortFileIOUsingAssociatedDataLength(Cobol.ClosePortFileIOUsingAssociatedDataLength closePortFileIOUsingAssociatedDataLength, P p) {
        Cobol.ClosePortFileIOUsingAssociatedDataLength c = closePortFileIOUsingAssociatedDataLength;
        c = c.withIdentifier(visitAndCast(c.getIdentifier(), p));
        return c;
    }

    @Override
    public Cobol.ClosePortFileIOUsingCloseDisposition visitClosePortFileIOUsingCloseDisposition(Cobol.ClosePortFileIOUsingCloseDisposition closePortFileIOUsingCloseDisposition, P p) {
        return closePortFileIOUsingCloseDisposition;
    }

    @Override
    public Cobol.CloseReelUnitStatement visitCloseReelUnitStatement(Cobol.CloseReelUnitStatement closeReelUnitStatement, P p) {
        return closeReelUnitStatement;
    }

    @Override
    public Cobol.CloseRelativeStatement visitCloseRelativeStatement(Cobol.CloseRelativeStatement closeRelativeStatement, P p) {
        return closeRelativeStatement;
    }

    @Override
    public Cobol.CodeSetClause visitCodeSetClause(Cobol.CodeSetClause codeSetClause, P p) {
        Cobol.CodeSetClause c = codeSetClause;
        c = c.withAlphabetName(visitAndCast(c.getAlphabetName(), p));
        return c;
    }

    @Override
    public Cobol.CollatingSequenceAlphabet visitCollatingSequenceAlphabet(Cobol.CollatingSequenceAlphabet collatingSequenceAlphabet, P p) {
        Cobol.CollatingSequenceAlphabet c = collatingSequenceAlphabet;
        c = c.withAlphabetName(visitAndCast(c.getAlphabetName(), p));
        return c;
    }

    @Override
    public Cobol.CollatingSequenceClause visitCollatingSequenceClause(Cobol.CollatingSequenceClause collatingSequenceClause, P p) {
        Cobol.CollatingSequenceClause c = collatingSequenceClause;
        c = c.withAlphabetName(ListUtils.map(c.getAlphabetName(), it -> visitAndCast(it, p)));
        c = c.withAlphanumeric(visitAndCast(c.getAlphanumeric(), p));
        c = c.withNational(visitAndCast(c.getNational(), p));
        return c;
    }

    @Override
    public Cobol.CombinableCondition visitCombinableCondition(Cobol.CombinableCondition combinableCondition, P p) {
        Cobol.CombinableCondition c = combinableCondition;
        c = c.withSimpleCondition(visitAndCast(c.getSimpleCondition(), p));
        return c;
    }

    @Override
    public Cobol.CommitmentControlClause visitCommitmentControlClause(Cobol.CommitmentControlClause commitmentControlClause, P p) {
        Cobol.CommitmentControlClause c = commitmentControlClause;
        c = c.withFileName(visitAndCast(c.getFileName(), p));
        return c;
    }

    @Override
    public Cobol.CommunicationDescriptionEntryFormat1 visitCommunicationDescriptionEntryFormat1(Cobol.CommunicationDescriptionEntryFormat1 communicationDescriptionEntryFormat1, P p) {
        Cobol.CommunicationDescriptionEntryFormat1 c = communicationDescriptionEntryFormat1;
        c = c.withName(visitAndCast(c.getName(), p));
        c = c.withInputs(ListUtils.map(c.getInputs(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.CommunicationDescriptionEntryFormat2 visitCommunicationDescriptionEntryFormat2(Cobol.CommunicationDescriptionEntryFormat2 communicationDescriptionEntryFormat2, P p) {
        Cobol.CommunicationDescriptionEntryFormat2 c = communicationDescriptionEntryFormat2;
        c = c.withName(visitAndCast(c.getName(), p));
        c = c.withOutputs(ListUtils.map(c.getOutputs(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.CommunicationDescriptionEntryFormat3 visitCommunicationDescriptionEntryFormat3(Cobol.CommunicationDescriptionEntryFormat3 communicationDescriptionEntryFormat3, P p) {
        Cobol.CommunicationDescriptionEntryFormat3 c = communicationDescriptionEntryFormat3;
        c = c.withName(visitAndCast(c.getName(), p));
        c = c.withInitialIOs(ListUtils.map(c.getInitialIOs(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.CommunicationSection visitCommunicationSection(Cobol.CommunicationSection communicationSection, P p) {
        Cobol.CommunicationSection c = communicationSection;
        c = c.withEntries(ListUtils.map(c.getEntries(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.CompilationUnit visitCompilationUnit(Cobol.CompilationUnit compilationUnit, P p) {
        return super.visitCompilationUnit(compilationUnit, p);
    }

    @Override
    public Cobol.Compute visitCompute(Cobol.Compute compute, P p) {
        Cobol.Compute c = compute;
        c = c.withRoundables(ListUtils.map(c.getRoundables(), it -> visitAndCast(it, p)));
        c = c.withArithmeticExpression(visitAndCast(c.getArithmeticExpression(), p));
        c = c.withOnSizeErrorPhrase(visitAndCast(c.getOnSizeErrorPhrase(), p));
        c = c.withNotOnSizeErrorPhrase(visitAndCast(c.getNotOnSizeErrorPhrase(), p));
        return c;
    }

    @Override
    public Cobol.Condition visitCondition(Cobol.Condition condition, P p) {
        Cobol.Condition c = condition;
        c = c.withCombinableCondition(visitAndCast(c.getCombinableCondition(), p));
        c = c.withAndOrConditions(ListUtils.map(c.getAndOrConditions(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.ConditionNameReference visitConditionNameReference(Cobol.ConditionNameReference conditionNameReference, P p) {
        Cobol.ConditionNameReference c = conditionNameReference;
        c = c.withName(visitAndCast(c.getName(), p));
        c = c.withInDatas(ListUtils.map(c.getInDatas(), it -> visitAndCast(it, p)));
        c = c.withInFile(visitAndCast(c.getInFile(), p));
        c = c.withReferences(ListUtils.map(c.getReferences(), it -> visitAndCast(it, p)));
        c = c.withInMnemonics(ListUtils.map(c.getInMnemonics(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.ConditionNameSubscriptReference visitConditionNameSubscriptReference(Cobol.ConditionNameSubscriptReference conditionNameSubscriptReference, P p) {
        Cobol.ConditionNameSubscriptReference c = conditionNameSubscriptReference;
        c = c.withSubscripts(ListUtils.map(c.getSubscripts(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.ConfigurationSection visitConfigurationSection(Cobol.ConfigurationSection configurationSection, P p) {
        Cobol.ConfigurationSection c = configurationSection;
        c = c.withParagraphs(ListUtils.map(c.getParagraphs(), it -> visitAndCast(it, p)));
        return c;
    }

    @Override
    public Cobol.Continue visitContinue(Cobol.Continue continuez, P p) {
        return continuez;
    }

    @Override
    public Cobol.CurrencyClause visitCurrencyClause(Cobol.CurrencyClause currencyClause, P p) {
        Cobol.CurrencyClause c = currencyClause;
        c = c.withLiteral(visitAndCast(c.getLiteral(), p));
        c = c.withPictureSymbolLiteral(visitAndCast(c.getPictureSymbolLiteral(), p));
        return c;
    }
}
