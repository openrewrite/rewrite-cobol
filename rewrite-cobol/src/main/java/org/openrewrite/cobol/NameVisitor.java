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
        return a;
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

    @Override
    public Cobol.DataAlignedClause visitDataAlignedClause(Cobol.DataAlignedClause dataAlignedClause, P p) {
        return dataAlignedClause;
    }

    @Override
    public Cobol.DataBaseSection visitDataBaseSection(Cobol.DataBaseSection dataBaseSection, P p) {
        Cobol.DataBaseSection d = dataBaseSection;
        d = d.withEntries(ListUtils.map(d.getEntries(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataBaseSectionEntry visitDataBaseSectionEntry(Cobol.DataBaseSectionEntry dataBaseSectionEntry, P p) {
        Cobol.DataBaseSectionEntry d = dataBaseSectionEntry;
        d = d.withDb(visitAndCast(d.getDb(), p));
        d = d.withFrom(visitAndCast(d.getFrom(), p));
        d = d.withTo(visitAndCast(d.getTo(), p));
        return d;
    }

    @Override
    public Cobol.DataBlankWhenZeroClause visitDataBlankWhenZeroClause(Cobol.DataBlankWhenZeroClause dataBlankWhenZeroClause, P p) {
        return dataBlankWhenZeroClause;
    }

    @Override
    public Cobol.DataCommonOwnLocalClause visitDataCommonOwnLocalClause(Cobol.DataCommonOwnLocalClause dataCommonOwnLocalClause, P p) {
        return dataCommonOwnLocalClause;
    }

    @Override
    public Cobol.DataDescriptionEntry visitDataDescriptionEntry(Cobol.DataDescriptionEntry dataDescriptionEntry, P p) {
        Cobol.DataDescriptionEntry d = dataDescriptionEntry;
        d = d.withName(visitAndCast(d.getName(), p));
        d = d.withClauses(ListUtils.map(d.getClauses(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataDivision visitDataDivision(Cobol.DataDivision dataDivision, P p) {
        Cobol.DataDivision d = dataDivision;
        d = d.withSections(ListUtils.map(d.getSections(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataExternalClause visitDataExternalClause(Cobol.DataExternalClause dataExternalClause, P p) {
        Cobol.DataExternalClause d = dataExternalClause;
        d = d.withLiteral(visitAndCast(d.getLiteral(), p));
        return d;
    }

    @Override
    public Cobol.DataGlobalClause visitDataGlobalClause(Cobol.DataGlobalClause dataGlobalClause, P p) {
        return dataGlobalClause;
    }

    @Override
    public Cobol.DataIntegerStringClause visitDataIntegerStringClause(Cobol.DataIntegerStringClause dataIntegerStringClause, P p) {
        return dataIntegerStringClause;
    }

    @Override
    public Cobol.DataJustifiedClause visitDataJustifiedClause(Cobol.DataJustifiedClause dataJustifiedClause, P p) {
        return dataJustifiedClause;
    }

    @Override
    public Cobol.DataOccursClause visitDataOccursClause(Cobol.DataOccursClause dataOccursClause, P p) {
        Cobol.DataOccursClause d = dataOccursClause;
        d = d.withName(visitAndCast(d.getName(), p));
        d = d.withDataOccursTo(visitAndCast(d.getDataOccursTo(), p));
        d = d.withDataOccursDepending(visitAndCast(d.getDataOccursDepending(), p));
        d = d.withSortIndexed(ListUtils.map(d.getSortIndexed(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataOccursDepending visitDataOccursDepending(Cobol.DataOccursDepending dataOccursDepending, P p) {
        Cobol.DataOccursDepending d = dataOccursDepending;
        d = d.withQualifiedDataName(visitAndCast(d.getQualifiedDataName(), p));
        return d;
    }

    @Override
    public Cobol.DataOccursIndexed visitDataOccursIndexed(Cobol.DataOccursIndexed dataOccursIndexed, P p) {
        Cobol.DataOccursIndexed d = dataOccursIndexed;
        d = d.withIndexNames(ListUtils.map(d.getIndexNames(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataOccursSort visitDataOccursSort(Cobol.DataOccursSort dataOccursSort, P p) {
        Cobol.DataOccursSort d = dataOccursSort;
        d = d.withQualifiedDataNames(ListUtils.map(d.getQualifiedDataNames(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataOccursTo visitDataOccursTo(Cobol.DataOccursTo dataOccursTo, P p) {
        Cobol.DataOccursTo d = dataOccursTo;
        d = d.withIntegerLiteral(visitAndCast(d.getIntegerLiteral(), p));
        return d;
    }

    @Override
    public Cobol.DataPictureClause visitDataPictureClause(Cobol.DataPictureClause dataPictureClause, P p) {
        Cobol.DataPictureClause d = dataPictureClause;
        d = d.withPictures(ListUtils.map(d.getPictures(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataReceivedByClause visitDataReceivedByClause(Cobol.DataReceivedByClause dataReceivedByClause, P p) {
        return dataReceivedByClause;
    }

    @Override
    public Cobol.DataRecordAreaClause visitDataRecordAreaClause(Cobol.DataRecordAreaClause dataRecordAreaClause, P p) {
        return dataRecordAreaClause;
    }

    @Override
    public Cobol.DataRecordsClause visitDataRecordsClause(Cobol.DataRecordsClause dataRecordsClause, P p) {
        Cobol.DataRecordsClause d = dataRecordsClause;
        d = d.withDataName(ListUtils.map(d.getDataName(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataRedefinesClause visitDataRedefinesClause(Cobol.DataRedefinesClause dataRedefinesClause, P p) {
        Cobol.DataRedefinesClause d = dataRedefinesClause;
        d = d.withDataName(visitAndCast(d.getDataName(), p));
        return d;
    }

    @Override
    public Cobol.DataRenamesClause visitDataRenamesClause(Cobol.DataRenamesClause dataRenamesClause, P p) {
        Cobol.DataRenamesClause d = dataRenamesClause;
        d = d.withFromName(visitAndCast(d.getFromName(), p));
        d = d.withToName(visitAndCast(d.getToName(), p));
        return d;
    }

    @Override
    public Cobol.DataSignClause visitDataSignClause(Cobol.DataSignClause dataSignClause, P p) {
        return dataSignClause;
    }

    @Override
    public Cobol.DataSynchronizedClause visitDataSynchronizedClause(Cobol.DataSynchronizedClause dataSynchronizedClause, P p) {
        return dataSynchronizedClause;
    }

    @Override
    public Cobol.DataThreadLocalClause visitDataThreadLocalClause(Cobol.DataThreadLocalClause dataThreadLocalClause, P p) {
        return dataThreadLocalClause;
    }

    @Override
    public Cobol.DataTypeClause visitDataTypeClause(Cobol.DataTypeClause dataTypeClause, P p) {
        Cobol.DataTypeClause d = dataTypeClause;
        d = d.withParenthesized(visitAndCast(d.getParenthesized(), p));
        return d;
    }

    @Override
    public Cobol.DataTypeDefClause visitDataTypeDefClause(Cobol.DataTypeDefClause dataTypeDefClause, P p) {
        return dataTypeDefClause;
    }

    @Override
    public Cobol.DataUsageClause visitDataUsageClause(Cobol.DataUsageClause dataUsageClause, P p) {
        return dataUsageClause;
    }

    @Override
    public Cobol.DataUsingClause visitDataUsingClause(Cobol.DataUsingClause dataUsingClause, P p) {
        Cobol.DataUsingClause d = dataUsingClause;
        d = d.withName(visitAndCast(d.getName(), p));
        return d;
    }

    @Override
    public Cobol.DataValueClause visitDataValueClause(Cobol.DataValueClause dataValueClause, P p) {
        Cobol.DataValueClause d = dataValueClause;
        d = d.withCobols(ListUtils.map(d.getCobols(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DataValueInterval visitDataValueInterval(Cobol.DataValueInterval dataValueInterval, P p) {
        Cobol.DataValueInterval d = dataValueInterval;
        d = d.withFrom(visitAndCast(d.getFrom(), p));
        d = d.withTo(visitAndCast(d.getTo(), p));
        return d;
    }

    @Override
    public Cobol.DataValueIntervalTo visitDataValueIntervalTo(Cobol.DataValueIntervalTo dataValueIntervalTo, P p) {
        Cobol.DataValueIntervalTo d = dataValueIntervalTo;
        d = d.withLiteral(visitAndCast(d.getLiteral(), p));
        return d;
    }

    @Override
    public Cobol.DataWithLowerBoundsClause visitDataWithLowerBoundsClause(Cobol.DataWithLowerBoundsClause dataWithLowerBoundsClause, P p) {
        return dataWithLowerBoundsClause;
    }

    @Override
    public Cobol.DecimalPointClause visitDecimalPointClause(Cobol.DecimalPointClause decimalPointClause, P p) {
        return decimalPointClause;
    }

    @Override
    public Cobol.DefaultComputationalSignClause visitDefaultComputationalSignClause(Cobol.DefaultComputationalSignClause defaultComputationalSignClause, P p) {
        return defaultComputationalSignClause;
    }

    @Override
    public Cobol.DefaultDisplaySignClause visitDefaultDisplaySignClause(Cobol.DefaultDisplaySignClause defaultDisplaySignClause, P p) {
        return defaultDisplaySignClause;
    }

    @Override
    public Cobol.Delete visitDelete(Cobol.Delete delete, P p) {
        Cobol.Delete d = delete;
        d = d.withFileName(visitAndCast(d.getFileName(), p));
        d = d.withInvalidKey(visitAndCast(d.getInvalidKey(), p));
        d = d.withNotInvalidKey(visitAndCast(d.getNotInvalidKey(), p));
        return d;
    }

    @Override
    public Cobol.DestinationCountClause visitDestinationCountClause(Cobol.DestinationCountClause destinationCountClause, P p) {
        Cobol.DestinationCountClause d = destinationCountClause;
        d = d.withDataDescName(visitAndCast(d.getDataDescName(), p));
        return d;
    }

    @Override
    public Cobol.DestinationTableClause visitDestinationTableClause(Cobol.DestinationTableClause destinationTableClause, P p) {
        Cobol.DestinationTableClause d = destinationTableClause;
        d = d.withIntegerLiteral(visitAndCast(d.getIntegerLiteral(), p));
        d = d.withIndexNames(ListUtils.map(d.getIndexNames(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.Disable visitDisable(Cobol.Disable disable, P p) {
        Cobol.Disable d = disable;
        d = d.withCdName(visitAndCast(d.getCdName(), p));
        d = d.withKeyName(visitAndCast(d.getKeyName(), p));
        return d;
    }

    @Override
    public Cobol.Display visitDisplay(Cobol.Display display, P p) {
        Cobol.Display d = display;
        d = d.withOperands(ListUtils.map(d.getOperands(), it -> visitAndCast(it, p)));
        d = d.withDisplayAt(visitAndCast(d.getDisplayAt(), p));
        d = d.withDisplayUpon(visitAndCast(d.getDisplayUpon(), p));
        d = d.withOnExceptionClause(visitAndCast(d.getOnExceptionClause(), p));
        d = d.withNotOnExceptionClause(visitAndCast(d.getNotOnExceptionClause(), p));
        return d;
    }

    @Override
    public Cobol.DisplayAt visitDisplayAt(Cobol.DisplayAt displayAt, P p) {
        Cobol.DisplayAt d = displayAt;
        d = d.withName(visitAndCast(d.getName(), p));
        return d;
    }

    @Override
    public Cobol.DisplayUpon visitDisplayUpon(Cobol.DisplayUpon displayUpon, P p) {
        Cobol.DisplayUpon d = displayUpon;
        d = d.withName(visitAndCast(d.getName(), p));
        return d;
    }

    @Override
    public Cobol.Divide visitDivide(Cobol.Divide divide, P p) {
        Cobol.Divide d = divide;
        d = d.withName(visitAndCast(d.getName(), p));
        d = d.withAction(visitAndCast(d.getAction(), p));
        d = d.withDivideRemainder(visitAndCast(d.getDivideRemainder(), p));
        d = d.withOnSizeErrorPhrase(visitAndCast(d.getOnSizeErrorPhrase(), p));
        d = d.withNotOnSizeErrorPhrase(visitAndCast(d.getNotOnSizeErrorPhrase(), p));
        return d;
    }

    @Override
    public Cobol.DivideGiving visitDivideGiving(Cobol.DivideGiving divideGiving, P p) {
        Cobol.DivideGiving d = divideGiving;
        d = d.withName(visitAndCast(d.getName(), p));
        return d;
    }

    @Override
    public Cobol.DivideGivingPhrase visitDivideGivingPhrase(Cobol.DivideGivingPhrase divideGivingPhrase, P p) {
        Cobol.DivideGivingPhrase d = divideGivingPhrase;
        d = d.withRoundables(ListUtils.map(d.getRoundables(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DivideInto visitDivideInto(Cobol.DivideInto divideInto, P p) {
        Cobol.DivideInto d = divideInto;
        d = d.withRoundables(ListUtils.map(d.getRoundables(), it -> visitAndCast(it, p)));
        return d;
    }

    @Override
    public Cobol.DivideRemainder visitDivideRemainder(Cobol.DivideRemainder divideRemainder, P p) {
        Cobol.DivideRemainder d = divideRemainder;
        d = d.withName(visitAndCast(d.getName(), p));
        return d;
    }

    @Override
    public Cobol.Enable visitEnable(Cobol.Enable enable, P p) {
        Cobol.Enable e = enable;
        e = e.withCdName(visitAndCast(e.getCdName(), p));
        e = e.withKeyName(visitAndCast(e.getKeyName(), p));
        return e;
    }

    @Override
    public Cobol.EndKeyClause visitEndKeyClause(Cobol.EndKeyClause endKeyClause, P p) {
        Cobol.EndKeyClause e = endKeyClause;
        e = e.withName(visitAndCast(e.getName(), p));
        return e;
    }

    @Override
    public Cobol.EndProgram visitEndProgram(Cobol.EndProgram endProgram, P p) {
        Cobol.EndProgram e = endProgram;
        e = e.withProgramName(visitAndCast(e.getProgramName(), p));
        return e;
    }

    @Override
    public Cobol.Entry visitEntry(Cobol.Entry entry, P p) {
        Cobol.Entry e = entry;
        e = e.withLiteral(visitAndCast(e.getLiteral(), p));
        e = e.withIdentifiers(ListUtils.map(e.getIdentifiers(), it -> visitAndCast(it, p)));
        return e;
    }

    @Override
    public Cobol.EnvironmentDivision visitEnvironmentDivision(Cobol.EnvironmentDivision environmentDivision, P p) {
        Cobol.EnvironmentDivision e = environmentDivision;
        e = e.withBody(ListUtils.map(e.getBody(), it -> visitAndCast(it, p)));
        return e;
    }

    @Override
    public Cobol.EnvironmentSwitchNameClause visitEnvironmentSwitchNameClause(Cobol.EnvironmentSwitchNameClause environmentSwitchNameClause, P p) {
        Cobol.EnvironmentSwitchNameClause e = environmentSwitchNameClause;
        e = e.withEnvironmentName(visitAndCast(e.getEnvironmentName(), p));
        e = e.withMnemonicName(visitAndCast(e.getMnemonicName(), p));
        e = e.withEnvironmentSwitchNameSpecialNamesStatusPhrase(visitAndCast(e.getEnvironmentSwitchNameSpecialNamesStatusPhrase(), p));
        return e;
    }

    @Override
    public Cobol.EnvironmentSwitchNameSpecialNamesStatusPhrase visitEnvironmentSwitchNameSpecialNamesStatusPhrase(Cobol.EnvironmentSwitchNameSpecialNamesStatusPhrase environmentSwitchNameSpecialNamesStatusPhrase, P p) {
        Cobol.EnvironmentSwitchNameSpecialNamesStatusPhrase e = environmentSwitchNameSpecialNamesStatusPhrase;
        e = e.withCobols(ListUtils.map(e.getCobols(), it -> visitAndCast(it, p)));
        return e;
    }

    @Override
    public Cobol.ErrorKeyClause visitErrorKeyClause(Cobol.ErrorKeyClause errorKeyClause, P p) {
        Cobol.ErrorKeyClause e = errorKeyClause;
        e = e.withName(visitAndCast(e.getName(), p));
        return e;
    }

    @Override
    public Cobol.Evaluate visitEvaluate(Cobol.Evaluate evaluate, P p) {
        Cobol.Evaluate e = evaluate;
        e = e.withSelect(visitAndCast(e.getSelect(), p));
        e = e.withAlsoSelect(ListUtils.map(e.getAlsoSelect(), it -> visitAndCast(it, p)));
        e = e.withWhenPhrase(ListUtils.map(e.getWhenPhrase(), it -> visitAndCast(it, p)));
        e = e.withWhenOther(visitAndCast(e.getWhenOther(), p));
        return e;
    }

    @Override
    public Cobol.EvaluateAlso visitEvaluateAlso(Cobol.EvaluateAlso evaluateAlso, P p) {
        Cobol.EvaluateAlso e = evaluateAlso;
        e = e.withSelect(visitAndCast(e.getSelect(), p));
        return e;
    }

    @Override
    public Cobol.EvaluateAlsoCondition visitEvaluateAlsoCondition(Cobol.EvaluateAlsoCondition evaluateAlsoCondition, P p) {
        Cobol.EvaluateAlsoCondition e = evaluateAlsoCondition;
        e = e.withCondition(visitAndCast(e.getCondition(), p));
        return e;
    }

    @Override
    public Cobol.EvaluateCondition visitEvaluateCondition(Cobol.EvaluateCondition evaluateCondition, P p) {
        Cobol.EvaluateCondition e = evaluateCondition;
        e = e.withCondition(visitAndCast(e.getCondition(), p));
        e = e.withEvaluateThrough(visitAndCast(e.getEvaluateThrough(), p));
        return e;
    }

    @Override
    public Cobol.EvaluateThrough visitEvaluateThrough(Cobol.EvaluateThrough evaluateThrough, P p) {
        Cobol.EvaluateThrough e = evaluateThrough;
        e = e.withValue(visitAndCast(e.getValue(), p));
        return e;
    }

    @Override
    public Cobol.EvaluateWhen visitEvaluateWhen(Cobol.EvaluateWhen evaluateWhen, P p) {
        Cobol.EvaluateWhen e = evaluateWhen;
        e = e.withCondition(visitAndCast(e.getCondition(), p));
        e = e.withAlsoCondition(ListUtils.map(e.getAlsoCondition(), it -> visitAndCast(it, p)));
        return e;
    }

    @Override
    public Cobol.EvaluateWhenPhrase visitEvaluateWhenPhrase(Cobol.EvaluateWhenPhrase evaluateWhenPhrase, P p) {
        Cobol.EvaluateWhenPhrase e = evaluateWhenPhrase;
        e = e.withWhens(ListUtils.map(e.getWhens(), it -> visitAndCast(it, p)));
        e = e.withStatements(ListUtils.map(e.getStatements(), it -> visitAndCast(it, p)));
        return e;
    }

    @Override
    public Cobol.ExecCicsStatement visitExecCicsStatement(Cobol.ExecCicsStatement execCicsStatement, P p) {
        // TODO: https://github.com/moderneinc/rewrite-cobol/issues/70.
        // The grammar rule means that the ExecCicsStatement is a single token.
        return execCicsStatement;
    }

    @Override
    public Cobol.ExecSqlImsStatement visitExecSqlImsStatement(Cobol.ExecSqlImsStatement execSqlImsStatement, P p) {
        // TODO: https://github.com/moderneinc/rewrite-cobol/issues/70.
        // The grammar rule means that the ExecCicsStatement is a single token.
        return execSqlImsStatement;
    }

    @Override
    public Cobol.ExecSqlStatement visitExecSqlStatement(Cobol.ExecSqlStatement execSqlStatement, P p) {
        // TODO: https://github.com/moderneinc/rewrite-cobol/issues/70.
        // The grammar rule means that the ExecCicsStatement is a single token.
        return execSqlStatement;
    }

    @Override
    public Cobol.Exhibit visitExhibit(Cobol.Exhibit exhibit, P p) {
        Cobol.Exhibit e = exhibit;
        e = e.withOperands(ListUtils.map(e.getOperands(), it -> visitAndCast(it, p)));
        return e;
    }

    @Override
    public Cobol.Exit visitExit(Cobol.Exit exit, P p) {
        return exit;
    }

    @Override
    public Cobol.ExternalClause visitExternalClause(Cobol.ExternalClause externalClause, P p) {
        return externalClause;
    }

    @Override
    public Cobol.FigurativeConstant visitFigurativeConstant(Cobol.FigurativeConstant figurativeConstant, P p) {
        Cobol.FigurativeConstant f = figurativeConstant;
        f = f.withLiteral(visitAndCast(f.getLiteral(), p));
        return f;
    }

    @Override
    public Cobol.FileControlEntry visitFileControlEntry(Cobol.FileControlEntry fileControlEntry, P p) {
        Cobol.FileControlEntry f = fileControlEntry;
        f = f.withSelectClause(visitAndCast(f.getSelectClause(), p));
        f = f.withControlClauses(ListUtils.map(f.getControlClauses(), it -> visitAndCast(it, p)));
        return f;
    }

    @Override
    public Cobol.FileControlParagraph visitFileControlParagraph(Cobol.FileControlParagraph fileControlParagraph, P p) {
        Cobol.FileControlParagraph f = fileControlParagraph;
        f = f.withControlEntries(ListUtils.map(f.getControlEntries(), it -> visitAndCast(it, p)));
        return f;
    }

    @Override
    public Cobol.FileDescriptionEntry visitFileDescriptionEntry(Cobol.FileDescriptionEntry fileDescriptionEntry, P p) {
        Cobol.FileDescriptionEntry f = fileDescriptionEntry;
        f = f.withName(visitAndCast(f.getName(), p));
        f = f.withClauses(ListUtils.map(f.getClauses(), it -> visitAndCast(it, p)));
        f = f.withDataDescriptions(ListUtils.map(f.getDataDescriptions(), it -> visitAndCast(it, p)));
        return f;
    }

    @Override
    public Cobol.FileSection visitFileSection(Cobol.FileSection fileSection, P p) {
        Cobol.FileSection f = fileSection;
        f = f.withFileDescriptionEntry(ListUtils.map(f.getFileDescriptionEntry(), it -> visitAndCast(it, p)));
        return f;
    }

    @Override
    public Cobol.FileStatusClause visitFileStatusClause(Cobol.FileStatusClause fileStatusClause, P p) {
        Cobol.FileStatusClause f = fileStatusClause;
        f = f.withQualifiedDataNames(ListUtils.map(f.getQualifiedDataNames(), it -> visitAndCast(it, p)));
        return f;
    }

    @Override
    public Cobol.FunctionCall visitFunctionCall(Cobol.FunctionCall functionCall, P p) {
        Cobol.FunctionCall f = functionCall;
        f = f.withFunctionName(visitAndCast(f.getFunctionName(), p));
        f = f.withArguments(ListUtils.map(f.getArguments(), it -> visitAndCast(it, p)));
        f = f.withReferenceModifier(visitAndCast(f.getReferenceModifier(), p));
        return f;
    }

    @Override
    public Cobol.Generate visitGenerate(Cobol.Generate generate, P p) {
        Cobol.Generate g = generate;
        g = g.withReportName(visitAndCast(g.getReportName(), p));
        return g;
    }

    @Override
    public Cobol.GlobalClause visitGlobalClause(Cobol.GlobalClause globalClause, P p) {
        return globalClause;
    }

    @Override
    public Cobol.GoBack visitGoBack(Cobol.GoBack goBack, P p) {
        return goBack;
    }

    @Override
    public Cobol.GoTo visitGoTo(Cobol.GoTo _goTo, P p) {
        Cobol.GoTo g = _goTo;
        g = g.withStatement(visitAndCast(g.getStatement(), p));
        return g;
    }

    @Override
    public Cobol.GoToDependingOnStatement visitGoToDependingOnStatement(Cobol.GoToDependingOnStatement goToDependingOnStatement, P p) {
        Cobol.GoToDependingOnStatement g = goToDependingOnStatement;
        g = g.withProcedureNames(ListUtils.map(g.getProcedureNames(), it -> visitAndCast(it, p)));
        g = g.withIdentifier(visitAndCast(g.getIdentifier(), p));
        return g;
    }

    @Override
    public Cobol.IdentificationDivision visitIdentificationDivision(Cobol.IdentificationDivision identificationDivision, P p) {
        Cobol.IdentificationDivision i = identificationDivision;
        i = i.withProgramIdParagraph(visitAndCast(i.getProgramIdParagraph(), p));
        i = i.withParagraphs(ListUtils.map(i.getParagraphs(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.IdentificationDivisionParagraph visitIdentificationDivisionParagraph(Cobol.IdentificationDivisionParagraph identificationDivisionParagraph, P p) {
        Cobol.IdentificationDivisionParagraph i = identificationDivisionParagraph;
        i = i.withCommentEntry(visitAndCast(i.getCommentEntry(), p));
        return i;
    }

    @Override
    public Cobol.If visitIf(Cobol.If _if, P p) {
        Cobol.If i = _if;
        i = i.withCondition(visitAndCast(i.getCondition(), p));
        i = i.withIfThen(visitAndCast(i.getIfThen(), p));
        i = i.withIfElse(visitAndCast(i.getIfElse(), p));
        return i;
    }

    @Override
    public Cobol.IfElse visitIfElse(Cobol.IfElse ifElse, P p) {
        Cobol.IfElse i = ifElse;
        i = i.withStatements(ListUtils.map(i.getStatements(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.IfThen visitIfThen(Cobol.IfThen ifThen, P p) {
        Cobol.IfThen i = ifThen;
        i = i.withStatements(ListUtils.map(i.getStatements(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InData visitInData(Cobol.InData inData, P p) {
        Cobol.InData i = inData;
        i = i.withName(visitAndCast(i.getName(), p));
        return i;
    }

    @Override
    public Cobol.InFile visitInFile(Cobol.InFile inFile, P p) {
        Cobol.InFile i = inFile;
        i = i.withName(visitAndCast(i.getName(), p));
        return i;
    }

    @Override
    public Cobol.Initialize visitInitialize(Cobol.Initialize initialize, P p) {
        Cobol.Initialize i = initialize;
        i = i.withIdentifiers(ListUtils.map(i.getIdentifiers(), it -> visitAndCast(it, p)));
        i = i.withInitializeReplacingPhrase(visitAndCast(i.getInitializeReplacingPhrase(), p));
        return i;
    }

    @Override
    public Cobol.InitializeReplacingBy visitInitializeReplacingBy(Cobol.InitializeReplacingBy initializeReplacingBy, P p) {
        Cobol.InitializeReplacingBy i = initializeReplacingBy;
        i = i.withIdentifier(visitAndCast(i.getIdentifier(), p));
        return i;
    }

    @Override
    public Cobol.InitializeReplacingPhrase visitInitializeReplacingPhrase(Cobol.InitializeReplacingPhrase initializeReplacingPhrase, P p) {
        Cobol.InitializeReplacingPhrase i = initializeReplacingPhrase;
        i = i.withInitializeReplacingBy(ListUtils.map(i.getInitializeReplacingBy(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.Initiate visitInitiate(Cobol.Initiate initiate, P p) {
        Cobol.Initiate i = initiate;
        i = i.withReportNames(ListUtils.map(i.getReportNames(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InLibrary visitInLibrary(Cobol.InLibrary inLibrary, P p) {
        Cobol.InLibrary i = inLibrary;
        i = i.withWord(visitAndCast(i.getWord(), p));
        i = i.withName(visitAndCast(i.getName(), p));
        return i;
    }

    @Override
    public Cobol.InMnemonic visitInMnemonic(Cobol.InMnemonic inMnemonic, P p) {
        Cobol.InMnemonic i = inMnemonic;
        i = i.withName(visitAndCast(i.getName(), p));
        return i;
    }

    @Override
    public Cobol.InputOutputSection visitInputOutputSection(Cobol.InputOutputSection inputOutputSection, P p) {
        Cobol.InputOutputSection i = inputOutputSection;
        i = i.withParagraphs(ListUtils.map(i.getParagraphs(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InSection visitInSection(Cobol.InSection inSection, P p) {
        Cobol.InSection i = inSection;
        i = i.withName(visitAndCast(i.getName(), p));
        return i;
    }

    @Override
    public Cobol.Inspect visitInspect(Cobol.Inspect inspect, P p) {
        Cobol.Inspect i = inspect;
        i = i.withIdentifier(visitAndCast(i.getIdentifier(), p));
        i = i.withPhrase(visitAndCast(i.getPhrase(), p));
        return i;
    }

    @Override
    public Cobol.InspectAllLeading visitInspectAllLeading(Cobol.InspectAllLeading inspectAllLeading, P p) {
        Cobol.InspectAllLeading i = inspectAllLeading;
        i = i.withName(visitAndCast(i.getName(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectAllLeadings visitInspectAllLeadings(Cobol.InspectAllLeadings inspectAllLeadings, P p) {
        Cobol.InspectAllLeadings i = inspectAllLeadings;
        i = i.withLeadings(ListUtils.map(i.getLeadings(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectBeforeAfter visitInspectBeforeAfter(Cobol.InspectBeforeAfter inspectBeforeAfter, P p) {
        Cobol.InspectBeforeAfter i = inspectBeforeAfter;
        i = i.withIdentifier(visitAndCast(i.getIdentifier(), p));
        return i;
    }

    @Override
    public Cobol.InspectBy visitInspectBy(Cobol.InspectBy inspectBy, P p) {
        Cobol.InspectBy i = inspectBy;
        i = i.withIdentifier(visitAndCast(i.getIdentifier(), p));
        return i;
    }

    @Override
    public Cobol.InspectCharacters visitInspectCharacters(Cobol.InspectCharacters inspectCharacters, P p) {
        Cobol.InspectCharacters i = inspectCharacters;
        i = i.withInspections(ListUtils.map(i.getInspections(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectConvertingPhrase visitInspectConvertingPhrase(Cobol.InspectConvertingPhrase inspectConvertingPhrase, P p) {
        Cobol.InspectConvertingPhrase i = inspectConvertingPhrase;
        i = i.withIdentifier(visitAndCast(i.getIdentifier(), p));
        i = i.withInspectTo(visitAndCast(i.getInspectTo(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectFor visitInspectFor(Cobol.InspectFor inspectFor, P p) {
        Cobol.InspectFor i = inspectFor;
        i = i.withIdentifier(visitAndCast(i.getIdentifier(), p));
        i = i.withInspects(ListUtils.map(i.getInspects(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectReplacingAllLeading visitInspectReplacingAllLeading(Cobol.InspectReplacingAllLeading inspectReplacingAllLeading, P p) {
        Cobol.InspectReplacingAllLeading i = inspectReplacingAllLeading;
        i = i.withIdentifier(visitAndCast(i.getIdentifier(), p));
        i = i.withInspectBy(visitAndCast(i.getInspectBy(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectReplacingAllLeadings visitInspectReplacingAllLeadings(Cobol.InspectReplacingAllLeadings inspectReplacingAllLeadings, P p) {
        Cobol.InspectReplacingAllLeadings i = inspectReplacingAllLeadings;
        i = i.withInspections(ListUtils.map(i.getInspections(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectReplacingCharacters visitInspectReplacingCharacters(Cobol.InspectReplacingCharacters inspectReplacingCharacters, P p) {
        Cobol.InspectReplacingCharacters i = inspectReplacingCharacters;
        i = i.withInspectBy(visitAndCast(i.getInspectBy(), p));
        i = i.withInspections(ListUtils.map(i.getInspections(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectReplacingPhrase visitInspectReplacingPhrase(Cobol.InspectReplacingPhrase inspectReplacingPhrase, P p) {
        Cobol.InspectReplacingPhrase i = inspectReplacingPhrase;
        i = i.withInspections(ListUtils.map(i.getInspections(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectTallyingPhrase visitInspectTallyingPhrase(Cobol.InspectTallyingPhrase inspectTallyingPhrase, P p) {
        Cobol.InspectTallyingPhrase i = inspectTallyingPhrase;
        i = i.withInspectFors(ListUtils.map(i.getInspectFors(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectTallyingReplacingPhrase visitInspectTallyingReplacingPhrase(Cobol.InspectTallyingReplacingPhrase inspectTallyingReplacingPhrase, P p) {
        Cobol.InspectTallyingReplacingPhrase i = inspectTallyingReplacingPhrase;
        i = i.withInspectFors(ListUtils.map(i.getInspectFors(), it -> visitAndCast(it, p)));
        i = i.withReplacingPhrases(ListUtils.map(i.getReplacingPhrases(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.InspectTo visitInspectTo(Cobol.InspectTo inspectTo, P p) {
        Cobol.InspectTo i = inspectTo;
        i = i.withIdentifier(visitAndCast(i.getIdentifier(), p));
        return i;
    }

    @Override
    public Cobol.InTable visitInTable(Cobol.InTable inTable, P p) {
        Cobol.InTable i = inTable;
        i = i.withTableCall(visitAndCast(i.getTableCall(), p));
        return i;
    }

    @Override
    public Cobol.IoControlParagraph visitIoControlParagraph(Cobol.IoControlParagraph ioControlParagraph, P p) {
        Cobol.IoControlParagraph i = ioControlParagraph;
        i = i.withFileName(visitAndCast(i.getFileName(), p));
        i = i.withClauses(ListUtils.map(i.getClauses(), it -> visitAndCast(it, p)));
        return i;
    }

    @Override
    public Cobol.LabelRecordsClause visitLabelRecordsClause(Cobol.LabelRecordsClause labelRecordsClause, P p) {
        Cobol.LabelRecordsClause l = labelRecordsClause;
        l = l.withDataNames(ListUtils.map(l.getDataNames(), it -> visitAndCast(it, p)));
        return l;
    }

    @Override
    public Cobol.LibraryAttributeClauseFormat1 visitLibraryAttributeClauseFormat1(Cobol.LibraryAttributeClauseFormat1 libraryAttributeClauseFormat1, P p) {
        return libraryAttributeClauseFormat1;
    }

    @Override
    public Cobol.LibraryAttributeClauseFormat2 visitLibraryAttributeClauseFormat2(Cobol.LibraryAttributeClauseFormat2 libraryAttributeClauseFormat2, P p) {
        Cobol.LibraryAttributeClauseFormat2 l = libraryAttributeClauseFormat2;
        l = l.withLibraryAttributeFunction(visitAndCast(l.getLibraryAttributeFunction(), p));
        l = l.withLibraryAttributeParameter(visitAndCast(l.getLibraryAttributeParameter(), p));
        l = l.withLibraryAttributeTitle(visitAndCast(l.getLibraryAttributeTitle(), p));
        return l;
    }

    @Override
    public Cobol.LibraryAttributeFunction visitLibraryAttributeFunction(Cobol.LibraryAttributeFunction libraryAttributeFunction, P p) {
        Cobol.LibraryAttributeFunction l = libraryAttributeFunction;
        l = l.withLiteral(visitAndCast(l.getLiteral(), p));
        return l;
    }

    @Override
    public Cobol.LibraryAttributeParameter visitLibraryAttributeParameter(Cobol.LibraryAttributeParameter libraryAttributeParameter, P p) {
        Cobol.LibraryAttributeParameter l = libraryAttributeParameter;
        l = l.withLiteral(visitAndCast(l.getLiteral(), p));
        return l;
    }

    @Override
    public Cobol.LibraryAttributeTitle visitLibraryAttributeTitle(Cobol.LibraryAttributeTitle libraryAttributeTitle, P p) {
        Cobol.LibraryAttributeTitle l = libraryAttributeTitle;
        l = l.withLiteral(visitAndCast(l.getLiteral(), p));
        return l;
    }

    @Override
    public Cobol.LibraryDescriptionEntryFormat1 visitLibraryDescriptionEntryFormat1(Cobol.LibraryDescriptionEntryFormat1 libraryDescriptionEntryFormat1, P p) {
        Cobol.LibraryDescriptionEntryFormat1 l = libraryDescriptionEntryFormat1;
        l = l.withLibraryName(visitAndCast(l.getLibraryName(), p));
        l = l.withLibraryAttributeClauseFormat1(visitAndCast(l.getLibraryAttributeClauseFormat1(), p));
        l = l.withLibraryEntryProcedureClauseFormat1(visitAndCast(l.getLibraryEntryProcedureClauseFormat1(), p));
        return l;
    }

    @Override
    public Cobol.LibraryDescriptionEntryFormat2 visitLibraryDescriptionEntryFormat2(Cobol.LibraryDescriptionEntryFormat2 libraryDescriptionEntryFormat2, P p) {
        Cobol.LibraryDescriptionEntryFormat2 l = libraryDescriptionEntryFormat2;
        l = l.withLibraryName(visitAndCast(l.getLibraryName(), p));
        l = l.withLibraryIsGlobalClause(visitAndCast(l.getLibraryIsGlobalClause(), p));
        l = l.withLibraryIsCommonClause(visitAndCast(l.getLibraryIsCommonClause(), p));
        l = l.withClauseFormats(ListUtils.map(l.getClauseFormats(), it -> visitAndCast(it, p)));
        return l;
    }

    @Override
    public Cobol.LibraryEntryProcedureClauseFormat1 visitLibraryEntryProcedureClauseFormat1(Cobol.LibraryEntryProcedureClauseFormat1 libraryEntryProcedureClauseFormat1, P p) {
        Cobol.LibraryEntryProcedureClauseFormat1 l = libraryEntryProcedureClauseFormat1;
        l = l.withProgramName(visitAndCast(l.getProgramName(), p));
        l = l.withLibraryEntryProcedureForClause(visitAndCast(l.getLibraryEntryProcedureForClause(), p));
        return l;
    }

    @Override
    public Cobol.LibraryEntryProcedureClauseFormat2 visitLibraryEntryProcedureClauseFormat2(Cobol.LibraryEntryProcedureClauseFormat2 libraryEntryProcedureClauseFormat2, P p) {
        Cobol.LibraryEntryProcedureClauseFormat2 l = libraryEntryProcedureClauseFormat2;
        l = l.withProgramName(visitAndCast(l.getProgramName(), p));
        l = l.withLibraryEntryProcedureForClause(visitAndCast(l.getLibraryEntryProcedureForClause(), p));
        l = l.withLibraryEntryProcedureWithClause(visitAndCast(l.getLibraryEntryProcedureWithClause(), p));
        l = l.withLibraryEntryProcedureUsingClause(visitAndCast(l.getLibraryEntryProcedureUsingClause(), p));
        l = l.withLibraryEntryProcedureGivingClause(visitAndCast(l.getLibraryEntryProcedureGivingClause(), p));
        return l;
    }

    @Override
    public Cobol.LibraryEntryProcedureForClause visitLibraryEntryProcedureForClause(Cobol.LibraryEntryProcedureForClause libraryEntryProcedureForClause, P p) {
        Cobol.LibraryEntryProcedureForClause l = libraryEntryProcedureForClause;
        l = l.withLiteral(visitAndCast(l.getLiteral(), p));
        return l;
    }

    @Override
    public Cobol.LibraryEntryProcedureGivingClause visitLibraryEntryProcedureGivingClause(Cobol.LibraryEntryProcedureGivingClause libraryEntryProcedureGivingClause, P p) {
        Cobol.LibraryEntryProcedureGivingClause l = libraryEntryProcedureGivingClause;
        l = l.withDataName(visitAndCast(l.getDataName(), p));
        return l;
    }

    @Override
    public Cobol.LibraryEntryProcedureUsingClause visitLibraryEntryProcedureUsingClause(Cobol.LibraryEntryProcedureUsingClause libraryEntryProcedureUsingClause, P p) {
        Cobol.LibraryEntryProcedureUsingClause l = libraryEntryProcedureUsingClause;
        l = l.withNames(ListUtils.map(l.getNames(), it -> visitAndCast(it, p)));
        return l;
    }

    @Override
    public Cobol.LibraryEntryProcedureWithClause visitLibraryEntryProcedureWithClause(Cobol.LibraryEntryProcedureWithClause libraryEntryProcedureWithClause, P p) {
        Cobol.LibraryEntryProcedureWithClause l = libraryEntryProcedureWithClause;
        l = l.withNames(ListUtils.map(l.getNames(), it -> visitAndCast(it, p)));
        return l;
    }

    @Override
    public Cobol.LibraryIsCommonClause visitLibraryIsCommonClause(Cobol.LibraryIsCommonClause libraryIsCommonClause, P p) {
        return libraryIsCommonClause;
    }

    @Override
    public Cobol.LibraryIsGlobalClause visitLibraryIsGlobalClause(Cobol.LibraryIsGlobalClause libraryIsGlobalClause, P p) {
        return libraryIsGlobalClause;
    }

    @Override
    public Cobol.LinageClause visitLinageClause(Cobol.LinageClause linageClause, P p) {
        Cobol.LinageClause l = linageClause;
        l = l.withName(visitAndCast(l.getName(), p));
        l = l.withLinageAt(ListUtils.map(l.getLinageAt(), it -> visitAndCast(it, p)));
        return l;
    }

    @Override
    public Cobol.LinageFootingAt visitLinageFootingAt(Cobol.LinageFootingAt linageFootingAt, P p) {
        Cobol.LinageFootingAt l = linageFootingAt;
        l = l.withName(visitAndCast(l.getName(), p));
        return l;
    }

    @Override
    public Cobol.LinageLinesAtBottom visitLinageLinesAtBottom(Cobol.LinageLinesAtBottom linageLinesAtBottom, P p) {
        Cobol.LinageLinesAtBottom l = linageLinesAtBottom;
        l = l.withName(visitAndCast(l.getName(), p));
        return l;
    }

    @Override
    public Cobol.LinageLinesAtTop visitLinageLinesAtTop(Cobol.LinageLinesAtTop linageLinesAtTop, P p) {
        Cobol.LinageLinesAtTop l = linageLinesAtTop;
        l = l.withName(visitAndCast(l.getName(), p));
        return l;
    }

    @Override
    public Cobol.LinkageSection visitLinkageSection(Cobol.LinkageSection linkageSection, P p) {
        Cobol.LinkageSection l = linkageSection;
        l = l.withDataDescriptions(ListUtils.map(l.getDataDescriptions(), it -> visitAndCast(it, p)));
        return l;
    }

    @Override
    public Cobol.LocalStorageSection visitLocalStorageSection(Cobol.LocalStorageSection localStorageSection, P p) {
        Cobol.LocalStorageSection l = localStorageSection;
        l = l.withLocalName(visitAndCast(l.getLocalName(), p));
        l = l.withDataDescriptions(ListUtils.map(l.getDataDescriptions(), it -> visitAndCast(it, p)));
        return l;
    }

    @Override
    public Cobol.Merge visitMerge(Cobol.Merge merge, P p) {
        Cobol.Merge m = merge;
        m = m.withFileName(visitAndCast(m.getFileName(), p));
        m = m.withMergeOnKeyClause(ListUtils.map(m.getMergeOnKeyClause(), it -> visitAndCast(it, p)));
        m = m.withMergeCollatingSequencePhrase(visitAndCast(m.getMergeCollatingSequencePhrase(), p));
        m = m.withMergeUsing(ListUtils.map(m.getMergeUsing(), it -> visitAndCast(it, p)));
        m = m.withMergeOutputProcedurePhrase(visitAndCast(m.getMergeOutputProcedurePhrase(), p));
        m = m.withMergeGivingPhrase(ListUtils.map(m.getMergeGivingPhrase(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.Mergeable visitMergeable(Cobol.Mergeable mergeable, P p) {
        Cobol.Mergeable m = mergeable;
        m = m.withName(visitAndCast(m.getName(), p));
        return m;
    }

    @Override
    public Cobol.MergeCollatingSequencePhrase visitMergeCollatingSequencePhrase(Cobol.MergeCollatingSequencePhrase mergeCollatingSequencePhrase, P p) {
        Cobol.MergeCollatingSequencePhrase m = mergeCollatingSequencePhrase;
        m = m.withName(ListUtils.map(m.getName(), it -> visitAndCast(it, p)));
        m = m.withMergeCollatingAlphanumeric(visitAndCast(m.getMergeCollatingAlphanumeric(), p));
        m = m.withMergeCollatingNational(visitAndCast(m.getMergeCollatingNational(), p));
        return m;
    }

    @Override
    public Cobol.MergeGiving visitMergeGiving(Cobol.MergeGiving mergeGiving, P p) {
        Cobol.MergeGiving m = mergeGiving;
        m = m.withName(visitAndCast(m.getName(), p));
        return m;
    }

    @Override
    public Cobol.MergeGivingPhrase visitMergeGivingPhrase(Cobol.MergeGivingPhrase mergeGivingPhrase, P p) {
        Cobol.MergeGivingPhrase m = mergeGivingPhrase;
        m = m.withMergeGiving(ListUtils.map(m.getMergeGiving(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.MergeOnKeyClause visitMergeOnKeyClause(Cobol.MergeOnKeyClause mergeOnKeyClause, P p) {
        Cobol.MergeOnKeyClause m = mergeOnKeyClause;
        m = m.withQualifiedDataName(ListUtils.map(m.getQualifiedDataName(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.MergeOutputProcedurePhrase visitMergeOutputProcedurePhrase(Cobol.MergeOutputProcedurePhrase mergeOutputProcedurePhrase, P p) {
        Cobol.MergeOutputProcedurePhrase m = mergeOutputProcedurePhrase;
        m = m.withProcedureName(visitAndCast(m.getProcedureName(), p));
        m = m.withMergeOutputThrough(visitAndCast(m.getMergeOutputThrough(), p));
        return m;
    }

    @Override
    public Cobol.MergeOutputThrough visitMergeOutputThrough(Cobol.MergeOutputThrough mergeOutputThrough, P p) {
        Cobol.MergeOutputThrough m = mergeOutputThrough;
        m = m.withProcedureName(visitAndCast(m.getProcedureName(), p));
        return m;
    }

    @Override
    public Cobol.MergeUsing visitMergeUsing(Cobol.MergeUsing mergeUsing, P p) {
        Cobol.MergeUsing m = mergeUsing;
        m = m.withFileNames(ListUtils.map(m.getFileNames(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.MessageCountClause visitMessageCountClause(Cobol.MessageCountClause messageCountClause, P p) {
        Cobol.MessageCountClause m = messageCountClause;
        m = m.withDataDescName(visitAndCast(m.getDataDescName(), p));
        return m;
    }

    @Override
    public Cobol.MessageDateClause visitMessageDateClause(Cobol.MessageDateClause messageDateClause, P p) {
        Cobol.MessageDateClause m = messageDateClause;
        m = m.withDataDescName(visitAndCast(m.getDataDescName(), p));
        return m;
    }

    @Override
    public Cobol.MessageTimeClause visitMessageTimeClause(Cobol.MessageTimeClause messageTimeClause, P p) {
        Cobol.MessageTimeClause m = messageTimeClause;
        m = m.withDataDescName(visitAndCast(m.getDataDescName(), p));
        return m;
    }

    @Override
    public Cobol.MoveCorrespondingToStatement visitMoveCorrespondingToStatement(Cobol.MoveCorrespondingToStatement moveCorrespondingToStatement, P p) {
        Cobol.MoveCorrespondingToStatement m = moveCorrespondingToStatement;
        m = m.withMoveCorrespondingToSendingArea(visitAndCast(m.getMoveCorrespondingToSendingArea(), p));
        m = m.withIdentifiers(ListUtils.map(m.getIdentifiers(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.MoveStatement visitMoveStatement(Cobol.MoveStatement moveStatement, P p) {
        Cobol.MoveStatement m = moveStatement;
        m = m.withMoveToStatement(visitAndCast(m.getMoveToStatement(), p));
        return m;
    }

    @Override
    public Cobol.MoveToStatement visitMoveToStatement(Cobol.MoveToStatement moveToStatement, P p) {
        Cobol.MoveToStatement m = moveToStatement;
        m = m.withFrom(visitAndCast(m.getFrom(), p));
        m = m.withNames(ListUtils.map(m.getNames(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.MultDiv visitMultDiv(Cobol.MultDiv multDiv, P p) {
        Cobol.MultDiv m = multDiv;
        m = m.withPowers(visitAndCast(m.getPowers(), p));
        return m;
    }

    @Override
    public Cobol.MultDivs visitMultDivs(Cobol.MultDivs multDivs, P p) {
        Cobol.MultDivs m = multDivs;
        m = m.withPowers(visitAndCast(m.getPowers(), p));
        m = m.withMultDivs(ListUtils.map(m.getMultDivs(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.MultipleFileClause visitMultipleFileClause(Cobol.MultipleFileClause multipleFileClause, P p) {
        Cobol.MultipleFileClause m = multipleFileClause;
        m = m.withFilePositions(ListUtils.map(m.getFilePositions(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.MultipleFilePosition visitMultipleFilePosition(Cobol.MultipleFilePosition multipleFilePosition, P p) {
        Cobol.MultipleFilePosition m = multipleFilePosition;
        m = m.withFileName(visitAndCast(m.getFileName(), p));
        m = m.withIntegerLiteral(visitAndCast(m.getIntegerLiteral(), p));
        return m;
    }

    @Override
    public Cobol.Multiply visitMultiply(Cobol.Multiply multiply, P p) {
        Cobol.Multiply m = multiply;
        m = m.withMultiplicand(visitAndCast(m.getMultiplicand(), p));
        m = m.withMultiply(visitAndCast(m.getMultiply(), p));
        m = m.withOnSizeErrorPhrase(visitAndCast(m.getOnSizeErrorPhrase(), p));
        m = m.withNotOnSizeErrorPhrase(visitAndCast(m.getNotOnSizeErrorPhrase(), p));
        return m;
    }

    @Override
    public Cobol.MultiplyGiving visitMultiplyGiving(Cobol.MultiplyGiving multiplyGiving, P p) {
        Cobol.MultiplyGiving m = multiplyGiving;
        m = m.withOperand(visitAndCast(m.getOperand(), p));
        m = m.withResult(ListUtils.map(m.getResult(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.MultiplyRegular visitMultiplyRegular(Cobol.MultiplyRegular multiplyRegular, P p) {
        Cobol.MultiplyRegular m = multiplyRegular;
        m = m.withOperand(ListUtils.map(m.getOperand(), it -> visitAndCast(it, p)));
        return m;
    }

    @Override
    public Cobol.NextSentence visitNextSentence(Cobol.NextSentence nextSentence, P p) {
        Cobol.NextSentence n = nextSentence;
        n = n.withWords(ListUtils.map(n.getWords(), it -> visitAndCast(it, p)));
        return n;
    }

    @Override
    public Cobol.ObjectComputer visitObjectComputer(Cobol.ObjectComputer objectComputer, P p) {
        Cobol.ObjectComputer o = objectComputer;
        o = o.withComputer(visitAndCast(o.getComputer(), p));
        return o;
    }

    @Override
    public Cobol.ObjectComputerDefinition visitObjectComputerDefinition(Cobol.ObjectComputerDefinition objectComputerDefinition, P p) {
        Cobol.ObjectComputerDefinition o = objectComputerDefinition;
        o = o.withComputerName(visitAndCast(o.getComputerName(), p));
        o = o.withSpecifications(ListUtils.map(o.getSpecifications(), it -> visitAndCast(it, p)));
        return o;
    }

    @Override
    public Cobol.OdtClause visitOdtClause(Cobol.OdtClause odtClause, P p) {
        Cobol.OdtClause o = odtClause;
        o = o.withMnemonicName(visitAndCast(o.getMnemonicName(), p));
        return o;
    }

    @Override
    public Cobol.Open visitOpen(Cobol.Open open, P p) {
        Cobol.Open o = open;
        o = o.withOpen(ListUtils.map(o.getOpen(), it -> visitAndCast(it, p)));
        return o;
    }

    @Override
    public Cobol.Openable visitOpenable(Cobol.Openable openable, P p) {
        Cobol.Openable o = openable;
        o = o.withFileName(visitAndCast(o.getFileName(), p));
        o = o.withFileName(visitAndCast(o.getFileName(), p));
        return o;
    }

    @Override
    public Cobol.OpenInputOutputStatement visitOpenInputOutputStatement(Cobol.OpenInputOutputStatement openInputOutputStatement, P p) {
        Cobol.OpenInputOutputStatement o = openInputOutputStatement;
        o = o.withOpenInput(ListUtils.map(o.getOpenInput(), it -> visitAndCast(it, p)));
        return o;
    }

    @Override
    public Cobol.OpenIOExtendStatement visitOpenIOExtendStatement(Cobol.OpenIOExtendStatement openIOExtendStatement, P p) {
        Cobol.OpenIOExtendStatement o = openIOExtendStatement;
        o = o.withFileNames(ListUtils.map(o.getFileNames(), it -> visitAndCast(it, p)));
        return o;
    }

    @Override
    public Cobol.OrganizationClause visitOrganizationClause(Cobol.OrganizationClause organizationClause, P p) {
        return organizationClause;
    }

    @Override
    public Cobol.PaddingCharacterClause visitPaddingCharacterClause(Cobol.PaddingCharacterClause paddingCharacterClause, P p) {
        Cobol.PaddingCharacterClause pp = paddingCharacterClause;
        pp = pp.withName(visitAndCast(pp.getName(), p));
        return pp;
    }

    @Override
    public Cobol.Paragraph visitParagraph(Cobol.Paragraph paragraph, P p) {
        Cobol.Paragraph pp = paragraph;
        pp = pp.withParagraphName(visitAndCast(pp.getParagraphName(), p));
        pp = pp.withAlteredGoTo(visitAndCast(pp.getAlteredGoTo(), p));
        pp = pp.withSentences(ListUtils.map(pp.getSentences(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.Paragraphs visitParagraphs(Cobol.Paragraphs paragraphs, P p) {
        Cobol.Paragraphs pp = paragraphs;
        pp = pp.withSentences(ListUtils.map(pp.getSentences(), it -> visitAndCast(it, p)));
        pp = pp.withParagraphs(ListUtils.map(pp.getParagraphs(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.Parenthesized visitParenthesized(Cobol.Parenthesized parenthesized, P p) {
        Cobol.Parenthesized pp = parenthesized;
        pp = pp.withContents(ListUtils.map(pp.getContents(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.PasswordClause visitPasswordClause(Cobol.PasswordClause passwordClause, P p) {
        Cobol.PasswordClause pp = passwordClause;
        pp = pp.withDataName(visitAndCast(pp.getDataName(), p));
        return pp;
    }

    @Override
    public Cobol.Perform visitPerform(Cobol.Perform perform, P p) {
        Cobol.Perform pp = perform;
        pp = pp.withStatement(visitAndCast(pp.getStatement(), p));
        return pp;
    }

    @Override
    public Cobol.Performable visitPerformable(Cobol.Performable performable, P p) {
        Cobol.Performable pp = performable;
        pp = pp.withExpression(visitAndCast(pp.getExpression(), p));
        return pp;
    }

    @Override
    public Cobol.PerformInlineStatement visitPerformInlineStatement(Cobol.PerformInlineStatement performInlineStatement, P p) {
        Cobol.PerformInlineStatement pp = performInlineStatement;
        pp = pp.withPerformType(visitAndCast(pp.getPerformType(), p));
        pp = pp.withStatements(ListUtils.map(pp.getStatements(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.PerformProcedureStatement visitPerformProcedureStatement(Cobol.PerformProcedureStatement performProcedureStatement, P p) {
        Cobol.PerformProcedureStatement pp = performProcedureStatement;
        pp = pp.withProcedureName(visitAndCast(pp.getProcedureName(), p));
        pp = pp.withThroughProcedure(visitAndCast(pp.getThroughProcedure(), p));
        pp = pp.withPerformType(visitAndCast(pp.getPerformType(), p));
        return pp;
    }

    @Override
    public Cobol.PerformTestClause visitPerformTestClause(Cobol.PerformTestClause performTestClause, P p) {
        return performTestClause;
    }

    @Override
    public Cobol.PerformTimes visitPerformTimes(Cobol.PerformTimes performTimes, P p) {
        Cobol.PerformTimes pp = performTimes;
        pp = pp.withValue(visitAndCast(pp.getValue(), p));
        return pp;
    }

    @Override
    public Cobol.PerformUntil visitPerformUntil(Cobol.PerformUntil performUntil, P p) {
        Cobol.PerformUntil pp = performUntil;
        pp = pp.withPerformTestClause(visitAndCast(pp.getPerformTestClause(), p));
        pp = pp.withCondition(visitAndCast(pp.getCondition(), p));
        return pp;
    }

    @Override
    public Cobol.PerformVarying visitPerformVarying(Cobol.PerformVarying performVarying, P p) {
        Cobol.PerformVarying pp = performVarying;
        pp = pp.withCobols(ListUtils.map(pp.getCobols(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.PerformVaryingClause visitPerformVaryingClause(Cobol.PerformVaryingClause performVaryingClause, P p) {
        Cobol.PerformVaryingClause pp = performVaryingClause;
        pp = pp.withPerformVaryingPhrase(visitAndCast(pp.getPerformVaryingPhrase(), p));
        pp = pp.withPerformAfter(ListUtils.map(pp.getPerformAfter(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.PerformVaryingPhrase visitPerformVaryingPhrase(Cobol.PerformVaryingPhrase performVaryingPhrase, P p) {
        Cobol.PerformVaryingPhrase pp = performVaryingPhrase;
        pp = pp.withName(visitAndCast(pp.getName(), p));
        pp = pp.withFrom(visitAndCast(pp.getFrom(), p));
        pp = pp.withBy(visitAndCast(pp.getBy(), p));
        pp = pp.withUntil(visitAndCast(pp.getUntil(), p));
        return pp;
    }

    @Override
    public Cobol.Picture visitPicture(Cobol.Picture picture, P p) {
        Cobol.Picture pp = picture;
        pp = pp.withWords(ListUtils.map(pp.getWords(), it -> visitAndCast(it, p)));
        pp = pp.withParenthesized(visitAndCast(pp.getParenthesized(), p));
        return pp;
    }

    @Override
    public Cobol.PictureString visitPictureString(Cobol.PictureString pictureString, P p) {
        Cobol.PictureString pp = pictureString;
        pp = pp.withPictures(ListUtils.map(pp.getPictures(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.PlusMinus visitPlusMinus(Cobol.PlusMinus plusMinus, P p) {
        Cobol.PlusMinus pp = plusMinus;
        pp = pp.withMultDivs(visitAndCast(pp.getMultDivs(), p));
        return pp;
    }

    @Override
    public Cobol.Power visitPower(Cobol.Power power, P p) {
        Cobol.Power pp = power;
        pp = pp.withExpression(visitAndCast(pp.getExpression(), p));
        return pp;
    }

    @Override
    public Cobol.Powers visitPowers(Cobol.Powers powers, P p) {
        Cobol.Powers pp = powers;
        pp = pp.withExpression(visitAndCast(pp.getExpression(), p));
        pp = pp.withPowers(ListUtils.map(pp.getPowers(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.ProcedureDeclarative visitProcedureDeclarative(Cobol.ProcedureDeclarative procedureDeclarative, P p) {
        Cobol.ProcedureDeclarative pp = procedureDeclarative;
        pp = pp.withProcedureSectionHeader(visitAndCast(pp.getProcedureSectionHeader(), p));
        pp = pp.withUseStatement(visitAndCast(pp.getUseStatement(), p));
        pp = pp.withParagraphs(visitAndCast(pp.getParagraphs(), p));
        return pp;
    }

    @Override
    public Cobol.ProcedureDeclaratives visitProcedureDeclaratives(Cobol.ProcedureDeclaratives procedureDeclaratives, P p) {
        Cobol.ProcedureDeclaratives pp = procedureDeclaratives;
        pp = pp.withProcedureDeclarative(ListUtils.map(pp.getProcedureDeclarative(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.ProcedureDivision visitProcedureDivision(Cobol.ProcedureDivision procedureDivision, P p) {
        Cobol.ProcedureDivision pp = procedureDivision;
        pp = pp.withProcedureDivisionUsingClause(visitAndCast(pp.getProcedureDivisionUsingClause(), p));
        pp = pp.withProcedureDivisionGivingClause(visitAndCast(pp.getProcedureDivisionGivingClause(), p));
        pp = pp.withProcedureDeclaratives(visitAndCast(pp.getProcedureDeclaratives(), p));
        pp = pp.withBody(visitAndCast(pp.getBody(), p));
        return pp;
    }

    @Override
    public Cobol.ProcedureDivisionBody visitProcedureDivisionBody(Cobol.ProcedureDivisionBody procedureDivisionBody, P p) {
        Cobol.ProcedureDivisionBody pp = procedureDivisionBody;
        pp = pp.withParagraphs(visitAndCast(pp.getParagraphs(), p));
        pp = pp.withProcedureSection(ListUtils.map(pp.getProcedureSection(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.ProcedureDivisionByReference visitProcedureDivisionByReference(Cobol.ProcedureDivisionByReference procedureDivisionByReference, P p) {
        Cobol.ProcedureDivisionByReference pp = procedureDivisionByReference;
        pp = pp.withReference(visitAndCast(pp.getReference(), p));
        return pp;
    }

    @Override
    public Cobol.ProcedureDivisionByReferencePhrase visitProcedureDivisionByReferencePhrase(Cobol.ProcedureDivisionByReferencePhrase procedureDivisionByReferencePhrase, P p) {
        Cobol.ProcedureDivisionByReferencePhrase pp = procedureDivisionByReferencePhrase;
        pp = pp.withProcedureDivisionByReference(ListUtils.map(pp.getProcedureDivisionByReference(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.ProcedureDivisionByValuePhrase visitProcedureDivisionByValuePhrase(Cobol.ProcedureDivisionByValuePhrase procedureDivisionByValuePhrase, P p) {
        Cobol.ProcedureDivisionByValuePhrase pp = procedureDivisionByValuePhrase;
        pp = pp.withPhrases(ListUtils.map(pp.getPhrases(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.ProcedureDivisionGivingClause visitProcedureDivisionGivingClause(Cobol.ProcedureDivisionGivingClause procedureDivisionGivingClause, P p) {
        Cobol.ProcedureDivisionGivingClause pp = procedureDivisionGivingClause;
        pp = pp.withDataName(visitAndCast(pp.getDataName(), p));
        return pp;
    }

    @Override
    public Cobol.ProcedureDivisionUsingClause visitProcedureDivisionUsingClause(Cobol.ProcedureDivisionUsingClause procedureDivisionUsingClause, P p) {
        Cobol.ProcedureDivisionUsingClause pp = procedureDivisionUsingClause;
        pp = pp.withProcedureDivisionUsingParameter(ListUtils.map(pp.getProcedureDivisionUsingParameter(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.ProcedureName visitProcedureName(Cobol.ProcedureName procedureName, P p) {
        Cobol.ProcedureName pp = procedureName;
        pp = pp.withParagraphName(visitAndCast(pp.getParagraphName(), p));
        pp = pp.withInSection(visitAndCast(pp.getInSection(), p));
        pp = pp.withSectionName(visitAndCast(pp.getSectionName(), p));
        return pp;
    }

    @Override
    public Cobol.ProcedureSection visitProcedureSection(Cobol.ProcedureSection procedureSection, P p) {
        Cobol.ProcedureSection pp = procedureSection;
        pp = pp.withProcedureSectionHeader(visitAndCast(pp.getProcedureSectionHeader(), p));
        pp = pp.withParagraphs(visitAndCast(pp.getParagraphs(), p));
        return pp;
    }

    @Override
    public Cobol.ProcedureSectionHeader visitProcedureSectionHeader(Cobol.ProcedureSectionHeader procedureSectionHeader, P p) {
        Cobol.ProcedureSectionHeader pp = procedureSectionHeader;
        pp = pp.withSectionName(visitAndCast(pp.getSectionName(), p));
        pp = pp.withIdentifier(visitAndCast(pp.getIdentifier(), p));
        return pp;
    }

    @Override
    public Cobol.ProgramIdParagraph visitProgramIdParagraph(Cobol.ProgramIdParagraph programIdParagraph, P p) {
        Cobol.ProgramIdParagraph pp = programIdParagraph;
        pp = pp.withProgramName(visitAndCast(pp.getProgramName(), p));
        pp = pp.withCommentEntry(visitAndCast(pp.getCommentEntry(), p));
        return pp;
    }

    @Override
    public Cobol.ProgramLibrarySection visitProgramLibrarySection(Cobol.ProgramLibrarySection programLibrarySection, P p) {
        Cobol.ProgramLibrarySection pp = programLibrarySection;
        pp = pp.withLibraryDescriptionEntries(ListUtils.map(pp.getLibraryDescriptionEntries(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.ProgramUnit visitProgramUnit(Cobol.ProgramUnit programUnit, P p) {
        Cobol.ProgramUnit pp = programUnit;
        pp = pp.withIdentificationDivision(visitAndCast(pp.getIdentificationDivision(), p));
        pp = pp.withEnvironmentDivision(visitAndCast(pp.getEnvironmentDivision(), p));
        pp = pp.withDataDivision(visitAndCast(pp.getDataDivision(), p));
        pp = pp.withProcedureDivision(visitAndCast(pp.getProcedureDivision(), p));
        pp = pp.withProgramUnits(ListUtils.map(pp.getProgramUnits(), it -> visitAndCast(it, p)));
        pp = pp.withEndProgram(visitAndCast(pp.getEndProgram(), p));
        return pp;
    }

    @Override
    public Cobol.Purge visitPurge(Cobol.Purge purge, P p) {
        Cobol.Purge pp = purge;
        pp = pp.withNames(ListUtils.map(pp.getNames(), it -> visitAndCast(it, p)));
        return pp;
    }

    @Override
    public Cobol.QualifiedDataName visitQualifiedDataName(Cobol.QualifiedDataName qualifiedDataName, P p) {
        Cobol.QualifiedDataName q = qualifiedDataName;
        q = q.withDataName(visitAndCast(q.getDataName(), p));
        return q;
    }

    @Override
    public Cobol.QualifiedDataNameFormat1 visitQualifiedDataNameFormat1(Cobol.QualifiedDataNameFormat1 qualifiedDataNameFormat1, P p) {
        Cobol.QualifiedDataNameFormat1 q = qualifiedDataNameFormat1;
        q = q.withName(visitAndCast(q.getName(), p));
        q = q.withQualifiedInData(ListUtils.map(q.getQualifiedInData(), it -> visitAndCast(it, p)));
        q = q.withInFile(visitAndCast(q.getInFile(), p));
        return q;
    }

    @Override
    public Cobol.QualifiedDataNameFormat2 visitQualifiedDataNameFormat2(Cobol.QualifiedDataNameFormat2 qualifiedDataNameFormat2, P p) {
        Cobol.QualifiedDataNameFormat2 q = qualifiedDataNameFormat2;
        q = q.withParagraphName(visitAndCast(q.getParagraphName(), p));
        q = q.withInSection(visitAndCast(q.getInSection(), p));
        return q;
    }

    @Override
    public Cobol.QualifiedDataNameFormat3 visitQualifiedDataNameFormat3(Cobol.QualifiedDataNameFormat3 qualifiedDataNameFormat3, P p) {
        Cobol.QualifiedDataNameFormat3 q = qualifiedDataNameFormat3;
        q = q.withTextName(visitAndCast(q.getTextName(), p));
        q = q.withInLibrary(visitAndCast(q.getInLibrary(), p));
        return q;
    }

    @Override
    public Cobol.QualifiedDataNameFormat4 visitQualifiedDataNameFormat4(Cobol.QualifiedDataNameFormat4 qualifiedDataNameFormat4, P p) {
        Cobol.QualifiedDataNameFormat4 q = qualifiedDataNameFormat4;
        q = q.withInFile(visitAndCast(q.getInFile(), p));
        return q;
    }

    @Override
    public Cobol.QualifiedInData visitQualifiedInData(Cobol.QualifiedInData qualifiedInData, P p) {
        Cobol.QualifiedInData q = qualifiedInData;
        q = q.withIn(visitAndCast(q.getIn(), p));
        return q;
    }

    @Override
    public Cobol.Read visitRead(Cobol.Read read, P p) {
        Cobol.Read r = read;
        r = r.withFileName(visitAndCast(r.getFileName(), p));
        r = r.withReadInto(visitAndCast(r.getReadInto(), p));
        r = r.withReadWith(visitAndCast(r.getReadWith(), p));
        r = r.withReadKey(visitAndCast(r.getReadKey(), p));
        r = r.withInvalidKeyPhrase(visitAndCast(r.getInvalidKeyPhrase(), p));
        r = r.withNotInvalidKeyPhrase(visitAndCast(r.getNotInvalidKeyPhrase(), p));
        r = r.withAtEndPhrase(visitAndCast(r.getAtEndPhrase(), p));
        r = r.withNotAtEndPhrase(visitAndCast(r.getNotAtEndPhrase(), p));
        return r;
    }

    @Override
    public Cobol.ReadInto visitReadInto(Cobol.ReadInto readInto, P p) {
        Cobol.ReadInto r = readInto;
        r = r.withIdentifier(visitAndCast(r.getIdentifier(), p));
        return r;
    }

    @Override
    public Cobol.ReadKey visitReadKey(Cobol.ReadKey readKey, P p) {
        Cobol.ReadKey r = readKey;
        r = r.withQualifiedDataName(visitAndCast(r.getQualifiedDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReadWith visitReadWith(Cobol.ReadWith readWith, P p) {
        return readWith;
    }

    @Override
    public Cobol.Receivable visitReceivable(Cobol.Receivable receivable, P p) {
        Cobol.Receivable r = receivable;
        r = r.withValue(visitAndCast(r.getValue(), p));
        return r;
    }

    @Override
    public Cobol.Receive visitReceive(Cobol.Receive receive, P p) {
        Cobol.Receive r = receive;
        r = r.withFromOrInto(visitAndCast(r.getFromOrInto(), p));
        r = r.withOnExceptionClause(visitAndCast(r.getOnExceptionClause(), p));
        r = r.withNotOnExceptionClause(visitAndCast(r.getNotOnExceptionClause(), p));
        return r;
    }

    @Override
    public Cobol.ReceiveFrom visitReceiveFrom(Cobol.ReceiveFrom receiveFrom, P p) {
        Cobol.ReceiveFrom r = receiveFrom;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReceiveFromStatement visitReceiveFromStatement(Cobol.ReceiveFromStatement receiveFromStatement, P p) {
        Cobol.ReceiveFromStatement r = receiveFromStatement;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        r = r.withReceiveFrom(visitAndCast(r.getReceiveFrom(), p));
        r = r.withBeforeWithThreadSizeStatus(ListUtils.map(r.getBeforeWithThreadSizeStatus(), it -> visitAndCast(it, p)));
        return r;
    }

    @Override
    public Cobol.ReceiveIntoStatement visitReceiveIntoStatement(Cobol.ReceiveIntoStatement receiveIntoStatement, P p) {
        Cobol.ReceiveIntoStatement r = receiveIntoStatement;
        r = r.withCdName(visitAndCast(r.getCdName(), p));
        r = r.withIdentifier(visitAndCast(r.getIdentifier(), p));
        r = r.withReceiveNoData(visitAndCast(r.getReceiveNoData(), p));
        r = r.withReceiveWithData(visitAndCast(r.getReceiveWithData(), p));
        return r;
    }

    @Override
    public Cobol.ReceiveWith visitReceiveWith(Cobol.ReceiveWith receiveWith, P p) {
        return receiveWith;
    }

    @Override
    public Cobol.RecordContainsClause visitRecordContainsClause(Cobol.RecordContainsClause recordContainsClause, P p) {
        Cobol.RecordContainsClause r = recordContainsClause;
        r = r.withClause(visitAndCast(r.getClause(), p));
        return r;
    }

    @Override
    public Cobol.RecordContainsClauseFormat1 visitRecordContainsClauseFormat1(Cobol.RecordContainsClauseFormat1 recordContainsClauseFormat1, P p) {
        Cobol.RecordContainsClauseFormat1 r = recordContainsClauseFormat1;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        return r;
    }

    @Override
    public Cobol.RecordContainsClauseFormat2 visitRecordContainsClauseFormat2(Cobol.RecordContainsClauseFormat2 recordContainsClauseFormat2, P p) {
        Cobol.RecordContainsClauseFormat2 r = recordContainsClauseFormat2;
        r = r.withFromClause(ListUtils.map(r.getFromClause(), it -> visitAndCast(it, p)));
        r = r.withQualifiedDataName(ListUtils.map(r.getQualifiedDataName(), it -> visitAndCast(it, p)));
        return r;
    }

    @Override
    public Cobol.RecordContainsClauseFormat3 visitRecordContainsClauseFormat3(Cobol.RecordContainsClauseFormat3 recordContainsClauseFormat3, P p) {
        Cobol.RecordContainsClauseFormat3 r = recordContainsClauseFormat3;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        r = r.withRecordContainsTo(visitAndCast(r.getRecordContainsTo(), p));
        return r;
    }

    @Override
    public Cobol.RecordContainsTo visitRecordContainsTo(Cobol.RecordContainsTo recordContainsTo, P p) {
        Cobol.RecordContainsTo r = recordContainsTo;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        return r;
    }

    @Override
    public Cobol.RecordDelimiterClause visitRecordDelimiterClause(Cobol.RecordDelimiterClause recordDelimiterClause, P p) {
        Cobol.RecordDelimiterClause r = recordDelimiterClause;
        r = r.withName(visitAndCast(r.getName(), p));
        return r;
    }

    @Override
    public Cobol.RecordingModeClause visitRecordingModeClause(Cobol.RecordingModeClause recordingModeClause, P p) {
        Cobol.RecordingModeClause r = recordingModeClause;
        r = r.withMode(visitAndCast(r.getMode(), p));
        return r;
    }

    @Override
    public Cobol.RecordKeyClause visitRecordKeyClause(Cobol.RecordKeyClause recordKeyClause, P p) {
        Cobol.RecordKeyClause r = recordKeyClause;
        r = r.withQualifiedDataName(visitAndCast(r.getQualifiedDataName(), p));
        r = r.withPasswordClause(visitAndCast(r.getPasswordClause(), p));
        return r;
    }

    @Override
    public Cobol.ReferenceModifier visitReferenceModifier(Cobol.ReferenceModifier referenceModifier, P p) {
        Cobol.ReferenceModifier r = referenceModifier;
        r = r.withCharacterPosition(visitAndCast(r.getCharacterPosition(), p));
        r = r.withLength(visitAndCast(r.getLength(), p));
        return r;
    }

    @Override
    public Cobol.RelationalOperator visitRelationalOperator(Cobol.RelationalOperator relationalOperator, P p) {
        return relationalOperator;
    }

    @Override
    public Cobol.RelationArithmeticComparison visitRelationArithmeticComparison(Cobol.RelationArithmeticComparison relationArithmeticComparison, P p) {
        Cobol.RelationArithmeticComparison r = relationArithmeticComparison;
        r = r.withArithmeticExpressionA(visitAndCast(r.getArithmeticExpressionA(), p));
        r = r.withRelationalOperator(visitAndCast(r.getRelationalOperator(), p));
        r = r.withArithmeticExpressionB(visitAndCast(r.getArithmeticExpressionB(), p));
        return r;
    }

    @Override
    public Cobol.RelationCombinedComparison visitRelationCombinedComparison(Cobol.RelationCombinedComparison relationCombinedComparison, P p) {
        Cobol.RelationCombinedComparison r = relationCombinedComparison;
        r = r.withArithmeticExpression(visitAndCast(r.getArithmeticExpression(), p));
        r = r.withRelationalOperator(visitAndCast(r.getRelationalOperator(), p));
        r = r.withCombinedCondition(visitAndCast(r.getCombinedCondition(), p));
        return r;
    }

    @Override
    public Cobol.RelationCombinedCondition visitRelationCombinedCondition(Cobol.RelationCombinedCondition relationCombinedCondition, P p) {
        Cobol.RelationCombinedCondition r = relationCombinedCondition;
        r = r.withRelationalArithmeticExpressions(ListUtils.map(r.getRelationalArithmeticExpressions(), it -> visitAndCast(it, p)));
        return r;
    }

    @Override
    public Cobol.RelationSignCondition visitRelationSignCondition(Cobol.RelationSignCondition relationSignCondition, P p) {
        Cobol.RelationSignCondition r = relationSignCondition;
        r = r.withArithmeticExpression(visitAndCast(r.getArithmeticExpression(), p));
        return r;
    }

    @Override
    public Cobol.RelativeKeyClause visitRelativeKeyClause(Cobol.RelativeKeyClause relativeKeyClause, P p) {
        Cobol.RelativeKeyClause r = relativeKeyClause;
        r = r.withQualifiedDataName(visitAndCast(r.getQualifiedDataName(), p));
        return r;
    }

    @Override
    public Cobol.Release visitRelease(Cobol.Release release, P p) {
        Cobol.Release r = release;
        r = r.withRecordName(visitAndCast(r.getRecordName(), p));
        r = r.withQualifiedDataName(visitAndCast(r.getQualifiedDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportClause visitReportClause(Cobol.ReportClause reportClause, P p) {
        Cobol.ReportClause r = reportClause;
        r = r.withReportName(ListUtils.map(r.getReportName(), it -> visitAndCast(it, p)));
        return r;
    }

    @Override
    public Cobol.ReportDescription visitReportDescription(Cobol.ReportDescription reportDescription, P p) {
        Cobol.ReportDescription r = reportDescription;
        r = r.withReportDescriptionEntry(visitAndCast(r.getReportDescriptionEntry(), p));
        r = r.withGroupDescriptionEntries(ListUtils.map(r.getGroupDescriptionEntries(), it -> visitAndCast(it, p)));
        return r;
    }

    @Override
    public Cobol.ReportDescriptionEntry visitReportDescriptionEntry(Cobol.ReportDescriptionEntry reportDescriptionEntry, P p) {
        Cobol.ReportDescriptionEntry r = reportDescriptionEntry;
        r = r.withQualifiedDataName(visitAndCast(r.getQualifiedDataName(), p));
        r = r.withReportDescriptionGlobalClause(visitAndCast(r.getReportDescriptionGlobalClause(), p));
        r = r.withReportDescriptionPageLimitClause(visitAndCast(r.getReportDescriptionPageLimitClause(), p));
        r = r.withReportDescriptionHeadingClause(visitAndCast(r.getReportDescriptionHeadingClause(), p));
        r = r.withReportDescriptionFirstDetailClause(visitAndCast(r.getReportDescriptionFirstDetailClause(), p));
        r = r.withReportDescriptionLastDetailClause(visitAndCast(r.getReportDescriptionLastDetailClause(), p));
        r = r.withReportDescriptionFootingClause(visitAndCast(r.getReportDescriptionFootingClause(), p));
        return r;
    }

    @Override
    public Cobol.ReportDescriptionFirstDetailClause visitReportDescriptionFirstDetailClause(Cobol.ReportDescriptionFirstDetailClause reportDescriptionFirstDetailClause, P p) {
        Cobol.ReportDescriptionFirstDetailClause r = reportDescriptionFirstDetailClause;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportDescriptionFootingClause visitReportDescriptionFootingClause(Cobol.ReportDescriptionFootingClause reportDescriptionFootingClause, P p) {
        Cobol.ReportDescriptionFootingClause r = reportDescriptionFootingClause;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportDescriptionGlobalClause visitReportDescriptionGlobalClause(Cobol.ReportDescriptionGlobalClause reportDescriptionGlobalClause, P p) {
        return reportDescriptionGlobalClause;
    }

    @Override
    public Cobol.ReportDescriptionHeadingClause visitReportDescriptionHeadingClause(Cobol.ReportDescriptionHeadingClause reportDescriptionHeadingClause, P p) {
        Cobol.ReportDescriptionHeadingClause r = reportDescriptionHeadingClause;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportDescriptionLastDetailClause visitReportDescriptionLastDetailClause(Cobol.ReportDescriptionLastDetailClause reportDescriptionLastDetailClause, P p) {
        Cobol.ReportDescriptionLastDetailClause r = reportDescriptionLastDetailClause;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportDescriptionPageLimitClause visitReportDescriptionPageLimitClause(Cobol.ReportDescriptionPageLimitClause reportDescriptionPageLimitClause, P p) {
        Cobol.ReportDescriptionPageLimitClause r = reportDescriptionPageLimitClause;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupBlankWhenZeroClause visitReportGroupBlankWhenZeroClause(Cobol.ReportGroupBlankWhenZeroClause reportGroupBlankWhenZeroClause, P p) {
        return reportGroupBlankWhenZeroClause;
    }

    @Override
    public Cobol.ReportGroupColumnNumberClause visitReportGroupColumnNumberClause(Cobol.ReportGroupColumnNumberClause reportGroupColumnNumberClause, P p) {
        Cobol.ReportGroupColumnNumberClause r = reportGroupColumnNumberClause;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupDescriptionEntryFormat1 visitReportGroupDescriptionEntryFormat1(Cobol.ReportGroupDescriptionEntryFormat1 reportGroupDescriptionEntryFormat1, P p) {
        Cobol.ReportGroupDescriptionEntryFormat1 r = reportGroupDescriptionEntryFormat1;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        r = r.withGroupLineNumberClause(visitAndCast(r.getGroupLineNumberClause(), p));
        r = r.withGroupNextGroupClause(visitAndCast(r.getGroupNextGroupClause(), p));
        r = r.withGroupTypeClause(visitAndCast(r.getGroupTypeClause(), p));
        r = r.withGroupUsageClause(visitAndCast(r.getGroupUsageClause(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupDescriptionEntryFormat2 visitReportGroupDescriptionEntryFormat2(Cobol.ReportGroupDescriptionEntryFormat2 reportGroupDescriptionEntryFormat2, P p) {
        Cobol.ReportGroupDescriptionEntryFormat2 r = reportGroupDescriptionEntryFormat2;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        r = r.withReportGroupLineNumberClause(visitAndCast(r.getReportGroupLineNumberClause(), p));
        r = r.withGroupUsageClause(visitAndCast(r.getGroupUsageClause(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupDescriptionEntryFormat3 visitReportGroupDescriptionEntryFormat3(Cobol.ReportGroupDescriptionEntryFormat3 reportGroupDescriptionEntryFormat3, P p) {
        Cobol.ReportGroupDescriptionEntryFormat3 r = reportGroupDescriptionEntryFormat3;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        r = r.withClauses(ListUtils.map(r.getClauses(), c -> visitAndCast(c, p)));
        return r;
    }

    @Override
    public Cobol.ReportGroupIndicateClause visitReportGroupIndicateClause(Cobol.ReportGroupIndicateClause reportGroupIndicateClause, P p) {
        return reportGroupIndicateClause;
    }

    @Override
    public Cobol.ReportGroupJustifiedClause visitReportGroupJustifiedClause(Cobol.ReportGroupJustifiedClause reportGroupJustifiedClause, P p) {
        return reportGroupJustifiedClause;
    }

    @Override
    public Cobol.ReportGroupLineNumberClause visitReportGroupLineNumberClause(Cobol.ReportGroupLineNumberClause reportGroupLineNumberClause, P p) {
        Cobol.ReportGroupLineNumberClause r = reportGroupLineNumberClause;
        r = r.withClause(visitAndCast(r.getClause(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupLineNumberNextPage visitReportGroupLineNumberNextPage(Cobol.ReportGroupLineNumberNextPage reportGroupLineNumberNextPage, P p) {
        Cobol.ReportGroupLineNumberNextPage r = reportGroupLineNumberNextPage;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupLineNumberPlus visitReportGroupLineNumberPlus(Cobol.ReportGroupLineNumberPlus reportGroupLineNumberPlus, P p) {
        Cobol.ReportGroupLineNumberPlus r = reportGroupLineNumberPlus;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupNextGroupClause visitReportGroupNextGroupClause(Cobol.ReportGroupNextGroupClause reportGroupNextGroupClause, P p) {
        Cobol.ReportGroupNextGroupClause r = reportGroupNextGroupClause;
        r = r.withClause(visitAndCast(r.getClause(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupNextGroupNextPage visitReportGroupNextGroupNextPage(Cobol.ReportGroupNextGroupNextPage reportGroupNextGroupNextPage, P p) {
        return reportGroupNextGroupNextPage;
    }

    @Override
    public Cobol.ReportGroupNextGroupPlus visitReportGroupNextGroupPlus(Cobol.ReportGroupNextGroupPlus reportGroupNextGroupPlus, P p) {
        Cobol.ReportGroupNextGroupPlus r = reportGroupNextGroupPlus;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupPictureClause visitReportGroupPictureClause(Cobol.ReportGroupPictureClause reportGroupPictureClause, P p) {
        Cobol.ReportGroupPictureClause r = reportGroupPictureClause;
        r = r.withPictureString(visitAndCast(r.getPictureString(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupResetClause visitReportGroupResetClause(Cobol.ReportGroupResetClause reportGroupResetClause, P p) {
        Cobol.ReportGroupResetClause r = reportGroupResetClause;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupSignClause visitReportGroupSignClause(Cobol.ReportGroupSignClause reportGroupSignClause, P p) {
        return reportGroupSignClause;
    }

    @Override
    public Cobol.ReportGroupSourceClause visitReportGroupSourceClause(Cobol.ReportGroupSourceClause reportGroupSourceClause, P p) {
        Cobol.ReportGroupSourceClause r = reportGroupSourceClause;
        r = r.withIdentifier(visitAndCast(r.getIdentifier(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupSumClause visitReportGroupSumClause(Cobol.ReportGroupSumClause reportGroupSumClause, P p) {
        Cobol.ReportGroupSumClause r = reportGroupSumClause;
        r = r.withCobols(ListUtils.map(r.getCobols(), c -> visitAndCast(c, p)));
        return r;
    }

    @Override
    public Cobol.ReportGroupTypeClause visitReportGroupTypeClause(Cobol.ReportGroupTypeClause reportGroupTypeClause, P p) {
        Cobol.ReportGroupTypeClause r = reportGroupTypeClause;
        r = r.withType(visitAndCast(r.getType(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupTypeControlFooting visitReportGroupTypeControlFooting(Cobol.ReportGroupTypeControlFooting reportGroupTypeControlFooting, P p) {
        Cobol.ReportGroupTypeControlFooting r = reportGroupTypeControlFooting;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupTypeControlHeading visitReportGroupTypeControlHeading(Cobol.ReportGroupTypeControlHeading reportGroupTypeControlHeading, P p) {
        Cobol.ReportGroupTypeControlHeading r = reportGroupTypeControlHeading;
        r = r.withDataName(visitAndCast(r.getDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportGroupTypeDetail visitReportGroupTypeDetail(Cobol.ReportGroupTypeDetail reportGroupTypeDetail, P p) {
        return reportGroupTypeDetail;
    }

    @Override
    public Cobol.ReportGroupTypePageFooting visitReportGroupTypePageFooting(Cobol.ReportGroupTypePageFooting reportGroupTypePageFooting, P p) {
        return reportGroupTypePageFooting;
    }

    @Override
    public Cobol.ReportGroupTypePageHeading visitReportGroupTypePageHeading(Cobol.ReportGroupTypePageHeading reportGroupTypePageHeading, P p) {
        return reportGroupTypePageHeading;
    }

    @Override
    public Cobol.ReportGroupTypeReportFooting visitReportGroupTypeReportFooting(Cobol.ReportGroupTypeReportFooting reportGroupTypeReportFooting, P p) {
        return reportGroupTypeReportFooting;
    }

    @Override
    public Cobol.ReportGroupTypeReportHeading visitReportGroupTypeReportHeading(Cobol.ReportGroupTypeReportHeading reportGroupTypeReportHeading, P p) {
        return reportGroupTypeReportHeading;
    }

    @Override
    public Cobol.ReportGroupUsageClause visitReportGroupUsageClause(Cobol.ReportGroupUsageClause reportGroupUsageClause, P p) {
        return reportGroupUsageClause;
    }

    @Override
    public Cobol.ReportGroupValueClause visitReportGroupValueClause(Cobol.ReportGroupValueClause reportGroupValueClause, P p) {
        Cobol.ReportGroupValueClause r = reportGroupValueClause;
        r = r.withLiteral(visitAndCast(r.getLiteral(), p));
        return r;
    }

    @Override
    public Cobol.ReportName visitReportName(Cobol.ReportName reportName, P p) {
        Cobol.ReportName r = reportName;
        r = r.withQualifiedDataName(visitAndCast(r.getQualifiedDataName(), p));
        return r;
    }

    @Override
    public Cobol.ReportSection visitReportSection(Cobol.ReportSection reportSection, P p) {
        Cobol.ReportSection r = reportSection;
        r = r.withDescriptions(ListUtils.map(r.getDescriptions(), d -> visitAndCast(d, p)));
        return r;
    }

    @Override
    public Cobol.RerunClause visitRerunClause(Cobol.RerunClause rerunClause, P p) {
        Cobol.RerunClause r = rerunClause;
        r = r.withName(visitAndCast(r.getName(), p));
        r = r.withAction(visitAndCast(r.getAction(), p));
        return r;
    }

    @Override
    public Cobol.RerunEveryClock visitRerunEveryClock(Cobol.RerunEveryClock rerunEveryClock, P p) {
        Cobol.RerunEveryClock r = rerunEveryClock;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        return r;
    }

    @Override
    public Cobol.RerunEveryOf visitRerunEveryOf(Cobol.RerunEveryOf rerunEveryOf, P p) {
        Cobol.RerunEveryOf r = rerunEveryOf;
        r = r.withFileName(visitAndCast(r.getFileName(), p));
        return r;
    }

    @Override
    public Cobol.RerunEveryRecords visitRerunEveryRecords(Cobol.RerunEveryRecords rerunEveryRecords, P p) {
        Cobol.RerunEveryRecords r = rerunEveryRecords;
        r = r.withIntegerLiteral(visitAndCast(r.getIntegerLiteral(), p));
        return r;
    }

    @Override
    public Cobol.ReserveClause visitReserveClause(Cobol.ReserveClause reserveClause, P p) {
        Cobol.ReserveClause r = reserveClause;
        r = r.withWords(ListUtils.map(r.getWords(), w -> visitAndCast(w, p)));
        return r;
    }

    @Override
    public Cobol.ReserveNetworkClause visitReserveNetworkClause(Cobol.ReserveNetworkClause reserveNetworkClause, P p) {
        return reserveNetworkClause;
    }

    @Override
    public Cobol.Return visitReturn(Cobol.Return returnz, P p) {
        Cobol.Return r = returnz;
        r = r.withFileName(visitAndCast(r.getFileName(), p));
        r = r.withInto(visitAndCast(r.getInto(), p));
        r = r.withAtEndPhrase(visitAndCast(r.getAtEndPhrase(), p));
        r = r.withNotAtEndPhrase(visitAndCast(r.getNotAtEndPhrase(), p));
        return r;
    }

    @Override
    public Cobol.ReturnInto visitReturnInto(Cobol.ReturnInto returnInto, P p) {
        Cobol.ReturnInto r = returnInto;
        r = r.withQualifiedDataName(visitAndCast(r.getQualifiedDataName(), p));
        return r;
    }

    @Override
    public Cobol.Rewrite visitRewrite(Cobol.Rewrite rewrite, P p) {
        Cobol.Rewrite r = rewrite;
        r = r.withRecordName(visitAndCast(r.getRecordName(), p));
        r = r.withRewriteFrom(visitAndCast(r.getRewriteFrom(), p));
        r = r.withInvalidKeyPhrase(visitAndCast(r.getInvalidKeyPhrase(), p));
        r = r.withNotInvalidKeyPhrase(visitAndCast(r.getNotInvalidKeyPhrase(), p));
        return r;
    }

    @Override
    public Cobol.RewriteFrom visitRewriteFrom(Cobol.RewriteFrom rewriteFrom, P p) {
        Cobol.RewriteFrom r = rewriteFrom;
        r = r.withIdentifier(visitAndCast(r.getIdentifier(), p));
        return r;
    }

    @Override
    public Cobol.Roundable visitRoundable(Cobol.Roundable roundable, P p) {
        Cobol.Roundable r = roundable;
        r = r.withIdentifier(visitAndCast(r.getIdentifier(), p));
        return r;
    }
}
