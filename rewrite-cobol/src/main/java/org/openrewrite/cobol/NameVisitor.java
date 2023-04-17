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

}
