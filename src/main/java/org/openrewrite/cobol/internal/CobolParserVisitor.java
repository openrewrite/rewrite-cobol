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
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.openrewrite.ExecutionContext;
import org.openrewrite.FileAttributes;
import org.openrewrite.cobol.internal.grammar.CobolBaseVisitor;
import org.openrewrite.cobol.internal.grammar.CobolParser;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Collections.*;
import static org.openrewrite.Tree.randomId;
import static org.openrewrite.cobol.internal.CobolGrammarToken.COMMENT_ENTRY;
import static org.openrewrite.cobol.internal.CobolGrammarToken.END_OF_FILE;
import static org.openrewrite.cobol.tree.Space.format;

public class CobolParserVisitor extends CobolBaseVisitor<Object> {

    private final Path path;

    @Nullable
    private final FileAttributes fileAttributes;

    private final String source;
    private final Charset charset;
    private final boolean charsetBomMarked;
    private final CobolDialect cobolDialect;

    private final Map<String, CobolPreprocessor.CopyStatement> copyMap = new HashMap<>();
    private final Map<String, CobolPreprocessor.ReplaceByStatement> replaceByMap = new HashMap<>();
    private final Map<String, CobolPreprocessor.ReplaceOffStatement> replaceOffMap = new HashMap<>();
    private final Map<String, Replace> replaceMap = new HashMap<>();
    private final Map<String, ReplaceAdditiveType> replaceAdditiveTypeMap = new HashMap<>();
    private final Map<String, ReplaceReductiveType> replaceReductiveTypeMap = new HashMap<>();
    private final Set<String> templateKeys = new HashSet<>();

    // Areas may be a Set of Integer to reduce memory, each method to create the marker would generate the string.
    private final Map<Integer, String> sequenceAreas = new HashMap<>();

    // A LinkedHash map is used for Indicators to collect information about the next line of code. I.E. continuation indicators.
    private final Map<Integer, String> indicatorAreas = new LinkedHashMap<>();
    // A LinkedHash map is used for CommentAreas to determine if the current token ends the line.
    private final Map<Integer, String> commentAreas = new LinkedHashMap<>();

    private final Set<String> separators = new HashSet<>();
    private final Set<Character> commentIndicators = new HashSet<>();
    private int cursor = 0;

    // Trigger condition to remove whitespace added by the template.
    private boolean removeTemplateCommentArea;
    private boolean removeColumnMarkers;
    private boolean isAdditiveCommentArea;

    private String copyStartComment = null;
    private String copyStopComment = null;
    private String copyUuidComment = null;
    // Set to true when the parser detects the CopyStartComment to add the currentCopy to each `Cobol$Word`.
    private boolean inCopiedText;

    // TODO: this could use a map instead.
    @Nullable
    private ReplaceAdditiveType replaceAdditiveType = null;

    @Nullable
    private CobolPreprocessor.CopyStatement currentCopy = null;

    private String replaceStartComment = null;
    private String replaceStopComment = null;
    private String replaceAdditiveWhitespaceComment = null;
    private String replaceUuidComment = null;

    private String replaceByStartComment = null;
    private String replaceByStopComment = null;

    private String replaceOffStartComment = null;
    private String replaceOffStopComment = null;

    private String replaceAdditiveTypeStartComment = null;
    private String replaceAdditiveTypeStopComment = null;

    private String replaceAddWordStartComment = null;
    private String replaceAddWordStopComment = null;

    private String replaceReductiveTypeStartComment = null;
    private String replaceReductiveTypeStopComment = null;

    private String uuidComment = null;

    private Integer nextIndex = null;

    public CobolParserVisitor(Path path,
                              @Nullable FileAttributes fileAttributes,
                              String source,
                              Charset charset,
                              boolean charsetBomMarked,
                              CobolDialect cobolDialect,
                              Collection<CobolPreprocessor.CopyStatement> copyStatements,
                              Collection<CobolPreprocessor.ReplaceByStatement> replaceByStatements,
                              Collection<CobolPreprocessor.ReplaceOffStatement> replaceOffStatements,
                              Collection<Replace> replaces,
                              Collection<ReplaceAdditiveType> replaceAdditiveTypes,
                              Collection<ReplaceReductiveType> replaceReductiveTypes) {
        this.path = path;
        this.fileAttributes = fileAttributes;
        this.source = source;
        this.charset = charset;
        this.charsetBomMarked = charsetBomMarked;
        this.cobolDialect = cobolDialect;

        copyStatements.forEach(it -> copyMap.putIfAbsent(it.getId().toString(), it));
        replaceByStatements.forEach(it -> replaceByMap.putIfAbsent(it.getId().toString(), it));
        replaceOffStatements.forEach(it -> replaceOffMap.putIfAbsent(it.getId().toString(), it));
        replaces.forEach(it -> replaceMap.putIfAbsent(it.getId().toString(), it));
        replaceAdditiveTypes.forEach(it -> replaceAdditiveTypeMap.putIfAbsent(it.getId().toString(), it));
        replaceReductiveTypes.forEach(it -> replaceReductiveTypeMap.putIfAbsent(it.getId().toString(), it));
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

    private void init() {
        String[] parts = source.split("\n");

        if (cobolDialect.getColumns() == CobolDialect.Columns.IBM_ANSI_85) {
            CobolDialect.Columns columns = cobolDialect.getColumns();
            int pos = 0;
            for (String part : parts) {
                boolean isCRLF = part.endsWith("\r");
                String cleanedPart = isCRLF ? part.substring(0, part.length() - 1) : part;

                String sequenceArea = cleanedPart.substring(columns.getSequenceArea(), columns.getIndicatorArea());
                sequenceAreas.put(pos, sequenceArea);
                pos += sequenceArea.length();

                String indicatorArea = cleanedPart.substring(columns.getIndicatorArea(), columns.getContentArea());
                indicatorAreas.put(pos, indicatorArea);
                pos += indicatorArea.length();

                String contentArea = cleanedPart.substring(columns.getContentArea(), columns.getOtherArea());
                pos += contentArea.length();

                String otherArea = cleanedPart.length() > columns.getContentArea() ? cleanedPart.substring(columns.getOtherArea()) : "";
                if (!otherArea.isEmpty()) {
                    commentAreas.put(pos, otherArea);
                    pos += otherArea.length();
                }
                pos += isCRLF ? 2 : 1; // Increment for new line delimiter.
            }

            separators.addAll(cobolDialect.getSeparators());
            commentIndicators.addAll(cobolDialect.getCommentIndicators());

            CobolPreprocessorOutputSourcePrinter<ExecutionContext> templatePrinter = new CobolPreprocessorOutputSourcePrinter<>(cobolDialect, true);

            this.copyStartComment = getCommentFromKey(templatePrinter.getCopyStartComment());
            this.templateKeys.add(copyStartComment);

            this.copyStopComment = getCommentFromKey(templatePrinter.getCopyStopComment());
            this.templateKeys.add(copyStopComment);

            this.copyUuidComment = getCommentFromKey(templatePrinter.getCopyUuidKey());
            this.templateKeys.add(copyUuidComment);

            this.replaceStartComment = getCommentFromKey(templatePrinter.getReplaceStartComment());
            this.templateKeys.add(replaceStartComment);

            this.replaceStopComment = getCommentFromKey(templatePrinter.getReplaceStopComment());
            this.templateKeys.add(replaceStopComment);

            this.replaceUuidComment = getCommentFromKey(templatePrinter.getReplaceUuidComment());
            this.templateKeys.add(replaceUuidComment);

            this.replaceAdditiveWhitespaceComment = getCommentFromKey(templatePrinter.getReplaceTypeAdditiveComment());
            this.templateKeys.add(replaceAdditiveWhitespaceComment);

            this.replaceByStartComment = getCommentFromKey(templatePrinter.getReplaceByStartComment());
            this.templateKeys.add(replaceByStartComment);

            this.replaceByStopComment = getCommentFromKey(templatePrinter.getReplaceByStopComment());
            this.templateKeys.add(replaceByStopComment);

            this.replaceOffStartComment = getCommentFromKey(templatePrinter.getReplaceOffStartComment());
            this.templateKeys.add(replaceOffStartComment);

            this.replaceOffStopComment = getCommentFromKey(templatePrinter.getReplaceOffStopComment());
            this.templateKeys.add(replaceOffStopComment);

            this.replaceAddWordStartComment = getCommentFromKey(templatePrinter.getReplaceAddWordStartComment());
            this.templateKeys.add(replaceAddWordStartComment);

            this.replaceAddWordStopComment = getCommentFromKey(templatePrinter.getReplaceAddWordStopComment());
            this.templateKeys.add(replaceAddWordStopComment);

            this.replaceAdditiveTypeStartComment = getCommentFromKey(templatePrinter.getReplaceTypeAdditiveStartComment());
            this.templateKeys.add(replaceAdditiveTypeStartComment);

            this.replaceAdditiveTypeStopComment = getCommentFromKey(templatePrinter.getReplaceTypeAdditiveStopComment());
            this.templateKeys.add(replaceAdditiveTypeStopComment);

            this.replaceReductiveTypeStartComment = getCommentFromKey(templatePrinter.getReplaceTypeReductiveStartComment());
            this.templateKeys.add(replaceReductiveTypeStartComment);

            this.replaceReductiveTypeStopComment = getCommentFromKey(templatePrinter.getReplaceTypeReductiveStopComment());
            this.templateKeys.add(replaceReductiveTypeStopComment);

            this.uuidComment = getCommentFromKey(templatePrinter.getUuidComment());
            this.templateKeys.add(uuidComment);

        } else if (cobolDialect.getColumns() == CobolDialect.Columns.HP_TANDEM) {
            throw new UnsupportedOperationException("Implement me.");
        } else {
            throw new UnsupportedOperationException("CobolDialect is not supported: " + cobolDialect.getColumns().name());
        }
    }

    @Override
    public Cobol.CompilationUnit visitCompilationUnit(CobolParser.CompilationUnitContext ctx) {
        init();

        Space prefix = Space.EMPTY;
        List<Cobol.ProgramUnit> programUnits = new ArrayList<>(ctx.programUnit().size());
        for (CobolParser.ProgramUnitContext pu : ctx.programUnit()) {
            programUnits.add(visitProgramUnit(pu));
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
                (Cobol.Word) visit(ctx.EOF())
        );
    }

    @Override
    public Object visitAbbreviation(CobolParser.AbbreviationContext ctx) {
        return new Cobol.Abbreviation(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FROM(), ctx.DATE(), ctx.YYYYMMDD(), ctx.DAY(), ctx.YYYYDDD(), ctx.DAY_OF_WEEK(), ctx.TIME(), ctx.TIMER(), ctx.TODAYS_DATE(), ctx.MMDDYYYY(), ctx.TODAYS_NAME(), ctx.YEAR())
        );
    }

    @Override
    public Object visitAcceptFromEscapeKeyStatement(CobolParser.AcceptFromEscapeKeyStatementContext ctx) {
        return new Cobol.AcceptFromEscapeKeyStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FROM(), ctx.ESCAPE(), ctx.KEY())
        );
    }

    @Override
    public Object visitAcceptFromMnemonicStatement(CobolParser.AcceptFromMnemonicStatementContext ctx) {
        return new Cobol.AcceptFromMnemonicStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.FROM()),
                (Identifier) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Object visitAcceptMessageCountStatement(CobolParser.AcceptMessageCountStatementContext ctx) {
        return new Cobol.AcceptMessageCountStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.MESSAGE(), ctx.COUNT())
        );
    }

    @Override
    public Object visitAcceptStatement(CobolParser.AcceptStatementContext ctx) {
        return new Cobol.Accept(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ACCEPT()),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ACCESS(), ctx.MODE(), ctx.IS()),
                visit(ctx.SEQUENTIAL(), ctx.RANDOM(), ctx.DYNAMIC(), ctx.EXCLUSIVE())
        );
    }

    @Override
    public Object visitAddCorrespondingStatement(CobolParser.AddCorrespondingStatementContext ctx) {
        return new Cobol.AddCorresponding(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.CORRESPONDING(), ctx.CORR()),
                (Identifier) visit(ctx.identifier()),
                (Cobol.Word) visit(ctx.TO()),
                (Cobol.Roundable) visit(ctx.addTo())
        );
    }

    @Override
    public Cobol.Add visitAddStatement(CobolParser.AddStatementContext ctx) {
        return new Cobol.Add(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ADD()),
                visit(ctx.addToStatement(), ctx.addToGivingStatement(), ctx.addCorrespondingStatement()),
                visitNullable(ctx.onSizeErrorPhrase()),
                visitNullable(ctx.notOnSizeErrorPhrase()),
                visitNullable(ctx.END_ADD())
        );
    }

    @Override
    public Cobol.AddToGiving visitAddToGivingStatement(CobolParser.AddToGivingStatementContext ctx) {
        return new Cobol.AddToGiving(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.addFrom()),
                visitNullable(ctx.TO()),
                convertAll(ctx.addToGiving()),
                (Cobol.Word) visit(ctx.GIVING()),
                convertAll(ctx.addGiving())
        );
    }

    @Override
    public Cobol.AddTo visitAddToStatement(CobolParser.AddToStatementContext ctx) {
        return new Cobol.AddTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.addFrom()),
                visitNullable(ctx.TO()),
                convertAll(ctx.addTo())
        );
    }

    @Override
    public Cobol.AlphabetAlso visitAlphabetAlso(CobolParser.AlphabetAlsoContext ctx) {
        return new Cobol.AlphabetAlso(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ALSO()),
                convertAll(ctx.literal())
        );
    }

    @Override
    public Cobol.AlphabetClause visitAlphabetClauseFormat1(CobolParser.AlphabetClauseFormat1Context ctx) {
        return new Cobol.AlphabetClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ALPHABET()),
                (Name) visit(ctx.alphabetName()),
                convertAllList(singletonList(ctx.FOR()), singletonList(ctx.ALPHANUMERIC()), singletonList(ctx.IS()),
                        singletonList(ctx.EBCDIC()), singletonList(ctx.ASCII()), singletonList(ctx.STANDARD_1()),
                        singletonList(ctx.STANDARD_2()), singletonList(ctx.NATIVE()),
                        singletonList(ctx.cobolWord()), ctx.alphabetLiterals())
        );
    }

    @Override
    public Cobol.AlphabetClause visitAlphabetClauseFormat2(CobolParser.AlphabetClauseFormat2Context ctx) {
        return new Cobol.AlphabetClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ALPHABET()),
                (Name) visit(ctx.alphabetName()),
                convertAllList(singletonList(ctx.FOR()), singletonList(ctx.NATIONAL()), singletonList(ctx.IS()),
                        singletonList(ctx.NATIVE()), singletonList(ctx.CCSVERSION()), singletonList(ctx.literal()))
        );
    }

    @Override
    public Cobol.AlphabetLiteral visitAlphabetLiterals(CobolParser.AlphabetLiteralsContext ctx) {
        return new Cobol.AlphabetLiteral(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Literal) visit(ctx.literal()),
                visitNullable(ctx.alphabetThrough()),
                ctx.alphabetAlso() == null ? null : convertAll(ctx.alphabetAlso())
        );
    }

    @Override
    public Cobol.AlphabetThrough visitAlphabetThrough(CobolParser.AlphabetThroughContext ctx) {
        return new Cobol.AlphabetThrough(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.THROUGH(), ctx.THRU()),
                (Literal) visit(ctx.literal())
        );
    }

    @Override
    public Cobol.AlterProceedTo visitAlterProceedTo(CobolParser.AlterProceedToContext ctx) {
        return new Cobol.AlterProceedTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.ProcedureName) visit(ctx.procedureName(0)),
                wordsList(ctx.TO(0), ctx.PROCEED(), ctx.PROCEED() != null ? ctx.TO(1) : null),
                (Cobol.ProcedureName) visit(ctx.procedureName(1))
        );
    }

    @Override
    public Cobol.AlterStatement visitAlterStatement(CobolParser.AlterStatementContext ctx) {
        return new Cobol.AlterStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ALTER()),
                convertAll(ctx.alterProceedTo())
        );
    }

    @Override
    public Object visitAlteredGoTo(CobolParser.AlteredGoToContext ctx) {
        return new Cobol.AlteredGoTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.GO(), ctx.TO()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitAlternateRecordKeyClause(CobolParser.AlternateRecordKeyClauseContext ctx) {
        return new Cobol.AlternateRecordKeyClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ALTERNATE(), ctx.RECORD(), ctx.KEY(), ctx.IS()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                visitNullable(ctx.passwordClause()),
                wordsList(ctx.WITH(), ctx.DUPLICATES())
        );
    }

    @Override
    public Object visitAndOrCondition(CobolParser.AndOrConditionContext ctx) {
        return new Cobol.AndOrCondition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.AND(), ctx.OR()),
                visitNullable(ctx.combinableCondition()),
                convertAll(ctx.abbreviation())
        );
    }

    @Override
    public Object visitArgument(CobolParser.ArgumentContext ctx) {
        return new Cobol.Argument(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.literal(), ctx.identifier(), ctx.qualifiedDataName(), ctx.indexName(), ctx.arithmeticExpression()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitArithmeticExpression(CobolParser.ArithmeticExpressionContext ctx) {
        return new Cobol.ArithmeticExpression(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.MultDivs) visit(ctx.multDivs()),
                convertAll(ctx.plusMinus())
        );
    }

    @Override
    public Object visitAssignClause(CobolParser.AssignClauseContext ctx) {
        return new Cobol.AssignClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ASSIGN(), ctx.TO(), ctx.DISK(), ctx.DISPLAY(), ctx.KEYBOARD(), ctx.PORT(), ctx.PRINTER(),
                        ctx.READER(), ctx.REMOTE(), ctx.TAPE(), ctx.VIRTUAL(), ctx.DYNAMIC(), ctx.EXTERNAL()),
                ctx.assignmentName() != null ? visitNullable(ctx.assignmentName()) :
                        ctx.literal() != null ? visitNullable(ctx.literal()) : null
        );
    }

    @Override
    public Cobol.StatementPhrase visitAtEndPhrase(CobolParser.AtEndPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.AT(), ctx.END()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitAuthorParagraph(CobolParser.AuthorParagraphContext ctx) {
        return new Cobol.IdentificationDivisionParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.AUTHOR()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                visitNullable(ctx.commentEntry()),
                null,
                null
        );
    }

    @Override
    public Object visitBasis(CobolParser.BasisContext ctx) {
        if (ctx.arithmeticExpression() != null) {
            return new Cobol.Parenthesized(
                    randomId(),
                    Space.EMPTY,
                    Markers.EMPTY,
                    (Cobol.Word) visit(ctx.LPARENCHAR()),
                    singletonList((Cobol) visit(ctx.arithmeticExpression())),
                    (Cobol.Word) visit(ctx.RPARENCHAR())
            );
        } else {
            return visit(ctx.identifier(), ctx.literal());
        }
    }

    @Override
    public Object visitBlockContainsClause(CobolParser.BlockContainsClauseContext ctx) {
        return new Cobol.BlockContainsClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BLOCK(), ctx.CONTAINS()),
                visitNullable(ctx.integerLiteral()),
                visitNullable(ctx.blockContainsTo()),
                ctx.RECORDS() != null ? (Cobol.Word) visit(ctx.RECORDS()) :
                        ctx.CHARACTERS() != null ? (Cobol.Word)visit(ctx.CHARACTERS()) : null
        );
    }

    @Override
    public Object visitBlockContainsTo(CobolParser.BlockContainsToContext ctx) {
        return new Cobol.BlockContainsTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.TO()),
                (Cobol.Word) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitCallByContent(CobolParser.CallByContentContext ctx) {
        return new Cobol.CallBy(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ADDRESS(), ctx.LENGTH(), ctx.OF(), ctx.OMITTED()),
                ctx.identifier() != null ? (Name) visit(ctx.identifier()) :
                        ctx.literal() != null ? (Name) visit(ctx.literal()) : null
        );
    }

    @Override
    public Object visitCallByContentPhrase(CobolParser.CallByContentPhraseContext ctx) {
        return new Cobol.CallPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BY(), ctx.CONTENT()),
                convertAll(ctx.callByContent())
        );
    }

    @Override
    public Object visitCallByReference(CobolParser.CallByReferenceContext ctx) {
        return new Cobol.CallBy(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ADDRESS(), ctx.OF(), ctx.INTEGER(), ctx.STRING(), ctx.OMITTED()),
                ctx.identifier() != null ? (Name) visit(ctx.identifier()) :
                        ctx.literal() != null ? (Name) visit(ctx.literal()) :
                                ctx.fileName() != null ? (Name) visit(ctx.fileName()) : null
        );
    }

    @Override
    public Object visitCallByReferencePhrase(CobolParser.CallByReferencePhraseContext ctx) {
        return new Cobol.CallPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BY(), ctx.REFERENCE()),
                convertAll(ctx.callByReference())
        );
    }

    @Override
    public Object visitCallByValue(CobolParser.CallByValueContext ctx) {
        return new Cobol.CallBy(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ADDRESS(), ctx.LENGTH(), ctx.OF()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitCallByValuePhrase(CobolParser.CallByValuePhraseContext ctx) {
        return new Cobol.CallPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BY(), ctx.VALUE()),
                convertAll(ctx.callByValue())
        );
    }

    @Override
    public Object visitCallGivingPhrase(CobolParser.CallGivingPhraseContext ctx) {
        return new Cobol.CallGivingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.GIVING(), ctx.RETURNING()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitCallStatement(CobolParser.CallStatementContext ctx) {
        return new Cobol.Call(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.USING()),
                convertAll(ctx.callUsingParameter())
        );
    }

    @Override
    public Object visitCancelCall(CobolParser.CancelCallContext ctx) {
        return new Cobol.CancelCall(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.libraryName()),
                wordsList(ctx.BYTITLE(), ctx.BYFUNCTION()),
                visitNullable(ctx.identifier()),
                visitNullable(ctx.literal())
        );
    }

    @Override
    public Object visitCancelStatement(CobolParser.CancelStatementContext ctx) {
        return new Cobol.Cancel(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CANCEL()),
                convertAll(ctx.cancelCall())
        );
    }

    @Override
    public Cobol.ChannelClause visitChannelClause(CobolParser.ChannelClauseContext ctx) {
        return new Cobol.ChannelClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CHANNEL()),
                (Literal) visit(ctx.integerLiteral()),
                visitNullable(ctx.IS()),
                (Identifier) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Cobol.ValuedObjectComputerClause visitCharacterSetClause(CobolParser.CharacterSetClauseContext ctx) {
        return new Cobol.ValuedObjectComputerClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                Cobol.ValuedObjectComputerClause.Type.CharacterSet,
                wordsList(ctx.CHARACTER(), ctx.SET(), ctx.DOT_FS()),
                null,
                null
        );
    }

    @Override
    public Object visitClassClause(CobolParser.ClassClauseContext ctx) {
        return new Cobol.ClassClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CLASS()),
                visitNullable(ctx.className()),
                wordsList(ctx.FOR(), ctx.ALPHANUMERIC(), ctx.NATIONAL(), ctx.IS()),
                convertAll(ctx.classClauseThrough())
        );
    }

    @Override
    public Object visitClassClauseThrough(CobolParser.ClassClauseThroughContext ctx) {
        return new Cobol.ClassClauseThrough(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.classClauseFrom()),
                ctx.THROUGH() != null ? (Cobol.Word) visit(ctx.THROUGH()) :
                        ctx.THRU() != null ? (Cobol.Word) visit(ctx.THRU()) : null,
                visitNullable(ctx.classClauseTo())
        );
    }

    @Override
    public Object visitClassCondition(CobolParser.ClassConditionContext ctx) {
        return new Cobol.ClassCondition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.identifier()),
                wordsList(ctx.IS(), ctx.NOT()),
                visit(ctx.NUMERIC(), ctx.ALPHABETIC(), ctx.ALPHABETIC_LOWER(), ctx.ALPHABETIC_UPPER(), ctx.DBCS(), ctx.KANJI(), ctx.className())
        );
    }

    @Override
    public Object visitCloseFile(CobolParser.CloseFileContext ctx) {
        return new Cobol.CloseFile(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.NO(), ctx.WAIT(), ctx.USING()),
                convertAll(ctx.closePortFileIOUsing())
        );
    }

    @Override
    public Object visitClosePortFileIOUsingAssociatedData(CobolParser.ClosePortFileIOUsingAssociatedDataContext ctx) {
        return new Cobol.ClosePortFileIOUsingAssociatedData(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ASSOCIATED_DATA()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitClosePortFileIOUsingAssociatedDataLength(CobolParser.ClosePortFileIOUsingAssociatedDataLengthContext ctx) {
        return new Cobol.ClosePortFileIOUsingAssociatedDataLength(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ASSOCIATED_DATA_LENGTH(), ctx.OF()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitClosePortFileIOUsingCloseDisposition(CobolParser.ClosePortFileIOUsingCloseDispositionContext ctx) {
        return new Cobol.ClosePortFileIOUsingCloseDisposition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.CLOSE_DISPOSITION(), ctx.OF(), ctx.ABORT(), ctx.ORDERLY())
        );
    }

    @Override
    public Object visitCloseReelUnitStatement(CobolParser.CloseReelUnitStatementContext ctx) {
        return new Cobol.CloseReelUnitStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.REEL(), ctx.UNIT(), ctx.FOR(), ctx.REMOVAL(), ctx.WITH(), ctx.NO(), ctx.REWIND(), ctx.LOCK())
        );
    }

    @Override
    public Object visitCloseRelativeStatement(CobolParser.CloseRelativeStatementContext ctx) {
        return new Cobol.CloseRelativeStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.NO(), ctx.REWIND(), ctx.LOCK())
        );
    }

    @Override
    public Object visitCloseStatement(CobolParser.CloseStatementContext ctx) {
        return new Cobol.Close(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CLOSE()),
                convertAll(ctx.closeFile())
        );
    }

    @Override
    public Object visitCodeSetClause(CobolParser.CodeSetClauseContext ctx) {
        return new Cobol.CodeSetClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.CODE_SET(), ctx.IS()),
                (Cobol.Word) visit(ctx.alphabetName())
        );
    }

    @Override
    public Cobol.CollatingSequenceClause visitCollatingSequenceClause(CobolParser.CollatingSequenceClauseContext ctx) {
        return new Cobol.CollatingSequenceClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PROGRAM(), ctx.COLLATING(), ctx.SEQUENCE()),
                (Cobol.Word) visit(ctx.IS()),
                convertAll(ctx.alphabetName()),
                visitNullable(ctx.collatingSequenceClauseAlphanumeric()),
                visitNullable(ctx.collatingSequenceClauseNational())
        );
    }

    @Override
    public Cobol.CollatingSequenceAlphabet visitCollatingSequenceClauseAlphanumeric(CobolParser.CollatingSequenceClauseAlphanumericContext ctx) {
        return new Cobol.CollatingSequenceAlphabet(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FOR(), ctx.ALPHANUMERIC(), ctx.IS()),
                (Identifier) visit(ctx.alphabetName())
        );
    }

    @Override
    public Object visitCollatingSequenceClauseNational(CobolParser.CollatingSequenceClauseNationalContext ctx) {
        return new Cobol.CollatingSequenceAlphabet(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FOR(), ctx.NATIONAL(), ctx.IS()),
                (Identifier) visit(ctx.alphabetName())
        );
    }

    @Override
    public Object visitCombinableCondition(CobolParser.CombinableConditionContext ctx) {
        return new Cobol.CombinableCondition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.NOT()),
                (Cobol) visit(ctx.simpleCondition())
        );
    }

    @Override
    public Object visitCommentEntry(CobolParser.CommentEntryContext ctx) {
        return new Cobol.CommentEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.COMMENTENTRYLINE())
        );
    }

    @Override
    public Object visitCommitmentControlClause(CobolParser.CommitmentControlClauseContext ctx) {
        return new Cobol.CommitmentControlClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.COMMITMENT(), ctx.CONTROL(), ctx.FOR()),
                (Cobol.Word) visit(ctx.fileName())
        );
    }

    @Override
    public Object visitCommunicationDescriptionEntryFormat1(CobolParser.CommunicationDescriptionEntryFormat1Context ctx) {
        return new Cobol.CommunicationDescriptionEntryFormat1(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CD()),
                visitNullable(ctx.cdName()),
                wordsList(ctx.FOR(), ctx.INITIAL(), ctx.INPUT()),
                convertAll(ctx.symbolicQueueClause(),
                        ctx.symbolicSubQueueClause(),
                        ctx.messageDateClause(),
                        ctx.messageTimeClause(),
                        ctx.symbolicSourceClause(),
                        ctx.textLengthClause(),
                        ctx.endKeyClause(),
                        ctx.statusKeyClause(),
                        ctx.messageCountClause(),
                        ctx.dataDescName()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitCommunicationDescriptionEntryFormat2(CobolParser.CommunicationDescriptionEntryFormat2Context ctx) {
        return new Cobol.CommunicationDescriptionEntryFormat2(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CD()),
                visitNullable(ctx.cdName()),
                wordsList(ctx.FOR(), ctx.OUTPUT()),
                convertAll(ctx.destinationCountClause(),
                        ctx.textLengthClause(),
                        ctx.statusKeyClause(),
                        ctx.destinationTableClause(),
                        ctx.errorKeyClause(),
                        ctx.symbolicDestinationClause()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitCommunicationDescriptionEntryFormat3(CobolParser.CommunicationDescriptionEntryFormat3Context ctx) {
        return new Cobol.CommunicationDescriptionEntryFormat3(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CD()),
                visitNullable(ctx.cdName()),
                wordsList(ctx.FOR(), ctx.INITIAL(), ctx.I_O()),
                convertAll(ctx.messageDateClause(),
                        ctx.messageTimeClause(),
                        ctx.symbolicTerminalClause(),
                        ctx.textLengthClause(),
                        ctx.endKeyClause(),
                        ctx.statusKeyClause(),
                        ctx.dataDescName()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitCommunicationSection(CobolParser.CommunicationSectionContext ctx) {
        return new Cobol.CommunicationSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.COMMUNICATION(), ctx.SECTION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.communicationDescriptionEntry(), ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Object visitComputeStatement(CobolParser.ComputeStatementContext ctx) {
        return new Cobol.Compute(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.COMPUTE()),
                convertAll(ctx.computeStore()),
                visit(ctx.EQUALCHAR(), ctx.EQUAL()),
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
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.CombinableCondition) visit(ctx.combinableCondition()),
                convertAll(ctx.andOrCondition())
        );
    }

    @Override
    public Cobol.ConditionNameReference visitConditionNameReference(CobolParser.ConditionNameReferenceContext ctx) {
        return new Cobol.ConditionNameReference(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.conditionName()),
                convertAll(ctx.inData()),
                visitNullable(ctx.inFile()),
                convertAll(ctx.conditionNameSubscriptReference()),
                convertAll(ctx.inMnemonic())
        );
    }

    @Override
    public Cobol visitConditionNameSubscriptReference(CobolParser.ConditionNameSubscriptReferenceContext ctx) {
        return new Cobol.Parenthesized(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.LPARENCHAR()),
                convertAllList(ctx.COMMACHAR(), ctx.subscript()),
                (Cobol.Word) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Cobol.ConfigurationSection visitConfigurationSection(CobolParser.ConfigurationSectionContext ctx) {
        return new Cobol.ConfigurationSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.CONFIGURATION(), ctx.SECTION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.configurationSectionParagraph())
        );
    }

    @Override
    public Object visitContinueStatement(CobolParser.ContinueStatementContext ctx) {
        return new Cobol.Continue(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CONTINUE())
        );
    }

    @Override
    public Cobol.CurrencyClause visitCurrencySignClause(CobolParser.CurrencySignClauseContext ctx) {
        return new Cobol.CurrencyClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.CURRENCY(), ctx.SIGN(), ctx.IS()),
                (Literal) visit(ctx.literal(0)),
                wordsList(ctx.WITH(), ctx.PICTURE(), ctx.SYMBOL()),
                ctx.literal().size() > 1 ? (Literal) visit(ctx.literal(1)) : null
        );
    }

    @Override
    public Object visitDataAlignedClause(CobolParser.DataAlignedClauseContext ctx) {
        return new Cobol.DataAlignedClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ALIGNED())
        );
    }

    @Override
    public Object visitDataBaseSection(CobolParser.DataBaseSectionContext ctx) {
        return new Cobol.DataBaseSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DATA_BASE(), ctx.SECTION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.dataBaseSectionEntry())
        );
    }

    @Override
    public Object visitDataBaseSectionEntry(CobolParser.DataBaseSectionEntryContext ctx) {
        return new Cobol.DataBaseSectionEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.integerLiteral()),
                (Literal) visit(ctx.literal(0)),
                (Cobol.Word) visit(ctx.INVOKE()),
                (Literal) visit(ctx.literal(1))
        );
    }

    @Override
    public Object visitDataBlankWhenZeroClause(CobolParser.DataBlankWhenZeroClauseContext ctx) {
        return new Cobol.DataBlankWhenZeroClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BLANK(), ctx.WHEN(), ctx.ZERO(), ctx.ZEROS(), ctx.ZEROES())
        );
    }

    @Override
    public Object visitDataCommonOwnLocalClause(CobolParser.DataCommonOwnLocalClauseContext ctx) {
        return new Cobol.DataCommonOwnLocalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.COMMON(), ctx.OWN(), ctx.LOCAL())
        );
    }

    @Override
    public Object visitDataDescriptionEntryExecSql(CobolParser.DataDescriptionEntryExecSqlContext ctx) {
        return new Cobol.DataDescriptionEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.EXECSQLLINE()),
                null,
                null,
                visitNullable(ctx.DOT_FS())
        );
    }

    @Override
    public Cobol.DataDescriptionEntry visitDataDescriptionEntryFormat1(CobolParser.DataDescriptionEntryFormat1Context ctx) {
        return new Cobol.DataDescriptionEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LEVEL_NUMBER_77(), ctx.INTEGERLITERAL()),
                ctx.dataName() != null ? (Cobol.Word) visit(ctx.dataName()) :
                        ctx.FILLER() != null ? (Cobol.Word) visit(ctx.FILLER()) : null,
                convertAll(ctx.dataDescriptionEntryFormat1Clause()),
                visitNullable(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitDataDescriptionEntryFormat2(CobolParser.DataDescriptionEntryFormat2Context ctx) {
        return new Cobol.DataDescriptionEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LEVEL_NUMBER_66()),
                (Cobol.Word) visit(ctx.dataName()),
                convertAll(singletonList(ctx.dataRenamesClause())),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitDataDescriptionEntryFormat3(CobolParser.DataDescriptionEntryFormat3Context ctx) {
        return new Cobol.DataDescriptionEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LEVEL_NUMBER_88()),
                (Cobol.Word) visit(ctx.conditionName()),
                convertAll(singletonList(ctx.dataValueClause())),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Cobol.DataDivision visitDataDivision(CobolParser.DataDivisionContext ctx) {
        return new Cobol.DataDivision(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DATA(), ctx.DIVISION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.dataDivisionSection())
        );
    }

    @Override
    public Object visitDataExternalClause(CobolParser.DataExternalClauseContext ctx) {
        return new Cobol.DataExternalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.EXTERNAL(), ctx.BY()),
                visitNullable(ctx.literal())
        );
    }

    @Override
    public Object visitDataGlobalClause(CobolParser.DataGlobalClauseContext ctx) {
        return new Cobol.DataGlobalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.GLOBAL())
        );
    }

    @Override
    public Object visitDataIntegerStringClause(CobolParser.DataIntegerStringClauseContext ctx) {
        return new Cobol.DataIntegerStringClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.INTEGER(), ctx.STRING())
        );
    }

    @Override
    public Object visitDataJustifiedClause(CobolParser.DataJustifiedClauseContext ctx) {
        return new Cobol.DataJustifiedClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.JUSTIFIED(), ctx.JUST(), ctx.RIGHT())
        );
    }

    @Override
    public Object visitDataOccursClause(CobolParser.DataOccursClauseContext ctx) {
        return new Cobol.DataOccursClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.OCCURS()),
                visit(ctx.identifier(), ctx.integerLiteral()),
                visitNullable(ctx.dataOccursTo()),
                visitNullable(ctx.TIMES()),
                visitNullable(ctx.dataOccursDepending()),
                convertAll(ctx.dataOccursSort(), ctx.dataOccursIndexed())
        );
    }

    @Override
    public Object visitDataOccursDepending(CobolParser.DataOccursDependingContext ctx) {
        return new Cobol.DataOccursDepending(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DEPENDING(), ctx.ON()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitDataOccursIndexed(CobolParser.DataOccursIndexedContext ctx) {
        return new Cobol.DataOccursIndexed(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.INDEXED(), ctx.BY(), ctx.LOCAL()),
                convertAll(ctx.indexName())
        );
    }

    @Override
    public Object visitDataOccursSort(CobolParser.DataOccursSortContext ctx) {
        return new Cobol.DataOccursSort(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ASCENDING(), ctx.DESCENDING(), ctx.KEY(), ctx.IS()),
                convertAll(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitDataOccursTo(CobolParser.DataOccursToContext ctx) {
        return new Cobol.DataOccursTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.TO()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.DataPictureClause visitDataPictureClause(CobolParser.DataPictureClauseContext ctx) {
        return new Cobol.DataPictureClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PICTURE(), ctx.PIC(), ctx.IS()),
                convertAll(singletonList(ctx.pictureString()))
        );
    }

    @Override
    public Object visitDataReceivedByClause(CobolParser.DataReceivedByClauseContext ctx) {
        return new Cobol.DataReceivedByClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RECEIVED(), ctx.BY(), ctx.CONTENT(), ctx.REFERENCE(), ctx.REF())
        );
    }

    @Override
    public Object visitDataRecordAreaClause(CobolParser.DataRecordAreaClauseContext ctx) {
        return new Cobol.DataRecordAreaClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RECORD(), ctx.AREA())
        );
    }

    @Override
    public Object visitDataRecordsClause(CobolParser.DataRecordsClauseContext ctx) {
        return new Cobol.DataRecordsClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DATA(), ctx.RECORD(), ctx.IS(), ctx.RECORDS(), ctx.ARE()),
                convertAll(ctx.dataName())
        );
    }

    @Override
    public Object visitDataRedefinesClause(CobolParser.DataRedefinesClauseContext ctx) {
        return new Cobol.DataRedefinesClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.REDEFINES()),
                (Cobol.Word) visit(ctx.dataName())
        );
    }

    @Override
    public Object visitDataRenamesClause(CobolParser.DataRenamesClauseContext ctx) {
        return new Cobol.DataRenamesClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.RENAMES()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName(0)),
                ctx.THROUGH() != null ? (Cobol.Word) visit(ctx.THROUGH()) :
                        ctx.THRU() != null ? (Cobol.Word) visit(ctx.THRU()) : null,
                ctx.qualifiedDataName().size() == 1 ? null : (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName(1))
        );
    }

    @Override
    public Object visitDataSignClause(CobolParser.DataSignClauseContext ctx) {
        return new Cobol.DataSignClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SIGN(), ctx.IS(), ctx.LEADING(), ctx.TRAILING(), ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Object visitDataSynchronizedClause(CobolParser.DataSynchronizedClauseContext ctx) {
        return new Cobol.DataSynchronizedClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SYNCHRONIZED(), ctx.SYNC(), ctx.LEFT(), ctx.RIGHT())
        );
    }

    @Override
    public Object visitDataThreadLocalClause(CobolParser.DataThreadLocalClauseContext ctx) {
        return new Cobol.DataThreadLocalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.THREAD_LOCAL())
        );
    }

    @Override
    public Object visitDataTypeClause(CobolParser.DataTypeClauseContext ctx) {
        return new Cobol.DataTypeClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.TYPE(), ctx.IS(), ctx.SHORT_DATE(), ctx.LONG_DATE(), ctx.NUMERIC_DATE(),
                        ctx.NUMERIC_TIME(), ctx.LONG_TIME(), ctx.CLOB(), ctx.BLOB(), ctx.DBCLOB()),
                ctx.integerLiteral() == null ? null : new Cobol.Parenthesized(
                        randomId(),
                        Space.EMPTY,
                        Markers.EMPTY,
                        (Cobol.Word) visit(ctx.LPARENCHAR()),
                        singletonList((Cobol) visit(ctx.integerLiteral())),
                        (Cobol.Word) visit(ctx.RPARENCHAR())
                )
        );
    }

    @Override
    public Object visitDataTypeDefClause(CobolParser.DataTypeDefClauseContext ctx) {
        return new Cobol.DataTypeDefClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.TYPEDEF())
        );
    }

    @Override
    public Object visitDataUsageClause(CobolParser.DataUsageClauseContext ctx) {
        return new Cobol.DataUsageClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.USAGE(), ctx.IS(), ctx.BINARY(), ctx.TRUNCATED(), ctx.EXTENDED(), ctx.BIT(),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.USING(), ctx.LANGUAGE(), ctx.CONVENTION(), ctx.OF()),
                visit(ctx.cobolWord(), ctx.dataName())
        );
    }

    @Override
    public Object visitDataValueClause(CobolParser.DataValueClauseContext ctx) {
        return new Cobol.DataValueClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.VALUE(), ctx.VALUES(), ctx.IS(), ctx.ARE()),
                convertAllList(ctx.COMMACHAR(), ctx.dataValueInterval())
        );
    }

    @Override
    public Object visitDataValueInterval(CobolParser.DataValueIntervalContext ctx) {
        return new Cobol.DataValueInterval(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.dataValueIntervalFrom()),
                visitNullable(ctx.dataValueIntervalTo())
        );
    }

    @Override
    public Object visitDataValueIntervalTo(CobolParser.DataValueIntervalToContext ctx) {
        return new Cobol.DataValueIntervalTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.THROUGH(), ctx.THRU()),
                (Literal) visit(ctx.literal())
        );
    }

    @Override
    public Object visitDataWithLowerBoundsClause(CobolParser.DataWithLowerBoundsClauseContext ctx) {
        return new Cobol.DataWithLowerBoundsClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.LOWER(), ctx.BOUNDS())
        );
    }

    @Override
    public Object visitDateCompiledParagraph(CobolParser.DateCompiledParagraphContext ctx) {
        return new Cobol.IdentificationDivisionParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.DATE_COMPILED()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                visitNullable(ctx.commentEntry()),
                null,
                null
        );
    }

    @Override
    public Object visitDateWrittenParagraph(CobolParser.DateWrittenParagraphContext ctx) {
        return new Cobol.IdentificationDivisionParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.DATE_WRITTEN()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                visitNullable(ctx.commentEntry()),
                null,
                null
        );
    }

    @Override
    public Cobol.DecimalPointClause visitDecimalPointClause(CobolParser.DecimalPointClauseContext ctx) {
        return new Cobol.DecimalPointClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DECIMAL_POINT(), ctx.IS(), ctx.COMMA())
        );
    }

    @Override
    public Cobol.DefaultComputationalSignClause visitDefaultComputationalSignClause(CobolParser.DefaultComputationalSignClauseContext ctx) {
        return new Cobol.DefaultComputationalSignClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DEFAULT(), ctx.COMPUTATIONAL(), ctx.COMP(), ctx.SIGN(), ctx.IS(),
                        ctx.LEADING(), ctx.TRAILING(), ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Cobol.DefaultDisplaySignClause visitDefaultDisplaySignClause(CobolParser.DefaultDisplaySignClauseContext ctx) {
        return new Cobol.DefaultDisplaySignClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DEFAULT_DISPLAY(), ctx.SIGN(), ctx.IS(), ctx.LEADING(), ctx.TRAILING(),
                        ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Object visitDeleteStatement(CobolParser.DeleteStatementContext ctx) {
        return new Cobol.Delete(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.DELETE()),
                (Name) visit(ctx.fileName()),
                visitNullable(ctx.RECORD()),
                visitNullable(ctx.invalidKeyPhrase()),
                visitNullable(ctx.notInvalidKeyPhrase()),
                visitNullable(ctx.END_DELETE())
        );
    }

    @Override
    public Object visitDestinationCountClause(CobolParser.DestinationCountClauseContext ctx) {
        return new Cobol.DestinationCountClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DESTINATION(), ctx.COUNT(), ctx.IS()),
                visitNullable(ctx.dataDescName())
        );
    }

    @Override
    public Object visitDestinationTableClause(CobolParser.DestinationTableClauseContext ctx) {
        return new Cobol.DestinationTableClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DESTINATION(), ctx.TABLE(), ctx.OCCURS()),
                visitNullable(ctx.integerLiteral()),
                wordsList(ctx.TIMES(), ctx.INDEXED(), ctx.BY()),
                convertAll(ctx.indexName())
        );
    }

    @Override
    public Object visitDisableStatement(CobolParser.DisableStatementContext ctx) {
        return new Cobol.Disable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.DISABLE()),
                wordsList(ctx.INPUT(), ctx.I_O(), ctx.TERMINAL(), ctx.OUTPUT()),
                (Name) visit(ctx.cdName()),
                visitNullable(ctx.WITH()),
                (Cobol.Word) visit(ctx.KEY()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Cobol.ValuedObjectComputerClause visitDiskSizeClause(CobolParser.DiskSizeClauseContext ctx) {
        return new Cobol.ValuedObjectComputerClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                Cobol.ValuedObjectComputerClause.Type.Disk,
                wordsList(ctx.DISK(), ctx.SIZE()),
                ctx.integerLiteral() != null ? (Cobol) visit(ctx.integerLiteral()) :
                        ctx.cobolWord() != null ? (Cobol) visit(ctx.cobolWord()) : null,
                ctx.WORDS() != null ? (Cobol.Word) visit(ctx.WORDS()) :
                        ctx.MODULES() != null ? (Cobol.Word) visit(ctx.MODULES()) : null
        );
    }

    @Override
    public Object visitDisplayAt(CobolParser.DisplayAtContext ctx) {
        return new Cobol.DisplayAt(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.AT()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Cobol.Display visitDisplayStatement(CobolParser.DisplayStatementContext ctx) {
        return new Cobol.Display(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.DISPLAY()),
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
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.UPON()),
                visit(ctx.mnemonicName(), ctx.environmentName())
        );
    }

    @Override
    public Object visitDisplayWith(CobolParser.DisplayWithContext ctx) {
        return wordsList(ctx.WITH(), ctx.NO(), ctx.ADVANCING());
    }

    @Override
    public Object visitDivideByGivingStatement(CobolParser.DivideByGivingStatementContext ctx) {
        return new Cobol.DivideGiving(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.BY()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.divideGivingPhrase())
        );
    }

    @Override
    public Object visitDivideGivingPhrase(CobolParser.DivideGivingPhraseContext ctx) {
        return new Cobol.DivideGivingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.GIVING()),
                convertAll(ctx.divideGiving())
        );
    }

    @Override
    public Object visitDivideIntoGivingStatement(CobolParser.DivideIntoGivingStatementContext ctx) {
        return new Cobol.DivideGiving(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INTO()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.divideGivingPhrase())
        );
    }

    @Override
    public Object visitDivideIntoStatement(CobolParser.DivideIntoStatementContext ctx) {
        return new Cobol.DivideInto(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INTO()),
                convertAll(ctx.divideInto())
        );
    }

    @Override
    public Object visitDivideRemainder(CobolParser.DivideRemainderContext ctx) {
        return new Cobol.DivideRemainder(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.REMAINDER()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitDivideStatement(CobolParser.DivideStatementContext ctx) {
        return new Cobol.Divide(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.DIVIDE()),
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
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ENABLE()),
                wordsList(ctx.INPUT(), ctx.I_O(), ctx.TERMINAL(), ctx.OUTPUT()),
                (Name) visit(ctx.cdName()),
                visitNullable(ctx.WITH()),
                (Cobol.Word) visit(ctx.KEY()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitEndKeyClause(CobolParser.EndKeyClauseContext ctx) {
        return new Cobol.EndKeyClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.END(), ctx.KEY(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Cobol.EndProgram visitEndProgramStatement(CobolParser.EndProgramStatementContext ctx) {
        return new Cobol.EndProgram(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.END(), ctx.PROGRAM()),
                (Name) visit(ctx.programName()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitEntryStatement(CobolParser.EntryStatementContext ctx) {
        return new Cobol.Entry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ENTRY()),
                (Literal) visit(ctx.literal()),
                (Cobol.Word) visit(ctx.USING()),
                convertAll(ctx.identifier())
        );
    }

    @Override
    public Cobol.EnvironmentDivision visitEnvironmentDivision(CobolParser.EnvironmentDivisionContext ctx) {
        return new Cobol.EnvironmentDivision(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ENVIRONMENT(), ctx.DIVISION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.environmentDivisionBody())
        );
    }

    @Override
    public Object visitEnvironmentSwitchNameClause(CobolParser.EnvironmentSwitchNameClauseContext ctx) {
        return new Cobol.EnvironmentSwitchNameClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.environmentName()),
                visitNullable(ctx.IS()),
                visitNullable(ctx.mnemonicName()),
                visitNullable(ctx.environmentSwitchNameSpecialNamesStatusPhrase())
        );
    }

    @Override
    public Object visitEnvironmentSwitchNameSpecialNamesStatusPhrase(CobolParser.EnvironmentSwitchNameSpecialNamesStatusPhraseContext ctx) {
        return new Cobol.EnvironmentSwitchNameSpecialNamesStatusPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(singletonList(ctx.ON()), singletonList(ctx.OFF()), ctx.STATUS(), ctx.IS(), ctx.condition())
        );
    }

    @Override
    public Object visitErrorKeyClause(CobolParser.ErrorKeyClauseContext ctx) {
        return new Cobol.ErrorKeyClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ERROR(), ctx.KEY(), ctx.IS()),
                (Name) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitEvaluateAlsoCondition(CobolParser.EvaluateAlsoConditionContext ctx) {
        return new Cobol.EvaluateAlsoCondition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ALSO()),
                (Cobol.EvaluateCondition) visit(ctx.evaluateCondition())
        );
    }

    @Override
    public Object visitEvaluateAlsoSelect(CobolParser.EvaluateAlsoSelectContext ctx) {
        return new Cobol.EvaluateAlso(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ALSO()),
                (Cobol) visit(ctx.evaluateSelect())
        );
    }

    @Override
    public Object visitEvaluateCondition(CobolParser.EvaluateConditionContext ctx) {
        return new Cobol.EvaluateCondition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ANY(), ctx.NOT()),
                ctx.ANY() != null ? null : visit(ctx.evaluateValue(), ctx.condition(), ctx.booleanLiteral()),
                visitNullable(ctx.evaluateThrough())
        );
    }

    @Override
    public Object visitEvaluateStatement(CobolParser.EvaluateStatementContext ctx) {
        return new Cobol.Evaluate(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.EVALUATE()),
                (Cobol) visit(ctx.evaluateSelect()),
                convertAll(ctx.evaluateAlsoSelect()),
                convertAll(ctx.evaluateWhenPhrase()),
                visitNullable(ctx.evaluateWhenOther()),
                visitNullable(ctx.END_EVALUATE())
        );
    }

    @Override
    public Object visitEvaluateThrough(CobolParser.EvaluateThroughContext ctx) {
        return new Cobol.EvaluateThrough(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.THROUGH(), ctx.THRU()),
                (Cobol) visit(ctx.evaluateValue())
        );
    }

    @Override
    public Object visitEvaluateWhen(CobolParser.EvaluateWhenContext ctx) {
        return new Cobol.EvaluateWhen(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.WHEN()),
                (Cobol.EvaluateCondition) visit(ctx.evaluateCondition()),
                convertAll(ctx.evaluateAlsoCondition())
        );
    }

    @Override
    public Object visitEvaluateWhenOther(CobolParser.EvaluateWhenOtherContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WHEN(), ctx.OTHER()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitEvaluateWhenPhrase(CobolParser.EvaluateWhenPhraseContext ctx) {
        return new Cobol.EvaluateWhenPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.evaluateWhen()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitExecCicsStatement(CobolParser.ExecCicsStatementContext ctx) {
        return new Cobol.ExecCicsStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.EXECCICSLINE())
        );
    }

    @Override
    public Object visitExecSqlImsStatement(CobolParser.ExecSqlImsStatementContext ctx) {
        return new Cobol.ExecSqlImsStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.EXECSQLIMSLINE())
        );
    }

    @Override
    public Object visitExecSqlStatement(CobolParser.ExecSqlStatementContext ctx) {
        return new Cobol.ExecSqlStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.EXECSQLLINE())
        );
    }

    @Override
    public Cobol.Exhibit visitExhibitStatement(CobolParser.ExhibitStatementContext ctx) {
        return new Cobol.Exhibit(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.EXHIBIT(), ctx.NAMED(), ctx.CHANGED()),
                convertAll(ctx.exhibitOperand())
        );
    }

    @Override
    public Object visitExitStatement(CobolParser.ExitStatementContext ctx) {
        return new Cobol.Exit(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.EXIT(), ctx.PROGRAM())
        );
    }

    @Override
    public Object visitExternalClause(CobolParser.ExternalClauseContext ctx) {
        return new Cobol.ExternalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.EXTERNAL())
        );
    }

    @Override
    public Object visitFigurativeConstant(CobolParser.FigurativeConstantContext ctx) {
        return new Cobol.FigurativeConstant(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.ALL(), ctx.HIGH_VALUE(), ctx.HIGH_VALUES(), ctx.LOW_VALUE(), ctx.LOW_VALUES(),
                        ctx.NULL(), ctx.NULLS(), ctx.QUOTE(), ctx.QUOTES(),
                        ctx.SPACE(), ctx.SPACES(), ctx.ZERO(), ctx.ZEROS(), ctx.ZEROES()),
                visitNullable(ctx.literal())
        );
    }

    @Override
    public Object visitFileControlEntry(CobolParser.FileControlEntryContext ctx) {
        return new Cobol.FileControlEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol) visit(ctx.selectClause()),
                convertAll(ctx.fileControlClause())
        );
    }

    @Override
    public Object visitFileControlParagraph(CobolParser.FileControlParagraphContext ctx) {
        return new Cobol.FileControlParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.FILE_CONTROL()),
                convertAllList(ctx.DOT_FS(), ctx.fileControlEntry())
        );
    }

    @Override
    public Cobol.FileDescriptionEntry visitFileDescriptionEntry(CobolParser.FileDescriptionEntryContext ctx) {
        return new Cobol.FileDescriptionEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.FD(), ctx.SD()),
                (Cobol.Word) visit(ctx.fileName()),
                convertAllList(ctx.DOT_FS(), ctx.fileDescriptionEntryClause()),
                convertAll(ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Cobol.FileSection visitFileSection(CobolParser.FileSectionContext ctx) {
        return new Cobol.FileSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FILE(), ctx.SECTION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.fileDescriptionEntry())
        );
    }

    @Override
    public Object visitFileStatusClause(CobolParser.FileStatusClauseContext ctx) {
        return new Cobol.FileStatusClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FILE(), ctx.STATUS(), ctx.IS()),
                convertAll(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.FunctionCall visitFunctionCall(CobolParser.FunctionCallContext ctx) {
        return new Cobol.FunctionCall(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.FUNCTION()),
                (Cobol.Word) visit(ctx.functionName()),
                convertAll(ctx.functionCallArguments()),
                visitNullable(ctx.referenceModifier())
        );
    }

    @Override
    public Object visitFunctionCallArguments(CobolParser.FunctionCallArgumentsContext ctx) {
        return new Cobol.Parenthesized(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.LPARENCHAR()),
                convertAllList(ctx.COMMACHAR(), ctx.argument()),
                (Cobol.Word) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitGenerateStatement(CobolParser.GenerateStatementContext ctx) {
        return new Cobol.Generate(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.GENERATE()),
                (Cobol.QualifiedDataName) visit(ctx.reportName())
        );
    }

    @Override
    public Object visitGlobalClause(CobolParser.GlobalClauseContext ctx) {
        return new Cobol.GlobalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.GLOBAL())
        );
    }

    @Override
    public Object visitGoToDependingOnStatement(CobolParser.GoToDependingOnStatementContext ctx) {
        return new Cobol.GoToDependingOnStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.procedureName()),
                wordsList(ctx.MORE_LABELS(), ctx.DEPENDING(), ctx.ON()),
                visitNullable(ctx.identifier())
        );
    }

    @Override
    public Object visitGoToStatement(CobolParser.GoToStatementContext ctx) {
        return new Cobol.GoTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.GO(), ctx.TO()),
                visit(ctx.goToStatementSimple(), ctx.goToDependingOnStatement())
        );
    }

    @Override
    public Object visitGobackStatement(CobolParser.GobackStatementContext ctx) {
        return new Cobol.GoBack(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.GOBACK())
        );
    }

    @Override
    public Cobol visitIdentificationDivision(CobolParser.IdentificationDivisionContext ctx) {
        return new Cobol.IdentificationDivision(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IDENTIFICATION(), ctx.ID(), ctx.DIVISION(), ctx.DOT_FS()),
                (Cobol.ProgramIdParagraph) visit(ctx.programIdParagraph()),
                convertAll(ctx.identificationDivisionBody())
        );
    }

    @Override
    public Object visitIfElse(CobolParser.IfElseContext ctx) {
        return new Cobol.IfElse(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ELSE()),
                wordsList(ctx.NEXT(), ctx.SENTENCE()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitIfStatement(CobolParser.IfStatementContext ctx) {
        return new Cobol.If(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.IF()),
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
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.THEN()),
                wordsList(ctx.NEXT(), ctx.SENTENCE()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitInData(CobolParser.InDataContext ctx) {
        return new Cobol.InData(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.dataName())
        );
    }

    @Override
    public Object visitInFile(CobolParser.InFileContext ctx) {
        return new Cobol.InFile(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.fileName())
        );
    }

    @Override
    public Object visitInLibrary(CobolParser.InLibraryContext ctx) {
        return new Cobol.InLibrary(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.libraryName())
        );
    }

    @Override
    public Object visitInMnemonic(CobolParser.InMnemonicContext ctx) {
        return new Cobol.InMnemonic(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Cobol.InSection visitInSection(CobolParser.InSectionContext ctx) {
        return new Cobol.InSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.sectionName())
        );
    }

    @Override
    public Object visitInTable(CobolParser.InTableContext ctx) {
        return new Cobol.InLibrary(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.IN(), ctx.OF()),
                (Name) visit(ctx.tableCall())
        );
    }

    @Override
    public Object visitInstallationParagraph(CobolParser.InstallationParagraphContext ctx) {
        return new Cobol.IdentificationDivisionParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INSTALLATION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                visitNullable(ctx.commentEntry()),
                null,
                null
        );
    }

    @Override
    public Object visitInitializeReplacingBy(CobolParser.InitializeReplacingByContext ctx) {
        return new Cobol.InitializeReplacingBy(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ALPHABETIC(), ctx.ALPHANUMERIC(), ctx.ALPHANUMERIC_EDITED(),
                        ctx.NATIONAL(), ctx.NATIONAL_EDITED(), ctx.NUMERIC(), ctx.NUMERIC_EDITED(),
                        ctx.DBCS(), ctx.EGCS(), ctx.DATA(), ctx.BY()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitInitializeReplacingPhrase(CobolParser.InitializeReplacingPhraseContext ctx) {
        return new Cobol.InitializeReplacingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.REPLACING()),
                convertAll(ctx.initializeReplacingBy())
        );
    }

    @Override
    public Object visitInitializeStatement(CobolParser.InitializeStatementContext ctx) {
        return new Cobol.Initialize(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INITIALIZE()),
                convertAll(ctx.identifier()),
                visitNullable(ctx.initializeReplacingPhrase())
        );
    }

    @Override
    public Object visitInitiateStatement(CobolParser.InitiateStatementContext ctx) {
        return new Cobol.Initiate(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INITIATE()),
                convertAll(ctx.reportName())
        );
    }

    @Override
    public Object visitInputOutputSection(CobolParser.InputOutputSectionContext ctx) {
        return new Cobol.InputOutputSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.INPUT_OUTPUT(), ctx.SECTION(), ctx.DOT_FS()),
                convertAll(ctx.inputOutputSectionParagraph())
        );
    }

    @Override
    public Object visitInspectAllLeading(CobolParser.InspectAllLeadingContext ctx) {
        return new Cobol.InspectAllLeading(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                convertAll(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectAllLeadings(CobolParser.InspectAllLeadingsContext ctx) {
        return new Cobol.InspectAllLeadings(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.ALL(), ctx.LEADING()),
                convertAll(ctx.inspectAllLeading())
        );
    }

    @Override
    public Object visitInspectBeforeAfter(CobolParser.InspectBeforeAfterContext ctx) {
        return new Cobol.InspectBeforeAfter(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BEFORE(), ctx.AFTER(), ctx.INITIAL()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitInspectBy(CobolParser.InspectByContext ctx) {
        return new Cobol.InspectBy(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.BY()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitInspectCharacters(CobolParser.InspectCharactersContext ctx) {
        return new Cobol.InspectCharacters(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.CHARACTER(), ctx.CHARACTERS()),
                convertAll(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectConvertingPhrase(CobolParser.InspectConvertingPhraseContext ctx) {
        return new Cobol.InspectConvertingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.CONVERTING()),
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.InspectTo) visit(ctx.inspectTo()),
                convertAll(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectFor(CobolParser.InspectForContext ctx) {
        return new Cobol.InspectFor(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Identifier) visit(ctx.identifier()),
                (Cobol.Word) visit(ctx.FOR()),
                convertAll(ctx.inspectCharacters(), ctx.inspectAllLeadings())
        );
    }

    @Override
    public Object visitInspectReplacingAllLeading(CobolParser.InspectReplacingAllLeadingContext ctx) {
        return new Cobol.InspectReplacingAllLeading(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.InspectBy) visit(ctx.inspectBy()),
                convertAll(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectReplacingAllLeadings(CobolParser.InspectReplacingAllLeadingsContext ctx) {
        return new Cobol.InspectReplacingAllLeadings(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.ALL(), ctx.LEADING(), ctx.FIRST()),
                convertAll(ctx.inspectReplacingAllLeading())
        );
    }

    @Override
    public Object visitInspectReplacingCharacters(CobolParser.InspectReplacingCharactersContext ctx) {
        return new Cobol.InspectReplacingCharacters(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.CHARACTER(), ctx.CHARACTERS()),
                (Cobol.InspectBy) visit(ctx.inspectBy()),
                convertAll(ctx.inspectBeforeAfter())
        );
    }

    @Override
    public Object visitInspectReplacingPhrase(CobolParser.InspectReplacingPhraseContext ctx) {
        return new Cobol.InspectReplacingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.REPLACING()),
                convertAll(ctx.inspectReplacingCharacters(), ctx.inspectReplacingAllLeadings())
        );
    }

    @Override
    public Object visitInspectStatement(CobolParser.InspectStatementContext ctx) {
        return new Cobol.Inspect(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INSPECT()),
                (Identifier) visit(ctx.identifier()),
                visit(ctx.inspectTallyingPhrase(), ctx.inspectReplacingPhrase(),
                        ctx.inspectTallyingReplacingPhrase(), ctx.inspectConvertingPhrase())
        );
    }

    @Override
    public Object visitInspectTallyingPhrase(CobolParser.InspectTallyingPhraseContext ctx) {
        return new Cobol.InspectTallyingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.TALLYING()),
                convertAll(ctx.inspectFor())
        );
    }

    @Override
    public Object visitInspectTallyingReplacingPhrase(CobolParser.InspectTallyingReplacingPhraseContext ctx) {
        return new Cobol.InspectTallyingReplacingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.TALLYING()),
                convertAll(ctx.inspectFor()),
                convertAll(ctx.inspectReplacingPhrase())
        );
    }

    @Override
    public Object visitInspectTo(CobolParser.InspectToContext ctx) {
        return new Cobol.InspectTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.TO()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Cobol.StatementPhrase visitInvalidKeyPhrase(CobolParser.InvalidKeyPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.INVALID(), ctx.KEY()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitIoControlParagraph(CobolParser.IoControlParagraphContext ctx) {
        return new Cobol.IoControlParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.I_O_CONTROL()),
                (Cobol.Word) visit(ctx.DOT_FS(0)),
                visitNullable(ctx.fileName()),
                ctx.fileName() == null ? null : (Cobol.Word) visit(ctx.DOT_FS(1)),
                convertAll(ctx.ioControlClause()),
                ctx.ioControlClause().isEmpty() ? null : (Cobol.Word) visit(ctx.DOT_FS(ctx.DOT_FS().size() == 2 ? 1 : 2))
        );
    }

    @Override
    public Object visitLabelRecordsClause(CobolParser.LabelRecordsClauseContext ctx) {
        return new Cobol.LabelRecordsClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LABEL(), ctx.RECORD(), ctx.IS(), ctx.RECORDS(), ctx.ARE(), ctx.OMITTED(), ctx.STANDARD()),
                convertAll(ctx.dataName())
        );
    }

    @Override
    public Object visitLibraryAttributeClauseFormat1(CobolParser.LibraryAttributeClauseFormat1Context ctx) {
        return new Cobol.LibraryAttributeClauseFormat1(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ATTRIBUTE(), ctx.SHARING(), ctx.IS(), ctx.DONTCARE(), ctx.PRIVATE(), ctx.SHAREDBYRUNUNIT(), ctx.SHAREDBYALL())
        );
    }

    @Override
    public Object visitLibraryAttributeClauseFormat2(CobolParser.LibraryAttributeClauseFormat2Context ctx) {
        return new Cobol.LibraryAttributeClauseFormat2(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ATTRIBUTE()),
                visitNullable(ctx.libraryAttributeFunction()),
                wordsList(ctx.LIBACCESS(), ctx.IS(), ctx.BYFUNCTION(), ctx.BYTITLE()),
                visitNullable(ctx.libraryAttributeParameter()),
                visitNullable(ctx.libraryAttributeTitle())
        );
    }

    @Override
    public Object visitLibraryAttributeFunction(CobolParser.LibraryAttributeFunctionContext ctx) {
        return new Cobol.LibraryAttributeFunction(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FUNCTIONNAME(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitLibraryAttributeParameter(CobolParser.LibraryAttributeParameterContext ctx) {
        return new Cobol.LibraryAttributeParameter(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LIBPARAMETER(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitLibraryAttributeTitle(CobolParser.LibraryAttributeTitleContext ctx) {
        return new Cobol.LibraryAttributeTitle(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.TITLE(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitLibraryDescriptionEntryFormat1(CobolParser.LibraryDescriptionEntryFormat1Context ctx) {
        return new Cobol.LibraryDescriptionEntryFormat1(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.LD()),
                (Cobol.Word) visit(ctx.libraryName()),
                (Cobol.Word) visit(ctx.EXPORT()),
                visitNullable(ctx.libraryAttributeClauseFormat1()),
                visitNullable(ctx.libraryEntryProcedureClauseFormat1())
        );
    }

    @Override
    public Object visitLibraryDescriptionEntryFormat2(CobolParser.LibraryDescriptionEntryFormat2Context ctx) {
        return new Cobol.LibraryDescriptionEntryFormat2(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.LB()),
                (Cobol.Word) visit(ctx.libraryName()),
                (Cobol.Word) visit(ctx.IMPORT()),
                visitNullable(ctx.libraryIsGlobalClause()),
                visitNullable(ctx.libraryIsCommonClause()),
                convertAll(ctx.libraryAttributeClauseFormat2(), ctx.libraryEntryProcedureClauseFormat2())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureClauseFormat1(CobolParser.LibraryEntryProcedureClauseFormat1Context ctx) {
        return new Cobol.LibraryEntryProcedureClauseFormat1(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ENTRY_PROCEDURE()),
                (Cobol.Word) visit(ctx.programName()),
                visitNullable(ctx.libraryEntryProcedureForClause())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureClauseFormat2(CobolParser.LibraryEntryProcedureClauseFormat2Context ctx) {
        return new Cobol.LibraryEntryProcedureClauseFormat2(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ENTRY_PROCEDURE()),
                (Cobol.Word) visit(ctx.programName()),
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
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.FOR()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureGivingClause(CobolParser.LibraryEntryProcedureGivingClauseContext ctx) {
        return new Cobol.LibraryEntryProcedureGivingClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.GIVING()),
                (Cobol.Word) visit(ctx.dataName())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureUsingClause(CobolParser.LibraryEntryProcedureUsingClauseContext ctx) {
        return new Cobol.LibraryEntryProcedureUsingClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.USING()),
                convertAll(ctx.libraryEntryProcedureUsingName())
        );
    }

    @Override
    public Object visitLibraryEntryProcedureWithClause(CobolParser.LibraryEntryProcedureWithClauseContext ctx) {
        return new Cobol.LibraryEntryProcedureWithClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.WITH()),
                convertAll(ctx.libraryEntryProcedureWithName())
        );
    }

    @Override
    public Object visitLibraryIsCommonClause(CobolParser.LibraryIsCommonClauseContext ctx) {
        return new Cobol.LibraryIsCommonClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.COMMON())
        );
    }

    @Override
    public Object visitLibraryIsGlobalClause(CobolParser.LibraryIsGlobalClauseContext ctx) {
        return new Cobol.LibraryIsGlobalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.GLOBAL())
        );
    }

    @Override
    public Object visitLinageClause(CobolParser.LinageClauseContext ctx) {
        return new Cobol.LinageClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LINAGE(), ctx.IS()),
                visit(ctx.dataName(), ctx.integerLiteral()),
                visitNullable(ctx.LINES()),
                convertAll(ctx.linageAt())
        );
    }

    @Override
    public Object visitLinageFootingAt(CobolParser.LinageFootingAtContext ctx) {
        return new Cobol.LinageFootingAt(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.FOOTING(), ctx.AT()),
                visit(ctx.dataName(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitLinageLinesAtBottom(CobolParser.LinageLinesAtBottomContext ctx) {
        return new Cobol.LinageLinesAtBottom(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LINES(), ctx.AT(), ctx.BOTTOM()),
                visit(ctx.dataName(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitLinageLinesAtTop(CobolParser.LinageLinesAtTopContext ctx) {
        return new Cobol.LinageLinesAtTop(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LINES(), ctx.AT(), ctx.TOP()),
                visit(ctx.dataName(), ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.LinkageSection visitLinkageSection(CobolParser.LinkageSectionContext ctx) {
        return new Cobol.LinkageSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LINKAGE(), ctx.SECTION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Cobol.LocalStorageSection visitLocalStorageSection(CobolParser.LocalStorageSectionContext ctx) {
        return new Cobol.LocalStorageSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LOCAL_STORAGE(), ctx.SECTION()),
                (Cobol.Word) visit(ctx.DOT_FS().get(0)),
                visitNullable(ctx.LD()),
                visitNullable(ctx.localName()),
                ctx.DOT_FS().size() == 1 ? null : (Cobol.Word) visit(ctx.DOT_FS().get(1)),
                convertAll(ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Cobol.ValuedObjectComputerClause visitMemorySizeClause(CobolParser.MemorySizeClauseContext ctx) {
        return new Cobol.ValuedObjectComputerClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                Cobol.ValuedObjectComputerClause.Type.Memory,
                wordsList(ctx.MEMORY(), ctx.SIZE()),
                (Cobol) visit(ctx.integerLiteral() == null ? ctx.cobolWord() : ctx.integerLiteral()),
                ctx.WORDS() != null ? (Cobol.Word) visit(ctx.WORDS()) :
                        ctx.CHARACTERS() != null ? (Cobol.Word) visit(ctx.CHARACTERS()) :
                                ctx.MODULES() != null ? (Cobol.Word) visit(ctx.MODULES()) : null
        );
    }

    @Override
    public Cobol.Mergeable visitMergeCollatingAlphanumeric(CobolParser.MergeCollatingAlphanumericContext ctx) {
        return new Cobol.Mergeable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FOR(), ctx.ALPHANUMERIC(), ctx.IS()),
                (Name) visit(ctx.alphabetName())
        );
    }

    @Override
    public Cobol.Mergeable visitMergeCollatingNational(CobolParser.MergeCollatingNationalContext ctx) {
        return new Cobol.Mergeable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FOR(), ctx.NATIONAL(), ctx.IS()),
                (Name) visit(ctx.alphabetName())
        );
    }

    @Override
    public Cobol.MergeCollatingSequencePhrase visitMergeCollatingSequencePhrase(CobolParser.MergeCollatingSequencePhraseContext ctx) {
        return new Cobol.MergeCollatingSequencePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.COLLATING(), ctx.SEQUENCE(), ctx.IS()),
                convertAll(ctx.alphabetName()),
                visitNullable(ctx.mergeCollatingAlphanumeric()),
                visitNullable(ctx.mergeCollatingNational())
        );
    }

    @Override
    public Cobol.MergeGiving visitMergeGiving(CobolParser.MergeGivingContext ctx) {
        return new Cobol.MergeGiving(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.fileName()),
                wordsList(ctx.LOCK(), ctx.SAVE(), ctx.NO(), ctx.REWIND(), ctx.RELEASE(), ctx.WITH(), ctx.REMOVE(), ctx.CRUNCH())
        );
    }

    @Override
    public Cobol.MergeGivingPhrase visitMergeGivingPhrase(CobolParser.MergeGivingPhraseContext ctx) {
        return new Cobol.MergeGivingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.GIVING()),
                convertAll(ctx.mergeGiving())
        );
    }

    @Override
    public Cobol.MergeOnKeyClause visitMergeOnKeyClause(CobolParser.MergeOnKeyClauseContext ctx) {
        return new Cobol.MergeOnKeyClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ON(), ctx.ASCENDING(), ctx.DESCENDING(), ctx.KEY()),
                convertAll(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.MergeOutputProcedurePhrase visitMergeOutputProcedurePhrase(CobolParser.MergeOutputProcedurePhraseContext ctx) {
        return new Cobol.MergeOutputProcedurePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.OUTPUT(), ctx.PROCEDURE(), ctx.IS()),
                visitProcedureName(ctx.procedureName()),
                visitNullable(ctx.mergeOutputThrough())
        );
    }

    @Override
    public Cobol.MergeOutputThrough visitMergeOutputThrough(CobolParser.MergeOutputThroughContext ctx) {
        return new Cobol.MergeOutputThrough(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.THROUGH(), ctx.THRU()),
                visitProcedureName(ctx.procedureName())
        );
    }

    @Override
    public Cobol.Merge visitMergeStatement(CobolParser.MergeStatementContext ctx) {
        return new Cobol.Merge(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.MERGE()),
                (Name) visit(ctx.fileName()),
                convertAll(ctx.mergeOnKeyClause()),
                visitNullable(ctx.mergeCollatingSequencePhrase()),
                convertAll(ctx.mergeUsing()),
                visitNullable(ctx.mergeOutputProcedurePhrase()),
                convertAll(ctx.mergeGivingPhrase())
        );
    }

    @Override
    public Cobol.MergeUsing visitMergeUsing(CobolParser.MergeUsingContext ctx) {
        return new Cobol.MergeUsing(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.USING()),
                convertAll(ctx.fileName())
        );
    }

    @Override
    public Object visitMessageCountClause(CobolParser.MessageCountClauseContext ctx) {
        return new Cobol.MessageCountClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.MESSAGE(), ctx.COUNT(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitMessageDateClause(CobolParser.MessageDateClauseContext ctx) {
        return new Cobol.MessageDateClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.MESSAGE(), ctx.DATE(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitMessageTimeClause(CobolParser.MessageTimeClauseContext ctx) {
        return new Cobol.MessageTimeClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.MESSAGE(), ctx.TIME(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Cobol.MoveCorrespondingToStatement visitMoveCorrespondingToStatement(CobolParser.MoveCorrespondingToStatementContext ctx) {
        return new Cobol.MoveCorrespondingToStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.CORRESPONDING(), ctx.CORR()),
                (Identifier) visit(ctx.moveCorrespondingToSendingArea()),
                (Cobol.Word) visit(ctx.TO()),
                convertAll(ctx.identifier())
        );
    }

    @Override
    public Cobol.MoveStatement visitMoveStatement(CobolParser.MoveStatementContext ctx) {
        return new Cobol.MoveStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.MOVE(), ctx.ALL()),
                visit(ctx.moveCorrespondingToStatement(), ctx.moveToStatement())
        );
    }

    @Override
    public Cobol.MoveToStatement visitMoveToStatement(CobolParser.MoveToStatementContext ctx) {
        return new Cobol.MoveToStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.moveToSendingArea()),
                (Cobol.Word) visit(ctx.TO()),
                convertAll(ctx.identifier())
        );
    }

    @Override
    public Object visitMultDiv(CobolParser.MultDivContext ctx) {
        return new Cobol.MultDiv(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.ASTERISKCHAR(), ctx.SLASHCHAR()),
                (Cobol.Powers) visit(ctx.powers())
        );
    }

    @Override
    public Object visitMultDivs(CobolParser.MultDivsContext ctx) {
        return new Cobol.MultDivs(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Powers) visit(ctx.powers()),
                convertAll(ctx.multDiv())
        );
    }

    @Override
    public Object visitMultipleFileClause(CobolParser.MultipleFileClauseContext ctx) {
        return new Cobol.MultipleFileClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.MULTIPLE(), ctx.FILE(), ctx.TAPE(), ctx.CONTAINS()),
                convertAll(ctx.multipleFilePosition())
        );
    }

    @Override
    public Object visitMultipleFilePosition(CobolParser.MultipleFilePositionContext ctx) {
        return new Cobol.MultipleFilePosition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.fileName()),
                visitNullable(ctx.POSITION()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.MultiplyGiving visitMultiplyGiving(CobolParser.MultiplyGivingContext ctx) {
        return new Cobol.MultiplyGiving(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.multiplyGivingOperand()),
                (Cobol.Word) visit(ctx.GIVING()),
                convertAll(ctx.multiplyGivingResult())
        );
    }

    @Override
    public Cobol.MultiplyRegular visitMultiplyRegular(CobolParser.MultiplyRegularContext ctx) {
        return new Cobol.MultiplyRegular(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.multiplyRegularOperand())
        );
    }

    @Override
    public Cobol.Multiply visitMultiplyStatement(CobolParser.MultiplyStatementContext ctx) {
        return new Cobol.Multiply(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.MULTIPLY()),
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.Word) visit(ctx.BY()),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NEXT(), ctx.SENTENCE())
        );
    }

    @Override
    public Cobol.StatementPhrase visitNotAtEndPhrase(CobolParser.NotAtEndPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NOT(), ctx.AT(), ctx.END()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Cobol.StatementPhrase visitNotInvalidKeyPhrase(CobolParser.NotInvalidKeyPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NOT(), ctx.INVALID(), ctx.KEY()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitNotOnExceptionClause(CobolParser.NotOnExceptionClauseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NOT(), ctx.ON(), ctx.EXCEPTION()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Cobol.StatementPhrase visitNotOnOverflowPhrase(CobolParser.NotOnOverflowPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NOT(), ctx.ON(), ctx.OVERFLOW()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Cobol.StatementPhrase visitNotOnSizeErrorPhrase(CobolParser.NotOnSizeErrorPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NOT(), ctx.ON(), ctx.SIZE(), ctx.ERROR()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Cobol.ObjectComputer visitObjectComputerParagraph(CobolParser.ObjectComputerParagraphContext ctx) {
        return new Cobol.ObjectComputer(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.OBJECT_COMPUTER(), ctx.DOT_FS(0)),
                ctx.computerName() == null ? null :
                        new Cobol.ObjectComputerDefinition(
                                randomId(),
                                Space.EMPTY,
                                Markers.EMPTY,
                                (Cobol.Word) visit(ctx.computerName()),
                                convertAll(ctx.objectComputerClause())
                        ),
                ctx.DOT_FS().size() == 1 ? null : (Cobol.Word) visit(ctx.DOT_FS(1))
        );
    }

    @Override
    public Cobol.OdtClause visitOdtClause(CobolParser.OdtClauseContext ctx) {
        return new Cobol.OdtClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ODT(), ctx.IS()),
                (Identifier) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Object visitOnExceptionClause(CobolParser.OnExceptionClauseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ON(), ctx.EXCEPTION()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Cobol.StatementPhrase visitOnOverflowPhrase(CobolParser.OnOverflowPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ON(), ctx.OVERFLOW()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitOnSizeErrorPhrase(CobolParser.OnSizeErrorPhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ON(), ctx.SIZE(), ctx.ERROR()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Cobol.OpenIOExtendStatement visitOpenExtendStatement(CobolParser.OpenExtendStatementContext ctx) {
        return new Cobol.OpenIOExtendStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.EXTEND()),
                convertAll(ctx.fileName())
        );
    }

    @Override
    public Cobol.OpenIOExtendStatement visitOpenIOStatement(CobolParser.OpenIOStatementContext ctx) {
        return new Cobol.OpenIOExtendStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.I_O()),
                convertAll(ctx.fileName())
        );
    }

    @Override
    public Cobol.Openable visitOpenInput(CobolParser.OpenInputContext ctx) {
        return new Cobol.Openable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.fileName()),
                wordsList(ctx.REVERSED(), ctx.WITH(), ctx.NO(), ctx.REWIND())
        );
    }

    @Override
    public Cobol.OpenInputOutputStatement visitOpenInputStatement(CobolParser.OpenInputStatementContext ctx) {
        return new Cobol.OpenInputOutputStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INPUT()),
                convertAll(ctx.openInput())
        );
    }

    @Override
    public Cobol.Openable visitOpenOutput(CobolParser.OpenOutputContext ctx) {
        return new Cobol.Openable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.fileName()),
                wordsList(ctx.WITH(), ctx.NO(), ctx.REWIND())
        );
    }

    @Override
    public Cobol.OpenInputOutputStatement visitOpenOutputStatement(CobolParser.OpenOutputStatementContext ctx) {
        return new Cobol.OpenInputOutputStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.OUTPUT()),
                convertAll(ctx.openOutput())
        );
    }

    @Override
    public Cobol.Open visitOpenStatement(CobolParser.OpenStatementContext ctx) {
        return new Cobol.Open(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.OPEN()),
                convertAll(ctx.openInputStatement(), ctx.openOutputStatement(), ctx.openIOStatement(),
                        ctx.openExtendStatement())
        );
    }

    @Override
    public Object visitOrganizationClause(CobolParser.OrganizationClauseContext ctx) {
        return new Cobol.OrganizationClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ORGANIZATION(), ctx.IS(), ctx.LINE(), ctx.RECORD(), ctx.BINARY(),
                        ctx.SEQUENTIAL(), ctx.RELATIVE(), ctx.INDEXED())
        );
    }

    @Override
    public Object visitPaddingCharacterClause(CobolParser.PaddingCharacterClauseContext ctx) {
        return new Cobol.PaddingCharacterClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PADDING(), ctx.CHARACTER(), ctx.IS()),
                visit(ctx.qualifiedDataName(), ctx.literal())
        );
    }

    @Override
    public Object visitParagraph(CobolParser.ParagraphContext ctx) {
        return new Cobol.Paragraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.paragraphName()),
                visitNullable(ctx.DOT_FS()),
                visitNullable(ctx.alteredGoTo()),
                convertAll(ctx.sentence())
        );
    }

    @Override
    public Cobol.Paragraphs visitParagraphs(CobolParser.ParagraphsContext ctx) {
        return new Cobol.Paragraphs(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.sentence()),
                convertAll(ctx.paragraph())
        );
    }

    @Override
    public Object visitPasswordClause(CobolParser.PasswordClauseContext ctx) {
        return new Cobol.PasswordClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PASSWORD(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataName())
        );
    }

    @Override
    public Cobol.Performable visitPerformAfter(CobolParser.PerformAfterContext ctx) {
        return new Cobol.Performable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.AFTER()),
                (Cobol) visit(ctx.performVaryingPhrase())
        );
    }

    @Override
    public Cobol.Performable visitPerformBy(CobolParser.PerformByContext ctx) {
        return new Cobol.Performable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.BY()),
                visit(ctx.identifier(), ctx.literal(), ctx.arithmeticExpression())
        );
    }

    @Override
    public Cobol.Performable visitPerformFrom(CobolParser.PerformFromContext ctx) {
        return new Cobol.Performable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.FROM()),
                visit(ctx.identifier(), ctx.literal(), ctx.arithmeticExpression())
        );
    }

    @Override
    public Cobol.PerformInlineStatement visitPerformInlineStatement(CobolParser.PerformInlineStatementContext ctx) {
        return new Cobol.PerformInlineStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.performType()),
                convertAll(ctx.statement()),
                (Cobol.Word) visit(ctx.END_PERFORM())
        );
    }

    @Override
    public Cobol.PerformProcedureStatement visitPerformProcedureStatement(CobolParser.PerformProcedureStatementContext ctx) {
        return new Cobol.PerformProcedureStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.ProcedureName) visit(ctx.procedureName(0)),
                ctx.THROUGH() != null ? (Cobol.Word) visit(ctx.THROUGH()) :
                        ctx.THRU() != null ? (Cobol.Word) visit(ctx.THRU()) : null,
                (ctx.procedureName().size() > 1) ? (Cobol.ProcedureName) visit(ctx.procedureName(1)) : null,
                visitNullable(ctx.performType())
        );
    }

    @Override
    public Cobol.Perform visitPerformStatement(CobolParser.PerformStatementContext ctx) {
        return new Cobol.Perform(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.PERFORM()),
                visit(ctx.performInlineStatement(), ctx.performProcedureStatement())
        );
    }

    @Override
    public Cobol.PerformTestClause visitPerformTestClause(CobolParser.PerformTestClauseContext ctx) {
        return new Cobol.PerformTestClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.TEST(), ctx.BEFORE(), ctx.AFTER())
        );
    }

    @Override
    public Cobol.PerformTimes visitPerformTimes(CobolParser.PerformTimesContext ctx) {
        return new Cobol.PerformTimes(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.integerLiteral()),
                (Cobol.Word) visit(ctx.TIMES())
        );
    }

    @Override
    public Cobol.PerformUntil visitPerformUntil(CobolParser.PerformUntilContext ctx) {
        return new Cobol.PerformUntil(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.performTestClause()),
                (Cobol.Word) visit(ctx.UNTIL()),
                (Cobol.Condition) visit(ctx.condition())
        );
    }

    @Override
    public Cobol.PerformVarying visitPerformVarying(CobolParser.PerformVaryingContext ctx) {
        return new Cobol.PerformVarying(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(singletonList(ctx.performVaryingClause()), singletonList(ctx.performTestClause()))
        );
    }

    @Override
    public Cobol.PerformVaryingClause visitPerformVaryingClause(CobolParser.PerformVaryingClauseContext ctx) {
        return new Cobol.PerformVaryingClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.VARYING()),
                (Cobol.PerformVaryingPhrase) visit(ctx.performVaryingPhrase()),
                convertAll(ctx.performAfter())
        );
    }

    @Override
    public Cobol.PerformVaryingPhrase visitPerformVaryingPhrase(CobolParser.PerformVaryingPhraseContext ctx) {
        return new Cobol.PerformVaryingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                (Cobol.Performable) visit(ctx.performFrom()),
                (Cobol.Performable) visit(ctx.performBy()),
                (Cobol.PerformUntil) visit(ctx.performUntil())
        );
    }

    @Override
    public Cobol.Picture visitPicture(CobolParser.PictureContext ctx) {
        return new Cobol.Picture(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.pictureChars()),
                visitNullable(ctx.pictureCardinality())
        );
    }

    @Override
    public Object visitPictureCardinality(CobolParser.PictureCardinalityContext ctx) {
        return new Cobol.Parenthesized(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.LPARENCHAR()),
                singletonList((Cobol) visit(ctx.integerLiteral())),
                (Cobol.Word) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitPictureString(CobolParser.PictureStringContext ctx) {
        return new Cobol.PictureString(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.picture())
        );
    }

    @Override
    public Object visitPlusMinus(CobolParser.PlusMinusContext ctx) {
        return new Cobol.PlusMinus(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.PLUSCHAR(), ctx.MINUSCHAR()),
                (Cobol.MultDivs) visit(ctx.multDivs())
        );
    }

    @Override
    public Object visitPower(CobolParser.PowerContext ctx) {
        return new Cobol.Power(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.DOUBLEASTERISKCHAR()),
                (Cobol) visit(ctx.basis())
        );
    }

    @Override
    public Object visitPowers(CobolParser.PowersContext ctx) {
        return new Cobol.Powers(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                ctx.PLUSCHAR() != null ? (Cobol.Word) visit(ctx.PLUSCHAR()) :
                        ctx.MINUSCHAR() != null ? (Cobol.Word) visit(ctx.MINUSCHAR()) : null,
                (Cobol) visit(ctx.basis()),
                convertAll(ctx.power())
        );
    }

    @Override
    public Object visitProcedureDeclarative(CobolParser.ProcedureDeclarativeContext ctx) {
        return new Cobol.ProcedureDeclarative(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.ProcedureSectionHeader) visit(ctx.procedureSectionHeader()),
                (Cobol.Word) visit(ctx.DOT_FS(0)),
                (Cobol.UseStatement) visit(ctx.useStatement()),
                (Cobol.Word) visit(ctx.DOT_FS(1)),
                (Cobol.Paragraphs) visit(ctx.paragraphs())
        );
    }

    @Override
    public Object visitProcedureDeclaratives(CobolParser.ProcedureDeclarativesContext ctx) {
        return new Cobol.ProcedureDeclaratives(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.DECLARATIVES(0)),
                (Cobol.Word) visit(ctx.DOT_FS(0)),
                convertAll(ctx.procedureDeclarative()),
                wordsList(ctx.END(), ctx.DECLARATIVES(1)),
                (Cobol.Word) visit(ctx.DOT_FS(1))
        );
    }

    @Override
    public Cobol.ProcedureDivision visitProcedureDivision(CobolParser.ProcedureDivisionContext ctx) {
        return new Cobol.ProcedureDivision(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PROCEDURE(), ctx.DIVISION()),
                visitNullable(ctx.procedureDivisionUsingClause()),
                visitNullable(ctx.procedureDivisionGivingClause()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                visitNullable(ctx.procedureDeclaratives()),
                (Cobol.ProcedureDivisionBody) visit(ctx.procedureDivisionBody())
        );
    }

    @Override
    public Cobol.ProcedureDivisionBody visitProcedureDivisionBody(CobolParser.ProcedureDivisionBodyContext ctx) {
        return new Cobol.ProcedureDivisionBody(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Paragraphs) visit(ctx.paragraphs()),
                convertAll(ctx.procedureSection())
        );
    }

    @Override
    public Cobol.ProcedureDivisionByReference visitProcedureDivisionByReference(CobolParser.ProcedureDivisionByReferenceContext ctx) {
        if (ctx.ANY() == null) {
            return new Cobol.ProcedureDivisionByReference(
                    randomId(),
                    Space.EMPTY,
                    Markers.EMPTY,
                    visitNullable(ctx.OPTIONAL()),
                    (ctx.identifier() == null) ? (Name) visit(ctx.fileName()) : (Name) visit(ctx.identifier())
            );
        } else {
            return new Cobol.ProcedureDivisionByReference(
                    randomId(),
                    Space.EMPTY,
                    Markers.EMPTY,
                    (Cobol.Word) visit(ctx.ANY()),
                    null);
        }
    }

    @Override
    public Cobol.ProcedureDivisionByReferencePhrase visitProcedureDivisionByReferencePhrase(CobolParser.ProcedureDivisionByReferencePhraseContext ctx) {
        return new Cobol.ProcedureDivisionByReferencePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BY(), ctx.REFERENCE()),
                convertAll(ctx.procedureDivisionByReference())
        );
    }

    @Override
    public Cobol.ProcedureDivisionByValuePhrase visitProcedureDivisionByValuePhrase(CobolParser.ProcedureDivisionByValuePhraseContext ctx) {
        return new Cobol.ProcedureDivisionByValuePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BY(), ctx.VALUE()),
                convertAll(ctx.procedureDivisionByValue())
        );
    }

    @Override
    public Object visitProcedureDivisionGivingClause(CobolParser.ProcedureDivisionGivingClauseContext ctx) {
        return new Cobol.ProcedureDivisionGivingClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.GIVING(), ctx.RETURNING()),
                (Name) visit(ctx.dataName())
        );
    }

    @Override
    public Cobol.ProcedureDivisionUsingClause visitProcedureDivisionUsingClause(CobolParser.ProcedureDivisionUsingClauseContext ctx) {
        return new Cobol.ProcedureDivisionUsingClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.USING(), ctx.CHAINING()),
                convertAll(ctx.procedureDivisionUsingParameter())
        );
    }

    @Override
    public Cobol.ProcedureName visitProcedureName(CobolParser.ProcedureNameContext ctx) {
        return new Cobol.ProcedureName(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.ProcedureSectionHeader) visit(ctx.procedureSectionHeader()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                (Cobol.Paragraphs) visit(ctx.paragraphs())
        );
    }

    @Override
    public Object visitProcedureSectionHeader(CobolParser.ProcedureSectionHeaderContext ctx) {
        return new Cobol.ProcedureSectionHeader(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.sectionName()),
                (Cobol.Word) visit(ctx.SECTION()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.ProgramIdParagraph visitProgramIdParagraph(CobolParser.ProgramIdParagraphContext ctx) {
        return new Cobol.ProgramIdParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.PROGRAM_ID()),
                (Cobol.Word) visit(ctx.DOT_FS(0)),
                (Name) visit(ctx.programName()),
                wordsList(ctx.IS(), ctx.COMMON(), ctx.INITIAL(), ctx.LIBRARY(), ctx.DEFINITION(), ctx.RECURSIVE(), ctx.PROGRAM()),
                ctx.DOT_FS().size() == 1 ? null : visitNullable(ctx.DOT_FS(1))
        );
    }

    @Override
    public Object visitProgramLibrarySection(CobolParser.ProgramLibrarySectionContext ctx) {
        return new Cobol.ProgramLibrarySection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PROGRAM_LIBRARY(), ctx.SECTION(), ctx.DOT_FS()),
                convertAll(ctx.libraryDescriptionEntry())
        );
    }

    @Override
    public Cobol.ProgramUnit visitProgramUnit(CobolParser.ProgramUnitContext ctx) {
        return new Cobol.ProgramUnit(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.IdentificationDivision) visit(ctx.identificationDivision()),
                visitNullable(ctx.environmentDivision()),
                visitNullable(ctx.dataDivision()),
                visitNullable(ctx.procedureDivision()),
                convertAll(ctx.programUnit()),
                visitNullable(ctx.endProgramStatement())
        );
    }

    @Override
    public Cobol.Purge visitPurgeStatement(CobolParser.PurgeStatementContext ctx) {
        return new Cobol.Purge(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.PURGE()),
                convertAll(ctx.cdName())
        );
    }

    @Override
    public Object visitQualifiedDataName(CobolParser.QualifiedDataNameContext ctx) {
        return new Cobol.QualifiedDataName(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.qualifiedDataNameFormat1(), ctx.qualifiedDataNameFormat1(), ctx.qualifiedDataNameFormat3(), ctx.qualifiedDataNameFormat4())
        );
    }

    @Override
    public Object visitQualifiedDataNameFormat1(CobolParser.QualifiedDataNameFormat1Context ctx) {
        return new Cobol.QualifiedDataNameFormat1(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.dataName(), ctx.conditionName()),
                convertAll(ctx.qualifiedInData()),
                visitNullable(ctx.inFile())
        );
    }

    @Override
    public Object visitQualifiedDataNameFormat2(CobolParser.QualifiedDataNameFormat2Context ctx) {
        return new Cobol.QualifiedDataNameFormat2(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.paragraphName()),
                (Cobol.InSection) visit(ctx.inSection())
        );
    }

    @Override
    public Object visitQualifiedDataNameFormat3(CobolParser.QualifiedDataNameFormat3Context ctx) {
        return new Cobol.QualifiedDataNameFormat3(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.textName()),
                (Cobol.InLibrary) visit(ctx.inLibrary())
        );
    }

    @Override
    public Object visitQualifiedDataNameFormat4(CobolParser.QualifiedDataNameFormat4Context ctx) {
        return new Cobol.QualifiedDataNameFormat4(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.LINAGE_COUNTER()),
                (Cobol.InFile) visit(ctx.inFile())
        );
    }

    @Override
    public Object visitQualifiedInData(CobolParser.QualifiedInDataContext ctx) {
        return new Cobol.QualifiedDataName(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.inData(), ctx.inTable())
        );
    }

    @Override
    public Cobol.ReadInto visitReadInto(CobolParser.ReadIntoContext ctx) {
        return new Cobol.ReadInto(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INTO()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Cobol.ReadKey visitReadKey(CobolParser.ReadKeyContext ctx) {
        return new Cobol.ReadKey(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.KEY(), ctx.IS()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.Read visitReadStatement(CobolParser.ReadStatementContext ctx) {
        return new Cobol.Read(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.READ()),
                (Name) visit(ctx.fileName()),
                wordsList(ctx.NEXT(), ctx.RECORD()),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.KEPT(), ctx.NO(), ctx.LOCK(), ctx.WAIT())
        );
    }

    @Override
    public Cobol.Receivable visitReceiveBefore(CobolParser.ReceiveBeforeContext ctx) {
        return new Cobol.Receivable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BEFORE(), ctx.TIME()),
                visit(ctx.numericLiteral(), ctx.identifier())
        );
    }

    @Override
    public Cobol.ReceiveFrom visitReceiveFrom(CobolParser.ReceiveFromContext ctx) {
        return new Cobol.ReceiveFrom(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LAST(), ctx.ANY(), ctx.THREAD()),
                visitNullable(ctx.dataName())
        );
    }

    @Override
    public Cobol.ReceiveFromStatement visitReceiveFromStatement(CobolParser.ReceiveFromStatementContext ctx) {
        return new Cobol.ReceiveFromStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.dataName()),
                (Cobol.Word) visit(ctx.FROM()),
                (Cobol.ReceiveFrom) visit(ctx.receiveFrom()),
                convertAll(ctx.receiveBefore(), ctx.receiveWith(), ctx.receiveThread(), ctx.receiveSize(), ctx.receiveStatus())
        );
    }

    @Override
    public Cobol.ReceiveIntoStatement visitReceiveIntoStatement(CobolParser.ReceiveIntoStatementContext ctx) {
        return new Cobol.ReceiveIntoStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.cdName()),
                wordsList(ctx.MESSAGE(), ctx.SEGMENT(), ctx.INTO()),
                (Identifier) visit(ctx.identifier()),
                visitNullable(ctx.receiveNoData()),
                visitNullable(ctx.receiveWithData())
        );
    }

    @Override
    public Cobol.StatementPhrase visitReceiveNoData(CobolParser.ReceiveNoDataContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NO(), ctx.DATA()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Cobol.Receivable visitReceiveSize(CobolParser.ReceiveSizeContext ctx) {
        return new Cobol.Receivable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SIZE(), ctx.IN()),
                visit(ctx.numericLiteral(), ctx.identifier())
        );
    }

    @Override
    public Cobol.Receive visitReceiveStatement(CobolParser.ReceiveStatementContext ctx) {
        return new Cobol.Receive(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.RECEIVE()),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.STATUS(), ctx.IN()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Cobol.Receivable visitReceiveThread(CobolParser.ReceiveThreadContext ctx) {
        return new Cobol.Receivable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.THREAD(), ctx.IN()),
                (Name) visit(ctx.dataName())
        );
    }

    @Override
    public Cobol.ReceiveWith visitReceiveWith(CobolParser.ReceiveWithContext ctx) {
        return new Cobol.ReceiveWith(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.NO(), ctx.WAIT())
        );
    }

    @Override
    public Cobol.StatementPhrase visitReceiveWithData(CobolParser.ReceiveWithDataContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.DATA()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitRecordContainsClause(CobolParser.RecordContainsClauseContext ctx) {
        return new Cobol.RecordContainsClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.RECORD()),
                visit(ctx.recordContainsClauseFormat1(), ctx.recordContainsClauseFormat2(), ctx.recordContainsClauseFormat3())
        );
    }

    @Override
    public Object visitRecordContainsClauseFormat1(CobolParser.RecordContainsClauseFormat1Context ctx) {
        return new Cobol.RecordContainsClauseFormat1(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.CONTAINS()),
                (Cobol.Word) visit(ctx.integerLiteral()),
                visitNullable(ctx.CHARACTERS())
        );
    }

    @Override
    public Object visitRecordContainsClauseFormat2(CobolParser.RecordContainsClauseFormat2Context ctx) {
        return new Cobol.RecordContainsClauseFormat2(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.VARYING(), ctx.IN(), ctx.SIZE()),
                convertAllList(singletonList(ctx.FROM()), singletonList(ctx.integerLiteral()), singletonList(ctx.recordContainsTo()), singletonList(ctx.CHARACTERS())),
                convertAllList(singletonList(ctx.DEPENDING()), singletonList(ctx.ON()), singletonList(ctx.qualifiedDataName()))
        );
    }

    @Override
    public Object visitRecordContainsClauseFormat3(CobolParser.RecordContainsClauseFormat3Context ctx) {
        return new Cobol.RecordContainsClauseFormat3(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.CONTAINS()),
                (Cobol.Word) visit(ctx.integerLiteral()),
                (Cobol.RecordContainsTo) visit(ctx.recordContainsTo()),
                visitNullable(ctx.CHARACTERS())
        );
    }

    @Override
    public Object visitRecordContainsTo(CobolParser.RecordContainsToContext ctx) {
        return new Cobol.RecordContainsTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.TO()),
                (Cobol.Word) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitRecordDelimiterClause(CobolParser.RecordDelimiterClauseContext ctx) {
        return new Cobol.RecordDelimiterClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RECORD(), ctx.DELIMITER(), ctx.IS(), ctx.STANDARD_1(), ctx.IMPLICIT()),
                visitNullable(ctx.assignmentName())
        );
    }

    @Override
    public Object visitRecordKeyClause(CobolParser.RecordKeyClauseContext ctx) {
        return new Cobol.RecordKeyClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RECORD(), ctx.KEY(), ctx.IS()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                visitNullable(ctx.passwordClause()),
                wordsList(ctx.WITH(), ctx.DUPLICATES())
        );
    }

    @Override
    public Object visitRecordingModeClause(CobolParser.RecordingModeClauseContext ctx) {
        return new Cobol.RecordingModeClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RECORDING(), ctx.MODE(), ctx.IS()),
                visitNullable(ctx.modeStatement())
        );
    }

    @Override
    public Object visitReferenceModifier(CobolParser.ReferenceModifierContext ctx) {
        return new Cobol.ReferenceModifier(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.LPARENCHAR()),
                (Cobol.ArithmeticExpression) visit(ctx.characterPosition()),
                (Cobol.Word) visit(ctx.COLONCHAR()),
                visitNullable(ctx.length()),
                (Cobol.Word) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitRelationArithmeticComparison(CobolParser.RelationArithmeticComparisonContext ctx) {
        return new Cobol.RelationArithmeticComparison(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.ArithmeticExpression) visit(ctx.arithmeticExpression()),
                (Cobol.RelationalOperator) visit(ctx.relationalOperator()),
                new Cobol.Parenthesized(
                        randomId(),
                        Space.EMPTY,
                        Markers.EMPTY,
                        (Cobol.Word) visit(ctx.LPARENCHAR()),
                        singletonList((Cobol) visit(ctx.relationCombinedCondition())),
                        (Cobol.Word) visit(ctx.RPARENCHAR())
                )
        );
    }

    @Override
    public Object visitRelationCombinedCondition(CobolParser.RelationCombinedConditionContext ctx) {
        return new Cobol.RelationCombinedCondition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(ctx.AND(), ctx.OR(), ctx.arithmeticExpression())
        );
    }

    @Override
    public Object visitRelationSignCondition(CobolParser.RelationSignConditionContext ctx) {
        return new Cobol.RelationSignCondition(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.ArithmeticExpression) visit(ctx.arithmeticExpression()),
                wordsList(ctx.IS(), ctx.NOT(), ctx.POSITIVE(), ctx.NEGATIVE(), ctx.ZERO())
        );
    }

    @Override
    public Object visitRelationalOperator(CobolParser.RelationalOperatorContext ctx) {
        return new Cobol.RelationalOperator(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.ARE(), ctx.NOT(),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RELATIVE(), ctx.KEY(), ctx.IS()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.Release visitReleaseStatement(CobolParser.ReleaseStatementContext ctx) {
        return new Cobol.Release(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.RELEASE()),
                (Cobol.QualifiedDataName) visit(ctx.recordName()),
                visitNullable(ctx.FROM()),
                visitNullable(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitReportClause(CobolParser.ReportClauseContext ctx) {
        return new Cobol.ReportClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.REPORT(), ctx.IS(), ctx.REPORTS(), ctx.ARE()),
                convertAll(ctx.reportName())
        );
    }

    @Override
    public Object visitRemarksParagraph(CobolParser.RemarksParagraphContext ctx) {
        return new Cobol.IdentificationDivisionParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.REMARKS()),
                (Cobol.Word) visit(ctx.DOT_FS(0)),
                visitNullable(ctx.commentEntry()),
                wordsList(ctx.END_REMARKS()),
                ctx.DOT_FS().size() == 1 ? null : (Cobol.Word) visit(ctx.DOT_FS(1))
        );
    }

    @Override
    public Object visitReportDescription(CobolParser.ReportDescriptionContext ctx) {
        return new Cobol.ReportDescription(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.ReportDescriptionEntry) visit(ctx.reportDescriptionEntry()),
                convertAll(ctx.reportGroupDescriptionEntry())
        );
    }

    @Override
    public Object visitReportDescriptionEntry(CobolParser.ReportDescriptionEntryContext ctx) {
        return new Cobol.ReportDescriptionEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.RD()),
                (Cobol.QualifiedDataName) visit(ctx.reportName()),
                visitNullable(ctx.reportDescriptionGlobalClause()),
                visitNullable(ctx.reportDescriptionPageLimitClause()),
                visitNullable(ctx.reportDescriptionHeadingClause()),
                visitNullable(ctx.reportDescriptionFirstDetailClause()),
                visitNullable(ctx.reportDescriptionLastDetailClause()),
                visitNullable(ctx.reportDescriptionFootingClause()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitReportDescriptionFirstDetailClause(CobolParser.ReportDescriptionFirstDetailClauseContext ctx) {
        return new Cobol.ReportDescriptionFirstDetailClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FIRST(), ctx.DETAIL()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportDescriptionFootingClause(CobolParser.ReportDescriptionFootingClauseContext ctx) {
        return new Cobol.ReportDescriptionFootingClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.FOOTING()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportDescriptionGlobalClause(CobolParser.ReportDescriptionGlobalClauseContext ctx) {
        return new Cobol.ReportDescriptionGlobalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.IS(), ctx.GLOBAL())
        );
    }

    @Override
    public Object visitReportDescriptionHeadingClause(CobolParser.ReportDescriptionHeadingClauseContext ctx) {
        return new Cobol.ReportDescriptionHeadingClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.HEADING()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportDescriptionLastDetailClause(CobolParser.ReportDescriptionLastDetailClauseContext ctx) {
        return new Cobol.ReportDescriptionLastDetailClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LAST(), ctx.DETAIL()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportDescriptionPageLimitClause(CobolParser.ReportDescriptionPageLimitClauseContext ctx) {
        return new Cobol.ReportDescriptionPageLimitClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PAGE(), ctx.LIMIT(), ctx.IS(), ctx.LIMITS(), ctx.ARE()),
                (Name) visit(ctx.integerLiteral()),
                ctx.LINE() != null ? (Cobol.Word) visit(ctx.LINE()) :
                        ctx.LINES() != null ? (Cobol.Word) visit(ctx.LINES()) : null
        );
    }

    @Override
    public Object visitReportGroupBlankWhenZeroClause(CobolParser.ReportGroupBlankWhenZeroClauseContext ctx) {
        return new Cobol.ReportGroupBlankWhenZeroClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BLANK(), ctx.WHEN(), ctx.ZERO())
        );
    }

    @Override
    public Object visitReportGroupColumnNumberClause(CobolParser.ReportGroupColumnNumberClauseContext ctx) {
        return new Cobol.ReportGroupColumnNumberClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.COLUMN(), ctx.NUMBER(), ctx.IS()),
                (Name) visit(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportGroupDescriptionEntryFormat1(CobolParser.ReportGroupDescriptionEntryFormat1Context ctx) {
        return new Cobol.ReportGroupDescriptionEntryFormat1(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.integerLiteral()),
                (Cobol.Word) visit(ctx.dataName()),
                visitNullable(ctx.reportGroupLineNumberClause()),
                visitNullable(ctx.reportGroupNextGroupClause()),
                (Cobol.ReportGroupTypeClause) visit(ctx.reportGroupTypeClause()),
                visitNullable(ctx.reportGroupUsageClause()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitReportGroupDescriptionEntryFormat2(CobolParser.ReportGroupDescriptionEntryFormat2Context ctx) {
        return new Cobol.ReportGroupDescriptionEntryFormat2(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.integerLiteral()),
                visitNullable(ctx.dataName()),
                visitNullable(ctx.reportGroupLineNumberClause()),
                (Cobol.ReportGroupUsageClause) visit(ctx.reportGroupUsageClause()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitReportGroupDescriptionEntryFormat3(CobolParser.ReportGroupDescriptionEntryFormat3Context ctx) {
        return new Cobol.ReportGroupDescriptionEntryFormat3(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.integerLiteral()),
                visitNullable(ctx.dataName()),
                convertAll(ctx.reportGroupPictureClause(),
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
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitReportGroupIndicateClause(CobolParser.ReportGroupIndicateClauseContext ctx) {
        return new Cobol.ReportGroupIndicateClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.GROUP(), ctx.INDICATE())
        );
    }

    @Override
    public Object visitReportGroupJustifiedClause(CobolParser.ReportGroupJustifiedClauseContext ctx) {
        return new Cobol.ReportGroupJustifiedClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.JUSTIFIED(), ctx.JUST(), ctx.RIGHT())
        );
    }

    @Override
    public Object visitReportGroupLineNumberClause(CobolParser.ReportGroupLineNumberClauseContext ctx) {
        return new Cobol.ReportGroupLineNumberClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LINE(), ctx.NUMBER(), ctx.IS()),
                visit(ctx.reportGroupLineNumberNextPage(), ctx.reportGroupLineNumberPlus())
        );
    }

    @Override
    public Object visitReportGroupLineNumberNextPage(CobolParser.ReportGroupLineNumberNextPageContext ctx) {
        return new Cobol.ReportGroupLineNumberNextPage(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.integerLiteral()),
                wordsList(ctx.ON(), ctx.NEXT(), ctx.PAGE())
        );
    }

    @Override
    public Object visitReportGroupLineNumberPlus(CobolParser.ReportGroupLineNumberPlusContext ctx) {
        return new Cobol.ReportGroupLineNumberPlus(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.PLUS()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportGroupNextGroupClause(CobolParser.ReportGroupNextGroupClauseContext ctx) {
        return new Cobol.ReportGroupNextGroupClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NEXT(), ctx.GROUP(), ctx.IS()),
                visit(ctx.integerLiteral(), ctx.reportGroupNextGroupNextPage(), ctx.reportGroupNextGroupPlus())
        );
    }

    @Override
    public Object visitReportGroupNextGroupNextPage(CobolParser.ReportGroupNextGroupNextPageContext ctx) {
        return new Cobol.ReportGroupNextGroupNextPage(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NEXT(), ctx.PAGE())
        );
    }

    @Override
    public Object visitReportGroupNextGroupPlus(CobolParser.ReportGroupNextGroupPlusContext ctx) {
        return new Cobol.ReportGroupNextGroupPlus(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.PLUS()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitReportGroupPictureClause(CobolParser.ReportGroupPictureClauseContext ctx) {
        return new Cobol.ReportGroupPictureClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PICTURE(), ctx.PIC(), ctx.IS()),
                (Cobol.PictureString) visit(ctx.pictureString())
        );
    }

    @Override
    public Object visitReportGroupResetClause(CobolParser.ReportGroupResetClauseContext ctx) {
        return new Cobol.ReportGroupResetClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RESET(), ctx.ON(), ctx.FINAL()),
                visitNullable(ctx.dataName())
        );
    }

    @Override
    public Object visitReportGroupSignClause(CobolParser.ReportGroupSignClauseContext ctx) {
        return new Cobol.ReportGroupSignClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SIGN(), ctx.IS(), ctx.LEADING(), ctx.TRAILING(), ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Object visitReportGroupSourceClause(CobolParser.ReportGroupSourceClauseContext ctx) {
        return new Cobol.ReportGroupSourceClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SOURCE(), ctx.IS()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitReportGroupSumClause(CobolParser.ReportGroupSumClauseContext ctx) {
        return new Cobol.ReportGroupSumClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(singletonList(ctx.SUM()), ctx.COMMACHAR(), ctx.identifier(), singletonList(ctx.UPON()), ctx.dataName())
        );
    }

    @Override
    public Object visitReportGroupTypeClause(CobolParser.ReportGroupTypeClauseContext ctx) {
        return new Cobol.ReportGroupTypeClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.TYPE(), ctx.IS()),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.CONTROL(), ctx.FOOTING(), ctx.CF(), ctx.FINAL()),
                visitNullable(ctx.dataName())
        );
    }

    @Override
    public Object visitReportGroupTypeControlHeading(CobolParser.ReportGroupTypeControlHeadingContext ctx) {
        return new Cobol.ReportGroupTypeControlHeading(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.CONTROL(), ctx.HEADING(), ctx.CH(), ctx.FINAL()),
                visitNullable(ctx.dataName())
        );
    }

    @Override
    public Object visitReportGroupTypeDetail(CobolParser.ReportGroupTypeDetailContext ctx) {
        return new Cobol.ReportGroupTypeDetail(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DETAIL(), ctx.DE())
        );
    }

    @Override
    public Object visitReportGroupTypePageFooting(CobolParser.ReportGroupTypePageFootingContext ctx) {
        return new Cobol.ReportGroupTypePageFooting(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PAGE(), ctx.FOOTING(), ctx.PF())
        );
    }

    @Override
    public Object visitReportGroupTypePageHeading(CobolParser.ReportGroupTypePageHeadingContext ctx) {
        return new Cobol.ReportGroupTypePageHeading(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PAGE(), ctx.HEADING(), ctx.PH())
        );
    }

    @Override
    public Object visitReportGroupTypeReportFooting(CobolParser.ReportGroupTypeReportFootingContext ctx) {
        return new Cobol.ReportGroupTypeReportFooting(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.REPORT(), ctx.FOOTING(), ctx.RF())
        );
    }

    @Override
    public Object visitReportGroupTypeReportHeading(CobolParser.ReportGroupTypeReportHeadingContext ctx) {
        return new Cobol.ReportGroupTypeReportHeading(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.REPORT(), ctx.HEADING(), ctx.RH())
        );
    }

    @Override
    public Object visitReportGroupUsageClause(CobolParser.ReportGroupUsageClauseContext ctx) {
        return new Cobol.ReportGroupUsageClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.USAGE(), ctx.IS(), ctx.DISPLAY(), ctx.DISPLAY_1())
        );
    }

    @Override
    public Object visitReportGroupValueClause(CobolParser.ReportGroupValueClauseContext ctx) {
        return new Cobol.ReportGroupValueClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.VALUE(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitReportSection(CobolParser.ReportSectionContext ctx) {
        return new Cobol.ReportSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.REPORT(), ctx.SECTION(), ctx.DOT_FS()),
                convertAll(ctx.reportDescription())
        );
    }

    @Override
    public Object visitRerunClause(CobolParser.RerunClauseContext ctx) {
        return new Cobol.RerunClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.RERUN()),
                visitNullable(ctx.ON()),
                visit(ctx.assignmentName(), ctx.fileName()),
                (Cobol.Word) visit(ctx.EVERY()),
                visit(ctx.rerunEveryRecords(), ctx.rerunEveryOf(), ctx.rerunEveryClock())
        );
    }

    @Override
    public Object visitRerunEveryClock(CobolParser.RerunEveryClockContext ctx) {
        return new Cobol.RerunEveryClock(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.integerLiteral()),
                visitNullable(ctx.CLOCK_UNITS())
        );
    }

    @Override
    public Object visitRerunEveryOf(CobolParser.RerunEveryOfContext ctx) {
        return new Cobol.RerunEveryOf(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.END(), ctx.OF().size() == 1 ? null : ctx.OF(0), ctx.REEL(), ctx.UNIT(), ctx.OF(ctx.OF().size() == 1 ? 0 : 1)),
                (Cobol.Word) visit(ctx.fileName())
        );
    }

    @Override
    public Object visitRerunEveryRecords(CobolParser.RerunEveryRecordsContext ctx) {
        return new Cobol.RerunEveryRecords(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.integerLiteral()),
                (Cobol.Word) visit(ctx.RECORDS())
        );
    }

    @Override
    public Object visitReserveClause(CobolParser.ReserveClauseContext ctx) {
        return new Cobol.ReserveClause(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RESERVE(), ctx.WORDS(), ctx.LIST(), ctx.IS(), ctx.NETWORK(), ctx.CAPABLE())
        );
    }

    @Override
    public Cobol.ReturnInto visitReturnInto(CobolParser.ReturnIntoContext ctx) {
        return new Cobol.ReturnInto(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INTO()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.Return visitReturnStatement(CobolParser.ReturnStatementContext ctx) {
        return new Cobol.Return(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.RETURN()),
                (Name) visit(ctx.fileName()),
                visitNullable(ctx.RECORD()),
                visitNullable(ctx.returnInto()),
                (Cobol.StatementPhrase) visit(ctx.atEndPhrase()),
                visitNullable(ctx.notAtEndPhrase()),
                visitNullable(ctx.END_RETURN())
        );
    }

    @Override
    public Object visitRewriteFrom(CobolParser.RewriteFromContext ctx) {
        return new Cobol.RewriteFrom(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.FROM()),
                (Name) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitRewriteStatement(CobolParser.RewriteStatementContext ctx) {
        return new Cobol.Rewrite(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.REWRITE()),
                (Cobol.QualifiedDataName) visit(ctx.recordName()),
                visitNullable(ctx.rewriteFrom()),
                visitNullable(ctx.invalidKeyPhrase()),
                visitNullable(ctx.notInvalidKeyPhrase()),
                visitNullable(ctx.END_REWRITE())
        );
    }

    @Override
    public Cobol.Roundable visitRoundable(CobolParser.RoundableContext ctx) {
        return new Cobol.Roundable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Identifier) visit(ctx.identifier()),
                visitNullable(ctx.ROUNDED())
        );
    }

    @Override
    public Object visitSameClause(CobolParser.SameClauseContext ctx) {
        return new Cobol.SameClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SAME(), ctx.RECORD(), ctx.SORT(), ctx.SORT_MERGE(), ctx.AREA(), ctx.FOR()),
                convertAll(ctx.fileName())
        );
    }

    @Override
    public Object visitSecurityParagraph(CobolParser.SecurityParagraphContext ctx) {
        return new Cobol.IdentificationDivisionParagraph(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.SECURITY()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                visitNullable(ctx.commentEntry()),
                null,
                null
        );
    }

    @Override
    public Object visitScreenDescriptionAutoClause(CobolParser.ScreenDescriptionAutoClauseContext ctx) {
        return new Cobol.ScreenDescriptionAutoClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.AUTO(), ctx.AUTO_SKIP())
        );
    }

    @Override
    public Object visitScreenDescriptionBackgroundColorClause(CobolParser.ScreenDescriptionBackgroundColorClauseContext ctx) {
        return new Cobol.ScreenDescriptionBackgroundColorClause(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.BELL(), ctx.BEEP())
        );
    }

    @Override
    public Object visitScreenDescriptionBlankClause(CobolParser.ScreenDescriptionBlankClauseContext ctx) {
        return new Cobol.ScreenDescriptionBlankClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BLANK(), ctx.SCREEN(), ctx.LINE())
        );
    }

    @Override
    public Object visitScreenDescriptionBlankWhenZeroClause(CobolParser.ScreenDescriptionBlankWhenZeroClauseContext ctx) {
        return new Cobol.ScreenDescriptionBlankWhenZeroClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BLANK(), ctx.WHEN(), ctx.ZERO())
        );
    }

    @Override
    public Object visitScreenDescriptionBlinkClause(CobolParser.ScreenDescriptionBlinkClauseContext ctx) {
        return new Cobol.ScreenDescriptionBlinkClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.BLINK())
        );
    }

    @Override
    public Object visitScreenDescriptionColumnClause(CobolParser.ScreenDescriptionColumnClauseContext ctx) {
        return new Cobol.ScreenDescriptionColumnClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.COLUMN(), ctx.COL(), ctx.NUMBER(), ctx.IS(), ctx.PLUS(), ctx.PLUSCHAR(), ctx.MINUSCHAR()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitScreenDescriptionControlClause(CobolParser.ScreenDescriptionControlClauseContext ctx) {
        return new Cobol.ScreenDescriptionControlClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.CONTROL(), ctx.IS()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionEntry(CobolParser.ScreenDescriptionEntryContext ctx) {
        return new Cobol.ScreenDescriptionEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INTEGERLITERAL()),
                ctx.FILLER() != null ? (Cobol.Word) visit(ctx.FILLER()) :
                        ctx.screenName() != null ? (Cobol.Word) visit(ctx.screenName()) : null,
                convertAll(ctx.screenDescriptionBlankClause(),
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
                        ctx.screenDescriptionZeroFillClause()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Object visitScreenDescriptionEraseClause(CobolParser.ScreenDescriptionEraseClauseContext ctx) {
        return new Cobol.ScreenDescriptionEraseClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ERASE(), ctx.EOL(), ctx.EOS())
        );
    }

    @Override
    public Object visitScreenDescriptionForegroundColorClause(CobolParser.ScreenDescriptionForegroundColorClauseContext ctx) {
        return new Cobol.ScreenDescriptionForegroundColorClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FOREGROUND_COLOR(), ctx.FOREGROUND_COLOUR(), ctx.IS()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitScreenDescriptionFromClause(CobolParser.ScreenDescriptionFromClauseContext ctx) {
        return new Cobol.ScreenDescriptionFromClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.FROM()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.screenDescriptionToClause())
        );
    }

    @Override
    public Object visitScreenDescriptionFullClause(CobolParser.ScreenDescriptionFullClauseContext ctx) {
        return new Cobol.ScreenDescriptionFullClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.FULL(), ctx.LENGTH_CHECK())
        );
    }

    @Override
    public Object visitScreenDescriptionGridClause(CobolParser.ScreenDescriptionGridClauseContext ctx) {
        return new Cobol.ScreenDescriptionGridClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.GRID(), ctx.LEFTLINE(), ctx.OVERLINE())
        );
    }

    @Override
    public Object visitScreenDescriptionJustifiedClause(CobolParser.ScreenDescriptionJustifiedClauseContext ctx) {
        return new Cobol.ScreenDescriptionJustifiedClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.JUSTIFIED(), ctx.JUST(), ctx.RIGHT())
        );
    }

    @Override
    public Object visitScreenDescriptionLightClause(CobolParser.ScreenDescriptionLightClauseContext ctx) {
        return new Cobol.ScreenDescriptionLightClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.HIGHLIGHT(), ctx.LOWLIGHT())
        );
    }

    @Override
    public Object visitScreenDescriptionLineClause(CobolParser.ScreenDescriptionLineClauseContext ctx) {
        return new Cobol.ScreenDescriptionLineClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.LINE(), ctx.NUMBER(), ctx.IS(), ctx.PLUS(), ctx.PLUSCHAR(), ctx.MINUSCHAR()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitScreenDescriptionPictureClause(CobolParser.ScreenDescriptionPictureClauseContext ctx) {
        return new Cobol.ScreenDescriptionPictureClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PICTURE(), ctx.PIC(), ctx.IS()),
                (Cobol.PictureString) visit(ctx.pictureString())
        );
    }

    @Override
    public Object visitScreenDescriptionPromptClause(CobolParser.ScreenDescriptionPromptClauseContext ctx) {
        return new Cobol.ScreenDescriptionPromptClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.PROMPT(), ctx.CHARACTER(), ctx.IS()),
                visit(ctx.identifier(), ctx.literal()),
                visitNullable(ctx.screenDescriptionPromptOccursClause())
        );
    }

    @Override
    public Object visitScreenDescriptionPromptOccursClause(CobolParser.ScreenDescriptionPromptOccursClauseContext ctx) {
        return new Cobol.ScreenDescriptionPromptOccursClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.OCCURS()),
                (Cobol.Word) visit(ctx.integerLiteral()),
                visitNullable(ctx.TIMES())
        );
    }

    @Override
    public Object visitScreenDescriptionRequiredClause(CobolParser.ScreenDescriptionRequiredClauseContext ctx) {
        return new Cobol.ScreenDescriptionRequiredClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.REQUIRED(), ctx.EMPTY_CHECK())
        );
    }

    @Override
    public Object visitScreenDescriptionReverseVideoClause(CobolParser.ScreenDescriptionReverseVideoClauseContext ctx) {
        return new Cobol.ScreenDescriptionReverseVideoClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.REVERSE_VIDEO())
        );
    }

    @Override
    public Object visitScreenDescriptionSecureClause(CobolParser.ScreenDescriptionSecureClauseContext ctx) {
        return new Cobol.ScreenDescriptionSecureClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.SECURE(), ctx.NO_ECHO())
        );
    }

    @Override
    public Object visitScreenDescriptionSignClause(CobolParser.ScreenDescriptionSignClauseContext ctx) {
        return new Cobol.ScreenDescriptionSignClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SIGN(), ctx.IS(), ctx.LEADING(), ctx.TRAILING(), ctx.SEPARATE(), ctx.CHARACTER())
        );
    }

    @Override
    public Object visitScreenDescriptionSizeClause(CobolParser.ScreenDescriptionSizeClauseContext ctx) {
        return new Cobol.ScreenDescriptionSizeClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SIZE(), ctx.IS()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionToClause(CobolParser.ScreenDescriptionToClauseContext ctx) {
        return new Cobol.ScreenDescriptionToClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.TO()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionUnderlineClause(CobolParser.ScreenDescriptionUnderlineClauseContext ctx) {
        return new Cobol.ScreenDescriptionUnderlineClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.UNDERLINE())
        );
    }

    @Override
    public Object visitScreenDescriptionUsageClause(CobolParser.ScreenDescriptionUsageClauseContext ctx) {
        return new Cobol.ScreenDescriptionUsageClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.USAGE(), ctx.IS(), ctx.DISPLAY(), ctx.DISPLAY_1())
        );
    }

    @Override
    public Object visitScreenDescriptionUsingClause(CobolParser.ScreenDescriptionUsingClauseContext ctx) {
        return new Cobol.ScreenDescriptionUsingClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.USING()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitScreenDescriptionValueClause(CobolParser.ScreenDescriptionValueClauseContext ctx) {
        return new Cobol.ScreenDescriptionValueClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.VALUE(), ctx.IS()),
                (Name) visit(ctx.literal())
        );
    }

    @Override
    public Object visitScreenDescriptionZeroFillClause(CobolParser.ScreenDescriptionZeroFillClauseContext ctx) {
        return new Cobol.ScreenDescriptionZeroFillClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.ZERO_FILL())
        );
    }

    @Override
    public Object visitScreenSection(CobolParser.ScreenSectionContext ctx) {
        return new Cobol.ScreenSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SCREEN(), ctx.SECTION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.screenDescriptionEntry())
        );
    }

    @Override
    public Cobol.Search visitSearchStatement(CobolParser.SearchStatementContext ctx) {
        return new Cobol.Search(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SEARCH(), ctx.ALL()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                visitNullable(ctx.searchVarying()),
                visitNullable(ctx.atEndPhrase()),
                convertAll(ctx.searchWhen()),
                visitNullable(ctx.END_SEARCH())
        );
    }

    @Override
    public Cobol.SearchVarying visitSearchVarying(CobolParser.SearchVaryingContext ctx) {
        return new Cobol.SearchVarying(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.VARYING()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.SearchWhen visitSearchWhen(CobolParser.SearchWhenContext ctx) {
        return new Cobol.SearchWhen(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.WHEN()),
                (Cobol.Condition) visit(ctx.condition()),
                wordsList(ctx.NEXT(), ctx.SENTENCE()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Cobol.ValuedObjectComputerClause visitSegmentLimitClause(CobolParser.SegmentLimitClauseContext ctx) {
        return new Cobol.ValuedObjectComputerClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                Cobol.ValuedObjectComputerClause.Type.SegmentLimit,
                wordsList(ctx.SEGMENT_LIMIT(), ctx.IS()),
                (Cobol) visit(ctx.integerLiteral()),
                null
        );
    }

    @Override
    public Object visitSelectClause(CobolParser.SelectClauseContext ctx) {
        return new Cobol.SelectClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SELECT(), ctx.OPTIONAL()),
                (Cobol.Word) visit(ctx.fileName())
        );
    }

    @Override
    public Cobol.SendAdvancingLines visitSendAdvancingLines(CobolParser.SendAdvancingLinesContext ctx) {
        return new Cobol.SendAdvancingLines(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                ctx.LINE() != null ? (Cobol.Word) visit(ctx.LINE()) :
                        ctx.LINES() != null ? (Cobol.Word) visit(ctx.LINES()) : null
        );
    }

    @Override
    public Cobol.SendPhrase visitSendAdvancingPhrase(CobolParser.SendAdvancingPhraseContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BEFORE(), ctx.AFTER(), ctx.ADVANCING()),
                visit(ctx.sendAdvancingPage(), ctx.sendAdvancingLines(), ctx.sendAdvancingMnemonic())
        );
    }

    @Override
    public Cobol.SendPhrase visitSendFromPhrase(CobolParser.SendFromPhraseContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FROM()),
                (Cobol) visit(ctx.identifier())
        );
    }

    @Override
    public Cobol.SendPhrase visitSendReplacingPhrase(CobolParser.SendReplacingPhraseContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.REPLACING(), ctx.LINE()),
                null
        );
    }

    @Override
    public Cobol.Send visitSendStatement(CobolParser.SendStatementContext ctx) {
        return new Cobol.Send(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.SEND()),
                visit(ctx.sendStatementSync(), ctx.sendStatementAsync()),
                visitNullable(ctx.onExceptionClause()),
                visitNullable(ctx.notOnExceptionClause())
        );
    }

    @Override
    public Cobol.SendPhrase visitSendStatementAsync(CobolParser.SendStatementAsyncContext ctx) {
        return new Cobol.SendPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.TO(), ctx.TOP(), ctx.BOTTOM()),
                (Cobol) visit(ctx.identifier())
        );
    }

    @Override
    public Cobol.SendStatementSync visitSendStatementSync(CobolParser.SendStatementSyncContext ctx) {
        return new Cobol.SendStatementSync(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.EGI(), ctx.EMI(), ctx.ESI()),
                visitNullable(ctx.identifier())
        );
    }

    @Override
    public Cobol.Sentence visitSentence(CobolParser.SentenceContext ctx) {
        return new Cobol.Sentence(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.statement()),
                (Cobol.Word) visit(ctx.DOT_FS())
        );
    }

    @Override
    public Cobol.Set visitSetStatement(CobolParser.SetStatementContext ctx) {
        return new Cobol.Set(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.SET()),
                convertAll(ctx.setToStatement()),
                visitNullable(ctx.setUpDownByStatement())
        );
    }

    @Override
    public Cobol.SetTo visitSetToStatement(CobolParser.SetToStatementContext ctx) {
        return new Cobol.SetTo(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.setTo()),
                (Cobol.Word) visit(ctx.TO()),
                convertAll(ctx.setToValue())
        );
    }

    @Override
    public Cobol.SetUpDown visitSetUpDownByStatement(CobolParser.SetUpDownByStatementContext ctx) {
        return new Cobol.SetUpDown(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.setTo()),
                wordsList(ctx.DOWN(), ctx.UP(), ctx.BY()),
                (Name) visit(ctx.setByValue())
        );
    }

    @Override
    public Object visitSimpleCondition(CobolParser.SimpleConditionContext ctx) {
        return ctx.condition() != null ? new Cobol.Parenthesized(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.LPARENCHAR()),
                singletonList((Cobol) visit(ctx.condition())),
                (Cobol.Word) visit(ctx.RPARENCHAR())
        ) : visit(ctx.relationCondition(), ctx.classCondition(), ctx.conditionNameReference());
    }

    @Override
    public Cobol.SortProcedurePhrase visitSortCollatingAlphanumeric(CobolParser.SortCollatingAlphanumericContext ctx) {
        return new Cobol.SortProcedurePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FOR(), ctx.ALPHANUMERIC(), ctx.IS()),
                (Cobol.Word) visit(ctx.alphabetName()),
                null
        );
    }

    @Override
    public Cobol.SortProcedurePhrase visitSortCollatingNational(CobolParser.SortCollatingNationalContext ctx) {
        return new Cobol.SortProcedurePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FOR(), ctx.NATIONAL(), ctx.IS()),
                (Cobol.Word) visit(ctx.alphabetName()),
                null
        );
    }

    @Override
    public Cobol.SortCollatingSequencePhrase visitSortCollatingSequencePhrase(CobolParser.SortCollatingSequencePhraseContext ctx) {
        return new Cobol.SortCollatingSequencePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.COLLATING(), ctx.SEQUENCE(), ctx.IS()),
                convertAll(ctx.alphabetName()),
                visitNullable(ctx.sortCollatingAlphanumeric()),
                visitNullable(ctx.sortCollatingNational())
        );
    }

    @Override
    public Cobol.Sortable visitSortDuplicatesPhrase(CobolParser.SortDuplicatesPhraseContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.DUPLICATES(), ctx.IN(), ctx.ORDER()),
                Collections.emptyList()
        );
    }

    @Override
    public Cobol.SortGiving visitSortGiving(CobolParser.SortGivingContext ctx) {
        return new Cobol.SortGiving(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.fileName()),
                wordsList(ctx.LOCK(), ctx.SAVE(), ctx.NO(), ctx.REWIND(), ctx.RELEASE(), ctx.WITH(), ctx.REMOVE(), ctx.CRUNCH())
        );
    }

    @Override
    public Cobol.Sortable visitSortGivingPhrase(CobolParser.SortGivingPhraseContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.GIVING()),
                convertAll(ctx.sortGiving())
        );
    }

    @Override
    public Cobol.SortProcedurePhrase visitSortInputProcedurePhrase(CobolParser.SortInputProcedurePhraseContext ctx) {
        return new Cobol.SortProcedurePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.INPUT(), ctx.PROCEDURE(), ctx.IS()),
                (Name) visit(ctx.procedureName()),
                visitNullable(ctx.sortInputThrough())
        );
    }

    @Override
    public Cobol.Sortable visitSortInputThrough(CobolParser.SortInputThroughContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.THROUGH(), ctx.THRU()),
                convertAll(Collections.singletonList(ctx.procedureName()))
        );
    }

    @Override
    public Cobol.Sortable visitSortOnKeyClause(CobolParser.SortOnKeyClauseContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ON(), ctx.ASCENDING(), ctx.DESCENDING(), ctx.KEY()),
                convertAll(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.SortProcedurePhrase visitSortOutputProcedurePhrase(CobolParser.SortOutputProcedurePhraseContext ctx) {
        return new Cobol.SortProcedurePhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.OUTPUT(), ctx.PROCEDURE(), ctx.IS()),
                visitNullable(ctx.procedureName()),
                visitNullable(ctx.sortOutputThrough())
        );
    }

    @Override
    public Cobol.Sortable visitSortOutputThrough(CobolParser.SortOutputThroughContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.THROUGH(), ctx.THRU()),
                convertAll(Collections.singletonList(ctx.procedureName()))
        );
    }

    @Override
    public Cobol.Sort visitSortStatement(CobolParser.SortStatementContext ctx) {
        return new Cobol.Sort(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.SORT()),
                visitNullable(ctx.fileName()),
                convertAll(ctx.sortOnKeyClause()),
                visitNullable(ctx.sortDuplicatesPhrase()),
                visitNullable(ctx.sortCollatingSequencePhrase()),
                visitNullable(ctx.sortInputProcedurePhrase()),
                convertAll(ctx.sortUsing()),
                visitNullable(ctx.sortOutputProcedurePhrase()),
                convertAll(ctx.sortGivingPhrase())
        );
    }

    @Override
    public Cobol.Sortable visitSortUsing(CobolParser.SortUsingContext ctx) {
        return new Cobol.Sortable(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.USING()),
                convertAll(ctx.fileName())
        );
    }

    @Override
    public Cobol.SourceComputer visitSourceComputerParagraph(CobolParser.SourceComputerParagraphContext ctx) {
        return new Cobol.SourceComputer(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SOURCE_COMPUTER(), ctx.DOT_FS(0)),
                ctx.computerName() == null ? null : new Cobol.SourceComputerDefinition(
                        randomId(),
                        Space.EMPTY,
                        Markers.EMPTY,
                        (Cobol.Word) visit(ctx.computerName()),
                        wordsList(ctx.WITH(), ctx.DEBUGGING(), ctx.MODE())
                ),
                ctx.DOT_FS().size() == 1 ? null : (Cobol.Word) visit(ctx.DOT_FS(1))
        );
    }

    @Override
    public Cobol.SpecialNames visitSpecialNamesParagraph(CobolParser.SpecialNamesParagraphContext ctx) {
        return new Cobol.SpecialNames(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.SPECIAL_NAMES()),
                (Cobol.Word) visit(ctx.DOT_FS(0)),
                convertAll(ctx.specialNameClause()),
                ctx.DOT_FS().size() == 1 ? null : (Cobol.Word) visit(ctx.DOT_FS(1))
        );
    }

    @Override
    public Object visitStartKey(CobolParser.StartKeyContext ctx) {
        return new Cobol.StartKey(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.KEY(), ctx.IS(),
                        ctx.NOT(), ctx.GREATER(), ctx.LESS(), ctx.THAN(), ctx.OR(), ctx.EQUAL(), ctx.TO(),
                        ctx.MORETHANCHAR(), ctx.LESSTHANCHAR(), ctx.MORETHANOREQUAL(), ctx.EQUALCHAR()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitStartStatement(CobolParser.StartStatementContext ctx) {
        return new Cobol.Start(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.START()),
                (Cobol.Word) visit(ctx.fileName()),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.STATUS(), ctx.KEY(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Cobol.Stop visitStopStatement(CobolParser.StopStatementContext ctx) {
        return new Cobol.Stop(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.STOP(), ctx.RUN()),
                ctx.literal() != null ? (Cobol) visit(ctx.literal()) :
                        ctx.stopStatementGiving() != null ? (Cobol) visit(ctx.stopStatementGiving()) : null
        );
    }

    @Override
    public Object visitStopStatementGiving(CobolParser.StopStatementGivingContext ctx) {
        return new Cobol.StopStatementGiving(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.RUN(), ctx.GIVING(), ctx.RETURNING()),
                visit(ctx.identifier(), ctx.integerLiteral())
        );
    }

    @Override
    public Object visitStringDelimitedByPhrase(CobolParser.StringDelimitedByPhraseContext ctx) {
        return new Cobol.StringDelimitedByPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DELIMITED(), ctx.BY()),
                visit(ctx.SIZE(), ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitStringForPhrase(CobolParser.StringForPhraseContext ctx) {
        return new Cobol.StringForPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.FOR()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitStringIntoPhrase(CobolParser.StringIntoPhraseContext ctx) {
        return new Cobol.StringIntoPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INTO()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitStringSendingPhrase(CobolParser.StringSendingPhraseContext ctx) {
        return new Cobol.StringSendingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(ctx.COMMACHAR(), ctx.stringSending()),
                visit(ctx.stringDelimitedByPhrase(), ctx.stringForPhrase())
        );
    }

    @Override
    public Object visitStringStatement(CobolParser.StringStatementContext ctx) {
        return new Cobol.StringStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.STRING()),
                convertAll(ctx.stringSendingPhrase()),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.POINTER()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Cobol.Subscript visitSubscript(CobolParser.SubscriptContext ctx) {
        return new Cobol.Subscript(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                ctx.ALL() == null && ctx.qualifiedDataName() == null &&
                        ctx.indexName() == null && ctx.arithmeticExpression() == null ? null :
                        visit(ctx.ALL(), ctx.qualifiedDataName(), ctx.indexName(), ctx.arithmeticExpression()),
                visitNullable(ctx.integerLiteral())
        );
    }

    @Override
    public Object visitSubtractCorrespondingStatement(CobolParser.SubtractCorrespondingStatementContext ctx) {
        return new Cobol.SubtractCorrespondingStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.CORRESPONDING(), ctx.CORR()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                (Cobol.Word) visit(ctx.FROM()),
                (Cobol.SubtractMinuendCorresponding) visit(ctx.subtractMinuendCorresponding())
        );
    }

    @Override
    public Object visitSubtractFromGivingStatement(CobolParser.SubtractFromGivingStatementContext ctx) {
        return new Cobol.SubtractFromGivingStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.subtractSubtrahend()),
                (Cobol.Word) visit(ctx.FROM()),
                (Name) visit(ctx.subtractMinuendGiving()),
                (Cobol.Word) visit(ctx.GIVING()),
                convertAll(ctx.subtractGiving())
        );
    }

    @Override
    public Object visitSubtractFromStatement(CobolParser.SubtractFromStatementContext ctx) {
        return new Cobol.SubtractFromStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.subtractSubtrahend()),
                (Cobol.Word) visit(ctx.FROM()),
                convertAll(ctx.subtractMinuend())
        );
    }

    @Override
    public Object visitSubtractMinuendCorresponding(CobolParser.SubtractMinuendCorrespondingContext ctx) {
        return new Cobol.SubtractMinuendCorresponding(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                visitNullable(ctx.ROUNDED())
        );
    }

    @Override
    public Object visitSubtractStatement(CobolParser.SubtractStatementContext ctx) {
        return new Cobol.Subtract(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.SUBTRACT()),
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
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.symbolicCharacter()),
                ctx.IS() != null ? (Cobol.Word) visit(ctx.IS()) :
                        ctx.ARE() != null ? (Cobol.Word) visit(ctx.ARE()) : null,
                convertAll(ctx.integerLiteral())
        );
    }

    @Override
    public Cobol.SymbolicCharactersClause visitSymbolicCharactersClause(CobolParser.SymbolicCharactersClauseContext ctx) {
        return new Cobol.SymbolicCharactersClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SYMBOLIC(), ctx.CHARACTERS(), ctx.FOR(), ctx.ALPHANUMERIC(), ctx.NATIONAL()),
                convertAll(ctx.symbolicCharacters()),
                visitNullable(ctx.IN()),
                visitNullable(ctx.alphabetName())
        );
    }

    @Override
    public Object visitSymbolicDestinationClause(CobolParser.SymbolicDestinationClauseContext ctx) {
        return new Cobol.SymbolicDestinationClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SYMBOLIC(), ctx.DESTINATION(), ctx.IS()),
                visitNullable(ctx.dataDescName())
        );
    }

    @Override
    public Object visitSymbolicQueueClause(CobolParser.SymbolicQueueClauseContext ctx) {
        return new Cobol.SymbolicQueueClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SYMBOLIC(), ctx.QUEUE(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitSymbolicSourceClause(CobolParser.SymbolicSourceClauseContext ctx) {
        return new Cobol.SymbolicSourceClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SYMBOLIC(), ctx.SOURCE(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitSymbolicSubQueueClause(CobolParser.SymbolicSubQueueClauseContext ctx) {
        return new Cobol.SymbolicSubQueueClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SYMBOLIC(), ctx.SUB_QUEUE_1(), ctx.SUB_QUEUE_2(), ctx.SUB_QUEUE_3(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitSymbolicTerminalClause(CobolParser.SymbolicTerminalClauseContext ctx) {
        return new Cobol.SymbolicTerminalClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.SYMBOLIC(), ctx.TERMINAL(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Cobol.TableCall visitTableCall(CobolParser.TableCallContext ctx) {
        return new Cobol.TableCall(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName()),
                convertAll(ctx.tableCallSubscripts()),
                visitNullable(ctx.referenceModifier())
        );
    }

    @Override
    public Cobol.Parenthesized visitTableCallSubscripts(CobolParser.TableCallSubscriptsContext ctx) {
        return new Cobol.Parenthesized(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.LPARENCHAR()),
                convertAllList(ctx.COMMACHAR(), ctx.subscript()),
                (Cobol.Word) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public Cobol.Word visitTerminal(TerminalNode node) {
        List<Marker> markers = new ArrayList<>();
        Space prefix = processTokenText(node.getText(), markers);
        String text = END_OF_FILE.equals(node.getText()) ? "" :
                node.getText().startsWith(COMMENT_ENTRY) ? node.getText().substring(COMMENT_ENTRY.length()) : node.getText();
        return new Cobol.Word(
                randomId(),
                prefix,
                markers.isEmpty() ? Markers.EMPTY : Markers.build(markers),
                text
        );
    }

    @Override
    public Object visitTerminateStatement(CobolParser.TerminateStatementContext ctx) {
        return new Cobol.Terminate(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.TERMINATE()),
                (Cobol.QualifiedDataName) visit(ctx.reportName())
        );
    }

    @Override
    public Object visitTextLengthClause(CobolParser.TextLengthClauseContext ctx) {
        return new Cobol.TextLengthClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.TEXT(), ctx.LENGTH(), ctx.IS()),
                (Cobol.Word) visit(ctx.dataDescName())
        );
    }

    @Override
    public Object visitUnstringCountIn(CobolParser.UnstringCountInContext ctx) {
        return new Cobol.UnstringCountIn(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.COUNT(), ctx.IN()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitUnstringDelimitedByPhrase(CobolParser.UnstringDelimitedByPhraseContext ctx) {
        return new Cobol.UnstringDelimitedByPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DELIMITED(), ctx.BY(), ctx.ALL()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitUnstringDelimiterIn(CobolParser.UnstringDelimiterInContext ctx) {
        return new Cobol.UnstringDelimiterIn(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.DELIMITER(), ctx.IN()),
                (Identifier) visit(ctx.identifier())
        );
    }

    @Override
    public Object visitUnstringInto(CobolParser.UnstringIntoContext ctx) {
        return new Cobol.UnstringInto(
                randomId(),
                Space.EMPTY,
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
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.INTO()),
                convertAll(ctx.unstringInto())
        );
    }

    @Override
    public Object visitUnstringOrAllPhrase(CobolParser.UnstringOrAllPhraseContext ctx) {
        return new Cobol.UnstringOrAllPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.OR(), ctx.ALL()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitUnstringSendingPhrase(CobolParser.UnstringSendingPhraseContext ctx) {
        return new Cobol.UnstringSendingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Identifier) visit(ctx.identifier()),
                visitNullable(ctx.unstringDelimitedByPhrase()),
                convertAll(ctx.unstringOrAllPhrase())
        );
    }

    @Override
    public Object visitUnstringStatement(CobolParser.UnstringStatementContext ctx) {
        return new Cobol.UnString(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.UNSTRING()),
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
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.TALLYING(), ctx.IN()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitUnstringWithPointerPhrase(CobolParser.UnstringWithPointerPhraseContext ctx) {
        return new Cobol.UnstringWithPointerPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WITH(), ctx.POINTER()),
                (Cobol.QualifiedDataName) visit(ctx.qualifiedDataName())
        );
    }

    @Override
    public Object visitUseAfterClause(CobolParser.UseAfterClauseContext ctx) {
        return new Cobol.UseAfterClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.GLOBAL(), ctx.AFTER(), ctx.STANDARD(), ctx.EXCEPTION(), ctx.ERROR(), ctx.PROCEDURE(), ctx.ON()),
                (Cobol.UseAfterOn) visit(ctx.useAfterOn())
        );
    }

    @Override
    public Object visitUseAfterOn(CobolParser.UseAfterOnContext ctx) {
        return new Cobol.UseAfterOn(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.INPUT(), ctx.OUTPUT(), ctx.I_O(), ctx.EXTEND()),
                convertAll(ctx.fileName())
        );
    }

    @Override
    public Object visitUseDebugClause(CobolParser.UseDebugClauseContext ctx) {
        return new Cobol.UseDebugClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.FOR(), ctx.DEBUGGING(), ctx.ON()),
                convertAll(ctx.useDebugOn())
        );
    }

    @Override
    public Object visitUseDebugOn(CobolParser.UseDebugOnContext ctx) {
        return new Cobol.UseDebugOn(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.ALL(), ctx.PROCEDURES(), ctx.REFERENCES(), ctx.OF()),
                ctx.PROCEDURES() != null ? null : visit(ctx.identifier(), ctx.fileName(), ctx.procedureName())
        );
    }

    @Override
    public Object visitUseStatement(CobolParser.UseStatementContext ctx) {
        return new Cobol.UseStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.USE()),
                visit(ctx.useAfterClause(), ctx.useDebugClause())
        );
    }

    @Override
    public Object visitValueOfClause(CobolParser.ValueOfClauseContext ctx) {
        return new Cobol.ValueOfClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.VALUE(), ctx.OF()),
                convertAll(ctx.valuePair())
        );
    }

    @Override
    public Object visitValuePair(CobolParser.ValuePairContext ctx) {
        return new Cobol.ValuePair(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.systemName()),
                visitNullable(ctx.IS()),
                visit(ctx.qualifiedDataName(), ctx.literal())
        );
    }

    @Override
    public Cobol.WorkingStorageSection visitWorkingStorageSection(CobolParser.WorkingStorageSectionContext ctx) {
        return new Cobol.WorkingStorageSection(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.WORKING_STORAGE(), ctx.SECTION()),
                (Cobol.Word) visit(ctx.DOT_FS()),
                convertAll(ctx.dataDescriptionEntry())
        );
    }

    @Override
    public Object visitWriteAdvancingLines(CobolParser.WriteAdvancingLinesContext ctx) {
        return new Cobol.WriteAdvancingLines(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.identifier(), ctx.literal()),
                ctx.LINE() != null ? (Cobol.Word) visit(ctx.LINE()) :
                        ctx.LINES() != null ? (Cobol.Word) visit(ctx.LINES()) : null
        );
    }

    @Override
    public Object visitWriteAdvancingMnemonic(CobolParser.WriteAdvancingMnemonicContext ctx) {
        return new Cobol.WriteAdvancingMnemonic(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Name) visit(ctx.mnemonicName())
        );
    }

    @Override
    public Object visitWriteAdvancingPage(CobolParser.WriteAdvancingPageContext ctx) {
        return new Cobol.WriteAdvancingPage(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.PAGE())
        );
    }

    @Override
    public Object visitWriteAdvancingPhrase(CobolParser.WriteAdvancingPhraseContext ctx) {
        return new Cobol.WriteAdvancingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.BEFORE(), ctx.AFTER(), ctx.ADVANCING()),
                visit(ctx.writeAdvancingPage(), ctx.writeAdvancingLines(), ctx.writeAdvancingMnemonic())
        );
    }

    @Override
    public Object visitWriteAtEndOfPagePhrase(CobolParser.WriteAtEndOfPagePhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.AT(), ctx.END_OF_PAGE(), ctx.EOP()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitWriteFromPhrase(CobolParser.WriteFromPhraseContext ctx) {
        return new Cobol.WriteFromPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visitNullable(ctx.FROM()),
                visit(ctx.identifier(), ctx.literal())
        );
    }

    @Override
    public Object visitWriteNotAtEndOfPagePhrase(CobolParser.WriteNotAtEndOfPagePhraseContext ctx) {
        return new Cobol.StatementPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.NOT(), ctx.AT(), ctx.END_OF_PAGE(), ctx.EOP()),
                convertAll(ctx.statement())
        );
    }

    @Override
    public Object visitWriteStatement(CobolParser.WriteStatementContext ctx) {
        return new Cobol.Write(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (Cobol.Word) visit(ctx.WRITE()),
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
        int endIndex = indexOfNextNonWhitespace(cursor, source);
        String prefix = source.substring(cursor, endIndex);
        cursor += prefix.length();
        return format(prefix);
    }

    private int indexOfNextNonWhitespace(int cursor, String source) {
        int delimIndex = cursor;
        boolean isColumnArea = false;
        for (; delimIndex < source.length(); delimIndex++) {
            if (source.length() > delimIndex + 1) {
                // separators are equivalent to a whitespace character, but are specific combinations of characters based on the dialect.
                // I.E. In IBM-ANSI-85, comma space `, ` or semicolon space `; ` are equivalent to a space.
                if (separators.contains(source.substring(delimIndex, delimIndex + 2))) {
                    continue;
                }
            }

            // Do not consume whitespace in blank column areas.
            isColumnArea = sequenceAreas.containsKey(delimIndex) || indicatorAreas.containsKey(delimIndex) || commentAreas.containsKey(delimIndex);
            if (!Character.isWhitespace(source.substring(delimIndex, delimIndex + 1).charAt(0)) || isColumnArea) {
                break; // found it!
            }
        }

        if (!isColumnArea && nextIndex != null && nextIndex > 0) {
            int totalWhitespace = (delimIndex - cursor);
            int prefixCount = nextIndex > totalWhitespace ? nextIndex - totalWhitespace : totalWhitespace - nextIndex;
            int templateWhitespace = totalWhitespace - prefixCount;
            this.cursor += templateWhitespace;

            delimIndex = this.cursor + prefixCount;
            nextIndex = null;
            removeColumnMarkers = true;
        }
        return delimIndex;
    }

    private @Nullable List<Cobol.Word> wordsList(TerminalNode... wordNodes) {
        List<Cobol.Word> words = new ArrayList<>(wordNodes.length);
        for (TerminalNode wordNode : wordNodes) {
            if (wordNode != null) {
                Cobol.Word cw = (Cobol.Word) visit(wordNode);
                words.add(cw);
            }
        }

        if (words.isEmpty()) {
            return null;
        }

        return words;
    }

    private <C, T extends ParseTree> List<C> convertAll(List<T> trees, Function<T, C> convert) {
        List<C> converted = new ArrayList<>(trees.size());
        for (T tree : trees) {
            converted.add(convert.apply(tree));
        }
        return converted;
    }

    @SafeVarargs
    private final <C extends Cobol> List<C> convertAll(List<? extends ParserRuleContext>... trees) {
        return convertAll(Arrays.stream(trees)
                .filter(Objects::nonNull)
                .flatMap(Collection::stream)
                .sorted(Comparator.comparingInt(it -> it.start.getStartIndex()))
                .collect(Collectors.toList()));
    }

    private <C extends Cobol, T extends ParseTree> List<C> convertAll(List<T> trees) {
        //noinspection unchecked
        return convertAll(trees, t -> (C) visit(t));
    }

    @SafeVarargs
    private final List<Cobol> convertAllList(List<? extends ParseTree>... trees) {
        return Arrays.stream(trees)
                .flatMap(Collection::stream)
                .filter(Objects::nonNull)
                .sorted(Comparator.comparingInt(it -> it instanceof TerminalNode ? ((TerminalNode) it).getSymbol().getStartIndex() :
                        ((ParserRuleContext) it).getStart().getStartIndex()))
                .map(it -> (Cobol) visit(it))
                .collect(Collectors.toList());
    }

    /**
     * Return the prefix of the TerminalNode AND collect applicable markers.
     * Markers consist of COBOL areas that are removed during preprocessing.
     */
    private Space processTokenText(String text, List<Marker> markers) {

        parseCommentsAndEmptyLines(text, markers);

        int saveCursor = cursor;
        sequenceArea();
        indicatorArea();

        Integer nextIndicator = indicatorAreas.keySet().stream()
                .filter(it -> it > cursor)
                .findFirst()
                .orElse(null);
        boolean isContinued = nextIndicator != null && indicatorAreas.get(nextIndicator).equals("-");
        cursor = saveCursor;

        Character delimiter = null;
        if (text.startsWith("'") || text.startsWith("\"")) {
            delimiter = text.charAt(0);
        }

        // A literal continued on a new line.
        if (delimiter != null && isContinued) {
            return processLiteral(text, markers, delimiter);
        } else if (END_OF_FILE.equals(text) && source.substring(cursor).isEmpty()) {
            return Space.EMPTY;
        }

        return processText(text, markers, isContinued);
    }

    private void parseCommentsAndEmptyLines(String text, List<Marker> markers) {
        int saveCursor = cursor;
        SequenceArea sequenceArea = sequenceArea();
        IndicatorArea indicatorArea = indicatorArea();

        // CommentEntry tags are required to be recognized the COBOL grammar.
        // The CommentEntry tag (hopefully does not exist in the original source code.) and is removed before generating the AST.
        boolean isCommentEntry = text.startsWith(COMMENT_ENTRY);
        if (!isCommentEntry && indicatorArea != null) {

            List<Lines.Line> lines = new ArrayList<>();

            int iterations = 0;
            int max = 250;
            while (iterations < max) {
                // Stop after all the trailing comments have been parsed.
                if (source.substring(cursor).isEmpty()) {
                    break;
                }

                String contentArea = source.substring(cursor, cursor - cobolDialect.getColumns().getIndicatorArea() - 1 + cobolDialect.getColumns().getOtherArea());
                if (!(isCommentIndicator(indicatorArea) || contentArea.trim().isEmpty() || templateKeys.contains(contentArea))) {
                    break;
                }

                if (templateKeys.contains(contentArea)) {
                    if (replaceByStartComment.equals(contentArea)) {
                        ReplaceBy replaceBy = getReplaceByMarker();
                        markers.add(replaceBy);
                    } else if (replaceStartComment.equals(contentArea)) {
                        replaceStartComment();
                    } else if (replaceAdditiveWhitespaceComment.equals(contentArea)) {
                        replaceAdditiveWhitespace();
                    } else if (replaceUuidComment.equals(contentArea)) {
                        Replace replace = getReplaceMarker();
                        markers.add(replace);
                    } else if (replaceAdditiveTypeStartComment.equals(contentArea)) {
                        replaceAdditiveStartComment();
                    } else if (replaceAdditiveTypeStopComment.equals(contentArea)) {
                        replaceAdditiveStopComment();
                    } else if (replaceReductiveTypeStartComment.equals(contentArea)) {
                        ReplaceReductiveType replaceReductiveType = getReplaceReductiveTypeMarker();
                        markers.add(replaceReductiveType);
                    } else if (replaceAddWordStartComment.equals(contentArea)) {
                        parseComment(replaceAddWordStartComment);
                        iterations = max;
                    } else if (replaceAddWordStopComment.equals(contentArea)) {
                        parseComment(replaceAddWordStopComment);
                    } else if (replaceOffStartComment.equals(contentArea)) {
                        ReplaceOff replaceOff = getReplaceOffMarker();
                        markers.add(replaceOff);
                    } else if (copyStartComment.equals(contentArea)) {
                        copyStartComment();
                    } else if (copyUuidComment.equals(contentArea)) {
                        copyUuidComment();
                    } else if (copyStopComment.equals(contentArea)) {
                        copyStopComment();
                    }
                } else {
                    cursor += contentArea.length();
                    CommentArea commentArea = commentArea();
                    Lines.Line line = new Lines.Line(randomId(), sequenceArea, indicatorArea, contentArea, commentArea, inCopiedText);
                    lines.add(line);
                }

                saveCursor = cursor;
                sequenceArea = sequenceArea();
                indicatorArea = indicatorArea();
                iterations++;
            }
            if (!lines.isEmpty()) {
                markers.add(new Lines(randomId(), lines));
            }
        }

        cursor = saveCursor;
    }

    private Space processLiteral(String text, List<Marker> markers, Character delimiter) {
        Map<Integer, Markers> continuations = new HashMap<>();
        List<Marker> continuation = new ArrayList<>(2);

        // Check if the literal starts at the beginning of a line.
        SequenceArea sequenceArea = sequenceArea();
        IndicatorArea indicatorArea = indicatorArea();

        if (sequenceArea != null) {
            continuation.add(sequenceArea);
        }

        if (indicatorArea != null) {
            continuation.add(indicatorArea);
        }

        // Set the Word prefix.
        Space prefix = whitespace();

        // Add a continuation at position 0 to print before the literal starts.
        if (!continuation.isEmpty()) {
            continuations.put(0, Markers.build(continuation));
        }

        int matchedCount = 0;
        int iterations = 0;
        while (matchedCount < text.length() && iterations < 250) {
            continuation = new ArrayList<>(3);

            String current = source.substring(cursor);
            char[] charArray = text.substring(matchedCount).toCharArray();
            char[] sourceArray = current.toCharArray();

            int end = 0;
            for (; end < charArray.length; end++) {
                if (charArray[end] != sourceArray[end] || commentAreas.containsKey(cursor)) {
                    break;
                }
                cursor++;
            }

            String matchedText = current.substring(0, end);
            matchedCount += matchedText.length();

            CommentArea commentArea = commentArea();
            if (commentArea != null) {
                continuation.add(commentArea);
            }

            if (matchedCount == text.length()) {
                if (!continuation.isEmpty()) {
                    continuations.put(matchedCount + 1, Markers.build(continuation));
                }
                break;
            }

            sequenceArea = sequenceArea();
            indicatorArea = indicatorArea(delimiter, true);

            if (sequenceArea != null) {
                continuation.add(sequenceArea);
            }
            if (indicatorArea != null) {
                continuation.add(indicatorArea);
            }

            if (!continuation.isEmpty()) {
                continuations.put(matchedCount, Markers.build(continuation));
            }

            iterations++;
        }

        markers.add(new Continuation(randomId(), continuations));
        if (currentCopy != null) {
            markers.add(new Copy(randomId(), currentCopy));
        }
        return prefix;
    }

    private Space processText(String text, List<Marker> markers, boolean checkContinuation) {
        SequenceArea sequenceArea = sequenceArea();
        IndicatorArea indicatorArea = indicatorArea();

        // CommentEntry tags are required to be recognized the COBOL grammar.
        // The CommentEntry tag (hopefully does not exist in the original source code.) and is removed before generating the AST.
        boolean isCommentEntry = text.startsWith(COMMENT_ENTRY);
        if (isCommentEntry) {
            text = text.substring(COMMENT_ENTRY.length());
        }

        // An inline comment entry will have a null sequence area.
        Space prefix = isCommentEntry ? Space.EMPTY : whitespace();

        boolean isContinued = false;
        if (checkContinuation) {
            // CommentAreas are optional text that will precede the end of line.
            Integer nextCommentArea = commentAreas.keySet().stream()
                    .filter(it -> it > cursor)
                    .findFirst()
                    .orElse(null);

            String current = source.substring(cursor);
            int newLinePos = current.indexOf("\n");
            int endPos = (nextCommentArea != null && nextCommentArea < (newLinePos + cursor)) ? nextCommentArea : (newLinePos + cursor);

            current = source.substring(cursor, endPos).trim();
            // There are two types of continuations.
            // 1. The text is a grammar token followed by a new line, which is handled by normal markers.
            // 2. The text is a combination of two tokens, which has been parsed as a single word and requires continuation markers.
            if (!current.startsWith(text)) {
                Map<Integer, Markers> continuations = new HashMap<>();
                List<Marker> continuation = new ArrayList<>(2);

                if (sequenceArea != null) {
                    continuation.add(sequenceArea);
                }

                if (indicatorArea != null) {
                    continuation.add(indicatorArea);
                }

                if (!continuation.isEmpty()) {
                    continuations.put(0, Markers.build(continuation));
                }

                int matchedCount = 0;
                int iterations = 0;
                while (iterations < 250) {
                    continuation = new ArrayList<>(3);

                    current = source.substring(cursor);
                    char[] charArray = text.substring(matchedCount).toCharArray();
                    char[] sourceArray = current.toCharArray();

                    int end = 0;
                    for (; end < charArray.length; end++) {
                        if (charArray[end] != sourceArray[end] || commentAreas.containsKey(cursor)) {
                            break;
                        }
                        cursor++;
                    }

                    String matchedText = current.substring(0, end);
                    matchedCount += matchedText.length();

                    CommentArea commentArea = commentArea();
                    if (commentArea != null) {
                        continuation.add(commentArea);
                    }

                    if (matchedCount == text.length()) {
                        if (!continuation.isEmpty()) {
                            continuations.put(matchedCount + 1, Markers.build(continuation));
                        }
                        break;
                    }

                    sequenceArea = sequenceArea();
                    if (sequenceArea != null) {
                        continuation.add(sequenceArea);
                    }

                    indicatorArea = indicatorArea(text.charAt(matchedCount), false);
                    if (indicatorArea != null) {
                        continuation.add(indicatorArea);
                    }

                    if (!continuation.isEmpty()) {
                        continuations.put(matchedCount, Markers.build(continuation));
                    }

                    iterations++;
                }
                markers.add(new Continuation(randomId(), continuations));
                isContinued = true;
            }
        }

        // A replaceAdditiveType means a replacement clause added additional words to the AST.
        // The prefixes were generated by the preprocessor from the matched replaced clause.
        // The prefix is injected into the source code via a comment here.
        if (replaceAdditiveType != null) {
            markers.add(replaceAdditiveType);

            cursor += text.length();
            int end = source.substring(cursor).indexOf("\n") + 1;
            cursor += end;
        } else if (!isContinued) {
            if (removeColumnMarkers) {
                removeColumnMarkers = false;
            } else {
                if (sequenceArea != null) {
                    markers.add(sequenceArea);
                }

                if (indicatorArea != null) {
                    markers.add(indicatorArea);
                }
            }

            if (!END_OF_FILE.equals(text)) {
                cursor += text.length();

                CommentArea commentArea = commentArea();
                if (commentArea != null) {
                    markers.add(commentArea);
                }
            }
        }

        if (currentCopy != null) {
            markers.add(new Copy(randomId(), currentCopy));
        }
        return prefix;
    }

    private String getCommentFromKey(String key) {
        int contentAreaStart = cobolDialect.getColumns().getContentArea();
        int contentAreaEnd = cobolDialect.getColumns().getOtherArea();
        return key.substring(contentAreaStart, contentAreaEnd);
    }

    private void copyStartComment() {
        parseComment(copyStartComment);

        inCopiedText = true;
        removeTemplateCommentArea = true;

        // Reset copy info.
        currentCopy = null;
        removeColumnMarkers = false;
        nextIndex = null;
    }

    private void copyStopComment() {
        parseComment(copyStopComment);

        currentCopy = null;
        inCopiedText = false;

        sequenceArea();
        indicatorArea();

        String numberOfSpaces = source.substring(cursor, cursor + source.substring(cursor).indexOf("\n") + 1);
        cursor += numberOfSpaces.length();
        nextIndex = Integer.valueOf(numberOfSpaces.trim());
    }

    private void copyUuidComment() {
        parseComment(copyUuidComment);

        removeTemplateCommentArea = false;

        String uuid = getUuid();
        currentCopy = copyMap.get(uuid.trim());
    }

    private void replaceByStartComment() {
        parseComment(replaceByStartComment);

        removeTemplateCommentArea = true;
        isAdditiveCommentArea = false;

        // Reset replace info.
        removeColumnMarkers = false;
        nextIndex = null;

        String current = source.substring(cursor);
        current = current.substring(0, current.indexOf("\n") + 1);
        cursor += current.length();
    }

    private void replaceAdditiveWhitespace() {
        parseComment(replaceAdditiveWhitespaceComment);

        removeTemplateCommentArea = false;
        isAdditiveCommentArea = true;
    }

    private void replaceStartComment() {
        parseComment(replaceStartComment);

        removeTemplateCommentArea = true;

        // Reset replace info.
        removeColumnMarkers = false;
        nextIndex = null;
    }

    private void replaceAdditiveStartComment() {
        parseComment(replaceAdditiveTypeStartComment);

        String current = source.substring(cursor);
        current = current.substring(0, current.indexOf("\n") + 1);
        cursor += current.length();

        parseComment(uuidComment);

        removeTemplateCommentArea = false;

        String uuid = getUuid();

        this.replaceAdditiveType = replaceAdditiveTypeMap.get(uuid.trim());
    }

    private void replaceAdditiveStopComment() {
        parseComment(replaceAdditiveTypeStopComment);

        sequenceArea();
        indicatorArea();

        String numberOfSpaces = source.substring(cursor, cursor + source.substring(cursor).indexOf("\n") + 1);
        cursor += numberOfSpaces.length();
        nextIndex = Integer.valueOf(numberOfSpaces.trim());

        this.replaceAdditiveType = null;
    }

    private void replaceOffStartComment() {
        parseComment(replaceOffStartComment);

        String current = source.substring(cursor);
        current = current.substring(0, current.indexOf("\n") + 1);
        cursor += current.length();
    }

    private Replace getReplaceMarker() {
        parseComment(replaceUuidComment);

        removeTemplateCommentArea = false;

        String uuid = getUuid();

        Replace replace = replaceMap.get(uuid.trim());

        parseComment(replaceStopComment);

        if (isAdditiveCommentArea) {
            isAdditiveCommentArea = false;
        } else {
            sequenceArea();
            indicatorArea();

            String numberOfSpaces = source.substring(cursor, cursor + source.substring(cursor).indexOf("\n") + 1);
            cursor += numberOfSpaces.length();
            nextIndex = Integer.valueOf(numberOfSpaces.trim());
        }

        return replace;
    }

    private ReplaceReductiveType getReplaceReductiveTypeMarker() {
        parseComment(replaceReductiveTypeStartComment);

        parseComment(uuidComment);

        removeTemplateCommentArea = false;

        String uuid = getUuid();

        ReplaceReductiveType replaceReductiveType = replaceReductiveTypeMap.get(uuid.trim());

        parseComment(replaceReductiveTypeStopComment);

        sequenceArea();
        indicatorArea();

        String numberOfSpaces = source.substring(cursor, cursor + source.substring(cursor).indexOf("\n") + 1);
        cursor += numberOfSpaces.length();
        nextIndex = Integer.valueOf(numberOfSpaces.trim());

        return replaceReductiveType;
    }

    private ReplaceOff getReplaceOffMarker() {
        replaceOffStartComment();

        parseComment(uuidComment);

        String uuid = getUuid();

        parseComment(replaceOffStopComment);

        sequenceArea();
        indicatorArea();

        // Unknown; this might content with other methods that set nextIndex.
        String numberOfSpaces = source.substring(cursor, cursor + source.substring(cursor).indexOf("\n") + 1);
        cursor += numberOfSpaces.length();
        nextIndex = Integer.valueOf(numberOfSpaces.trim());

        CobolPreprocessor.ReplaceOffStatement statement = replaceOffMap.get(uuid.trim());
        return new ReplaceOff(randomId(), statement);
    }

    private ReplaceBy getReplaceByMarker() {
        replaceByStartComment();

        parseComment(uuidComment);

        String uuid = getUuid();

        parseComment(replaceByStopComment);

        sequenceArea();
        indicatorArea();

        String numberOfSpaces = source.substring(cursor, cursor + source.substring(cursor).indexOf("\n") + 1);
        cursor += numberOfSpaces.length();
        nextIndex = Integer.valueOf(numberOfSpaces.trim());

        CobolPreprocessor.ReplaceByStatement statement = replaceByMap.get(uuid.trim());
        return new ReplaceBy(randomId(), statement);
    }

    private void parseComment(String comment) {
        sequenceArea();
        indicatorArea();

        cursor += comment.length();
        cursor++; // Increment passed the \n.
    }

    private String getUuid() {
        sequenceArea();
        indicatorArea();
        String uuid = source.substring(cursor, cursor + source.substring(cursor).indexOf("\n") + 1);
        cursor += uuid.length();
        return uuid;
    }

    private boolean isCommentIndicator(@Nullable IndicatorArea area) {
        boolean isUnknownIndicator = area != null && ("G".equals(area.getIndicator()) || "J".equals(area.getIndicator()) || "P".equals(area.getIndicator()));
        return area != null && (isUnknownIndicator || commentIndicators.contains(area.getIndicator().charAt(0)));
    }

    @Nullable
    private SequenceArea sequenceArea() {
        if (sequenceAreas.containsKey(cursor)) {
            String sequence = sequenceAreas.get(cursor);
            cursor += sequence.length();

            return new SequenceArea(randomId(), sequence);
        }
        return null;
    }

    @Nullable
    private IndicatorArea indicatorArea() {
        return indicatorArea(null, false);
    }

    /**
     * Return the IndicatorArea based on the current cursor position if it exists.
     *
     * @param continuationDelimiter the next expected Character in the source that comes after the indicator.
     * @param isStringLiteral String literals and Keywords/Identifiers have different rules for line continuations.
     *                        A continued String literal will be prefixed by the delimiter (' or "),
     *                        which needs to exist in the indicator marker.
     *                        I.E. 000001-|<whitespace including the delimiter " or '>|some continued string literal.
     *
     *                        A continued Keyword/Identifier should not include the delimiter.
     *                        I.E. 000001-|<whitespace added to indicator>|TOKEN-NAME.
     */
    @Nullable
    private IndicatorArea indicatorArea(@Nullable Character continuationDelimiter, boolean isStringLiteral) {
        if (indicatorAreas.containsKey(cursor)) {
            String indicatorArea = indicatorAreas.get(cursor);
            cursor += indicatorArea.length();

            String continuationText = null;
            if (continuationDelimiter != null) {
                String current = source.substring(cursor);
                int pos = current.indexOf(continuationDelimiter);
                if (pos != -1) {
                    int endPos = (isStringLiteral ? 1 : 0) + current.indexOf(continuationDelimiter);
                    continuationText = current.substring(0, endPos);
                    cursor += continuationText.length();
                }
            }

            return new IndicatorArea(randomId(), indicatorArea, continuationText);
        }
        return null;
    }

    /**
     * Return the CommentArea based on the current cursor position if it exists.
     * `removeTemplateCommentArea` is used to remove CommentAreas that were added by the COBOL copy template.
     */
    @Nullable
    private CommentArea commentArea() {
        int saveCursor = cursor;

        Space before = whitespace();
        String comment = null;
        Space endLine = Space.EMPTY;

        if (commentAreas.containsKey(cursor)) {
            comment = commentAreas.get(cursor);
            cursor += comment.length();
            endLine = whitespace();
        }

        if (!(removeTemplateCommentArea && comment == null && before.getWhitespace().endsWith("\n")) && (before.getWhitespace().endsWith("\n") || comment != null)) {
            return new CommentArea(randomId(), before, comment == null ? "" : comment, endLine, isAdditiveCommentArea);
        }

        if (!removeTemplateCommentArea || !before.getWhitespace().endsWith("\n")) {
            cursor = saveCursor;
        }
        return null;
    }
}
