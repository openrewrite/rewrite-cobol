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
import org.openrewrite.FileAttributes;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorBaseVisitor;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Collections.singletonList;
import static org.openrewrite.Tree.randomId;
import static org.openrewrite.cobol.internal.CobolGrammarToken.COMMENT_ENTRY;
import static org.openrewrite.cobol.internal.CobolGrammarToken.END_OF_FILE;
import static org.openrewrite.cobol.tree.Space.format;

public class CobolPreprocessorParserVisitor extends CobolPreprocessorBaseVisitor<Object> {

    private final Path path;

    @Nullable
    private final FileAttributes fileAttributes;

    private final String source;
    private final Charset charset;
    private final boolean charsetBomMarked;
    private final CobolDialect cobolDialect;

    // Areas may be a Set of Integer to reduce memory, each method to create the marker would generate the string.
    private final Map<Integer, String> sequenceAreas = new HashMap<>();

    // A LinkedHash map is used for Indicators to collect information about the next line of code. I.E. continuation indicators.
    private final Map<Integer, String> indicatorAreas = new LinkedHashMap<>();
    // A LinkedHash map is used for CommentAreas to determine if the current token ends the line.
    private final Map<Integer, String> commentAreas = new LinkedHashMap<>();

    private final Set<String> separators = new HashSet<>();
    private final Set<Character> commentIndicators = new HashSet<>();
    private int cursor = 0;

    public CobolPreprocessorParserVisitor(Path path, @Nullable FileAttributes fileAttributes,
                                          String source, Charset charset, boolean charsetBomMarked, CobolDialect cobolDialect) {
        this.path = path;
        this.fileAttributes = fileAttributes;
        this.source = source;
        this.charset = charset;
        this.charsetBomMarked = charsetBomMarked;
        this.cobolDialect = cobolDialect;
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
    public CobolPreprocessor.CompilationUnit visitStartRule(CobolPreprocessorParser.StartRuleContext ctx) {
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
        } else if (cobolDialect.getColumns() == CobolDialect.Columns.HP_TANDEM) {
            throw new UnsupportedOperationException("Implement me.");
        } else {
            throw new UnsupportedOperationException("CobolDialect is not supported: " + cobolDialect.getColumns().name());
        }
    }

    @Override
    public Object visitCharData(CobolPreprocessorParser.CharDataContext ctx) {
        return new CobolPreprocessor.CharData(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.charDataLine())
        );
    }

    @Override
    public Object visitCharDataKeyword(CobolPreprocessorParser.CharDataKeywordContext ctx) {
        // Pass through.
        return super.visitCharDataKeyword(ctx);
    }

    @Override
    public Object visitCharDataLine(CobolPreprocessorParser.CharDataLineContext ctx) {
        return new CobolPreprocessor.CharDataLine(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(ctx.cobolWord(), ctx.literal(), ctx.filename(), ctx.commentEntry(), ctx.TEXT(), ctx.DOT(), ctx.LPARENCHAR(), ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitCharDataLineNoDot(CobolPreprocessorParser.CharDataLineNoDotContext ctx) {
        return new CobolPreprocessor.CharDataLine(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(ctx.cobolWord(), ctx.literal(), ctx.filename(), ctx.commentEntry(), ctx.TEXT(), ctx.LPARENCHAR(), ctx.RPARENCHAR())
        );
    }

    @Override
    public Object visitCharDataSql(CobolPreprocessorParser.CharDataSqlContext ctx) {
        return new CobolPreprocessor.CharDataSql(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(ctx.charDataLine())
        );
    }

    @Override
    public Object visitCobolWord(CobolPreprocessorParser.CobolWordContext ctx) {
        // Pass through.
        return super.visitCobolWord(ctx);
    }

    @Override
    public Object visitCommentEntry(CobolPreprocessorParser.CommentEntryContext ctx) {
        return new CobolPreprocessor.CommentEntry(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAll(ctx.COMMENTENTRYLINE())
        );
    }

    @Override
    public Object visitCompilerOption(CobolPreprocessorParser.CompilerOptionContext ctx) {
        return new CobolPreprocessor.CompilerOption(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                convertAllList(
                        // Each item is listed in alphabetical order to reduce risk of types, and make it simpler to
                        // find missing compiler options.
                        // `convertAllList` will sort each of the terminals across all the lists based on the symbol order.
                        singletonList(ctx.ADATA()),
                        singletonList(ctx.ADV()),
                        singletonList(ctx.ALIAS()),
                        singletonList(ctx.ANSI()),
                        singletonList(ctx.ANY()),
                        singletonList(ctx.APOST()),
                        singletonList(ctx.AR()),
                        singletonList(ctx.ARITH()),
                        singletonList(ctx.AUTO()),
                        singletonList(ctx.AWO()),

                        singletonList(ctx.BIN()),
                        singletonList(ctx.BLOCK0()),
                        singletonList(ctx.BUF()),
                        singletonList(ctx.BUFSIZE()),

                        singletonList(ctx.C_CHAR()),
                        singletonList(ctx.CBLCARD()),
                        singletonList(ctx.CICS()),
                        singletonList(ctx.CO()),
                        singletonList(ctx.COBOL2()),
                        singletonList(ctx.COBOL3()),
                        singletonList(ctx.cobolWord()),
                        singletonList(ctx.CODEPAGE()),
                        ctx.COMMACHAR(),
                        singletonList(ctx.COMPAT()),
                        singletonList(ctx.COMPILE()),
                        singletonList(ctx.CP()),
                        singletonList(ctx.CPP()),
                        singletonList(ctx.CPSM()),
                        singletonList(ctx.CS()),
                        singletonList(ctx.CURR()),
                        singletonList(ctx.CURRENCY()),

                        singletonList(ctx.D_CHAR()),
                        singletonList(ctx.DATA()),
                        singletonList(ctx.DATEPROC()),
                        singletonList(ctx.DBCS()),
                        singletonList(ctx.DD()),
                        singletonList(ctx.DEBUG()),
                        singletonList(ctx.DECK()),
                        singletonList(ctx.DIAGTRUNC()),
                        singletonList(ctx.DLL()),
                        singletonList(ctx.DP()),
                        singletonList(ctx.DTR()),
                        singletonList(ctx.DU()),
                        singletonList(ctx.DUMP()),
                        singletonList(ctx.DYN()),
                        singletonList(ctx.DYNAM()),

                        ctx.E_CHAR(),
                        singletonList(ctx.EDF()),
                        singletonList(ctx.EJPD()),
                        singletonList(ctx.EN()),
                        singletonList(ctx.ENGLISH()),
                        singletonList(ctx.EPILOG()),
                        singletonList(ctx.EXIT()),
                        singletonList(ctx.EXP()),
                        singletonList(ctx.EXPORTALL()),
                        singletonList(ctx.EXTEND()),

                        singletonList(ctx.F_CHAR()),
                        singletonList(ctx.FASTSRT()),
                        singletonList(ctx.FEPI()),
                        singletonList(ctx.FLAG()),
                        singletonList(ctx.FLAGSTD()),
                        singletonList(ctx.FSRT()),
                        singletonList(ctx.FULL()),

                        singletonList(ctx.GDS()),
                        singletonList(ctx.GRAPHIC()),

                        singletonList(ctx.H_CHAR()),
                        singletonList(ctx.HOOK()),

                        ctx.I_CHAR(),
                        singletonList(ctx.INTDATE()),

                        singletonList(ctx.JA()),
                        singletonList(ctx.JP()),

                        singletonList(ctx.KA()),

                        singletonList(ctx.LANG()),
                        singletonList(ctx.LANGUAGE()),
                        singletonList(ctx.LC()),
                        singletonList(ctx.LEASM()),
                        singletonList(ctx.LENGTH()),
                        singletonList(ctx.LIB()),
                        singletonList(ctx.LINKAGE()),
                        singletonList(ctx.LIN()),
                        singletonList(ctx.LINECOUNT()),
                        singletonList(ctx.LIST()),
                        ctx.literal(),
                        singletonList(ctx.LILIAN()),
                        singletonList(ctx.LM()),
                        singletonList(ctx.LONGMIXED()),
                        singletonList(ctx.LONGUPPER()),
                        singletonList(ctx.LPARENCHAR()),
                        singletonList(ctx.LU()),

                        singletonList(ctx.M_CHAR()),
                        singletonList(ctx.MAP()),
                        singletonList(ctx.MARGINS()),
                        singletonList(ctx.MAX()),
                        singletonList(ctx.MD()),
                        singletonList(ctx.MDECK()),
                        singletonList(ctx.MIG()),
                        singletonList(ctx.MIXED()),

                        singletonList(ctx.N_CHAR()),
                        singletonList(ctx.NAME()),
                        singletonList(ctx.NAT()),
                        singletonList(ctx.NATIONAL()),
                        singletonList(ctx.NATLANG()),
                        singletonList(ctx.NN()),
                        singletonList(ctx.NOADATA()),
                        singletonList(ctx.NOADV()),
                        singletonList(ctx.NOALIAS()),
                        singletonList(ctx.NOAWO()),
                        singletonList(ctx.NOBLOCK0()),
                        singletonList(ctx.NOCBLCARD()),
                        singletonList(ctx.NOCICS()),
                        singletonList(ctx.NOCMPR2()),
                        singletonList(ctx.NOC()),
                        singletonList(ctx.NOCOMPILE()),
                        singletonList(ctx.NOCPSM()),
                        singletonList(ctx.NOCURR()),
                        singletonList(ctx.NOCURRENCY()),
                        singletonList(ctx.NOD()),
                        singletonList(ctx.NODATEPROC()),
                        singletonList(ctx.NODBCS()),
                        singletonList(ctx.NODE()),
                        singletonList(ctx.NODEBUG()),
                        singletonList(ctx.NODECK()),
                        singletonList(ctx.NODIAGTRUNC()),
                        singletonList(ctx.NODLL()),
                        singletonList(ctx.NODP()),
                        singletonList(ctx.NODTR()),
                        singletonList(ctx.NODYN()),
                        singletonList(ctx.NODYNAM()),
                        singletonList(ctx.NODU()),
                        singletonList(ctx.NODUMP()),
                        singletonList(ctx.NOEDF()),
                        singletonList(ctx.NOEJPD()),
                        singletonList(ctx.NOEPILOG()),
                        singletonList(ctx.NOEXIT()),
                        singletonList(ctx.NOEXP()),
                        singletonList(ctx.NOEXPORTALL()),
                        singletonList(ctx.NOF()),
                        singletonList(ctx.NOFASTSRT()),
                        singletonList(ctx.NOFEPI()),
                        singletonList(ctx.NOFLAG()),
                        singletonList(ctx.NOFLAGMIG()),
                        singletonList(ctx.NOFLAGSTD()),
                        singletonList(ctx.NOFSRT()),
                        singletonList(ctx.NOGRAPHIC()),
                        singletonList(ctx.NOHOOK()),
                        singletonList(ctx.NOLENGTH()),
                        singletonList(ctx.NOLIB()),
                        singletonList(ctx.NOLINKAGE()),
                        singletonList(ctx.NOLIST()),
                        singletonList(ctx.NOMAP()),
                        singletonList(ctx.NOMD()),
                        singletonList(ctx.NOMDECK()),
                        singletonList(ctx.NONAME()),
                        singletonList(ctx.NONUM()),
                        singletonList(ctx.NONUMBER()),
                        singletonList(ctx.NOOBJ()),
                        singletonList(ctx.NOOBJECT()),
                        singletonList(ctx.NOOFF()),
                        singletonList(ctx.NOOFFSET()),
                        singletonList(ctx.NOOPSEQUENCE()),
                        singletonList(ctx.NOOPT()),
                        singletonList(ctx.NOOPTIMIZE()),
                        singletonList(ctx.NOOPTIONS()),
                        singletonList(ctx.NOP()),
                        singletonList(ctx.NOPFD()),
                        singletonList(ctx.NOPROLOG()),
                        singletonList(ctx.NORENT()),
                        singletonList(ctx.NOS()),
                        singletonList(ctx.NOSEP()),
                        singletonList(ctx.NOSEPARATE()),
                        singletonList(ctx.NOSEQ()),
                        singletonList(ctx.NOSEQUENCE()),
                        singletonList(ctx.NOSOURCE()),
                        singletonList(ctx.NOSPIE()),
                        singletonList(ctx.NOSQL()),
                        singletonList(ctx.NOSQLC()),
                        singletonList(ctx.NOSQLCCSID()),
                        singletonList(ctx.NOSSR()),
                        singletonList(ctx.NOSSRANGE()),
                        singletonList(ctx.NOSTDTRUNC()),
                        singletonList(ctx.NOTERM()),
                        singletonList(ctx.NOTERMINAL()),
                        singletonList(ctx.NOTEST()),
                        singletonList(ctx.NOTHREAD()),
                        singletonList(ctx.NOTRIG()),
                        singletonList(ctx.NOVBREF()),
                        singletonList(ctx.NOWD()),
                        singletonList(ctx.NOWORD()),
                        singletonList(ctx.NOX()),
                        singletonList(ctx.NOXREF()),
                        singletonList(ctx.NOZWB()),
                        singletonList(ctx.NS()),
                        singletonList(ctx.NSEQ()),
                        singletonList(ctx.NSYMBOL()),
                        singletonList(ctx.NUM()),
                        singletonList(ctx.NUMBER()),
                        singletonList(ctx.NUMPROC()),

                        singletonList(ctx.OBJ()),
                        singletonList(ctx.OBJECT()),
                        singletonList(ctx.OFF()),
                        singletonList(ctx.OFFSET()),
                        singletonList(ctx.OPMARGINS()),
                        singletonList(ctx.OPSEQUENCE()),
                        singletonList(ctx.OP()),
                        singletonList(ctx.OPT()),
                        singletonList(ctx.OPTFILE()),
                        singletonList(ctx.OPTIMIZE()),
                        singletonList(ctx.OPTIONS()),
                        singletonList(ctx.OUT()),
                        singletonList(ctx.OUTDD()),

                        singletonList(ctx.PFD()),
                        singletonList(ctx.PGMN()),
                        singletonList(ctx.PGMNAME()),
                        singletonList(ctx.PROLOG()),

                        singletonList(ctx.Q_CHAR()),
                        singletonList(ctx.QUOTE()),

                        singletonList(ctx.RENT()),
                        singletonList(ctx.RMODE()),
                        singletonList(ctx.RPARENCHAR()),

                        ctx.S_CHAR(),
                        singletonList(ctx.SEP()),
                        singletonList(ctx.SEPARATE()),
                        singletonList(ctx.SEQ()),
                        singletonList(ctx.SEQUENCE()),
                        singletonList(ctx.SHORT()),
                        singletonList(ctx.SIZE()),
                        singletonList(ctx.SOURCE()),
                        singletonList(ctx.SP()),
                        singletonList(ctx.SPACE()),
                        singletonList(ctx.SPIE()),
                        singletonList(ctx.SS()),
                        singletonList(ctx.SSR()),
                        singletonList(ctx.SSRANGE()),
                        singletonList(ctx.STD()),
                        singletonList(ctx.SQL()),
                        singletonList(ctx.SQLC()),
                        singletonList(ctx.SQLCCSID()),
                        singletonList(ctx.SYSEIB()),
                        singletonList(ctx.SZ()),

                        singletonList(ctx.TERM()),
                        singletonList(ctx.TERMINAL()),
                        singletonList(ctx.THREAD()),
                        singletonList(ctx.TEST()),
                        singletonList(ctx.TRIG()),
                        singletonList(ctx.TRUNC()),

                        ctx.U_CHAR(),
                        singletonList(ctx.UE()),
                        singletonList(ctx.UPPER()),

                        singletonList(ctx.VBREF()),

                        singletonList(ctx.WD()),
                        singletonList(ctx.WORD()),

                        singletonList(ctx.X_CHAR()),
                        singletonList(ctx.XP()),
                        singletonList(ctx.XMLPARSE()),
                        singletonList(ctx.XMLSS()),
                        singletonList(ctx.XREF()),

                        singletonList(ctx.YEARWINDOW()),
                        singletonList(ctx.YW()),

                        singletonList(ctx.ZWB()),

                        ctx.W_CHAR()
                )
        );
    }

    @Override
    public Object visitCompilerOptions(CobolPreprocessorParser.CompilerOptionsContext ctx) {
        return new CobolPreprocessor.CompilerOptions(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.PROCESS(), ctx.CBL()),
                convertAllList(ctx.COMMACHAR(), ctx.compilerOption(), ctx.compilerXOpts())
        );
    }

    @Override
    public Object visitCompilerXOpts(CobolPreprocessorParser.CompilerXOptsContext ctx) {
        return new CobolPreprocessor.CompilerXOpts(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.Word) visit(ctx.XOPTS()),
                (CobolPreprocessor.Word) visit(ctx.LPARENCHAR()),
                convertAllList(ctx.COMMACHAR(), ctx.compilerOption()),
                (CobolPreprocessor.Word) visit(ctx.RPARENCHAR())
        );
    }

    @Override
    public CobolPreprocessor.CompilationUnit visitCompilationUnit(CobolPreprocessorParser.CompilationUnitContext ctx) {
        init();

        return new CobolPreprocessor.CompilationUnit(
                randomId(),
                path,
                fileAttributes,
                Space.EMPTY,
                Markers.EMPTY,
                charset.name(),
                charsetBomMarked,
                null,
                convertAll(ctx.charDataLine(),
                        ctx.compilerOptions(),
                        ctx.copyStatement(),
                        ctx.execCicsStatement(),
                        ctx.execSqlStatement(),
                        ctx.execSqlImsStatement(),
                        ctx.ejectStatement(),
                        ctx.replaceArea(),
                        ctx.replaceOffStatement(),
                        ctx.skipStatement(),
                        ctx.titleStatement()),
                (CobolPreprocessor.Word) visit(ctx.EOF())
        );
    }

    @Override
    public Object visitCopyLibrary(CobolPreprocessorParser.CopyLibraryContext ctx) {
        // Pass through.
        return super.visitCopyLibrary(ctx);
    }

    @Override
    public Object visitCopySource(CobolPreprocessorParser.CopySourceContext ctx) {
        return new CobolPreprocessor.CopySource(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.literal(), ctx.cobolWord(), ctx.filename()),
                ctx.OF() == null && ctx.IN() == null ? null : visit(ctx.OF(), ctx.IN()),
                visitNullable(ctx.copyLibrary())
        );
    }

    @Override
    public Object visitCopyStatement(CobolPreprocessorParser.CopyStatementContext ctx) {
        return new CobolPreprocessor.CopyStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.Word) visit(ctx.COPY()),
                (CobolPreprocessor.CopySource) visit(ctx.copySource()),
                convertAllList(ctx.directoryPhrase(), ctx.familyPhrase(), ctx.replacingPhrase(), ctx.SUPPRESS()),
                (CobolPreprocessor.Word) visit(ctx.DOT()),
                null
        );
    }

    @Override
    public Object visitDirectoryPhrase(CobolPreprocessorParser.DirectoryPhraseContext ctx) {
        return new CobolPreprocessor.DirectoryPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.OF(), ctx.IN()),
                visit(ctx.literal(), ctx.cobolWord())
        );
    }

    @Override
    public Object visitEjectStatement(CobolPreprocessorParser.EjectStatementContext ctx) {
        return new CobolPreprocessor.EjectStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.Word) visit(ctx.EJECT()),
                visitNullable(ctx.DOT())
        );
    }

    @Override
    public Object visitExecCicsStatement(CobolPreprocessorParser.ExecCicsStatementContext ctx) {
        return new CobolPreprocessor.ExecStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.EXEC(), ctx.CICS()),
                (CobolPreprocessor) visit(ctx.charData()),
                (CobolPreprocessor.Word) visit(ctx.END_EXEC()),
                visitNullable(ctx.DOT())
        );
    }

    @Override
    public Object visitExecSqlStatement(CobolPreprocessorParser.ExecSqlStatementContext ctx) {
        return new CobolPreprocessor.ExecStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.EXEC(), ctx.SQL()),
                (CobolPreprocessor) visit(ctx.charDataSql()),
                (CobolPreprocessor.Word) visit(ctx.END_EXEC()),
                visitNullable(ctx.DOT())
        );
    }

    @Override
    public Object visitExecSqlImsStatement(CobolPreprocessorParser.ExecSqlImsStatementContext ctx) {
        return new CobolPreprocessor.ExecStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.EXEC(), ctx.SQLIMS()),
                (CobolPreprocessor) visit(ctx.charData()),
                (CobolPreprocessor.Word) visit(ctx.END_EXEC()),
                visitNullable(ctx.DOT())
        );
    }

    @Override
    public Object visitFamilyPhrase(CobolPreprocessorParser.FamilyPhraseContext ctx) {
        return new CobolPreprocessor.FamilyPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.Word) visit(ctx.ON()),
                visit(ctx.literal(), ctx.cobolWord())
        );
    }

    @Override
    public Object visitFilename(CobolPreprocessorParser.FilenameContext ctx) {
        // Pass through.
        return super.visitFilename(ctx);
    }

    @Override
    public Object visitLiteral(CobolPreprocessorParser.LiteralContext ctx) {
        // Pass through.
        return super.visitLiteral(ctx);
    }

    @Override
    public Object visitPseudoText(CobolPreprocessorParser.PseudoTextContext ctx) {
        return new CobolPreprocessor.PseudoText(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.Word) visit(ctx.DOUBLEEQUALCHAR().get(0)),
                visitNullable(ctx.charData()),
                (CobolPreprocessor.Word) visit(ctx.DOUBLEEQUALCHAR().get(1))
        );
    }

    @Override
    public Object visitReplaceable(CobolPreprocessorParser.ReplaceableContext ctx) {
        // Pass through.
        return super.visitReplaceable(ctx);
    }

    @Override
    public Object visitReplaceArea(CobolPreprocessorParser.ReplaceAreaContext ctx) {
        return new CobolPreprocessor.ReplaceArea(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.ReplaceByStatement) visit(ctx.replaceByStatement()),
                convertAllList(ctx.copyStatement(), ctx.charData()),
                visitNullable(ctx.replaceOffStatement())
        );
    }

    @Override
    public Object visitReplaceByStatement(CobolPreprocessorParser.ReplaceByStatementContext ctx) {
        return new CobolPreprocessor.ReplaceByStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.Word) visit(ctx.REPLACE()),
                convertAll(ctx.replaceClause()),
                (CobolPreprocessor.Word) visit(ctx.DOT())
        );
    }

    @Override
    public Object visitReplaceClause(CobolPreprocessorParser.ReplaceClauseContext ctx) {
        return new CobolPreprocessor.ReplaceClause(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor) visit(ctx.replaceable()),
                (CobolPreprocessor.Word) visit(ctx.BY()),
                (CobolPreprocessor) visit(ctx.replacement()),
                convertAll(ctx.directoryPhrase()),
                visitNullable(ctx.familyPhrase())
        );
    }

    @Override
    public Object visitReplacement(CobolPreprocessorParser.ReplacementContext ctx) {
        // Pass through.
        return super.visitReplacement(ctx);
    }

    @Override
    public Object visitReplaceOffStatement(CobolPreprocessorParser.ReplaceOffStatementContext ctx) {
        return new CobolPreprocessor.ReplaceOffStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                wordsList(ctx.REPLACE(), ctx.OFF()),
                (CobolPreprocessor.Word) visit(ctx.DOT())
        );
    }

    @Override
    public Object visitReplacingPhrase(CobolPreprocessorParser.ReplacingPhraseContext ctx) {
        return new CobolPreprocessor.ReplacingPhrase(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.Word) visit(ctx.REPLACING()),
                convertAll(ctx.replaceClause())
        );
    }

    @Override
    public Object visitSkipStatement(CobolPreprocessorParser.SkipStatementContext ctx) {
        return new CobolPreprocessor.SkipStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                visit(ctx.SKIP1(), ctx.SKIP2(), ctx.SKIP3()),
                visitNullable(ctx.DOT())
        );
    }

    @Override
    public Object visitTerminal(TerminalNode node) {
        List<Marker> markers = new ArrayList<>();
        Space prefix = processTokenText(node.getText(), markers);
        String text = END_OF_FILE.equals(node.getText()) ? "" :
                node.getText().startsWith(COMMENT_ENTRY) ? node.getText().substring(COMMENT_ENTRY.length()) : node.getText();
        return new CobolPreprocessor.Word(
                randomId(),
                prefix,
                markers.isEmpty() ? Markers.EMPTY : Markers.build(markers),
                text
        );
    }

    @Override
    public Object visitTitleStatement(CobolPreprocessorParser.TitleStatementContext ctx) {
        return new CobolPreprocessor.TitleStatement(
                randomId(),
                Space.EMPTY,
                Markers.EMPTY,
                (CobolPreprocessor.Word) visit(ctx.TITLE()),
                (CobolPreprocessor.Word) visit(ctx.literal()),
                visitNullable(ctx.DOT())
        );
    }

    private Space whitespace() {
        String prefix = source.substring(cursor, indexOfNextNonWhitespace(cursor, source));
        cursor += prefix.length();
        return format(prefix);
    }

    private int indexOfNextNonWhitespace(int cursor, String source) {
        int delimIndex = cursor;
        for (; delimIndex < source.length(); delimIndex++) {
            if (source.length() > delimIndex + 1) {
                // separators are equivalent to a whitespace character, but are specific combinations of characters based on the dialect.
                // I.E. In IBM-ANSI-85, comma space `, ` or semicolon space `; ` are equivalent to a space.
                if (separators.contains(source.substring(delimIndex, delimIndex + 2))) {
                    continue;
                }
            }

            // Do not consume whitespace in blank column areas.
            boolean isColumnArea = sequenceAreas.containsKey(delimIndex) || indicatorAreas.containsKey(delimIndex) || commentAreas.containsKey(delimIndex);
            if (!Character.isWhitespace(source.substring(delimIndex, delimIndex + 1).charAt(0)) || isColumnArea) {
                break; // found it!
            }
        }
        return delimIndex;
    }

    private @Nullable List<CobolPreprocessor.Word> wordsList(TerminalNode... wordNodes) {
        List<CobolPreprocessor.Word> words = new ArrayList<>(wordNodes.length);
        for (TerminalNode wordNode : wordNodes) {
            if (wordNode != null) {
                CobolPreprocessor.Word cw = (CobolPreprocessor.Word) visit(wordNode);
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
    private final <C extends CobolPreprocessor> List<C> convertAll(List<? extends ParserRuleContext>... trees) {
        return convertAll(Arrays.stream(trees)
                .filter(Objects::nonNull)
                .flatMap(Collection::stream)
                .sorted(Comparator.comparingInt(it -> it.start.getStartIndex()))
                .collect(Collectors.toList()));
    }

    private <C extends CobolPreprocessor, T extends ParseTree> List<C> convertAll(List<T> trees) {
        //noinspection unchecked
        return convertAll(trees, t -> (C) visit(t));
    }

    @SafeVarargs
    private final List<CobolPreprocessor> convertAllList(List<? extends ParseTree>... trees) {
        return Arrays.stream(trees)
                .flatMap(Collection::stream)
                .filter(Objects::nonNull)
                .sorted(Comparator.comparingInt(it -> it instanceof TerminalNode ? ((TerminalNode) it).getSymbol().getStartIndex() :
                        ((ParserRuleContext) it).getStart().getStartIndex()))
                .map(it -> (CobolPreprocessor) visit(it))
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
                .findFirst().orElse(null);
        boolean isContinued = nextIndicator != null && indicatorAreas.get(nextIndicator).equals("-");
        cursor = saveCursor;

        Character delimiter = null;
        if (text.startsWith("'") || text.startsWith("\"")) {
            delimiter = text.charAt(0);
        }

        // A literal continued on a new line.
        if (isContinued && delimiter != null) {
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
            while (iterations < 250) {
                // Stop after all the trailing comments have been parsed.
                if (source.substring(cursor).isEmpty()) {
                    break;
                }

                String contentArea = source.substring(cursor, cursor - cobolDialect.getColumns().getIndicatorArea() - 1 + cobolDialect.getColumns().getOtherArea());
                if (!(isCommentIndicator(indicatorArea) || contentArea.trim().isEmpty())) {
                    break;
                }

                cursor += contentArea.length();
                CommentArea commentArea = commentArea();
                Lines.Line line = new Lines.Line(randomId(), sequenceArea, indicatorArea, contentArea, commentArea, false);
                lines.add(line);

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
        while (iterations < 250) {
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
                return prefix;
            }
        }

        if (sequenceArea != null) {
            markers.add(sequenceArea);
        }

        if (indicatorArea != null) {
            markers.add(indicatorArea);
        }

        if (!END_OF_FILE.equals(text)) {
            cursor += text.length();

            CommentArea commentArea = commentArea();
            if (commentArea != null) {
                markers.add(commentArea);
            }
        }
        return prefix;
    }

    private boolean isCommentIndicator(@Nullable IndicatorArea area) {
        return area != null && (commentIndicators.contains(area.getIndicator().charAt(0)));
    }

    /**
     * Return the SequenceArea based on the current cursor position if it exists.
     */
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

        if (before.getWhitespace().endsWith("\n") || comment != null) {
            return new CommentArea(randomId(), before, comment == null ? "" : comment, endLine, false);
        }

        cursor = saveCursor;
        return null;
    }
}
