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

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;
import org.antlr.v4.runtime.*;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Parser;
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.internal.CobolPreprocessorParserVisitor;
import org.openrewrite.cobol.internal.IbmAnsi85;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorLexer;
import org.openrewrite.cobol.proleap.*;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.cobol.tree.Replace;
import org.openrewrite.cobol.tree.Space;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;
import org.openrewrite.text.PlainText;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.io.File;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static org.openrewrite.Tree.randomId;
import static org.openrewrite.cobol.proleap.ProLeapCobolDialect.ANSI85;
import static org.openrewrite.cobol.proleap.ProLeapCobolPreprocessor.CobolSourceFormatEnum.FIXED;

/**
 * Read preprocessed COBOL and execute preprocessor commands.
 */
public class CobolPreprocessorParser implements Parser<CobolPreprocessor.CompilationUnit> {
    private static final List<String> COBOL_FILE_EXTENSIONS = Arrays.asList(".cbl", ".cpy");

    private final CobolDialect cobolDialect;
    private final ProLeapCobolDialect proLeapCobolDialect;
    private final ProLeapCobolPreprocessor.CobolSourceFormatEnum sourceFormat;
    private final boolean enableCopy;
    private final boolean enableReplace;

    // Lazily loaded maps of the original source objects.
    private Set<CobolPreprocessor.CopyStatement> copyStatements = null;
    private Set<CobolPreprocessor.ReplaceByStatement> replaceRules = null;
    private Set<CobolPreprocessor.ReplaceOffStatement> replaceOffs = null;
    private Set<Replace> replaces = null;

    public CobolPreprocessorParser(CobolDialect cobolDialect,
                                   ProLeapCobolDialect proLeapCobolDialect,
                                   ProLeapCobolPreprocessor.CobolSourceFormatEnum sourceFormat,
                                   boolean enableCopy,
                                   boolean enableReplace) {
        this.cobolDialect = cobolDialect;
        this.proLeapCobolDialect = proLeapCobolDialect;
        this.sourceFormat = sourceFormat;
        this.enableCopy = enableCopy;
        this.enableReplace = enableReplace;
    }

    @Override
    public List<CobolPreprocessor.CompilationUnit> parseInputs(Iterable<org.openrewrite.Parser.Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
        ParsingEventListener parsingListener = ParsingExecutionContextView.view(ctx).getParsingListener();
        return acceptedInputs(sourceFiles).stream()
                .map(sourceFile -> {
                    Timer.Builder timer = Timer.builder("rewrite.parse")
                            .description("The time spent parsing a COBOL file")
                            .tag("file.type", "COBOL");
                    Timer.Sample sample = Timer.start();
                    try {
                        EncodingDetectingInputStream is = sourceFile.getSource();
                        String sourceStr = is.readFully();

                        String prepareSource = preprocessCobol(sourceStr, is.getCharset());
                        org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser parser =
                                new org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser(
                                        new CommonTokenStream(new CobolPreprocessorLexer(CharStreams.fromString(prepareSource))));

                        CobolPreprocessorParserVisitor parserVisitor = new CobolPreprocessorParserVisitor(
                                sourceFile.getRelativePath(relativeTo),
                                sourceFile.getFileAttributes(),
                                sourceStr,
                                is.getCharset(),
                                is.isCharsetBomMarked(),
                                cobolDialect
                        );

                        CobolPreprocessor.CompilationUnit preprocessedCU = parserVisitor.visitStartRule(parser.startRule());

                        if (enableCopy) {
                            List<Path> copyBookPaths = getCopyBooks();
                            List<CobolPreprocessor.CopyBook> copyBooks = parseCopyBooks(copyBookPaths, relativeTo);
                            PreprocessCopyVisitor<ExecutionContext> copyPhase = new PreprocessCopyVisitor<>(copyBooks);

                            // CU after copy includes the copied source.
                            preprocessedCU = (CobolPreprocessor.CompilationUnit) copyPhase.visit(preprocessedCU, new InMemoryExecutionContext());
                        }

                        if (enableReplace) {
                            PreprocessReplaceVisitor<ExecutionContext> replacePhase = new PreprocessReplaceVisitor<>();

                            // CU after replace has replaced words in each ReplaceArea based on the ReplaceClause.
                            preprocessedCU = (CobolPreprocessor.CompilationUnit) replacePhase.visit(preprocessedCU, new InMemoryExecutionContext());
                        }

                        assert preprocessedCU != null;

                        sample.stop(MetricsHelper.successTags(timer).register(Metrics.globalRegistry));
                        parsingListener.parsed(sourceFile, preprocessedCU);
                        return preprocessedCU;
                    } catch (Throwable t) {
                        sample.stop(MetricsHelper.errorTags(timer, t).register(Metrics.globalRegistry));
                        ctx.getOnError().accept(new IllegalStateException(sourceFile.getPath() + " " + t.getMessage(), t));
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(toList());
    }

    private String preprocessCobol(String source, @Nullable Charset encoding) {
        CobolParserParams params = new CobolParserParams(
                encoding,
                emptyList(),
                getCobolFileExtensions(),
                getCopyBookFiles(),
                proLeapCobolDialect,
                sourceFormat,
                true
        );

        ProLeapCobolPreprocessor proleapCobolPreprocessor = new ProLeapCobolPreprocessor(
                new CobolCommentEntriesMarker(),
                new CobolDocumentParser(),
                new CobolInlineCommentEntriesNormalizer(),
                new CobolLineIndicatorProcessor(),
                new CobolLineReader()
        );

        return proleapCobolPreprocessor.rewriteLines(source, params);
    }

    // Note: the NIST CopyBooks contain COBOL with the correct column areas, which may not be required.
    // This method will break if the CopyBook does not match the dialect (fixable during print or a formatting visitor).
    public List<CobolPreprocessor.CopyBook> parseCopyBooks(List<Path> paths, @Nullable Path relativeTo) {
        List<CobolPreprocessor.CopyBook> copyBooks = new ArrayList<>();

        for (Path path : paths) {
            PlainText plainText;
            try (InputStream inputStream = Files.newInputStream(path)) {
                EncodingDetectingInputStream is = new EncodingDetectingInputStream(inputStream);
                plainText = new PlainText(
                        randomId(),
                        path,
                        Markers.EMPTY,
                        is.getCharset().name(),
                        is.isCharsetBomMarked(),
                        null,
                        null,
                        is.readFully()
                );
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

            String preprocessedCobol = preprocessCobol(plainText.getText(), plainText.getCharset());
            org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser parser =
                    new org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser(
                            new CommonTokenStream(new CobolPreprocessorLexer(CharStreams.fromString(preprocessedCobol))));

            //noinspection ConstantConditions
            CobolPreprocessorParserVisitor parserVisitor = new CobolPreprocessorParserVisitor(
                    relativeTo == null ? plainText.getSourcePath() : plainText.getSourcePath().relativize(relativeTo),
                    plainText.getFileAttributes(),
                    plainText.getText(),
                    plainText.getCharset(),
                    plainText.isCharsetBomMarked(),
                    cobolDialect
            );

            CobolPreprocessor.CompilationUnit cu = parserVisitor.visitStartRule(parser.startRule());
            CobolPreprocessor parsedCopySource = cu.getCobols().get(0);

            // TODO: How should the SourceFile and CopyBook be linked together?
            CobolPreprocessor.CopyBook copyBook = new CobolPreprocessor.CopyBook(
                    randomId(),
                    Space.EMPTY,
                    Markers.EMPTY,
                    path,
                    parsedCopySource,
                    cu.getEof()
            );

            copyBooks.add(copyBook);
        }

        return copyBooks;
    }

    /**
     * There may be A LOT of copy books in a COBOL codebase, but we do not know how the parser is provided the copy books.
     * We also do not know how they are detected or what types of conventions exist.
     *
     * Temporarily hardcoded to implement COPY/REPLACE, but this SHOULD be auto-detected / configurable.
     * A temp ADHOC solution may be an auto-detected style that contains all the relevant info.
     */
    private List<Path> getCopyBooks() {
        String userDir = System.getProperty("user.dir");
        String copyBooks = "/src/test/resources/gov/nist/copybooks";
        return Arrays.stream(Objects.requireNonNull(Paths.get(userDir + copyBooks).toFile().listFiles()))
                .filter(it -> !it.isDirectory())
                .map(File::toPath)
                .collect(toList());
    }

    private List<File> getCopyBookFiles() {
        String userDir = System.getProperty("user.dir");
        String copyBooks = "/src/test/resources/gov/nist/copybooks";
        return Arrays.stream(Objects.requireNonNull(Paths.get(userDir + copyBooks).toFile().listFiles())).collect(toList());
    }

    private List<String> getCobolFileExtensions() {
        return singletonList("CPY");
    }

    @Override
    public List<CobolPreprocessor.CompilationUnit> parse(String... sources) {
        return parse(new InMemoryExecutionContext(), sources);
    }

    @Override
    public boolean accept(Path path) {
        String s = path.toString().toLowerCase();
        for (String COBOL_FILE_EXTENSION : COBOL_FILE_EXTENSIONS) {
            if (s.endsWith(COBOL_FILE_EXTENSION)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public Path sourcePathFromSourceText(Path prefix, String sourceCode) {
        return prefix.resolve("file.CBL");
    }

    public static CobolPreprocessorParser.Builder builder() {
        return new CobolPreprocessorParser.Builder();
    }

    public static class Builder extends org.openrewrite.Parser.Builder {

        CobolDialect cobolDialect;
        ProLeapCobolDialect proLeapCobolDialect;
        ProLeapCobolPreprocessor.CobolSourceFormatEnum sourceFormat;
        boolean enableCopy;
        boolean enableReplace;

        public Builder() {
            super(Cobol.CompilationUnit.class);
        }

        @Override
        public CobolPreprocessorParser build() {
            return new CobolPreprocessorParser(
                    cobolDialect == null ? new IbmAnsi85() : cobolDialect,
                    proLeapCobolDialect == null ? ANSI85 : proLeapCobolDialect,
                    sourceFormat == null ? FIXED : sourceFormat,
                    enableCopy,
                    enableReplace);
        }

        public Builder setCobolDialect(CobolDialect cobolDialect) {
            this.cobolDialect = cobolDialect;
            return this;
        }

        public Builder setProLeapCobolDialect(ProLeapCobolDialect proLeapCobolDialect) {
            this.proLeapCobolDialect = proLeapCobolDialect;
            return this;
        }

        public Builder setSourceFormat(ProLeapCobolPreprocessor.CobolSourceFormatEnum sourceFormat) {
            this.sourceFormat = sourceFormat;
            return this;
        }

        public Builder enableCopy() {
            this.enableCopy = true;
            return this;
        }

        public Builder enableReplace() {
            this.enableReplace = true;
            return this;
        }

        @Override
        public String getDslName() {
            return "preprocessCobol";
        }
    }

    private static class ForwardingErrorListener extends BaseErrorListener {
        private final Path sourcePath;
        private final ExecutionContext ctx;

        private ForwardingErrorListener(Path sourcePath, ExecutionContext ctx) {
            this.sourcePath = sourcePath;
            this.ctx = ctx;
        }

        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                                int line, int charPositionInLine, String msg, RecognitionException e) {
            ctx.getOnError().accept(new CobolParsingException(sourcePath,
                    String.format("Syntax error in %s at line %d:%d %s.", sourcePath, line, charPositionInLine, msg), e));
        }
    }

    // TODO clean up.
    public Set<CobolPreprocessor.CopyStatement> getCopyStatements(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (copyStatements == null) {
            getOriginalSources(cu);
        }
        return copyStatements;
    }

    public Set<CobolPreprocessor.ReplaceByStatement> getReplaceByStatements(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaceRules == null) {
            getOriginalSources(cu);
        }
        return replaceRules;
    }

    public Set<CobolPreprocessor.ReplaceOffStatement> getReplaceOffStatements(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaceOffs == null) {
            getOriginalSources(cu);
        }
        return replaceOffs;
    }

    public Set<Replace> getReplaces(@Nullable CobolPreprocessor.CompilationUnit cu) {
        if (replaces == null) {
            getOriginalSources(cu);
        }
        return replaces;
    }

    public void getOriginalSources(@Nullable CobolPreprocessor.CompilationUnit cu) {
        this.copyStatements = new HashSet<>();
        this.replaceRules = new HashSet<>();
        this.replaceOffs = new HashSet<>();
        this.replaces = new HashSet<>();

        CobolPreprocessorIsoVisitor<ExecutionContext> visitor = new CobolPreprocessorIsoVisitor<ExecutionContext>() {
            @Override
            public CobolPreprocessor.CopyStatement visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement,
                                                                      ExecutionContext executionContext) {
                copyStatements.add(copyStatement);
                return super.visitCopyStatement(copyStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.ReplaceByStatement visitReplaceByStatement(CobolPreprocessor.ReplaceByStatement replaceByStatement, ExecutionContext executionContext) {
                replaceRules.add(replaceByStatement);
                return super.visitReplaceByStatement(replaceByStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.ReplaceOffStatement visitReplaceOffStatement(CobolPreprocessor.ReplaceOffStatement replaceOffStatement, ExecutionContext executionContext) {
                replaceOffs.add(replaceOffStatement);
                return super.visitReplaceOffStatement(replaceOffStatement, executionContext);
            }

            @Override
            public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
                Optional<Replace> replace = word.getMarkers().findFirst(Replace.class);
                replace.ifPresent(it -> replaces.add(it));
                return super.visitWord(word, executionContext);
            }
        };

        visitor.visit(cu, new InMemoryExecutionContext());

    }

    public static final class CobolPreProcessorKeywords {
        CobolPreProcessorKeywords() {}

        // Run `cobol_keyword_permutations.py` in antlr to print the keywords.
        private static final String[] RESERVED_WORDS = new String[] {
                "ADATA",
                "ADV",
                "ALIAS",
                "ANSI",
                "ANY",
                "APOST",
                "AR",
                "ARITH",
                "AUTO",
                "AWO",
                "BIN",
                "BLOCK0",
                "BUF",
                "BUFSIZE",
                "BY",
                "CBL",
                "CBLCARD",
                "CICS",
                "CO",
                "COBOL2",
                "COBOL3",
                "CODEPAGE",
                "COMMACHAR",
                "COMMENTENTRYLINE",
                "COMMENTENTRYTAG",
                "COMMENTLINE",
                "COMMENTTAG",
                "COMPAT",
                "COMPILE",
                "COPY",
                "CP",
                "CPP",
                "CPSM",
                "CS",
                "CURR",
                "CURRENCY",
                "C_CHAR",
                "DATA",
                "DATEPROC",
                "DBCS",
                "DD",
                "DEBUG",
                "DECK",
                "DIAGTRUNC",
                "DLI",
                "DLL",
                "DOT",
                "DOUBLEEQUALCHAR",
                "DP",
                "DTR",
                "DU",
                "DUMP",
                "DYN",
                "DYNAM",
                "D_CHAR",
                "EDF",
                "EJECT",
                "EJPD",
                "EN",
                "END_EXEC",
                "ENGLISH",
                "EPILOG",
                "EXCI",
                "EXEC",
                "EXIT",
                "EXP",
                "EXPORTALL",
                "EXTEND",
                "E_CHAR",
                "FASTSRT",
                "FEPI",
                "FILENAME",
                "FLAG",
                "FLAGSTD",
                "FSRT",
                "FULL",
                "F_CHAR",
                "GDS",
                "GRAPHIC",
                "HOOK",
                "H_CHAR",
                "IDENTIFIER",
                "IN",
                "INTDATE",
                "I_CHAR",
                "JA",
                "JP",
                "KA",
                "LANG",
                "LANGUAGE",
                "LC",
                "LEASM",
                "LENGTH",
                "LIB",
                "LILIAN",
                "LIN",
                "LINECOUNT",
                "LINKAGE",
                "LIST",
                "LM",
                "LONGMIXED",
                "LONGUPPER",
                "LPARENCHAR",
                "LU",
                "MAP",
                "MARGINS",
                "MAX",
                "MD",
                "MDECK",
                "MIG",
                "MIXED",
                "M_CHAR",
                "NAME",
                "NAT",
                "NATIONAL",
                "NATLANG",
                "NEWLINE",
                "NN",
                "NO",
                "NOADATA",
                "NOADV",
                "NOALIAS",
                "NOAWO",
                "NOBLOCK0",
                "NOC",
                "NOCBLCARD",
                "NOCICS",
                "NOCMPR2",
                "NOCOMPILE",
                "NOCPSM",
                "NOCURR",
                "NOCURRENCY",
                "NOD",
                "NODATEPROC",
                "NODBCS",
                "NODE",
                "NODEBUG",
                "NODECK",
                "NODIAGTRUNC",
                "NODLL",
                "NODP",
                "NODTR",
                "NODU",
                "NODUMP",
                "NODYN",
                "NODYNAM",
                "NOEDF",
                "NOEJPD",
                "NOEPILOG",
                "NOEXIT",
                "NOEXP",
                "NOEXPORTALL",
                "NOF",
                "NOFASTSRT",
                "NOFEPI",
                "NOFLAG",
                "NOFLAGMIG",
                "NOFLAGSTD",
                "NOFSRT",
                "NOGRAPHIC",
                "NOHOOK",
                "NOLENGTH",
                "NOLIB",
                "NOLINKAGE",
                "NOLIST",
                "NOMAP",
                "NOMD",
                "NOMDECK",
                "NONAME",
                "NONNUMERICLITERAL",
                "NONUM",
                "NONUMBER",
                "NOOBJ",
                "NOOBJECT",
                "NOOFF",
                "NOOFFSET",
                "NOOPSEQUENCE",
                "NOOPT",
                "NOOPTIMIZE",
                "NOOPTIONS",
                "NOP",
                "NOPFD",
                "NOPROLOG",
                "NORENT",
                "NOS",
                "NOSEP",
                "NOSEPARATE",
                "NOSEQ",
                "NOSEQUENCE",
                "NOSOURCE",
                "NOSPIE",
                "NOSQL",
                "NOSQLC",
                "NOSQLCCSID",
                "NOSSR",
                "NOSSRANGE",
                "NOSTDTRUNC",
                "NOTERM",
                "NOTERMINAL",
                "NOTEST",
                "NOTHREAD",
                "NOTRIG",
                "NOVBREF",
                "NOWD",
                "NOWORD",
                "NOX",
                "NOXREF",
                "NOZWB",
                "NS",
                "NSEQ",
                "NSYMBOL",
                "NUM",
                "NUMBER",
                "NUMERICLITERAL",
                "NUMPROC",
                "N_CHAR",
                "OBJ",
                "OBJECT",
                "OF",
                "OFF",
                "OFFSET",
                "ON",
                "OP",
                "OPMARGINS",
                "OPSEQUENCE",
                "OPT",
                "OPTFILE",
                "OPTIMIZE",
                "OPTIONS",
                "OUT",
                "OUTDD",
                "PFD",
                "PGMN",
                "PGMNAME",
                "PPTDBG",
                "PROCESS",
                "PROLOG",
                "QUOTE",
                "Q_CHAR",
                "RENT",
                "REPLACE",
                "REPLACING",
                "RMODE",
                "RPARENCHAR",
                "SEP",
                "SEPARATE",
                "SEPARATOR",
                "SEQ",
                "SEQUENCE",
                "SHORT",
                "SIZE",
                "SKIP1",
                "SKIP2",
                "SKIP3",
                "SOURCE",
                "SP",
                "SPACE",
                "SPIE",
                "SQL",
                "SQLC",
                "SQLCCSID",
                "SQLIMS",
                "SS",
                "SSR",
                "SSRANGE",
                "STD",
                "SUPPRESS",
                "SYSEIB",
                "SZ",
                "S_CHAR",
                "TERM",
                "TERMINAL",
                "TEST",
                "TEXT",
                "THREAD",
                "TITLE",
                "TRIG",
                "TRUNC",
                "UE",
                "UPPER",
                "U_CHAR",
                "VBREF",
                "WD",
                "WORD",
                "WS",
                "W_CHAR",
                "XMLPARSE",
                "XMLSS",
                "XOPTS",
                "XP",
                "XREF",
                "X_CHAR",
                "YEARWINDOW",
                "YW",
                "ZWB"
        };

        private static final Set<String> RESERVED_WORDS_SET = new HashSet<>(Arrays.asList(RESERVED_WORDS));

        public static boolean isReserved(String word) {
            return RESERVED_WORDS_SET.contains(word);
        }
    }
}
