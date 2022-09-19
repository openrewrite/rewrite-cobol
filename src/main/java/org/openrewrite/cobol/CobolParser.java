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
import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.cobol.internal.CobolParserVisitor;
import org.openrewrite.cobol.internal.CobolPreprocessorOutputPrinter;
import org.openrewrite.cobol.internal.grammar.CobolLexer;
import org.openrewrite.cobol.proleap.*;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.internal.EncodingDetectingInputStream;
import org.openrewrite.internal.MetricsHelper;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.tree.ParsingEventListener;
import org.openrewrite.tree.ParsingExecutionContextView;

import java.nio.file.Path;
import java.util.*;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

public class CobolParser implements Parser<Cobol.CompilationUnit> {
    private static final List<String> COBOL_FILE_EXTENSIONS = Arrays.asList(".cbl", ".cpy");

    private final CobolDialect cobolDialect;
    private final ProLeapCobolDialect proLeapCobolDialect;
    private final ProLeapCobolPreprocessor.CobolSourceFormatEnum sourceFormat;

    public CobolParser(CobolDialect cobolDialect,
                       ProLeapCobolDialect proLeapCobolDialect,
                       ProLeapCobolPreprocessor.CobolSourceFormatEnum sourceFormat) {

        this.cobolDialect = cobolDialect;
        this.proLeapCobolDialect = proLeapCobolDialect;
        this.sourceFormat = sourceFormat;
    }

    @Override
    public List<Cobol.CompilationUnit> parseInputs(Iterable<Input> sourceFiles, @Nullable Path relativeTo, ExecutionContext ctx) {
        ParsingEventListener parsingListener = ParsingExecutionContextView.view(ctx).getParsingListener();
        return acceptedInputs(sourceFiles).stream()
                .map(sourceFile -> {
                    Timer.Builder timer = Timer.builder("rewrite.parse")
                            .description("The time spent parsing a COBOL file")
                            .tag("file.type", "COBOL");
                    Timer.Sample sample = Timer.start();
                    try {
                        EncodingDetectingInputStream is = sourceFile.getSource();

                        CobolPreprocessorParser cobolPreprocessorParser = CobolPreprocessorParser.builder()
                                .setCobolDialect(cobolDialect)
                                .setProLeapCobolDialect(proLeapCobolDialect)
                                .setSourceFormat(sourceFormat)
                                .enableCopy()
                                .enableReplace()
                                .build();

                        CobolPreprocessor.CompilationUnit preprocessedCU = cobolPreprocessorParser.parseInputs(singletonList(sourceFile), relativeTo, ctx).get(0);

                        // TODO: explicit prints are only necessary because we cannot print through the CU with printAll().
                        // Currently, the default print is the original source code.
                        // We may add print options to print the post processed COBOL with Columns to use as the source.
                        // And without the columns to use in the CobolParser.

                        // Print processed code to parse COBOL.
                        PrintOutputCapture<ExecutionContext> cobolParserOutput = new PrintOutputCapture<>(new InMemoryExecutionContext());
                        CobolPreprocessorOutputPrinter<ExecutionContext> printWithoutColumns = new CobolPreprocessorOutputPrinter<>(cobolDialect, false);
                        printWithoutColumns.visit(preprocessedCU, cobolParserOutput);

                        org.openrewrite.cobol.internal.grammar.CobolParser parser =
                                new org.openrewrite.cobol.internal.grammar.CobolParser(
                                        new CommonTokenStream(new CobolLexer(CharStreams.fromString(cobolParserOutput.getOut()))));

                        // Print the pre-processed code to parse COBOL.
                        PrintOutputCapture<ExecutionContext> sourceOutput = new PrintOutputCapture<>(new InMemoryExecutionContext());
                        CobolPreprocessorOutputPrinter<ExecutionContext> printWithColumns = new CobolPreprocessorOutputPrinter<>(cobolDialect, true);
                        printWithColumns.visit(preprocessedCU, sourceOutput);

                        Cobol.CompilationUnit compilationUnit = new CobolParserVisitor(
                                sourceFile.getRelativePath(relativeTo),
                                sourceFile.getFileAttributes(),
                                sourceOutput.getOut(),
                                is.getCharset(),
                                is.isCharsetBomMarked(),
                                cobolDialect,
                                cobolPreprocessorParser.getCopyStatements(preprocessedCU),
                                cobolPreprocessorParser.getReplaceByStatements(preprocessedCU),
                                cobolPreprocessorParser.getReplaceOffStatements(preprocessedCU),
                                cobolPreprocessorParser.getReplaces(preprocessedCU)
                        ).visitStartRule(parser.startRule());

                        sample.stop(MetricsHelper.successTags(timer).register(Metrics.globalRegistry));
                        parsingListener.parsed(sourceFile, compilationUnit);
                        return compilationUnit;
                    } catch (Throwable t) {
                        sample.stop(MetricsHelper.errorTags(timer, t).register(Metrics.globalRegistry));
                        ctx.getOnError().accept(new IllegalStateException(sourceFile.getPath() + " " + t.getMessage(), t));
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(toList());
    }

    @Override
    public List<Cobol.CompilationUnit> parse(String... sources) {
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

    private static final class CobolKeywords {
        CobolKeywords() {}

        // Run `cobol_keyword_permutations.py` in antlr to print the keywords.
        private static final String[] RESERVED_WORDS = new String[] {
                "ABORT",
                "ACCEPT",
                "ACCESS",
                "ADD",
                "ADDRESS",
                "ADVANCING",
                "AFTER",
                "ALIGNED",
                "ALL",
                "ALPHABET",
                "ALPHABETIC",
                "ALPHABETIC_LOWER",
                "ALPHABETIC_UPPER",
                "ALPHANUMERIC",
                "ALPHANUMERIC_EDITED",
                "ALSO",
                "ALTER",
                "ALTERNATE",
                "AMPCHAR",
                "AND",
                "ANY",
                "ARE",
                "AREA",
                "AREAS",
                "AS",
                "ASCENDING",
                "ASCII",
                "ASSIGN",
                "ASSOCIATED_DATA",
                "ASSOCIATED_DATA_LENGTH",
                "ASTERISKCHAR",
                "AT",
                "ATTRIBUTE",
                "AUTHOR",
                "AUTO",
                "AUTO_SKIP",
                "BACKGROUND_COLOR",
                "BACKGROUND_COLOUR",
                "BASIS",
                "BEEP",
                "BEFORE",
                "BEGINNING",
                "BELL",
                "BINARY",
                "BIT",
                "BLANK",
                "BLINK",
                "BLOB",
                "BLOCK",
                "BOTTOM",
                "BOUNDS",
                "BY",
                "BYFUNCTION",
                "BYTITLE",
                "CALL",
                "CANCEL",
                "CAPABLE",
                "CCSVERSION",
                "CD",
                "CF",
                "CH",
                "CHAINING",
                "CHANGED",
                "CHANNEL",
                "CHARACTER",
                "CHARACTERS",
                "CLASS",
                "CLASS_ID",
                "CLOB",
                "CLOCK_UNITS",
                "CLOSE",
                "CLOSE_DISPOSITION",
                "COBOL",
                "CODE",
                "CODE_SET",
                "COL",
                "COLLATING",
                "COLONCHAR",
                "COLUMN",
                "COMMA",
                "COMMACHAR",
                "COMMENTENTRYLINE",
                "COMMENTENTRYTAG",
                "COMMENTLINE",
                "COMMENTTAG",
                "COMMITMENT",
                "COMMON",
                "COMMUNICATION",
                "COMP",
                "COMPUTATIONAL",
                "COMPUTATIONAL_1",
                "COMPUTATIONAL_2",
                "COMPUTATIONAL_3",
                "COMPUTATIONAL_4",
                "COMPUTATIONAL_5",
                "COMPUTE",
                "COMP_1",
                "COMP_2",
                "COMP_3",
                "COMP_4",
                "COMP_5",
                "COM_REG",
                "CONFIGURATION",
                "CONTAINS",
                "CONTENT",
                "CONTINUE",
                "CONTROL",
                "CONTROLS",
                "CONTROL_POINT",
                "CONVENTION",
                "CONVERTING",
                "COPY",
                "CORR",
                "CORRESPONDING",
                "COUNT",
                "CRUNCH",
                "CURRENCY",
                "CURSOR",
                "DATA",
                "DATA_BASE",
                "DATE",
                "DATE_COMPILED",
                "DATE_WRITTEN",
                "DAY",
                "DAY_OF_WEEK",
                "DBCLOB",
                "DBCS",
                "DE",
                "DEBUGGING",
                "DEBUG_CONTENTS",
                "DEBUG_ITEM",
                "DEBUG_LINE",
                "DEBUG_NAME",
                "DEBUG_SUB_1",
                "DEBUG_SUB_2",
                "DEBUG_SUB_3",
                "DECIMAL_POINT",
                "DECLARATIVES",
                "DEFAULT",
                "DEFAULT_DISPLAY",
                "DEFINITION",
                "DELETE",
                "DELIMITED",
                "DELIMITER",
                "DEPENDING",
                "DESCENDING",
                "DESTINATION",
                "DETAIL",
                "DFHRESP",
                "DFHVALUE",
                "DISABLE",
                "DISK",
                "DISPLAY",
                "DISPLAY_1",
                "DIVIDE",
                "DIVISION",
                "DOLLARCHAR",
                "DONTCARE",
                "DOT_FS",
                "DOUBLE",
                "DOUBLEASTERISKCHAR",
                "DOUBLEQUOTE",
                "DOWN",
                "DUPLICATES",
                "DYNAMIC",
                "EBCDIC",
                "EGCS",
                "EGI",
                "ELSE",
                "EMI",
                "EMPTY_CHECK",
                "ENABLE",
                "END",
                "ENDING",
                "END_ACCEPT",
                "END_ADD",
                "END_CALL",
                "END_COMPUTE",
                "END_DELETE",
                "END_DISPLAY",
                "END_DIVIDE",
                "END_EVALUATE",
                "END_IF",
                "END_MULTIPLY",
                "END_OF_PAGE",
                "END_PERFORM",
                "END_READ",
                "END_RECEIVE",
                "END_REMARKS",
                "END_RETURN",
                "END_REWRITE",
                "END_SEARCH",
                "END_START",
                "END_STRING",
                "END_SUBTRACT",
                "END_UNSTRING",
                "END_WRITE",
                "ENTER",
                "ENTRY",
                "ENTRY_PROCEDURE",
                "ENVIRONMENT",
                "EOL",
                "EOP",
                "EOS",
                "EQUAL",
                "EQUALCHAR",
                "ERASE",
                "ERROR",
                "ESCAPE",
                "ESI",
                "EVALUATE",
                "EVENT",
                "EVERY",
                "EXCEPTION",
                "EXCLUSIVE",
                "EXECCICSLINE",
                "EXECCICSTAG",
                "EXECSQLIMSLINE",
                "EXECSQLIMSTAG",
                "EXECSQLLINE",
                "EXECSQLTAG",
                "EXHIBIT",
                "EXIT",
                "EXPORT",
                "EXTEND",
                "EXTENDED",
                "EXTERNAL",
                "FALSE",
                "FD",
                "FILE",
                "FILE_CONTROL",
                "FILLER",
                "FINAL",
                "FIRST",
                "FOOTING",
                "FOR",
                "FOREGROUND_COLOR",
                "FOREGROUND_COLOUR",
                "FROM",
                "FULL",
                "FUNCTION",
                "FUNCTIONNAME",
                "FUNCTION_POINTER",
                "GENERATE",
                "GIVING",
                "GLOBAL",
                "GO",
                "GOBACK",
                "GREATER",
                "GRID",
                "GROUP",
                "HEADING",
                "HIGHLIGHT",
                "HIGH_VALUE",
                "HIGH_VALUES",
                "ID",
                "IDENTIFICATION",
                "IDENTIFIER",
                "IF",
                "IMPLICIT",
                "IMPORT",
                "IN",
                "INDEX",
                "INDEXED",
                "INDICATE",
                "INITIAL",
                "INITIALIZE",
                "INITIATE",
                "INPUT",
                "INPUT_OUTPUT",
                "INSPECT",
                "INSTALLATION",
                "INTEGER",
                "INTEGERLITERAL",
                "INTO",
                "INVALID",
                "INVOKE",
                "IS",
                "I_O",
                "I_O_CONTROL",
                "JUST",
                "JUSTIFIED",
                "KANJI",
                "KEPT",
                "KEY",
                "KEYBOARD",
                "LABEL",
                "LANGUAGE",
                "LAST",
                "LB",
                "LD",
                "LEADING",
                "LEFT",
                "LEFTLINE",
                "LENGTH",
                "LENGTH_CHECK",
                "LESS",
                "LESSTHANCHAR",
                "LESSTHANOREQUAL",
                "LEVEL_NUMBER_66",
                "LEVEL_NUMBER_77",
                "LEVEL_NUMBER_88",
                "LIBACCESS",
                "LIBPARAMETER",
                "LIBRARY",
                "LIMIT",
                "LIMITS",
                "LINAGE",
                "LINAGE_COUNTER",
                "LINE",
                "LINES",
                "LINE_COUNTER",
                "LINKAGE",
                "LIST",
                "LOCAL",
                "LOCAL_STORAGE",
                "LOCK",
                "LONG_DATE",
                "LONG_TIME",
                "LOWER",
                "LOWLIGHT",
                "LOW_VALUE",
                "LOW_VALUES",
                "LPARENCHAR",
                "MEMORY",
                "MERGE",
                "MESSAGE",
                "MINUSCHAR",
                "MMDDYYYY",
                "MODE",
                "MODULES",
                "MORETHANCHAR",
                "MORETHANOREQUAL",
                "MORE_LABELS",
                "MOVE",
                "MULTIPLE",
                "MULTIPLY",
                "NAMED",
                "NATIONAL",
                "NATIONAL_EDITED",
                "NATIVE",
                "NEGATIVE",
                "NETWORK",
                "NEWLINE",
                "NEXT",
                "NO",
                "NONNUMERICLITERAL",
                "NOT",
                "NOTEQUALCHAR",
                "NO_ECHO",
                "NULL",
                "NULLS",
                "NUMBER",
                "NUMERIC",
                "NUMERICLITERAL",
                "NUMERIC_DATE",
                "NUMERIC_EDITED",
                "NUMERIC_TIME",
                "OBJECT_COMPUTER",
                "OCCURS",
                "ODT",
                "OF",
                "OFF",
                "OMITTED",
                "ON",
                "OPEN",
                "OPTIONAL",
                "OR",
                "ORDER",
                "ORDERLY",
                "ORGANIZATION",
                "OTHER",
                "OUTPUT",
                "OVERFLOW",
                "OVERLINE",
                "OWN",
                "PACKED_DECIMAL",
                "PADDING",
                "PAGE",
                "PAGE_COUNTER",
                "PASSWORD",
                "PERFORM",
                "PF",
                "PH",
                "PIC",
                "PICTURE",
                "PLUS",
                "PLUSCHAR",
                "POINTER",
                "PORT",
                "POSITION",
                "POSITIVE",
                "PRINTER",
                "PRINTING",
                "PRIVATE",
                "PROCEDURE",
                "PROCEDURES",
                "PROCEDURE_POINTER",
                "PROCEED",
                "PROCESS",
                "PROGRAM",
                "PROGRAM_ID",
                "PROGRAM_LIBRARY",
                "PROMPT",
                "PURGE",
                "QUEUE",
                "QUOTE",
                "QUOTES",
                "RANDOM",
                "RD",
                "READ",
                "READER",
                "REAL",
                "RECEIVE",
                "RECEIVED",
                "RECORD",
                "RECORDING",
                "RECORDS",
                "RECURSIVE",
                "REDEFINES",
                "REEL",
                "REF",
                "REFERENCE",
                "REFERENCES",
                "RELATIVE",
                "RELEASE",
                "REMAINDER",
                "REMARKS",
                "REMOTE",
                "REMOVAL",
                "REMOVE",
                "RENAMES",
                "REPLACE",
                "REPLACING",
                "REPORT",
                "REPORTING",
                "REPORTS",
                "REQUIRED",
                "RERUN",
                "RESERVE",
                "RESET",
                "RETURN",
                "RETURNING",
                "RETURN_CODE",
                "REVERSED",
                "REVERSE_VIDEO",
                "REWIND",
                "REWRITE",
                "RF",
                "RH",
                "RIGHT",
                "ROUNDED",
                "RPARENCHAR",
                "RUN",
                "SAME",
                "SAVE",
                "SCREEN",
                "SD",
                "SEARCH",
                "SECTION",
                "SECURE",
                "SECURITY",
                "SEGMENT",
                "SEGMENT_LIMIT",
                "SELECT",
                "SEND",
                "SENTENCE",
                "SEPARATE",
                "SEPARATOR",
                "SEQUENCE",
                "SEQUENTIAL",
                "SET",
                "SHARED",
                "SHAREDBYALL",
                "SHAREDBYRUNUNIT",
                "SHARING",
                "SHIFT_IN",
                "SHIFT_OUT",
                "SHORT_DATE",
                "SIGN",
                "SINGLEQUOTE",
                "SIZE",
                "SLASHCHAR",
                "SORT",
                "SORT_CONTROL",
                "SORT_CORE_SIZE",
                "SORT_FILE_SIZE",
                "SORT_MERGE",
                "SORT_MESSAGE",
                "SORT_MODE_SIZE",
                "SORT_RETURN",
                "SOURCE",
                "SOURCE_COMPUTER",
                "SPACE",
                "SPACES",
                "SPECIAL_NAMES",
                "SQL",
                "STANDARD",
                "STANDARD_1",
                "STANDARD_2",
                "START",
                "STATUS",
                "STOP",
                "STRING",
                "SUBTRACT",
                "SUB_QUEUE_1",
                "SUB_QUEUE_2",
                "SUB_QUEUE_3",
                "SUM",
                "SUPPRESS",
                "SYMBOL",
                "SYMBOLIC",
                "SYNC",
                "SYNCHRONIZED",
                "TABLE",
                "TALLY",
                "TALLYING",
                "TAPE",
                "TASK",
                "TERMINAL",
                "TERMINATE",
                "TEST",
                "TEXT",
                "THAN",
                "THEN",
                "THREAD",
                "THREAD_LOCAL",
                "THROUGH",
                "THRU",
                "TIME",
                "TIMER",
                "TIMES",
                "TITLE",
                "TO",
                "TODAYS_DATE",
                "TODAYS_NAME",
                "TOP",
                "TRAILING",
                "TRUE",
                "TRUNCATED",
                "TYPE",
                "TYPEDEF",
                "UNDERLINE",
                "UNIT",
                "UNSTRING",
                "UNTIL",
                "UP",
                "UPON",
                "USAGE",
                "USE",
                "USING",
                "VALUE",
                "VALUES",
                "VARYING",
                "VIRTUAL",
                "WAIT",
                "WHEN",
                "WHEN_COMPILED",
                "WITH",
                "WORDS",
                "WORKING_STORAGE",
                "WRITE",
                "WS",
                "YEAR",
                "YYYYDDD",
                "YYYYMMDD",
                "ZERO",
                "ZEROES",
                "ZEROS",
                "ZERO_FILL"
        };

        private static final Set<String> RESERVED_WORDS_SET = new HashSet<>(Arrays.asList(RESERVED_WORDS));

        public static boolean isReserved(String word) {
            return RESERVED_WORDS_SET.contains(word);
        }
    }
}
