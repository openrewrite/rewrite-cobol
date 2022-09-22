package org.openrewrite.cobol;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Tree;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.*;

import static org.openrewrite.Tree.randomId;

@EqualsAndHashCode(callSuper = true)
@Value
public class PreprocessWordContinuations extends Recipe {

    @Override
    public String getDisplayName() {
        return "";
    }

    @Override
    public String getDescription() {
        return "";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new CobolPreprocessorIsoVisitor<ExecutionContext>() {
            @Override
            public CobolPreprocessor.CompilationUnit visitCompilationUnit(CobolPreprocessor.CompilationUnit compilationUnit, ExecutionContext executionContext) {
                CobolPreprocessor.CompilationUnit cu = super.visitCompilationUnit(compilationUnit, executionContext);

                Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords = ContinuedLines.find(cu);
                MergeKeywords mergeKeywords = new MergeKeywords(continuedWords);
                cu = (CobolPreprocessor.CompilationUnit) mergeKeywords.visitNonNull(cu, executionContext);

                return cu;
            }
        };
    }

    private static class ContinuedLines extends CobolPreprocessorIsoVisitor<Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>>> {
        public static Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> find(CobolPreprocessor.CompilationUnit compilationUnit) {
            CobolPreprocessorIsoVisitor<Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>>> visitor =
                    new CobolPreprocessorIsoVisitor<Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>>>() {
                        // The previous 2 values are required because the CobolProcessor.g4 considers `-` a token.
                        private CobolPreprocessor.Word previous2 = null;
                        private CobolPreprocessor.Word previous = null;

                        private CobolPreprocessor.Word currentKey = null;
                        private boolean inContinuation = false;

                        @Override
                        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords) {
                            Optional<IndicatorArea> indicatorArea = word.getMarkers().findFirst(IndicatorArea.class);
                            if (indicatorArea.isPresent() && "-".equals(indicatorArea.get().getIndicator())) {
                                // A continuation is immediately continued again.
                                if (inContinuation) {
                                    List<CobolPreprocessor.Word> words = continuedWords.computeIfAbsent(currentKey, key -> new ArrayList<>());
                                    words.add(word);
                                }

                                // Special case, because CobolPreprocessor creates a Word for `-` characters.
                                else if (previous != null && "-".equals(previous.getWord()) &&
                                    // The continued `-` is a part of the previous word. I.E. CONT-B.
                                    // Otherwise, the `-` is something like `VALUE -10`.
                                    previous.getPrefix() == Space.EMPTY && previous2 != null) {
                                    currentKey = previous2;
                                    inContinuation = true;
                                    List<CobolPreprocessor.Word> words = continuedWords.computeIfAbsent(currentKey, key -> new ArrayList<>());
                                    words.add(previous);
                                    words.add(word);
                                } else {
                                    currentKey = previous;
                                    inContinuation = true;
                                    List<CobolPreprocessor.Word> words = continuedWords.computeIfAbsent(currentKey, key -> new ArrayList<>());
                                    words.add(word);
                                }
                            } else if (inContinuation) {
                                if (word.getPrefix() != Space.EMPTY || ".".equals(word.getWord())) {
                                    inContinuation = false;
                                } else {
                                    List<CobolPreprocessor.Word> words = continuedWords.computeIfAbsent(currentKey, key -> new ArrayList<>());
                                    words.add(word);
                                }
                            }

                            previous2 = previous;
                            previous = word;
                            return super.visitWord(word, continuedWords);
                        }
            };

            Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> orderedWords = new HashMap<>();
            visitor.visit(compilationUnit, orderedWords);

            return orderedWords;
        }
    }

    private static class MergeKeywords extends CobolPreprocessorIsoVisitor<ExecutionContext> {
        private final Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords;
        private Map<CobolPreprocessor.Word, CobolPreprocessor.Word> replacements = null;

        public MergeKeywords(Map<CobolPreprocessor.Word, List<CobolPreprocessor.Word>> continuedWords) {
            this.continuedWords = continuedWords;
        }

        @Override
        public CobolPreprocessor.Word visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
            // TODO: fix me ... requires a MARKER otherwise the original source formatting is lost.
            if (continuedWords.containsKey(word)) {
                Map<CobolPreprocessor.Word, CobolPreprocessor.Word> result = getReplacements(word, continuedWords.get(word));
                if (result.containsKey(word)) {
                    word = result.get(word);
                    result.remove(word);
                    this.replacements = result;
                }
            } else if (replacements != null && replacements.containsKey(word)){
                word = replacements.get(word);
            }
            return super.visitWord(word, executionContext);
        }

        private Map<CobolPreprocessor.Word, CobolPreprocessor.Word> getReplacements(CobolPreprocessor.Word key, List<CobolPreprocessor.Word> values) {
            Map<CobolPreprocessor.Word, CobolPreprocessor.Word> result = new HashMap<>();
            String concatenateWord = concatenateWords(key, values);

            Map<Integer, Markers> continuations = new HashMap<>();
            Continuation continuedLines = new Continuation(randomId(), continuations);

            CobolPreprocessor.Word newKey = key.withMarkers(key.getMarkers().addIfAbsent(continuedLines));
            boolean isTwoParts = CobolKeywords.isReserved(key.getWord()) && CobolKeywords.isReserved(concatenateWord.substring(key.getWord().length()));
            if (CobolKeywords.isReserved(concatenateWord) || isTwoParts) {
                List<Marker> continuation = new ArrayList<>(2);

                Optional<SequenceArea> sequenceAreaOptional = newKey.getMarkers().findFirst(SequenceArea.class);
                sequenceAreaOptional.ifPresent(continuation::add);
                newKey = newKey.withMarkers(newKey.getMarkers().removeByType(SequenceArea.class));

                Optional<IndicatorArea> indicatorAreaOptional = newKey.getMarkers().findFirst(IndicatorArea.class);
                indicatorAreaOptional.ifPresent(continuation::add);
                newKey = newKey.withMarkers(newKey.getMarkers().removeByType(IndicatorArea.class));

                int pos = 0;
                if (!continuation.isEmpty()) {
                    continuations.put(pos, Markers.build(continuation));
                }

                if (isTwoParts) {
                    concatenateWord = key.getWord() + " " + concatenateWord.substring(key.getWord().length());
                    pos++;
                }

                newKey = newKey.withWord(concatenateWord);
                pos += key.getWord().length();

                newKey = newKey.withMarkers(newKey.getMarkers().removeByType(CommentArea.class));
                result.put(key, newKey);

                for (CobolPreprocessor.Word value : values) {
                    continuation = new ArrayList<>(3);

                    // Add the CommentArea from the original word to align the columns.
                    Optional<CommentArea> commentAreaOptional;
                    int firstPos = key.getWord().length() + (isTwoParts ? 1 : 0);
                    if (pos == firstPos) {
                        commentAreaOptional = key.getMarkers().findFirst(CommentArea.class);
                        if (commentAreaOptional.isPresent()) {
                            CommentArea commentArea = commentAreaOptional.get();
                            if (isTwoParts) {
                                int removeSpace = commentArea.getPrefix().getWhitespace().length() - 1;
                                commentArea = commentArea.withPrefix(
                                        commentArea.getPrefix().withWhitespace(
                                                commentArea.getPrefix().getWhitespace().substring(0, removeSpace)));
                            }
                            continuation.add(commentArea);
                        }
                    } else {
                        commentAreaOptional = value.getMarkers().findFirst(CommentArea.class);
                        commentAreaOptional.ifPresent(continuation::add);
                    }

                    sequenceAreaOptional = value.getMarkers().findFirst(SequenceArea.class);
                    sequenceAreaOptional.ifPresent(continuation::add);

                    indicatorAreaOptional = value.getMarkers().findFirst(IndicatorArea.class);
                    // Move the prefix from the concatenated value to the indicator marker.
                    if (indicatorAreaOptional.isPresent()) {
                        IndicatorArea indicatorArea = indicatorAreaOptional.get();
                        indicatorArea = indicatorArea.withContinuationPrefix(value.getPrefix().getWhitespace());
                        continuation.add(indicatorArea);
                    }

                    if (!continuation.isEmpty()) {
                        continuations.put(pos, Markers.build(continuation));
                    }
                    pos += value.getWord().length();
                }

                values.forEach(k -> {
                    CobolPreprocessor.Word updated = k.withWord("");
                    updated = updated.withMarkers(Markers.EMPTY);
                    updated = updated.withPrefix(Space.EMPTY);
                    result.put(k, updated);
                });
            } else {
                result.put(key, newKey.withWord(concatenateWord));
                values.forEach(w -> result.put(w, w.withWord("")));
                System.out.println(concatenateWord);
            }
            return result;
        }

        private String concatenateWords(CobolPreprocessor.Word key, List<CobolPreprocessor.Word> values) {
            StringBuilder word = new StringBuilder();
            word.append(key.getWord());
            values.forEach(w -> word.append(w.getWord()));
            return word.toString();
        }
    }

    public static final class CobolKeywords {
        CobolKeywords() {}

        // Run `cobol_keyword_permutations.py` in `antlr` package to print the keywords.
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
