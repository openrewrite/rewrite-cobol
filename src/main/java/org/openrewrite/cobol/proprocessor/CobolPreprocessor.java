/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import lombok.Value;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.List;
import java.util.regex.Pattern;

@Value
public class CobolPreprocessor {
    public static final String INDICATOR_FIELD = "([ABCdD$\\t\\-/*# ])";
    public static final String NEWLINE = "\n";
    public static final String WS = " ";
    public static final String CHAR_ASTERISK = "*";
    public static final String CHAR_D = "D";
    public static final String CHAR_D_ = "d";
    public static final String CHAR_DOLLAR_SIGN = "$";
    public static final String CHAR_MINUS = "-";
    public static final String CHAR_SLASH = "/";
    public static final String COMMENT_TAG = "*>";
    public static final String COMMENT_ENTRY_TAG = "*>CE";
    public static final String EXEC_CICS_TAG = "*>EXECCICS";
    public static final String EXEC_END_TAG = "}";
    public static final String EXEC_SQL_TAG = "*>EXECSQL";
    public static final String EXEC_SQLIMS_TAG = "*>EXECSQLIMS";

    CobolCommentEntriesMarker cobolCommentEntriesMarker;
    CobolDocumentParser cobolDocumentParser;
    CobolInlineCommentEntriesNormalizer cobolInlineCommentEntriesNormalizer;
    CobolLineIndicatorProcessor cobolLineIndicatorProcessor;
    CobolLineReader cobolLineReader;

    String parseDocument(List<CobolLine> lines, CobolParserParams params) {
        String code = CobolLineWriter.write(lines);
        return cobolDocumentParser.processLines(code, params);
    }

    public String process(File cobolFile, CobolParserParams params) throws IOException {
        Charset charset = params.getCharset();

        String cobolFileContent = new String(Files.readAllBytes(cobolFile.toPath()), charset);
        return process(cobolFileContent, params);
    }

    public String process(String cobolCode, CobolParserParams params) {
        List<CobolLine> lines = readLines(cobolCode, params);
        List<CobolLine> rewrittenLines = rewriteLines(lines);
        return parseDocument(rewrittenLines, params);
    }

    List<CobolLine> readLines(String cobolCode, CobolParserParams params) {
        return cobolLineReader.processLines(cobolCode, params);
    }

    /**
     * Normalizes lines of given COBOL source code, so that comment entries can be
     * parsed and lines have a unified line format.
     */
    List<CobolLine> rewriteLines(List<CobolLine> lines) {
        List<CobolLine> lineIndicatorProcessedLines = cobolLineIndicatorProcessor.processLines(lines);
        List<CobolLine> normalizedInlineCommentEntriesLines = cobolInlineCommentEntriesNormalizer.processLines(lineIndicatorProcessedLines);
        return cobolCommentEntriesMarker.processLines(normalizedInlineCommentEntriesLines);
    }

    public enum CobolSourceFormatEnum {

        /**
         * Fixed format, standard ANSI / IBM reference. Each line 80 chars.<br />
         * <br />
         * 1-6: sequence area<br />
         * 7: indicator field<br />
         * 8-12: area A<br />
         * 13-72: area B<br />
         * 73-80: comments<br />
         */
        FIXED("(.{0,6})(?:" + INDICATOR_FIELD + "(.{0,4})(.{0,61})(.*))?", true),

        /**
         * HP Tandem format.<br />
         * <br />
         * 1: indicator field<br />
         * 2-5: optional area A<br />
         * 6-132: optional area B<br />
         */
        TANDEM("()(?:" + INDICATOR_FIELD + "(.{0,4})(.*)())?", false),

        /**
         * Variable format.<br />
         * <br />
         * 1-6: sequence area<br />
         * 7: indicator field<br />
         * 8-12: optional area A<br />
         * 13-*: optional area B<br />
         */
        VARIABLE("(.{0,6})(?:" + INDICATOR_FIELD + "(.{0,4})(.*)())?", true);

        private final boolean commentEntryMultiLine;

        private final Pattern pattern;

        private final String regex;

        CobolSourceFormatEnum(String regex, boolean commentEntryMultiLine) {
            this.regex = regex;
            pattern = Pattern.compile(regex);
            this.commentEntryMultiLine = commentEntryMultiLine;
        }

        public Pattern getPattern() {
            return pattern;
        }

        public String getRegex() {
            return regex;
        }

        public boolean isCommentEntryMultiLine() {
            return commentEntryMultiLine;
        }
    }
}
