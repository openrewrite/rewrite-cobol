/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorBaseListener;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser;
import org.openrewrite.internal.lang.Nullable;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;

/**
 * ANTLR visitor, which preprocesses a given COBOL program by executing COPY and
 * REPLACE statements.
 */
@Value
@EqualsAndHashCode(callSuper = true)
public class CobolDocumentParserListener extends CobolPreprocessorBaseListener {

    Stack<CobolDocumentContext> contexts;
    CobolParserParams params;
    BufferedTokenStream tokens;
    CobolWordCopyBookFinder cobolWordCopyBookFinder;
    FilenameCopyBookFinder filenameCopyBookFinder;
    LiteralCopyBookFinder literalCopyBookFinder;

    public CobolDocumentParserListener(CobolParserParams params, BufferedTokenStream tokens) {
        this.params = params;
        this.tokens = tokens;

        contexts = new Stack<>();
        cobolWordCopyBookFinder = new CobolWordCopyBookFinder();
        filenameCopyBookFinder = new FilenameCopyBookFinder();
        literalCopyBookFinder = new LiteralCopyBookFinder();
        contexts.push(new CobolDocumentContext());
    }

    String buildLines(String text, String linePrefix) {
        StringBuilder sb = new StringBuilder(text.length());
        Scanner scanner = new Scanner(text);
        boolean firstLine = true;

        while (scanner.hasNextLine()) {
            if (!firstLine) {
                sb.append(CobolPreprocessor.NEWLINE);
            }

            String line = scanner.nextLine();
            String trimmedLine = line.trim();
            String prefixedLine = linePrefix + CobolPreprocessor.WS + trimmedLine;
            String suffixedLine = prefixedLine.replaceAll("(?i)(end-exec)",
                    "$1 " + CobolPreprocessor.EXEC_END_TAG);

            sb.append(suffixedLine);
            firstLine = false;
        }

        scanner.close();
        return sb.toString();
    }

    public CobolDocumentContext context() {
        return contexts.peek();
    }

    public void enterCompilerOptions(CobolPreprocessorParser.CompilerOptionsContext ctx) {
        // push a new context for COMPILER OPTIONS terminals
        push();
    }

    public void enterCopyStatement(CobolPreprocessorParser.CopyStatementContext ctx) {
        // push a new context for COPY terminals
        push();
    }

    public void enterEjectStatement(CobolPreprocessorParser.EjectStatementContext ctx) {
        push();
    }

    public void enterExecCicsStatement(CobolPreprocessorParser.ExecCicsStatementContext ctx) {
        // push a new context for SQL terminals
        push();
    }

    public void enterExecSqlImsStatement(CobolPreprocessorParser.ExecSqlImsStatementContext ctx) {
        // push a new context for SQL IMS terminals
        push();
    }

    public void enterExecSqlStatement(CobolPreprocessorParser.ExecSqlStatementContext ctx) {
        // push a new context for SQL terminals
        push();
    }

    public void enterReplaceArea(CobolPreprocessorParser.ReplaceAreaContext ctx) {
        push();
    }

    public void enterReplaceByStatement(CobolPreprocessorParser.ReplaceByStatementContext ctx) {
        push();
    }

    public void enterReplaceOffStatement(CobolPreprocessorParser.ReplaceOffStatementContext ctx) {
        push();
    }

    public void enterSkipStatement(CobolPreprocessorParser.SkipStatementContext ctx) {
        push();
    }

    public void enterTitleStatement(CobolPreprocessorParser.TitleStatementContext ctx) {
        push();
    }


    public void exitCompilerOptions(CobolPreprocessorParser.CompilerOptionsContext ctx) {
        // throw away COMPILER OPTIONS terminals
        pop();
    }


    public void exitCopyStatement(CobolPreprocessorParser.CopyStatementContext ctx) {
        // throw away COPY terminals
        pop();

        // a new context for the copy book content
        push();

        /*
         * replacement phrase
         */
        for (CobolPreprocessorParser.ReplacingPhraseContext replacingPhrase : ctx.replacingPhrase()) {
            context().storeReplaceablesAndReplacements(replacingPhrase.replaceClause());
        }

        /*
         * copy the copy book
         */
        CobolPreprocessorParser.CopySourceContext copySource = ctx.copySource();
        String copyBookContent = getCopyBookContent(copySource, params);

        if (copyBookContent != null) {
            context().write(copyBookContent + CobolPreprocessor.NEWLINE);
            context().replaceReplaceablesByReplacements(tokens);
        }

        String content = context().read();
        pop();

        context().write(content);
    }


    public void exitEjectStatement(CobolPreprocessorParser.EjectStatementContext ctx) {
        // throw away eject statement
        pop();
    }


    public void exitExecCicsStatement(CobolPreprocessorParser.ExecCicsStatementContext ctx) {
        // throw away EXEC CICS terminals
        pop();

        // a new context for the CICS statement
        push();

        /*
         * text
         */
        String text = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
        String linePrefix = CobolLine.createBlankSequenceArea(params.getFormat())
                + CobolPreprocessor.EXEC_CICS_TAG;
        String lines = buildLines(text, linePrefix);

        context().write(lines);

        String content = context().read();
        pop();

        context().write(content);
    }


    public void exitExecSqlImsStatement(CobolPreprocessorParser.ExecSqlImsStatementContext ctx) {
        // throw away EXEC SQLIMS terminals
        pop();

        // a new context for the SQLIMS statement
        push();

        /*
         * text
         */
        String text = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
        String linePrefix = CobolLine.createBlankSequenceArea(params.getFormat())
                + CobolPreprocessor.EXEC_SQLIMS_TAG;
        String lines = buildLines(text, linePrefix);

        context().write(lines);

        String content = context().read();
        pop();

        context().write(content);
    }


    public void exitExecSqlStatement(CobolPreprocessorParser.ExecSqlStatementContext ctx) {
        // throw away EXEC SQL terminals
        pop();

        // a new context for the SQL statement
        push();

        /*
         * text
         */
        String text = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
        String linePrefix = CobolLine.createBlankSequenceArea(params.getFormat())
                + CobolPreprocessor.EXEC_SQL_TAG;
        String lines = buildLines(text, linePrefix);

        context().write(lines);

        String content = context().read();
        pop();

        context().write(content);
    }


    public void exitReplaceArea(CobolPreprocessorParser.ReplaceAreaContext ctx) {
        /*
         * replacement phrase
         */
        List<CobolPreprocessorParser.ReplaceClauseContext> replaceClauses = ctx.replaceByStatement().replaceClause();
        context().storeReplaceablesAndReplacements(replaceClauses);

        context().replaceReplaceablesByReplacements(tokens);
        String content = context().read();

        pop();
        context().write(content);
    }


    public void exitReplaceByStatement(CobolPreprocessorParser.ReplaceByStatementContext ctx) {
        // throw away terminals
        pop();
    }


    public void exitReplaceOffStatement(CobolPreprocessorParser.ReplaceOffStatementContext ctx) {
        // throw away REPLACE OFF terminals
        pop();
    }


    public void exitSkipStatement(CobolPreprocessorParser.SkipStatementContext ctx) {
        // throw away skip statement
        pop();
    }


    public void exitTitleStatement(CobolPreprocessorParser.TitleStatementContext ctx) {
        // throw away title statement
        pop();
    }

    @Nullable
    File findCopyBook(CobolPreprocessorParser.CopySourceContext copySource, CobolParserParams params) {
        File result;

        if (copySource.cobolWord() != null) {
            result = cobolWordCopyBookFinder.findCopyBook(params, copySource.cobolWord());
        } else if (copySource.literal() != null) {
            result = literalCopyBookFinder.findCopyBook(params, copySource.literal());
        } else if (copySource.filename() != null) {
            result = filenameCopyBookFinder.findCopyBook(params, copySource.filename());
        } else {
            result = null;
        }

        return result;
    }

    @Nullable
    String getCopyBookContent(CobolPreprocessorParser.CopySourceContext copySource, CobolParserParams params) {
        File copyBook = findCopyBook(copySource, params);
        String result;

        if (copyBook == null) {
            throw new IllegalStateException("Could not find copy book " + copySource.getText()
                    + " in directory of COBOL input file or copy books param object.");
        } else {
            try {
                result = new CobolPreprocessor(
                        new CobolCommentEntriesMarker(),
                        new CobolDocumentParser(),
                        new CobolInlineCommentEntriesNormalizer(),
                        new CobolLineIndicatorProcessor(),
                        new CobolLineReader()
                ).process(copyBook, params);
            } catch (IOException e) {
                result = null;
            }
        }

        return result;
    }

    /**
     * Pops the current preprocessing context from the stack.
     */
    @SuppressWarnings("UnusedReturnValue")
    CobolDocumentContext pop() {
        return contexts.pop();
    }

    /**
     * Pushes a new preprocessing context onto the stack.
     */
    @SuppressWarnings("UnusedReturnValue")
    CobolDocumentContext push() {
        return contexts.push(new CobolDocumentContext());
    }


    public void visitTerminal(TerminalNode node) {
        int tokPos = node.getSourceInterval().a;
        context().write(TokenUtils.getHiddenTokensToLeft(tokPos, tokens));

        if (TokenUtils.isNotEOF(node)) {
            String text = node.getText();
            context().write(text);
        }
    }
}
