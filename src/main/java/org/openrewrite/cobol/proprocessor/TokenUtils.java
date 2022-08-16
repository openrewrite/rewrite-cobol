/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorLexer;

import java.util.List;

public class TokenUtils {

    public static String getHiddenTokensToLeft(int tokPos, BufferedTokenStream tokens) {
        List<Token> refChannel = tokens.getHiddenTokensToLeft(tokPos, CobolPreprocessorLexer.HIDDEN);
        StringBuilder sb = new StringBuilder();

        if (refChannel != null) {
            for (Token refToken : refChannel) {
                String text = refToken.getText();
                sb.append(text);
            }
        }

        return sb.toString();
    }

    public static String getTextIncludingHiddenTokens(ParseTree ctx, BufferedTokenStream tokens) {
        CobolHiddenTokenCollectorListener listener = new CobolHiddenTokenCollectorListener(tokens);
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, ctx);

        return listener.read();
    }

    public static boolean isNotEOF(TerminalNode node) {
        return Token.EOF != node.getSymbol().getType();
    }
}
