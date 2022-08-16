/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorBaseListener;

/**
 * ANTLR listener, which collects visible as well as hidden tokens for a given
 * parse tree in a string buffer.
 */
public class CobolHiddenTokenCollectorListener extends CobolPreprocessorBaseListener {

    boolean firstTerminal = true;

    private final StringBuffer outputBuffer = new StringBuffer();

    private final BufferedTokenStream tokens;

    public CobolHiddenTokenCollectorListener(BufferedTokenStream tokens) {
        this.tokens = tokens;
    }

    public String read() {
        return outputBuffer.toString();
    }

    @Override
    public void visitTerminal(TerminalNode node) {
        if (!firstTerminal) {
            int tokPos = node.getSourceInterval().a;
            outputBuffer.append(TokenUtils.getHiddenTokensToLeft(tokPos, tokens));
        }

        if (TokenUtils.isNotEOF(node)) {
            String text = node.getText();
            outputBuffer.append(text);
        }

        firstTerminal = false;
    }
}