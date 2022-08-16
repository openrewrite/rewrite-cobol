/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser;
import org.openrewrite.internal.lang.Nullable;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A mapping from a replaceable to a replacement.
 */
public class CobolReplacementMapping implements Comparable<CobolReplacementMapping> {

    public CobolPreprocessorParser.ReplaceableContext replaceable;

    public CobolPreprocessorParser.ReplacementContext replacement;

    @Override
    public int compareTo(CobolReplacementMapping o) {
        return o.replaceable.getText().length() - replaceable.getText().length();
    }

    private String extractPseudoText(CobolPreprocessorParser.PseudoTextContext pseudoTextCtx, BufferedTokenStream tokens) {
        String pseudoText = TokenUtils.getTextIncludingHiddenTokens(pseudoTextCtx, tokens).trim();
        return pseudoText.replaceAll("^==", "").replaceAll("==$", "").trim();
    }

    /**
     * Whitespace in Cobol replaceables matches line breaks. Hence, the replaceable
     * search string has to be enhanced to a regex, which is returned by this
     * function.
     */
    @Nullable
    private String getRegexFromReplaceable(@Nullable String replaceable) {
        String result;

        if (replaceable == null) {
            result = null;
        } else {
            String[] parts = replaceable.split("\\s+");
            String[] regexParts = new String[parts.length];
            String regexSeparator = "[\\r\\n\\s]+";

            for (int i = 0; i < parts.length; i++) {
                String part = parts[i];
                regexParts[i] = Pattern.quote(part);
            }

            result = String.join(regexSeparator, regexParts);
        }

        return result;
    }

    @Nullable
    private String getText(CobolPreprocessorParser.ReplaceableContext ctx, BufferedTokenStream tokens) {
        String result;

        if (ctx.pseudoText() != null) {
            result = extractPseudoText(ctx.pseudoText(), tokens);
        } else if (ctx.charDataLine() != null) {
            result = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
        } else if (ctx.cobolWord() != null) {
            result = ctx.getText();
        } else if (ctx.literal() != null) {
            result = ctx.literal().getText();
        } else {
            result = null;
        }

        return result;
    }

    @Nullable
    private String getText(CobolPreprocessorParser.ReplacementContext ctx, BufferedTokenStream tokens) {
        String result;

        if (ctx.pseudoText() != null) {
            result = extractPseudoText(ctx.pseudoText(), tokens);
        } else if (ctx.charDataLine() != null) {
            result = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
        } else if (ctx.cobolWord() != null) {
            result = ctx.getText();
        } else if (ctx.literal() != null) {
            result = ctx.literal().getText();
        } else {
            result = null;
        }

        return result;
    }

    @Nullable
    protected String replace(String string, BufferedTokenStream tokens) {
        String replaceableString = getText(replaceable, tokens);
        String replacementString = getText(replacement, tokens);

        String result;

        if (replaceableString != null && replacementString != null) {
            // regex for the replaceable
            String replaceableRegex = getRegexFromReplaceable(replaceableString);
            if (replaceableRegex == null) {
                return null;
            }

            // regex for the replacement
            String quotedReplacementRegex = Matcher.quoteReplacement(replacementString);

            result = Pattern.compile(replaceableRegex).matcher(string).replaceAll(quotedReplacementRegex);
        } else {
            result = string;
        }

        return result;
    }

    @Override
    public String toString() {
        return replaceable.getText() + " -> " + replacement.getText();
    }
}
