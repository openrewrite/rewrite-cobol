/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorLexer;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser;

/**
 * Preprocessor, which parses and processes COPY REPLACE and EXEC SQL
 * statements.
 */
public class CobolDocumentParser {

    protected String[] triggers = new String[]{"cbl", "copy", "exec sql", "exec sqlims", "exec cics", "process",
            "replace", "eject", "skip1", "skip2", "skip3", "title"};

    protected boolean containsTrigger(String code, String[] triggers) {
        String codeLowerCase = code.toLowerCase();
        boolean result = false;

        for (String trigger : triggers) {
            boolean containsTrigger = codeLowerCase.contains(trigger);

            if (containsTrigger) {
                result = true;
                break;
            }
        }

        return result;
    }

    protected CobolDocumentParserListener createDocumentParserListener(CobolParserParams params,
                                                                       CommonTokenStream tokens) {
        return new CobolDocumentParserListener(params, tokens);
    }

    public String processLines(String code, CobolParserParams params) {
        boolean requiresProcessorExecution = containsTrigger(code, triggers);
        String result;

        if (requiresProcessorExecution) {
            result = processWithParser(code, params);
        } else {
            result = code;
        }

        return result;
    }

    protected String processWithParser(String code, CobolParserParams params) {
        // run the lexer
        CobolPreprocessorLexer lexer = new CobolPreprocessorLexer(CharStreams.fromString(code));

        // get a list of matched tokens
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // pass the tokens to the parser
        CobolPreprocessorParser parser = new CobolPreprocessorParser(tokens);

        // specify our entry point
        CobolPreprocessorParser.StartRuleContext startRule = parser.startRule();

        // analyze contained copy books
        CobolDocumentParserListener listener = createDocumentParserListener(params, tokens);
        ParseTreeWalker walker = new ParseTreeWalker();

        walker.walk(listener, startRule);

        return listener.context().read();
    }
}
