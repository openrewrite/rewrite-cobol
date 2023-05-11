/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.internal;

/**
 * Collection keys required by the COBOL antlr grammar.
 */
public class CobolGrammarToken {
    public static final String COMMENT_ENTRY = "*>CE ";
    public static final String COMMENT = "*> ";
    public static final String END_OF_FILE = "<EOF>";
}
