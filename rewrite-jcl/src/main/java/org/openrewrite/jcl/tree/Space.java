/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.jcl.tree;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import lombok.EqualsAndHashCode;
import org.openrewrite.internal.lang.Nullable;

import java.util.Map;
import java.util.WeakHashMap;

/**
 * JCL white space.
 */
@EqualsAndHashCode
@JsonIdentityInfo(generator = ObjectIdGenerators.IntSequenceGenerator.class, property = "@ref")
public class Space {
    public static final Space EMPTY = new Space("", false);

    @Nullable
    private final String whitespace;

    @Nullable
    private final Boolean isContinued;

    /*
     * Most occurrences of spaces will have no comments or markers and will be repeated frequently throughout a source file.
     * e.g.: a single space between keywords, or the common indentation of every line in a block.
     * So use flyweights to avoid storing many instances of functionally identical spaces
     */
    private static final Map<String, Space> flyweights = new WeakHashMap<>();

    private Space(@Nullable String whitespace, @Nullable Boolean isContinued) {
        this.whitespace = whitespace == null || whitespace.isEmpty() ? null : whitespace;
        this.isContinued = isContinued;
    }

    @JsonCreator
    public static Space build(@Nullable String whitespace, @Nullable Boolean isContinued) {
        if (!Boolean.TRUE.equals(isContinued) && (whitespace == null || whitespace.isEmpty())) {
            return Space.EMPTY;
        }
        String key = (whitespace == null ? "" : whitespace) + (isContinued == null ? "" : isContinued);
        return flyweights.computeIfAbsent(key, k -> new Space(whitespace, isContinued));
    }

    public String getIndent() {
        return getWhitespaceIndent(whitespace);
    }

    private String getWhitespaceIndent(@Nullable String whitespace) {
        if (whitespace == null) {
            return "";
        }
        int lastNewline = whitespace.lastIndexOf('\n');
        if (lastNewline >= 0) {
            return whitespace.substring(lastNewline + 1);
        } else if (lastNewline == whitespace.length() - 1) {
            return "";
        }
        return whitespace;
    }

    public String getWhitespace() {
        return whitespace == null ? "" : whitespace;
    }


    public Space withWhitespace(String whitespace) {
        if (whitespace.isEmpty()) {
            return Space.EMPTY;
        }

        if (this.whitespace == null || whitespace.equals(this.whitespace)) {
            return this;
        }
        return build(whitespace, isContinued);
    }

    public Boolean isContinued() {
        return isContinued != null && isContinued;
    }

    public Space withContinued(boolean continued) {
        if (this.isContinued == null || continued == this.isContinued) {
            return this;
        }
        return build(whitespace, continued);
    }

    public boolean isEmpty() {
        return this == EMPTY;
    }

    private static final String[] spaces = {
            "·₁", "·₂", "·₃", "·₄", "·₅", "·₆", "·₇", "·₈", "·₉", "·₊"
    };

    private static final String[] tabs = {
            "-₁", "-₂", "-₃", "-₄", "-₅", "-₆", "-₇", "-₈", "-₉", "-₊"
    };

    @Override
    public String toString() {
        StringBuilder printedWs = new StringBuilder();
        int lastNewline = 0;
        if (whitespace != null) {
            char[] charArray = whitespace.toCharArray();
            for (int i = 0; i < charArray.length; i++) {
                char c = charArray[i];
                if (c == '\n') {
                    printedWs.append("\\n");
                    lastNewline = i + 1;
                } else if (c == '\r') {
                    printedWs.append("\\r");
                    lastNewline = i + 1;
                } else if (c == ' ') {
                    printedWs.append(spaces[(i - lastNewline) % 10]);
                } else if (c == '\t') {
                    printedWs.append(tabs[(i - lastNewline) % 10]);
                }
            }
        }

        return "Space(whitespace='" + printedWs + "')";
    }

    public enum Location {
        TODO, // REMOVE
        ASSIGNMENT,
        ASSIGNMENT_PREFIX,
        COMPILATION_UNIT_PREFIX,
        COMPILATION_UNIT_EOF,
        DATA_DEFINITION_STATEMENT_PREFIX,
        EXEC_STATEMENT_PREFIX,
        IDENTIFIER_PREFIX,
        JCL_STATEMENT_PREFIX,
        JOB_STATEMENT_PREFIX,
        JCL_NAME_PREFIX,
        LITERAL_PREFIX,
        OUTPUT_STATEMENT_PREFIX,
        PARAMETERS,
        PARENTHESES,
        PARENTHESES_PREFIX,
        PEND_PREFIX,
        PROC_STATEMENT_PREFIX,
        SET_STATEMENT_PREFIX,
        XMIT_STATEMENT_PREFIX,
        UNSUPPORTED_PREFIX
    }
}
