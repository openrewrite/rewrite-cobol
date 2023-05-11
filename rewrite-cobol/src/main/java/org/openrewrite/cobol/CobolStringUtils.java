/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol;

public class CobolStringUtils {

    private static final String EMPTY_STRING = "";

    private CobolStringUtils() {
    }

    public static String trimLeadingChar(String contentArea) {
        return contentArea.substring(1);
    }

    public static String trimLeadingWhitespace(String contentArea) {
        return contentArea.replaceAll("^\\s+", EMPTY_STRING);
    }

    public static String trimTrailingWhitespace(String contentArea) {
        return contentArea.replaceAll("\\s+$", EMPTY_STRING);
    }

    public static boolean isSubstituteCharacter(String text) {
        return text.length() == 1 && '\u001A' == text.charAt(0);
    }
}
