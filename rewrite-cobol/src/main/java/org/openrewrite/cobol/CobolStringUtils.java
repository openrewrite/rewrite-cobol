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
}
