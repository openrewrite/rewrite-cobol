package org.openrewrite.cobol;

import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.internal.StringUtils;

public class CobolPrinterUtils {

    private CobolPrinterUtils() {
    }

    public static int getInsertIndex(String output) {
        int insertIndex = output.lastIndexOf("\n");
        return insertIndex == -1 ? 0 : insertIndex + 1;
    }

    public static int getCurrentIndex(String output) {
        int index = output.lastIndexOf("\n");
        return index == -1 ? index : output.substring(index + 1).length();
    }

    public static String generateWhitespace(int count) {
        if (count < 0) {
            throw new IllegalStateException("Negative index detected.");
        }
        return fillArea(' ', count);
    }

    public static String fillArea(Character character, int count) {
        if (count < 0) {
            throw new IllegalStateException("Negative index detected.");
        }
        return StringUtils.repeat(String.valueOf(character), count);
    }

    public static int getContentAreaLength(CobolDialect cobolDialect) {
        if (cobolDialect.getColumns().getOtherArea() - cobolDialect.getColumns().getContentArea() < 0) {
            throw new IllegalStateException("Negative index detected.");
        }
        return cobolDialect.getColumns().getOtherArea() - cobolDialect.getColumns().getContentArea();
    }
}
