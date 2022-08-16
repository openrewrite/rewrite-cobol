/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CobolInlineCommentEntriesNormalizer {

    protected static String denormalizedCommentEntryRegex = "\\*>[^ ]";

    protected Pattern denormalizedCommentEntryPattern = Pattern.compile(denormalizedCommentEntryRegex);

    public CobolLine processLine(CobolLine line) {
        Matcher matcher = denormalizedCommentEntryPattern.matcher(line.getContentArea());
        CobolLine result;

        if (!matcher.find()) {
            result = line;
        } else {
            String newContentArea = line.getContentArea().replace(CobolPreprocessor.COMMENT_TAG,
                    CobolPreprocessor.COMMENT_TAG + CobolPreprocessor.WS);
            result = CobolLine.copyCobolLineWithContentArea(newContentArea, line);
        }

        return result;
    }

    public List<CobolLine> processLines(List<CobolLine> lines) {
        List<CobolLine> result = new ArrayList<>();

        for (CobolLine line : lines) {
            CobolLine processedLine = processLine(line);
            result.add(processedLine);
        }

        return result;
    }
}
