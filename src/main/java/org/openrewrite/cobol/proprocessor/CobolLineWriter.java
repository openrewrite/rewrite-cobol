/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import java.util.List;

public class CobolLineWriter {
    public static String write(List<CobolLine> lines) {
        StringBuilder sb = new StringBuilder();
        boolean noContinuations = lines.stream().noneMatch(l -> CobolLineTypeEnum.CONTINUATION.equals(l.getType()));
        for (CobolLine line : lines) {
            boolean notContinuationLine = !CobolLineTypeEnum.CONTINUATION.equals(line.getType());

            if (notContinuationLine) {
                if (line.getNumber() > 0) {
                    sb.append(CobolPreprocessor.NEWLINE);
                }

                sb.append(line.getBlankSequenceArea());

                // Temporary work around to account for malformed COBOL in CobolDivisionTest.
                sb.append(noContinuations ? line.getIndicatorAreaOriginal() : line.getIndicatorArea());
                sb.append(line.getContentAreaOriginal());
            } else {
                sb.append(line.getContentArea());
            }

        }

        return sb.toString();
    }
}
