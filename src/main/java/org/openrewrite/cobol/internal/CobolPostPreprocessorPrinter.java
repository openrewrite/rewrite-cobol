/*
 * Copyright 2022 the original author or authors.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * https://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openrewrite.cobol.internal;

import org.openrewrite.PrintOutputCapture;
import org.openrewrite.cobol.tree.*;

public class CobolPostPreprocessorPrinter<P> extends CobolPreprocessorPrinter<P> {

    @Override
    public CobolPreprocessor visitCopyStatement(CobolPreprocessor.CopyStatement copyStatement, PrintOutputCapture<P> p) {
        return copyStatement;
    }

    @Override
    public CobolPreprocessor visitReplaceArea(CobolPreprocessor.ReplaceArea replaceArea, PrintOutputCapture<P> p) {
        return replaceArea;
    }

    public CobolPreprocessor visitWord(CobolPreprocessor.Word word, PrintOutputCapture<P> p) {
        // TODO: fix comment area whitespace.
        visitSpace(word.getPrefix(), p);
        visitMarkers(word.getMarkers(), p);
        p.append(word.getWord());
        return word;
    }
}
