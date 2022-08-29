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
package org.openrewrite.cobol;

import org.openrewrite.cobol.internal.CobolParserVisitor;
import org.openrewrite.cobol.proprocessor.*;
import org.openrewrite.cobol.tree.Cobol;

public class CobolHpTandemParser extends CobolParser {

    public CobolHpTandemParser() {
        super(CobolParserVisitor.CobolDialect.HP_TANDEM, CobolDialect.ANSI85, CobolPreprocessor.CobolSourceFormatEnum.TANDEM);
    }

    public static CobolIbmAnsi85Parser.Builder builder() {
        return new CobolIbmAnsi85Parser.Builder();
    }

    public static class Builder extends org.openrewrite.Parser.Builder {

        public Builder() {
            super(Cobol.CompilationUnit.class);
        }

        @Override
        public CobolParser build() {
            return new CobolHpTandemParser();
        }

        @Override
        public String getDslName() {
            return "cobol";
        }
    }
}