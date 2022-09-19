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

import java.util.Collection;
import java.util.Set;

public interface CobolDialect {
    Collection<String> getSeparators();

    Columns getColumns();

    enum Columns {

        /**
         * Fixed format, standard ANSI / IBM reference. Each line 80 chars.<br />
         * <br />
         * 1-6: sequence area<br />
         * 7: indicator field<br />
         * 8-12: area A<br />
         * 13-72: area B<br />
         * 73-80: comments<br />
         */
        IBM_ANSI_85(0, 6, 7, 72),

        /**
         * HP Tandem format.<br />
         * <br />
         * 1: indicator field<br />
         * 2-5: optional area A<br />
         * 6-132: optional area B<br />
         */
        HP_TANDEM(Integer.MIN_VALUE, 0, 1, 5);

        private final int sequenceArea;
        private final int indicatorArea;
        private final int contentArea;
        private final int otherArea;

        Columns(int sequenceArea,
                int indicatorArea,
                int contentArea,
                int otherArea) {
            this.sequenceArea = sequenceArea;
            this.indicatorArea = indicatorArea;
            this.contentArea = contentArea;
            this.otherArea = otherArea;
        }

        public int getSequenceArea() {
            return sequenceArea;
        }

        public int getIndicatorArea() {
            return indicatorArea;
        }

        public int getContentArea() {
            return contentArea;
        }

        public int getOtherArea() {
            return otherArea;
        }
    }
}
