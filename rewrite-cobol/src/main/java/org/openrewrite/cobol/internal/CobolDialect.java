/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.internal;

import java.util.Collection;

public interface CobolDialect {
    Collection<String> getSeparators();
    Collection<Character> getCommentIndicators();
    Columns getColumns();

    static IbmAnsi85 ibmAnsi85() {
        return IbmAnsi85.getInstance();
    }

    static HpTandem hpTandem() {
        return HpTandem.getInstance();
    }

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
