package org.openrewrite.cobol.tree.preprocessor;

import org.junit.jupiter.api.Test;
import org.openrewrite.ExecutionContext;
import org.openrewrite.cobol.CobolTest;
import org.openrewrite.cobol.internal.CobolPreprocessorPrinter;

import static org.openrewrite.cobol.Assertions.preprocessor;

public class CobolPreprocessorCopyBookTest extends CobolTest {
    private final CobolPreprocessorPrinter<ExecutionContext> printer = new CobolPreprocessorPrinter<>(false, true);

    @Test
    void newLineInContentArea() {
        rewriteRun(
          preprocessor(
            """
              000001 IDENTIFICATION  DIVISION  .
              000002 PROGRAM-ID    . HELLO     .
              000003 PROCEDURE DIVISION        .
              000004 DISPLAY 'Hello world!'    .
              000005 STOP RUN                  .
              """
          )
        );
    }

    @Test
    void altl1() {
        rewriteRun(
          preprocessor(getNistResource("ALTL1.CPY"))
        );
    }

    @Test
    void altlb() {
        rewriteRun(
          preprocessor(getNistResource("ALTLB.CPY"))
        );
    }

    @Test
    void k1daa() {
        rewriteRun(
          preprocessor(getNistResource("K1DAA.CPY"))
        );
    }

    @Test
    void k1fda() {
        rewriteRun(
          preprocessor(getNistResource("K1FDA.CPY"))
        );
    }

    @Test
    void k1p01() {
        rewriteRun(
          preprocessor(getNistResource("K1P01.CPY"))
        );
    }

    @Test
    void k1pra() {
        rewriteRun(
          preprocessor(getNistResource("K1PRA.CPY"))
        );
    }

    @Test
    void k1prb() {
        rewriteRun(
          preprocessor(getNistResource("K1PRB.CPY"))
        );
    }

    @Test
    void k1prc() {
        rewriteRun(
          preprocessor(getNistResource("K1PRC.CPY"))
        );
    }

    @Test
    void k1sea() {
        rewriteRun(
          preprocessor(getNistResource("K1SEA.CPY"))
        );
    }

    @Test
    void k1w01() {
        rewriteRun(
          preprocessor(getNistResource("K1W01.CPY"))
        );
    }

    @Test
    void k1w02() {
        rewriteRun(
          preprocessor(getNistResource("K1W02.CPY"))
        );
    }

    @Test
    void k1w03() {
        rewriteRun(
          preprocessor(getNistResource("K1W03.CPY"))
        );
    }

    @Test
    void k1w04() {
        rewriteRun(
          preprocessor(getNistResource("K1W04.CPY"))
        );
    }

    @Test
    void k1wka() {
        rewriteRun(
          preprocessor(getNistResource("K1WKA.CPY"))
        );
    }

    @Test
    void k1wkb() {
        rewriteRun(
          preprocessor(getNistResource("K1WKB.CPY"))
        );
    }

    @Test
    void k1wkc() {
        rewriteRun(
          preprocessor(getNistResource("K1WKC.CPY"))
        );
    }

    @Test
    void k1wky() {
        rewriteRun(
          preprocessor(getNistResource("K1WKY.CPY"))
        );
    }

    @Test
    void k1wkz() {
        rewriteRun(
          preprocessor(getNistResource("K1WKZ.CPY"))
        );
    }

    @Test
    void k2pra() {
        rewriteRun(
          preprocessor(getNistResource("K2PRA.CPY"))
        );
    }

    @Test
    void k3fca() {
        rewriteRun(
          preprocessor(getNistResource("K3FCA.CPY"))
        );
    }

    @Test
    void k3fcb() {
        rewriteRun(
          preprocessor(getNistResource("K3FCB.CPY"))
        );
    }

    @Test
    void k3ioa() {
        rewriteRun(
          preprocessor(getNistResource("K3IOA.CPY"))
        );
    }

    @Test
    void k3iob() {
        rewriteRun(
          preprocessor(getNistResource("K3IOB.CPY"))
        );
    }

    @Test
    void k3lge() {
        rewriteRun(
          preprocessor(getNistResource("K3LGE.CPY"))
        );
    }

    @Test
    void k3oca() {
        rewriteRun(
          preprocessor(getNistResource("K3OCA.CPY"))
        );
    }

    @Test
    void k3sml() {
        rewriteRun(
          preprocessor(getNistResource("K3SML.CPY"))
        );
    }

    @Test
    void k3sna() {
        rewriteRun(
          preprocessor(getNistResource("K3SNA.CPY"))
        );
    }

    @Test
    void k3snb() {
        rewriteRun(
          preprocessor(getNistResource("K3SNB.CPY"))
        );
    }

    @Test
    void k5sda() {
        rewriteRun(
          preprocessor(getNistResource("K5SDA.CPY"))
        );
    }

    @Test
    void k6sca() {
        rewriteRun(
          preprocessor(getNistResource("K6SCA.CPY"))
        );
    }

    @Test
    void k7sea() {
        rewriteRun(
          preprocessor(getNistResource("K7SEA.CPY"))
        );
    }

    @Test
    void k501A() {
        rewriteRun(
          preprocessor(getNistResource("K501A.CPY"))
        );
    }

    @Test
    void k501B() {
        rewriteRun(
          preprocessor(getNistResource("K501B.CPY"))
        );
    }

    @Test
    void kk208A() {
        rewriteRun(
          preprocessor(getNistResource("KK208A.CPY"))
        );
    }

    @Test
    void kp001() {
        rewriteRun(
          preprocessor(getNistResource("KP001.CPY"))
        );
    }

    @Test
    void kp002() {
        rewriteRun(
          preprocessor(getNistResource("KP002.CPY"))
        );
    }

    @Test
    void kp003() {
        rewriteRun(
          preprocessor(getNistResource("KP003.CPY"))
        );
    }

    @Test
    void kp004() {
        rewriteRun(
          preprocessor(getNistResource("KP004.CPY"))
        );
    }

    @Test
    void kp005() {
        rewriteRun(
          preprocessor(getNistResource("KP005.CPY"))
        );
    }

    @Test
    void kp006() {
        rewriteRun(
          preprocessor(getNistResource("KP006.CPY"))
        );
    }

    @Test
    void kp007() {
        rewriteRun(
          preprocessor(getNistResource("KP007.CPY"))
        );
    }

    @Test
    void kp008() {
        rewriteRun(
          preprocessor(getNistResource("KP008.CPY"))
        );
    }

    @Test
    void kp009() {
        rewriteRun(
          preprocessor(getNistResource("KP009.CPY"))
        );
    }

    @Test
    void kp010() {
        rewriteRun(
          preprocessor(getNistResource("KP010.CPY"))
        );
    }

    @Test
    void ksm31() {
        rewriteRun(
          preprocessor(getNistResource("KSM31.CPY"))
        );
    }

    @Test
    void ksm41() {
        rewriteRun(
          preprocessor(getNistResource("KSM41.CPY"))
        );
    }

    @Test
    void trailingSub() {
        rewriteRun(
          preprocessor(getNistResource("K1WKA_TRAILING_SUB.CPY"))
        );
    }
}
