package org.openrewrite.cobol.tree.preprocessor;

import org.junit.jupiter.api.Test;
import org.openrewrite.cobol.CobolTest;

import static org.openrewrite.cobol.Assertions.preprocessor;

public class CobolPreprocessorNistTest extends CobolTest {

    @Test
    void cm101M() {
        rewriteRun(
          preprocessor(getNistResource("CM101M.CBL"))
        );
    }

    @Test
    void cm102M() {
        rewriteRun(
          preprocessor(getNistResource("CM102M.CBL"))
        );
    }

    @Test
    void cm103M() {
        rewriteRun(
          preprocessor(getNistResource("CM103M.CBL"))
        );
    }

    @Test
    void cm104M() {
        rewriteRun(
          preprocessor(getNistResource("CM104M.CBL"))
        );
    }

    @Test
    void cm105M() {
        rewriteRun(
          preprocessor(getNistResource("CM105M.CBL"))
        );
    }

    @Test
    void cm201M() {
        rewriteRun(
          preprocessor(getNistResource("CM201M.CBL"))
        );
    }

    @Test
    void cm202M() {
        rewriteRun(
          preprocessor(getNistResource("CM202M.CBL"))
        );
    }

    @Test
    void cm303M() {
        rewriteRun(
          preprocessor(getNistResource("CM303M.CBL"))
        );
    }
}
