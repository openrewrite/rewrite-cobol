package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

public class JobTest implements RewriteTest {

    @Test
    void job() {
        rewriteRun(
          jcl(
              """
              """
          )
        );
    }
}
