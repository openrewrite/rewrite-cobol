package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class Jes3Test implements RewriteTest {
    @Test
    void statement() {
        rewriteRun(
          jcl("//*FORMAT")
        );
    }
}
