package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class PendTest implements RewriteTest {

    @Test
    void proc() {
        rewriteRun(
          jcl("//Name PROC")
        );
    }
}
