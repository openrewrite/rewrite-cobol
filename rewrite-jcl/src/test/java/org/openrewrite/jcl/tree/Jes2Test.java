package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class Jes2Test implements RewriteTest {
    @Test
    void statement() {
        rewriteRun(
          jcl("/*JOBPARM")
        );
    }
}
