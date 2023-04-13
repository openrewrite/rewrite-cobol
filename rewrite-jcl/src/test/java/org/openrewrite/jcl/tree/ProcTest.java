package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class ProcTest implements RewriteTest {

    @Test
    void proc() {
        rewriteRun(
          jcl("//Name PROC")
        );
    }

    @Test
    void parameterAssignment() {
        rewriteRun(
          jcl("//Name PROC STATUS=OLD")
        );
    }

    @Test
    void specialCharacters() {
        rewriteRun(
          jcl("//Name PROC PARM3='3400-6'")
        );
    }

    @Test
    void multipleParameterTypes() {
        rewriteRun(
          jcl("//Name PROC STATUS=OLD,LIBRARY=SYSLIB,NUMBER=777777")
        );
    }
}
