package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class XmitTest implements RewriteTest {

    @Test
    void exec() {
        rewriteRun(
          jcl("//Name XMIT")
        );
    }

    @Test
    void parameterAssignment() {
        rewriteRun(
          jcl("//Name XMIT PGM=WT1")
        );
    }

    @Test
    void specialCharacters() {
        rewriteRun(
          jcl("//Name XMIT PGM='3400-6'")
        );
    }

    @Test
    void parensAssignment() {
        rewriteRun(
          jcl("//Name XMIT COND.PSTEP3=(4,GT,PSTEP1)")
        );
    }

    @Test
    void startsWithComma() {
        rewriteRun(
          jcl("//Name XMIT TIME=(,50)")
        );
    }

    @Test
    void multipleParameterTypes() {
        rewriteRun(
          jcl("//Name XMIT COND.PSTEP3=(4,GT,PSTEP1),RD=R")
        );
    }

    @Test
    void procParameter() {
        rewriteRun(
          jcl("//Name XMIT PROC=WRIT35")
        );
    }
}
