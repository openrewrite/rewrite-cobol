package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class ExecTest implements RewriteTest {

    @Test
    void exec() {
        rewriteRun(
          jcl("//PSTEP1 EXEC")
        );
    }

    @Test
    void parameterAssignment() {
        rewriteRun(
          jcl("//PSTEP1 EXEC PGM=WT1")
        );
    }

    @Test
    void specialCharacters() {
        rewriteRun(
          jcl("//PSTEP1 EXEC PGM='3400-6'")
        );
    }

    @Test
    void parensAssignment() {
        rewriteRun(
          jcl("//PSTEP1 EXEC COND.PSTEP3=(4,GT,PSTEP1)")
        );
    }

    @Test
    void startsWithComma() {
        rewriteRun(
          jcl("//PSTEP1 EXEC TIME=(,50)")
        );
    }

    @Test
    void multipleParameterTypes() {
        rewriteRun(
          jcl("//STEPB EXEC COND.PSTEP3=(4,GT,PSTEP1),RD=R")
        );
    }

    @Test
    void procParameter() {
        rewriteRun(
          jcl("//STEPB EXEC PROC=WRIT35")
        );
    }
}
