package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class JobTest implements RewriteTest {

    @Test
    void job() {
        rewriteRun(
          jcl("//Name JOB")
        );
    }

    @Test
    void parameterLiteral() {
        rewriteRun(
          jcl("//Name JOB 'name'")
        );
    }

    @Test
    void parameterLiteralStartsWithComma() {
        rewriteRun(
                jcl("//JOB1 JOB ,'H.H. MORRILL'")
        );
    }

    @Test
    void parameterAssignment() {
        rewriteRun(
          jcl("//Name JOB CLASS=A")
        );
    }

    @Test
    void specialCharacters() {
        rewriteRun(
          jcl("//Name JOB CLASS='3400-6'")
        );
    }

    @Test
    void parensAssignment() {
        rewriteRun(
          jcl("//Name JOB MSGLEVEL=(1,1)")
        );
    }

    @Test
    void startsWithComma() {
        rewriteRun(
          jcl("//Name JOB (,DEPTD58,921)")
        );
    }

    @Test
    void multipleParameterTypes() {
        rewriteRun(
          jcl("//Name JOB 'name',CLASS=A,MSGLEVEL=(1,1)")
        );
    }
}
