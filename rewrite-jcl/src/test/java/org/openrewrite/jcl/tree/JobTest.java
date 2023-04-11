package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.ExpectedToFail;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class JobTest implements RewriteTest {

    @Test
    void job() {
        rewriteRun(
          jcl("//JobName JOB")
        );
    }

    @Test
    void parameterLiteral() {
        rewriteRun(
          jcl("//JobName JOB 'jobname'")
        );
    }

    @Test
    void parameterAssignment() {
        rewriteRun(
                jcl("//JobName JOB CLASS=A")
        );
    }

    @Test
    void parensAssignment() {
        rewriteRun(
                jcl("//JobName JOB MSGLEVEL=(1,1)")
        );
    }

    @ExpectedToFail("Formalize parameters in the grammar, add parameters to the LST element, parse/print")
    @Test
    void multipleParameterTypes() {
        rewriteRun(
          jcl("//JobName JOB 'jobname',CLASS=A,MSGLEVEL=(1,1)")
        );
    }
}
