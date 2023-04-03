package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class JobTest implements RewriteTest {

    @Test
    void job() {
        rewriteRun(
          jcl(
            """
            //JobName JOB
            """
          )
        );
    }

    @Test
    void jobWithParameters() {
        rewriteRun(
          jcl(
            """
            //JobName JOB 'jobname',CLASS=A,MSGCLASS=X
            """
          )
        );
    }
}
