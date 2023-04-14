package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class JclTest implements RewriteTest {

    @Test
    void example() {
        rewriteRun(
          jcl(
            """
            //JOB1 JOB ,'H.H. MORRILL'
            //ADD1 OUTPUT COPIES=2
            //STEPA EXEC PROC=P
            //PS1.OUTA OUTPUT CONTROL=DOUBLE,COPIES=5
            //PS1.DSB DD OUTPUT=*.ADD1
            //PS1.DSE DD *
            """
          )
        );
    }
}
