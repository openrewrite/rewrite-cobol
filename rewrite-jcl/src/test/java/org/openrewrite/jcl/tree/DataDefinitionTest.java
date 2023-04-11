package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.ExpectedToFail;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class DataDefinitionTest  implements RewriteTest {

    @ExpectedToFail("Add support for continuations ...")
    @Test
    void ddExample() {
        rewriteRun(
                jcl(
                  """
                  //INOUT4 DD DSNAME=DS4,UNIT=3380,VOL=SER=111112,
                  // DISP=(NEW,KEEP),SPACE=(TRK,(5,1,2))
                  """)
        );
    }
}
