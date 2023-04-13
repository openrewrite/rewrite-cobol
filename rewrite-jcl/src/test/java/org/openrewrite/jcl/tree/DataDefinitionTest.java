package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.ExpectedToFail;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class DataDefinitionTest  implements RewriteTest {

    @Test
    void dd() {
        rewriteRun(
          jcl("//INOUT4 DD")
        );
    }

    @Test
    void parameterAssignment() {
        rewriteRun(
          jcl("//DDSMS1 DD DSNAME=ALPHA.PGM")
        );
    }

    @Test
    void specialCharacters() {
        rewriteRun(
          jcl("//DDSMS1 DD DSNAME='3400-6'")
        );
    }

    @Test
    void multiAssignment() {
        rewriteRun(
          jcl("//DDSMS1 DD VOLUME=SER=389984")
        );
    }
    @Test
    void nameWithParameter() {
        rewriteRun(
          jcl("//DDSMS1 DD DSNAME=REPORT.THREE(WEEK3)")
        );
    }

    @Test
    void parensAssignment() {
        rewriteRun(
          jcl("//DDSMS1 DD DISP=(NEW,KEEP)")
        );
    }

    @Test
    void startsWithComma() {
        rewriteRun(
          jcl("//DDSMS1 DD DISP=(,KEEP)")
        );
    }

    @Test
    void multipleParameterTypes() {
        rewriteRun(
          jcl("//INOUT4 DD DSNAME=DS4,DISP=(NEW,KEEP),SPACE=(TRK,(5,1,2))")
        );
    }

    @ExpectedToFail("Add support for continuations ...")
    @Test
    void continuation() {
        rewriteRun(
          jcl(
            """
            //INOUT4 DD DSNAME=DS4,UNIT=3380,VOL=SER=111112,
            // DISP=(NEW,KEEP),SPACE=(TRK,(5,1,2))
            """
          )
        );
    }
}
