package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.ExpectedToFail;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class DataDefinitionTest  implements RewriteTest {

    @Test
    void dd() {
        rewriteRun(
          jcl("//Name DD")
        );
    }

    @Test
    void parameterAssignment() {
        rewriteRun(
          jcl("//Name DD DSNAME=ALPHA.PGM")
        );
    }

    @Test
    void specialCharacters() {
        rewriteRun(
          jcl("//Name DD DSNAME='3400-6'")
        );
    }

    @Test
    void multiAssignment() {
        rewriteRun(
          jcl("//Name DD VOLUME=SER=389984")
        );
    }
    @Test
    void nameWithParameter() {
        rewriteRun(
          jcl("//Name DD DSNAME=REPORT.THREE(WEEK3)")
        );
    }

    @Test
    void parensAssignment() {
        rewriteRun(
          jcl("//Name DD DISP=(NEW,KEEP)")
        );
    }

    @Test
    void startsWithComma() {
        rewriteRun(
          jcl("//Name DD DISP=(,KEEP)")
        );
    }

    @Test
    void multipleParameterTypes() {
        rewriteRun(
          jcl("//Name DD DSNAME=DS4,DISP=(NEW,KEEP),SPACE=(TRK,(5,1,2))")
        );
    }

    @Test
    void outputParameter() {
        rewriteRun(
          jcl("//Name DD OUTPUT=(*.OUT1,*.OUT2)")
        );
    }

    @ExpectedToFail("Add support for continuations ...")
    @Test
    void continuation() {
        rewriteRun(
          jcl(
            """
            //Name DD DSNAME=DS4,UNIT=3380,VOL=SER=111112,
            // DISP=(NEW,KEEP),SPACE=(TRK,(5,1,2))
            """
          )
        );
    }
}
