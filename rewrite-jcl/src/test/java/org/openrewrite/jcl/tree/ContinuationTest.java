/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.ExpectedToFail;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class ContinuationTest implements RewriteTest {

    @Test
    void splitByParam() {
        rewriteRun(
          jcl(
            """
            //Name DD DSNAME=DS4,UNIT=3380,VOL=SER=111112,
            //      DISP=(NEW,KEEP),SPACE=(TRK,(5,1,2))
            """
          )
        );
    }

    @Test
    void endCommaWithWhitespace() {
        rewriteRun(
          jcl(
            """
            //Name DD DSNAME=DS4,UNIT=3380,VOL=SER=111112         ,
            //      DISP=(NEW,KEEP),SPACE=(TRK,(5,1,2))
            """
          )
        );
    }

    @Test
    void blankLabelsAndContinuations() {
        rewriteRun(
          jcl(
            """
            //DUMPCHK  JOB 'accounting_info',MSGLEVEL=(1,1)
            //DUMPCHK  PROC
            //DUMPCHK  EXEC PGM=DMPCHKO,REGION=5M,PARM='/&SG,&JDATE,&DAY'
            //STEPLIB  DD DSN=JCR.PGM.LOAD,DISP=SHR
            //CDS      DD DSN=DATAMGT.CDS,DISP=SHR
            //         DD DSN=DATAMGT.CDS.CLEAR,DISP=SHR
            //         DD DSN=DATAMGT.CDS.Y43DUMPS,DISP=SHR
            //Name DD DSNAME=DS4,UNIT=3380,VOL=SER=111112,
            //      DISP=(NEW,KEEP),SPACE=(TRK,(5,1,2))
            //LOG      DD DSN=SYS1.TSODUMP.LOG,DISP=SHR
            //SYSPRINT DD SYSOUT=*
            //         PEND
            //         EXEC DUMPCHK
            """
          )
        );
    }

    @ExpectedToFail("Requires preprocessing. Note: the comma exists in ''")
    @Test
    void referenceReplacement() {
        rewriteRun(
          jcl(
            """
            //SET1 SET VAL1=’ABC,’
            //S1 EXEC PGM=IEFBR14,PARM=&VAL1
            //    TIME=30
            """
          )
        );
    }

}
