/*
 * Copyright 2022 the original author or authors.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * https://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openrewrite.cobol.tree

import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.InMemoryExecutionContext
import org.openrewrite.PrintOutputCapture
import org.openrewrite.cobol.Assertions.cobolCopy
import org.openrewrite.cobol.CobolIbmAnsi85Parser
import org.openrewrite.cobol.CobolPreprocessorParser
import org.openrewrite.cobol.CobolPreprocessorVisitor
import org.openrewrite.cobol.internal.CobolPostPreprocessorPrinter
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import org.openrewrite.test.RewriteTest.toRecipe
import java.nio.file.Files
import java.nio.file.Path

class CobolPreprocessorCopyTest : RewriteTest {

    override fun defaults(spec: RecipeSpec) {
        spec.parser(CobolPreprocessorParser.builder().enableCopy())
            .recipe(toRecipe {
            object : CobolPreprocessorVisitor<ExecutionContext>() {
                override fun visitSpace(space: Space, p: ExecutionContext): Space {
                    val whitespace = space.whitespace.trim()
                    // TODO: separators should be isolated to a dialect.
                    if (!(whitespace.equals(",") || whitespace.equals(";") || whitespace.isEmpty())) {
                        return space.withWhitespace("(~~>${space.whitespace}<~~)")
                    }
                    return space
                }

                override fun visitCopyStatement(
                    copyStatement: CobolPreprocessor.CopyStatement,
                    p: ExecutionContext
                ): CobolPreprocessor {
                    val copyBook = copyStatement.copyBook!!

                    val output = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    val printer = CobolPostPreprocessorPrinter<ExecutionContext>()
                    printer.visit(copyBook, output)

                    val source = getSource(copyBook.sourcePath)
                    assertThat(source).isEqualTo(output.getOut())
                    return super.visitCopyStatement(copyStatement, p)
                }
            }
        }).parser(CobolIbmAnsi85Parser.builder())
    }

    private fun getSource(copyBook: Path): String {
        var source = ""
        try {
            Files.newInputStream(copyBook).use { inputStream ->
                val `is` = EncodingDetectingInputStream(inputStream)
                source = `is`.readFully()
            }
        } catch (e: Exception) {
            throw RuntimeException(e)
        }
        return source
    }

    @Test
    fun sm101A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM101A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM1014.2
            000200 PROGRAM-ID.                                                      SM1014.2
            000300     SM101A.                                                      SM1014.2
            000400****************************************************************  SM1014.2
            000500*                                                              *  SM1014.2
            000600*    VALIDATION FOR:-                                          *  SM1014.2
            000700*                                                              *  SM1014.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1014.2
            000900*                                                              *  SM1014.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1014.2
            001100*                                                              *  SM1014.2
            001200*                                                              *  SM1014.2
            001300*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM1014.2
            001400*                                                              *  SM1014.2
            001500*        X-55  - SYSTEM PRINTER NAME.                          *  SM1014.2
            001600*        X-82  - SOURCE COMPUTER NAME.                         *  SM1014.2
            001700*        X-83  - OBJECT COMPUTER NAME.                         *  SM1014.2
            001800*                                                              *  SM1014.2
            001900****************************************************************  SM1014.2
            002000*                                                              *  SM1014.2
            002100*    PROGRAM SM101A TESTS THE USE OF THE "COPY" STATEMENT      *  SM1014.2
            002200*    IN A FILE DESCRIPTION WITH ITS RELATED 01 ENTRIES IN THE  *  SM1014.2
            002300*    WORKING-STORAGE SECTION AND IN THE PROCEDURE DIVISION.    *  SM1014.2
            002400*    IT CREATES A SEQUENTIAL FILE WHICH IS INPUT TO SM102A TO  *  SM1014.2
            002500*    CHECK THE PROPER EXECUTION OF THE "COPY" STATEMENT IN     *  SM1014.2
            002600*    SM101A.  IT ALSO TESTS THE EFFECT OF A "COPY" STATEMENT   *  SM1014.2
            002700*    APPEARING ON A DEBUGGING LINE.                            *  SM1014.2
            002800*                                                              *  SM1014.2
            002900****************************************************************  SM1014.2
            003000 ENVIRONMENT DIVISION.                                            SM1014.2
            003100 CONFIGURATION SECTION.                                           SM1014.2
            003200 SOURCE-COMPUTER.                                                 SM1014.2
            003300     XXXXX082.                                                    SM1014.2
            003400 OBJECT-COMPUTER.                                                 SM1014.2
            003500     XXXXX083.                                                    SM1014.2
            003600 INPUT-OUTPUT SECTION.                                            SM1014.2
            003700 FILE-CONTROL.                                                    SM1014.2
            003800     SELECT PRINT-FILE ASSIGN TO                                  SM1014.2
            003900     XXXXX055.                                                    SM1014.2
            004000     SELECT TEST-FILE ASSIGN TO                                   SM1014.2
            004100     XXXXP001.                                                    SM1014.2
            004200 DATA DIVISION.                                                   SM1014.2
            004300 FILE SECTION.                                                    SM1014.2
            004400 FD  PRINT-FILE.                                                  SM1014.2
            004500 01  PRINT-REC PICTURE X(120).                                    SM1014.2
            004600 01  DUMMY-RECORD PICTURE X(120).                                 SM1014.2
            004700                                                                  SM1014.2
            004800                                                                  SM1014.2
            004900                                                                  SM1014.2
            005000                                                                  SM1014.2
            005100                                                                  SM1014.2
            005200*                                                                 SM1014.2
            005300*********************** COPY STATEMENT USED **********************SM1014.2
            005400*                                                                 SM1014.2
            005500*FD  TEST-FILE                                        COPY K1FDA. SM1014.2
            005600*                                                                 SM1014.2
            005700******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            005800 FD  TEST-FILE                                         COPY K1FDA.SM1014.2
            005900*********************** END OF COPIED TEXT ***********************SM1014.2
            006000                                                                  SM1014.2
            006100                                                                  SM1014.2
            006200                                                                  SM1014.2
            006300                                                                  SM1014.2
            006400                                                                  SM1014.2
            006500*                                                                 SM1014.2
            006600*********************** COPY STATEMENT USED **********************SM1014.2
            006700*                                                                 SM1014.2
            006800*01  TST-TEST                                         COPY K101A. SM1014.2
            006900*                                                                 SM1014.2
            007000******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            007100 01  TST-TEST                                          COPY K101A.SM1014.2
            007200*********************** END OF COPIED TEXT ***********************SM1014.2
            007300 WORKING-STORAGE SECTION.                                         SM1014.2
            007400*                                                                 SM1014.2
            007500*********************** COPY STATEMENT USED **********************SM1014.2
            007600*                                                                 SM1014.2
            007700*77  RCD-1                                            COPY K1W01. SM1014.2
            007800*                                                                 SM1014.2
            007900******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            008000 77  RCD-1                                             COPY K1W01.SM1014.2
            008100*********************** END OF COPIED TEXT ***********************SM1014.2
            008200 77  RCD-3 PICTURE 9(5) VALUE 10901.                              SM1014.2
            008300*                                                                 SM1014.2
            008400*********************** COPY STATEMENT USED **********************SM1014.2
            008500*                                                                 SM1014.2
            008600*77  COPY K1W02.                                                  SM1014.2
            008700*                                                                 SM1014.2
            008800******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            008900 77  COPY K1W02.                                                  SM1014.2
            009000*********************** END OF COPIED TEXT ***********************SM1014.2
            009100                       14003.                                     SM1014.2
            009200 77  RCD-6 PICTURE 9(5) VALUE 19922.                              SM1014.2
            009300*                                                                 SM1014.2
            009400*********************** COPY STATEMENT USED **********************SM1014.2
            009500*                                                                 SM1014.2
            009600*77  COPY K1W03.   VALUE 3543.                                    SM1014.2
            009700*                                                                 SM1014.2
            009800******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            009900 77  COPY K1W03.   VALUE 3543.                                    SM1014.2
            010000*********************** END OF COPIED TEXT ***********************SM1014.2
            010100 77  COPYSECT-1 PICTURE 9(5) VALUE 72459.                         SM1014.2
            010200 77  COPYSECT-2 PICTURE 9(5) VALUE 12132.                         SM1014.2
            010300 77  COPYSECT-3 PICTURE X(5) VALUE "TSTLI".                       SM1014.2
            010400 77  COPYSECT-4 PICTURE X(5) VALUE "BCOPY".                       SM1014.2
            010500*                                                                 SM1014.2
            010600*********************** COPY STATEMENT USED **********************SM1014.2
            010700*                                                                 SM1014.2
            010800*COPY  K1W04.                                                     SM1014.2
            010900*                                                                 SM1014.2
            011000******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            011100 COPY  K1W04.                                                     SM1014.2
            011200*********************** END OF COPIED TEXT ***********************SM1014.2
            011300 77  PROC-1 PICTURE 999 VALUE 123.                                SM1014.2
            011400 77  PROC-2 PICTURE 999 VALUE 456.                                SM1014.2
            011500 77  WSTR-1  PICTURE X(3) VALUE "ABC".                            SM1014.2
            011600                                                                  SM1014.2
            011700                                                                  SM1014.2
            011800                                                                  SM1014.2
            011900                                                                  SM1014.2
            012000                                                                  SM1014.2
            012100 01  WSTR-2.                                                      SM1014.2
            012200*                                                                 SM1014.2
            012300*********************** COPY STATEMENT USED **********************SM1014.2
            012400*                                                                 SM1014.2
            012500*                                            COPY K1WKA.          SM1014.2
            012600*                                                                 SM1014.2
            012700******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            012800                                             COPY K1WKA.          SM1014.2
            012900*********************** END OF COPIED TEXT ***********************SM1014.2
            013000 01  TEST-RESULTS.                                                SM1014.2
            013100     02 FILLER                   PIC X      VALUE SPACE.          SM1014.2
            013200     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM1014.2
            013300     02 FILLER                   PIC X      VALUE SPACE.          SM1014.2
            013400     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM1014.2
            013500     02 FILLER                   PIC X      VALUE SPACE.          SM1014.2
            013600     02  PAR-NAME.                                                SM1014.2
            013700       03 FILLER                 PIC X(19)  VALUE SPACE.          SM1014.2
            013800       03  PARDOT-X              PIC X      VALUE SPACE.          SM1014.2
            013900       03 DOTVALUE               PIC 99     VALUE ZERO.           SM1014.2
            014000     02 FILLER                   PIC X(8)   VALUE SPACE.          SM1014.2
            014100     02 RE-MARK                  PIC X(61).                       SM1014.2
            014200 01  TEST-COMPUTED.                                               SM1014.2
            014300     02 FILLER                   PIC X(30)  VALUE SPACE.          SM1014.2
            014400     02 FILLER                   PIC X(17)  VALUE                 SM1014.2
            014500            "       COMPUTED=".                                   SM1014.2
            014600     02 COMPUTED-X.                                               SM1014.2
            014700     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM1014.2
            014800     03 COMPUTED-N               REDEFINES COMPUTED-A             SM1014.2
            014900                                 PIC -9(9).9(9).                  SM1014.2
            015000     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM1014.2
            015100     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM1014.2
            015200     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM1014.2
            015300     03       CM-18V0 REDEFINES COMPUTED-A.                       SM1014.2
            015400         04 COMPUTED-18V0                    PIC -9(18).          SM1014.2
            015500         04 FILLER                           PIC X.               SM1014.2
            015600     03 FILLER PIC X(50) VALUE SPACE.                             SM1014.2
            015700 01  TEST-CORRECT.                                                SM1014.2
            015800     02 FILLER PIC X(30) VALUE SPACE.                             SM1014.2
            015900     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM1014.2
            016000     02 CORRECT-X.                                                SM1014.2
            016100     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM1014.2
            016200     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM1014.2
            016300     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM1014.2
            016400     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM1014.2
            016500     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM1014.2
            016600     03      CR-18V0 REDEFINES CORRECT-A.                         SM1014.2
            016700         04 CORRECT-18V0                     PIC -9(18).          SM1014.2
            016800         04 FILLER                           PIC X.               SM1014.2
            016900     03 FILLER PIC X(2) VALUE SPACE.                              SM1014.2
            017000     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM1014.2
            017100 01  CCVS-C-1.                                                    SM1014.2
            017200     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM1014.2
            017300-    "SS  PARAGRAPH-NAME                                          SM1014.2
            017400-    "       REMARKS".                                            SM1014.2
            017500     02 FILLER                     PIC X(20)    VALUE SPACE.      SM1014.2
            017600 01  CCVS-C-2.                                                    SM1014.2
            017700     02 FILLER                     PIC X        VALUE SPACE.      SM1014.2
            017800     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM1014.2
            017900     02 FILLER                     PIC X(15)    VALUE SPACE.      SM1014.2
            018000     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM1014.2
            018100     02 FILLER                     PIC X(94)    VALUE SPACE.      SM1014.2
            018200 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM1014.2
            018300 01  REC-CT                        PIC 99       VALUE ZERO.       SM1014.2
            018400 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM1014.2
            018500 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM1014.2
            018600 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM1014.2
            018700 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM1014.2
            018800 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM1014.2
            018900 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM1014.2
            019000 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM1014.2
            019100 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM1014.2
            019200 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM1014.2
            019300 01  CCVS-H-1.                                                    SM1014.2
            019400     02  FILLER                    PIC X(39)    VALUE SPACES.     SM1014.2
            019500     02  FILLER                    PIC X(42)    VALUE             SM1014.2
            019600     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM1014.2
            019700     02  FILLER                    PIC X(39)    VALUE SPACES.     SM1014.2
            019800 01  CCVS-H-2A.                                                   SM1014.2
            019900   02  FILLER                        PIC X(40)  VALUE SPACE.      SM1014.2
            020000   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM1014.2
            020100   02  FILLER                        PIC XXXX   VALUE             SM1014.2
            020200     "4.2 ".                                                      SM1014.2
            020300   02  FILLER                        PIC X(28)  VALUE             SM1014.2
            020400            " COPY - NOT FOR DISTRIBUTION".                       SM1014.2
            020500   02  FILLER                        PIC X(41)  VALUE SPACE.      SM1014.2
            020600                                                                  SM1014.2
            020700 01  CCVS-H-2B.                                                   SM1014.2
            020800   02  FILLER                        PIC X(15)  VALUE             SM1014.2
            020900            "TEST RESULT OF ".                                    SM1014.2
            021000   02  TEST-ID                       PIC X(9).                    SM1014.2
            021100   02  FILLER                        PIC X(4)   VALUE             SM1014.2
            021200            " IN ".                                               SM1014.2
            021300   02  FILLER                        PIC X(12)  VALUE             SM1014.2
            021400     " HIGH       ".                                              SM1014.2
            021500   02  FILLER                        PIC X(22)  VALUE             SM1014.2
            021600            " LEVEL VALIDATION FOR ".                             SM1014.2
            021700   02  FILLER                        PIC X(58)  VALUE             SM1014.2
            021800     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1014.2
            021900 01  CCVS-H-3.                                                    SM1014.2
            022000     02  FILLER                      PIC X(34)  VALUE             SM1014.2
            022100            " FOR OFFICIAL USE ONLY    ".                         SM1014.2
            022200     02  FILLER                      PIC X(58)  VALUE             SM1014.2
            022300     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1014.2
            022400     02  FILLER                      PIC X(28)  VALUE             SM1014.2
            022500            "  COPYRIGHT   1985 ".                                SM1014.2
            022600 01  CCVS-E-1.                                                    SM1014.2
            022700     02 FILLER                       PIC X(52)  VALUE SPACE.      SM1014.2
            022800     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM1014.2
            022900     02 ID-AGAIN                     PIC X(9).                    SM1014.2
            023000     02 FILLER                       PIC X(45)  VALUE SPACES.     SM1014.2
            023100 01  CCVS-E-2.                                                    SM1014.2
            023200     02  FILLER                      PIC X(31)  VALUE SPACE.      SM1014.2
            023300     02  FILLER                      PIC X(21)  VALUE SPACE.      SM1014.2
            023400     02 CCVS-E-2-2.                                               SM1014.2
            023500         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM1014.2
            023600         03 FILLER                   PIC X      VALUE SPACE.      SM1014.2
            023700         03 ENDER-DESC               PIC X(44)  VALUE             SM1014.2
            023800            "ERRORS ENCOUNTERED".                                 SM1014.2
            023900 01  CCVS-E-3.                                                    SM1014.2
            024000     02  FILLER                      PIC X(22)  VALUE             SM1014.2
            024100            " FOR OFFICIAL USE ONLY".                             SM1014.2
            024200     02  FILLER                      PIC X(12)  VALUE SPACE.      SM1014.2
            024300     02  FILLER                      PIC X(58)  VALUE             SM1014.2
            024400     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1014.2
            024500     02  FILLER                      PIC X(13)  VALUE SPACE.      SM1014.2
            024600     02 FILLER                       PIC X(15)  VALUE             SM1014.2
            024700             " COPYRIGHT 1985".                                   SM1014.2
            024800 01  CCVS-E-4.                                                    SM1014.2
            024900     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM1014.2
            025000     02 FILLER                       PIC X(4)   VALUE " OF ".     SM1014.2
            025100     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM1014.2
            025200     02 FILLER                       PIC X(40)  VALUE             SM1014.2
            025300      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM1014.2
            025400 01  XXINFO.                                                      SM1014.2
            025500     02 FILLER                       PIC X(19)  VALUE             SM1014.2
            025600            "*** INFORMATION ***".                                SM1014.2
            025700     02 INFO-TEXT.                                                SM1014.2
            025800       04 FILLER                     PIC X(8)   VALUE SPACE.      SM1014.2
            025900       04 XXCOMPUTED                 PIC X(20).                   SM1014.2
            026000       04 FILLER                     PIC X(5)   VALUE SPACE.      SM1014.2
            026100       04 XXCORRECT                  PIC X(20).                   SM1014.2
            026200     02 INF-ANSI-REFERENCE           PIC X(48).                   SM1014.2
            026300 01  HYPHEN-LINE.                                                 SM1014.2
            026400     02 FILLER  PIC IS X VALUE IS SPACE.                          SM1014.2
            026500     02 FILLER  PIC IS X(65)    VALUE IS "************************SM1014.2
            026600-    "*****************************************".                 SM1014.2
            026700     02 FILLER  PIC IS X(54)    VALUE IS "************************SM1014.2
            026800-    "******************************".                            SM1014.2
            026900 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM1014.2
            027000     "SM101A".                                                    SM1014.2
            027100 PROCEDURE DIVISION.                                              SM1014.2
            027200 CCVS1 SECTION.                                                   SM1014.2
            027300 OPEN-FILES.                                                      SM1014.2
            027400     OPEN     OUTPUT PRINT-FILE.                                  SM1014.2
            027500     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM1014.2
            027600     MOVE    SPACE TO TEST-RESULTS.                               SM1014.2
            027700     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM1014.2
            027800     GO TO CCVS1-EXIT.                                            SM1014.2
            027900 CLOSE-FILES.                                                     SM1014.2
            028000     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM1014.2
            028100 TERMINATE-CCVS.                                                  SM1014.2
            028200*    EXIT PROGRAM.                                                SM1014.2
            028300*TERMINATE-CALL.                                                  SM1014.2
            028400     STOP     RUN.                                                SM1014.2
            028500 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM1014.2
            028600 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM1014.2
            028700 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM1014.2
            028800 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM1014.2
            028900     MOVE "****TEST DELETED****" TO RE-MARK.                      SM1014.2
            029000 PRINT-DETAIL.                                                    SM1014.2
            029100     IF REC-CT NOT EQUAL TO ZERO                                  SM1014.2
            029200             MOVE "." TO PARDOT-X                                 SM1014.2
            029300             MOVE REC-CT TO DOTVALUE.                             SM1014.2
            029400     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM1014.2
            029500     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM1014.2
            029600        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM1014.2
            029700          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM1014.2
            029800     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM1014.2
            029900     MOVE SPACE TO CORRECT-X.                                     SM1014.2
            030000     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM1014.2
            030100     MOVE     SPACE TO RE-MARK.                                   SM1014.2
            030200 HEAD-ROUTINE.                                                    SM1014.2
            030300     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1014.2
            030400     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1014.2
            030500     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1014.2
            030600     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1014.2
            030700 COLUMN-NAMES-ROUTINE.                                            SM1014.2
            030800     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1014.2
            030900     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1014.2
            031000     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM1014.2
            031100 END-ROUTINE.                                                     SM1014.2
            031200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM1014.2
            031300 END-RTN-EXIT.                                                    SM1014.2
            031400     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1014.2
            031500 END-ROUTINE-1.                                                   SM1014.2
            031600      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM1014.2
            031700      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM1014.2
            031800      ADD PASS-COUNTER TO ERROR-HOLD.                             SM1014.2
            031900*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM1014.2
            032000      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM1014.2
            032100      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM1014.2
            032200      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM1014.2
            032300      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM1014.2
            032400  END-ROUTINE-12.                                                 SM1014.2
            032500      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM1014.2
            032600     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM1014.2
            032700         MOVE "NO " TO ERROR-TOTAL                                SM1014.2
            032800         ELSE                                                     SM1014.2
            032900         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM1014.2
            033000     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM1014.2
            033100     PERFORM WRITE-LINE.                                          SM1014.2
            033200 END-ROUTINE-13.                                                  SM1014.2
            033300     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM1014.2
            033400         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM1014.2
            033500         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM1014.2
            033600     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM1014.2
            033700     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1014.2
            033800      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM1014.2
            033900          MOVE "NO " TO ERROR-TOTAL                               SM1014.2
            034000      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM1014.2
            034100      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM1014.2
            034200      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM1014.2
            034300     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1014.2
            034400 WRITE-LINE.                                                      SM1014.2
            034500     ADD 1 TO RECORD-COUNT.                                       SM1014.2
            034600*    IF RECORD-COUNT GREATER 50                                   SM1014.2
            034700*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM1014.2
            034800*        MOVE SPACE TO DUMMY-RECORD                               SM1014.2
            034900*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM1014.2
            035000*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM1014.2
            035100*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM1014.2
            035200*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM1014.2
            035300*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM1014.2
            035400*        MOVE ZERO TO RECORD-COUNT.                               SM1014.2
            035500     PERFORM WRT-LN.                                              SM1014.2
            035600 WRT-LN.                                                          SM1014.2
            035700     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM1014.2
            035800     MOVE SPACE TO DUMMY-RECORD.                                  SM1014.2
            035900 BLANK-LINE-PRINT.                                                SM1014.2
            036000     PERFORM WRT-LN.                                              SM1014.2
            036100 FAIL-ROUTINE.                                                    SM1014.2
            036200     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM1014.2
            036300     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM1014.2
            036400     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1014.2
            036500     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM1014.2
            036600     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1014.2
            036700     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1014.2
            036800     GO TO  FAIL-ROUTINE-EX.                                      SM1014.2
            036900 FAIL-ROUTINE-WRITE.                                              SM1014.2
            037000     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM1014.2
            037100     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM1014.2
            037200     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM1014.2
            037300     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM1014.2
            037400 FAIL-ROUTINE-EX. EXIT.                                           SM1014.2
            037500 BAIL-OUT.                                                        SM1014.2
            037600     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM1014.2
            037700     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM1014.2
            037800 BAIL-OUT-WRITE.                                                  SM1014.2
            037900     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM1014.2
            038000     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1014.2
            038100     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1014.2
            038200     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1014.2
            038300 BAIL-OUT-EX. EXIT.                                               SM1014.2
            038400 CCVS1-EXIT.                                                      SM1014.2
            038500     EXIT.                                                        SM1014.2
            038600 INITIALIZATION SECTION.                                          SM1014.2
            038700 SM101A-INIT.                                                     SM1014.2
            038800     OPEN     OUTPUT TEST-FILE.                                   SM1014.2
            038900     MOVE     "OUTPUT OF SM101A IS USED AS" TO RE-MARK.           SM1014.2
            039000     PERFORM  PRINT-DETAIL.                                       SM1014.2
            039100     MOVE     "INPUT FOR SM102A."           TO RE-MARK.           SM1014.2
            039200     PERFORM  PRINT-DETAIL.                                       SM1014.2
            039300     MOVE     "COPY ---" TO FEATURE.                              SM1014.2
            039400     PERFORM  PRINT-DETAIL.                                       SM1014.2
            039500 WORKING-STORAGE-TEST SECTION.                                    SM1014.2
            039600 COPY-TEST-1.                                                     SM1014.2
            039700     IF       WSTR-1 EQUAL TO WSTR-2                              SM1014.2
            039800              PERFORM PASS GO TO COPY-WRITE-1.                    SM1014.2
            039900*        NOTE TESTS COPYING OF WORKING-STORAGE ENTRIES.           SM1014.2
            040000     GO       TO COPY-FAIL-1.                                     SM1014.2
            040100 COPY-DELETE-1.                                                   SM1014.2
            040200     PERFORM  DE-LETE.                                            SM1014.2
            040300     GO       TO COPY-WRITE-1.                                    SM1014.2
            040400 COPY-FAIL-1.                                                     SM1014.2
            040500     MOVE     WSTR-2 TO COMPUTED-A.                               SM1014.2
            040600     MOVE     "ABC" TO CORRECT-A                                  SM1014.2
            040700     PERFORM  FAIL.                                               SM1014.2
            040800 COPY-WRITE-1.                                                    SM1014.2
            040900     MOVE     "  WKNG-STORAGE ENTRY" TO FEATURE                   SM1014.2
            041000     MOVE     "COPY-TEST-1 " TO PAR-NAME.                         SM1014.2
            041100     PERFORM  PRINT-DETAIL.                                       SM1014.2
            041200 PARAGRAPH-TEST SECTION.                                          SM1014.2
            041300 COPY-TEST-2.                                                     SM1014.2
            041400                                                                  SM1014.2
            041500                                                                  SM1014.2
            041600                                                                  SM1014.2
            041700                                                                  SM1014.2
            041800                                                                  SM1014.2
            041900*                                                                 SM1014.2
            042000*********************** COPY STATEMENT USED **********************SM1014.2
            042100*                                                                 SM1014.2
            042200*                                                     COPY K1PRA. SM1014.2
            042300*                                                                 SM1014.2
            042400******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            042500                                                       COPY K1PRA.SM1014.2
            042600*********************** END OF COPIED TEXT ***********************SM1014.2
            042700 COPY-TESTT-2.                                                    SM1014.2
            042800     IF       PROC-1 EQUAL TO PROC-2                              SM1014.2
            042900              PERFORM PASS GO TO COPY-WRITE-2.                    SM1014.2
            043000*        NOTE TESTS COPYING OF A PROCEDURE DIVISION STATEMENT.    SM1014.2
            043100     GO       TO COPY-FAIL-2.                                     SM1014.2
            043200 COPY-DELETE-2.                                                   SM1014.2
            043300     PERFORM  DE-LETE.                                            SM1014.2
            043400     GO       TO COPY-WRITE-2.                                    SM1014.2
            043500 COPY-FAIL-2.                                                     SM1014.2
            043600     MOVE     PROC-2 TO COMPUTED-N.                               SM1014.2
            043700     MOVE     123 TO CORRECT-N.                                   SM1014.2
            043800     PERFORM  FAIL.                                               SM1014.2
            043900 COPY-WRITE-2.                                                    SM1014.2
            044000     MOVE     "  PROCEDURE" TO FEATURE                            SM1014.2
            044100     MOVE     "COPY-TEST-2 " TO PAR-NAME.                         SM1014.2
            044200     PERFORM  PRINT-DETAIL.                                       SM1014.2
            044300 SECTION-TEST SECTION.                                            SM1014.2
            044400                                                                  SM1014.2
            044500                                                                  SM1014.2
            044600                                                                  SM1014.2
            044700                                                                  SM1014.2
            044800                                                                  SM1014.2
            044900*                                                                 SM1014.2
            045000*********************** COPY STATEMENT USED **********************SM1014.2
            045100*                                                                 SM1014.2
            045200*                                                     COPY K1SEA. SM1014.2
            045300*                                                                 SM1014.2
            045400******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            045500                                                       COPY K1SEA.SM1014.2
            045600D                                                      COPY K1SEA.SM1014.2
            045700*********************** END OF COPIED TEXT ***********************SM1014.2
            045800 COPY-INIT-A.                                                     SM1014.2
            045900     MOVE     "  SECTION" TO FEATURE.                             SM1014.2
            046000 COPY-TEST-3.                                                     SM1014.2
            046100     IF       COPYSECT-1 EQUAL TO 95427                           SM1014.2
            046200              PERFORM PASS GO TO COPY-WRITE-3.                    SM1014.2
            046300*        NOTE COPY-TEST-3, 4, 5, 6 TEST THE COPYING OF AN         SM1014.2
            046400*             ENTIRE SECTION.                                     SM1014.2
            046500     GO       TO COPY-FAIL-3.                                     SM1014.2
            046600 COPY-DELETE-3.                                                   SM1014.2
            046700     PERFORM  DE-LETE.                                            SM1014.2
            046800     GO       TO COPY-WRITE-3.                                    SM1014.2
            046900 COPY-FAIL-3.                                                     SM1014.2
            047000     MOVE     COPYSECT-1 TO COMPUTED-N.                           SM1014.2
            047100     MOVE     95427   TO CORRECT-N.                               SM1014.2
            047200     PERFORM  FAIL.                                               SM1014.2
            047300 COPY-WRITE-3.                                                    SM1014.2
            047400     MOVE     "COPY-TEST-3 " TO PAR-NAME.                         SM1014.2
            047500     PERFORM  PRINT-DETAIL.                                       SM1014.2
            047600 COPY-TEST-4.                                                     SM1014.2
            047700     IF       COPYSECT-2 EQUAL TO 23121                           SM1014.2
            047800              PERFORM PASS GO TO COPY-WRITE-4.                    SM1014.2
            047900     GO       TO COPY-FAIL-4.                                     SM1014.2
            048000 COPY-DELETE-4.                                                   SM1014.2
            048100     PERFORM  DE-LETE.                                            SM1014.2
            048200     GO       TO COPY-WRITE-4.                                    SM1014.2
            048300 COPY-FAIL-4.                                                     SM1014.2
            048400     MOVE     COPYSECT-2 TO COMPUTED-N.                           SM1014.2
            048500     MOVE     23121   TO CORRECT-N.                               SM1014.2
            048600     PERFORM  FAIL.                                               SM1014.2
            048700 COPY-WRITE-4.                                                    SM1014.2
            048800     MOVE     "COPY-TEST-4 " TO PAR-NAME.                         SM1014.2
            048900     PERFORM  PRINT-DETAIL.                                       SM1014.2
            049000 COPY-TEST-5.                                                     SM1014.2
            049100     IF       COPYSECT-3 EQUAL TO "LIBCO"                         SM1014.2
            049200              PERFORM PASS GO TO COPY-WRITE-5.                    SM1014.2
            049300     GO       TO COPY-FAIL-5.                                     SM1014.2
            049400 COPY-DELETE-5.                                                   SM1014.2
            049500     PERFORM  DE-LETE.                                            SM1014.2
            049600     GO       TO COPY-WRITE-5.                                    SM1014.2
            049700 COPY-FAIL-5.                                                     SM1014.2
            049800     MOVE     COPYSECT-3 TO COMPUTED-A.                           SM1014.2
            049900     MOVE     "LIBCO" TO CORRECT-A.                               SM1014.2
            050000     PERFORM  FAIL.                                               SM1014.2
            050100 COPY-WRITE-5.                                                    SM1014.2
            050200     MOVE     "COPY-TEST-5 " TO PAR-NAME.                         SM1014.2
            050300     PERFORM  PRINT-DETAIL.                                       SM1014.2
            050400 COPY-TEST-6.                                                     SM1014.2
            050500     IF       COPYSECT-4 EQUAL TO "PYTST"                         SM1014.2
            050600              PERFORM PASS GO TO COPY-WRITE-6.                    SM1014.2
            050700     GO       TO COPY-FAIL-6.                                     SM1014.2
            050800 COPY-DELETE-6.                                                   SM1014.2
            050900     PERFORM  DE-LETE.                                            SM1014.2
            051000     GO       TO COPY-WRITE-6.                                    SM1014.2
            051100 COPY-FAIL-6.                                                     SM1014.2
            051200     MOVE     COPYSECT-4 TO COMPUTED-A.                           SM1014.2
            051300     MOVE     "PYTST" TO CORRECT-A.                               SM1014.2
            051400     PERFORM  FAIL.                                               SM1014.2
            051500 COPY-WRITE-6.                                                    SM1014.2
            051600     MOVE     "COPY-TEST-6 " TO PAR-NAME.                         SM1014.2
            051700     PERFORM  PRINT-DETAIL.                                       SM1014.2
            051800 BUILD SECTION.                                                   SM1014.2
            051900 COPY-TEST-7.                                                     SM1014.2
            052000     MOVE     RCD-1 TO TST-FLD-1.                                 SM1014.2
            052100     WRITE    TST-TEST.                                           SM1014.2
            052200     MOVE     RCD-2 TO TST-FLD-1.                                 SM1014.2
            052300     WRITE    TST-TEST.                                           SM1014.2
            052400     MOVE     RCD-3 TO TST-FLD-1.                                 SM1014.2
            052500     WRITE    TST-TEST.                                           SM1014.2
            052600     MOVE     RCD-4 TO TST-FLD-1.                                 SM1014.2
            052700     WRITE    TST-TEST.                                           SM1014.2
            052800     MOVE     RCD-5 TO TST-FLD-1.                                 SM1014.2
            052900     WRITE    TST-TEST.                                           SM1014.2
            053000     MOVE     RCD-6 TO TST-FLD-1.                                 SM1014.2
            053100     WRITE    TST-TEST.                                           SM1014.2
            053200     MOVE     RCD-7 TO TST-FLD-1.                                 SM1014.2
            053300     WRITE    TST-TEST.                                           SM1014.2
            053400     PERFORM  PASS.                                               SM1014.2
            053500     GO       TO COPY-WRITE-7.                                    SM1014.2
            053600 COPY-DELETE-7.                                                   SM1014.2
            053700     PERFORM  DE-LETE.                                            SM1014.2
            053800 COPY-WRITE-7.                                                    SM1014.2
            053900     MOVE     "  FILE DESCRIPTION" TO FEATURE.                    SM1014.2
            054000     MOVE     "COPY-TEST-7" TO PAR-NAME.                          SM1014.2
            054100     MOVE     "OUTPUT CHECKED IN SM102A" TO RE-MARK.              SM1014.2
            054200     PERFORM  PRINT-DETAIL.                                       SM1014.2
            054300 COPY-TEST-8.                                                     SM1014.2
            054400*                                                                 SM1014.2
            054500*********************** COPY STATEMENT USED **********************SM1014.2
            054600*                                                                 SM1014.2
            054700*    ADD     COPY K1P01. TO WRK-DS-05V00.                         SM1014.2
            054800*                                                                 SM1014.2
            054900******************** COPIED TEXT BEGINS BELOW ********************SM1014.2
            055000     ADD     COPY K1P01. TO WRK-DS-05V00.                         SM1014.2
            055100*********************** END OF COPIED TEXT ***********************SM1014.2
            055200     IF       WRK-DS-05V00 EQUAL TO 97523                         SM1014.2
            055300             PERFORM PASS                                         SM1014.2
            055400             GO TO COPY-WRITE-8.                                  SM1014.2
            055500     GO TO    COPY-FAIL-8.                                        SM1014.2
            055600 COPY-DELETE-8.                                                   SM1014.2
            055700     PERFORM DE-LETE.                                             SM1014.2
            055800     GO TO    COPY-WRITE-8.                                       SM1014.2
            055900 COPY-FAIL-8.                                                     SM1014.2
            056000     MOVE    WRK-DS-05V00 TO COMPUTED-N.                          SM1014.2
            056100     MOVE    97523        TO CORRECT-N.                           SM1014.2
            056200     PERFORM FAIL.                                                SM1014.2
            056300 COPY-WRITE-8.                                                    SM1014.2
            056400     MOVE     "COPY-TEST-8" TO PAR-NAME.                          SM1014.2
            056500     PERFORM PRINT-DETAIL.                                        SM1014.2
            056600     CLOSE    TEST-FILE.                                          SM1014.2
            056700 CCVS-EXIT SECTION.                                               SM1014.2
            056800 CCVS-999999.                                                     SM1014.2
            056900     GO TO CLOSE-FILES.                                           SM1014.2
                  *END-OF,SM101A                                                                  
        """)
    )

    @Test
    fun sm103A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM103A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM1034.2
            000200 PROGRAM-ID.                                                      SM1034.2
            000300     SM103A.                                                      SM1034.2
            000400****************************************************************  SM1034.2
            000500*                                                              *  SM1034.2
            000600*    VALIDATION FOR:-                                          *  SM1034.2
            000700*                                                              *  SM1034.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1034.2
            000900*                                                              *  SM1034.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1034.2
            001100*                                                              *  SM1034.2
            001200****************************************************************  SM1034.2
            001300*                                                              *  SM1034.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM1034.2
            001500*                                                              *  SM1034.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM1034.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM1034.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM1034.2
            001900*                                                              *  SM1034.2
            002000****************************************************************  SM1034.2
            002100*                                                              *  SM1034.2
            002200*    PROGRAM SM103A TESTS THE USE OF THE "COPY" STATEMENT IN   *  SM1034.2
            002300*    THE IDENTIFICATION DIVISION AND ENVIRONMENT DIVISION      *  SM1034.2
            002400*    (SOURCE-COMPUTER, OBJECT-COMPUTER, SPECIAL-NAMES,         *  SM1034.2
            002500*    FILE-CONTROL AND I-O-CONTROL ENTRIES).                    *  SM1034.2
            002600*    A SEQUENTIAL FILE IS PRODUCED WHICH IS READ AND CHECKED   *  SM1034.2
            002700*    BY SM104A.                                                *  SM1034.2
            002800*    THE MAXIMUM AND MINIMUM LENGTHS OF A LIBRARY TEXT WORD    *  SM1034.2
            002900*    ARE ALSO TESTED.                                          *  SM1034.2
            003000*                                                              *  SM1034.2
            003100****************************************************************  SM1034.2
            003200 SECURITY.                                                        SM1034.2
            003300     COPY K3SNA.                                                  SM1034.2
            003400 ENVIRONMENT DIVISION.                                            SM1034.2
            003500 CONFIGURATION SECTION.                                           SM1034.2
            003600                                                                  SM1034.2
            003700                                                                  SM1034.2
            003800                                                                  SM1034.2
            003900                                                                  SM1034.2
            004000                                                                  SM1034.2
            004100*                                                                 SM1034.2
            004200*********************** COPY STATEMENT USED **********************SM1034.2
            004300*                                                                 SM1034.2
            004400*SOURCE-COMPUTER.                                     COPY K3SCA  SM1034.2
            004500*                                                                 SM1034.2
            004600******************** COPIED TEXT BEGINS BELOW ********************SM1034.2
            004700 SOURCE-COMPUTER.                                      COPY K3SCA.SM1034.2
            004800*********************** END OF COPIED TEXT ***********************SM1034.2
            004900                                                                  SM1034.2
            005000                                                                  SM1034.2
            005100                                                                  SM1034.2
            005200                                                                  SM1034.2
            005300                                                                  SM1034.2
            005400*                                                                 SM1034.2
            005500*********************** COPY STATEMENT USED **********************SM1034.2
            005600*                                                                 SM1034.2
            005700*OBJECT-COMPUTER.                                     COPY K3OCA  SM1034.2
            005800*                                                                 SM1034.2
            005900******************** COPIED TEXT BEGINS BELOW ********************SM1034.2
            006000 OBJECT-COMPUTER.                                      COPY K3OCA.SM1034.2
            006100*********************** END OF COPIED TEXT ***********************SM1034.2
            006200                                                                  SM1034.2
            006300                                                                  SM1034.2
            006400                                                                  SM1034.2
            006500                                                                  SM1034.2
            006600                                                                  SM1034.2
            006700*                                                                 SM1034.2
            006800*********************** COPY STATEMENT USED **********************SM1034.2
            006900*                                                                 SM1034.2
            007000*SPECIAL-NAMES.                                       COPY K3SNA. SM1034.2
            007100*                                                                 SM1034.2
            007200******************** COPIED TEXT BEGINS BELOW ********************SM1034.2
            007300 SPECIAL-NAMES.                                        COPY K3SNA.SM1034.2
            007400*********************** END OF COPIED TEXT ***********************SM1034.2
            007500 INPUT-OUTPUT SECTION.                                            SM1034.2
            007600                                                                  SM1034.2
            007700                                                                  SM1034.2
            007800                                                                  SM1034.2
            007900                                                                  SM1034.2
            008000                                                                  SM1034.2
            008100*                                                                 SM1034.2
            008200*********************** COPY STATEMENT USED **********************SM1034.2
            008300*                                                                 SM1034.2
            008400*FILE-CONTROL.                                        COPY K3FCA. SM1034.2
            008500*                                                                 SM1034.2
            008600******************** COPIED TEXT BEGINS BELOW ********************SM1034.2
            008700 FILE-CONTROL.                                         COPY K3FCA.SM1034.2
            008800*********************** END OF COPIED TEXT ***********************SM1034.2
            008900                                                                  SM1034.2
            009000                                                                  SM1034.2
            009100                                                                  SM1034.2
            009200                                                                  SM1034.2
            009300                                                                  SM1034.2
            009400*                                                                 SM1034.2
            009500*********************** COPY STATEMENT USED **********************SM1034.2
            009600*                                                                 SM1034.2
            009700*I-O-CONTROL                                          COPY K3IOA  SM1034.2
            009800*                                                                 SM1034.2
            009900******************** COPIED TEXT BEGINS BELOW ********************SM1034.2
            010000 I-O-CONTROL.                                          COPY K3IOA.SM1034.2
            010100*********************** END OF COPIED TEXT ***********************SM1034.2
            010200 DATA DIVISION.                                                   SM1034.2
            010300 FILE SECTION.                                                    SM1034.2
            010400 FD  PRINT-FILE.                                                  SM1034.2
            010500 01  PRINT-REC PICTURE X(120).                                    SM1034.2
            010600 01  DUMMY-RECORD PICTURE X(120).                                 SM1034.2
            010700 FD  TEST-FILE                                                    SM1034.2
            010800     LABEL RECORD STANDARD                                        SM1034.2
            010900C    VALUE OF                                                     SM1034.2
            011000C    XXXXX074                                                     SM1034.2
            011100C    IS                                                           SM1034.2
            011200C    XXXXX075                                                     SM1034.2
            011300*    XXXXX069                                                     SM1034.2
            011400     DATA RECORD TEST-REC.                                        SM1034.2
            011500 01  TEST-REC.                                                    SM1034.2
            011600     02  TST-FLD-1 PICTURE 9(5).                                  SM1034.2
            011700     02  TST-FLD-2 PICTURE X(13).                                 SM1034.2
            011800     02  FILLER PICTURE X(102).                                   SM1034.2
            011900 FD  TEST-FILE2                                                   SM1034.2
            012000     LABEL RECORD STANDARD                                        SM1034.2
            012100C    VALUE OF                                                     SM1034.2
            012200C    XXXXX074                                                     SM1034.2
            012300C    IS                                                           SM1034.2
            012400C    XXXXX076                                                     SM1034.2
            012500*    XXXXX069                                                     SM1034.2
            012600     DATA RECORD TEST-REC2.                                       SM1034.2
            012700 01  TEST-REC2.                                                   SM1034.2
            012800     02  TST-FLD-3 PICTURE 9(5).                                  SM1034.2
            012900     02  TST-FLD-4 PICTURE X(13).                                 SM1034.2
            013000     02  FILLER PICTURE X(102).                                   SM1034.2
            013100 WORKING-STORAGE SECTION.                                         SM1034.2
            013200 77  RCD-1 PICTURE 9(5) VALUE 97532.                              SM1034.2
            013300 77  RCD-2 PICTURE 9(5) VALUE 23479.                              SM1034.2
            013400 77  RCD-3 PICTURE 9(5) VALUE 10901.                              SM1034.2
            013500 77  RCD-4 PICTURE 9(5) VALUE 02734.                              SM1034.2
            013600 77  RCD-5 PICTURE 9(5) VALUE 14003.                              SM1034.2
            013700 77  RCD-6 PICTURE 9(5) VALUE 19922.                              SM1034.2
            013800 77  RCD-7 PICTURE 9(5) VALUE 03543.                              SM1034.2
            013900 01  S-N-1 PICTURE 9(8)V99 VALUE IS 12345678,91.                  SM1034.2
            014000 01  S-N-2 PICTURE ZZ.ZZZ.ZZZ,99.                                 SM1034.2
            014100 01  WRK-DU-9                    PIC 9          VALUE ZERO.       SM1034.2
            014200 01  WRK-DU-99                   PIC 99         VALUE ZERO.       SM1034.2
            014300 01  WRK-DU-99-LONGER            PIC 99         VALUE ZERO.       SM1034.2
            014400 01  WRK-DU-00001                PIC 9.                           SM1034.2
            014500 01  WRK-XN-00322                PIC X(322).                      SM1034.2
            014600 01  FILLER REDEFINES WRK-XN-00322.                               SM1034.2
            014700   03  WRK-XN-00322-1         PIC X.                              SM1034.2
            014800   03  WRK-XN-00322-2-322.                                        SM1034.2
            014900     05  WRK-XN-00322-2-3     PIC X.                              SM1034.2
            015000     05  WRK-XN-00322-20      PIC X(20)                           SM1034.2
            015100                              OCCURS 16                           SM1034.2
            015200                              INDEXED BY X1.                      SM1034.2
            015300 01  TEST-RESULTS.                                                SM1034.2
            015400     02 FILLER                   PIC X      VALUE SPACE.          SM1034.2
            015500     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM1034.2
            015600     02 FILLER                   PIC X      VALUE SPACE.          SM1034.2
            015700     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM1034.2
            015800     02 FILLER                   PIC X      VALUE SPACE.          SM1034.2
            015900     02  PAR-NAME.                                                SM1034.2
            016000       03 FILLER                 PIC X(19)  VALUE SPACE.          SM1034.2
            016100       03  PARDOT-X              PIC X      VALUE SPACE.          SM1034.2
            016200       03 DOTVALUE               PIC 99     VALUE ZERO.           SM1034.2
            016300     02 FILLER                   PIC X(8)   VALUE SPACE.          SM1034.2
            016400     02 RE-MARK                  PIC X(61).                       SM1034.2
            016500 01  TEST-COMPUTED.                                               SM1034.2
            016600     02 FILLER                   PIC X(30)  VALUE SPACE.          SM1034.2
            016700     02 FILLER                   PIC X(17)  VALUE                 SM1034.2
            016800            "       COMPUTED=".                                   SM1034.2
            016900     02 COMPUTED-X.                                               SM1034.2
            017000     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM1034.2
            017100     03 COMPUTED-N               REDEFINES COMPUTED-A             SM1034.2
            017200                                 PIC -9(9).9(9).                  SM1034.2
            017300     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM1034.2
            017400     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM1034.2
            017500     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM1034.2
            017600     03       CM-18V0 REDEFINES COMPUTED-A.                       SM1034.2
            017700         04 COMPUTED-18V0                    PIC -9(18).          SM1034.2
            017800         04 FILLER                           PIC X.               SM1034.2
            017900     03 FILLER PIC X(50) VALUE SPACE.                             SM1034.2
            018000 01  TEST-CORRECT.                                                SM1034.2
            018100     02 FILLER PIC X(30) VALUE SPACE.                             SM1034.2
            018200     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM1034.2
            018300     02 CORRECT-X.                                                SM1034.2
            018400     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM1034.2
            018500     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM1034.2
            018600     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM1034.2
            018700     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM1034.2
            018800     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM1034.2
            018900     03      CR-18V0 REDEFINES CORRECT-A.                         SM1034.2
            019000         04 CORRECT-18V0                     PIC -9(18).          SM1034.2
            019100         04 FILLER                           PIC X.               SM1034.2
            019200     03 FILLER PIC X(2) VALUE SPACE.                              SM1034.2
            019300     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM1034.2
            019400 01  CCVS-C-1.                                                    SM1034.2
            019500     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM1034.2
            019600-    "SS  PARAGRAPH-NAME                                          SM1034.2
            019700-    "       REMARKS".                                            SM1034.2
            019800     02 FILLER                     PIC X(20)    VALUE SPACE.      SM1034.2
            019900 01  CCVS-C-2.                                                    SM1034.2
            020000     02 FILLER                     PIC X        VALUE SPACE.      SM1034.2
            020100     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM1034.2
            020200     02 FILLER                     PIC X(15)    VALUE SPACE.      SM1034.2
            020300     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM1034.2
            020400     02 FILLER                     PIC X(94)    VALUE SPACE.      SM1034.2
            020500 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM1034.2
            020600 01  REC-CT                        PIC 99       VALUE ZERO.       SM1034.2
            020700 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM1034.2
            020800 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM1034.2
            020900 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM1034.2
            021000 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM1034.2
            021100 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM1034.2
            021200 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM1034.2
            021300 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM1034.2
            021400 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM1034.2
            021500 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM1034.2
            021600 01  CCVS-H-1.                                                    SM1034.2
            021700     02  FILLER                    PIC X(39)    VALUE SPACES.     SM1034.2
            021800     02  FILLER                    PIC X(42)    VALUE             SM1034.2
            021900     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM1034.2
            022000     02  FILLER                    PIC X(39)    VALUE SPACES.     SM1034.2
            022100 01  CCVS-H-2A.                                                   SM1034.2
            022200   02  FILLER                        PIC X(40)  VALUE SPACE.      SM1034.2
            022300   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM1034.2
            022400   02  FILLER                        PIC XXXX   VALUE             SM1034.2
            022500     "4.2 ".                                                      SM1034.2
            022600   02  FILLER                        PIC X(28)  VALUE             SM1034.2
            022700            " COPY - NOT FOR DISTRIBUTION".                       SM1034.2
            022800   02  FILLER                        PIC X(41)  VALUE SPACE.      SM1034.2
            022900                                                                  SM1034.2
            023000 01  CCVS-H-2B.                                                   SM1034.2
            023100   02  FILLER                        PIC X(15)  VALUE             SM1034.2
            023200            "TEST RESULT OF ".                                    SM1034.2
            023300   02  TEST-ID                       PIC X(9).                    SM1034.2
            023400   02  FILLER                        PIC X(4)   VALUE             SM1034.2
            023500            " IN ".                                               SM1034.2
            023600   02  FILLER                        PIC X(12)  VALUE             SM1034.2
            023700     " HIGH       ".                                              SM1034.2
            023800   02  FILLER                        PIC X(22)  VALUE             SM1034.2
            023900            " LEVEL VALIDATION FOR ".                             SM1034.2
            024000   02  FILLER                        PIC X(58)  VALUE             SM1034.2
            024100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1034.2
            024200 01  CCVS-H-3.                                                    SM1034.2
            024300     02  FILLER                      PIC X(34)  VALUE             SM1034.2
            024400            " FOR OFFICIAL USE ONLY    ".                         SM1034.2
            024500     02  FILLER                      PIC X(58)  VALUE             SM1034.2
            024600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1034.2
            024700     02  FILLER                      PIC X(28)  VALUE             SM1034.2
            024800            "  COPYRIGHT   1985 ".                                SM1034.2
            024900 01  CCVS-E-1.                                                    SM1034.2
            025000     02 FILLER                       PIC X(52)  VALUE SPACE.      SM1034.2
            025100     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM1034.2
            025200     02 ID-AGAIN                     PIC X(9).                    SM1034.2
            025300     02 FILLER                       PIC X(45)  VALUE SPACES.     SM1034.2
            025400 01  CCVS-E-2.                                                    SM1034.2
            025500     02  FILLER                      PIC X(31)  VALUE SPACE.      SM1034.2
            025600     02  FILLER                      PIC X(21)  VALUE SPACE.      SM1034.2
            025700     02 CCVS-E-2-2.                                               SM1034.2
            025800         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM1034.2
            025900         03 FILLER                   PIC X      VALUE SPACE.      SM1034.2
            026000         03 ENDER-DESC               PIC X(44)  VALUE             SM1034.2
            026100            "ERRORS ENCOUNTERED".                                 SM1034.2
            026200 01  CCVS-E-3.                                                    SM1034.2
            026300     02  FILLER                      PIC X(22)  VALUE             SM1034.2
            026400            " FOR OFFICIAL USE ONLY".                             SM1034.2
            026500     02  FILLER                      PIC X(12)  VALUE SPACE.      SM1034.2
            026600     02  FILLER                      PIC X(58)  VALUE             SM1034.2
            026700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1034.2
            026800     02  FILLER                      PIC X(13)  VALUE SPACE.      SM1034.2
            026900     02 FILLER                       PIC X(15)  VALUE             SM1034.2
            027000             " COPYRIGHT 1985".                                   SM1034.2
            027100 01  CCVS-E-4.                                                    SM1034.2
            027200     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM1034.2
            027300     02 FILLER                       PIC X(4)   VALUE " OF ".     SM1034.2
            027400     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM1034.2
            027500     02 FILLER                       PIC X(40)  VALUE             SM1034.2
            027600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM1034.2
            027700 01  XXINFO.                                                      SM1034.2
            027800     02 FILLER                       PIC X(19)  VALUE             SM1034.2
            027900            "*** INFORMATION ***".                                SM1034.2
            028000     02 INFO-TEXT.                                                SM1034.2
            028100       04 FILLER                     PIC X(8)   VALUE SPACE.      SM1034.2
            028200       04 XXCOMPUTED                 PIC X(20).                   SM1034.2
            028300       04 FILLER                     PIC X(5)   VALUE SPACE.      SM1034.2
            028400       04 XXCORRECT                  PIC X(20).                   SM1034.2
            028500     02 INF-ANSI-REFERENCE           PIC X(48).                   SM1034.2
            028600 01  HYPHEN-LINE.                                                 SM1034.2
            028700     02 FILLER  PIC IS X VALUE IS SPACE.                          SM1034.2
            028800     02 FILLER  PIC IS X(65)    VALUE IS "************************SM1034.2
            028900-    "*****************************************".                 SM1034.2
            029000     02 FILLER  PIC IS X(54)    VALUE IS "************************SM1034.2
            029100-    "******************************".                            SM1034.2
            029200 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM1034.2
            029300     "SM103A".                                                    SM1034.2
            029400 PROCEDURE DIVISION.                                              SM1034.2
            029500 CCVS1 SECTION.                                                   SM1034.2
            029600 OPEN-FILES.                                                      SM1034.2
            029700     OPEN     OUTPUT PRINT-FILE.                                  SM1034.2
            029800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM1034.2
            029900     MOVE    SPACE TO TEST-RESULTS.                               SM1034.2
            030000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM1034.2
            030100     GO TO CCVS1-EXIT.                                            SM1034.2
            030200 CLOSE-FILES.                                                     SM1034.2
            030300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM1034.2
            030400 TERMINATE-CCVS.                                                  SM1034.2
            030500*    EXIT PROGRAM.                                                SM1034.2
            030600*TERMINATE-CALL.                                                  SM1034.2
            030700     STOP     RUN.                                                SM1034.2
            030800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM1034.2
            030900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM1034.2
            031000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM1034.2
            031100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM1034.2
            031200     MOVE "****TEST DELETED****" TO RE-MARK.                      SM1034.2
            031300 PRINT-DETAIL.                                                    SM1034.2
            031400     IF REC-CT NOT EQUAL TO ZERO                                  SM1034.2
            031500             MOVE "." TO PARDOT-X                                 SM1034.2
            031600             MOVE REC-CT TO DOTVALUE.                             SM1034.2
            031700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM1034.2
            031800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM1034.2
            031900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM1034.2
            032000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM1034.2
            032100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM1034.2
            032200     MOVE SPACE TO CORRECT-X.                                     SM1034.2
            032300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM1034.2
            032400     MOVE     SPACE TO RE-MARK.                                   SM1034.2
            032500 HEAD-ROUTINE.                                                    SM1034.2
            032600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1034.2
            032700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1034.2
            032800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1034.2
            032900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1034.2
            033000 COLUMN-NAMES-ROUTINE.                                            SM1034.2
            033100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
            033200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
            033300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM1034.2
            033400 END-ROUTINE.                                                     SM1034.2
            033500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM1034.2
            033600 END-RTN-EXIT.                                                    SM1034.2
            033700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
            033800 END-ROUTINE-1.                                                   SM1034.2
            033900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM1034.2
            034000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM1034.2
            034100      ADD PASS-COUNTER TO ERROR-HOLD.                             SM1034.2
            034200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM1034.2
            034300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM1034.2
            034400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM1034.2
            034500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM1034.2
            034600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM1034.2
            034700  END-ROUTINE-12.                                                 SM1034.2
            034800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM1034.2
            034900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM1034.2
            035000         MOVE "NO " TO ERROR-TOTAL                                SM1034.2
            035100         ELSE                                                     SM1034.2
            035200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM1034.2
            035300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM1034.2
            035400     PERFORM WRITE-LINE.                                          SM1034.2
            035500 END-ROUTINE-13.                                                  SM1034.2
            035600     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM1034.2
            035700         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM1034.2
            035800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM1034.2
            035900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM1034.2
            036000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
            036100      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM1034.2
            036200          MOVE "NO " TO ERROR-TOTAL                               SM1034.2
            036300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM1034.2
            036400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM1034.2
            036500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM1034.2
            036600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
            036700 WRITE-LINE.                                                      SM1034.2
            036800     ADD 1 TO RECORD-COUNT.                                       SM1034.2
            036900*    IF RECORD-COUNT GREATER 50                                   SM1034.2
            037000*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM1034.2
            037100*        MOVE SPACE TO DUMMY-RECORD                               SM1034.2
            037200*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM1034.2
            037300*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM1034.2
            037400*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM1034.2
            037500*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM1034.2
            037600*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM1034.2
            037700*        MOVE ZERO TO RECORD-COUNT.                               SM1034.2
            037800     PERFORM WRT-LN.                                              SM1034.2
            037900 WRT-LN.                                                          SM1034.2
            038000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM1034.2
            038100     MOVE SPACE TO DUMMY-RECORD.                                  SM1034.2
            038200 BLANK-LINE-PRINT.                                                SM1034.2
            038300     PERFORM WRT-LN.                                              SM1034.2
            038400 FAIL-ROUTINE.                                                    SM1034.2
            038500     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM1034.2
            038600     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM1034.2
            038700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1034.2
            038800     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM1034.2
            038900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
            039000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1034.2
            039100     GO TO  FAIL-ROUTINE-EX.                                      SM1034.2
            039200 FAIL-ROUTINE-WRITE.                                              SM1034.2
            039300     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM1034.2
            039400     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM1034.2
            039500     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM1034.2
            039600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM1034.2
            039700 FAIL-ROUTINE-EX. EXIT.                                           SM1034.2
            039800 BAIL-OUT.                                                        SM1034.2
            039900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM1034.2
            040000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM1034.2
            040100 BAIL-OUT-WRITE.                                                  SM1034.2
            040200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM1034.2
            040300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1034.2
            040400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
            040500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1034.2
            040600 BAIL-OUT-EX. EXIT.                                               SM1034.2
            040700 CCVS1-EXIT.                                                      SM1034.2
            040800     EXIT.                                                        SM1034.2
            040900 INITIALIZATION SECTION.                                          SM1034.2
            041000 SM103A-INIT.                                                     SM1034.2
            041100     MOVE     "ALL TESTS IN SM103A PRODUCE" TO RE-MARK.           SM1034.2
            041200     PERFORM  PRINT-DETAIL.                                       SM1034.2
            041300     MOVE     "OUTPUT CHECKED IN SM104A."   TO RE-MARK.           SM1034.2
            041400     PERFORM  PRINT-DETAIL.                                       SM1034.2
            041500     MOVE     "COPY ---" TO FEATURE.                              SM1034.2
            041600     PERFORM  PRINT-DETAIL.                                       SM1034.2
            041700 SPECIAL-NAMES-TEST SECTION.                                      SM1034.2
            041800 COPY-TEST-1.                                                     SM1034.2
            041900     MOVE     S-N-1 TO S-N-2.                                     SM1034.2
            042000*    NOTE     THIS ROUTINE USES A COPIED DECIMAL-POINT IS COMMA   SM1034.2
            042100*             CLAUSE IN SPECIAL-NAMES --- THE EDITING IN S-N-2    SM1034.2
            042200*             WOULD NOT BE VALID WITHOUT THIS CLAUSE.             SM1034.2
            042300     PERFORM  PASS.                                               SM1034.2
            042400     GO       TO COPY-WRITE-1.                                    SM1034.2
            042500 COPY-DELETE-1.                                                   SM1034.2
            042600     PERFORM  DE-LETE.                                            SM1034.2
            042700 COPY-WRITE-1.                                                    SM1034.2
            042800     MOVE     "  DEC POINT IS COMMA" TO FEATURE.                  SM1034.2
            042900     MOVE     "COPY-TEST-1 " TO PAR-NAME.                         SM1034.2
            043000     PERFORM  PRINT-DETAIL.                                       SM1034.2
            043100 BUILD SECTION.                                                   SM1034.2
            043200 COPY-TEST-2.                                                     SM1034.2
            043300     OPEN     OUTPUT TEST-FILE.                                   SM1034.2
            043400     MOVE     S-N-2 TO TST-FLD-2.                                 SM1034.2
            043500     MOVE     RCD-1 TO TST-FLD-1.                                 SM1034.2
            043600     WRITE    TEST-REC.                                           SM1034.2
            043700     MOVE     RCD-2 TO TST-FLD-1.                                 SM1034.2
            043800     WRITE    TEST-REC.                                           SM1034.2
            043900     MOVE     RCD-3 TO TST-FLD-1.                                 SM1034.2
            044000     WRITE    TEST-REC.                                           SM1034.2
            044100     MOVE     RCD-4 TO TST-FLD-1.                                 SM1034.2
            044200     WRITE    TEST-REC.                                           SM1034.2
            044300     MOVE     RCD-5 TO TST-FLD-1.                                 SM1034.2
            044400     WRITE    TEST-REC.                                           SM1034.2
            044500     MOVE     RCD-6 TO TST-FLD-1.                                 SM1034.2
            044600     WRITE    TEST-REC.                                           SM1034.2
            044700     MOVE     RCD-7 TO TST-FLD-1.                                 SM1034.2
            044800     WRITE    TEST-REC.                                           SM1034.2
            044900     CLOSE    TEST-FILE.                                          SM1034.2
            045000     OPEN OUTPUT TEST-FILE2.                                      SM1034.2
            045100     MOVE ZERO TO TST-FLD-3.                                      SM1034.2
            045200     MOVE "DDDDD" TO TST-FLD-4.                                   SM1034.2
            045300     WRITE TEST-REC2.                                             SM1034.2
            045400     CLOSE TEST-FILE2.                                            SM1034.2
            045500     PERFORM  PASS.                                               SM1034.2
            045600     GO       TO COPY-WRITE-2.                                    SM1034.2
            045700 COPY-DELETE-2.                                                   SM1034.2
            045800     PERFORM DE-LETE.                                             SM1034.2
            045900 COPY-WRITE-2.                                                    SM1034.2
            046000     MOVE   "  ENVIR DIV ENTRIES" TO FEATURE.                     SM1034.2
            046100     MOVE   "COPY-TEST-2 " TO PAR-NAME.                           SM1034.2
            046200     PERFORM PRINT-DETAIL.                                        SM1034.2
            046300*                                                                 SM1034.2
            046400 COPY-TEST-3.                                                     SM1034.2
            046500*    ===-->  MINIMUM LENGTH TEXT WORD  <--===                     SM1034.2
            046600     MOVE   "XII-2 2.3 SR8" TO ANSI-REFERENCE.                    SM1034.2
            046700     MOVE   "COPY-TEST-3"   TO PAR-NAME.                          SM1034.2
            046800     MOVE    8  TO WRK-DU-00001.                                  SM1034.2
            046900     GO TO   COPY-TEST-3-0.                                       SM1034.2
            047000 COPY-DELETE-3.                                                   SM1034.2
            047100     PERFORM DE-LETE.                                             SM1034.2
            047200     PERFORM PRINT-DETAIL.                                        SM1034.2
            047300     GO TO   COPY-INIT-4.                                         SM1034.2
            047400 COPY-TEST-3-0.                                                   SM1034.2
            047500********************* COPY TEXT USED ***************************  SM1034.2
            047600*                            8                                 *  SM1034.2
            047700*********************END OF COPY TEXT***************************  SM1034.2
            047800     IF      WRK-DU-00001 =                                       SM1034.2
            047900     COPY    K3SML.                                               SM1034.2
            048000             PERFORM PASS                                         SM1034.2
            048100             PERFORM PRINT-DETAIL                                 SM1034.2
            048200     ELSE                                                         SM1034.2
            048300             MOVE   "COPYING SINGLE CHARACTER FAILED"             SM1034.2
            048400                  TO RE-MARK                                      SM1034.2
            048500             MOVE    8   TO CORRECT-N                             SM1034.2
            048600             MOVE    WRK-DU-00001 TO COMPUTED-N                   SM1034.2
            048700             PERFORM FAIL                                         SM1034.2
            048800             PERFORM PRINT-DETAIL.                                SM1034.2
            048900*                                                                 SM1034.2
            049000 COPY-INIT-4.                                                     SM1034.2
            049100*    ===-->  MAXIMUM LENGTH TEXT WORD  <--===                     SM1034.2
            049200     MOVE   "XII-2 2.3 (SR8) AND XII-5 2.4(GR11)"                 SM1034.2
            049300          TO ANSI-REFERENCE.                                      SM1034.2
            049400     MOVE   "COPY-TEST-4" TO PAR-NAME.                            SM1034.2
            049500     MOVE    SPACES      TO WRK-XN-00322.                         SM1034.2
            049600     MOVE    1 TO REC-CT.                                         SM1034.2
            049700     GO TO   COPY-TEST-4-0.                                       SM1034.2
            049800 COPY-DELETE-4.                                                   SM1034.2
            049900     PERFORM DE-LETE.                                             SM1034.2
            050000     PERFORM PRINT-DETAIL.                                        SM1034.2
            050100     GO TO   CCVS-EXIT.                                           SM1034.2
            050200 COPY-TEST-4-0.                                                   SM1034.2
            050300********************* COPY TEXT USED ***************************  SM1034.2
            050400*    MOVE 1 TO WRK-DU-9, ADD 1 TO WRK-DU-9, ADD 1 TO WRK-DU-9, ADDSM1034.2
            050500*     1 TO WRK-DU-99, ADD 1 TO WRK-DU-9, ADD 1 TO WRK-DU-99, ADD 1SM1034.2
            050600*     TO WRK-DU-99, ADD 1 TO WRK-DU-99, ADD 1 TO WRK-DU-99, ADD 1 SM1034.2
            050700*    TO WRK-DU-9, ADD 1 TO WRK-DU-9, ADD 1 TO WRK-DU-99, ADD 1 TO SM1034.2
            050800*    WRK-DU-99, ADD 1 TO WRK-DU-99, ADD 1 TO WRK-DU-99, ADD 10 TO SM1034.2
            050900*    WRK-DU-99-LONGER.                                            SM1034.2
            051000*********************END OF COPY TEXT***************************  SM1034.2
            051100*                                                                 SM1034.2
            051200     COPY    K3LGE.                                               SM1034.2
            051300*                                                                 SM1034.2
            051400 COPY-TEST-4-1.                                                   SM1034.2
            051500     MOVE   "COPY-TEST-4-1" TO PAR-NAME.                          SM1034.2
            051600     IF      WRK-DU-9 = 6                                         SM1034.2
            051700             PERFORM PASS                                         SM1034.2
            051800             PERFORM PRINT-DETAIL                                 SM1034.2
            051900     ELSE                                                         SM1034.2
            052000             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
            052100                  TO RE-MARK                                      SM1034.2
            052200             MOVE    6   TO CORRECT-N                             SM1034.2
            052300             MOVE    WRK-DU-9 TO COMPUTED-N                       SM1034.2
            052400             PERFORM FAIL                                         SM1034.2
            052500             PERFORM PRINT-DETAIL.                                SM1034.2
            052600     ADD     1 TO REC-CT.                                         SM1034.2
            052700 COPY-TEST-4-2.                                                   SM1034.2
            052800     MOVE   "COPY-TEST-4-2" TO PAR-NAME.                          SM1034.2
            052900     IF      WRK-DU-99 = 9                                        SM1034.2
            053000             PERFORM PASS                                         SM1034.2
            053100             PERFORM PRINT-DETAIL                                 SM1034.2
            053200     ELSE                                                         SM1034.2
            053300             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
            053400                  TO RE-MARK                                      SM1034.2
            053500             MOVE    9   TO CORRECT-N                             SM1034.2
            053600             MOVE    WRK-DU-99 TO COMPUTED-N                      SM1034.2
            053700             PERFORM FAIL                                         SM1034.2
            053800             PERFORM PRINT-DETAIL.                                SM1034.2
            053900     ADD     1 TO REC-CT.                                         SM1034.2
            054000 COPY-TEST-4-3.                                                   SM1034.2
            054100     MOVE   "COPY-TEST-4-3" TO PAR-NAME.                          SM1034.2
            054200     IF      WRK-DU-99-LONGER = 10                                SM1034.2
            054300             PERFORM PASS                                         SM1034.2
            054400             PERFORM PRINT-DETAIL                                 SM1034.2
            054500     ELSE                                                         SM1034.2
            054600             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
            054700                  TO RE-MARK                                      SM1034.2
            054800             MOVE    10  TO CORRECT-N                             SM1034.2
            054900             MOVE    WRK-DU-99-LONGER TO COMPUTED-N               SM1034.2
            055000             PERFORM FAIL                                         SM1034.2
            055100             PERFORM PRINT-DETAIL.                                SM1034.2
            055200*                                                                 SM1034.2
            055300 CCVS-EXIT SECTION.                                               SM1034.2
            055400 CCVS-999999.                                                     SM1034.2
            055500     GO TO CLOSE-FILES.                                           SM1034.2
                  *END-OF,SM103A                                                                  
        """)
    )

    @Test
    fun sm105A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM105A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM1054.2
            000200 PROGRAM-ID.                                                      SM1054.2
            000300     SM105A.                                                      SM1054.2
            000400****************************************************************  SM1054.2
            000500*                                                              *  SM1054.2
            000600*    VALIDATION FOR:-                                          *  SM1054.2
            000700*                                                              *  SM1054.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1054.2
            000900*                                                              *  SM1054.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1054.2
            001100*                                                              *  SM1054.2
            001200****************************************************************  SM1054.2
            001300*                                                              *  SM1054.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM1054.2
            001500*                                                              *  SM1054.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM1054.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM1054.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM1054.2
            001900*                                                              *  SM1054.2
            002000****************************************************************  SM1054.2
            002100*                                                              *  SM1054.2
            002200*    PROGRAM NC105A TESTS THE USE OF THE "COPY" STATEMENT IN   *  SM1054.2
            002300*    THE DATA DIVISION FOR A SORT DESCRIPTION ENTRY AND THE    *  SM1054.2
            002400*    ASSOCIATED RECORD DESCRIPTION ENTRIES.                    *  SM1054.2
            002500*                                                              *  SM1054.2
            002600****************************************************************  SM1054.2
            002700                                                                  SM1054.2
            002800 ENVIRONMENT DIVISION.                                            SM1054.2
            002900 CONFIGURATION SECTION.                                           SM1054.2
            003000 SOURCE-COMPUTER.                                                 SM1054.2
            003100     XXXXX082.                                                    SM1054.2
            003200 OBJECT-COMPUTER.                                                 SM1054.2
            003300     XXXXX083.                                                    SM1054.2
            003400 INPUT-OUTPUT SECTION.                                            SM1054.2
            003500 FILE-CONTROL.                                                    SM1054.2
            003600     SELECT PRINT-FILE ASSIGN TO                                  SM1054.2
            003700     XXXXX055.                                                    SM1054.2
            003800     SELECT SORTFILE-1E ASSIGN TO                                 SM1054.2
            003900     XXXXX027.                                                    SM1054.2
            004000     SELECT SORTOUT-1E ASSIGN TO                                  SM1054.2
            004100     XXXXX001.                                                    SM1054.2
            004200 DATA DIVISION.                                                   SM1054.2
            004300 FILE SECTION.                                                    SM1054.2
            004400 FD  PRINT-FILE.                                                  SM1054.2
            004500 01  PRINT-REC PICTURE X(120).                                    SM1054.2
            004600 01  DUMMY-RECORD PICTURE X(120).                                 SM1054.2
            004700                                                                  SM1054.2
            004800                                                                  SM1054.2
            004900                                                                  SM1054.2
            005000                                                                  SM1054.2
            005100                                                                  SM1054.2
            005200*                                                                 SM1054.2
            005300*********************** COPY STATEMENT USED **********************SM1054.2
            005400*                                                                 SM1054.2
            005500*SD  SORTFILE-1E                                      COPY K5SDA. SM1054.2
            005600*                                                                 SM1054.2
            005700******************** COPIED TEXT BEGINS BELOW ********************SM1054.2
            005800 SD  SORTFILE-1E                                       COPY K5SDA.SM1054.2
            005900*********************** END OF COPIED TEXT ***********************SM1054.2
            006000                                                                  SM1054.2
            006100                                                                  SM1054.2
            006200                                                                  SM1054.2
            006300                                                                  SM1054.2
            006400                                                                  SM1054.2
            006500 01  S-RECORD.                                                    SM1054.2
            006600*                                                                 SM1054.2
            006700*********************** COPY STATEMENT USED **********************SM1054.2
            006800*                                                                 SM1054.2
            006900*                                                     COPY K501A. SM1054.2
            007000*                                                                 SM1054.2
            007100******************** COPIED TEXT BEGINS BELOW ********************SM1054.2
            007200                                                       COPY K501A.SM1054.2
            007300*********************** END OF COPIED TEXT ***********************SM1054.2
            007400 FD  SORTOUT-1E                                                   SM1054.2
            007500     BLOCK CONTAINS 10 RECORDS                                    SM1054.2
            007600     LABEL RECORDS ARE STANDARD                                   SM1054.2
            007700C    VALUE OF                                                     SM1054.2
            007800C    XXXXX074                                                     SM1054.2
            007900C    IS                                                           SM1054.2
            008000C    XXXXX075                                                     SM1054.2
            008100*    XXXXX069                                                     SM1054.2
            008200     DATA RECORD SORTED.                                          SM1054.2
            008300 01  SORTED PICTURE X(120).                                       SM1054.2
            008400 WORKING-STORAGE SECTION.                                         SM1054.2
            008500 77  C0 PICTURE 9 VALUE 0.                                        SM1054.2
            008600 77  C1 PICTURE 9 VALUE 1.                                        SM1054.2
            008700 77  C2 PICTURE 9 VALUE 2.                                        SM1054.2
            008800 77  C6 PICTURE 9 VALUE 6.                                        SM1054.2
            008900 77  C3 PICTURE 9 VALUE 3.                                        SM1054.2
            009000 01  WKEYS-GROUP.                                                 SM1054.2
            009100     02  WKEY-1  PICTURE 9.                                       SM1054.2
            009200     02  WKEY-2  PICTURE 99.                                      SM1054.2
            009300     02  WKEY-3  PICTURE 999.                                     SM1054.2
            009400     02  WKEY-4  PICTURE 9999.                                    SM1054.2
            009500     02  WKEY-5 PICTURE 9(5).                                     SM1054.2
            009600 01  WKEYS-RDF REDEFINES WKEYS-GROUP PICTURE 9(15).               SM1054.2
            009700 01  TEST-RESULTS.                                                SM1054.2
            009800     02 FILLER                   PIC X      VALUE SPACE.          SM1054.2
            009900     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM1054.2
            010000     02 FILLER                   PIC X      VALUE SPACE.          SM1054.2
            010100     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM1054.2
            010200     02 FILLER                   PIC X      VALUE SPACE.          SM1054.2
            010300     02  PAR-NAME.                                                SM1054.2
            010400       03 FILLER                 PIC X(19)  VALUE SPACE.          SM1054.2
            010500       03  PARDOT-X              PIC X      VALUE SPACE.          SM1054.2
            010600       03 DOTVALUE               PIC 99     VALUE ZERO.           SM1054.2
            010700     02 FILLER                   PIC X(8)   VALUE SPACE.          SM1054.2
            010800     02 RE-MARK                  PIC X(61).                       SM1054.2
            010900 01  TEST-COMPUTED.                                               SM1054.2
            011000     02 FILLER                   PIC X(30)  VALUE SPACE.          SM1054.2
            011100     02 FILLER                   PIC X(17)  VALUE                 SM1054.2
            011200            "       COMPUTED=".                                   SM1054.2
            011300     02 COMPUTED-X.                                               SM1054.2
            011400     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM1054.2
            011500     03 COMPUTED-N               REDEFINES COMPUTED-A             SM1054.2
            011600                                 PIC -9(9).9(9).                  SM1054.2
            011700     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM1054.2
            011800     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM1054.2
            011900     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM1054.2
            012000     03       CM-18V0 REDEFINES COMPUTED-A.                       SM1054.2
            012100         04 COMPUTED-18V0                    PIC -9(18).          SM1054.2
            012200         04 FILLER                           PIC X.               SM1054.2
            012300     03 FILLER PIC X(50) VALUE SPACE.                             SM1054.2
            012400 01  TEST-CORRECT.                                                SM1054.2
            012500     02 FILLER PIC X(30) VALUE SPACE.                             SM1054.2
            012600     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM1054.2
            012700     02 CORRECT-X.                                                SM1054.2
            012800     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM1054.2
            012900     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM1054.2
            013000     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM1054.2
            013100     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM1054.2
            013200     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM1054.2
            013300     03      CR-18V0 REDEFINES CORRECT-A.                         SM1054.2
            013400         04 CORRECT-18V0                     PIC -9(18).          SM1054.2
            013500         04 FILLER                           PIC X.               SM1054.2
            013600     03 FILLER PIC X(2) VALUE SPACE.                              SM1054.2
            013700     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM1054.2
            013800 01  CCVS-C-1.                                                    SM1054.2
            013900     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM1054.2
            014000-    "SS  PARAGRAPH-NAME                                          SM1054.2
            014100-    "       REMARKS".                                            SM1054.2
            014200     02 FILLER                     PIC X(20)    VALUE SPACE.      SM1054.2
            014300 01  CCVS-C-2.                                                    SM1054.2
            014400     02 FILLER                     PIC X        VALUE SPACE.      SM1054.2
            014500     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM1054.2
            014600     02 FILLER                     PIC X(15)    VALUE SPACE.      SM1054.2
            014700     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM1054.2
            014800     02 FILLER                     PIC X(94)    VALUE SPACE.      SM1054.2
            014900 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM1054.2
            015000 01  REC-CT                        PIC 99       VALUE ZERO.       SM1054.2
            015100 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM1054.2
            015200 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM1054.2
            015300 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM1054.2
            015400 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM1054.2
            015500 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM1054.2
            015600 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM1054.2
            015700 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM1054.2
            015800 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM1054.2
            015900 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM1054.2
            016000 01  CCVS-H-1.                                                    SM1054.2
            016100     02  FILLER                    PIC X(39)    VALUE SPACES.     SM1054.2
            016200     02  FILLER                    PIC X(42)    VALUE             SM1054.2
            016300     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM1054.2
            016400     02  FILLER                    PIC X(39)    VALUE SPACES.     SM1054.2
            016500 01  CCVS-H-2A.                                                   SM1054.2
            016600   02  FILLER                        PIC X(41)  VALUE SPACE.      SM1054.2
            016700   02  FILLER                        PIC X(39)  VALUE             SM1054.2
            016800            "CCVS85  NCC COPY - NOT FOR DISTRIBUTION".            SM1054.2
            016900   02  FILLER                        PIC X(40)  VALUE SPACE.      SM1054.2
            017000                                                                  SM1054.2
            017100 01  CCVS-H-2B.                                                   SM1054.2
            017200   02  FILLER                        PIC X(15)  VALUE             SM1054.2
            017300            "TEST RESULT OF ".                                    SM1054.2
            017400   02  TEST-ID                       PIC X(9).                    SM1054.2
            017500   02  FILLER                        PIC X(4)   VALUE             SM1054.2
            017600            " IN ".                                               SM1054.2
            017700   02  FILLER                        PIC X(12)  VALUE             SM1054.2
            017800     " HIGH       ".                                              SM1054.2
            017900   02  FILLER                        PIC X(22)  VALUE             SM1054.2
            018000            " LEVEL VALIDATION FOR ".                             SM1054.2
            018100   02  FILLER                        PIC X(58)  VALUE             SM1054.2
            018200     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1054.2
            018300 01  CCVS-H-3.                                                    SM1054.2
            018400     02  FILLER                      PIC X(34)  VALUE             SM1054.2
            018500            " FOR OFFICIAL USE ONLY    ".                         SM1054.2
            018600     02  FILLER                      PIC X(58)  VALUE             SM1054.2
            018700     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1054.2
            018800     02  FILLER                      PIC X(28)  VALUE             SM1054.2
            018900            "  COPYRIGHT   1985 ".                                SM1054.2
            019000 01  CCVS-E-1.                                                    SM1054.2
            019100     02 FILLER                       PIC X(52)  VALUE SPACE.      SM1054.2
            019200     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM1054.2
            019300     02 ID-AGAIN                     PIC X(9).                    SM1054.2
            019400     02 FILLER                       PIC X(45)  VALUE             SM1054.2
            019500            " NTIS DISTRIBUTION COBOL 85".                        SM1054.2
            019600 01  CCVS-E-2.                                                    SM1054.2
            019700     02  FILLER                      PIC X(31)  VALUE SPACE.      SM1054.2
            019800     02  FILLER                      PIC X(21)  VALUE SPACE.      SM1054.2
            019900     02 CCVS-E-2-2.                                               SM1054.2
            020000         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM1054.2
            020100         03 FILLER                   PIC X      VALUE SPACE.      SM1054.2
            020200         03 ENDER-DESC               PIC X(44)  VALUE             SM1054.2
            020300            "ERRORS ENCOUNTERED".                                 SM1054.2
            020400 01  CCVS-E-3.                                                    SM1054.2
            020500     02  FILLER                      PIC X(22)  VALUE             SM1054.2
            020600            " FOR OFFICIAL USE ONLY".                             SM1054.2
            020700     02  FILLER                      PIC X(12)  VALUE SPACE.      SM1054.2
            020800     02  FILLER                      PIC X(58)  VALUE             SM1054.2
            020900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1054.2
            021000     02  FILLER                      PIC X(13)  VALUE SPACE.      SM1054.2
            021100     02 FILLER                       PIC X(15)  VALUE             SM1054.2
            021200             " COPYRIGHT 1985".                                   SM1054.2
            021300 01  CCVS-E-4.                                                    SM1054.2
            021400     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM1054.2
            021500     02 FILLER                       PIC X(4)   VALUE " OF ".     SM1054.2
            021600     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM1054.2
            021700     02 FILLER                       PIC X(40)  VALUE             SM1054.2
            021800      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM1054.2
            021900 01  XXINFO.                                                      SM1054.2
            022000     02 FILLER                       PIC X(19)  VALUE             SM1054.2
            022100            "*** INFORMATION ***".                                SM1054.2
            022200     02 INFO-TEXT.                                                SM1054.2
            022300       04 FILLER                     PIC X(8)   VALUE SPACE.      SM1054.2
            022400       04 XXCOMPUTED                 PIC X(20).                   SM1054.2
            022500       04 FILLER                     PIC X(5)   VALUE SPACE.      SM1054.2
            022600       04 XXCORRECT                  PIC X(20).                   SM1054.2
            022700     02 INF-ANSI-REFERENCE           PIC X(48).                   SM1054.2
            022800 01  HYPHEN-LINE.                                                 SM1054.2
            022900     02 FILLER  PIC IS X VALUE IS SPACE.                          SM1054.2
            023000     02 FILLER  PIC IS X(65)    VALUE IS "************************SM1054.2
            023100-    "*****************************************".                 SM1054.2
            023200     02 FILLER  PIC IS X(54)    VALUE IS "************************SM1054.2
            023300-    "******************************".                            SM1054.2
            023400 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM1054.2
            023500     "SM105A".                                                    SM1054.2
            023600 PROCEDURE DIVISION.                                              SM1054.2
            023700 CCVS1 SECTION.                                                   SM1054.2
            023800 OPEN-FILES.                                                      SM1054.2
            023900     OPEN     OUTPUT PRINT-FILE.                                  SM1054.2
            024000     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM1054.2
            024100     MOVE    SPACE TO TEST-RESULTS.                               SM1054.2
            024200     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM1054.2
            024300     GO TO CCVS1-EXIT.                                            SM1054.2
            024400 CLOSE-FILES.                                                     SM1054.2
            024500     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM1054.2
            024600 TERMINATE-CCVS.                                                  SM1054.2
            024700*    EXIT PROGRAM.                                                SM1054.2
            024800*TERMINATE-CALL.                                                  SM1054.2
            024900     STOP     RUN.                                                SM1054.2
            025000 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM1054.2
            025100 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM1054.2
            025200 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM1054.2
            025300 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM1054.2
            025400     MOVE "****TEST DELETED****" TO RE-MARK.                      SM1054.2
            025500 PRINT-DETAIL.                                                    SM1054.2
            025600     IF REC-CT NOT EQUAL TO ZERO                                  SM1054.2
            025700             MOVE "." TO PARDOT-X                                 SM1054.2
            025800             MOVE REC-CT TO DOTVALUE.                             SM1054.2
            025900     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM1054.2
            026000     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM1054.2
            026100        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM1054.2
            026200          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM1054.2
            026300     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM1054.2
            026400     MOVE SPACE TO CORRECT-X.                                     SM1054.2
            026500     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM1054.2
            026600     MOVE     SPACE TO RE-MARK.                                   SM1054.2
            026700 HEAD-ROUTINE.                                                    SM1054.2
            026800     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1054.2
            026900     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1054.2
            027000     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1054.2
            027100     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1054.2
            027200 COLUMN-NAMES-ROUTINE.                                            SM1054.2
            027300     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1054.2
            027400     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1054.2
            027500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM1054.2
            027600 END-ROUTINE.                                                     SM1054.2
            027700     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM1054.2
            027800 END-RTN-EXIT.                                                    SM1054.2
            027900     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1054.2
            028000 END-ROUTINE-1.                                                   SM1054.2
            028100      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM1054.2
            028200      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM1054.2
            028300      ADD PASS-COUNTER TO ERROR-HOLD.                             SM1054.2
            028400*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM1054.2
            028500      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM1054.2
            028600      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM1054.2
            028700      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM1054.2
            028800      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM1054.2
            028900  END-ROUTINE-12.                                                 SM1054.2
            029000      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM1054.2
            029100     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM1054.2
            029200         MOVE "NO " TO ERROR-TOTAL                                SM1054.2
            029300         ELSE                                                     SM1054.2
            029400         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM1054.2
            029500     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM1054.2
            029600     PERFORM WRITE-LINE.                                          SM1054.2
            029700 END-ROUTINE-13.                                                  SM1054.2
            029800     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM1054.2
            029900         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM1054.2
            030000         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM1054.2
            030100     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM1054.2
            030200     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1054.2
            030300      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM1054.2
            030400          MOVE "NO " TO ERROR-TOTAL                               SM1054.2
            030500      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM1054.2
            030600      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM1054.2
            030700      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM1054.2
            030800     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1054.2
            030900 WRITE-LINE.                                                      SM1054.2
            031000     ADD 1 TO RECORD-COUNT.                                       SM1054.2
            031100*    IF RECORD-COUNT GREATER 50                                   SM1054.2
            031200*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM1054.2
            031300*        MOVE SPACE TO DUMMY-RECORD                               SM1054.2
            031400*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM1054.2
            031500*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM1054.2
            031600*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM1054.2
            031700*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM1054.2
            031800*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM1054.2
            031900*        MOVE ZERO TO RECORD-COUNT.                               SM1054.2
            032000     PERFORM WRT-LN.                                              SM1054.2
            032100 WRT-LN.                                                          SM1054.2
            032200     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM1054.2
            032300     MOVE SPACE TO DUMMY-RECORD.                                  SM1054.2
            032400 BLANK-LINE-PRINT.                                                SM1054.2
            032500     PERFORM WRT-LN.                                              SM1054.2
            032600 FAIL-ROUTINE.                                                    SM1054.2
            032700     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM1054.2
            032800     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM1054.2
            032900     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1054.2
            033000     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM1054.2
            033100     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1054.2
            033200     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1054.2
            033300     GO TO  FAIL-ROUTINE-EX.                                      SM1054.2
            033400 FAIL-ROUTINE-WRITE.                                              SM1054.2
            033500     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM1054.2
            033600     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM1054.2
            033700     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM1054.2
            033800     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM1054.2
            033900 FAIL-ROUTINE-EX. EXIT.                                           SM1054.2
            034000 BAIL-OUT.                                                        SM1054.2
            034100     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM1054.2
            034200     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM1054.2
            034300 BAIL-OUT-WRITE.                                                  SM1054.2
            034400     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM1054.2
            034500     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1054.2
            034600     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1054.2
            034700     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1054.2
            034800 BAIL-OUT-EX. EXIT.                                               SM1054.2
            034900 CCVS1-EXIT.                                                      SM1054.2
            035000     EXIT.                                                        SM1054.2
            035100 SORT-INIT SECTION.                                               SM1054.2
            035200 I-1.                                                             SM1054.2
            035300     SORT SORTFILE-1E                                             SM1054.2
            035400     ON ASCENDING KEY KEY-1                                       SM1054.2
            035500     ON DESCENDING KEY KEY-2                                      SM1054.2
            035600     ON ASCENDING KEY KEY-3                                       SM1054.2
            035700     DESCENDING KEY-4 KEY-5                                       SM1054.2
            035800     INPUT PROCEDURE IS INSORT                                    SM1054.2
            035900     OUTPUT PROCEDURE IS OUTP1 THRU OUTP3.                        SM1054.2
            036000 I-2.                                                             SM1054.2
            036100     GO TO    CLOSE-FILES.                                        SM1054.2
            036200 INSORT SECTION.                                                  SM1054.2
            036300 IN-2.                                                            SM1054.2
            036400     MOVE 900009000000000 TO RDF-KEYS.                            SM1054.2
            036500     RELEASE S-RECORD.                                            SM1054.2
            036600     MOVE 009000000900009 TO RDF-KEYS.                            SM1054.2
            036700     RELEASE S-RECORD.                                            SM1054.2
            036800     MOVE 900008000000000 TO RDF-KEYS.                            SM1054.2
            036900     RELEASE S-RECORD.                                            SM1054.2
            037000     MOVE 009000000900008 TO RDF-KEYS.                            SM1054.2
            037100     RELEASE S-RECORD.                                            SM1054.2
            037200*    NOTE HI-LOW CONTROL RECORDS DONE.                            SM1054.2
            037300     MOVE 300003000000000 TO WKEYS-RDF.                           SM1054.2
            037400 IN-3.                                                            SM1054.2
            037500     PERFORM IN-4 2 TIMES.                                        SM1054.2
            037600     GO TO IN-EXIT.                                               SM1054.2
            037700 IN-4.                                                            SM1054.2
            037800     SUBTRACT C1 FROM WKEY-1.                                     SM1054.2
            037900     PERFORM IN-5 6 TIMES.                                        SM1054.2
            038000 IN-5.                                                            SM1054.2
            038100     IF WKEY-2 IS EQUAL TO C6                                     SM1054.2
            038200         MOVE C0 TO WKEY-2.                                       SM1054.2
            038300     ADD C1 TO WKEY-2.                                            SM1054.2
            038400     PERFORM IN-6 2 TIMES.                                        SM1054.2
            038500 IN-6.                                                            SM1054.2
            038600     IF WKEY-3 IS EQUAL TO C1                                     SM1054.2
            038700         MOVE C3 TO WKEY-3.                                       SM1054.2
            038800     SUBTRACT C1 FROM WKEY-3.                                     SM1054.2
            038900     PERFORM IN-7 2 TIMES.                                        SM1054.2
            039000 IN-7.                                                            SM1054.2
            039100     IF WKEY-4 EQUAL TO C2                                        SM1054.2
            039200         MOVE C0 TO WKEY-4.                                       SM1054.2
            039300     ADD C1 TO WKEY-4.                                            SM1054.2
            039400     PERFORM IN-8 2 TIMES.                                        SM1054.2
            039500 IN-8.                                                            SM1054.2
            039600     IF WKEY-5 IS EQUAL TO C2                                     SM1054.2
            039700         MOVE C0 TO WKEY-5.                                       SM1054.2
            039800     ADD C1 TO WKEY-5.                                            SM1054.2
            039900     MOVE WKEYS-RDF TO RDF-KEYS.                                  SM1054.2
            040000     RELEASE S-RECORD.                                            SM1054.2
            040100 IN-EXIT.                                                         SM1054.2
            040200     EXIT.                                                        SM1054.2
            040300 OUTP1 SECTION.                                                   SM1054.2
            040400 SM105-INIT.                                                      SM1054.2
            040500     OPEN     OUTPUT SORTOUT-1E.                                  SM1054.2
            040600     MOVE     "COPY SORT DESCR" TO FEATURE.                       SM1054.2
            040700 COPY-TEST-1.                                                     SM1054.2
            040800     PERFORM  RET-1.                                              SM1054.2
            040900     IF       RDF-KEYS EQUAL TO 009000000900009                   SM1054.2
            041000              PERFORM PASS-1 GO TO COPY-WRITE-1.                  SM1054.2
            041100     GO       TO COPY-FAIL-1-1.                                   SM1054.2
            041200 COPY-DELETE-1.                                                   SM1054.2
            041300     PERFORM  DE-LETE-1.                                          SM1054.2
            041400     GO       TO COPY-WRITE-1.                                    SM1054.2
            041500 COPY-FAIL-1-1.                                                   SM1054.2
            041600     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            041700     MOVE     009000000900009 TO CORRECT-18V0.                    SM1054.2
            041800     PERFORM  FAIL-1.                                             SM1054.2
            041900 COPY-WRITE-1.                                                    SM1054.2
            042000     MOVE     "COPY-TEST-1 " TO PAR-NAME.                         SM1054.2
            042100     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            042200 COPY-TEST-2.                                                     SM1054.2
            042300     PERFORM  RET-1.                                              SM1054.2
            042400     IF       RDF-KEYS EQUAL TO 009000000900008                   SM1054.2
            042500              PERFORM PASS-1 GO TO COPY-WRITE-2.                  SM1054.2
            042600     GO       TO COPY-FAIL-1-2.                                   SM1054.2
            042700 COPY-DELETE-2.                                                   SM1054.2
            042800     PERFORM  DE-LETE-1.                                          SM1054.2
            042900     GO       TO COPY-WRITE-2.                                    SM1054.2
            043000 COPY-FAIL-1-2.                                                   SM1054.2
            043100     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            043200     MOVE     009000000900008 TO CORRECT-18V0.                    SM1054.2
            043300     PERFORM  FAIL-1.                                             SM1054.2
            043400 COPY-WRITE-2.                                                    SM1054.2
            043500     MOVE     "COPY-TEST-2 " TO PAR-NAME.                         SM1054.2
            043600     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            043700 COPY-TEST-3.                                                     SM1054.2
            043800     PERFORM  RET-1.                                              SM1054.2
            043900     IF       RDF-KEYS EQUAL TO 106001000200002                   SM1054.2
            044000              PERFORM PASS-1 GO TO COPY-WRITE-3.                  SM1054.2
            044100     GO       TO COPY-FAIL-1-3.                                   SM1054.2
            044200 COPY-DELETE-3.                                                   SM1054.2
            044300     PERFORM  DE-LETE-1.                                          SM1054.2
            044400     GO       TO COPY-WRITE-3.                                    SM1054.2
            044500 COPY-FAIL-1-3.                                                   SM1054.2
            044600     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            044700     MOVE     106001000200002 TO CORRECT-18V0.                    SM1054.2
            044800     PERFORM  FAIL-1.                                             SM1054.2
            044900 COPY-WRITE-3.                                                    SM1054.2
            045000     MOVE     "COPY-TEST-3 " TO PAR-NAME.                         SM1054.2
            045100     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            045200 OUTP2 SECTION.                                                   SM1054.2
            045300 COPY-TEST-4.                                                     SM1054.2
            045400     PERFORM  RET-2 48 TIMES.                                     SM1054.2
            045500     IF       RDF-KEYS EQUAL TO 206001000200002                   SM1054.2
            045600              PERFORM PASS-1 GO TO COPY-WRITE-4.                  SM1054.2
            045700     GO       TO COPY-FAIL-1-4.                                   SM1054.2
            045800 COPY-DELETE-4.                                                   SM1054.2
            045900     PERFORM  DE-LETE-1.                                          SM1054.2
            046000     GO       TO COPY-WRITE-4.                                    SM1054.2
            046100 COPY-FAIL-1-4.                                                   SM1054.2
            046200     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            046300     MOVE     206001000200002 TO CORRECT-18V0.                    SM1054.2
            046400     PERFORM  FAIL-1.                                             SM1054.2
            046500 COPY-WRITE-4.                                                    SM1054.2
            046600     MOVE     "COPY-TEST-4 " TO PAR-NAME.                         SM1054.2
            046700     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            046800 COPY-TEST-5.                                                     SM1054.2
            046900     PERFORM  RET-2 40 TIMES.                                     SM1054.2
            047000     IF       RDF-KEYS EQUAL TO 201001000200002                   SM1054.2
            047100              PERFORM PASS-1 GO TO COPY-WRITE-5.                  SM1054.2
            047200     GO       TO COPY-FAIL-1-5.                                   SM1054.2
            047300 COPY-DELETE-5.                                                   SM1054.2
            047400     PERFORM  DE-LETE-1.                                          SM1054.2
            047500     GO       TO COPY-WRITE-5.                                    SM1054.2
            047600 COPY-FAIL-1-5.                                                   SM1054.2
            047700     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            047800     MOVE     201001000200002 TO CORRECT-18V0.                    SM1054.2
            047900     PERFORM  FAIL-1.                                             SM1054.2
            048000 COPY-WRITE-5.                                                    SM1054.2
            048100     MOVE     "COPY-TEST-5 " TO PAR-NAME.                         SM1054.2
            048200     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            048300 COPY-TEST-6.                                                     SM1054.2
            048400     PERFORM  RET-2  7 TIMES.                                     SM1054.2
            048500     IF       RDF-KEYS EQUAL TO 201002000100001                   SM1054.2
            048600              PERFORM PASS-1 GO TO COPY-WRITE-6.                  SM1054.2
            048700     GO       TO COPY-FAIL-1-6.                                   SM1054.2
            048800 COPY-DELETE-6.                                                   SM1054.2
            048900     PERFORM  DE-LETE-1.                                          SM1054.2
            049000     GO       TO COPY-WRITE-6.                                    SM1054.2
            049100 COPY-FAIL-1-6.                                                   SM1054.2
            049200     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            049300     MOVE     201002000100001 TO CORRECT-18V0.                    SM1054.2
            049400     PERFORM  FAIL-1.                                             SM1054.2
            049500 COPY-WRITE-6.                                                    SM1054.2
            049600     MOVE     "COPY-TEST-6 " TO PAR-NAME.                         SM1054.2
            049700     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            049800 COPY-TEST-7.                                                     SM1054.2
            049900     PERFORM  RET-2.                                              SM1054.2
            050000     IF       RDF-KEYS EQUAL TO 900008000000000                   SM1054.2
            050100              PERFORM PASS-1 GO TO COPY-WRITE-7.                  SM1054.2
            050200     GO       TO COPY-FAIL-1-7.                                   SM1054.2
            050300 COPY-DELETE-7.                                                   SM1054.2
            050400     PERFORM  DE-LETE-1.                                          SM1054.2
            050500     GO       TO COPY-WRITE-7.                                    SM1054.2
            050600 COPY-FAIL-1-7.                                                   SM1054.2
            050700     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            050800     MOVE     900008000000000 TO CORRECT-18V0.                    SM1054.2
            050900     PERFORM  FAIL-1.                                             SM1054.2
            051000 COPY-WRITE-7.                                                    SM1054.2
            051100     MOVE     "COPY-TEST-7 " TO PAR-NAME.                         SM1054.2
            051200     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            051300 COPY-TEST-8.                                                     SM1054.2
            051400     PERFORM  RET-2.                                              SM1054.2
            051500     IF       RDF-KEYS EQUAL TO 900009000000000                   SM1054.2
            051600              PERFORM PASS-1 GO TO COPY-WRITE-8.                  SM1054.2
            051700     GO       TO COPY-FAIL-1-8.                                   SM1054.2
            051800 COPY-DELETE-8.                                                   SM1054.2
            051900     PERFORM  DE-LETE-1.                                          SM1054.2
            052000     GO       TO COPY-WRITE-8.                                    SM1054.2
            052100 COPY-FAIL-1-8.                                                   SM1054.2
            052200     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            052300     MOVE     900009000000000 TO CORRECT-18V0.                    SM1054.2
            052400     PERFORM  FAIL-1.                                             SM1054.2
            052500 COPY-WRITE-8.                                                    SM1054.2
            052600     MOVE     "COPY-TEST-8 " TO PAR-NAME.                         SM1054.2
            052700     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            052800 COPY-TEST-9.                                                     SM1054.2
            052900     RETURN   SORTFILE-1E END                                     SM1054.2
            053000              PERFORM PASS-1 GO TO COPY-WRITE-9.                  SM1054.2
            053100*    NOTE     THE FOLLOWING STATEMENTS SHOULD NOT BE EXECUTED.    SM1054.2
            053200     PERFORM  FAIL-1.                                             SM1054.2
            053300     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM1054.2
            053400     MOVE     "END OF FILE NOT FOUND" TO RE-MARK.                 SM1054.2
            053500     GO       TO COPY-WRITE-9.                                    SM1054.2
            053600 COPY-DELETE-9.                                                   SM1054.2
            053700     PERFORM  DE-LETE-1.                                          SM1054.2
            053800 COPY-WRITE-9.                                                    SM1054.2
            053900     MOVE     "COPY-TEST-9 " TO PAR-NAME.                         SM1054.2
            054000     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            054100 OUTP3 SECTION.                                                   SM1054.2
            054200 RET-0.                                                           SM1054.2
            054300     CLOSE    SORTOUT-1E.                                         SM1054.2
            054400     GO       TO LIB1E-EXIT.                                      SM1054.2
            054500 RET-1.                                                           SM1054.2
            054600     RETURN   SORTFILE-1E RECORD AT END GO TO BAD-FILE.           SM1054.2
            054700     MOVE     S-RECORD TO SORTED.                                 SM1054.2
            054800     WRITE    SORTED.                                             SM1054.2
            054900 RET-2.                                                           SM1054.2
            055000     RETURN   SORTFILE-1E           END GO TO BAD-FILE.           SM1054.2
            055100     MOVE     S-RECORD TO SORTED.                                 SM1054.2
            055200     WRITE    SORTED.                                             SM1054.2
            055300 BAD-FILE.                                                        SM1054.2
            055400     PERFORM  FAIL-1.                                             SM1054.2
            055500     MOVE     "BAD-FILE" TO PAR-NAME.                             SM1054.2
            055600     MOVE     "EOF PREMATURELY FOUND" TO RE-MARK.                 SM1054.2
            055700     PERFORM  PRINT-DETAIL-1.                                     SM1054.2
            055800     CLOSE    SORTOUT-1E.                                         SM1054.2
            055900     GO TO    LIB1E-EXIT.                                         SM1054.2
            056000 INSPT-1. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.       SM1054.2
            056100 PASS-1.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.         SM1054.2
            056200 FAIL-1.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.        SM1054.2
            056300 DE-LETE-1.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.    SM1054.2
            056400     MOVE "****TEST DELETED****" TO RE-MARK.                      SM1054.2
            056500 PRINT-DETAIL-1.                                                  SM1054.2
            056600     IF REC-CT NOT EQUAL TO ZERO                                  SM1054.2
            056700             MOVE "." TO PARDOT-X                                 SM1054.2
            056800             MOVE REC-CT TO DOTVALUE.                             SM1054.2
            056900     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE-1.    SM1054.2
            057000     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE-1             SM1054.2
            057100        PERFORM FAIL-ROUTINE-1 THRU FAIL-ROUTINE-EX-1             SM1054.2
            057200          ELSE PERFORM BAIL-OUT-1 THRU BAIL-OUT-EX-1.             SM1054.2
            057300     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM1054.2
            057400     MOVE SPACE TO CORRECT-X.                                     SM1054.2
            057500     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM1054.2
            057600     MOVE     SPACE TO RE-MARK.                                   SM1054.2
            057700 WRITE-LINE-1.                                                    SM1054.2
            057800     ADD 1 TO RECORD-COUNT.                                       SM1054.2
            057900*    IF RECORD-COUNT GREATER 50                                   SM1054.2
            058000*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM1054.2
            058100*        MOVE SPACE TO DUMMY-RECORD                               SM1054.2
            058200*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM1054.2
            058300*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN-1           SM1054.2
            058400*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN-1 2 TIMES   SM1054.2
            058500*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN-1        SM1054.2
            058600*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM1054.2
            058700*        MOVE ZERO TO RECORD-COUNT.                               SM1054.2
            058800     PERFORM WRT-LN-1.                                            SM1054.2
            058900 WRT-LN-1.                                                        SM1054.2
            059000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM1054.2
            059100     MOVE SPACE TO DUMMY-RECORD.                                  SM1054.2
            059200 BLANK-LINE-PRINT-1.                                              SM1054.2
            059300     PERFORM WRT-LN-1.                                            SM1054.2
            059400 FAIL-ROUTINE-1.                                                  SM1054.2
            059500     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-1.     SM1054.2
            059600     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-1.      SM1054.2
            059700     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    SM1054.2
            059800     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-1 2 TIMES.   SM1054.2
            059900     GO TO FAIL-ROUTINE-EX-1.                                     SM1054.2
            060000 FAIL-RTN-WRITE-1.                                                SM1054.2
            060100     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE-1         SM1054.2
            060200     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE-1 2 TIMES. SM1054.2
            060300 FAIL-ROUTINE-EX-1. EXIT.                                         SM1054.2
            060400 BAIL-OUT-1.                                                      SM1054.2
            060500     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE-1.     SM1054.2
            060600     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX-1.             SM1054.2
            060700 BAIL-OUT-WRITE-1.                                                SM1054.2
            060800     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM1054.2
            060900     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-1 2 TIMES.   SM1054.2
            061000 BAIL-OUT-EX-1. EXIT.                                             SM1054.2
            061100 LIB1E-EXIT.                                                      SM1054.2
            061200     EXIT.                                                        SM1054.2
                  *END-OF,SM105A                                                                  
        """)
    )

    @Test
    fun sm106A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM106A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM1064.2
            000200 PROGRAM-ID.                                                      SM1064.2
            000300     SM106A.                                                      SM1064.2
            000400****************************************************************  SM1064.2
            000500*                                                              *  SM1064.2
            000600*    VALIDATION FOR:-                                          *  SM1064.2
            000700*                                                              *  SM1064.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1064.2
            000900*                                                              *  SM1064.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1064.2
            001100*                                                              *  SM1064.2
            001200****************************************************************  SM1064.2
            001300*                                                              *  SM1064.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM1064.2
            001500*                                                              *  SM1064.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM1064.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM1064.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM1064.2
            001900*                                                              *  SM1064.2
            002000****************************************************************  SM1064.2
            002100 ENVIRONMENT DIVISION.                                            SM1064.2
            002200*                                                                 SM1064.2
            002300*********************** COPY STATEMENT USED **********************SM1064.2
            002400*                                                                 SM1064.2
            002500*COPY   K6SCA                                                     SM1064.2
            002600*                                                                 SM1064.2
            002700******************** COPIED TEXT BEGINS BELOW ********************SM1064.2
            002800 COPY   K6SCA.                                                    SM1064.2
            002900*********************** END OF COPIED TEXT ***********************SM1064.2
                  *END-OF,SM106A                                                                  
        """)
    )

    @Test
    fun sm107A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM107A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM1074.2
            000200 PROGRAM-ID.                                                      SM1074.2
            000300     SM107A.                                                      SM1074.2
            000400****************************************************************  SM1074.2
            000500*                                                              *  SM1074.2
            000600*    VALIDATION FOR:-                                          *  SM1074.2
            000700*                                                              *  SM1074.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1074.2
            000900*                                                              *  SM1074.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1074.2
            001100*                                                              *  SM1074.2
            001200****************************************************************  SM1074.2
            001300*                                                              *  SM1074.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM1074.2
            001500*                                                              *  SM1074.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM1074.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM1074.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM1074.2
            001900*                                                              *  SM1074.2
            002000****************************************************************  SM1074.2
            002100*                                                              *  SM1074.2
            002200*    PROGRAM SM107A TESTS THE CAPABILITY TO COPY 1599 RECORDS  *  SM1074.2
            002300*    BY A SINGLE "COPY" STATEMENT IN THE PROCEDURE DIVISION.   *  SM1074.2
            002400*                                                              *  SM1074.2
            002500****************************************************************  SM1074.2
            002600 ENVIRONMENT DIVISION.                                            SM1074.2
            002700 CONFIGURATION SECTION.                                           SM1074.2
            002800 SOURCE-COMPUTER.                                                 SM1074.2
            002900     XXXXX082.                                                    SM1074.2
            003000 OBJECT-COMPUTER.                                                 SM1074.2
            003100     XXXXX083.                                                    SM1074.2
            003200 INPUT-OUTPUT SECTION.                                            SM1074.2
            003300 FILE-CONTROL.                                                    SM1074.2
            003400     SELECT PRINT-FILE ASSIGN TO                                  SM1074.2
            003500     XXXXX055.                                                    SM1074.2
            003600 DATA DIVISION.                                                   SM1074.2
            003700 FILE SECTION.                                                    SM1074.2
            003800 FD  PRINT-FILE.                                                  SM1074.2
            003900 01  PRINT-REC PICTURE X(120).                                    SM1074.2
            004000 01  DUMMY-RECORD PICTURE X(120).                                 SM1074.2
            004100 WORKING-STORAGE SECTION.                                         SM1074.2
            004200 01  TEST-RESULTS.                                                SM1074.2
            004300     02 FILLER                   PIC X      VALUE SPACE.          SM1074.2
            004400     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM1074.2
            004500     02 FILLER                   PIC X      VALUE SPACE.          SM1074.2
            004600     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM1074.2
            004700     02 FILLER                   PIC X      VALUE SPACE.          SM1074.2
            004800     02  PAR-NAME.                                                SM1074.2
            004900       03 FILLER                 PIC X(19)  VALUE SPACE.          SM1074.2
            005000       03  PARDOT-X              PIC X      VALUE SPACE.          SM1074.2
            005100       03 DOTVALUE               PIC 99     VALUE ZERO.           SM1074.2
            005200     02 FILLER                   PIC X(8)   VALUE SPACE.          SM1074.2
            005300     02 RE-MARK                  PIC X(61).                       SM1074.2
            005400 01  TEST-COMPUTED.                                               SM1074.2
            005500     02 FILLER                   PIC X(30)  VALUE SPACE.          SM1074.2
            005600     02 FILLER                   PIC X(17)  VALUE                 SM1074.2
            005700            "       COMPUTED=".                                   SM1074.2
            005800     02 COMPUTED-X.                                               SM1074.2
            005900     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM1074.2
            006000     03 COMPUTED-N               REDEFINES COMPUTED-A             SM1074.2
            006100                                 PIC -9(9).9(9).                  SM1074.2
            006200     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM1074.2
            006300     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM1074.2
            006400     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM1074.2
            006500     03       CM-18V0 REDEFINES COMPUTED-A.                       SM1074.2
            006600         04 COMPUTED-18V0                    PIC -9(18).          SM1074.2
            006700         04 FILLER                           PIC X.               SM1074.2
            006800     03 FILLER PIC X(50) VALUE SPACE.                             SM1074.2
            006900 01  TEST-CORRECT.                                                SM1074.2
            007000     02 FILLER PIC X(30) VALUE SPACE.                             SM1074.2
            007100     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM1074.2
            007200     02 CORRECT-X.                                                SM1074.2
            007300     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM1074.2
            007400     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM1074.2
            007500     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM1074.2
            007600     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM1074.2
            007700     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM1074.2
            007800     03      CR-18V0 REDEFINES CORRECT-A.                         SM1074.2
            007900         04 CORRECT-18V0                     PIC -9(18).          SM1074.2
            008000         04 FILLER                           PIC X.               SM1074.2
            008100     03 FILLER PIC X(2) VALUE SPACE.                              SM1074.2
            008200     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM1074.2
            008300 01  CCVS-C-1.                                                    SM1074.2
            008400     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM1074.2
            008500-    "SS  PARAGRAPH-NAME                                          SM1074.2
            008600-    "       REMARKS".                                            SM1074.2
            008700     02 FILLER                     PIC X(20)    VALUE SPACE.      SM1074.2
            008800 01  CCVS-C-2.                                                    SM1074.2
            008900     02 FILLER                     PIC X        VALUE SPACE.      SM1074.2
            009000     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM1074.2
            009100     02 FILLER                     PIC X(15)    VALUE SPACE.      SM1074.2
            009200     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM1074.2
            009300     02 FILLER                     PIC X(94)    VALUE SPACE.      SM1074.2
            009400 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM1074.2
            009500 01  REC-CT                        PIC 99       VALUE ZERO.       SM1074.2
            009600 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM1074.2
            009700 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM1074.2
            009800 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM1074.2
            009900 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM1074.2
            010000 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM1074.2
            010100 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM1074.2
            010200 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM1074.2
            010300 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM1074.2
            010400 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM1074.2
            010500 01  CCVS-H-1.                                                    SM1074.2
            010600     02  FILLER                    PIC X(39)    VALUE SPACES.     SM1074.2
            010700     02  FILLER                    PIC X(42)    VALUE             SM1074.2
            010800     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM1074.2
            010900     02  FILLER                    PIC X(39)    VALUE SPACES.     SM1074.2
            011000 01  CCVS-H-2A.                                                   SM1074.2
            011100   02  FILLER                        PIC X(40)  VALUE SPACE.      SM1074.2
            011200   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM1074.2
            011300   02  FILLER                        PIC XXXX   VALUE             SM1074.2
            011400     "4.2 ".                                                      SM1074.2
            011500   02  FILLER                        PIC X(28)  VALUE             SM1074.2
            011600            " COPY - NOT FOR DISTRIBUTION".                       SM1074.2
            011700   02  FILLER                        PIC X(41)  VALUE SPACE.      SM1074.2
            011800                                                                  SM1074.2
            011900 01  CCVS-H-2B.                                                   SM1074.2
            012000   02  FILLER                        PIC X(15)  VALUE             SM1074.2
            012100            "TEST RESULT OF ".                                    SM1074.2
            012200   02  TEST-ID                       PIC X(9).                    SM1074.2
            012300   02  FILLER                        PIC X(4)   VALUE             SM1074.2
            012400            " IN ".                                               SM1074.2
            012500   02  FILLER                        PIC X(12)  VALUE             SM1074.2
            012600     " HIGH       ".                                              SM1074.2
            012700   02  FILLER                        PIC X(22)  VALUE             SM1074.2
            012800            " LEVEL VALIDATION FOR ".                             SM1074.2
            012900   02  FILLER                        PIC X(58)  VALUE             SM1074.2
            013000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1074.2
            013100 01  CCVS-H-3.                                                    SM1074.2
            013200     02  FILLER                      PIC X(34)  VALUE             SM1074.2
            013300            " FOR OFFICIAL USE ONLY    ".                         SM1074.2
            013400     02  FILLER                      PIC X(58)  VALUE             SM1074.2
            013500     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1074.2
            013600     02  FILLER                      PIC X(28)  VALUE             SM1074.2
            013700            "  COPYRIGHT   1985 ".                                SM1074.2
            013800 01  CCVS-E-1.                                                    SM1074.2
            013900     02 FILLER                       PIC X(52)  VALUE SPACE.      SM1074.2
            014000     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM1074.2
            014100     02 ID-AGAIN                     PIC X(9).                    SM1074.2
            014200     02 FILLER                       PIC X(45)  VALUE SPACES.     SM1074.2
            014300 01  CCVS-E-2.                                                    SM1074.2
            014400     02  FILLER                      PIC X(31)  VALUE SPACE.      SM1074.2
            014500     02  FILLER                      PIC X(21)  VALUE SPACE.      SM1074.2
            014600     02 CCVS-E-2-2.                                               SM1074.2
            014700         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM1074.2
            014800         03 FILLER                   PIC X      VALUE SPACE.      SM1074.2
            014900         03 ENDER-DESC               PIC X(44)  VALUE             SM1074.2
            015000            "ERRORS ENCOUNTERED".                                 SM1074.2
            015100 01  CCVS-E-3.                                                    SM1074.2
            015200     02  FILLER                      PIC X(22)  VALUE             SM1074.2
            015300            " FOR OFFICIAL USE ONLY".                             SM1074.2
            015400     02  FILLER                      PIC X(12)  VALUE SPACE.      SM1074.2
            015500     02  FILLER                      PIC X(58)  VALUE             SM1074.2
            015600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1074.2
            015700     02  FILLER                      PIC X(13)  VALUE SPACE.      SM1074.2
            015800     02 FILLER                       PIC X(15)  VALUE             SM1074.2
            015900             " COPYRIGHT 1985".                                   SM1074.2
            016000 01  CCVS-E-4.                                                    SM1074.2
            016100     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM1074.2
            016200     02 FILLER                       PIC X(4)   VALUE " OF ".     SM1074.2
            016300     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM1074.2
            016400     02 FILLER                       PIC X(40)  VALUE             SM1074.2
            016500      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM1074.2
            016600 01  XXINFO.                                                      SM1074.2
            016700     02 FILLER                       PIC X(19)  VALUE             SM1074.2
            016800            "*** INFORMATION ***".                                SM1074.2
            016900     02 INFO-TEXT.                                                SM1074.2
            017000       04 FILLER                     PIC X(8)   VALUE SPACE.      SM1074.2
            017100       04 XXCOMPUTED                 PIC X(20).                   SM1074.2
            017200       04 FILLER                     PIC X(5)   VALUE SPACE.      SM1074.2
            017300       04 XXCORRECT                  PIC X(20).                   SM1074.2
            017400     02 INF-ANSI-REFERENCE           PIC X(48).                   SM1074.2
            017500 01  HYPHEN-LINE.                                                 SM1074.2
            017600     02 FILLER  PIC IS X VALUE IS SPACE.                          SM1074.2
            017700     02 FILLER  PIC IS X(65)    VALUE IS "************************SM1074.2
            017800-    "*****************************************".                 SM1074.2
            017900     02 FILLER  PIC IS X(54)    VALUE IS "************************SM1074.2
            018000-    "******************************".                            SM1074.2
            018100 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM1074.2
            018200     "SM107A".                                                    SM1074.2
            018300 PROCEDURE DIVISION.                                              SM1074.2
            018400 CCVS1 SECTION.                                                   SM1074.2
            018500 OPEN-FILES.                                                      SM1074.2
            018600     OPEN     OUTPUT PRINT-FILE.                                  SM1074.2
            018700     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM1074.2
            018800     MOVE    SPACE TO TEST-RESULTS.                               SM1074.2
            018900     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM1074.2
            019000     GO TO CCVS1-EXIT.                                            SM1074.2
            019100 CLOSE-FILES.                                                     SM1074.2
            019200     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM1074.2
            019300 TERMINATE-CCVS.                                                  SM1074.2
            019400*    EXIT PROGRAM.                                                SM1074.2
            019500*TERMINATE-CALL.                                                  SM1074.2
            019600     STOP     RUN.                                                SM1074.2
            019700 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM1074.2
            019800 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM1074.2
            019900 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM1074.2
            020000 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM1074.2
            020100     MOVE "****TEST DELETED****" TO RE-MARK.                      SM1074.2
            020200 PRINT-DETAIL.                                                    SM1074.2
            020300     IF REC-CT NOT EQUAL TO ZERO                                  SM1074.2
            020400             MOVE "." TO PARDOT-X                                 SM1074.2
            020500             MOVE REC-CT TO DOTVALUE.                             SM1074.2
            020600     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM1074.2
            020700     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM1074.2
            020800        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM1074.2
            020900          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM1074.2
            021000     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM1074.2
            021100     MOVE SPACE TO CORRECT-X.                                     SM1074.2
            021200     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM1074.2
            021300     MOVE     SPACE TO RE-MARK.                                   SM1074.2
            021400 HEAD-ROUTINE.                                                    SM1074.2
            021500     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1074.2
            021600     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1074.2
            021700     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1074.2
            021800     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1074.2
            021900 COLUMN-NAMES-ROUTINE.                                            SM1074.2
            022000     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1074.2
            022100     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1074.2
            022200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM1074.2
            022300 END-ROUTINE.                                                     SM1074.2
            022400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM1074.2
            022500 END-RTN-EXIT.                                                    SM1074.2
            022600     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1074.2
            022700 END-ROUTINE-1.                                                   SM1074.2
            022800      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM1074.2
            022900      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM1074.2
            023000      ADD PASS-COUNTER TO ERROR-HOLD.                             SM1074.2
            023100*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM1074.2
            023200      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM1074.2
            023300      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM1074.2
            023400      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM1074.2
            023500      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM1074.2
            023600  END-ROUTINE-12.                                                 SM1074.2
            023700      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM1074.2
            023800     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM1074.2
            023900         MOVE "NO " TO ERROR-TOTAL                                SM1074.2
            024000         ELSE                                                     SM1074.2
            024100         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM1074.2
            024200     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM1074.2
            024300     PERFORM WRITE-LINE.                                          SM1074.2
            024400 END-ROUTINE-13.                                                  SM1074.2
            024500     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM1074.2
            024600         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM1074.2
            024700         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM1074.2
            024800     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM1074.2
            024900     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1074.2
            025000      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM1074.2
            025100          MOVE "NO " TO ERROR-TOTAL                               SM1074.2
            025200      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM1074.2
            025300      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM1074.2
            025400      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM1074.2
            025500     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1074.2
            025600 WRITE-LINE.                                                      SM1074.2
            025700     ADD 1 TO RECORD-COUNT.                                       SM1074.2
            025800*    IF RECORD-COUNT GREATER 50                                   SM1074.2
            025900*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM1074.2
            026000*        MOVE SPACE TO DUMMY-RECORD                               SM1074.2
            026100*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM1074.2
            026200*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM1074.2
            026300*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM1074.2
            026400*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM1074.2
            026500*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM1074.2
            026600*        MOVE ZERO TO RECORD-COUNT.                               SM1074.2
            026700     PERFORM WRT-LN.                                              SM1074.2
            026800 WRT-LN.                                                          SM1074.2
            026900     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM1074.2
            027000     MOVE SPACE TO DUMMY-RECORD.                                  SM1074.2
            027100 BLANK-LINE-PRINT.                                                SM1074.2
            027200     PERFORM WRT-LN.                                              SM1074.2
            027300 FAIL-ROUTINE.                                                    SM1074.2
            027400     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM1074.2
            027500     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM1074.2
            027600     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1074.2
            027700     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM1074.2
            027800     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1074.2
            027900     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1074.2
            028000     GO TO  FAIL-ROUTINE-EX.                                      SM1074.2
            028100 FAIL-ROUTINE-WRITE.                                              SM1074.2
            028200     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM1074.2
            028300     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM1074.2
            028400     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM1074.2
            028500     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM1074.2
            028600 FAIL-ROUTINE-EX. EXIT.                                           SM1074.2
            028700 BAIL-OUT.                                                        SM1074.2
            028800     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM1074.2
            028900     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM1074.2
            029000 BAIL-OUT-WRITE.                                                  SM1074.2
            029100     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM1074.2
            029200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1074.2
            029300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1074.2
            029400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1074.2
            029500 BAIL-OUT-EX. EXIT.                                               SM1074.2
            029600 CCVS1-EXIT.                                                      SM1074.2
            029700     EXIT.                                                        SM1074.2
            029800 WARNING-MSG.                                                     SM1074.2
            029900     MOVE " IF NO OTHER REPORT LINES APPEAR BELOW, ""COPY K7SEA"" SM1074.2
            030000-         "FAILED." TO PRINT-REC.                                 SM1074.2
            030100     PERFORM WRITE-LINE.                                          SM1074.2
            030200*                                                                 SM1074.2
            030300*********************** COPY STATEMENT USED **********************SM1074.2
            030400*                                                                 SM1074.2
            030500*COPY  K7SEA                                                      SM1074.2
            030600*                                                                 SM1074.2
            030700******************** COPIED TEXT BEGINS BELOW ********************SM1074.2
            030800  COPY  K7SEA.                                                    SM1074.2
            030900*********************** END OF COPIED TEXT ***********************SM1074.2
            031000 CCVS-EXIT SECTION.                                               SM1074.2
            031100 CCVS-999999.                                                     SM1074.2
            031200     GO TO CLOSE-FILES.                                           SM1074.2
                  *END-OF,SM107A                                                                  
        """)
    )

    @Test
    fun sm201A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM201A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM2014.2
            000200 PROGRAM-ID.                                                      SM2014.2
            000300     SM201A.                                                      SM2014.2
            000400****************************************************************  SM2014.2
            000500*                                                              *  SM2014.2
            000600*    VALIDATION FOR:-                                          *  SM2014.2
            000700*                                                              *  SM2014.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2014.2
            000900*                                                              *  SM2014.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2014.2
            001100*                                                              *  SM2014.2
            001200****************************************************************  SM2014.2
            001300*                                                              *  SM2014.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM2014.2
            001500*                                                              *  SM2014.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM2014.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM2014.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM2014.2
            001900*                                                              *  SM2014.2
            002000****************************************************************  SM2014.2
            002100*                                                              *  SM2014.2
            002200*    PROGRAM SM201A TESTS THE "REPLACING" PHRASE OF THE "COPY" *  SM2014.2
            002300*    STATEMENT IN THE WORKING-STORAGE SECTION AND              *  SM2014.2
            002400*    PROCEDURE DIVISION AND PRODUCES A SEQUENTIAL OUTPUT FILE  *  SM2014.2
            002500*    USING "COPY"ED CODE, WHICH IS SUBSEQUENTLY CHECKED BY     *  SM2014.2
            002600*    SM202A.                                                   *  SM2014.2
            002700*                                                              *  SM2014.2
            002800****************************************************************  SM2014.2
            002900 ENVIRONMENT DIVISION.                                            SM2014.2
            003000 CONFIGURATION SECTION.                                           SM2014.2
            003100 SOURCE-COMPUTER.                                                 SM2014.2
            003200     XXXXX082.                                                    SM2014.2
            003300 OBJECT-COMPUTER.                                                 SM2014.2
            003400     XXXXX083.                                                    SM2014.2
            003500 INPUT-OUTPUT SECTION.                                            SM2014.2
            003600 FILE-CONTROL.                                                    SM2014.2
            003700     SELECT PRINT-FILE ASSIGN TO                                  SM2014.2
            003800     XXXXX055.                                                    SM2014.2
            003900     SELECT TEST-FILE ASSIGN TO                                   SM2014.2
            004000     XXXXP001.                                                    SM2014.2
            004100 DATA DIVISION.                                                   SM2014.2
            004200 FILE SECTION.                                                    SM2014.2
            004300 FD  PRINT-FILE.                                                  SM2014.2
            004400 01  PRINT-REC PICTURE X(120).                                    SM2014.2
            004500 01  DUMMY-RECORD PICTURE X(120).                                 SM2014.2
            004600                                                                  SM2014.2
            004700                                                                  SM2014.2
            004800                                                                  SM2014.2
            004900                                                                  SM2014.2
            005000                                                                  SM2014.2
            005100*                                                                 SM2014.2
            005200*********************** COPY STATEMENT USED **********************SM2014.2
            005300*                                                                 SM2014.2
            005400*FD  TEST-FILE                                         COPY K1FDA SM2014.2
            005500*             REPLACING                                           SM2014.2
            005600*             PROOF-REC BY TST-TEST.                              SM2014.2
            005700*                                                                 SM2014.2
            005800******************** COPIED TEXT BEGINS BELOW ********************SM2014.2
            005900 FD  TEST-FILE                                         COPY K1FDA SM2014.2
            006000              REPLACING                                           SM2014.2
            006100              PROOF-REC BY TST-TEST.                              SM2014.2
            006200*********************** END OF COPIED TEXT ***********************SM2014.2
            006300                                                                  SM2014.2
            006400                                                                  SM2014.2
            006500                                                                  SM2014.2
            006600                                                                  SM2014.2
            006700                                                                  SM2014.2
            006800*                                                                 SM2014.2
            006900*********************** COPY STATEMENT USED **********************SM2014.2
            007000*                                                                 SM2014.2
            007100*01  TST-TEST                                          COPY K101A SM2014.2
            007200*             REPLACING                                           SM2014.2
            007300*                       TST-FLD-1 BY TF-1.                        SM2014.2
            007400*                                                                 SM2014.2
            007500******************** COPIED TEXT BEGINS BELOW ********************SM2014.2
            007600 01  TST-TEST                                          COPY K101A SM2014.2
            007700              REPLACING                                           SM2014.2
            007800                        TST-FLD-1 BY TF-1.                        SM2014.2
            007900*********************** END OF COPIED TEXT ***********************SM2014.2
            008000 WORKING-STORAGE SECTION.                                         SM2014.2
            008100 77  RCD-1 PICTURE 9(5) VALUE 97532.                              SM2014.2
            008200 77  RCD-2 PICTURE 9(5) VALUE 23479.                              SM2014.2
            008300 77  RCD-3 PICTURE 9(5) VALUE 10901.                              SM2014.2
            008400 77  RCD-4 PICTURE 9(5) VALUE 02734.                              SM2014.2
            008500 77  RCD-5 PICTURE 9(5) VALUE 14003.                              SM2014.2
            008600 77  RCD-6 PICTURE 9(5) VALUE 19922.                              SM2014.2
            008700 77  RCD-7 PICTURE 9(5) VALUE 03543.                              SM2014.2
            008800*                                                                 SM2014.2
            008900*********************** COPY STATEMENT USED **********************SM2014.2
            009000*                                                                 SM2014.2
            009100*01  TEXT-TEST-1 COPY K101A                                       SM2014.2
            009200*            REPLACING ==02 TST-FLD-1  PICTURE 9(5). 02 FILLER    SM2014.2
            009300*                      PICTURE X(115)==                           SM2014.2
            009400*            BY        ==02 FILLER PICTURE X(115).  02 TXT-FLD-1  SM2014.2
            009500*                      PIC 9(5)==.                                SM2014.2
            009600*                                                                 SM2014.2
            009700******************** COPIED TEXT BEGINS BELOW ********************SM2014.2
            009800 01  TEXT-TEST-1 COPY K101A                                       SM2014.2
            009900             REPLACING ==02 TST-FLD-1  PICTURE 9(5). 02 FILLER    SM2014.2
            010000                       PICTURE X(115)==                           SM2014.2
            010100             BY        ==02 FILLER PICTURE X(115).  02 TXT-FLD-1  SM2014.2
            010200                       PIC 9(5)==.                                SM2014.2
            010300*********************** END OF COPIED TEXT ***********************SM2014.2
            010400 01  WSTR-1.                                                      SM2014.2
            010500     02  WSTR-1A PICTURE XXX VALUE "ABC".                         SM2014.2
            010600                                                                  SM2014.2
            010700                                                                  SM2014.2
            010800                                                                  SM2014.2
            010900                                                                  SM2014.2
            011000                                                                  SM2014.2
            011100 01  WSTR-2.                                                      SM2014.2
            011200*                                                                 SM2014.2
            011300*********************** COPY STATEMENT USED **********************SM2014.2
            011400*                                                                 SM2014.2
            011500*                                                      COPY K1WKA SM2014.2
            011600*             REPLACING WSTR-2A BY WSTR999.                       SM2014.2
            011700*                                                                 SM2014.2
            011800******************** COPIED TEXT BEGINS BELOW ********************SM2014.2
            011900                                                       COPY K1WKA SM2014.2
            012000              REPLACING WSTR-2A BY WSTR999.                       SM2014.2
            012100*********************** END OF COPIED TEXT ***********************SM2014.2
            012200                                                                  SM2014.2
            012300                                                                  SM2014.2
            012400                                                                  SM2014.2
            012500                                                                  SM2014.2
            012600                                                                  SM2014.2
            012700 01  WSTR-3.                                                      SM2014.2
            012800*                                                                 SM2014.2
            012900*********************** COPY STATEMENT USED **********************SM2014.2
            013000*                                                                 SM2014.2
            013100*                                                      COPY K1WKA.SM2014.2
            013200*                                                                 SM2014.2
            013300******************** COPIED TEXT BEGINS BELOW ********************SM2014.2
            013400                                                       COPY K1WKA.SM2014.2
            013500*********************** END OF COPIED TEXT ***********************SM2014.2
            013600                                                                  SM2014.2
            013700                                                                  SM2014.2
            013800                                                                  SM2014.2
            013900                                                                  SM2014.2
            014000                                                                  SM2014.2
            014100*                                                                 SM2014.2
            014200*********************** COPY STATEMENT USED **********************SM2014.2
            014300*                                                                 SM2014.2
            014400*01  WSTR-4.                                           COPY K1WKB SM2014.2
            014500*             REPLACING WSTR4A BY WSTR91                          SM2014.2
            014600*                       WSTR4B BY WSTR92                          SM2014.2
            014700*                       WSTR4C BY WSTR93.                         SM2014.2
            014800*                                                                 SM2014.2
            014900******************** COPIED TEXT BEGINS BELOW ********************SM2014.2
            015000 01  WSTR-4.                                           COPY K1WKB SM2014.2
            015100              REPLACING WSTR4A BY WSTR91                          SM2014.2
            015200                        WSTR4B BY WSTR92                          SM2014.2
            015300                        WSTR4C BY WSTR93.                         SM2014.2
            015400*********************** END OF COPIED TEXT ***********************SM2014.2
            015500                                                                  SM2014.2
            015600                                                                  SM2014.2
            015700                                                                  SM2014.2
            015800                                                                  SM2014.2
            015900                                                                  SM2014.2
            016000*                                                                 SM2014.2
            016100*********************** COPY STATEMENT USED **********************SM2014.2
            016200*                                                                 SM2014.2
            016300*01  WSTR-5.                                           COPY K1WKB.SM2014.2
            016400*                                                                 SM2014.2
            016500******************** COPIED TEXT BEGINS BELOW ********************SM2014.2
            016600 01  WSTR-5.                                           COPY K1WKB.SM2014.2
            016700*********************** END OF COPIED TEXT ***********************SM2014.2
            016800 01  TEST-RESULTS.                                                SM2014.2
            016900     02 FILLER                   PIC X      VALUE SPACE.          SM2014.2
            017000     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM2014.2
            017100     02 FILLER                   PIC X      VALUE SPACE.          SM2014.2
            017200     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM2014.2
            017300     02 FILLER                   PIC X      VALUE SPACE.          SM2014.2
            017400     02  PAR-NAME.                                                SM2014.2
            017500       03 FILLER                 PIC X(19)  VALUE SPACE.          SM2014.2
            017600       03  PARDOT-X              PIC X      VALUE SPACE.          SM2014.2
            017700       03 DOTVALUE               PIC 99     VALUE ZERO.           SM2014.2
            017800     02 FILLER                   PIC X(8)   VALUE SPACE.          SM2014.2
            017900     02 RE-MARK                  PIC X(61).                       SM2014.2
            018000 01  TEST-COMPUTED.                                               SM2014.2
            018100     02 FILLER                   PIC X(30)  VALUE SPACE.          SM2014.2
            018200     02 FILLER                   PIC X(17)  VALUE                 SM2014.2
            018300            "       COMPUTED=".                                   SM2014.2
            018400     02 COMPUTED-X.                                               SM2014.2
            018500     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM2014.2
            018600     03 COMPUTED-N               REDEFINES COMPUTED-A             SM2014.2
            018700                                 PIC -9(9).9(9).                  SM2014.2
            018800     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM2014.2
            018900     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM2014.2
            019000     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM2014.2
            019100     03       CM-18V0 REDEFINES COMPUTED-A.                       SM2014.2
            019200         04 COMPUTED-18V0                    PIC -9(18).          SM2014.2
            019300         04 FILLER                           PIC X.               SM2014.2
            019400     03 FILLER PIC X(50) VALUE SPACE.                             SM2014.2
            019500 01  TEST-CORRECT.                                                SM2014.2
            019600     02 FILLER PIC X(30) VALUE SPACE.                             SM2014.2
            019700     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM2014.2
            019800     02 CORRECT-X.                                                SM2014.2
            019900     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM2014.2
            020000     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM2014.2
            020100     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM2014.2
            020200     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM2014.2
            020300     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM2014.2
            020400     03      CR-18V0 REDEFINES CORRECT-A.                         SM2014.2
            020500         04 CORRECT-18V0                     PIC -9(18).          SM2014.2
            020600         04 FILLER                           PIC X.               SM2014.2
            020700     03 FILLER PIC X(2) VALUE SPACE.                              SM2014.2
            020800     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM2014.2
            020900 01  CCVS-C-1.                                                    SM2014.2
            021000     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM2014.2
            021100-    "SS  PARAGRAPH-NAME                                          SM2014.2
            021200-    "       REMARKS".                                            SM2014.2
            021300     02 FILLER                     PIC X(20)    VALUE SPACE.      SM2014.2
            021400 01  CCVS-C-2.                                                    SM2014.2
            021500     02 FILLER                     PIC X        VALUE SPACE.      SM2014.2
            021600     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM2014.2
            021700     02 FILLER                     PIC X(15)    VALUE SPACE.      SM2014.2
            021800     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM2014.2
            021900     02 FILLER                     PIC X(94)    VALUE SPACE.      SM2014.2
            022000 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM2014.2
            022100 01  REC-CT                        PIC 99       VALUE ZERO.       SM2014.2
            022200 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM2014.2
            022300 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM2014.2
            022400 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM2014.2
            022500 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM2014.2
            022600 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM2014.2
            022700 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM2014.2
            022800 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM2014.2
            022900 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM2014.2
            023000 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM2014.2
            023100 01  CCVS-H-1.                                                    SM2014.2
            023200     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2014.2
            023300     02  FILLER                    PIC X(42)    VALUE             SM2014.2
            023400     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM2014.2
            023500     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2014.2
            023600 01  CCVS-H-2A.                                                   SM2014.2
            023700   02  FILLER                        PIC X(40)  VALUE SPACE.      SM2014.2
            023800   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM2014.2
            023900   02  FILLER                        PIC XXXX   VALUE             SM2014.2
            024000     "4.2 ".                                                      SM2014.2
            024100   02  FILLER                        PIC X(28)  VALUE             SM2014.2
            024200            " COPY - NOT FOR DISTRIBUTION".                       SM2014.2
            024300   02  FILLER                        PIC X(41)  VALUE SPACE.      SM2014.2
            024400                                                                  SM2014.2
            024500 01  CCVS-H-2B.                                                   SM2014.2
            024600   02  FILLER                        PIC X(15)  VALUE             SM2014.2
            024700            "TEST RESULT OF ".                                    SM2014.2
            024800   02  TEST-ID                       PIC X(9).                    SM2014.2
            024900   02  FILLER                        PIC X(4)   VALUE             SM2014.2
            025000            " IN ".                                               SM2014.2
            025100   02  FILLER                        PIC X(12)  VALUE             SM2014.2
            025200     " HIGH       ".                                              SM2014.2
            025300   02  FILLER                        PIC X(22)  VALUE             SM2014.2
            025400            " LEVEL VALIDATION FOR ".                             SM2014.2
            025500   02  FILLER                        PIC X(58)  VALUE             SM2014.2
            025600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2014.2
            025700 01  CCVS-H-3.                                                    SM2014.2
            025800     02  FILLER                      PIC X(34)  VALUE             SM2014.2
            025900            " FOR OFFICIAL USE ONLY    ".                         SM2014.2
            026000     02  FILLER                      PIC X(58)  VALUE             SM2014.2
            026100     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2014.2
            026200     02  FILLER                      PIC X(28)  VALUE             SM2014.2
            026300            "  COPYRIGHT   1985 ".                                SM2014.2
            026400 01  CCVS-E-1.                                                    SM2014.2
            026500     02 FILLER                       PIC X(52)  VALUE SPACE.      SM2014.2
            026600     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM2014.2
            026700     02 ID-AGAIN                     PIC X(9).                    SM2014.2
            026800     02 FILLER                       PIC X(45)  VALUE SPACES.     SM2014.2
            026900 01  CCVS-E-2.                                                    SM2014.2
            027000     02  FILLER                      PIC X(31)  VALUE SPACE.      SM2014.2
            027100     02  FILLER                      PIC X(21)  VALUE SPACE.      SM2014.2
            027200     02 CCVS-E-2-2.                                               SM2014.2
            027300         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM2014.2
            027400         03 FILLER                   PIC X      VALUE SPACE.      SM2014.2
            027500         03 ENDER-DESC               PIC X(44)  VALUE             SM2014.2
            027600            "ERRORS ENCOUNTERED".                                 SM2014.2
            027700 01  CCVS-E-3.                                                    SM2014.2
            027800     02  FILLER                      PIC X(22)  VALUE             SM2014.2
            027900            " FOR OFFICIAL USE ONLY".                             SM2014.2
            028000     02  FILLER                      PIC X(12)  VALUE SPACE.      SM2014.2
            028100     02  FILLER                      PIC X(58)  VALUE             SM2014.2
            028200     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2014.2
            028300     02  FILLER                      PIC X(13)  VALUE SPACE.      SM2014.2
            028400     02 FILLER                       PIC X(15)  VALUE             SM2014.2
            028500             " COPYRIGHT 1985".                                   SM2014.2
            028600 01  CCVS-E-4.                                                    SM2014.2
            028700     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM2014.2
            028800     02 FILLER                       PIC X(4)   VALUE " OF ".     SM2014.2
            028900     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM2014.2
            029000     02 FILLER                       PIC X(40)  VALUE             SM2014.2
            029100      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM2014.2
            029200 01  XXINFO.                                                      SM2014.2
            029300     02 FILLER                       PIC X(19)  VALUE             SM2014.2
            029400            "*** INFORMATION ***".                                SM2014.2
            029500     02 INFO-TEXT.                                                SM2014.2
            029600       04 FILLER                     PIC X(8)   VALUE SPACE.      SM2014.2
            029700       04 XXCOMPUTED                 PIC X(20).                   SM2014.2
            029800       04 FILLER                     PIC X(5)   VALUE SPACE.      SM2014.2
            029900       04 XXCORRECT                  PIC X(20).                   SM2014.2
            030000     02 INF-ANSI-REFERENCE           PIC X(48).                   SM2014.2
            030100 01  HYPHEN-LINE.                                                 SM2014.2
            030200     02 FILLER  PIC IS X VALUE IS SPACE.                          SM2014.2
            030300     02 FILLER  PIC IS X(65)    VALUE IS "************************SM2014.2
            030400-    "*****************************************".                 SM2014.2
            030500     02 FILLER  PIC IS X(54)    VALUE IS "************************SM2014.2
            030600-    "******************************".                            SM2014.2
            030700 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM2014.2
            030800     "SM201A".                                                    SM2014.2
            030900 PROCEDURE DIVISION.                                              SM2014.2
            031000 CCVS1 SECTION.                                                   SM2014.2
            031100 OPEN-FILES.                                                      SM2014.2
            031200     OPEN     OUTPUT PRINT-FILE.                                  SM2014.2
            031300     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM2014.2
            031400     MOVE    SPACE TO TEST-RESULTS.                               SM2014.2
            031500     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM2014.2
            031600     GO TO CCVS1-EXIT.                                            SM2014.2
            031700 CLOSE-FILES.                                                     SM2014.2
            031800     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM2014.2
            031900 TERMINATE-CCVS.                                                  SM2014.2
            032000*    EXIT PROGRAM.                                                SM2014.2
            032100*TERMINATE-CALL.                                                  SM2014.2
            032200     STOP     RUN.                                                SM2014.2
            032300 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM2014.2
            032400 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM2014.2
            032500 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM2014.2
            032600 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM2014.2
            032700     MOVE "****TEST DELETED****" TO RE-MARK.                      SM2014.2
            032800 PRINT-DETAIL.                                                    SM2014.2
            032900     IF REC-CT NOT EQUAL TO ZERO                                  SM2014.2
            033000             MOVE "." TO PARDOT-X                                 SM2014.2
            033100             MOVE REC-CT TO DOTVALUE.                             SM2014.2
            033200     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM2014.2
            033300     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM2014.2
            033400        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM2014.2
            033500          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM2014.2
            033600     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM2014.2
            033700     MOVE SPACE TO CORRECT-X.                                     SM2014.2
            033800     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM2014.2
            033900     MOVE     SPACE TO RE-MARK.                                   SM2014.2
            034000 HEAD-ROUTINE.                                                    SM2014.2
            034100     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2014.2
            034200     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2014.2
            034300     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2014.2
            034400     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2014.2
            034500 COLUMN-NAMES-ROUTINE.                                            SM2014.2
            034600     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2014.2
            034700     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2014.2
            034800     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM2014.2
            034900 END-ROUTINE.                                                     SM2014.2
            035000     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM2014.2
            035100 END-RTN-EXIT.                                                    SM2014.2
            035200     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2014.2
            035300 END-ROUTINE-1.                                                   SM2014.2
            035400      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM2014.2
            035500      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM2014.2
            035600      ADD PASS-COUNTER TO ERROR-HOLD.                             SM2014.2
            035700*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM2014.2
            035800      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM2014.2
            035900      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM2014.2
            036000      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM2014.2
            036100      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM2014.2
            036200  END-ROUTINE-12.                                                 SM2014.2
            036300      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM2014.2
            036400     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM2014.2
            036500         MOVE "NO " TO ERROR-TOTAL                                SM2014.2
            036600         ELSE                                                     SM2014.2
            036700         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM2014.2
            036800     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM2014.2
            036900     PERFORM WRITE-LINE.                                          SM2014.2
            037000 END-ROUTINE-13.                                                  SM2014.2
            037100     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM2014.2
            037200         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM2014.2
            037300         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM2014.2
            037400     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM2014.2
            037500     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2014.2
            037600      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM2014.2
            037700          MOVE "NO " TO ERROR-TOTAL                               SM2014.2
            037800      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM2014.2
            037900      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM2014.2
            038000      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM2014.2
            038100     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2014.2
            038200 WRITE-LINE.                                                      SM2014.2
            038300     ADD 1 TO RECORD-COUNT.                                       SM2014.2
            038400*    IF RECORD-COUNT GREATER 50                                   SM2014.2
            038500*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM2014.2
            038600*        MOVE SPACE TO DUMMY-RECORD                               SM2014.2
            038700*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM2014.2
            038800*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM2014.2
            038900*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM2014.2
            039000*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM2014.2
            039100*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM2014.2
            039200*        MOVE ZERO TO RECORD-COUNT.                               SM2014.2
            039300     PERFORM WRT-LN.                                              SM2014.2
            039400 WRT-LN.                                                          SM2014.2
            039500     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM2014.2
            039600     MOVE SPACE TO DUMMY-RECORD.                                  SM2014.2
            039700 BLANK-LINE-PRINT.                                                SM2014.2
            039800     PERFORM WRT-LN.                                              SM2014.2
            039900 FAIL-ROUTINE.                                                    SM2014.2
            040000     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM2014.2
            040100     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM2014.2
            040200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2014.2
            040300     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM2014.2
            040400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2014.2
            040500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2014.2
            040600     GO TO  FAIL-ROUTINE-EX.                                      SM2014.2
            040700 FAIL-ROUTINE-WRITE.                                              SM2014.2
            040800     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM2014.2
            040900     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM2014.2
            041000     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM2014.2
            041100     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM2014.2
            041200 FAIL-ROUTINE-EX. EXIT.                                           SM2014.2
            041300 BAIL-OUT.                                                        SM2014.2
            041400     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM2014.2
            041500     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM2014.2
            041600 BAIL-OUT-WRITE.                                                  SM2014.2
            041700     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM2014.2
            041800     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2014.2
            041900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2014.2
            042000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2014.2
            042100 BAIL-OUT-EX. EXIT.                                               SM2014.2
            042200 CCVS1-EXIT.                                                      SM2014.2
            042300     EXIT.                                                        SM2014.2
            042400 INITIALIZATION SECTION.                                          SM2014.2
            042500 SM201A-INIT.                                                     SM2014.2
            042600     OPEN     OUTPUT TEST-FILE.                                   SM2014.2
            042700     MOVE     "OUTPUT OF SM201A IS USED AS" TO RE-MARK.           SM2014.2
            042800     PERFORM  PRINT-DETAIL.                                       SM2014.2
            042900     MOVE     "INPUT FOR SM202A."           TO RE-MARK.           SM2014.2
            043000     PERFORM  PRINT-DETAIL.                                       SM2014.2
            043100     MOVE     "COPY 01 LEVEL --- " TO FEATURE.                    SM2014.2
            043200     PERFORM  PRINT-DETAIL.                                       SM2014.2
            043300 WORKING-STORAGE-TEST SECTION.                                    SM2014.2
            043400 COPY-TEST-1.                                                     SM2014.2
            043500     IF       WSTR-1A EQUAL TO WSTR999                            SM2014.2
            043600              PERFORM PASS GO TO COPY-WRITE-1.                    SM2014.2
            043700*        NOTE TESTS COPYING WITH REPLACEMENT IN WORKING-STORAGE.  SM2014.2
            043800     GO       TO COPY-FAIL-1.                                     SM2014.2
            043900 COPY-DELETE-1.                                                   SM2014.2
            044000     PERFORM  DE-LETE.                                            SM2014.2
            044100     GO       TO COPY-WRITE-1.                                    SM2014.2
            044200 COPY-FAIL-1.                                                     SM2014.2
            044300     MOVE     WSTR999 TO COMPUTED-A.                              SM2014.2
            044400     MOVE     "ABC" TO CORRECT-A.                                 SM2014.2
            044500     PERFORM  FAIL.                                               SM2014.2
            044600 COPY-WRITE-1.                                                    SM2014.2
            044700     MOVE     "  REPLACING" TO FEATURE.                           SM2014.2
            044800     MOVE     "COPY-TEST-1 " TO PAR-NAME.                         SM2014.2
            044900     PERFORM  PRINT-DETAIL.                                       SM2014.2
            045000 COPY-TEST-2.                                                     SM2014.2
            045100     IF       WSTR-2A EQUAL TO "ABC"                              SM2014.2
            045200              PERFORM PASS GO TO COPY-WRITE-2.                    SM2014.2
            045300*    NOTE     TESTS ORDINARY COPY OF ENTRIES WHICH ARE ALSO COPIEDSM2014.2
            045400*             BY COPY REPLACING.                                  SM2014.2
            045500     GO       TO COPY-FAIL-2.                                     SM2014.2
            045600 COPY-DELETE-2.                                                   SM2014.2
            045700     PERFORM  DE-LETE.                                            SM2014.2
            045800     GO       TO COPY-WRITE-2.                                    SM2014.2
            045900 COPY-FAIL-2.                                                     SM2014.2
            046000     MOVE     WSTR-2A TO COMPUTED-A.                              SM2014.2
            046100     MOVE     "ABC" TO CORRECT-A.                                 SM2014.2
            046200     PERFORM  FAIL.                                               SM2014.2
            046300 COPY-WRITE-2.                                                    SM2014.2
            046400     MOVE     "  (NO REPLACING)" TO FEATURE.                      SM2014.2
            046500     MOVE     "COPY-TEST-2 " TO PAR-NAME.                         SM2014.2
            046600     PERFORM  PRINT-DETAIL.                                       SM2014.2
            046700 COPY-INIT-A.                                                     SM2014.2
            046800     MOVE     "  REPLACING" TO FEATURE.                           SM2014.2
            046900 COPY-TEST-3.                                                     SM2014.2
            047000     IF       WSTR91 EQUAL TO "ABC"                               SM2014.2
            047100              PERFORM PASS GO TO COPY-WRITE-3.                    SM2014.2
            047200*        NOTE COPY-TEST-3, 4, AND 5 TEST COPYING WITH A           SM2014.2
            047300*             SERIES OF REPLACEMENTS.                             SM2014.2
            047400     GO       TO COPY-FAIL-3.                                     SM2014.2
            047500 COPY-DELETE-3.                                                   SM2014.2
            047600     PERFORM  DE-LETE.                                            SM2014.2
            047700     GO       TO COPY-WRITE-3.                                    SM2014.2
            047800 COPY-FAIL-3.                                                     SM2014.2
            047900     MOVE     WSTR91 TO COMPUTED-A.                               SM2014.2
            048000     MOVE     "ABC" TO CORRECT-A.                                 SM2014.2
            048100     PERFORM  FAIL.                                               SM2014.2
            048200 COPY-WRITE-3.                                                    SM2014.2
            048300     MOVE     "COPY-TEST-3 " TO PAR-NAME.                         SM2014.2
            048400     PERFORM  PRINT-DETAIL.                                       SM2014.2
            048500 COPY-TEST-4.                                                     SM2014.2
            048600     IF       WSTR92 EQUAL TO "DEF"                               SM2014.2
            048700              PERFORM PASS GO TO COPY-WRITE-4.                    SM2014.2
            048800     GO       TO COPY-FAIL-4.                                     SM2014.2
            048900 COPY-DELETE-4.                                                   SM2014.2
            049000     PERFORM  DE-LETE.                                            SM2014.2
            049100     GO       TO COPY-WRITE-4.                                    SM2014.2
            049200 COPY-FAIL-4.                                                     SM2014.2
            049300     MOVE     WSTR92 TO COMPUTED-A.                               SM2014.2
            049400     MOVE     "DEF" TO CORRECT-A.                                 SM2014.2
            049500     PERFORM  FAIL.                                               SM2014.2
            049600 COPY-WRITE-4.                                                    SM2014.2
            049700     MOVE     "COPY-TEST-4 " TO PAR-NAME.                         SM2014.2
            049800     PERFORM  PRINT-DETAIL.                                       SM2014.2
            049900 COPY-TEST-5.                                                     SM2014.2
            050000     IF       WSTR93 EQUAL TO "GHI"                               SM2014.2
            050100              PERFORM PASS GO TO COPY-WRITE-5.                    SM2014.2
            050200     GO       TO COPY-FAIL-5.                                     SM2014.2
            050300 COPY-DELETE-5.                                                   SM2014.2
            050400     PERFORM  DE-LETE.                                            SM2014.2
            050500     GO       TO COPY-WRITE-5.                                    SM2014.2
            050600 COPY-FAIL-5.                                                     SM2014.2
            050700     MOVE     WSTR93 TO COMPUTED-A.                               SM2014.2
            050800     MOVE     "GHI" TO CORRECT-A.                                 SM2014.2
            050900     PERFORM  FAIL.                                               SM2014.2
            051000 COPY-WRITE-5.                                                    SM2014.2
            051100     MOVE     "COPY-TEST-5 " TO PAR-NAME.                         SM2014.2
            051200     PERFORM  PRINT-DETAIL.                                       SM2014.2
            051300 COPY-INIT-B.                                                     SM2014.2
            051400     MOVE     "  (NOT REPLACING)" TO FEATURE.                     SM2014.2
            051500 COPY-TEST-6.                                                     SM2014.2
            051600     IF       WSTR4A EQUAL TO "ABC"                               SM2014.2
            051700              PERFORM PASS GO TO COPY-WRITE-6.                    SM2014.2
            051800*        NOTE COPY-TEST-6, 7, AND 8 TEST ORDINARY COPYING OF      SM2014.2
            051900*             ENTRIES WHICH ARE ALSO COPIED WITH REPLACEMENT.     SM2014.2
            052000     GO       TO COPY-FAIL-6.                                     SM2014.2
            052100 COPY-DELETE-6.                                                   SM2014.2
            052200     PERFORM  DE-LETE.                                            SM2014.2
            052300     GO       TO COPY-WRITE-6.                                    SM2014.2
            052400 COPY-FAIL-6.                                                     SM2014.2
            052500     MOVE     WSTR4A TO COMPUTED-A.                               SM2014.2
            052600     MOVE     "ABC" TO CORRECT-A.                                 SM2014.2
            052700     PERFORM  FAIL.                                               SM2014.2
            052800 COPY-WRITE-6.                                                    SM2014.2
            052900     MOVE     "COPY-TEST-6 " TO PAR-NAME.                         SM2014.2
            053000     PERFORM  PRINT-DETAIL.                                       SM2014.2
            053100 COPY-TEST-7.                                                     SM2014.2
            053200     IF       WSTR4B EQUAL TO "DEF"                               SM2014.2
            053300              PERFORM PASS GO TO COPY-WRITE-7.                    SM2014.2
            053400     GO       TO COPY-FAIL-7.                                     SM2014.2
            053500 COPY-DELETE-7.                                                   SM2014.2
            053600     PERFORM  DE-LETE.                                            SM2014.2
            053700     GO       TO COPY-WRITE-7.                                    SM2014.2
            053800 COPY-FAIL-7.                                                     SM2014.2
            053900     MOVE     WSTR4B TO COMPUTED-A.                               SM2014.2
            054000     MOVE     "DEF" TO CORRECT-A.                                 SM2014.2
            054100     PERFORM  FAIL.                                               SM2014.2
            054200 COPY-WRITE-7.                                                    SM2014.2
            054300     MOVE     "COPY-TEST-7 " TO PAR-NAME.                         SM2014.2
            054400     PERFORM  PRINT-DETAIL.                                       SM2014.2
            054500 COPY-TEST-8.                                                     SM2014.2
            054600     IF       WSTR4C EQUAL TO "GHI"                               SM2014.2
            054700              PERFORM PASS GO TO COPY-WRITE-8.                    SM2014.2
            054800     GO       TO COPY-FAIL-8.                                     SM2014.2
            054900 COPY-DELETE-8.                                                   SM2014.2
            055000     PERFORM  DE-LETE.                                            SM2014.2
            055100     GO       TO COPY-WRITE-8.                                    SM2014.2
            055200 COPY-FAIL-8.                                                     SM2014.2
            055300     MOVE     WSTR4C TO COMPUTED-A.                               SM2014.2
            055400     MOVE     "GHI" TO CORRECT-A.                                 SM2014.2
            055500     PERFORM  FAIL.                                               SM2014.2
            055600 COPY-WRITE-8.                                                    SM2014.2
            055700     MOVE     "COPY-TEST-8 " TO PAR-NAME.                         SM2014.2
            055800     PERFORM  PRINT-DETAIL.                                       SM2014.2
            055900 PARAGRAPH-TEST SECTION.                                          SM2014.2
            056000 COPY-TEST-9.                                                     SM2014.2
            056100                                                                  SM2014.2
            056200                                                                  SM2014.2
            056300                                                                  SM2014.2
            056400                                                                  SM2014.2
            056500                                                                  SM2014.2
            056600*                                                                 SM2014.2
            056700*********************** COPY STATEMENT USED **********************SM2014.2
            056800*                                                                 SM2014.2
            056900*                                                      COPY K1PRB SM2014.2
            057000*             REPLACING WSTR4C BY WSTR4B.                         SM2014.2
            057100*                                                                 SM2014.2
            057200******************** COPIED TEXT BEGINS BELOW ********************SM2014.2
            057300                                                       COPY K1PRB SM2014.2
            057400              REPLACING WSTR4C BY WSTR4B.                         SM2014.2
            057500*********************** END OF COPIED TEXT ***********************SM2014.2
            057600*    NOTE     COPY A PROCEDURE WHICH REFERENCES COPIED DATA.      SM2014.2
            057700     IF       WSTR-4 EQUAL TO "DEFABCDEF"                         SM2014.2
            057800              PERFORM PASS GO TO COPY-WRITE-9.                    SM2014.2
            057900     GO       TO COPY-FAIL-9.                                     SM2014.2
            058000 COPY-DELETE-9.                                                   SM2014.2
            058100     PERFORM  DE-LETE.                                            SM2014.2
            058200     GO       TO COPY-WRITE-9.                                    SM2014.2
            058300 COPY-FAIL-9.                                                     SM2014.2
            058400     MOVE     WSTR-4 TO COMPUTED-A.                               SM2014.2
            058500     MOVE     "DEFABCDEF" TO CORRECT-A.                           SM2014.2
            058600     PERFORM  FAIL.                                               SM2014.2
            058700 COPY-WRITE-9.                                                    SM2014.2
            058800     MOVE     "COPY PARA REPLACING" TO FEATURE.                   SM2014.2
            058900     MOVE     "COPY-TEST-9 " TO PAR-NAME.                         SM2014.2
            059000     PERFORM  PRINT-DETAIL.                                       SM2014.2
            059100 BUILD SECTION.                                                   SM2014.2
            059200 COPY-TEST-10.                                                    SM2014.2
            059300     MOVE     RCD-1 TO TF-1.                                      SM2014.2
            059400     WRITE    TST-TEST.                                           SM2014.2
            059500     MOVE     RCD-2 TO TF-1.                                      SM2014.2
            059600     WRITE    TST-TEST.                                           SM2014.2
            059700     MOVE     RCD-3 TO TF-1.                                      SM2014.2
            059800     WRITE    TST-TEST.                                           SM2014.2
            059900     MOVE     RCD-4 TO TF-1.                                      SM2014.2
            060000     WRITE    TST-TEST.                                           SM2014.2
            060100     MOVE     RCD-5 TO TF-1.                                      SM2014.2
            060200     WRITE    TST-TEST.                                           SM2014.2
            060300     MOVE     RCD-6 TO TF-1.                                      SM2014.2
            060400     WRITE    TST-TEST.                                           SM2014.2
            060500     MOVE     RCD-7 TO TF-1.                                      SM2014.2
            060600     WRITE    TST-TEST.                                           SM2014.2
            060700     PERFORM  PASS.                                               SM2014.2
            060800     GO       TO COPY-WRITE-10.                                   SM2014.2
            060900 COPY-DELETE-10.                                                  SM2014.2
            061000     PERFORM  DE-LETE.                                            SM2014.2
            061100 COPY-WRITE-10.                                                   SM2014.2
            061200     MOVE     "COPY FD REPLACING" TO FEATURE.                     SM2014.2
            061300     MOVE     "COPY-TEST-10 " TO PAR-NAME.                        SM2014.2
            061400     MOVE     "OUTPUT PASSED ONTO SM202" TO RE-MARK.              SM2014.2
            061500     PERFORM  PRINT-DETAIL.                                       SM2014.2
            061600     CLOSE    TEST-FILE.                                          SM2014.2
            061700 MORE-TESTS SECTION.                                              SM2014.2
            061800 COPY-TEST-11.                                                    SM2014.2
            061900     MOVE SPACES TO TEXT-TEST-1.                                  SM2014.2
            062000     MOVE 12345 TO TXT-FLD-1.                                     SM2014.2
            062100     IF TEXT-TEST-1 IS EQUAL TO "                                 SM2014.2
            062200-    "                                                            SM2014.2
            062300-    "                      12345"                                SM2014.2
            062400         PERFORM PASS  ELSE  PERFORM FAIL.                        SM2014.2
            062500     GO TO COPY-WRITE-11.                                         SM2014.2
            062600 COPY-DELETE-11.                                                  SM2014.2
            062700     PERFORM DE-LETE.                                             SM2014.2
            062800 COPY-WRITE-11.                                                   SM2014.2
            062900     MOVE "PSEUDO TEXT" TO FEATURE.                               SM2014.2
            063000     MOVE "COPY-TEST-11" TO PAR-NAME.                             SM2014.2
            063100     PERFORM PRINT-DETAIL.                                        SM2014.2
            063200 CCVS-EXIT SECTION.                                               SM2014.2
            063300 CCVS-999999.                                                     SM2014.2
            063400     GO TO CLOSE-FILES.                                           SM2014.2
                  *END-OF,SM201A                                                                  
        """)
    )

    @Test
    fun sm202A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM201A,SUBPRG,SM202A                                              
            000100 IDENTIFICATION DIVISION.                                         SM2024.2
            000200 PROGRAM-ID.                                                      SM2024.2
            000300     SM202A.                                                      SM2024.2
            000400****************************************************************  SM2024.2
            000500*                                                              *  SM2024.2
            000600*    VALIDATION FOR:-                                          *  SM2024.2
            000700*                                                              *  SM2024.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2024.2
            000900*                                                              *  SM2024.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2024.2
            001100*                                                              *  SM2024.2
            001200****************************************************************  SM2024.2
            001300*                                                              *  SM2024.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM2024.2
            001500*                                                              *  SM2024.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM2024.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM2024.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM2024.2
            001900*                                                              *  SM2024.2
            002000****************************************************************  SM2024.2
            002100*                                                              *  SM2024.2
            002200*    PROGRAM SM202A READS THE FILE PRODUCED BY SM201A TO       *  SM2024.2
            002300*    VERIFY THE PROPER EXECUTION OF THE "COPY REPLACING"       *  SM2024.2
            002400*    STATEMENTS IN SM201A.  A NUMBER OF FURTHER TESTS USING    *  SM2024.2
            002500*    VARIOUS NUMERIC AMD ALPHANUMERIC LITERALS, QUALIFIED      *  SM2024.2
            002600*    DATA NAMES AND MULTIPLE "REPLACING" OPERANDS ARE ALSO     *  SM2024.2
            002700*    CARRIED OUT.                                              *  SM2024.2
            002800*                                                              *  SM2024.2
            002900****************************************************************  SM2024.2
            003000 ENVIRONMENT DIVISION.                                            SM2024.2
            003100 CONFIGURATION SECTION.                                           SM2024.2
            003200 SOURCE-COMPUTER.                                                 SM2024.2
            003300     XXXXX082.                                                    SM2024.2
            003400 OBJECT-COMPUTER.                                                 SM2024.2
            003500     XXXXX083.                                                    SM2024.2
            003600 INPUT-OUTPUT SECTION.                                            SM2024.2
            003700 FILE-CONTROL.                                                    SM2024.2
            003800     SELECT PRINT-FILE ASSIGN TO                                  SM2024.2
            003900     XXXXX055.                                                    SM2024.2
            004000     SELECT TEST-FILE ASSIGN TO                                   SM2024.2
            004100     XXXXD001.                                                    SM2024.2
            004200 DATA DIVISION.                                                   SM2024.2
            004300 FILE SECTION.                                                    SM2024.2
            004400 FD  PRINT-FILE.                                                  SM2024.2
            004500 01  PRINT-REC PICTURE X(120).                                    SM2024.2
            004600 01  DUMMY-RECORD PICTURE X(120).                                 SM2024.2
            004700 FD  TEST-FILE                                                    SM2024.2
            004800     LABEL RECORD STANDARD                                        SM2024.2
            004900C    VALUE OF                                                     SM2024.2
            005000C    XXXXX074                                                     SM2024.2
            005100C    IS                                                           SM2024.2
            005200C    XXXXX075                                                     SM2024.2
            005300*    XXXXX069                                                     SM2024.2
            005400     DATA RECORD IS PROOF-REC.                                    SM2024.2
            005500 01  PROOF-REC.                                                   SM2024.2
            005600     02  TF-1 PICTURE 9(5).                                       SM2024.2
            005700     02  FILLER PICTURE X(115).                                   SM2024.2
            005800 WORKING-STORAGE SECTION.                                         SM2024.2
            005900 01  COUNTER-16 PICTURE 9 VALUE 1.                                SM2024.2
            006000 01  TOTAL-AREA.                                                  SM2024.2
            006100     02 AREA-1          PICTURE AAAAA.                            SM2024.2
            006200     02 AREA-2          PICTURE XXXXB.                            SM2024.2
            006300     02 AREA-3          PICTURE XXXXX.                            SM2024.2
            006400     02 AREA-4          PICTURE ZZZZZ.                            SM2024.2
            006500 01  MISLEADING-DATA.                                             SM2024.2
            006600     02 FALSE-DATA-1    PICTURE AAAAA VALUE "FALSE".              SM2024.2
            006700     02 FALSE-DATA-2    PICTURE XXXXX VALUE " TENT".              SM2024.2
            006800     02 FALSE-DATA-3    PICTURE XXXXX VALUE "- 5 =".              SM2024.2
            006900     02 FALSE-DATA-4    PICTURE 99999 VALUE 00012.                SM2024.2
            007000 01  QUALIFIED-DATA.                                              SM2024.2
            007100     02 TRUE-Q-02.                                                SM2024.2
            007200         03 TRUE-Q-03.                                            SM2024.2
            007300             04 TRUE-Q-04 PICTURE A(5) VALUE "TRUE ".             SM2024.2
            007400         03 FALSE-Q-03.                                           SM2024.2
            007500             04 TRUE-Q-04 PICTURE A(5) VALUE "FIGHT".             SM2024.2
            007600     02 FALSE-Q-02.                                               SM2024.2
            007700         03 TRUE-Q-03.                                            SM2024.2
            007800             04 TRUE-Q-04 PICTURE A(5) VALUE "DRIVE".             SM2024.2
            007900         03 FALSE-Q-03.                                           SM2024.2
            008000             04 TRUE-Q-04 PICTURE A(5) VALUE "THROW".             SM2024.2
            008100 01  RE-SUB-DATA        PICTURE X(40) VALUE                       SM2024.2
            008200     "ABCDEFGHIJKLMNOPQRST+ 2 =UVWXYZYXWVUTSRQ".                  SM2024.2
            008300 01  SUBSCRIPTED-DATA REDEFINES RE-SUB-DATA.                      SM2024.2
            008400     02 X OCCURS 2 TIMES.                                         SM2024.2
            008500         03 Y OCCURS 2 TIMES.                                     SM2024.2
            008600             04 Z OCCURS 2 TIMES PICTURE X(5).                    SM2024.2
            008700 01  TEST-RESULTS.                                                SM2024.2
            008800     02 FILLER                   PIC X      VALUE SPACE.          SM2024.2
            008900     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM2024.2
            009000     02 FILLER                   PIC X      VALUE SPACE.          SM2024.2
            009100     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM2024.2
            009200     02 FILLER                   PIC X      VALUE SPACE.          SM2024.2
            009300     02  PAR-NAME.                                                SM2024.2
            009400       03 FILLER                 PIC X(19)  VALUE SPACE.          SM2024.2
            009500       03  PARDOT-X              PIC X      VALUE SPACE.          SM2024.2
            009600       03 DOTVALUE               PIC 99     VALUE ZERO.           SM2024.2
            009700     02 FILLER                   PIC X(8)   VALUE SPACE.          SM2024.2
            009800     02 RE-MARK                  PIC X(61).                       SM2024.2
            009900 01  TEST-COMPUTED.                                               SM2024.2
            010000     02 FILLER                   PIC X(30)  VALUE SPACE.          SM2024.2
            010100     02 FILLER                   PIC X(17)  VALUE                 SM2024.2
            010200            "       COMPUTED=".                                   SM2024.2
            010300     02 COMPUTED-X.                                               SM2024.2
            010400     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM2024.2
            010500     03 COMPUTED-N               REDEFINES COMPUTED-A             SM2024.2
            010600                                 PIC -9(9).9(9).                  SM2024.2
            010700     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM2024.2
            010800     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM2024.2
            010900     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM2024.2
            011000     03       CM-18V0 REDEFINES COMPUTED-A.                       SM2024.2
            011100         04 COMPUTED-18V0                    PIC -9(18).          SM2024.2
            011200         04 FILLER                           PIC X.               SM2024.2
            011300     03 FILLER PIC X(50) VALUE SPACE.                             SM2024.2
            011400 01  TEST-CORRECT.                                                SM2024.2
            011500     02 FILLER PIC X(30) VALUE SPACE.                             SM2024.2
            011600     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM2024.2
            011700     02 CORRECT-X.                                                SM2024.2
            011800     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM2024.2
            011900     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM2024.2
            012000     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM2024.2
            012100     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM2024.2
            012200     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM2024.2
            012300     03      CR-18V0 REDEFINES CORRECT-A.                         SM2024.2
            012400         04 CORRECT-18V0                     PIC -9(18).          SM2024.2
            012500         04 FILLER                           PIC X.               SM2024.2
            012600     03 FILLER PIC X(2) VALUE SPACE.                              SM2024.2
            012700     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM2024.2
            012800 01  CCVS-C-1.                                                    SM2024.2
            012900     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM2024.2
            013000-    "SS  PARAGRAPH-NAME                                          SM2024.2
            013100-    "       REMARKS".                                            SM2024.2
            013200     02 FILLER                     PIC X(20)    VALUE SPACE.      SM2024.2
            013300 01  CCVS-C-2.                                                    SM2024.2
            013400     02 FILLER                     PIC X        VALUE SPACE.      SM2024.2
            013500     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM2024.2
            013600     02 FILLER                     PIC X(15)    VALUE SPACE.      SM2024.2
            013700     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM2024.2
            013800     02 FILLER                     PIC X(94)    VALUE SPACE.      SM2024.2
            013900 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM2024.2
            014000 01  REC-CT                        PIC 99       VALUE ZERO.       SM2024.2
            014100 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM2024.2
            014200 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM2024.2
            014300 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM2024.2
            014400 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM2024.2
            014500 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM2024.2
            014600 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM2024.2
            014700 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM2024.2
            014800 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM2024.2
            014900 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM2024.2
            015000 01  CCVS-H-1.                                                    SM2024.2
            015100     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2024.2
            015200     02  FILLER                    PIC X(42)    VALUE             SM2024.2
            015300     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM2024.2
            015400     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2024.2
            015500 01  CCVS-H-2A.                                                   SM2024.2
            015600   02  FILLER                        PIC X(40)  VALUE SPACE.      SM2024.2
            015700   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM2024.2
            015800   02  FILLER                        PIC XXXX   VALUE             SM2024.2
            015900     "4.2 ".                                                      SM2024.2
            016000   02  FILLER                        PIC X(28)  VALUE             SM2024.2
            016100            " COPY - NOT FOR DISTRIBUTION".                       SM2024.2
            016200   02  FILLER                        PIC X(41)  VALUE SPACE.      SM2024.2
            016300                                                                  SM2024.2
            016400 01  CCVS-H-2B.                                                   SM2024.2
            016500   02  FILLER                        PIC X(15)  VALUE             SM2024.2
            016600            "TEST RESULT OF ".                                    SM2024.2
            016700   02  TEST-ID                       PIC X(9).                    SM2024.2
            016800   02  FILLER                        PIC X(4)   VALUE             SM2024.2
            016900            " IN ".                                               SM2024.2
            017000   02  FILLER                        PIC X(12)  VALUE             SM2024.2
            017100     " HIGH       ".                                              SM2024.2
            017200   02  FILLER                        PIC X(22)  VALUE             SM2024.2
            017300            " LEVEL VALIDATION FOR ".                             SM2024.2
            017400   02  FILLER                        PIC X(58)  VALUE             SM2024.2
            017500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2024.2
            017600 01  CCVS-H-3.                                                    SM2024.2
            017700     02  FILLER                      PIC X(34)  VALUE             SM2024.2
            017800            " FOR OFFICIAL USE ONLY    ".                         SM2024.2
            017900     02  FILLER                      PIC X(58)  VALUE             SM2024.2
            018000     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2024.2
            018100     02  FILLER                      PIC X(28)  VALUE             SM2024.2
            018200            "  COPYRIGHT   1985 ".                                SM2024.2
            018300 01  CCVS-E-1.                                                    SM2024.2
            018400     02 FILLER                       PIC X(52)  VALUE SPACE.      SM2024.2
            018500     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM2024.2
            018600     02 ID-AGAIN                     PIC X(9).                    SM2024.2
            018700     02 FILLER                       PIC X(45)  VALUE SPACES.     SM2024.2
            018800 01  CCVS-E-2.                                                    SM2024.2
            018900     02  FILLER                      PIC X(31)  VALUE SPACE.      SM2024.2
            019000     02  FILLER                      PIC X(21)  VALUE SPACE.      SM2024.2
            019100     02 CCVS-E-2-2.                                               SM2024.2
            019200         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM2024.2
            019300         03 FILLER                   PIC X      VALUE SPACE.      SM2024.2
            019400         03 ENDER-DESC               PIC X(44)  VALUE             SM2024.2
            019500            "ERRORS ENCOUNTERED".                                 SM2024.2
            019600 01  CCVS-E-3.                                                    SM2024.2
            019700     02  FILLER                      PIC X(22)  VALUE             SM2024.2
            019800            " FOR OFFICIAL USE ONLY".                             SM2024.2
            019900     02  FILLER                      PIC X(12)  VALUE SPACE.      SM2024.2
            020000     02  FILLER                      PIC X(58)  VALUE             SM2024.2
            020100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2024.2
            020200     02  FILLER                      PIC X(13)  VALUE SPACE.      SM2024.2
            020300     02 FILLER                       PIC X(15)  VALUE             SM2024.2
            020400             " COPYRIGHT 1985".                                   SM2024.2
            020500 01  CCVS-E-4.                                                    SM2024.2
            020600     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM2024.2
            020700     02 FILLER                       PIC X(4)   VALUE " OF ".     SM2024.2
            020800     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM2024.2
            020900     02 FILLER                       PIC X(40)  VALUE             SM2024.2
            021000      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM2024.2
            021100 01  XXINFO.                                                      SM2024.2
            021200     02 FILLER                       PIC X(19)  VALUE             SM2024.2
            021300            "*** INFORMATION ***".                                SM2024.2
            021400     02 INFO-TEXT.                                                SM2024.2
            021500       04 FILLER                     PIC X(8)   VALUE SPACE.      SM2024.2
            021600       04 XXCOMPUTED                 PIC X(20).                   SM2024.2
            021700       04 FILLER                     PIC X(5)   VALUE SPACE.      SM2024.2
            021800       04 XXCORRECT                  PIC X(20).                   SM2024.2
            021900     02 INF-ANSI-REFERENCE           PIC X(48).                   SM2024.2
            022000 01  HYPHEN-LINE.                                                 SM2024.2
            022100     02 FILLER  PIC IS X VALUE IS SPACE.                          SM2024.2
            022200     02 FILLER  PIC IS X(65)    VALUE IS "************************SM2024.2
            022300-    "*****************************************".                 SM2024.2
            022400     02 FILLER  PIC IS X(54)    VALUE IS "************************SM2024.2
            022500-    "******************************".                            SM2024.2
            022600 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM2024.2
            022700     "SM202A".                                                    SM2024.2
            022800 PROCEDURE DIVISION.                                              SM2024.2
            022900 CCVS1 SECTION.                                                   SM2024.2
            023000 OPEN-FILES.                                                      SM2024.2
            023100     OPEN     OUTPUT PRINT-FILE.                                  SM2024.2
            023200     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM2024.2
            023300     MOVE    SPACE TO TEST-RESULTS.                               SM2024.2
            023400     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM2024.2
            023500     GO TO CCVS1-EXIT.                                            SM2024.2
            023600 CLOSE-FILES.                                                     SM2024.2
            023700     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM2024.2
            023800 TERMINATE-CCVS.                                                  SM2024.2
            023900*    EXIT PROGRAM.                                                SM2024.2
            024000*TERMINATE-CALL.                                                  SM2024.2
            024100     STOP     RUN.                                                SM2024.2
            024200 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM2024.2
            024300 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM2024.2
            024400 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM2024.2
            024500 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM2024.2
            024600     MOVE "****TEST DELETED****" TO RE-MARK.                      SM2024.2
            024700 PRINT-DETAIL.                                                    SM2024.2
            024800     IF REC-CT NOT EQUAL TO ZERO                                  SM2024.2
            024900             MOVE "." TO PARDOT-X                                 SM2024.2
            025000             MOVE REC-CT TO DOTVALUE.                             SM2024.2
            025100     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM2024.2
            025200     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM2024.2
            025300        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM2024.2
            025400          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM2024.2
            025500     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM2024.2
            025600     MOVE SPACE TO CORRECT-X.                                     SM2024.2
            025700     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM2024.2
            025800     MOVE     SPACE TO RE-MARK.                                   SM2024.2
            025900 HEAD-ROUTINE.                                                    SM2024.2
            026000     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2024.2
            026100     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2024.2
            026200     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2024.2
            026300     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2024.2
            026400 COLUMN-NAMES-ROUTINE.                                            SM2024.2
            026500     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2024.2
            026600     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2024.2
            026700     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM2024.2
            026800 END-ROUTINE.                                                     SM2024.2
            026900     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM2024.2
            027000 END-RTN-EXIT.                                                    SM2024.2
            027100     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2024.2
            027200 END-ROUTINE-1.                                                   SM2024.2
            027300      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM2024.2
            027400      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM2024.2
            027500      ADD PASS-COUNTER TO ERROR-HOLD.                             SM2024.2
            027600*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM2024.2
            027700      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM2024.2
            027800      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM2024.2
            027900      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM2024.2
            028000      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM2024.2
            028100  END-ROUTINE-12.                                                 SM2024.2
            028200      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM2024.2
            028300     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM2024.2
            028400         MOVE "NO " TO ERROR-TOTAL                                SM2024.2
            028500         ELSE                                                     SM2024.2
            028600         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM2024.2
            028700     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM2024.2
            028800     PERFORM WRITE-LINE.                                          SM2024.2
            028900 END-ROUTINE-13.                                                  SM2024.2
            029000     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM2024.2
            029100         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM2024.2
            029200         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM2024.2
            029300     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM2024.2
            029400     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2024.2
            029500      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM2024.2
            029600          MOVE "NO " TO ERROR-TOTAL                               SM2024.2
            029700      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM2024.2
            029800      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM2024.2
            029900      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM2024.2
            030000     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2024.2
            030100 WRITE-LINE.                                                      SM2024.2
            030200     ADD 1 TO RECORD-COUNT.                                       SM2024.2
            030300*    IF RECORD-COUNT GREATER 50                                   SM2024.2
            030400*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM2024.2
            030500*        MOVE SPACE TO DUMMY-RECORD                               SM2024.2
            030600*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM2024.2
            030700*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM2024.2
            030800*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM2024.2
            030900*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM2024.2
            031000*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM2024.2
            031100*        MOVE ZERO TO RECORD-COUNT.                               SM2024.2
            031200     PERFORM WRT-LN.                                              SM2024.2
            031300 WRT-LN.                                                          SM2024.2
            031400     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM2024.2
            031500     MOVE SPACE TO DUMMY-RECORD.                                  SM2024.2
            031600 BLANK-LINE-PRINT.                                                SM2024.2
            031700     PERFORM WRT-LN.                                              SM2024.2
            031800 FAIL-ROUTINE.                                                    SM2024.2
            031900     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM2024.2
            032000     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM2024.2
            032100     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2024.2
            032200     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM2024.2
            032300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2024.2
            032400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2024.2
            032500     GO TO  FAIL-ROUTINE-EX.                                      SM2024.2
            032600 FAIL-ROUTINE-WRITE.                                              SM2024.2
            032700     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM2024.2
            032800     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM2024.2
            032900     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM2024.2
            033000     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM2024.2
            033100 FAIL-ROUTINE-EX. EXIT.                                           SM2024.2
            033200 BAIL-OUT.                                                        SM2024.2
            033300     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM2024.2
            033400     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM2024.2
            033500 BAIL-OUT-WRITE.                                                  SM2024.2
            033600     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM2024.2
            033700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2024.2
            033800     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2024.2
            033900     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2024.2
            034000 BAIL-OUT-EX. EXIT.                                               SM2024.2
            034100 CCVS1-EXIT.                                                      SM2024.2
            034200     EXIT.                                                        SM2024.2
            034300 INITIALIZATION SECTION.                                          SM2024.2
            034400 SM202A-INIT.                                                     SM2024.2
            034500     OPEN     INPUT TEST-FILE.                                    SM2024.2
            034600     MOVE     "SM202A CHECKS A FILE WHICH" TO RE-MARK.            SM2024.2
            034700     PERFORM  PRINT-DETAIL.                                       SM2024.2
            034800     MOVE     "WAS GENERATED IN SM201A."   TO RE-MARK.            SM2024.2
            034900     PERFORM  PRINT-DETAIL.                                       SM2024.2
            035000     MOVE     "COPY FD REPLACING" TO FEATURE.                     SM2024.2
            035100 FD-TEST SECTION.                                                 SM2024.2
            035200 COPY-TEST-11.                                                    SM2024.2
            035300     PERFORM  READ-TEST-FILE.                                     SM2024.2
            035400     IF       TF-1 EQUAL TO 97532                                 SM2024.2
            035500              PERFORM PASS GO TO COPY-WRITE-11.                   SM2024.2
            035600     GO       TO COPY-FAIL-11.                                    SM2024.2
            035700 COPY-DELETE-11.                                                  SM2024.2
            035800     PERFORM  DE-LETE.                                            SM2024.2
            035900     GO       TO COPY-WRITE-11.                                   SM2024.2
            036000 COPY-FAIL-11.                                                    SM2024.2
            036100     MOVE     TF-1 TO COMPUTED-N.                                 SM2024.2
            036200     MOVE     97532 TO CORRECT-N.                                 SM2024.2
            036300     PERFORM  FAIL.                                               SM2024.2
            036400 COPY-WRITE-11.                                                   SM2024.2
            036500     MOVE     "COPY-TEST-11 " TO PAR-NAME.                        SM2024.2
            036600     PERFORM  PRINT-DETAIL.                                       SM2024.2
            036700 COPY-TEST-12.                                                    SM2024.2
            036800     PERFORM  READ-TEST-FILE.                                     SM2024.2
            036900     IF       TF-1 EQUAL TO 23479                                 SM2024.2
            037000              PERFORM PASS GO TO COPY-WRITE-12.                   SM2024.2
            037100     GO       TO COPY-FAIL-12.                                    SM2024.2
            037200 COPY-DELETE-12.                                                  SM2024.2
            037300     PERFORM  DE-LETE.                                            SM2024.2
            037400     GO       TO COPY-WRITE-12.                                   SM2024.2
            037500 COPY-FAIL-12.                                                    SM2024.2
            037600     MOVE     TF-1 TO COMPUTED-N.                                 SM2024.2
            037700     MOVE     23479 TO CORRECT-N.                                 SM2024.2
            037800     PERFORM  FAIL.                                               SM2024.2
            037900 COPY-WRITE-12.                                                   SM2024.2
            038000     MOVE     "COPY-TEST-12 " TO PAR-NAME.                        SM2024.2
            038100     PERFORM  PRINT-DETAIL.                                       SM2024.2
            038200 COPY-TEST-13.                                                    SM2024.2
            038300     PERFORM  READ-TEST-FILE 3 TIMES.                             SM2024.2
            038400     IF       TF-1 EQUAL TO 14003                                 SM2024.2
            038500              PERFORM PASS GO TO COPY-WRITE-13.                   SM2024.2
            038600     GO       TO COPY-FAIL-13.                                    SM2024.2
            038700 COPY-DELETE-13.                                                  SM2024.2
            038800     PERFORM  DE-LETE.                                            SM2024.2
            038900     GO       TO COPY-WRITE-13.                                   SM2024.2
            039000 COPY-FAIL-13.                                                    SM2024.2
            039100     MOVE     TF-1 TO COMPUTED-N.                                 SM2024.2
            039200     MOVE     14003 TO CORRECT-N.                                 SM2024.2
            039300     PERFORM  FAIL.                                               SM2024.2
            039400 COPY-WRITE-13.                                                   SM2024.2
            039500     MOVE     "COPY-TEST-13 " TO PAR-NAME.                        SM2024.2
            039600     PERFORM  PRINT-DETAIL.                                       SM2024.2
            039700 COPY-TEST-14.                                                    SM2024.2
            039800     PERFORM  READ-TEST-FILE 2 TIMES.                             SM2024.2
            039900     IF       TF-1 EQUAL TO 03543                                 SM2024.2
            040000              PERFORM PASS GO TO COPY-WRITE-14.                   SM2024.2
            040100     GO       TO COPY-FAIL-14.                                    SM2024.2
            040200 COPY-DELETE-14.                                                  SM2024.2
            040300     PERFORM  DE-LETE.                                            SM2024.2
            040400     GO       TO COPY-WRITE-14.                                   SM2024.2
            040500 COPY-FAIL-14.                                                    SM2024.2
            040600     MOVE     TF-1 TO COMPUTED-N.                                 SM2024.2
            040700     MOVE     03543 TO CORRECT-N.                                 SM2024.2
            040800     PERFORM  FAIL.                                               SM2024.2
            040900 COPY-WRITE-14.                                                   SM2024.2
            041000     MOVE     "COPY-TEST-14 " TO PAR-NAME.                        SM2024.2
            041100     PERFORM  PRINT-DETAIL.                                       SM2024.2
            041200 COPY-INIT-A.                                                     SM2024.2
            041300     MOVE     "COPY REPLACING ---  " TO FEATURE.                  SM2024.2
            041400     PERFORM  PRINT-DETAIL.                                       SM2024.2
            041500     MOVE     "  PARAGRAPH-NAMES   " TO FEATURE.                  SM2024.2
            041600 COPY-TEST-15 SECTION.                                            SM2024.2
            041700                                                                  SM2024.2
            041800                                                                  SM2024.2
            041900                                                                  SM2024.2
            042000                                                                  SM2024.2
            042100                                                                  SM2024.2
            042200*                                                                 SM2024.2
            042300*********************** COPY STATEMENT USED **********************SM2024.2
            042400*                                                                 SM2024.2
            042500*                                                      COPY K2SEA SM2024.2
            042600*             REPLACING PARA-X BY PARA-2                          SM2024.2
            042700*                       12345 BY PARA-3                           SM2024.2
            042800*                       DUMMY-PASS BY PASS.                       SM2024.2
            042900*                                                                 SM2024.2
            043000******************** COPIED TEXT BEGINS BELOW ********************SM2024.2
            043100                                                       COPY K2SEA SM2024.2
            043200              REPLACING PARA-X BY PARA-2                          SM2024.2
            043300                        12345 BY PARA-3                           SM2024.2
            043400                        DUMMY-PASS BY PASS.                       SM2024.2
            043500*********************** END OF COPIED TEXT ***********************SM2024.2
            043600 COPY-A-15 SECTION.                                               SM2024.2
            043700 COPY-DELETE-15.                                                  SM2024.2
            043800     PERFORM  DE-LETE.                                            SM2024.2
            043900 COPY-WRITE-15.                                                   SM2024.2
            044000     MOVE     "COPY-TEST-15" TO PAR-NAME.                         SM2024.2
            044100     PERFORM  PRINT-DETAIL.                                       SM2024.2
            044200 COPY-PARA SECTION.                                               SM2024.2
            044300 COPY-INIT-B.                                                     SM2024.2
            044400     MOVE     "  BY LITERALS      " TO FEATURE.                   SM2024.2
            044500 COPY-TEST-16.                                                    SM2024.2
            044600                                                                  SM2024.2
            044700                                                                  SM2024.2
            044800                                                                  SM2024.2
            044900                                                                  SM2024.2
            045000                                                                  SM2024.2
            045100*                                                                 SM2024.2
            045200*********************** COPY STATEMENT USED **********************SM2024.2
            045300*                                                                 SM2024.2
            045400*                                                      COPY K2PRA SM2024.2
            045500*             REPLACING FALSE-DATA-1 BY "TRUE "                   SM2024.2
            045600*                       FALSE-DATA-2 BY " TWO${'$'}"                   SM2024.2
            045700*                       FALSE-DATA-3 BY "+ 2 ="                   SM2024.2
            045800*                       FALSE-DATA-4 BY 4.                        SM2024.2
            045900*                                                                 SM2024.2
            046000******************** COPIED TEXT BEGINS BELOW ********************SM2024.2
            046100                                                       COPY K2PRA SM2024.2
            046200              REPLACING FALSE-DATA-1 BY "TRUE "                   SM2024.2
            046300                        FALSE-DATA-2 BY " TWO${'$'}"                   SM2024.2
            046400                        FALSE-DATA-3 BY "+ 2 ="                   SM2024.2
            046500                        FALSE-DATA-4 BY 4.                        SM2024.2
            046600*********************** END OF COPIED TEXT ***********************SM2024.2
            046700 COPY-DELETE-16.                                                  SM2024.2
            046800     PERFORM  DE-LETE.                                            SM2024.2
            046900 COPY-WRITE-16.                                                   SM2024.2
            047000     IF COUNTER-16 IS EQUAL TO 0                                  SM2024.2
            047100         PERFORM FAIL                                             SM2024.2
            047200         GO TO COPY-WRITE-17                                      SM2024.2
            047300     ELSE                                                         SM2024.2
            047400         SUBTRACT 1 FROM COUNTER-16.                              SM2024.2
            047500     IF       P-OR-F EQUAL TO "FAIL*"                             SM2024.2
            047600              MOVE TOTAL-AREA TO COMPUTED-A                       SM2024.2
            047700              MOVE "TRUE  TWO + 2 =    4" TO CORRECT-A.           SM2024.2
            047800     MOVE     "COPY-TEST-16" TO PAR-NAME.                         SM2024.2
            047900     PERFORM  PRINT-DETAIL.                                       SM2024.2
            048000 COPY-INIT-17.                                                    SM2024.2
            048100     MOVE     SPACE TO TOTAL-AREA.                                SM2024.2
            048200 COPY-TEST-17.                                                    SM2024.2
            048300                                                                  SM2024.2
            048400                                                                  SM2024.2
            048500                                                                  SM2024.2
            048600                                                                  SM2024.2
            048700                                                                  SM2024.2
            048800*                                                                 SM2024.2
            048900*********************** COPY STATEMENT USED **********************SM2024.2
            049000*                                                                 SM2024.2
            049100*                                                      COPY K2PRA SM2024.2
            049200*             REPLACING FALSE-DATA-1 BY TRUE-Q-04 OF TRUE-Q-03    SM2024.2
            049300*                                                 IN TRUE-Q-02    SM2024.2
            049400*                       COPY-WRITE-16 BY COPY-WRITE-17            SM2024.2
            049500*                       FALSE-DATA-2 BY " TWO FIVE "              SM2024.2
            049600*                       FALSE-DATA-3 BY Z(2, 1, 1)                SM2024.2
            049700*                       FALSE-DATA-4 BY +000004.99.               SM2024.2
            049800*                                                                 SM2024.2
            049900******************** COPIED TEXT BEGINS BELOW ********************SM2024.2
            050000                                                       COPY K2PRA SM2024.2
            050100              REPLACING FALSE-DATA-1 BY TRUE-Q-04 OF TRUE-Q-03    SM2024.2
            050200                                                  IN TRUE-Q-02    SM2024.2
            050300                        COPY-WRITE-16 BY COPY-WRITE-17            SM2024.2
            050400                        FALSE-DATA-2 BY " TWO FIVE "              SM2024.2
            050500                        FALSE-DATA-3 BY Z (2, 1, 1)               SM2024.2
            050600                        FALSE-DATA-4 BY +000004.99.               SM2024.2
            050700*********************** END OF COPIED TEXT ***********************SM2024.2
            050800 COPY-DELETE-17.                                                  SM2024.2
            050900     PERFORM  DE-LETE.                                            SM2024.2
            051000 COPY-WRITE-17.                                                   SM2024.2
            051100     IF       P-OR-F EQUAL TO "FAIL*"                             SM2024.2
            051200              MOVE TOTAL-AREA TO COMPUTED-A                       SM2024.2
            051300              MOVE "TRUE  TWO + 2 =    4" TO CORRECT-A.           SM2024.2
            051400     MOVE     "COPY-TEST-17" TO PAR-NAME.                         SM2024.2
            051500     PERFORM  PRINT-DETAIL.                                       SM2024.2
            051600     CLOSE    TEST-FILE.                                          SM2024.2
            051700     GO TO CCVS-EXIT.                                             SM2024.2
            051800 READ-TEST-FILE.                                                  SM2024.2
            051900     READ     TEST-FILE          AT END GO TO BAD-FILE.           SM2024.2
            052000 BAD-FILE.                                                        SM2024.2
            052100     PERFORM  FAIL.                                               SM2024.2
            052200     MOVE     "BAD-FILE" TO PAR-NAME.                             SM2024.2
            052300     MOVE     "EOF PREMATURELY FOUND" TO RE-MARK.                 SM2024.2
            052400     PERFORM  PRINT-DETAIL.                                       SM2024.2
            052500     CLOSE    TEST-FILE.                                          SM2024.2
            052600     GO TO CCVS-EXIT.                                             SM2024.2
            052700 CCVS-EXIT SECTION.                                               SM2024.2
            052800 CCVS-999999.                                                     SM2024.2
            052900     GO TO CLOSE-FILES.                                           SM2024.2
                  *END-OF,SM202A                                                                  
        """)
    )

    @Test
    fun sm203A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM203A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM2034.2
            000200 PROGRAM-ID.                                                      SM2034.2
            000300     SM203A.                                                      SM2034.2
            000400****************************************************************  SM2034.2
            000500*                                                              *  SM2034.2
            000600*    VALIDATION FOR:-                                          *  SM2034.2
            000700*                                                              *  SM2034.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2034.2
            000900*                                                              *  SM2034.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2034.2
            001100*                                                              *  SM2034.2
            001200****************************************************************  SM2034.2
            001300*                                                              *  SM2034.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM2034.2
            001500*                                                              *  SM2034.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM2034.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM2034.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM2034.2
            001900*                                                              *  SM2034.2
            002000****************************************************************  SM2034.2
            002100*                                                              *  SM2034.2
            002200*    PROGRAM SM203A TESTS THE USE OF THE "COPY" STATEMENT      *  SM2034.2
            002300*    "REPLACING" PHRASE IN THE ENVIRONMENT DIVISION.           *  SM2034.2
            002400*    A SEQUENTIAL FILE IS PRODUCED USING "COPY"ED TEXT AND     *  SM2034.2
            002500*    THIS IS CHECKED IN PROGRAM SM204A.                        *  SM2034.2
            002600*                                                              *  SM2034.2
            002700****************************************************************  SM2034.2
            002800 ENVIRONMENT DIVISION.                                            SM2034.2
            002900 CONFIGURATION SECTION.                                           SM2034.2
            003000 SOURCE-COMPUTER.                                                 SM2034.2
            003100     XXXXX082.                                                    SM2034.2
            003200 OBJECT-COMPUTER.                                                 SM2034.2
            003300     XXXXX083.                                                    SM2034.2
            003400ASPECIAL-NAMES.                                        COPY K3SNB SM2034.2
            003500A             REPLACING DUMMY-SW-1 BY SW-1                        SM2034.2
            003600A                       DUMMY-ON BY SWITCH-ON                     SM2034.2
            003700A                       DUMMY-OFF BY SWITCH-OFF.                  SM2034.2
            003800 INPUT-OUTPUT SECTION.                                            SM2034.2
            003900                                                                  SM2034.2
            004000                                                                  SM2034.2
            004100                                                                  SM2034.2
            004200                                                                  SM2034.2
            004300                                                                  SM2034.2
            004400*                                                                 SM2034.2
            004500*********************** COPY STATEMENT USED **********************SM2034.2
            004600*                                                                 SM2034.2
            004700*FILE-CONTROL.                                         COPY K3FCB SM2034.2
            004800*             REPLACING DUMMY-TEST-FILE BY TEST-FILE.             SM2034.2
            004900*                                                                 SM2034.2
            005000******************** COPIED TEXT BEGINS BELOW ********************SM2034.2
            005100 FILE-CONTROL.                                         COPY K3FCB SM2034.2
            005200              REPLACING DUMMY-TEST-FILE BY TEST-FILE.             SM2034.2
            005300*********************** END OF COPIED TEXT ***********************SM2034.2
            005400                                                                  SM2034.2
            005500                                                                  SM2034.2
            005600                                                                  SM2034.2
            005700                                                                  SM2034.2
            005800                                                                  SM2034.2
            005900*                                                                 SM2034.2
            006000*********************** COPY STATEMENT USED **********************SM2034.2
            006100*                                                                 SM2034.2
            006200*I-O-CONTROL.                                          COPY K3IOB SM2034.2
            006300*             REPLACING DUMMY-PRINT-FILE BY PRINT-FILE.           SM2034.2
            006400*                                                                 SM2034.2
            006500******************** COPIED TEXT BEGINS BELOW ********************SM2034.2
            006600 I-O-CONTROL.                                          COPY K3IOB SM2034.2
            006700              REPLACING DUMMY-PRINT-FILE BY PRINT-FILE.           SM2034.2
            006800*********************** END OF COPIED TEXT ***********************SM2034.2
            006900 DATA DIVISION.                                                   SM2034.2
            007000 FILE SECTION.                                                    SM2034.2
            007100 FD  PRINT-FILE.                                                  SM2034.2
            007200 01  PRINT-REC PICTURE X(120).                                    SM2034.2
            007300 01  DUMMY-RECORD PICTURE X(120).                                 SM2034.2
            007400 FD  TEST-FILE                                                    SM2034.2
            007500     LABEL RECORD STANDARD                                        SM2034.2
            007600C    VALUE OF                                                     SM2034.2
            007700C    XXXXX074                                                     SM2034.2
            007800C    IS                                                           SM2034.2
            007900C    XXXXX077                                                     SM2034.2
            008000*    XXXXX069                                                     SM2034.2
            008100     DATA RECORD IS PROOF-REC.                                    SM2034.2
            008200 01  PROOF-REC.                                                   SM2034.2
            008300     02  TF-1 PICTURE 9(5).                                       SM2034.2
            008400     02  FILLER PICTURE X(115).                                   SM2034.2
            008500 WORKING-STORAGE SECTION.                                         SM2034.2
            008600 77  RCD-1 PICTURE 9(5) VALUE 97532.                              SM2034.2
            008700 77  RCD-2 PICTURE 9(5) VALUE 23479.                              SM2034.2
            008800 77  RCD-3 PICTURE 9(5) VALUE 10901.                              SM2034.2
            008900 77  RCD-4 PICTURE 9(5) VALUE 02734.                              SM2034.2
            009000 77  RCD-5 PICTURE 9(5) VALUE 14003.                              SM2034.2
            009100 77  RCD-6 PICTURE 9(5) VALUE 19922.                              SM2034.2
            009200 77  RCD-7 PICTURE 9(5) VALUE 03543.                              SM2034.2
            009300 01  TEST-RESULTS.                                                SM2034.2
            009400     02 FILLER                   PIC X      VALUE SPACE.          SM2034.2
            009500     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM2034.2
            009600     02 FILLER                   PIC X      VALUE SPACE.          SM2034.2
            009700     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM2034.2
            009800     02 FILLER                   PIC X      VALUE SPACE.          SM2034.2
            009900     02  PAR-NAME.                                                SM2034.2
            010000       03 FILLER                 PIC X(19)  VALUE SPACE.          SM2034.2
            010100       03  PARDOT-X              PIC X      VALUE SPACE.          SM2034.2
            010200       03 DOTVALUE               PIC 99     VALUE ZERO.           SM2034.2
            010300     02 FILLER                   PIC X(8)   VALUE SPACE.          SM2034.2
            010400     02 RE-MARK                  PIC X(61).                       SM2034.2
            010500 01  TEST-COMPUTED.                                               SM2034.2
            010600     02 FILLER                   PIC X(30)  VALUE SPACE.          SM2034.2
            010700     02 FILLER                   PIC X(17)  VALUE                 SM2034.2
            010800            "       COMPUTED=".                                   SM2034.2
            010900     02 COMPUTED-X.                                               SM2034.2
            011000     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM2034.2
            011100     03 COMPUTED-N               REDEFINES COMPUTED-A             SM2034.2
            011200                                 PIC -9(9).9(9).                  SM2034.2
            011300     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM2034.2
            011400     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM2034.2
            011500     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM2034.2
            011600     03       CM-18V0 REDEFINES COMPUTED-A.                       SM2034.2
            011700         04 COMPUTED-18V0                    PIC -9(18).          SM2034.2
            011800         04 FILLER                           PIC X.               SM2034.2
            011900     03 FILLER PIC X(50) VALUE SPACE.                             SM2034.2
            012000 01  TEST-CORRECT.                                                SM2034.2
            012100     02 FILLER PIC X(30) VALUE SPACE.                             SM2034.2
            012200     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM2034.2
            012300     02 CORRECT-X.                                                SM2034.2
            012400     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM2034.2
            012500     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM2034.2
            012600     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM2034.2
            012700     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM2034.2
            012800     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM2034.2
            012900     03      CR-18V0 REDEFINES CORRECT-A.                         SM2034.2
            013000         04 CORRECT-18V0                     PIC -9(18).          SM2034.2
            013100         04 FILLER                           PIC X.               SM2034.2
            013200     03 FILLER PIC X(2) VALUE SPACE.                              SM2034.2
            013300     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM2034.2
            013400 01  CCVS-C-1.                                                    SM2034.2
            013500     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM2034.2
            013600-    "SS  PARAGRAPH-NAME                                          SM2034.2
            013700-    "       REMARKS".                                            SM2034.2
            013800     02 FILLER                     PIC X(20)    VALUE SPACE.      SM2034.2
            013900 01  CCVS-C-2.                                                    SM2034.2
            014000     02 FILLER                     PIC X        VALUE SPACE.      SM2034.2
            014100     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM2034.2
            014200     02 FILLER                     PIC X(15)    VALUE SPACE.      SM2034.2
            014300     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM2034.2
            014400     02 FILLER                     PIC X(94)    VALUE SPACE.      SM2034.2
            014500 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM2034.2
            014600 01  REC-CT                        PIC 99       VALUE ZERO.       SM2034.2
            014700 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM2034.2
            014800 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM2034.2
            014900 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM2034.2
            015000 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM2034.2
            015100 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM2034.2
            015200 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM2034.2
            015300 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM2034.2
            015400 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM2034.2
            015500 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM2034.2
            015600 01  CCVS-H-1.                                                    SM2034.2
            015700     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2034.2
            015800     02  FILLER                    PIC X(42)    VALUE             SM2034.2
            015900     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM2034.2
            016000     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2034.2
            016100 01  CCVS-H-2A.                                                   SM2034.2
            016200   02  FILLER                        PIC X(40)  VALUE SPACE.      SM2034.2
            016300   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM2034.2
            016400   02  FILLER                        PIC XXXX   VALUE             SM2034.2
            016500     "4.2 ".                                                      SM2034.2
            016600   02  FILLER                        PIC X(28)  VALUE             SM2034.2
            016700            " COPY - NOT FOR DISTRIBUTION".                       SM2034.2
            016800   02  FILLER                        PIC X(41)  VALUE SPACE.      SM2034.2
            016900                                                                  SM2034.2
            017000 01  CCVS-H-2B.                                                   SM2034.2
            017100   02  FILLER                        PIC X(15)  VALUE             SM2034.2
            017200            "TEST RESULT OF ".                                    SM2034.2
            017300   02  TEST-ID                       PIC X(9).                    SM2034.2
            017400   02  FILLER                        PIC X(4)   VALUE             SM2034.2
            017500            " IN ".                                               SM2034.2
            017600   02  FILLER                        PIC X(12)  VALUE             SM2034.2
            017700     " HIGH       ".                                              SM2034.2
            017800   02  FILLER                        PIC X(22)  VALUE             SM2034.2
            017900            " LEVEL VALIDATION FOR ".                             SM2034.2
            018000   02  FILLER                        PIC X(58)  VALUE             SM2034.2
            018100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2034.2
            018200 01  CCVS-H-3.                                                    SM2034.2
            018300     02  FILLER                      PIC X(34)  VALUE             SM2034.2
            018400            " FOR OFFICIAL USE ONLY    ".                         SM2034.2
            018500     02  FILLER                      PIC X(58)  VALUE             SM2034.2
            018600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2034.2
            018700     02  FILLER                      PIC X(28)  VALUE             SM2034.2
            018800            "  COPYRIGHT   1985 ".                                SM2034.2
            018900 01  CCVS-E-1.                                                    SM2034.2
            019000     02 FILLER                       PIC X(52)  VALUE SPACE.      SM2034.2
            019100     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM2034.2
            019200     02 ID-AGAIN                     PIC X(9).                    SM2034.2
            019300     02 FILLER                       PIC X(45)  VALUE SPACES.     SM2034.2
            019400 01  CCVS-E-2.                                                    SM2034.2
            019500     02  FILLER                      PIC X(31)  VALUE SPACE.      SM2034.2
            019600     02  FILLER                      PIC X(21)  VALUE SPACE.      SM2034.2
            019700     02 CCVS-E-2-2.                                               SM2034.2
            019800         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM2034.2
            019900         03 FILLER                   PIC X      VALUE SPACE.      SM2034.2
            020000         03 ENDER-DESC               PIC X(44)  VALUE             SM2034.2
            020100            "ERRORS ENCOUNTERED".                                 SM2034.2
            020200 01  CCVS-E-3.                                                    SM2034.2
            020300     02  FILLER                      PIC X(22)  VALUE             SM2034.2
            020400            " FOR OFFICIAL USE ONLY".                             SM2034.2
            020500     02  FILLER                      PIC X(12)  VALUE SPACE.      SM2034.2
            020600     02  FILLER                      PIC X(58)  VALUE             SM2034.2
            020700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2034.2
            020800     02  FILLER                      PIC X(13)  VALUE SPACE.      SM2034.2
            020900     02 FILLER                       PIC X(15)  VALUE             SM2034.2
            021000             " COPYRIGHT 1985".                                   SM2034.2
            021100 01  CCVS-E-4.                                                    SM2034.2
            021200     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM2034.2
            021300     02 FILLER                       PIC X(4)   VALUE " OF ".     SM2034.2
            021400     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM2034.2
            021500     02 FILLER                       PIC X(40)  VALUE             SM2034.2
            021600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM2034.2
            021700 01  XXINFO.                                                      SM2034.2
            021800     02 FILLER                       PIC X(19)  VALUE             SM2034.2
            021900            "*** INFORMATION ***".                                SM2034.2
            022000     02 INFO-TEXT.                                                SM2034.2
            022100       04 FILLER                     PIC X(8)   VALUE SPACE.      SM2034.2
            022200       04 XXCOMPUTED                 PIC X(20).                   SM2034.2
            022300       04 FILLER                     PIC X(5)   VALUE SPACE.      SM2034.2
            022400       04 XXCORRECT                  PIC X(20).                   SM2034.2
            022500     02 INF-ANSI-REFERENCE           PIC X(48).                   SM2034.2
            022600 01  HYPHEN-LINE.                                                 SM2034.2
            022700     02 FILLER  PIC IS X VALUE IS SPACE.                          SM2034.2
            022800     02 FILLER  PIC IS X(65)    VALUE IS "************************SM2034.2
            022900-    "*****************************************".                 SM2034.2
            023000     02 FILLER  PIC IS X(54)    VALUE IS "************************SM2034.2
            023100-    "******************************".                            SM2034.2
            023200 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM2034.2
            023300     "SM203A".                                                    SM2034.2
            023400 PROCEDURE DIVISION.                                              SM2034.2
            023500 CCVS1 SECTION.                                                   SM2034.2
            023600 OPEN-FILES.                                                      SM2034.2
            023700     OPEN     OUTPUT PRINT-FILE.                                  SM2034.2
            023800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM2034.2
            023900     MOVE    SPACE TO TEST-RESULTS.                               SM2034.2
            024000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM2034.2
            024100     GO TO CCVS1-EXIT.                                            SM2034.2
            024200 CLOSE-FILES.                                                     SM2034.2
            024300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM2034.2
            024400 TERMINATE-CCVS.                                                  SM2034.2
            024500*    EXIT PROGRAM.                                                SM2034.2
            024600*TERMINATE-CALL.                                                  SM2034.2
            024700     STOP     RUN.                                                SM2034.2
            024800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM2034.2
            024900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM2034.2
            025000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM2034.2
            025100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM2034.2
            025200     MOVE "****TEST DELETED****" TO RE-MARK.                      SM2034.2
            025300 PRINT-DETAIL.                                                    SM2034.2
            025400     IF REC-CT NOT EQUAL TO ZERO                                  SM2034.2
            025500             MOVE "." TO PARDOT-X                                 SM2034.2
            025600             MOVE REC-CT TO DOTVALUE.                             SM2034.2
            025700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM2034.2
            025800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM2034.2
            025900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM2034.2
            026000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM2034.2
            026100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM2034.2
            026200     MOVE SPACE TO CORRECT-X.                                     SM2034.2
            026300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM2034.2
            026400     MOVE     SPACE TO RE-MARK.                                   SM2034.2
            026500 HEAD-ROUTINE.                                                    SM2034.2
            026600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2034.2
            026700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2034.2
            026800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2034.2
            026900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2034.2
            027000 COLUMN-NAMES-ROUTINE.                                            SM2034.2
            027100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2034.2
            027200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2034.2
            027300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM2034.2
            027400 END-ROUTINE.                                                     SM2034.2
            027500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM2034.2
            027600 END-RTN-EXIT.                                                    SM2034.2
            027700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2034.2
            027800 END-ROUTINE-1.                                                   SM2034.2
            027900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM2034.2
            028000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM2034.2
            028100      ADD PASS-COUNTER TO ERROR-HOLD.                             SM2034.2
            028200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM2034.2
            028300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM2034.2
            028400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM2034.2
            028500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM2034.2
            028600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM2034.2
            028700  END-ROUTINE-12.                                                 SM2034.2
            028800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM2034.2
            028900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM2034.2
            029000         MOVE "NO " TO ERROR-TOTAL                                SM2034.2
            029100         ELSE                                                     SM2034.2
            029200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM2034.2
            029300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM2034.2
            029400     PERFORM WRITE-LINE.                                          SM2034.2
            029500 END-ROUTINE-13.                                                  SM2034.2
            029600     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM2034.2
            029700         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM2034.2
            029800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM2034.2
            029900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM2034.2
            030000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2034.2
            030100      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM2034.2
            030200          MOVE "NO " TO ERROR-TOTAL                               SM2034.2
            030300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM2034.2
            030400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM2034.2
            030500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM2034.2
            030600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2034.2
            030700 WRITE-LINE.                                                      SM2034.2
            030800     ADD 1 TO RECORD-COUNT.                                       SM2034.2
            030900*    IF RECORD-COUNT GREATER 50                                   SM2034.2
            031000*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM2034.2
            031100*        MOVE SPACE TO DUMMY-RECORD                               SM2034.2
            031200*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM2034.2
            031300*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM2034.2
            031400*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM2034.2
            031500*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM2034.2
            031600*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM2034.2
            031700*        MOVE ZERO TO RECORD-COUNT.                               SM2034.2
            031800     PERFORM WRT-LN.                                              SM2034.2
            031900 WRT-LN.                                                          SM2034.2
            032000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM2034.2
            032100     MOVE SPACE TO DUMMY-RECORD.                                  SM2034.2
            032200 BLANK-LINE-PRINT.                                                SM2034.2
            032300     PERFORM WRT-LN.                                              SM2034.2
            032400 FAIL-ROUTINE.                                                    SM2034.2
            032500     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM2034.2
            032600     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM2034.2
            032700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2034.2
            032800     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM2034.2
            032900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2034.2
            033000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2034.2
            033100     GO TO  FAIL-ROUTINE-EX.                                      SM2034.2
            033200 FAIL-ROUTINE-WRITE.                                              SM2034.2
            033300     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM2034.2
            033400     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM2034.2
            033500     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM2034.2
            033600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM2034.2
            033700 FAIL-ROUTINE-EX. EXIT.                                           SM2034.2
            033800 BAIL-OUT.                                                        SM2034.2
            033900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM2034.2
            034000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM2034.2
            034100 BAIL-OUT-WRITE.                                                  SM2034.2
            034200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM2034.2
            034300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2034.2
            034400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2034.2
            034500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2034.2
            034600 BAIL-OUT-EX. EXIT.                                               SM2034.2
            034700 CCVS1-EXIT.                                                      SM2034.2
            034800     EXIT.                                                        SM2034.2
            034900 INITIALIZATION SECTION.                                          SM2034.2
            035000 SM203-INIT.                                                      SM2034.2
            035100     OPEN     OUTPUT TEST-FILE.                                   SM2034.2
            035200 BUILD SECTION.                                                   SM2034.2
            035300 COPY-TEST-1.                                                     SM2034.2
            035400     MOVE     RCD-1 TO TF-1.                                      SM2034.2
            035500     WRITE    PROOF-REC.                                          SM2034.2
            035600     MOVE     RCD-2 TO TF-1.                                      SM2034.2
            035700     WRITE    PROOF-REC.                                          SM2034.2
            035800     MOVE     RCD-3 TO TF-1.                                      SM2034.2
            035900     WRITE    PROOF-REC.                                          SM2034.2
            036000     MOVE     RCD-4 TO TF-1.                                      SM2034.2
            036100     WRITE    PROOF-REC.                                          SM2034.2
            036200     MOVE     RCD-5 TO TF-1.                                      SM2034.2
            036300     WRITE    PROOF-REC.                                          SM2034.2
            036400     MOVE     RCD-6 TO TF-1.                                      SM2034.2
            036500     WRITE    PROOF-REC.                                          SM2034.2
            036600     MOVE     RCD-7 TO TF-1.                                      SM2034.2
            036700     WRITE    PROOF-REC.                                          SM2034.2
            036800     PERFORM  PASS.                                               SM2034.2
            036900     GO       TO COPY-WRITE-1.                                    SM2034.2
            037000 COPY-DELETE-1.                                                   SM2034.2
            037100     PERFORM  DE-LETE.                                            SM2034.2
            037200 COPY-WRITE-1.                                                    SM2034.2
            037300     MOVE     "COPY ENV DIV REPLAC" TO FEATURE.                   SM2034.2
            037400     MOVE     "COPY-TEST-1 " TO PAR-NAME.                         SM2034.2
            037500     PERFORM  PRINT-DETAIL.                                       SM2034.2
            037600     CLOSE    TEST-FILE.                                          SM2034.2
            037700 CCVS-EXIT SECTION.                                               SM2034.2
            037800 CCVS-999999.                                                     SM2034.2
            037900     GO TO CLOSE-FILES.                                           SM2034.2
                  *END-OF,SM203A                                                                  
        """)
    )

    @Test
    fun sm205A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM205A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM2054.2
            000200 PROGRAM-ID.                                                      SM2054.2
            000300     SM205A.                                                      SM2054.2
            000400****************************************************************  SM2054.2
            000500*                                                              *  SM2054.2
            000600*    VALIDATION FOR:-                                          *  SM2054.2
            000700*                                                              *  SM2054.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2054.2
            000900*                                                              *  SM2054.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2054.2
            001100*                                                              *  SM2054.2
            001200****************************************************************  SM2054.2
            001300*                                                              *  SM2054.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM2054.2
            001500*                                                              *  SM2054.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM2054.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM2054.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM2054.2
            001900*                                                              *  SM2054.2
            002000****************************************************************  SM2054.2
            002100*                                                              *  SM2054.2
            002200*    PROGRAM SM205A TESTS THE USE OF THE "COPY" STATEMENT WITH *  SM2054.2
            002300*    ITS "REPLACING" PHRASE FOR A SORT DESCRIPTION AND RELATED *  SM2054.2
            002400*    ENTRIES.  (THIS PROGRAM ASSUMES THAT PROGRAM ST101        *  SM2054.2
            002500*    PERFORMS CORRECTLY).                                      *  SM2054.2
            002600*                                                              *  SM2054.2
            002700****************************************************************  SM2054.2
            002800 ENVIRONMENT DIVISION.                                            SM2054.2
            002900 CONFIGURATION SECTION.                                           SM2054.2
            003000 SOURCE-COMPUTER.                                                 SM2054.2
            003100     XXXXX082.                                                    SM2054.2
            003200 OBJECT-COMPUTER.                                                 SM2054.2
            003300     XXXXX083.                                                    SM2054.2
            003400 INPUT-OUTPUT SECTION.                                            SM2054.2
            003500 FILE-CONTROL.                                                    SM2054.2
            003600     SELECT PRINT-FILE ASSIGN TO                                  SM2054.2
            003700     XXXXX055.                                                    SM2054.2
            003800     SELECT SORTFILE-2E ASSIGN TO                                 SM2054.2
            003900     XXXXX027.                                                    SM2054.2
            004000     SELECT SORTOUT-2E ASSIGN TO                                  SM2054.2
            004100     XXXXX001.                                                    SM2054.2
            004200 DATA DIVISION.                                                   SM2054.2
            004300 FILE SECTION.                                                    SM2054.2
            004400 FD  PRINT-FILE.                                                  SM2054.2
            004500 01  PRINT-REC PICTURE X(120).                                    SM2054.2
            004600 01  DUMMY-RECORD PICTURE X(120).                                 SM2054.2
            004700                                                                  SM2054.2
            004800                                                                  SM2054.2
            004900                                                                  SM2054.2
            005000                                                                  SM2054.2
            005100                                                                  SM2054.2
            005200*                                                                 SM2054.2
            005300*********************** COPY STATEMENT USED **********************SM2054.2
            005400*                                                                 SM2054.2
            005500*SD  SORTFILE-2E                                       COPY K5SDB SM2054.2
            005600*                   REPLACING J-RECORD BY S-RECORD.               SM2054.2
            005700*                                                                 SM2054.2
            005800******************** COPIED TEXT BEGINS BELOW ********************SM2054.2
            005900 SD  SORTFILE-2E                                       COPY K5SDB SM2054.2
            006000              REPLACING J-RECORD BY S-RECORD.                     SM2054.2
            006100*********************** END OF COPIED TEXT ***********************SM2054.2
            006200                                                                  SM2054.2
            006300                                                                  SM2054.2
            006400                                                                  SM2054.2
            006500                                                                  SM2054.2
            006600                                                                  SM2054.2
            006700*                                                                 SM2054.2
            006800*********************** COPY STATEMENT USED **********************SM2054.2
            006900*                                                                 SM2054.2
            007000*01  S-RECORD.                                         COPY K501B SM2054.2
            007100*             REPLACING KEY-A BY KEY-1                            SM2054.2
            007200*                       XYZ-KEYS BY RDF-KEYS.                     SM2054.2
            007300*                                                                 SM2054.2
            007400******************** COPIED TEXT BEGINS BELOW ********************SM2054.2
            007500 01  S-RECORD.                                         COPY K501B SM2054.2
            007600              REPLACING KEY-A BY KEY-1                            SM2054.2
            007700                        XYZ-KEYS BY RDF-KEYS.                     SM2054.2
            007800*********************** END OF COPIED TEXT ***********************SM2054.2
            007900 FD  SORTOUT-2E                                                   SM2054.2
            008000     BLOCK CONTAINS 10 RECORDS                                    SM2054.2
            008100     LABEL RECORDS ARE STANDARD                                   SM2054.2
            008200C    VALUE OF                                                     SM2054.2
            008300C    XXXXX074                                                     SM2054.2
            008400C    IS                                                           SM2054.2
            008500C    XXXXX076                                                     SM2054.2
            008600*    XXXXX069                                                     SM2054.2
            008700     DATA RECORD SORTED.                                          SM2054.2
            008800 01  SORTED PICTURE X(120).                                       SM2054.2
            008900 WORKING-STORAGE SECTION.                                         SM2054.2
            009000 77  C0 PICTURE 9 VALUE 0.                                        SM2054.2
            009100 77  C1 PICTURE 9 VALUE 1.                                        SM2054.2
            009200 77  C2 PICTURE 9 VALUE 2.                                        SM2054.2
            009300 77  C6 PICTURE 9 VALUE 6.                                        SM2054.2
            009400 77  C3 PICTURE 9 VALUE 3.                                        SM2054.2
            009500 01  WKEYS-GROUP.                                                 SM2054.2
            009600     02  WKEY-1  PICTURE 9.                                       SM2054.2
            009700     02  WKEY-2  PICTURE 99.                                      SM2054.2
            009800     02  WKEY-3  PICTURE 999.                                     SM2054.2
            009900     02  WKEY-4  PICTURE 9999.                                    SM2054.2
            010000     02  WKEY-5 PICTURE 9(5).                                     SM2054.2
            010100 01  WKEYS-RDF REDEFINES WKEYS-GROUP PICTURE 9(15).               SM2054.2
            010200 01  TEST-RESULTS.                                                SM2054.2
            010300     02 FILLER                   PIC X      VALUE SPACE.          SM2054.2
            010400     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM2054.2
            010500     02 FILLER                   PIC X      VALUE SPACE.          SM2054.2
            010600     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM2054.2
            010700     02 FILLER                   PIC X      VALUE SPACE.          SM2054.2
            010800     02  PAR-NAME.                                                SM2054.2
            010900       03 FILLER                 PIC X(19)  VALUE SPACE.          SM2054.2
            011000       03  PARDOT-X              PIC X      VALUE SPACE.          SM2054.2
            011100       03 DOTVALUE               PIC 99     VALUE ZERO.           SM2054.2
            011200     02 FILLER                   PIC X(8)   VALUE SPACE.          SM2054.2
            011300     02 RE-MARK                  PIC X(61).                       SM2054.2
            011400 01  TEST-COMPUTED.                                               SM2054.2
            011500     02 FILLER                   PIC X(30)  VALUE SPACE.          SM2054.2
            011600     02 FILLER                   PIC X(17)  VALUE                 SM2054.2
            011700            "       COMPUTED=".                                   SM2054.2
            011800     02 COMPUTED-X.                                               SM2054.2
            011900     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM2054.2
            012000     03 COMPUTED-N               REDEFINES COMPUTED-A             SM2054.2
            012100                                 PIC -9(9).9(9).                  SM2054.2
            012200     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM2054.2
            012300     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM2054.2
            012400     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM2054.2
            012500     03       CM-18V0 REDEFINES COMPUTED-A.                       SM2054.2
            012600         04 COMPUTED-18V0                    PIC -9(18).          SM2054.2
            012700         04 FILLER                           PIC X.               SM2054.2
            012800     03 FILLER PIC X(50) VALUE SPACE.                             SM2054.2
            012900 01  TEST-CORRECT.                                                SM2054.2
            013000     02 FILLER PIC X(30) VALUE SPACE.                             SM2054.2
            013100     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM2054.2
            013200     02 CORRECT-X.                                                SM2054.2
            013300     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM2054.2
            013400     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM2054.2
            013500     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM2054.2
            013600     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM2054.2
            013700     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM2054.2
            013800     03      CR-18V0 REDEFINES CORRECT-A.                         SM2054.2
            013900         04 CORRECT-18V0                     PIC -9(18).          SM2054.2
            014000         04 FILLER                           PIC X.               SM2054.2
            014100     03 FILLER PIC X(2) VALUE SPACE.                              SM2054.2
            014200     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM2054.2
            014300 01  CCVS-C-1.                                                    SM2054.2
            014400     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM2054.2
            014500-    "SS  PARAGRAPH-NAME                                          SM2054.2
            014600-    "       REMARKS".                                            SM2054.2
            014700     02 FILLER                     PIC X(20)    VALUE SPACE.      SM2054.2
            014800 01  CCVS-C-2.                                                    SM2054.2
            014900     02 FILLER                     PIC X        VALUE SPACE.      SM2054.2
            015000     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM2054.2
            015100     02 FILLER                     PIC X(15)    VALUE SPACE.      SM2054.2
            015200     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM2054.2
            015300     02 FILLER                     PIC X(94)    VALUE SPACE.      SM2054.2
            015400 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM2054.2
            015500 01  REC-CT                        PIC 99       VALUE ZERO.       SM2054.2
            015600 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM2054.2
            015700 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM2054.2
            015800 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM2054.2
            015900 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM2054.2
            016000 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM2054.2
            016100 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM2054.2
            016200 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM2054.2
            016300 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM2054.2
            016400 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM2054.2
            016500 01  CCVS-H-1.                                                    SM2054.2
            016600     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2054.2
            016700     02  FILLER                    PIC X(42)    VALUE             SM2054.2
            016800     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM2054.2
            016900     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2054.2
            017000 01  CCVS-H-2A.                                                   SM2054.2
            017100   02  FILLER                        PIC X(40)  VALUE SPACE.      SM2054.2
            017200   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM2054.2
            017300   02  FILLER                        PIC XXXX   VALUE             SM2054.2
            017400     "4.2 ".                                                      SM2054.2
            017500   02  FILLER                        PIC X(28)  VALUE             SM2054.2
            017600            " COPY - NOT FOR DISTRIBUTION".                       SM2054.2
            017700   02  FILLER                        PIC X(41)  VALUE SPACE.      SM2054.2
            017800                                                                  SM2054.2
            017900 01  CCVS-H-2B.                                                   SM2054.2
            018000   02  FILLER                        PIC X(15)  VALUE             SM2054.2
            018100            "TEST RESULT OF ".                                    SM2054.2
            018200   02  TEST-ID                       PIC X(9).                    SM2054.2
            018300   02  FILLER                        PIC X(4)   VALUE             SM2054.2
            018400            " IN ".                                               SM2054.2
            018500   02  FILLER                        PIC X(12)  VALUE             SM2054.2
            018600     " HIGH       ".                                              SM2054.2
            018700   02  FILLER                        PIC X(22)  VALUE             SM2054.2
            018800            " LEVEL VALIDATION FOR ".                             SM2054.2
            018900   02  FILLER                        PIC X(58)  VALUE             SM2054.2
            019000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2054.2
            019100 01  CCVS-H-3.                                                    SM2054.2
            019200     02  FILLER                      PIC X(34)  VALUE             SM2054.2
            019300            " FOR OFFICIAL USE ONLY    ".                         SM2054.2
            019400     02  FILLER                      PIC X(58)  VALUE             SM2054.2
            019500     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2054.2
            019600     02  FILLER                      PIC X(28)  VALUE             SM2054.2
            019700            "  COPYRIGHT   1985 ".                                SM2054.2
            019800 01  CCVS-E-1.                                                    SM2054.2
            019900     02 FILLER                       PIC X(52)  VALUE SPACE.      SM2054.2
            020000     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM2054.2
            020100     02 ID-AGAIN                     PIC X(9).                    SM2054.2
            020200     02 FILLER                       PIC X(45)  VALUE SPACES.     SM2054.2
            020300 01  CCVS-E-2.                                                    SM2054.2
            020400     02  FILLER                      PIC X(31)  VALUE SPACE.      SM2054.2
            020500     02  FILLER                      PIC X(21)  VALUE SPACE.      SM2054.2
            020600     02 CCVS-E-2-2.                                               SM2054.2
            020700         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM2054.2
            020800         03 FILLER                   PIC X      VALUE SPACE.      SM2054.2
            020900         03 ENDER-DESC               PIC X(44)  VALUE             SM2054.2
            021000            "ERRORS ENCOUNTERED".                                 SM2054.2
            021100 01  CCVS-E-3.                                                    SM2054.2
            021200     02  FILLER                      PIC X(22)  VALUE             SM2054.2
            021300            " FOR OFFICIAL USE ONLY".                             SM2054.2
            021400     02  FILLER                      PIC X(12)  VALUE SPACE.      SM2054.2
            021500     02  FILLER                      PIC X(58)  VALUE             SM2054.2
            021600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2054.2
            021700     02  FILLER                      PIC X(13)  VALUE SPACE.      SM2054.2
            021800     02 FILLER                       PIC X(15)  VALUE             SM2054.2
            021900             " COPYRIGHT 1985".                                   SM2054.2
            022000 01  CCVS-E-4.                                                    SM2054.2
            022100     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM2054.2
            022200     02 FILLER                       PIC X(4)   VALUE " OF ".     SM2054.2
            022300     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM2054.2
            022400     02 FILLER                       PIC X(40)  VALUE             SM2054.2
            022500      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM2054.2
            022600 01  XXINFO.                                                      SM2054.2
            022700     02 FILLER                       PIC X(19)  VALUE             SM2054.2
            022800            "*** INFORMATION ***".                                SM2054.2
            022900     02 INFO-TEXT.                                                SM2054.2
            023000       04 FILLER                     PIC X(8)   VALUE SPACE.      SM2054.2
            023100       04 XXCOMPUTED                 PIC X(20).                   SM2054.2
            023200       04 FILLER                     PIC X(5)   VALUE SPACE.      SM2054.2
            023300       04 XXCORRECT                  PIC X(20).                   SM2054.2
            023400     02 INF-ANSI-REFERENCE           PIC X(48).                   SM2054.2
            023500 01  HYPHEN-LINE.                                                 SM2054.2
            023600     02 FILLER  PIC IS X VALUE IS SPACE.                          SM2054.2
            023700     02 FILLER  PIC IS X(65)    VALUE IS "************************SM2054.2
            023800-    "*****************************************".                 SM2054.2
            023900     02 FILLER  PIC IS X(54)    VALUE IS "************************SM2054.2
            024000-    "******************************".                            SM2054.2
            024100 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM2054.2
            024200     "SM205A".                                                    SM2054.2
            024300 PROCEDURE DIVISION.                                              SM2054.2
            024400 CCVS1 SECTION.                                                   SM2054.2
            024500 OPEN-FILES.                                                      SM2054.2
            024600     OPEN     OUTPUT PRINT-FILE.                                  SM2054.2
            024700     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM2054.2
            024800     MOVE    SPACE TO TEST-RESULTS.                               SM2054.2
            024900     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM2054.2
            025000     GO TO CCVS1-EXIT.                                            SM2054.2
            025100 CLOSE-FILES.                                                     SM2054.2
            025200     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM2054.2
            025300 TERMINATE-CCVS.                                                  SM2054.2
            025400*    EXIT PROGRAM.                                                SM2054.2
            025500*TERMINATE-CALL.                                                  SM2054.2
            025600     STOP     RUN.                                                SM2054.2
            025700 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM2054.2
            025800 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM2054.2
            025900 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM2054.2
            026000 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM2054.2
            026100     MOVE "****TEST DELETED****" TO RE-MARK.                      SM2054.2
            026200 PRINT-DETAIL.                                                    SM2054.2
            026300     IF REC-CT NOT EQUAL TO ZERO                                  SM2054.2
            026400             MOVE "." TO PARDOT-X                                 SM2054.2
            026500             MOVE REC-CT TO DOTVALUE.                             SM2054.2
            026600     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM2054.2
            026700     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM2054.2
            026800        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM2054.2
            026900          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM2054.2
            027000     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM2054.2
            027100     MOVE SPACE TO CORRECT-X.                                     SM2054.2
            027200     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM2054.2
            027300     MOVE     SPACE TO RE-MARK.                                   SM2054.2
            027400 HEAD-ROUTINE.                                                    SM2054.2
            027500     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2054.2
            027600     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2054.2
            027700     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2054.2
            027800     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2054.2
            027900 COLUMN-NAMES-ROUTINE.                                            SM2054.2
            028000     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2054.2
            028100     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2054.2
            028200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM2054.2
            028300 END-ROUTINE.                                                     SM2054.2
            028400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM2054.2
            028500 END-RTN-EXIT.                                                    SM2054.2
            028600     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2054.2
            028700 END-ROUTINE-1.                                                   SM2054.2
            028800      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM2054.2
            028900      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM2054.2
            029000      ADD PASS-COUNTER TO ERROR-HOLD.                             SM2054.2
            029100*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM2054.2
            029200      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM2054.2
            029300      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM2054.2
            029400      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM2054.2
            029500      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM2054.2
            029600  END-ROUTINE-12.                                                 SM2054.2
            029700      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM2054.2
            029800     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM2054.2
            029900         MOVE "NO " TO ERROR-TOTAL                                SM2054.2
            030000         ELSE                                                     SM2054.2
            030100         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM2054.2
            030200     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM2054.2
            030300     PERFORM WRITE-LINE.                                          SM2054.2
            030400 END-ROUTINE-13.                                                  SM2054.2
            030500     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM2054.2
            030600         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM2054.2
            030700         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM2054.2
            030800     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM2054.2
            030900     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2054.2
            031000      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM2054.2
            031100          MOVE "NO " TO ERROR-TOTAL                               SM2054.2
            031200      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM2054.2
            031300      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM2054.2
            031400      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM2054.2
            031500     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2054.2
            031600 WRITE-LINE.                                                      SM2054.2
            031700     ADD 1 TO RECORD-COUNT.                                       SM2054.2
            031800*    IF RECORD-COUNT GREATER 50                                   SM2054.2
            031900*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM2054.2
            032000*        MOVE SPACE TO DUMMY-RECORD                               SM2054.2
            032100*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM2054.2
            032200*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM2054.2
            032300*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM2054.2
            032400*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM2054.2
            032500*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM2054.2
            032600*        MOVE ZERO TO RECORD-COUNT.                               SM2054.2
            032700     PERFORM WRT-LN.                                              SM2054.2
            032800 WRT-LN.                                                          SM2054.2
            032900     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM2054.2
            033000     MOVE SPACE TO DUMMY-RECORD.                                  SM2054.2
            033100 BLANK-LINE-PRINT.                                                SM2054.2
            033200     PERFORM WRT-LN.                                              SM2054.2
            033300 FAIL-ROUTINE.                                                    SM2054.2
            033400     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM2054.2
            033500     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM2054.2
            033600     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2054.2
            033700     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM2054.2
            033800     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2054.2
            033900     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2054.2
            034000     GO TO  FAIL-ROUTINE-EX.                                      SM2054.2
            034100 FAIL-ROUTINE-WRITE.                                              SM2054.2
            034200     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM2054.2
            034300     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM2054.2
            034400     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM2054.2
            034500     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM2054.2
            034600 FAIL-ROUTINE-EX. EXIT.                                           SM2054.2
            034700 BAIL-OUT.                                                        SM2054.2
            034800     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM2054.2
            034900     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM2054.2
            035000 BAIL-OUT-WRITE.                                                  SM2054.2
            035100     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM2054.2
            035200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2054.2
            035300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2054.2
            035400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2054.2
            035500 BAIL-OUT-EX. EXIT.                                               SM2054.2
            035600 CCVS1-EXIT.                                                      SM2054.2
            035700     EXIT.                                                        SM2054.2
            035800 SORT-INIT SECTION.                                               SM2054.2
            035900 I-1.                                                             SM2054.2
            036000     SORT SORTFILE-2E                                             SM2054.2
            036100     ON ASCENDING KEY KEY-1                                       SM2054.2
            036200     ON DESCENDING KEY KEY-2                                      SM2054.2
            036300     ON ASCENDING KEY KEY-3                                       SM2054.2
            036400     DESCENDING KEY-4 KEY-5                                       SM2054.2
            036500     INPUT PROCEDURE IS INSORT                                    SM2054.2
            036600     OUTPUT PROCEDURE IS OUTP1 THRU OUTP3.                        SM2054.2
            036700 I-2.                                                             SM2054.2
            036800     GO TO    CLOSE-FILES.                                        SM2054.2
            036900 INSORT SECTION.                                                  SM2054.2
            037000 IN-1.                                                            SM2054.2
            037100*        NOTE TESTS ORDINARY COPYING OF ENTRIES WHICH ARE ALSO    SM2054.2
            037200*             COPIED WITH REPLACEMENT.                            SM2054.2
            037300 IN-2.                                                            SM2054.2
            037400     MOVE 900009000000000 TO RDF-KEYS.                            SM2054.2
            037500     RELEASE S-RECORD.                                            SM2054.2
            037600     MOVE 009000000900009 TO RDF-KEYS.                            SM2054.2
            037700     RELEASE S-RECORD.                                            SM2054.2
            037800     MOVE 900008000000000 TO RDF-KEYS.                            SM2054.2
            037900     RELEASE S-RECORD.                                            SM2054.2
            038000     MOVE 009000000900008 TO RDF-KEYS.                            SM2054.2
            038100     RELEASE S-RECORD.                                            SM2054.2
            038200*    NOTE HI-LOW CONTROL RECORDS DONE.                            SM2054.2
            038300     MOVE 300003000000000 TO WKEYS-RDF.                           SM2054.2
            038400 IN-3.                                                            SM2054.2
            038500     PERFORM IN-4 2 TIMES.                                        SM2054.2
            038600     GO TO IN-EXIT.                                               SM2054.2
            038700 IN-4.                                                            SM2054.2
            038800     SUBTRACT C1 FROM WKEY-1.                                     SM2054.2
            038900     PERFORM IN-5 6 TIMES.                                        SM2054.2
            039000 IN-5.                                                            SM2054.2
            039100     IF WKEY-2 IS EQUAL TO C6                                     SM2054.2
            039200         MOVE C0 TO WKEY-2.                                       SM2054.2
            039300     ADD C1 TO WKEY-2.                                            SM2054.2
            039400     PERFORM IN-6 2 TIMES.                                        SM2054.2
            039500 IN-6.                                                            SM2054.2
            039600     IF WKEY-3 IS EQUAL TO C1                                     SM2054.2
            039700         MOVE C3 TO WKEY-3.                                       SM2054.2
            039800     SUBTRACT C1 FROM WKEY-3.                                     SM2054.2
            039900     PERFORM IN-7 2 TIMES.                                        SM2054.2
            040000 IN-7.                                                            SM2054.2
            040100     IF       WKEY-4 EQUAL TO C2                                  SM2054.2
            040200         MOVE C0 TO WKEY-4.                                       SM2054.2
            040300     ADD C1 TO WKEY-4.                                            SM2054.2
            040400     PERFORM IN-8 2 TIMES.                                        SM2054.2
            040500 IN-8.                                                            SM2054.2
            040600     IF WKEY-5 IS EQUAL TO C2                                     SM2054.2
            040700         MOVE C0 TO WKEY-5.                                       SM2054.2
            040800     ADD C1 TO WKEY-5.                                            SM2054.2
            040900     MOVE WKEYS-RDF TO RDF-KEYS.                                  SM2054.2
            041000     RELEASE S-RECORD.                                            SM2054.2
            041100 IN-EXIT.                                                         SM2054.2
            041200     EXIT.                                                        SM2054.2
            041300 OUTP1 SECTION.                                                   SM2054.2
            041400 WOUTPT1.                                                         SM2054.2
            041500     OPEN     OUTPUT SORTOUT-2E.                                  SM2054.2
            041600     MOVE     SPACE TO TEST-RESULTS.                              SM2054.2
            041700     MOVE     "COPY SD REPLACING" TO FEATURE.                     SM2054.2
            041800 COPY-TEST-1.                                                     SM2054.2
            041900     PERFORM  RET-1.                                              SM2054.2
            042000     IF       RDF-KEYS EQUAL TO 009000000900009                   SM2054.2
            042100              PERFORM PASS-1 GO TO COPY-WRITE-1.                  SM2054.2
            042200     GO       TO COPY-FAIL-1-1.                                   SM2054.2
            042300 COPY-DELETE-1.                                                   SM2054.2
            042400     PERFORM  DE-LETE-1.                                          SM2054.2
            042500     GO       TO COPY-WRITE-1.                                    SM2054.2
            042600 COPY-FAIL-1-1.                                                   SM2054.2
            042700     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            042800     MOVE     009000000900009 TO CORRECT-18V0.                    SM2054.2
            042900     PERFORM  FAIL-1.                                             SM2054.2
            043000 COPY-WRITE-1.                                                    SM2054.2
            043100     MOVE     "COPY-TEST-1 " TO PAR-NAME.                         SM2054.2
            043200     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            043300 COPY-TEST-2.                                                     SM2054.2
            043400     PERFORM  RET-1.                                              SM2054.2
            043500     IF       RDF-KEYS EQUAL TO 009000000900008                   SM2054.2
            043600              PERFORM PASS-1 GO TO COPY-WRITE-2.                  SM2054.2
            043700     GO       TO COPY-FAIL-1-2.                                   SM2054.2
            043800 COPY-DELETE-2.                                                   SM2054.2
            043900     PERFORM  DE-LETE-1.                                          SM2054.2
            044000     GO       TO COPY-WRITE-2.                                    SM2054.2
            044100 COPY-FAIL-1-2.                                                   SM2054.2
            044200     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            044300     MOVE     009000000900008 TO CORRECT-18V0.                    SM2054.2
            044400     PERFORM  FAIL-1.                                             SM2054.2
            044500 COPY-WRITE-2.                                                    SM2054.2
            044600     MOVE     "COPY-TEST-2 " TO PAR-NAME.                         SM2054.2
            044700     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            044800 COPY-TEST-3.                                                     SM2054.2
            044900     PERFORM  RET-1.                                              SM2054.2
            045000     IF       RDF-KEYS EQUAL TO 106001000200002                   SM2054.2
            045100              PERFORM PASS-1 GO TO COPY-WRITE-3.                  SM2054.2
            045200     GO       TO COPY-FAIL-1-3.                                   SM2054.2
            045300 COPY-DELETE-3.                                                   SM2054.2
            045400     PERFORM  DE-LETE-1.                                          SM2054.2
            045500     GO       TO COPY-WRITE-3.                                    SM2054.2
            045600 COPY-FAIL-1-3.                                                   SM2054.2
            045700     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            045800     MOVE     106001000200002 TO CORRECT-18V0.                    SM2054.2
            045900     PERFORM  FAIL-1.                                             SM2054.2
            046000 COPY-WRITE-3.                                                    SM2054.2
            046100     MOVE     "COPY-TEST-3 " TO PAR-NAME.                         SM2054.2
            046200     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            046300 COPY-TEST-4.                                                     SM2054.2
            046400     PERFORM  RET-2 48 TIMES.                                     SM2054.2
            046500     IF       RDF-KEYS EQUAL TO 206001000200002                   SM2054.2
            046600              PERFORM PASS-1 GO TO COPY-WRITE-4.                  SM2054.2
            046700     GO       TO COPY-FAIL-1-4.                                   SM2054.2
            046800 COPY-DELETE-4.                                                   SM2054.2
            046900     PERFORM  DE-LETE-1.                                          SM2054.2
            047000     GO       TO COPY-WRITE-4.                                    SM2054.2
            047100 COPY-FAIL-1-4.                                                   SM2054.2
            047200     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            047300     MOVE     206001000200002 TO CORRECT-18V0.                    SM2054.2
            047400     PERFORM  FAIL-1.                                             SM2054.2
            047500 COPY-WRITE-4.                                                    SM2054.2
            047600*        NOTE COPYING OF A PROCEDURE WHICH REFERENCES COPIED DATA.SM2054.2
            047700     MOVE "COPY-TEST-4 " TO PAR-NAME.                             SM2054.2
            047800     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            047900 COPY-TEST-5.                                                     SM2054.2
            048000     PERFORM  RET-2 40 TIMES.                                     SM2054.2
            048100     IF       RDF-KEYS EQUAL TO 201001000200002                   SM2054.2
            048200              PERFORM PASS-1 GO TO COPY-WRITE-5.                  SM2054.2
            048300     GO       TO COPY-FAIL-1-5.                                   SM2054.2
            048400 COPY-DELETE-5.                                                   SM2054.2
            048500     PERFORM  DE-LETE-1.                                          SM2054.2
            048600     GO       TO COPY-WRITE-5.                                    SM2054.2
            048700 COPY-FAIL-1-5.                                                   SM2054.2
            048800     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            048900     MOVE     201001000200002 TO CORRECT-18V0.                    SM2054.2
            049000     PERFORM  FAIL-1.                                             SM2054.2
            049100 COPY-WRITE-5.                                                    SM2054.2
            049200     MOVE     "COPY-TEST-5 " TO PAR-NAME.                         SM2054.2
            049300     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            049400 COPY-TEST-6.                                                     SM2054.2
            049500     PERFORM  RET-2  7 TIMES.                                     SM2054.2
            049600     IF       RDF-KEYS EQUAL TO 201002000100001                   SM2054.2
            049700              PERFORM PASS-1 GO TO COPY-WRITE-6.                  SM2054.2
            049800     GO       TO COPY-FAIL-1-6.                                   SM2054.2
            049900 COPY-DELETE-6.                                                   SM2054.2
            050000     PERFORM  DE-LETE-1.                                          SM2054.2
            050100     GO       TO COPY-WRITE-6.                                    SM2054.2
            050200 COPY-FAIL-1-6.                                                   SM2054.2
            050300     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            050400     MOVE     201002000100001 TO CORRECT-18V0.                    SM2054.2
            050500     PERFORM  FAIL-1.                                             SM2054.2
            050600 COPY-WRITE-6.                                                    SM2054.2
            050700     MOVE     "COPY-TEST-6 " TO PAR-NAME.                         SM2054.2
            050800     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            050900 COPY-TEST-7.                                                     SM2054.2
            051000     PERFORM  RET-2.                                              SM2054.2
            051100     IF       RDF-KEYS EQUAL TO 900008000000000                   SM2054.2
            051200              PERFORM PASS-1 GO TO COPY-WRITE-7.                  SM2054.2
            051300     GO       TO COPY-FAIL-1-7.                                   SM2054.2
            051400 COPY-DELETE-7.                                                   SM2054.2
            051500     PERFORM  DE-LETE-1.                                          SM2054.2
            051600     GO       TO COPY-WRITE-7.                                    SM2054.2
            051700 COPY-FAIL-1-7.                                                   SM2054.2
            051800     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            051900     MOVE     900008000000000 TO CORRECT-18V0.                    SM2054.2
            052000     PERFORM  FAIL-1.                                             SM2054.2
            052100 COPY-WRITE-7.                                                    SM2054.2
            052200     MOVE     "COPY-TEST-7 " TO PAR-NAME.                         SM2054.2
            052300     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            052400 COPY-TEST-8.                                                     SM2054.2
            052500     PERFORM  RET-2.                                              SM2054.2
            052600     IF       RDF-KEYS EQUAL TO 900009000000000                   SM2054.2
            052700              PERFORM PASS-1 GO TO COPY-WRITE-8.                  SM2054.2
            052800     GO       TO COPY-FAIL-1-8.                                   SM2054.2
            052900 COPY-DELETE-8.                                                   SM2054.2
            053000     PERFORM  DE-LETE-1.                                          SM2054.2
            053100     GO       TO COPY-WRITE-8.                                    SM2054.2
            053200 COPY-FAIL-1-8.                                                   SM2054.2
            053300     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            053400     MOVE     900009000000000 TO CORRECT-18V0.                    SM2054.2
            053500     PERFORM  FAIL-1.                                             SM2054.2
            053600 COPY-WRITE-8.                                                    SM2054.2
            053700     MOVE     "COPY-TEST-8 " TO PAR-NAME.                         SM2054.2
            053800     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            053900 OUTP2 SECTION.                                                   SM2054.2
            054000 COPY-TEST-9.                                                     SM2054.2
            054100     RETURN   SORTFILE-2E END                                     SM2054.2
            054200              PERFORM PASS-1 GO TO COPY-WRITE-9.                  SM2054.2
            054300*    NOTE     THE FOLLOWING STATEMENTS SHOULD NOT BE EXECUTED.    SM2054.2
            054400     MOVE     RDF-KEYS TO COMPUTED-18V0.                          SM2054.2
            054500     MOVE     "END OF FILE NOT FOUND" TO RE-MARK.                 SM2054.2
            054600 COPY-DELETE-9.                                                   SM2054.2
            054700     PERFORM  DE-LETE-1.                                          SM2054.2
            054800 COPY-WRITE-9.                                                    SM2054.2
            054900     MOVE     "COPY-TEST-9 " TO PAR-NAME.                         SM2054.2
            055000     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            055100     CLOSE    SORTOUT-2E.                                         SM2054.2
            055200     GO       TO LIB2E-EXIT.                                      SM2054.2
            055300 OUTP3 SECTION.                                                   SM2054.2
            055400 RET-1.                                                           SM2054.2
            055500     RETURN   SORTFILE-2E RECORD AT END GO TO BAD-FILE.           SM2054.2
            055600     MOVE     S-RECORD TO SORTED.                                 SM2054.2
            055700     WRITE    SORTED.                                             SM2054.2
            055800 RET-2.                                                           SM2054.2
            055900     RETURN   SORTFILE-2E           END GO TO BAD-FILE.           SM2054.2
            056000     MOVE     S-RECORD TO SORTED.                                 SM2054.2
            056100     WRITE    SORTED.                                             SM2054.2
            056200 BAD-FILE.                                                        SM2054.2
            056300     PERFORM  FAIL-1.                                             SM2054.2
            056400     MOVE     "BAD-FILE" TO PAR-NAME.                             SM2054.2
            056500     MOVE     "EOF PREMATURELY FOUND" TO RE-MARK.                 SM2054.2
            056600     PERFORM  PRINT-DETAIL-1.                                     SM2054.2
            056700     CLOSE    SORTOUT-2E.                                         SM2054.2
            056800     GO TO LIB2E-EXIT.                                            SM2054.2
            056900 INSPT-1. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.       SM2054.2
            057000 PASS-1.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.         SM2054.2
            057100 FAIL-1.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.        SM2054.2
            057200 DE-LETE-1.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.    SM2054.2
            057300     MOVE "****TEST DELETED****" TO RE-MARK.                      SM2054.2
            057400 PRINT-DETAIL-1.                                                  SM2054.2
            057500     IF REC-CT NOT EQUAL TO ZERO                                  SM2054.2
            057600             MOVE "." TO PARDOT-X                                 SM2054.2
            057700             MOVE REC-CT TO DOTVALUE.                             SM2054.2
            057800     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE-1.    SM2054.2
            057900     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE-1             SM2054.2
            058000        PERFORM FAIL-ROUTINE-1 THRU FAIL-ROUTINE-EX-1             SM2054.2
            058100          ELSE PERFORM BAIL-OUT-1 THRU BAIL-OUT-EX-1.             SM2054.2
            058200     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM2054.2
            058300     MOVE SPACE TO CORRECT-X.                                     SM2054.2
            058400     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM2054.2
            058500     MOVE     SPACE TO RE-MARK.                                   SM2054.2
            058600 WRITE-LINE-1.                                                    SM2054.2
            058700     ADD 1 TO RECORD-COUNT.                                       SM2054.2
            058800*    IF RECORD-COUNT GREATER 50                                   SM2054.2
            058900*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM2054.2
            059000*        MOVE SPACE TO DUMMY-RECORD                               SM2054.2
            059100*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM2054.2
            059200*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN-1           SM2054.2
            059300*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN-1 2 TIMES   SM2054.2
            059400*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN-1        SM2054.2
            059500*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM2054.2
            059600*        MOVE ZERO TO RECORD-COUNT.                               SM2054.2
            059700     PERFORM WRT-LN-1.                                            SM2054.2
            059800 WRT-LN-1.                                                        SM2054.2
            059900     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM2054.2
            060000     MOVE SPACE TO DUMMY-RECORD.                                  SM2054.2
            060100 BLANK-LINE-PRINT-1.                                              SM2054.2
            060200     PERFORM WRT-LN-1.                                            SM2054.2
            060300 FAIL-ROUTINE-1.                                                  SM2054.2
            060400     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-1.     SM2054.2
            060500     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-1.      SM2054.2
            060600     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    SM2054.2
            060700     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-1 2 TIMES.   SM2054.2
            060800     GO TO FAIL-ROUTINE-EX-1.                                     SM2054.2
            060900 FAIL-RTN-WRITE-1.                                                SM2054.2
            061000     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE-1         SM2054.2
            061100     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE-1 2 TIMES. SM2054.2
            061200 FAIL-ROUTINE-EX-1. EXIT.                                         SM2054.2
            061300 BAIL-OUT-1.                                                      SM2054.2
            061400     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE-1.     SM2054.2
            061500     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX-1.             SM2054.2
            061600 BAIL-OUT-WRITE-1.                                                SM2054.2
            061700     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM2054.2
            061800     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-1 2 TIMES.   SM2054.2
            061900 BAIL-OUT-EX-1. EXIT.                                             SM2054.2
            062000 LIB2E-EXIT.                                                      SM2054.2
            062100     EXIT.                                                        SM2054.2
                  *END-OF,SM205A                                                                  
        """)
    )

    @Disabled("Requires continuation markers for tokens.")
    @Test
    fun sm206A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM206A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM2064.2
            000200 PROGRAM-ID.                                                      SM2064.2
            000300     SM206A.                                                      SM2064.2
            000400****************************************************************  SM2064.2
            000500*                                                              *  SM2064.2
            000600*    VALIDATION FOR:-                                          *  SM2064.2
            000700*                                                              *  SM2064.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2064.2
            000900*                                                              *  SM2064.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2064.2
            001100*                                                              *  SM2064.2
            001200****************************************************************  SM2064.2
            001300*                                                              *  SM2064.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM2064.2
            001500*                                                              *  SM2064.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM2064.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM2064.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM2064.2
            001900*                                                              *  SM2064.2
            002000****************************************************************  SM2064.2
            002100*                                                              *  SM2064.2
            002200*    PROGRAM NC206A TESTS THE "REPLACING" PHRASE OF THE "COPY" *  SM2064.2
            002300*    STATEMENT USING A VARIETY OF PSEUDO-TEXT OPERANDS.        *  SM2064.2
            002400*    MAXIMUM AND MINIMUM LENGTH TEXT WORDS ARE ALSO TESTED.    *  SM2064.2
            002500*                                                              *  SM2064.2
            002600****************************************************************  SM2064.2
            002700 ENVIRONMENT DIVISION.                                            SM2064.2
            002800 CONFIGURATION SECTION.                                           SM2064.2
            002900 SOURCE-COMPUTER.                                                 SM2064.2
            003000     XXXXX082.                                                    SM2064.2
            003100 OBJECT-COMPUTER.                                                 SM2064.2
            003200     XXXXX083.                                                    SM2064.2
            003300 INPUT-OUTPUT SECTION.                                            SM2064.2
            003400 FILE-CONTROL.                                                    SM2064.2
            003500     SELECT PRINT-FILE ASSIGN TO                                  SM2064.2
            003600     XXXXX055.                                                    SM2064.2
            003700 DATA DIVISION.                                                   SM2064.2
            003800 FILE SECTION.                                                    SM2064.2
            003900 FD  PRINT-FILE.                                                  SM2064.2
            004000 01  PRINT-REC PICTURE X(120).                                    SM2064.2
            004100 01  DUMMY-RECORD PICTURE X(120).                                 SM2064.2
            004200 WORKING-STORAGE SECTION.                                         SM2064.2
            004300 01  GRP-001.                                                     SM2064.2
            004400     02 GRP-002.                                                  SM2064.2
            004500        04 GRP-004.                                               SM2064.2
            004600           06 GRP-006.                                            SM2064.2
            004700              08 WRK-XN-00005-001 PIC X(5) VALUE "FIRST".         SM2064.2
            004800              08 WRK-XN-00050-O005F-001  OCCURS 5 TIMES.          SM2064.2
            004900                 10 WRK-XN-00005-O005-001 PIC X(5).               SM2064.2
            005000                 10 WRK-DS-05V00-O005-001 PIC S9(5).              SM2064.2
            005100     02 GRP-003.                                                  SM2064.2
            005200        04 GRP-004.                                               SM2064.2
            005300           06 GRP-006.                                            SM2064.2
            005400              08 WRK-XN-00005-001 PIC X(5) VALUE "SECON".         SM2064.2
            005500              08 WRK-XN-00050-O005F-001 OCCURS 5 TIMES.           SM2064.2
            005600                 10 WRK-XN-00005-O005-001 PIC X(5).               SM2064.2
            005700                 10 WRK-DS-05V00-O005-001 PIC S9(5).              SM2064.2
            005800 01  GRP-007.                                                     SM2064.2
            005900              08 WRK-XN-00005-001 PIC X(5) VALUE "THIRD".         SM2064.2
            006000 01  WRK-DS-09V00-901 PIC S9(9) VALUE ZERO.                       SM2064.2
            006100 01  WRK-DS-09V00-902 PIC S9(9) VALUE ZERO.                       SM2064.2
            006200 01  WRK-XN-00001     PIC  X.                                     SM2064.2
            006300 01  WRK-XN-00322     PIC  X(322).                                SM2064.2
            006400 01  FILLER REDEFINES WRK-XN-00322.                               SM2064.2
            006500   03  WRK-XN-00322-1         PIC X.                              SM2064.2
            006600   03  WRK-XN-00322-2-322.                                        SM2064.2
            006700     05  WRK-XN-00322-2-3     PIC X.                              SM2064.2
            006800     05  WRK-XN-00322-20      PIC X(20)                           SM2064.2
            006900                              OCCURS 16                           SM2064.2
            007000                              INDEXED BY X1.                      SM2064.2
            007100 01  WRK-DU-9                    PIC 9          VALUE ZERO.       SM2064.2
            007200 01  WRK-DU-99                   PIC 99         VALUE ZERO.       SM2064.2
            007300 01  WRK-DU-99-LONGER            PIC 99         VALUE ZERO.       SM2064.2
            007400 01  TEST-RESULTS.                                                SM2064.2
            007500     02 FILLER                   PIC X      VALUE SPACE.          SM2064.2
            007600     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM2064.2
            007700     02 FILLER                   PIC X      VALUE SPACE.          SM2064.2
            007800     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM2064.2
            007900     02 FILLER                   PIC X      VALUE SPACE.          SM2064.2
            008000     02  PAR-NAME.                                                SM2064.2
            008100       03 FILLER                 PIC X(19)  VALUE SPACE.          SM2064.2
            008200       03  PARDOT-X              PIC X      VALUE SPACE.          SM2064.2
            008300       03 DOTVALUE               PIC 99     VALUE ZERO.           SM2064.2
            008400     02 FILLER                   PIC X(8)   VALUE SPACE.          SM2064.2
            008500     02 RE-MARK                  PIC X(61).                       SM2064.2
            008600 01  TEST-COMPUTED.                                               SM2064.2
            008700     02 FILLER                   PIC X(30)  VALUE SPACE.          SM2064.2
            008800     02 FILLER                   PIC X(17)  VALUE                 SM2064.2
            008900            "       COMPUTED=".                                   SM2064.2
            009000     02 COMPUTED-X.                                               SM2064.2
            009100     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM2064.2
            009200     03 COMPUTED-N               REDEFINES COMPUTED-A             SM2064.2
            009300                                 PIC -9(9).9(9).                  SM2064.2
            009400     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM2064.2
            009500     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM2064.2
            009600     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM2064.2
            009700     03       CM-18V0 REDEFINES COMPUTED-A.                       SM2064.2
            009800         04 COMPUTED-18V0                    PIC -9(18).          SM2064.2
            009900         04 FILLER                           PIC X.               SM2064.2
            010000     03 FILLER PIC X(50) VALUE SPACE.                             SM2064.2
            010100 01  TEST-CORRECT.                                                SM2064.2
            010200     02 FILLER PIC X(30) VALUE SPACE.                             SM2064.2
            010300     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM2064.2
            010400     02 CORRECT-X.                                                SM2064.2
            010500     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM2064.2
            010600     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM2064.2
            010700     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM2064.2
            010800     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM2064.2
            010900     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM2064.2
            011000     03      CR-18V0 REDEFINES CORRECT-A.                         SM2064.2
            011100         04 CORRECT-18V0                     PIC -9(18).          SM2064.2
            011200         04 FILLER                           PIC X.               SM2064.2
            011300     03 FILLER PIC X(2) VALUE SPACE.                              SM2064.2
            011400     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM2064.2
            011500 01  CCVS-C-1.                                                    SM2064.2
            011600     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM2064.2
            011700-    "SS  PARAGRAPH-NAME                                          SM2064.2
            011800-    "       REMARKS".                                            SM2064.2
            011900     02 FILLER                     PIC X(20)    VALUE SPACE.      SM2064.2
            012000 01  CCVS-C-2.                                                    SM2064.2
            012100     02 FILLER                     PIC X        VALUE SPACE.      SM2064.2
            012200     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM2064.2
            012300     02 FILLER                     PIC X(15)    VALUE SPACE.      SM2064.2
            012400     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM2064.2
            012500     02 FILLER                     PIC X(94)    VALUE SPACE.      SM2064.2
            012600 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM2064.2
            012700 01  REC-CT                        PIC 99       VALUE ZERO.       SM2064.2
            012800 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM2064.2
            012900 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM2064.2
            013000 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM2064.2
            013100 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM2064.2
            013200 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM2064.2
            013300 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM2064.2
            013400 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM2064.2
            013500 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM2064.2
            013600 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM2064.2
            013700 01  CCVS-H-1.                                                    SM2064.2
            013800     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2064.2
            013900     02  FILLER                    PIC X(42)    VALUE             SM2064.2
            014000     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM2064.2
            014100     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2064.2
            014200 01  CCVS-H-2A.                                                   SM2064.2
            014300   02  FILLER                        PIC X(40)  VALUE SPACE.      SM2064.2
            014400   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM2064.2
            014500   02  FILLER                        PIC XXXX   VALUE             SM2064.2
            014600     "4.2 ".                                                      SM2064.2
            014700   02  FILLER                        PIC X(28)  VALUE             SM2064.2
            014800            " COPY - NOT FOR DISTRIBUTION".                       SM2064.2
            014900   02  FILLER                        PIC X(41)  VALUE SPACE.      SM2064.2
            015000                                                                  SM2064.2
            015100 01  CCVS-H-2B.                                                   SM2064.2
            015200   02  FILLER                        PIC X(15)  VALUE             SM2064.2
            015300            "TEST RESULT OF ".                                    SM2064.2
            015400   02  TEST-ID                       PIC X(9).                    SM2064.2
            015500   02  FILLER                        PIC X(4)   VALUE             SM2064.2
            015600            " IN ".                                               SM2064.2
            015700   02  FILLER                        PIC X(12)  VALUE             SM2064.2
            015800     " HIGH       ".                                              SM2064.2
            015900   02  FILLER                        PIC X(22)  VALUE             SM2064.2
            016000            " LEVEL VALIDATION FOR ".                             SM2064.2
            016100   02  FILLER                        PIC X(58)  VALUE             SM2064.2
            016200     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2064.2
            016300 01  CCVS-H-3.                                                    SM2064.2
            016400     02  FILLER                      PIC X(34)  VALUE             SM2064.2
            016500            " FOR OFFICIAL USE ONLY    ".                         SM2064.2
            016600     02  FILLER                      PIC X(58)  VALUE             SM2064.2
            016700     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2064.2
            016800     02  FILLER                      PIC X(28)  VALUE             SM2064.2
            016900            "  COPYRIGHT   1985 ".                                SM2064.2
            017000 01  CCVS-E-1.                                                    SM2064.2
            017100     02 FILLER                       PIC X(52)  VALUE SPACE.      SM2064.2
            017200     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM2064.2
            017300     02 ID-AGAIN                     PIC X(9).                    SM2064.2
            017400     02 FILLER                       PIC X(45)  VALUE SPACES.     SM2064.2
            017500 01  CCVS-E-2.                                                    SM2064.2
            017600     02  FILLER                      PIC X(31)  VALUE SPACE.      SM2064.2
            017700     02  FILLER                      PIC X(21)  VALUE SPACE.      SM2064.2
            017800     02 CCVS-E-2-2.                                               SM2064.2
            017900         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM2064.2
            018000         03 FILLER                   PIC X      VALUE SPACE.      SM2064.2
            018100         03 ENDER-DESC               PIC X(44)  VALUE             SM2064.2
            018200            "ERRORS ENCOUNTERED".                                 SM2064.2
            018300 01  CCVS-E-3.                                                    SM2064.2
            018400     02  FILLER                      PIC X(22)  VALUE             SM2064.2
            018500            " FOR OFFICIAL USE ONLY".                             SM2064.2
            018600     02  FILLER                      PIC X(12)  VALUE SPACE.      SM2064.2
            018700     02  FILLER                      PIC X(58)  VALUE             SM2064.2
            018800     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2064.2
            018900     02  FILLER                      PIC X(13)  VALUE SPACE.      SM2064.2
            019000     02 FILLER                       PIC X(15)  VALUE             SM2064.2
            019100             " COPYRIGHT 1985".                                   SM2064.2
            019200 01  CCVS-E-4.                                                    SM2064.2
            019300     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM2064.2
            019400     02 FILLER                       PIC X(4)   VALUE " OF ".     SM2064.2
            019500     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM2064.2
            019600     02 FILLER                       PIC X(40)  VALUE             SM2064.2
            019700      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM2064.2
            019800 01  XXINFO.                                                      SM2064.2
            019900     02 FILLER                       PIC X(19)  VALUE             SM2064.2
            020000            "*** INFORMATION ***".                                SM2064.2
            020100     02 INFO-TEXT.                                                SM2064.2
            020200       04 FILLER                     PIC X(8)   VALUE SPACE.      SM2064.2
            020300       04 XXCOMPUTED                 PIC X(20).                   SM2064.2
            020400       04 FILLER                     PIC X(5)   VALUE SPACE.      SM2064.2
            020500       04 XXCORRECT                  PIC X(20).                   SM2064.2
            020600     02 INF-ANSI-REFERENCE           PIC X(48).                   SM2064.2
            020700 01  HYPHEN-LINE.                                                 SM2064.2
            020800     02 FILLER  PIC IS X VALUE IS SPACE.                          SM2064.2
            020900     02 FILLER  PIC IS X(65)    VALUE IS "************************SM2064.2
            021000-    "*****************************************".                 SM2064.2
            021100     02 FILLER  PIC IS X(54)    VALUE IS "************************SM2064.2
            021200-    "******************************".                            SM2064.2
            021300 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM2064.2
            021400     "SM206A".                                                    SM2064.2
            021500 PROCEDURE DIVISION.                                              SM2064.2
            021600 CCVS1 SECTION.                                                   SM2064.2
            021700 OPEN-FILES.                                                      SM2064.2
            021800     OPEN     OUTPUT PRINT-FILE.                                  SM2064.2
            021900     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM2064.2
            022000     MOVE    SPACE TO TEST-RESULTS.                               SM2064.2
            022100     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM2064.2
            022200     GO TO CCVS1-EXIT.                                            SM2064.2
            022300 CLOSE-FILES.                                                     SM2064.2
            022400     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM2064.2
            022500 TERMINATE-CCVS.                                                  SM2064.2
            022600*    EXIT PROGRAM.                                                SM2064.2
            022700*TERMINATE-CALL.                                                  SM2064.2
            022800     STOP     RUN.                                                SM2064.2
            022900 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM2064.2
            023000 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM2064.2
            023100 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM2064.2
            023200 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM2064.2
            023300     MOVE "****TEST DELETED****" TO RE-MARK.                      SM2064.2
            023400 PRINT-DETAIL.                                                    SM2064.2
            023500     IF REC-CT NOT EQUAL TO ZERO                                  SM2064.2
            023600             MOVE "." TO PARDOT-X                                 SM2064.2
            023700             MOVE REC-CT TO DOTVALUE.                             SM2064.2
            023800     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM2064.2
            023900     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM2064.2
            024000        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM2064.2
            024100          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM2064.2
            024200     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM2064.2
            024300     MOVE SPACE TO CORRECT-X.                                     SM2064.2
            024400     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM2064.2
            024500     MOVE     SPACE TO RE-MARK.                                   SM2064.2
            024600 HEAD-ROUTINE.                                                    SM2064.2
            024700     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2064.2
            024800     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2064.2
            024900     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2064.2
            025000     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2064.2
            025100 COLUMN-NAMES-ROUTINE.                                            SM2064.2
            025200     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2064.2
            025300     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2064.2
            025400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM2064.2
            025500 END-ROUTINE.                                                     SM2064.2
            025600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM2064.2
            025700 END-RTN-EXIT.                                                    SM2064.2
            025800     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2064.2
            025900 END-ROUTINE-1.                                                   SM2064.2
            026000      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM2064.2
            026100      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM2064.2
            026200      ADD PASS-COUNTER TO ERROR-HOLD.                             SM2064.2
            026300*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM2064.2
            026400      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM2064.2
            026500      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM2064.2
            026600      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM2064.2
            026700      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM2064.2
            026800  END-ROUTINE-12.                                                 SM2064.2
            026900      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM2064.2
            027000     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM2064.2
            027100         MOVE "NO " TO ERROR-TOTAL                                SM2064.2
            027200         ELSE                                                     SM2064.2
            027300         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM2064.2
            027400     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM2064.2
            027500     PERFORM WRITE-LINE.                                          SM2064.2
            027600 END-ROUTINE-13.                                                  SM2064.2
            027700     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM2064.2
            027800         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM2064.2
            027900         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM2064.2
            028000     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM2064.2
            028100     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2064.2
            028200      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM2064.2
            028300          MOVE "NO " TO ERROR-TOTAL                               SM2064.2
            028400      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM2064.2
            028500      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM2064.2
            028600      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM2064.2
            028700     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2064.2
            028800 WRITE-LINE.                                                      SM2064.2
            028900     ADD 1 TO RECORD-COUNT.                                       SM2064.2
            029000*    IF RECORD-COUNT GREATER 50                                   SM2064.2
            029100*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM2064.2
            029200*        MOVE SPACE TO DUMMY-RECORD                               SM2064.2
            029300*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM2064.2
            029400*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM2064.2
            029500*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM2064.2
            029600*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM2064.2
            029700*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM2064.2
            029800*        MOVE ZERO TO RECORD-COUNT.                               SM2064.2
            029900     PERFORM WRT-LN.                                              SM2064.2
            030000 WRT-LN.                                                          SM2064.2
            030100     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM2064.2
            030200     MOVE SPACE TO DUMMY-RECORD.                                  SM2064.2
            030300 BLANK-LINE-PRINT.                                                SM2064.2
            030400     PERFORM WRT-LN.                                              SM2064.2
            030500 FAIL-ROUTINE.                                                    SM2064.2
            030600     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM2064.2
            030700     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM2064.2
            030800     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2064.2
            030900     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM2064.2
            031000     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2064.2
            031100     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2064.2
            031200     GO TO  FAIL-ROUTINE-EX.                                      SM2064.2
            031300 FAIL-ROUTINE-WRITE.                                              SM2064.2
            031400     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM2064.2
            031500     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM2064.2
            031600     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM2064.2
            031700     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM2064.2
            031800 FAIL-ROUTINE-EX. EXIT.                                           SM2064.2
            031900 BAIL-OUT.                                                        SM2064.2
            032000     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM2064.2
            032100     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM2064.2
            032200 BAIL-OUT-WRITE.                                                  SM2064.2
            032300     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM2064.2
            032400     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2064.2
            032500     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2064.2
            032600     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2064.2
            032700 BAIL-OUT-EX. EXIT.                                               SM2064.2
            032800 CCVS1-EXIT.                                                      SM2064.2
            032900     EXIT.                                                        SM2064.2
            033000 SECT-SM206-0001 SECTION.                                         SM2064.2
            033100*                                                                 SM2064.2
            033200*********************** COPY STATEMENT USED **********************SM2064.2
            033300*                                                                 SM2064.2
            033400*    COPY                                                    KP001SM2064.2
            033500*            REPLACING ==PERFORM FAIL. == BY ====.                SM2064.2
            033600*                                                                 SM2064.2
            033700******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            033800     COPY                                                    KP001SM2064.2
            033900             REPLACING ==PERFORM FAIL. == BY ====.                SM2064.2
            034000*********************** END OF COPIED TEXT ***********************SM2064.2
            034100 SECT-SM206-0002 SECTION.                                         SM2064.2
            034200 PST-INIT-002.                                                    SM2064.2
            034300     MOVE   +00005 TO WRK-DS-05V00-O005-001 OF GRP-002 (1).       SM2064.2
            034400     MOVE   +000000005 TO WRK-DS-09V00-901.                       SM2064.2
            034500 PST-TEST-002.                                                    SM2064.2
            034600*    THIS TEST EXERCISES THE REPLACING PHRASE BY REPLACING        SM2064.2
            034700*    PSEUDO-TEXT BY AN IDENTIFIER.                                SM2064.2
            034800     MOVE    "PSEUDO-TEXT/IDENTIFR" TO FEATURE.                   SM2064.2
            034900*                                                                 SM2064.2
            035000*********************** COPY STATEMENT USED **********************SM2064.2
            035100*                                                                 SM2064.2
            035200*    COPY                                                    KP002SM2064.2
            035300*            REPLACING == WRK-DS-09V00-901                        SM2064.2
            035400*                         SUBTRACT 1 FROM                         SM2064.2
            035500*                         WRK-DS-05V00-O005-001 IN GRP-002 (1)==  SM2064.2
            035600*            BY       WRK-DS-05V00-O005-001 IN WRK-XN-00050-0005  SM2064.2
            035700*-(COL 7)          F-001 IN GRP-006 IN GRP-004 IN GRP-002 IN GRP-0SM2064.2
            035800*-(COL 7)              01 (1).                                    SM2064.2
            035900*                                                                 SM2064.2
            036000******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            036100     COPY                                                    KP002SM2064.2
            036200             REPLACING == WRK-DS-09V00-901                        SM2064.2
            036300                          SUBTRACT 1 FROM                         SM2064.2
            036400                          WRK-DS-05V00-O005-001 IN GRP-002 (1)==  SM2064.2
            036500             BY         WRK-DS-05V00-O005-001 IN WRK-XN-00050-O005SM2064.2
            036600-                  F-001 IN GRP-006 IN GRP-004 IN GRP-002 IN GRP-0SM2064.2
            036700-                      01 (1).                                    SM2064.2
            036800*********************** END OF COPIED TEXT ***********************SM2064.2
            036900     MOVE    "PST-TEST-002" TO PAR-NAME.                          SM2064.2
            037000     MOVE    01 TO REC-CT.                                        SM2064.2
            037100     IF      WRK-DS-05V00-O005-001 OF GRP-002 (1) EQUAL TO +6     SM2064.2
            037200             PERFORM PASS                                         SM2064.2
            037300             ELSE                                                 SM2064.2
            037400             MOVE +6 TO CORRECT-18V0                              SM2064.2
            037500             MOVE WRK-DS-05V00-O005-001 OF GRP-002 (1) TO         SM2064.2
            037600                 COMPUTED-18V0                                    SM2064.2
            037700             PERFORM FAIL.                                        SM2064.2
            037800     PERFORM PRINT-DETAIL.                                        SM2064.2
            037900*    THIS IDENTIFIER SHOULD HAVE BEEN INCREMENTED BY ONE AS A     SM2064.2
            038000*    RESULT OF THE REPLACING PHRASE SPECIFIED IN THE COPY         SM2064.2
            038100*    STATEMENT.                                                   SM2064.2
            038200     ADD     +01 TO REC-CT.                                       SM2064.2
            038300     IF      WRK-DS-09V00-901 NOT EQUAL TO +5                     SM2064.2
            038400             MOVE +5 TO CORRECT-18V0                              SM2064.2
            038500             MOVE WRK-DS-09V00-901 TO COMPUTED-18V0               SM2064.2
            038600             PERFORM FAIL                                         SM2064.2
            038700             ELSE                                                 SM2064.2
            038800             PERFORM PASS.                                        SM2064.2
            038900*    THIS IDENTIFIER SHOULD NOT HAVE BEEN ALTERED AS A RESULT OF  SM2064.2
            039000*    THE REPLACING PHRASE OF THE COPY STATEMENT.                  SM2064.2
            039100     PERFORM PRINT-DETAIL.                                        SM2064.2
            039200     ADD     +01 TO REC-CT.                                       SM2064.2
            039300     IF      WRK-DS-05V00-O005-001 IN WRK-XN-00050-O005F-001 IN   SM2064.2
            039400             GRP-006 IN GRP-004 IN GRP-003 (2) EQUAL TO +9        SM2064.2
            039500             PERFORM PASS                                         SM2064.2
            039600             ELSE                                                 SM2064.2
            039700             MOVE WRK-DS-05V00-O005-001   IN                      SM2064.2
            039800                  WRK-XN-00050-O005F-001  IN                      SM2064.2
            039900                  GRP-006                 IN                      SM2064.2
            040000                  GRP-004                 IN                      SM2064.2
            040100                  GRP-003 (2) TO COMPUTED-18V0                    SM2064.2
            040200             MOVE +9 TO  CORRECT-18V0                             SM2064.2
            040300             PERFORM FAIL.                                        SM2064.2
            040400*    THE REPLACING PHRASE SHOULD NOT HAVE AFFECTED THE ACTION TO  SM2064.2
            040500*    BE TAKEN ON THIS IDENTIFIER IN THE TEXT BEING COPIED.        SM2064.2
            040600*                                                                 SM2064.2
            040700*                                                                 SM2064.2
            040800     PERFORM PRINT-DETAIL.                                        SM2064.2
            040900*THIS IS THE BEGINNING OF PST-TEST-003.                           SM2064.2
            041000*                                                                 SM2064.2
            041100 PST-INIT-003.                                                    SM2064.2
            041200     MOVE "PSEUDO-TEXT/LITERAL" TO FEATURE.                       SM2064.2
            041300     MOVE "PST-TEST-003" TO PAR-NAME.                             SM2064.2
            041400     MOVE    +00005 TO WRK-DS-05V00-O005-001 OF GRP-002 (3).      SM2064.2
            041500     MOVE    +000000005 TO WRK-DS-09V00-901.                      SM2064.2
            041600     MOVE    ZERO TO  WRK-DS-05V00-O005-001 IN GRP-003 (3).       SM2064.2
            041700*                                                                 SM2064.2
            041800*********************** COPY STATEMENT USED **********************SM2064.2
            041900*                                                                 SM2064.2
            042000*    COPY                                                    KP003SM2064.2
            042100*             REPLACING ==+00001== BY  +2                         SM2064.2
            042200*                       == 1 ==    BY  -3.                        SM2064.2
            042300*                                                                 SM2064.2
            042400******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            042500     COPY                                                    KP003SM2064.2
            042600             REPLACING ==+00001== BY  +2                          SM2064.2
            042700                       == 1 ==    BY  -3 .                        SM2064.2
            042800*********************** END OF COPIED TEXT ***********************SM2064.2
            042900 PST-TEST-003-1.                                                  SM2064.2
            043000     MOVE    01 TO REC-CT.                                        SM2064.2
            043100     IF      WRK-DS-05V00-O005-001 IN GRP-003 (3) EQUAL TO +00009 SM2064.2
            043200             PERFORM PASS                                         SM2064.2
            043300             ELSE                                                 SM2064.2
            043400             MOVE   +009 TO CORRECT-18V0                          SM2064.2
            043500             MOVE   WRK-DS-05V00-O005-001  IN                     SM2064.2
            043600                    GRP-003 (3) TO COMPUTED-18V0                  SM2064.2
            043700             PERFORM FAIL.                                        SM2064.2
            043800     PERFORM PRINT-DETAIL.                                        SM2064.2
            043900     ADD     +01 TO REC-CT.                                       SM2064.2
            044000     IF      WRK-DS-09V00-901 EQUAL TO +000000007                 SM2064.2
            044100             PERFORM PASS                                         SM2064.2
            044200             ELSE                                                 SM2064.2
            044300             PERFORM FAIL                                         SM2064.2
            044400             MOVE  +7 TO CORRECT-18V0                             SM2064.2
            044500             MOVE  WRK-DS-09V00-901 TO COMPUTED-18V0.             SM2064.2
            044600     PERFORM PRINT-DETAIL.                                        SM2064.2
            044700     ADD     +01 TO REC-CT.                                       SM2064.2
            044800     IF      WRK-DS-05V00-O005-001 OF GRP-002 (3) EQUAL TO +8     SM2064.2
            044900             PERFORM PASS                                         SM2064.2
            045000             ELSE                                                 SM2064.2
            045100             MOVE +8 TO CORRECT-18V0                              SM2064.2
            045200             PERFORM FAIL                                         SM2064.2
            045300             MOVE WRK-DS-05V00-O005-001 IN GRP-002 (3) TO         SM2064.2
            045400             COMPUTED-18V0.                                       SM2064.2
            045500     PERFORM PRINT-DETAIL.                                        SM2064.2
            045600     MOVE 0 TO WRK-DS-09V00-901.                                  SM2064.2
            045700*THE NEXT BIT OF CODING REPRESENTS WHAT WE FEEL IS PST-TEST-004,  SM2064.2
            045800*            WHAT YOU SEE IS WHAT THIS COMPILER FEELS IS          SM2064.2
            045900*                PST-TEST-004.                                    SM2064.2
            046000*                                                                 SM2064.2
            046100*********************** COPY STATEMENT USED **********************SM2064.2
            046200*                                                                 SM2064.2
            046300*            COPY                                            KP004SM2064.2
            046400*                REPLACING ==THIS IS NOT REAL COBOL-74 SYNTAX HOWESM2064.2
            046500*-(COL 7)        VER SHOVE==                                      SM2064.2
            046600*                BY MOVE                                          SM2064.2
            046700*                   == DELETE==                                   SM2064.2
            046800*                BY DE-LETE.                                      SM2064.2
            046900*                                                                 SM2064.2
            047000******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            047100             COPY                                        KP004    SM2064.2
            047200                 REPLACING ==THIS IS NOT REAL COBOL-74 SYNTAX HOWESM2064.2
            047300-                VER SHOVE==                                      SM2064.2
            047400                 BY MOVE                                          SM2064.2
            047500                     == DELETE==                                  SM2064.2
            047600                 BY  DE-LETE.                                     SM2064.2
            047700*********************** END OF COPIED TEXT ***********************SM2064.2
            047800 PST-WRITE-004.                                                   SM2064.2
            047900     MOVE    "PST-TEST-004" TO PAR-NAME.                          SM2064.2
            048000     MOVE    01 TO REC-CT.                                        SM2064.2
            048100     IF      WRK-DS-09V00-901 EQUAL TO 5                          SM2064.2
            048200             PERFORM PASS                                         SM2064.2
            048300             ELSE                                                 SM2064.2
            048400             PERFORM FAIL                                         SM2064.2
            048500             MOVE 5 TO CORRECT-18V0                               SM2064.2
            048600             MOVE WRK-DS-09V00-901 TO COMPUTED-18V0.              SM2064.2
            048700     PERFORM PRINT-DETAIL.                                        SM2064.2
            048800     ADD     1 TO REC-CT.                                         SM2064.2
            048900     IF      WRK-DS-09V00-902 EQUAL TO 2                          SM2064.2
            049000             PERFORM PASS                                         SM2064.2
            049100             ELSE                                                 SM2064.2
            049200             MOVE 2 TO CORRECT-18V0                               SM2064.2
            049300             MOVE WRK-DS-09V00-902 TO COMPUTED-18V0               SM2064.2
            049400             PERFORM FAIL.                                        SM2064.2
            049500     PERFORM PRINT-DETAIL.                                        SM2064.2
            049600 PST-TEST-005.                                                    SM2064.2
            049700     MOVE 0 TO WRK-DS-09V00-901.                                  SM2064.2
            049800*                                                                 SM2064.2
            049900*********************** COPY STATEMENT USED **********************SM2064.2
            050000*                                                                 SM2064.2
            050100*    COPY                                                    KP005SM2064.2
            050200*             REPLACING == 1 == BY  == 5 ==                       SM2064.2
            050300*                       == 5 == BY  == 7 ==.                      SM2064.2
            050400*                                                                 SM2064.2
            050500******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            050600     COPY                                                    KP005SM2064.2
            050700              REPLACING == 1 == BY  == 5 ==                       SM2064.2
            050800                        == 5 == BY  == 7 ==.                      SM2064.2
            050900*********************** END OF COPIED TEXT ***********************SM2064.2
            051000     IF WRK-DS-09V00-901 IS EQUAL TO 5                            SM2064.2
            051100         PERFORM PASS   GO TO PST-WRITE-005.                      SM2064.2
            051200     PERFORM FAIL.                                                SM2064.2
            051300     MOVE WRK-DS-09V00-901 TO COMPUTED-18V0.                      SM2064.2
            051400     MOVE 5 TO CORRECT-18V0.                                      SM2064.2
            051500     IF WRK-DS-09V00-901 IS EQUAL TO 7                            SM2064.2
            051600         MOVE "CASCADED REPLACEMENT PERFORMED" TO RE-MARK.        SM2064.2
            051700     GO TO PST-WRITE-005.                                         SM2064.2
            051800 PST-DELETE-005.                                                  SM2064.2
            051900     PERFORM DE-LETE.                                             SM2064.2
            052000 PST-WRITE-005.                                                   SM2064.2
            052100     MOVE "CASCADED REPLACE PST" TO FEATURE.                      SM2064.2
            052200     MOVE "PST-TEST-005" TO PAR-NAME.                             SM2064.2
            052300     MOVE 01 TO REC-CT.                                           SM2064.2
            052400     PERFORM PRINT-DETAIL.                                        SM2064.2
            052500 PST-TEST-006.                                                    SM2064.2
            052600     MOVE 0 TO WRK-DS-09V00-901.                                  SM2064.2
            052700*                                                                 SM2064.2
            052800*********************** COPY STATEMENT USED **********************SM2064.2
            052900*                                                                 SM2064.2
            053000*    COPY                                                    KP006SM2064.2
            053100*            REPLACING ==001== BY == 3 ==                         SM2064.2
            053200*                      ==005== BY == 7 ==.                        SM2064.2
            053300*                                                                 SM2064.2
            053400******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            053500     COPY                                                    KP006SM2064.2
            053600             REPLACING ==001== BY == 3 ==                         SM2064.2
            053700                       ==005== BY == 7 ==.                        SM2064.2
            053800*********************** END OF COPIED TEXT ***********************SM2064.2
            053900     IF WRK-DS-09V00-901 IS EQUAL TO 1005                         SM2064.2
            054000         PERFORM PASS   GO TO PST-WRITE-006.                      SM2064.2
            054100     PERFORM FAIL.                                                SM2064.2
            054200     MOVE WRK-DS-09V00-901 TO COMPUTED-18V0.                      SM2064.2
            054300     MOVE 1005 TO CORRECT-18V0.                                   SM2064.2
            054400     IF WRK-DS-09V00-901 IS EQUAL TO 10                           SM2064.2
            054500         MOVE "PART REPLACING, CONT IGNORED" TO RE-MARK.          SM2064.2
            054600     IF WRK-DS-09V00-901 IS EQUAL TO 37                           SM2064.2
            054700         MOVE "PART REPLACING, CONT HONORED" TO RE-MARK.          SM2064.2
            054800     GO TO PST-WRITE-006.                                         SM2064.2
            054900 PST-DELETE-006.                                                  SM2064.2
            055000     PERFORM DE-LETE.                                             SM2064.2
            055100 PST-WRITE-006.                                                   SM2064.2
            055200     MOVE "CONT LIT/PST PART RPL" TO FEATURE.                     SM2064.2
            055300     MOVE "PST-TEST-006" TO PAR-NAME.                             SM2064.2
            055400     PERFORM PRINT-DETAIL.                                        SM2064.2
            055500 PST-TEST-007.                                                    SM2064.2
            055600     PERFORM FAIL.                                                SM2064.2
            055700     SUBTRACT 1 FROM ERROR-COUNTER.                               SM2064.2
            055800*                                                                 SM2064.2
            055900*********************** COPY STATEMENT USED **********************SM2064.2
            056000*                                                                 SM2064.2
            056100*    COPY                                                   KP007 SM2064.2
            056200*        REPLACING ==FAIL. SUBTRACT 1 FROM ERROR-COUNTER. ==      SM2064.2
            056300*        BY ==PASS. ==.                                           SM2064.2
            056400*                                                                 SM2064.2
            056500******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            056600     COPY                                                   KP007 SM2064.2
            056700         REPLACING ==FAIL. SUBTRACT 1 FROM ERROR-COUNTER. ==      SM2064.2
            056800         BY ==PASS. ==.                                           SM2064.2
            056900*********************** END OF COPIED TEXT ***********************SM2064.2
            057000     IF P-OR-F IS EQUAL TO "FAIL*"  ADD 1 TO ERROR-COUNTER.       SM2064.2
            057100     GO TO PST-WRITE-007.                                         SM2064.2
            057200 PST-DELETE-007.                                                  SM2064.2
            057300     PERFORM DE-LETE.                                             SM2064.2
            057400 PST-WRITE-007.                                                   SM2064.2
            057500     MOVE "PST/EMBEDDED COMMENT" TO FEATURE.                      SM2064.2
            057600     MOVE "PST-TEST-007" TO PAR-NAME.                             SM2064.2
            057700     MOVE 01 TO REC-CT.                                           SM2064.2
            057800     PERFORM PRINT-DETAIL.                                        SM2064.2
            057900 PST-TEST-008.                                                    SM2064.2
            058000*    PERFORM PASS.                                                SM2064.2
            058100*                                                                 SM2064.2
            058200*********************** COPY STATEMENT USED **********************SM2064.2
            058300*                                                                 SM2064.2
            058400*D   COPY                                                  KP007. SM2064.2
            058500*                                                                 SM2064.2
            058600******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            058700*D   COPY                                                  KP007. SM2064.2
            058800*********************** END OF COPIED TEXT ***********************SM2064.2
            058900*    IF P-OR-F IS EQUAL TO "FAIL*"  ADD 1 TO ERROR-COUNTER.       SM2064.2
            059000*    GO TO PST-WRITE-008.                                         SM2064.2
            059100 PST-DELETE-008.                                                  SM2064.2
            059200     PERFORM DE-LETE.                                             SM2064.2
            059300 PST-WRITE-008.                                                   SM2064.2
            059400     MOVE "COPY IN DEBUG LINE" TO FEATURE.                        SM2064.2
            059500     MOVE "PST-TEST-008" TO PAR-NAME.                             SM2064.2
            059600     PERFORM PRINT-DETAIL.                                        SM2064.2
            059700 PST-TEST-009.                                                    SM2064.2
            059800     PERFORM FAIL.                                                SM2064.2
            059900     SUBTRACT 1 FROM ERROR-COUNTER.                               SM2064.2
            060000*                                                                 SM2064.2
            060100*********************** COPY STATEMENT USED **********************SM2064.2
            060200*                                                                 SM2064.2
            060300*    COPY                                                   KP008 SM2064.2
            060400*        REPLACING ==FAIL. THIS IS GARBAGE. SUBTRACT 1 FROM       SM2064.2
            060500*                    ERROR-COUNTER. ==                            SM2064.2
            060600*        BY  ==PASS. ==.                                          SM2064.2
            060700*                                                                 SM2064.2
            060800******************** COPIED TEXT BEGINS BELOW ********************SM2064.2
            060900     COPY                                                   KP008 SM2064.2
            061000         REPLACING ==FAIL. THIS IS GARBAGE. SUBTRACT 1 FROM       SM2064.2
            061100                     ERROR-COUNTER. ==                            SM2064.2
            061200         BY  ==PASS. ==.                                          SM2064.2
            061300*********************** END OF COPIED TEXT ***********************SM2064.2
            061400     IF P-OR-F IS EQUAL TO "FAIL*"  ADD 1 TO ERROR-COUNTER.       SM2064.2
            061500     GO TO PST-WRITE-009.                                         SM2064.2
            061600 PST-DELETE-009.                                                  SM2064.2
            061700     PERFORM DE-LETE.                                             SM2064.2
            061800 PST-WRITE-009.                                                   SM2064.2
            061900     MOVE "DEBUG LINE IN TEXT" TO FEATURE.                        SM2064.2
            062000     MOVE "PST-TEST-009" TO PAR-NAME.                             SM2064.2
            062100     PERFORM PRINT-DETAIL.                                        SM2064.2
            062200*                                                                 SM2064.2
            062300 PST-TEST-10.                                                     SM2064.2
            062400*    ===-->  MINIMUM LENGTH TEXT WORD  <--===                     SM2064.2
            062500     MOVE   "XII-2 2.3 SR8" TO ANSI-REFERENCE.                    SM2064.2
            062600     MOVE   "PST-TEST-10"   TO PAR-NAME.                          SM2064.2
            062700     MOVE   "T" TO WRK-XN-00001.                                  SM2064.2
            062800     GO TO   PST-TEST-10-0.                                       SM2064.2
            062900 PST-DELETE-10.                                                   SM2064.2
            063000     PERFORM DE-LETE.                                             SM2064.2
            063100     PERFORM PRINT-DETAIL.                                        SM2064.2
            063200     GO TO   PST-INIT-11.                                         SM2064.2
            063300 PST-TEST-10-0.                                                   SM2064.2
            063400********************* COPY TEXT USED ***************************  SM2064.2
            063500*    IF      WRK-XN-00001 = "G"                                *  SM2064.2
            063600*********************END OF COPY TEXT***************************  SM2064.2
            063700     COPY    KP009                                                SM2064.2
            063800             REPLACING =="G"== BY =="T"==.                        SM2064.2
            063900                                                                  SM2064.2
            064000             PERFORM PASS                                         SM2064.2
            064100             PERFORM PRINT-DETAIL                                 SM2064.2
            064200     ELSE                                                         SM2064.2
            064300             MOVE   "REPLACING SINGLE CHARACTER FAILED"           SM2064.2
            064400                  TO RE-MARK                                      SM2064.2
            064500             MOVE   "T"  TO CORRECT-X                             SM2064.2
            064600             MOVE    WRK-XN-00001 TO COMPUTED-X                   SM2064.2
            064700             PERFORM FAIL                                         SM2064.2
            064800             PERFORM PRINT-DETAIL.                                SM2064.2
            064900*                                                                 SM2064.2
            065000 PST-INIT-11.                                                     SM2064.2
            065100*    ===-->  MAXIMUM LENGTH TEXT WORD  <--===                     SM2064.2
            065200     MOVE   "XII-2 2.3 (SR8) AND XII-5 2.4(GR11)"                 SM2064.2
            065300          TO ANSI-REFERENCE.                                      SM2064.2
            065400     MOVE   "PST-TEST-11" TO PAR-NAME.                            SM2064.2
            065500     MOVE    SPACES      TO WRK-XN-00322.                         SM2064.2
            065600     MOVE    1 TO REC-CT.                                         SM2064.2
            065700 REP-TEST-11-0.                                                   SM2064.2
            065800********************* COPY TEXT USED ***************************  SM2064.2
            065900*    YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYSM2064.2
            066000*    YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYSM2064.2
            066100*    YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYSM2064.2
            066200*    YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYSM2064.2
            066300*    YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYSM2064.2
            066400*    YYYYYYYYYYYYYYYYY                                            SM2064.2
            066500*********************END OF COPY TEXT***************************  SM2064.2
            066600 PST-DELETE-11.                                                   SM2064.2
            066700     PERFORM DE-LETE.                                             SM2064.2
            066800     PERFORM PRINT-DETAIL.                                        SM2064.2
            066900     GO TO   CCVS-EXIT.                                           SM2064.2
            067000 PST-TEST-11-1.                                                   SM2064.2
            067100     MOVE   "PST-TEST-11-1" TO PAR-NAME.                          SM2064.2
            067200     IF      WRK-DU-9 = 6                                         SM2064.2
            067300             PERFORM PASS                                         SM2064.2
            067400             PERFORM PRINT-DETAIL                                 SM2064.2
            067500     ELSE                                                         SM2064.2
            067600             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM2064.2
            067700                  TO RE-MARK                                      SM2064.2
            067800             MOVE    6   TO CORRECT-N                             SM2064.2
            067900             MOVE    WRK-DU-9 TO COMPUTED-N                       SM2064.2
            068000             PERFORM FAIL                                         SM2064.2
            068100             PERFORM PRINT-DETAIL.                                SM2064.2
            068200     ADD     1 TO REC-CT.                                         SM2064.2
            068300 PST-TEST-11-2.                                                   SM2064.2
            068400     MOVE   "PST-TEST-11-2" TO PAR-NAME.                          SM2064.2
            068500     IF      WRK-DU-99 = 9                                        SM2064.2
            068600             PERFORM PASS                                         SM2064.2
            068700             PERFORM PRINT-DETAIL                                 SM2064.2
            068800     ELSE                                                         SM2064.2
            068900             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM2064.2
            069000                  TO RE-MARK                                      SM2064.2
            069100             MOVE    9   TO CORRECT-N                             SM2064.2
            069200             MOVE    WRK-DU-99 TO COMPUTED-N                      SM2064.2
            069300             PERFORM FAIL                                         SM2064.2
            069400             PERFORM PRINT-DETAIL.                                SM2064.2
            069500     ADD     1 TO REC-CT.                                         SM2064.2
            069600 PST-TEST-11-3.                                                   SM2064.2
            069700     MOVE   "PST-TEST-11-3" TO PAR-NAME.                          SM2064.2
            069800     IF      WRK-DU-99-LONGER = 10                                SM2064.2
            069900             PERFORM PASS                                         SM2064.2
            070000             PERFORM PRINT-DETAIL                                 SM2064.2
            070100     ELSE                                                         SM2064.2
            070200             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM2064.2
            070300                  TO RE-MARK                                      SM2064.2
            070400             MOVE    10  TO CORRECT-N                             SM2064.2
            070500             MOVE    WRK-DU-99-LONGER TO COMPUTED-N               SM2064.2
            070600             PERFORM FAIL                                         SM2064.2
            070700             PERFORM PRINT-DETAIL.                                SM2064.2
            070800*                                                                 SM2064.2
            070900 CCVS-EXIT SECTION.                                               SM2064.2
            071000 CCVS-999999.                                                     SM2064.2
            071100     GO TO CLOSE-FILES.                                           SM2064.2
                  *END-OF,SM206A                                                                  
        """)
    )

    @Test
    fun sm207A() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM207A                                                            
            000100 IDENTIFICATION DIVISION.                                         SM2074.2
            000200 PROGRAM-ID.                                                      SM2074.2
            000300     SM207A.                                                      SM2074.2
            000400****************************************************************  SM2074.2
            000500*                                                              *  SM2074.2
            000600*    VALIDATION FOR:-                                          *  SM2074.2
            000700*                                                              *  SM2074.2
            000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2074.2
            000900*                                                              *  SM2074.2
            001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2074.2
            001100*                                                              *  SM2074.2
            001200****************************************************************  SM2074.2
            001300*                                                              *  SM2074.2
            001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM2074.2
            001500*                                                              *  SM2074.2
            001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM2074.2
            001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM2074.2
            001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM2074.2
            001900*                                                              *  SM2074.2
            002000****************************************************************  SM2074.2
            002100*                                                              *  SM2074.2
            002200*    PROGRAM SM207A TESTS THE "COPY" STATEMENT USING TWO       *  SM2074.2
            002300*    DIFFERENT LIBRARY NAMES TO QUALIFY THE SAME TEXT NAME.    *  SM2074.2
            002400*                                                              *  SM2074.2
            002500****************************************************************  SM2074.2
            002600*            VP-ROUTINE CONTROL OF LIBRARY CREATION               SM2074.2
            002700*            --------------------------------------               SM2074.2
            002800*    WHEN THE LIBRARIES ARE PREPARED (CREATED) IN PREPARATION FOR SM2074.2
            002900*    RUNNING OF SM207, THE TEXT WHICH WILL BE PLACED IN THE       SM2074.2
            003000*    LIBRARY EQUATED TO X-47 SHOULD BE SELECTED FROM THE          SM2074.2
            003100*    POPULATION FILE USING THE PLUS-CARD "+ALTLB".  THE TEXT      SM2074.2
            003200*    WHICH WILL BE PLACED IN THE LIBRARY EQUATED TO X-48 SHOULD   SM2074.2
            003300*    BE SELECTED FROM THE POPULATION FILE USING THE PLUS-CARD     SM2074.2
            003400*    "+ALTL1,,,ALTLB".                                            SM2074.2
            003500                                                                  SM2074.2
            003600                                                                  SM2074.2
            003700 ENVIRONMENT DIVISION.                                            SM2074.2
            003800 CONFIGURATION SECTION.                                           SM2074.2
            003900 SOURCE-COMPUTER.                                                 SM2074.2
            004000     XXXXX082.                                                    SM2074.2
            004100 OBJECT-COMPUTER.                                                 SM2074.2
            004200     XXXXX083.                                                    SM2074.2
            004300 INPUT-OUTPUT SECTION.                                            SM2074.2
            004400 FILE-CONTROL.                                                    SM2074.2
            004500     SELECT PRINT-FILE ASSIGN TO                                  SM2074.2
            004600     XXXXX055.                                                    SM2074.2
            004700 DATA DIVISION.                                                   SM2074.2
            004800 FILE SECTION.                                                    SM2074.2
            004900 FD  PRINT-FILE.                                                  SM2074.2
            005000 01  PRINT-REC PICTURE X(120).                                    SM2074.2
            005100 01  DUMMY-RECORD PICTURE X(120).                                 SM2074.2
            005200 WORKING-STORAGE SECTION.                                         SM2074.2
            005300 01  TEST-RESULTS.                                                SM2074.2
            005400     02 FILLER                   PIC X      VALUE SPACE.          SM2074.2
            005500     02 FEATURE                  PIC X(20)  VALUE SPACE.          SM2074.2
            005600     02 FILLER                   PIC X      VALUE SPACE.          SM2074.2
            005700     02 P-OR-F                   PIC X(5)   VALUE SPACE.          SM2074.2
            005800     02 FILLER                   PIC X      VALUE SPACE.          SM2074.2
            005900     02  PAR-NAME.                                                SM2074.2
            006000       03 FILLER                 PIC X(19)  VALUE SPACE.          SM2074.2
            006100       03  PARDOT-X              PIC X      VALUE SPACE.          SM2074.2
            006200       03 DOTVALUE               PIC 99     VALUE ZERO.           SM2074.2
            006300     02 FILLER                   PIC X(8)   VALUE SPACE.          SM2074.2
            006400     02 RE-MARK                  PIC X(61).                       SM2074.2
            006500 01  TEST-COMPUTED.                                               SM2074.2
            006600     02 FILLER                   PIC X(30)  VALUE SPACE.          SM2074.2
            006700     02 FILLER                   PIC X(17)  VALUE                 SM2074.2
            006800            "       COMPUTED=".                                   SM2074.2
            006900     02 COMPUTED-X.                                               SM2074.2
            007000     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          SM2074.2
            007100     03 COMPUTED-N               REDEFINES COMPUTED-A             SM2074.2
            007200                                 PIC -9(9).9(9).                  SM2074.2
            007300     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         SM2074.2
            007400     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     SM2074.2
            007500     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     SM2074.2
            007600     03       CM-18V0 REDEFINES COMPUTED-A.                       SM2074.2
            007700         04 COMPUTED-18V0                    PIC -9(18).          SM2074.2
            007800         04 FILLER                           PIC X.               SM2074.2
            007900     03 FILLER PIC X(50) VALUE SPACE.                             SM2074.2
            008000 01  TEST-CORRECT.                                                SM2074.2
            008100     02 FILLER PIC X(30) VALUE SPACE.                             SM2074.2
            008200     02 FILLER PIC X(17) VALUE "       CORRECT =".                SM2074.2
            008300     02 CORRECT-X.                                                SM2074.2
            008400     03 CORRECT-A                  PIC X(20) VALUE SPACE.         SM2074.2
            008500     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      SM2074.2
            008600     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         SM2074.2
            008700     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     SM2074.2
            008800     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     SM2074.2
            008900     03      CR-18V0 REDEFINES CORRECT-A.                         SM2074.2
            009000         04 CORRECT-18V0                     PIC -9(18).          SM2074.2
            009100         04 FILLER                           PIC X.               SM2074.2
            009200     03 FILLER PIC X(2) VALUE SPACE.                              SM2074.2
            009300     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     SM2074.2
            009400 01  CCVS-C-1.                                                    SM2074.2
            009500     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASM2074.2
            009600-    "SS  PARAGRAPH-NAME                                          SM2074.2
            009700-    "       REMARKS".                                            SM2074.2
            009800     02 FILLER                     PIC X(20)    VALUE SPACE.      SM2074.2
            009900 01  CCVS-C-2.                                                    SM2074.2
            010000     02 FILLER                     PIC X        VALUE SPACE.      SM2074.2
            010100     02 FILLER                     PIC X(6)     VALUE "TESTED".   SM2074.2
            010200     02 FILLER                     PIC X(15)    VALUE SPACE.      SM2074.2
            010300     02 FILLER                     PIC X(4)     VALUE "FAIL".     SM2074.2
            010400     02 FILLER                     PIC X(94)    VALUE SPACE.      SM2074.2
            010500 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       SM2074.2
            010600 01  REC-CT                        PIC 99       VALUE ZERO.       SM2074.2
            010700 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       SM2074.2
            010800 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       SM2074.2
            010900 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       SM2074.2
            011000 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       SM2074.2
            011100 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       SM2074.2
            011200 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       SM2074.2
            011300 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      SM2074.2
            011400 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       SM2074.2
            011500 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     SM2074.2
            011600 01  CCVS-H-1.                                                    SM2074.2
            011700     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2074.2
            011800     02  FILLER                    PIC X(42)    VALUE             SM2074.2
            011900     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 SM2074.2
            012000     02  FILLER                    PIC X(39)    VALUE SPACES.     SM2074.2
            012100 01  CCVS-H-2A.                                                   SM2074.2
            012200   02  FILLER                        PIC X(40)  VALUE SPACE.      SM2074.2
            012300   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  SM2074.2
            012400   02  FILLER                        PIC XXXX   VALUE             SM2074.2
            012500     "4.2 ".                                                      SM2074.2
            012600   02  FILLER                        PIC X(28)  VALUE             SM2074.2
            012700            " COPY - NOT FOR DISTRIBUTION".                       SM2074.2
            012800   02  FILLER                        PIC X(41)  VALUE SPACE.      SM2074.2
            012900                                                                  SM2074.2
            013000 01  CCVS-H-2B.                                                   SM2074.2
            013100   02  FILLER                        PIC X(15)  VALUE             SM2074.2
            013200            "TEST RESULT OF ".                                    SM2074.2
            013300   02  TEST-ID                       PIC X(9).                    SM2074.2
            013400   02  FILLER                        PIC X(4)   VALUE             SM2074.2
            013500            " IN ".                                               SM2074.2
            013600   02  FILLER                        PIC X(12)  VALUE             SM2074.2
            013700     " HIGH       ".                                              SM2074.2
            013800   02  FILLER                        PIC X(22)  VALUE             SM2074.2
            013900            " LEVEL VALIDATION FOR ".                             SM2074.2
            014000   02  FILLER                        PIC X(58)  VALUE             SM2074.2
            014100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2074.2
            014200 01  CCVS-H-3.                                                    SM2074.2
            014300     02  FILLER                      PIC X(34)  VALUE             SM2074.2
            014400            " FOR OFFICIAL USE ONLY    ".                         SM2074.2
            014500     02  FILLER                      PIC X(58)  VALUE             SM2074.2
            014600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM2074.2
            014700     02  FILLER                      PIC X(28)  VALUE             SM2074.2
            014800            "  COPYRIGHT   1985 ".                                SM2074.2
            014900 01  CCVS-E-1.                                                    SM2074.2
            015000     02 FILLER                       PIC X(52)  VALUE SPACE.      SM2074.2
            015100     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              SM2074.2
            015200     02 ID-AGAIN                     PIC X(9).                    SM2074.2
            015300     02 FILLER                       PIC X(45)  VALUE SPACES.     SM2074.2
            015400 01  CCVS-E-2.                                                    SM2074.2
            015500     02  FILLER                      PIC X(31)  VALUE SPACE.      SM2074.2
            015600     02  FILLER                      PIC X(21)  VALUE SPACE.      SM2074.2
            015700     02 CCVS-E-2-2.                                               SM2074.2
            015800         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      SM2074.2
            015900         03 FILLER                   PIC X      VALUE SPACE.      SM2074.2
            016000         03 ENDER-DESC               PIC X(44)  VALUE             SM2074.2
            016100            "ERRORS ENCOUNTERED".                                 SM2074.2
            016200 01  CCVS-E-3.                                                    SM2074.2
            016300     02  FILLER                      PIC X(22)  VALUE             SM2074.2
            016400            " FOR OFFICIAL USE ONLY".                             SM2074.2
            016500     02  FILLER                      PIC X(12)  VALUE SPACE.      SM2074.2
            016600     02  FILLER                      PIC X(58)  VALUE             SM2074.2
            016700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM2074.2
            016800     02  FILLER                      PIC X(13)  VALUE SPACE.      SM2074.2
            016900     02 FILLER                       PIC X(15)  VALUE             SM2074.2
            017000             " COPYRIGHT 1985".                                   SM2074.2
            017100 01  CCVS-E-4.                                                    SM2074.2
            017200     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      SM2074.2
            017300     02 FILLER                       PIC X(4)   VALUE " OF ".     SM2074.2
            017400     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      SM2074.2
            017500     02 FILLER                       PIC X(40)  VALUE             SM2074.2
            017600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       SM2074.2
            017700 01  XXINFO.                                                      SM2074.2
            017800     02 FILLER                       PIC X(19)  VALUE             SM2074.2
            017900            "*** INFORMATION ***".                                SM2074.2
            018000     02 INFO-TEXT.                                                SM2074.2
            018100       04 FILLER                     PIC X(8)   VALUE SPACE.      SM2074.2
            018200       04 XXCOMPUTED                 PIC X(20).                   SM2074.2
            018300       04 FILLER                     PIC X(5)   VALUE SPACE.      SM2074.2
            018400       04 XXCORRECT                  PIC X(20).                   SM2074.2
            018500     02 INF-ANSI-REFERENCE           PIC X(48).                   SM2074.2
            018600 01  HYPHEN-LINE.                                                 SM2074.2
            018700     02 FILLER  PIC IS X VALUE IS SPACE.                          SM2074.2
            018800     02 FILLER  PIC IS X(65)    VALUE IS "************************SM2074.2
            018900-    "*****************************************".                 SM2074.2
            019000     02 FILLER  PIC IS X(54)    VALUE IS "************************SM2074.2
            019100-    "******************************".                            SM2074.2
            019200 01  CCVS-PGM-ID                     PIC X(9)   VALUE             SM2074.2
            019300     "SM207A".                                                    SM2074.2
            019400 PROCEDURE DIVISION.                                              SM2074.2
            019500 CCVS1 SECTION.                                                   SM2074.2
            019600 OPEN-FILES.                                                      SM2074.2
            019700     OPEN     OUTPUT PRINT-FILE.                                  SM2074.2
            019800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM2074.2
            019900     MOVE    SPACE TO TEST-RESULTS.                               SM2074.2
            020000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM2074.2
            020100     GO TO CCVS1-EXIT.                                            SM2074.2
            020200 CLOSE-FILES.                                                     SM2074.2
            020300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM2074.2
            020400 TERMINATE-CCVS.                                                  SM2074.2
            020500*    EXIT PROGRAM.                                                SM2074.2
            020600*TERMINATE-CALL.                                                  SM2074.2
            020700     STOP     RUN.                                                SM2074.2
            020800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM2074.2
            020900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM2074.2
            021000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM2074.2
            021100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM2074.2
            021200     MOVE "****TEST DELETED****" TO RE-MARK.                      SM2074.2
            021300 PRINT-DETAIL.                                                    SM2074.2
            021400     IF REC-CT NOT EQUAL TO ZERO                                  SM2074.2
            021500             MOVE "." TO PARDOT-X                                 SM2074.2
            021600             MOVE REC-CT TO DOTVALUE.                             SM2074.2
            021700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM2074.2
            021800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM2074.2
            021900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM2074.2
            022000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM2074.2
            022100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM2074.2
            022200     MOVE SPACE TO CORRECT-X.                                     SM2074.2
            022300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM2074.2
            022400     MOVE     SPACE TO RE-MARK.                                   SM2074.2
            022500 HEAD-ROUTINE.                                                    SM2074.2
            022600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2074.2
            022700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM2074.2
            022800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2074.2
            022900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM2074.2
            023000 COLUMN-NAMES-ROUTINE.                                            SM2074.2
            023100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2074.2
            023200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2074.2
            023300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM2074.2
            023400 END-ROUTINE.                                                     SM2074.2
            023500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM2074.2
            023600 END-RTN-EXIT.                                                    SM2074.2
            023700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2074.2
            023800 END-ROUTINE-1.                                                   SM2074.2
            023900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM2074.2
            024000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM2074.2
            024100      ADD PASS-COUNTER TO ERROR-HOLD.                             SM2074.2
            024200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM2074.2
            024300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM2074.2
            024400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM2074.2
            024500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM2074.2
            024600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM2074.2
            024700  END-ROUTINE-12.                                                 SM2074.2
            024800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM2074.2
            024900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM2074.2
            025000         MOVE "NO " TO ERROR-TOTAL                                SM2074.2
            025100         ELSE                                                     SM2074.2
            025200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM2074.2
            025300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM2074.2
            025400     PERFORM WRITE-LINE.                                          SM2074.2
            025500 END-ROUTINE-13.                                                  SM2074.2
            025600     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM2074.2
            025700         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM2074.2
            025800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM2074.2
            025900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM2074.2
            026000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2074.2
            026100      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM2074.2
            026200          MOVE "NO " TO ERROR-TOTAL                               SM2074.2
            026300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM2074.2
            026400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM2074.2
            026500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM2074.2
            026600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM2074.2
            026700 WRITE-LINE.                                                      SM2074.2
            026800     ADD 1 TO RECORD-COUNT.                                       SM2074.2
            026900*    IF RECORD-COUNT GREATER 50                                   SM2074.2
            027000*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM2074.2
            027100*        MOVE SPACE TO DUMMY-RECORD                               SM2074.2
            027200*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM2074.2
            027300*        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM2074.2
            027400*        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM2074.2
            027500*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM2074.2
            027600*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM2074.2
            027700*        MOVE ZERO TO RECORD-COUNT.                               SM2074.2
            027800     PERFORM WRT-LN.                                              SM2074.2
            027900 WRT-LN.                                                          SM2074.2
            028000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM2074.2
            028100     MOVE SPACE TO DUMMY-RECORD.                                  SM2074.2
            028200 BLANK-LINE-PRINT.                                                SM2074.2
            028300     PERFORM WRT-LN.                                              SM2074.2
            028400 FAIL-ROUTINE.                                                    SM2074.2
            028500     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM2074.2
            028600     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM2074.2
            028700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2074.2
            028800     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM2074.2
            028900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2074.2
            029000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2074.2
            029100     GO TO  FAIL-ROUTINE-EX.                                      SM2074.2
            029200 FAIL-ROUTINE-WRITE.                                              SM2074.2
            029300     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM2074.2
            029400     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM2074.2
            029500     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM2074.2
            029600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM2074.2
            029700 FAIL-ROUTINE-EX. EXIT.                                           SM2074.2
            029800 BAIL-OUT.                                                        SM2074.2
            029900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM2074.2
            030000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM2074.2
            030100 BAIL-OUT-WRITE.                                                  SM2074.2
            030200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM2074.2
            030300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM2074.2
            030400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM2074.2
            030500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM2074.2
            030600 BAIL-OUT-EX. EXIT.                                               SM2074.2
            030700 CCVS1-EXIT.                                                      SM2074.2
            030800     EXIT.                                                        SM2074.2
            030900 SECT-SM207A-001 SECTION.                                         SM2074.2
            031000 QUAL-TEST-01.                                                    SM2074.2
            031100     MOVE "NOTHING COPIED" TO RE-MARK.                            SM2074.2
            031200     PERFORM FAIL.                                                SM2074.2
            031300*                                                                 SM2074.2
            031400*********************** COPY STATEMENT USED **********************SM2074.2
            031500*                                                                 SM2074.2
            031600*    COPY ALTLB OF                                                SM2074.2
            031700*    XXXXX047.                                                    SM2074.2
            031800*                                                                 SM2074.2
            031900******************** COPIED TEXT BEGINS BELOW ********************SM2074.2
            032000     COPY ALTLB OF                                                SM2074.2
            032100     XXXXX047.                                                    SM2074.2
            032200*********************** END OF COPIED TEXT ***********************SM2074.2
            032300     GO TO QUAL-WRITE-01.                                         SM2074.2
            032400 QUAL-DELETE-01.                                                  SM2074.2
            032500     PERFORM DE-LETE.                                             SM2074.2
            032600 QUAL-WRITE-01.                                                   SM2074.2
            032700     MOVE "QUAL-TEST-01" TO PAR-NAME.                             SM2074.2
            032800     MOVE "QUALIFIED LIBRY NAME" TO FEATURE.                      SM2074.2
            032900     PERFORM PRINT-DETAIL.                                        SM2074.2
            033000 QUAL-TEST-02.                                                    SM2074.2
            033100     ADD 1 TO ERROR-COUNTER.                                      SM2074.2
            033200*                                                                 SM2074.2
            033300*********************** COPY STATEMENT USED **********************SM2074.2
            033400*                                                                 SM2074.2
            033500*    COPY ALTLB IN                                                SM2074.2
            033600*    XXXXX048.                                                    SM2074.2
            033700*                                                                 SM2074.2
            033800******************** COPIED TEXT BEGINS BELOW ********************SM2074.2
            033900     COPY ALTLB IN                                                SM2074.2
            034000     XXXXX048.                                                    SM2074.2
            034100*********************** END OF COPIED TEXT ***********************SM2074.2
            034200     IF P-OR-F IS EQUAL TO "PASS "                                SM2074.2
            034300         PERFORM FAIL                                             SM2074.2
            034400         MOVE "TEXT COPIED FROM WRONG LIBRARY" TO RE-MARK         SM2074.2
            034500         GO TO QUAL-WRITE-02.                                     SM2074.2
            034600     IF P-OR-F IS EQUAL TO "FAIL*"                                SM2074.2
            034700         PERFORM PASS                                             SM2074.2
            034800         SUBTRACT 1 FROM ERROR-COUNTER                            SM2074.2
            034900         MOVE SPACES TO RE-MARK                                   SM2074.2
            035000         GO TO QUAL-WRITE-02.                                     SM2074.2
            035100     PERFORM FAIL.                                                SM2074.2
            035200     SUBTRACT 1 FROM ERROR-COUNTER.                               SM2074.2
            035300     MOVE "NOTHING COPIED" TO RE-MARK.                            SM2074.2
            035400     GO TO QUAL-WRITE-02.                                         SM2074.2
            035500 QUAL-DELETE-02.                                                  SM2074.2
            035600     PERFORM DE-LETE.                                             SM2074.2
            035700 QUAL-WRITE-02.                                                   SM2074.2
            035800     MOVE "QUAL-TEST-02" TO PAR-NAME.                             SM2074.2
            035900     PERFORM PRINT-DETAIL.                                        SM2074.2
            036000 CCVS-EXIT SECTION.                                               SM2074.2
            036100 CCVS-999999.                                                     SM2074.2
            036200     GO TO CLOSE-FILES.                                           SM2074.2
                  *END-OF,SM207A                                                                  
        """)
    )

    @Test
    fun sm301M() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM301M                                                            
            000100 IDENTIFICATION DIVISION.                                         SM3014.2
            000200 PROGRAM-ID.                                                      SM3014.2
            000300      SM301M.                                                     SM3014.2
            000400*The following program tests the flagging of the intermediate     SM3014.2
            000500*subset COPY feature.                                             SM3014.2
            000600 ENVIRONMENT DIVISION.                                            SM3014.2
            000700 CONFIGURATION SECTION.                                           SM3014.2
            000800 SOURCE-COMPUTER.                                                 SM3014.2
            000900     XXXXX082.                                                    SM3014.2
            001000 OBJECT-COMPUTER.                                                 SM3014.2
            001100     XXXXX083.                                                    SM3014.2
            001200                                                                  SM3014.2
            001300                                                                  SM3014.2
            001400 DATA DIVISION.                                                   SM3014.2
            001500                                                                  SM3014.2
            001600 PROCEDURE DIVISION.                                              SM3014.2
            001700                                                                  SM3014.2
            001800 SM301M-CONTROL.                                                  SM3014.2
            001900     PERFORM SM301M-COPY.                                         SM3014.2
            002000     STOP RUN.                                                    SM3014.2
            002100                                                                  SM3014.2
            002200 SM301M-COPY.                                                     SM3014.2
            002300*Message expected for following statement: NON-CONFORMING STANDARDSM3014.2
            002400     COPY KSM31.                                                  SM3014.2
            002500                                                                  SM3014.2
            002600                                                                  SM3014.2
            002700*TOTAL NUMBER OF FLAGS EXPECTED = 1.                              SM3014.2
                  *END-OF,SM301M                                                                  
        """)
    )

    @Test
    fun sm401M() = rewriteRun(
        cobolCopy("""
                  *HEADER,COBOL,SM401M                                                            
            000100 IDENTIFICATION DIVISION.                                         SM4014.2
            000200 PROGRAM-ID.                                                      SM4014.2
            000300     SM401M.                                                      SM4014.2
            000400*THE FOLLOWING PROGRAM TESTS THE FLAGGING OF HIGH                 SM4014.2
            000500*SUBSET FEATURES THAT ARE USED IN SOURCE TEXT                     SM4014.2
            000600*MANIPULATION.                                                    SM4014.2
            000700 ENVIRONMENT DIVISION.                                            SM4014.2
            000800 CONFIGURATION SECTION.                                           SM4014.2
            000900 SOURCE-COMPUTER.                                                 SM4014.2
            001000     XXXXX082.                                                    SM4014.2
            001100 OBJECT-COMPUTER.                                                 SM4014.2
            001200     XXXXX083.                                                    SM4014.2
            001300                                                                  SM4014.2
            001400                                                                  SM4014.2
            001500 DATA DIVISION.                                                   SM4014.2
            001600 PROCEDURE DIVISION.                                              SM4014.2
            001700                                                                  SM4014.2
            001800 SM401M-CONTROL.                                                  SM4014.2
            001900     PERFORM SM401M-COPYREP THRU SM401M-REPL.                     SM4014.2
            002000     STOP RUN.                                                    SM4014.2
            002100                                                                  SM4014.2
            002200 SM401M-COPYREP.                                                  SM4014.2
            002300*Message expected for following statement: NON-CONFORMING STANDARDSM3014.2
            002400     COPY KSM41 REPLACING "PIG" BY "HORSE".                       SM4014.2
            002500                                                                  SM4014.2
            002600 SM401M-REPL.                                                     SM4014.2
            002700     REPLACE OFF.                                                 SM4014.2
            002800*Message expected for above statement: NON-CONFORMING STANDARD    SM4014.2
            002900                                                                  SM4014.2
            003000*TOTAL NUMBER OF FLAGS EXPECTED = 2.                              SM4014.2
                  *END-OF,SM401M                                                                  
        """)
    )
}
