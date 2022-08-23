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

import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.cobol.Assertions.cobol
import org.openrewrite.cobol.CobolVisitor
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import org.openrewrite.test.RewriteTest.toRecipe

class CobolNistDBTest : RewriteTest {

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(toRecipe {
            object : CobolVisitor<ExecutionContext>() {
                override fun visitSpace(space: Space, p: ExecutionContext): Space {
                    if (space.whitespace.trim().isNotEmpty()) {
                        return space.withWhitespace("(~~>${space.whitespace}<~~)")
                    }
                    return space
                }
            }
        })
    }

    @Disabled("Requires `S` and `Y` indicators")
    @Test
    fun db1014_2() = rewriteRun(
        cobol(
            """
                  *HEADER,COBOL,DB101A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB1014.2
            000200 PROGRAM-ID.                                                      DB1014.2
            000300     DB101A.                                                      DB1014.2
            000400 AUTHOR.                                                          DB1014.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB1014.2
            000600 INSTALLATION.                                                    DB1014.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB1014.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB1014.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB1014.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB1014.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB1014.2
            001200                                                                  DB1014.2
            001300     PHONE   (703) 756-6153                                       DB1014.2
            001400                                                                  DB1014.2
            001500     " HIGH       ".                                              DB1014.2
            001600 DATE-WRITTEN.                                                    DB1014.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB1014.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB1014.2
            001900     "4.2 ".                                                      DB1014.2
            002000 SECURITY.                                                        DB1014.2
            002100     NONE.                                                        DB1014.2
            002200*                                                                 DB1014.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB1014.2
            002400*                                                                 DB1014.2
            002500*                       PROGRAM ABSTRACT                          DB1014.2
            002600*                                                                 DB1014.2
            002700*    DB101A TESTS THE BASIC OPERATION OF THE DEBUG MODULE WHEN    DB1014.2
            002800*    BOTH THE COMPILE AND OBJECT TIME DEBUGGING SWITCHES ARE      DB1014.2
            002900*    TURNED ON.  THE PROGRAM CONTAINS BOTH DEBUG LINES AND SIMPLE DB1014.2
            003000*    DEBUGGING PROCEDURES.  THE DEBUGGING PROCEDURES ARE SPECI-   DB1014.2
            003100*    FIED FOR PROCEDURE-NAMES AND PROCEDURE-NAME SERIES.  THE     DB1014.2
            003200*    FOLLOWING CONDITIONS ARE EVALUATED FOR THE "DEBUG-ITEM"      DB1014.2
            003300*    REGISTER                                                     DB1014.2
            003400*                                                                 DB1014.2
            003500*        (1)  START OF PROGRAM                                    DB1014.2
            003600*        (2)  REFERENCE BY "ALTER"                                DB1014.2
            003700*        (3)  REFERENCE BY "GO TO"                                DB1014.2
            003800*        (4)  REFERENCE BY "PERFORM"                              DB1014.2
            003900*        (5)  SEQUENTIAL PASSAGE OF CONTROL  (FALL THROUGH)       DB1014.2
            004000*                                                                 DB1014.2
            004100*    BEFORE BEGINNING EXECUTION OF THE OBJECT PROGRAM,            DB1014.2
            004200*    WHATEVER JOB CONTROL LANGUAGE IS NECESSARY TO ACTIVATE       DB1014.2
            004300*    (TURN ON) THE OBJECT TIME DEBUGGING SWITCH SHOULD BE         DB1014.2
            004400*    SUBMITTED.                                                   DB1014.2
            004500*                                                                 DB1014.2
            004600*                                                                 DB1014.2
            004700*                                                                 DB1014.2
            004800*                                                                 DB1014.2
            004900 ENVIRONMENT DIVISION.                                            DB1014.2
            005000 CONFIGURATION SECTION.                                           DB1014.2
            005100 SOURCE-COMPUTER.                                                 DB1014.2
            005200     XXXXX082                                                     DB1014.2
            005300         WITH DEBUGGING MODE.                                     DB1014.2
            005400 OBJECT-COMPUTER.                                                 DB1014.2
            005500     XXXXX083.                                                    DB1014.2
            005600 INPUT-OUTPUT SECTION.                                            DB1014.2
            005700 FILE-CONTROL.                                                    DB1014.2
            005800     SELECT PRINT-FILE ASSIGN TO                                  DB1014.2
            005900     XXXXX055.                                                    DB1014.2
            006000 DATA DIVISION.                                                   DB1014.2
            006100 FILE SECTION.                                                    DB1014.2
            006200 FD  PRINT-FILE                                                   DB1014.2
            006300     LABEL RECORDS                                                DB1014.2
            006400     XXXXX084                                                     DB1014.2
            006500     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB1014.2
            006600 01  PRINT-REC PICTURE X(120).                                    DB1014.2
            006700 01  DUMMY-RECORD PICTURE X(120).                                 DB1014.2
            006800 WORKING-STORAGE SECTION.                                         DB1014.2
            006900 77  A PIC 9 COMP VALUE 1.                                        DB1014.2
            007000 77  B PIC 9 COMP VALUE 5.                                        DB1014.2
            007100 77  C PIC 9 COMP VALUE 9.                                        DB1014.2
            007200 77  D PIC 99 COMP.                                               DB1014.2
            007300 77  RESULT-FLAG PIC 99 VALUE 0.                                  DB1014.2
            007400 77  DBLINE-HOLD PIC X(6).                                        DB1014.2
            007500 77  DBNAME-HOLD PIC X(30).                                       DB1014.2
            007600 77  DBCONT-HOLD PIC X(30).                                       DB1014.2
            007700 77  FIVE PIC 9 COMP VALUE 5.                                     DB1014.2
            007800 01  SIZE-19.                                                     DB1014.2
            007900     02  FILLER PIC X.                                            DB1014.2
            008000     02  SIZE-18.                                                 DB1014.2
            008100         03  FILLER PIC X.                                        DB1014.2
            008200         03  SIZE-17.                                             DB1014.2
            008300             04  FILLER PIC X.                                    DB1014.2
            008400             04  SIZE-16.                                         DB1014.2
            008500                 05  FILLER PIC X.                                DB1014.2
            008600                 05  SIZE-15.                                     DB1014.2
            008700                     06  FILLER PIC X.                            DB1014.2
            008800                     06  SIZE-14.                                 DB1014.2
            008900                         07  FILLER PIC X.                        DB1014.2
            009000                         07  SIZE-13.                             DB1014.2
            009100                             08  FILLER PIC X.                    DB1014.2
            009200                             08  SIZE-12.                         DB1014.2
            009300                                 09  FILLER PIC XX.               DB1014.2
            009400                                 09  SIZE-10.                     DB1014.2
            009500                                     10  FILLER PICTURE X(5).     DB1014.2
            009600                                     10  SIZE-5 PICTURE X(5).     DB1014.2
            009700 01  TEST-RESULTS.                                                DB1014.2
            009800     02 FILLER                    PICTURE X VALUE SPACE.          DB1014.2
            009900     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB1014.2
            010000     02 FILLER                    PICTURE X VALUE SPACE.          DB1014.2
            010100     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB1014.2
            010200     02 FILLER                    PICTURE X  VALUE SPACE.         DB1014.2
            010300     02  PAR-NAME.                                                DB1014.2
            010400       03 FILLER PICTURE X(12) VALUE SPACE.                       DB1014.2
            010500       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB1014.2
            010600       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB1014.2
            010700       03 FILLER PIC X(5) VALUE SPACE.                            DB1014.2
            010800     02 FILLER PIC X(10) VALUE SPACE.                             DB1014.2
            010900     02 RE-MARK PIC X(61).                                        DB1014.2
            011000 01  TEST-COMPUTED.                                               DB1014.2
            011100     02 FILLER PIC X(30) VALUE SPACE.                             DB1014.2
            011200     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB1014.2
            011300     02 COMPUTED-X.                                               DB1014.2
            011400     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB1014.2
            011500     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB1014.2
            011600     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB1014.2
            011700     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB1014.2
            011800     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB1014.2
            011900     03       CM-18V0 REDEFINES COMPUTED-A.                       DB1014.2
            012000         04 COMPUTED-18V0                   PICTURE -9(18).       DB1014.2
            012100         04 FILLER                          PICTURE X.            DB1014.2
            012200     03 FILLER PIC X(50) VALUE SPACE.                             DB1014.2
            012300 01  TEST-CORRECT.                                                DB1014.2
            012400     02 FILLER PIC X(30) VALUE SPACE.                             DB1014.2
            012500     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB1014.2
            012600     02 CORRECT-X.                                                DB1014.2
            012700     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB1014.2
            012800     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB1014.2
            012900     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB1014.2
            013000     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB1014.2
            013100     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB1014.2
            013200     03      CR-18V0 REDEFINES CORRECT-A.                         DB1014.2
            013300         04 CORRECT-18V0                    PICTURE -9(18).       DB1014.2
            013400         04 FILLER                          PICTURE X.            DB1014.2
            013500     03 FILLER PIC X(50) VALUE SPACE.                             DB1014.2
            013600 01  CCVS-C-1.                                                    DB1014.2
            013700     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB1014.2
            013800-    "SS  PARAGRAPH-NAME                                          DB1014.2
            013900-    "        REMARKS".                                           DB1014.2
            014000     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB1014.2
            014100 01  CCVS-C-2.                                                    DB1014.2
            014200     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1014.2
            014300     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB1014.2
            014400     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB1014.2
            014500     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB1014.2
            014600     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB1014.2
            014700 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB1014.2
            014800 01  REC-CT PICTURE 99 VALUE ZERO.                                DB1014.2
            014900 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB1014.2
            015000 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB1014.2
            015100 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB1014.2
            015200 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB1014.2
            015300 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB1014.2
            015400 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB1014.2
            015500 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB1014.2
            015600 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB1014.2
            015700 01  CCVS-H-1.                                                    DB1014.2
            015800     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB1014.2
            015900     02 FILLER PICTURE X(67) VALUE                                DB1014.2
            016000     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB1014.2
            016100-    " SYSTEM".                                                   DB1014.2
            016200     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB1014.2
            016300 01  CCVS-H-2.                                                    DB1014.2
            016400     02 FILLER PICTURE X(52) VALUE IS                             DB1014.2
            016500     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB1014.2
            016600     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB1014.2
            016700     02 TEST-ID PICTURE IS X(9).                                  DB1014.2
            016800     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB1014.2
            016900 01  CCVS-H-3.                                                    DB1014.2
            017000     02  FILLER PICTURE X(34) VALUE                               DB1014.2
            017100     " FOR OFFICIAL USE ONLY    ".                                DB1014.2
            017200     02  FILLER PICTURE X(58) VALUE                               DB1014.2
            017300     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB1014.2
            017400     02  FILLER PICTURE X(28) VALUE                               DB1014.2
            017500     "  COPYRIGHT   1974 ".                                       DB1014.2
            017600 01  CCVS-E-1.                                                    DB1014.2
            017700     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB1014.2
            017800     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB1014.2
            017900     02 ID-AGAIN PICTURE IS X(9).                                 DB1014.2
            018000     02 FILLER PICTURE X(45) VALUE IS                             DB1014.2
            018100     " NTIS DISTRIBUTION COBOL 74".                               DB1014.2
            018200 01  CCVS-E-2.                                                    DB1014.2
            018300     02  FILLER                   PICTURE X(31)  VALUE            DB1014.2
            018400     SPACE.                                                       DB1014.2
            018500     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB1014.2
            018600     02 CCVS-E-2-2.                                               DB1014.2
            018700         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB1014.2
            018800         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB1014.2
            018900         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB1014.2
            019000 01  CCVS-E-3.                                                    DB1014.2
            019100     02  FILLER PICTURE X(22) VALUE                               DB1014.2
            019200     " FOR OFFICIAL USE ONLY".                                    DB1014.2
            019300     02  FILLER PICTURE X(12) VALUE SPACE.                        DB1014.2
            019400     02  FILLER PICTURE X(58) VALUE                               DB1014.2
            019500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB1014.2
            019600     02  FILLER PICTURE X(13) VALUE SPACE.                        DB1014.2
            019700     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB1014.2
            019800 01  CCVS-E-4.                                                    DB1014.2
            019900     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB1014.2
            020000     02 FILLER PIC XXXX VALUE " OF ".                             DB1014.2
            020100     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB1014.2
            020200     02 FILLER PIC X(40) VALUE                                    DB1014.2
            020300      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB1014.2
            020400 01  XXINFO.                                                      DB1014.2
            020500     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB1014.2
            020600     02 INFO-TEXT.                                                DB1014.2
            020700     04 FILLER PIC X(20) VALUE SPACE.                             DB1014.2
            020800     04 XXCOMPUTED PIC X(20).                                     DB1014.2
            020900     04 FILLER PIC X(5) VALUE SPACE.                              DB1014.2
            021000     04 XXCORRECT PIC X(20).                                      DB1014.2
            021100 01  HYPHEN-LINE.                                                 DB1014.2
            021200     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1014.2
            021300     02 FILLER PICTURE IS X(65) VALUE IS "************************DB1014.2
            021400-    "*****************************************".                 DB1014.2
            021500     02 FILLER PICTURE IS X(54) VALUE IS "************************DB1014.2
            021600-    "******************************".                            DB1014.2
            021700 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB1014.2
            021800     "DB101A".                                                    DB1014.2
            021900 PROCEDURE DIVISION.                                              DB1014.2
            022000 DECLARATIVES.                                                    DB1014.2
            022100 START-UP SECTION.                                                DB1014.2
            022200     USE FOR DEBUGGING ON CCVS1.                                  DB1014.2
            022300 BEGIN-START-UP.                                                  DB1014.2
            022400     MOVE 1 TO RESULT-FLAG.                                       DB1014.2
            022500 DB-COMMON.                                                       DB1014.2
            022600     MOVE DEBUG-LINE TO DBLINE-HOLD.                              DB1014.2
            022700     MOVE DEBUG-NAME TO DBNAME-HOLD.                              DB1014.2
            022800     MOVE DEBUG-CONTENTS TO DBCONT-HOLD.                          DB1014.2
            022900 FALL-THROUGH-AND-SERIES SECTION.                                 DB1014.2
            023000     USE FOR DEBUGGING ON FALL-THROUGH-TEST                       DB1014.2
            023100              PROC-SERIES-TEST.                                   DB1014.2
            023200 BEGIN-FALL-THROUGH-AND-SERIES.                                   DB1014.2
            023300     PERFORM DB-COMMON.                                           DB1014.2
            023400     MOVE 2 TO RESULT-FLAG.                                       DB1014.2
            023500 GO-TO SECTION.                                                   DB1014.2
            023600     USE FOR DEBUGGING ON GO-TO-TEST.                             DB1014.2
            023700 BEGIN-GO-TO.                                                     DB1014.2
            023800     PERFORM DB-COMMON.                                           DB1014.2
            023900     MOVE 3 TO RESULT-FLAG.                                       DB1014.2
            024000 ALTER-PARAGRAPH SECTION.                                         DB1014.2
            024100     USE FOR DEBUGGING ON ALTERABLE-PARAGRAPH.                    DB1014.2
            024200 BEGIN-ALTER-PARAGRAPH.                                           DB1014.2
            024300     PERFORM DB-COMMON.                                           DB1014.2
            024400     MOVE 4 TO RESULT-FLAG.                                       DB1014.2
            024500 LOOP-ITERATION SECTION.                                          DB1014.2
            024600     USE FOR DEBUGGING ON LOOP-ROUTINE.                           DB1014.2
            024700 BEGIN-LOOP-ITERATION.                                            DB1014.2
            024800     PERFORM DB-COMMON.                                           DB1014.2
            024900     ADD 1 TO RESULT-FLAG.                                        DB1014.2
            025000 PERFORM-THRU SECTION.                                            DB1014.2
            025100     USE FOR DEBUGGING ON DO-NOTHING-1.                           DB1014.2
            025200 BEGIN-PERFORM-THRU.                                              DB1014.2
            025300     PERFORM DB-COMMON.                                           DB1014.2
            025400     ADD 1 TO RESULT-FLAG.                                        DB1014.2
            025500 END DECLARATIVES.                                                DB1014.2
            025600******************************************************************DB1014.2
            025700*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            025800*    OUTPUT REPORT AS "START-PROGRAM-TEST" SHOULD POINT TO THE   *DB1014.2
            025900*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            026000*    WHICH READS, "OPEN OUTPUT PRINT-FILE."                      *DB1014.2
            026100******************************************************************DB1014.2
            026200 CCVS1 SECTION.                                                   DB1014.2
            026300 OPEN-FILES.                                                      DB1014.2
            026400     OPEN     OUTPUT PRINT-FILE.                                  DB1014.2
            026500     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB1014.2
            026600     MOVE    SPACE TO TEST-RESULTS.                               DB1014.2
            026700     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB1014.2
            026800     GO TO CCVS1-EXIT.                                            DB1014.2
            026900 CLOSE-FILES.                                                     DB1014.2
            027000     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB1014.2
            027100 TERMINATE-CCVS.                                                  DB1014.2
            027200S    EXIT PROGRAM.                                                DB1014.2
            027300STERMINATE-CALL.                                                  DB1014.2
            027400     STOP     RUN.                                                DB1014.2
            027500 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB1014.2
            027600 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB1014.2
            027700 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB1014.2
            027800 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB1014.2
            027900     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1014.2
            028000 PRINT-DETAIL.                                                    DB1014.2
            028100     IF REC-CT NOT EQUAL TO ZERO                                  DB1014.2
            028200             MOVE "." TO PARDOT-X                                 DB1014.2
            028300             MOVE REC-CT TO DOTVALUE.                             DB1014.2
            028400     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB1014.2
            028500     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB1014.2
            028600        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB1014.2
            028700          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB1014.2
            028800     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB1014.2
            028900     MOVE SPACE TO CORRECT-X.                                     DB1014.2
            029000     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1014.2
            029100     MOVE     SPACE TO RE-MARK.                                   DB1014.2
            029200 HEAD-ROUTINE.                                                    DB1014.2
            029300     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1014.2
            029400     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB1014.2
            029500     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB1014.2
            029600 COLUMN-NAMES-ROUTINE.                                            DB1014.2
            029700     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1014.2
            029800     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1014.2
            029900     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB1014.2
            030000 END-ROUTINE.                                                     DB1014.2
            030100     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB1014.2
            030200 END-RTN-EXIT.                                                    DB1014.2
            030300     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1014.2
            030400 END-ROUTINE-1.                                                   DB1014.2
            030500      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB1014.2
            030600      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB1014.2
            030700      ADD PASS-COUNTER TO ERROR-HOLD.                             DB1014.2
            030800*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB1014.2
            030900      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB1014.2
            031000      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB1014.2
            031100      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB1014.2
            031200      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB1014.2
            031300  END-ROUTINE-12.                                                 DB1014.2
            031400      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB1014.2
            031500     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB1014.2
            031600         MOVE "NO " TO ERROR-TOTAL                                DB1014.2
            031700         ELSE                                                     DB1014.2
            031800         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB1014.2
            031900     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB1014.2
            032000     PERFORM WRITE-LINE.                                          DB1014.2
            032100 END-ROUTINE-13.                                                  DB1014.2
            032200     IF DELETE-CNT IS EQUAL TO ZERO                               DB1014.2
            032300         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB1014.2
            032400         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB1014.2
            032500     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB1014.2
            032600     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1014.2
            032700      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB1014.2
            032800          MOVE "NO " TO ERROR-TOTAL                               DB1014.2
            032900      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB1014.2
            033000      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB1014.2
            033100      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB1014.2
            033200     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1014.2
            033300 WRITE-LINE.                                                      DB1014.2
            033400     ADD 1 TO RECORD-COUNT.                                       DB1014.2
            033500Y    IF RECORD-COUNT GREATER 50                                   DB1014.2
            033600Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1014.2
            033700Y        MOVE SPACE TO DUMMY-RECORD                               DB1014.2
            033800Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1014.2
            033900Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB1014.2
            034000Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB1014.2
            034100Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB1014.2
            034200Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1014.2
            034300Y        MOVE ZERO TO RECORD-COUNT.                               DB1014.2
            034400     PERFORM WRT-LN.                                              DB1014.2
            034500 WRT-LN.                                                          DB1014.2
            034600     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1014.2
            034700     MOVE SPACE TO DUMMY-RECORD.                                  DB1014.2
            034800 BLANK-LINE-PRINT.                                                DB1014.2
            034900     PERFORM WRT-LN.                                              DB1014.2
            035000 FAIL-ROUTINE.                                                    DB1014.2
            035100     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB1014.2
            035200     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB1014.2
            035300     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB1014.2
            035400     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1014.2
            035500     GO TO FAIL-ROUTINE-EX.                                       DB1014.2
            035600 FAIL-ROUTINE-WRITE.                                              DB1014.2
            035700     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB1014.2
            035800     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB1014.2
            035900 FAIL-ROUTINE-EX. EXIT.                                           DB1014.2
            036000 BAIL-OUT.                                                        DB1014.2
            036100     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB1014.2
            036200     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB1014.2
            036300 BAIL-OUT-WRITE.                                                  DB1014.2
            036400     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB1014.2
            036500     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1014.2
            036600 BAIL-OUT-EX. EXIT.                                               DB1014.2
            036700 CCVS1-EXIT.                                                      DB1014.2
            036800     EXIT.                                                        DB1014.2
            036900 START-PROGRAM-TEST.                                              DB1014.2
            037000     IF RESULT-FLAG IS NOT EQUAL TO 1                             DB1014.2
            037100         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
            037200         PERFORM FAIL                                             DB1014.2
            037300         GO TO START-PROGRAM-WRITE.                               DB1014.2
            037400     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            037500     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            037600     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
            037700     PERFORM INSPT.                                               DB1014.2
            037800     PERFORM START-PROGRAM-WRITE.                                 DB1014.2
            037900     MOVE DBNAME-HOLD TO SIZE-5.                                  DB1014.2
            038000     IF SIZE-5 IS EQUAL TO "CCVS1"                                DB1014.2
            038100         PERFORM PASS  ELSE                                       DB1014.2
            038200         MOVE "CCVS1" TO CORRECT-A                                DB1014.2
            038300         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            038400         PERFORM FAIL.                                            DB1014.2
            038500 START-PROGRAM-TEST-1.                                            DB1014.2
            038600     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            038700     PERFORM START-PROGRAM-WRITE.                                 DB1014.2
            038800     MOVE DBCONT-HOLD TO SIZE-13.                                 DB1014.2
            038900     IF SIZE-13 IS EQUAL TO "START PROGRAM"                       DB1014.2
            039000         PERFORM PASS ELSE                                        DB1014.2
            039100         MOVE "START PROGRAM" TO CORRECT-A                        DB1014.2
            039200         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
            039300         PERFORM FAIL.                                            DB1014.2
            039400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
            039500     GO TO START-PROGRAM-WRITE.                                   DB1014.2
            039600 START-PROGRAM-DELETE.                                            DB1014.2
            039700     PERFORM DE-LETE.                                             DB1014.2
            039800 START-PROGRAM-WRITE.                                             DB1014.2
            039900     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            040000     MOVE "START-PROGRAM-TEST" TO PAR-NAME.                       DB1014.2
            040100     PERFORM PRINT-DETAIL.                                        DB1014.2
            040200******************************************************************DB1014.2
            040300*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            040400*    OUTPUT REPORT AS "FALL-THROUGH-TEST" SHOULD POINT TO THE    *DB1014.2
            040500*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            040600*    WHICH READS, "MOVE 0 TO RESULT-FLAG."                       *DB1014.2
            040700******************************************************************DB1014.2
            040800     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            040900 FALL-THROUGH-TEST.                                               DB1014.2
            041000     IF RESULT-FLAG IS NOT EQUAL TO 2                             DB1014.2
            041100         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
            041200         PERFORM FAIL                                             DB1014.2
            041300         GO TO FALL-THROUGH-WRITE.                                DB1014.2
            041400     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            041500     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            041600     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
            041700     PERFORM INSPT.                                               DB1014.2
            041800     PERFORM FALL-THROUGH-WRITE.                                  DB1014.2
            041900     MOVE DBNAME-HOLD TO SIZE-17.                                 DB1014.2
            042000     IF SIZE-17 IS EQUAL TO "FALL-THROUGH-TEST"                   DB1014.2
            042100         PERFORM PASS ELSE                                        DB1014.2
            042200         MOVE "FALL-THROUGH-TEST" TO CORRECT-A                    DB1014.2
            042300         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            042400         PERFORM FAIL.                                            DB1014.2
            042500     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            042600     PERFORM FALL-THROUGH-WRITE.                                  DB1014.2
            042700     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
            042800     IF SIZE-12 IS EQUAL TO "FALL THROUGH"                        DB1014.2
            042900         PERFORM PASS ELSE                                        DB1014.2
            043000         MOVE "FALL THROUGH" TO CORRECT-A                         DB1014.2
            043100         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
            043200         PERFORM FAIL.                                            DB1014.2
            043300     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
            043400     GO TO FALL-THROUGH-WRITE.                                    DB1014.2
            043500 FALL-THROUGH-DELETE.                                             DB1014.2
            043600     PERFORM DE-LETE.                                             DB1014.2
            043700 FALL-THROUGH-WRITE.                                              DB1014.2
            043800     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            043900     MOVE "FALL-THROUGH-TEST" TO PAR-NAME.                        DB1014.2
            044000     PERFORM PRINT-DETAIL.                                        DB1014.2
            044100******************************************************************DB1014.2
            044200*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            044300*    OUTPUT REPORT AS "PROC-SERIES-TEST" SHOULD POINT TO THE     *DB1014.2
            044400*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            044500*    WHICH READS, "MOVE 0 TO RESULT-FLAG."                       *DB1014.2
            044600******************************************************************DB1014.2
            044700     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            044800 PROC-SERIES-TEST.                                                DB1014.2
            044900     IF RESULT-FLAG IS NOT EQUAL TO 2                             DB1014.2
            045000         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
            045100         PERFORM FAIL                                             DB1014.2
            045200         GO TO PROC-SERIES-WRITE.                                 DB1014.2
            045300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            045400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            045500     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
            045600     PERFORM INSPT.                                               DB1014.2
            045700     PERFORM PROC-SERIES-WRITE.                                   DB1014.2
            045800     MOVE DBNAME-HOLD TO SIZE-16.                                 DB1014.2
            045900     IF SIZE-16 IS EQUAL TO "PROC-SERIES-TEST"                    DB1014.2
            046000         PERFORM PASS ELSE                                        DB1014.2
            046100         MOVE "PROC-SERIES-TEST" TO CORRECT-A                     DB1014.2
            046200         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            046300         PERFORM FAIL.                                            DB1014.2
            046400     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            046500     PERFORM PROC-SERIES-WRITE.                                   DB1014.2
            046600     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
            046700     IF SIZE-12 IS EQUAL TO "FALL THROUGH"                        DB1014.2
            046800         PERFORM PASS ELSE                                        DB1014.2
            046900         MOVE "FALL THROUGH" TO CORRECT-A                         DB1014.2
            047000         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
            047100         PERFORM FAIL.                                            DB1014.2
            047200     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
            047300     GO TO PROC-SERIES-WRITE.                                     DB1014.2
            047400 PROC-SERIES-DELETE.                                              DB1014.2
            047500     PERFORM DE-LETE.                                             DB1014.2
            047600 PROC-SERIES-WRITE.                                               DB1014.2
            047700     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            047800     MOVE "PROC-SERIES-TEST" TO PAR-NAME.                         DB1014.2
            047900     PERFORM PRINT-DETAIL.                                        DB1014.2
            048000     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            048100******************************************************************DB1014.2
            048200*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            048300*    OUTPUT REPORT AS "GO-TO-TEST" SHOULD POINT TO THE           *DB1014.2
            048400*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            048500*    WHICH READS, "GO TO GO-TO-TEST.".                           *DB1014.2
            048600******************************************************************DB1014.2
            048700 ALTERABLE-PARAGRAPH.                                             DB1014.2
            048800     GO TO GO-TO-TEST.                                            DB1014.2
            048900 FILLER-PARAGRAPH.                                                DB1014.2
            049000     DISPLAY "ALTER FAILED AT ALTER-TEST-INIT".                   DB1014.2
            049100     PERFORM FAIL.                                                DB1014.2
            049200     GO TO ALTERED-GO-TO-TEST.                                    DB1014.2
            049300 GO-TO-TEST.                                                      DB1014.2
            049400     IF RESULT-FLAG IS NOT EQUAL TO 3                             DB1014.2
            049500         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
            049600         PERFORM FAIL                                             DB1014.2
            049700         GO TO GO-TO-WRITE.                                       DB1014.2
            049800     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            049900     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            050000     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
            050100     PERFORM INSPT.                                               DB1014.2
            050200     PERFORM GO-TO-WRITE.                                         DB1014.2
            050300     MOVE DBNAME-HOLD TO SIZE-10.                                 DB1014.2
            050400     IF SIZE-10 IS EQUAL TO "GO-TO-TEST"                          DB1014.2
            050500         PERFORM PASS ELSE                                        DB1014.2
            050600         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            050700         MOVE "GO-TO-TEST" TO CORRECT-A                           DB1014.2
            050800         PERFORM FAIL.                                            DB1014.2
            050900     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            051000     PERFORM GO-TO-WRITE.                                         DB1014.2
            051100     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
            051200     IF       SIZE-12 IS EQUAL TO SPACE                           DB1014.2
            051300              PERFORM PASS                                        DB1014.2
            051400              ELSE                                                DB1014.2
            051500              PERFORM FAIL                                        DB1014.2
            051600              MOVE DBCONT-HOLD TO COMPUTED-A                      DB1014.2
            051700              MOVE "SPACES" TO CORRECT-A.                         DB1014.2
            051800     MOVE     "DEBUG-CONTENTS" TO RE-MARK.                        DB1014.2
            051900     GO TO     GO-TO-WRITE.                                       DB1014.2
            052000 GO-TO-DELETE.                                                    DB1014.2
            052100         PERFORM DE-LETE.                                         DB1014.2
            052200 GO-TO-WRITE.                                                     DB1014.2
            052300     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            052400     MOVE "GO-TO-TEST" TO PAR-NAME.                               DB1014.2
            052500     PERFORM PRINT-DETAIL.                                        DB1014.2
            052600     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            052700******************************************************************DB1014.2
            052800*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            052900*    OUTPUT REPORT AS "ALTER-TEST" SHOULD POINT TO THE           *DB1014.2
            053000*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            053100*    WHICH READS, "ALTER ALTERABLE-PARAGRAPH TO PROCEED TO       *DB1014.2
            053200*    ALTERED-GO-TO-TEST.".                                       *DB1014.2
            053300******************************************************************DB1014.2
            053400 ALTER-TEST-INIT.                                                 DB1014.2
            053500     ALTER ALTERABLE-PARAGRAPH TO PROCEED TO ALTERED-GO-TO-TEST.  DB1014.2
            053600 ALTER-TEST.                                                      DB1014.2
            053700     IF RESULT-FLAG IS NOT EQUAL TO 4                             DB1014.2
            053800         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
            053900         PERFORM FAIL                                             DB1014.2
            054000         GO TO ALTER-WRITE.                                       DB1014.2
            054100     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            054200     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            054300     MOVE DBLINE-HOLD TO COMPUTED-A                               DB1014.2
            054400     PERFORM INSPT.                                               DB1014.2
            054500     PERFORM ALTER-WRITE.                                         DB1014.2
            054600     MOVE DBNAME-HOLD TO SIZE-19.                                 DB1014.2
            054700     IF SIZE-19 IS EQUAL TO "ALTERABLE-PARAGRAPH"                 DB1014.2
            054800         PERFORM PASS ELSE                                        DB1014.2
            054900         MOVE "ALTERABLE-PARAGRAPH" TO CORRECT-A                  DB1014.2
            055000         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            055100         PERFORM FAIL.                                            DB1014.2
            055200     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            055300     PERFORM ALTER-WRITE.                                         DB1014.2
            055400     MOVE DBCONT-HOLD TO SIZE-18.                                 DB1014.2
            055500     IF SIZE-18 IS EQUAL TO "ALTERED-GO-TO-TEST"                  DB1014.2
            055600         PERFORM PASS ELSE                                        DB1014.2
            055700         MOVE "ALTERED-GO-TO-TEST" TO CORRECT-A                   DB1014.2
            055800         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
            055900         PERFORM FAIL.                                            DB1014.2
            056000     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
            056100     GO TO ALTER-WRITE.                                           DB1014.2
            056200 ALTER-DELETE.                                                    DB1014.2
            056300     PERFORM DE-LETE.                                             DB1014.2
            056400 ALTER-WRITE.                                                     DB1014.2
            056500     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            056600     MOVE "ALTER-TEST" TO PAR-NAME.                               DB1014.2
            056700     PERFORM PRINT-DETAIL.                                        DB1014.2
            056800     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            056900******************************************************************DB1014.2
            057000*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            057100*    OUTPUT REPORT AS "ALTERED-GO-TO-TEST" SHOULD POINT TO THE   *DB1014.2
            057200*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            057300*    WHICH READS, "GO TO ALTERABLE-PARAGRAPH.".                  *DB1014.2
            057400******************************************************************DB1014.2
            057500 ALTER-WRITE-END.                                                 DB1014.2
            057600     GO TO ALTERABLE-PARAGRAPH.                                   DB1014.2
            057700 ALTERED-GO-TO-TEST.                                              DB1014.2
            057800     IF RESULT-FLAG IS NOT EQUAL TO 4                             DB1014.2
            057900         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
            058000         PERFORM FAIL                                             DB1014.2
            058100         GO TO ALTERED-GO-TO-WRITE.                               DB1014.2
            058200     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            058300     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            058400     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
            058500     PERFORM INSPT.                                               DB1014.2
            058600     PERFORM ALTERED-GO-TO-WRITE.                                 DB1014.2
            058700     MOVE DBNAME-HOLD TO SIZE-19.                                 DB1014.2
            058800     IF SIZE-19 IS EQUAL TO "ALTERABLE-PARAGRAPH"                 DB1014.2
            058900         PERFORM PASS ELSE                                        DB1014.2
            059000         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            059100         MOVE "ALTERABLE-PARAGRAPH" TO CORRECT-A                  DB1014.2
            059200         PERFORM FAIL.                                            DB1014.2
            059300     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            059400     PERFORM ALTERED-GO-TO-WRITE.                                 DB1014.2
            059500     IF       DBCONT-HOLD EQUAL TO SPACE                          DB1014.2
            059600              PERFORM PASS                                        DB1014.2
            059700              ELSE                                                DB1014.2
            059800              PERFORM FAIL                                        DB1014.2
            059900              MOVE DBCONT-HOLD TO COMPUTED-A                      DB1014.2
            060000              MOVE "SPACES" TO CORRECT-A.                         DB1014.2
            060100     MOVE     "DEBUG-CONTENTS" TO RE-MARK.                        DB1014.2
            060200     GO TO ALTERED-GO-TO-WRITE.                                   DB1014.2
            060300 ALTERED-GO-TO-DELETE.                                            DB1014.2
            060400     PERFORM DE-LETE.                                             DB1014.2
            060500 ALTERED-GO-TO-WRITE.                                             DB1014.2
            060600     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            060700     MOVE "ALTERED-GO-TO-TEST" TO PAR-NAME.                       DB1014.2
            060800     PERFORM PRINT-DETAIL.                                        DB1014.2
            060900     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            061000******************************************************************DB1014.2
            061100*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            061200*    OUTPUT REPORT AS "PERF-ITERATION-TEST" SHOULD POINT TO THE  *DB1014.2
            061300*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            061400*    WHICH READS, "PERFORM LOOP-ROUTINE FIVE TIMES.".            *DB1014.2
            061500******************************************************************DB1014.2
            061600 PERF-ITERATION-TEST.                                             DB1014.2
            061700     PERFORM LOOP-ROUTINE FIVE TIMES.                             DB1014.2
            061800     IF RESULT-FLAG IS NOT EQUAL TO 5                             DB1014.2
            061900         MOVE "05" TO CORRECT-A                                   DB1014.2
            062000         MOVE RESULT-FLAG TO COMPUTED-A                           DB1014.2
            062100         MOVE "NO. OF TIMES USE PROC EXECUTED" TO RE-MARK         DB1014.2
            062200         PERFORM FAIL                                             DB1014.2
            062300              ELSE                                                DB1014.2
            062400              MOVE "PROC EXECUTED FIVE TIMES" TO RE-MARK          DB1014.2
            062500              PERFORM PASS.                                       DB1014.2
            062600     IF RESULT-FLAG IS EQUAL TO 0                                 DB1014.2
            062700         GO TO PERF-ITERATION-WRITE                               DB1014.2
            062800         ELSE PERFORM PERF-ITERATION-WRITE.                       DB1014.2
            062900     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
            063000     PERFORM INSPT.                                               DB1014.2
            063100     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            063200     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            063300     PERFORM PERF-ITERATION-WRITE.                                DB1014.2
            063400     MOVE DBNAME-HOLD TO SIZE-12.                                 DB1014.2
            063500     IF SIZE-12 IS EQUAL TO "LOOP-ROUTINE"                        DB1014.2
            063600         PERFORM PASS ELSE                                        DB1014.2
            063700         MOVE "LOOP-ROUTINE" TO CORRECT-A                         DB1014.2
            063800         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            063900         PERFORM FAIL.                                            DB1014.2
            064000     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            064100     PERFORM PERF-ITERATION-WRITE.                                DB1014.2
            064200     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
            064300     IF SIZE-12 IS EQUAL TO "PERFORM LOOP"                        DB1014.2
            064400         PERFORM PASS ELSE                                        DB1014.2
            064500         MOVE "PERFORM LOOP" TO CORRECT-A                         DB1014.2
            064600         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
            064700         PERFORM FAIL.                                            DB1014.2
            064800     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
            064900     GO TO PERF-ITERATION-WRITE.                                  DB1014.2
            065000 PERF-ITERATION-DELETE.                                           DB1014.2
            065100     PERFORM DE-LETE.                                             DB1014.2
            065200 PERF-ITERATION-WRITE.                                            DB1014.2
            065300     MOVE "PERF-ITERATION-TEST" TO PAR-NAME.                      DB1014.2
            065400     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            065500     PERFORM PRINT-DETAIL.                                        DB1014.2
            065600     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            065700 PERF-ITERATION-END.                                              DB1014.2
            065800     GO TO PERFORM-THRU-TEST.                                     DB1014.2
            065900 LOOP-ROUTINE.                                                    DB1014.2
            066000**NESTED PERFORMS ARE USED HERE TO ATTEMPT TO PREVENT OPTIMIZER   DB1014.2
            066100* ACTION RESULTING IN LOOP UNFOLDING AND REDUCTION.               DB1014.2
            066200     PERFORM DO-NOTHING.                                          DB1014.2
            066300******************************************************************DB1014.2
            066400*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            066500*    OUTPUT REPORT AS "PERFORM-THRU-TEST" SHOULD POINT TO THE    *DB1014.2
            066600*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            066700*    WHICH READS, "ADD A B C GIVING D.".                         *DB1014.2
            066800******************************************************************DB1014.2
            066900 DO-NOTHING.                                                      DB1014.2
            067000     ADD A B C GIVING D.                                          DB1014.2
            067100 DO-NOTHING-1.                                                    DB1014.2
            067200     SUBTRACT A FROM B.                                           DB1014.2
            067300 PERFORM-THRU-TEST.                                               DB1014.2
            067400     PERFORM DO-NOTHING THRU DO-NOTHING-1 FIVE TIMES.             DB1014.2
            067500     IF RESULT-FLAG IS NOT EQUAL TO 5                             DB1014.2
            067600         MOVE "05" TO CORRECT-A                                   DB1014.2
            067700         MOVE RESULT-FLAG TO COMPUTED-A                           DB1014.2
            067800         MOVE "NO. OF TIMES USE PROC EXECUTED" TO RE-MARK         DB1014.2
            067900         PERFORM FAIL                                             DB1014.2
            068000              ELSE                                                DB1014.2
            068100              MOVE "PROC EXECUTED FIVE TIMES" TO RE-MARK          DB1014.2
            068200              PERFORM PASS.                                       DB1014.2
            068300     IF RESULT-FLAG IS EQUAL TO 0                                 DB1014.2
            068400         GO TO PERFORM-THRU-WRITE   ELSE                          DB1014.2
            068500         PERFORM PERFORM-THRU-WRITE.                              DB1014.2
            068600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            068700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            068800     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
            068900     PERFORM INSPT.                                               DB1014.2
            069000     PERFORM PERFORM-THRU-WRITE.                                  DB1014.2
            069100     MOVE DBNAME-HOLD TO SIZE-12.                                 DB1014.2
            069200     IF SIZE-12 IS EQUAL TO "DO-NOTHING-1"                        DB1014.2
            069300         PERFORM PASS   ELSE                                      DB1014.2
            069400         MOVE "DO-NOTHING-1" TO CORRECT-A                         DB1014.2
            069500         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            069600         PERFORM FAIL.                                            DB1014.2
            069700     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            069800     PERFORM PERFORM-THRU-WRITE.                                  DB1014.2
            069900     MOVE DBCONT-HOLD TO SIZE-12                                  DB1014.2
            070000     IF SIZE-12 IS EQUAL TO "FALL THROUGH"                        DB1014.2
            070100         PERFORM PASS   ELSE                                      DB1014.2
            070200         MOVE "FALL THROUGH" TO CORRECT-A                         DB1014.2
            070300         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
            070400         PERFORM FAIL.                                            DB1014.2
            070500     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
            070600     GO TO PERFORM-THRU-WRITE.                                    DB1014.2
            070700 PERFORM-THRU-DELETE.                                             DB1014.2
            070800     PERFORM DE-LETE.                                             DB1014.2
            070900 PERFORM-THRU-WRITE.                                              DB1014.2
            071000     MOVE "PERFORM-THRU-TEST" TO PAR-NAME.                        DB1014.2
            071100     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            071200     PERFORM PRINT-DETAIL.                                        DB1014.2
            071300     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            071400******************************************************************DB1014.2
            071500*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
            071600*    OUTPUT REPORT AS "SIMPLE-PERFORM-TEST" SHOULD POINT TO THE  *DB1014.2
            071700*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
            071800*    WHICH READS, "PERFORM LOOP-ROUTINE.".                       *DB1014.2
            071900******************************************************************DB1014.2
            072000 SIMPLE-PERFORM-TEST.                                             DB1014.2
            072100     PERFORM LOOP-ROUTINE.                                        DB1014.2
            072200     IF RESULT-FLAG IS NOT EQUAL TO 1                             DB1014.2
            072300         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
            072400         PERFORM FAIL                                             DB1014.2
            072500         GO TO SIMPLE-PERFORM-WRITE.                              DB1014.2
            072600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
            072700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
            072800     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
            072900     PERFORM INSPT.                                               DB1014.2
            073000     PERFORM SIMPLE-PERFORM-WRITE.                                DB1014.2
            073100     MOVE DBNAME-HOLD TO SIZE-12.                                 DB1014.2
            073200     IF SIZE-12 IS EQUAL TO "LOOP-ROUTINE"                        DB1014.2
            073300         PERFORM PASS   ELSE                                      DB1014.2
            073400         MOVE "LOOP-ROUTINE" TO CORRECT-A                         DB1014.2
            073500         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
            073600         PERFORM FAIL.                                            DB1014.2
            073700     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
            073800     PERFORM SIMPLE-PERFORM-WRITE.                                DB1014.2
            073900     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
            074000     IF SIZE-12 IS EQUAL TO "PERFORM LOOP"                        DB1014.2
            074100         PERFORM PASS   ELSE                                      DB1014.2
            074200         MOVE "PERFORM LOOP" TO CORRECT-A                         DB1014.2
            074300         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
            074400         PERFORM FAIL.                                            DB1014.2
            074500     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
            074600     GO TO SIMPLE-PERFORM-WRITE.                                  DB1014.2
            074700 SIMPLE-PERFORM-DELETE.                                           DB1014.2
            074800     PERFORM DE-LETE.                                             DB1014.2
            074900 SIMPLE-PERFORM-WRITE.                                            DB1014.2
            075000     MOVE "SIMPLE-PERFORM-TEST" TO PAR-NAME.                      DB1014.2
            075100     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
            075200     PERFORM PRINT-DETAIL.                                        DB1014.2
            075300     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
            075400 DEBUG-LINE-TESTS-INIT.                                           DB1014.2
            075500     MOVE "DEBUG LINE TESTS" TO FEATURE.                          DB1014.2
            075600 DEBUG-LINE-TEST-01.                                              DB1014.2
            075700     MOVE "COMPLETE ENTITY" TO RE-MARK.                           DB1014.2
            075800     PERFORM FAIL.                                                DB1014.2
            075900D    PERFORM PASS  SUBTRACT 1 FROM ERROR-COUNTER.                 DB1014.2
            076000     GO TO DEBUG-LINE-WRITE-01.                                   DB1014.2
            076100 DEBUG-LINE-DELETE-01.                                            DB1014.2
            076200     PERFORM DE-LETE.                                             DB1014.2
            076300 DEBUG-LINE-WRITE-01.                                             DB1014.2
            076400     MOVE "DEBUG-LINE-TEST-01" TO PAR-NAME.                       DB1014.2
            076500     PERFORM PRINT-DETAIL.                                        DB1014.2
            076600 DEBUG-LINE-TEST-02.                                              DB1014.2
            076700     MOVE "CONSECUTIVE DEBUG LINES" TO RE-MARK.                   DB1014.2
            076800     PERFORM FAIL.                                                DB1014.2
            076900D    PERFORM PASS.                                                DB1014.2
            077000D    SUBTRACT 1 FROM ERROR-COUNTER.                               DB1014.2
            077100     GO TO DEBUG-LINE-WRITE-02.                                   DB1014.2
            077200 DEBUG-LINE-DELETE-02.                                            DB1014.2
            077300     PERFORM DE-LETE.                                             DB1014.2
            077400 DEBUG-LINE-WRITE-02.                                             DB1014.2
            077500     MOVE "DEBUG-LINE-TEST-02" TO PAR-NAME.                       DB1014.2
            077600     PERFORM PRINT-DETAIL.                                        DB1014.2
            077700 DEBUG-LINE-TEST-03.                                              DB1014.2
            077800     MOVE "BROKEN STATEMENTS" TO RE-MARK.                         DB1014.2
            077900     PERFORM                                                      DB1014.2
            078000D        PASS.  GO TO DEBUG-LINE-WRITE-03.                        DB1014.2
            078100DDEBUG-LINE-TEST-03-A.    PERFORM                                 DB1014.2
            078200                             FAIL.                                DB1014.2
            078300                             GO TO DEBUG-LINE-WRITE-03.           DB1014.2
            078400 DEBUG-LINE-DELETE-03.                                            DB1014.2
            078500     PERFORM DE-LETE.                                             DB1014.2
            078600 DEBUG-LINE-WRITE-03.                                             DB1014.2
            078700     MOVE "DEBUG-LINE-TEST-03" TO PAR-NAME.                       DB1014.2
            078800     PERFORM PRINT-DETAIL.                                        DB1014.2
            078900 DEBUG-LINE-TEST-04.                                              DB1014.2
            079000     MOVE "NESTED COMMENTS" TO RE-MARK.                           DB1014.2
            079100D    PERFORM                                                      DB1014.2
            079200*        FAIL.  GO TO DEBUG-LINE-WRITE-04.                        DB1014.2
            079300*DEBUG-LINE-TEST-04-A.    PERFORM                                 DB1014.2
            079400D                         PASS.  GO TO DEBUG-LINE-WRITE-04.       DB1014.2
            079500 DEBUG-LINE-TEST-04-B.                                            DB1014.2
            079600     MOVE "    FAILURE 04B" TO COMPUTED-A.                        DB1014.2
            079700     PERFORM FAIL.                                                DB1014.2
            079800     GO TO DEBUG-LINE-WRITE-04.                                   DB1014.2
            079900 DEBUG-LINE-DELETE-04.                                            DB1014.2
            080000     PERFORM DE-LETE.                                             DB1014.2
            080100 DEBUG-LINE-WRITE-04.                                             DB1014.2
            080200     MOVE "DEBUG-LINE-TEST-04" TO PAR-NAME.                       DB1014.2
            080300     PERFORM PRINT-DETAIL.                                        DB1014.2
            080400 DEBUG-LINE-TEST-05.                                              DB1014.2
            080500     MOVE "NESTED INSIDE COMMENTS" TO RE-MARK.                    DB1014.2
            080600*    PERFORM FAIL.                                                DB1014.2
            080700*    GO TO DEBUG-LINE-WRITE-05.                                   DB1014.2
            080800*DEBUG-LINE-TEST-05-A.                                            DB1014.2
            080900D    PERFORM PASS.                                                DB1014.2
            081000D    GO TO DEBUG-LINE-WRITE-05.                                   DB1014.2
            081100*DEBUG-LINE-TEST-05-B.                                            DB1014.2
            081200*    MOVE "    FAILURE 05B" TO COMPUTED-A.                        DB1014.2
            081300*    PERFORM FAIL.  GO TO DEBUG-LINE-WRITE-05.                    DB1014.2
            081400 DEBUG-LINE-TEST-05-C.                                            DB1014.2
            081500     MOVE "    FAILURE 05C" TO COMPUTED-A.                        DB1014.2
            081600     PERFORM FAIL.   GO TO DEBUG-LINE-WRITE-05.                   DB1014.2
            081700 DEBUG-LINE-DELETE-05.                                            DB1014.2
            081800     PERFORM DE-LETE.                                             DB1014.2
            081900 DEBUG-LINE-WRITE-05.                                             DB1014.2
            082000     MOVE "DEBUG-LINE-TEST-05" TO PAR-NAME.                       DB1014.2
            082100     PERFORM PRINT-DETAIL.                                        DB1014.2
            082200 CCVS-EXIT SECTION.                                               DB1014.2
            082300 CCVS-999999.                                                     DB1014.2
            082400     GO TO CLOSE-FILES.                                           DB1014.2
                  *END-OF,DB101A                                                            
        """)
    )

    @Disabled("Requires indicator flags `S` and `Y`")
    @Test
    fun db1024_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB102A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB1024.2
            000200 PROGRAM-ID.                                                      DB1024.2
            000300     DB102A.                                                      DB1024.2
            000400 AUTHOR.                                                          DB1024.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB1024.2
            000600 INSTALLATION.                                                    DB1024.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB1024.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB1024.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB1024.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB1024.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB1024.2
            001200                                                                  DB1024.2
            001300     PHONE   (703) 756-6153                                       DB1024.2
            001400                                                                  DB1024.2
            001500     " HIGH       ".                                              DB1024.2
            001600 DATE-WRITTEN.                                                    DB1024.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB1024.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB1024.2
            001900     "4.2 ".                                                      DB1024.2
            002000 SECURITY.                                                        DB1024.2
            002100     NONE.                                                        DB1024.2
            002200*                                                                 DB1024.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB1024.2
            002400*                                                                 DB1024.2
            002500*                       PROGRAM ABSTRACT                          DB1024.2
            002600*                                                                 DB1024.2
            002700*    DB102A TESTS THE BASIC OPERATION OF THE DEBUG MODULE         DB1024.2
            002800*    FACILITIES WHEN THE COMPILE TIME DEBUGGING SWITCH IS ON      DB1024.2
            002900*    AND THE OBJECT TIME SWITCH IS OFF.  ALL DEBUG LINES AND      DB1024.2
            003000*    DEBUGGING PROCEDURES SHOULD BE INCLUDED IN COMPILATION AND   DB1024.2
            003100*    GENERATE CODE.                                               DB1024.2
            003200*                                                                 DB1024.2
            003300*    BEFORE BEGINNING EXECUTION OF THE OBJECT PROGRAM, THE JOB    DB1024.2
            003400*    CONTROL LANGUAGE NECESSARY TO DEACTIVATE (TURN OFF) THE      DB1024.2
            003500*    OBJECT TIME DEBUGGING SWITCH MUST BE SUBMITTED.              DB1024.2
            003600*                                                                 DB1024.2
            003700*    AT EXECUTION TIME, CODE GENERATED FROM DEBUG LINES SHOULD    DB1024.2
            003800*    BE EXECUTED, BUT DEBUGGING PROCEDURES SHOULD BE DEACTIVATED  DB1024.2
            003900*    BY THE OBJECT TIME SWITCH.                                   DB1024.2
            004000*                                                                 DB1024.2
            004100*                                                                 DB1024.2
            004200*                                                                 DB1024.2
            004300 ENVIRONMENT DIVISION.                                            DB1024.2
            004400 CONFIGURATION SECTION.                                           DB1024.2
            004500 SOURCE-COMPUTER.                                                 DB1024.2
            004600     XXXXX082                                                     DB1024.2
            004700         WITH DEBUGGING MODE.                                     DB1024.2
            004800 OBJECT-COMPUTER.                                                 DB1024.2
            004900     XXXXX083.                                                    DB1024.2
            005000 INPUT-OUTPUT SECTION.                                            DB1024.2
            005100 FILE-CONTROL.                                                    DB1024.2
            005200     SELECT PRINT-FILE ASSIGN TO                                  DB1024.2
            005300     XXXXX055.                                                    DB1024.2
            005400 DATA DIVISION.                                                   DB1024.2
            005500 FILE SECTION.                                                    DB1024.2
            005600 FD  PRINT-FILE                                                   DB1024.2
            005700     LABEL RECORDS                                                DB1024.2
            005800     XXXXX084                                                     DB1024.2
            005900     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB1024.2
            006000 01  PRINT-REC PICTURE X(120).                                    DB1024.2
            006100 01  DUMMY-RECORD PICTURE X(120).                                 DB1024.2
            006200 WORKING-STORAGE SECTION.                                         DB1024.2
            006300 77  A PIC 9 COMP VALUE 1.                                        DB1024.2
            006400 77  B PIC 9 COMP VALUE 5.                                        DB1024.2
            006500 77  C PIC 9 COMP VALUE 9.                                        DB1024.2
            006600 77  D PIC 99 COMP.                                               DB1024.2
            006700 77  RESULT-FLAG PIC 99 COMP VALUE 0.                             DB1024.2
            006800 77  DBLINE-HOLD PIC X(6).                                        DB1024.2
            006900 77  DBNAME-HOLD PIC X(30).                                       DB1024.2
            007000 77  DBCONT-HOLD PIC X(30).                                       DB1024.2
            007100 77  FIVE PIC 9 COMP VALUE 5.                                     DB1024.2
            007200 01  SIZE-19.                                                     DB1024.2
            007300     02  FILLER PIC X.                                            DB1024.2
            007400     02  SIZE-18.                                                 DB1024.2
            007500         03  FILLER PIC X.                                        DB1024.2
            007600         03  SIZE-17.                                             DB1024.2
            007700             04  FILLER PIC X.                                    DB1024.2
            007800             04  SIZE-16.                                         DB1024.2
            007900                 05  FILLER PIC X.                                DB1024.2
            008000                 05  SIZE-15.                                     DB1024.2
            008100                     06  FILLER PIC X.                            DB1024.2
            008200                     06  SIZE-14.                                 DB1024.2
            008300                         07  FILLER PIC X.                        DB1024.2
            008400                         07  SIZE-13.                             DB1024.2
            008500                             08  FILLER PIC X.                    DB1024.2
            008600                             08  SIZE-12.                         DB1024.2
            008700                                 09  FILLER PIC X.                DB1024.2
            008800                                 09  SIZE-11.                     DB1024.2
            008900                                     10  FILLER PIC X.            DB1024.2
            009000                                     10  SIZE-10 PIC X(10).       DB1024.2
            009100 01  TEST-RESULTS.                                                DB1024.2
            009200     02 FILLER                    PICTURE X VALUE SPACE.          DB1024.2
            009300     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB1024.2
            009400     02 FILLER                    PICTURE X VALUE SPACE.          DB1024.2
            009500     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB1024.2
            009600     02 FILLER                    PICTURE X  VALUE SPACE.         DB1024.2
            009700     02  PAR-NAME.                                                DB1024.2
            009800       03 FILLER PICTURE X(12) VALUE SPACE.                       DB1024.2
            009900       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB1024.2
            010000       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB1024.2
            010100       03 FILLER PIC X(5) VALUE SPACE.                            DB1024.2
            010200     02 FILLER PIC X(10) VALUE SPACE.                             DB1024.2
            010300     02 RE-MARK PIC X(61).                                        DB1024.2
            010400 01  TEST-COMPUTED.                                               DB1024.2
            010500     02 FILLER PIC X(30) VALUE SPACE.                             DB1024.2
            010600     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB1024.2
            010700     02 COMPUTED-X.                                               DB1024.2
            010800     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB1024.2
            010900     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB1024.2
            011000     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB1024.2
            011100     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB1024.2
            011200     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB1024.2
            011300     03       CM-18V0 REDEFINES COMPUTED-A.                       DB1024.2
            011400         04 COMPUTED-18V0                   PICTURE -9(18).       DB1024.2
            011500         04 FILLER                          PICTURE X.            DB1024.2
            011600     03 FILLER PIC X(50) VALUE SPACE.                             DB1024.2
            011700 01  TEST-CORRECT.                                                DB1024.2
            011800     02 FILLER PIC X(30) VALUE SPACE.                             DB1024.2
            011900     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB1024.2
            012000     02 CORRECT-X.                                                DB1024.2
            012100     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB1024.2
            012200     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB1024.2
            012300     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB1024.2
            012400     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB1024.2
            012500     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB1024.2
            012600     03      CR-18V0 REDEFINES CORRECT-A.                         DB1024.2
            012700         04 CORRECT-18V0                    PICTURE -9(18).       DB1024.2
            012800         04 FILLER                          PICTURE X.            DB1024.2
            012900     03 FILLER PIC X(50) VALUE SPACE.                             DB1024.2
            013000 01  CCVS-C-1.                                                    DB1024.2
            013100     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB1024.2
            013200-    "SS  PARAGRAPH-NAME                                          DB1024.2
            013300-    "        REMARKS".                                           DB1024.2
            013400     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB1024.2
            013500 01  CCVS-C-2.                                                    DB1024.2
            013600     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1024.2
            013700     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB1024.2
            013800     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB1024.2
            013900     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB1024.2
            014000     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB1024.2
            014100 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB1024.2
            014200 01  REC-CT PICTURE 99 VALUE ZERO.                                DB1024.2
            014300 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB1024.2
            014400 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB1024.2
            014500 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB1024.2
            014600 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB1024.2
            014700 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB1024.2
            014800 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB1024.2
            014900 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB1024.2
            015000 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB1024.2
            015100 01  CCVS-H-1.                                                    DB1024.2
            015200     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB1024.2
            015300     02 FILLER PICTURE X(67) VALUE                                DB1024.2
            015400     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB1024.2
            015500-    " SYSTEM".                                                   DB1024.2
            015600     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB1024.2
            015700 01  CCVS-H-2.                                                    DB1024.2
            015800     02 FILLER PICTURE X(52) VALUE IS                             DB1024.2
            015900     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB1024.2
            016000     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB1024.2
            016100     02 TEST-ID PICTURE IS X(9).                                  DB1024.2
            016200     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB1024.2
            016300 01  CCVS-H-3.                                                    DB1024.2
            016400     02  FILLER PICTURE X(34) VALUE                               DB1024.2
            016500     " FOR OFFICIAL USE ONLY    ".                                DB1024.2
            016600     02  FILLER PICTURE X(58) VALUE                               DB1024.2
            016700     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB1024.2
            016800     02  FILLER PICTURE X(28) VALUE                               DB1024.2
            016900     "  COPYRIGHT   1974 ".                                       DB1024.2
            017000 01  CCVS-E-1.                                                    DB1024.2
            017100     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB1024.2
            017200     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB1024.2
            017300     02 ID-AGAIN PICTURE IS X(9).                                 DB1024.2
            017400     02 FILLER PICTURE X(45) VALUE IS                             DB1024.2
            017500     " NTIS DISTRIBUTION COBOL 74".                               DB1024.2
            017600 01  CCVS-E-2.                                                    DB1024.2
            017700     02  FILLER                   PICTURE X(31)  VALUE            DB1024.2
            017800     SPACE.                                                       DB1024.2
            017900     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB1024.2
            018000     02 CCVS-E-2-2.                                               DB1024.2
            018100         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB1024.2
            018200         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB1024.2
            018300         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB1024.2
            018400 01  CCVS-E-3.                                                    DB1024.2
            018500     02  FILLER PICTURE X(22) VALUE                               DB1024.2
            018600     " FOR OFFICIAL USE ONLY".                                    DB1024.2
            018700     02  FILLER PICTURE X(12) VALUE SPACE.                        DB1024.2
            018800     02  FILLER PICTURE X(58) VALUE                               DB1024.2
            018900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB1024.2
            019000     02  FILLER PICTURE X(13) VALUE SPACE.                        DB1024.2
            019100     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB1024.2
            019200 01  CCVS-E-4.                                                    DB1024.2
            019300     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB1024.2
            019400     02 FILLER PIC XXXX VALUE " OF ".                             DB1024.2
            019500     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB1024.2
            019600     02 FILLER PIC X(40) VALUE                                    DB1024.2
            019700      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB1024.2
            019800 01  XXINFO.                                                      DB1024.2
            019900     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB1024.2
            020000     02 INFO-TEXT.                                                DB1024.2
            020100     04 FILLER PIC X(20) VALUE SPACE.                             DB1024.2
            020200     04 XXCOMPUTED PIC X(20).                                     DB1024.2
            020300     04 FILLER PIC X(5) VALUE SPACE.                              DB1024.2
            020400     04 XXCORRECT PIC X(20).                                      DB1024.2
            020500 01  HYPHEN-LINE.                                                 DB1024.2
            020600     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1024.2
            020700     02 FILLER PICTURE IS X(65) VALUE IS "************************DB1024.2
            020800-    "*****************************************".                 DB1024.2
            020900     02 FILLER PICTURE IS X(54) VALUE IS "************************DB1024.2
            021000-    "******************************".                            DB1024.2
            021100 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB1024.2
            021200     "DB102A".                                                    DB1024.2
            021300 PROCEDURE DIVISION.                                              DB1024.2
            021400 DECLARATIVES.                                                    DB1024.2
            021500 START-UP SECTION.                                                DB1024.2
            021600     USE FOR DEBUGGING ON OPEN-FILES.                             DB1024.2
            021700 BEGIN-START-UP.                                                  DB1024.2
            021800     MOVE 1 TO RESULT-FLAG.                                       DB1024.2
            021900 DB-COMMON.                                                       DB1024.2
            022000     MOVE DEBUG-LINE TO DBLINE-HOLD.                              DB1024.2
            022100     MOVE DEBUG-NAME TO DBNAME-HOLD.                              DB1024.2
            022200     MOVE DEBUG-CONTENTS TO DBCONT-HOLD.                          DB1024.2
            022300 FALL-THROUGH-AND-SERIES SECTION.                                 DB1024.2
            022400     USE FOR DEBUGGING ON FALL-THROUGH-TEST                       DB1024.2
            022500              PROC-SERIES-TEST.                                   DB1024.2
            022600 BEGIN-FALL-THROUGH-AND-SERIES.                                   DB1024.2
            022700     PERFORM DB-COMMON.                                           DB1024.2
            022800     MOVE 2 TO RESULT-FLAG.                                       DB1024.2
            022900 GO-TO SECTION.                                                   DB1024.2
            023000     USE FOR DEBUGGING ON GO-TO-TEST.                             DB1024.2
            023100 BEGIN-GO-TO.                                                     DB1024.2
            023200     PERFORM DB-COMMON.                                           DB1024.2
            023300     MOVE 3 TO RESULT-FLAG.                                       DB1024.2
            023400 ALTER-PARAGRAPH SECTION.                                         DB1024.2
            023500     USE FOR DEBUGGING ON ALTERABLE-PARAGRAPH.                    DB1024.2
            023600 BEGIN-ALTER-PARAGRAPH.                                           DB1024.2
            023700     PERFORM DB-COMMON.                                           DB1024.2
            023800     MOVE 4 TO RESULT-FLAG.                                       DB1024.2
            023900 LOOP-ITERATION SECTION.                                          DB1024.2
            024000     USE FOR DEBUGGING ON LOOP-ROUTINE.                           DB1024.2
            024100 BEGIN-LOOP-ITERATION.                                            DB1024.2
            024200     PERFORM DB-COMMON.                                           DB1024.2
            024300     ADD 1 TO RESULT-FLAG.                                        DB1024.2
            024400 PERFORM-THRU SECTION.                                            DB1024.2
            024500     USE FOR DEBUGGING ON DO-NOTHING-1.                           DB1024.2
            024600 BEGIN-PERFORM-THRU.                                              DB1024.2
            024700     PERFORM DB-COMMON.                                           DB1024.2
            024800     ADD 1 TO RESULT-FLAG.                                        DB1024.2
            024900 END DECLARATIVES.                                                DB1024.2
            025000 CCVS1 SECTION.                                                   DB1024.2
            025100 OPEN-FILES.                                                      DB1024.2
            025200     OPEN     OUTPUT PRINT-FILE.                                  DB1024.2
            025300     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB1024.2
            025400     MOVE    SPACE TO TEST-RESULTS.                               DB1024.2
            025500     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB1024.2
            025600     GO TO CCVS1-EXIT.                                            DB1024.2
            025700 CLOSE-FILES.                                                     DB1024.2
            025800     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB1024.2
            025900 TERMINATE-CCVS.                                                  DB1024.2
            026000S    EXIT PROGRAM.                                                DB1024.2
            026100STERMINATE-CALL.                                                  DB1024.2
            026200     STOP     RUN.                                                DB1024.2
            026300 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB1024.2
            026400 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB1024.2
            026500 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB1024.2
            026600 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB1024.2
            026700     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1024.2
            026800 PRINT-DETAIL.                                                    DB1024.2
            026900     IF REC-CT NOT EQUAL TO ZERO                                  DB1024.2
            027000             MOVE "." TO PARDOT-X                                 DB1024.2
            027100             MOVE REC-CT TO DOTVALUE.                             DB1024.2
            027200     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB1024.2
            027300     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB1024.2
            027400        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB1024.2
            027500          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB1024.2
            027600     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB1024.2
            027700     MOVE SPACE TO CORRECT-X.                                     DB1024.2
            027800     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1024.2
            027900     MOVE     SPACE TO RE-MARK.                                   DB1024.2
            028000 HEAD-ROUTINE.                                                    DB1024.2
            028100     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1024.2
            028200     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB1024.2
            028300     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB1024.2
            028400 COLUMN-NAMES-ROUTINE.                                            DB1024.2
            028500     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1024.2
            028600     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1024.2
            028700     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB1024.2
            028800 END-ROUTINE.                                                     DB1024.2
            028900     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB1024.2
            029000 END-RTN-EXIT.                                                    DB1024.2
            029100     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1024.2
            029200 END-ROUTINE-1.                                                   DB1024.2
            029300      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB1024.2
            029400      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB1024.2
            029500      ADD PASS-COUNTER TO ERROR-HOLD.                             DB1024.2
            029600*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB1024.2
            029700      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB1024.2
            029800      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB1024.2
            029900      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB1024.2
            030000      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB1024.2
            030100  END-ROUTINE-12.                                                 DB1024.2
            030200      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB1024.2
            030300     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB1024.2
            030400         MOVE "NO " TO ERROR-TOTAL                                DB1024.2
            030500         ELSE                                                     DB1024.2
            030600         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB1024.2
            030700     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB1024.2
            030800     PERFORM WRITE-LINE.                                          DB1024.2
            030900 END-ROUTINE-13.                                                  DB1024.2
            031000     IF DELETE-CNT IS EQUAL TO ZERO                               DB1024.2
            031100         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB1024.2
            031200         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB1024.2
            031300     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB1024.2
            031400     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1024.2
            031500      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB1024.2
            031600          MOVE "NO " TO ERROR-TOTAL                               DB1024.2
            031700      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB1024.2
            031800      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB1024.2
            031900      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB1024.2
            032000     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1024.2
            032100 WRITE-LINE.                                                      DB1024.2
            032200     ADD 1 TO RECORD-COUNT.                                       DB1024.2
            032300Y    IF RECORD-COUNT GREATER 50                                   DB1024.2
            032400Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1024.2
            032500Y        MOVE SPACE TO DUMMY-RECORD                               DB1024.2
            032600Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1024.2
            032700Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB1024.2
            032800Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB1024.2
            032900Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB1024.2
            033000Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1024.2
            033100Y        MOVE ZERO TO RECORD-COUNT.                               DB1024.2
            033200     PERFORM WRT-LN.                                              DB1024.2
            033300 WRT-LN.                                                          DB1024.2
            033400     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1024.2
            033500     MOVE SPACE TO DUMMY-RECORD.                                  DB1024.2
            033600 BLANK-LINE-PRINT.                                                DB1024.2
            033700     PERFORM WRT-LN.                                              DB1024.2
            033800 FAIL-ROUTINE.                                                    DB1024.2
            033900     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB1024.2
            034000     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB1024.2
            034100     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB1024.2
            034200     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1024.2
            034300     GO TO FAIL-ROUTINE-EX.                                       DB1024.2
            034400 FAIL-ROUTINE-WRITE.                                              DB1024.2
            034500     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB1024.2
            034600     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB1024.2
            034700 FAIL-ROUTINE-EX. EXIT.                                           DB1024.2
            034800 BAIL-OUT.                                                        DB1024.2
            034900     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB1024.2
            035000     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB1024.2
            035100 BAIL-OUT-WRITE.                                                  DB1024.2
            035200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB1024.2
            035300     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1024.2
            035400 BAIL-OUT-EX. EXIT.                                               DB1024.2
            035500 CCVS1-EXIT.                                                      DB1024.2
            035600     EXIT.                                                        DB1024.2
            035700 START-PROGRAM-TEST.                                              DB1024.2
            035800     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            035900         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            036000         PERFORM FAIL                                             DB1024.2
            036100         PERFORM START-PROGRAM-WRITE                              DB1024.2
            036200         ELSE PERFORM PASS                                        DB1024.2
            036300         GO TO START-PROGRAM-WRITE.                               DB1024.2
            036400     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            036500     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            036600     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            036700     PERFORM START-PROGRAM-WRITE.                                 DB1024.2
            036800     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            036900     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            037000     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            037100     PERFORM START-PROGRAM-WRITE.                                 DB1024.2
            037200     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            037300     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            037400     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            037500     GO TO   START-PROGRAM-WRITE.                                 DB1024.2
            037600 START-PROGRAM-DELETE.                                            DB1024.2
            037700     PERFORM DE-LETE.                                             DB1024.2
            037800 START-PROGRAM-WRITE.                                             DB1024.2
            037900     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            038000     MOVE "START-PROGRAM-TEST" TO PAR-NAME.                       DB1024.2
            038100     PERFORM PRINT-DETAIL.                                        DB1024.2
            038200     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            038300 FALL-THROUGH-TEST.                                               DB1024.2
            038400     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            038500         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            038600         PERFORM FAIL                                             DB1024.2
            038700         PERFORM FALL-THROUGH-WRITE                               DB1024.2
            038800         ELSE PERFORM PASS                                        DB1024.2
            038900         GO TO FALL-THROUGH-WRITE.                                DB1024.2
            039000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            039100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            039200     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            039300     PERFORM FALL-THROUGH-WRITE.                                  DB1024.2
            039400     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            039500     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            039600     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            039700     PERFORM FALL-THROUGH-WRITE.                                  DB1024.2
            039800     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            039900     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            040000     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            040100     GO TO   FALL-THROUGH-WRITE.                                  DB1024.2
            040200 FALL-THROUGH-DELETE.                                             DB1024.2
            040300     PERFORM DE-LETE.                                             DB1024.2
            040400 FALL-THROUGH-WRITE.                                              DB1024.2
            040500     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            040600     MOVE "FALL-THROUGH-TEST" TO PAR-NAME.                        DB1024.2
            040700     PERFORM PRINT-DETAIL.                                        DB1024.2
            040800     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            040900 PROC-SERIES-TEST.                                                DB1024.2
            041000     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            041100         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            041200         PERFORM FAIL                                             DB1024.2
            041300         PERFORM PROC-SERIES-WRITE                                DB1024.2
            041400         ELSE PERFORM PASS                                        DB1024.2
            041500         GO TO PROC-SERIES-WRITE.                                 DB1024.2
            041600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            041700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            041800     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            041900     PERFORM PROC-SERIES-WRITE.                                   DB1024.2
            042000     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            042100     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            042200     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            042300     PERFORM PROC-SERIES-WRITE.                                   DB1024.2
            042400     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            042500     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            042600     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            042700     GO TO   PROC-SERIES-WRITE.                                   DB1024.2
            042800 PROC-SERIES-DELETE.                                              DB1024.2
            042900     PERFORM DE-LETE.                                             DB1024.2
            043000 PROC-SERIES-WRITE.                                               DB1024.2
            043100     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            043200     MOVE "PROC-SERIES-TEST" TO PAR-NAME.                         DB1024.2
            043300     PERFORM PRINT-DETAIL.                                        DB1024.2
            043400     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            043500 ALTERABLE-PARAGRAPH.                                             DB1024.2
            043600     GO TO GO-TO-TEST.                                            DB1024.2
            043700 FILLER-PARAGRAPH.                                                DB1024.2
            043800     DISPLAY "ALTER FAILED AT ALTER-TEST-INIT".                   DB1024.2
            043900     PERFORM FAIL.                                                DB1024.2
            044000     GO TO ALTERED-GO-TO-TEST.                                    DB1024.2
            044100 GO-TO-TEST.                                                      DB1024.2
            044200     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            044300         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            044400         PERFORM FAIL                                             DB1024.2
            044500         PERFORM GO-TO-WRITE                                      DB1024.2
            044600         ELSE PERFORM PASS                                        DB1024.2
            044700         GO TO GO-TO-WRITE.                                       DB1024.2
            044800     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            044900     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            045000     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            045100     PERFORM GO-TO-WRITE.                                         DB1024.2
            045200     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            045300     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            045400     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            045500     PERFORM GO-TO-WRITE.                                         DB1024.2
            045600     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            045700     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            045800     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            045900     GO TO   GO-TO-WRITE.                                         DB1024.2
            046000 GO-TO-DELETE.                                                    DB1024.2
            046100     PERFORM DE-LETE.                                             DB1024.2
            046200 GO-TO-WRITE.                                                     DB1024.2
            046300     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            046400     MOVE "GO-TO-TEST" TO PAR-NAME.                               DB1024.2
            046500     PERFORM PRINT-DETAIL.                                        DB1024.2
            046600     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            046700 ALTER-TEST-INIT.                                                 DB1024.2
            046800     ALTER ALTERABLE-PARAGRAPH TO PROCEED TO ALTERED-GO-TO-TEST.  DB1024.2
            046900 ALTER-TEST.                                                      DB1024.2
            047000     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            047100         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            047200         PERFORM FAIL                                             DB1024.2
            047300         PERFORM ALTER-WRITE                                      DB1024.2
            047400         ELSE PERFORM PASS                                        DB1024.2
            047500         GO TO ALTER-WRITE.                                       DB1024.2
            047600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            047700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            047800     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            047900     PERFORM ALTER-WRITE.                                         DB1024.2
            048000     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            048100     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            048200     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            048300     PERFORM ALTER-WRITE.                                         DB1024.2
            048400     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            048500     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            048600     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            048700     GO TO   ALTER-WRITE.                                         DB1024.2
            048800 ALTER-DELETE.                                                    DB1024.2
            048900     PERFORM DE-LETE.                                             DB1024.2
            049000 ALTER-WRITE.                                                     DB1024.2
            049100     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            049200     MOVE "ALTER-TEST" TO PAR-NAME.                               DB1024.2
            049300     PERFORM PRINT-DETAIL.                                        DB1024.2
            049400     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            049500 ALTER-WRITE-END.                                                 DB1024.2
            049600     GO TO ALTERABLE-PARAGRAPH.                                   DB1024.2
            049700 ALTERED-GO-TO-TEST.                                              DB1024.2
            049800     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            049900         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            050000         PERFORM FAIL                                             DB1024.2
            050100         PERFORM ALTERED-GO-TO-WRITE                              DB1024.2
            050200         ELSE PERFORM PASS                                        DB1024.2
            050300         GO TO ALTERED-GO-TO-WRITE.                               DB1024.2
            050400     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            050500     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            050600     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            050700     PERFORM ALTERED-GO-TO-WRITE.                                 DB1024.2
            050800     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            050900     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            051000     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            051100     PERFORM ALTERED-GO-TO-WRITE.                                 DB1024.2
            051200     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            051300     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            051400     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            051500     GO TO   ALTERED-GO-TO-WRITE.                                 DB1024.2
            051600 ALTERED-GO-TO-DELETE.                                            DB1024.2
            051700     PERFORM DE-LETE.                                             DB1024.2
            051800 ALTERED-GO-TO-WRITE.                                             DB1024.2
            051900     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            052000     MOVE "ALTERED-GO-TO-TEST" TO PAR-NAME.                       DB1024.2
            052100     PERFORM PRINT-DETAIL.                                        DB1024.2
            052200     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            052300 PERF-ITERATION-TEST.                                             DB1024.2
            052400     PERFORM LOOP-ROUTINE FIVE TIMES.                             DB1024.2
            052500     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            052600         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            052700         PERFORM FAIL                                             DB1024.2
            052800         PERFORM PERF-ITERATION-WRITE                             DB1024.2
            052900         ELSE  PERFORM PASS                                       DB1024.2
            053000         GO TO PERF-ITERATION-WRITE.                              DB1024.2
            053100     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            053200     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            053300     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            053400     PERFORM PERF-ITERATION-WRITE.                                DB1024.2
            053500     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            053600     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            053700     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            053800     PERFORM PERF-ITERATION-WRITE.                                DB1024.2
            053900     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            054000     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            054100     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            054200     GO TO   PERF-ITERATION-WRITE.                                DB1024.2
            054300 PERF-ITERATION-DELETE.                                           DB1024.2
            054400     PERFORM DE-LETE.                                             DB1024.2
            054500 PERF-ITERATION-WRITE.                                            DB1024.2
            054600     MOVE "PERF-ITERATION-TEST" TO PAR-NAME.                      DB1024.2
            054700     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            054800     PERFORM PRINT-DETAIL.                                        DB1024.2
            054900     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            055000 PERF-ITERATION-END.                                              DB1024.2
            055100     GO TO PERFORM-THRU-TEST.                                     DB1024.2
            055200 LOOP-ROUTINE.                                                    DB1024.2
            055300**NESTED PERFORMS ARE USED HERE TO ATTEMPT TO PREVENT OPTIMIZER   DB1024.2
            055400* ACTION RESULTING IN LOOP UNFOLDING AND REDUCTION.               DB1024.2
            055500     PERFORM DO-NOTHING.                                          DB1024.2
            055600 DO-NOTHING.                                                      DB1024.2
            055700     ADD A B C GIVING D.                                          DB1024.2
            055800 DO-NOTHING-1.                                                    DB1024.2
            055900     SUBTRACT A FROM B.                                           DB1024.2
            056000 PERFORM-THRU-TEST.                                               DB1024.2
            056100     PERFORM DO-NOTHING THRU DO-NOTHING-1 FIVE TIMES.             DB1024.2
            056200     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            056300         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            056400         PERFORM FAIL                                             DB1024.2
            056500         PERFORM PERFORM-THRU-WRITE                               DB1024.2
            056600         ELSE PERFORM PASS                                        DB1024.2
            056700         GO TO PERFORM-THRU-WRITE.                                DB1024.2
            056800     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            056900     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            057000     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            057100     PERFORM PERFORM-THRU-WRITE.                                  DB1024.2
            057200     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            057300     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            057400     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            057500     PERFORM PERFORM-THRU-WRITE.                                  DB1024.2
            057600     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            057700     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            057800     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            057900     GO TO   PERFORM-THRU-WRITE.                                  DB1024.2
            058000 PERFORM-THRU-DELETE.                                             DB1024.2
            058100     PERFORM DE-LETE.                                             DB1024.2
            058200 PERFORM-THRU-WRITE.                                              DB1024.2
            058300     MOVE "PERFORM-THRU-TEST" TO PAR-NAME.                        DB1024.2
            058400     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            058500     PERFORM PRINT-DETAIL.                                        DB1024.2
            058600     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            058700 SIMPLE-PERFORM-TEST.                                             DB1024.2
            058800     PERFORM LOOP-ROUTINE.                                        DB1024.2
            058900     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
            059000         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
            059100         PERFORM FAIL                                             DB1024.2
            059200         PERFORM SIMPLE-PERFORM-WRITE                             DB1024.2
            059300         ELSE PERFORM PASS                                        DB1024.2
            059400         GO TO SIMPLE-PERFORM-WRITE.                              DB1024.2
            059500     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            059600     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
            059700     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
            059800     PERFORM SIMPLE-PERFORM-WRITE.                                DB1024.2
            059900     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
            060000     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
            060100     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
            060200     PERFORM SIMPLE-PERFORM-WRITE.                                DB1024.2
            060300     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
            060400     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
            060500     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
            060600     GO TO   SIMPLE-PERFORM-WRITE.                                DB1024.2
            060700 SIMPLE-PERFORM-DELETE.                                           DB1024.2
            060800     PERFORM DE-LETE.                                             DB1024.2
            060900 SIMPLE-PERFORM-WRITE.                                            DB1024.2
            061000     MOVE "SIMPLE-PERFORM-TEST" TO PAR-NAME.                      DB1024.2
            061100     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
            061200     PERFORM PRINT-DETAIL.                                        DB1024.2
            061300     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
            061400 DEBUG-LINE-TESTS-INIT.                                           DB1024.2
            061500     MOVE "DEBUG LINE TESTS" TO FEATURE.                          DB1024.2
            061600 DEBUG-LINE-TEST-01.                                              DB1024.2
            061700     MOVE "COMPLETE ENTITY" TO RE-MARK.                           DB1024.2
            061800     PERFORM FAIL.                                                DB1024.2
            061900D    PERFORM PASS  SUBTRACT 1 FROM ERROR-COUNTER.                 DB1024.2
            062000     GO TO DEBUG-LINE-WRITE-01.                                   DB1024.2
            062100 DEBUG-LINE-DELETE-01.                                            DB1024.2
            062200     PERFORM DE-LETE.                                             DB1024.2
            062300 DEBUG-LINE-WRITE-01.                                             DB1024.2
            062400     MOVE "DEBUG-LINE-TEST-01" TO PAR-NAME.                       DB1024.2
            062500     PERFORM PRINT-DETAIL.                                        DB1024.2
            062600 DEBUG-LINE-TEST-02.                                              DB1024.2
            062700     MOVE "CONSECUTIVE DEBUG LINES" TO RE-MARK.                   DB1024.2
            062800     PERFORM FAIL.                                                DB1024.2
            062900D    PERFORM PASS.                                                DB1024.2
            063000D    SUBTRACT 1 FROM ERROR-COUNTER.                               DB1024.2
            063100     GO TO DEBUG-LINE-WRITE-02.                                   DB1024.2
            063200 DEBUG-LINE-DELETE-02.                                            DB1024.2
            063300     PERFORM DE-LETE.                                             DB1024.2
            063400 DEBUG-LINE-WRITE-02.                                             DB1024.2
            063500     MOVE "DEBUG-LINE-TEST-02" TO PAR-NAME.                       DB1024.2
            063600     PERFORM PRINT-DETAIL.                                        DB1024.2
            063700 DEBUG-LINE-TEST-03.                                              DB1024.2
            063800     MOVE "BROKEN STATEMENTS" TO RE-MARK.                         DB1024.2
            063900     PERFORM                                                      DB1024.2
            064000D        PASS.  GO TO DEBUG-LINE-WRITE-03.                        DB1024.2
            064100DDEBUG-LINE-TEST-03-A.    PERFORM                                 DB1024.2
            064200                             FAIL.                                DB1024.2
            064300                             GO TO DEBUG-LINE-WRITE-03.           DB1024.2
            064400 DEBUG-LINE-DELETE-03.                                            DB1024.2
            064500     PERFORM DE-LETE.                                             DB1024.2
            064600 DEBUG-LINE-WRITE-03.                                             DB1024.2
            064700     MOVE "DEBUG-LINE-TEST-03" TO PAR-NAME.                       DB1024.2
            064800     PERFORM PRINT-DETAIL.                                        DB1024.2
            064900 DEBUG-LINE-TEST-04.                                              DB1024.2
            065000     MOVE "NESTED COMMENTS" TO RE-MARK.                           DB1024.2
            065100D    PERFORM                                                      DB1024.2
            065200*        FAIL.  GO TO DEBUG-LINE-WRITE-04.                        DB1024.2
            065300*DEBUG-LINE-TEST-04-A.    PERFORM                                 DB1024.2
            065400D                         PASS.  GO TO DEBUG-LINE-WRITE-04.       DB1024.2
            065500 DEBUG-LINE-TEST-04-B.                                            DB1024.2
            065600     MOVE "    FAILURE 04B" TO COMPUTED-A.                        DB1024.2
            065700     PERFORM FAIL.                                                DB1024.2
            065800     GO TO DEBUG-LINE-WRITE-04.                                   DB1024.2
            065900 DEBUG-LINE-DELETE-04.                                            DB1024.2
            066000     PERFORM DE-LETE.                                             DB1024.2
            066100 DEBUG-LINE-WRITE-04.                                             DB1024.2
            066200     MOVE "DEBUG-LINE-TEST-04" TO PAR-NAME.                       DB1024.2
            066300     PERFORM PRINT-DETAIL.                                        DB1024.2
            066400 DEBUG-LINE-TEST-05.                                              DB1024.2
            066500     MOVE "NESTED INSIDE COMMENTS" TO RE-MARK.                    DB1024.2
            066600*    PERFORM FAIL.                                                DB1024.2
            066700*    GO TO DEBUG-LINE-WRITE-05.                                   DB1024.2
            066800*DEBUG-LINE-TEST-05-A.                                            DB1024.2
            066900D    PERFORM PASS.                                                DB1024.2
            067000D    GO TO DEBUG-LINE-WRITE-05.                                   DB1024.2
            067100*DEBUG-LINE-TEST-05-B.                                            DB1024.2
            067200*    MOVE "    FAILURE 05B" TO COMPUTED-A.                        DB1024.2
            067300*    PERFORM FAIL.  GO TO DEBUG-LINE-WRITE-05.                    DB1024.2
            067400 DEBUG-LINE-TEST-05-C.                                            DB1024.2
            067500     MOVE "    FAILURE 05C" TO COMPUTED-A.                        DB1024.2
            067600     PERFORM FAIL.   GO TO DEBUG-LINE-WRITE-05.                   DB1024.2
            067700 DEBUG-LINE-DELETE-05.                                            DB1024.2
            067800     PERFORM DE-LETE.                                             DB1024.2
            067900 DEBUG-LINE-WRITE-05.                                             DB1024.2
            068000     MOVE "DEBUG-LINE-TEST-05" TO PAR-NAME.                       DB1024.2
            068100     PERFORM PRINT-DETAIL.                                        DB1024.2
            068200 CCVS-EXIT SECTION.                                               DB1024.2
            068300 CCVS-999999.                                                     DB1024.2
            068400     GO TO CLOSE-FILES.                                           DB1024.2
                  *END-OF,DB102A                                                            
        """)
    )

    @Disabled("Requires S and Y indicators")
    @Test
    fun db1034_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB103M                                                      
            000100 IDENTIFICATION DIVISION.                                         DB1034.2
            000200 PROGRAM-ID.                                                      DB1034.2
            000300     DB103M.                                                      DB1034.2
            000400 AUTHOR.                                                          DB1034.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB1034.2
            000600 INSTALLATION.                                                    DB1034.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB1034.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB1034.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB1034.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB1034.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB1034.2
            001200                                                                  DB1034.2
            001300     PHONE   (703) 756-6153                                       DB1034.2
            001400                                                                  DB1034.2
            001500     " HIGH       ".                                              DB1034.2
            001600 DATE-WRITTEN.                                                    DB1034.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB1034.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB1034.2
            001900     "4.2 ".                                                      DB1034.2
            002000 SECURITY.                                                        DB1034.2
            002100     NONE.                                                        DB1034.2
            002200*                                                                 DB1034.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB1034.2
            002400*                                                                 DB1034.2
            002500*                       PROGRAM ABSTRACT                          DB1034.2
            002600*                                                                 DB1034.2
            002700*    DB103M TESTS THE BASIC OPERATION OF DEBUG MODULE FACILITIES  DB1034.2
            002800*    WHEN THE COMPILE TIME DEBUGGING SWITCH IS OFF.  ALL DEBUG    DB1034.2
            002900*    LINES  SHOULD BE TREATED AS COMMENTS AND NO CODE SHOULD      DB1034.2
            003000*    BE GENERATED FOR EITHER DEBUG LINES OR DEBUGGING PROCED-     DB1034.2
            003100*    URES.                                                        DB1034.2
            003200*                                                                 DB1034.2
            003300*    THE OBJECT PROGRAM FOR DB103M SHOULD BE EXECUTED TWICE;ONCE  DB1034.2
            003400*    WITH THE OBJECT TIME DEBUGGING SWITCH ENABLED (ON), AND ONCE DB1034.2
            003500*    WITH THE OBJECT TIME DEBUGGING SWITCH DISABLED (OFF).  BOTH  DB1034.2
            003600*    EXECUTION RUNS SHOULD YIELD THE SAME RESULTS AS THE SETTING  DB1034.2
            003700*    OF THE OBJECT TIME DEBUGGING SWITCH SHOULD MAKE NO DIFFER-   DB1034.2
            003800*    ENCE SINCE THE COMPILE TIME DEBUGGING SWITCH WAS INITIALLY   DB1034.2
            003900*    DISABLED.                                                    DB1034.2
            004000*                                                                 DB1034.2
            004100*                                                                 DB1034.2
            004200 ENVIRONMENT DIVISION.                                            DB1034.2
            004300 CONFIGURATION SECTION.                                           DB1034.2
            004400 SOURCE-COMPUTER.                                                 DB1034.2
            004500     XXXXX082.                                                    DB1034.2
            004600 OBJECT-COMPUTER.                                                 DB1034.2
            004700     XXXXX083.                                                    DB1034.2
            004800 INPUT-OUTPUT SECTION.                                            DB1034.2
            004900 FILE-CONTROL.                                                    DB1034.2
            005000     SELECT PRINT-FILE ASSIGN TO                                  DB1034.2
            005100     XXXXX055.                                                    DB1034.2
            005200 DATA DIVISION.                                                   DB1034.2
            005300 FILE SECTION.                                                    DB1034.2
            005400 FD  PRINT-FILE                                                   DB1034.2
            005500     LABEL RECORDS                                                DB1034.2
            005600     XXXXX084                                                     DB1034.2
            005700     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB1034.2
            005800 01  PRINT-REC PICTURE X(120).                                    DB1034.2
            005900 01  DUMMY-RECORD PICTURE X(120).                                 DB1034.2
            006000 WORKING-STORAGE SECTION.                                         DB1034.2
            006100 77  A PIC 9 COMP VALUE 1.                                        DB1034.2
            006200 77  B PIC 9 COMP VALUE 5.                                        DB1034.2
            006300 77  C PIC 9 COMP VALUE 9.                                        DB1034.2
            006400 77  D PIC 99 COMP.                                               DB1034.2
            006500 77  RESULT-FLAG PIC 99 COMP VALUE 0.                             DB1034.2
            006600 77  DBLINE-HOLD PIC X(6).                                        DB1034.2
            006700 77  DBNAME-HOLD PIC X(30).                                       DB1034.2
            006800 77  DBCONT-HOLD PIC X(30).                                       DB1034.2
            006900 77  FIVE PIC 9 COMP VALUE 5.                                     DB1034.2
            007000 01  SIZE-19.                                                     DB1034.2
            007100     02  FILLER PIC X.                                            DB1034.2
            007200     02  SIZE-18.                                                 DB1034.2
            007300         03  FILLER PIC X.                                        DB1034.2
            007400         03  SIZE-17.                                             DB1034.2
            007500             04  FILLER PIC X.                                    DB1034.2
            007600             04  SIZE-16.                                         DB1034.2
            007700                 05  FILLER PIC X.                                DB1034.2
            007800                 05  SIZE-15.                                     DB1034.2
            007900                     06  FILLER PIC X.                            DB1034.2
            008000                     06  SIZE-14.                                 DB1034.2
            008100                         07  FILLER PIC X.                        DB1034.2
            008200                         07  SIZE-13.                             DB1034.2
            008300                             08  FILLER PIC X.                    DB1034.2
            008400                             08  SIZE-12.                         DB1034.2
            008500                                 09  FILLER PIC X.                DB1034.2
            008600                                 09  SIZE-11.                     DB1034.2
            008700                                     10  FILLER PIC X.            DB1034.2
            008800                                     10  SIZE-10 PIC X(10).       DB1034.2
            008900 01  TEST-RESULTS.                                                DB1034.2
            009000     02 FILLER                    PICTURE X VALUE SPACE.          DB1034.2
            009100     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB1034.2
            009200     02 FILLER                    PICTURE X VALUE SPACE.          DB1034.2
            009300     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB1034.2
            009400     02 FILLER                    PICTURE X  VALUE SPACE.         DB1034.2
            009500     02  PAR-NAME.                                                DB1034.2
            009600       03 FILLER PICTURE X(12) VALUE SPACE.                       DB1034.2
            009700       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB1034.2
            009800       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB1034.2
            009900       03 FILLER PIC X(5) VALUE SPACE.                            DB1034.2
            010000     02 FILLER PIC X(10) VALUE SPACE.                             DB1034.2
            010100     02 RE-MARK PIC X(61).                                        DB1034.2
            010200 01  TEST-COMPUTED.                                               DB1034.2
            010300     02 FILLER PIC X(30) VALUE SPACE.                             DB1034.2
            010400     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB1034.2
            010500     02 COMPUTED-X.                                               DB1034.2
            010600     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB1034.2
            010700     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB1034.2
            010800     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB1034.2
            010900     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB1034.2
            011000     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB1034.2
            011100     03       CM-18V0 REDEFINES COMPUTED-A.                       DB1034.2
            011200         04 COMPUTED-18V0                   PICTURE -9(18).       DB1034.2
            011300         04 FILLER                          PICTURE X.            DB1034.2
            011400     03 FILLER PIC X(50) VALUE SPACE.                             DB1034.2
            011500 01  TEST-CORRECT.                                                DB1034.2
            011600     02 FILLER PIC X(30) VALUE SPACE.                             DB1034.2
            011700     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB1034.2
            011800     02 CORRECT-X.                                                DB1034.2
            011900     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB1034.2
            012000     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB1034.2
            012100     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB1034.2
            012200     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB1034.2
            012300     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB1034.2
            012400     03      CR-18V0 REDEFINES CORRECT-A.                         DB1034.2
            012500         04 CORRECT-18V0                    PICTURE -9(18).       DB1034.2
            012600         04 FILLER                          PICTURE X.            DB1034.2
            012700     03 FILLER PIC X(50) VALUE SPACE.                             DB1034.2
            012800 01  CCVS-C-1.                                                    DB1034.2
            012900     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB1034.2
            013000-    "SS  PARAGRAPH-NAME                                          DB1034.2
            013100-    "        REMARKS".                                           DB1034.2
            013200     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB1034.2
            013300 01  CCVS-C-2.                                                    DB1034.2
            013400     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1034.2
            013500     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB1034.2
            013600     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB1034.2
            013700     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB1034.2
            013800     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB1034.2
            013900 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB1034.2
            014000 01  REC-CT PICTURE 99 VALUE ZERO.                                DB1034.2
            014100 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB1034.2
            014200 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB1034.2
            014300 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB1034.2
            014400 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB1034.2
            014500 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB1034.2
            014600 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB1034.2
            014700 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB1034.2
            014800 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB1034.2
            014900 01  CCVS-H-1.                                                    DB1034.2
            015000     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB1034.2
            015100     02 FILLER PICTURE X(67) VALUE                                DB1034.2
            015200     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB1034.2
            015300-    " SYSTEM".                                                   DB1034.2
            015400     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB1034.2
            015500 01  CCVS-H-2.                                                    DB1034.2
            015600     02 FILLER PICTURE X(52) VALUE IS                             DB1034.2
            015700     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB1034.2
            015800     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB1034.2
            015900     02 TEST-ID PICTURE IS X(9).                                  DB1034.2
            016000     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB1034.2
            016100 01  CCVS-H-3.                                                    DB1034.2
            016200     02  FILLER PICTURE X(34) VALUE                               DB1034.2
            016300     " FOR OFFICIAL USE ONLY    ".                                DB1034.2
            016400     02  FILLER PICTURE X(58) VALUE                               DB1034.2
            016500     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB1034.2
            016600     02  FILLER PICTURE X(28) VALUE                               DB1034.2
            016700     "  COPYRIGHT   1974 ".                                       DB1034.2
            016800 01  CCVS-E-1.                                                    DB1034.2
            016900     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB1034.2
            017000     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB1034.2
            017100     02 ID-AGAIN PICTURE IS X(9).                                 DB1034.2
            017200     02 FILLER PICTURE X(45) VALUE IS                             DB1034.2
            017300     " NTIS DISTRIBUTION COBOL 74".                               DB1034.2
            017400 01  CCVS-E-2.                                                    DB1034.2
            017500     02  FILLER                   PICTURE X(31)  VALUE            DB1034.2
            017600     SPACE.                                                       DB1034.2
            017700     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB1034.2
            017800     02 CCVS-E-2-2.                                               DB1034.2
            017900         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB1034.2
            018000         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB1034.2
            018100         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB1034.2
            018200 01  CCVS-E-3.                                                    DB1034.2
            018300     02  FILLER PICTURE X(22) VALUE                               DB1034.2
            018400     " FOR OFFICIAL USE ONLY".                                    DB1034.2
            018500     02  FILLER PICTURE X(12) VALUE SPACE.                        DB1034.2
            018600     02  FILLER PICTURE X(58) VALUE                               DB1034.2
            018700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB1034.2
            018800     02  FILLER PICTURE X(13) VALUE SPACE.                        DB1034.2
            018900     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB1034.2
            019000 01  CCVS-E-4.                                                    DB1034.2
            019100     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB1034.2
            019200     02 FILLER PIC XXXX VALUE " OF ".                             DB1034.2
            019300     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB1034.2
            019400     02 FILLER PIC X(40) VALUE                                    DB1034.2
            019500      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB1034.2
            019600 01  XXINFO.                                                      DB1034.2
            019700     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB1034.2
            019800     02 INFO-TEXT.                                                DB1034.2
            019900     04 FILLER PIC X(20) VALUE SPACE.                             DB1034.2
            020000     04 XXCOMPUTED PIC X(20).                                     DB1034.2
            020100     04 FILLER PIC X(5) VALUE SPACE.                              DB1034.2
            020200     04 XXCORRECT PIC X(20).                                      DB1034.2
            020300 01  HYPHEN-LINE.                                                 DB1034.2
            020400     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1034.2
            020500     02 FILLER PICTURE IS X(65) VALUE IS "************************DB1034.2
            020600-    "*****************************************".                 DB1034.2
            020700     02 FILLER PICTURE IS X(54) VALUE IS "************************DB1034.2
            020800-    "******************************".                            DB1034.2
            020900 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB1034.2
            021000     "DB103M".                                                    DB1034.2
            021100 PROCEDURE DIVISION.                                              DB1034.2
            021200 DECLARATIVES.                                                    DB1034.2
            021300 START-UP SECTION.                                                DB1034.2
            021400     USE FOR DEBUGGING ON OPEN-FILES.                             DB1034.2
            021500 BEGIN-START-UP.                                                  DB1034.2
            021600     MOVE 1 TO RESULT-FLAG.                                       DB1034.2
            021700 DB-COMMON.                                                       DB1034.2
            021800     MOVE DEBUG-LINE TO DBLINE-HOLD.                              DB1034.2
            021900     MOVE DEBUG-NAME TO DBNAME-HOLD.                              DB1034.2
            022000     MOVE DEBUG-CONTENTS TO DBCONT-HOLD.                          DB1034.2
            022100 FALL-THROUGH-AND-SERIES SECTION.                                 DB1034.2
            022200     USE FOR DEBUGGING ON FALL-THROUGH-TEST                       DB1034.2
            022300              PROC-SERIES-TEST.                                   DB1034.2
            022400 BEGIN-FALL-THROUGH-AND-SERIES.                                   DB1034.2
            022500     PERFORM DB-COMMON.                                           DB1034.2
            022600     MOVE 2 TO RESULT-FLAG.                                       DB1034.2
            022700 GO-TO SECTION.                                                   DB1034.2
            022800     USE FOR DEBUGGING ON GO-TO-TEST.                             DB1034.2
            022900 BEGIN-GO-TO.                                                     DB1034.2
            023000     PERFORM DB-COMMON.                                           DB1034.2
            023100     MOVE 3 TO RESULT-FLAG.                                       DB1034.2
            023200 ALTER-PARAGRAPH SECTION.                                         DB1034.2
            023300     USE FOR DEBUGGING ON ALTERABLE-PARAGRAPH.                    DB1034.2
            023400 BEGIN-ALTER-PARAGRAPH.                                           DB1034.2
            023500     PERFORM DB-COMMON.                                           DB1034.2
            023600     MOVE 4 TO RESULT-FLAG.                                       DB1034.2
            023700 LOOP-ITERATION SECTION.                                          DB1034.2
            023800     USE FOR DEBUGGING ON LOOP-ROUTINE.                           DB1034.2
            023900 BEGIN-LOOP-ITERATION.                                            DB1034.2
            024000     PERFORM DB-COMMON.                                           DB1034.2
            024100     ADD 1 TO RESULT-FLAG.                                        DB1034.2
            024200 PERFORM-THRU SECTION.                                            DB1034.2
            024300     USE FOR DEBUGGING ON DO-NOTHING-1.                           DB1034.2
            024400 BEGIN-PERFORM-THRU.                                              DB1034.2
            024500     PERFORM DB-COMMON.                                           DB1034.2
            024600     ADD 1 TO RESULT-FLAG.                                        DB1034.2
            024700 END DECLARATIVES.                                                DB1034.2
            024800 CCVS1 SECTION.                                                   DB1034.2
            024900 OPEN-FILES.                                                      DB1034.2
            025000     OPEN     OUTPUT PRINT-FILE.                                  DB1034.2
            025100     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB1034.2
            025200     MOVE    SPACE TO TEST-RESULTS.                               DB1034.2
            025300     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB1034.2
            025400     GO TO CCVS1-EXIT.                                            DB1034.2
            025500 CLOSE-FILES.                                                     DB1034.2
            025600     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB1034.2
            025700 TERMINATE-CCVS.                                                  DB1034.2
            025800S    EXIT PROGRAM.                                                DB1034.2
            025900STERMINATE-CALL.                                                  DB1034.2
            026000     STOP     RUN.                                                DB1034.2
            026100 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB1034.2
            026200 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB1034.2
            026300 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB1034.2
            026400 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB1034.2
            026500     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1034.2
            026600 PRINT-DETAIL.                                                    DB1034.2
            026700     IF REC-CT NOT EQUAL TO ZERO                                  DB1034.2
            026800             MOVE "." TO PARDOT-X                                 DB1034.2
            026900             MOVE REC-CT TO DOTVALUE.                             DB1034.2
            027000     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB1034.2
            027100     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB1034.2
            027200        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB1034.2
            027300          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB1034.2
            027400     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB1034.2
            027500     MOVE SPACE TO CORRECT-X.                                     DB1034.2
            027600     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1034.2
            027700     MOVE     SPACE TO RE-MARK.                                   DB1034.2
            027800 HEAD-ROUTINE.                                                    DB1034.2
            027900     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1034.2
            028000     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB1034.2
            028100     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB1034.2
            028200 COLUMN-NAMES-ROUTINE.                                            DB1034.2
            028300     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1034.2
            028400     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1034.2
            028500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB1034.2
            028600 END-ROUTINE.                                                     DB1034.2
            028700     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB1034.2
            028800 END-RTN-EXIT.                                                    DB1034.2
            028900     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1034.2
            029000 END-ROUTINE-1.                                                   DB1034.2
            029100      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB1034.2
            029200      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB1034.2
            029300      ADD PASS-COUNTER TO ERROR-HOLD.                             DB1034.2
            029400*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB1034.2
            029500      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB1034.2
            029600      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB1034.2
            029700      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB1034.2
            029800      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB1034.2
            029900  END-ROUTINE-12.                                                 DB1034.2
            030000      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB1034.2
            030100     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB1034.2
            030200         MOVE "NO " TO ERROR-TOTAL                                DB1034.2
            030300         ELSE                                                     DB1034.2
            030400         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB1034.2
            030500     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB1034.2
            030600     PERFORM WRITE-LINE.                                          DB1034.2
            030700 END-ROUTINE-13.                                                  DB1034.2
            030800     IF DELETE-CNT IS EQUAL TO ZERO                               DB1034.2
            030900         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB1034.2
            031000         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB1034.2
            031100     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB1034.2
            031200     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1034.2
            031300      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB1034.2
            031400          MOVE "NO " TO ERROR-TOTAL                               DB1034.2
            031500      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB1034.2
            031600      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB1034.2
            031700      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB1034.2
            031800     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1034.2
            031900 WRITE-LINE.                                                      DB1034.2
            032000     ADD 1 TO RECORD-COUNT.                                       DB1034.2
            032100Y    IF RECORD-COUNT GREATER 50                                   DB1034.2
            032200Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1034.2
            032300Y        MOVE SPACE TO DUMMY-RECORD                               DB1034.2
            032400Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1034.2
            032500Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB1034.2
            032600Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB1034.2
            032700Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB1034.2
            032800Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1034.2
            032900Y        MOVE ZERO TO RECORD-COUNT.                               DB1034.2
            033000     PERFORM WRT-LN.                                              DB1034.2
            033100 WRT-LN.                                                          DB1034.2
            033200     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1034.2
            033300     MOVE SPACE TO DUMMY-RECORD.                                  DB1034.2
            033400 BLANK-LINE-PRINT.                                                DB1034.2
            033500     PERFORM WRT-LN.                                              DB1034.2
            033600 FAIL-ROUTINE.                                                    DB1034.2
            033700     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB1034.2
            033800     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB1034.2
            033900     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB1034.2
            034000     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1034.2
            034100     GO TO FAIL-ROUTINE-EX.                                       DB1034.2
            034200 FAIL-ROUTINE-WRITE.                                              DB1034.2
            034300     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB1034.2
            034400     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB1034.2
            034500 FAIL-ROUTINE-EX. EXIT.                                           DB1034.2
            034600 BAIL-OUT.                                                        DB1034.2
            034700     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB1034.2
            034800     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB1034.2
            034900 BAIL-OUT-WRITE.                                                  DB1034.2
            035000     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB1034.2
            035100     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1034.2
            035200 BAIL-OUT-EX. EXIT.                                               DB1034.2
            035300 CCVS1-EXIT.                                                      DB1034.2
            035400     EXIT.                                                        DB1034.2
            035500 START-PROGRAM-TEST.                                              DB1034.2
            035600     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            035700         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            035800         PERFORM FAIL                                             DB1034.2
            035900         PERFORM START-PROGRAM-WRITE                              DB1034.2
            036000         ELSE PERFORM PASS                                        DB1034.2
            036100         GO TO START-PROGRAM-WRITE.                               DB1034.2
            036200     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            036300     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            036400     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            036500     PERFORM START-PROGRAM-WRITE.                                 DB1034.2
            036600     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            036700     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            036800     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            036900     PERFORM START-PROGRAM-WRITE.                                 DB1034.2
            037000     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            037100     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            037200     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            037300     GO TO   START-PROGRAM-WRITE.                                 DB1034.2
            037400 START-PROGRAM-DELETE.                                            DB1034.2
            037500     PERFORM DE-LETE.                                             DB1034.2
            037600 START-PROGRAM-WRITE.                                             DB1034.2
            037700     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            037800     MOVE "START-PROGRAM-TEST" TO PAR-NAME.                       DB1034.2
            037900     PERFORM PRINT-DETAIL.                                        DB1034.2
            038000     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            038100 FALL-THROUGH-TEST.                                               DB1034.2
            038200     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            038300         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            038400         PERFORM FAIL                                             DB1034.2
            038500         PERFORM FALL-THROUGH-WRITE                               DB1034.2
            038600         ELSE PERFORM PASS                                        DB1034.2
            038700         GO TO FALL-THROUGH-WRITE.                                DB1034.2
            038800     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            038900     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            039000     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            039100     PERFORM FALL-THROUGH-WRITE.                                  DB1034.2
            039200     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            039300     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            039400     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            039500     PERFORM FALL-THROUGH-WRITE.                                  DB1034.2
            039600     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            039700     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            039800     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            039900     GO TO   FALL-THROUGH-WRITE.                                  DB1034.2
            040000 FALL-THROUGH-DELETE.                                             DB1034.2
            040100     PERFORM DE-LETE.                                             DB1034.2
            040200 FALL-THROUGH-WRITE.                                              DB1034.2
            040300     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            040400     MOVE "FALL-THROUGH-TEST" TO PAR-NAME.                        DB1034.2
            040500     PERFORM PRINT-DETAIL.                                        DB1034.2
            040600     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            040700 PROC-SERIES-TEST.                                                DB1034.2
            040800     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            040900         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            041000         PERFORM FAIL                                             DB1034.2
            041100         PERFORM PROC-SERIES-WRITE                                DB1034.2
            041200         ELSE PERFORM PASS                                        DB1034.2
            041300         GO TO PROC-SERIES-WRITE.                                 DB1034.2
            041400     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            041500     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            041600     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            041700     PERFORM PROC-SERIES-WRITE.                                   DB1034.2
            041800     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            041900     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            042000     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            042100     PERFORM PROC-SERIES-WRITE.                                   DB1034.2
            042200     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            042300     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            042400     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            042500     GO TO   PROC-SERIES-WRITE.                                   DB1034.2
            042600 PROC-SERIES-DELETE.                                              DB1034.2
            042700     PERFORM DE-LETE.                                             DB1034.2
            042800 PROC-SERIES-WRITE.                                               DB1034.2
            042900     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            043000     MOVE "PROC-SERIES-TEST" TO PAR-NAME.                         DB1034.2
            043100     PERFORM PRINT-DETAIL.                                        DB1034.2
            043200     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            043300 ALTERABLE-PARAGRAPH.                                             DB1034.2
            043400     GO TO GO-TO-TEST.                                            DB1034.2
            043500 FILLER-PARAGRAPH.                                                DB1034.2
            043600     DISPLAY "ALTER FAILED AT ALTER-TEST-INIT".                   DB1034.2
            043700     PERFORM FAIL.                                                DB1034.2
            043800     GO TO ALTERED-GO-TO-TEST.                                    DB1034.2
            043900 GO-TO-TEST.                                                      DB1034.2
            044000     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            044100         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            044200         PERFORM FAIL                                             DB1034.2
            044300         PERFORM GO-TO-WRITE                                      DB1034.2
            044400         ELSE PERFORM PASS                                        DB1034.2
            044500         GO TO GO-TO-WRITE.                                       DB1034.2
            044600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            044700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            044800     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            044900     PERFORM GO-TO-WRITE.                                         DB1034.2
            045000     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            045100     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            045200     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            045300     PERFORM GO-TO-WRITE.                                         DB1034.2
            045400     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            045500     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            045600     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            045700     GO TO   GO-TO-WRITE.                                         DB1034.2
            045800 GO-TO-DELETE.                                                    DB1034.2
            045900     PERFORM DE-LETE.                                             DB1034.2
            046000 GO-TO-WRITE.                                                     DB1034.2
            046100     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            046200     MOVE "GO-TO-TEST" TO PAR-NAME.                               DB1034.2
            046300     PERFORM PRINT-DETAIL.                                        DB1034.2
            046400     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            046500 ALTER-TEST-INIT.                                                 DB1034.2
            046600     ALTER ALTERABLE-PARAGRAPH TO PROCEED TO ALTERED-GO-TO-TEST.  DB1034.2
            046700 ALTER-TEST.                                                      DB1034.2
            046800     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            046900         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            047000         PERFORM FAIL                                             DB1034.2
            047100         PERFORM ALTER-WRITE                                      DB1034.2
            047200         ELSE PERFORM PASS                                        DB1034.2
            047300         GO TO ALTER-WRITE.                                       DB1034.2
            047400     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            047500     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            047600     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            047700     PERFORM ALTER-WRITE.                                         DB1034.2
            047800     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            047900     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            048000     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            048100     PERFORM ALTER-WRITE.                                         DB1034.2
            048200     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            048300     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            048400     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            048500     GO TO   ALTER-WRITE.                                         DB1034.2
            048600 ALTER-DELETE.                                                    DB1034.2
            048700     PERFORM DE-LETE.                                             DB1034.2
            048800 ALTER-WRITE.                                                     DB1034.2
            048900     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            049000     MOVE "ALTER-TEST" TO PAR-NAME.                               DB1034.2
            049100     PERFORM PRINT-DETAIL.                                        DB1034.2
            049200     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            049300 ALTER-WRITE-END.                                                 DB1034.2
            049400     GO TO ALTERABLE-PARAGRAPH.                                   DB1034.2
            049500 ALTERED-GO-TO-TEST.                                              DB1034.2
            049600     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            049700         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            049800         PERFORM FAIL                                             DB1034.2
            049900         PERFORM ALTERED-GO-TO-WRITE                              DB1034.2
            050000         ELSE PERFORM PASS                                        DB1034.2
            050100         GO TO ALTERED-GO-TO-WRITE.                               DB1034.2
            050200     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            050300     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            050400     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            050500     PERFORM ALTERED-GO-TO-WRITE.                                 DB1034.2
            050600     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            050700     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            050800     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            050900     PERFORM ALTERED-GO-TO-WRITE.                                 DB1034.2
            051000     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            051100     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            051200     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            051300     GO TO   ALTERED-GO-TO-WRITE.                                 DB1034.2
            051400 ALTERED-GO-TO-DELETE.                                            DB1034.2
            051500     PERFORM DE-LETE.                                             DB1034.2
            051600 ALTERED-GO-TO-WRITE.                                             DB1034.2
            051700     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            051800     MOVE "ALTERED-GO-TO-TEST" TO PAR-NAME.                       DB1034.2
            051900     PERFORM PRINT-DETAIL.                                        DB1034.2
            052000     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            052100 PERF-ITERATION-TEST.                                             DB1034.2
            052200     PERFORM LOOP-ROUTINE FIVE TIMES.                             DB1034.2
            052300     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            052400         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            052500         PERFORM FAIL                                             DB1034.2
            052600         PERFORM PERF-ITERATION-WRITE                             DB1034.2
            052700         ELSE  PERFORM PASS                                       DB1034.2
            052800         GO TO PERF-ITERATION-WRITE.                              DB1034.2
            052900     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            053000     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            053100     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            053200     PERFORM PERF-ITERATION-WRITE.                                DB1034.2
            053300     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            053400     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            053500     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            053600     PERFORM PERF-ITERATION-WRITE.                                DB1034.2
            053700     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            053800     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            053900     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            054000     GO TO   PERF-ITERATION-WRITE.                                DB1034.2
            054100 PERF-ITERATION-DELETE.                                           DB1034.2
            054200     PERFORM DE-LETE.                                             DB1034.2
            054300 PERF-ITERATION-WRITE.                                            DB1034.2
            054400     MOVE "PERF-ITERATION-TEST" TO PAR-NAME.                      DB1034.2
            054500     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            054600     PERFORM PRINT-DETAIL.                                        DB1034.2
            054700     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            054800 PERF-ITERATION-END.                                              DB1034.2
            054900     GO TO PERFORM-THRU-TEST.                                     DB1034.2
            055000 LOOP-ROUTINE.                                                    DB1034.2
            055100**NESTED PERFORMS ARE USED HERE TO ATTEMPT TO PREVENT OPTIMIZER   DB1034.2
            055200* ACTION RESULTING IN LOOP UNFOLDING AND REDUCTION.               DB1034.2
            055300     PERFORM DO-NOTHING.                                          DB1034.2
            055400 DO-NOTHING.                                                      DB1034.2
            055500     ADD A B C GIVING D.                                          DB1034.2
            055600 DO-NOTHING-1.                                                    DB1034.2
            055700     SUBTRACT A FROM B.                                           DB1034.2
            055800 PERFORM-THRU-TEST.                                               DB1034.2
            055900     PERFORM DO-NOTHING THRU DO-NOTHING-1 FIVE TIMES.             DB1034.2
            056000     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            056100         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            056200         PERFORM FAIL                                             DB1034.2
            056300         PERFORM PERFORM-THRU-WRITE                               DB1034.2
            056400         ELSE PERFORM PASS                                        DB1034.2
            056500         GO TO PERFORM-THRU-WRITE.                                DB1034.2
            056600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            056700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            056800     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            056900     PERFORM PERFORM-THRU-WRITE.                                  DB1034.2
            057000     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            057100     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            057200     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            057300     PERFORM PERFORM-THRU-WRITE.                                  DB1034.2
            057400     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            057500     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            057600     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            057700     GO TO   PERFORM-THRU-WRITE.                                  DB1034.2
            057800 PERFORM-THRU-DELETE.                                             DB1034.2
            057900     PERFORM DE-LETE.                                             DB1034.2
            058000 PERFORM-THRU-WRITE.                                              DB1034.2
            058100     MOVE "PERFORM-THRU-TEST" TO PAR-NAME.                        DB1034.2
            058200     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            058300     PERFORM PRINT-DETAIL.                                        DB1034.2
            058400     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            058500 SIMPLE-PERFORM-TEST.                                             DB1034.2
            058600     PERFORM LOOP-ROUTINE.                                        DB1034.2
            058700     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1034.2
            058800         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1034.2
            058900         PERFORM FAIL                                             DB1034.2
            059000         PERFORM SIMPLE-PERFORM-WRITE                             DB1034.2
            059100         ELSE PERFORM PASS                                        DB1034.2
            059200         GO TO SIMPLE-PERFORM-WRITE.                              DB1034.2
            059300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            059400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1034.2
            059500     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1034.2
            059600     PERFORM SIMPLE-PERFORM-WRITE.                                DB1034.2
            059700     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1034.2
            059800     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1034.2
            059900     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1034.2
            060000     PERFORM SIMPLE-PERFORM-WRITE.                                DB1034.2
            060100     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1034.2
            060200     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1034.2
            060300     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1034.2
            060400     GO TO   SIMPLE-PERFORM-WRITE.                                DB1034.2
            060500 SIMPLE-PERFORM-DELETE.                                           DB1034.2
            060600     PERFORM DE-LETE.                                             DB1034.2
            060700 SIMPLE-PERFORM-WRITE.                                            DB1034.2
            060800     MOVE "SIMPLE-PERFORM-TEST" TO PAR-NAME.                      DB1034.2
            060900     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1034.2
            061000     PERFORM PRINT-DETAIL.                                        DB1034.2
            061100     MOVE 0 TO RESULT-FLAG.                                       DB1034.2
            061200 DEBUG-LINE-TESTS-INIT.                                           DB1034.2
            061300     MOVE "DEBUG LINE TESTS" TO FEATURE.                          DB1034.2
            061400 DEBUG-LINE-TEST-01.                                              DB1034.2
            061500     MOVE "COMPLETE ENTITY" TO RE-MARK.                           DB1034.2
            061600     PERFORM PASS.                                                DB1034.2
            061700D    PERFORM FAIL.                                                DB1034.2
            061800     GO TO DEBUG-LINE-WRITE-01.                                   DB1034.2
            061900 DEBUG-LINE-DELETE-01.                                            DB1034.2
            062000     PERFORM DE-LETE.                                             DB1034.2
            062100 DEBUG-LINE-WRITE-01.                                             DB1034.2
            062200     MOVE "DEBUG-LINE-TEST-01" TO PAR-NAME.                       DB1034.2
            062300     PERFORM PRINT-DETAIL.                                        DB1034.2
            062400 DEBUG-LINE-TEST-02.                                              DB1034.2
            062500     MOVE "CONSECUTIVE DEBUG LINES" TO RE-MARK.                   DB1034.2
            062600     PERFORM PASS.                                                DB1034.2
            062700D    PERFORM FAIL.                                                DB1034.2
            062800D    SUBTRACT 1 FROM D.                                           DB1034.2
            062900     GO TO DEBUG-LINE-WRITE-02.                                   DB1034.2
            063000 DEBUG-LINE-DELETE-02.                                            DB1034.2
            063100     PERFORM DE-LETE.                                             DB1034.2
            063200 DEBUG-LINE-WRITE-02.                                             DB1034.2
            063300     MOVE "DEBUG-LINE-TEST-02" TO PAR-NAME.                       DB1034.2
            063400     PERFORM PRINT-DETAIL.                                        DB1034.2
            063500 DEBUG-LINE-TEST-03.                                              DB1034.2
            063600     MOVE "BROKEN STATEMENTS" TO RE-MARK.                         DB1034.2
            063700     PERFORM                                                      DB1034.2
            063800D        FAIL.   GO TO DEBUG-LINE-WRITE-03.                       DB1034.2
            063900DDEBUG-LINE-TEST-03A.     PERFORM                                 DB1034.2
            064000                             PASS.                                DB1034.2
            064100                             GO TO DEBUG-LINE-WRITE-03.           DB1034.2
            064200 DEBUG-LINE-DELETE-03.                                            DB1034.2
            064300     PERFORM DE-LETE.                                             DB1034.2
            064400 DEBUG-LINE-WRITE-03.                                             DB1034.2
            064500     MOVE "DEBUG-LINE-TEST-03" TO PAR-NAME.                       DB1034.2
            064600     PERFORM PRINT-DETAIL.                                        DB1034.2
            064700 DEBUG-LINE-TEST-04.                                              DB1034.2
            064800     MOVE "NESTED COMMENTS" TO RE-MARK.                           DB1034.2
            064900D    PERFORM FAIL.                                                DB1034.2
            065000*    PERFORM FAIL.  MOVE "COMMENTS EXECUTED" TO COMPUTED-A.       DB1034.2
            065100*    GO TO DEBUG-LINE-WRITE-04.                                   DB1034.2
            065200*DEBUG-LINE-TEST-04-A.                                            DB1034.2
            065300D    GO TO DEBUG-LINE-WRITE-04.                                   DB1034.2
            065400 DEBUG-LINE-TEST-04-B.                                            DB1034.2
            065500     PERFORM PASS.                                                DB1034.2
            065600     GO TO DEBUG-LINE-WRITE-04.                                   DB1034.2
            065700 DEBUG-LINE-DELETE-04.                                            DB1034.2
            065800     PERFORM DE-LETE.                                             DB1034.2
            065900 DEBUG-LINE-WRITE-04.                                             DB1034.2
            066000     MOVE "DEBUG-LINE-TEST-04" TO PAR-NAME.                       DB1034.2
            066100     PERFORM PRINT-DETAIL.                                        DB1034.2
            066200 DEBUG-LINE-TEST-05.                                              DB1034.2
            066300     MOVE "NESTED INSIDE COMMENTS" TO RE-MARK.                    DB1034.2
            066400*    PERFORM FAIL.  MOVE "COMMENTS EXECUTED" TO COMPUTED-A.       DB1034.2
            066500*    GO TO DEBUG-LINE-WRITE-05.                                   DB1034.2
            066600DDEBUG-LINE-TEST-05-A.                                            DB1034.2
            066700D    PERFORM FAIL.                                                DB1034.2
            066800D    GO TO DEBUG-LINE-WRITE-05.                                   DB1034.2
            066900*DEBUG-LINE-TEST-05-B.                                            DB1034.2
            067000*    MOVE "    FAILURE 05B" TO COMPUTED-A.                        DB1034.2
            067100*    PERFORM FAIL.  GO TO DEBUG-LINE-WRITE-05.                    DB1034.2
            067200 DEBUG-LINE-TEST-05-C.                                            DB1034.2
            067300     PERFORM PASS.                                                DB1034.2
            067400     GO TO DEBUG-LINE-WRITE-05.                                   DB1034.2
            067500 DEBUG-LINE-DELETE-05.                                            DB1034.2
            067600     PERFORM DE-LETE.                                             DB1034.2
            067700 DEBUG-LINE-WRITE-05.                                             DB1034.2
            067800     MOVE "DEBUG-LINE-TEST-05" TO PAR-NAME.                       DB1034.2
            067900     PERFORM PRINT-DETAIL.                                        DB1034.2
            068000 CCVS-EXIT SECTION.                                               DB1034.2
            068100 CCVS-999999.                                                     DB1034.2
            068200     GO TO CLOSE-FILES.                                           DB1034.2
                  *END-OF,DB103M                                                            
        """)
    )

    @Disabled("Requires C, G, S, Y, X indicators")
    @Test
    fun db1044_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB104A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB1044.2
            000200 PROGRAM-ID.                                                      DB1044.2
            000300     DB104A.                                                      DB1044.2
            000400 AUTHOR.                                                          DB1044.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB1044.2
            000600 INSTALLATION.                                                    DB1044.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB1044.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB1044.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB1044.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB1044.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB1044.2
            001200                                                                  DB1044.2
            001300     PHONE   (703) 756-6153                                       DB1044.2
            001400                                                                  DB1044.2
            001500     " HIGH       ".                                              DB1044.2
            001600 DATE-WRITTEN.                                                    DB1044.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB1044.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB1044.2
            001900     "4.2 ".                                                      DB1044.2
            002000 SECURITY.                                                        DB1044.2
            002100     NONE.                                                        DB1044.2
            002200*                                                                 DB1044.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB1044.2
            002400*                                                                 DB1044.2
            002500*                       PROGRAM ABSTRACT                          DB1044.2
            002600*                                                                 DB1044.2
            002700*    DB104A TESTS THE CAPABILITY OF THE DEBUG MODULE TO HANDLE    DB1044.2
            002800*    PROCEDURES TIED TO SORT INPUT, SORT OUTPUT, AND FILE         DB1044.2
            002900*    DECLARATIVE PROCEDURES.  THIS PROGRAM IS TO BE COMPILED AND  DB1044.2
            003000*    EXECUTED WITH BOTH COMPILE AND OBJECT TIME DEBUGGING         DB1044.2
            003100*    SWITCHES ENABLED.  THE PROGRAM FIRST BUILDS A SEQUENTIAL     DB1044.2
            003200*    FILE CONTAINING 99 EIGHTY CHARACTER RECORDS.  THIS FILE      DB1044.2
            003300*    IS THEN SORTED.                                              DB1044.2
            003400*                                                                 DB1044.2
            003500*    ALL DEBUGGING PROCEDURES SHOULD BE INCLUDED IN COMPILATION   DB1044.2
            003600*    AND GENERATE CODE.  BEFORE BEGINNING EXECUTION OF THE OBJECT DB1044.2
            003700*    PROGRAM, THE JOB CONTROL LANGUAGE NECESSARY TO ACTIVATE      DB1044.2
            003800*    THE OBJECT TIME DEBUGGING SWITCH MUST BE SUBMITTED.          DB1044.2
            003900*                                                                 DB1044.2
            004000*    EXECUTION OF THE PROGRAM"S SORT SHOULD TRIGGER DEBUGGING     DB1044.2
            004100*    PROCEDURES AT THE BEGINNING OF THE SORT INPUT AND SORT       DB1044.2
            004200*    OUTPUT PROCEDURES.  DURING EXECUTION OF THE SORT INPUT       DB1044.2
            004300*    PROCEDURE, END-OF-FILE CONDITION ON THE INPUT FILE SHOULD    DB1044.2
            004400*    TRIGGER A DECLARATIVE PROCEDURE ASSOCIATED WITH THE FILE,    DB1044.2
            004500*    AND THIS IN TURN SHOULD CAUSE EXECUTION OF A DEBUGGING       DB1044.2
            004600*    PROCEDURE MONITORING THE FILE DECLARATIVE PROCEDURE.         DB1044.2
            004700*                                                                 DB1044.2
            004800*    THE PERFORMANCE OF THE SORT VERB IS NOT CHECKED IN DB104.    DB1044.2
            004900*                                                                 DB1044.2
            005000*                                                                 DB1044.2
            005100*                                                                 DB1044.2
            005200 ENVIRONMENT DIVISION.                                            DB1044.2
            005300 CONFIGURATION SECTION.                                           DB1044.2
            005400 SOURCE-COMPUTER.                                                 DB1044.2
            005500     XXXXX082                                                     DB1044.2
            005600         WITH DEBUGGING MODE.                                     DB1044.2
            005700 OBJECT-COMPUTER.                                                 DB1044.2
            005800     XXXXX083.                                                    DB1044.2
            005900 INPUT-OUTPUT SECTION.                                            DB1044.2
            006000 FILE-CONTROL.                                                    DB1044.2
            006100     SELECT PRINT-FILE ASSIGN TO                                  DB1044.2
            006200     XXXXX055.                                                    DB1044.2
            006300     SELECT GEN-FILE ASSIGN TO                                    DB1044.2
            006400     XXXXX014                                                     DB1044.2
            006500     FILE STATUS IS GEN-STATUS.                                   DB1044.2
            006600*      XXXXX014  REPLACE WITH SEQUENTIAL ACCESS SCRATCH FILE NAME DB1044.2
            006700     SELECT SORT-FILE ASSIGN TO                                   DB1044.2
            006800     XXXXX027.                                                    DB1044.2
            006900*      XXXXX27  REPLACE WITH SORT FILE NAME                       DB1044.2
            007000 DATA DIVISION.                                                   DB1044.2
            007100 FILE SECTION.                                                    DB1044.2
            007200 FD  PRINT-FILE                                                   DB1044.2
            007300     LABEL RECORDS                                                DB1044.2
            007400     XXXXX084                                                     DB1044.2
            007500     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB1044.2
            007600 01  PRINT-REC PICTURE X(120).                                    DB1044.2
            007700 01  DUMMY-RECORD PICTURE X(120).                                 DB1044.2
            007800 FD  GEN-FILE                                                     DB1044.2
            007900C    VALUE OF                                                     DB1044.2
            008000C    XXXXX074                                                     DB1044.2
            008100*      XXXXX074  REPLACE WITH IMPLEMENTOR NAME (*OPT C ONLY)      DB1044.2
            008200C    IS                                                           DB1044.2
            008300C    XXXXX075                                                     DB1044.2
            008400*      XXXXX075  REPLACE WITH VALUE CLAUSE OBJECT (*OPT C ONLY)   DB1044.2
            008500G    XXXXX069                                                     DB1044.2
            008600*      XXXXX069  REPLACE WITH ADDITIONAL INFO (*OPT G ONLY)       DB1044.2
            008700     LABEL RECORD IS STANDARD.                                    DB1044.2
            008800 01  GEN-REC PIC X(80).                                           DB1044.2
            008900 SD  SORT-FILE.                                                   DB1044.2
            009000 01  SORT-REC.                                                    DB1044.2
            009100     02  FILLER PIC X(34).                                        DB1044.2
            009200     02  SORT-REC-NO PIC 9(6).                                    DB1044.2
            009300     02  FILLER PIC X(40).                                        DB1044.2
            009400 WORKING-STORAGE SECTION.                                         DB1044.2
            009500 77  RESULT-FLAG PIC 99 COMP VALUE 0.                             DB1044.2
            009600 77  DBLINE-HOLD PIC X(6).                                        DB1044.2
            009700 77  DBNAME-HOLD PIC X(30).                                       DB1044.2
            009800 77  DBCONT-HOLD PIC X(30).                                       DB1044.2
            009900 01  FILE-RECORD-INFORMATION-REC.                                 DB1044.2
            010000     03 FILE-RECORD-INFO-SKELETON.                                DB1044.2
            010100        05 FILLER                 PICTURE X(48)       VALUE       DB1044.2
            010200             "FILE=      ,RECORD=      /0,RECNO=000000,UPDT=00".  DB1044.2
            010300        05 FILLER                 PICTURE X(46)       VALUE       DB1044.2
            010400             ",ODO=0000,PGM=     ,LRECL=000000,BLKSIZ  =0000".    DB1044.2
            010500        05 FILLER                 PICTURE X(26)       VALUE       DB1044.2
            010600             ",LFIL=000000,ORG=  ,LBLR= ".                        DB1044.2
            010700        05 FILLER                 PICTURE X(37)       VALUE       DB1044.2
            010800             ",RECKEY=                             ".             DB1044.2
            010900        05 FILLER                 PICTURE X(38)       VALUE       DB1044.2
            011000             ",ALTKEY1=                             ".            DB1044.2
            011100        05 FILLER                 PICTURE X(38)       VALUE       DB1044.2
            011200             ",ALTKEY2=                             ".            DB1044.2
            011300        05 FILLER                 PICTURE X(7)        VALUE SPACE.DB1044.2
            011400     03 FILE-RECORD-INFO          OCCURS  10  TIMES.              DB1044.2
            011500        05 FILE-RECORD-INFO-P1-120.                               DB1044.2
            011600           07 FILLER              PIC X(5).                       DB1044.2
            011700           07 XFILE-NAME           PIC X(6).                      DB1044.2
            011800           07 FILLER              PIC X(8).                       DB1044.2
            011900           07 XRECORD-NAME         PIC X(6).                      DB1044.2
            012000           07 FILLER              PIC X(1).                       DB1044.2
            012100           07 REELUNIT-NUMBER     PIC 9(1).                       DB1044.2
            012200           07 FILLER              PIC X(7).                       DB1044.2
            012300           07 XRECORD-NUMBER       PIC 9(6).                      DB1044.2
            012400           07 FILLER              PIC X(6).                       DB1044.2
            012500           07 UPDATE-NUMBER       PIC 9(2).                       DB1044.2
            012600           07 FILLER              PIC X(5).                       DB1044.2
            012700           07 ODO-NUMBER          PIC 9(4).                       DB1044.2
            012800           07 FILLER              PIC X(5).                       DB1044.2
            012900           07 XPROGRAM-NAME        PIC X(5).                      DB1044.2
            013000           07 FILLER              PIC X(7).                       DB1044.2
            013100           07 XRECORD-LENGTH       PIC 9(6).                      DB1044.2
            013200           07 FILLER              PIC X(7).                       DB1044.2
            013300           07 CHARS-OR-RECORDS    PIC X(2).                       DB1044.2
            013400           07 FILLER              PIC X(1).                       DB1044.2
            013500           07 XBLOCK-SIZE          PIC 9(4).                      DB1044.2
            013600           07 FILLER              PIC X(6).                       DB1044.2
            013700           07 RECORDS-IN-FILE     PIC 9(6).                       DB1044.2
            013800           07 FILLER              PIC X(5).                       DB1044.2
            013900           07 XFILE-ORGANIZATION   PIC X(2).                      DB1044.2
            014000           07 FILLER              PIC X(6).                       DB1044.2
            014100           07 XLABEL-TYPE          PIC X(1).                      DB1044.2
            014200        05 FILE-RECORD-INFO-P121-240.                             DB1044.2
            014300           07 FILLER              PIC X(8).                       DB1044.2
            014400           07 XRECORD-KEY          PIC X(29).                     DB1044.2
            014500           07 FILLER              PIC X(9).                       DB1044.2
            014600           07 ALTERNATE-KEY1      PIC X(29).                      DB1044.2
            014700           07 FILLER              PIC X(9).                       DB1044.2
            014800           07 ALTERNATE-KEY2      PIC X(29).                      DB1044.2
            014900           07 FILLER              PIC X(7).                       DB1044.2
            015000 01  GEN-STATUS.                                                  DB1044.2
            015100     02  END-FLAG PIC X.                                          DB1044.2
            015200     02  FILLER PIC X.                                            DB1044.2
            015300 01  SIZE-13.                                                     DB1044.2
            015400     02  FILLER PIC XX.                                           DB1044.2
            015500     02  SIZE-11.                                                 DB1044.2
            015600         03  FILLER PIC X.                                        DB1044.2
            015700         03  SIZE-10.                                             DB1044.2
            015800             04  FILLER PIC XX.                                   DB1044.2
            015900             04  SIZE-8.                                          DB1044.2
            016000                 05  FILLER PIC X.                                DB1044.2
            016100                 05  SIZE-7 PIC X(7).                             DB1044.2
            016200 01  TEST-RESULTS.                                                DB1044.2
            016300     02 FILLER                    PICTURE X VALUE SPACE.          DB1044.2
            016400     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB1044.2
            016500     02 FILLER                    PICTURE X VALUE SPACE.          DB1044.2
            016600     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB1044.2
            016700     02 FILLER                    PICTURE X  VALUE SPACE.         DB1044.2
            016800     02  PAR-NAME.                                                DB1044.2
            016900       03 FILLER PICTURE X(12) VALUE SPACE.                       DB1044.2
            017000       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB1044.2
            017100       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB1044.2
            017200       03 FILLER PIC X(5) VALUE SPACE.                            DB1044.2
            017300     02 FILLER PIC X(10) VALUE SPACE.                             DB1044.2
            017400     02 RE-MARK PIC X(61).                                        DB1044.2
            017500 01  TEST-COMPUTED.                                               DB1044.2
            017600     02 FILLER PIC X(30) VALUE SPACE.                             DB1044.2
            017700     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB1044.2
            017800     02 COMPUTED-X.                                               DB1044.2
            017900     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB1044.2
            018000     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB1044.2
            018100     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB1044.2
            018200     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB1044.2
            018300     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB1044.2
            018400     03       CM-18V0 REDEFINES COMPUTED-A.                       DB1044.2
            018500         04 COMPUTED-18V0                   PICTURE -9(18).       DB1044.2
            018600         04 FILLER                          PICTURE X.            DB1044.2
            018700     03 FILLER PIC X(50) VALUE SPACE.                             DB1044.2
            018800 01  TEST-CORRECT.                                                DB1044.2
            018900     02 FILLER PIC X(30) VALUE SPACE.                             DB1044.2
            019000     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB1044.2
            019100     02 CORRECT-X.                                                DB1044.2
            019200     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB1044.2
            019300     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB1044.2
            019400     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB1044.2
            019500     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB1044.2
            019600     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB1044.2
            019700     03      CR-18V0 REDEFINES CORRECT-A.                         DB1044.2
            019800         04 CORRECT-18V0                    PICTURE -9(18).       DB1044.2
            019900         04 FILLER                          PICTURE X.            DB1044.2
            020000     03 FILLER PIC X(50) VALUE SPACE.                             DB1044.2
            020100 01  CCVS-C-1.                                                    DB1044.2
            020200     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB1044.2
            020300-    "SS  PARAGRAPH-NAME                                          DB1044.2
            020400-    "        REMARKS".                                           DB1044.2
            020500     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB1044.2
            020600 01  CCVS-C-2.                                                    DB1044.2
            020700     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1044.2
            020800     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB1044.2
            020900     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB1044.2
            021000     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB1044.2
            021100     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB1044.2
            021200 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB1044.2
            021300 01  REC-CT PICTURE 99 VALUE ZERO.                                DB1044.2
            021400 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB1044.2
            021500 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB1044.2
            021600 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB1044.2
            021700 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB1044.2
            021800 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB1044.2
            021900 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB1044.2
            022000 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB1044.2
            022100 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB1044.2
            022200 01  CCVS-H-1.                                                    DB1044.2
            022300     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB1044.2
            022400     02 FILLER PICTURE X(67) VALUE                                DB1044.2
            022500     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB1044.2
            022600-    " SYSTEM".                                                   DB1044.2
            022700     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB1044.2
            022800 01  CCVS-H-2.                                                    DB1044.2
            022900     02 FILLER PICTURE X(52) VALUE IS                             DB1044.2
            023000     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB1044.2
            023100     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB1044.2
            023200     02 TEST-ID PICTURE IS X(9).                                  DB1044.2
            023300     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB1044.2
            023400 01  CCVS-H-3.                                                    DB1044.2
            023500     02  FILLER PICTURE X(34) VALUE                               DB1044.2
            023600     " FOR OFFICIAL USE ONLY    ".                                DB1044.2
            023700     02  FILLER PICTURE X(58) VALUE                               DB1044.2
            023800     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB1044.2
            023900     02  FILLER PICTURE X(28) VALUE                               DB1044.2
            024000     "  COPYRIGHT   1974 ".                                       DB1044.2
            024100 01  CCVS-E-1.                                                    DB1044.2
            024200     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB1044.2
            024300     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB1044.2
            024400     02 ID-AGAIN PICTURE IS X(9).                                 DB1044.2
            024500     02 FILLER PICTURE X(45) VALUE IS                             DB1044.2
            024600     " NTIS DISTRIBUTION COBOL 74".                               DB1044.2
            024700 01  CCVS-E-2.                                                    DB1044.2
            024800     02  FILLER                   PICTURE X(31)  VALUE            DB1044.2
            024900     SPACE.                                                       DB1044.2
            025000     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB1044.2
            025100     02 CCVS-E-2-2.                                               DB1044.2
            025200         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB1044.2
            025300         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB1044.2
            025400         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB1044.2
            025500 01  CCVS-E-3.                                                    DB1044.2
            025600     02  FILLER PICTURE X(22) VALUE                               DB1044.2
            025700     " FOR OFFICIAL USE ONLY".                                    DB1044.2
            025800     02  FILLER PICTURE X(12) VALUE SPACE.                        DB1044.2
            025900     02  FILLER PICTURE X(58) VALUE                               DB1044.2
            026000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB1044.2
            026100     02  FILLER PICTURE X(13) VALUE SPACE.                        DB1044.2
            026200     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB1044.2
            026300 01  CCVS-E-4.                                                    DB1044.2
            026400     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB1044.2
            026500     02 FILLER PIC XXXX VALUE " OF ".                             DB1044.2
            026600     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB1044.2
            026700     02 FILLER PIC X(40) VALUE                                    DB1044.2
            026800      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB1044.2
            026900 01  XXINFO.                                                      DB1044.2
            027000     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB1044.2
            027100     02 INFO-TEXT.                                                DB1044.2
            027200     04 FILLER PIC X(20) VALUE SPACE.                             DB1044.2
            027300     04 XXCOMPUTED PIC X(20).                                     DB1044.2
            027400     04 FILLER PIC X(5) VALUE SPACE.                              DB1044.2
            027500     04 XXCORRECT PIC X(20).                                      DB1044.2
            027600 01  HYPHEN-LINE.                                                 DB1044.2
            027700     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1044.2
            027800     02 FILLER PICTURE IS X(65) VALUE IS "************************DB1044.2
            027900-    "*****************************************".                 DB1044.2
            028000     02 FILLER PICTURE IS X(54) VALUE IS "************************DB1044.2
            028100-    "******************************".                            DB1044.2
            028200 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB1044.2
            028300     "DB104A".                                                    DB1044.2
            028400 PROCEDURE DIVISION.                                              DB1044.2
            028500 DECLARATIVES.                                                    DB1044.2
            028600 SORT-IN-PROC SECTION.                                            DB1044.2
            028700     USE FOR DEBUGGING ON SORT-IN.                                DB1044.2
            028800 BEGIN-SORT-IN-PROC.                                              DB1044.2
            028900     MOVE 1 TO RESULT-FLAG.                                       DB1044.2
            029000 DB-COMMON.                                                       DB1044.2
            029100     MOVE DEBUG-LINE TO DBLINE-HOLD.                              DB1044.2
            029200     MOVE DEBUG-NAME TO DBNAME-HOLD.                              DB1044.2
            029300     MOVE DEBUG-CONTENTS TO DBCONT-HOLD.                          DB1044.2
            029400 SORT-OUT-PROC SECTION.                                           DB1044.2
            029500     USE FOR DEBUGGING ON SORT-OUT.                               DB1044.2
            029600 BEGIN-SORT-OUT-PROC.                                             DB1044.2
            029700     MOVE 2 TO RESULT-FLAG.                                       DB1044.2
            029800     PERFORM DB-COMMON.                                           DB1044.2
            029900 USE-PROC SECTION.                                                DB1044.2
            030000     USE FOR DEBUGGING ON AT-END-PROC.                            DB1044.2
            030100 BEGIN-USE-PROC.                                                  DB1044.2
            030200     ADD 3 TO RESULT-FLAG.                                        DB1044.2
            030300     PERFORM DB-COMMON.                                           DB1044.2
            030400 AT-END-PROC SECTION.                                             DB1044.2
            030500     USE AFTER ERROR PROCEDURE ON GEN-FILE.                       DB1044.2
            030600 BEGIN-AT-END-PROC.                                               DB1044.2
            030700     ADD 4 TO RESULT-FLAG.                                        DB1044.2
            030800 END DECLARATIVES.                                                DB1044.2
            030900 CCVS1 SECTION.                                                   DB1044.2
            031000 OPEN-FILES.                                                      DB1044.2
            031100     OPEN     OUTPUT PRINT-FILE.                                  DB1044.2
            031200     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB1044.2
            031300     MOVE    SPACE TO TEST-RESULTS.                               DB1044.2
            031400     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB1044.2
            031500     GO TO CCVS1-EXIT.                                            DB1044.2
            031600 CLOSE-FILES.                                                     DB1044.2
            031700     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB1044.2
            031800 TERMINATE-CCVS.                                                  DB1044.2
            031900S    EXIT PROGRAM.                                                DB1044.2
            032000STERMINATE-CALL.                                                  DB1044.2
            032100     STOP     RUN.                                                DB1044.2
            032200 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB1044.2
            032300 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB1044.2
            032400 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB1044.2
            032500 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB1044.2
            032600     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1044.2
            032700 PRINT-DETAIL.                                                    DB1044.2
            032800     IF REC-CT NOT EQUAL TO ZERO                                  DB1044.2
            032900             MOVE "." TO PARDOT-X                                 DB1044.2
            033000             MOVE REC-CT TO DOTVALUE.                             DB1044.2
            033100     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB1044.2
            033200     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB1044.2
            033300        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB1044.2
            033400          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB1044.2
            033500     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB1044.2
            033600     MOVE SPACE TO CORRECT-X.                                     DB1044.2
            033700     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1044.2
            033800     MOVE     SPACE TO RE-MARK.                                   DB1044.2
            033900 HEAD-ROUTINE.                                                    DB1044.2
            034000     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1044.2
            034100     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB1044.2
            034200     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB1044.2
            034300 COLUMN-NAMES-ROUTINE.                                            DB1044.2
            034400     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1044.2
            034500     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1044.2
            034600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB1044.2
            034700 END-ROUTINE.                                                     DB1044.2
            034800     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB1044.2
            034900 END-RTN-EXIT.                                                    DB1044.2
            035000     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1044.2
            035100 END-ROUTINE-1.                                                   DB1044.2
            035200      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB1044.2
            035300      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB1044.2
            035400      ADD PASS-COUNTER TO ERROR-HOLD.                             DB1044.2
            035500*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB1044.2
            035600      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB1044.2
            035700      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB1044.2
            035800      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB1044.2
            035900      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB1044.2
            036000  END-ROUTINE-12.                                                 DB1044.2
            036100      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB1044.2
            036200     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB1044.2
            036300         MOVE "NO " TO ERROR-TOTAL                                DB1044.2
            036400         ELSE                                                     DB1044.2
            036500         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB1044.2
            036600     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB1044.2
            036700     PERFORM WRITE-LINE.                                          DB1044.2
            036800 END-ROUTINE-13.                                                  DB1044.2
            036900     IF DELETE-CNT IS EQUAL TO ZERO                               DB1044.2
            037000         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB1044.2
            037100         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB1044.2
            037200     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB1044.2
            037300     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1044.2
            037400      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB1044.2
            037500          MOVE "NO " TO ERROR-TOTAL                               DB1044.2
            037600      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB1044.2
            037700      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB1044.2
            037800      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB1044.2
            037900     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1044.2
            038000 WRITE-LINE.                                                      DB1044.2
            038100     ADD 1 TO RECORD-COUNT.                                       DB1044.2
            038200Y    IF RECORD-COUNT GREATER 50                                   DB1044.2
            038300Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1044.2
            038400Y        MOVE SPACE TO DUMMY-RECORD                               DB1044.2
            038500Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1044.2
            038600Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB1044.2
            038700Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB1044.2
            038800Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB1044.2
            038900Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1044.2
            039000Y        MOVE ZERO TO RECORD-COUNT.                               DB1044.2
            039100     PERFORM WRT-LN.                                              DB1044.2
            039200 WRT-LN.                                                          DB1044.2
            039300     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1044.2
            039400     MOVE SPACE TO DUMMY-RECORD.                                  DB1044.2
            039500 BLANK-LINE-PRINT.                                                DB1044.2
            039600     PERFORM WRT-LN.                                              DB1044.2
            039700 FAIL-ROUTINE.                                                    DB1044.2
            039800     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB1044.2
            039900     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB1044.2
            040000     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB1044.2
            040100     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1044.2
            040200     GO TO FAIL-ROUTINE-EX.                                       DB1044.2
            040300 FAIL-ROUTINE-WRITE.                                              DB1044.2
            040400     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB1044.2
            040500     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB1044.2
            040600 FAIL-ROUTINE-EX. EXIT.                                           DB1044.2
            040700 BAIL-OUT.                                                        DB1044.2
            040800     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB1044.2
            040900     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB1044.2
            041000 BAIL-OUT-WRITE.                                                  DB1044.2
            041100     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB1044.2
            041200     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1044.2
            041300 BAIL-OUT-EX. EXIT.                                               DB1044.2
            041400 CCVS1-EXIT.                                                      DB1044.2
            041500     EXIT.                                                        DB1044.2
            041600 BEGIN-FILE-GENERATION.                                           DB1044.2
            041700     MOVE FILE-RECORD-INFO-SKELETON TO FILE-RECORD-INFO (1).      DB1044.2
            041800     MOVE "GEN-FI" TO XFILE-NAME (1).                             DB1044.2
            041900     MOVE "GEN-RE" TO XRECORD-NAME (1).                           DB1044.2
            042000     MOVE "DB104A" TO XPROGRAM-NAME (1).                          DB1044.2
            042100     MOVE 80 TO XRECORD-LENGTH (1).                               DB1044.2
            042200     OPEN OUTPUT GEN-FILE.                                        DB1044.2
            042300     MOVE 99 TO XRECORD-NUMBER (1).                               DB1044.2
            042400 GEN-LOOP.                                                        DB1044.2
            042500     MOVE FILE-RECORD-INFO (1) TO GEN-REC.                        DB1044.2
            042600     WRITE GEN-REC.                                               DB1044.2
            042700     IF XRECORD-NUMBER (1) IS GREATER THAN 5                      DB1044.2
            042800         SUBTRACT 5 FROM XRECORD-NUMBER (1)                       DB1044.2
            042900         GO TO GEN-LOOP.                                          DB1044.2
            043000 END-OF-GEN-LOOP.                                                 DB1044.2
            043100     MOVE 98 TO XRECORD-NUMBER (1).                               DB1044.2
            043200     PERFORM GEN-LOOP.                                            DB1044.2
            043300     MOVE 97 TO XRECORD-NUMBER (1).                               DB1044.2
            043400     PERFORM GEN-LOOP.                                            DB1044.2
            043500     MOVE 96 TO XRECORD-NUMBER (1).                               DB1044.2
            043600     PERFORM GEN-LOOP.                                            DB1044.2
            043700     MOVE 95 TO XRECORD-NUMBER (1).                               DB1044.2
            043800     PERFORM GEN-LOOP.                                            DB1044.2
            043900     CLOSE GEN-FILE.                                              DB1044.2
            044000******************************************************************DB1044.2
            044100*    THE DEBUG-LINE (INSPT) SUBTESTS FOR THE TESTS NAMED IN THE  *DB1044.2
            044200*    OUTPUT REPORT AS "SORT-IN-2" AND "SORT-OUT-2" SHOULD POINT  *DB1044.2
            044300*    TO THE "SORT" STATEMENT WHICH APPEARS IN THE PARAGRAPH      *DB1044.2
            044400*    BELOW NAMED "BEGIN-TESTS".                                  *DB1044.2
            044500******************************************************************DB1044.2
            044600 BEGIN-TESTS.                                                     DB1044.2
            044700     MOVE 0 TO RESULT-FLAG.                                       DB1044.2
            044800     SORT SORT-FILE ON ASCENDING KEY SORT-REC-NO                  DB1044.2
            044900         INPUT PROCEDURE IS SORT-IN                               DB1044.2
            045000         OUTPUT PROCEDURE IS SORT-OUT.                            DB1044.2
            045100     GO TO AFTER-SORT.                                            DB1044.2
            045200 SORT-IN SECTION.                                                 DB1044.2
            045300 SORT-IN-1.                                                       DB1044.2
            045400     MOVE "SORT-IN-1" TO PAR-NAME.                                DB1044.2
            045500     IF RESULT-FLAG IS NOT EQUAL TO 1                             DB1044.2
            045600         MOVE "DEBUG PROCEDURE NOT EXECUTED" TO RE-MARK           DB1044.2
            045700         PERFORM FAIL-1                                           DB1044.2
            045800         PERFORM SORT-IN-WRITE                                    DB1044.2
            045900         GO TO SORT-IN-5                                          DB1044.2
            046000         ELSE  PERFORM PASS-1                                     DB1044.2
            046100         MOVE "DEBUG PROCEDURE EXECUTED" TO RE-MARK.              DB1044.2
            046200     PERFORM SORT-IN-WRITE.                                       DB1044.2
            046300     GO TO SORT-IN-2.                                             DB1044.2
            046400 SORT-IN-DELETE.                                                  DB1044.2
            046500     MOVE "SORT-IN" TO PAR-NAME.                                  DB1044.2
            046600     PERFORM DE-LETE-1.                                           DB1044.2
            046700     PERFORM SORT-IN-WRITE.                                       DB1044.2
            046800     GO TO SORT-IN-5.                                             DB1044.2
            046900 SORT-IN-WRITE.                                                   DB1044.2
            047000     MOVE "DEBUG SORT INPUT" TO FEATURE.                          DB1044.2
            047100     PERFORM PRINT-DETAIL-1.                                      DB1044.2
            047200 SORT-IN-2.                                                       DB1044.2
            047300     MOVE "SORT-IN-2" TO PAR-NAME.                                DB1044.2
            047400     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1044.2
            047500     MOVE "DEBUG-LINE, SEE NEXT LINE" TO RE-MARK.                 DB1044.2
            047600     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1044.2
            047700     PERFORM   INSPT-1.                                           DB1044.2
            047800     PERFORM SORT-IN-WRITE.                                       DB1044.2
            047900 SORT-IN-3.                                                       DB1044.2
            048000     MOVE DBNAME-HOLD TO SIZE-7.                                  DB1044.2
            048100     IF SIZE-7 IS EQUAL TO "SORT-IN"                              DB1044.2
            048200         PERFORM PASS-1 ELSE                                      DB1044.2
            048300         MOVE "SORT-IN" TO CORRECT-A                              DB1044.2
            048400         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1044.2
            048500         PERFORM FAIL-1.                                          DB1044.2
            048600     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1044.2
            048700     MOVE "SORT-IN-3" TO PAR-NAME.                                DB1044.2
            048800     PERFORM SORT-IN-WRITE.                                       DB1044.2
            048900 SORT-IN-4.                                                       DB1044.2
            049000     MOVE DBCONT-HOLD TO SIZE-10.                                 DB1044.2
            049100     IF SIZE-10 IS EQUAL TO "SORT INPUT"                          DB1044.2
            049200         PERFORM PASS-1 ELSE                                      DB1044.2
            049300         MOVE "SORT INPUT" TO CORRECT-A                           DB1044.2
            049400         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1044.2
            049500         PERFORM FAIL-1.                                          DB1044.2
            049600     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1044.2
            049700     MOVE "SORT-IN-4" TO PAR-NAME.                                DB1044.2
            049800     PERFORM SORT-IN-WRITE.                                       DB1044.2
            049900 SORT-IN-5.                                                       DB1044.2
            050000     OPEN INPUT GEN-FILE.                                         DB1044.2
            050100     MOVE 0 TO RESULT-FLAG.                                       DB1044.2
            050200******************************************************************DB1044.2
            050300*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1044.2
            050400*    OUTPUT REPORT AS "SORT-USE-TEST" SHOULD POINT TO THE        *DB1044.2
            050500*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1044.2
            050600*    WHICH READS, "READ GEN-FILE".                               *DB1044.2
            050700******************************************************************DB1044.2
            050800 SORT-USE-TEST.                                                   DB1044.2
            050900     READ GEN-FILE                                                DB1044.2
            051000*        AT END GO TO SORT-USE-DELETE.                            DB1044.2
            051100*                                                                 DB1044.2
            051200*    IN CASE IMPLEMENTATION FAILS TO NOTIFY PROGRAM OF            DB1044.2
            051300*    END-OF-FILE VIA STATUS OR DECLARATIVE PROC, REMOVE ASTERISK  DB1044.2
            051400*    FROM THE FIRST OF THESE COMMENT LINES AND PERMIT THE AT END  DB1044.2
            051500*    CLAUSE TO BE COMPILED; THIS WILL RESULT IN TEST DELETION.    DB1044.2
            051600*                                                                 DB1044.2
            051700     IF RESULT-FLAG IS EQUAL TO 3                                 DB1044.2
            051800         CLOSE GEN-FILE GO TO SORT-USE-1.                         DB1044.2
            051900     IF RESULT-FLAG IS EQUAL TO 4                                 DB1044.2
            052000         CLOSE GEN-FILE GO TO SORT-USE-3.                         DB1044.2
            052100     IF RESULT-FLAG IS EQUAL TO 7                                 DB1044.2
            052200         CLOSE GEN-FILE                                           DB1044.2
            052300         PERFORM PASS-1                                           DB1044.2
            052400         MOVE "BOTH PROCEDURES EXECUTED" TO RE-MARK               DB1044.2
            052500         PERFORM SORT-USE-WRITE                                   DB1044.2
            052600         GO TO SORT-USE-2.                                        DB1044.2
            052700     IF END-FLAG IS EQUAL TO "1"                                  DB1044.2
            052800         CLOSE GEN-FILE GO TO SORT-USE-4.                         DB1044.2
            052900     RELEASE SORT-REC FROM GEN-REC.                               DB1044.2
            053000     GO TO SORT-USE-TEST.                                         DB1044.2
            053100 SORT-USE-DELETE.                                                 DB1044.2
            053200     CLOSE GEN-FILE.                                              DB1044.2
            053300     PERFORM DE-LETE-1.                                           DB1044.2
            053400     GO TO SORT-USE-WRITE.                                        DB1044.2
            053500 SORT-USE-1.                                                      DB1044.2
            053600     MOVE "ERROR PROCEDURE NOT COMPLETED" TO RE-MARK.             DB1044.2
            053700     PERFORM SORT-USE-WRITE.                                      DB1044.2
            053800 SORT-USE-2.                                                      DB1044.2
            053900     MOVE "DEBUG-LINE, SEE NEXT LINE" TO RE-MARK.                 DB1044.2
            054000     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1044.2
            054100     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1044.2
            054200     PERFORM   INSPT-1.                                           DB1044.2
            054300     PERFORM SORT-USE-WRITE.                                      DB1044.2
            054400     MOVE DBNAME-HOLD TO SIZE-11.                                 DB1044.2
            054500     IF SIZE-11 IS EQUAL TO "AT-END-PROC"                         DB1044.2
            054600         PERFORM PASS-1 ELSE                                      DB1044.2
            054700         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1044.2
            054800         MOVE "AT-END-PROC" TO CORRECT-A                          DB1044.2
            054900         PERFORM FAIL-1.                                          DB1044.2
            055000     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1044.2
            055100     PERFORM SORT-USE-WRITE.                                      DB1044.2
            055200     MOVE DBCONT-HOLD TO SIZE-13.                                 DB1044.2
            055300     IF SIZE-13 IS EQUAL TO "USE PROCEDURE"                       DB1044.2
            055400         PERFORM PASS-1 ELSE                                      DB1044.2
            055500         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1044.2
            055600         MOVE "USE PROCEDURE" TO CORRECT-A                        DB1044.2
            055700         PERFORM FAIL-1.                                          DB1044.2
            055800     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1044.2
            055900     GO TO SORT-USE-WRITE.                                        DB1044.2
            056000 SORT-USE-3.                                                      DB1044.2
            056100     MOVE "DEBUG ON USE PROC NOT EXECUTED" TO RE-MARK.            DB1044.2
            056200     PERFORM FAIL-1.                                              DB1044.2
            056300     GO TO SORT-USE-WRITE.                                        DB1044.2
            056400 SORT-USE-4.                                                      DB1044.2
            056500     MOVE "DEBUG AND USE PROCS BOTH FAIL" TO RE-MARK.             DB1044.2
            056600     PERFORM FAIL-1.                                              DB1044.2
            056700 SORT-USE-WRITE.                                                  DB1044.2
            056800     MOVE "SORT-USE-TEST" TO PAR-NAME.                            DB1044.2
            056900     MOVE "DEBUG USE PROC" TO FEATURE.                            DB1044.2
            057000     PERFORM PRINT-DETAIL-1.                                      DB1044.2
            057100 SORT-USE-DONE.                                                   DB1044.2
            057200     GO TO SORT-IN-EXIT.                                          DB1044.2
            057300 INSPT-1. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.       DB1044.2
            057400 PASS-1.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.         DB1044.2
            057500 FAIL-1.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.        DB1044.2
            057600 DE-LETE-1.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.        DB1044.2
            057700     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1044.2
            057800 PRINT-DETAIL-1.                                                  DB1044.2
            057900     IF REC-CT NOT EQUAL TO ZERO                                  DB1044.2
            058000             MOVE "." TO PARDOT-X                                 DB1044.2
            058100             MOVE REC-CT TO DOTVALUE.                             DB1044.2
            058200     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE-1.    DB1044.2
            058300     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE-1             DB1044.2
            058400        PERFORM FAIL-ROUTINE-1 THRU FAIL-ROUTINE-EX-1             DB1044.2
            058500          ELSE PERFORM BAIL-OUT-1 THRU BAIL-OUT-EX-1.             DB1044.2
            058600     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB1044.2
            058700     MOVE SPACE TO CORRECT-X.                                     DB1044.2
            058800     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1044.2
            058900     MOVE     SPACE TO RE-MARK.                                   DB1044.2
            059000 WRITE-LINE-1.                                                    DB1044.2
            059100     ADD 1 TO RECORD-COUNT.                                       DB1044.2
            059200Y    IF RECORD-COUNT GREATER 50                                   DB1044.2
            059300Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1044.2
            059400Y        MOVE SPACE TO DUMMY-RECORD                               DB1044.2
            059500Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1044.2
            059600Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN-1           DB1044.2
            059700Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN-1 2 TIMES   DB1044.2
            059800Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN-1        DB1044.2
            059900Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1044.2
            060000Y        MOVE ZERO TO RECORD-COUNT.                               DB1044.2
            060100     PERFORM WRT-LN-1.                                            DB1044.2
            060200 WRT-LN-1.                                                        DB1044.2
            060300     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1044.2
            060400     MOVE SPACE TO DUMMY-RECORD.                                  DB1044.2
            060500 BLANK-LINE-PRINT-1.                                              DB1044.2
            060600     PERFORM WRT-LN-1.                                            DB1044.2
            060700 FAIL-ROUTINE-1.                                                  DB1044.2
            060800     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-1.     DB1044.2
            060900     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-1.      DB1044.2
            061000     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB1044.2
            061100     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-1 2 TIMES.   DB1044.2
            061200     GO TO FAIL-ROUTINE-EX-1.                                     DB1044.2
            061300 FAIL-RTN-WRITE-1.                                                DB1044.2
            061400     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE-1         DB1044.2
            061500     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE-1 2 TIMES. DB1044.2
            061600 FAIL-ROUTINE-EX-1. EXIT.                                         DB1044.2
            061700 BAIL-OUT-1.                                                      DB1044.2
            061800     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE-1.     DB1044.2
            061900     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX-1.             DB1044.2
            062000 BAIL-OUT-WRITE-1.                                                DB1044.2
            062100     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB1044.2
            062200     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-1 2 TIMES.   DB1044.2
            062300 BAIL-OUT-EX-1. EXIT.                                             DB1044.2
            062400 SORT-IN-EXIT.                                                    DB1044.2
            062500     MOVE 0 TO RESULT-FLAG.                                       DB1044.2
            062600 SORT-OUT SECTION.                                                DB1044.2
            062700 SORT-OUT-1.                                                      DB1044.2
            062800     MOVE "SORT-OUT-1" TO PAR-NAME.                               DB1044.2
            062900     IF RESULT-FLAG IS NOT EQUAL TO 2                             DB1044.2
            063000         PERFORM FAIL-2                                           DB1044.2
            063100         MOVE "DEBUG PROCEDURE NOT EXECUTED" TO RE-MARK           DB1044.2
            063200         GO TO SORT-OUT-WRITE.                                    DB1044.2
            063300     PERFORM PASS-2.                                              DB1044.2
            063400     MOVE "DEBUG PROCEDURE EXECUTED" TO RE-MARK.                  DB1044.2
            063500     PERFORM SORT-OUT-WRITE.                                      DB1044.2
            063600 SORT-OUT-2.                                                      DB1044.2
            063700     MOVE "SORT-OUT-2" TO PAR-NAME.                               DB1044.2
            063800     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1044.2
            063900     MOVE "DEBUG-LINE, SEE NEXT LINE" TO RE-MARK.                 DB1044.2
            064000     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1044.2
            064100     PERFORM   INSPT-2.                                           DB1044.2
            064200     PERFORM SORT-OUT-WRITE.                                      DB1044.2
            064300 SORT-OUT-3.                                                      DB1044.2
            064400     MOVE "SORT-OUT-3" TO PAR-NAME.                               DB1044.2
            064500     MOVE DBNAME-HOLD TO SIZE-8.                                  DB1044.2
            064600     IF SIZE-8 IS EQUAL TO "SORT-OUT"                             DB1044.2
            064700         PERFORM PASS-2 ELSE                                      DB1044.2
            064800         MOVE "SORT-OUT" TO CORRECT-A                             DB1044.2
            064900         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1044.2
            065000         PERFORM FAIL-2.                                          DB1044.2
            065100     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1044.2
            065200     PERFORM SORT-OUT-WRITE.                                      DB1044.2
            065300 SORT-OUT-4.                                                      DB1044.2
            065400     MOVE "SORT-OUT-4" TO PAR-NAME.                               DB1044.2
            065500     MOVE DBCONT-HOLD TO SIZE-11.                                 DB1044.2
            065600     IF SIZE-11 IS EQUAL TO "SORT OUTPUT"                         DB1044.2
            065700         PERFORM PASS-2 ELSE                                      DB1044.2
            065800         MOVE "SORT OUTPUT" TO CORRECT-A                          DB1044.2
            065900         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1044.2
            066000     PERFORM FAIL-2.                                              DB1044.2
            066100     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1044.2
            066200     GO TO SORT-OUT-WRITE.                                        DB1044.2
            066300 SORT-OUT-DELETE.                                                 DB1044.2
            066400     MOVE "SORT-OUT" TO PAR-NAME.                                 DB1044.2
            066500     PERFORM DE-LETE-2.                                           DB1044.2
            066600 SORT-OUT-WRITE.                                                  DB1044.2
            066700     MOVE "DEBUG SORT OUTPUT" TO FEATURE.                         DB1044.2
            066800     PERFORM PRINT-DETAIL-2.                                      DB1044.2
            066900 SORT-OUT-5.                                                      DB1044.2
            067000     OPEN OUTPUT GEN-FILE.                                        DB1044.2
            067100 SORT-OUT-6.                                                      DB1044.2
            067200     RETURN SORT-FILE INTO GEN-REC                                DB1044.2
            067300         AT END GO TO SORT-OUT-EXIT.                              DB1044.2
            067400     WRITE GEN-REC.                                               DB1044.2
            067500     GO TO SORT-OUT-6.                                            DB1044.2
            067600 INSPT-2. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.       DB1044.2
            067700 PASS-2.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.         DB1044.2
            067800 FAIL-2.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.        DB1044.2
            067900 DE-LETE-2.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.        DB1044.2
            068000     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1044.2
            068100 PRINT-DETAIL-2.                                                  DB1044.2
            068200     IF REC-CT NOT EQUAL TO ZERO                                  DB1044.2
            068300             MOVE "." TO PARDOT-X                                 DB1044.2
            068400             MOVE REC-CT TO DOTVALUE.                             DB1044.2
            068500     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE-2.    DB1044.2
            068600     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE-2             DB1044.2
            068700        PERFORM FAIL-ROUTINE-2 THRU FAIL-ROUTINE-EX-2             DB1044.2
            068800          ELSE PERFORM BAIL-OUT-2 THRU BAIL-OUT-EX-2.             DB1044.2
            068900     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB1044.2
            069000     MOVE SPACE TO CORRECT-X.                                     DB1044.2
            069100     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1044.2
            069200     MOVE     SPACE TO RE-MARK.                                   DB1044.2
            069300 WRITE-LINE-2.                                                    DB1044.2
            069400     ADD 1 TO RECORD-COUNT.                                       DB1044.2
            069500Y    IF RECORD-COUNT GREATER 50                                   DB1044.2
            069600Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1044.2
            069700Y        MOVE SPACE TO DUMMY-RECORD                               DB1044.2
            069800Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1044.2
            069900Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN-2           DB1044.2
            070000Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN-2 2 TIMES   DB1044.2
            070100Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN-2        DB1044.2
            070200Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1044.2
            070300Y        MOVE ZERO TO RECORD-COUNT.                               DB1044.2
            070400     PERFORM WRT-LN-2.                                            DB1044.2
            070500 WRT-LN-2.                                                        DB1044.2
            070600     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1044.2
            070700     MOVE SPACE TO DUMMY-RECORD.                                  DB1044.2
            070800 BLANK-LINE-PRINT-2.                                              DB1044.2
            070900     PERFORM WRT-LN-2.                                            DB1044.2
            071000 FAIL-ROUTINE-2.                                                  DB1044.2
            071100     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-2.     DB1044.2
            071200     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-2.      DB1044.2
            071300     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB1044.2
            071400     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-2 2 TIMES.   DB1044.2
            071500     GO TO FAIL-ROUTINE-EX-2.                                     DB1044.2
            071600 FAIL-RTN-WRITE-2.                                                DB1044.2
            071700     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE-2         DB1044.2
            071800     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE-2 2 TIMES. DB1044.2
            071900 FAIL-ROUTINE-EX-2. EXIT.                                         DB1044.2
            072000 BAIL-OUT-2.                                                      DB1044.2
            072100     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE-2.     DB1044.2
            072200     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX-2.             DB1044.2
            072300 BAIL-OUT-WRITE-2.                                                DB1044.2
            072400     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB1044.2
            072500     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-2 2 TIMES.   DB1044.2
            072600 BAIL-OUT-EX-2. EXIT.                                             DB1044.2
            072700 SORT-OUT-EXIT.                                                   DB1044.2
            072800     CLOSE GEN-FILE.                                              DB1044.2
            072900     MOVE 0 TO RESULT-FLAG.                                       DB1044.2
            073000 END-OF-SORT SECTION.                                             DB1044.2
            073100 AFTER-SORT.                                                      DB1044.2
            073200     EXIT.                                                        DB1044.2
            073300XDUMP-CODING SECTION.                                             DB1044.2
            073400XBEGIN-DUMP.                                                      DB1044.2
            073500X    OPEN INPUT GEN-FILE.                                         DB1044.2
            073600X    PERFORM BLANK-LINE-PRINT.                                    DB1044.2
            073700X    MOVE " DUMP OF GEN-FILE FOLLOWS:" TO PRINT-REC.              DB1044.2
            073800X    PERFORM WRITE-LINE.                                          DB1044.2
            073900XDUMP-FILE-1.                                                     DB1044.2
            074000X    READ GEN-FILE AT END GO TO DUMP-FILE-2.                      DB1044.2
            074100X    MOVE GEN-REC TO PRINT-REC.                                   DB1044.2
            074200X    PERFORM WRITE-LINE.                                          DB1044.2
            074300X    GO TO DUMP-FILE-1.                                           DB1044.2
            074400XDUMP-FILE-2.                                                     DB1044.2
            074500X    CLOSE GEN-FILE.                                              DB1044.2
            074600 CCVS-EXIT SECTION.                                               DB1044.2
            074700 CCVS-999999.                                                     DB1044.2
            074800     GO TO CLOSE-FILES.                                           DB1044.2
                  *END-OF,DB104A                                                            
        """)
    )

    @Disabled("Requires Y and T indicators")
    @Test
    fun db1054_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB105A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB1054.2
            000200 PROGRAM-ID.                                                      DB1054.2
            000300     DB105A.                                                      DB1054.2
            000400 AUTHOR.                                                          DB1054.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB1054.2
            000600 INSTALLATION.                                                    DB1054.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB1054.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB1054.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB1054.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB1054.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB1054.2
            001200                                                                  DB1054.2
            001300     PHONE   (703) 756-6153                                       DB1054.2
            001400                                                                  DB1054.2
            001500     " HIGH       ".                                              DB1054.2
            001600 DATE-WRITTEN.                                                    DB1054.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB1054.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB1054.2
            001900     "4.2 ".                                                      DB1054.2
            002000 SECURITY.                                                        DB1054.2
            002100     NONE.                                                        DB1054.2
            002200*                                                                 DB1054.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB1054.2
            002400*                                                                 DB1054.2
            002500*                       PROGRAM ABSTRACT                          DB1054.2
            002600*                                                                 DB1054.2
            002700*    DB105A TESTS THE CAPABILITY OF THE DEBUG MODULE TO MONITOR   DB1054.2
            002800*    ALL PROCEDURES WITH A SINGLE DEBUGGING DECLARATIVE.  THIS    DB1054.2
            002900*    PROGRAM IS TO BE COMPILED AND EXECUTED WITH BOTH COMPILE     DB1054.2
            003000*    AND OBJECT TIME DEBUGGING SWITCHES ON.  THE DEBUGGING        DB1054.2
            003100*    PROCEDURE SHOULD BE INCLUDED IN THE COMPILATION AND          DB1054.2
            003200*    GENERATE CODE.  DURING EXECUTION, EACH PROCEDURE SHOULD      DB1054.2
            003300*    TRIGGER THE DEBUGGING PROCEDURE WHICH SHOULD STACK THE       DB1054.2
            003400*    NAME OF THE PROCEDURE CALLING IT.  PRIOR TO BEING STACKED,   DB1054.2
            003500*    EACH NAME IS POTENTIALLY ADJUSTED BY MODIFYING A FIXED-      DB1054.2
            003600*    LOCATION NUMERIC SUBFIELD IN THE NAME.  THE CONSEQUENCE IS   DB1054.2
            003700*    THAT IF THE PROGRAM EXECUTES PROPERLY, THE NAMES THAT ARE    DB1054.2
            003800*    STACKED WILL BE UNIQUE AND IN AN INCREMENTING SEQUENCE IN    DB1054.2
            003900*    THE NUMERIC SUBFIELD.  NEAR THE END OF THE PROGRAM, THE      DB1054.2
            004000*    STACKING FUNCTION IS DISABLED AND THE NAME STACK IS COMPARED DB1054.2
            004100*    TO A STATIC TABLE CONTAINING PROCEDURE-NAMES IN THE ORDER    DB1054.2
            004200*    IN WHICH THE PROCEDURES SHOULD HAVE STACKED.                 DB1054.2
            004300*                                                                 DB1054.2
            004400*   DB105A"S REPORT DIFFERS SLIGHTLY FROM THE NOMINAL CCVS FORMAT.DB1054.2
            004500*    IF EXECUTION IS PERFECT, THE REPORT WILL CONSIST OF 227      DB1054.2
            004600*    LINES SHOWING                                                DB1054.2
            004700*                                                                 DB1054.2
            004800*        (A)  PROGRAM PROCEDURE NAME, AS IT APPEARS IN THE        DB1054.2
            004900*             PROGRAM.                                            DB1054.2
            005000*        (B)  ADJUSTED PROCEDURE NAME, AFTER ITS NUMERIC SUBFIELD DB1054.2
            005100*             HAS BEEN ADJUSTED.                                  DB1054.2
            005200*        (C)  ADJUSTED DEBUG-NAME, THAT WAS STACKED BY THE        DB1054.2
            005300*             DEBUGGING PROCEDURE.                                DB1054.2
            005400*                                                                 DB1054.2
            005500*    NOMINALLY, THE NUMERIC SUBFIELDS OF THE PROCEDURE NAMES      DB1054.2
            005600*    SHOULD APPEAR IN ASCENDING SEQUENCE.  ANY DEVIATIONS IN THE  DB1054.2
            005700*    STACKING SEQUENCE FROM THE EXPECTED SEQUENCE WILL CAUSE      DB1054.2
            005800*    ADDITIONAL REPORT LINES TO BE GENERATED WITH ONE OR MORE     DB1054.2
            005900*    COLUMNS BLANK.  IF NOTHING EVER APPEARS IN THE "ADJUSTED     DB1054.2
            006000*    DEBUG-NAME" COLUMN, IT MAY BE ASSUMED THAT THE DEBUGGING     DB1054.2
            006100*    PROCEDURE WAS NEVER EXECUTED.                                DB1054.2
            006200*                                                                 DB1054.2
            006300*    IT IS A FUNDAMENTAL ASSUMPTION OF DB105A THAT WHEN A SECTION DB1054.2
            006400*    IS ENTERED, THE DEBUGGING SECTION WILL BE CALLED TWICE, ONCE DB1054.2
            006500*    FOR THE SECTION NAME AND ONCE FOR THE PARAGRAPH NAME THAT    DB1054.2
            006600*    IMMEDIATELY FOLLOWS THE SECTION NAME.  ADDITIONALLY, DB105A  DB1054.2
            006700*    TRAPS ANY FAILURES IN PROGRAM FLOW CAUSED BY A FAILURE OF    DB1054.2
            006800*    VERBS FROM THE NUCLEUS MODULE.  THESE FAILURES ARE SUMMED    DB1054.2
            006900*    AND REPORTED AT THE BOTTOM OF DB105A"S REPORT.  IF ANY       DB1054.2
            007000*    PROCEDURE NAMES BEGINNING WITH "PROC-000" APPEAR IN THE      DB1054.2
            007100*    "ADJUSTED DEBUG-NAME" COLUMN OF THE REPORT, THESE RESULT     DB1054.2
            007200*    FROM EXECUTION OF PROCEDURES WHICH SHOULD NOT HAVE BEEN      DB1054.2
            007300*    EXECUTED IF THE PROGRAM HAD FOLLOWED THE PROPER CONTROL FLOW DB1054.2
            007400*    SEQUENCE.                                                    DB1054.2
            007500*                                                                 DB1054.2
            007600*                                                                 DB1054.2
            007700*                                                                 DB1054.2
            007800 ENVIRONMENT DIVISION.                                            DB1054.2
            007900 CONFIGURATION SECTION.                                           DB1054.2
            008000 SOURCE-COMPUTER.                                                 DB1054.2
            008100     XXXXX082                                                     DB1054.2
            008200     WITH DEBUGGING MODE.                                         DB1054.2
            008300 OBJECT-COMPUTER.                                                 DB1054.2
            008400     XXXXX083.                                                    DB1054.2
            008500 INPUT-OUTPUT SECTION.                                            DB1054.2
            008600 FILE-CONTROL.                                                    DB1054.2
            008700     SELECT PRINT-FILE ASSIGN TO                                  DB1054.2
            008800     XXXXX055.                                                    DB1054.2
            008900 DATA DIVISION.                                                   DB1054.2
            009000 FILE SECTION.                                                    DB1054.2
            009100 FD  PRINT-FILE                                                   DB1054.2
            009200     LABEL RECORDS                                                DB1054.2
            009300     XXXXX084                                                     DB1054.2
            009400     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB1054.2
            009500 01  PRINT-REC PICTURE X(120).                                    DB1054.2
            009600 01  DUMMY-RECORD PICTURE X(120).                                 DB1054.2
            009700 WORKING-STORAGE SECTION.                                         DB1054.2
            009800 77  ATWO-DS-01V00                PICTURE S9                      DB1054.2
            009900                                  VALUE 2.                        DB1054.2
            010000 77  P-COUNT                      PICTURE 9(6).                   DB1054.2
            010100 77  THREE                              PICTURE IS 9 VALUE IS 3.  DB1054.2
            010200 77  XRAY                               PICTURE IS X.             DB1054.2
            010300 77  ALTERLOOP                          PICTURE IS 9 VALUE IS     DB1054.2
            010400     ZERO.                                                        DB1054.2
            010500 77  BYPASS PICTURE IS 9 VALUE IS 1.                              DB1054.2
            010600 77  STACK-END PICTURE IS 999 COMPUTATIONAL.                      DB1054.2
            010700 77  INCREMENT PICTURE IS 99.                                     DB1054.2
            010800 77  PROC-ACTIVE PICTURE IS 9 VALUE IS 1.                         DB1054.2
            010900 01  PROCEDURE-NAMES.                                             DB1054.2
            011000     02  FILLER PIC X(25) VALUE "PROC-001-BEGIN-TESTS     ".      DB1054.2
            011100     02  FILLER PIC X(25) VALUE "PROC-002-GO--TEST-1      ".      DB1054.2
            011200     02  FILLER PIC X(25) VALUE "PROC-003-GO--WRITE-1     ".      DB1054.2
            011300     02  FILLER PIC X(25) VALUE "PROC-004-GO--INIT-2      ".      DB1054.2
            011400     02  FILLER PIC X(25) VALUE "PROC-005-GO--TEST-2      ".      DB1054.2
            011500     02  FILLER PIC X(25) VALUE "PROC-005-GO--A           ".      DB1054.2
            011600     02  FILLER PIC X(25) VALUE "PROC-005-GO--TEST-2      ".      DB1054.2
            011700     02  FILLER PIC X(25) VALUE "PROC-005-GO--B           ".      DB1054.2
            011800     02  FILLER PIC X(25) VALUE "PROC-005-GO--TEST-2      ".      DB1054.2
            011900     02  FILLER PIC X(25) VALUE "PROC-005-GO--C           ".      DB1054.2
            012000     02  FILLER PIC X(25) VALUE "PROC-005-GO--TEST-2      ".      DB1054.2
            012100     02  FILLER PIC X(25) VALUE "PROC-005-GO--D           ".      DB1054.2
            012200     02  FILLER PIC X(25) VALUE "PROC-005-GO--TEST-2      ".      DB1054.2
            012300     02  FILLER PIC X(25) VALUE "PROC-005-GO--A           ".      DB1054.2
            012400     02  FILLER PIC X(25) VALUE "PROC-005-GO--E           ".      DB1054.2
            012500     02  FILLER PIC X(25) VALUE "PROC-016-GO--WRITE-2     ".      DB1054.2
            012600     02  FILLER PIC X(25) VALUE "PROC-017-GO--TEST-3      ".      DB1054.2
            012700     02  FILLER PIC X(25) VALUE "PROC-018-GO--PASS-3      ".      DB1054.2
            012800     02  FILLER PIC X(25) VALUE "PROC-019-GO--WRITE-3     ".      DB1054.2
            012900     02  FILLER PIC X(25) VALUE "PROC-020-GO--TEST-4      ".      DB1054.2
            013000     02  FILLER PIC X(25) VALUE "PROC-021-GO--PASS-4      ".      DB1054.2
            013100     02  FILLER PIC X(25) VALUE "PROC-022-GO--PAS-4       ".      DB1054.2
            013200     02  FILLER PIC X(25) VALUE "PROC-023-GO--WRITE-4     ".      DB1054.2
            013300     02  FILLER PIC X(25) VALUE "PROC-024-ALTER-INIT      ".      DB1054.2
            013400     02  FILLER PIC X(25) VALUE "PROC-025-ALTER-TEST-1    ".      DB1054.2
            013500     02  FILLER PIC X(25) VALUE "PROC-026-ALTER-A         ".      DB1054.2
            013600     02  FILLER PIC X(25) VALUE "PROC-026-ALTER-A         ".      DB1054.2
            013700     02  FILLER PIC X(25) VALUE "PROC-027-ALTER-C         ".      DB1054.2
            013800     02  FILLER PIC X(25) VALUE "PROC-028-ALTER-WRITE-1   ".      DB1054.2
            013900     02  FILLER PIC X(25) VALUE "PROC-030-ALTER-TEST-3    ".      DB1054.2
            014000     02  FILLER PIC X(25) VALUE "PROC-031-ALTER-G         ".      DB1054.2
            014100     02  FILLER PIC X(25) VALUE "PROC-031-ALTER-G         ".      DB1054.2
            014200     02  FILLER PIC X(25) VALUE "PROC-032-ALTER-I         ".      DB1054.2
            014300     02  FILLER PIC X(25) VALUE "PROC-031-ALTER-G         ".      DB1054.2
            014400     02  FILLER PIC X(25) VALUE "PROC-031-ALTER-G         ".      DB1054.2
            014500     02  FILLER PIC X(25) VALUE "PROC-032-ALTER-WRITE-3   ".      DB1054.2
            014600     02  FILLER PIC X(25) VALUE "PROC-037-EXIT-TEST-1     ".      DB1054.2
            014700     02  FILLER PIC X(25) VALUE "PROC-038-EXIT-CHECK-1    ".      DB1054.2
            014800     02  FILLER PIC X(25) VALUE "PROC-039-EXIT-WRITE-1    ".      DB1054.2
            014900     02  FILLER PIC X(25) VALUE "PROC-040-PFM-TEST-1      ".      DB1054.2
            015000     02  FILLER PIC X(25) VALUE "PROC-041-PFM-A           ".      DB1054.2
            015100     02  FILLER PIC X(25) VALUE "PROC-042-PFM-WRITE-1     ".      DB1054.2
            015200     02  FILLER PIC X(25) VALUE "PROC-043-PFM-TEST-2      ".      DB1054.2
            015300     02  FILLER PIC X(25) VALUE "PROC-041-PFM-A           ".      DB1054.2
            015400     02  FILLER PIC X(25) VALUE "PROC-045-PFM-B           ".      DB1054.2
            015500     02  FILLER PIC X(25) VALUE "PROC-046-PFM-WRITE-2     ".      DB1054.2
            015600     02  FILLER PIC X(25) VALUE "PROC-047-PFM-TEST-3      ".      DB1054.2
            015700     02  FILLER PIC X(25) VALUE "PROC-048-PFM-C           ".      DB1054.2
            015800     02  FILLER PIC X(25) VALUE "PROC-048-PFM-C           ".      DB1054.2
            015900     02  FILLER PIC X(25) VALUE "PROC-048-PFM-C           ".      DB1054.2
            016000     02  FILLER PIC X(25) VALUE "PROC-048-PFM-C           ".      DB1054.2
            016100     02  FILLER PIC X(25) VALUE "PROC-048-PFM-C           ".      DB1054.2
            016200     02  FILLER PIC X(25) VALUE "PROC-048-PFM-C           ".      DB1054.2
            016300     02  FILLER PIC X(25) VALUE "PROC-048-PFM-WRITE-3     ".      DB1054.2
            016400     02  FILLER PIC X(25) VALUE "PROC-055-PFM-TEST-4      ".      DB1054.2
            016500     02  FILLER PIC X(25) VALUE "PROC-056-PFM-E           ".      DB1054.2
            016600     02  FILLER PIC X(25) VALUE "PROC-057-PFM-F           ".      DB1054.2
            016700     02  FILLER PIC X(25) VALUE "PROC-058-PFM-G           ".      DB1054.2
            016800     02  FILLER PIC X(25) VALUE "PROC-059-PFM-H           ".      DB1054.2
            016900     02  FILLER PIC X(25) VALUE "PROC-060-PFM-WRITE-4     ".      DB1054.2
            017000     02  FILLER PIC X(25) VALUE "PROC-061-PFM-TEST-5      ".      DB1054.2
            017100     02  FILLER PIC X(25) VALUE "PROC-062-PFM-J           ".      DB1054.2
            017200     02  FILLER PIC X(25) VALUE "PROC-063-PFM-L           ".      DB1054.2
            017300     02  FILLER PIC X(25) VALUE "PROC-064-PFM-WRITE-5     ".      DB1054.2
            017400     02  FILLER PIC X(25) VALUE "PROC-065-PFM-TEST-6      ".      DB1054.2
            017500     02  FILLER PIC X(25) VALUE "PROC-066-PFM-N           ".      DB1054.2
            017600     02  FILLER PIC X(25) VALUE "PROC-067-PFM-O           ".      DB1054.2
            017700     02  FILLER PIC X(25) VALUE "PROC-068-PFM-P           ".      DB1054.2
            017800     02  FILLER PIC X(25) VALUE "PROC-069-PFM-WRITE-6     ".      DB1054.2
            017900     02  FILLER PIC X(25) VALUE "PROC-070-PFM-TEST-7      ".      DB1054.2
            018000     02  FILLER PIC X(25) VALUE "PROC-071-PFM-V           ".      DB1054.2
            018100     02  FILLER PIC X(25) VALUE "PROC-072-PFM-W           ".      DB1054.2
            018200     02  FILLER PIC X(25) VALUE "PROC-073-PFM-X           ".      DB1054.2
            018300     02  FILLER PIC X(25) VALUE "PROC-074-PFM-Y           ".      DB1054.2
            018400     02  FILLER PIC X(25) VALUE "PROC-075-PFM-Z           ".      DB1054.2
            018500     02  FILLER PIC X(25) VALUE "PROC-071-PFM-V           ".      DB1054.2
            018600     02  FILLER PIC X(25) VALUE "PROC-072-PFM-W           ".      DB1054.2
            018700     02  FILLER PIC X(25) VALUE "PROC-073-PFM-X           ".      DB1054.2
            018800     02  FILLER PIC X(25) VALUE "PROC-074-PFM-Y           ".      DB1054.2
            018900     02  FILLER PIC X(25) VALUE "PROC-075-PFM-Z           ".      DB1054.2
            019000     02  FILLER PIC X(25) VALUE "PROC-071-PFM-V           ".      DB1054.2
            019100     02  FILLER PIC X(25) VALUE "PROC-072-PFM-W           ".      DB1054.2
            019200     02  FILLER PIC X(25) VALUE "PROC-073-PFM-X           ".      DB1054.2
            019300     02  FILLER PIC X(25) VALUE "PROC-074-PFM-Y           ".      DB1054.2
            019400     02  FILLER PIC X(25) VALUE "PROC-075-PFM-Z           ".      DB1054.2
            019500     02  FILLER PIC X(25) VALUE "PROC-071-PFM-V           ".      DB1054.2
            019600     02  FILLER PIC X(25) VALUE "PROC-072-PFM-W           ".      DB1054.2
            019700     02  FILLER PIC X(25) VALUE "PROC-073-PFM-X           ".      DB1054.2
            019800     02  FILLER PIC X(25) VALUE "PROC-074-PFM-Y           ".      DB1054.2
            019900     02  FILLER PIC X(25) VALUE "PROC-075-PFM-Z           ".      DB1054.2
            020000     02  FILLER PIC X(25) VALUE "PROC-071-PFM-V           ".      DB1054.2
            020100     02  FILLER PIC X(25) VALUE "PROC-072-PFM-W           ".      DB1054.2
            020200     02  FILLER PIC X(25) VALUE "PROC-073-PFM-X           ".      DB1054.2
            020300     02  FILLER PIC X(25) VALUE "PROC-074-PFM-Y           ".      DB1054.2
            020400     02  FILLER PIC X(25) VALUE "PROC-075-PFM-Z           ".      DB1054.2
            020500     02  FILLER PIC X(25) VALUE "PROC-096-PFM-WRITE-7     ".      DB1054.2
            020600     02  FILLER PIC X(25) VALUE "PROC-097-PFM-TEST-08     ".      DB1054.2
            020700     02  FILLER PIC X(25) VALUE "PROC-098-PFM-B-8         ".      DB1054.2
            020800     02  FILLER PIC X(25) VALUE "PROC-097-PFM-A-8         ".      DB1054.2
            020900     02  FILLER PIC X(25) VALUE "PROC-098-PFM-B-8         ".      DB1054.2
            021000     02  FILLER PIC X(25) VALUE "PROC-097-PFM-A-8         ".      DB1054.2
            021100     02  FILLER PIC X(25) VALUE "PROC-098-PFM-B-8         ".      DB1054.2
            021200     02  FILLER PIC X(25) VALUE "PROC-097-PFM-TESTT-8     ".      DB1054.2
            021300     02  FILLER PIC X(25) VALUE "PROC-098-PFM-TESTTT-8    ".      DB1054.2
            021400     02  FILLER PIC X(25) VALUE "PROC-105-PFM-WRITE-08    ".      DB1054.2
            021500     02  FILLER PIC X(25) VALUE "PROC-106-PFM-TEST-09     ".      DB1054.2
            021600     02  FILLER PIC X(25) VALUE "PROC-107-PFM-B-9         ".      DB1054.2
            021700     02  FILLER PIC X(25) VALUE "PROC-106-PFM-A-9         ".      DB1054.2
            021800     02  FILLER PIC X(25) VALUE "PROC-107-PFM-B-9         ".      DB1054.2
            021900     02  FILLER PIC X(25) VALUE "PROC-106-PFM-A-9         ".      DB1054.2
            022000     02  FILLER PIC X(25) VALUE "PROC-107-PFM-B-9         ".      DB1054.2
            022100     02  FILLER PIC X(25) VALUE "PROC-106-PFM-A-9         ".      DB1054.2
            022200     02  FILLER PIC X(25) VALUE "PROC-107-PFM-B-9         ".      DB1054.2
            022300     02  FILLER PIC X(25) VALUE "PROC-106-PFM-TESTT-9     ".      DB1054.2
            022400     02  FILLER PIC X(25) VALUE "PROC-107-PFM-TESTTT-9    ".      DB1054.2
            022500     02  FILLER PIC X(25) VALUE "PROC-116-PFM-WRITE-09    ".      DB1054.2
            022600     02  FILLER PIC X(25) VALUE "PROC-117-PFM-TEST-10     ".      DB1054.2
            022700     02  FILLER PIC X(25) VALUE "PROC-118-PFM-B-10        ".      DB1054.2
            022800     02  FILLER PIC X(25) VALUE "PROC-119-PFM-C-10        ".      DB1054.2
            022900     02  FILLER PIC X(25) VALUE "PROC-120-PFM-D-10        ".      DB1054.2
            023000     02  FILLER PIC X(25) VALUE "PROC-117-PFM-A-10        ".      DB1054.2
            023100     02  FILLER PIC X(25) VALUE "PROC-118-PFM-B-10        ".      DB1054.2
            023200     02  FILLER PIC X(25) VALUE "PROC-119-PFM-C-10        ".      DB1054.2
            023300     02  FILLER PIC X(25) VALUE "PROC-120-PFM-D-10        ".      DB1054.2
            023400     02  FILLER PIC X(25) VALUE "PROC-117-PFM-A-10        ".      DB1054.2
            023500     02  FILLER PIC X(25) VALUE "PROC-118-PFM-B-10        ".      DB1054.2
            023600     02  FILLER PIC X(25) VALUE "PROC-119-PFM-C-10        ".      DB1054.2
            023700     02  FILLER PIC X(25) VALUE "PROC-120-PFM-D-10        ".      DB1054.2
            023800     02  FILLER PIC X(25) VALUE "PROC-118-PFM-B-10        ".      DB1054.2
            023900     02  FILLER PIC X(25) VALUE "PROC-119-PFM-C-10        ".      DB1054.2
            024000     02  FILLER PIC X(25) VALUE "PROC-120-PFM-D-10        ".      DB1054.2
            024100     02  FILLER PIC X(25) VALUE "PROC-117-PFM-A-10        ".      DB1054.2
            024200     02  FILLER PIC X(25) VALUE "PROC-118-PFM-B-10        ".      DB1054.2
            024300     02  FILLER PIC X(25) VALUE "PROC-119-PFM-C-10        ".      DB1054.2
            024400     02  FILLER PIC X(25) VALUE "PROC-120-PFM-D-10        ".      DB1054.2
            024500     02  FILLER PIC X(25) VALUE "PROC-121-PFM-TESTT-10    ".      DB1054.2
            024600     02  FILLER PIC X(25) VALUE "PROC-122-PFM-TESTTT-10   ".      DB1054.2
            024700     02  FILLER PIC X(25) VALUE "PROC-138-PFM-WRITE-10    ".      DB1054.2
            024800     02  FILLER PIC X(25) VALUE "PROC-139-PFM-TEST-11     ".      DB1054.2
            024900     02  FILLER PIC X(25) VALUE "PROC-140-PFM-B-11        ".      DB1054.2
            025000     02  FILLER PIC X(25) VALUE "PROC-139-PFM-C-11        ".      DB1054.2
            025100     02  FILLER PIC X(25) VALUE "PROC-140-PFM-D-11        ".      DB1054.2
            025200     02  FILLER PIC X(25) VALUE "PROC-139-PFM-A-11        ".      DB1054.2
            025300     02  FILLER PIC X(25) VALUE "PROC-140-PFM-B-11        ".      DB1054.2
            025400     02  FILLER PIC X(25) VALUE "PROC-139-PFM-C-11        ".      DB1054.2
            025500     02  FILLER PIC X(25) VALUE "PROC-140-PFM-D-11        ".      DB1054.2
            025600     02  FILLER PIC X(25) VALUE "PROC-139-PFM-A-11        ".      DB1054.2
            025700     02  FILLER PIC X(25) VALUE "PROC-140-PFM-B-11        ".      DB1054.2
            025800     02  FILLER PIC X(25) VALUE "PROC-139-PFM-C-11        ".      DB1054.2
            025900     02  FILLER PIC X(25) VALUE "PROC-140-PFM-D-11        ".      DB1054.2
            026000     02  FILLER PIC X(25) VALUE "PROC-139-PFM-A-11        ".      DB1054.2
            026100     02  FILLER PIC X(25) VALUE "PROC-140-PFM-B-11        ".      DB1054.2
            026200     02  FILLER PIC X(25) VALUE "PROC-139-PFM-C-11        ".      DB1054.2
            026300     02  FILLER PIC X(25) VALUE "PROC-140-PFM-D-11        ".      DB1054.2
            026400     02  FILLER PIC X(25) VALUE "PROC-139-PFM-A-11        ".      DB1054.2
            026500     02  FILLER PIC X(25) VALUE "PROC-140-PFM-B-11        ".      DB1054.2
            026600     02  FILLER PIC X(25) VALUE "PROC-139-PFM-C-11        ".      DB1054.2
            026700     02  FILLER PIC X(25) VALUE "PROC-140-PFM-D-11        ".      DB1054.2
            026800     02  FILLER PIC X(25) VALUE "PROC-140-PFM-B-11        ".      DB1054.2
            026900     02  FILLER PIC X(25) VALUE "PROC-139-PFM-C-11        ".      DB1054.2
            027000     02  FILLER PIC X(25) VALUE "PROC-140-PFM-D-11        ".      DB1054.2
            027100     02  FILLER PIC X(25) VALUE "PROC-139-PFM-A-11        ".      DB1054.2
            027200     02  FILLER PIC X(25) VALUE "PROC-140-PFM-B-11        ".      DB1054.2
            027300     02  FILLER PIC X(25) VALUE "PROC-139-PFM-C-11        ".      DB1054.2
            027400     02  FILLER PIC X(25) VALUE "PROC-140-PFM-D-11        ".      DB1054.2
            027500     02  FILLER PIC X(25) VALUE "PROC-139-PFM-TESTT-11    ".      DB1054.2
            027600     02  FILLER PIC X(25) VALUE "PROC-140-PFM-TESTTT-11   ".      DB1054.2
            027700     02  FILLER PIC X(25) VALUE "PROC-168-PFM-WRITE-11    ".      DB1054.2
            027800     02  FILLER PIC X(25) VALUE "PROC-169-PFM-TEST-12     ".      DB1054.2
            027900     02  FILLER PIC X(25) VALUE "PROC-170-PFM-A-12        ".      DB1054.2
            028000     02  FILLER PIC X(25) VALUE "PROC-171-PFM-B-12        ".      DB1054.2
            028100     02  FILLER PIC X(25) VALUE "PROC-172-PFM-C-12        ".      DB1054.2
            028200     02  FILLER PIC X(25) VALUE "PROC-173-PFM-D-12        ".      DB1054.2
            028300     02  FILLER PIC X(25) VALUE "PROC-174-PFM-E-12        ".      DB1054.2
            028400     02  FILLER PIC X(25) VALUE "PROC-175-PFM-TESTT-12    ".      DB1054.2
            028500     02  FILLER PIC X(25) VALUE "PROC-176-PFM-WRITE-12    ".      DB1054.2
            028600     02  FILLER PIC X(25) VALUE "PROC-177-PFM-TEST-13     ".      DB1054.2
            028700     02  FILLER PIC X(25) VALUE "PROC-178-PFM-A-13        ".      DB1054.2
            028800     02  FILLER PIC X(25) VALUE "PROC-177-PFM-B-13        ".      DB1054.2
            028900     02  FILLER PIC X(25) VALUE "PROC-178-PFM-A-13        ".      DB1054.2
            029000     02  FILLER PIC X(25) VALUE "PROC-177-PFM-B-13        ".      DB1054.2
            029100     02  FILLER PIC X(25) VALUE "PROC-178-PFM-A-13        ".      DB1054.2
            029200     02  FILLER PIC X(25) VALUE "PROC-177-PFM-B-13        ".      DB1054.2
            029300     02  FILLER PIC X(25) VALUE "PROC-178-PFM-A-13        ".      DB1054.2
            029400     02  FILLER PIC X(25) VALUE "PROC-177-PFM-B-13        ".      DB1054.2
            029500     02  FILLER PIC X(25) VALUE "PROC-178-PFM-TESTT-13    ".      DB1054.2
            029600     02  FILLER PIC X(25) VALUE "PROC-187-PFM-WRITE-13    ".      DB1054.2
            029700     02  FILLER PIC X(25) VALUE "PROC-188-PFM-TEST-14     ".      DB1054.2
            029800     02  FILLER PIC X(25) VALUE "PROC-189-A101            ".      DB1054.2
            029900     02  FILLER PIC X(25) VALUE "PROC-190-A102            ".      DB1054.2
            030000     02  FILLER PIC X(25) VALUE "PROC-191-A103            ".      DB1054.2
            030100     02  FILLER PIC X(25) VALUE "PROC-192-A104            ".      DB1054.2
            030200     02  FILLER PIC X(25) VALUE "PROC-193-A105            ".      DB1054.2
            030300     02  FILLER PIC X(25) VALUE "PROC-194-A106            ".      DB1054.2
            030400     02  FILLER PIC X(25) VALUE "PROC-195-A107            ".      DB1054.2
            030500     02  FILLER PIC X(25) VALUE "PROC-196-A108            ".      DB1054.2
            030600     02  FILLER PIC X(25) VALUE "PROC-197-A109            ".      DB1054.2
            030700     02  FILLER PIC X(25) VALUE "PROC-198-A110            ".      DB1054.2
            030800     02  FILLER PIC X(25) VALUE "PROC-199-A111            ".      DB1054.2
            030900     02  FILLER PIC X(25) VALUE "PROC-200-A112            ".      DB1054.2
            031000     02  FILLER PIC X(25) VALUE "PROC-201-A113            ".      DB1054.2
            031100     02  FILLER PIC X(25) VALUE "PROC-202-A114            ".      DB1054.2
            031200     02  FILLER PIC X(25) VALUE "PROC-203-A115            ".      DB1054.2
            031300     02  FILLER PIC X(25) VALUE "PROC-204-A116            ".      DB1054.2
            031400     02  FILLER PIC X(25) VALUE "PROC-205-A117            ".      DB1054.2
            031500     02  FILLER PIC X(25) VALUE "PROC-206-A118            ".      DB1054.2
            031600     02  FILLER PIC X(25) VALUE "PROC-207-A119            ".      DB1054.2
            031700     02  FILLER PIC X(25) VALUE "PROC-208-A120            ".      DB1054.2
            031800     02  FILLER PIC X(25) VALUE "PROC-209-A121            ".      DB1054.2
            031900     02  FILLER PIC X(25) VALUE "PROC-210-PFM-WRITE-14    ".      DB1054.2
            032000     02  FILLER PIC X(25) VALUE "PROC-211-PFM-A-15        ".      DB1054.2
            032100     02  FILLER PIC X(25) VALUE "PROC-212-PFM-TEST-15     ".      DB1054.2
            032200     02  FILLER PIC X(25) VALUE "PROC-213-PFM-G-15        ".      DB1054.2
            032300     02  FILLER PIC X(25) VALUE "PROC-214-PFM-H-15        ".      DB1054.2
            032400     02  FILLER PIC X(25) VALUE "PROC-215-PFM-E-15        ".      DB1054.2
            032500     02  FILLER PIC X(25) VALUE "PROC-216-PFM-L-15        ".      DB1054.2
            032600     02  FILLER PIC X(25) VALUE "PROC-217-PFM-B-15        ".      DB1054.2
            032700     02  FILLER PIC X(25) VALUE "PROC-218-PFM-WRITE-15    ".      DB1054.2
            032800     02  FILLER PIC X(25) VALUE "PROC-219-PFM-TEST-LAST   ".      DB1054.2
            032900     02  FILLER PIC X(25) VALUE "PROC-220-PFM-U           ".      DB1054.2
            033000     02  FILLER PIC X(25) VALUE "PROC-220-PFM-U           ".      DB1054.2
            033100     02  FILLER PIC X(25) VALUE "PROC-220-PFM-U           ".      DB1054.2
            033200     02  FILLER PIC X(25) VALUE "PROC-220-PFM-U           ".      DB1054.2
            033300     02  FILLER PIC X(25) VALUE "PROC-220-PFM-U           ".      DB1054.2
            033400     02  FILLER PIC X(25) VALUE "PROC-220-PFM-U           ".      DB1054.2
            033500     02  FILLER PIC X(25) VALUE "PROC-220-PFM-U           ".      DB1054.2
            033600     02  FILLER PIC X(25) VALUE "PROC-227-PFM-WRITE-LAST  ".      DB1054.2
            033700 01  STATIC-TABLE REDEFINES PROCEDURE-NAMES.                      DB1054.2
            033800     02  EXPECTED-NAME OCCURS 227 TIMES INDEXED BY STATIC-INDEX   DB1054.2
            033900         PICTURE IS X(25).                                        DB1054.2
            034000 01  STACKING-AREA.                                               DB1054.2
            034100     02  PROC-NAME OCCURS 500 TIMES INDEXED BY STACK-INDEX.       DB1054.2
            034200         03  PROC-LOC PICTURE IS X(4).                            DB1054.2
            034300         03  FILLER PICTURE IS X.                                 DB1054.2
            034400         03  BASE-NUMBER PICTURE IS 999.                          DB1054.2
            034500         03  FILLER PICTURE IS X.                                 DB1054.2
            034600         03  BASE-NAME PICTURE IS X(16).                          DB1054.2
            034700 01  TABLE-ENTRY.                                                 DB1054.2
            034800     02  FILLER PICTURE IS X(5).                                  DB1054.2
            034900     02  TABLE-ENTRY-BASE PICTURE IS 999.                         DB1054.2
            035000     02  FILLER PICTURE IS X(17).                                 DB1054.2
            035100 01  FLOW-FAILURE-1.                                              DB1054.2
            035200     02  FILLER PICTURE IS X VALUE IS SPACE.                      DB1054.2
            035300     02  FILLER PICTURE IS X(43) VALUE                            DB1054.2
            035400         "COUNT OF LEVEL 1 NUCLEUS FLOW FAILURES WAS ".           DB1054.2
            035500     02  NUC-FAILURE-COUNT PICTURE IS 999 VALUE 0.                DB1054.2
            035600     02  FILLER PICTURE X VALUE ".".                              DB1054.2
            035700 01  FLOW-FAILURE-2 PICTURE IS X(75) VALUE IS                     DB1054.2
            035800         " A NON-ZERO COUNT WILL CAUSE FAILURES TO APPEAR IN THE ADB1054.2
            035900-        "BOVE REPORT.".                                          DB1054.2
            036000 01  NOTE-RECORD.                                                 DB1054.2
            036100     02 A     PICTURE X VALUE SPACE.                              DB1054.2
            036200     02 B     PICTURE X VALUE SPACE.                              DB1054.2
            036300     02 C     PICTURE X VALUE SPACE.                              DB1054.2
            036400     02 D     PICTURE X VALUE SPACE.                              DB1054.2
            036500     02 E     PICTURE X VALUE SPACE.                              DB1054.2
            036600     02 F     PICTURE X VALUE SPACE.                              DB1054.2
            036700     02 G     PICTURE X VALUE SPACE.                              DB1054.2
            036800     02 H     PICTURE X VALUE SPACE.                              DB1054.2
            036900     02 I     PICTURE X VALUE SPACE.                              DB1054.2
            037000     02 J     PICTURE X VALUE SPACE.                              DB1054.2
            037100     02 K     PICTURE X VALUE SPACE.                              DB1054.2
            037200     02 L     PICTURE X VALUE SPACE.                              DB1054.2
            037300     02 M     PICTURE X VALUE SPACE.                              DB1054.2
            037400     02 N     PICTURE X VALUE SPACE.                              DB1054.2
            037500     02 O     PICTURE X VALUE SPACE.                              DB1054.2
            037600     02 P     PICTURE X VALUE SPACE.                              DB1054.2
            037700 01  GO-TABLE.                                                    DB1054.2
            037800     02 GO-SCRIPT OCCURS 8 TIMES PICTURE 9.                       DB1054.2
            037900 01  GO-TO-DEPEND                       PICTURE IS 9 VALUE IS 0.  DB1054.2
            038000 01  GO-TO-DEEP                         PICTURE IS 9 VALUE IS 1.  DB1054.2
            038100 01  PERFORM1                           PICTURE IS XXX            DB1054.2
            038200     VALUE IS SPACE.                                              DB1054.2
            038300 01  PERFORM2                           PICTURE IS S999           DB1054.2
            038400     VALUE IS 20.                                                 DB1054.2
            038500 01  PERFORM4                           PICTURE IS S99V9.         DB1054.2
            038600 01  PERFORM5                           PICTURE IS 999            DB1054.2
            038700     VALUE IS ZERO.                                               DB1054.2
            038800 01  PERFORM-KEY                        PICTURE IS 9.             DB1054.2
            038900 01  PERFORM-HOLD.                                                DB1054.2
            039000     02  TEST-LETTER OCCURS 20 TIMES  PICTURE X.                  DB1054.2
            039100 01  TEST-RESULTS.                                                DB1054.2
            039200     02 FILLER                    PICTURE X VALUE SPACE.          DB1054.2
            039300     02 FEATURE                   PICTURE X(20).                  DB1054.2
            039400     02 FILLER                    PICTURE XX VALUE SPACE.         DB1054.2
            039500     02 P-OR-F                    PICTURE X(5).                   DB1054.2
            039600     02 FILLER                    PICTURE XX  VALUE SPACE.        DB1054.2
            039700     02  PAR-NAME.                                                DB1054.2
            039800       03 FILLER PICTURE X(12).                                   DB1054.2
            039900       03  PARDOT-X PICTURE X.                                    DB1054.2
            040000       03 DOTVALUE PICTURE 99.                                    DB1054.2
            040100       03 FILLER PICTURE IS X(10).                                DB1054.2
            040200     02 FILLER                    PICTURE X VALUE SPACE.          DB1054.2
            040300     02  CORRECT-A  PICTURE IS X(25).                             DB1054.2
            040400     02  CORRECT-NFIELD REDEFINES CORRECT-A.                      DB1054.2
            040500         03  CORRECT-N  PICTURE -9(9).9(9).                       DB1054.2
            040600         03  FILLER PICTURE X(5).                                 DB1054.2
            040700     02 FILLER                    PICTURE XX VALUE SPACE.         DB1054.2
            040800     02  COMPUTED-A PICTURE IS X(25).                             DB1054.2
            040900     02  COMPUTED-NFIELD REDEFINES COMPUTED-A.                    DB1054.2
            041000         03  COMPUTED-N PICTURE -9(9).9(9).                       DB1054.2
            041100         03  FILLER PICTURE X(5).                                 DB1054.2
            041200     02  RE-MARK PICTURE IS XXX.                                  DB1054.2
            041300 01  COLUMNS-LINE-1.                                              DB1054.2
            041400     02 PAGE-CONTROL-C PICTURE IS X VALUE IS SPACE.               DB1054.2
            041500     02  FILLER PICTURE IS X(7) VALUE IS SPACE.                   DB1054.2
            041600     02  FILLER PICTURE IS X(7) VALUE IS "FEATURE".               DB1054.2
            041700     02  FILLER PICTURE IS X(9) VALUE IS SPACE.                   DB1054.2
            041800     02  FILLER PICTURE IS X(4) VALUE IS "PASS".                  DB1054.2
            041900     02  FILLER PICTURE IS X(10) VALUE IS SPACE.                  DB1054.2
            042000     02  FILLER PICTURE IS X(7)  VALUE IS "PROGRAM".              DB1054.2
            042100     02  FILLER PICTURE IS X(19) VALUE IS SPACE.                  DB1054.2
            042200     02  FILLER PICTURE IS X(8)  VALUE IS "ADJUSTED".             DB1054.2
            042300     02  FILLER PICTURE IS X(18) VALUE IS SPACE.                  DB1054.2
            042400     02  FILLER PICTURE IS X(8)  VALUE IS "ADJUSTED".             DB1054.2
            042500 01  COLUMNS-LINE-2.                                              DB1054.2
            042600     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1054.2
            042700     02  FILLER PICTURE IS X(7) VALUE IS SPACE.                   DB1054.2
            042800     02  FILLER PICTURE IS X(6)  VALUE IS "TESTED".               DB1054.2
            042900     02  FILLER PICTURE IS X(10) VALUE IS SPACE.                  DB1054.2
            043000     02  FILLER PICTURE IS X(4)  VALUE IS "FAIL".                 DB1054.2
            043100     02  FILLER PICTURE IS X(7)  VALUE IS SPACE.                  DB1054.2
            043200     02  FILLER PICTURE IS X(14) VALUE IS "PROCEDURE NAME".       DB1054.2
            043300     02  FILLER PICTURE IS X(12) VALUE IS SPACE.                  DB1054.2
            043400     02  FILLER PICTURE IS X(14) VALUE IS "PROCEDURE NAME".       DB1054.2
            043500     02  FILLER PICTURE IS X(14) VALUE IS SPACE.                  DB1054.2
            043600     02  FILLER PICTURE IS X(10) VALUE IS "DEBUG-NAME".           DB1054.2
            043700 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB1054.2
            043800 01  REC-CT PICTURE 99 VALUE ZERO.                                DB1054.2
            043900 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB1054.2
            044000 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB1054.2
            044100 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB1054.2
            044200 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB1054.2
            044300 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB1054.2
            044400 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB1054.2
            044500 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB1054.2
            044600 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB1054.2
            044700 01  CCVS-H-1.                                                    DB1054.2
            044800     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB1054.2
            044900     02 FILLER PICTURE X(67) VALUE                                DB1054.2
            045000     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB1054.2
            045100-    " SYSTEM".                                                   DB1054.2
            045200     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB1054.2
            045300 01  CCVS-H-2.                                                    DB1054.2
            045400     02 FILLER PICTURE X(52) VALUE IS                             DB1054.2
            045500     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB1054.2
            045600     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB1054.2
            045700     02 TEST-ID PICTURE IS X(9).                                  DB1054.2
            045800     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB1054.2
            045900 01  CCVS-H-3.                                                    DB1054.2
            046000     02  FILLER PICTURE X(34) VALUE                               DB1054.2
            046100     " FOR OFFICIAL USE ONLY    ".                                DB1054.2
            046200     02  FILLER PICTURE X(58) VALUE                               DB1054.2
            046300     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB1054.2
            046400     02  FILLER PICTURE X(28) VALUE                               DB1054.2
            046500     "  COPYRIGHT   1974 ".                                       DB1054.2
            046600 01  CCVS-E-1.                                                    DB1054.2
            046700     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB1054.2
            046800     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB1054.2
            046900     02 ID-AGAIN PICTURE IS X(9).                                 DB1054.2
            047000     02 FILLER PICTURE X(45) VALUE IS                             DB1054.2
            047100     " NTIS DISTRIBUTION COBOL 74".                               DB1054.2
            047200 01  CCVS-E-2.                                                    DB1054.2
            047300     02  FILLER                   PICTURE X(31)  VALUE            DB1054.2
            047400     SPACE.                                                       DB1054.2
            047500     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB1054.2
            047600     02 CCVS-E-2-2.                                               DB1054.2
            047700         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB1054.2
            047800         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB1054.2
            047900         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB1054.2
            048000 01  CCVS-E-3.                                                    DB1054.2
            048100     02  FILLER PICTURE X(22) VALUE                               DB1054.2
            048200     " FOR OFFICIAL USE ONLY".                                    DB1054.2
            048300     02  FILLER PICTURE X(12) VALUE SPACE.                        DB1054.2
            048400     02  FILLER PICTURE X(58) VALUE                               DB1054.2
            048500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB1054.2
            048600     02  FILLER PICTURE X(13) VALUE SPACE.                        DB1054.2
            048700     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB1054.2
            048800 01  CCVS-E-4.                                                    DB1054.2
            048900     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB1054.2
            049000     02 FILLER PIC XXXX VALUE " OF ".                             DB1054.2
            049100     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB1054.2
            049200     02 FILLER PIC X(40) VALUE                                    DB1054.2
            049300      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB1054.2
            049400 01  XXINFO.                                                      DB1054.2
            049500     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB1054.2
            049600     02 INFO-TEXT.                                                DB1054.2
            049700     04 FILLER PIC X(20) VALUE SPACE.                             DB1054.2
            049800     04 XXCOMPUTED PIC X(20).                                     DB1054.2
            049900     04 FILLER PIC X(5) VALUE SPACE.                              DB1054.2
            050000     04 XXCORRECT PIC X(20).                                      DB1054.2
            050100 01  HYPHEN-LINE.                                                 DB1054.2
            050200     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1054.2
            050300     02 FILLER PICTURE IS X(65) VALUE IS "************************DB1054.2
            050400-    "*****************************************".                 DB1054.2
            050500     02 FILLER PICTURE IS X(54) VALUE IS "************************DB1054.2
            050600-    "******************************".                            DB1054.2
            050700 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB1054.2
            050800     "DB105A".                                                    DB1054.2
            050900 PROCEDURE DIVISION.                                              DB1054.2
            051000 DECLARATIVES.                                                    DB1054.2
            051100 DEBUG-ALL-PROCS SECTION.                                         DB1054.2
            051200     USE FOR DEBUGGING ON ALL PROCEDURES.                         DB1054.2
            051300 DEBUG-ALL-0.                                                     DB1054.2
            051400     MOVE 0 TO PROC-ACTIVE.                                       DB1054.2
            051500     IF BYPASS IS EQUAL TO 1 GO TO DEBUG-ALL-EXIT.                DB1054.2
            051600     MOVE DEBUG-NAME TO PROC-NAME (STACK-INDEX).                  DB1054.2
            051700     IF PROC-LOC (STACK-INDEX) IS EQUAL TO "PROC"                 DB1054.2
            051800             INSPECT PROC-NAME (STACK-INDEX)                      DB1054.2
            051900             REPLACING CHARACTERS BY " " AFTER INITIAL " ".       DB1054.2
            052000     IF BASE-NUMBER (STACK-INDEX) IS NUMERIC                      DB1054.2
            052100             ADD INCREMENT TO BASE-NUMBER (STACK-INDEX).          DB1054.2
            052200     IF STACK-INDEX IS EQUAL TO 500                               DB1054.2
            052300             MOVE 1 TO BYPASS                                     DB1054.2
            052400             GO TO DEBUG-ALL-EXIT.                                DB1054.2
            052500     SET STACK-INDEX UP BY 1.                                     DB1054.2
            052600 DEBUG-ALL-EXIT.                                                  DB1054.2
            052700     EXIT.                                                        DB1054.2
            052800 END DECLARATIVES.                                                DB1054.2
            052900 CCVS1 SECTION.                                                   DB1054.2
            053000 OPEN-FILES.                                                      DB1054.2
            053100     OPEN     OUTPUT PRINT-FILE.                                  DB1054.2
            053200     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB1054.2
            053300     MOVE    SPACE TO TEST-RESULTS.                               DB1054.2
            053400     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB1054.2
            053500     GO TO CCVS1-EXIT.                                            DB1054.2
            053600 CLOSE-FILES.                                                     DB1054.2
            053700     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB1054.2
            053800 TERMINATE-CCVS.                                                  DB1054.2
            053900T    EXIT PROGRAM.                                                DB1054.2
            054000TTERMINATE-CALL.                                                  DB1054.2
            054100     STOP     RUN.                                                DB1054.2
            054200 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB1054.2
            054300 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB1054.2
            054400 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB1054.2
            054500 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB1054.2
            054600     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1054.2
            054700 PRINT-DETAIL.                                                    DB1054.2
            054800     IF REC-CT NOT EQUAL TO ZERO                                  DB1054.2
            054900             MOVE "." TO PARDOT-X                                 DB1054.2
            055000             MOVE REC-CT TO DOTVALUE.                             DB1054.2
            055100     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB1054.2
            055200     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-A.              DB1054.2
            055300     MOVE SPACE TO CORRECT-A.                                     DB1054.2
            055400     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1054.2
            055500     MOVE     SPACE TO RE-MARK.                                   DB1054.2
            055600 HEAD-ROUTINE.                                                    DB1054.2
            055700     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1054.2
            055800     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB1054.2
            055900     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB1054.2
            056000 COLUMN-NAMES-ROUTINE.                                            DB1054.2
            056100     MOVE COLUMNS-LINE-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.     DB1054.2
            056200     MOVE COLUMNS-LINE-2 TO DUMMY-RECORD.                         DB1054.2
            056300     PERFORM WRITE-LINE 2 TIMES.                                  DB1054.2
            056400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB1054.2
            056500 END-ROUTINE.                                                     DB1054.2
            056600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB1054.2
            056700 END-RTN-EXIT.                                                    DB1054.2
            056800     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1054.2
            056900 END-ROUTINE-1.                                                   DB1054.2
            057000      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB1054.2
            057100      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB1054.2
            057200      ADD PASS-COUNTER TO ERROR-HOLD.                             DB1054.2
            057300*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB1054.2
            057400      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB1054.2
            057500      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB1054.2
            057600      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB1054.2
            057700      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB1054.2
            057800  END-ROUTINE-12.                                                 DB1054.2
            057900      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB1054.2
            058000     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB1054.2
            058100         MOVE "NO " TO ERROR-TOTAL                                DB1054.2
            058200         ELSE                                                     DB1054.2
            058300         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB1054.2
            058400     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB1054.2
            058500     PERFORM WRITE-LINE.                                          DB1054.2
            058600 END-ROUTINE-13.                                                  DB1054.2
            058700     IF DELETE-CNT IS EQUAL TO ZERO                               DB1054.2
            058800         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB1054.2
            058900         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB1054.2
            059000     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB1054.2
            059100     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1054.2
            059200      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB1054.2
            059300          MOVE "NO " TO ERROR-TOTAL                               DB1054.2
            059400      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB1054.2
            059500      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB1054.2
            059600      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB1054.2
            059700     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1054.2
            059800 WRITE-LINE.                                                      DB1054.2
            059900     ADD 1 TO RECORD-COUNT.                                       DB1054.2
            060000Y    IF RECORD-COUNT GREATER 50                                   DB1054.2
            060100Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1054.2
            060200Y        MOVE SPACE TO DUMMY-RECORD                               DB1054.2
            060300Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1054.2
            060400Y        MOVE COLUMNS-LINE-1 TO DUMMY-RECORD PERFORM WRT-LN       DB1054.2
            060500Y        MOVE COLUMNS-LINE-2 TO DUMMY-RECORD PERFORM WRT-LN       DB1054.2
            060600Y            2 TIMES                                              DB1054.2
            060700Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB1054.2
            060800Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1054.2
            060900Y        MOVE ZERO TO RECORD-COUNT.                               DB1054.2
            061000     PERFORM   WRT-LN.                                            DB1054.2
            061100 WRT-LN.                                                          DB1054.2
            061200     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1054.2
            061300     MOVE SPACE TO DUMMY-RECORD.                                  DB1054.2
            061400*                                                                 DB1054.2
            061500 CCVS1-EXIT.                                                      DB1054.2
            061600     EXIT.                                                        DB1054.2
            061700 INITIALIZE-PROC-NAME-STACK.                                      DB1054.2
            061800     MOVE 0 TO BYPASS.                                            DB1054.2
            061900     SET STACK-INDEX TO 1.                                        DB1054.2
            062000     MOVE 0 TO INCREMENT.                                         DB1054.2
            062100 PROC-001-BEGIN-TESTS SECTION.                                    DB1054.2
            062200 PROC-002-GO--TEST-1.                                             DB1054.2
            062300     MOVE 0 TO INCREMENT.                                         DB1054.2
            062400     GO TO    PROC-003-GO--WRITE-1.                               DB1054.2
            062500 PROC-000-GO--DELETE-1.                                           DB1054.2
            062600     MOVE 0 TO INCREMENT.                                         DB1054.2
            062700     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            062800     GO TO    PROC-003-GO--WRITE-1.                               DB1054.2
            062900 PROC-003-GO--WRITE-1.                                            DB1054.2
            063000     MOVE 0 TO INCREMENT.                                         DB1054.2
            063100     MOVE "GO TO " TO FEATURE.                                    DB1054.2
            063200     MOVE "PROC-002-GO--TEST-1" TO PAR-NAME.                      DB1054.2
            063300 PROC-004-GO--INIT-2.                                             DB1054.2
            063400     MOVE 0 TO INCREMENT.                                         DB1054.2
            063500     MOVE "PROC-005-GO--TEST-2" TO PAR-NAME.                      DB1054.2
            063600     MOVE     SPACE TO P-OR-F.                                    DB1054.2
            063700     MOVE "GO TO DEPENDING" TO FEATURE.                           DB1054.2
            063800 PROC-005-GO--TEST-2.                                             DB1054.2
            063900     ADD 1 TO INCREMENT.                                          DB1054.2
            064000     MOVE     SPACE TO FEATURE.                                   DB1054.2
            064100     GO TO    PROC-005-GO--B                                      DB1054.2
            064200              PROC-005-GO--D                                      DB1054.2
            064300              PROC-005-GO--C DEPENDING ON GO-TO-DEPEND.           DB1054.2
            064400     GO TO    PROC-005-GO--A.                                     DB1054.2
            064500 PROC-000-GO--DELETE-2.                                           DB1054.2
            064600     MOVE 0 TO INCREMENT.                                         DB1054.2
            064700     MOVE "PROC-005-GO--TEST-2" TO PAR-NAME.                      DB1054.2
            064800     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            064900     GO TO    PROC-016-GO--WRITE-2.                               DB1054.2
            065000 PROC-005-GO--A.                                                  DB1054.2
            065100     ADD 1 TO INCREMENT.                                          DB1054.2
            065200     MOVE "PROC-005-GO--A" TO PAR-NAME.                           DB1054.2
            065300     IF       GO-TO-DEPEND EQUAL TO 0                             DB1054.2
            065400              ADD 1 TO GO-TO-DEPEND                               DB1054.2
            065500              GO TO PROC-005-GO--TEST-2.                          DB1054.2
            065600     IF       GO-TO-DEPEND GREATER THAN 3                         DB1054.2
            065700              GO TO PROC-005-GO--E.                               DB1054.2
            065800     PERFORM  PROC-000-NUCLEUS-FAILURE                            DB1054.2
            065900              MOVE 1 TO GO-TO-DEPEND                              DB1054.2
            066000              GO TO PROC-005-GO--TEST-2.                          DB1054.2
            066100 PROC-005-GO--B.                                                  DB1054.2
            066200     ADD 1 TO INCREMENT.                                          DB1054.2
            066300     MOVE "PROC-005-GO--B" TO PAR-NAME.                           DB1054.2
            066400     IF       GO-TO-DEPEND NOT EQUAL TO 1                         DB1054.2
            066500              PERFORM PROC-000-NUCLEUS-FAILURE                    DB1054.2
            066600              MOVE 3 TO GO-TO-DEPEND                              DB1054.2
            066700              GO TO PROC-005-GO--TEST-2.                          DB1054.2
            066800     ADD      2 TO GO-TO-DEPEND.                                  DB1054.2
            066900     GO TO    PROC-005-GO--TEST-2.                                DB1054.2
            067000 PROC-005-GO--C.                                                  DB1054.2
            067100     ADD 1 TO INCREMENT.                                          DB1054.2
            067200     MOVE "PROC-005-GO--C" TO PAR-NAME.                           DB1054.2
            067300     IF       GO-TO-DEPEND NOT EQUAL TO 3                         DB1054.2
            067400              PERFORM PROC-000-NUCLEUS-FAILURE                    DB1054.2
            067500              MOVE 2 TO GO-TO-DEPEND                              DB1054.2
            067600              GO TO PROC-005-GO--TEST-2.                          DB1054.2
            067700     SUBTRACT 1 FROM GO-TO-DEPEND.                                DB1054.2
            067800     GO TO    PROC-005-GO--TEST-2.                                DB1054.2
            067900 PROC-005-GO--D.                                                  DB1054.2
            068000     ADD 1 TO INCREMENT.                                          DB1054.2
            068100     MOVE "PROC-005-GO--D" TO PAR-NAME.                           DB1054.2
            068200     IF       GO-TO-DEPEND NOT EQUAL TO 2                         DB1054.2
            068300              PERFORM PROC-000-NUCLEUS-FAILURE                    DB1054.2
            068400              MOVE 4 TO GO-TO-DEPEND                              DB1054.2
            068500              GO TO PROC-005-GO--TEST-2.                          DB1054.2
            068600     ADD      2 TO GO-TO-DEPEND.                                  DB1054.2
            068700     GO TO    PROC-005-GO--TEST-2.                                DB1054.2
            068800 PROC-005-GO--E.                                                  DB1054.2
            068900     MOVE 0 TO INCREMENT.                                         DB1054.2
            069000     MOVE "PROC-005-GO--E" TO PAR-NAME.                           DB1054.2
            069100     IF       GO-TO-DEPEND EQUAL TO 4                             DB1054.2
            069200              GO TO PROC-016-GO--WRITE-2.                         DB1054.2
            069300     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            069400 PROC-016-GO--WRITE-2.                                            DB1054.2
            069500     MOVE 0 TO INCREMENT.                                         DB1054.2
            069600     ADD 1 TO REC-CT.                                             DB1054.2
            069700 PROC-017-GO--TEST-3.                                             DB1054.2
            069800     MOVE 0 TO INCREMENT.                                         DB1054.2
            069900     GO TO    PROC-018-GO--PASS-3.                                DB1054.2
            070000 PROC-000-GO--TEST-3A.                                            DB1054.2
            070100     MOVE 0 TO INCREMENT.                                         DB1054.2
            070200     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            070300     GO TO    PROC-019-GO--WRITE-3.                               DB1054.2
            070400 PROC-000-GO--DELETE-3.                                           DB1054.2
            070500     MOVE 0 TO INCREMENT.                                         DB1054.2
            070600     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            070700     GO TO    PROC-019-GO--WRITE-3.                               DB1054.2
            070800 PROC-018-GO--PASS-3   SECTION.                                   DB1054.2
            070900 PROC-019-GO--WRITE-3.                                            DB1054.2
            071000     MOVE 0 TO INCREMENT.                                         DB1054.2
            071100     MOVE "GO TO" TO FEATURE.                                     DB1054.2
            071200     MOVE "PROC-017-GO--TEST-3" TO PAR-NAME.                      DB1054.2
            071300 PROC-020-GO--TEST-4.                                             DB1054.2
            071400     MOVE 0 TO INCREMENT.                                         DB1054.2
            071500     GO TO    PROC-021-GO--PASS-4                                 DB1054.2
            071600              PROC-000-GO--NUC-FAIL-4 DEPENDING ON GO-TO-DEEP.    DB1054.2
            071700*    NOTE THAT PROC-021-GO--PASS-4 IS A SECTION-NAME.             DB1054.2
            071800     GO TO    PROC-000-GO--NUC-FAIL-4.                            DB1054.2
            071900 PROC-000-GO--DELETE-4.                                           DB1054.2
            072000     MOVE 0 TO INCREMENT.                                         DB1054.2
            072100     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            072200     GO TO    PROC-023-GO--WRITE-4.                               DB1054.2
            072300 PROC-021-GO--PASS-4   SECTION.                                   DB1054.2
            072400 PROC-022-GO--PAS-4.                                              DB1054.2
            072500     MOVE 0 TO INCREMENT.                                         DB1054.2
            072600     IF       GO-TO-DEEP EQUAL TO 1                               DB1054.2
            072700              GO TO PROC-023-GO--WRITE-4.                         DB1054.2
            072800 PROC-000-GO--NUC-FAIL-4.                                         DB1054.2
            072900     MOVE 0 TO INCREMENT.                                         DB1054.2
            073000     MOVE     GO-TO-DEEP TO COMPUTED-N.                           DB1054.2
            073100     MOVE     1 TO CORRECT-N.                                     DB1054.2
            073200     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            073300 PROC-023-GO--WRITE-4.                                            DB1054.2
            073400     MOVE 0 TO INCREMENT.                                         DB1054.2
            073500     MOVE "GO TO DEPENDING" TO FEATURE.                           DB1054.2
            073600     MOVE "PROC-020-GO--TEST-4" TO PAR-NAME.                      DB1054.2
            073700 PROC-024-ALTER-INIT.                                             DB1054.2
            073800     MOVE 0 TO INCREMENT.                                         DB1054.2
            073900     MOVE "ALTER" TO FEATURE.                                     DB1054.2
            074000 PROC-025-ALTER-TEST-1.                                           DB1054.2
            074100     MOVE 0 TO INCREMENT.                                         DB1054.2
            074200     ALTER    PROC-026-ALTER-A TO PROCEED TO PROC-027-ALTER-C.    DB1054.2
            074300     MOVE 1 TO INCREMENT.                                         DB1054.2
            074400     GO TO    PROC-026-ALTER-A.                                   DB1054.2
            074500 PROC-000-ALTER-DELETE-1.                                         DB1054.2
            074600     MOVE 0 TO INCREMENT.                                         DB1054.2
            074700     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            074800     GO TO    PROC-028-ALTER-WRITE-1.                             DB1054.2
            074900 PROC-026-ALTER-A.                                                DB1054.2
            075000     GO TO    PROC-000-ALTER-B.                                   DB1054.2
            075100 PROC-000-ALTER-B.                                                DB1054.2
            075200     MOVE 0 TO INCREMENT.                                         DB1054.2
            075300     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            075400     GO TO    PROC-028-ALTER-WRITE-1.                             DB1054.2
            075500 PROC-027-ALTER-C.                                                DB1054.2
            075600     ADD 0 TO INCREMENT.                                          DB1054.2
            075700 PROC-028-ALTER-WRITE-1.                                          DB1054.2
            075800     MOVE 0 TO INCREMENT.                                         DB1054.2
            075900     MOVE "PROC-025-ALTER-TEST-1" TO PAR-NAME.                    DB1054.2
            076000 PROC-030-ALTER-TEST-3.                                           DB1054.2
            076100     MOVE 0 TO INCREMENT.                                         DB1054.2
            076200     ALTER    PROC-031-ALTER-G TO PROCEED TO PROC-032-ALTER-I.    DB1054.2
            076300     MOVE 1 TO INCREMENT.                                         DB1054.2
            076400     GO TO    PROC-031-ALTER-G.                                   DB1054.2
            076500 PROC-000-ALTER-DELETE-3.                                         DB1054.2
            076600     MOVE 0 TO INCREMENT.                                         DB1054.2
            076700     PERFORM PROC-000-NUCLEUS-FAILURE.                            DB1054.2
            076800     GO TO    PROC-032-ALTER-WRITE-3.                             DB1054.2
            076900 PROC-031-ALTER-G.                                                DB1054.2
            077000     GO TO    PROC-000-ALTER-H.                                   DB1054.2
            077100 PROC-000-ALTER-H.                                                DB1054.2
            077200     MOVE 0 TO INCREMENT.                                         DB1054.2
            077300     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            077400     GO TO    PROC-032-ALTER-WRITE-3.                             DB1054.2
            077500 PROC-032-ALTER-I.                                                DB1054.2
            077600     MOVE 3 TO INCREMENT.                                         DB1054.2
            077700     ADD      1 TO ALTERLOOP.                                     DB1054.2
            077800     IF       ALTERLOOP GREATER THAN 1                            DB1054.2
            077900              PERFORM PROC-000-NUCLEUS-FAILURE                    DB1054.2
            078000              GO TO PROC-032-ALTER-WRITE-3.                       DB1054.2
            078100     ALTER    PROC-031-ALTER-G TO  PROC-032-ALTER-WRITE-3.        DB1054.2
            078200     MOVE 4 TO INCREMENT.                                         DB1054.2
            078300     GO TO    PROC-031-ALTER-G.                                   DB1054.2
            078400 PROC-032-ALTER-WRITE-3.                                          DB1054.2
            078500     MOVE 0 TO INCREMENT.                                         DB1054.2
            078600     MOVE "PROC-030-ALTER-TEST-3" TO PAR-NAME.                    DB1054.2
            078700 PROC-037-EXIT-TEST-1.                                            DB1054.2
            078800     MOVE 0 TO INCREMENT.                                         DB1054.2
            078900     GO TO    PROC-038-EXIT-CHECK-1.                              DB1054.2
            079000 PROC-000-EXIT-DELETE-1.                                          DB1054.2
            079100     MOVE 0 TO INCREMENT.                                         DB1054.2
            079200     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            079300     GO TO    PROC-039-EXIT-WRITE-1.                              DB1054.2
            079400 PROC-038-EXIT-CHECK-1.                                           DB1054.2
            079500     EXIT.                                                        DB1054.2
            079600 PROC-039-EXIT-WRITE-1.                                           DB1054.2
            079700     MOVE 0 TO INCREMENT.                                         DB1054.2
            079800     MOVE "EXIT" TO FEATURE.                                      DB1054.2
            079900     MOVE "PROC-037-EXIT-TEST-1" TO PAR-NAME.                     DB1054.2
            080000 PROC-040-PFM-TEST-1.                                             DB1054.2
            080100     MOVE 0 TO INCREMENT.                                         DB1054.2
            080200     MOVE     1 TO PERFORM-KEY.                                   DB1054.2
            080300     PERFORM  PROC-041-PFM-A.                                     DB1054.2
            080400     IF       PERFORM1 EQUAL TO "ABC"                             DB1054.2
            080500              NEXT SENTENCE                                       DB1054.2
            080600              ELSE                                                DB1054.2
            080700              PERFORM PROC-000-NUCLEUS-FAILURE.                   DB1054.2
            080800     GO TO    PROC-042-PFM-WRITE-1.                               DB1054.2
            080900 PROC-000-PFM-DELETE-1.                                           DB1054.2
            081000     MOVE 0 TO INCREMENT.                                         DB1054.2
            081100     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            081200 PROC-042-PFM-WRITE-1.                                            DB1054.2
            081300     MOVE 0 TO INCREMENT.                                         DB1054.2
            081400     MOVE "PERFORM" TO FEATURE.                                   DB1054.2
            081500     MOVE "PFM-TEST-1" TO PAR-NAME.                               DB1054.2
            081600 PROC-043-PFM-TEST-2.                                             DB1054.2
            081700     MOVE 3 TO INCREMENT.                                         DB1054.2
            081800     MOVE     2 TO PERFORM-KEY.                                   DB1054.2
            081900     GO TO    PROC-041-PFM-A.                                     DB1054.2
            082000 PROC-000-PFM-DELETE-2.                                           DB1054.2
            082100     MOVE 0 TO INCREMENT.                                         DB1054.2
            082200     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            082300     GO TO    PROC-046-PFM-WRITE-2.                               DB1054.2
            082400 PROC-041-PFM-A.                                                  DB1054.2
            082500     MOVE 0 TO INCREMENT.                                         DB1054.2
            082600     IF       PERFORM-KEY EQUAL TO 1                              DB1054.2
            082700              MOVE "ABC" TO PERFORM1                              DB1054.2
            082800              ELSE                                                DB1054.2
            082900              MOVE "XYZ" TO PERFORM1.                             DB1054.2
            083000 PROC-045-PFM-B.                                                  DB1054.2
            083100     MOVE 0 TO INCREMENT.                                         DB1054.2
            083200     IF       PERFORM-KEY EQUAL TO 1                              DB1054.2
            083300              PERFORM PROC-000-NUCLEUS-FAILURE                    DB1054.2
            083400              GO TO PROC-043-PFM-TEST-2.                          DB1054.2
            083500     IF       PERFORM1 EQUAL TO "XYZ"                             DB1054.2
            083600              NEXT SENTENCE                                       DB1054.2
            083700              ELSE                                                DB1054.2
            083800              PERFORM PROC-000-NUCLEUS-FAILURE.                   DB1054.2
            083900 PROC-046-PFM-WRITE-2.                                            DB1054.2
            084000     MOVE 0 TO INCREMENT.                                         DB1054.2
            084100     MOVE "PERFORM" TO FEATURE.                                   DB1054.2
            084200     MOVE "PROC-043-PFM-TEST-2" TO PAR-NAME.                      DB1054.2
            084300 PROC-047-PFM-TEST-3.                                             DB1054.2
            084400     MOVE 0 TO INCREMENT.                                         DB1054.2
            084500     PERFORM  PROC-048-PFM-C 3 TIMES.                             DB1054.2
            084600     PERFORM  PROC-048-PFM-C THREE TIMES.                         DB1054.2
            084700     IF       PERFORM2 EQUAL TO 56                                DB1054.2
            084800              NEXT SENTENCE                                       DB1054.2
            084900              ELSE                                                DB1054.2
            085000              PERFORM PROC-000-NUCLEUS-FAILURE.                   DB1054.2
            085100     GO TO    PROC-048-PFM-WRITE-3.                               DB1054.2
            085200 PROC-000-PFM-DELETE-3.                                           DB1054.2
            085300     MOVE 0 TO INCREMENT.                                         DB1054.2
            085400     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            085500 PROC-048-PFM-WRITE-3.                                            DB1054.2
            085600     MOVE 0 TO INCREMENT.                                         DB1054.2
            085700     MOVE "PERFORM TIMES" TO FEATURE.                             DB1054.2
            085800     MOVE "PROC-047-PFM-TEST-3" TO PAR-NAME.                      DB1054.2
            085900 PROC-055-PFM-TEST-4.                                             DB1054.2
            086000     MOVE 0 TO INCREMENT.                                         DB1054.2
            086100     PERFORM  PROC-056-PFM-E THRU PROC-059-PFM-H.                 DB1054.2
            086200     IF       PERFORM4 EQUAL TO 70.0                              DB1054.2
            086300              NEXT SENTENCE                                       DB1054.2
            086400              ELSE                                                DB1054.2
            086500              PERFORM PROC-000-NUCLEUS-FAILURE.                   DB1054.2
            086600     GO TO    PROC-060-PFM-WRITE-4.                               DB1054.2
            086700 PROC-000-PFM-DELETE-4.                                           DB1054.2
            086800     MOVE 0 TO INCREMENT.                                         DB1054.2
            086900     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            087000 PROC-060-PFM-WRITE-4.                                            DB1054.2
            087100     MOVE 0 TO INCREMENT.                                         DB1054.2
            087200     MOVE "NESTED PERFORM THRU" TO FEATURE.                       DB1054.2
            087300     MOVE "PROC-055-PFM-TEST-4" TO PAR-NAME.                      DB1054.2
            087400 PROC-061-PFM-TEST-5.                                             DB1054.2
            087500     MOVE 0 TO INCREMENT.                                         DB1054.2
            087600     PERFORM  PROC-062-PFM-J.                                     DB1054.2
            087700     IF       PERFORM2 EQUAL TO 312                               DB1054.2
            087800              NEXT SENTENCE                                       DB1054.2
            087900              ELSE                                                DB1054.2
            088000              PERFORM PROC-000-NUCLEUS-FAILURE.                   DB1054.2
            088100     GO TO    PROC-064-PFM-WRITE-5.                               DB1054.2
            088200 PROC-000-PFM-DELETE-5.                                           DB1054.2
            088300     MOVE 0 TO INCREMENT.                                         DB1054.2
            088400     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            088500 PROC-064-PFM-WRITE-5.                                            DB1054.2
            088600     MOVE 0 TO INCREMENT.                                         DB1054.2
            088700     MOVE "NESTED PERFORM" TO FEATURE.                            DB1054.2
            088800     MOVE "PROC-061-PFM-TEST-5" TO PAR-NAME.                      DB1054.2
            088900 PROC-065-PFM-TEST-6.                                             DB1054.2
            089000     MOVE 0 TO INCREMENT.                                         DB1054.2
            089100     PERFORM  PROC-066-PFM-N.                                     DB1054.2
            089200     GO TO    PROC-069-PFM-WRITE-6.                               DB1054.2
            089300 PROC-000-PFM-DELETE-6.                                           DB1054.2
            089400     MOVE 0 TO INCREMENT.                                         DB1054.2
            089500     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            089600 PROC-069-PFM-WRITE-6.                                            DB1054.2
            089700     MOVE 0 TO INCREMENT.                                         DB1054.2
            089800     MOVE "PERFORM SECTION-NAME" TO FEATURE.                      DB1054.2
            089900     MOVE "PROC-065-PFM-TEST-6" TO PAR-NAME.                      DB1054.2
            090000 PROC-070-PFM-TEST-7.                                             DB1054.2
            090100     MOVE 0 TO INCREMENT.                                         DB1054.2
            090200     PERFORM  PROC-071-PFM-V THRU PROC-075-PFM-Z 5 TIMES.         DB1054.2
            090300     MOVE 0 TO INCREMENT.                                         DB1054.2
            090400     GO       TO PROC-096-PFM-WRITE-7.                            DB1054.2
            090500 PROC-000-PFM-DELETE-7.                                           DB1054.2
            090600     MOVE 0 TO INCREMENT.                                         DB1054.2
            090700     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            090800 PROC-096-PFM-WRITE-7.                                            DB1054.2
            090900     MOVE 0 TO INCREMENT.                                         DB1054.2
            091000     MOVE     "PERFORM EXIT PARAS" TO FEATURE.                    DB1054.2
            091100     MOVE "PROC-070-PFM-TEST-7" TO PAR-NAME.                      DB1054.2
            091200 PROC-097-PFM-TEST-08.                                            DB1054.2
            091300     MOVE 0 TO INCREMENT.                                         DB1054.2
            091400     MOVE     ZERO TO P-COUNT                                     DB1054.2
            091500     PERFORM  PROC-098-PFM-B-8.                                   DB1054.2
            091600     ADD      1 TO P-COUNT.                                       DB1054.2
            091700     PERFORM PROC-097-PFM-A-8.                                    DB1054.2
            091800     ADD      1 TO P-COUNT.                                       DB1054.2
            091900 PROC-097-PFM-A-8   SECTION.                                      DB1054.2
            092000 PROC-098-PFM-B-8.                                                DB1054.2
            092100     ADD 2 TO INCREMENT.                                          DB1054.2
            092200     ADD      100 TO P-COUNT.                                     DB1054.2
            092300 PROC-097-PFM-TESTT-8 SECTION.                                    DB1054.2
            092400 PROC-098-PFM-TESTTT-8.                                           DB1054.2
            092500     MOVE 0 TO INCREMENT.                                         DB1054.2
            092600     IF       P-COUNT EQUAL TO 000302                             DB1054.2
            092700              GO TO PROC-105-PFM-WRITE-08.                        DB1054.2
            092800     GO       TO PROC-000-PFM-NUC-FAIL-08.                        DB1054.2
            092900 PROC-000-DELETE-08.                                              DB1054.2
            093000     MOVE 0 TO INCREMENT.                                         DB1054.2
            093100     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            093200     GO       TO PROC-105-PFM-WRITE-08.                           DB1054.2
            093300 PROC-000-PFM-NUC-FAIL-08.                                        DB1054.2
            093400     MOVE 0 TO INCREMENT.                                         DB1054.2
            093500     MOVE     P-COUNT TO COMPUTED-N.                              DB1054.2
            093600     MOVE     000302 TO CORRECT-N.                                DB1054.2
            093700     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            093800 PROC-105-PFM-WRITE-08.                                           DB1054.2
            093900     MOVE 0 TO INCREMENT.                                         DB1054.2
            094000     MOVE "PERFORM             " TO FEATURE.                      DB1054.2
            094100     MOVE     "PROC-097-PFM-TEST-08" TO PAR-NAME.                 DB1054.2
            094200 PROC-106-PFM-TEST-09.                                            DB1054.2
            094300     MOVE 0 TO INCREMENT.                                         DB1054.2
            094400     MOVE     ZERO TO P-COUNT.                                    DB1054.2
            094500     PERFORM PROC-107-PFM-B-9   1 TIMES.                          DB1054.2
            094600     ADD      1 TO P-COUNT.                                       DB1054.2
            094700     PERFORM  PROC-106-PFM-A-9   ATWO-DS-01V00 TIMES.             DB1054.2
            094800     ADD      1 TO P-COUNT.                                       DB1054.2
            094900 PROC-106-PFM-A-9   SECTION.                                      DB1054.2
            095000 PROC-107-PFM-B-9.                                                DB1054.2
            095100     ADD 2 TO INCREMENT.                                          DB1054.2
            095200     ADD      100 TO P-COUNT.                                     DB1054.2
            095300 PROC-106-PFM-TESTT-9 SECTION.                                    DB1054.2
            095400 PROC-107-PFM-TESTTT-9.                                           DB1054.2
            095500     MOVE 0 TO INCREMENT.                                         DB1054.2
            095600     IF       P-COUNT EQUAL TO 000402                             DB1054.2
            095700              GO TO PROC-116-PFM-WRITE-09.                        DB1054.2
            095800     GO       TO PROC-000-PFM-NUC-FAIL-09.                        DB1054.2
            095900 PROC-000-PFM-DELETE-09.                                          DB1054.2
            096000     MOVE 0 TO INCREMENT.                                         DB1054.2
            096100     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            096200     GO       TO PROC-116-PFM-WRITE-09.                           DB1054.2
            096300 PROC-000-PFM-NUC-FAIL-09.                                        DB1054.2
            096400     MOVE 0 TO INCREMENT.                                         DB1054.2
            096500     MOVE     P-COUNT TO COMPUTED-N.                              DB1054.2
            096600     MOVE     000502 TO CORRECT-N.                                DB1054.2
            096700     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            096800 PROC-116-PFM-WRITE-09.                                           DB1054.2
            096900     MOVE 0 TO INCREMENT.                                         DB1054.2
            097000     MOVE "PERFORM TIMES       " TO FEATURE.                      DB1054.2
            097100     MOVE     "PROC-106-PFM-TEST-09" TO PAR-NAME.                 DB1054.2
            097200 PROC-117-PFM-TEST-10.                                            DB1054.2
            097300     MOVE 0 TO INCREMENT.                                         DB1054.2
            097400     MOVE     ZERO TO P-COUNT.                                    DB1054.2
            097500     PERFORM  PROC-118-PFM-B-10 THROUGH PROC-120-PFM-D-10.        DB1054.2
            097600     MOVE 4 TO INCREMENT.                                         DB1054.2
            097700     ADD      1 TO P-COUNT                                        DB1054.2
            097800     PERFORM  PROC-117-PFM-A-10 THRU PROC-119-PFM-C-10.           DB1054.2
            097900     MOVE 8 TO INCREMENT.                                         DB1054.2
            098000     ADD      1 TO P-COUNT.                                       DB1054.2
            098100     PERFORM  PROC-117-PFM-A-10 THRU PROC-120-PFM-D-10.           DB1054.2
            098200     MOVE 11 TO INCREMENT.                                        DB1054.2
            098300     ADD      1 TO P-COUNT.                                       DB1054.2
            098400     PERFORM  PROC-118-PFM-B-10 THRU PROC-119-PFM-C-10.           DB1054.2
            098500     MOVE 15 TO INCREMENT.                                        DB1054.2
            098600     ADD     1 TO P-COUNT.                                        DB1054.2
            098700 PROC-117-PFM-A-10 SECTION.                                       DB1054.2
            098800 PROC-118-PFM-B-10.                                               DB1054.2
            098900     ADD      100 TO P-COUNT.                                     DB1054.2
            099000 PROC-119-PFM-C-10 SECTION.                                       DB1054.2
            099100 PROC-120-PFM-D-10.                                               DB1054.2
            099200     ADD      10000 TO P-COUNT.                                   DB1054.2
            099300 PROC-121-PFM-TESTT-10 SECTION.                                   DB1054.2
            099400 PROC-122-PFM-TESTTT-10.                                          DB1054.2
            099500     MOVE 0 TO INCREMENT.                                         DB1054.2
            099600     IF       P-COUNT EQUAL TO 050504                             DB1054.2
            099700              GO TO PROC-138-PFM-WRITE-10.                        DB1054.2
            099800     GO       TO PROC-000-PFM-NUC-FAIL-10.                        DB1054.2
            099900 PROC-000-PFM-DELETE-10.                                          DB1054.2
            100000     MOVE 0 TO INCREMENT.                                         DB1054.2
            100100     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            100200     GO       TO PROC-138-PFM-WRITE-10.                           DB1054.2
            100300 PROC-000-PFM-NUC-FAIL-10.                                        DB1054.2
            100400     MOVE 0 TO INCREMENT.                                         DB1054.2
            100500     MOVE     P-COUNT TO COMPUTED-N.                              DB1054.2
            100600     MOVE     050504 TO CORRECT-N.                                DB1054.2
            100700     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            100800 PROC-138-PFM-WRITE-10.                                           DB1054.2
            100900     MOVE 0 TO INCREMENT.                                         DB1054.2
            101000     MOVE "PERFORM THRU        " TO FEATURE.                      DB1054.2
            101100     MOVE     "PROC-117-PFM-TEST-10" TO PAR-NAME.                 DB1054.2
            101200 PROC-139-PFM-TEST-11.                                            DB1054.2
            101300     MOVE 0 TO INCREMENT.                                         DB1054.2
            101400     MOVE     ZERO TO P-COUNT.                                    DB1054.2
            101500     PERFORM PROC-140-PFM-B-11 THROUGH PROC-140-PFM-D-11 1 TIMES. DB1054.2
            101600     MOVE 4 TO INCREMENT.                                         DB1054.2
            101700     ADD      1 TO P-COUNT.                                       DB1054.2
            101800     PERFORM  PROC-139-PFM-A-11 THRU PROC-139-PFM-C-11 2 TIMES.   DB1054.2
            101900     MOVE 12 TO INCREMENT.                                        DB1054.2
            102000     ADD      1 TO P-COUNT.                                       DB1054.2
            102100     PERFORM  PROC-139-PFM-A-11 THRU PROC-140-PFM-D-11 2 TIMES.   DB1054.2
            102200     MOVE 19 TO INCREMENT.                                        DB1054.2
            102300     ADD      1 TO P-COUNT.                                       DB1054.2
            102400     PERFORM PROC-140-PFM-B-11 THRU PROC-139-PFM-C-11   1 TIMES.  DB1054.2
            102500     MOVE 23 TO INCREMENT.                                        DB1054.2
            102600     ADD      1 TO P-COUNT.                                       DB1054.2
            102700 PROC-139-PFM-A-11 SECTION.                                       DB1054.2
            102800 PROC-140-PFM-B-11.                                               DB1054.2
            102900     ADD 2 TO INCREMENT.                                          DB1054.2
            103000     ADD      100 TO P-COUNT.                                     DB1054.2
            103100 PROC-139-PFM-C-11 SECTION.                                       DB1054.2
            103200 PROC-140-PFM-D-11.                                               DB1054.2
            103300     ADD 2 TO INCREMENT.                                          DB1054.2
            103400     ADD     10000 TO P-COUNT.                                    DB1054.2
            103500 PROC-139-PFM-TESTT-11 SECTION.                                   DB1054.2
            103600 PROC-140-PFM-TESTTT-11.                                          DB1054.2
            103700     MOVE 0 TO INCREMENT.                                         DB1054.2
            103800     IF       P-COUNT EQUAL TO 070704                             DB1054.2
            103900              GO TO PROC-168-PFM-WRITE-11.                        DB1054.2
            104000     GO       TO PROC-000-PFM-NUC-FAIL-11.                        DB1054.2
            104100 PROC-000-PFM-DELETE-11.                                          DB1054.2
            104200     MOVE 0 TO INCREMENT.                                         DB1054.2
            104300     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            104400     GO       TO PROC-168-PFM-WRITE-11.                           DB1054.2
            104500 PROC-000-PFM-NUC-FAIL-11.                                        DB1054.2
            104600     MOVE 0 TO INCREMENT.                                         DB1054.2
            104700     MOVE     P-COUNT TO COMPUTED-N.                              DB1054.2
            104800     MOVE     090904 TO CORRECT-N.                                DB1054.2
            104900     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            105000 PROC-168-PFM-WRITE-11.                                           DB1054.2
            105100     MOVE 0 TO INCREMENT.                                         DB1054.2
            105200     MOVE "PERFORM THRU, TIMES " TO FEATURE.                      DB1054.2
            105300     MOVE     "PROC-139-PFM-TEST-11" TO PAR-NAME.                 DB1054.2
            105400 PROC-169-PFM-TEST-12.                                            DB1054.2
            105500     MOVE 0 TO INCREMENT.                                         DB1054.2
            105600     MOVE     ZERO TO P-COUNT.                                    DB1054.2
            105700     ADD      1 TO P-COUNT.                                       DB1054.2
            105800     PERFORM  PROC-170-PFM-A-12.                                  DB1054.2
            105900     ADD      2 TO P-COUNT.                                       DB1054.2
            106000     GO       TO PROC-175-PFM-TESTT-12.                           DB1054.2
            106100 PROC-170-PFM-A-12.                                               DB1054.2
            106200     MOVE 0 TO INCREMENT.                                         DB1054.2
            106300     ADD      10 TO P-COUNT.                                      DB1054.2
            106400     PERFORM  PROC-171-PFM-B-12.                                  DB1054.2
            106500     ADD      20 TO P-COUNT.                                      DB1054.2
            106600 PROC-171-PFM-B-12.                                               DB1054.2
            106700     MOVE 0 TO INCREMENT.                                         DB1054.2
            106800     ADD      100 TO P-COUNT.                                     DB1054.2
            106900     PERFORM  PROC-172-PFM-C-12.                                  DB1054.2
            107000     ADD      200 TO P-COUNT.                                     DB1054.2
            107100 PROC-172-PFM-C-12.                                               DB1054.2
            107200     MOVE 0 TO INCREMENT.                                         DB1054.2
            107300     ADD      1000 TO P-COUNT.                                    DB1054.2
            107400     PERFORM  PROC-173-PFM-D-12.                                  DB1054.2
            107500     ADD      2000 TO P-COUNT.                                    DB1054.2
            107600 PROC-173-PFM-D-12.                                               DB1054.2
            107700     MOVE 0 TO INCREMENT.                                         DB1054.2
            107800     ADD      10000 TO P-COUNT.                                   DB1054.2
            107900     PERFORM  PROC-174-PFM-E-12.                                  DB1054.2
            108000     ADD      20000 TO P-COUNT.                                   DB1054.2
            108100 PROC-174-PFM-E-12.                                               DB1054.2
            108200     MOVE 0 TO INCREMENT.                                         DB1054.2
            108300     ADD      100000 TO P-COUNT.                                  DB1054.2
            108400 PROC-175-PFM-TESTT-12.                                           DB1054.2
            108500     MOVE 0 TO INCREMENT.                                         DB1054.2
            108600     IF       P-COUNT EQUAL TO 133333                             DB1054.2
            108700              GO TO PROC-176-PFM-WRITE-12.                        DB1054.2
            108800     GO       TO PROC-000-PFM-NUC-FAIL-12.                        DB1054.2
            108900 PROC-000-PFM-DELETE-12.                                          DB1054.2
            109000     MOVE 0 TO INCREMENT.                                         DB1054.2
            109100     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            109200     GO       TO PROC-176-PFM-WRITE-12.                           DB1054.2
            109300 PROC-000-PFM-NUC-FAIL-12.                                        DB1054.2
            109400     MOVE 0 TO INCREMENT.                                         DB1054.2
            109500     MOVE     P-COUNT TO COMPUTED-N.                              DB1054.2
            109600     MOVE     133333 TO CORRECT-N.                                DB1054.2
            109700     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            109800 PROC-176-PFM-WRITE-12.                                           DB1054.2
            109900     MOVE 0 TO INCREMENT.                                         DB1054.2
            110000     MOVE "NESTED PERFORM      " TO FEATURE.                      DB1054.2
            110100     MOVE     "PROC-169-PFM-TEST-12" TO PAR-NAME.                 DB1054.2
            110200 PROC-177-PFM-TEST-13.                                            DB1054.2
            110300     MOVE 0 TO INCREMENT.                                         DB1054.2
            110400     MOVE     ZERO TO P-COUNT.                                    DB1054.2
            110500     PERFORM  PROC-178-PFM-A-13 THRU PROC-177-PFM-B-13.           DB1054.2
            110600     ADD      1 TO P-COUNT.                                       DB1054.2
            110700     MOVE 2 TO INCREMENT.                                         DB1054.2
            110800     PERFORM  PROC-178-PFM-A-13 THRU PROC-177-PFM-B-13 2 TIMES.   DB1054.2
            110900     ADD      2 TO P-COUNT.                                       DB1054.2
            111000 PROC-178-PFM-A-13.                                               DB1054.2
            111100     ADD 2 TO INCREMENT.                                          DB1054.2
            111200     ADD      100 TO P-COUNT.                                     DB1054.2
            111300 PROC-177-PFM-B-13.                                               DB1054.2
            111400     EXIT.                                                        DB1054.2
            111500 PROC-178-PFM-TESTT-13.                                           DB1054.2
            111600     MOVE 0 TO INCREMENT.                                         DB1054.2
            111700     IF       P-COUNT EQUAL TO 000403                             DB1054.2
            111800              GO TO PROC-187-PFM-WRITE-13.                        DB1054.2
            111900     GO       TO PROC-000-PFM-NUC-FAIL-13.                        DB1054.2
            112000 PROC-000-PFM-DELETE-13.                                          DB1054.2
            112100     MOVE 0 TO INCREMENT.                                         DB1054.2
            112200     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            112300     GO       TO PROC-187-PFM-WRITE-13.                           DB1054.2
            112400 PROC-000-PFM-NUC-FAIL-13.                                        DB1054.2
            112500     MOVE 0 TO INCREMENT.                                         DB1054.2
            112600     MOVE     P-COUNT TO COMPUTED-N.                              DB1054.2
            112700     MOVE     000403 TO CORRECT-N.                                DB1054.2
            112800     MOVE     "PERFORM WITH EXIT" TO FEATURE.                     DB1054.2
            112900     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            113000 PROC-187-PFM-WRITE-13.                                           DB1054.2
            113100     MOVE 0 TO INCREMENT.                                         DB1054.2
            113200     MOVE     "PROC-177-PFM-TEST-13" TO PAR-NAME.                 DB1054.2
            113300 PROC-188-PFM-TEST-14.                                            DB1054.2
            113400     MOVE 0 TO INCREMENT.                                         DB1054.2
            113500     PERFORM PROC-189-A101.                                       DB1054.2
            113600     IF PERFORM-HOLD EQUAL TO "ABCDEFGHIJKLMNOPQRST"              DB1054.2
            113700         GO TO PROC-210-PFM-WRITE-14.                             DB1054.2
            113800     MOVE "ABCDEFGHIJKLMNOPQRST" TO CORRECT-A.                    DB1054.2
            113900     MOVE PERFORM-HOLD TO COMPUTED-A.                             DB1054.2
            114000     PERFORM PROC-000-NUCLEUS-FAILURE.                            DB1054.2
            114100     GO TO PROC-210-PFM-WRITE-14.                                 DB1054.2
            114200 PROC-000-PFM-DELETE-14.                                          DB1054.2
            114300     MOVE 0 TO INCREMENT.                                         DB1054.2
            114400     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            114500 PROC-210-PFM-WRITE-14.                                           DB1054.2
            114600     MOVE 0 TO INCREMENT.                                         DB1054.2
            114700     MOVE     "PROC-188-PFM-TEST-14" TO PAR-NAME.                 DB1054.2
            114800 PROC-211-PFM-A-15 SECTION.                                       DB1054.2
            114900 PROC-212-PFM-TEST-15.                                            DB1054.2
            115000     MOVE 0 TO INCREMENT.                                         DB1054.2
            115100     PERFORM  PROC-213-PFM-G-15 THRU PROC-217-PFM-B-15.           DB1054.2
            115200     GO       TO PROC-218-PFM-WRITE-15.                           DB1054.2
            115300 PROC-000-PFM-DELETE-15.                                          DB1054.2
            115400     MOVE 0 TO INCREMENT.                                         DB1054.2
            115500     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            115600     GO       TO PROC-218-PFM-WRITE-15.                           DB1054.2
            115700 PROC-217-PFM-B-15.                                               DB1054.2
            115800     MOVE 0 TO INCREMENT.                                         DB1054.2
            115900     ADD 1 TO REC-CT.                                             DB1054.2
            116000 PROC-000-PFM-C-15.                                               DB1054.2
            116100     MOVE 0 TO INCREMENT.                                         DB1054.2
            116200     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            116300     MOVE     "RETURN MECHANISM LOST" TO RE-MARK.                 DB1054.2
            116400     GO       TO PROC-218-PFM-WRITE-15.                           DB1054.2
            116500 PROC-000-PFM-D-15.                                               DB1054.2
            116600     MOVE 0 TO INCREMENT.                                         DB1054.2
            116700     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            116800     MOVE     "PERFORM GOT LOST IN GO TOS" TO RE-MARK.            DB1054.2
            116900     GO       TO PROC-218-PFM-WRITE-15.                           DB1054.2
            117000 PROC-215-PFM-E-15.                                               DB1054.2
            117100     MOVE 0 TO INCREMENT.                                         DB1054.2
            117200     GO       TO PROC-216-PFM-L-15.                               DB1054.2
            117300 PROC-000-PFM-F-15.                                               DB1054.2
            117400     MOVE 0 TO INCREMENT.                                         DB1054.2
            117500     GO       TO PROC-000-PFM-D-15.                               DB1054.2
            117600 PROC-213-PFM-G-15 SECTION.                                       DB1054.2
            117700 PROC-214-PFM-H-15.                                               DB1054.2
            117800     MOVE 0 TO INCREMENT.                                         DB1054.2
            117900     GO       TO PROC-215-PFM-E-15.                               DB1054.2
            118000 PROC-000-PFM-I-15.                                               DB1054.2
            118100     MOVE 0 TO INCREMENT.                                         DB1054.2
            118200     GO       TO PROC-000-PFM-D-15.                               DB1054.2
            118300 PROC-000-PFM-J-15 SECTION.                                       DB1054.2
            118400 PROC-000-PFM-K-15.                                               DB1054.2
            118500     MOVE 0 TO INCREMENT.                                         DB1054.2
            118600     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            118700     MOVE     "PROC-000-PFM-K-15 ENTERED" TO RE-MARK.             DB1054.2
            118800     GO       TO PROC-218-PFM-WRITE-15.                           DB1054.2
            118900 PROC-216-PFM-L-15.                                               DB1054.2
            119000     MOVE 0 TO INCREMENT.                                         DB1054.2
            119100     GO       TO PROC-217-PFM-B-15.                               DB1054.2
            119200 PROC-218-PFM-WRITE-15.                                           DB1054.2
            119300     MOVE 0 TO INCREMENT.                                         DB1054.2
            119400     MOVE     "PERFORM GO TO PARAS" TO FEATURE.                   DB1054.2
            119500     MOVE     "PROC-212-PFM-TEST-15" TO PAR-NAME.                 DB1054.2
            119600 PROC-219-PFM-TEST-LAST.                                          DB1054.2
            119700     MOVE 0 TO INCREMENT.                                         DB1054.2
            119800     MOVE     7 TO PERFORM5.                                      DB1054.2
            119900     PERFORM  PROC-220-PFM-U PERFORM5 TIMES.                      DB1054.2
            120000     MOVE 0 TO INCREMENT.                                         DB1054.2
            120100     IF       PERFORM5 EQUAL TO 707                               DB1054.2
            120200              GO TO PROC-227-PFM-WRITE-LAST.                      DB1054.2
            120300     GO       TO PROC-000-PFM-NUC-FAIL-LST.                       DB1054.2
            120400 PROC-000-PFM-DELETE-LAST.                                        DB1054.2
            120500     MOVE 0 TO INCREMENT.                                         DB1054.2
            120600     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            120700     GO       TO PROC-227-PFM-WRITE-LAST.                         DB1054.2
            120800 PROC-000-PFM-NUC-FAIL-LST.                                       DB1054.2
            120900     MOVE 0 TO INCREMENT.                                         DB1054.2
            121000     MOVE     PERFORM5 TO COMPUTED-N.                             DB1054.2
            121100     MOVE     707 TO CORRECT-N.                                   DB1054.2
            121200     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            121300 PROC-227-PFM-WRITE-LAST.                                         DB1054.2
            121400     IF PROC-ACTIVE IS NOT EQUAL TO 0                             DB1054.2
            121500         MOVE " DEBUGGING PROCEDURE WAS NEVER EXECUTED.  NO FURTHEDB1054.2
            121600-             "R REPORT WILL BE GENERATED." TO DUMMY-RECORD       DB1054.2
            121700         PERFORM WRITE-LINE                                       DB1054.2
            121800         MOVE 227 TO ERROR-COUNTER                                DB1054.2
            121900         GO TO CCVS-EXIT.                                         DB1054.2
            122000     MOVE 0 TO INCREMENT.                                         DB1054.2
            122100     MOVE SPACE TO TEST-RESULTS.                                  DB1054.2
            122200     MOVE "DEBUG ALL PROCEDURES" TO FEATURE.                      DB1054.2
            122300     MOVE 0 TO REC-CT.                                            DB1054.2
            122400     MOVE 1 TO BYPASS.                                            DB1054.2
            122500     SET STACK-END TO STACK-INDEX.                                DB1054.2
            122600     SET STACK-INDEX TO 1.                                        DB1054.2
            122700     SET STATIC-INDEX TO 1.                                       DB1054.2
            122800     PERFORM NAME-LISTER 227 TIMES.                               DB1054.2
            122900 EMPTY-THE-STACK.                                                 DB1054.2
            123000     IF STACK-INDEX IS NOT LESS THAN STACK-END GO TO STACK-EMPTY. DB1054.2
            123100     PERFORM CORRECT-GT-STACK.                                    DB1054.2
            123200     GO TO EMPTY-THE-STACK.                                       DB1054.2
            123300 STACK-EMPTY.                                                     DB1054.2
            123400     MOVE SPACE TO DUMMY-RECORD.                                  DB1054.2
            123500     MOVE 51 TO RECORD-COUNT.                                     DB1054.2
            123600     PERFORM WRITE-LINE.                                          DB1054.2
            123700     MOVE FLOW-FAILURE-1 TO DUMMY-RECORD.                         DB1054.2
            123800     PERFORM WRITE-LINE.                                          DB1054.2
            123900     MOVE FLOW-FAILURE-2 TO DUMMY-RECORD.                         DB1054.2
            124000     PERFORM WRITE-LINE 2 TIMES.                                  DB1054.2
            124100*                                                                 DB1054.2
            124200     GO TO    CCVS-EXIT.                                          DB1054.2
            124300 NAME-LISTER SECTION.                                             DB1054.2
            124400 NAME-LISTER-1.                                                   DB1054.2
            124500     MOVE EXPECTED-NAME (STATIC-INDEX) TO TABLE-ENTRY.            DB1054.2
            124600     SET TABLE-ENTRY-BASE TO STATIC-INDEX.                        DB1054.2
            124700     IF STACK-INDEX IS EQUAL TO STACK-END GO TO STACK-GT-CORRECT. DB1054.2
            124800     IF BASE-NUMBER (STACK-INDEX) IS NOT NUMERIC                  DB1054.2
            124900         GO TO CORRECT-GT-STACK.                                  DB1054.2
            125000     IF TABLE-ENTRY-BASE IS GREATER THAN BASE-NUMBER (STACK-INDEX)DB1054.2
            125100         GO TO CORRECT-GT-STACK.                                  DB1054.2
            125200     IF TABLE-ENTRY-BASE IS LESS THAN BASE-NUMBER (STACK-INDEX)   DB1054.2
            125300         GO TO STACK-GT-CORRECT.                                  DB1054.2
            125400 CORRECT-EQ-STACK.                                                DB1054.2
            125500     MOVE EXPECTED-NAME (STATIC-INDEX) TO PAR-NAME.               DB1054.2
            125600     MOVE TABLE-ENTRY TO CORRECT-A.                               DB1054.2
            125700     MOVE PROC-NAME (STACK-INDEX) TO COMPUTED-A.                  DB1054.2
            125800     IF CORRECT-A IS NOT EQUAL TO COMPUTED-A PERFORM FAIL         DB1054.2
            125900                                        ELSE PERFORM PASS.        DB1054.2
            126000     PERFORM PRINT-DETAIL.                                        DB1054.2
            126100     IF STACK-INDEX IS LESS THAN 500                              DB1054.2
            126200             SET STACK-INDEX UP BY 1.                             DB1054.2
            126300     IF STATIC-INDEX IS LESS THAN 227                             DB1054.2
            126400             SET STATIC-INDEX UP BY 1.                            DB1054.2
            126500     GO TO NAME-LISTER-EXIT.                                      DB1054.2
            126600 CORRECT-GT-STACK.                                                DB1054.2
            126700     MOVE PROC-NAME (STACK-INDEX) TO COMPUTED-A.                  DB1054.2
            126800     PERFORM FAIL.                                                DB1054.2
            126900     PERFORM PRINT-DETAIL.                                        DB1054.2
            127000     IF STACK-INDEX IS LESS THAN 500                              DB1054.2
            127100             SET STACK-INDEX UP BY 1.                             DB1054.2
            127200 CORRECT-GT-STACK-1.                                              DB1054.2
            127300     GO TO NAME-LISTER-1.                                         DB1054.2
            127400 STACK-GT-CORRECT.                                                DB1054.2
            127500     MOVE EXPECTED-NAME (STATIC-INDEX) TO PAR-NAME.               DB1054.2
            127600     MOVE TABLE-ENTRY TO CORRECT-A.                               DB1054.2
            127700     PERFORM FAIL.                                                DB1054.2
            127800     PERFORM PRINT-DETAIL.                                        DB1054.2
            127900     IF STATIC-INDEX IS LESS THAN 227                             DB1054.2
            128000             SET STATIC-INDEX UP BY 1.                            DB1054.2
            128100 NAME-LISTER-EXIT.                                                DB1054.2
            128200     EXIT.                                                        DB1054.2
            128300 OTHER-PROCEDURES SECTION.                                        DB1054.2
            128400 PROC-000-NUCLEUS-FAILURE.                                        DB1054.2
            128500     ADD 1 TO NUC-FAILURE-COUNT.                                  DB1054.2
            128600 PROC-209-A121.                                                   DB1054.2
            128700     EXIT.                                                        DB1054.2
            128800 PROC-208-A120.                                                   DB1054.2
            128900     MOVE 0 TO INCREMENT.                                         DB1054.2
            129000     MOVE "T" TO TEST-LETTER (20).                                DB1054.2
            129100     PERFORM PROC-209-A121.                                       DB1054.2
            129200 PROC-207-A119.                                                   DB1054.2
            129300     MOVE 0 TO INCREMENT.                                         DB1054.2
            129400     MOVE "S" TO TEST-LETTER (19).                                DB1054.2
            129500     PERFORM PROC-208-A120.                                       DB1054.2
            129600 PROC-206-A118.                                                   DB1054.2
            129700     MOVE 0 TO INCREMENT.                                         DB1054.2
            129800     MOVE "R" TO TEST-LETTER (18).                                DB1054.2
            129900     PERFORM PROC-207-A119.                                       DB1054.2
            130000 PROC-205-A117.                                                   DB1054.2
            130100     MOVE 0 TO INCREMENT.                                         DB1054.2
            130200     MOVE "Q" TO TEST-LETTER (17).                                DB1054.2
            130300     PERFORM PROC-206-A118.                                       DB1054.2
            130400 PROC-204-A116.                                                   DB1054.2
            130500     MOVE 0 TO INCREMENT.                                         DB1054.2
            130600     MOVE "P" TO TEST-LETTER (16).                                DB1054.2
            130700     PERFORM PROC-205-A117.                                       DB1054.2
            130800 PROC-203-A115.                                                   DB1054.2
            130900     MOVE 0 TO INCREMENT.                                         DB1054.2
            131000     MOVE "O" TO TEST-LETTER (15).                                DB1054.2
            131100     PERFORM PROC-204-A116.                                       DB1054.2
            131200 PROC-202-A114.                                                   DB1054.2
            131300     MOVE 0 TO INCREMENT.                                         DB1054.2
            131400     MOVE "N" TO TEST-LETTER (14).                                DB1054.2
            131500     PERFORM PROC-203-A115.                                       DB1054.2
            131600 PROC-201-A113.                                                   DB1054.2
            131700     MOVE 0 TO INCREMENT.                                         DB1054.2
            131800     MOVE "M" TO TEST-LETTER (13).                                DB1054.2
            131900     PERFORM PROC-202-A114.                                       DB1054.2
            132000 PROC-200-A112.                                                   DB1054.2
            132100     MOVE 0 TO INCREMENT.                                         DB1054.2
            132200     MOVE "L" TO TEST-LETTER (12).                                DB1054.2
            132300     PERFORM PROC-201-A113.                                       DB1054.2
            132400 PROC-199-A111.                                                   DB1054.2
            132500     MOVE 0 TO INCREMENT.                                         DB1054.2
            132600     MOVE "K" TO TEST-LETTER (11).                                DB1054.2
            132700     PERFORM PROC-200-A112.                                       DB1054.2
            132800 PROC-198-A110.                                                   DB1054.2
            132900     MOVE 0 TO INCREMENT.                                         DB1054.2
            133000     MOVE "J" TO TEST-LETTER (10).                                DB1054.2
            133100     PERFORM PROC-199-A111.                                       DB1054.2
            133200 PROC-197-A109.                                                   DB1054.2
            133300     MOVE 0 TO INCREMENT.                                         DB1054.2
            133400     MOVE "I" TO TEST-LETTER (9).                                 DB1054.2
            133500     PERFORM PROC-198-A110.                                       DB1054.2
            133600 PROC-196-A108.                                                   DB1054.2
            133700     MOVE 0 TO INCREMENT.                                         DB1054.2
            133800     MOVE "H" TO TEST-LETTER (8).                                 DB1054.2
            133900     PERFORM PROC-197-A109.                                       DB1054.2
            134000 PROC-195-A107.                                                   DB1054.2
            134100     MOVE 0 TO INCREMENT.                                         DB1054.2
            134200     MOVE "G" TO TEST-LETTER (7).                                 DB1054.2
            134300     PERFORM PROC-196-A108.                                       DB1054.2
            134400 PROC-194-A106.                                                   DB1054.2
            134500     MOVE 0 TO INCREMENT.                                         DB1054.2
            134600     MOVE "F" TO TEST-LETTER (6).                                 DB1054.2
            134700     PERFORM PROC-195-A107.                                       DB1054.2
            134800 PROC-193-A105.                                                   DB1054.2
            134900     MOVE 0 TO INCREMENT.                                         DB1054.2
            135000     MOVE "E" TO TEST-LETTER (5).                                 DB1054.2
            135100     PERFORM PROC-194-A106.                                       DB1054.2
            135200 PROC-192-A104.                                                   DB1054.2
            135300     MOVE 0 TO INCREMENT.                                         DB1054.2
            135400     MOVE "D" TO TEST-LETTER (4).                                 DB1054.2
            135500     PERFORM PROC-193-A105.                                       DB1054.2
            135600 PROC-191-A103.                                                   DB1054.2
            135700     MOVE 0 TO INCREMENT.                                         DB1054.2
            135800     MOVE "C" TO TEST-LETTER (3).                                 DB1054.2
            135900     PERFORM PROC-192-A104.                                       DB1054.2
            136000 PROC-190-A102.                                                   DB1054.2
            136100     MOVE 0 TO INCREMENT.                                         DB1054.2
            136200     MOVE "B" TO TEST-LETTER (2).                                 DB1054.2
            136300     PERFORM PROC-191-A103.                                       DB1054.2
            136400 PROC-189-A101.                                                   DB1054.2
            136500     MOVE 0 TO INCREMENT.                                         DB1054.2
            136600     MOVE "A" TO TEST-LETTER (1).                                 DB1054.2
            136700     PERFORM PROC-190-A102.                                       DB1054.2
            136800 PROC-048-PFM-C.                                                  DB1054.2
            136900     ADD 1 TO INCREMENT.                                          DB1054.2
            137000     ADD      6 TO PERFORM2.                                      DB1054.2
            137100 PROC-000-PFM-D.                                                  DB1054.2
            137200     MOVE 0 TO INCREMENT.                                         DB1054.2
            137300     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            137400     GO TO    PROC-055-PFM-TEST-4.                                DB1054.2
            137500*    NOTE CONTROL SHOULD NOT PASS TO THIS PARAGRAPH               DB1054.2
            137600*    FROM THE PREVIOUS ONE.                                       DB1054.2
            137700 PROC-056-PFM-E.                                                  DB1054.2
            137800     MOVE 0 TO INCREMENT.                                         DB1054.2
            137900     MOVE "CSW" TO PERFORM1.                                      DB1054.2
            138000     PERFORM  PROC-057-PFM-F THRU PROC-058-PFM-G.                 DB1054.2
            138100     SUBTRACT .8 FROM PERFORM4.                                   DB1054.2
            138200     GO TO    PROC-059-PFM-H.                                     DB1054.2
            138300 PROC-057-PFM-F.                                                  DB1054.2
            138400     MOVE 0 TO INCREMENT.                                         DB1054.2
            138500     MOVE     60.5 TO PERFORM4.                                   DB1054.2
            138600 PROC-058-PFM-G.                                                  DB1054.2
            138700     MOVE 0 TO INCREMENT.                                         DB1054.2
            138800     ADD      10.3 TO PERFORM4.                                   DB1054.2
            138900 PROC-059-PFM-H.                                                  DB1054.2
            139000     EXIT.                                                        DB1054.2
            139100 PROC-000-PFM-I.                                                  DB1054.2
            139200     MOVE 0 TO INCREMENT.                                         DB1054.2
            139300     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            139400     GO TO    PROC-060-PFM-WRITE-4.                               DB1054.2
            139500*    NOTE CONTROL SHOULD NOT PASS TO THIS PARAGRAPH               DB1054.2
            139600*    FROM THE PREVIOUS ONE.                                       DB1054.2
            139700 PROC-062-PFM-J.                                                  DB1054.2
            139800     MOVE 0 TO INCREMENT.                                         DB1054.2
            139900     MOVE "YES" TO PERFORM1.                                      DB1054.2
            140000     PERFORM  PROC-063-PFM-L.                                     DB1054.2
            140100     MULTIPLY 3 BY PERFORM2.                                      DB1054.2
            140200 PROC-000-PFM-K.                                                  DB1054.2
            140300     MOVE 0 TO INCREMENT.                                         DB1054.2
            140400     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            140500     GO TO    PROC-064-PFM-WRITE-5.                               DB1054.2
            140600*    NOTE CONTROL SHOULD NOT PASS TO THIS PARAGRAPH               DB1054.2
            140700*    FROM THE PREVIOUS ONE.                                       DB1054.2
            140800 PROC-063-PFM-L.                                                  DB1054.2
            140900     MOVE 0 TO INCREMENT.                                         DB1054.2
            141000     MOVE     4 TO PERFORM2.                                      DB1054.2
            141100     ADD      100 TO PERFORM2.                                    DB1054.2
            141200 PROC-000-PFM-M.                                                  DB1054.2
            141300     MOVE 0 TO INCREMENT.                                         DB1054.2
            141400     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            141500     GO TO    PROC-064-PFM-WRITE-5.                               DB1054.2
            141600*    NOTE CONTROL SHOULD NOT PASS TO THIS PARAGRAPH               DB1054.2
            141700*    FROM THE PREVIOUS ONE.                                       DB1054.2
            141800 PROC-066-PFM-N        SECTION.                                   DB1054.2
            141900 PROC-067-PFM-O.                                                  DB1054.2
            142000     MOVE 0 TO INCREMENT.                                         DB1054.2
            142100     ADD 1 TO NUC-FAILURE-COUNT.                                  DB1054.2
            142200 PROC-068-PFM-P.                                                  DB1054.2
            142300     MOVE 0 TO INCREMENT.                                         DB1054.2
            142400     SUBTRACT 1 FROM NUC-FAILURE-COUNT.                           DB1054.2
            142500 PROC-000-PFM-Q        SECTION.                                   DB1054.2
            142600 PROC-000-PFM-R.                                                  DB1054.2
            142700     MOVE 0 TO INCREMENT.                                         DB1054.2
            142800     PERFORM  PROC-000-NUCLEUS-FAILURE.                           DB1054.2
            142900     GO TO    PROC-069-PFM-WRITE-6.                               DB1054.2
            143000*    NOTE CONTROL SHOULD NOT PASS TO THIS PARAGRAPH FROM THE      DB1054.2
            143100*        PREVIOUS ONE.                                            DB1054.2
            143200 PROC-000-PFM-S.                                                  DB1054.2
            143300     MOVE 0 TO INCREMENT.                                         DB1054.2
            143400     ADD      1 TO PERFORM5.                                      DB1054.2
            143500 PROC-000-PFM-T.                                                  DB1054.2
            143600     MOVE 0 TO INCREMENT.                                         DB1054.2
            143700     ADD      10 TO PERFORM5.                                     DB1054.2
            143800 PROC-220-PFM-U.                                                  DB1054.2
            143900     ADD 1 TO INCREMENT.                                          DB1054.2
            144000     ADD      100 TO PERFORM5.                                    DB1054.2
            144100     IF       PERFORM5 GREATER THAN 899                           DB1054.2
            144200              MOVE PERFORM5 TO COMPUTED-N                         DB1054.2
            144300              MOVE 707 TO CORRECT-N                               DB1054.2
            144400              PERFORM PROC-000-NUCLEUS-FAILURE.                   DB1054.2
            144500 PROC-071-PFM-V.       EXIT.                                      DB1054.2
            144600 PROC-072-PFM-W.       EXIT.                                      DB1054.2
            144700 PROC-073-PFM-X.       EXIT.                                      DB1054.2
            144800 PROC-074-PFM-Y.       EXIT.                                      DB1054.2
            144900 PROC-075-PFM-Z.       ADD  5  TO INCREMENT.                      DB1054.2
            145000 CCVS-EXIT SECTION.                                               DB1054.2
            145100 CCVS-999999.                                                     DB1054.2
            145200     GO TO CLOSE-FILES.                                           DB1054.2
                  *END-OF,DB105A                                                            
        """)
    )

    @Disabled("Requires Y indicator")
    @Test
    fun db2014_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB201A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB2014.2
            000200 PROGRAM-ID.                                                      DB2014.2
            000300     DB201A.                                                      DB2014.2
            000400 AUTHOR.                                                          DB2014.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB2014.2
            000600 INSTALLATION.                                                    DB2014.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB2014.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB2014.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB2014.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB2014.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB2014.2
            001200                                                                  DB2014.2
            001300     PHONE   (703) 756-6153                                       DB2014.2
            001400                                                                  DB2014.2
            001500     " HIGH       ".                                              DB2014.2
            001600 DATE-WRITTEN.                                                    DB2014.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB2014.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB2014.2
            001900     "4.2 ".                                                      DB2014.2
            002000 SECURITY.                                                        DB2014.2
            002100     NONE.                                                        DB2014.2
            002200*                                                                 DB2014.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB2014.2
            002400*                                                                 DB2014.2
            002500*                       PROGRAM ABSTRACT                          DB2014.2
            002600*                                                                 DB2014.2
            002700*    DB201A TESTS THE CAPABILITY OF THE DEBUG MODULE TO HANDLE    DB2014.2
            002800*    DEBUGGING PROCEDURES WHICH ARE MONITORING IDENTIFIERS        DB2014.2
            002900*    SPECIFIED WITH AND WITHOUT THE "ALL REFERENCES" OPTION.      DB2014.2
            003000*    THIS PROGRAM IS TO BE COMPILED AND EXECUTED WITH BOTH        DB2014.2
            003100*    COMPILE AND OBJECT TIME DEBUGGING SWITCHES ENABLED.  THE     DB2014.2
            003200*    DEBUGGING PROCEDURES SHOULD BE INCLUDED IN COMPILATION       DB2014.2
            003300*    AND GENERATE CODE.  DEBUGGING ACTIONS ON THE FOLLOWING       DB2014.2
            003400*    CONDITIONS ARE ANALYZED                                      DB2014.2
            003500*                                                                 DB2014.2
            003600*        (1)  REFERENCE TO IDENTIFIER WITHIN "VARYING", "AFTER",  DB2014.2
            003700*             AND "UNTIL" PHRASES OF "PERFORM" STATEMENTS.        DB2014.2
            003800*        (2)  REFERENCE TO CHANGED AND UNCHANGED IDENTIFIER       DB2014.2
            003900*             FIELDS.                                             DB2014.2
            004000*        (3)  REFERENCE TO SUBSCRIPTED IDENTIFIERS.               DB2014.2
            004100*        (4)  REFERENCE TO QUALIFIED IDENTIFIERS.                 DB2014.2
            004200*        (5)  REFERENCE TO IDENTIFIER USED IN "GO TO DEPENDING".  DB2014.2
            004300*        (6)  REFERENCE TO IDENTIFIER IN UNEXECUTED STATEMENTS.   DB2014.2
            004400*        (7)  MULTIPLE REFERENCES TO SAME IDENTIFIER IN SAME      DB2014.2
            004500*             STATEMENT.                                          DB2014.2
            004600*                                                                 DB2014.2
            004700*                                                                 DB2014.2
            004800*                                                                 DB2014.2
            004900 ENVIRONMENT DIVISION.                                            DB2014.2
            005000 CONFIGURATION SECTION.                                           DB2014.2
            005100 SOURCE-COMPUTER.                                                 DB2014.2
            005200     XXXXX082                                                     DB2014.2
            005300         WITH DEBUGGING MODE.                                     DB2014.2
            005400 OBJECT-COMPUTER.                                                 DB2014.2
            005500     XXXXX083.                                                    DB2014.2
            005600 INPUT-OUTPUT SECTION.                                            DB2014.2
            005700 FILE-CONTROL.                                                    DB2014.2
            005800     SELECT PRINT-FILE ASSIGN TO                                  DB2014.2
            005900     XXXXX055.                                                    DB2014.2
            006000 DATA DIVISION.                                                   DB2014.2
            006100 FILE SECTION.                                                    DB2014.2
            006200 FD  PRINT-FILE                                                   DB2014.2
            006300     LABEL RECORDS                                                DB2014.2
            006400     XXXXX084                                                     DB2014.2
            006500     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB2014.2
            006600 01  PRINT-REC PICTURE X(120).                                    DB2014.2
            006700 01  DUMMY-RECORD PICTURE X(120).                                 DB2014.2
            006800 WORKING-STORAGE SECTION.                                         DB2014.2
            006900 77  COUNTER PIC 999 VALUE 0.                                     DB2014.2
            007000 77  GO-TO-DEP-KEY PIC 9.                                         DB2014.2
            007100 77  GO-TO-DEP-KEY-1 PIC 9.                                       DB2014.2
            007200 01  ITEM-1.                                                      DB2014.2
            007300     02  KEY-1 PIC 99   VALUE ZERO.                               DB2014.2
            007400     02  LINE-1 PIC X(6).                                         DB2014.2
            007500     02  NAME-1 PIC X(30).                                        DB2014.2
            007600     02  UNQUAL-NAME-1 PIC X(30).                                 DB2014.2
            007700     02  SUB-1-1 PIC X(5).                                        DB2014.2
            007800     02  SUB-2-1 PIC X(5).                                        DB2014.2
            007900     02  SUB-3-1 PIC X(5).                                        DB2014.2
            008000     02  CONTENTS-1 PIC X(30).                                    DB2014.2
            008100 01  ITEM-2.                                                      DB2014.2
            008200     02  KEY-2 PIC 99.                                            DB2014.2
            008300     02  LINE-2 PIC X(6).                                         DB2014.2
            008400     02  NAME-2 PIC X(30).                                        DB2014.2
            008500     02  UNQUAL-NAME-2 PIC X(30).                                 DB2014.2
            008600     02  CONTENTS-2 PIC X(30).                                    DB2014.2
            008700 01  ID-1 PIC 99.                                                 DB2014.2
            008800 01  ID-1A REDEFINES ID-1 PIC 99.                                 DB2014.2
            008900 01  ID-2 PIC 99.                                                 DB2014.2
            009000 01  ID-2A REDEFINES ID-2 PIC 99.                                 DB2014.2
            009100 01  ID-3 PIC 99.                                                 DB2014.2
            009200 01  A-GROUP.                                                     DB2014.2
            009300     02  A1.                                                      DB2014.2
            009400         03  AB1 PIC X OCCURS 5 TIMES.                            DB2014.2
            009500         03  AB2.                                                 DB2014.2
            009600             04  ABC1 PIC X.                                      DB2014.2
            009700             04  ABC2 PIC X.                                      DB2014.2
            009800     02  A2.                                                      DB2014.2
            009900         03  AB1 PIC X OCCURS 5 TIMES.                            DB2014.2
            010000         03  AB2.                                                 DB2014.2
            010100             04  ABC1 PIC X.                                      DB2014.2
            010200             04  ABC2 PIC X.                                      DB2014.2
            010300 01  B-GROUP.                                                     DB2014.2
            010400     02  B-LEVEL-1 OCCURS 10 TIMES INDEXED BY I.                  DB2014.2
            010500         03  B-LEVEL-2 OCCURS 10 TIMES INDEXED BY J.              DB2014.2
            010600             04  B-LEVEL-3 OCCURS 10 TIMES INDEXED BY K PIC X.    DB2014.2
            010700 01  TEST-RESULTS.                                                DB2014.2
            010800     02 FILLER                    PICTURE X VALUE SPACE.          DB2014.2
            010900     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB2014.2
            011000     02 FILLER                    PICTURE X VALUE SPACE.          DB2014.2
            011100     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB2014.2
            011200     02 FILLER                    PICTURE X  VALUE SPACE.         DB2014.2
            011300     02  PAR-NAME.                                                DB2014.2
            011400       03 FILLER PICTURE X(12) VALUE SPACE.                       DB2014.2
            011500       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB2014.2
            011600       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB2014.2
            011700       03 FILLER PIC X(5) VALUE SPACE.                            DB2014.2
            011800     02 FILLER PIC X(10) VALUE SPACE.                             DB2014.2
            011900     02 RE-MARK PIC X(61).                                        DB2014.2
            012000 01  TEST-COMPUTED.                                               DB2014.2
            012100     02 FILLER PIC X(30) VALUE SPACE.                             DB2014.2
            012200     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB2014.2
            012300     02 COMPUTED-X.                                               DB2014.2
            012400     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB2014.2
            012500     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB2014.2
            012600     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB2014.2
            012700     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB2014.2
            012800     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB2014.2
            012900     03       CM-18V0 REDEFINES COMPUTED-A.                       DB2014.2
            013000         04 COMPUTED-18V0                   PICTURE -9(18).       DB2014.2
            013100         04 FILLER                          PICTURE X.            DB2014.2
            013200     03 FILLER PIC X(50) VALUE SPACE.                             DB2014.2
            013300 01  TEST-CORRECT.                                                DB2014.2
            013400     02 FILLER PIC X(30) VALUE SPACE.                             DB2014.2
            013500     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB2014.2
            013600     02 CORRECT-X.                                                DB2014.2
            013700     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB2014.2
            013800     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB2014.2
            013900     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB2014.2
            014000     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB2014.2
            014100     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB2014.2
            014200     03      CR-18V0 REDEFINES CORRECT-A.                         DB2014.2
            014300         04 CORRECT-18V0                    PICTURE -9(18).       DB2014.2
            014400         04 FILLER                          PICTURE X.            DB2014.2
            014500     03 FILLER PIC X(50) VALUE SPACE.                             DB2014.2
            014600 01  CCVS-C-1.                                                    DB2014.2
            014700     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB2014.2
            014800-    "SS  PARAGRAPH-NAME                                          DB2014.2
            014900-    "        REMARKS".                                           DB2014.2
            015000     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB2014.2
            015100 01  CCVS-C-2.                                                    DB2014.2
            015200     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2014.2
            015300     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB2014.2
            015400     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB2014.2
            015500     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB2014.2
            015600     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB2014.2
            015700 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB2014.2
            015800 01  REC-CT PICTURE 99 VALUE ZERO.                                DB2014.2
            015900 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB2014.2
            016000 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB2014.2
            016100 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB2014.2
            016200 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB2014.2
            016300 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB2014.2
            016400 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB2014.2
            016500 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB2014.2
            016600 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB2014.2
            016700 01  CCVS-H-1.                                                    DB2014.2
            016800     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB2014.2
            016900     02 FILLER PICTURE X(67) VALUE                                DB2014.2
            017000     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB2014.2
            017100-    " SYSTEM".                                                   DB2014.2
            017200     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB2014.2
            017300 01  CCVS-H-2.                                                    DB2014.2
            017400     02 FILLER PICTURE X(52) VALUE IS                             DB2014.2
            017500     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB2014.2
            017600     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB2014.2
            017700     02 TEST-ID PICTURE IS X(9).                                  DB2014.2
            017800     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB2014.2
            017900 01  CCVS-H-3.                                                    DB2014.2
            018000     02  FILLER PICTURE X(34) VALUE                               DB2014.2
            018100     " FOR OFFICIAL USE ONLY    ".                                DB2014.2
            018200     02  FILLER PICTURE X(58) VALUE                               DB2014.2
            018300     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB2014.2
            018400     02  FILLER PICTURE X(28) VALUE                               DB2014.2
            018500     "  COPYRIGHT   1974 ".                                       DB2014.2
            018600 01  CCVS-E-1.                                                    DB2014.2
            018700     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB2014.2
            018800     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB2014.2
            018900     02 ID-AGAIN PICTURE IS X(9).                                 DB2014.2
            019000     02 FILLER PICTURE X(45) VALUE IS                             DB2014.2
            019100     " NTIS DISTRIBUTION COBOL 74".                               DB2014.2
            019200 01  CCVS-E-2.                                                    DB2014.2
            019300     02  FILLER                   PICTURE X(31)  VALUE            DB2014.2
            019400     SPACE.                                                       DB2014.2
            019500     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB2014.2
            019600     02 CCVS-E-2-2.                                               DB2014.2
            019700         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB2014.2
            019800         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB2014.2
            019900         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB2014.2
            020000 01  CCVS-E-3.                                                    DB2014.2
            020100     02  FILLER PICTURE X(22) VALUE                               DB2014.2
            020200     " FOR OFFICIAL USE ONLY".                                    DB2014.2
            020300     02  FILLER PICTURE X(12) VALUE SPACE.                        DB2014.2
            020400     02  FILLER PICTURE X(58) VALUE                               DB2014.2
            020500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB2014.2
            020600     02  FILLER PICTURE X(13) VALUE SPACE.                        DB2014.2
            020700     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB2014.2
            020800 01  CCVS-E-4.                                                    DB2014.2
            020900     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB2014.2
            021000     02 FILLER PIC XXXX VALUE " OF ".                             DB2014.2
            021100     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB2014.2
            021200     02 FILLER PIC X(40) VALUE                                    DB2014.2
            021300      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB2014.2
            021400 01  XXINFO.                                                      DB2014.2
            021500     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB2014.2
            021600     02 INFO-TEXT.                                                DB2014.2
            021700     04 FILLER PIC X(20) VALUE SPACE.                             DB2014.2
            021800     04 XXCOMPUTED PIC X(20).                                     DB2014.2
            021900     04 FILLER PIC X(5) VALUE SPACE.                              DB2014.2
            022000     04 XXCORRECT PIC X(20).                                      DB2014.2
            022100 01  HYPHEN-LINE.                                                 DB2014.2
            022200     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2014.2
            022300     02 FILLER PICTURE IS X(65) VALUE IS "************************DB2014.2
            022400-    "*****************************************".                 DB2014.2
            022500     02 FILLER PICTURE IS X(54) VALUE IS "************************DB2014.2
            022600-    "******************************".                            DB2014.2
            022700 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB2014.2
            022800     "DB201A".                                                    DB2014.2
            022900 PROCEDURE DIVISION.                                              DB2014.2
            023000 DECLARATIVES.                                                    DB2014.2
            023100 GO-TO-DEPENDING-PROC-1 SECTION.                                  DB2014.2
            023200     USE FOR DEBUGGING ON  ALL REFERENCES OF GO-TO-DEP-KEY        DB2014.2
            023300                           ALL               ID-2.                DB2014.2
            023400 GO-TO-DEPEND-1.                                                  DB2014.2
            023500     ADD 1 TO KEY-1.                                              DB2014.2
            023600 DB-COMMON-1.                                                     DB2014.2
            023700     MOVE DEBUG-LINE TO LINE-1.                                   DB2014.2
            023800     MOVE DEBUG-NAME TO NAME-1  UNQUAL-NAME-1.                    DB2014.2
            023900     MOVE DEBUG-CONTENTS TO CONTENTS-1.                           DB2014.2
            024000 DB-CLEAR-QUALIFIER-1.                                            DB2014.2
            024100     INSPECT UNQUAL-NAME-1 REPLACING CHARACTERS BY SPACES         DB2014.2
            024200         AFTER INITIAL SPACE.                                     DB2014.2
            024300 GO-TO-DEPENDING-PROC-2 SECTION.                                  DB2014.2
            024400     USE FOR DEBUGGING ON G-T-D-2.                                DB2014.2
            024500 GO-TO-DEPEND-2.                                                  DB2014.2
            024600     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            024700         MOVE 2 TO KEY-2                                          DB2014.2
            024800         ELSE MOVE 1 TO KEY-2.                                    DB2014.2
            024900 DB-COMMON-2.                                                     DB2014.2
            025000     MOVE DEBUG-LINE TO LINE-2.                                   DB2014.2
            025100     MOVE DEBUG-NAME TO NAME-2  UNQUAL-NAME-2.                    DB2014.2
            025200     MOVE DEBUG-CONTENTS TO CONTENTS-2.                           DB2014.2
            025300 DB-CLEAR-QUALIFIER-2.                                            DB2014.2
            025400     INSPECT UNQUAL-NAME-2 REPLACING CHARACTERS BY SPACES         DB2014.2
            025500         AFTER INITIAL SPACE.                                     DB2014.2
            025600 GO-TO-DEPENDING-PROC-3 SECTION.                                  DB2014.2
            025700     USE FOR DEBUGGING ON GO-TO-DEP-KEY-1.                        DB2014.2
            025800 GO-TO-DEPEND-3.                                                  DB2014.2
            025900     MOVE 1 TO KEY-1.                                             DB2014.2
            026000 PERFORM-PROC-1 SECTION.                                          DB2014.2
            026100     USE FOR DEBUGGING ON ID-1.                                   DB2014.2
            026200 PERFORM-1.                                                       DB2014.2
            026300     ADD 1 TO KEY-1.                                              DB2014.2
            026400     PERFORM DB-COMMON-1.                                         DB2014.2
            026500     PERFORM DB-CLEAR-QUALIFIER-1.                                DB2014.2
            026600 TABLE-PROC-1 SECTION.                                            DB2014.2
            026700     USE FOR DEBUGGING ON  B-LEVEL-1  B-LEVEL-2  B-LEVEL-3.       DB2014.2
            026800 TABLE-1.                                                         DB2014.2
            026900     MOVE 1 TO KEY-1.                                             DB2014.2
            027000     PERFORM DB-COMMON-1.                                         DB2014.2
            027100     PERFORM DB-CLEAR-QUALIFIER-1.                                DB2014.2
            027200 DB-MOVE-SUBSC-1.                                                 DB2014.2
            027300     MOVE DEBUG-SUB-1 TO SUB-1-1.                                 DB2014.2
            027400     IF PAR-NAME = "SUBSC-TEST-2"                                 DB2014.2
            027500          MOVE DEBUG-SUB-2 TO SUB-2-1.                            DB2014.2
            027600     MOVE DEBUG-SUB-3 TO SUB-3-1.                                 DB2014.2
            027700 QUAL-PROC-1 SECTION.                                             DB2014.2
            027800     USE FOR DEBUGGING ON ALL REFERENCES OF ABC1 OF AB2 OF A1     DB2014.2
            027900                          ALL                       AB2 OF A2.    DB2014.2
            028000 QUAL-1.                                                          DB2014.2
            028100     MOVE 1 TO KEY-1.                                             DB2014.2
            028200     PERFORM DB-COMMON-1.                                         DB2014.2
            028300 QUAL-SUBC-PROC-1 SECTION.                                        DB2014.2
            028400     USE FOR DEBUGGING ON ALL REFERENCES OF AB1 OF A1.            DB2014.2
            028500 QUAL-SUBC-1.                                                     DB2014.2
            028600     MOVE 1 TO KEY-1.                                             DB2014.2
            028700     PERFORM DB-COMMON-1.                                         DB2014.2
            028800     MOVE DEBUG-SUB-1 TO SUB-1-1.                                 DB2014.2
            028900 END DECLARATIVES.                                                DB2014.2
            029000 CCVS1 SECTION.                                                   DB2014.2
            029100 OPEN-FILES.                                                      DB2014.2
            029200     OPEN     OUTPUT PRINT-FILE.                                  DB2014.2
            029300     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB2014.2
            029400     MOVE    SPACE TO TEST-RESULTS.                               DB2014.2
            029500     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB2014.2
            029600     GO TO CCVS1-EXIT.                                            DB2014.2
            029700 CLOSE-FILES.                                                     DB2014.2
            029800     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB2014.2
            029900 TERMINATE-CCVS.                                                  DB2014.2
            030000S    EXIT PROGRAM.                                                DB2014.2
            030100STERMINATE-CALL.                                                  DB2014.2
            030200     STOP     RUN.                                                DB2014.2
            030300 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB2014.2
            030400 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB2014.2
            030500 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB2014.2
            030600 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB2014.2
            030700     MOVE "****TEST DELETED****" TO RE-MARK.                      DB2014.2
            030800 PRINT-DETAIL.                                                    DB2014.2
            030900     IF REC-CT NOT EQUAL TO ZERO                                  DB2014.2
            031000             MOVE "." TO PARDOT-X                                 DB2014.2
            031100             MOVE REC-CT TO DOTVALUE.                             DB2014.2
            031200     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB2014.2
            031300     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB2014.2
            031400        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB2014.2
            031500          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB2014.2
            031600     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB2014.2
            031700     MOVE SPACE TO CORRECT-X.                                     DB2014.2
            031800     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB2014.2
            031900     MOVE     SPACE TO RE-MARK.                                   DB2014.2
            032000 HEAD-ROUTINE.                                                    DB2014.2
            032100     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2014.2
            032200     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB2014.2
            032300     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB2014.2
            032400 COLUMN-NAMES-ROUTINE.                                            DB2014.2
            032500     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2014.2
            032600     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2014.2
            032700     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB2014.2
            032800 END-ROUTINE.                                                     DB2014.2
            032900     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB2014.2
            033000 END-RTN-EXIT.                                                    DB2014.2
            033100     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2014.2
            033200 END-ROUTINE-1.                                                   DB2014.2
            033300      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB2014.2
            033400      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB2014.2
            033500      ADD PASS-COUNTER TO ERROR-HOLD.                             DB2014.2
            033600*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB2014.2
            033700      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB2014.2
            033800      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB2014.2
            033900      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB2014.2
            034000      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB2014.2
            034100  END-ROUTINE-12.                                                 DB2014.2
            034200      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB2014.2
            034300     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB2014.2
            034400         MOVE "NO " TO ERROR-TOTAL                                DB2014.2
            034500         ELSE                                                     DB2014.2
            034600         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB2014.2
            034700     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB2014.2
            034800     PERFORM WRITE-LINE.                                          DB2014.2
            034900 END-ROUTINE-13.                                                  DB2014.2
            035000     IF DELETE-CNT IS EQUAL TO ZERO                               DB2014.2
            035100         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB2014.2
            035200         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB2014.2
            035300     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB2014.2
            035400     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2014.2
            035500      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB2014.2
            035600          MOVE "NO " TO ERROR-TOTAL                               DB2014.2
            035700      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB2014.2
            035800      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB2014.2
            035900      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB2014.2
            036000     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2014.2
            036100 WRITE-LINE.                                                      DB2014.2
            036200     ADD 1 TO RECORD-COUNT.                                       DB2014.2
            036300Y    IF RECORD-COUNT GREATER 50                                   DB2014.2
            036400Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB2014.2
            036500Y        MOVE SPACE TO DUMMY-RECORD                               DB2014.2
            036600Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB2014.2
            036700Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB2014.2
            036800Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB2014.2
            036900Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB2014.2
            037000Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB2014.2
            037100Y        MOVE ZERO TO RECORD-COUNT.                               DB2014.2
            037200     PERFORM WRT-LN.                                              DB2014.2
            037300 WRT-LN.                                                          DB2014.2
            037400     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB2014.2
            037500     MOVE SPACE TO DUMMY-RECORD.                                  DB2014.2
            037600 BLANK-LINE-PRINT.                                                DB2014.2
            037700     PERFORM WRT-LN.                                              DB2014.2
            037800 FAIL-ROUTINE.                                                    DB2014.2
            037900     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB2014.2
            038000     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB2014.2
            038100     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB2014.2
            038200     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2014.2
            038300     GO TO FAIL-ROUTINE-EX.                                       DB2014.2
            038400 FAIL-ROUTINE-WRITE.                                              DB2014.2
            038500     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB2014.2
            038600     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB2014.2
            038700 FAIL-ROUTINE-EX. EXIT.                                           DB2014.2
            038800 BAIL-OUT.                                                        DB2014.2
            038900     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB2014.2
            039000     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB2014.2
            039100 BAIL-OUT-WRITE.                                                  DB2014.2
            039200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB2014.2
            039300     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2014.2
            039400 BAIL-OUT-EX. EXIT.                                               DB2014.2
            039500 CCVS1-EXIT.                                                      DB2014.2
            039600     EXIT.                                                        DB2014.2
            039700 SECT-DB201A-0001 SECTION.                                        DB2014.2
            039800 GO-TO-DEPENDING-INIT.                                            DB2014.2
            039900     MOVE "GO TO DEP/ALL REF" TO FEATURE.                         DB2014.2
            040000     MOVE "GO-TO-DEPENDING" TO PAR-NAME.                          DB2014.2
            040100 GO-TO-DEPENDING-0.                                               DB2014.2
            040200     MOVE 2 TO GO-TO-DEP-KEY.                                     DB2014.2
            040300     MOVE SPACES TO ITEM-1  ITEM-2.                               DB2014.2
            040400     MOVE 0 TO KEY-1  KEY-2.                                      DB2014.2
            040500******************************************************************DB2014.2
            040600*    THE DEBUG-LINE (INSPT) TESTS NAMED IN THE OUTPUT REPORT AS  *DB2014.2
            040700*    "G-T-D-2A" AND "G-T-D-3A" SHOULD EACH POINT TO THE          *DB2014.2
            040800*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB2014.2
            040900*    WHICH READS, "GO TO G-T-D-1 G-T-D-2 G-T-D-5 DEPENDING ON    *DB2014.2
            041000*    GO-TO-DEP-KEY.".                                            *DB2014.2
            041100******************************************************************DB2014.2
            041200     GO TO G-T-D-1  G-T-D-2  G-T-D-5  DEPENDING ON GO-TO-DEP-KEY. DB2014.2
            041300 GO-TO-DEPENDING-DELETE.                                          DB2014.2
            041400     GO TO G-T-D-2-DELETE.                                        DB2014.2
            041500 G-T-D-1.                                                         DB2014.2
            041600     GO TO G-T-D-2-DELETE.                                        DB2014.2
            041700 G-T-D-2.                                                         DB2014.2
            041800     MOVE "G-T-D-2" TO PAR-NAME.                                  DB2014.2
            041900     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            042000         MOVE "DEBUG EXECUTED ON IDENTIFR" TO RE-MARK             DB2014.2
            042100         PERFORM PASS                                             DB2014.2
            042200         PERFORM G-T-D-WRITE                                      DB2014.2
            042300         GO TO G-T-D-2A                                           DB2014.2
            042400     ELSE PERFORM FAIL                                            DB2014.2
            042500         MOVE "DEBUG NOT EXECUTED ON IDENTIFR" TO RE-MARK         DB2014.2
            042600         PERFORM G-T-D-WRITE                                      DB2014.2
            042700         PERFORM DELETE-SUBTESTS-2                                DB2014.2
            042800         GO TO G-T-D-3.                                           DB2014.2
            042900 G-T-D-2-DELETE.                                                  DB2014.2
            043000     PERFORM DE-LETE.                                             DB2014.2
            043100     PERFORM G-T-D-WRITE.                                         DB2014.2
            043200     PERFORM DELETE-SUBTESTS-2.                                   DB2014.2
            043300     GO TO G-T-D-3-DELETE.                                        DB2014.2
            043400 DELETE-SUBTESTS-2.                                               DB2014.2
            043500     MOVE "G-T-D-2A" TO PAR-NAME.                                 DB2014.2
            043600     PERFORM DE-LETE.                                             DB2014.2
            043700     PERFORM G-T-D-WRITE.                                         DB2014.2
            043800     MOVE "G-T-D-2B" TO PAR-NAME.                                 DB2014.2
            043900     PERFORM DE-LETE.                                             DB2014.2
            044000     PERFORM G-T-D-WRITE.                                         DB2014.2
            044100     MOVE "G-T-D-2C" TO PAR-NAME.                                 DB2014.2
            044200     PERFORM DE-LETE.                                             DB2014.2
            044300     PERFORM G-T-D-WRITE.                                         DB2014.2
            044400 G-T-D-2A.                                                        DB2014.2
            044500     MOVE "G-T-D-2A" TO PAR-NAME.                                 DB2014.2
            044600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2014.2
            044700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2014.2
            044800     MOVE LINE-1 TO COMPUTED-A.                                   DB2014.2
            044900     PERFORM INSPT.                                               DB2014.2
            045000     PERFORM G-T-D-WRITE.                                         DB2014.2
            045100 G-T-D-2B.                                                        DB2014.2
            045200     MOVE "G-T-D-2B" TO PAR-NAME.                                 DB2014.2
            045300     IF UNQUAL-NAME-1 IS EQUAL TO "GO-TO-DEP-KEY"                 DB2014.2
            045400         PERFORM PASS                                             DB2014.2
            045500     ELSE PERFORM FAIL                                            DB2014.2
            045600         MOVE "GO-TO-DEP-KEY" TO CORRECT-A                        DB2014.2
            045700         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            045800     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            045900     PERFORM G-T-D-WRITE.                                         DB2014.2
            046000 G-T-D-2C.                                                        DB2014.2
            046100     MOVE "G-T-D-2C" TO PAR-NAME.                                 DB2014.2
            046200     IF CONTENTS-1 IS EQUAL TO "2"                                DB2014.2
            046300         PERFORM PASS                                             DB2014.2
            046400     ELSE PERFORM FAIL                                            DB2014.2
            046500         MOVE "2" TO CORRECT-A                                    DB2014.2
            046600         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            046700     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            046800     PERFORM G-T-D-WRITE.                                         DB2014.2
            046900 G-T-D-3.                                                         DB2014.2
            047000     MOVE "G-T-D-3" TO PAR-NAME.                                  DB2014.2
            047100     IF KEY-2 IS EQUAL TO 1 OR 2                                  DB2014.2
            047200         MOVE "DEBUG ON PROC-NAME EXECUTED" TO RE-MARK            DB2014.2
            047300         PERFORM PASS                                             DB2014.2
            047400         PERFORM G-T-D-WRITE                                      DB2014.2
            047500         GO TO G-T-D-3A                                           DB2014.2
            047600     ELSE                                                         DB2014.2
            047700         MOVE "DEBUG ON PRC-NAME NOT EXECUTED" TO RE-MARK         DB2014.2
            047800         PERFORM FAIL                                             DB2014.2
            047900         PERFORM G-T-D-WRITE                                      DB2014.2
            048000         PERFORM DELETE-SUBTESTS-3                                DB2014.2
            048100         GO TO G-T-D-4.                                           DB2014.2
            048200 G-T-D-3-DELETE.                                                  DB2014.2
            048300     MOVE "G-T-D-3" TO PAR-NAME.                                  DB2014.2
            048400     PERFORM DE-LETE.                                             DB2014.2
            048500     PERFORM G-T-D-WRITE.                                         DB2014.2
            048600     PERFORM DELETE-SUBTESTS-3                                    DB2014.2
            048700     GO TO G-T-D-4-DELETE.                                        DB2014.2
            048800 DELETE-SUBTESTS-3.                                               DB2014.2
            048900     MOVE "G-T-D-3A" TO PAR-NAME.                                 DB2014.2
            049000     PERFORM DE-LETE.                                             DB2014.2
            049100     PERFORM G-T-D-WRITE.                                         DB2014.2
            049200     MOVE "G-T-D-3B" TO PAR-NAME.                                 DB2014.2
            049300     PERFORM DE-LETE.                                             DB2014.2
            049400     PERFORM G-T-D-WRITE.                                         DB2014.2
            049500     MOVE "G-T-D-3C" TO PAR-NAME.                                 DB2014.2
            049600     PERFORM DE-LETE.                                             DB2014.2
            049700     PERFORM G-T-D-WRITE.                                         DB2014.2
            049800 G-T-D-3A.                                                        DB2014.2
            049900     MOVE "G-T-D-3A" TO PAR-NAME.                                 DB2014.2
            050000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2014.2
            050100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2014.2
            050200     MOVE LINE-2 TO COMPUTED-A.                                   DB2014.2
            050300     PERFORM INSPT.                                               DB2014.2
            050400     PERFORM G-T-D-WRITE.                                         DB2014.2
            050500 G-T-D-3B.                                                        DB2014.2
            050600     MOVE "G-T-D-3B" TO PAR-NAME.                                 DB2014.2
            050700     IF UNQUAL-NAME-2 IS EQUAL TO "G-T-D-2"                       DB2014.2
            050800         PERFORM PASS                                             DB2014.2
            050900     ELSE PERFORM FAIL                                            DB2014.2
            051000         MOVE "G-T-D-2" TO CORRECT-A                              DB2014.2
            051100         MOVE NAME-2 TO COMPUTED-A.                               DB2014.2
            051200     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            051300     PERFORM G-T-D-WRITE.                                         DB2014.2
            051400 G-T-D-3C.                                                        DB2014.2
            051500     MOVE "G-T-D-3C" TO PAR-NAME.                                 DB2014.2
            051600     IF CONTENTS-2 IS EQUAL TO SPACES                             DB2014.2
            051700         PERFORM PASS                                             DB2014.2
            051800     ELSE PERFORM FAIL                                            DB2014.2
            051900         MOVE "(SPACES)" TO CORRECT-A                             DB2014.2
            052000         MOVE CONTENTS-2 TO COMPUTED-A.                           DB2014.2
            052100     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            052200     PERFORM G-T-D-WRITE.                                         DB2014.2
            052300 G-T-D-4.                                                         DB2014.2
            052400     MOVE "G-T-D-4" TO PAR-NAME.                                  DB2014.2
            052500     IF KEY-2 IS EQUAL TO 2                                       DB2014.2
            052600         MOVE "PROCS EXECUTED IN RIGHT ORDER" TO RE-MARK          DB2014.2
            052700         PERFORM PASS                                             DB2014.2
            052800     ELSE PERFORM FAIL                                            DB2014.2
            052900         MOVE "PROCS EXECUTED IN WRONG ORDER" TO RE-MARK.         DB2014.2
            053000     GO TO G-T-D-WRITE.                                           DB2014.2
            053100 G-T-D-4-DELETE.                                                  DB2014.2
            053200     MOVE "G-T-D-4" TO PAR-NAME.                                  DB2014.2
            053300     PERFORM DE-LETE.                                             DB2014.2
            053400     GO TO G-T-D-WRITE.                                           DB2014.2
            053500 G-T-D-5.                                                         DB2014.2
            053600     GO TO G-T-D-2-DELETE.                                        DB2014.2
            053700 G-T-D-WRITE.                                                     DB2014.2
            053800     PERFORM PRINT-DETAIL.                                        DB2014.2
            053900 G-T-D-6-INIT.                                                    DB2014.2
            054000     MOVE "GO TO DEP/NOT ALL" TO FEATURE.                         DB2014.2
            054100     MOVE "G-T-D-6" TO PAR-NAME.                                  DB2014.2
            054200 G-T-D-6.                                                         DB2014.2
            054300     MOVE 2 TO GO-TO-DEP-KEY-1.                                   DB2014.2
            054400     MOVE SPACES TO ITEM-1  ITEM-2.                               DB2014.2
            054500     MOVE 0 TO KEY-1  KEY-2.                                      DB2014.2
            054600     GO TO G-T-D-7  G-T-D-8  G-T-D-9 DEPENDING ON GO-TO-DEP-KEY-1.DB2014.2
            054700 G-T-D-6-DELETE.                                                  DB2014.2
            054800     PERFORM DE-LETE.                                             DB2014.2
            054900     PERFORM G-T-D-WRITE.                                         DB2014.2
            055000     GO TO PERFORM-TESTS-INIT.                                    DB2014.2
            055100 G-T-D-7.                                                         DB2014.2
            055200     GO TO G-T-D-8.                                               DB2014.2
            055300 G-T-D-8.                                                         DB2014.2
            055400     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            055500         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            055600         PERFORM PASS                                             DB2014.2
            055700     ELSE PERFORM FAIL                                            DB2014.2
            055800         MOVE "DEBUG PROC EXECUTED" TO RE-MARK.                   DB2014.2
            055900     PERFORM G-T-D-WRITE.                                         DB2014.2
            056000     GO TO PERFORM-TESTS-INIT.                                    DB2014.2
            056100 G-T-D-9.                                                         DB2014.2
            056200     GO TO G-T-D-8.                                               DB2014.2
            056300 PERFORM-TESTS-INIT.                                              DB2014.2
            056400     MOVE "PERFORM VARYING" TO FEATURE.                           DB2014.2
            056500     MOVE SPACES TO ITEM-1.                                       DB2014.2
            056600     MOVE "PERFORM-VARY-1" TO PAR-NAME.                           DB2014.2
            056700     MOVE 0 TO KEY-1.                                             DB2014.2
            056800******************************************************************DB2014.2
            056900*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2014.2
            057000*    "P-V-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT WHICH     *DB2014.2
            057100*    FOLLOWS THIS COMMENT SET AND WHICH READS, "PERFORM          *DB2014.2
            057200*    P-V-1-SUBR VARYING ID-1 FROM 1 BY 1 UNTIL ID-1A IS GREATER  *DB2014.2
            057300*    THAN 5.".                                                   *DB2014.2
            057400******************************************************************DB2014.2
            057500 PERFORM-VARY-1.                                                  DB2014.2
            057600     PERFORM P-V-1-SUBR  VARYING ID-1 FROM 1 BY 1                 DB2014.2
            057700         UNTIL ID-1A IS GREATER THAN 5.                           DB2014.2
            057800     GO TO P-V-1-TEST.                                            DB2014.2
            057900 PERFORM-VARYING-1-DELETE.                                        DB2014.2
            058000     PERFORM DE-LETE.                                             DB2014.2
            058100     PERFORM P-V-WRITE.                                           DB2014.2
            058200     PERFORM DELETE-VARYING-SUBTESTS.                             DB2014.2
            058300     GO TO PERFORM-VARYING-2-DELETE.                              DB2014.2
            058400 DELETE-VARYING-SUBTESTS.                                         DB2014.2
            058500     MOVE "P-V-1A" TO PAR-NAME.                                   DB2014.2
            058600     PERFORM DE-LETE.                                             DB2014.2
            058700     PERFORM P-V-WRITE.                                           DB2014.2
            058800     MOVE "P-V-1B" TO PAR-NAME.                                   DB2014.2
            058900     PERFORM DE-LETE.                                             DB2014.2
            059000     PERFORM P-V-WRITE.                                           DB2014.2
            059100     MOVE "P-V-1C" TO PAR-NAME.                                   DB2014.2
            059200     PERFORM DE-LETE.                                             DB2014.2
            059300     PERFORM P-V-WRITE.                                           DB2014.2
            059400 P-V-1-SUBR.                                                      DB2014.2
            059500     ADD 1 TO COUNTER.                                            DB2014.2
            059600 P-V-1-TEST.                                                      DB2014.2
            059700     IF KEY-1 IS EQUAL TO 6                                       DB2014.2
            059800         MOVE "DEBUG PROC EXECUTED 6 TIMES" TO RE-MARK            DB2014.2
            059900         PERFORM PASS                                             DB2014.2
            060000     ELSE PERFORM FAIL                                            DB2014.2
            060100         MOVE "NO. TIMES DEBUG PROC EXECUTED" TO RE-MARK          DB2014.2
            060200         MOVE KEY-1 TO COMPUTED-18V0                              DB2014.2
            060300         MOVE 6 TO CORRECT-18V0.                                  DB2014.2
            060400     PERFORM P-V-WRITE.                                           DB2014.2
            060500     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            060600         PERFORM DELETE-VARYING-SUBTESTS                          DB2014.2
            060700         GO TO PERFORM-VARYING-2-DELETE.                          DB2014.2
            060800 P-V-1A.                                                          DB2014.2
            060900     MOVE "P-V-1A" TO PAR-NAME.                                   DB2014.2
            061000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2014.2
            061100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2014.2
            061200     MOVE LINE-1 TO COMPUTED-A.                                   DB2014.2
            061300     PERFORM INSPT.                                               DB2014.2
            061400     PERFORM P-V-WRITE.                                           DB2014.2
            061500 P-V-1B.                                                          DB2014.2
            061600     MOVE "P-V-1B" TO PAR-NAME.                                   DB2014.2
            061700     IF UNQUAL-NAME-1 IS EQUAL TO "ID-1"                          DB2014.2
            061800         PERFORM PASS                                             DB2014.2
            061900     ELSE PERFORM FAIL                                            DB2014.2
            062000         MOVE "ID-1" TO CORRECT-A                                 DB2014.2
            062100         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            062200     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            062300     PERFORM P-V-WRITE.                                           DB2014.2
            062400 P-V-1C.                                                          DB2014.2
            062500     MOVE "P-V-1C" TO PAR-NAME.                                   DB2014.2
            062600     IF CONTENTS-1 IS EQUAL TO "06"                               DB2014.2
            062700         PERFORM PASS                                             DB2014.2
            062800     ELSE PERFORM FAIL                                            DB2014.2
            062900         MOVE "06" TO CORRECT-A                                   DB2014.2
            063000         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            063100     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            063200     PERFORM P-V-WRITE.                                           DB2014.2
            063300 PERFORM-VARY-2.                                                  DB2014.2
            063400     PERFORM PERFORM-TESTS-INIT.                                  DB2014.2
            063500     MOVE "PERFORM-VARY-2" TO PAR-NAME.                           DB2014.2
            063600     PERFORM P-V-1-SUBR VARYING ID-1 FROM 1 BY 1                  DB2014.2
            063700         UNTIL ID-1 IS GREATER THAN 5.                            DB2014.2
            063800     GO TO P-V-2-TEST.                                            DB2014.2
            063900 PERFORM-VARYING-2-DELETE.                                        DB2014.2
            064000     MOVE "PERFORM-VARY-2" TO PAR-NAME.                           DB2014.2
            064100     PERFORM DE-LETE.                                             DB2014.2
            064200     PERFORM P-V-WRITE.                                           DB2014.2
            064300     GO TO PERFORM-AFTER-INIT.                                    DB2014.2
            064400 P-V-2-TEST.                                                      DB2014.2
            064500     IF KEY-1 IS EQUAL TO 12                                      DB2014.2
            064600         PERFORM PASS                                             DB2014.2
            064700         MOVE "DEBUG PROC EXECUTED 12 TIMES" TO RE-MARK           DB2014.2
            064800     ELSE PERFORM FAIL                                            DB2014.2
            064900         MOVE "12" TO CORRECT-A                                   DB2014.2
            065000         MOVE KEY-1 TO COMPUTED-A                                 DB2014.2
            065100         MOVE "NO. TIMES DEBUG PROC EXECUTED" TO RE-MARK.         DB2014.2
            065200 P-V-WRITE.                                                       DB2014.2
            065300     PERFORM PRINT-DETAIL.                                        DB2014.2
            065400 PERFORM-AFTER-INIT.                                              DB2014.2
            065500     MOVE "PERFORM AFTER" TO FEATURE.                             DB2014.2
            065600     MOVE SPACES TO ITEM-1.                                       DB2014.2
            065700     MOVE 0 TO KEY-1.                                             DB2014.2
            065800     MOVE "PERFORM-AFTER-1" TO PAR-NAME.                          DB2014.2
            065900 PERFORM-AFTER-1.                                                 DB2014.2
            066000     PERFORM P-V-1-SUBR                                           DB2014.2
            066100         VARYING ID-2A FROM 1 BY 1 UNTIL ID-2A IS GREATER THAN 5  DB2014.2
            066200         AFTER ID-1 FROM 1 BY 1 UNTIL ID-1A IS GREATER THAN 5.    DB2014.2
            066300     GO TO PERFORM-AFTER-1-TEST.                                  DB2014.2
            066400 PERFORM-AFTER-1-DELETE.                                          DB2014.2
            066500     PERFORM DE-LETE.                                             DB2014.2
            066600     PERFORM P-A-WRITE.                                           DB2014.2
            066700     GO TO DELETE-AFTER-SUBTEST.                                  DB2014.2
            066800 PERFORM-AFTER-1-TEST.                                            DB2014.2
            066900     IF KEY-1 IS EQUAL TO 31                                      DB2014.2
            067000         MOVE "DEBUG PROC EXECUTED 31 TIMES" TO RE-MARK           DB2014.2
            067100         PERFORM PASS                                             DB2014.2
            067200     ELSE PERFORM FAIL                                            DB2014.2
            067300         MOVE "NO. TIMES DEBUG PROC EXECUTED" TO RE-MARK          DB2014.2
            067400         MOVE KEY-1 TO COMPUTED-18V0                              DB2014.2
            067500         MOVE 31 TO CORRECT-18V0.                                 DB2014.2
            067600     PERFORM P-A-WRITE.                                           DB2014.2
            067700     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            067800         GO TO DELETE-AFTER-SUBTEST.                              DB2014.2
            067900 PERFORM-AFT-1A.                                                  DB2014.2
            068000     MOVE "PERFORM-AFT-1A" TO PAR-NAME.                           DB2014.2
            068100     IF CONTENTS-1 IS EQUAL TO "01"                               DB2014.2
            068200         PERFORM PASS                                             DB2014.2
            068300     ELSE PERFORM FAIL                                            DB2014.2
            068400         MOVE "01" TO CORRECT-A                                   DB2014.2
            068500         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            068600     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            068700     GO TO P-A-WRITE.                                             DB2014.2
            068800 DELETE-AFTER-SUBTEST.                                            DB2014.2
            068900     MOVE "PERFORM-AFT-1A"  TO PAR-NAME.                          DB2014.2
            069000     PERFORM DE-LETE.                                             DB2014.2
            069100 P-A-WRITE.                                                       DB2014.2
            069200     PERFORM PRINT-DETAIL.                                        DB2014.2
            069300 PERFORM-UNTIL-INIT.                                              DB2014.2
            069400     MOVE "PERFORM UNTIL" TO FEATURE.                             DB2014.2
            069500     MOVE SPACES TO ITEM-1.                                       DB2014.2
            069600     MOVE 0 TO KEY-1.                                             DB2014.2
            069700     MOVE "PERFORM-UNTIL-1" TO PAR-NAME.                          DB2014.2
            069800 PERFORM-UNTIL-1.                                                 DB2014.2
            069900     PERFORM P-V-1-SUBR                                           DB2014.2
            070000         VARYING ID-1A FROM 1 BY 1 UNTIL ID-1 IS GREATER THAN 5.  DB2014.2
            070100     GO TO PERFORM-UNTIL-1-TEST.                                  DB2014.2
            070200 PERFORM-UNTIL-1-DELETE.                                          DB2014.2
            070300     PERFORM DE-LETE.                                             DB2014.2
            070400     PERFORM P-U-WRITE.                                           DB2014.2
            070500     GO TO DELETE-UNTIL-SUBTEST.                                  DB2014.2
            070600 PERFORM-UNTIL-1-TEST.                                            DB2014.2
            070700     IF KEY-1 IS EQUAL TO 6                                       DB2014.2
            070800         MOVE "DEBUG PROC EXECUTED 6 TIMES" TO RE-MARK            DB2014.2
            070900         PERFORM PASS                                             DB2014.2
            071000     ELSE PERFORM FAIL                                            DB2014.2
            071100         MOVE "NO. TIMES DEBUG PROC EXECUTED" TO RE-MARK          DB2014.2
            071200         MOVE KEY-1 TO COMPUTED-18V0                              DB2014.2
            071300         MOVE 6 TO CORRECT-18V0.                                  DB2014.2
            071400     PERFORM P-A-WRITE.                                           DB2014.2
            071500     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            071600         GO TO DELETE-UNTIL-SUBTEST.                              DB2014.2
            071700 PERFORM-UNT-1A.                                                  DB2014.2
            071800     MOVE "PERFORM-UNT-1A" TO PAR-NAME.                           DB2014.2
            071900     IF CONTENTS-1 IS EQUAL TO "06"                               DB2014.2
            072000         PERFORM PASS                                             DB2014.2
            072100     ELSE PERFORM FAIL                                            DB2014.2
            072200         MOVE "06" TO CORRECT-A                                   DB2014.2
            072300         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            072400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            072500     GO TO P-U-WRITE.                                             DB2014.2
            072600 DELETE-UNTIL-SUBTEST.                                            DB2014.2
            072700     MOVE "PERFORM-UNT-1A" TO PAR-NAME.                           DB2014.2
            072800     PERFORM DE-LETE.                                             DB2014.2
            072900 P-U-WRITE.                                                       DB2014.2
            073000     PERFORM PRINT-DETAIL.                                        DB2014.2
            073100 BYPASSED-CODE-1.                                                 DB2014.2
            073200     MOVE 0 TO ID-3  KEY-1  KEY-2.                                DB2014.2
            073300     MULTIPLY ID-1A BY ID-3.                                      DB2014.2
            073400     MOVE "ALL REF OF IDENT" TO FEATURE.                          DB2014.2
            073500     IF ID-3 IS NOT EQUAL TO 0                                    DB2014.2
            073600         MOVE 1 TO ID-2  KEY-2.                                   DB2014.2
            073700     IF KEY-2 IS NOT EQUAL TO 0                                   DB2014.2
            073800         GO TO BYPASSED-CODE-DELETE.                              DB2014.2
            073900     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            074000         PERFORM PASS                                             DB2014.2
            074100     ELSE PERFORM FAIL                                            DB2014.2
            074200         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK.               DB2014.2
            074300     GO TO BYPASSED-CODE-WRITE.                                   DB2014.2
            074400 BYPASSED-CODE-DELETE.                                            DB2014.2
            074500     PERFORM DE-LETE.                                             DB2014.2
            074600 BYPASSED-CODE-WRITE.                                             DB2014.2
            074700     MOVE "BYPASSED-CODE-1" TO PAR-NAME.                          DB2014.2
            074800     PERFORM PRINT-DETAIL.                                        DB2014.2
            074900 REDEFINED-ID-1.                                                  DB2014.2
            075000     MOVE 0 TO KEY-1.                                             DB2014.2
            075100     MOVE "ALL REF OF IDENT" TO FEATURE.                          DB2014.2
            075200     MOVE 0 TO ID-2A.                                             DB2014.2
            075300     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            075400         PERFORM PASS                                             DB2014.2
            075500     ELSE PERFORM FAIL                                            DB2014.2
            075600         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK.               DB2014.2
            075700     GO TO REDEFINED-ID-WRITE.                                    DB2014.2
            075800 REDEFINED-ID-DELETE.                                             DB2014.2
            075900     PERFORM DE-LETE.                                             DB2014.2
            076000 REDEFINED-ID-WRITE.                                              DB2014.2
            076100     MOVE "REDEFINED-ID-1" TO PAR-NAME.                           DB2014.2
            076200     PERFORM PRINT-DETAIL.                                        DB2014.2
            076300 MOVE-TEST-1-INIT.                                                DB2014.2
            076400     MOVE "ALL REF OF IDENT" TO FEATURE.                          DB2014.2
            076500     MOVE "MOVE-TEST-1" TO PAR-NAME.                              DB2014.2
            076600     MOVE SPACES TO ITEM-1.                                       DB2014.2
            076700     MOVE 0 TO KEY-1.                                             DB2014.2
            076800******************************************************************DB2014.2
            076900*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2014.2
            077000*    "MOVE-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT     *DB2014.2
            077100*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS, "MOVE 3 TO  *DB2014.2
            077200*    ID-2.".                                                     *DB2014.2
            077300******************************************************************DB2014.2
            077400 MOVE-TEST-1.                                                     DB2014.2
            077500     MOVE 3 TO ID-2.                                              DB2014.2
            077600     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            077700         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2014.2
            077800         PERFORM PASS                                             DB2014.2
            077900         PERFORM MOVE-TEST-WRITE                                  DB2014.2
            078000         GO TO MOVE-TEST-1A                                       DB2014.2
            078100     ELSE PERFORM FAIL                                            DB2014.2
            078200         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            078300         PERFORM MOVE-TEST-WRITE                                  DB2014.2
            078400         PERFORM DELETE-MOVE-1-SUBTESTS                           DB2014.2
            078500         GO TO MOVE-TEST-2-INIT.                                  DB2014.2
            078600 MOVE-TEST-1-DELETE.                                              DB2014.2
            078700     PERFORM DE-LETE.                                             DB2014.2
            078800     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            078900     PERFORM DELETE-MOVE-1-SUBTESTS.                              DB2014.2
            079000     GO TO MOVE-TEST-2-INIT.                                      DB2014.2
            079100 DELETE-MOVE-1-SUBTESTS.                                          DB2014.2
            079200     MOVE "MOVE-TEST-1A" TO PAR-NAME.                             DB2014.2
            079300     PERFORM DE-LETE.                                             DB2014.2
            079400     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            079500     MOVE "MOVE-TEST-1B" TO PAR-NAME.                             DB2014.2
            079600     PERFORM DE-LETE.                                             DB2014.2
            079700     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            079800     MOVE "MOVE-TEST-1C" TO PAR-NAME.                             DB2014.2
            079900     PERFORM DE-LETE.                                             DB2014.2
            080000     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            080100 MOVE-TEST-1A.                                                    DB2014.2
            080200     MOVE "MOVE-TEST-1A" TO PAR-NAME.                             DB2014.2
            080300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2014.2
            080400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2014.2
            080500     MOVE LINE-1 TO COMPUTED-A.                                   DB2014.2
            080600     PERFORM INSPT.                                               DB2014.2
            080700     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            080800 MOVE-TEST-1B.                                                    DB2014.2
            080900     MOVE "MOVE-TEST-1B" TO PAR-NAME.                             DB2014.2
            081000     IF UNQUAL-NAME-1 IS EQUAL TO "ID-2"                          DB2014.2
            081100         PERFORM PASS                                             DB2014.2
            081200     ELSE PERFORM FAIL                                            DB2014.2
            081300         MOVE "ID-2" TO CORRECT-A                                 DB2014.2
            081400         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            081500     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            081600     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            081700 MOVE-TEST-1C.                                                    DB2014.2
            081800     MOVE "MOVE-TEST-1C" TO PAR-NAME.                             DB2014.2
            081900     IF CONTENTS-1 IS EQUAL TO "03"                               DB2014.2
            082000         PERFORM PASS                                             DB2014.2
            082100     ELSE PERFORM FAIL                                            DB2014.2
            082200         MOVE "03" TO CORRECT-A                                   DB2014.2
            082300         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            082400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            082500     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            082600 MOVE-TEST-2-INIT.                                                DB2014.2
            082700     MOVE "NOT ALL REF OF IDENT" TO FEATURE.                      DB2014.2
            082800     MOVE "MOVE-TEST-2" TO PAR-NAME.                              DB2014.2
            082900     MOVE 0 TO ID-1A.                                             DB2014.2
            083000     MOVE SPACES TO ITEM-1.                                       DB2014.2
            083100     MOVE 0 TO KEY-1.                                             DB2014.2
            083200 MOVE-TEST-2.                                                     DB2014.2
            083300     MOVE 5 TO ID-1.                                              DB2014.2
            083400     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            083500         MOVE "DEBUG EXECUTED" TO RE-MARK                         DB2014.2
            083600         PERFORM PASS                                             DB2014.2
            083700         PERFORM MOVE-TEST-WRITE                                  DB2014.2
            083800         GO TO MOVE-TEST-2A                                       DB2014.2
            083900     ELSE PERFORM FAIL                                            DB2014.2
            084000         MOVE "DEBUG NOT EXECUTED" TO RE-MARK                     DB2014.2
            084100         PERFORM MOVE-TEST-WRITE.                                 DB2014.2
            084200 DELETE-MOVE-2-SUBTEST.                                           DB2014.2
            084300     MOVE "MOVE-TEST-2A" TO PAR-NAME                              DB2014.2
            084400     PERFORM DE-LETE.                                             DB2014.2
            084500     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            084600     GO TO MOVE-TEST-3-DELETE.                                    DB2014.2
            084700 MOVE-TEST-2A.                                                    DB2014.2
            084800     MOVE "MOVE-TEST-2A" TO PAR-NAME.                             DB2014.2
            084900     IF CONTENTS-1 IS EQUAL TO "05"                               DB2014.2
            085000         PERFORM PASS                                             DB2014.2
            085100     ELSE PERFORM FAIL                                            DB2014.2
            085200         MOVE "05" TO CORRECT-A                                   DB2014.2
            085300         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            085400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            085500     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            085600 MOVE-TEST-3-INIT.                                                DB2014.2
            085700     MOVE "REPEATED NOT ALL REF" TO FEATURE.                      DB2014.2
            085800     MOVE "MOVE-TEST-3" TO PAR-NAME.                              DB2014.2
            085900     MOVE 5 TO ID-1A.                                             DB2014.2
            086000     MOVE SPACES TO ITEM-1.                                       DB2014.2
            086100     MOVE 0 TO KEY-1.                                             DB2014.2
            086200 MOVE-TEST-3.                                                     DB2014.2
            086300     MOVE 5 TO ID-1.                                              DB2014.2
            086400     IF KEY-1 IS  EQUAL TO 1                                      DB2014.2
            086500         MOVE "REPEATED MOVE INVOKED PROC" TO RE-MARK             DB2014.2
            086600     ELSE MOVE "REPEATED MOVE DIDN""T CALL PROC" TO RE-MARK.      DB2014.2
            086700     MOVE "INFO" TO P-OR-F.                                       DB2014.2
            086800     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            086900     GO TO MOVE-TEST-4-INIT.                                      DB2014.2
            087000 MOVE-TEST-3-DELETE.                                              DB2014.2
            087100     MOVE "REPEATED/NOT ALL REF" TO FEATURE.                      DB2014.2
            087200     MOVE "MOVE-TEST-3" TO PAR-NAME.                              DB2014.2
            087300     PERFORM DE-LETE.                                             DB2014.2
            087400     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            087500 MOVE-TEST-4-INIT.                                                DB2014.2
            087600     MOVE 2 TO ID-2A.                                             DB2014.2
            087700     MOVE SPACES TO ITEM-1.                                       DB2014.2
            087800     MOVE 0 TO KEY-1.                                             DB2014.2
            087900     MOVE "SRC OPR/ALL REF" TO FEATURE.                           DB2014.2
            088000     MOVE "MOVE-TEST-4" TO PAR-NAME.                              DB2014.2
            088100******************************************************************DB2014.2
            088200*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2014.2
            088300*    "MOVE-TEST-4A" SHOULD POINT TO THE EXECUTABLE STATEMENT     *DB2014.2
            088400*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS, "MOVE ID-2  *DB2014.2
            088500*    TO ID-3.".                                                  *DB2014.2
            088600******************************************************************DB2014.2
            088700 MOVE-TEST-4.                                                     DB2014.2
            088800     MOVE ID-2 TO ID-3.                                           DB2014.2
            088900     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            089000         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2014.2
            089100         PERFORM PASS                                             DB2014.2
            089200         PERFORM MOVE-TEST-WRITE                                  DB2014.2
            089300         GO TO MOVE-TEST-4A                                       DB2014.2
            089400     ELSE PERFORM FAIL                                            DB2014.2
            089500         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            089600         PERFORM MOVE-TEST-WRITE                                  DB2014.2
            089700         PERFORM DELETE-MOVE-4-SUBTESTS                           DB2014.2
            089800         GO TO MOVE-TEST-5-INIT.                                  DB2014.2
            089900 MOVE-TEST-4-DELETE.                                              DB2014.2
            090000     PERFORM DE-LETE.                                             DB2014.2
            090100     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            090200     PERFORM DELETE-MOVE-4-SUBTESTS.                              DB2014.2
            090300     GO TO MOVE-TEST-5-INIT.                                      DB2014.2
            090400 DELETE-MOVE-4-SUBTESTS.                                          DB2014.2
            090500     MOVE "MOVE-TEST-4A" TO PAR-NAME.                             DB2014.2
            090600     PERFORM DE-LETE.                                             DB2014.2
            090700     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            090800     MOVE "MOVE-TEST-4B" TO PAR-NAME.                             DB2014.2
            090900     PERFORM DE-LETE.                                             DB2014.2
            091000     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            091100     MOVE "MOVE-TEST-4C" TO PAR-NAME.                             DB2014.2
            091200     PERFORM DE-LETE.                                             DB2014.2
            091300     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            091400 MOVE-TEST-4A.                                                    DB2014.2
            091500     MOVE "MOVE-TEST-4A" TO PAR-NAME.                             DB2014.2
            091600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2014.2
            091700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2014.2
            091800     MOVE LINE-1 TO COMPUTED-A.                                   DB2014.2
            091900     PERFORM INSPT.                                               DB2014.2
            092000     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            092100 MOVE-TEST-4B.                                                    DB2014.2
            092200     MOVE "MOVE-TEST-4B" TO PAR-NAME.                             DB2014.2
            092300     IF UNQUAL-NAME-1 IS EQUAL TO "ID-2"                          DB2014.2
            092400         PERFORM PASS                                             DB2014.2
            092500     ELSE PERFORM FAIL                                            DB2014.2
            092600         MOVE "ID-2" TO CORRECT-A                                 DB2014.2
            092700         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            092800     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            092900     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            093000 MOVE-TEST-4C.                                                    DB2014.2
            093100     MOVE "MOVE-TEST-4C" TO PAR-NAME.                             DB2014.2
            093200     IF CONTENTS-1 IS EQUAL TO "02"                               DB2014.2
            093300         PERFORM PASS                                             DB2014.2
            093400     ELSE PERFORM FAIL                                            DB2014.2
            093500         MOVE "02" TO CORRECT-A                                   DB2014.2
            093600         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            093700     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            093800     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            093900 MOVE-TEST-5-INIT.                                                DB2014.2
            094000     MOVE 2 TO ID-1A.                                             DB2014.2
            094100     MOVE SPACES TO ITEM-1.                                       DB2014.2
            094200     MOVE 0 TO KEY-1                                              DB2014.2
            094300     MOVE "SRC OPR/NOT ALL REF" TO FEATURE.                       DB2014.2
            094400     MOVE "MOVE-TEST-5" TO PAR-NAME.                              DB2014.2
            094500 MOVE-TEST-5.                                                     DB2014.2
            094600     MOVE ID-1 TO ID-3.                                           DB2014.2
            094700     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            094800         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            094900         PERFORM PASS                                             DB2014.2
            095000     ELSE PERFORM FAIL                                            DB2014.2
            095100         MOVE "DEBUG PROC EXECUTED" TO RE-MARK.                   DB2014.2
            095200     PERFORM MOVE-TEST-WRITE.                                     DB2014.2
            095300     GO TO ADD-TEST-1-INIT.                                       DB2014.2
            095400 MOVE-TEST-5-DELETE.                                              DB2014.2
            095500     PERFORM DE-LETE.                                             DB2014.2
            095600 MOVE-TEST-WRITE.                                                 DB2014.2
            095700     PERFORM PRINT-DETAIL.                                        DB2014.2
            095800 ADD-TEST-1-INIT.                                                 DB2014.2
            095900     MOVE 1 TO ID-2A.                                             DB2014.2
            096000     MOVE SPACES TO ITEM-1.                                       DB2014.2
            096100     MOVE 0 TO KEY-1  ID-3.                                       DB2014.2
            096200     MOVE "MULT SRC OPR/ALL REF" TO FEATURE.                      DB2014.2
            096300     MOVE "ADD-TEST-1" TO PAR-NAME.                               DB2014.2
            096400******************************************************************DB2014.2
            096500*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2014.2
            096600*    "ADD-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT      *DB2014.2
            096700*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS,             *DB2014.2
            096800*    "ADD  ID-2  ID-2  ID-2  ID-2    TO    ID-3.".               *DB2014.2
            096900******************************************************************DB2014.2
            097000 ADD-TEST-1.                                                      DB2014.2
            097100     ADD  ID-2  ID-2  ID-2  ID-2    TO    ID-3.                   DB2014.2
            097200     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            097300         PERFORM PASS                                             DB2014.2
            097400         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2014.2
            097500     ELSE  PERFORM FAIL                                           DB2014.2
            097600         MOVE "NO. OF TIMES DEBUG EXECUTED" TO RE-MARK            DB2014.2
            097700         MOVE KEY-1 TO COMPUTED-18V0                              DB2014.2
            097800         MOVE 1 TO CORRECT-18V0.                                  DB2014.2
            097900     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            098000     GO TO ADD-TEST-1A.                                           DB2014.2
            098100 ADD-TEST-1-DELETE.                                               DB2014.2
            098200     PERFORM DE-LETE.                                             DB2014.2
            098300     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            098400     PERFORM DELETE-ADD-1-SUBTESTS.                               DB2014.2
            098500     GO TO ADD-TEST-2-INIT.                                       DB2014.2
            098600 DELETE-ADD-1-SUBTESTS.                                           DB2014.2
            098700     MOVE "ADD-TEST-1A" TO PAR-NAME.                              DB2014.2
            098800     PERFORM DE-LETE.                                             DB2014.2
            098900     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            099000     MOVE "ADD-TEST-1B" TO PAR-NAME.                              DB2014.2
            099100     PERFORM DE-LETE.                                             DB2014.2
            099200     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            099300     MOVE "ADD-TEST-1C" TO PAR-NAME.                              DB2014.2
            099400     PERFORM DE-LETE.                                             DB2014.2
            099500     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            099600 ADD-TEST-1A.                                                     DB2014.2
            099700     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            099800         PERFORM DELETE-ADD-1-SUBTESTS                            DB2014.2
            099900         GO TO ADD-TEST-2-INIT.                                   DB2014.2
            100000     MOVE "ADD-TEST-1A" TO PAR-NAME.                              DB2014.2
            100100     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2014.2
            100200     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2014.2
            100300     MOVE LINE-1 TO COMPUTED-A.                                   DB2014.2
            100400     PERFORM INSPT.                                               DB2014.2
            100500     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            100600 ADD-TEST-1B.                                                     DB2014.2
            100700     MOVE "ADD-TEST-1B" TO PAR-NAME.                              DB2014.2
            100800     IF UNQUAL-NAME-1 IS EQUAL TO "ID-2"                          DB2014.2
            100900         PERFORM PASS                                             DB2014.2
            101000     ELSE PERFORM FAIL                                            DB2014.2
            101100         MOVE "ID-2" TO CORRECT-A                                 DB2014.2
            101200         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            101300     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            101400     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            101500 ADD-TEST-1C.                                                     DB2014.2
            101600     MOVE "ADD-TEST-1C" TO PAR-NAME.                              DB2014.2
            101700     IF CONTENTS-1 IS EQUAL TO "01"                               DB2014.2
            101800         PERFORM PASS                                             DB2014.2
            101900     ELSE  PERFORM FAIL                                           DB2014.2
            102000         MOVE "01" TO CORRECT-A                                   DB2014.2
            102100         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            102200     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            102300     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            102400 ADD-TEST-2-INIT.                                                 DB2014.2
            102500     MOVE 1 TO ID-1A.                                             DB2014.2
            102600     MOVE SPACES TO ITEM-1.                                       DB2014.2
            102700     MOVE 0 TO  KEY-1  ID-3.                                      DB2014.2
            102800     MOVE "MULT SRC OPR/NOT ALL" TO FEATURE.                      DB2014.2
            102900     MOVE "ADD-TEST-2" TO PAR-NAME.                               DB2014.2
            103000 ADD-TEST-2.                                                      DB2014.2
            103100     ADD  ID-1  ID-1  ID-1  ID-1    TO    ID-3.                   DB2014.2
            103200     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            103300         PERFORM PASS                                             DB2014.2
            103400         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            103500     ELSE  PERFORM FAIL                                           DB2014.2
            103600         MOVE "NO. OF TIMES DEBUG EXECUTED" TO RE-MARK            DB2014.2
            103700         MOVE KEY-1 TO COMPUTED-A                                 DB2014.2
            103800         MOVE "0" TO CORRECT-A.                                   DB2014.2
            103900     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            104000     GO TO ADD-TEST-3-INIT.                                       DB2014.2
            104100 ADD-TEST-2-DELETE.                                               DB2014.2
            104200     PERFORM DE-LETE.                                             DB2014.2
            104300     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            104400 ADD-TEST-3-INIT.                                                 DB2014.2
            104500     MOVE 1 TO ID-2A.                                             DB2014.2
            104600     MOVE SPACES TO ITEM-1.                                       DB2014.2
            104700     MOVE 0 TO KEY-1.                                             DB2014.2
            104800     MOVE "SRC-RCV OPR/ALL REF" TO FEATURE.                       DB2014.2
            104900     MOVE "ADD-TEST-3" TO PAR-NAME.                               DB2014.2
            105000 ADD-TEST-3.                                                      DB2014.2
            105100     ADD  ID-2  ID-2  ID-2  ID-2    TO    ID-2.                   DB2014.2
            105200     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            105300         PERFORM PASS                                             DB2014.2
            105400         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2014.2
            105500         PERFORM ADD-TEST-WRITE                                   DB2014.2
            105600         GO TO ADD-TEST-3A                                        DB2014.2
            105700     ELSE PERFORM FAIL                                            DB2014.2
            105800         MOVE "NO. OF TIMES DEBUG EXECUTED" TO RE-MARK            DB2014.2
            105900         MOVE KEY-1 TO COMPUTED-18V0                              DB2014.2
            106000         MOVE 1 TO CORRECT-18V0.                                  DB2014.2
            106100     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            106200     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            106300         PERFORM DELETE-ADD-TEST-3-SUBTEST                        DB2014.2
            106400         GO TO ADD-TEST-4-INIT                                    DB2014.2
            106500     ELSE GO TO ADD-TEST-3A.                                      DB2014.2
            106600 ADD-TEST-3-DELETE.                                               DB2014.2
            106700     PERFORM DE-LETE.                                             DB2014.2
            106800     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            106900     PERFORM DELETE-ADD-TEST-3-SUBTEST.                           DB2014.2
            107000     GO TO ADD-TEST-4-INIT.                                       DB2014.2
            107100 DELETE-ADD-TEST-3-SUBTEST.                                       DB2014.2
            107200     MOVE "ADD-TEST-3A" TO PAR-NAME.                              DB2014.2
            107300     PERFORM DE-LETE.                                             DB2014.2
            107400     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            107500 ADD-TEST-3A.                                                     DB2014.2
            107600     MOVE "ADD-TEST-3A" TO PAR-NAME.                              DB2014.2
            107700     IF CONTENTS-1 IS EQUAL TO "05"                               DB2014.2
            107800         PERFORM PASS                                             DB2014.2
            107900     ELSE PERFORM FAIL                                            DB2014.2
            108000         MOVE "05" TO CORRECT-A                                   DB2014.2
            108100         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            108200     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            108300     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            108400 ADD-TEST-4-INIT.                                                 DB2014.2
            108500     MOVE 1 TO ID-1A.                                             DB2014.2
            108600     MOVE SPACES TO ITEM-1.                                       DB2014.2
            108700     MOVE 0 TO KEY-1.                                             DB2014.2
            108800     MOVE "SRC-RCV OPR/NOT ALL" TO FEATURE.                       DB2014.2
            108900     MOVE "ADD-TEST-4" TO PAR-NAME.                               DB2014.2
            109000 ADD-TEST-4.                                                      DB2014.2
            109100     ADD  ID-1  ID-1  ID-1  ID-1    TO    ID-1.                   DB2014.2
            109200     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            109300         PERFORM PASS                                             DB2014.2
            109400         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2014.2
            109500         PERFORM ADD-TEST-WRITE                                   DB2014.2
            109600         GO TO ADD-TEST-4A                                        DB2014.2
            109700     ELSE  PERFORM FAIL                                           DB2014.2
            109800         MOVE "NO. OF TIMES DEBUG EXECUTED" TO RE-MARK            DB2014.2
            109900         MOVE KEY-1 TO COMPUTED-18V0                              DB2014.2
            110000         MOVE 1 TO CORRECT-18V0.                                  DB2014.2
            110100     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            110200     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            110300         PERFORM DELETE-ADD-TEST-4-SUBTEST                        DB2014.2
            110400         GO TO SUBC-TEST-1-INIT                                   DB2014.2
            110500     ELSE  GO TO ADD-TEST-4A.                                     DB2014.2
            110600 ADD-TEST-4-DELETE.                                               DB2014.2
            110700     PERFORM DE-LETE.                                             DB2014.2
            110800     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            110900     PERFORM DELETE-ADD-TEST-4-SUBTEST.                           DB2014.2
            111000     GO TO SUBC-TEST-1-INIT.                                      DB2014.2
            111100 DELETE-ADD-TEST-4-SUBTEST.                                       DB2014.2
            111200     MOVE "ADD-TEST-4A" TO PAR-NAME.                              DB2014.2
            111300     PERFORM DE-LETE.                                             DB2014.2
            111400     PERFORM ADD-TEST-WRITE.                                      DB2014.2
            111500 ADD-TEST-4A.                                                     DB2014.2
            111600     MOVE "ADD-TEST-4A" TO PAR-NAME.                              DB2014.2
            111700     IF CONTENTS-1 IS EQUAL TO "05"                               DB2014.2
            111800         PERFORM PASS                                             DB2014.2
            111900     ELSE  PERFORM FAIL                                           DB2014.2
            112000         MOVE "05" TO CORRECT-A                                   DB2014.2
            112100         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            112200     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            112300 ADD-TEST-WRITE.                                                  DB2014.2
            112400     PERFORM PRINT-DETAIL.                                        DB2014.2
            112500 SUBC-TEST-1-INIT.                                                DB2014.2
            112600     MOVE "SUBSC-TEST-1" TO PAR-NAME.                             DB2014.2
            112700     MOVE "ONE-DIM SUBSCRIPT" TO FEATURE.                         DB2014.2
            112800     MOVE SPACE TO ITEM-1.                                        DB2014.2
            112900     MOVE 0 TO KEY-1.                                             DB2014.2
            113000     SET I TO 5.                                                  DB2014.2
            113100******************************************************************DB2014.2
            113200*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2014.2
            113300*    "SUBSC-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT    *DB2014.2
            113400*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS, "MOVE       *DB2014.2
            113500*    "ABCDE" TO B-LEVEL1 (I).".                                  *DB2014.2
            113600******************************************************************DB2014.2
            113700 SUBSC-TEST-1.                                                    DB2014.2
            113800     MOVE "ABCDE" TO B-LEVEL-1 (I).                               DB2014.2
            113900     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            114000         PERFORM PASS                                             DB2014.2
            114100         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2014.2
            114200         PERFORM SUBSC-TEST-WRITE                                 DB2014.2
            114300         GO TO SUBSC-TEST-1A                                      DB2014.2
            114400     ELSE  PERFORM FAIL                                           DB2014.2
            114500         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            114600         PERFORM SUBSC-TEST-WRITE                                 DB2014.2
            114700         PERFORM DELETE-SUBSC-TEST-1-SUBTESTS                     DB2014.2
            114800         GO TO SUBSC-TEST-2-INIT.                                 DB2014.2
            114900 SUBSC-TEST-1-DELETE.                                             DB2014.2
            115000     PERFORM DE-LETE.                                             DB2014.2
            115100     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            115200     PERFORM DELETE-SUBSC-TEST-1-SUBTESTS.                        DB2014.2
            115300     GO TO SUBSC-TEST-2-INIT.                                     DB2014.2
            115400 DELETE-SUBSC-TEST-1-SUBTESTS.                                    DB2014.2
            115500     MOVE "SUBSC-TEST-1A" TO PAR-NAME.                            DB2014.2
            115600     PERFORM DE-LETE.                                             DB2014.2
            115700     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            115800     MOVE "SUBSC-TEST-1B" TO PAR-NAME.                            DB2014.2
            115900     PERFORM DE-LETE.                                             DB2014.2
            116000     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            116100     MOVE "SUBSC-TEST-1C" TO PAR-NAME.                            DB2014.2
            116200     PERFORM DE-LETE.                                             DB2014.2
            116300     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            116400     MOVE "SUBSC-TEST-1D" TO PAR-NAME.                            DB2014.2
            116500     PERFORM DE-LETE.                                             DB2014.2
            116600     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            116700     MOVE "SUBSC-TEST-1E" TO PAR-NAME.                            DB2014.2
            116800     PERFORM DE-LETE.                                             DB2014.2
            116900     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            117000     MOVE "SUBSC-TEST-1F" TO PAR-NAME.                            DB2014.2
            117100     PERFORM DE-LETE.                                             DB2014.2
            117200     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            117300 SUBSC-TEST-1A.                                                   DB2014.2
            117400     MOVE "SUBSC-TEST-1A" TO PAR-NAME.                            DB2014.2
            117500     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2014.2
            117600     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2014.2
            117700     MOVE LINE-1 TO COMPUTED-A.                                   DB2014.2
            117800     PERFORM INSPT.                                               DB2014.2
            117900     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            118000 SUBSC-TEST-1B.                                                   DB2014.2
            118100     MOVE "SUBSC-TEST-1B" TO PAR-NAME.                            DB2014.2
            118200     IF UNQUAL-NAME-1 IS EQUAL TO "B-LEVEL-1"                     DB2014.2
            118300         PERFORM PASS                                             DB2014.2
            118400     ELSE PERFORM FAIL                                            DB2014.2
            118500         MOVE "B-LEVEL-1" TO CORRECT-A                            DB2014.2
            118600         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            118700     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            118800     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            118900 SUBSC-TEST-1C.                                                   DB2014.2
            119000     MOVE "SUBSC-TEST-1C" TO PAR-NAME.                            DB2014.2
            119100     IF CONTENTS-1 IS EQUAL TO "ABCDE"                            DB2014.2
            119200         PERFORM PASS                                             DB2014.2
            119300     ELSE  PERFORM FAIL                                           DB2014.2
            119400         MOVE "ABCDE" TO CORRECT-A                                DB2014.2
            119500         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            119600     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            119700     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            119800 SUBSC-TEST-1D.                                                   DB2014.2
            119900     MOVE "SUBSC-TEST-1D" TO PAR-NAME.                            DB2014.2
            120000     IF SUB-1-1 IS EQUAL TO "0005"                                DB2014.2
            120100         PERFORM PASS                                             DB2014.2
            120200     ELSE  PERFORM FAIL                                           DB2014.2
            120300         MOVE "0005" TO CORRECT-A                                 DB2014.2
            120400         MOVE SUB-1-1 TO COMPUTED-A.                              DB2014.2
            120500     MOVE "DEBUG-SUB-1" TO RE-MARK.                               DB2014.2
            120600     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            120700 SUBSC-TEST-1E.                                                   DB2014.2
            120800     MOVE "SUBSC-TEST-1E" TO PAR-NAME.                            DB2014.2
            120900     PERFORM DE-LETE.                                             DB2014.2
            121000     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            121100 SUBSC-TEST-1F.                                                   DB2014.2
            121200     MOVE "SUBSC-TEST-1F" TO PAR-NAME.                            DB2014.2
            121300     PERFORM DE-LETE.                                             DB2014.2
            121400     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            121500 SUBSC-TEST-2-INIT.                                               DB2014.2
            121600     MOVE "SUBSC-TEST-2" TO PAR-NAME.                             DB2014.2
            121700     MOVE "THR-DIM SUBSCRIPT" TO FEATURE.                         DB2014.2
            121800     MOVE SPACES TO ITEM-1.                                       DB2014.2
            121900     MOVE 0 TO KEY-1.                                             DB2014.2
            122000     SET I TO 4                                                   DB2014.2
            122100     SET J TO 6.                                                  DB2014.2
            122200     SET K TO 8.                                                  DB2014.2
            122300 SUBSC-TEST-2.                                                    DB2014.2
            122400     MOVE "Z" TO B-LEVEL-3 (I, J, K).                             DB2014.2
            122500     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            122600         PERFORM PASS                                             DB2014.2
            122700         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2014.2
            122800         PERFORM SUBSC-TEST-WRITE                                 DB2014.2
            122900         GO TO SUBSC-TEST-2A                                      DB2014.2
            123000     ELSE  PERFORM FAIL                                           DB2014.2
            123100         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            123200         PERFORM SUBSC-TEST-WRITE                                 DB2014.2
            123300         PERFORM DELETE-SUBSC-TEST-2-SUBTESTS                     DB2014.2
            123400         GO TO QUAL-TEST-1-INIT.                                  DB2014.2
            123500  SUBSC-TEST-2-DELETE.                                            DB2014.2
            123600     PERFORM DE-LETE.                                             DB2014.2
            123700     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            123800     PERFORM DELETE-SUBSC-TEST-2-SUBTESTS.                        DB2014.2
            123900     GO TO QUAL-TEST-1-INIT.                                      DB2014.2
            124000 DELETE-SUBSC-TEST-2-SUBTESTS.                                    DB2014.2
            124100     MOVE "SUBSC-TEST-2A" TO PAR-NAME.                            DB2014.2
            124200     PERFORM DE-LETE.                                             DB2014.2
            124300     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            124400     MOVE "SUBSC-TEST-2B" TO PAR-NAME.                            DB2014.2
            124500     PERFORM DE-LETE.                                             DB2014.2
            124600     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            124700     MOVE "SUBSC-TEST-2C" TO PAR-NAME.                            DB2014.2
            124800     PERFORM DE-LETE.                                             DB2014.2
            124900     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            125000     MOVE "SUBSC-TEST-2D" TO PAR-NAME.                            DB2014.2
            125100     PERFORM DE-LETE.                                             DB2014.2
            125200     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            125300 SUBSC-TEST-2A.                                                   DB2014.2
            125400     MOVE "SUBSC-TEST-2A" TO PAR-NAME.                            DB2014.2
            125500     IF CONTENTS-1 IS EQUAL TO "Z"                                DB2014.2
            125600         PERFORM PASS                                             DB2014.2
            125700     ELSE  PERFORM FAIL                                           DB2014.2
            125800         MOVE "Z" TO CORRECT-A                                    DB2014.2
            125900         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            126000     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            126100     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            126200 SUBSC-TEST-2B.                                                   DB2014.2
            126300     MOVE "SUBSC-TEST-2B" TO PAR-NAME.                            DB2014.2
            126400     IF SUB-1-1 IS EQUAL TO "0004"                                DB2014.2
            126500         PERFORM PASS                                             DB2014.2
            126600     ELSE  PERFORM FAIL                                           DB2014.2
            126700         MOVE "0004" TO CORRECT-A                                 DB2014.2
            126800         MOVE SUB-1-1 TO COMPUTED-A.                              DB2014.2
            126900     MOVE "DEBUG-SUB-1" TO RE-MARK.                               DB2014.2
            127000     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            127100 SUBSC-TEST-2C.                                                   DB2014.2
            127200     MOVE "SUBSC-TEST-2C" TO PAR-NAME.                            DB2014.2
            127300     IF SUB-2-1 IS EQUAL TO "0006"                                DB2014.2
            127400         PERFORM PASS                                             DB2014.2
            127500     ELSE  PERFORM FAIL                                           DB2014.2
            127600         MOVE "0006" TO CORRECT-A                                 DB2014.2
            127700         MOVE SUB-2-1 TO COMPUTED-A.                              DB2014.2
            127800     MOVE "DEBUG-SUB-2" TO RE-MARK.                               DB2014.2
            127900     PERFORM SUBSC-TEST-WRITE.                                    DB2014.2
            128000 SUBSC-TEST-2D.                                                   DB2014.2
            128100     MOVE "SUBSC-TEST-2D" TO PAR-NAME.                            DB2014.2
            128200     IF SUB-3-1 IS EQUAL TO "0008"                                DB2014.2
            128300         PERFORM PASS                                             DB2014.2
            128400     ELSE  PERFORM FAIL                                           DB2014.2
            128500         MOVE "0008" TO CORRECT-A                                 DB2014.2
            128600         MOVE SUB-3-1 TO COMPUTED-A.                              DB2014.2
            128700     MOVE "DEBUG-SUB-3" TO RE-MARK.                               DB2014.2
            128800 SUBSC-TEST-WRITE.                                                DB2014.2
            128900     PERFORM PRINT-DETAIL.                                        DB2014.2
            129000 QUAL-TEST-1-INIT.                                                DB2014.2
            129100     MOVE "QUAL-TEST-1" TO PAR-NAME.                              DB2014.2
            129200     MOVE "1-LEVEL QUALIFICATN" TO FEATURE.                       DB2014.2
            129300     MOVE SPACES TO ITEM-1.                                       DB2014.2
            129400     MOVE 0 TO KEY-1.                                             DB2014.2
            129500******************************************************************DB2014.2
            129600*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2014.2
            129700*    "QUAL-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT     *DB2014.2
            129800*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS,             *DB2014.2
            129900*    "MOVE "XY" TO  AB2 OF A2.".                                 *DB2014.2
            130000******************************************************************DB2014.2
            130100 QUAL-TEST-1.                                                     DB2014.2
            130200     MOVE "XY" TO  AB2 OF A2.                                     DB2014.2
            130300     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            130400         PERFORM PASS                                             DB2014.2
            130500         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2014.2
            130600             PERFORM QUAL-TEST-WRITE                              DB2014.2
            130700         GO TO QUAL-TEST-1A                                       DB2014.2
            130800     ELSE  PERFORM FAIL                                           DB2014.2
            130900         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            131000         PERFORM QUAL-TEST-WRITE                                  DB2014.2
            131100         PERFORM DELETE-QUAL-TEST-1-SUBTESTS                      DB2014.2
            131200         GO TO QUAL-TEST-2-INIT.                                  DB2014.2
            131300 QUAL-TEST-1-DELETE.                                              DB2014.2
            131400     PERFORM DE-LETE.                                             DB2014.2
            131500     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            131600     PERFORM DELETE-QUAL-TEST-1-SUBTESTS.                         DB2014.2
            131700     GO TO QUAL-TEST-2-INIT.                                      DB2014.2
            131800 DELETE-QUAL-TEST-1-SUBTESTS.                                     DB2014.2
            131900     MOVE "QUAL-TEST-1A" TO PAR-NAME.                             DB2014.2
            132000     PERFORM DE-LETE                                              DB2014.2
            132100     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            132200     MOVE "QUAL-TEST-1B" TO PAR-NAME.                             DB2014.2
            132300     PERFORM DE-LETE.                                             DB2014.2
            132400     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            132500     MOVE "QUAL-TEST-1C" TO PAR-NAME.                             DB2014.2
            132600     PERFORM DE-LETE.                                             DB2014.2
            132700     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            132800 QUAL-TEST-1A.                                                    DB2014.2
            132900     MOVE "QUAL-TEST-1A" TO PAR-NAME.                             DB2014.2
            133000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2014.2
            133100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2014.2
            133200     MOVE LINE-1 TO COMPUTED-A.                                   DB2014.2
            133300     PERFORM INSPT.                                               DB2014.2
            133400     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            133500 QUAL-TEST-1B.                                                    DB2014.2
            133600         MOVE "QUAL-TEST-1B" TO PAR-NAME.                         DB2014.2
            133700         IF NAME-1 IS EQUAL TO "AB2 OF A2"   OR                   DB2014.2
            133800            NAME-1 IS EQUAL TO "AB2 IN A2"                        DB2014.2
            133900         PERFORM PASS                                             DB2014.2
            134000     ELSE  PERFORM FAIL                                           DB2014.2
            134100         MOVE "AB2 OF(IN) A2" TO CORRECT-A                        DB2014.2
            134200         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            134300     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            134400     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            134500 QUAL-TEST-1C.                                                    DB2014.2
            134600     MOVE "QUAL-TEST-1C" TO PAR-NAME.                             DB2014.2
            134700     IF CONTENTS-1 IS EQUAL TO "XY"                               DB2014.2
            134800         PERFORM PASS                                             DB2014.2
            134900     ELSE  PERFORM FAIL                                           DB2014.2
            135000         MOVE "XY" TO CORRECT-A                                   DB2014.2
            135100         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            135200     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            135300     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            135400 QUAL-TEST-2-INIT.                                                DB2014.2
            135500     MOVE "QUAL-TEST-2" TO PAR-NAME.                              DB2014.2
            135600     MOVE "1-LEVEL QUALIFICATN" TO FEATURE.                       DB2014.2
            135700     MOVE SPACES TO ITEM-1.                                       DB2014.2
            135800     MOVE 0 TO KEY-1.                                             DB2014.2
            135900 QUAL-TEST-2.                                                     DB2014.2
            136000     MOVE "CD" TO AB2 OF A1.                                      DB2014.2
            136100     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            136200         PERFORM PASS                                             DB2014.2
            136300         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            136400     ELSE  PERFORM FAIL                                           DB2014.2
            136500         MOVE "DEBUG PROC EXECUTED" TO RE-MARK.                   DB2014.2
            136600     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            136700     GO TO QUAL-TEST-3-INIT.                                      DB2014.2
            136800 QUAL-TEST-2-DELETE.                                              DB2014.2
            136900     PERFORM DE-LETE.                                             DB2014.2
            137000     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            137100 QUAL-TEST-3-INIT.                                                DB2014.2
            137200     MOVE "QUAL-TEST-3" TO PAR-NAME.                              DB2014.2
            137300     MOVE "1-LEVEL QUALIFICATN" TO FEATURE.                       DB2014.2
            137400     MOVE SPACES TO ITEM-1.                                       DB2014.2
            137500     MOVE 0 TO KEY-1.                                             DB2014.2
            137600 QUAL-TEST-3.                                                     DB2014.2
            137700     MOVE "Q" TO ABC2 OF AB2 OF A2.                               DB2014.2
            137800     IF KEY-1 IS EQUAL TO 0                                       DB2014.2
            137900         PERFORM PASS                                             DB2014.2
            138000         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            138100     ELSE  PERFORM FAIL                                           DB2014.2
            138200         MOVE "DEBUG PROC EXECUTED" TO RE-MARK.                   DB2014.2
            138300     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            138400     GO TO QUAL-TEST-4-INIT.                                      DB2014.2
            138500 QUAL-TEST-3-DELETE.                                              DB2014.2
            138600     PERFORM DE-LETE.                                             DB2014.2
            138700     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            138800 QUAL-TEST-4-INIT.                                                DB2014.2
            138900     MOVE "QUAL-TEST-4" TO PAR-NAME.                              DB2014.2
            139000     MOVE "2-LEVEL QUALIFICATN" TO FEATURE.                       DB2014.2
            139100     MOVE SPACES TO ITEM-1.                                       DB2014.2
            139200     MOVE 0 TO KEY-1.                                             DB2014.2
            139300 QUAL-TEST-4.                                                     DB2014.2
            139400     MOVE "G" TO ABC1 OF AB2 OF A1.                               DB2014.2
            139500     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            139600         PERFORM PASS                                             DB2014.2
            139700         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2014.2
            139800         PERFORM QUAL-TEST-WRITE                                  DB2014.2
            139900         GO TO QUAL-TEST-4A                                       DB2014.2
            140000     ELSE  PERFORM FAIL                                           DB2014.2
            140100         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            140200         PERFORM QUAL-TEST-WRITE                                  DB2014.2
            140300         PERFORM DELETE-QUAL-TEST-4-SUBTESTS                      DB2014.2
            140400         GO TO QUAL-TEST-5-INIT.                                  DB2014.2
            140500 QUAL-TEST-4-DELETE.                                              DB2014.2
            140600     PERFORM DE-LETE.                                             DB2014.2
            140700     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            140800     PERFORM DELETE-QUAL-TEST-4-SUBTESTS.                         DB2014.2
            140900     GO TO QUAL-TEST-5-INIT.                                      DB2014.2
            141000 DELETE-QUAL-TEST-4-SUBTESTS.                                     DB2014.2
            141100     MOVE "QUAL-TEST-4A" TO PAR-NAME.                             DB2014.2
            141200     PERFORM DE-LETE.                                             DB2014.2
            141300     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            141400     MOVE "QUAL-TEST-4B" TO PAR-NAME.                             DB2014.2
            141500     PERFORM DE-LETE.                                             DB2014.2
            141600     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            141700 QUAL-TEST-4A.                                                    DB2014.2
            141800     MOVE "QUAL-TEST-4A" TO PAR-NAME.                             DB2014.2
            141900     IF NAME-1 IS EQUAL TO "ABC1 OF AB2 OF A1"  OR                DB2014.2
            142000        NAME-1 IS EQUAL TO "ABC1 IN AB2 IN A1"                    DB2014.2
            142100         PERFORM PASS                                             DB2014.2
            142200     ELSE PERFORM FAIL                                            DB2014.2
            142300         MOVE "ABC1 OF AB2 OF A1" TO CORRECT-A                    DB2014.2
            142400         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            142500     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            142600     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            142700 QUAL-TEST-4B.                                                    DB2014.2
            142800     MOVE "QUAL-TEST-4B" TO PAR-NAME.                             DB2014.2
            142900     IF CONTENTS-1 IS EQUAL TO "G"                                DB2014.2
            143000         PERFORM PASS                                             DB2014.2
            143100     ELSE  PERFORM FAIL                                           DB2014.2
            143200         MOVE "G" TO CORRECT-A                                    DB2014.2
            143300         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            143400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            143500     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            143600 QUAL-TEST-5-INIT.                                                DB2014.2
            143700     MOVE "QUAL-TEST-5" TO PAR-NAME.                              DB2014.2
            143800     MOVE "QUALIFIED SUBSC ITEM" TO FEATURE.                      DB2014.2
            143900     MOVE SPACES TO ITEM-1.                                       DB2014.2
            144000     MOVE 0 TO KEY-1.                                             DB2014.2
            144100 QUAL-TEST-5.                                                     DB2014.2
            144200     MOVE "F" TO AB1 OF A1 (3).                                   DB2014.2
            144300     IF KEY-1 IS EQUAL TO 1                                       DB2014.2
            144400         PERFORM PASS                                             DB2014.2
            144500         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2014.2
            144600         PERFORM QUAL-TEST-WRITE                                  DB2014.2
            144700         GO TO QUAL-TEST-5A                                       DB2014.2
            144800     ELSE  PERFORM FAIL                                           DB2014.2
            144900         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2014.2
            145000         PERFORM QUAL-TEST-WRITE                                  DB2014.2
            145100         PERFORM DELETE-QUAL-TEST-5-SUBTESTS                      DB2014.2
            145200         GO TO END-OF-DB201A.                                     DB2014.2
            145300 QUAL-TEST-5-DELETE.                                              DB2014.2
            145400     PERFORM DE-LETE.                                             DB2014.2
            145500     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            145600     PERFORM DELETE-QUAL-TEST-5-SUBTESTS.                         DB2014.2
            145700     GO TO END-OF-DB201A.                                         DB2014.2
            145800 DELETE-QUAL-TEST-5-SUBTESTS.                                     DB2014.2
            145900     MOVE "QUAL-TEST-5A" TO PAR-NAME.                             DB2014.2
            146000     PERFORM DE-LETE.                                             DB2014.2
            146100     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            146200     MOVE "QUAL-TEST-5B" TO PAR-NAME.                             DB2014.2
            146300     PERFORM DE-LETE.                                             DB2014.2
            146400     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            146500     MOVE "QUAL-TEST-5C" TO PAR-NAME.                             DB2014.2
            146600     PERFORM DE-LETE.                                             DB2014.2
            146700     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            146800     MOVE "QUAL-TEST-5D" TO PAR-NAME.                             DB2014.2
            146900     PERFORM DE-LETE.                                             DB2014.2
            147000     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            147100     MOVE "QUAL-TEST-5E" TO PAR-NAME.                             DB2014.2
            147200     PERFORM DE-LETE.                                             DB2014.2
            147300     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            147400 QUAL-TEST-5A.                                                    DB2014.2
            147500     MOVE "QUAL-TEST-5A" TO PAR-NAME.                             DB2014.2
            147600     IF NAME-1 IS EQUAL TO "AB1 OF A1"  OR                        DB2014.2
            147700        NAME-1 IS EQUAL TO "AB1 IN A1"                            DB2014.2
            147800         PERFORM PASS                                             DB2014.2
            147900     ELSE  PERFORM FAIL                                           DB2014.2
            148000         MOVE "AB1 OF(IN) A1" TO CORRECT-A                        DB2014.2
            148100         MOVE NAME-1 TO COMPUTED-A.                               DB2014.2
            148200     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2014.2
            148300     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            148400 QUAL-TEST-5B.                                                    DB2014.2
            148500     MOVE "QUAL-TEST-5B" TO PAR-NAME.                             DB2014.2
            148600     IF CONTENTS-1 IS EQUAL TO "F"                                DB2014.2
            148700         PERFORM PASS                                             DB2014.2
            148800     ELSE  PERFORM FAIL                                           DB2014.2
            148900         MOVE "F" TO CORRECT-A                                    DB2014.2
            149000         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2014.2
            149100     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2014.2
            149200     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            149300 QUAL-TEST-5C.                                                    DB2014.2
            149400     MOVE "QUAL-TEST-5C" TO PAR-NAME.                             DB2014.2
            149500     IF SUB-1-1 IS EQUAL TO "0003"                                DB2014.2
            149600         PERFORM PASS                                             DB2014.2
            149700     ELSE  PERFORM FAIL                                           DB2014.2
            149800         MOVE "0003" TO CORRECT-A                                 DB2014.2
            149900         MOVE SUB-1-1 TO COMPUTED-A.                              DB2014.2
            150000     MOVE "DEBUG-SUB-1" TO RE-MARK.                               DB2014.2
            150100     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            150200 QUAL-TEST-5D.                                                    DB2014.2
            150300     MOVE "QUAL-TEST-5D" TO PAR-NAME.                             DB2014.2
            150400     PERFORM DE-LETE.                                             DB2014.2
            150500     PERFORM QUAL-TEST-WRITE.                                     DB2014.2
            150600 QUAL-TEST-5E.                                                    DB2014.2
            150700     MOVE "QUAL-TEST-5E" TO PAR-NAME.                             DB2014.2
            150800     PERFORM DE-LETE.                                             DB2014.2
            150900 QUAL-TEST-WRITE.                                                 DB2014.2
            151000     PERFORM PRINT-DETAIL.                                        DB2014.2
            151100 END-OF-DB201A.                                                   DB2014.2
            151200     EXIT.                                                        DB2014.2
            151300 CCVS-EXIT SECTION.                                               DB2014.2
            151400 CCVS-999999.                                                     DB2014.2
            151500     GO TO CLOSE-FILES.                                           DB2014.2
                  *END-OF,DB201A                                                            
        """)
    )

    @Disabled("Requires Y and X")
    @Test
    fun db2024_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB202A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB2024.2
            000200 PROGRAM-ID.                                                      DB2024.2
            000300     DB202A.                                                      DB2024.2
            000400 AUTHOR.                                                          DB2024.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB2024.2
            000600 INSTALLATION.                                                    DB2024.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB2024.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB2024.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB2024.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB2024.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB2024.2
            001200                                                                  DB2024.2
            001300     PHONE   (703) 756-6153                                       DB2024.2
            001400                                                                  DB2024.2
            001500     " HIGH       ".                                              DB2024.2
            001600 DATE-WRITTEN.                                                    DB2024.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB2024.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB2024.2
            001900     "4.2 ".                                                      DB2024.2
            002000 SECURITY.                                                        DB2024.2
            002100     NONE.                                                        DB2024.2
            002200*                                                                 DB2024.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB2024.2
            002400*                                                                 DB2024.2
            002500*                       PROGRAM ABSTRACT                          DB2024.2
            002600*                                                                 DB2024.2
            002700*    DB202A TESTS THE CPABILITY OF THE DEBUG MODULE TO HANDLE     DB2024.2
            002800*    DEBUGGING PROCEDURES WHICH ARE MONITORING I-O FUNCTIONS      DB2024.2
            002900*    OF THE SEQUENTIAL I-O MODULE.  THIS PROGRAM IS TO BE         DB2024.2
            003000*    COMPILED AND EXECUTED WITH BOTH COMPILE AND OBJECT TIME      DB2024.2
            003100*    DEBUGGING SWITCHES ON.  THE DEBUGGING PROCEDURES SHOULD      DB2024.2
            003200*    BE INCLUDED IN COMPILATION AND GENERATE CODE.                DB2024.2
            003300*                                                                 DB2024.2
            003400*    DURING EXECUTION, A SEQUENTIAL FILE IS CREATED CONTAINING    DB2024.2
            003500*    80-CHARACTER RECORDS.  THE FILE US THEN READ.  EXECUTION     DB2024.2
            003600*    OF "OPEN", "READ", AND "WRITE" FUNCTION SHOULD TRIGGER THE   DB2024.2
            003700*    APPROPRIATE DEBUGGING PROCEDURES.                            DB2024.2
            003800*                                                                 DB2024.2
            003900*                                                                 DB2024.2
            004000*                                                                 DB2024.2
            004100 ENVIRONMENT DIVISION.                                            DB2024.2
            004200 CONFIGURATION SECTION.                                           DB2024.2
            004300 SOURCE-COMPUTER.                                                 DB2024.2
            004400     XXXXX082                                                     DB2024.2
            004500     WITH DEBUGGING MODE.                                         DB2024.2
            004600 OBJECT-COMPUTER.                                                 DB2024.2
            004700     XXXXX083.                                                    DB2024.2
            004800 INPUT-OUTPUT SECTION.                                            DB2024.2
            004900 FILE-CONTROL.                                                    DB2024.2
            005000     SELECT PRINT-FILE ASSIGN TO                                  DB2024.2
            005100     XXXXX055.                                                    DB2024.2
            005200     SELECT SEQ-FILE ASSIGN TO                                    DB2024.2
            005300     XXXXX014.                                                    DB2024.2
            005400 DATA DIVISION.                                                   DB2024.2
            005500 FILE SECTION.                                                    DB2024.2
            005600 FD  PRINT-FILE                                                   DB2024.2
            005700     LABEL RECORDS                                                DB2024.2
            005800     XXXXX084                                                     DB2024.2
            005900     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB2024.2
            006000 01  PRINT-REC PICTURE X(120).                                    DB2024.2
            006100 01  DUMMY-RECORD PICTURE X(120).                                 DB2024.2
            006200 FD  SEQ-FILE                                                     DB2024.2
            006300C    VALUE OF                                                     DB2024.2
            006400C    XXXXX074                                                     DB2024.2
            006500*      XXXXX074  REPLACE WITH IMPLEMENTOR NAME (*OPT C ONLY)      DB2024.2
            006600C    IS                                                           DB2024.2
            006700C    XXXXX075                                                     DB2024.2
            006800*      XXXXX075  REPLACE WITH VALUE CLAUSE OBJECT (*OPT C ONLY)   DB2024.2
            006900G    XXXXX069                                                     DB2024.2
            007000*      XXXXX069  REPLACE WITH ADDITIONAL INFO (*OPT G ONLY)       DB2024.2
            007100     LABEL RECORDS ARE STANDARD.                                  DB2024.2
            007200 01  SEQ-REC-1 PIC X(120).                                        DB2024.2
            007300 01  SEQ-REC-2 PIC X(120).                                        DB2024.2
            007400 01  SEQ-REC-3 PIC X(120).                                        DB2024.2
            007500 WORKING-STORAGE SECTION.                                         DB2024.2
            007600 01  ITEM-1.                                                      DB2024.2
            007700     02  KEY-1 PIC 99.                                            DB2024.2
            007800     02  LINE-1 PIC X(6).                                         DB2024.2
            007900     02  NAME-1 PIC X(30).                                        DB2024.2
            008000     02  UNQUAL-NAME-1 PIC X(30).                                 DB2024.2
            008100     02  CONTENTS-1 PIC X(120).                                   DB2024.2
            008200     02  CONTENTS-REC PIC X(120).                                 DB2024.2
            008300 01  FILE-RECORD-INFORMATION-REC.                                 DB2024.2
            008400     03 FILE-RECORD-INFO-SKELETON.                                DB2024.2
            008500        05 FILLER                 PICTURE X(48)       VALUE       DB2024.2
            008600             "FILE=      ,RECORD=      /0,RECNO=000000,UPDT=00".  DB2024.2
            008700        05 FILLER                 PICTURE X(46)       VALUE       DB2024.2
            008800             ",ODO=0000,PGM=     ,LRECL=000000,BLKSIZ  =0000".    DB2024.2
            008900        05 FILLER                 PICTURE X(26)       VALUE       DB2024.2
            009000             ",LFIL=000000,ORG=  ,LBLR= ".                        DB2024.2
            009100        05 FILLER                 PICTURE X(37)       VALUE       DB2024.2
            009200             ",RECKEY=                             ".             DB2024.2
            009300        05 FILLER                 PICTURE X(38)       VALUE       DB2024.2
            009400             ",ALTKEY1=                             ".            DB2024.2
            009500        05 FILLER                 PICTURE X(38)       VALUE       DB2024.2
            009600             ",ALTKEY2=                             ".            DB2024.2
            009700        05 FILLER                 PICTURE X(7)        VALUE SPACE.DB2024.2
            009800     03 FILE-RECORD-INFO          OCCURS  10  TIMES.              DB2024.2
            009900        05 FILE-RECORD-INFO-P1-120.                               DB2024.2
            010000           07 FILLER              PIC X(5).                       DB2024.2
            010100           07 XFILE-NAME           PIC X(6).                      DB2024.2
            010200           07 FILLER              PIC X(8).                       DB2024.2
            010300           07 XRECORD-NAME         PIC X(6).                      DB2024.2
            010400           07 FILLER              PIC X(1).                       DB2024.2
            010500           07 REELUNIT-NUMBER     PIC 9(1).                       DB2024.2
            010600           07 FILLER              PIC X(7).                       DB2024.2
            010700           07 XRECORD-NUMBER       PIC 9(6).                      DB2024.2
            010800           07 FILLER              PIC X(6).                       DB2024.2
            010900           07 UPDATE-NUMBER       PIC 9(2).                       DB2024.2
            011000           07 FILLER              PIC X(5).                       DB2024.2
            011100           07 ODO-NUMBER          PIC 9(4).                       DB2024.2
            011200           07 FILLER              PIC X(5).                       DB2024.2
            011300           07 XPROGRAM-NAME        PIC X(5).                      DB2024.2
            011400           07 FILLER              PIC X(7).                       DB2024.2
            011500           07 XRECORD-LENGTH       PIC 9(6).                      DB2024.2
            011600           07 FILLER              PIC X(7).                       DB2024.2
            011700           07 CHARS-OR-RECORDS    PIC X(2).                       DB2024.2
            011800           07 FILLER              PIC X(1).                       DB2024.2
            011900           07 XBLOCK-SIZE          PIC 9(4).                      DB2024.2
            012000           07 FILLER              PIC X(6).                       DB2024.2
            012100           07 RECORDS-IN-FILE     PIC 9(6).                       DB2024.2
            012200           07 FILLER              PIC X(5).                       DB2024.2
            012300           07 XFILE-ORGANIZATION   PIC X(2).                      DB2024.2
            012400           07 FILLER              PIC X(6).                       DB2024.2
            012500           07 XLABEL-TYPE          PIC X(1).                      DB2024.2
            012600        05 FILE-RECORD-INFO-P121-240.                             DB2024.2
            012700           07 FILLER              PIC X(8).                       DB2024.2
            012800           07 XRECORD-KEY          PIC X(29).                     DB2024.2
            012900           07 FILLER              PIC X(9).                       DB2024.2
            013000           07 ALTERNATE-KEY1      PIC X(29).                      DB2024.2
            013100           07 FILLER              PIC X(9).                       DB2024.2
            013200           07 ALTERNATE-KEY2      PIC X(29).                      DB2024.2
            013300           07 FILLER              PIC X(7).                       DB2024.2
            013400 01  TEST-RESULTS.                                                DB2024.2
            013500     02 FILLER                    PICTURE X VALUE SPACE.          DB2024.2
            013600     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB2024.2
            013700     02 FILLER                    PICTURE X VALUE SPACE.          DB2024.2
            013800     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB2024.2
            013900     02 FILLER                    PICTURE X  VALUE SPACE.         DB2024.2
            014000     02  PAR-NAME.                                                DB2024.2
            014100       03 FILLER PICTURE X(12) VALUE SPACE.                       DB2024.2
            014200       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB2024.2
            014300       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB2024.2
            014400       03 FILLER PIC X(5) VALUE SPACE.                            DB2024.2
            014500     02 FILLER PIC X(10) VALUE SPACE.                             DB2024.2
            014600     02 RE-MARK PIC X(61).                                        DB2024.2
            014700 01  TEST-COMPUTED.                                               DB2024.2
            014800     02 FILLER PIC X(30) VALUE SPACE.                             DB2024.2
            014900     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB2024.2
            015000     02 COMPUTED-X.                                               DB2024.2
            015100     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB2024.2
            015200     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB2024.2
            015300     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB2024.2
            015400     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB2024.2
            015500     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB2024.2
            015600     03       CM-18V0 REDEFINES COMPUTED-A.                       DB2024.2
            015700         04 COMPUTED-18V0                   PICTURE -9(18).       DB2024.2
            015800         04 FILLER                          PICTURE X.            DB2024.2
            015900     03 FILLER PIC X(50) VALUE SPACE.                             DB2024.2
            016000 01  TEST-CORRECT.                                                DB2024.2
            016100     02 FILLER PIC X(30) VALUE SPACE.                             DB2024.2
            016200     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB2024.2
            016300     02 CORRECT-X.                                                DB2024.2
            016400     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB2024.2
            016500     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB2024.2
            016600     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB2024.2
            016700     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB2024.2
            016800     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB2024.2
            016900     03      CR-18V0 REDEFINES CORRECT-A.                         DB2024.2
            017000         04 CORRECT-18V0                    PICTURE -9(18).       DB2024.2
            017100         04 FILLER                          PICTURE X.            DB2024.2
            017200     03 FILLER PIC X(50) VALUE SPACE.                             DB2024.2
            017300 01  CCVS-C-1.                                                    DB2024.2
            017400     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB2024.2
            017500-    "SS  PARAGRAPH-NAME                                          DB2024.2
            017600-    "        REMARKS".                                           DB2024.2
            017700     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB2024.2
            017800 01  CCVS-C-2.                                                    DB2024.2
            017900     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2024.2
            018000     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB2024.2
            018100     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB2024.2
            018200     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB2024.2
            018300     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB2024.2
            018400 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB2024.2
            018500 01  REC-CT PICTURE 99 VALUE ZERO.                                DB2024.2
            018600 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB2024.2
            018700 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB2024.2
            018800 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB2024.2
            018900 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB2024.2
            019000 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB2024.2
            019100 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB2024.2
            019200 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB2024.2
            019300 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB2024.2
            019400 01  CCVS-H-1.                                                    DB2024.2
            019500     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB2024.2
            019600     02 FILLER PICTURE X(67) VALUE                                DB2024.2
            019700     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB2024.2
            019800-    " SYSTEM".                                                   DB2024.2
            019900     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB2024.2
            020000 01  CCVS-H-2.                                                    DB2024.2
            020100     02 FILLER PICTURE X(52) VALUE IS                             DB2024.2
            020200     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB2024.2
            020300     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB2024.2
            020400     02 TEST-ID PICTURE IS X(9).                                  DB2024.2
            020500     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB2024.2
            020600 01  CCVS-H-3.                                                    DB2024.2
            020700     02  FILLER PICTURE X(34) VALUE                               DB2024.2
            020800     " FOR OFFICIAL USE ONLY    ".                                DB2024.2
            020900     02  FILLER PICTURE X(58) VALUE                               DB2024.2
            021000     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB2024.2
            021100     02  FILLER PICTURE X(28) VALUE                               DB2024.2
            021200     "  COPYRIGHT   1974 ".                                       DB2024.2
            021300 01  CCVS-E-1.                                                    DB2024.2
            021400     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB2024.2
            021500     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB2024.2
            021600     02 ID-AGAIN PICTURE IS X(9).                                 DB2024.2
            021700     02 FILLER PICTURE X(45) VALUE IS                             DB2024.2
            021800     " NTIS DISTRIBUTION COBOL 74".                               DB2024.2
            021900 01  CCVS-E-2.                                                    DB2024.2
            022000     02  FILLER                   PICTURE X(31)  VALUE            DB2024.2
            022100     SPACE.                                                       DB2024.2
            022200     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB2024.2
            022300     02 CCVS-E-2-2.                                               DB2024.2
            022400         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB2024.2
            022500         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB2024.2
            022600         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB2024.2
            022700 01  CCVS-E-3.                                                    DB2024.2
            022800     02  FILLER PICTURE X(22) VALUE                               DB2024.2
            022900     " FOR OFFICIAL USE ONLY".                                    DB2024.2
            023000     02  FILLER PICTURE X(12) VALUE SPACE.                        DB2024.2
            023100     02  FILLER PICTURE X(58) VALUE                               DB2024.2
            023200     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB2024.2
            023300     02  FILLER PICTURE X(13) VALUE SPACE.                        DB2024.2
            023400     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB2024.2
            023500 01  CCVS-E-4.                                                    DB2024.2
            023600     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB2024.2
            023700     02 FILLER PIC XXXX VALUE " OF ".                             DB2024.2
            023800     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB2024.2
            023900     02 FILLER PIC X(40) VALUE                                    DB2024.2
            024000      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB2024.2
            024100 01  XXINFO.                                                      DB2024.2
            024200     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB2024.2
            024300     02 INFO-TEXT.                                                DB2024.2
            024400     04 FILLER PIC X(20) VALUE SPACE.                             DB2024.2
            024500     04 XXCOMPUTED PIC X(20).                                     DB2024.2
            024600     04 FILLER PIC X(5) VALUE SPACE.                              DB2024.2
            024700     04 XXCORRECT PIC X(20).                                      DB2024.2
            024800 01  HYPHEN-LINE.                                                 DB2024.2
            024900     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2024.2
            025000     02 FILLER PICTURE IS X(65) VALUE IS "************************DB2024.2
            025100-    "*****************************************".                 DB2024.2
            025200     02 FILLER PICTURE IS X(54) VALUE IS "************************DB2024.2
            025300-    "******************************".                            DB2024.2
            025400 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB2024.2
            025500     "DB202A".                                                    DB2024.2
            025600 PROCEDURE DIVISION.                                              DB2024.2
            025700 DECLARATIVES.                                                    DB2024.2
            025800 FILENAME-PROC SECTION.                                           DB2024.2
            025900     USE FOR DEBUGGING ON SEQ-FILE.                               DB2024.2
            026000 FILENAME-1.                                                      DB2024.2
            026100     MOVE 1 TO KEY-1.                                             DB2024.2
            026200 DB-COMMON.                                                       DB2024.2
            026300     MOVE DEBUG-LINE TO LINE-1.                                   DB2024.2
            026400     MOVE DEBUG-NAME TO NAME-1 UNQUAL-NAME-1.                     DB2024.2
            026500     MOVE DEBUG-CONTENTS TO CONTENTS-1.                           DB2024.2
            026600     INSPECT UNQUAL-NAME-1 REPLACING CHARACTERS BY SPACES         DB2024.2
            026700         AFTER INITIAL SPACE.                                     DB2024.2
            026800 WRITE-PROC SECTION.                                              DB2024.2
            026900     USE FOR DEBUGGING ON ALL REFERENCES OF SEQ-REC-1  SEQ-REC-2. DB2024.2
            027000 WRITE-1.                                                         DB2024.2
            027100     ADD 1 TO KEY-1.                                              DB2024.2
            027200     PERFORM DB-COMMON.                                           DB2024.2
            027300     MOVE SEQ-REC-3 TO CONTENTS-REC.                              DB2024.2
            027400 END DECLARATIVES.                                                DB2024.2
            027500 CCVS1 SECTION.                                                   DB2024.2
            027600 OPEN-FILES.                                                      DB2024.2
            027700     OPEN     OUTPUT PRINT-FILE.                                  DB2024.2
            027800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB2024.2
            027900     MOVE    SPACE TO TEST-RESULTS.                               DB2024.2
            028000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB2024.2
            028100     GO TO CCVS1-EXIT.                                            DB2024.2
            028200 CLOSE-FILES.                                                     DB2024.2
            028300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB2024.2
            028400 TERMINATE-CCVS.                                                  DB2024.2
            028500S    EXIT PROGRAM.                                                DB2024.2
            028600STERMINATE-CALL.                                                  DB2024.2
            028700     STOP     RUN.                                                DB2024.2
            028800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB2024.2
            028900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB2024.2
            029000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB2024.2
            029100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB2024.2
            029200     MOVE "****TEST DELETED****" TO RE-MARK.                      DB2024.2
            029300 PRINT-DETAIL.                                                    DB2024.2
            029400     IF REC-CT NOT EQUAL TO ZERO                                  DB2024.2
            029500             MOVE "." TO PARDOT-X                                 DB2024.2
            029600             MOVE REC-CT TO DOTVALUE.                             DB2024.2
            029700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB2024.2
            029800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB2024.2
            029900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB2024.2
            030000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB2024.2
            030100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB2024.2
            030200     MOVE SPACE TO CORRECT-X.                                     DB2024.2
            030300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB2024.2
            030400     MOVE     SPACE TO RE-MARK.                                   DB2024.2
            030500 HEAD-ROUTINE.                                                    DB2024.2
            030600     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2024.2
            030700     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB2024.2
            030800     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB2024.2
            030900 COLUMN-NAMES-ROUTINE.                                            DB2024.2
            031000     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2024.2
            031100     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2024.2
            031200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB2024.2
            031300 END-ROUTINE.                                                     DB2024.2
            031400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB2024.2
            031500 END-RTN-EXIT.                                                    DB2024.2
            031600     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2024.2
            031700 END-ROUTINE-1.                                                   DB2024.2
            031800      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB2024.2
            031900      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB2024.2
            032000      ADD PASS-COUNTER TO ERROR-HOLD.                             DB2024.2
            032100*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB2024.2
            032200      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB2024.2
            032300      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB2024.2
            032400      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB2024.2
            032500      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB2024.2
            032600  END-ROUTINE-12.                                                 DB2024.2
            032700      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB2024.2
            032800     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB2024.2
            032900         MOVE "NO " TO ERROR-TOTAL                                DB2024.2
            033000         ELSE                                                     DB2024.2
            033100         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB2024.2
            033200     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB2024.2
            033300     PERFORM WRITE-LINE.                                          DB2024.2
            033400 END-ROUTINE-13.                                                  DB2024.2
            033500     IF DELETE-CNT IS EQUAL TO ZERO                               DB2024.2
            033600         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB2024.2
            033700         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB2024.2
            033800     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB2024.2
            033900     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2024.2
            034000      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB2024.2
            034100          MOVE "NO " TO ERROR-TOTAL                               DB2024.2
            034200      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB2024.2
            034300      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB2024.2
            034400      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB2024.2
            034500     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2024.2
            034600 WRITE-LINE.                                                      DB2024.2
            034700     ADD 1 TO RECORD-COUNT.                                       DB2024.2
            034800Y    IF RECORD-COUNT GREATER 50                                   DB2024.2
            034900Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB2024.2
            035000Y        MOVE SPACE TO DUMMY-RECORD                               DB2024.2
            035100Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB2024.2
            035200Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB2024.2
            035300Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB2024.2
            035400Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB2024.2
            035500Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB2024.2
            035600Y        MOVE ZERO TO RECORD-COUNT.                               DB2024.2
            035700     PERFORM WRT-LN.                                              DB2024.2
            035800 WRT-LN.                                                          DB2024.2
            035900     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB2024.2
            036000     MOVE SPACE TO DUMMY-RECORD.                                  DB2024.2
            036100 BLANK-LINE-PRINT.                                                DB2024.2
            036200     PERFORM WRT-LN.                                              DB2024.2
            036300 FAIL-ROUTINE.                                                    DB2024.2
            036400     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB2024.2
            036500     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB2024.2
            036600     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB2024.2
            036700     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2024.2
            036800     GO TO FAIL-ROUTINE-EX.                                       DB2024.2
            036900 FAIL-ROUTINE-WRITE.                                              DB2024.2
            037000     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB2024.2
            037100     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB2024.2
            037200 FAIL-ROUTINE-EX. EXIT.                                           DB2024.2
            037300 BAIL-OUT.                                                        DB2024.2
            037400     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB2024.2
            037500     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB2024.2
            037600 BAIL-OUT-WRITE.                                                  DB2024.2
            037700     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB2024.2
            037800     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2024.2
            037900 BAIL-OUT-EX. EXIT.                                               DB2024.2
            038000 CCVS1-EXIT.                                                      DB2024.2
            038100     EXIT.                                                        DB2024.2
            038200 BEGIN-DB202A-TEST SECTION.                                       DB2024.2
            038300 OPEN-TEST-1-INIT.                                                DB2024.2
            038400     MOVE SPACES TO ITEM-1.                                       DB2024.2
            038500     MOVE 0 TO KEY-1.                                             DB2024.2
            038600     MOVE "OPEN-TEST-1" TO PAR-NAME.                              DB2024.2
            038700     MOVE "DEBUG OPEN FILENAME" TO FEATURE.                       DB2024.2
            038800******************************************************************DB2024.2
            038900*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2024.2
            039000*    "OPEN-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT     *DB2024.2
            039100*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS, "OPEN       *DB2024.2
            039200*    OUTPUT SEQ-FILE.".                                          *DB2024.2
            039300******************************************************************DB2024.2
            039400 OPEN-TEST-1.                                                     DB2024.2
            039500     OPEN OUTPUT SEQ-FILE.                                        DB2024.2
            039600     IF KEY-1 IS EQUAL TO 1                                       DB2024.2
            039700         PERFORM PASS                                             DB2024.2
            039800         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2024.2
            039900         PERFORM GEN-WRITE                                        DB2024.2
            040000         GO TO OPEN-TEST-1A                                       DB2024.2
            040100     ELSE PERFORM FAIL                                            DB2024.2
            040200         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2024.2
            040300         PERFORM GEN-WRITE                                        DB2024.2
            040400         PERFORM DELETE-OPEN-TEST-1-SUBTESTS                      DB2024.2
            040500         GO TO WRITE-TEST-1-INIT.                                 DB2024.2
            040600 OPEN-TEST-1-DELETE.                                              DB2024.2
            040700     PERFORM DE-LETE.                                             DB2024.2
            040800     PERFORM GEN-WRITE.                                           DB2024.2
            040900     PERFORM DELETE-OPEN-TEST-1-SUBTESTS.                         DB2024.2
            041000     GO TO WRITE-TEST-1-INIT.                                     DB2024.2
            041100 DELETE-OPEN-TEST-1-SUBTESTS.                                     DB2024.2
            041200     MOVE "OPEN-TEST-1A" TO PAR-NAME.                             DB2024.2
            041300     PERFORM DE-LETE.                                             DB2024.2
            041400     PERFORM GEN-WRITE.                                           DB2024.2
            041500     MOVE "OPEN-TEST-1B" TO PAR-NAME.                             DB2024.2
            041600     PERFORM DE-LETE.                                             DB2024.2
            041700     PERFORM GEN-WRITE.                                           DB2024.2
            041800     MOVE "OPEN-TEST-1C" TO PAR-NAME.                             DB2024.2
            041900     PERFORM DE-LETE.                                             DB2024.2
            042000     PERFORM GEN-WRITE.                                           DB2024.2
            042100 OPEN-TEST-1A.                                                    DB2024.2
            042200     MOVE "OPEN-TEST-1A" TO PAR-NAME.                             DB2024.2
            042300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2024.2
            042400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2024.2
            042500     MOVE LINE-1 TO COMPUTED-A.                                   DB2024.2
            042600     PERFORM INSPT.                                               DB2024.2
            042700     PERFORM GEN-WRITE.                                           DB2024.2
            042800 OPEN-TEST-1B.                                                    DB2024.2
            042900     MOVE "OPEN-TEST-1B" TO PAR-NAME.                             DB2024.2
            043000     IF UNQUAL-NAME-1 IS EQUAL TO "SEQ-FILE"                      DB2024.2
            043100         PERFORM PASS                                             DB2024.2
            043200     ELSE  PERFORM FAIL                                           DB2024.2
            043300         MOVE NAME-1 TO COMPUTED-A                                DB2024.2
            043400     MOVE "SEQ-FILE" TO CORRECT-A.                                DB2024.2
            043500     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2024.2
            043600     PERFORM GEN-WRITE.                                           DB2024.2
            043700 OPEN-TEST-1C.                                                    DB2024.2
            043800     MOVE "OPEN-TEST-1C" TO PAR-NAME.                             DB2024.2
            043900     IF CONTENTS-1 IS EQUAL TO SPACES                             DB2024.2
            044000         PERFORM PASS                                             DB2024.2
            044100     ELSE PERFORM FAIL                                            DB2024.2
            044200         MOVE CONTENTS-1 TO COMPUTED-A                            DB2024.2
            044300         MOVE "(SPACES)" TO CORRECT-A.                            DB2024.2
            044400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2024.2
            044500     PERFORM GEN-WRITE.                                           DB2024.2
            044600 WRITE-TEST-1-INIT.                                               DB2024.2
            044700     MOVE "WRITE-TEST-1" TO PAR-NAME.                             DB2024.2
            044800     MOVE SPACES TO ITEM-1.                                       DB2024.2
            044900     MOVE 0 TO KEY-1.                                             DB2024.2
            045000     MOVE "DEBUG WRITE/ALL REF" TO FEATURE.                       DB2024.2
            045100     MOVE FILE-RECORD-INFO-SKELETON TO FILE-RECORD-INFO (1).      DB2024.2
            045200     MOVE "SEQ-FI" TO XFILE-NAME (1).                             DB2024.2
            045300     MOVE "REC-1" TO XRECORD-NAME (1).                            DB2024.2
            045400     MOVE ".XXX." TO XPROGRAM-NAME (1).                           DB2024.2
            045500     MOVE 120 TO XRECORD-LENGTH (1).                              DB2024.2
            045600     MOVE "RC" TO CHARS-OR-RECORDS (1).                           DB2024.2
            045700     MOVE 1 TO XBLOCK-SIZE (1).                                   DB2024.2
            045800     MOVE 30 TO RECORDS-IN-FILE (1).                              DB2024.2
            045900     MOVE "SQ" TO XFILE-ORGANIZATION (1).                         DB2024.2
            046000     MOVE "S" TO XLABEL-TYPE (1).                                 DB2024.2
            046100 WRITE-TEST-1.                                                    DB2024.2
            046200     PERFORM WRITE-REC-1  10  TIMES.                              DB2024.2
            046300     IF KEY-1 IS EQUAL TO 10                                      DB2024.2
            046400         PERFORM PASS                                             DB2024.2
            046500         MOVE "DEBUG PROC EXECUTED 10 TIMES" TO RE-MARK           DB2024.2
            046600     ELSE PERFORM FAIL                                            DB2024.2
            046700         MOVE "NO. TIMES DEBUG PROC EXECUTED" TO RE-MARK          DB2024.2
            046800         MOVE KEY-1 TO COMPUTED-18V0                              DB2024.2
            046900         MOVE 10 TO CORRECT-18V0.                                 DB2024.2
            047000     PERFORM GEN-WRITE.                                           DB2024.2
            047100     IF KEY-1 IS EQUAL TO 0                                       DB2024.2
            047200         PERFORM DELETE-WRITE-TEST-1-SUBTESTS                     DB2024.2
            047300         GO TO WRITE-TEST-2-INIT                                  DB2024.2
            047400     ELSE  GO TO WRITE-TEST-1A.                                   DB2024.2
            047500 WRITE-TEST-1-DELETE.                                             DB2024.2
            047600     PERFORM DE-LETE.                                             DB2024.2
            047700     PERFORM GEN-WRITE.                                           DB2024.2
            047800     PERFORM DELETE-WRITE-TEST-1-SUBTESTS                         DB2024.2
            047900     GO TO WRITE-TEST-2-INIT.                                     DB2024.2
            048000 DELETE-WRITE-TEST-1-SUBTESTS.                                    DB2024.2
            048100     MOVE "WRITE-TEST-1A" TO PAR-NAME.                            DB2024.2
            048200     PERFORM DE-LETE.                                             DB2024.2
            048300     PERFORM GEN-WRITE.                                           DB2024.2
            048400     MOVE "WRITE-TEST-1B" TO PAR-NAME.                            DB2024.2
            048500     PERFORM DE-LETE.                                             DB2024.2
            048600     PERFORM GEN-WRITE.                                           DB2024.2
            048700     MOVE "WRITE-TEST-1C" TO PAR-NAME.                            DB2024.2
            048800     PERFORM DE-LETE.                                             DB2024.2
            048900     PERFORM GEN-WRITE.                                           DB2024.2
            049000     MOVE "WRITE-TEST-1D" TO PAR-NAME.                            DB2024.2
            049100     PERFORM DE-LETE.                                             DB2024.2
            049200     PERFORM GEN-WRITE.                                           DB2024.2
            049300 WRITE-REC-1.                                                     DB2024.2
            049400     MOVE SPACES TO SEQ-REC-3.                                    DB2024.2
            049500     ADD 1 TO XRECORD-NUMBER (1).                                 DB2024.2
            049600******************************************************************DB2024.2
            049700*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2024.2
            049800*    "WRITE-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT    *DB2024.2
            049900*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS,             *DB2024.2
            050000*    "WRITE SEQ-REC-1 FROM FILE-RECORD-INFO-P1-120 (1).".        *DB2024.2
            050100******************************************************************DB2024.2
            050200     WRITE SEQ-REC-1 FROM FILE-RECORD-INFO-P1-120 (1).            DB2024.2
            050300 WRITE-TEST-1A.                                                   DB2024.2
            050400     MOVE "WRITE-TEST-1A" TO PAR-NAME.                            DB2024.2
            050500     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2024.2
            050600     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2024.2
            050700     MOVE LINE-1 TO COMPUTED-A.                                   DB2024.2
            050800     PERFORM INSPT.                                               DB2024.2
            050900     PERFORM GEN-WRITE.                                           DB2024.2
            051000 WRITE-TEST-1B.                                                   DB2024.2
            051100     MOVE "WRITE-TEST-1B" TO PAR-NAME.                            DB2024.2
            051200     IF UNQUAL-NAME-1 IS EQUAL TO "SEQ-REC-1"                     DB2024.2
            051300         PERFORM PASS                                             DB2024.2
            051400     ELSE  PERFORM FAIL                                           DB2024.2
            051500         MOVE "SEQ-REC-1" TO CORRECT-A                            DB2024.2
            051600         MOVE NAME-1 TO COMPUTED-A.                               DB2024.2
            051700     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2024.2
            051800     PERFORM GEN-WRITE.                                           DB2024.2
            051900 WRITE-TEST-1C.                                                   DB2024.2
            052000     MOVE "WRITE-TEST-1C" TO PAR-NAME.                            DB2024.2
            052100     IF CONTENTS-REC IS EQUAL TO FILE-RECORD-INFO-P1-120 (1)      DB2024.2
            052200         PERFORM PASS                                             DB2024.2
            052300         MOVE "PROC EXECUTED AT PROPER TIME" TO RE-MARK           DB2024.2
            052400         PERFORM GEN-WRITE                                        DB2024.2
            052500         GO TO WRITE-TEST-1D                                      DB2024.2
            052600     ELSE  PERFORM FAIL                                           DB2024.2
            052700         MOVE "PROC NOT EXEC BETW MOVE / WRITE" TO RE-MARK        DB2024.2
            052800         MOVE "1ST LINE = REC AREA" TO COMPUTED-A                 DB2024.2
            052900         MOVE "2ND LINE = FROM FLD" TO CORRECT-A                  DB2024.2
            053000         PERFORM GEN-WRITE.                                       DB2024.2
            053100     MOVE CONTENTS-REC TO PRINT-REC.                              DB2024.2
            053200     PERFORM WRITE-LINE.                                          DB2024.2
            053300     MOVE FILE-RECORD-INFO-P1-120 (1) TO PRINT-REC.               DB2024.2
            053400     PERFORM WRITE-LINE.                                          DB2024.2
            053500 WRITE-TEST-1D.                                                   DB2024.2
            053600     MOVE "WRITE-TEST-1D" TO PAR-NAME.                            DB2024.2
            053700     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2024.2
            053800     IF CONTENTS-1 IS EQUAL TO FILE-RECORD-INFO-P1-120 (1)        DB2024.2
            053900         PERFORM PASS                                             DB2024.2
            054000         PERFORM GEN-WRITE                                        DB2024.2
            054100         GO TO WRITE-TEST-2-INIT                                  DB2024.2
            054200     ELSE  PERFORM FAIL                                           DB2024.2
            054300         MOVE "SEE 1ST LINE FOLLOW" TO COMPUTED-A                 DB2024.2
            054400         MOVE "SEE 2ND LINE FOLLOW" TO CORRECT-A                  DB2024.2
            054500         PERFORM GEN-WRITE.                                       DB2024.2
            054600     MOVE CONTENTS-1 TO PRINT-REC.                                DB2024.2
            054700     PERFORM WRITE-LINE.                                          DB2024.2
            054800     MOVE FILE-RECORD-INFO-P1-120 (1) TO PRINT-REC.               DB2024.2
            054900     PERFORM WRITE-LINE.                                          DB2024.2
            055000 WRITE-TEST-2-INIT.                                               DB2024.2
            055100     MOVE "WRITE-TEST-2" TO PAR-NAME.                             DB2024.2
            055200     MOVE SPACES TO ITEM-1.                                       DB2024.2
            055300     MOVE 0 TO KEY-1.                                             DB2024.2
            055400     MOVE "DEBUG WRITE/NOT ALL" TO FEATURE.                       DB2024.2
            055500     MOVE 10 TO XRECORD-NUMBER (1).                               DB2024.2
            055600     MOVE "REC-2" TO XRECORD-NAME (1).                            DB2024.2
            055700 WRITE-TEST-2.                                                    DB2024.2
            055800     PERFORM WRITE-REC-2 10 TIMES.                                DB2024.2
            055900     IF KEY-1 IS EQUAL TO 10                                      DB2024.2
            056000         PERFORM PASS                                             DB2024.2
            056100         MOVE "DEBUG PROC EXECUTED 10 TIMES" TO RE-MARK           DB2024.2
            056200     ELSE  PERFORM FAIL                                           DB2024.2
            056300         MOVE "NO. TIMES DEBUG PROC EXECUTED" TO RE-MARK          DB2024.2
            056400         MOVE KEY-1 TO COMPUTED-18V0                              DB2024.2
            056500         MOVE 10 TO CORRECT-18V0.                                 DB2024.2
            056600     PERFORM GEN-WRITE.                                           DB2024.2
            056700     IF KEY-1 IS EQUAL TO 0                                       DB2024.2
            056800         PERFORM DELETE-WRITE-TEST-2-SUBTESTS                     DB2024.2
            056900         GO TO WRITE-TEST-3-INIT                                  DB2024.2
            057000     ELSE GO TO WRITE-TEST-2A.                                    DB2024.2
            057100 WRITE-TEST-2-DELETE.                                             DB2024.2
            057200     PERFORM DE-LETE.                                             DB2024.2
            057300     PERFORM GEN-WRITE.                                           DB2024.2
            057400     PERFORM DELETE-WRITE-TEST-2-SUBTESTS                         DB2024.2
            057500     GO TO WRITE-TEST-3-INIT.                                     DB2024.2
            057600 DELETE-WRITE-TEST-2-SUBTESTS.                                    DB2024.2
            057700     MOVE "WRITE-TEST-2A" TO PAR-NAME.                            DB2024.2
            057800     PERFORM DE-LETE.                                             DB2024.2
            057900     PERFORM GEN-WRITE.                                           DB2024.2
            058000     MOVE "WRITE-TEST-2B" TO PAR-NAME.                            DB2024.2
            058100     PERFORM DE-LETE.                                             DB2024.2
            058200     PERFORM GEN-WRITE.                                           DB2024.2
            058300     MOVE "WRITE-TEST-2C" TO PAR-NAME.                            DB2024.2
            058400     PERFORM DE-LETE.                                             DB2024.2
            058500     PERFORM GEN-WRITE.                                           DB2024.2
            058600 WRITE-REC-2.                                                     DB2024.2
            058700     MOVE SPACES TO SEQ-REC-3.                                    DB2024.2
            058800     ADD 1 TO XRECORD-NUMBER (1).                                 DB2024.2
            058900     WRITE SEQ-REC-2 FROM FILE-RECORD-INFO-P1-120 (1).            DB2024.2
            059000 WRITE-TEST-2A.                                                   DB2024.2
            059100     MOVE "WRITE-TEST-2A" TO PAR-NAME.                            DB2024.2
            059200     IF UNQUAL-NAME-1 IS EQUAL TO "SEQ-REC-2"                     DB2024.2
            059300         PERFORM PASS                                             DB2024.2
            059400     ELSE  PERFORM FAIL                                           DB2024.2
            059500         MOVE "SEQ-REC-2" TO CORRECT-A                            DB2024.2
            059600         MOVE NAME-1 TO COMPUTED-A.                               DB2024.2
            059700     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2024.2
            059800     PERFORM GEN-WRITE.                                           DB2024.2
            059900 WRITE-TEST-2B.                                                   DB2024.2
            060000     MOVE "WRITE-TEST-2B" TO PAR-NAME.                            DB2024.2
            060100     IF CONTENTS-REC IS EQUAL TO FILE-RECORD-INFO-P1-120 (1)      DB2024.2
            060200         PERFORM PASS                                             DB2024.2
            060300         MOVE "PROC EXECUTED AT PROPER TIME" TO RE-MARK           DB2024.2
            060400         PERFORM GEN-WRITE                                        DB2024.2
            060500         GO TO WRITE-TEST-2C                                      DB2024.2
            060600     ELSE  PERFORM FAIL                                           DB2024.2
            060700         MOVE "PROC NOT EXEC BTWN MOVE / WRITE" TO RE-MARK        DB2024.2
            060800         MOVE "1ST LINE = REC AREA" TO COMPUTED-A                 DB2024.2
            060900         MOVE "2ND LINE = FROM FLD" TO CORRECT-A                  DB2024.2
            061000         PERFORM GEN-WRITE.                                       DB2024.2
            061100     MOVE CONTENTS-REC TO PRINT-REC.                              DB2024.2
            061200     PERFORM WRITE-LINE.                                          DB2024.2
            061300     MOVE FILE-RECORD-INFO-P1-120 (1) TO PRINT-REC.               DB2024.2
            061400     PERFORM WRITE-LINE.                                          DB2024.2
            061500 WRITE-TEST-2C.                                                   DB2024.2
            061600     MOVE "WRITE-TEST-2C" TO PAR-NAME.                            DB2024.2
            061700     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2024.2
            061800     IF CONTENTS-1 IS EQUAL TO FILE-RECORD-INFO-P1-120 (1)        DB2024.2
            061900         PERFORM PASS                                             DB2024.2
            062000         PERFORM GEN-WRITE                                        DB2024.2
            062100         GO TO WRITE-TEST-3-INIT                                  DB2024.2
            062200     ELSE  PERFORM FAIL                                           DB2024.2
            062300         MOVE "SEE 1ST LINE FOLLOW" TO COMPUTED-A                 DB2024.2
            062400         MOVE "SEE 2ND LINE FOLLOW" TO CORRECT-A                  DB2024.2
            062500         PERFORM GEN-WRITE.                                       DB2024.2
            062600     MOVE CONTENTS-1 TO PRINT-REC.                                DB2024.2
            062700     PERFORM WRITE-LINE.                                          DB2024.2
            062800     MOVE FILE-RECORD-INFO-P1-120 (1) TO PRINT-REC.               DB2024.2
            062900     PERFORM WRITE-LINE.                                          DB2024.2
            063000 WRITE-TEST-3-INIT.                                               DB2024.2
            063100     MOVE SPACES TO ITEM-1                                        DB2024.2
            063200     MOVE 0 TO KEY-1                                              DB2024.2
            063300     MOVE "WRITE/NO DEBUG PROC" TO FEATURE.                       DB2024.2
            063400     MOVE "WRITE-TEST-3" TO PAR-NAME.                             DB2024.2
            063500     MOVE "REC-3" TO XRECORD-NAME (1)                             DB2024.2
            063600     MOVE 20 TO XRECORD-NUMBER (1).                               DB2024.2
            063700 WRITE-TEST-3.                                                    DB2024.2
            063800     PERFORM WRITE-REC-3 10 TIMES.                                DB2024.2
            063900     IF KEY-1 IS EQUAL TO 0                                       DB2024.2
            064000         PERFORM PASS                                             DB2024.2
            064100         MOVE  "DEBUG PROC NOT EXECUTED" TO RE-MARK               DB2024.2
            064200     ELSE  PERFORM FAIL                                           DB2024.2
            064300         MOVE "NO. TIMES DEBUG PROC EXECUTED" TO RE-MARK          DB2024.2
            064400         MOVE KEY-1 TO COMPUTED-18V0                              DB2024.2
            064500         MOVE 0 TO CORRECT-18V0.                                  DB2024.2
            064600         PERFORM GEN-WRITE.                                       DB2024.2
            064700     GO TO CLOSE-TEST-1-INIT.                                     DB2024.2
            064800 WRITE-TEST-3-DELETE.                                             DB2024.2
            064900     PERFORM DE-LETE.                                             DB2024.2
            065000     PERFORM GEN-WRITE.                                           DB2024.2
            065100     GO TO CLOSE-TEST-1-INIT.                                     DB2024.2
            065200 WRITE-REC-3.                                                     DB2024.2
            065300     MOVE SPACES TO SEQ-REC-3.                                    DB2024.2
            065400     ADD 1 TO XRECORD-NUMBER (1).                                 DB2024.2
            065500     WRITE SEQ-REC-3 FROM FILE-RECORD-INFO-P1-120 (1).            DB2024.2
            065600 CLOSE-TEST-1-INIT.                                               DB2024.2
            065700     MOVE SPACES TO ITEM-1.                                       DB2024.2
            065800     MOVE 0 TO KEY-1.                                             DB2024.2
            065900     MOVE "CLOSE-TEST-1" TO PAR-NAME.                             DB2024.2
            066000     MOVE "DEBUG CLOSE FILENAME" TO FEATURE.                      DB2024.2
            066100******************************************************************DB2024.2
            066200*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2024.2
            066300*    "CLOSE-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT    *DB2024.2
            066400*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS,             *DB2024.2
            066500*    "CLOSE SEQ-FILE.".                                          *DB2024.2
            066600******************************************************************DB2024.2
            066700 CLOSE-TEST-1.                                                    DB2024.2
            066800     CLOSE SEQ-FILE.                                              DB2024.2
            066900     IF KEY-1 IS EQUAL TO 1                                       DB2024.2
            067000         PERFORM PASS                                             DB2024.2
            067100         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2024.2
            067200         PERFORM GEN-WRITE                                        DB2024.2
            067300         GO TO CLOSE-TEST-1A                                      DB2024.2
            067400     ELSE PERFORM  FAIL                                           DB2024.2
            067500         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2024.2
            067600         PERFORM GEN-WRITE                                        DB2024.2
            067700         PERFORM DELETE-CLOSE-TEST-1-SUBTESTS                     DB2024.2
            067800         GO TO OPEN-TEST-2-INIT.                                  DB2024.2
            067900 CLOSE-TEST-1-DELETE.                                             DB2024.2
            068000     PERFORM DE-LETE.                                             DB2024.2
            068100     PERFORM GEN-WRITE.                                           DB2024.2
            068200     PERFORM DELETE-CLOSE-TEST-1-SUBTESTS.                        DB2024.2
            068300     GO TO OPEN-TEST-2-INIT.                                      DB2024.2
            068400 DELETE-CLOSE-TEST-1-SUBTESTS.                                    DB2024.2
            068500     MOVE "CLOSE-TEST-1A" TO PAR-NAME.                            DB2024.2
            068600     PERFORM DE-LETE.                                             DB2024.2
            068700     PERFORM GEN-WRITE.                                           DB2024.2
            068800     MOVE "CLOSE-TEST-1B" TO PAR-NAME.                            DB2024.2
            068900     PERFORM DE-LETE.                                             DB2024.2
            069000     PERFORM GEN-WRITE.                                           DB2024.2
            069100     MOVE "CLOSE-TEST-1C" TO PAR-NAME.                            DB2024.2
            069200     PERFORM DE-LETE.                                             DB2024.2
            069300     PERFORM GEN-WRITE.                                           DB2024.2
            069400 CLOSE-TEST-1A.                                                   DB2024.2
            069500     MOVE "CLOSE-TEST-1A" TO PAR-NAME.                            DB2024.2
            069600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2024.2
            069700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2024.2
            069800     MOVE LINE-1 TO COMPUTED-A.                                   DB2024.2
            069900     PERFORM INSPT.                                               DB2024.2
            070000     PERFORM GEN-WRITE.                                           DB2024.2
            070100 CLOSE-TEST-1B.                                                   DB2024.2
            070200     MOVE "CLOSE-TEST-1B" TO PAR-NAME.                            DB2024.2
            070300     IF UNQUAL-NAME-1 IS EQUAL TO "SEQ-FILE"                      DB2024.2
            070400         PERFORM PASS                                             DB2024.2
            070500     ELSE  PERFORM FAIL                                           DB2024.2
            070600         MOVE NAME-1 TO COMPUTED-A                                DB2024.2
            070700         MOVE "SEQ-FILE" TO CORRECT-A.                            DB2024.2
            070800     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2024.2
            070900     PERFORM GEN-WRITE.                                           DB2024.2
            071000 CLOSE-TEST-1C.                                                   DB2024.2
            071100     MOVE "OPEN-TEST-1C" TO PAR-NAME.                             DB2024.2
            071200     IF CONTENTS-1 IS EQUAL TO SPACES                             DB2024.2
            071300         PERFORM PASS                                             DB2024.2
            071400     ELSE PERFORM FAIL                                            DB2024.2
            071500         MOVE CONTENTS-1 TO COMPUTED-A                            DB2024.2
            071600         MOVE "(SPACES)" TO CORRECT-A.                            DB2024.2
            071700     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2024.2
            071800     PERFORM GEN-WRITE.                                           DB2024.2
            071900 OPEN-TEST-2-INIT.                                                DB2024.2
            072000     MOVE SPACES TO ITEM-1.                                       DB2024.2
            072100     MOVE 0 TO KEY-1.                                             DB2024.2
            072200     MOVE "OPEN-TEST-2" TO PAR-NAME.                              DB2024.2
            072300     MOVE "DEBUG OPEN FILENAME" TO FEATURE.                       DB2024.2
            072400 OPEN-TEST-2.                                                     DB2024.2
            072500     OPEN INPUT SEQ-FILE.                                         DB2024.2
            072600     IF KEY-1 IS EQUAL TO 1                                       DB2024.2
            072700         PERFORM PASS                                             DB2024.2
            072800         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2024.2
            072900     ELSE PERFORM FAIL                                            DB2024.2
            073000         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK.               DB2024.2
            073100     PERFORM GEN-WRITE.                                           DB2024.2
            073200     GO TO READ-TEST-1-INIT.                                      DB2024.2
            073300 OPEN-TEST-2-DELETE.                                              DB2024.2
            073400     PERFORM DE-LETE.                                             DB2024.2
            073500     PERFORM GEN-WRITE.                                           DB2024.2
            073600 READ-TEST-1-INIT.                                                DB2024.2
            073700     MOVE SPACES TO ITEM-1.                                       DB2024.2
            073800     MOVE 0 TO KEY-1.                                             DB2024.2
            073900     MOVE "READ-TEST-1" TO PAR-NAME.                              DB2024.2
            074000     MOVE "DEBUG READ FILENAME" TO FEATURE.                       DB2024.2
            074100******************************************************************DB2024.2
            074200*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2024.2
            074300*    "READ-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT     *DB2024.2
            074400*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS, "READ       *DB2024.2
            074500*    SEQ-FILE AT END PERFORM READ-TEST-1-DELETE GO TO            *DB2024.2
            074600*    READ-TEST-2.".                                              *DB2024.2
            074700******************************************************************DB2024.2
            074800 READ-TEST-1.                                                     DB2024.2
            074900     READ SEQ-FILE  AT END                                        DB2024.2
            075000         PERFORM READ-TEST-1-DELETE                               DB2024.2
            075100         GO TO READ-TEST-2.                                       DB2024.2
            075200     IF KEY-1 IS EQUAL TO 1                                       DB2024.2
            075300         PERFORM PASS                                             DB2024.2
            075400         MOVE "DEBUG PROC EXECUTED" TO RE-MARK                    DB2024.2
            075500         PERFORM GEN-WRITE                                        DB2024.2
            075600         GO TO READ-TEST-1A                                       DB2024.2
            075700     ELSE  PERFORM FAIL                                           DB2024.2
            075800         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2024.2
            075900         PERFORM GEN-WRITE                                        DB2024.2
            076000         PERFORM DELETE-READ-TEST-1-SUBTESTS                      DB2024.2
            076100          GO TO READ-TEST-2-INIT.                                 DB2024.2
            076200 READ-TEST-1-DELETE.                                              DB2024.2
            076300     PERFORM DE-LETE.                                             DB2024.2
            076400     PERFORM GEN-WRITE.                                           DB2024.2
            076500     PERFORM DELETE-READ-TEST-1-SUBTESTS.                         DB2024.2
            076600 READ-TEST-1-DELETE-A.                                            DB2024.2
            076700     GO TO READ-TEST-2-DELETE.                                    DB2024.2
            076800 DELETE-READ-TEST-1-SUBTESTS.                                     DB2024.2
            076900     MOVE "READ-TEST-1A" TO PAR-NAME.                             DB2024.2
            077000     PERFORM DE-LETE.                                             DB2024.2
            077100     PERFORM GEN-WRITE.                                           DB2024.2
            077200     MOVE "READ-TEST-1B" TO PAR-NAME.                             DB2024.2
            077300     PERFORM DE-LETE.                                             DB2024.2
            077400     PERFORM GEN-WRITE.                                           DB2024.2
            077500     MOVE "READ-TEST-1C" TO PAR-NAME.                             DB2024.2
            077600     PERFORM DE-LETE.                                             DB2024.2
            077700     PERFORM GEN-WRITE.                                           DB2024.2
            077800 READ-TEST-1A.                                                    DB2024.2
            077900     MOVE "READ-TEST-1A" TO PAR-NAME.                             DB2024.2
            078000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2024.2
            078100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2024.2
            078200     MOVE LINE-1 TO COMPUTED-A.                                   DB2024.2
            078300     PERFORM INSPT.                                               DB2024.2
            078400     PERFORM GEN-WRITE.                                           DB2024.2
            078500 READ-TEST-1B.                                                    DB2024.2
            078600     MOVE "READ-TEST-1B" TO PAR-NAME.                             DB2024.2
            078700     IF UNQUAL-NAME-1 IS EQUAL TO "SEQ-FILE"                      DB2024.2
            078800         PERFORM PASS                                             DB2024.2
            078900     ELSE PERFORM FAIL                                            DB2024.2
            079000         MOVE "SEQ-FILE" TO CORRECT-A                             DB2024.2
            079100         MOVE NAME-1 TO COMPUTED-A.                               DB2024.2
            079200     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2024.2
            079300     PERFORM GEN-WRITE.                                           DB2024.2
            079400 READ-TEST-1C.                                                    DB2024.2
            079500     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2024.2
            079600     MOVE "READ-TEST-1C" TO PAR-NAME.                             DB2024.2
            079700     IF CONTENTS-1 IS EQUAL TO SEQ-REC-3                          DB2024.2
            079800         PERFORM PASS                                             DB2024.2
            079900         PERFORM GEN-WRITE                                        DB2024.2
            080000         GO TO READ-TEST-2-INIT                                   DB2024.2
            080100     ELSE PERFORM FAIL                                            DB2024.2
            080200         MOVE "SEE 1ST LINE FOLLOW" TO COMPUTED-A                 DB2024.2
            080300         MOVE "SEE 2ND LINE FOLLOW" TO CORRECT-A                  DB2024.2
            080400         PERFORM GEN-WRITE.                                       DB2024.2
            080500     MOVE CONTENTS-1 TO PRINT-REC.                                DB2024.2
            080600     PERFORM WRITE-LINE.                                          DB2024.2
            080700     MOVE SEQ-REC-3 TO PRINT-REC.                                 DB2024.2
            080800     PERFORM WRITE-LINE.                                          DB2024.2
            080900 READ-TEST-2-INIT.                                                DB2024.2
            081000     MOVE SPACES TO ITEM-1.                                       DB2024.2
            081100     MOVE 0 TO KEY-1.                                             DB2024.2
            081200     READ SEQ-FILE  AT END GO TO READ-TEST-2.                     DB2024.2
            081300     GO TO READ-TEST-2-INIT.                                      DB2024.2
            081400 READ-TEST-2-DELETE.                                              DB2024.2
            081500     MOVE "DEBUG READ AT END" TO FEATURE.                         DB2024.2
            081600     MOVE "READ-TEST-2" TO PAR-NAME.                              DB2024.2
            081700     PERFORM DE-LETE.                                             DB2024.2
            081800     PERFORM GEN-WRITE.                                           DB2024.2
            081900     GO TO CLOSE-SEQ-FILE.                                        DB2024.2
            082000 READ-TEST-2.                                                     DB2024.2
            082100     MOVE "DEBUG READ AT END" TO FEATURE.                         DB2024.2
            082200     MOVE "READ-TEST-2" TO PAR-NAME.                              DB2024.2
            082300     IF KEY-1 IS EQUAL TO 0                                       DB2024.2
            082400         PERFORM PASS                                             DB2024.2
            082500         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2024.2
            082600     ELSE  PERFORM FAIL                                           DB2024.2
            082700         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK.               DB2024.2
            082800 GEN-WRITE.                                                       DB2024.2
            082900     PERFORM PRINT-DETAIL.                                        DB2024.2
            083000 CLOSE-SEQ-FILE.                                                  DB2024.2
            083100     CLOSE SEQ-FILE.                                              DB2024.2
            083200XTAPE-DUMP SECTION.                                               DB2024.2
            083300XOPEN-FILE.                                                       DB2024.2
            083400X    OPEN INPUT SEQ-FILE.                                         DB2024.2
            083500XREAD-FILE.                                                       DB2024.2
            083600X    READ SEQ-FILE AT END GO TO CLOSE-FILE.                       DB2024.2
            083700X    MOVE SEQ-REC-3 TO PRINT-REC.                                 DB2024.2
            083800X    PERFORM WRITE-LINE.                                          DB2024.2
            083900X    GO TO READ-FILE.                                             DB2024.2
            084000XCLOSE-FILE.                                                      DB2024.2
            084100X    CLOSE SEQ-FILE.                                              DB2024.2
            084200 CCVS-EXIT SECTION.                                               DB2024.2
            084300 CCVS-999999.                                                     DB2024.2
            084400     GO TO CLOSE-FILES.                                           DB2024.2
                  *END-OF,DB202A                                                            
        """)
    )

    @Disabled("Requires X, Y, G, C, J indicators")
    @Test
    fun db2034_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB203A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB2034.2
            000200 PROGRAM-ID.                                                      DB2034.2
            000300     DB203A.                                                      DB2034.2
            000400 AUTHOR.                                                          DB2034.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB2034.2
            000600 INSTALLATION.                                                    DB2034.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB2034.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB2034.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB2034.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB2034.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB2034.2
            001200                                                                  DB2034.2
            001300     PHONE   (703) 756-6153                                       DB2034.2
            001400                                                                  DB2034.2
            001500     " HIGH       ".                                              DB2034.2
            001600 DATE-WRITTEN.                                                    DB2034.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB2034.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB2034.2
            001900     "4.2 ".                                                      DB2034.2
            002000 SECURITY.                                                        DB2034.2
            002100     NONE.                                                        DB2034.2
            002200*                                                                 DB2034.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB2034.2
            002400*                                                                 DB2034.2
            002500*                       PROGRAM ABSTRACT                          DB2034.2
            002600*                                                                 DB2034.2
            002700*    DB203A TESTS THE CAPABILITY OF THE DEBUG MODULE TO HANDLE    DB2034.2
            002800*    DEBUGGING PROCEDURES WHICH ARE MONITORING I-O FUNCTIONS      DB2034.2
            002900*    OF THE RELATIVE I-O OR INDEXED I-O MODULES.  THIS PROGRAM    DB2034.2
            003000*    IS TO BE COMPILED AND EXECUTED WITH BOTH COMPILE AND OBJECT  DB2034.2
            003100*    TIME DEBUGGING SWITCHES ON.  THE DEBUGGING PROCEDURES        DB2034.2
            003200*    SHOULD BE INCLUDED IN COMPILATION AND GENERATE CODE.         DB2034.2
            003300*    DURING EXECUTION, A FILE IS ASSIGNED IN DYNAMIC MODE,        DB2034.2
            003400*    CREATED SEQUENTIALLY, AND ACCESSED BOTH SEQUENTIALLY         DB2034.2
            003500*    AND RANDOMLY.  ITS RECORDS ARE 80 CHARACTERS IN LENGTH.      DB2034.2
            003600*    EXECUTION OF "OPEN", "READ", "WRITE", "REWRITE", "START",    DB2034.2
            003700*    AND "DELETE" FUNCTIONS SHOULD TRIGGER THE APPROPRIATE        DB2034.2
            003800*    DEBUGGING PROCEDURES.                                        DB2034.2
            003900*                                                                 DB2034.2
            004000*                                                                 DB2034.2
            004100*                                                                 DB2034.2
            004200 ENVIRONMENT DIVISION.                                            DB2034.2
            004300 CONFIGURATION SECTION.                                           DB2034.2
            004400 SOURCE-COMPUTER.                                                 DB2034.2
            004500     XXXXX082                                                     DB2034.2
            004600         WITH DEBUGGING MODE.                                     DB2034.2
            004700 OBJECT-COMPUTER.                                                 DB2034.2
            004800     XXXXX083.                                                    DB2034.2
            004900 SPECIAL-NAMES.                                                   DB2034.2
            005000     XXXXX056                                                     DB2034.2
            005100*      XXXXX056  REPLACE WITH DISPLAY IMPLEMENTOR NAME            DB2034.2
            005200         IS THE-SYSTEM-PRINTER.                                   DB2034.2
            005300 INPUT-OUTPUT SECTION.                                            DB2034.2
            005400 FILE-CONTROL.                                                    DB2034.2
            005500     SELECT PRINT-FILE ASSIGN TO                                  DB2034.2
            005600     XXXXX055.                                                    DB2034.2
            005700     SELECT IND-FILE ASSIGN TO                                    DB2034.2
            005800     XXXXX024                                                     DB2034.2
            005900J    XXXXX044                                                     DB2034.2
            006000*      XXXXX044  REPLACE WITH INDEX-FILE-NAME (*OPT J ONLY)       DB2034.2
            006100     ORGANIZATION IS INDEXED                                      DB2034.2
            006200     ACCESS MODE IS DYNAMIC                                       DB2034.2
            006300     RECORD KEY IS IND-KEY.                                       DB2034.2
            006400 DATA DIVISION.                                                   DB2034.2
            006500 FILE SECTION.                                                    DB2034.2
            006600 FD  PRINT-FILE                                                   DB2034.2
            006700     LABEL RECORDS                                                DB2034.2
            006800     XXXXX084                                                     DB2034.2
            006900     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB2034.2
            007000 01  PRINT-REC PICTURE X(120).                                    DB2034.2
            007100 01  DUMMY-RECORD PICTURE X(120).                                 DB2034.2
            007200 FD  IND-FILE                                                     DB2034.2
            007300C    VALUE OF                                                     DB2034.2
            007400C    XXXXX074                                                     DB2034.2
            007500*      XXXXX074  REPLACE WITH IMPLEMENTOR NAME (*OPT C ONLY)      DB2034.2
            007600C    IS                                                           DB2034.2
            007700C    XXXXX075                                                     DB2034.2
            007800*      XXXXX075  REPLACE WITH VALUE CLAUSE OBJECT (*OPT C ONLY)   DB2034.2
            007900G    XXXXX069                                                     DB2034.2
            008000*      XXXXX069  REPLACE WITH ADDITIONAL INFO (*OPT G ONLY)       DB2034.2
            008100     LABEL RECORDS ARE STANDARD.                                  DB2034.2
            008200 01  IND-REC-1.                                                   DB2034.2
            008300     02  FILLER PIC X(128).                                       DB2034.2
            008400     02  IND-KEY PIC XX.                                          DB2034.2
            008500     02  FILLER PIC X(110).                                       DB2034.2
            008600 01  IND-REC-2.                                                   DB2034.2
            008700     02  IND-REC-1H PIC X(120).                                   DB2034.2
            008800     02  IND-REC-2H PIC X(120).                                   DB2034.2
            008900 WORKING-STORAGE SECTION.                                         DB2034.2
            009000 01  ITEM-1.                                                      DB2034.2
            009100     02  KEY-1 PIC 99.                                            DB2034.2
            009200     02  LINE-1 PIC X(6).                                         DB2034.2
            009300     02  NAME-1 PIC X(30).                                        DB2034.2
            009400     02  UNQUAL-NAME-1 PIC X(30).                                 DB2034.2
            009500     02  CONTENTS-1.                                              DB2034.2
            009600         03  CONTENTS-1-1H PIC X(120).                            DB2034.2
            009700         03  CONTENTS-1-2H PIC X(120).                            DB2034.2
            009800     02  CONTENTS-REC.                                            DB2034.2
            009900         03  CONTENTS-REC-1H PIC X(120).                          DB2034.2
            010000         03  CONTENTS-REC-2H PIC X(120).                          DB2034.2
            010100 01  FILE-RECORD-INFORMATION-REC.                                 DB2034.2
            010200     03 FILE-RECORD-INFO-SKELETON.                                DB2034.2
            010300        05 FILLER                 PICTURE X(48)       VALUE       DB2034.2
            010400             "FILE=      ,RECORD=      /0,RECNO=000000,UPDT=00".  DB2034.2
            010500        05 FILLER                 PICTURE X(46)       VALUE       DB2034.2
            010600             ",ODO=0000,PGM=     ,LRECL=000000,BLKSIZ  =0000".    DB2034.2
            010700        05 FILLER                 PICTURE X(26)       VALUE       DB2034.2
            010800             ",LFIL=000000,ORG=  ,LBLR= ".                        DB2034.2
            010900        05 FILLER                 PICTURE X(37)       VALUE       DB2034.2
            011000             ",RECKEY=                             ".             DB2034.2
            011100        05 FILLER                 PICTURE X(38)       VALUE       DB2034.2
            011200             ",ALTKEY1=                             ".            DB2034.2
            011300        05 FILLER                 PICTURE X(38)       VALUE       DB2034.2
            011400             ",ALTKEY2=                             ".            DB2034.2
            011500        05 FILLER                 PICTURE X(7)        VALUE SPACE.DB2034.2
            011600     03 FILE-RECORD-INFO          OCCURS  10  TIMES.              DB2034.2
            011700        05 FILE-RECORD-INFO-P1-120.                               DB2034.2
            011800           07 FILLER              PIC X(5).                       DB2034.2
            011900           07 XFILE-NAME           PIC X(6).                      DB2034.2
            012000           07 FILLER              PIC X(8).                       DB2034.2
            012100           07 XRECORD-NAME         PIC X(6).                      DB2034.2
            012200           07 FILLER              PIC X(1).                       DB2034.2
            012300           07 REELUNIT-NUMBER     PIC 9(1).                       DB2034.2
            012400           07 FILLER              PIC X(7).                       DB2034.2
            012500           07 XRECORD-NUMBER       PIC 9(6).                      DB2034.2
            012600           07 FILLER              PIC X(6).                       DB2034.2
            012700           07 UPDATE-NUMBER       PIC 9(2).                       DB2034.2
            012800           07 FILLER              PIC X(5).                       DB2034.2
            012900           07 ODO-NUMBER          PIC 9(4).                       DB2034.2
            013000           07 FILLER              PIC X(5).                       DB2034.2
            013100           07 XPROGRAM-NAME        PIC X(5).                      DB2034.2
            013200           07 FILLER              PIC X(7).                       DB2034.2
            013300           07 XRECORD-LENGTH       PIC 9(6).                      DB2034.2
            013400           07 FILLER              PIC X(7).                       DB2034.2
            013500           07 CHARS-OR-RECORDS    PIC X(2).                       DB2034.2
            013600           07 FILLER              PIC X(1).                       DB2034.2
            013700           07 XBLOCK-SIZE          PIC 9(4).                      DB2034.2
            013800           07 FILLER              PIC X(6).                       DB2034.2
            013900           07 RECORDS-IN-FILE     PIC 9(6).                       DB2034.2
            014000           07 FILLER              PIC X(5).                       DB2034.2
            014100           07 XFILE-ORGANIZATION   PIC X(2).                      DB2034.2
            014200           07 FILLER              PIC X(6).                       DB2034.2
            014300           07 XLABEL-TYPE          PIC X(1).                      DB2034.2
            014400        05 FILE-RECORD-INFO-P121-240.                             DB2034.2
            014500           07 FILLER              PIC X(8).                       DB2034.2
            014600           07 XRECORD-KEY          PIC X(29).                     DB2034.2
            014700           07 FILLER              PIC X(9).                       DB2034.2
            014800           07 ALTERNATE-KEY1      PIC X(29).                      DB2034.2
            014900           07 FILLER              PIC X(9).                       DB2034.2
            015000           07 ALTERNATE-KEY2      PIC X(29).                      DB2034.2
            015100           07 FILLER              PIC X(7).                       DB2034.2
            015200 01  TEST-RESULTS.                                                DB2034.2
            015300     02 FILLER                    PICTURE X VALUE SPACE.          DB2034.2
            015400     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB2034.2
            015500     02 FILLER                    PICTURE X VALUE SPACE.          DB2034.2
            015600     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB2034.2
            015700     02 FILLER                    PICTURE X  VALUE SPACE.         DB2034.2
            015800     02  PAR-NAME.                                                DB2034.2
            015900       03 FILLER PICTURE X(12) VALUE SPACE.                       DB2034.2
            016000       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB2034.2
            016100       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB2034.2
            016200       03 FILLER PIC X(5) VALUE SPACE.                            DB2034.2
            016300     02 FILLER PIC X(10) VALUE SPACE.                             DB2034.2
            016400     02 RE-MARK PIC X(61).                                        DB2034.2
            016500 01  TEST-COMPUTED.                                               DB2034.2
            016600     02 FILLER PIC X(30) VALUE SPACE.                             DB2034.2
            016700     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB2034.2
            016800     02 COMPUTED-X.                                               DB2034.2
            016900     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB2034.2
            017000     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB2034.2
            017100     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB2034.2
            017200     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB2034.2
            017300     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB2034.2
            017400     03       CM-18V0 REDEFINES COMPUTED-A.                       DB2034.2
            017500         04 COMPUTED-18V0                   PICTURE -9(18).       DB2034.2
            017600         04 FILLER                          PICTURE X.            DB2034.2
            017700     03 FILLER PIC X(50) VALUE SPACE.                             DB2034.2
            017800 01  TEST-CORRECT.                                                DB2034.2
            017900     02 FILLER PIC X(30) VALUE SPACE.                             DB2034.2
            018000     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB2034.2
            018100     02 CORRECT-X.                                                DB2034.2
            018200     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB2034.2
            018300     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB2034.2
            018400     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB2034.2
            018500     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB2034.2
            018600     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB2034.2
            018700     03      CR-18V0 REDEFINES CORRECT-A.                         DB2034.2
            018800         04 CORRECT-18V0                    PICTURE -9(18).       DB2034.2
            018900         04 FILLER                          PICTURE X.            DB2034.2
            019000     03 FILLER PIC X(50) VALUE SPACE.                             DB2034.2
            019100 01  CCVS-C-1.                                                    DB2034.2
            019200     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB2034.2
            019300-    "SS  PARAGRAPH-NAME                                          DB2034.2
            019400-    "        REMARKS".                                           DB2034.2
            019500     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB2034.2
            019600 01  CCVS-C-2.                                                    DB2034.2
            019700     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2034.2
            019800     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB2034.2
            019900     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB2034.2
            020000     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB2034.2
            020100     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB2034.2
            020200 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB2034.2
            020300 01  REC-CT PICTURE 99 VALUE ZERO.                                DB2034.2
            020400 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB2034.2
            020500 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB2034.2
            020600 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB2034.2
            020700 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB2034.2
            020800 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB2034.2
            020900 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB2034.2
            021000 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB2034.2
            021100 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB2034.2
            021200 01  CCVS-H-1.                                                    DB2034.2
            021300     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB2034.2
            021400     02 FILLER PICTURE X(67) VALUE                                DB2034.2
            021500     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB2034.2
            021600-    " SYSTEM".                                                   DB2034.2
            021700     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB2034.2
            021800 01  CCVS-H-2.                                                    DB2034.2
            021900     02 FILLER PICTURE X(52) VALUE IS                             DB2034.2
            022000     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB2034.2
            022100     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB2034.2
            022200     02 TEST-ID PICTURE IS X(9).                                  DB2034.2
            022300     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB2034.2
            022400 01  CCVS-H-3.                                                    DB2034.2
            022500     02  FILLER PICTURE X(34) VALUE                               DB2034.2
            022600     " FOR OFFICIAL USE ONLY    ".                                DB2034.2
            022700     02  FILLER PICTURE X(58) VALUE                               DB2034.2
            022800     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB2034.2
            022900     02  FILLER PICTURE X(28) VALUE                               DB2034.2
            023000     "  COPYRIGHT   1974 ".                                       DB2034.2
            023100 01  CCVS-E-1.                                                    DB2034.2
            023200     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB2034.2
            023300     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB2034.2
            023400     02 ID-AGAIN PICTURE IS X(9).                                 DB2034.2
            023500     02 FILLER PICTURE X(45) VALUE IS                             DB2034.2
            023600     " NTIS DISTRIBUTION COBOL 74".                               DB2034.2
            023700 01  CCVS-E-2.                                                    DB2034.2
            023800     02  FILLER                   PICTURE X(31)  VALUE            DB2034.2
            023900     SPACE.                                                       DB2034.2
            024000     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB2034.2
            024100     02 CCVS-E-2-2.                                               DB2034.2
            024200         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB2034.2
            024300         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB2034.2
            024400         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB2034.2
            024500 01  CCVS-E-3.                                                    DB2034.2
            024600     02  FILLER PICTURE X(22) VALUE                               DB2034.2
            024700     " FOR OFFICIAL USE ONLY".                                    DB2034.2
            024800     02  FILLER PICTURE X(12) VALUE SPACE.                        DB2034.2
            024900     02  FILLER PICTURE X(58) VALUE                               DB2034.2
            025000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB2034.2
            025100     02  FILLER PICTURE X(13) VALUE SPACE.                        DB2034.2
            025200     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB2034.2
            025300 01  CCVS-E-4.                                                    DB2034.2
            025400     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB2034.2
            025500     02 FILLER PIC XXXX VALUE " OF ".                             DB2034.2
            025600     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB2034.2
            025700     02 FILLER PIC X(40) VALUE                                    DB2034.2
            025800      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB2034.2
            025900 01  XXINFO.                                                      DB2034.2
            026000     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB2034.2
            026100     02 INFO-TEXT.                                                DB2034.2
            026200     04 FILLER PIC X(20) VALUE SPACE.                             DB2034.2
            026300     04 XXCOMPUTED PIC X(20).                                     DB2034.2
            026400     04 FILLER PIC X(5) VALUE SPACE.                              DB2034.2
            026500     04 XXCORRECT PIC X(20).                                      DB2034.2
            026600 01  HYPHEN-LINE.                                                 DB2034.2
            026700     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2034.2
            026800     02 FILLER PICTURE IS X(65) VALUE IS "************************DB2034.2
            026900-    "*****************************************".                 DB2034.2
            027000     02 FILLER PICTURE IS X(54) VALUE IS "************************DB2034.2
            027100-    "******************************".                            DB2034.2
            027200 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB2034.2
            027300     "DB203A".                                                    DB2034.2
            027400 PROCEDURE DIVISION.                                              DB2034.2
            027500 DECLARATIVES.                                                    DB2034.2
            027600 FILENAME-PROC SECTION.                                           DB2034.2
            027700     USE FOR DEBUGGING ON IND-FILE  IND-REC-1.                    DB2034.2
            027800 FILENAME-1.                                                      DB2034.2
            027900     MOVE 1 TO KEY-1.                                             DB2034.2
            028000     MOVE DEBUG-LINE TO LINE-1.                                   DB2034.2
            028100     MOVE DEBUG-NAME TO  NAME-1  UNQUAL-NAME-1.                   DB2034.2
            028200     MOVE DEBUG-CONTENTS TO CONTENTS-1.                           DB2034.2
            028300     INSPECT UNQUAL-NAME-1  REPLACING CHARACTERS                  DB2034.2
            028400         BY SPACES AFTER INITIAL SPACE.                           DB2034.2
            028500 END DECLARATIVES.                                                DB2034.2
            028600 CCVS1 SECTION.                                                   DB2034.2
            028700 OPEN-FILES.                                                      DB2034.2
            028800     OPEN     OUTPUT PRINT-FILE.                                  DB2034.2
            028900     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB2034.2
            029000     MOVE    SPACE TO TEST-RESULTS.                               DB2034.2
            029100     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB2034.2
            029200     GO TO CCVS1-EXIT.                                            DB2034.2
            029300 CLOSE-FILES.                                                     DB2034.2
            029400     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB2034.2
            029500 TERMINATE-CCVS.                                                  DB2034.2
            029600S    EXIT PROGRAM.                                                DB2034.2
            029700STERMINATE-CALL.                                                  DB2034.2
            029800     STOP     RUN.                                                DB2034.2
            029900 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB2034.2
            030000 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB2034.2
            030100 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB2034.2
            030200 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB2034.2
            030300     MOVE "****TEST DELETED****" TO RE-MARK.                      DB2034.2
            030400 PRINT-DETAIL.                                                    DB2034.2
            030500     IF REC-CT NOT EQUAL TO ZERO                                  DB2034.2
            030600             MOVE "." TO PARDOT-X                                 DB2034.2
            030700             MOVE REC-CT TO DOTVALUE.                             DB2034.2
            030800     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB2034.2
            030900     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB2034.2
            031000        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB2034.2
            031100          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB2034.2
            031200     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB2034.2
            031300     MOVE SPACE TO CORRECT-X.                                     DB2034.2
            031400     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB2034.2
            031500     MOVE     SPACE TO RE-MARK.                                   DB2034.2
            031600 HEAD-ROUTINE.                                                    DB2034.2
            031700     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2034.2
            031800     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB2034.2
            031900     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB2034.2
            032000 COLUMN-NAMES-ROUTINE.                                            DB2034.2
            032100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2034.2
            032200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2034.2
            032300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB2034.2
            032400 END-ROUTINE.                                                     DB2034.2
            032500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB2034.2
            032600 END-RTN-EXIT.                                                    DB2034.2
            032700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2034.2
            032800 END-ROUTINE-1.                                                   DB2034.2
            032900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB2034.2
            033000      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB2034.2
            033100      ADD PASS-COUNTER TO ERROR-HOLD.                             DB2034.2
            033200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB2034.2
            033300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB2034.2
            033400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB2034.2
            033500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB2034.2
            033600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB2034.2
            033700  END-ROUTINE-12.                                                 DB2034.2
            033800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB2034.2
            033900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB2034.2
            034000         MOVE "NO " TO ERROR-TOTAL                                DB2034.2
            034100         ELSE                                                     DB2034.2
            034200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB2034.2
            034300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB2034.2
            034400     PERFORM WRITE-LINE.                                          DB2034.2
            034500 END-ROUTINE-13.                                                  DB2034.2
            034600     IF DELETE-CNT IS EQUAL TO ZERO                               DB2034.2
            034700         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB2034.2
            034800         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB2034.2
            034900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB2034.2
            035000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2034.2
            035100      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB2034.2
            035200          MOVE "NO " TO ERROR-TOTAL                               DB2034.2
            035300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB2034.2
            035400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB2034.2
            035500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB2034.2
            035600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2034.2
            035700 WRITE-LINE.                                                      DB2034.2
            035800     ADD 1 TO RECORD-COUNT.                                       DB2034.2
            035900Y    IF RECORD-COUNT GREATER 50                                   DB2034.2
            036000Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB2034.2
            036100Y        MOVE SPACE TO DUMMY-RECORD                               DB2034.2
            036200Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB2034.2
            036300Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB2034.2
            036400Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB2034.2
            036500Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB2034.2
            036600Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB2034.2
            036700Y        MOVE ZERO TO RECORD-COUNT.                               DB2034.2
            036800     PERFORM WRT-LN.                                              DB2034.2
            036900 WRT-LN.                                                          DB2034.2
            037000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB2034.2
            037100     MOVE SPACE TO DUMMY-RECORD.                                  DB2034.2
            037200 BLANK-LINE-PRINT.                                                DB2034.2
            037300     PERFORM WRT-LN.                                              DB2034.2
            037400 FAIL-ROUTINE.                                                    DB2034.2
            037500     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB2034.2
            037600     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB2034.2
            037700     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB2034.2
            037800     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2034.2
            037900     GO TO FAIL-ROUTINE-EX.                                       DB2034.2
            038000 FAIL-ROUTINE-WRITE.                                              DB2034.2
            038100     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB2034.2
            038200     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB2034.2
            038300 FAIL-ROUTINE-EX. EXIT.                                           DB2034.2
            038400 BAIL-OUT.                                                        DB2034.2
            038500     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB2034.2
            038600     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB2034.2
            038700 BAIL-OUT-WRITE.                                                  DB2034.2
            038800     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB2034.2
            038900     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2034.2
            039000 BAIL-OUT-EX. EXIT.                                               DB2034.2
            039100 CCVS1-EXIT.                                                      DB2034.2
            039200     EXIT.                                                        DB2034.2
            039300 BEGIN-DB203A-TESTS SECTION.                                      DB2034.2
            039400 SET-UP-REC-AREA.                                                 DB2034.2
            039500     MOVE FILE-RECORD-INFO-SKELETON TO FILE-RECORD-INFO (1).      DB2034.2
            039600     MOVE "IX-FD1" TO XFILE-NAME (1).                             DB2034.2
            039700     MOVE "REC-1" TO XRECORD-NAME (1).                            DB2034.2
            039800     MOVE ".XXX." TO XPROGRAM-NAME (1).                           DB2034.2
            039900     MOVE 240 TO XRECORD-LENGTH (1).                              DB2034.2
            040000     MOVE "RC" TO CHARS-OR-RECORDS (1).                           DB2034.2
            040100     MOVE 1 TO XBLOCK-SIZE (1).                                   DB2034.2
            040200     MOVE 5 TO RECORDS-IN-FILE (1).                               DB2034.2
            040300     MOVE "IX" TO XFILE-ORGANIZATION (1).                         DB2034.2
            040400     MOVE "S" TO XLABEL-TYPE (1).                                 DB2034.2
            040500 OPEN-TEST-1-INIT.                                                DB2034.2
            040600     MOVE 0 TO KEY-1.                                             DB2034.2
            040700     MOVE "OPEN-TEST-1" TO PAR-NAME.                              DB2034.2
            040800     MOVE "DEBUG OPEN OUTPUT" TO FEATURE.                         DB2034.2
            040900 OPEN-TEST-1.                                                     DB2034.2
            041000     OPEN OUTPUT IND-FILE.                                        DB2034.2
            041100     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            041200         PERFORM PASS                                             DB2034.2
            041300         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            041400     ELSE PERFORM FAIL                                            DB2034.2
            041500         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK.               DB2034.2
            041600     PERFORM GEN-WRITE.                                           DB2034.2
            041700     GO TO WRITE-TEST-1-INIT.                                     DB2034.2
            041800 OPEN-TEST-1-DELETE.                                              DB2034.2
            041900     PERFORM DE-LETE.                                             DB2034.2
            042000     PERFORM GEN-WRITE.                                           DB2034.2
            042100 WRITE-TEST-1-INIT.                                               DB2034.2
            042200     MOVE 0 TO KEY-1.                                             DB2034.2
            042300     MOVE "WRITE-TEST-1" TO PAR-NAME.                             DB2034.2
            042400     MOVE "DEBUG WRITE RECORD" TO FEATURE.                        DB2034.2
            042500 WRITE-TEST-1.                                                    DB2034.2
            042600     MOVE 1 TO XRECORD-NUMBER (1).                                DB2034.2
            042700     MOVE FILE-RECORD-INFO (1) TO IND-REC-2.                      DB2034.2
            042800     MOVE "02" TO IND-KEY.                                        DB2034.2
            042900     WRITE IND-REC-2 INVALID KEY GO TO ABORT-PGM.                 DB2034.2
            043000     MOVE 2 TO XRECORD-NUMBER (1).                                DB2034.2
            043100     MOVE FILE-RECORD-INFO (1) TO IND-REC-2.                      DB2034.2
            043200     MOVE "04" TO IND-KEY.                                        DB2034.2
            043300     WRITE IND-REC-2 INVALID KEY GO TO ABORT-PGM.                 DB2034.2
            043400     MOVE 3 TO XRECORD-NUMBER (1).                                DB2034.2
            043500     MOVE FILE-RECORD-INFO (1) TO IND-REC-2.                      DB2034.2
            043600     MOVE "06" TO IND-KEY.                                        DB2034.2
            043700     WRITE IND-REC-2 INVALID KEY GO TO ABORT-PGM.                 DB2034.2
            043800     MOVE 4 TO XRECORD-NUMBER (1).                                DB2034.2
            043900     MOVE FILE-RECORD-INFO (1) TO IND-REC-2.                      DB2034.2
            044000     MOVE "08" TO IND-KEY.                                        DB2034.2
            044100     WRITE IND-REC-2 INVALID KEY GO TO ABORT-PGM.                 DB2034.2
            044200     MOVE 5 TO XRECORD-NUMBER (1).                                DB2034.2
            044300     MOVE FILE-RECORD-INFO (1) TO IND-REC-2.                      DB2034.2
            044400     MOVE "10" TO IND-KEY.                                        DB2034.2
            044500     WRITE IND-REC-1 INVALID KEY GO TO ABORT-PGM.                 DB2034.2
            044600     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            044700         PERFORM PASS                                             DB2034.2
            044800         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            044900     ELSE PERFORM FAIL                                            DB2034.2
            045000         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK.               DB2034.2
            045100     PERFORM GEN-WRITE.                                           DB2034.2
            045200     GO TO CLOSE-TEST-1-INIT.                                     DB2034.2
            045300 WRITE-TEST-1-DELETE.                                             DB2034.2
            045400     PERFORM DE-LETE.                                             DB2034.2
            045500     PERFORM GEN-WRITE.                                           DB2034.2
            045600 CLOSE-TEST-1-INIT.                                               DB2034.2
            045700     MOVE 0 TO KEY-1.                                             DB2034.2
            045800     MOVE "CLOSE-TEST-1" TO PAR-NAME.                             DB2034.2
            045900     MOVE "DEBUG CLOSE FILE" TO FEATURE.                          DB2034.2
            046000 CLOSE-TEST-1.                                                    DB2034.2
            046100     CLOSE IND-FILE.                                              DB2034.2
            046200     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            046300         PERFORM PASS                                             DB2034.2
            046400         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            046500     ELSE PERFORM FAIL                                            DB2034.2
            046600         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK.               DB2034.2
            046700     PERFORM GEN-WRITE.                                           DB2034.2
            046800     GO TO OPEN-TEST-2-INIT.                                      DB2034.2
            046900 CLOSE-TEST-1-DELETE.                                             DB2034.2
            047000     PERFORM DE-LETE.                                             DB2034.2
            047100     PERFORM GEN-WRITE.                                           DB2034.2
            047200 OPEN-TEST-2-INIT.                                                DB2034.2
            047300     MOVE 0 TO KEY-1.                                             DB2034.2
            047400     MOVE "OPEN-TEST-2" TO PAR-NAME.                              DB2034.2
            047500     MOVE "DEBUG OPEN I-O" TO FEATURE.                            DB2034.2
            047600 OPEN-TEST-2.                                                     DB2034.2
            047700     OPEN I-O IND-FILE.                                           DB2034.2
            047800     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            047900         PERFORM PASS                                             DB2034.2
            048000         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            048100     ELSE  PERFORM FAIL                                           DB2034.2
            048200         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK.               DB2034.2
            048300     PERFORM GEN-WRITE.                                           DB2034.2
            048400     GO TO READ-TEST-1-INIT.                                      DB2034.2
            048500 OPEN-TEST-2-DELETE.                                              DB2034.2
            048600     PERFORM DE-LETE.                                             DB2034.2
            048700     PERFORM GEN-WRITE.                                           DB2034.2
            048800 READ-TEST-1-INIT.                                                DB2034.2
            048900     MOVE SPACES TO ITEM-1.                                       DB2034.2
            049000     MOVE 0 TO KEY-1.                                             DB2034.2
            049100     MOVE "READ-TEST-1" TO PAR-NAME.                              DB2034.2
            049200     MOVE "DEBUG READ RANDOM" TO FEATURE.                         DB2034.2
            049300 READ-TEST-1.                                                     DB2034.2
            049400     MOVE "04" TO IND-KEY.                                        DB2034.2
            049500******************************************************************DB2034.2
            049600*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2034.2
            049700*    "READ-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT     *DB2034.2
            049800*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS, "READ       *DB2034.2
            049900*    IND-FILE KEY IS IND-KEY INVALID KEY GO TO ABORT-PGM.".      *DB2034.2
            050000******************************************************************DB2034.2
            050100     READ IND-FILE  KEY IS IND-KEY                                DB2034.2
            050200         INVALID KEY GO TO ABORT-PGM.                             DB2034.2
            050300     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            050400         PERFORM PASS                                             DB2034.2
            050500         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            050600         PERFORM GEN-WRITE                                        DB2034.2
            050700         GO TO READ-TEST-1A                                       DB2034.2
            050800     ELSE  PERFORM FAIL                                           DB2034.2
            050900         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2034.2
            051000         PERFORM GEN-WRITE                                        DB2034.2
            051100         PERFORM DELETE-READ-TEST-1-SUBTESTS                      DB2034.2
            051200         GO TO REWRITE-TEST-1-INIT.                               DB2034.2
            051300 READ-TEST-1-DELETE.                                              DB2034.2
            051400     PERFORM DE-LETE.                                             DB2034.2
            051500     PERFORM GEN-WRITE.                                           DB2034.2
            051600     PERFORM DELETE-READ-TEST-1-SUBTESTS.                         DB2034.2
            051700     GO TO READ-TEST-2-INIT.                                      DB2034.2
            051800 DELETE-READ-TEST-1-SUBTESTS.                                     DB2034.2
            051900     MOVE "READ-TEST-1A" TO PAR-NAME.                             DB2034.2
            052000     PERFORM DE-LETE.                                             DB2034.2
            052100     PERFORM GEN-WRITE.                                           DB2034.2
            052200     MOVE "READ-TEST-1B" TO PAR-NAME.                             DB2034.2
            052300     PERFORM DE-LETE.                                             DB2034.2
            052400     PERFORM GEN-WRITE.                                           DB2034.2
            052500     MOVE "READ-TEST-1C" TO PAR-NAME.                             DB2034.2
            052600     PERFORM DE-LETE.                                             DB2034.2
            052700     PERFORM GEN-WRITE.                                           DB2034.2
            052800 READ-TEST-1A.                                                    DB2034.2
            052900     MOVE "READ-TEST-1A" TO PAR-NAME.                             DB2034.2
            053000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2034.2
            053100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2034.2
            053200     MOVE LINE-1 TO COMPUTED-A.                                   DB2034.2
            053300     PERFORM INSPT.                                               DB2034.2
            053400     PERFORM GEN-WRITE.                                           DB2034.2
            053500 READ-TEST-1B.                                                    DB2034.2
            053600     MOVE "READ-TEST-1B" TO PAR-NAME.                             DB2034.2
            053700     IF UNQUAL-NAME-1 IS EQUAL TO "IND-FILE"                      DB2034.2
            053800         PERFORM PASS                                             DB2034.2
            053900     ELSE  PERFORM FAIL                                           DB2034.2
            054000         MOVE "IND-FILE" TO CORRECT-A                             DB2034.2
            054100         MOVE NAME-1 TO COMPUTED-A.                               DB2034.2
            054200     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2034.2
            054300     PERFORM GEN-WRITE.                                           DB2034.2
            054400 READ-TEST-1C.                                                    DB2034.2
            054500     MOVE "READ-TEST-1C" TO PAR-NAME.                             DB2034.2
            054600     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2034.2
            054700     IF CONTENTS-1 IS EQUAL TO IND-REC-2                          DB2034.2
            054800         PERFORM PASS                                             DB2034.2
            054900         PERFORM GEN-WRITE                                        DB2034.2
            055000         GO TO READ-TEST-2-INIT                                   DB2034.2
            055100     ELSE  PERFORM FAIL                                           DB2034.2
            055200         MOVE "LINES 1 AND 3 BELOW" TO COMPUTED-A                 DB2034.2
            055300         MOVE "LINES 2 AND 4 BELOW" TO CORRECT-A.                 DB2034.2
            055400     PERFORM GEN-WRITE.                                           DB2034.2
            055500     MOVE CONTENTS-1-1H TO PRINT-REC.                             DB2034.2
            055600     PERFORM WRITE-LINE.                                          DB2034.2
            055700     MOVE IND-REC-1H TO PRINT-REC.                                DB2034.2
            055800     PERFORM WRITE-LINE.                                          DB2034.2
            055900     MOVE CONTENTS-1-2H TO PRINT-REC.                             DB2034.2
            056000     PERFORM WRITE-LINE.                                          DB2034.2
            056100     MOVE IND-REC-2H TO PRINT-REC.                                DB2034.2
            056200     PERFORM WRITE-LINE.                                          DB2034.2
            056300 READ-TEST-2-INIT.                                                DB2034.2
            056400     MOVE SPACES TO ITEM-1.                                       DB2034.2
            056500     MOVE 0 TO KEY-1.                                             DB2034.2
            056600     MOVE "READ-TEST-2" TO PAR-NAME.                              DB2034.2
            056700     MOVE "DEBUG READ INV KEY" TO FEATURE.                        DB2034.2
            056800 READ-TEST-2.                                                     DB2034.2
            056900     MOVE "05" TO IND-KEY.                                        DB2034.2
            057000     READ IND-FILE  KEY IS IND-KEY                                DB2034.2
            057100         INVALID KEY  GO TO READ-TEST-2-CONT.                     DB2034.2
            057200 READ-TEST-2-DELETE.                                              DB2034.2
            057300     PERFORM DE-LETE.                                             DB2034.2
            057400     PERFORM GEN-WRITE.                                           DB2034.2
            057500     GO TO REWRITE-TEST-1-INIT.                                   DB2034.2
            057600 READ-TEST-2-CONT.                                                DB2034.2
            057700     IF KEY-1 IS EQUAL TO 0                                       DB2034.2
            057800         PERFORM PASS                                             DB2034.2
            057900         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2034.2
            058000     ELSE  PERFORM FAIL                                           DB2034.2
            058100         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK.               DB2034.2
            058200     PERFORM GEN-WRITE.                                           DB2034.2
            058300 REWRITE-TEST-1-INIT.                                             DB2034.2
            058400     MOVE SPACES TO ITEM-1.                                       DB2034.2
            058500     MOVE 0 TO KEY-1.                                             DB2034.2
            058600     MOVE "REWRITE-TEST-1" TO PAR-NAME.                           DB2034.2
            058700     MOVE "DEBUG REWRITE RECORD" TO FEATURE.                      DB2034.2
            058800 REWRITE-TEST-1.                                                  DB2034.2
            058900     MOVE 2 TO XRECORD-NUMBER (1).                                DB2034.2
            059000     MOVE 1 TO UPDATE-NUMBER (1).                                 DB2034.2
            059100     MOVE FILE-RECORD-INFO (1) TO IND-REC-2.                      DB2034.2
            059200     MOVE "04" TO IND-KEY.                                        DB2034.2
            059300     MOVE IND-REC-2 TO CONTENTS-REC.                              DB2034.2
            059400******************************************************************DB2034.2
            059500*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2034.2
            059600*    "REWRITE-TEST-1A" SHOULD POINT TO THE EXECUTABLE STATEMENT  *DB2034.2
            059700*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS,             *DB2034.2
            059800*    "REWRITE IND-REC-1  INVALID KEY  GO TO ABORT-PGM.".         *DB2034.2
            059900******************************************************************DB2034.2
            060000     REWRITE IND-REC-1  INVALID KEY  GO TO ABORT-PGM.             DB2034.2
            060100     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            060200         PERFORM PASS                                             DB2034.2
            060300         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            060400         PERFORM GEN-WRITE                                        DB2034.2
            060500         GO TO REWRITE-TEST-1A                                    DB2034.2
            060600     ELSE  PERFORM FAIL                                           DB2034.2
            060700         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2034.2
            060800         PERFORM GEN-WRITE                                        DB2034.2
            060900         PERFORM DELETE-REWRITE-TEST-1-SUBTESTS                   DB2034.2
            061000         GO TO REWRITE-TEST-2-INIT.                               DB2034.2
            061100 REWRITE-TEST-1-DELETE.                                           DB2034.2
            061200     PERFORM DE-LETE.                                             DB2034.2
            061300     PERFORM GEN-WRITE.                                           DB2034.2
            061400     PERFORM DELETE-REWRITE-TEST-1-SUBTESTS.                      DB2034.2
            061500     GO TO REWRITE-TEST-2-INIT.                                   DB2034.2
            061600 DELETE-REWRITE-TEST-1-SUBTESTS.                                  DB2034.2
            061700     MOVE "REWRITE-TEST-1A" TO PAR-NAME.                          DB2034.2
            061800     PERFORM DE-LETE.                                             DB2034.2
            061900     PERFORM GEN-WRITE.                                           DB2034.2
            062000     MOVE "REWRITE-TEST-1B" TO PAR-NAME.                          DB2034.2
            062100     PERFORM DE-LETE.                                             DB2034.2
            062200     PERFORM GEN-WRITE.                                           DB2034.2
            062300     MOVE "REWRITE-TEST-1C" TO PAR-NAME.                          DB2034.2
            062400     PERFORM DE-LETE.                                             DB2034.2
            062500     PERFORM GEN-WRITE.                                           DB2034.2
            062600 REWRITE-TEST-1A.                                                 DB2034.2
            062700     MOVE "REWRITE-TEST-1A" TO PAR-NAME.                          DB2034.2
            062800     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2034.2
            062900     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2034.2
            063000     MOVE LINE-1 TO COMPUTED-A.                                   DB2034.2
            063100     PERFORM INSPT.                                               DB2034.2
            063200     PERFORM GEN-WRITE.                                           DB2034.2
            063300 REWRITE-TEST-1B.                                                 DB2034.2
            063400     MOVE "REWRITE-TEST-1B" TO PAR-NAME.                          DB2034.2
            063500     IF UNQUAL-NAME-1 IS EQUAL TO "IND-REC-1"                     DB2034.2
            063600         PERFORM PASS                                             DB2034.2
            063700     ELSE  PERFORM FAIL                                           DB2034.2
            063800         MOVE "IND-REC-1" TO CORRECT-A                            DB2034.2
            063900         MOVE NAME-1 TO COMPUTED-A.                               DB2034.2
            064000     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2034.2
            064100     PERFORM GEN-WRITE.                                           DB2034.2
            064200 REWRITE-TEST-1C.                                                 DB2034.2
            064300     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2034.2
            064400     MOVE "REWRITE-TEST-1C" TO PAR-NAME.                          DB2034.2
            064500     IF CONTENTS-1 IS EQUAL TO CONTENTS-REC                       DB2034.2
            064600         PERFORM PASS                                             DB2034.2
            064700         PERFORM GEN-WRITE                                        DB2034.2
            064800         GO TO REWRITE-TEST-2-INIT                                DB2034.2
            064900     ELSE  PERFORM FAIL                                           DB2034.2
            065000         MOVE "LINES 1 AND 3 BELOW" TO COMPUTED-A                 DB2034.2
            065100         MOVE "LINES 2 AND 4 BELOW" TO CORRECT-A                  DB2034.2
            065200         PERFORM GEN-WRITE.                                       DB2034.2
            065300     MOVE CONTENTS-1-1H TO PRINT-REC.                             DB2034.2
            065400     PERFORM WRITE-LINE.                                          DB2034.2
            065500     MOVE CONTENTS-REC-1H TO PRINT-REC.                           DB2034.2
            065600     PERFORM WRITE-LINE.                                          DB2034.2
            065700     MOVE CONTENTS-1-2H TO PRINT-REC.                             DB2034.2
            065800     PERFORM WRITE-LINE.                                          DB2034.2
            065900     MOVE CONTENTS-REC-2H TO PRINT-REC.                           DB2034.2
            066000     PERFORM WRITE-LINE.                                          DB2034.2
            066100 REWRITE-TEST-2-INIT.                                             DB2034.2
            066200     MOVE SPACES TO ITEM-1.                                       DB2034.2
            066300     MOVE 0 TO KEY-1.                                             DB2034.2
            066400     MOVE "REWRITE-TEST-2" TO PAR-NAME.                           DB2034.2
            066500     MOVE "DEBUG REWRITE INVLID" TO FEATURE.                      DB2034.2
            066600 REWRITE-TEST-2.                                                  DB2034.2
            066700     MOVE 6 TO XRECORD-NUMBER (1).                                DB2034.2
            066800     MOVE 1 TO UPDATE-NUMBER (1).                                 DB2034.2
            066900     MOVE FILE-RECORD-INFO (1) TO IND-REC-2.                      DB2034.2
            067000     MOVE "03" TO IND-KEY.                                        DB2034.2
            067100     REWRITE IND-REC-1  INVALID KEY  GO TO REWRITE-TEST-2-CONT.   DB2034.2
            067200 REWRITE-TEST-2-DELETE.                                           DB2034.2
            067300     PERFORM DE-LETE.                                             DB2034.2
            067400     PERFORM GEN-WRITE.                                           DB2034.2
            067500     GO TO START-TEST-1-INIT.                                     DB2034.2
            067600 REWRITE-TEST-2-CONT.                                             DB2034.2
            067700     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            067800         PERFORM PASS                                             DB2034.2
            067900         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            068000     ELSE  PERFORM FAIL                                           DB2034.2
            068100         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK.               DB2034.2
            068200     PERFORM GEN-WRITE.                                           DB2034.2
            068300 START-TEST-1-INIT.                                               DB2034.2
            068400     MOVE SPACES TO ITEM-1.                                       DB2034.2
            068500     MOVE 0 TO KEY-1.                                             DB2034.2
            068600     MOVE "START-TEST-1" TO PAR-NAME.                             DB2034.2
            068700     MOVE "DEBUG START FILENAME" TO FEATURE.                      DB2034.2
            068800 START-TEST-1.                                                    DB2034.2
            068900     MOVE "05" TO IND-KEY.                                        DB2034.2
            069000     START IND-FILE  KEY IS GREATER THAN IND-KEY                  DB2034.2
            069100         INVALID KEY  GO TO ABORT-PGM.                            DB2034.2
            069200     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            069300         PERFORM PASS                                             DB2034.2
            069400         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            069500         PERFORM GEN-WRITE                                        DB2034.2
            069600         GO TO START-TEST-1A                                      DB2034.2
            069700     ELSE  PERFORM FAIL                                           DB2034.2
            069800         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2034.2
            069900         PERFORM GEN-WRITE                                        DB2034.2
            070000         PERFORM DELETE-START-TEST-1-SUBTEST                      DB2034.2
            070100         GO TO START-TEST-2-INIT.                                 DB2034.2
            070200 START-TEST-1-DELETE.                                             DB2034.2
            070300     PERFORM DE-LETE.                                             DB2034.2
            070400     PERFORM GEN-WRITE.                                           DB2034.2
            070500     PERFORM DELETE-START-TEST-1-SUBTEST.                         DB2034.2
            070600     GO TO START-TEST-2-INIT.                                     DB2034.2
            070700 DELETE-START-TEST-1-SUBTEST.                                     DB2034.2
            070800     MOVE "START-TEST-1A" TO PAR-NAME.                            DB2034.2
            070900     PERFORM DE-LETE.                                             DB2034.2
            071000     PERFORM GEN-WRITE.                                           DB2034.2
            071100 START-TEST-1A.                                                   DB2034.2
            071200     MOVE "START-TEST-1A" TO PAR-NAME.                            DB2034.2
            071300     IF CONTENTS-1 IS EQUAL TO SPACES                             DB2034.2
            071400         PERFORM PASS                                             DB2034.2
            071500     ELSE  PERFORM FAIL                                           DB2034.2
            071600         MOVE CONTENTS-1 TO COMPUTED-A                            DB2034.2
            071700         MOVE "(SPACES)" TO CORRECT-A.                            DB2034.2
            071800     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2034.2
            071900     PERFORM GEN-WRITE.                                           DB2034.2
            072000 START-TEST-2-INIT.                                               DB2034.2
            072100     MOVE 0 TO KEY-1.                                             DB2034.2
            072200     MOVE "START-TEST-2" TO PAR-NAME.                             DB2034.2
            072300     MOVE "DEBUG START INV KEY" TO FEATURE.                       DB2034.2
            072400 START-TEST-2.                                                    DB2034.2
            072500     MOVE "12" TO IND-KEY.                                        DB2034.2
            072600     START IND-FILE  KEY IS GREATER THAN IND-KEY                  DB2034.2
            072700         INVALID KEY  GO TO START-TEST-2-CONT.                    DB2034.2
            072800 START-TEST-2-DELETE.                                             DB2034.2
            072900     PERFORM DE-LETE.                                             DB2034.2
            073000     PERFORM GEN-WRITE.                                           DB2034.2
            073100     GO TO DELETE-TEST-1-INIT.                                    DB2034.2
            073200 START-TEST-2-CONT.                                               DB2034.2
            073300     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            073400         PERFORM PASS                                             DB2034.2
            073500         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            073600     ELSE  PERFORM FAIL                                           DB2034.2
            073700         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK.               DB2034.2
            073800     PERFORM GEN-WRITE.                                           DB2034.2
            073900 DELETE-TEST-1-INIT.                                              DB2034.2
            074000     MOVE SPACES TO ITEM-1.                                       DB2034.2
            074100     MOVE 0 TO KEY-1.                                             DB2034.2
            074200     MOVE "DELETE-TEST-1" TO PAR-NAME.                            DB2034.2
            074300     MOVE "DEBUG DELETE FILE" TO FEATURE.                         DB2034.2
            074400 DELETE-TEST-1.                                                   DB2034.2
            074500     MOVE "06" TO IND-KEY.                                        DB2034.2
            074600     DELETE IND-FILE INVALID KEY GO TO ABORT-PGM.                 DB2034.2
            074700     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            074800         PERFORM PASS                                             DB2034.2
            074900         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            075000         PERFORM GEN-WRITE                                        DB2034.2
            075100         GO TO DELETE-TEST-1A                                     DB2034.2
            075200     ELSE  PERFORM FAIL                                           DB2034.2
            075300         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2034.2
            075400         PERFORM GEN-WRITE                                        DB2034.2
            075500         PERFORM DELETE-DELETE-TEST-1-SUBTEST                     DB2034.2
            075600         GO TO DELETE-TEST-2-INIT.                                DB2034.2
            075700 DELETE-TEST-1-DELETE.                                            DB2034.2
            075800     PERFORM DE-LETE.                                             DB2034.2
            075900     PERFORM GEN-WRITE.                                           DB2034.2
            076000     PERFORM DELETE-DELETE-TEST-1-SUBTEST.                        DB2034.2
            076100     GO TO DELETE-TEST-2-INIT.                                    DB2034.2
            076200 DELETE-DELETE-TEST-1-SUBTEST.                                    DB2034.2
            076300     MOVE "DELETE-TEST-1A" TO PAR-NAME.                           DB2034.2
            076400     PERFORM DE-LETE.                                             DB2034.2
            076500     PERFORM GEN-WRITE.                                           DB2034.2
            076600 DELETE-TEST-1A.                                                  DB2034.2
            076700     MOVE "DELETE-TEST-1A" TO PAR-NAME.                           DB2034.2
            076800     IF CONTENTS-1 IS EQUAL TO SPACES                             DB2034.2
            076900         PERFORM PASS                                             DB2034.2
            077000     ELSE  PERFORM FAIL                                           DB2034.2
            077100         MOVE "(SPACES)" TO CORRECT-A                             DB2034.2
            077200         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2034.2
            077300     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2034.2
            077400     PERFORM GEN-WRITE.                                           DB2034.2
            077500 DELETE-TEST-2-INIT.                                              DB2034.2
            077600     MOVE "DELETE-TEST-2" TO PAR-NAME.                            DB2034.2
            077700     MOVE "DEBUG DELETE INV KEY" TO FEATURE.                      DB2034.2
            077800     MOVE 0 TO KEY-1.                                             DB2034.2
            077900 DELETE-TEST-2.                                                   DB2034.2
            078000     MOVE "07" TO IND-KEY.                                        DB2034.2
            078100     DELETE IND-FILE INVALID KEY GO TO DELETE-TEST-2-CONT.        DB2034.2
            078200 DELETE-TEST-2-DELETE.                                            DB2034.2
            078300     PERFORM DE-LETE.                                             DB2034.2
            078400     PERFORM GEN-WRITE.                                           DB2034.2
            078500     GO TO CLOSE-IND-FILE.                                        DB2034.2
            078600 DELETE-TEST-2-CONT.                                              DB2034.2
            078700     IF KEY-1 IS EQUAL TO 1                                       DB2034.2
            078800         PERFORM PASS                                             DB2034.2
            078900         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2034.2
            079000     ELSE  PERFORM FAIL                                           DB2034.2
            079100         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK.               DB2034.2
            079200     PERFORM GEN-WRITE.                                           DB2034.2
            079300     GO TO CLOSE-IND-FILE.                                        DB2034.2
            079400 GEN-WRITE.                                                       DB2034.2
            079500     PERFORM PRINT-DETAIL.                                        DB2034.2
            079600 ABORT-PGM.                                                       DB2034.2
            079700     DISPLAY "INDEXED I-O MODULE - FILE HANDLING ERROR - PROGRAM ADB2034.2
            079800-            "BORTED."  UPON THE-SYSTEM-PRINTER.                  DB2034.2
            079900 CLOSE-IND-FILE.                                                  DB2034.2
            080000     CLOSE IND-FILE.                                              DB2034.2
            080100XOPEN-IND-FILE.                                                   DB2034.2
            080200X    OPEN INPUT IND-FILE.                                         DB2034.2
            080300X    MOVE SPACES TO PRINT-REC.                                    DB2034.2
            080400X    PERFORM WRITE-LINE.                                          DB2034.2
            080500X    MOVE " DUMP OF IND-FILE FOLLOWS" TO PRINT-REC.               DB2034.2
            080600X    PERFORM WRITE-LINE.                                          DB2034.2
            080700XREAD-IND-FILE.                                                   DB2034.2
            080800X    READ IND-FILE NEXT RECORD  AT END GO TO CLOSE-FILE-DUMP.     DB2034.2
            080900X    MOVE IND-REC-1H TO PRINT-REC.                                DB2034.2
            081000X    PERFORM WRITE-LINE.                                          DB2034.2
            081100X    MOVE IND-REC-2H TO PRINT-REC.                                DB2034.2
            081200X    PERFORM WRITE-LINE.                                          DB2034.2
            081300X    GO TO READ-IND-FILE.                                         DB2034.2
            081400XCLOSE-FILE-DUMP.                                                 DB2034.2
            081500X    CLOSE IND-FILE.                                              DB2034.2
            081600 CCVS-EXIT SECTION.                                               DB2034.2
            081700 CCVS-999999.                                                     DB2034.2
            081800     GO TO CLOSE-FILES.                                           DB2034.2
                  *END-OF,DB203A                                                            
        """)
    )

    @Disabled("Requires X, Y, G, C indicators")
    @Test
    fun db2044_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB204A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB2044.2
            000200 PROGRAM-ID.                                                      DB2044.2
            000300     DB204A.                                                      DB2044.2
            000400 AUTHOR.                                                          DB2044.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB2044.2
            000600 INSTALLATION.                                                    DB2044.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB2044.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB2044.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB2044.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB2044.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB2044.2
            001200                                                                  DB2044.2
            001300     PHONE   (703) 756-6153                                       DB2044.2
            001400                                                                  DB2044.2
            001500     " HIGH       ".                                              DB2044.2
            001600 DATE-WRITTEN.                                                    DB2044.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB2044.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB2044.2
            001900     "4.2 ".                                                      DB2044.2
            002000 SECURITY.                                                        DB2044.2
            002100     NONE.                                                        DB2044.2
            002200*                                                                 DB2044.2
            002300*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *DB2044.2
            002400*                                                                 DB2044.2
            002500*                       PROGRAM ABSTRACT                          DB2044.2
            002600*                                                                 DB2044.2
            002700*    DB204A TESTS THE CAPABILITY OF THE DEBUG MODULE TO HANDLE A  DB2044.2
            002800*    DEBUGGING PROCEDURE WHICH IS MONITORING A "MERGE OUTPUT"     DB2044.2
            002900*    PROCEDURE.  THIS PROGRAM IS TO BE COMPILED AND EXECUTED      DB2044.2
            003000*    WITH BOTH COMPILE AND OBJECT TIME DEBUGGING SWITCHES ON.     DB2044.2
            003100*    THE DEBUGGING PROCEDURE SHOULD BE INCLUDED IN COMPILATION    DB2044.2
            003200*    AND GENERATE CODE.  DURING EXECUTION, TWO SEQUENTIAL FILES   DB2044.2
            003300*    ARE CREATED WITH EACH CONTAINING 80-CHARACTER RECORDS IN     DB2044.2
            003400*    SORTED ORDER.  THE TWO FILES ARE THEN MERGED.  EXECUTION     DB2044.2
            003500*    OF THE MERGE OPERATION SHOULD TRIGGER THE DEBUGGING PRO-     DB2044.2
            003600*    CEDURE LINKED TO THE MERGE OUTPUT PROCEDURE-NAME.            DB2044.2
            003700*                                                                 DB2044.2
            003800*                                                                 DB2044.2
            003900*                                                                 DB2044.2
            004000 ENVIRONMENT DIVISION.                                            DB2044.2
            004100 CONFIGURATION SECTION.                                           DB2044.2
            004200 SOURCE-COMPUTER.                                                 DB2044.2
            004300     XXXXX082                                                     DB2044.2
            004400         WITH DEBUGGING MODE.                                     DB2044.2
            004500 OBJECT-COMPUTER.                                                 DB2044.2
            004600     XXXXX083.                                                    DB2044.2
            004700 INPUT-OUTPUT SECTION.                                            DB2044.2
            004800 FILE-CONTROL.                                                    DB2044.2
            004900     SELECT PRINT-FILE ASSIGN TO                                  DB2044.2
            005000     XXXXX055.                                                    DB2044.2
            005100     SELECT SQ-FS1 ASSIGN TO                                      DB2044.2
            005200     XXXXX014.                                                    DB2044.2
            005300     SELECT SQ-FS2  ASSIGN TO                                     DB2044.2
            005400     XXXXX015.                                                    DB2044.2
            005500     SELECT SQ-FS3  ASSIGN TO                                     DB2044.2
            005600     XXXXX016.                                                    DB2044.2
            005700     SELECT ST-FS4  ASSIGN TO                                     DB2044.2
            005800     XXXXX027.                                                    DB2044.2
            005900 DATA DIVISION.                                                   DB2044.2
            006000 FILE SECTION.                                                    DB2044.2
            006100 FD  PRINT-FILE                                                   DB2044.2
            006200     LABEL RECORDS                                                DB2044.2
            006300     XXXXX084                                                     DB2044.2
            006400     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB2044.2
            006500 01  PRINT-REC PICTURE X(120).                                    DB2044.2
            006600 01  DUMMY-RECORD PICTURE X(120).                                 DB2044.2
            006700 FD  SQ-FS1                                                       DB2044.2
            006800C    VALUE OF                                                     DB2044.2
            006900C    XXXXX074                                                     DB2044.2
            007000*      XXXXX074  REPLACE WITH IMPLEMENTOR NAME (*OPT C ONLY)      DB2044.2
            007100C    IS                                                           DB2044.2
            007200C    XXXXX075                                                     DB2044.2
            007300*      XXXXX075  REPLACE WITH VALUE CLAUSE OBJECT (*OPT C ONLY)   DB2044.2
            007400G    XXXXX069                                                     DB2044.2
            007500*      XXXXX069  REPLACE WITH ADDITIONAL INFO (*OPT G ONLY)       DB2044.2
            007600     LABEL RECORDS ARE STANDARD.                                  DB2044.2
            007700 01  REC-1 PIC X(120).                                            DB2044.2
            007800 FD  SQ-FS2                                                       DB2044.2
            007900C    VALUE OF                                                     DB2044.2
            008000C    XXXXX074                                                     DB2044.2
            008100*      XXXXX074  REPLACE WITH IMPLEMENTOR NAME (*OPT C ONLY)      DB2044.2
            008200C    IS                                                           DB2044.2
            008300C    XXXXX076                                                     DB2044.2
            008400*      XXXXX076  REPLACE WITH VALUE CLAUSE OBJECT (*OPT C ONLY)   DB2044.2
            008500G    XXXXX069                                                     DB2044.2
            008600*      XXXXX069  REPLACE WITH ADDITIONAL INFO (*OPT G ONLY)       DB2044.2
            008700     LABEL RECORDS ARE STANDARD.                                  DB2044.2
            008800 01  REC-2 PIC X(120).                                            DB2044.2
            008900 FD  SQ-FS3                                                       DB2044.2
            009000C    VALUE OF                                                     DB2044.2
            009100C    XXXXX074                                                     DB2044.2
            009200*      XXXXX074  REPLACE WITH IMPLEMENTOR NAME (*OPT C ONLY)      DB2044.2
            009300C    IS                                                           DB2044.2
            009400C    XXXXX077                                                     DB2044.2
            009500*      XXXXX077  REPLACE WITH VALUE CLAUSE OBJECT (*OPT C ONLY)   DB2044.2
            009600G    XXXXX069                                                     DB2044.2
            009700*      XXXXX069  REPLACE WITH ADDITIONAL INFO (*OPT G ONLY)       DB2044.2
            009800     LABEL RECORDS ARE STANDARD.                                  DB2044.2
            009900 01  REC-3 PIC X(120).                                            DB2044.2
            010000 SD  ST-FS4.                                                      DB2044.2
            010100 01  REC-4.                                                       DB2044.2
            010200     02  FILLER PIC X(34).                                        DB2044.2
            010300     02  SORT-KEY PIC X(6).                                       DB2044.2
            010400     02  FILLER PIC X(80).                                        DB2044.2
            010500 WORKING-STORAGE SECTION.                                         DB2044.2
            010600 01  ITEM-1.                                                      DB2044.2
            010700     02  KEY-1 PIC 99.                                            DB2044.2
            010800     02  LINE-1 PIC X(6).                                         DB2044.2
            010900     02  NAME-1 PIC X(30).                                        DB2044.2
            011000     02  UNQUAL-NAME-1 PIC X(30).                                 DB2044.2
            011100     02  CONTENTS-1 PIC X(30).                                    DB2044.2
            011200 01  FILE-RECORD-INFORMATION-REC.                                 DB2044.2
            011300     03 FILE-RECORD-INFO-SKELETON.                                DB2044.2
            011400        05 FILLER                 PICTURE X(48)       VALUE       DB2044.2
            011500             "FILE=      ,RECORD=      /0,RECNO=000000,UPDT=00".  DB2044.2
            011600        05 FILLER                 PICTURE X(46)       VALUE       DB2044.2
            011700             ",ODO=0000,PGM=     ,LRECL=000000,BLKSIZ  =0000".    DB2044.2
            011800        05 FILLER                 PICTURE X(26)       VALUE       DB2044.2
            011900             ",LFIL=000000,ORG=  ,LBLR= ".                        DB2044.2
            012000        05 FILLER                 PICTURE X(37)       VALUE       DB2044.2
            012100             ",RECKEY=                             ".             DB2044.2
            012200        05 FILLER                 PICTURE X(38)       VALUE       DB2044.2
            012300             ",ALTKEY1=                             ".            DB2044.2
            012400        05 FILLER                 PICTURE X(38)       VALUE       DB2044.2
            012500             ",ALTKEY2=                             ".            DB2044.2
            012600        05 FILLER                 PICTURE X(7)        VALUE SPACE.DB2044.2
            012700     03 FILE-RECORD-INFO          OCCURS  10  TIMES.              DB2044.2
            012800        05 FILE-RECORD-INFO-P1-120.                               DB2044.2
            012900           07 FILLER              PIC X(5).                       DB2044.2
            013000           07 XFILE-NAME           PIC X(6).                      DB2044.2
            013100           07 FILLER              PIC X(8).                       DB2044.2
            013200           07 XRECORD-NAME         PIC X(6).                      DB2044.2
            013300           07 FILLER              PIC X(1).                       DB2044.2
            013400           07 REELUNIT-NUMBER     PIC 9(1).                       DB2044.2
            013500           07 FILLER              PIC X(7).                       DB2044.2
            013600           07 XRECORD-NUMBER       PIC 9(6).                      DB2044.2
            013700           07 FILLER              PIC X(6).                       DB2044.2
            013800           07 UPDATE-NUMBER       PIC 9(2).                       DB2044.2
            013900           07 FILLER              PIC X(5).                       DB2044.2
            014000           07 ODO-NUMBER          PIC 9(4).                       DB2044.2
            014100           07 FILLER              PIC X(5).                       DB2044.2
            014200           07 XPROGRAM-NAME        PIC X(5).                      DB2044.2
            014300           07 FILLER              PIC X(7).                       DB2044.2
            014400           07 XRECORD-LENGTH       PIC 9(6).                      DB2044.2
            014500           07 FILLER              PIC X(7).                       DB2044.2
            014600           07 CHARS-OR-RECORDS    PIC X(2).                       DB2044.2
            014700           07 FILLER              PIC X(1).                       DB2044.2
            014800           07 XBLOCK-SIZE          PIC 9(4).                      DB2044.2
            014900           07 FILLER              PIC X(6).                       DB2044.2
            015000           07 RECORDS-IN-FILE     PIC 9(6).                       DB2044.2
            015100           07 FILLER              PIC X(5).                       DB2044.2
            015200           07 XFILE-ORGANIZATION   PIC X(2).                      DB2044.2
            015300           07 FILLER              PIC X(6).                       DB2044.2
            015400           07 XLABEL-TYPE          PIC X(1).                      DB2044.2
            015500        05 FILE-RECORD-INFO-P121-240.                             DB2044.2
            015600           07 FILLER              PIC X(8).                       DB2044.2
            015700           07 XRECORD-KEY          PIC X(29).                     DB2044.2
            015800           07 FILLER              PIC X(9).                       DB2044.2
            015900           07 ALTERNATE-KEY1      PIC X(29).                      DB2044.2
            016000           07 FILLER              PIC X(9).                       DB2044.2
            016100           07 ALTERNATE-KEY2      PIC X(29).                      DB2044.2
            016200           07 FILLER              PIC X(7).                       DB2044.2
            016300 01  TEST-RESULTS.                                                DB2044.2
            016400     02 FILLER                    PICTURE X VALUE SPACE.          DB2044.2
            016500     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB2044.2
            016600     02 FILLER                    PICTURE X VALUE SPACE.          DB2044.2
            016700     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB2044.2
            016800     02 FILLER                    PICTURE X  VALUE SPACE.         DB2044.2
            016900     02  PAR-NAME.                                                DB2044.2
            017000       03 FILLER PICTURE X(12) VALUE SPACE.                       DB2044.2
            017100       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB2044.2
            017200       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB2044.2
            017300       03 FILLER PIC X(5) VALUE SPACE.                            DB2044.2
            017400     02 FILLER PIC X(10) VALUE SPACE.                             DB2044.2
            017500     02 RE-MARK PIC X(61).                                        DB2044.2
            017600 01  TEST-COMPUTED.                                               DB2044.2
            017700     02 FILLER PIC X(30) VALUE SPACE.                             DB2044.2
            017800     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB2044.2
            017900     02 COMPUTED-X.                                               DB2044.2
            018000     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB2044.2
            018100     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB2044.2
            018200     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB2044.2
            018300     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB2044.2
            018400     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB2044.2
            018500     03       CM-18V0 REDEFINES COMPUTED-A.                       DB2044.2
            018600         04 COMPUTED-18V0                   PICTURE -9(18).       DB2044.2
            018700         04 FILLER                          PICTURE X.            DB2044.2
            018800     03 FILLER PIC X(50) VALUE SPACE.                             DB2044.2
            018900 01  TEST-CORRECT.                                                DB2044.2
            019000     02 FILLER PIC X(30) VALUE SPACE.                             DB2044.2
            019100     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB2044.2
            019200     02 CORRECT-X.                                                DB2044.2
            019300     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB2044.2
            019400     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB2044.2
            019500     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB2044.2
            019600     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB2044.2
            019700     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB2044.2
            019800     03      CR-18V0 REDEFINES CORRECT-A.                         DB2044.2
            019900         04 CORRECT-18V0                    PICTURE -9(18).       DB2044.2
            020000         04 FILLER                          PICTURE X.            DB2044.2
            020100     03 FILLER PIC X(50) VALUE SPACE.                             DB2044.2
            020200 01  CCVS-C-1.                                                    DB2044.2
            020300     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB2044.2
            020400-    "SS  PARAGRAPH-NAME                                          DB2044.2
            020500-    "        REMARKS".                                           DB2044.2
            020600     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB2044.2
            020700 01  CCVS-C-2.                                                    DB2044.2
            020800     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2044.2
            020900     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB2044.2
            021000     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB2044.2
            021100     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB2044.2
            021200     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB2044.2
            021300 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB2044.2
            021400 01  REC-CT PICTURE 99 VALUE ZERO.                                DB2044.2
            021500 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB2044.2
            021600 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB2044.2
            021700 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB2044.2
            021800 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB2044.2
            021900 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB2044.2
            022000 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB2044.2
            022100 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB2044.2
            022200 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB2044.2
            022300 01  CCVS-H-1.                                                    DB2044.2
            022400     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB2044.2
            022500     02 FILLER PICTURE X(67) VALUE                                DB2044.2
            022600     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB2044.2
            022700-    " SYSTEM".                                                   DB2044.2
            022800     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB2044.2
            022900 01  CCVS-H-2.                                                    DB2044.2
            023000     02 FILLER PICTURE X(52) VALUE IS                             DB2044.2
            023100     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB2044.2
            023200     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB2044.2
            023300     02 TEST-ID PICTURE IS X(9).                                  DB2044.2
            023400     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB2044.2
            023500 01  CCVS-H-3.                                                    DB2044.2
            023600     02  FILLER PICTURE X(34) VALUE                               DB2044.2
            023700     " FOR OFFICIAL USE ONLY    ".                                DB2044.2
            023800     02  FILLER PICTURE X(58) VALUE                               DB2044.2
            023900     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB2044.2
            024000     02  FILLER PICTURE X(28) VALUE                               DB2044.2
            024100     "  COPYRIGHT   1974 ".                                       DB2044.2
            024200 01  CCVS-E-1.                                                    DB2044.2
            024300     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB2044.2
            024400     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB2044.2
            024500     02 ID-AGAIN PICTURE IS X(9).                                 DB2044.2
            024600     02 FILLER PICTURE X(45) VALUE IS                             DB2044.2
            024700     " NTIS DISTRIBUTION COBOL 74".                               DB2044.2
            024800 01  CCVS-E-2.                                                    DB2044.2
            024900     02  FILLER                   PICTURE X(31)  VALUE            DB2044.2
            025000     SPACE.                                                       DB2044.2
            025100     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB2044.2
            025200     02 CCVS-E-2-2.                                               DB2044.2
            025300         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB2044.2
            025400         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB2044.2
            025500         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB2044.2
            025600 01  CCVS-E-3.                                                    DB2044.2
            025700     02  FILLER PICTURE X(22) VALUE                               DB2044.2
            025800     " FOR OFFICIAL USE ONLY".                                    DB2044.2
            025900     02  FILLER PICTURE X(12) VALUE SPACE.                        DB2044.2
            026000     02  FILLER PICTURE X(58) VALUE                               DB2044.2
            026100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB2044.2
            026200     02  FILLER PICTURE X(13) VALUE SPACE.                        DB2044.2
            026300     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB2044.2
            026400 01  CCVS-E-4.                                                    DB2044.2
            026500     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB2044.2
            026600     02 FILLER PIC XXXX VALUE " OF ".                             DB2044.2
            026700     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB2044.2
            026800     02 FILLER PIC X(40) VALUE                                    DB2044.2
            026900      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB2044.2
            027000 01  XXINFO.                                                      DB2044.2
            027100     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB2044.2
            027200     02 INFO-TEXT.                                                DB2044.2
            027300     04 FILLER PIC X(20) VALUE SPACE.                             DB2044.2
            027400     04 XXCOMPUTED PIC X(20).                                     DB2044.2
            027500     04 FILLER PIC X(5) VALUE SPACE.                              DB2044.2
            027600     04 XXCORRECT PIC X(20).                                      DB2044.2
            027700 01  HYPHEN-LINE.                                                 DB2044.2
            027800     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2044.2
            027900     02 FILLER PICTURE IS X(65) VALUE IS "************************DB2044.2
            028000-    "*****************************************".                 DB2044.2
            028100     02 FILLER PICTURE IS X(54) VALUE IS "************************DB2044.2
            028200-    "******************************".                            DB2044.2
            028300 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB2044.2
            028400     "DB204A".                                                    DB2044.2
            028500 PROCEDURE DIVISION.                                              DB2044.2
            028600 DECLARATIVES.                                                    DB2044.2
            028700 MERGE-PROC SECTION.                                              DB2044.2
            028800     USE FOR DEBUGGING ON MERGE-OUTPUT-PROC.                      DB2044.2
            028900 MERGE-1.                                                         DB2044.2
            029000     ADD 1 TO KEY-1.                                              DB2044.2
            029100     MOVE DEBUG-LINE TO LINE-1.                                   DB2044.2
            029200     MOVE DEBUG-NAME TO NAME-1  UNQUAL-NAME-1.                    DB2044.2
            029300     MOVE DEBUG-CONTENTS TO CONTENTS-1.                           DB2044.2
            029400     INSPECT UNQUAL-NAME-1 REPLACING CHARACTERS BY SPACES         DB2044.2
            029500         AFTER INITIAL SPACE.                                     DB2044.2
            029600 END DECLARATIVES.                                                DB2044.2
            029700 CCVS1 SECTION.                                                   DB2044.2
            029800 OPEN-FILES.                                                      DB2044.2
            029900     OPEN     OUTPUT PRINT-FILE.                                  DB2044.2
            030000     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB2044.2
            030100     MOVE    SPACE TO TEST-RESULTS.                               DB2044.2
            030200     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB2044.2
            030300     GO TO CCVS1-EXIT.                                            DB2044.2
            030400 CLOSE-FILES.                                                     DB2044.2
            030500     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB2044.2
            030600 TERMINATE-CCVS.                                                  DB2044.2
            030700S    EXIT PROGRAM.                                                DB2044.2
            030800STERMINATE-CALL.                                                  DB2044.2
            030900     STOP     RUN.                                                DB2044.2
            031000 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB2044.2
            031100 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB2044.2
            031200 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB2044.2
            031300 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB2044.2
            031400     MOVE "****TEST DELETED****" TO RE-MARK.                      DB2044.2
            031500 PRINT-DETAIL.                                                    DB2044.2
            031600     IF REC-CT NOT EQUAL TO ZERO                                  DB2044.2
            031700             MOVE "." TO PARDOT-X                                 DB2044.2
            031800             MOVE REC-CT TO DOTVALUE.                             DB2044.2
            031900     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB2044.2
            032000     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB2044.2
            032100        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB2044.2
            032200          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB2044.2
            032300     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB2044.2
            032400     MOVE SPACE TO CORRECT-X.                                     DB2044.2
            032500     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB2044.2
            032600     MOVE     SPACE TO RE-MARK.                                   DB2044.2
            032700 HEAD-ROUTINE.                                                    DB2044.2
            032800     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2044.2
            032900     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB2044.2
            033000     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB2044.2
            033100 COLUMN-NAMES-ROUTINE.                                            DB2044.2
            033200     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2044.2
            033300     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2044.2
            033400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB2044.2
            033500 END-ROUTINE.                                                     DB2044.2
            033600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB2044.2
            033700 END-RTN-EXIT.                                                    DB2044.2
            033800     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2044.2
            033900 END-ROUTINE-1.                                                   DB2044.2
            034000      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB2044.2
            034100      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB2044.2
            034200      ADD PASS-COUNTER TO ERROR-HOLD.                             DB2044.2
            034300*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB2044.2
            034400      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB2044.2
            034500      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB2044.2
            034600      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB2044.2
            034700      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB2044.2
            034800  END-ROUTINE-12.                                                 DB2044.2
            034900      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB2044.2
            035000     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB2044.2
            035100         MOVE "NO " TO ERROR-TOTAL                                DB2044.2
            035200         ELSE                                                     DB2044.2
            035300         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB2044.2
            035400     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB2044.2
            035500     PERFORM WRITE-LINE.                                          DB2044.2
            035600 END-ROUTINE-13.                                                  DB2044.2
            035700     IF DELETE-CNT IS EQUAL TO ZERO                               DB2044.2
            035800         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB2044.2
            035900         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB2044.2
            036000     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB2044.2
            036100     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2044.2
            036200      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB2044.2
            036300          MOVE "NO " TO ERROR-TOTAL                               DB2044.2
            036400      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB2044.2
            036500      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB2044.2
            036600      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB2044.2
            036700     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2044.2
            036800 WRITE-LINE.                                                      DB2044.2
            036900     ADD 1 TO RECORD-COUNT.                                       DB2044.2
            037000Y    IF RECORD-COUNT GREATER 50                                   DB2044.2
            037100Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB2044.2
            037200Y        MOVE SPACE TO DUMMY-RECORD                               DB2044.2
            037300Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB2044.2
            037400Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB2044.2
            037500Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB2044.2
            037600Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB2044.2
            037700Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB2044.2
            037800Y        MOVE ZERO TO RECORD-COUNT.                               DB2044.2
            037900     PERFORM WRT-LN.                                              DB2044.2
            038000 WRT-LN.                                                          DB2044.2
            038100     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB2044.2
            038200     MOVE SPACE TO DUMMY-RECORD.                                  DB2044.2
            038300 BLANK-LINE-PRINT.                                                DB2044.2
            038400     PERFORM WRT-LN.                                              DB2044.2
            038500 FAIL-ROUTINE.                                                    DB2044.2
            038600     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB2044.2
            038700     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB2044.2
            038800     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB2044.2
            038900     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2044.2
            039000     GO TO FAIL-ROUTINE-EX.                                       DB2044.2
            039100 FAIL-ROUTINE-WRITE.                                              DB2044.2
            039200     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB2044.2
            039300     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB2044.2
            039400 FAIL-ROUTINE-EX. EXIT.                                           DB2044.2
            039500 BAIL-OUT.                                                        DB2044.2
            039600     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB2044.2
            039700     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB2044.2
            039800 BAIL-OUT-WRITE.                                                  DB2044.2
            039900     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB2044.2
            040000     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2044.2
            040100 BAIL-OUT-EX. EXIT.                                               DB2044.2
            040200 CCVS1-EXIT.                                                      DB2044.2
            040300     EXIT.                                                        DB2044.2
            040400 CREATE-INPUT-FILES SECTION.                                      DB2044.2
            040500 SET-UP-REC-AREAS.                                                DB2044.2
            040600     MOVE FILE-RECORD-INFO-SKELETON TO FILE-RECORD-INFO (1)       DB2044.2
            040700                                       FILE-RECORD-INFO (2).      DB2044.2
            040800     MOVE "SQ-FS1" TO XFILE-NAME (1).                             DB2044.2
            040900     MOVE "SQ-FS2" TO XFILE-NAME (2).                             DB2044.2
            041000     MOVE "REC-1" TO XRECORD-NAME (1).                            DB2044.2
            041100     MOVE "REC-2" TO XRECORD-NAME (2).                            DB2044.2
            041200     MOVE ".XXX." TO XPROGRAM-NAME (1)                            DB2044.2
            041300                     XPROGRAM-NAME (2).                           DB2044.2
            041400     MOVE 120 TO XRECORD-LENGTH (1)                               DB2044.2
            041500                 XRECORD-LENGTH (2).                              DB2044.2
            041600     MOVE "RC" TO CHARS-OR-RECORDS (1)                            DB2044.2
            041700                  CHARS-OR-RECORDS (2).                           DB2044.2
            041800     MOVE 1 TO XBLOCK-SIZE (1)                                    DB2044.2
            041900               XBLOCK-SIZE (2).                                   DB2044.2
            042000     MOVE 10 TO RECORDS-IN-FILE (1)                               DB2044.2
            042100                RECORDS-IN-FILE (2).                              DB2044.2
            042200     MOVE "SQ" TO XFILE-ORGANIZATION (1)                          DB2044.2
            042300                  XFILE-ORGANIZATION (2).                         DB2044.2
            042400     MOVE "S" TO XLABEL-TYPE (1)                                  DB2044.2
            042500                 XLABEL-TYPE (2).                                 DB2044.2
            042600     OPEN OUTPUT SQ-FS1  SQ-FS2.                                  DB2044.2
            042700 WRITE-FILES.                                                     DB2044.2
            042800     MOVE 1 TO XRECORD-NUMBER (1).                                DB2044.2
            042900     MOVE 2 TO XRECORD-NUMBER (2).                                DB2044.2
            043000     PERFORM WRITE-FILES-SUBROUTINE 10 TIMES.                     DB2044.2
            043100     CLOSE SQ-FS1  SQ-FS2.                                        DB2044.2
            043200      GO TO BEGIN-DB204A-TESTS.                                   DB2044.2
            043300 WRITE-FILES-SUBROUTINE.                                          DB2044.2
            043400     MOVE FILE-RECORD-INFO (1) TO REC-1.                          DB2044.2
            043500     WRITE REC-1.                                                 DB2044.2
            043600     ADD 2 TO XRECORD-NUMBER (1).                                 DB2044.2
            043700     MOVE FILE-RECORD-INFO (2) TO REC-2.                          DB2044.2
            043800     WRITE REC-2.                                                 DB2044.2
            043900     ADD 2 TO XRECORD-NUMBER (2).                                 DB2044.2
            044000 BEGIN-DB204A-TESTS SECTION.                                      DB2044.2
            044100 MERGE-TEST-INIT.                                                 DB2044.2
            044200     MOVE "MERGE-TEST" TO PAR-NAME.                               DB2044.2
            044300     MOVE "MERGE OUTPUT PROC" TO FEATURE.                         DB2044.2
            044400     MOVE SPACES TO ITEM-1.                                       DB2044.2
            044500     MOVE 0 TO KEY-1.                                             DB2044.2
            044600******************************************************************DB2044.2
            044700*    THE DEBUG-LINE (INSPT) TEST NAMED IN THE OUTPUT REPORT AS   *DB2044.2
            044800*    "MERGE-TEST-A" SHOULD POINT TO THE EXECUTABLE STATEMENT     *DB2044.2
            044900*    WHICH FOLLOWS THIS COMMENT SET AND WHICH READS, "MERGE      *DB2044.2
            045000*    ST-FS4  ON ASCENDING KEY SORT-KEY  USING SQ-FS1 SQ-FS2      *DB2044.2
            045100*    OUTPUT PROCEDURE IS MERGE-OUTPUT-PROC.".                    *DB2044.2
            045200******************************************************************DB2044.2
            045300 MERGE-TEST.                                                      DB2044.2
            045400     MERGE ST-FS4  ON ASCENDING KEY SORT-KEY                      DB2044.2
            045500         USING  SQ-FS1  SQ-FS2                                    DB2044.2
            045600         OUTPUT PROCEDURE IS MERGE-OUTPUT-PROC.                   DB2044.2
            045700     IF KEY-1 IS EQUAL TO 1                                       DB2044.2
            045800         PERFORM PASS                                             DB2044.2
            045900         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2044.2
            046000     ELSE  PERFORM FAIL                                           DB2044.2
            046100         MOVE "NO. TIMES DEBUG PROC EXECUTED" TO RE-MARK          DB2044.2
            046200         MOVE 1 TO CORRECT-18V0                                   DB2044.2
            046300         MOVE KEY-1 TO COMPUTED-18V0.                             DB2044.2
            046400     PERFORM PRINT-DETAIL                                         DB2044.2
            046500     IF KEY-1 IS EQUAL TO 0                                       DB2044.2
            046600         PERFORM DELETE-MERGE-TEST-SUBTESTS                       DB2044.2
            046700         GO TO END-OF-DB204A                                      DB2044.2
            046800     ELSE GO TO MERGE-TEST-A.                                     DB2044.2
            046900 MERGE-TEST-DELETE.                                               DB2044.2
            047000     PERFORM DE-LETE.                                             DB2044.2
            047100     PERFORM PRINT-DETAIL.                                        DB2044.2
            047200     PERFORM DELETE-MERGE-TEST-SUBTESTS.                          DB2044.2
            047300     GO TO END-OF-DB204A.                                         DB2044.2
            047400 DELETE-MERGE-TEST-SUBTESTS.                                      DB2044.2
            047500     MOVE "MERGE-TEST-A" TO PAR-NAME.                             DB2044.2
            047600     PERFORM DE-LETE.                                             DB2044.2
            047700     PERFORM PRINT-DETAIL.                                        DB2044.2
            047800     MOVE "MERGE-TEST-B" TO PAR-NAME.                             DB2044.2
            047900     PERFORM DE-LETE.                                             DB2044.2
            048000     PERFORM PRINT-DETAIL.                                        DB2044.2
            048100     MOVE "MERGE-TEST-C" TO PAR-NAME.                             DB2044.2
            048200     PERFORM DE-LETE.                                             DB2044.2
            048300     PERFORM PRINT-DETAIL.                                        DB2044.2
            048400 MERGE-TEST-A.                                                    DB2044.2
            048500     MOVE "MERGE-TEST-A" TO PAR-NAME.                             DB2044.2
            048600     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2044.2
            048700     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2044.2
            048800     MOVE LINE-1 TO COMPUTED-A.                                   DB2044.2
            048900     PERFORM INSPT.                                               DB2044.2
            049000     PERFORM PRINT-DETAIL.                                        DB2044.2
            049100 MERGE-TEST-B.                                                    DB2044.2
            049200     MOVE "MERGE-TEST-B" TO PAR-NAME.                             DB2044.2
            049300     IF UNQUAL-NAME-1 IS EQUAL TO "MERGE-OUTPUT-PROC"             DB2044.2
            049400         PERFORM PASS                                             DB2044.2
            049500     ELSE  PERFORM FAIL                                           DB2044.2
            049600         MOVE "MERGE-OUTPUT-PROC" TO CORRECT-A                    DB2044.2
            049700         MOVE NAME-1 TO COMPUTED-A.                               DB2044.2
            049800     MOVE "DEBUG-NAME" TO RE-MARK                                 DB2044.2
            049900     PERFORM PRINT-DETAIL.                                        DB2044.2
            050000 MERGE-TEST-C.                                                    DB2044.2
            050100     MOVE "MERGE-TEST-C" TO PAR-NAME.                             DB2044.2
            050200     IF CONTENTS-1 IS EQUAL TO "MERGE OUTPUT"                     DB2044.2
            050300         PERFORM PASS                                             DB2044.2
            050400     ELSE  PERFORM FAIL                                           DB2044.2
            050500         MOVE "MERGE OUTPUT" TO CORRECT-A                         DB2044.2
            050600         MOVE CONTENTS-1 TO COMPUTED-A.                           DB2044.2
            050700     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2044.2
            050800     PERFORM PRINT-DETAIL.                                        DB2044.2
            050900     GO TO END-OF-DB204A.                                         DB2044.2
            051000 MERGE-OUTPUT-PROC SECTION.                                       DB2044.2
            051100 OPEN-OUTPUT-FILE.                                                DB2044.2
            051200     OPEN OUTPUT SQ-FS3.                                          DB2044.2
            051300 RETURN-RECORDS.                                                  DB2044.2
            051400     RETURN ST-FS4 RECORD INTO REC-3                              DB2044.2
            051500         AT END GO TO CLOSE-OUTPUT-FILE.                          DB2044.2
            051600     WRITE REC-3.                                                 DB2044.2
            051700     GO TO RETURN-RECORDS.                                        DB2044.2
            051800 CLOSE-OUTPUT-FILE.                                               DB2044.2
            051900     CLOSE SQ-FS3.                                                DB2044.2
            052000 END-OF-DB204A SECTION.                                           DB2044.2
            052100XDUMP-SQ-FS1.                                                     DB2044.2
            052200X    OPEN INPUT SQ-FS1.                                           DB2044.2
            052300X    MOVE "DUMP OF SQ-FS1 FOLLOWS:" TO PRINT-REC.                 DB2044.2
            052400X    PERFORM WRITE-LINE.                                          DB2044.2
            052500XREAD-SQ-FS1.                                                     DB2044.2
            052600X    READ SQ-FS1  AT END GO TO DUMP-SQ-FS2.                       DB2044.2
            052700X    MOVE REC-1 TO PRINT-REC.                                     DB2044.2
            052800X    PERFORM WRITE-LINE.                                          DB2044.2
            052900X    GO TO READ-SQ-FS1.                                           DB2044.2
            053000XDUMP-SQ-FS2.                                                     DB2044.2
            053100X    CLOSE SQ-FS1.                                                DB2044.2
            053200X    OPEN INPUT SQ-FS2.                                           DB2044.2
            053300X    MOVE "DUMP OF SQ-FS2 FOLLOWS:" TO PRINT-REC.                 DB2044.2
            053400X    PERFORM WRITE-LINE.                                          DB2044.2
            053500XREAD-SQ-FS2.                                                     DB2044.2
            053600X    READ SQ-FS2 AT END GO TO DUMP-SQ-FS3.                        DB2044.2
            053700X    MOVE REC-2 TO PRINT-REC.                                     DB2044.2
            053800X    PERFORM WRITE-LINE.                                          DB2044.2
            053900X    GO TO READ-SQ-FS2.                                           DB2044.2
            054000XDUMP-SQ-FS3.                                                     DB2044.2
            054100X    CLOSE SQ-FS2.                                                DB2044.2
            054200X    OPEN INPUT SQ-FS3.                                           DB2044.2
            054300X    MOVE "DUMP OF SQ-FS3 FOLLOWS:" TO PRINT-REC.                 DB2044.2
            054400X    PERFORM WRITE-LINE.                                          DB2044.2
            054500XREAD-SQ-FS3.                                                     DB2044.2
            054600X    READ SQ-FS3 AT END GO TO END-OF-TESTS.                       DB2044.2
            054700X    MOVE REC-3 TO PRINT-REC.                                     DB2044.2
            054800X    PERFORM WRITE-LINE.                                          DB2044.2
            054900X    GO TO READ-SQ-FS3.                                           DB2044.2
            055000 END-OF-TESTS.                                                    DB2044.2
            055100     EXIT.                                                        DB2044.2
            055200 CCVS-EXIT SECTION.                                               DB2044.2
            055300 CCVS-999999.                                                     DB2044.2
            055400     GO TO CLOSE-FILES.                                           DB2044.2
                  *END-OF,DB204A                                                            
        """)
    )

    @Disabled("Requires Y indicator")
    @Test
    fun db2054_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB205A                                                      
            000100 IDENTIFICATION DIVISION.                                         DB2054.2
            000200 PROGRAM-ID.                                                      DB2054.2
            000300     DB205A.                                                      DB2054.2
            000400 AUTHOR.                                                          DB2054.2
            000500     FEDERAL COMPILER TESTING CENTER.                             DB2054.2
            000600 INSTALLATION.                                                    DB2054.2
            000700     GENERAL SERVICES ADMINISTRATION                              DB2054.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                DB2054.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 DB2054.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               DB2054.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 DB2054.2
            001200                                                                  DB2054.2
            001300     PHONE   (703) 756-6153                                       DB2054.2
            001400                                                                  DB2054.2
            001500     " HIGH       ".                                              DB2054.2
            001600 DATE-WRITTEN.                                                    DB2054.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           DB2054.2
            001800     CREATION DATE     /    VALIDATION DATE                       DB2054.2
            001900     "4.2 ".                                                      DB2054.2
            002000 SECURITY.                                                        DB2054.2
            002100     NONE.                                                        DB2054.2
            002200*                                                                 DB2054.2
            002300 ENVIRONMENT DIVISION.                                            DB2054.2
            002400 CONFIGURATION SECTION.                                           DB2054.2
            002500 SOURCE-COMPUTER.                                                 DB2054.2
            002600     XXXXX082                                                     DB2054.2
            002700         WITH DEBUGGING MODE.                                     DB2054.2
            002800 OBJECT-COMPUTER.                                                 DB2054.2
            002900     XXXXX083.                                                    DB2054.2
            003000 INPUT-OUTPUT SECTION.                                            DB2054.2
            003100 FILE-CONTROL.                                                    DB2054.2
            003200     SELECT PRINT-FILE ASSIGN TO                                  DB2054.2
            003300     XXXXX055.                                                    DB2054.2
            003400 DATA DIVISION.                                                   DB2054.2
            003500 FILE SECTION.                                                    DB2054.2
            003600 FD  PRINT-FILE                                                   DB2054.2
            003700     LABEL RECORDS                                                DB2054.2
            003800     XXXXX084                                                     DB2054.2
            003900     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB2054.2
            004000 01  PRINT-REC PICTURE X(120).                                    DB2054.2
            004100 01  DUMMY-RECORD PICTURE X(120).                                 DB2054.2
            004200 WORKING-STORAGE SECTION.                                         DB2054.2
            004300 77  WORK-AREA PIC X(72).                                         DB2054.2
            004400 01  ITEM-1.                                                      DB2054.2
            004500     02  KEY-1 PIC 99.                                            DB2054.2
            004600     02  LINE-1 PIC X(6).                                         DB2054.2
            004700     02  NAME-1 PIC X(30).                                        DB2054.2
            004800     02  UNQUAL-NAME-1 PIC X(30).                                 DB2054.2
            004900     02  CONTENTS-1 PIC X(87).                                    DB2054.2
            005000 01  TEST-RESULTS.                                                DB2054.2
            005100     02 FILLER                    PICTURE X VALUE SPACE.          DB2054.2
            005200     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB2054.2
            005300     02 FILLER                    PICTURE X VALUE SPACE.          DB2054.2
            005400     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB2054.2
            005500     02 FILLER                    PICTURE X  VALUE SPACE.         DB2054.2
            005600     02  PAR-NAME.                                                DB2054.2
            005700       03 FILLER PICTURE X(12) VALUE SPACE.                       DB2054.2
            005800       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB2054.2
            005900       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB2054.2
            006000       03 FILLER PIC X(5) VALUE SPACE.                            DB2054.2
            006100     02 FILLER PIC X(10) VALUE SPACE.                             DB2054.2
            006200     02 RE-MARK PIC X(61).                                        DB2054.2
            006300 01  TEST-COMPUTED.                                               DB2054.2
            006400     02 FILLER PIC X(30) VALUE SPACE.                             DB2054.2
            006500     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB2054.2
            006600     02 COMPUTED-X.                                               DB2054.2
            006700     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB2054.2
            006800     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB2054.2
            006900     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB2054.2
            007000     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB2054.2
            007100     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB2054.2
            007200     03       CM-18V0 REDEFINES COMPUTED-A.                       DB2054.2
            007300         04 COMPUTED-18V0                   PICTURE -9(18).       DB2054.2
            007400         04 FILLER                          PICTURE X.            DB2054.2
            007500     03 FILLER PIC X(50) VALUE SPACE.                             DB2054.2
            007600 01  TEST-CORRECT.                                                DB2054.2
            007700     02 FILLER PIC X(30) VALUE SPACE.                             DB2054.2
            007800     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB2054.2
            007900     02 CORRECT-X.                                                DB2054.2
            008000     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB2054.2
            008100     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB2054.2
            008200     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB2054.2
            008300     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB2054.2
            008400     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB2054.2
            008500     03      CR-18V0 REDEFINES CORRECT-A.                         DB2054.2
            008600         04 CORRECT-18V0                    PICTURE -9(18).       DB2054.2
            008700         04 FILLER                          PICTURE X.            DB2054.2
            008800     03 FILLER PIC X(50) VALUE SPACE.                             DB2054.2
            008900 01  CCVS-C-1.                                                    DB2054.2
            009000     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB2054.2
            009100-    "SS  PARAGRAPH-NAME                                          DB2054.2
            009200-    "        REMARKS".                                           DB2054.2
            009300     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB2054.2
            009400 01  CCVS-C-2.                                                    DB2054.2
            009500     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2054.2
            009600     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB2054.2
            009700     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB2054.2
            009800     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB2054.2
            009900     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB2054.2
            010000 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB2054.2
            010100 01  REC-CT PICTURE 99 VALUE ZERO.                                DB2054.2
            010200 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB2054.2
            010300 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB2054.2
            010400 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB2054.2
            010500 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB2054.2
            010600 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB2054.2
            010700 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB2054.2
            010800 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB2054.2
            010900 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB2054.2
            011000 01  CCVS-H-1.                                                    DB2054.2
            011100     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB2054.2
            011200     02 FILLER PICTURE X(67) VALUE                                DB2054.2
            011300     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB2054.2
            011400-    " SYSTEM".                                                   DB2054.2
            011500     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB2054.2
            011600 01  CCVS-H-2.                                                    DB2054.2
            011700     02 FILLER PICTURE X(52) VALUE IS                             DB2054.2
            011800     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB2054.2
            011900     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB2054.2
            012000     02 TEST-ID PICTURE IS X(9).                                  DB2054.2
            012100     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB2054.2
            012200 01  CCVS-H-3.                                                    DB2054.2
            012300     02  FILLER PICTURE X(34) VALUE                               DB2054.2
            012400     " FOR OFFICIAL USE ONLY    ".                                DB2054.2
            012500     02  FILLER PICTURE X(58) VALUE                               DB2054.2
            012600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB2054.2
            012700     02  FILLER PICTURE X(28) VALUE                               DB2054.2
            012800     "  COPYRIGHT   1974 ".                                       DB2054.2
            012900 01  CCVS-E-1.                                                    DB2054.2
            013000     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB2054.2
            013100     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB2054.2
            013200     02 ID-AGAIN PICTURE IS X(9).                                 DB2054.2
            013300     02 FILLER PICTURE X(45) VALUE IS                             DB2054.2
            013400     " NTIS DISTRIBUTION COBOL 74".                               DB2054.2
            013500 01  CCVS-E-2.                                                    DB2054.2
            013600     02  FILLER                   PICTURE X(31)  VALUE            DB2054.2
            013700     SPACE.                                                       DB2054.2
            013800     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB2054.2
            013900     02 CCVS-E-2-2.                                               DB2054.2
            014000         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB2054.2
            014100         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB2054.2
            014200         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB2054.2
            014300 01  CCVS-E-3.                                                    DB2054.2
            014400     02  FILLER PICTURE X(22) VALUE                               DB2054.2
            014500     " FOR OFFICIAL USE ONLY".                                    DB2054.2
            014600     02  FILLER PICTURE X(12) VALUE SPACE.                        DB2054.2
            014700     02  FILLER PICTURE X(58) VALUE                               DB2054.2
            014800     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB2054.2
            014900     02  FILLER PICTURE X(13) VALUE SPACE.                        DB2054.2
            015000     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB2054.2
            015100 01  CCVS-E-4.                                                    DB2054.2
            015200     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB2054.2
            015300     02 FILLER PIC XXXX VALUE " OF ".                             DB2054.2
            015400     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB2054.2
            015500     02 FILLER PIC X(40) VALUE                                    DB2054.2
            015600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB2054.2
            015700 01  XXINFO.                                                      DB2054.2
            015800     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB2054.2
            015900     02 INFO-TEXT.                                                DB2054.2
            016000     04 FILLER PIC X(20) VALUE SPACE.                             DB2054.2
            016100     04 XXCOMPUTED PIC X(20).                                     DB2054.2
            016200     04 FILLER PIC X(5) VALUE SPACE.                              DB2054.2
            016300     04 XXCORRECT PIC X(20).                                      DB2054.2
            016400 01  HYPHEN-LINE.                                                 DB2054.2
            016500     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB2054.2
            016600     02 FILLER PICTURE IS X(65) VALUE IS "************************DB2054.2
            016700-    "*****************************************".                 DB2054.2
            016800     02 FILLER PICTURE IS X(54) VALUE IS "************************DB2054.2
            016900-    "******************************".                            DB2054.2
            017000 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB2054.2
            017100     "DB205A".                                                    DB2054.2
            017200 COMMUNICATION SECTION.                                           DB2054.2
            017300 CD  CM-INQUE FOR INPUT.                                          DB2054.2
            017400 01  INQUE-SPECS.                                                 DB2054.2
            017500     02  IN-QUEUE PIC X(12) VALUE                                 DB2054.2
            017600     XXXXX030.                                                    DB2054.2
            017700     02  FILLER PIC X(75) VALUE SPACES.                           DB2054.2
            017800 CD  CM-OUTQUE FOR OUTPUT.                                        DB2054.2
            017900 01  OUTQUE-SPECS.                                                DB2054.2
            018000     02  DEST-COUNT PIC 9(4) VALUE IS 1.                          DB2054.2
            018100     02  OUT-LENGTH PIC 9(4) VALUE IS 72.                         DB2054.2
            018200     02  OUTT-STATUS PIC X(3).                                    DB2054.2
            018300     02  SYM-DEST PIC X(12) VALUE IS                              DB2054.2
            018400     XXXXX032.                                                    DB2054.2
            018500 PROCEDURE DIVISION.                                              DB2054.2
            018600 DECLARATIVES.                                                    DB2054.2
            018700 DEBUG-PROCEDURE SECTION.                                         DB2054.2
            018800     USE FOR DEBUGGING ON CM-INQUE  CM-OUTQUE.                    DB2054.2
            018900 COMMUNICATION-PROC.                                              DB2054.2
            019000     MOVE 1 TO KEY-1.                                             DB2054.2
            019100     MOVE DEBUG-LINE TO LINE-1.                                   DB2054.2
            019200     MOVE DEBUG-NAME TO NAME-1  UNQUAL-NAME-1.                    DB2054.2
            019300     MOVE DEBUG-CONTENTS TO CONTENTS-1.                           DB2054.2
            019400     INSPECT UNQUAL-NAME-1 REPLACING CHARACTERS BY SPACES         DB2054.2
            019500         AFTER INITIAL SPACE.                                     DB2054.2
            019600 END DECLARATIVES.                                                DB2054.2
            019700 CCVS1 SECTION.                                                   DB2054.2
            019800 OPEN-FILES.                                                      DB2054.2
            019900     OPEN     OUTPUT PRINT-FILE.                                  DB2054.2
            020000     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB2054.2
            020100     MOVE    SPACE TO TEST-RESULTS.                               DB2054.2
            020200     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB2054.2
            020300     GO TO CCVS1-EXIT.                                            DB2054.2
            020400 CLOSE-FILES.                                                     DB2054.2
            020500     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB2054.2
            020600 TERMINATE-CCVS.                                                  DB2054.2
            020700S    EXIT PROGRAM.                                                DB2054.2
            020800STERMINATE-CALL.                                                  DB2054.2
            020900     STOP     RUN.                                                DB2054.2
            021000 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB2054.2
            021100 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB2054.2
            021200 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB2054.2
            021300 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB2054.2
            021400     MOVE "****TEST DELETED****" TO RE-MARK.                      DB2054.2
            021500 PRINT-DETAIL.                                                    DB2054.2
            021600     IF REC-CT NOT EQUAL TO ZERO                                  DB2054.2
            021700             MOVE "." TO PARDOT-X                                 DB2054.2
            021800             MOVE REC-CT TO DOTVALUE.                             DB2054.2
            021900     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB2054.2
            022000     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB2054.2
            022100        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB2054.2
            022200          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB2054.2
            022300     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB2054.2
            022400     MOVE SPACE TO CORRECT-X.                                     DB2054.2
            022500     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB2054.2
            022600     MOVE     SPACE TO RE-MARK.                                   DB2054.2
            022700 HEAD-ROUTINE.                                                    DB2054.2
            022800     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2054.2
            022900     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB2054.2
            023000     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB2054.2
            023100 COLUMN-NAMES-ROUTINE.                                            DB2054.2
            023200     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2054.2
            023300     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2054.2
            023400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB2054.2
            023500 END-ROUTINE.                                                     DB2054.2
            023600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB2054.2
            023700 END-RTN-EXIT.                                                    DB2054.2
            023800     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB2054.2
            023900 END-ROUTINE-1.                                                   DB2054.2
            024000      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB2054.2
            024100      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB2054.2
            024200      ADD PASS-COUNTER TO ERROR-HOLD.                             DB2054.2
            024300*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB2054.2
            024400      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB2054.2
            024500      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB2054.2
            024600      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB2054.2
            024700      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB2054.2
            024800  END-ROUTINE-12.                                                 DB2054.2
            024900      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB2054.2
            025000     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB2054.2
            025100         MOVE "NO " TO ERROR-TOTAL                                DB2054.2
            025200         ELSE                                                     DB2054.2
            025300         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB2054.2
            025400     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB2054.2
            025500     PERFORM WRITE-LINE.                                          DB2054.2
            025600 END-ROUTINE-13.                                                  DB2054.2
            025700     IF DELETE-CNT IS EQUAL TO ZERO                               DB2054.2
            025800         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB2054.2
            025900         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB2054.2
            026000     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB2054.2
            026100     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2054.2
            026200      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB2054.2
            026300          MOVE "NO " TO ERROR-TOTAL                               DB2054.2
            026400      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB2054.2
            026500      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB2054.2
            026600      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB2054.2
            026700     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB2054.2
            026800 WRITE-LINE.                                                      DB2054.2
            026900     ADD 1 TO RECORD-COUNT.                                       DB2054.2
            027000Y    IF RECORD-COUNT GREATER 50                                   DB2054.2
            027100Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB2054.2
            027200Y        MOVE SPACE TO DUMMY-RECORD                               DB2054.2
            027300Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB2054.2
            027400Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB2054.2
            027500Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB2054.2
            027600Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB2054.2
            027700Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB2054.2
            027800Y        MOVE ZERO TO RECORD-COUNT.                               DB2054.2
            027900     PERFORM WRT-LN.                                              DB2054.2
            028000 WRT-LN.                                                          DB2054.2
            028100     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB2054.2
            028200     MOVE SPACE TO DUMMY-RECORD.                                  DB2054.2
            028300 BLANK-LINE-PRINT.                                                DB2054.2
            028400     PERFORM WRT-LN.                                              DB2054.2
            028500 FAIL-ROUTINE.                                                    DB2054.2
            028600     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB2054.2
            028700     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB2054.2
            028800     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB2054.2
            028900     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2054.2
            029000     GO TO FAIL-ROUTINE-EX.                                       DB2054.2
            029100 FAIL-ROUTINE-WRITE.                                              DB2054.2
            029200     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB2054.2
            029300     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB2054.2
            029400 FAIL-ROUTINE-EX. EXIT.                                           DB2054.2
            029500 BAIL-OUT.                                                        DB2054.2
            029600     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB2054.2
            029700     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB2054.2
            029800 BAIL-OUT-WRITE.                                                  DB2054.2
            029900     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB2054.2
            030000     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB2054.2
            030100 BAIL-OUT-EX. EXIT.                                               DB2054.2
            030200 CCVS1-EXIT.                                                      DB2054.2
            030300     EXIT.                                                        DB2054.2
            030400 BEGIN-DB205A-TESTS SECTION.                                      DB2054.2
            030500 DISABLE-TEST-1-INIT.                                             DB2054.2
            030600     MOVE SPACES TO ITEM-1.                                       DB2054.2
            030700     MOVE 0 TO KEY-1.                                             DB2054.2
            030800     MOVE "DISABLE-TEST-1" TO PAR-NAME.                           DB2054.2
            030900     MOVE "DISABLE CD-NAME" TO FEATURE.                           DB2054.2
            031000 DISABLE-TEST-1.                                                  DB2054.2
            031100     DISABLE INPUT CM-INQUE WITH KEY                              DB2054.2
            031200     XXXXX031.                                                    DB2054.2
            031300     IF KEY-1 IS EQUAL TO 1                                       DB2054.2
            031400         PERFORM PASS                                             DB2054.2
            031500         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2054.2
            031600         PERFORM PRINT-DETAIL                                     DB2054.2
            031700         GO TO DISABLE-TEST-1A                                    DB2054.2
            031800     ELSE  PERFORM FAIL                                           DB2054.2
            031900         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2054.2
            032000         PERFORM PRINT-DETAIL                                     DB2054.2
            032100         PERFORM DELETE-DISABLE-TEST-1-SUBTEST                    DB2054.2
            032200         GO TO ENABLE-TEST-1-INIT.                                DB2054.2
            032300 DISABLE-TEST-1-DELETE.                                           DB2054.2
            032400     PERFORM DE-LETE.                                             DB2054.2
            032500     PERFORM PRINT-DETAIL.                                        DB2054.2
            032600     PERFORM DELETE-DISABLE-TEST-1-SUBTEST.                       DB2054.2
            032700     GO TO ENABLE-TEST-1-INIT.                                    DB2054.2
            032800 DELETE-DISABLE-TEST-1-SUBTEST.                                   DB2054.2
            032900     MOVE "DISABLE-TEST-1A" TO PAR-NAME.                          DB2054.2
            033000     PERFORM DE-LETE.                                             DB2054.2
            033100     PERFORM PRINT-DETAIL.                                        DB2054.2
            033200 DISABLE-TEST-1A.                                                 DB2054.2
            033300     MOVE "DISABLE-TEST-1A" TO PAR-NAME.                          DB2054.2
            033400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2054.2
            033500     MOVE "DISABLE-TEST-1A" TO PAR-NAME.                          DB2054.2
            033600     IF CONTENTS-1 IS EQUAL TO INQUE-SPECS                        DB2054.2
            033700         PERFORM PASS                                             DB2054.2
            033800         PERFORM PRINT-DETAIL                                     DB2054.2
            033900         GO TO ENABLE-TEST-1-INIT                                 DB2054.2
            034000     ELSE  PERFORM FAIL                                           DB2054.2
            034100         MOVE "1ST LINE FOLLOWING" TO CORRECT-A                   DB2054.2
            034200         MOVE "2ND LINE FOLLOWING" TO COMPUTED-A                  DB2054.2
            034300         PERFORM PRINT-DETAIL.                                    DB2054.2
            034400     MOVE INQUE-SPECS TO PRINT-REC.                               DB2054.2
            034500     PERFORM WRITE-LINE.                                          DB2054.2
            034600     MOVE CONTENTS-1 TO PRINT-REC.                                DB2054.2
            034700     PERFORM WRITE-LINE.                                          DB2054.2
            034800 ENABLE-TEST-1-INIT.                                              DB2054.2
            034900     MOVE SPACES TO ITEM-1.                                       DB2054.2
            035000     MOVE 0 TO KEY-1.                                             DB2054.2
            035100     MOVE "ENABLE-TEST-1" TO PAR-NAME.                            DB2054.2
            035200     MOVE "ENABLE CD-NAME" TO FEATURE.                            DB2054.2
            035300 ENABLE-TEST-1.                                                   DB2054.2
            035400     ENABLE OUTPUT CM-OUTQUE WITH KEY                             DB2054.2
            035500     XXXXX033.                                                    DB2054.2
            035600     IF KEY-1 IS EQUAL TO 1                                       DB2054.2
            035700         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2054.2
            035800         PERFORM PASS                                             DB2054.2
            035900         PERFORM PRINT-DETAIL                                     DB2054.2
            036000         GO TO ENABLE-TEST-1A                                     DB2054.2
            036100     ELSE  PERFORM FAIL                                           DB2054.2
            036200         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2054.2
            036300         PERFORM PRINT-DETAIL                                     DB2054.2
            036400         PERFORM DELETE-ENABLE-TEST-1-SUBTEST                     DB2054.2
            036500         GO TO ACCEPT-TEST-1-INIT.                                DB2054.2
            036600 ENABLE-TEST-1-DELETE.                                            DB2054.2
            036700     PERFORM DE-LETE.                                             DB2054.2
            036800     PERFORM PRINT-DETAIL.                                        DB2054.2
            036900     PERFORM DELETE-ENABLE-TEST-1-SUBTEST.                        DB2054.2
            037000     GO TO ACCEPT-TEST-1-INIT.                                    DB2054.2
            037100 DELETE-ENABLE-TEST-1-SUBTEST.                                    DB2054.2
            037200     MOVE "ENABLE-TEST-1A" TO PAR-NAME.                           DB2054.2
            037300     PERFORM DE-LETE.                                             DB2054.2
            037400     PERFORM PRINT-DETAIL.                                        DB2054.2
            037500 ENABLE-TEST-1A.                                                  DB2054.2
            037600     MOVE "ENABLE-TEST-1A" TO PAR-NAME.                           DB2054.2
            037700     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2054.2
            037800     IF CONTENTS-1 IS EQUAL TO OUTQUE-SPECS                       DB2054.2
            037900         PERFORM PASS                                             DB2054.2
            038000         PERFORM PRINT-DETAIL                                     DB2054.2
            038100         GO TO ACCEPT-TEST-1-INIT                                 DB2054.2
            038200     ELSE  PERFORM FAIL                                           DB2054.2
            038300         MOVE "1ST LINE FOLLOWING" TO CORRECT-A                   DB2054.2
            038400         MOVE "2ND LINE FOLLOWING" TO COMPUTED-A                  DB2054.2
            038500         PERFORM PRINT-DETAIL.                                    DB2054.2
            038600     MOVE OUTQUE-SPECS TO PRINT-REC.                              DB2054.2
            038700     PERFORM WRITE-LINE.                                          DB2054.2
            038800     MOVE CONTENTS-1 TO PRINT-REC.                                DB2054.2
            038900     PERFORM WRITE-LINE.                                          DB2054.2
            039000 ACCEPT-TEST-1-INIT.                                              DB2054.2
            039100     MOVE SPACES TO ITEM-1.                                       DB2054.2
            039200     MOVE 0 TO KEY-1.                                             DB2054.2
            039300     MOVE "ACCEPT-TEST-1" TO PAR-NAME.                            DB2054.2
            039400     MOVE "ACCEPT CD-NAME" TO FEATURE.                            DB2054.2
            039500 ACCEPT-TEST-1.                                                   DB2054.2
            039600     ACCEPT CM-INQUE MESSAGE COUNT.                               DB2054.2
            039700     IF KEY-1 IS EQUAL TO 1                                       DB2054.2
            039800         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2054.2
            039900         PERFORM PASS                                             DB2054.2
            040000         PERFORM PRINT-DETAIL                                     DB2054.2
            040100         GO TO ACCEPT-TEST-1A                                     DB2054.2
            040200     ELSE  PERFORM FAIL                                           DB2054.2
            040300         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2054.2
            040400         PERFORM PRINT-DETAIL                                     DB2054.2
            040500         PERFORM DELETE-ACCEPT-TEST-1-SUBTEST                     DB2054.2
            040600         GO TO RECEIVE-TEST-1-INIT.                               DB2054.2
            040700 ACCEPT-TEST-1-DELETE.                                            DB2054.2
            040800     PERFORM DE-LETE.                                             DB2054.2
            040900     PERFORM PRINT-DETAIL.                                        DB2054.2
            041000     PERFORM DELETE-ACCEPT-TEST-1-SUBTEST.                        DB2054.2
            041100     GO TO RECEIVE-TEST-1-INIT.                                   DB2054.2
            041200 DELETE-ACCEPT-TEST-1-SUBTEST.                                    DB2054.2
            041300     MOVE "ACCEPT-TEST-1-1A" TO PAR-NAME                          DB2054.2
            041400     PERFORM DE-LETE.                                             DB2054.2
            041500     PERFORM PRINT-DETAIL.                                        DB2054.2
            041600 ACCEPT-TEST-1A.                                                  DB2054.2
            041700     MOVE "ACCEPT-TEST-1A" TO PAR-NAME.                           DB2054.2
            041800     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2054.2
            041900     IF CONTENTS-1 IS EQUAL TO INQUE-SPECS                        DB2054.2
            042000         PERFORM PASS                                             DB2054.2
            042100         PERFORM PRINT-DETAIL                                     DB2054.2
            042200         GO TO RECEIVE-TEST-1-INIT                                DB2054.2
            042300     ELSE    PERFORM FAIL                                         DB2054.2
            042400         MOVE "1ST LINE FOLLOWING" TO CORRECT-A                   DB2054.2
            042500         MOVE "2ND LINE FOLLOWING" TO COMPUTED-A                  DB2054.2
            042600         PERFORM PRINT-DETAIL.                                    DB2054.2
            042700     MOVE INQUE-SPECS TO PRINT-REC.                               DB2054.2
            042800     PERFORM WRITE-LINE.                                          DB2054.2
            042900     MOVE CONTENTS-1 TO PRINT-REC.                                DB2054.2
            043000     PERFORM WRITE-LINE.                                          DB2054.2
            043100 RECEIVE-TEST-1-INIT.                                             DB2054.2
            043200     MOVE SPACES TO ITEM-1.                                       DB2054.2
            043300     MOVE 0 TO KEY-1.                                             DB2054.2
            043400     MOVE "RECEIVE-TEST-1" TO PAR-NAME.                           DB2054.2
            043500     MOVE "RECEIVE W/ NO DATA" TO FEATURE.                        DB2054.2
            043600 RECEIVE-TEST-1.                                                  DB2054.2
            043700     RECEIVE CM-INQUE MESSAGE INTO WORK-AREA                      DB2054.2
            043800         NO DATA GO TO RECEIVE-TEST-1-CONT.                       DB2054.2
            043900     GO TO RECEIVE-TEST-1-INIT.                                   DB2054.2
            044000 RECEIVE-TEST-1-DELETE.                                           DB2054.2
            044100     PERFORM DE-LETE.                                             DB2054.2
            044200     PERFORM PRINT-DETAIL.                                        DB2054.2
            044300     GO TO SEND-TEST-1-INIT.                                      DB2054.2
            044400 RECEIVE-TEST-1-CONT.                                             DB2054.2
            044500     IF KEY-1 IS EQUAL TO 0                                       DB2054.2
            044600         PERFORM PASS                                             DB2054.2
            044700         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2054.2
            044800     ELSE  PERFORM FAIL                                           DB2054.2
            044900         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK.               DB2054.2
            045000     PERFORM PRINT-DETAIL.                                        DB2054.2
            045100 SEND-TEST-1-INIT.                                                DB2054.2
            045200     ENABLE INPUT CM-INQUE WITH KEY                               DB2054.2
            045300     XXXXX031.                                                    DB2054.2
            045400     MOVE SPACES TO ITEM-1.                                       DB2054.2
            045500     MOVE 0 TO KEY-1.                                             DB2054.2
            045600     MOVE "SEND-TEST-1" TO PAR-NAME.                              DB2054.2
            045700     MOVE "SEND CD-NAME" TO FEATURE.                              DB2054.2
            045800 SEND-TEST-1.                                                     DB2054.2
            045900     MOVE "ENTER ONE MESSAGE NOW." TO WORK-AREA.                  DB2054.2
            046000******************************************************************DB2054.2
            046100*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB2054.2
            046200*    OUTPUT REPORT AS "SEND-TEST-1A" SHOULD POINT TO THE         *DB2054.2
            046300*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB2054.2
            046400*    WHICH READS, "SEND CM-OUTQUE FROM WORK-AREA WITH EGI.".     *DB2054.2
            046500******************************************************************DB2054.2
            046600     SEND CM-OUTQUE FROM WORK-AREA WITH EGI.                      DB2054.2
            046700     IF KEY-1 IS EQUAL TO 1                                       DB2054.2
            046800         PERFORM PASS                                             DB2054.2
            046900         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2054.2
            047000         PERFORM PRINT-DETAIL                                     DB2054.2
            047100         GO TO SEND-TEST-1A                                       DB2054.2
            047200     ELSE  PERFORM FAIL                                           DB2054.2
            047300         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2054.2
            047400         PERFORM PRINT-DETAIL                                     DB2054.2
            047500         PERFORM DELETE-SEND-TEST-1-SUBTESTS                      DB2054.2
            047600         GO TO RECEIVE-TEST-2-INIT.                               DB2054.2
            047700 SEND-TEST-1-DELETE.                                              DB2054.2
            047800     PERFORM DE-LETE.                                             DB2054.2
            047900     PERFORM PRINT-DETAIL.                                        DB2054.2
            048000     PERFORM DELETE-SEND-TEST-1-SUBTESTS.                         DB2054.2
            048100     GO TO RECEIVE-TEST-2-INIT.                                   DB2054.2
            048200 DELETE-SEND-TEST-1-SUBTESTS.                                     DB2054.2
            048300     MOVE "SEND-TEST-1A" TO PAR-NAME.                             DB2054.2
            048400     PERFORM DE-LETE.                                             DB2054.2
            048500     PERFORM PRINT-DETAIL.                                        DB2054.2
            048600     MOVE "SEND-TEST-1B" TO PAR-NAME.                             DB2054.2
            048700     PERFORM DE-LETE.                                             DB2054.2
            048800     PERFORM PRINT-DETAIL.                                        DB2054.2
            048900     MOVE "SEND-TEST-1C" TO PAR-NAME.                             DB2054.2
            049000     PERFORM DE-LETE.                                             DB2054.2
            049100     PERFORM PRINT-DETAIL.                                        DB2054.2
            049200 SEND-TEST-1A.                                                    DB2054.2
            049300     MOVE "SEND-TEST-1A" TO PAR-NAME.                             DB2054.2
            049400     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2054.2
            049500     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2054.2
            049600     MOVE LINE-1 TO COMPUTED-A.                                   DB2054.2
            049700     PERFORM INSPT.                                               DB2054.2
            049800     PERFORM PRINT-DETAIL.                                        DB2054.2
            049900 SEND-TEST-1B.                                                    DB2054.2
            050000     MOVE "SEND-TEST-1B" TO PAR-NAME.                             DB2054.2
            050100     IF UNQUAL-NAME-1 IS EQUAL TO "CM-OUTQUE"                     DB2054.2
            050200         PERFORM PASS                                             DB2054.2
            050300     ELSE PERFORM FAIL                                            DB2054.2
            050400         MOVE "CM-OUTQUE" TO CORRECT-A                            DB2054.2
            050500         MOVE NAME-1 TO COMPUTED-A.                               DB2054.2
            050600     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2054.2
            050700     PERFORM PRINT-DETAIL.                                        DB2054.2
            050800 SEND-TEST-1C.                                                    DB2054.2
            050900     MOVE "SEND-TEST-1C" TO PAR-NAME.                             DB2054.2
            051000     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2054.2
            051100     IF CONTENTS-1 IS EQUAL TO OUTQUE-SPECS                       DB2054.2
            051200         PERFORM PASS                                             DB2054.2
            051300         PERFORM PRINT-DETAIL                                     DB2054.2
            051400         GO TO RECEIVE-TEST-2-INIT                                DB2054.2
            051500     ELSE  PERFORM FAIL                                           DB2054.2
            051600         MOVE "1ST LINE FOLLOWING" TO CORRECT-A                   DB2054.2
            051700         MOVE "2ND LINE FOLLOWING" TO COMPUTED-A                  DB2054.2
            051800         PERFORM PRINT-DETAIL.                                    DB2054.2
            051900     MOVE OUTQUE-SPECS TO PRINT-REC.                              DB2054.2
            052000     PERFORM WRITE-LINE.                                          DB2054.2
            052100     MOVE CONTENTS-1 TO PRINT-REC.                                DB2054.2
            052200     PERFORM WRITE-LINE.                                          DB2054.2
            052300 RECEIVE-TEST-2-INIT.                                             DB2054.2
            052400     MOVE SPACES TO ITEM-1.                                       DB2054.2
            052500     MOVE 0 TO KEY-1.                                             DB2054.2
            052600     MOVE "RECEIVE-TEST-2" TO PAR-NAME.                           DB2054.2
            052700     MOVE "RECEIVE W/O NO DATA" TO FEATURE.                       DB2054.2
            052800******************************************************************DB2054.2
            052900*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB2054.2
            053000*    OUTPUT REPORT AS "RECEIVE-TEST-2A" SHOULD POINT TO THE      *DB2054.2
            053100*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB2054.2
            053200*    WHICH READS, "RECEIVE CM-INQUE MESSAGE INTO WORK-AREA.".    *DB2054.2
            053300******************************************************************DB2054.2
            053400 RECEIVE-TEST-2.                                                  DB2054.2
            053500     RECEIVE CM-INQUE MESSAGE INTO WORK-AREA.                     DB2054.2
            053600     IF KEY-1 IS EQUAL TO 1                                       DB2054.2
            053700         PERFORM PASS                                             DB2054.2
            053800         MOVE "DEBUG PROC WAS EXECUTED" TO RE-MARK                DB2054.2
            053900         PERFORM PRINT-DETAIL                                     DB2054.2
            054000         GO TO RECEIVE-TEST-2A                                    DB2054.2
            054100     ELSE  PERFORM FAIL                                           DB2054.2
            054200         MOVE "DEBUG PROC NOT EXECUTED" TO RE-MARK                DB2054.2
            054300         PERFORM PRINT-DETAIL                                     DB2054.2
            054400         PERFORM DELETE-RECEIVE-TEST-2-SUBTESTS                   DB2054.2
            054500         GO TO END-OF-DB205A.                                     DB2054.2
            054600 DELETE-RECEIVE-TEST-2.                                           DB2054.2
            054700     PERFORM DE-LETE.                                             DB2054.2
            054800     PERFORM PRINT-DETAIL.                                        DB2054.2
            054900     PERFORM DELETE-RECEIVE-TEST-2-SUBTESTS.                      DB2054.2
            055000     GO TO END-OF-DB205A.                                         DB2054.2
            055100 DELETE-RECEIVE-TEST-2-SUBTESTS.                                  DB2054.2
            055200     MOVE "RECEIVE-TEST-2A" TO PAR-NAME.                          DB2054.2
            055300     PERFORM DE-LETE.                                             DB2054.2
            055400     PERFORM PRINT-DETAIL.                                        DB2054.2
            055500     MOVE "RECEIVE-TEST-2B" TO PAR-NAME.                          DB2054.2
            055600     PERFORM DE-LETE.                                             DB2054.2
            055700     PERFORM PRINT-DETAIL.                                        DB2054.2
            055800     MOVE "RECEIVE-TEST-2C" TO PAR-NAME.                          DB2054.2
            055900     PERFORM DE-LETE.                                             DB2054.2
            056000     PERFORM PRINT-DETAIL.                                        DB2054.2
            056100 RECEIVE-TEST-2A.                                                 DB2054.2
            056200     MOVE "RECEIVE-TEST-2A" TO PAR-NAME.                          DB2054.2
            056300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB2054.2
            056400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB2054.2
            056500     MOVE LINE-1 TO COMPUTED-A.                                   DB2054.2
            056600     PERFORM INSPT.                                               DB2054.2
            056700     PERFORM PRINT-DETAIL.                                        DB2054.2
            056800 RECEIVE-TEST-2B.                                                 DB2054.2
            056900     MOVE "RECEIVE-TEST-2B" TO PAR-NAME.                          DB2054.2
            057000     IF UNQUAL-NAME-1 IS EQUAL TO "CM-INQUE"                      DB2054.2
            057100         PERFORM PASS                                             DB2054.2
            057200     ELSE  PERFORM FAIL                                           DB2054.2
            057300         MOVE "CM-INQUE" TO CORRECT-A                             DB2054.2
            057400         MOVE NAME-1 TO COMPUTED-A.                               DB2054.2
            057500     MOVE "DEBUG-NAME" TO RE-MARK.                                DB2054.2
            057600     PERFORM PRINT-DETAIL.                                        DB2054.2
            057700 RECEIVE-TEST-2C.                                                 DB2054.2
            057800     MOVE "RECEIVE-TEST-2C" TO PAR-NAME.                          DB2054.2
            057900     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB2054.2
            058000     IF CONTENTS-1 IS EQUAL TO INQUE-SPECS                        DB2054.2
            058100         PERFORM PASS                                             DB2054.2
            058200         PERFORM PRINT-DETAIL                                     DB2054.2
            058300         GO TO END-OF-DB205A                                      DB2054.2
            058400     ELSE  PERFORM FAIL                                           DB2054.2
            058500         MOVE "1ST LINE FOLLOWING" TO CORRECT-A                   DB2054.2
            058600         MOVE "2ND LINE FOLLOWING" TO COMPUTED-A.                 DB2054.2
            058700         PERFORM PRINT-DETAIL.                                    DB2054.2
            058800     MOVE INQUE-SPECS TO PRINT-REC.                               DB2054.2
            058900     PERFORM WRITE-LINE.                                          DB2054.2
            059000     MOVE CONTENTS-1 TO PRINT-REC.                                DB2054.2
            059100     PERFORM WRITE-LINE.                                          DB2054.2
            059200 END-OF-DB205A.                                                   DB2054.2
            059300     EXIT.                                                        DB2054.2
            059400 CCVS-EXIT SECTION.                                               DB2054.2
            059500 CCVS-999999.                                                     DB2054.2
            059600     GO TO CLOSE-FILES.                                           DB2054.2
                  *END-OF,DB205A                                                            
        """)
    )

    @Test
    fun db3014_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB301M                                                      
            000100 IDENTIFICATION DIVISION.                                         DB3014.2
            000200 PROGRAM-ID.                                                      DB3014.2
            000300     DB301M.                                                      DB3014.2
            000400*THE FOLLOWING PROGRAM TESTS THE FLAGGING OF LEVEL 1              DB3014.2
            000500*FEATURES OF THE DEBUGGING MODULE.                                DB3014.2
            000600 ENVIRONMENT DIVISION.                                            DB3014.2
            000700 CONFIGURATION SECTION.                                           DB3014.2
            000800 SOURCE-COMPUTER.                                                 DB3014.2
            000900     XXXXX082                                                     DB3014.2
            001000     WITH DEBUGGING MODE.                                         DB3014.2
            001100 OBJECT-COMPUTER.                                                 DB3014.2
            001200     XXXXX083.                                                    DB3014.2
            001300 INPUT-OUTPUT SECTION.                                            DB3014.2
            001400 FILE-CONTROL.                                                    DB3014.2
            001500     SELECT TFIL ASSIGN                                           DB3014.2
            001600     XXXXX014                                                     DB3014.2
            001700         ORGANIZATION IS SEQUENTIAL                               DB3014.2
            001800         ACCESS MODE IS SEQUENTIAL.                               DB3014.2
            001900 DATA DIVISION.                                                   DB3014.2
            002000 FILE SECTION.                                                    DB3014.2
            002100 FD TFIL.                                                         DB3014.2
            002200 01 FREC.                                                         DB3014.2
            002300     03 RKEY PIC 9(8).                                            DB3014.2
            002400                                                                  DB3014.2
            002500 PROCEDURE DIVISION.                                              DB3014.2
            002600                                                                  DB3014.2
            002700 DECLARATIVES.                                                    DB3014.2
            002800                                                                  DB3014.2
            002900                                                                  DB3014.2
            003000 BUGGING-2 SECTION.                                               DB3014.2
            003100                                                                  DB3014.2
            003200                                                                  DB3014.2
            003300     USE FOR DEBUGGING ON ALL PROCEDURES.                         DB3014.2
            003400*Message expected for above statement: NON-CONFORMING STANDARD    DB3014.2
            003500                                                                  DB3014.2
            003600                                                                  DB3014.2
            003700 END DECLARATIVES.                                                DB3014.2
            003800                                                                  DB3014.2
            003900 DB301M-FLAGS SECTION.                                            DB3014.2
            004000 DB301M-CONTROL.                                                  DB3014.2
            004100     DISPLAY "THIS IS A DUMMY PROCEDURE".                         DB3014.2
            004200     STOP RUN.                                                    DB3014.2
            004300                                                                  DB3014.2
            004400                                                                  DB3014.2
            004500*TOTAL NUMBER OF FLAGS EXPECTED = 1.                              DB3014.2
                  *END-OF,DB301M                                                            
        """)
    )

    @Test
    fun db3024_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB302M                                                      
            000100 IDENTIFICATION DIVISION.                                         DB3024.2
            000200 PROGRAM-ID.                                                      DB3024.2
            000300     DB302M.                                                      DB3024.2
            000400*THE FOLLOWING PROGRAM TESTS THE FLAGGING OF LEVEL 1              DB3024.2
            000500*OBSOLETE FEATURES THAT ARE USED IN DEBUGGING.                    DB3024.2
            000600 ENVIRONMENT DIVISION.                                            DB3024.2
            000700 CONFIGURATION SECTION.                                           DB3024.2
            000800 SOURCE-COMPUTER.                                                 DB3024.2
            000900     XXXXX082                                                     DB3024.2
            001000     WITH DEBUGGING MODE.                                         DB3024.2
            001100 OBJECT-COMPUTER.                                                 DB3024.2
            001200     XXXXX083.                                                    DB3024.2
            001300                                                                  DB3024.2
            001400 INPUT-OUTPUT SECTION.                                            DB3024.2
            001500 FILE-CONTROL.                                                    DB3024.2
            001600     SELECT TFIL ASSIGN                                           DB3024.2
            001700     XXXXX014                                                     DB3024.2
            001800         ORGANIZATION IS SEQUENTIAL                               DB3024.2
            001900         ACCESS MODE IS SEQUENTIAL.                               DB3024.2
            002000                                                                  DB3024.2
            002100 DATA DIVISION.                                                   DB3024.2
            002200 FILE SECTION.                                                    DB3024.2
            002300 FD TFIL.                                                         DB3024.2
            002400 01 FREC.                                                         DB3024.2
            002500     03 RKEY  PIC 9(8).                                           DB3024.2
            002600                                                                  DB3024.2
            002700 PROCEDURE DIVISION.                                              DB3024.2
            002800                                                                  DB3024.2
            002900 DECLARATIVES.                                                    DB3024.2
            003000                                                                  DB3024.2
            003100 BUGGING SECTION.                                                 DB3024.2
            003200                                                                  DB3024.2
            003300     USE FOR DEBUGGING ON DB302M-CONTROL.                         DB3024.2
            003400*Message expected for above statement: OBSOLETE                   DB3024.2
            003500                                                                  DB3024.2
            003600 END DECLARATIVES.                                                DB3024.2
            003700                                                                  DB3024.2
            003800 DB302-FLAGS SECTION.                                             DB3024.2
            003900                                                                  DB3024.2
            004000 DB302M-CONTROL.                                                  DB3024.2
            004100     DISPLAY "THIS IS A DUMMY PROCEDURE".                         DB3024.2
            004200     STOP RUN.                                                    DB3024.2
            004300                                                                  DB3024.2
            004400*TOTAL NUMBER OF FLAGS EXPECTED = 1.                              DB3024.2
                  *END-OF,DB302M                                                            
        """)
    )

    @Test
    fun db3034_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB303M                                                      
            000100 IDENTIFICATION DIVISION.                                         DB3034.2
            000200 PROGRAM-ID.                                                      DB3034.2
            000300     DB303M.                                                      DB3034.2
            000400*THE FOLLOWING PROGRAM TESTS THE FLAGGING OF LEVEL 2              DB3034.2
            000500*OBSOLETE FEATURES THAT ARE USED IN DEBUGGING.                    DB3034.2
            000600 ENVIRONMENT DIVISION.                                            DB3034.2
            000700 CONFIGURATION SECTION.                                           DB3034.2
            000800 SOURCE-COMPUTER.                                                 DB3034.2
            000900     XXXXX082                                                     DB3034.2
            001000     WITH DEBUGGING MODE.                                         DB3034.2
            001100 OBJECT-COMPUTER.                                                 DB3034.2
            001200     XXXXX083.                                                    DB3034.2
            001300 INPUT-OUTPUT SECTION.                                            DB3034.2
            001400 FILE-CONTROL.                                                    DB3034.2
            001500     SELECT TFIL ASSIGN                                           DB3034.2
            001600     XXXXX014                                                     DB3034.2
            001700         ORGANIZATION IS SEQUENTIAL                               DB3034.2
            001800         ACCESS MODE IS SEQUENTIAL.                               DB3034.2
            001900 DATA DIVISION.                                                   DB3034.2
            002000 FILE SECTION.                                                    DB3034.2
            002100 FD TFIL.                                                         DB3034.2
            002200 01 FREC.                                                         DB3034.2
            002300     03 RKEY PIC 9(8).                                            DB3034.2
            002400                                                                  DB3034.2
            002500                                                                  DB3034.2
            002600 PROCEDURE DIVISION.                                              DB3034.2
            002700                                                                  DB3034.2
            002800 DECLARATIVES.                                                    DB3034.2
            002900                                                                  DB3034.2
            003000                                                                  DB3034.2
            003100 BUGGING-2 SECTION.                                               DB3034.2
            003200                                                                  DB3034.2
            003300     USE FOR DEBUGGING ON ALL REFERENCES OF FREC.                 DB3034.2
            003400*Message expected for above statement: OBSOLETE                   DB3034.2
            003500                                                                  DB3034.2
            003600                                                                  DB3034.2
            003700                                                                  DB3034.2
            003800 BUGGING-3 SECTION.                                               DB3034.2
            003900                                                                  DB3034.2
            004000                                                                  DB3034.2
            004100     USE FOR DEBUGGING ON TFIL.                                   DB3034.2
            004200*Message expected for above statement: OBSOLETE                   DB3034.2
            004300                                                                  DB3034.2
            004400 END DECLARATIVES.                                                DB3034.2
            004500                                                                  DB3034.2
            004600 DB303M-FLAGS SECTION.                                            DB3034.2
            004700                                                                  DB3034.2
            004800 DB303M-CONTROL.                                                  DB3034.2
            004900     DISPLAY "THIS IS A DUMMY PROCEDURE".                         DB3034.2
            005000     STOP RUN.                                                    DB3034.2
            005100                                                                  DB3034.2
            005200*TOTAL NUMBER OF FLAGS EXPECTED = 2.                              DB3034.2
                  *END-OF,DB303M                                                            
        """)
    )

    @Test
    fun db3044_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,DB304M                                                      
            000100 IDENTIFICATION DIVISION.                                         DB3044.2
            000200 PROGRAM-ID.                                                      DB3044.2
            000300     DB304M.                                                      DB3044.2
            000400*THE FOLLOWING PROGRAM TESTS THE FLAGGING OF OBSOLETE             DB3044.2
            000500*LEVEL 2 COMMUNICATION FEATURES THAT ARE USED IN DEBUGGING.       DB3044.2
            000600*THIS TEST NEED NOT BE RUN IF COMMUNICATIONS NOT IMPLEMTENTED.    DB3044.2
            000700 ENVIRONMENT DIVISION.                                            DB3044.2
            000800 CONFIGURATION SECTION.                                           DB3044.2
            000900 SOURCE-COMPUTER.                                                 DB3044.2
            001000     XXXXX082                                                     DB3044.2
            001100     WITH DEBUGGING MODE.                                         DB3044.2
            001200 OBJECT-COMPUTER.                                                 DB3044.2
            001300     XXXXX083.                                                    DB3044.2
            001400 DATA DIVISION.                                                   DB3044.2
            001500 COMMUNICATION SECTION.                                           DB3044.2
            001600                                                                  DB3044.2
            001700 CD COMMNAME FOR INPUT.                                           DB3044.2
            001800 01 CREC.                                                         DB3044.2
            001900     03 CNAME1 PIC 9(8).                                          DB3044.2
            002000     03 FILLER PIC X(79).                                         DB3044.2
            002100                                                                  DB3044.2
            002200 PROCEDURE DIVISION.                                              DB3044.2
            002300                                                                  DB3044.2
            002400 DECLARATIVES.                                                    DB3044.2
            002500                                                                  DB3044.2
            002600 BUGGING SECTION.                                                 DB3044.2
            002700                                                                  DB3044.2
            002800     USE FOR DEBUGGING ON COMMNAME.                               DB3044.2
            002900*Message expected for above statement: OBSOLETE                   DB3044.2
            003000                                                                  DB3044.2
            003100                                                                  DB3044.2
            003200                                                                  DB3044.2
            003300 END DECLARATIVES.                                                DB3044.2
            003400                                                                  DB3044.2
            003500 DB304M-FLAGS SECTION.                                            DB3044.2
            003600                                                                  DB3044.2
            003700 DB304M-CONTROL.                                                  DB3044.2
            003800     DISPLAY "THIS IS A DUMMY PROCEDURE".                         DB3044.2
            003900     STOP RUN.                                                    DB3044.2
            004000                                                                  DB3044.2
            004100*TOTAL NUMBER OF FLAGS EXPECTED = 1.                              DB3044.2
                  *END-OF,DB304M                                                            
        """)
    )
}
