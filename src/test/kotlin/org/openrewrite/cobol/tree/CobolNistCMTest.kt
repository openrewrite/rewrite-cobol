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

import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.cobol.Assertions.cobol
import org.openrewrite.cobol.CobolVisitor
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import org.openrewrite.test.RewriteTest.toRecipe

class CobolNistCMTest : RewriteTest {

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

    @Test
    fun cm1014_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM101M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM1014.2
            000200 PROGRAM-ID.                                                      CM1014.2
            000300     CM101M.                                                      CM1014.2
            000400 AUTHOR.                                                          CM1014.2
            000500     FEDERAL COMPILER TESTING CENTER.                             CM1014.2
            000600 INSTALLATION.                                                    CM1014.2
            000700     GENERAL SERVICES ADMINISTRATION                              CM1014.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM1014.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM1014.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               CM1014.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 CM1014.2
            001200                                                                  CM1014.2
            001300     PHONE   (703) 756-6153                                       CM1014.2
            001400                                                                  CM1014.2
            001500     " HIGH       ".                                              CM1014.2
            001600 DATE-WRITTEN.                                                    CM1014.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           CM1014.2
            001800     CREATION DATE     /    VALIDATION DATE                       CM1014.2
            001900     "4.2 ".                                                      CM1014.2
            002000 SECURITY.                                                        CM1014.2
            002100     NONE.                                                        CM1014.2
            002200 ENVIRONMENT DIVISION.                                            CM1014.2
            002300 CONFIGURATION SECTION.                                           CM1014.2
            002400 SOURCE-COMPUTER.                                                 CM1014.2
            002500     XXXXX082.                                                    CM1014.2
            002600 OBJECT-COMPUTER.                                                 CM1014.2
            002700     XXXXX083.                                                    CM1014.2
            002800 INPUT-OUTPUT SECTION.                                            CM1014.2
            002900 FILE-CONTROL.                                                    CM1014.2
            003000     SELECT PRINT-FILE ASSIGN TO                                  CM1014.2
            003100     XXXXX055.                                                    CM1014.2
            003200 DATA DIVISION.                                                   CM1014.2
            003300 FILE SECTION.                                                    CM1014.2
            003400 FD  PRINT-FILE                                                   CM1014.2
            003500     LABEL RECORDS                                                CM1014.2
            003600     XXXXX084                                                     CM1014.2
            003700     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       CM1014.2
            003800 01  PRINT-REC PICTURE X(120).                                    CM1014.2
            003900 01  DUMMY-RECORD PICTURE X(120).                                 CM1014.2
            004000 WORKING-STORAGE SECTION.                                         CM1014.2
            004100 77  PASSWORD1 PIC X(10) VALUE                                    CM1014.2
            004200     XXXXX031.                                                    CM1014.2
            004300 77  DISABLE-STATUS PIC XX VALUE "99".                            CM1014.2
            004400 77  POLL-COUNT PIC 9(8).                                         CM1014.2
            004500 77  INIT-TIME PIC 9(8).                                          CM1014.2
            004600 77  COMP-TIME PIC 9(8).                                          CM1014.2
            004700 01  SYSTEM-TIME.                                                 CM1014.2
            004800     02  SYS-HRS PIC 99.                                          CM1014.2
            004900     02  SYS-MINS PIC 99.                                         CM1014.2
            005000     02  SYS-SECS PIC 99V99.                                      CM1014.2
            005100 01  MSG-TIME.                                                    CM1014.2
            005200     02  HOURS PIC 99.                                            CM1014.2
            005300     02  MINUTES PIC 99.                                          CM1014.2
            005400     02  SECONDS PIC 99V99.                                       CM1014.2
            005500 01  SPEC-LINE-1.                                                 CM1014.2
            005600     02  FILLER PIC X(40) VALUE                                   CM1014.2
            005700     " INITIAL ENABLE RETURNED STATUS CODE OF ".                  CM1014.2
            005800     02  INIT-ENABLE-STATUS PIC XX.                               CM1014.2
            005900 01  INCOMING-MSG.                                                CM1014.2
            006000     02  KILL-FIELD PIC X(4).                                     CM1014.2
            006100     02  FILLER PIC X(68).                                        CM1014.2
            006200 01  LOG-HDR-1.                                                   CM1014.2
            006300     02  FILLER PIC X(48) VALUE SPACES.                           CM1014.2
            006400     02  FILLER PIC X(24) VALUE "LOG OF INCOMING MESSAGES".       CM1014.2
            006500 01  LOG-HDR-2.                                                   CM1014.2
            006600     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            006700     02  FILLER PIC X(19) VALUE "SYMBOLIC SOURCE IS ".            CM1014.2
            006800     02  SYM-SOURCE PIC X(25).                                    CM1014.2
            006900     02  FILLER PIC X(16) VALUE "MESSAGE DATE IS ".               CM1014.2
            007000     02  MSG-DATE PIC 9(6) VALUE ZERO.                            CM1014.2
            007100 01  LOG-HDR-3.                                                   CM1014.2
            007200     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            007300     02  FILLER PIC X(12) VALUE "TIME  RECVD".                    CM1014.2
            007400     02  FILLER PIC X(9) VALUE "LOG LAG".                         CM1014.2
            007500     02  FILLER PIC X(7) VALUE "LENGTH".                          CM1014.2
            007600     02  FILLER PIC X(4) VALUE "END".                             CM1014.2
            007700     02  FILLER PIC X(3) VALUE "QD".                              CM1014.2
            007800     02  FILLER PIC X(40) VALUE "POLL COUNT".                     CM1014.2
            007900     02  FILLER PIC X(16) VALUE "MESSAGE CONTENTS".               CM1014.2
            008000 01  LOG-HDR-4.                                                   CM1014.2
            008100     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            008200     02  FILLER PIC X(11) VALUE ALL "-".                          CM1014.2
            008300     02  FILLER PIC X VALUE SPACES.                               CM1014.2
            008400     02  FILLER PIC X(7) VALUE ALL "-".                           CM1014.2
            008500     02  FILLER PIC X(2) VALUE SPACES.                            CM1014.2
            008600     02  FILLER PIC X(6) VALUE ALL "-".                           CM1014.2
            008700     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            008800     02  FILLER PIC XXX VALUE "---".                              CM1014.2
            008900     02  FILLER PIC X VALUE SPACES.                               CM1014.2
            009000     02  FILLER PIC XX VALUE "--".                                CM1014.2
            009100     02  FILLER PIC X VALUE SPACES.                               CM1014.2
            009200     02  FILLER PIC X(10) VALUE ALL "-".                          CM1014.2
            009300     02  FILLER PIC X(2) VALUE SPACES.                            CM1014.2
            009400     02  FILLER PIC X(72) VALUE ALL "-".                          CM1014.2
            009500 01  LOG-LINE.                                                    CM1014.2
            009600     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            009700     02  RECEIPT-TIME.                                            CM1014.2
            009800         03  HOURS PIC 99.                                        CM1014.2
            009900         03  FILLER PIC X VALUE ":".                              CM1014.2
            010000         03  MINUTES PIC 99.                                      CM1014.2
            010100         03  FILLER PIC X VALUE ":".                              CM1014.2
            010200         03  SECONDS PIC 99.99.                                   CM1014.2
            010300     02  LAG-TIME PIC -(4)9.99.                                   CM1014.2
            010400     02  FILLER PIC XX VALUE SPACES.                              CM1014.2
            010500     02  MESSAGE-LENGTH PIC ZZZ9.                                 CM1014.2
            010600     02  FILLER PIC XXX VALUE SPACES.                             CM1014.2
            010700     02  SENTINEL PIC XXX.                                        CM1014.2
            010800     02  FILLER PIC X VALUE SPACES.                               CM1014.2
            010900     02  QUEUE-DEPTH PIC 99.                                      CM1014.2
            011000     02  FILLER PIC X VALUE SPACES.                               CM1014.2
            011100     02  IDLE-COUNT PIC ZZ,ZZZ,ZZ9.                               CM1014.2
            011200     02  IDLE-OVERFLOW REDEFINES IDLE-COUNT PIC X(10).            CM1014.2
            011300     02  FILLER PIC XX VALUE SPACES.                              CM1014.2
            011400     02  MSG PIC X(72).                                           CM1014.2
            011500     66  LONG-NARRATIVE RENAMES LAG-TIME THRU MSG.                CM1014.2
            011600     66  SHORT-NARRATIVE RENAMES IDLE-COUNT THRU MSG.             CM1014.2
            011700 01  TEST-RESULTS.                                                CM1014.2
            011800     02 FILLER                    PICTURE X VALUE SPACE.          CM1014.2
            011900     02 FEATURE                   PICTURE X(18).                  CM1014.2
            012000     02 FILLER                    PICTURE X VALUE SPACE.          CM1014.2
            012100     02 P-OR-F                    PICTURE X(5).                   CM1014.2
            012200     02 FILLER                    PICTURE X  VALUE SPACE.         CM1014.2
            012300     02  PAR-NAME PIC X(20).                                      CM1014.2
            012400     02 FILLER                    PICTURE X VALUE SPACE.          CM1014.2
            012500     02  COMPUTED-A.                                              CM1014.2
            012600         03  FILLER PIC X(9) VALUE SPACES.                        CM1014.2
            012700         03  COMPUTED-STATUS PIC XX.                              CM1014.2
            012800         03  FILLER PIC X(9) VALUE SPACES.                        CM1014.2
            012900     02 FILLER                    PICTURE X VALUE SPACE.          CM1014.2
            013000     02  CORRECT-A.                                               CM1014.2
            013100         03  FILLER PIC X(9) VALUE SPACES.                        CM1014.2
            013200         03  CORRECT-STATUS PIC XX.                               CM1014.2
            013300         03  FILLER PIC X(9) VALUE SPACES.                        CM1014.2
            013400     02 FILLER                    PICTURE X VALUE SPACE.          CM1014.2
            013500     02 RE-MARK                   PICTURE X(30).                  CM1014.2
            013600 01  COLUMNS-LINE-1.                                              CM1014.2
            013700     02  FILLER PIC X(3) VALUE SPACES.                            CM1014.2
            013800     02  FILLER PIC X(17) VALUE "FEATURE TESTED".                 CM1014.2
            013900     02  FILLER PIC X(9) VALUE "RESLT".                           CM1014.2
            014000     02  FILLER PIC X(21) VALUE "PARAGRAPH NAME".                 CM1014.2
            014100     02  FILLER PIC X(22) VALUE "COMPUTED DATA".                  CM1014.2
            014200     02  FILLER PIC X(29) VALUE "CORRECT DATA".                   CM1014.2
            014300     02  FILLER PIC X(7) VALUE "REMARKS".                         CM1014.2
            014400 01  COLUMNS-LINE-2.                                              CM1014.2
            014500     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            014600     02  FILLER PIC X(18) VALUE ALL "-".                          CM1014.2
            014700     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            014800     02  FILLER PIC X(5) VALUE ALL "-".                           CM1014.2
            014900     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            015000     02  FILLER PIC X(20) VALUE ALL "-".                          CM1014.2
            015100     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            015200     02  FILLER PIC X(20) VALUE ALL "-".                          CM1014.2
            015300     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            015400     02  FILLER PIC X(20) VALUE ALL "-".                          CM1014.2
            015500     02  FILLER PIC X VALUE SPACE.                                CM1014.2
            015600     02  FILLER PIC X(31) VALUE ALL "-".                          CM1014.2
            015700 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         CM1014.2
            015800 01  REC-CT PICTURE 99 VALUE ZERO.                                CM1014.2
            015900 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        CM1014.2
            016000 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  CM1014.2
            016100 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          CM1014.2
            016200 01  PASS-COUNTER PIC 999 VALUE ZERO.                             CM1014.2
            016300 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              CM1014.2
            016400 01  ERROR-HOLD PIC 999 VALUE ZERO.                               CM1014.2
            016500 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           CM1014.2
            016600 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            CM1014.2
            016700 01  CCVS-H-1.                                                    CM1014.2
            016800     02  FILLER   PICTURE X(27)  VALUE SPACE.                     CM1014.2
            016900     02 FILLER PICTURE X(67) VALUE                                CM1014.2
            017000     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  CM1014.2
            017100-    " SYSTEM".                                                   CM1014.2
            017200     02  FILLER     PICTURE X(26)  VALUE SPACE.                   CM1014.2
            017300 01  CCVS-H-2.                                                    CM1014.2
            017400     02 FILLER PICTURE X(52) VALUE IS                             CM1014.2
            017500     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   CM1014.2
            017600     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   CM1014.2
            017700     02 TEST-ID PICTURE IS X(9).                                  CM1014.2
            017800     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   CM1014.2
            017900 01  CCVS-H-3.                                                    CM1014.2
            018000     02  FILLER PICTURE X(34) VALUE                               CM1014.2
            018100     " FOR OFFICIAL USE ONLY    ".                                CM1014.2
            018200     02  FILLER PICTURE X(58) VALUE                               CM1014.2
            018300     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".CM1014.2
            018400     02  FILLER PICTURE X(28) VALUE                               CM1014.2
            018500     "  COPYRIGHT   1974 ".                                       CM1014.2
            018600 01  CCVS-E-1.                                                    CM1014.2
            018700     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   CM1014.2
            018800     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        CM1014.2
            018900     02 ID-AGAIN PICTURE IS X(9).                                 CM1014.2
            019000     02 FILLER PICTURE X(45) VALUE IS                             CM1014.2
            019100     " NTIS DISTRIBUTION COBOL 74".                               CM1014.2
            019200 01  CCVS-E-2.                                                    CM1014.2
            019300     02  FILLER                   PICTURE X(31)  VALUE            CM1014.2
            019400     SPACE.                                                       CM1014.2
            019500     02  FILLER                   PICTURE X(21)  VALUE SPACE.     CM1014.2
            019600     02 CCVS-E-2-2.                                               CM1014.2
            019700         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            CM1014.2
            019800         03 FILLER PICTURE IS X VALUE IS SPACE.                   CM1014.2
            019900         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      CM1014.2
            020000 01  CCVS-E-3.                                                    CM1014.2
            020100     02  FILLER PICTURE X(22) VALUE                               CM1014.2
            020200     " FOR OFFICIAL USE ONLY".                                    CM1014.2
            020300     02  FILLER PICTURE X(12) VALUE SPACE.                        CM1014.2
            020400     02  FILLER PICTURE X(58) VALUE                               CM1014.2
            020500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".CM1014.2
            020600     02  FILLER PICTURE X(13) VALUE SPACE.                        CM1014.2
            020700     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 CM1014.2
            020800 01  CCVS-E-4.                                                    CM1014.2
            020900     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           CM1014.2
            021000     02 FILLER PIC XXXX VALUE " OF ".                             CM1014.2
            021100     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           CM1014.2
            021200     02 FILLER PIC X(40) VALUE                                    CM1014.2
            021300      "  TESTS WERE EXECUTED SUCCESSFULLY".                       CM1014.2
            021400 01  XXINFO.                                                      CM1014.2
            021500     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    CM1014.2
            021600     02 INFO-TEXT.                                                CM1014.2
            021700     04 FILLER PIC X(20) VALUE SPACE.                             CM1014.2
            021800     04 XXCOMPUTED PIC X(20).                                     CM1014.2
            021900     04 FILLER PIC X(5) VALUE SPACE.                              CM1014.2
            022000     04 XXCORRECT PIC X(20).                                      CM1014.2
            022100 01  HYPHEN-LINE.                                                 CM1014.2
            022200     02 FILLER PICTURE IS X VALUE IS SPACE.                       CM1014.2
            022300     02 FILLER PICTURE IS X(65) VALUE IS "************************CM1014.2
            022400-    "*****************************************".                 CM1014.2
            022500     02 FILLER PICTURE IS X(54) VALUE IS "************************CM1014.2
            022600-    "******************************".                            CM1014.2
            022700 01  CCVS-PGM-ID PIC X(6) VALUE                                   CM1014.2
            022800     "CM101M".                                                    CM1014.2
            022900 COMMUNICATION SECTION.                                           CM1014.2
            023000 CD  CM-INQUE-1 FOR INPUT                                         CM1014.2
            023100     SYMBOLIC QUEUE IS MAIN-QUEUE                                 CM1014.2
            023200     SYMBOLIC SUB-QUEUE-1 IS NO-SPEC-1                            CM1014.2
            023300     SYMBOLIC SUB-QUEUE-2 IS NO-SPEC-2                            CM1014.2
            023400     SYMBOLIC SUB-QUEUE-3 IS NO-SPEC-3                            CM1014.2
            023500     MESSAGE DATE IS DATE-RECEIVED                                CM1014.2
            023600     MESSAGE TIME IS TIME-RECEIVED                                CM1014.2
            023700     SYMBOLIC SOURCE IS WHERE-FROM                                CM1014.2
            023800     TEXT LENGTH IS MSG-LENGTH                                    CM1014.2
            023900     END KEY IS END-KEY                                           CM1014.2
            024000     STATUS KEY IS STATUS-KEY                                     CM1014.2
            024100     MESSAGE COUNT IS MSG-COUNT.                                  CM1014.2
            024200 PROCEDURE    DIVISION.                                           CM1014.2
            024300 SECT-CM101M-0001 SECTION.                                        CM1014.2
            024400 CM101M-INIT.                                                     CM1014.2
            024500     OPEN     OUTPUT PRINT-FILE.                                  CM1014.2
            024600     MOVE "CM101M     " TO TEST-ID.                               CM1014.2
            024700     MOVE     TEST-ID TO ID-AGAIN.                                CM1014.2
            024800     MOVE    SPACE TO TEST-RESULTS.                               CM1014.2
            024900     PERFORM HEAD-ROUTINE.                                        CM1014.2
            025000     MOVE                                                         CM1014.2
            025100     XXXXX030                                                     CM1014.2
            025200     TO MAIN-QUEUE.                                               CM1014.2
            025300     MOVE SPACES TO NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.                CM1014.2
            025400     ENABLE INPUT  CM-INQUE-1 WITH KEY                            CM1014.2
            025500     XXXXX031.                                                    CM1014.2
            025600     MOVE STATUS-KEY TO INIT-ENABLE-STATUS.                       CM1014.2
            025700     MOVE SPEC-LINE-1 TO PRINT-REC.                               CM1014.2
            025800     WRITE PRINT-REC                                              CM1014.2
            025900         AFTER 2 LINES.                                           CM1014.2
            026000     MOVE HYPHEN-LINE TO PRINT-REC.                               CM1014.2
            026100     WRITE PRINT-REC                                              CM1014.2
            026200         AFTER 2 LINES.                                           CM1014.2
            026300 LOG-INIT.                                                        CM1014.2
            026400     MOVE ZERO TO POLL-COUNT.                                     CM1014.2
            026500     MOVE ALL "*" TO MSG.                                         CM1014.2
            026600 LOG-MSG.                                                         CM1014.2
            026700     MOVE   SPACES TO INCOMING-MSG.                               CM1014.2
            026800     RECEIVE CM-INQUE-1 MESSAGE INTO INCOMING-MSG                 CM1014.2
            026900         NO DATA PERFORM INCREMENT-POLL-COUNT GO TO LOG-MSG.      CM1014.2
            027000     ACCEPT SYSTEM-TIME FROM TIME.                                CM1014.2
            027100     ACCEPT CM-INQUE-1 MESSAGE COUNT.                             CM1014.2
            027200     IF STATUS-KEY IS NOT EQUAL TO ZERO                           CM1014.2
            027300         DISPLAY "RUN ABORTED - STATUS KEY WAS " STATUS-KEY       CM1014.2
            027400         STOP RUN.                                                CM1014.2
            027500     IF MSG-DATE IS EQUAL TO ZERO PERFORM LOG-HEADER.             CM1014.2
            027600     IF KILL-FIELD IS EQUAL TO "KILL"                             CM1014.2
            027700         ACCEPT INIT-TIME FROM TIME                               CM1014.2
            027800         DISABLE INPUT CM-INQUE-1 WITH KEY                        CM1014.2
            027900     XXXXX031                                                     CM1014.2
            028000         ACCEPT COMP-TIME FROM TIME                               CM1014.2
            028100         MOVE STATUS-KEY TO DISABLE-STATUS.                       CM1014.2
            028200     MOVE TIME-RECEIVED TO MSG-TIME.                              CM1014.2
            028300     MOVE CORR MSG-TIME TO RECEIPT-TIME.                          CM1014.2
            028400     COMPUTE LAG-TIME =                                           CM1014.2
            028500         ((SYS-HRS * 3600) + (SYS-MINS * 60) + SYS-SECS) -        CM1014.2
            028600         ((HOURS OF MSG-TIME * 3600) + (MINUTES OF MSG-TIME * 60) CM1014.2
            028700         + SECONDS OF MSG-TIME).                                  CM1014.2
            028800     IF END-KEY IS EQUAL TO "3"                                   CM1014.2
            028900         MOVE "EGI" TO SENTINEL                                   CM1014.2
            029000         ELSE IF END-KEY IS EQUAL TO "2"                          CM1014.2
            029100         MOVE "EMI" TO SENTINEL                                   CM1014.2
            029200             ELSE MOVE END-KEY TO SENTINEL.                       CM1014.2
            029300     MOVE MSG-COUNT TO QUEUE-DEPTH.                               CM1014.2
            029400     MOVE MSG-LENGTH TO MESSAGE-LENGTH.                           CM1014.2
            029500     IF POLL-COUNT IS EQUAL TO 99999999                           CM1014.2
            029600         MOVE " OVERFLOW " TO IDLE-OVERFLOW                       CM1014.2
            029700         ELSE MOVE POLL-COUNT TO IDLE-COUNT.                      CM1014.2
            029800     MOVE INCOMING-MSG TO MSG.                                    CM1014.2
            029900     MOVE LOG-LINE TO PRINT-REC.                                  CM1014.2
            030000     PERFORM WRITE-LINE.                                          CM1014.2
            030100 LOG-MSG-01.                                                      CM1014.2
            030200     IF KILL-FIELD IS EQUAL TO "WAIT"                             CM1014.2
            030300         PERFORM GET-INITIAL-TIME                                 CM1014.2
            030400         PERFORM DELAY-FOR-30-SECS                                CM1014.2
            030500         GO TO LOG-INIT.                                          CM1014.2
            030600     IF KILL-FIELD IS NOT EQUAL TO "KILL" GO TO LOG-INIT.         CM1014.2
            030700 DISABLE-CM-INQUE-1.                                              CM1014.2
            030800     MOVE INIT-TIME TO MSG-TIME.                                  CM1014.2
            030900     MOVE CORR MSG-TIME TO RECEIPT-TIME.                          CM1014.2
            031000     MOVE "-DISABLE COMMAND INITIATED FROM PROGRAM"               CM1014.2
            031100         TO LONG-NARRATIVE.                                       CM1014.2
            031200     MOVE LOG-LINE TO PRINT-REC.                                  CM1014.2
            031300     WRITE PRINT-REC                                              CM1014.2
            031400         AFTER 2 LINES.                                           CM1014.2
            031500     MOVE COMP-TIME TO MSG-TIME.                                  CM1014.2
            031600     MOVE CORR MSG-TIME TO RECEIPT-TIME.                          CM1014.2
            031700     MOVE "- STATUS CODE OF" TO LONG-NARRATIVE.                   CM1014.2
            031800     MOVE DISABLE-STATUS TO QUEUE-DEPTH.                          CM1014.2
            031900     MOVE "AND EXECUTION CONTROL RETURNED FROM MCS"               CM1014.2
            032000         TO SHORT-NARRATIVE.                                      CM1014.2
            032100     MOVE LOG-LINE TO PRINT-REC.                                  CM1014.2
            032200     PERFORM WRITE-LINE.                                          CM1014.2
            032300     MOVE SPACES TO PRINT-REC.                                    CM1014.2
            032400     PERFORM WRITE-LINE.                                          CM1014.2
            032500 GET-INITIAL-TIME.                                                CM1014.2
            032600     ACCEPT SYSTEM-TIME FROM TIME.                                CM1014.2
            032700     COMPUTE INIT-TIME =                                          CM1014.2
            032800         SYS-HRS * 3600 + SYS-MINS * 60 + SYS-SECS.               CM1014.2
            032900 LOOK-FOR-LATE-TRANSMISSIONS.                                     CM1014.2
            033000     ACCEPT CM-INQUE-1 MESSAGE COUNT.                             CM1014.2
            033100     IF MSG-COUNT IS NOT EQUAL TO ZERO                            CM1014.2
            033200         PERFORM LOG-INIT THRU LOG-MSG                            CM1014.2
            033300         GO TO LOOK-FOR-LATE-TRANSMISSIONS.                       CM1014.2
            033400 GET-TIME-DIFFERENCE.                                             CM1014.2
            033500     ACCEPT SYSTEM-TIME FROM TIME.                                CM1014.2
            033600     COMPUTE COMP-TIME =                                          CM1014.2
            033700         SYS-HRS * 3600 + SYS-MINS * 60 + SYS-SECS - INIT-TIME.   CM1014.2
            033800 CHECK-FOR-15.                                                    CM1014.2
            033900     IF COMP-TIME IS LESS THAN 15                                 CM1014.2
            034000         GO TO LOOK-FOR-LATE-TRANSMISSIONS.                       CM1014.2
            034100 15-SECONDS-HAVE-ELAPSED.                                         CM1014.2
            034200     MOVE HYPHEN-LINE TO PRINT-REC.                               CM1014.2
            034300     WRITE PRINT-REC                                              CM1014.2
            034400         AFTER 2 LINES.                                           CM1014.2
            034500     MOVE HYPHEN-LINE TO PRINT-REC.                               CM1014.2
            034600     PERFORM WRITE-LINE.                                          CM1014.2
            034700 STATUS-TESTS-INIT.                                               CM1014.2
            034800     MOVE " BEGIN INPUT STATUS TESTS" TO PRINT-REC.               CM1014.2
            034900     WRITE PRINT-REC                                              CM1014.2
            035000         AFTER 2 LINES.                                           CM1014.2
            035100     MOVE COLUMNS-LINE-1 TO PRINT-REC.                            CM1014.2
            035200     WRITE PRINT-REC                                              CM1014.2
            035300         AFTER 2 LINES.                                           CM1014.2
            035400     MOVE COLUMNS-LINE-2 TO PRINT-REC.                            CM1014.2
            035500     PERFORM WRITE-LINE.                                          CM1014.2
            035600     MOVE SPACES TO PRINT-REC.                                    CM1014.2
            035700     PERFORM WRITE-LINE.                                          CM1014.2
            035800     MOVE "MCS STATUS WORD" TO FEATURE.                           CM1014.2
            035900 REC-STATUS-TEST-01.                                              CM1014.2
            036000     MOVE "QUEUE NAME NOT SPECIFIED" TO RE-MARK.                  CM1014.2
            036100     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            036200     MOVE SPACES TO MAIN-QUEUE NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.     CM1014.2
            036300     RECEIVE CM-INQUE-1 MESSAGE INTO INCOMING-MSG                 CM1014.2
            036400         NO DATA ADD 0 TO POLL-COUNT.                             CM1014.2
            036500     IF STATUS-KEY IS EQUAL TO "20"                               CM1014.2
            036600         PERFORM PASS GO TO REC-STATUS-WRITE-01.                  CM1014.2
            036700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            036800     MOVE "20" TO CORRECT-STATUS.                                 CM1014.2
            036900     PERFORM FAIL.                                                CM1014.2
            037000     GO TO REC-STATUS-WRITE-01.                                   CM1014.2
            037100 REC-STATUS-DELETE-01.                                            CM1014.2
            037200     PERFORM DE-LETE.                                             CM1014.2
            037300 REC-STATUS-WRITE-01.                                             CM1014.2
            037400     MOVE "REC-STATUS-TEST-01" TO PAR-NAME.                       CM1014.2
            037500     PERFORM PRINT-DETAIL.                                        CM1014.2
            037600 REC-STATUS-TEST-02.                                              CM1014.2
            037700     MOVE "UNKNOWN SUB-QUEUE-1 SPECIFIED" TO RE-MARK.             CM1014.2
            037800     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            037900     MOVE                                                         CM1014.2
            038000     XXXXX030                                                     CM1014.2
            038100         TO MAIN-QUEUE.                                           CM1014.2
            038200     MOVE "DUMMYNAME" TO NO-SPEC-1.                               CM1014.2
            038300     MOVE SPACES TO NO-SPEC-2 NO-SPEC-3.                          CM1014.2
            038400     RECEIVE CM-INQUE-1 MESSAGE INTO INCOMING-MSG                 CM1014.2
            038500         NO DATA ADD 0 TO POLL-COUNT.                             CM1014.2
            038600     IF STATUS-KEY IS EQUAL TO "20"                               CM1014.2
            038700         PERFORM PASS GO TO REC-STATUS-WRITE-02.                  CM1014.2
            038800     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            038900     MOVE "20" TO CORRECT-STATUS.                                 CM1014.2
            039000     PERFORM FAIL.                                                CM1014.2
            039100     GO TO REC-STATUS-WRITE-02.                                   CM1014.2
            039200 REC-STATUS-DELETE-02.                                            CM1014.2
            039300     PERFORM DE-LETE.                                             CM1014.2
            039400 REC-STATUS-WRITE-02.                                             CM1014.2
            039500     MOVE "REC-STATUS-TEST-02" TO PAR-NAME.                       CM1014.2
            039600     PERFORM PRINT-DETAIL.                                        CM1014.2
            039700 ACCPT-STATUS-TEST-01.                                            CM1014.2
            039800     MOVE "QUEUE NAME NOT SPECIFIED" TO RE-MARK.                  CM1014.2
            039900     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            040000     MOVE SPACES TO MAIN-QUEUE NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.     CM1014.2
            040100     ACCEPT CM-INQUE-1 MESSAGE COUNT.                             CM1014.2
            040200     IF STATUS-KEY IS EQUAL TO "20"                               CM1014.2
            040300         PERFORM PASS GO TO ACCPT-STATUS-WRITE-01.                CM1014.2
            040400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            040500     MOVE "20" TO CORRECT-STATUS.                                 CM1014.2
            040600     PERFORM FAIL.                                                CM1014.2
            040700     GO TO ACCPT-STATUS-WRITE-01.                                 CM1014.2
            040800 ACCPT-STATUS-DELETE-01.                                          CM1014.2
            040900     PERFORM DE-LETE.                                             CM1014.2
            041000 ACCPT-STATUS-WRITE-01.                                           CM1014.2
            041100     MOVE "ACCPT-STATUS-TEST-01" TO PAR-NAME.                     CM1014.2
            041200     PERFORM PRINT-DETAIL.                                        CM1014.2
            041300 ACCPT-STATUS-TEST-02.                                            CM1014.2
            041400     MOVE "UNKNOWN SUB-QUEUE-1 SPECIFIED" TO RE-MARK.             CM1014.2
            041500     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            041600     MOVE                                                         CM1014.2
            041700     XXXXX030                                                     CM1014.2
            041800         TO MAIN-QUEUE.                                           CM1014.2
            041900     MOVE "DUMMYNAME" TO NO-SPEC-1.                               CM1014.2
            042000     MOVE SPACES TO NO-SPEC-2 NO-SPEC-3.                          CM1014.2
            042100     ACCEPT CM-INQUE-1 COUNT.                                     CM1014.2
            042200     IF STATUS-KEY IS EQUAL TO "20"                               CM1014.2
            042300     PERFORM PASS GO TO ACCPT-STATUS-WRITE-02.                    CM1014.2
            042400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            042500     MOVE "20" TO CORRECT-STATUS.                                 CM1014.2
            042600     PERFORM FAIL.                                                CM1014.2
            042700      GO TO ACCPT-STATUS-WRITE-02.                                CM1014.2
            042800 ACCPT-STATUS-DELETE-02.                                          CM1014.2
            042900     PERFORM DE-LETE.                                             CM1014.2
            043000 ACCPT-STATUS-WRITE-02.                                           CM1014.2
            043100     MOVE "ACCPT-STATUS-TEST-02" TO PAR-NAME.                     CM1014.2
            043200     PERFORM PRINT-DETAIL.                                        CM1014.2
            043300 ENABL-STATUS-TEST-01.                                            CM1014.2
            043400     MOVE "QUEUE NAME NOT SPECIFIED" TO RE-MARK.                  CM1014.2
            043500     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            043600     MOVE SPACES TO MAIN-QUEUE NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.     CM1014.2
            043700     ENABLE INPUT CM-INQUE-1 WITH KEY                             CM1014.2
            043800     XXXXX031                                                     CM1014.2
            043900     IF STATUS-KEY IS EQUAL TO "20"                               CM1014.2
            044000         PERFORM PASS GO TO ENABL-STATUS-WRITE-01.                CM1014.2
            044100     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            044200     MOVE "20" TO CORRECT-STATUS.                                 CM1014.2
            044300     PERFORM FAIL.                                                CM1014.2
            044400     GO TO ENABL-STATUS-WRITE-01.                                 CM1014.2
            044500 ENABL-STATUS-DELETE-01.                                          CM1014.2
            044600     PERFORM DE-LETE.                                             CM1014.2
            044700 ENABL-STATUS-WRITE-01.                                           CM1014.2
            044800     MOVE "ENABL-STATUS-TEST-01" TO PAR-NAME.                     CM1014.2
            044900     PERFORM PRINT-DETAIL.                                        CM1014.2
            045000 ENABL-STATUS-TEST-02.                                            CM1014.2
            045100     MOVE "UNKNOWN SUB-QUEUE-1 SPECIFIED" TO RE-MARK.             CM1014.2
            045200     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            045300     MOVE                                                         CM1014.2
            045400     XXXXX030                                                     CM1014.2
            045500         TO MAIN-QUEUE.                                           CM1014.2
            045600     MOVE "DUMMYNAME" TO NO-SPEC-1.                               CM1014.2
            045700     MOVE SPACES TO NO-SPEC-2 NO-SPEC-3.                          CM1014.2
            045800     ENABLE INPUT CM-INQUE-1 KEY                                  CM1014.2
            045900         PASSWORD1.                                               CM1014.2
            046000     IF STATUS-KEY IS EQUAL TO "20"                               CM1014.2
            046100         PERFORM PASS GO TO ENABL-STATUS-WRITE-02.                CM1014.2
            046200     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            046300     MOVE "20" TO CORRECT-STATUS.                                 CM1014.2
            046400     PERFORM FAIL.                                                CM1014.2
            046500     GO TO ENABL-STATUS-WRITE-02.                                 CM1014.2
            046600 ENABL-STATUS-DELETE-02.                                          CM1014.2
            046700     PERFORM DE-LETE.                                             CM1014.2
            046800 ENABL-STATUS-WRITE-02.                                           CM1014.2
            046900     MOVE "ENABL-STATUS-TEST-02" TO PAR-NAME.                     CM1014.2
            047000     PERFORM PRINT-DETAIL.                                        CM1014.2
            047100 ENABL-STATUS-TEST-03.                                            CM1014.2
            047200     MOVE "INVALID PASSWORD USED" TO RE-MARK.                     CM1014.2
            047300     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            047400     MOVE                                                         CM1014.2
            047500     XXXXX030                                                     CM1014.2
            047600         TO MAIN-QUEUE.                                           CM1014.2
            047700     MOVE SPACES TO NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.                CM1014.2
            047800     ENABLE INPUT CM-INQUE-1 WITH KEY "LETMEIN".                  CM1014.2
            047900     IF STATUS-KEY IS EQUAL TO "40"                               CM1014.2
            048000         PERFORM PASS GO TO ENABL-STATUS-WRITE-03.                CM1014.2
            048100     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            048200     MOVE "40" TO CORRECT-STATUS.                                 CM1014.2
            048300     PERFORM FAIL.                                                CM1014.2
            048400     GO TO ENABL-STATUS-WRITE-03.                                 CM1014.2
            048500 ENABL-STATUS-DELETE-03.                                          CM1014.2
            048600     PERFORM DE-LETE.                                             CM1014.2
            048700 ENABL-STATUS-WRITE-03.                                           CM1014.2
            048800     MOVE "ENABL-STATUS-TEST-03" TO PAR-NAME.                     CM1014.2
            048900     PERFORM PRINT-DETAIL.                                        CM1014.2
            049000 ENABL-STATUS-TEST-04.                                            CM1014.2
            049100     MOVE "NO QUEUE NAME / WRONG PASSWORD" TO RE-MARK.            CM1014.2
            049200     MOVE  "99" TO STATUS-KEY.                                    CM1014.2
            049300     MOVE SPACES TO MAIN-QUEUE NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.     CM1014.2
            049400     ENABLE INPUT CM-INQUE-1 WITH KEY                             CM1014.2
            049500         "LETMEIN".                                               CM1014.2
            049600     MOVE "INFO" TO P-OR-F.                                       CM1014.2
            049700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            049800     MOVE "  INFO TEST FOR BOTH" TO CORRECT-A.                    CM1014.2
            049900     GO TO ENABL-STATUS-WRITE-04.                                 CM1014.2
            050000 ENABL-STATUS-DELETE-04.                                          CM1014.2
            050100     PERFORM DE-LETE.                                             CM1014.2
            050200 ENABL-STATUS-WRITE-04.                                           CM1014.2
            050300     MOVE "ENABL-STATUS-TEST-04" TO PAR-NAME.                     CM1014.2
            050400     PERFORM PRINT-DETAIL.                                        CM1014.2
            050500 DISAB-STATUS-TEST-01.                                            CM1014.2
            050600     MOVE "QUEUE NAME NOT SPECIFIED" TO RE-MARK.                  CM1014.2
            050700     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            050800     MOVE SPACES TO MAIN-QUEUE NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.     CM1014.2
            050900     DISABLE INPUT CM-INQUE-1 WITH KEY                            CM1014.2
            051000     XXXXX031                                                     CM1014.2
            051100     IF STATUS-KEY IS EQUAL TO "20"                               CM1014.2
            051200         PERFORM PASS GO TO DISAB-STATUS-WRITE-01.                CM1014.2
            051300     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            051400     MOVE "20" TO CORRECT-STATUS.                                 CM1014.2
            051500     PERFORM FAIL.                                                CM1014.2
            051600     GO TO DISAB-STATUS-WRITE-01.                                 CM1014.2
            051700 DISAB-STATUS-DELETE-01.                                          CM1014.2
            051800     PERFORM DE-LETE.                                             CM1014.2
            051900 DISAB-STATUS-WRITE-01.                                           CM1014.2
            052000     MOVE "DISAB-STATUS-TEST-01" TO PAR-NAME.                     CM1014.2
            052100     PERFORM PRINT-DETAIL.                                        CM1014.2
            052200 DISAB-STATUS-TEST-02.                                            CM1014.2
            052300     MOVE "UNKNOWN SUB-QUEUE-1 SPECIFIED" TO RE-MARK.             CM1014.2
            052400     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            052500     MOVE                                                         CM1014.2
            052600     XXXXX030                                                     CM1014.2
            052700         TO MAIN-QUEUE.                                           CM1014.2
            052800     MOVE "DUMMYNAME" TO NO-SPEC-1.                               CM1014.2
            052900     MOVE SPACES TO NO-SPEC-2 NO-SPEC-3.                          CM1014.2
            053000     DISABLE INPUT CM-INQUE-1 WITH KEY                            CM1014.2
            053100     PASSWORD1.                                                   CM1014.2
            053200     IF STATUS-KEY IS EQUAL TO "20"                               CM1014.2
            053300         PERFORM PASS GO TO DISAB-STATUS-WRITE-02.                CM1014.2
            053400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            053500     MOVE "20" TO CORRECT-STATUS.                                 CM1014.2
            053600     PERFORM FAIL.                                                CM1014.2
            053700      GO TO DISAB-STATUS-WRITE-02.                                CM1014.2
            053800 DISAB-STATUS-DELETE-02.                                          CM1014.2
            053900     PERFORM DE-LETE.                                             CM1014.2
            054000 DISAB-STATUS-WRITE-02.                                           CM1014.2
            054100     MOVE "DISAB-STATUS-TEST-02" TO PAR-NAME.                     CM1014.2
            054200     PERFORM PRINT-DETAIL.                                        CM1014.2
            054300 DISAB-STATUS-TEST-03.                                            CM1014.2
            054400     MOVE "INVALID PASSWORD USED" TO RE-MARK.                     CM1014.2
            054500     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            054600     MOVE                                                         CM1014.2
            054700     XXXXX030                                                     CM1014.2
            054800         TO MAIN-QUEUE.                                           CM1014.2
            054900     MOVE SPACES TO NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.                CM1014.2
            055000     DISABLE INPUT CM-INQUE-1 WITH KEY                            CM1014.2
            055100     "KILLITNOW".                                                 CM1014.2
            055200     IF STATUS-KEY IS EQUAL TO "40"                               CM1014.2
            055300         PERFORM PASS GO TO DISAB-STATUS-WRITE-03.                CM1014.2
            055400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1014.2
            055500     MOVE "40" TO CORRECT-STATUS.                                 CM1014.2
            055600     PERFORM FAIL.                                                CM1014.2
            055700     GO TO DISAB-STATUS-WRITE-03.                                 CM1014.2
            055800 DISAB-STATUS-DELETE-03.                                          CM1014.2
            055900     PERFORM DE-LETE.                                             CM1014.2
            056000 DISAB-STATUS-WRITE-03.                                           CM1014.2
            056100     MOVE "DISAB-STATUS-TEST-03" TO PAR-NAME.                     CM1014.2
            056200     PERFORM PRINT-DETAIL.                                        CM1014.2
            056300 RENAB-STATUS-TEST-01.                                            CM1014.2
            056400     MOVE "RE-ENABLE PREVIOUSLY DISABLED" TO RE-MARK.             CM1014.2
            056500     MOVE "99" TO STATUS-KEY.                                     CM1014.2
            056600     MOVE                                                         CM1014.2
            056700     XXXXX030                                                     CM1014.2
            056800         TO MAIN-QUEUE.                                           CM1014.2
            056900     MOVE SPACES TO NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.                CM1014.2
            057000     ENABLE INPUT CM-INQUE-1 KEY                                  CM1014.2
            057100     PASSWORD1.                                                   CM1014.2
            057200     IF STATUS-KEY IS EQUAL TO ZERO                               CM1014.2
            057300         PERFORM PASS GO TO RENAB-STATUS-WRITE-01.                CM1014.2
            057400     MOVE STATUS-KEY TO COMPUTED-STATUS                           CM1014.2
            057500     MOVE ZERO TO CORRECT-STATUS.                                 CM1014.2
            057600     PERFORM FAIL.                                                CM1014.2
            057700     GO TO RENAB-STATUS-WRITE-01.                                 CM1014.2
            057800 RENAB-STATUS-DELETE-01.                                          CM1014.2
            057900     PERFORM DE-LETE.                                             CM1014.2
            058000 RENAB-STATUS-WRITE-01.                                           CM1014.2
            058100     MOVE "RENAB-STATUS-TEST-01" TO PAR-NAME.                     CM1014.2
            058200     PERFORM PRINT-DETAIL.                                        CM1014.2
            058300 CLOSE-FILES.                                                     CM1014.2
            058400     PERFORM END-ROUTINE THRU END-ROUTINE-3.                      CM1014.2
            058500     CLOSE    PRINT-FILE.                                         CM1014.2
            058600     STOP     RUN.                                                CM1014.2
            058700 PASS.                                                            CM1014.2
            058800     MOVE "PASS" TO P-OR-F.                                       CM1014.2
            058900 FAIL.                                                            CM1014.2
            059000     ADD      1 TO ERROR-COUNTER.                                 CM1014.2
            059100     MOVE "FAIL*" TO P-OR-F.                                      CM1014.2
            059200 DE-LETE.                                                         CM1014.2
            059300     MOVE     SPACE TO P-OR-F.                                    CM1014.2
            059400     MOVE     "    ************    " TO COMPUTED-A.               CM1014.2
            059500     MOVE     "    ************    " TO CORRECT-A.                CM1014.2
            059600     MOVE "****TEST DELETED****" TO RE-MARK.                      CM1014.2
            059700     ADD 1 TO DELETE-CNT.                                         CM1014.2
            059800 PRINT-DETAIL.                                                    CM1014.2
            059900     MOVE     TEST-RESULTS TO PRINT-REC.                          CM1014.2
            060000     PERFORM WRITE-LINE.                                          CM1014.2
            060100     MOVE     SPACE TO P-OR-F.                                    CM1014.2
            060200     MOVE     SPACE TO COMPUTED-A.                                CM1014.2
            060300     MOVE SPACE TO CORRECT-A.                                     CM1014.2
            060400 END-ROUTINE.                                                     CM1014.2
            060500     MOVE     HYPHEN-LINE TO DUMMY-RECORD.                        CM1014.2
            060600     PERFORM WRITE-LINE.                                          CM1014.2
            060700 PARA-Z.                                                          CM1014.2
            060800     PERFORM  BLANK-LINE-PRINT 4 TIMES.                           CM1014.2
            060900     MOVE     CCVS-E-1 TO DUMMY-RECORD.                           CM1014.2
            061000     PERFORM WRITE-LINE.                                          CM1014.2
            061100 END-ROUTINE-1.                                                   CM1014.2
            061200     PERFORM  BLANK-LINE-PRINT.                                   CM1014.2
            061300     IF       ERROR-COUNTER IS EQUAL TO ZERO                      CM1014.2
            061400              GO TO END-ROUTINE-2.                                CM1014.2
            061500     MOVE     ERROR-COUNTER TO ERROR-TOTAL.                       CM1014.2
            061600     GO TO    END-ROUTINE-3.                                      CM1014.2
            061700 END-ROUTINE-2.                                                   CM1014.2
            061800     MOVE " NO" TO ERROR-TOTAL.                                   CM1014.2
            061900 END-ROUTINE-3.                                                   CM1014.2
            062000     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           CM1014.2
            062100     PERFORM WRITE-LINE.                                          CM1014.2
            062200     IF DELETE-CNT IS EQUAL TO ZERO                               CM1014.2
            062300         MOVE " NO" TO ERROR-TOTAL  ELSE                          CM1014.2
            062400     MOVE DELETE-CNT TO ERROR-TOTAL.                              CM1014.2
            062500     MOVE "TESTS DELETED     " TO ENDER-DESC.                     CM1014.2
            062600     MOVE CCVS-E-2 TO DUMMY-RECORD.                               CM1014.2
            062700     PERFORM WRITE-LINE.                                          CM1014.2
            062800 END-ROUTINE-4.                                                   CM1014.2
            062900     MOVE CCVS-E-3 TO DUMMY-RECORD.                               CM1014.2
            063000     PERFORM WRITE-LINE.                                          CM1014.2
            063100 BLANK-LINE-PRINT.                                                CM1014.2
            063200     MOVE     SPACE TO DUMMY-RECORD.                              CM1014.2
            063300     PERFORM WRITE-LINE.                                          CM1014.2
            063400 WRITE-LINE.                                                      CM1014.2
            063500     WRITE DUMMY-RECORD AFTER ADVANCING 1 LINE.                   CM1014.2
            063600 INCREMENT-POLL-COUNT.                                            CM1014.2
            063700     ADD 1 TO POLL-COUNT ON SIZE ERROR ADD 0 TO POLL-COUNT.       CM1014.2
            063800 DELAY-FOR-30-SECS.                                               CM1014.2
            063900     PERFORM GET-TIME-DIFFERENCE.                                 CM1014.2
            064000     IF COMP-TIME IS LESS THAN 30                                 CM1014.2
            064100         GO TO DELAY-FOR-30-SECS.                                 CM1014.2
            064200 LOG-HEADER.                                                      CM1014.2
            064300     MOVE LOG-HDR-1 TO PRINT-REC                                  CM1014.2
            064400     WRITE PRINT-REC                                              CM1014.2
            064500         AFTER 3 LINES.                                           CM1014.2
            064600     MOVE DATE-RECEIVED TO MSG-DATE.                              CM1014.2
            064700     MOVE WHERE-FROM TO SYM-SOURCE.                               CM1014.2
            064800     MOVE LOG-HDR-2 TO PRINT-REC.                                 CM1014.2
            064900     WRITE PRINT-REC                                              CM1014.2
            065000         AFTER 3 LINES.                                           CM1014.2
            065100     MOVE LOG-HDR-3 TO PRINT-REC.                                 CM1014.2
            065200     WRITE PRINT-REC                                              CM1014.2
            065300         AFTER 2 LINES.                                           CM1014.2
            065400     MOVE LOG-HDR-4 TO PRINT-REC.                                 CM1014.2
            065500     PERFORM WRITE-LINE.                                          CM1014.2
            065600     MOVE SPACES TO PRINT-REC.                                    CM1014.2
            065700     PERFORM WRITE-LINE.                                          CM1014.2
            065800 HEAD-ROUTINE.                                                    CM1014.2
            065900     MOVE CCVS-H-1 TO PRINT-REC                                   CM1014.2
            066000     WRITE PRINT-REC                                              CM1014.2
            066100         AFTER ADVANCING PAGE.                                    CM1014.2
            066200     MOVE CCVS-H-2 TO PRINT-REC.                                  CM1014.2
            066300     WRITE PRINT-REC                                              CM1014.2
            066400         AFTER 2 LINES.                                           CM1014.2
            066500     MOVE CCVS-H-3 TO PRINT-REC.                                  CM1014.2
            066600     WRITE PRINT-REC                                              CM1014.2
            066700         AFTER 5 LINES.                                           CM1014.2
            066800     MOVE HYPHEN-LINE TO PRINT-REC.                               CM1014.2
            066900     PERFORM WRITE-LINE.                                          CM1014.2
                  *END-OF,CM101M                                                            
        """)
    )

    @Test
    fun cm1024_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM102M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM1024.2
            000200 PROGRAM-ID.                                                      CM1024.2
            000300     CM102M.                                                      CM1024.2
            000400 AUTHOR.                                                          CM1024.2
            000500     FEDERAL COMPILER TESTING CENTER.                             CM1024.2
            000600 INSTALLATION.                                                    CM1024.2
            000700     GENERAL SERVICES ADMINISTRATION                              CM1024.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM1024.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM1024.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               CM1024.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 CM1024.2
            001200                                                                  CM1024.2
            001300     PHONE   (703) 756-6153                                       CM1024.2
            001400                                                                  CM1024.2
            001500     " HIGH       ".                                              CM1024.2
            001600 DATE-WRITTEN.                                                    CM1024.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           CM1024.2
            001800     CREATION DATE     /    VALIDATION DATE                       CM1024.2
            001900     "4.2 ".                                                      CM1024.2
            002000 SECURITY.                                                        CM1024.2
            002100     NONE.                                                        CM1024.2
            002200 ENVIRONMENT DIVISION.                                            CM1024.2
            002300 CONFIGURATION SECTION.                                           CM1024.2
            002400 SOURCE-COMPUTER.                                                 CM1024.2
            002500     XXXXX082.                                                    CM1024.2
            002600 OBJECT-COMPUTER.                                                 CM1024.2
            002700     XXXXX083.                                                    CM1024.2
            002800 INPUT-OUTPUT SECTION.                                            CM1024.2
            002900 FILE-CONTROL.                                                    CM1024.2
            003000     SELECT PRINT-FILE ASSIGN TO                                  CM1024.2
            003100     XXXXX055.                                                    CM1024.2
            003200 DATA DIVISION.                                                   CM1024.2
            003300 FILE SECTION.                                                    CM1024.2
            003400 FD  PRINT-FILE                                                   CM1024.2
            003500     LABEL RECORDS                                                CM1024.2
            003600     XXXXX084                                                     CM1024.2
            003700     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       CM1024.2
            003800 01  PRINT-REC PICTURE X(120).                                    CM1024.2
            003900 01  DUMMY-RECORD PICTURE X(120).                                 CM1024.2
            004000 WORKING-STORAGE SECTION.                                         CM1024.2
            004100 77  COMP-TWO PIC 9 COMP VALUE 2.                                 CM1024.2
            004200 77  TWO PIC 9 VALUE 2.                                           CM1024.2
            004300 77  COMP-THREE PIC 9 VALUE 3.                                    CM1024.2
            004400 77  THREE PIC 9 VALUE 3.                                         CM1024.2
            004500 77  SEND-SWITCH PIC 99 COMP.                                     CM1024.2
            004600 77  MSG-NUM PIC 9(4).                                            CM1024.2
            004700 77  MSG-70 PIC X(70).                                            CM1024.2
            004800 77  PASSWORD1 PIC X(10) VALUE                                    CM1024.2
            004900     XXXXX033.                                                    CM1024.2
            005000 01  ERR-MSG.                                                     CM1024.2
            005100     02  FILLER PIC X(33) VALUE                                   CM1024.2
            005200         "THIS MESSAGE SHOULD NOT APPEAR - ".                     CM1024.2
            005300     02  TEST-IND PIC X(4).                                       CM1024.2
            005400 01  LOG-HDR-1.                                                   CM1024.2
            005500     02  FILLER PIC X(48) VALUE SPACES.                           CM1024.2
            005600     02  FILLER PIC X(24) VALUE "LOG OF OUTGOING MESSAGES".       CM1024.2
            005700 01  LOG-HDR-2.                                                   CM1024.2
            005800     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            005900     02  FILLER PIC X(14) VALUE "START  TIME".                    CM1024.2
            006000     02  FILLER PIC X(10) VALUE "ELAPSED".                        CM1024.2
            006100     02  FILLER PIC X(13) VALUE "STATUS/ERR".                     CM1024.2
            006200     02  FILLER PIC X(41) VALUE "LENGTH".                         CM1024.2
            006300     02  FILLER PIC X(7) VALUE "MESSAGE".                         CM1024.2
            006400 01  LOG-HDR-3.                                                   CM1024.2
            006500     02  FILLER PIC X VALUE SPACES.                               CM1024.2
            006600     02  FILLER PIC X(11) VALUE ALL "-".                          CM1024.2
            006700     02  FILLER PIC XXX VALUE SPACES.                             CM1024.2
            006800     02  FILLER PIC X(7) VALUE ALL "-".                           CM1024.2
            006900     02  FILLER PIC XXX VALUE SPACES.                             CM1024.2
            007000     02  FILLER PIC X(10) VALUE ALL "-".                          CM1024.2
            007100     02  FILLER PIC XXX VALUE SPACES.                             CM1024.2
            007200     02  FILLER PIC X(6) VALUE ALL "-".                           CM1024.2
            007300     02  FILLER PIC XXX VALUE SPACES.                             CM1024.2
            007400     02  FILLER PIC X(72) VALUE ALL "-".                          CM1024.2
            007500 01  LOG-LINE.                                                    CM1024.2
            007600     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            007700     02  START-TIME.                                              CM1024.2
            007800         03  HOURS PIC 99.                                        CM1024.2
            007900         03  FILLER PIC X VALUE ":".                              CM1024.2
            008000         03  MINUTES PIC 99.                                      CM1024.2
            008100         03  FILLER PIC X VALUE ":".                              CM1024.2
            008200         03  SECONDS PIC 99.99.                                   CM1024.2
            008300     02  FILLER PIC XX VALUE SPACES.                              CM1024.2
            008400     02  ELAPSED PIC -(4)9.99.                                    CM1024.2
            008500     02  FILLER PIC X(7) VALUE SPACES.                            CM1024.2
            008600     02  STAT PIC 99.                                             CM1024.2
            008700     02  FILLER PIC X VALUE "/".                                  CM1024.2
            008800     02  ERR PIC 9.                                               CM1024.2
            008900     02  FILLER PIC X(5) VALUE SPACES.                            CM1024.2
            009000     02  LNTH PIC ZZZ9.                                           CM1024.2
            009100     02  FILLER PIC X(5) VALUE SPACES.                            CM1024.2
            009200     02  MSG-OUT PIC X(72).                                       CM1024.2
            009300 01  LOG-LINE-1.                                                  CM1024.2
            009400     02  FILLER PIC X(39) VALUE SPACES.                           CM1024.2
            009500     02  FILLER PIC X(8) VALUE "CONT".                            CM1024.2
            009600     02  MSG-FLD PIC X(72).                                       CM1024.2
            009700 01  SUPERIMPOSITION.                                             CM1024.2
            009800     02  S-ALL PIC X(4).                                          CM1024.2
            009900     02  S-WORDS PIC X(6).                                        CM1024.2
            010000     02  S-IN PIC X(3).                                           CM1024.2
            010100     02  S-THIS PIC X(5).                                         CM1024.2
            010200     02  S-MESSAGE PIC X(8).                                      CM1024.2
            010300     02  S-SHOULD PIC X(7).                                       CM1024.2
            010400     02  S-COME PIC X(5).                                         CM1024.2
            010500     02  S-OUT PIC X(4).                                          CM1024.2
            010600     02  S-ON PIC XXX.                                            CM1024.2
            010700     02  S-THE PIC X(4).                                          CM1024.2
            010800     02  S-SAME PIC X(5).                                         CM1024.2
            010900     02  S-LINE PIC X(5).                                         CM1024.2
            011000 01  MSG-A.                                                       CM1024.2
            011100     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            011200     02  MSG-B.                                                   CM1024.2
            011300         03  FILLER PIC X VALUE SPACE.                            CM1024.2
            011400         03  MSG-C.                                               CM1024.2
            011500             04  FILLER PIC X VALUE SPACE.                        CM1024.2
            011600             04  MSG-D.                                           CM1024.2
            011700                 05  FILLER PIC X VALUE SPACE.                    CM1024.2
            011800                 05  MSG-E.                                       CM1024.2
            011900                     06  FILLER PIC X(19) VALUE                   CM1024.2
            012000                             "THIS IS MESSAGE NO.".               CM1024.2
            012100                     06  MSG-NO PIC ZZZZ.                         CM1024.2
            012200                     06  FILLER PIC X(35) VALUE                   CM1024.2
            012300                             ".--THIS SENTENCE MUST NOT APPEAR.". CM1024.2
            012400 01  SYSTEM-TIME.                                                 CM1024.2
            012500     02  HOURS PIC 99.                                            CM1024.2
            012600     02  MINUTES PIC 99.                                          CM1024.2
            012700     02  SECONDS PIC 99V99.                                       CM1024.2
            012800 01  COMP-TIME.                                                   CM1024.2
            012900     02  COMP-HRS PIC 99.                                         CM1024.2
            013000     02  COMP-MINS PIC 99.                                        CM1024.2
            013100     02  COMP-SECS PIC 99V99.                                     CM1024.2
            013200 01  MSG-F.                                                       CM1024.2
            013300     02  FILLER PIC X(19) VALUE  "THIS IS MESSAGE NO.".           CM1024.2
            013400     02  MSG-F-NO PIC ZZZZ.                                       CM1024.2
            013500     02  FILLER PIC X(40) VALUE                                   CM1024.2
            013600             " AND SHOULD APPEAR AT THE TOP OF A PAGE.".          CM1024.2
            013700 01  MSG-G.                                                       CM1024.2
            013800     02  FILLER PIC X(19) VALUE  "THIS IS MESSAGE NO.".           CM1024.2
            013900     02  MSG-G-NO PIC ZZZZ.                                       CM1024.2
            014000     02  FILLER PIC X(41) VALUE                                   CM1024.2
            014100             " AND SHOULD APPEAR AFTER TWO BLANK LINES.".         CM1024.2
            014200 01  MSG-H.                                                       CM1024.2
            014300     02  FILLER PIC X(19) VALUE "THIS IS MESSAGE NO.".            CM1024.2
            014400     02  MSG-H-NO PIC ZZZZ.                                       CM1024.2
            014500     02  FILLER PIC X(41) VALUE                                   CM1024.2
            014600             " AND SHOULD APPEAR BEFORE ONE BLANK LINE.".         CM1024.2
            014700 01  LONG-MSG.                                                    CM1024.2
            014800     02  LONG-MSG-S1 PIC X(73) VALUE "ON PAGE XIII-21, PARAGRAPH 3CM1024.2
            014900-        ".5.4(1)C, THE COBOL STANDARD STATES, ""EXCESS ".        CM1024.2
            015000     02  LONG-MSG-S2 PIC X(67) VALUE "CHARACTERS OF A MESSAGE OR MCM1024.2
            015100-        "ESSAGE SEGMENT WILL NOT BE TRUNCATED.  ".               CM1024.2
            015200     02  LONG-MSG-S3 PIC X(71) VALUE "CHARACTERS WILL BE PACKED TOCM1024.2
            015300-        " A SIZE EQUAL TO THAT OF THE PHYSICAL LINE ".           CM1024.2
            015400     02  LONG-MSG-S4 PIC X(69) VALUE "AND THEN OUTPUTTED TO THE DECM1024.2
            015500-        "VICE.  THE PROCESS CONTINUES ON THE NEXT ".             CM1024.2
            015600     02  LONG-MSG-S5 PIC X(73) VALUE "LINE WITH THE EXCESS CHARACTCM1024.2
            015700-        "ERS.""  IF THIS ENTIRE PARAGRAPH WAS RECEIVED ".        CM1024.2
            015800     02  LONG-MSG-S6 PIC X(71) VALUE "BY THE DESIGNATED DEVICE, THCM1024.2
            015900-        "EN THE FOREGOING RULE IS SUPPORTED BY THIS ".           CM1024.2
            016000     02  LONG-MSG-S7 PIC X(9) VALUE "COMPILER.".                  CM1024.2
            016100 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         CM1024.2
            016200 01  REC-CT PICTURE 99 VALUE ZERO.                                CM1024.2
            016300 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        CM1024.2
            016400 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  CM1024.2
            016500 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          CM1024.2
            016600 01  PASS-COUNTER PIC 999 VALUE ZERO.                             CM1024.2
            016700 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              CM1024.2
            016800 01  ERROR-HOLD PIC 999 VALUE ZERO.                               CM1024.2
            016900 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           CM1024.2
            017000 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            CM1024.2
            017100 01  CCVS-H-1.                                                    CM1024.2
            017200     02  FILLER   PICTURE X(27)  VALUE SPACE.                     CM1024.2
            017300     02 FILLER PICTURE X(67) VALUE                                CM1024.2
            017400     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  CM1024.2
            017500-    " SYSTEM".                                                   CM1024.2
            017600     02  FILLER     PICTURE X(26)  VALUE SPACE.                   CM1024.2
            017700 01  CCVS-H-2.                                                    CM1024.2
            017800     02 FILLER PICTURE X(52) VALUE IS                             CM1024.2
            017900     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   CM1024.2
            018000     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   CM1024.2
            018100     02 TEST-ID PICTURE IS X(9).                                  CM1024.2
            018200     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   CM1024.2
            018300 01  CCVS-H-3.                                                    CM1024.2
            018400     02  FILLER PICTURE X(34) VALUE                               CM1024.2
            018500     " FOR OFFICIAL USE ONLY    ".                                CM1024.2
            018600     02  FILLER PICTURE X(58) VALUE                               CM1024.2
            018700     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".CM1024.2
            018800     02  FILLER PICTURE X(28) VALUE                               CM1024.2
            018900     "  COPYRIGHT   1974 ".                                       CM1024.2
            019000 01  CCVS-E-1.                                                    CM1024.2
            019100     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   CM1024.2
            019200     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        CM1024.2
            019300     02 ID-AGAIN PICTURE IS X(9).                                 CM1024.2
            019400     02 FILLER PICTURE X(45) VALUE IS                             CM1024.2
            019500     " NTIS DISTRIBUTION COBOL 74".                               CM1024.2
            019600 01  CCVS-E-2.                                                    CM1024.2
            019700     02  FILLER                   PICTURE X(31)  VALUE            CM1024.2
            019800     SPACE.                                                       CM1024.2
            019900     02  FILLER                   PICTURE X(21)  VALUE SPACE.     CM1024.2
            020000     02 CCVS-E-2-2.                                               CM1024.2
            020100         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            CM1024.2
            020200         03 FILLER PICTURE IS X VALUE IS SPACE.                   CM1024.2
            020300         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      CM1024.2
            020400 01  CCVS-E-3.                                                    CM1024.2
            020500     02  FILLER PICTURE X(22) VALUE                               CM1024.2
            020600     " FOR OFFICIAL USE ONLY".                                    CM1024.2
            020700     02  FILLER PICTURE X(12) VALUE SPACE.                        CM1024.2
            020800     02  FILLER PICTURE X(58) VALUE                               CM1024.2
            020900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".CM1024.2
            021000     02  FILLER PICTURE X(13) VALUE SPACE.                        CM1024.2
            021100     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 CM1024.2
            021200 01  CCVS-E-4.                                                    CM1024.2
            021300     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           CM1024.2
            021400     02 FILLER PIC XXXX VALUE " OF ".                             CM1024.2
            021500     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           CM1024.2
            021600     02 FILLER PIC X(40) VALUE                                    CM1024.2
            021700      "  TESTS WERE EXECUTED SUCCESSFULLY".                       CM1024.2
            021800 01  XXINFO.                                                      CM1024.2
            021900     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    CM1024.2
            022000     02 INFO-TEXT.                                                CM1024.2
            022100     04 FILLER PIC X(20) VALUE SPACE.                             CM1024.2
            022200     04 XXCOMPUTED PIC X(20).                                     CM1024.2
            022300     04 FILLER PIC X(5) VALUE SPACE.                              CM1024.2
            022400     04 XXCORRECT PIC X(20).                                      CM1024.2
            022500 01  HYPHEN-LINE.                                                 CM1024.2
            022600     02 FILLER PICTURE IS X VALUE IS SPACE.                       CM1024.2
            022700     02 FILLER PICTURE IS X(65) VALUE IS "************************CM1024.2
            022800-    "*****************************************".                 CM1024.2
            022900     02 FILLER PICTURE IS X(54) VALUE IS "************************CM1024.2
            023000-    "******************************".                            CM1024.2
            023100 01  CCVS-PGM-ID PIC X(6) VALUE                                   CM1024.2
            023200     "CM102M".                                                    CM1024.2
            023300 01  TEST-RESULTS.                                                CM1024.2
            023400     02 FILLER                    PICTURE X VALUE SPACE.          CM1024.2
            023500     02 FEATURE                   PICTURE X(18).                  CM1024.2
            023600     02 FILLER                    PICTURE X VALUE SPACE.          CM1024.2
            023700     02 P-OR-F                    PICTURE X(5).                   CM1024.2
            023800     02 FILLER                    PICTURE X  VALUE SPACE.         CM1024.2
            023900     02  PAR-NAME PIC X(20).                                      CM1024.2
            024000     02 FILLER                    PICTURE X VALUE SPACE.          CM1024.2
            024100     02 COMPUTED-A                PICTURE X(20).                  CM1024.2
            024200     02  COMPUTED-SLASH-SET REDEFINES COMPUTED-A.                 CM1024.2
            024300         03  FILLER PIC X(8).                                     CM1024.2
            024400         03  COMPUTED-STATUS PIC XX.                              CM1024.2
            024500         03  SLASH PIC X.                                         CM1024.2
            024600         03  COMPUTED-ERR-KEY PIC X.                              CM1024.2
            024700         03  FILLER PIC X(8).                                     CM1024.2
            024800     02 FILLER                    PICTURE X VALUE SPACE.          CM1024.2
            024900     02 CORRECT-A                 PICTURE X(20).                  CM1024.2
            025000     02  CORRECT-SLASH-SET REDEFINES CORRECT-A.                   CM1024.2
            025100         03  FILLER PIC X(8).                                     CM1024.2
            025200         03  CORRECT-2SLASH1 PIC 99/9.                            CM1024.2
            025300         03  FILLER PIC X(8).                                     CM1024.2
            025400     02 FILLER                    PICTURE X VALUE SPACE.          CM1024.2
            025500     02 RE-MARK                   PICTURE X(30).                  CM1024.2
            025600 01  COLUMNS-LINE-1.                                              CM1024.2
            025700     02  FILLER PIC X(3) VALUE SPACES.                            CM1024.2
            025800     02  FILLER PIC X(17) VALUE "FEATURE TESTED".                 CM1024.2
            025900     02  FILLER PIC X(9) VALUE "RESLT".                           CM1024.2
            026000     02  FILLER PIC X(21) VALUE "PARAGRAPH NAME".                 CM1024.2
            026100     02  FILLER PIC X(22) VALUE "COMPUTED DATA".                  CM1024.2
            026200     02  FILLER PIC X(29) VALUE "CORRECT DATA".                   CM1024.2
            026300     02  FILLER PIC X(7) VALUE "REMARKS".                         CM1024.2
            026400 01  COLUMNS-LINE-2.                                              CM1024.2
            026500     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            026600     02  FILLER PIC X(18) VALUE ALL "-".                          CM1024.2
            026700     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            026800     02  FILLER PIC X(5) VALUE ALL "-".                           CM1024.2
            026900     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            027000     02  FILLER PIC X(20) VALUE ALL "-".                          CM1024.2
            027100     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            027200     02  FILLER PIC X(20) VALUE ALL "-".                          CM1024.2
            027300     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            027400     02  FILLER PIC X(20) VALUE ALL "-".                          CM1024.2
            027500     02  FILLER PIC X VALUE SPACE.                                CM1024.2
            027600     02  FILLER PIC X(31) VALUE ALL "-".                          CM1024.2
            027700 COMMUNICATION SECTION.                                           CM1024.2
            027800 CD  CM-OUTQUE-1 FOR OUTPUT                                       CM1024.2
            027900     DESTINATION COUNT IS ONE                                     CM1024.2
            028000     TEXT LENGTH IS MSG-LENGTH                                    CM1024.2
            028100     STATUS KEY IS STATUS-KEY                                     CM1024.2
            028200     ERROR KEY IS ERR-KEY                                         CM1024.2
            028300     SYMBOLIC DESTINATION IS SYM-DEST.                            CM1024.2
            028400 PROCEDURE    DIVISION.                                           CM1024.2
            028500 SECT-CM102M-0001 SECTION.                                        CM1024.2
            028600 CM102M-INIT.                                                     CM1024.2
            028700     OPEN     OUTPUT PRINT-FILE.                                  CM1024.2
            028800     MOVE "CM102M     " TO TEST-ID.                               CM1024.2
            028900     MOVE     TEST-ID TO ID-AGAIN.                                CM1024.2
            029000     MOVE    SPACE TO TEST-RESULTS.                               CM1024.2
            029100     PERFORM HEAD-ROUTINE.                                        CM1024.2
            029200     PERFORM COLUMN-NAMES-ROUTINE.                                CM1024.2
            029300     MOVE "MCS STATUS WORD" TO FEATURE.                           CM1024.2
            029400 DISAB-STATUS-TEST-01.                                            CM1024.2
            029500     MOVE "INITIAL DISABLE TO OUTPUT CD" TO RE-MARK.              CM1024.2
            029600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            029700     MOVE 1 TO ONE.                                               CM1024.2
            029800     MOVE                                                         CM1024.2
            029900     XXXXX032                                                     CM1024.2
            030000         TO SYM-DEST.                                             CM1024.2
            030100     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
            030200     XXXXX033.                                                    CM1024.2
            030300     MOVE "INFO" TO P-OR-F.                                       CM1024.2
            030400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            030500     MOVE "/" TO SLASH.                                           CM1024.2
            030600     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            030700     MOVE "       INFO TEST FOR" TO CORRECT-A.                    CM1024.2
            030800     GO TO DISAB-STATUS-WRITE-01.                                 CM1024.2
            030900 DISAB-STATUS-DELETE-01.                                          CM1024.2
            031000     PERFORM DE-LETE.                                             CM1024.2
            031100 DISAB-STATUS-WRITE-01.                                           CM1024.2
            031200     MOVE "DISAB-STATUS-TEST-01" TO PAR-NAME.                     CM1024.2
            031300     PERFORM PRINT-DETAIL.                                        CM1024.2
            031400 DISAB-STATUS-TEST-02.                                            CM1024.2
            031500     MOVE "NO DESTINATION SPECIFIED" TO RE-MARK.                  CM1024.2
            031600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            031700     MOVE "GARBAGE" TO SYM-DEST.                                  CM1024.2
            031800     MOVE 1 TO ONE.                                               CM1024.2
            031900     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
            032000     XXXXX033.                                                    CM1024.2
            032100     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
            032200         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
            032300         PERFORM PASS GO TO DISAB-STATUS-WRITE-02.                CM1024.2
            032400     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
            032500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            032600     MOVE "/" TO SLASH.                                           CM1024.2
            032700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            032800     PERFORM FAIL.                                                CM1024.2
            032900     GO TO DISAB-STATUS-WRITE-02.                                 CM1024.2
            033000 DISAB-STATUS-DELETE-02.                                          CM1024.2
            033100     PERFORM DE-LETE.                                             CM1024.2
            033200 DISAB-STATUS-WRITE-02.                                           CM1024.2
            033300     MOVE "DISAB-STATUS-TEST-02" TO PAR-NAME.                     CM1024.2
            033400     PERFORM PRINT-DETAIL.                                        CM1024.2
            033500 DISAB-STATUS-TEST-03.                                            CM1024.2
            033600     MOVE "INVALID PASSWORD USED" TO RE-MARK.                     CM1024.2
            033700     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            033800     MOVE 1 TO ONE.                                               CM1024.2
            033900     MOVE                                                         CM1024.2
            034000     XXXXX032                                                     CM1024.2
            034100         TO SYM-DEST.                                             CM1024.2
            034200     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
            034300         "GARBAGE".                                               CM1024.2
            034400     IF STATUS-KEY IS EQUAL TO "40"                               CM1024.2
            034500         PERFORM PASS GO TO DISAB-STATUS-WRITE-03.                CM1024.2
            034600     MOVE 400 TO CORRECT-2SLASH1.                                 CM1024.2
            034700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            034800     MOVE "/" TO SLASH.                                           CM1024.2
            034900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            035000     PERFORM FAIL.                                                CM1024.2
            035100     GO TO DISAB-STATUS-WRITE-03.                                 CM1024.2
            035200 DISAB-STATUS-DELETE-03.                                          CM1024.2
            035300     PERFORM DE-LETE.                                             CM1024.2
            035400 DISAB-STATUS-WRITE-03.                                           CM1024.2
            035500     MOVE "DISAB-STATUS-TEST-03" TO PAR-NAME.                     CM1024.2
            035600     PERFORM PRINT-DETAIL.                                        CM1024.2
            035700 DISAB-STATUS-TEST-04.                                            CM1024.2
            035800     MOVE "INVALID DESTINATION COUNT (0)" TO RE-MARK.             CM1024.2
            035900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            036000     MOVE                                                         CM1024.2
            036100     XXXXX032                                                     CM1024.2
            036200         TO SYM-DEST.                                             CM1024.2
            036300     MOVE 0 TO ONE.                                               CM1024.2
            036400     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
            036500     XXXXX033.                                                    CM1024.2
            036600     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
            036700         PERFORM PASS GO TO DISAB-STATUS-WRITE-04.                CM1024.2
            036800     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
            036900     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            037000     MOVE "/" TO SLASH.                                           CM1024.2
            037100     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            037200     PERFORM FAIL.                                                CM1024.2
            037300     GO TO DISAB-STATUS-WRITE-04.                                 CM1024.2
            037400 DISAB-STATUS-DELETE-04.                                          CM1024.2
            037500     PERFORM DE-LETE.                                             CM1024.2
            037600 DISAB-STATUS-WRITE-04.                                           CM1024.2
            037700     MOVE "DISAB-STATUS-TEST-04" TO PAR-NAME.                     CM1024.2
            037800     PERFORM PRINT-DETAIL.                                        CM1024.2
            037900 DISAB-STATUS-TEST-05.                                            CM1024.2
            038000     MOVE "COMBINATION ERROR" TO RE-MARK.                         CM1024.2
            038100     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            038200     MOVE SPACES TO SYM-DEST.                                     CM1024.2
            038300     MOVE 0 TO ONE.                                               CM1024.2
            038400     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
            038500         "GARBAGE".                                               CM1024.2
            038600     MOVE "INFO" TO P-OR-F.                                       CM1024.2
            038700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            038800     MOVE "/" TO SLASH.                                           CM1024.2
            038900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            039000     GO TO DISAB-STATUS-WRITE-05.                                 CM1024.2
            039100 DISAB-STATUS-DELETE-05.                                          CM1024.2
            039200     PERFORM DE-LETE.                                             CM1024.2
            039300 DISAB-STATUS-WRITE-05.                                           CM1024.2
            039400     MOVE "DISAB-STATUS-TEST-05" TO PAR-NAME.                     CM1024.2
            039500     PERFORM PRINT-DETAIL.                                        CM1024.2
            039600 SEND-STATUS-TEST-01.                                             CM1024.2
            039700     MOVE "DESTINATION DISABLED" TO RE-MARK.                      CM1024.2
            039800     MOVE "CM102M- I AM THE FIRST MESSAGE IN QUEUE;" TO MSG-70.   CM1024.2
            039900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            040000     MOVE                                                         CM1024.2
            040100     XXXXX032                                                     CM1024.2
            040200         TO SYM-DEST.                                             CM1024.2
            040300     MOVE 1 TO ONE.                                               CM1024.2
            040400     MOVE 45 TO MSG-LENGTH.                                       CM1024.2
            040500     SEND CM-OUTQUE-1 FROM MSG-70 WITH EMI                        CM1024.2
            040600         AFTER ADVANCING PAGE.                                    CM1024.2
            040700     MOVE "THOU SHALT HAVE NO OTHER MESSAGES BEFORE ME." TO MSG-70CM1024.2
            040800     SEND CM-OUTQUE-1 FROM MSG-70 WITH EMI.                       CM1024.2
            040900     MOVE SPACES TO MSG-70.                                       CM1024.2
            041000     MOVE 1 TO MSG-LENGTH.                                        CM1024.2
            041100     SEND CM-OUTQUE-1 FROM MSG-70 WITH EGI.                       CM1024.2
            041200     IF STATUS-KEY IS EQUAL TO "10"                               CM1024.2
            041300         PERFORM PASS GO TO SEND-STATUS-WRITE-01.                 CM1024.2
            041400     MOVE 100 TO CORRECT-2SLASH1.                                 CM1024.2
            041500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            041600     MOVE "/" TO SLASH.                                           CM1024.2
            041700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            041800     PERFORM FAIL.                                                CM1024.2
            041900     GO TO SEND-STATUS-WRITE-01.                                  CM1024.2
            042000 SEND-STATUS-DELETE-01.                                           CM1024.2
            042100     PERFORM DE-LETE.                                             CM1024.2
            042200 SEND-STATUS-WRITE-01.                                            CM1024.2
            042300     MOVE "SEND-STATUS-TEST-01" TO PAR-NAME.                      CM1024.2
            042400     PERFORM PRINT-DETAIL.                                        CM1024.2
            042500 SEND-STATUS-TEST-02.                                             CM1024.2
            042600     MOVE "COMBINATION ERROR" TO RE-MARK.                         CM1024.2
            042700     MOVE SPACES TO SYM-DEST.                                     CM1024.2
            042800     MOVE 0 TO ONE.                                               CM1024.2
            042900     MOVE 100 TO MSG-LENGTH.                                      CM1024.2
            043000     MOVE "S-02" TO TEST-IND.                                     CM1024.2
            043100     SEND CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
            043200     MOVE "INFO" TO P-OR-F.                                       CM1024.2
            043300     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            043400     MOVE "/" TO SLASH.                                           CM1024.2
            043500     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            043600     GO TO SEND-STATUS-WRITE-02.                                  CM1024.2
            043700 SEND-STATUS-DELETE-02.                                           CM1024.2
            043800     PERFORM DE-LETE.                                             CM1024.2
            043900 SEND-STATUS-WRITE-02.                                            CM1024.2
            044000     MOVE "SEND-STATUS-TEST-02" TO PAR-NAME.                      CM1024.2
            044100     PERFORM PRINT-DETAIL.                                        CM1024.2
            044200 ENABL-STATUS-TEST-01.                                            CM1024.2
            044300     MOVE "DESTINATION NOT SPECIFIED" TO RE-MARK.                 CM1024.2
            044400     MOVE SPACES TO SYM-DEST.                                     CM1024.2
            044500     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            044600     MOVE 1 TO ONE.                                               CM1024.2
            044700     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
            044800     XXXXX033.                                                    CM1024.2
            044900     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
            045000         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
            045100         PERFORM PASS GO TO ENABL-STATUS-WRITE-01.                CM1024.2
            045200     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
            045300     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            045400     MOVE "/" TO SLASH.                                           CM1024.2
            045500     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            045600     PERFORM FAIL.                                                CM1024.2
            045700     GO TO ENABL-STATUS-WRITE-01.                                 CM1024.2
            045800 ENABL-STATUS-DELETE-01.                                          CM1024.2
            045900     PERFORM DE-LETE.                                             CM1024.2
            046000 ENABL-STATUS-WRITE-01.                                           CM1024.2
            046100     MOVE "ENABL-STATUS-TEST-01" TO PAR-NAME.                     CM1024.2
            046200     PERFORM PRINT-DETAIL.                                        CM1024.2
            046300 ENABL-STATUS-TEST-02.                                            CM1024.2
            046400     MOVE "INVALID DESTINATION COUNT (0)" TO RE-MARK.             CM1024.2
            046500     MOVE                                                         CM1024.2
            046600     XXXXX032                                                     CM1024.2
            046700         TO SYM-DEST.                                             CM1024.2
            046800     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            046900     MOVE 0 TO ONE.                                               CM1024.2
            047000     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
            047100     XXXXX033.                                                    CM1024.2
            047200     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
            047300         PERFORM PASS GO TO ENABL-STATUS-WRITE-02.                CM1024.2
            047400     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
            047500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            047600     MOVE "/" TO SLASH.                                           CM1024.2
            047700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            047800     PERFORM FAIL.                                                CM1024.2
            047900     GO TO ENABL-STATUS-WRITE-02.                                 CM1024.2
            048000 ENABL-STATUS-DELETE-02.                                          CM1024.2
            048100     PERFORM DE-LETE.                                             CM1024.2
            048200 ENABL-STATUS-WRITE-02.                                           CM1024.2
            048300     MOVE "ENABL-STATUS-TEST-02" TO PAR-NAME.                     CM1024.2
            048400     PERFORM PRINT-DETAIL.                                        CM1024.2
            048500 ENABL-STATUS-TEST-03.                                            CM1024.2
            048600     MOVE "INVALID PASSWORD USED" TO RE-MARK.                     CM1024.2
            048700     MOVE                                                         CM1024.2
            048800     XXXXX032                                                     CM1024.2
            048900         TO SYM-DEST.                                             CM1024.2
            049000     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            049100     MOVE 1 TO ONE.                                               CM1024.2
            049200     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
            049300         "GARBAGE".                                               CM1024.2
            049400     IF STATUS-KEY IS EQUAL TO "40"                               CM1024.2
            049500         PERFORM PASS GO TO ENABL-STATUS-WRITE-03.                CM1024.2
            049600     MOVE 400 TO CORRECT-2SLASH1.                                 CM1024.2
            049700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            049800     MOVE "/" TO SLASH.                                           CM1024.2
            049900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            050000     PERFORM FAIL.                                                CM1024.2
            050100     GO TO ENABL-STATUS-WRITE-03.                                 CM1024.2
            050200 ENABL-STATUS-DELETE-03.                                          CM1024.2
            050300     PERFORM DE-LETE.                                             CM1024.2
            050400 ENABL-STATUS-WRITE-03.                                           CM1024.2
            050500     MOVE "ENABL-STATUS-TEST-03" TO PAR-NAME.                     CM1024.2
            050600     PERFORM PRINT-DETAIL.                                        CM1024.2
            050700 ENABL-STATUS-TEST-04.                                            CM1024.2
            050800     MOVE "VALID ENABLE/NO ERROR EXPECTED" TO RE-MARK.            CM1024.2
            050900     MOVE                                                         CM1024.2
            051000     XXXXX032                                                     CM1024.2
            051100         TO SYM-DEST.                                             CM1024.2
            051200     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            051300     MOVE 1 TO ONE.                                               CM1024.2
            051400     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
            051500     XXXXX033.                                                    CM1024.2
            051600     IF STATUS-KEY IS EQUAL TO ZERO                               CM1024.2
            051700         PERFORM PASS GO TO ENABL-STATUS-WRITE-04.                CM1024.2
            051800     MOVE 0 TO CORRECT-2SLASH1.                                   CM1024.2
            051900     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            052000     MOVE "/" TO SLASH.                                           CM1024.2
            052100     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            052200     PERFORM FAIL.                                                CM1024.2
            052300     GO TO ENABL-STATUS-WRITE-04.                                 CM1024.2
            052400 ENABL-STATUS-DELETE-04.                                          CM1024.2
            052500     PERFORM DE-LETE.                                             CM1024.2
            052600 ENABL-STATUS-WRITE-04.                                           CM1024.2
            052700     MOVE "ENABL-STATUS-TEST-04" TO PAR-NAME.                     CM1024.2
            052800     PERFORM PRINT-DETAIL.                                        CM1024.2
            052900 SEND-STATUS-TEST-03.                                             CM1024.2
            053000     MOVE "DESTINATION UNKNOWN" TO RE-MARK.                       CM1024.2
            053100     MOVE "GARBAGE" TO SYM-DEST.                                  CM1024.2
            053200     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            053300     MOVE 1 TO ONE.                                               CM1024.2
            053400     MOVE 37 TO MSG-LENGTH.                                       CM1024.2
            053500     MOVE "S-03" TO TEST-IND.                                     CM1024.2
            053600     SEND CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
            053700     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
            053800         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
            053900         PERFORM PASS GO TO SEND-STATUS-WRITE-03.                 CM1024.2
            054000     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
            054100     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            054200     MOVE "/" TO SLASH.                                           CM1024.2
            054300     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            054400     PERFORM FAIL.                                                CM1024.2
            054500     GO TO SEND-STATUS-WRITE-03.                                  CM1024.2
            054600 SEND-STATUS-DELETE-03.                                           CM1024.2
            054700     PERFORM DE-LETE.                                             CM1024.2
            054800 SEND-STATUS-WRITE-03.                                            CM1024.2
            054900     MOVE "SEND-STATUS-TEST-03" TO PAR-NAME.                      CM1024.2
            055000     PERFORM PRINT-DETAIL.                                        CM1024.2
            055100 SEND-STATUS-TEST-04.                                             CM1024.2
            055200     MOVE "DESTINATION COUNT INVALID (0)" TO RE-MARK.             CM1024.2
            055300     MOVE                                                         CM1024.2
            055400     XXXXX032                                                     CM1024.2
            055500         TO SYM-DEST.                                             CM1024.2
            055600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            055700     MOVE 0 TO ONE.                                               CM1024.2
            055800     MOVE 37 TO MSG-LENGTH.                                       CM1024.2
            055900     MOVE "S-04" TO TEST-IND.                                     CM1024.2
            056000     SEND CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
            056100     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
            056200         PERFORM PASS GO TO SEND-STATUS-WRITE-04.                 CM1024.2
            056300     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
            056400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            056500     MOVE "/" TO SLASH.                                           CM1024.2
            056600     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            056700     PERFORM FAIL.                                                CM1024.2
            056800     GO TO SEND-STATUS-WRITE-04.                                  CM1024.2
            056900 SEND-STATUS-DELETE-04.                                           CM1024.2
            057000     PERFORM DE-LETE.                                             CM1024.2
            057100 SEND-STATUS-WRITE-04.                                            CM1024.2
            057200     MOVE "SEND-STATUS-TEST-04" TO PAR-NAME.                      CM1024.2
            057300     PERFORM PRINT-DETAIL.                                        CM1024.2
            057400 SEND-STATUS-TEST-05.                                             CM1024.2
            057500     MOVE "CHARACTER COUNT EXCESSIVE" TO RE-MARK.                 CM1024.2
            057600     MOVE                                                         CM1024.2
            057700     XXXXX032                                                     CM1024.2
            057800         TO SYM-DEST.                                             CM1024.2
            057900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
            058000     MOVE 1 TO ONE.                                               CM1024.2
            058100     MOVE 38 TO MSG-LENGTH.                                       CM1024.2
            058200     MOVE "S-05" TO TEST-IND.                                     CM1024.2
            058300     SEND CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
            058400     IF STATUS-KEY IS EQUAL TO "50"                               CM1024.2
            058500         PERFORM PASS GO TO SEND-STATUS-WRITE-05.                 CM1024.2
            058600     MOVE 500 TO CORRECT-2SLASH1.                                 CM1024.2
            058700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
            058800     MOVE "/" TO SLASH.                                           CM1024.2
            058900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
            059000     PERFORM FAIL.                                                CM1024.2
            059100     GO TO SEND-STATUS-WRITE-05.                                  CM1024.2
            059200 SEND-STATUS-DELETE-05.                                           CM1024.2
            059300     PERFORM DE-LETE.                                             CM1024.2
            059400 SEND-STATUS-WRITE-05.                                            CM1024.2
            059500     MOVE "SEND-STATUS-TEST-05" TO PAR-NAME.                      CM1024.2
            059600     PERFORM PRINT-DETAIL.                                        CM1024.2
            059700 STATUS-TESTS-COMPLETED.                                          CM1024.2
            059800     PERFORM END-ROUTINE.                                         CM1024.2
            059900     PERFORM END-ROUTINE-1 THRU END-ROUTINE-3.                    CM1024.2
            060000     PERFORM END-ROUTINE.                                         CM1024.2
            060100     MOVE LOG-HDR-1 TO PRINT-REC.                                 CM1024.2
            060200     WRITE PRINT-REC                                              CM1024.2
            060300         AFTER 3 LINES.                                           CM1024.2
            060400     MOVE LOG-HDR-2 TO PRINT-REC.                                 CM1024.2
            060500     WRITE PRINT-REC                                              CM1024.2
            060600         AFTER 3 LINES.                                           CM1024.2
            060700     MOVE LOG-HDR-3 TO PRINT-REC.                                 CM1024.2
            060800     WRITE PRINT-REC.                                             CM1024.2
            060900     PERFORM BLANK-LINE-PRINT.                                    CM1024.2
            061000 VARIABLE-LENGTH-MSGS.                                            CM1024.2
            061100     MOVE 1 TO ONE.                                               CM1024.2
            061200     MOVE                                                         CM1024.2
            061300     XXXXX032                                                     CM1024.2
            061400         TO SYM-DEST.                                             CM1024.2
            061500     MOVE 1 TO MSG-NO SEND-SWITCH.                                CM1024.2
            061600     MOVE 28 TO MSG-LENGTH.                                       CM1024.2
            061700     MOVE MSG-A TO MSG-OUT.                                       CM1024.2
            061800     PERFORM SEND-AND-LOG.                                        CM1024.2
            061900     MOVE 2 TO MSG-NO.                                            CM1024.2
            062000     MOVE 27 TO MSG-LENGTH.                                       CM1024.2
            062100     MOVE MSG-B TO MSG-OUT.                                       CM1024.2
            062200     PERFORM SEND-AND-LOG.                                        CM1024.2
            062300     MOVE 3 TO MSG-NO.                                            CM1024.2
            062400     MOVE 26 TO MSG-LENGTH.                                       CM1024.2
            062500     MOVE MSG-C TO MSG-OUT.                                       CM1024.2
            062600     PERFORM SEND-AND-LOG.                                        CM1024.2
            062700     MOVE 4 TO MSG-NO.                                            CM1024.2
            062800     MOVE 25 TO MSG-LENGTH.                                       CM1024.2
            062900     MOVE MSG-D TO MSG-OUT.                                       CM1024.2
            063000     PERFORM SEND-AND-LOG.                                        CM1024.2
            063100     MOVE 2 TO SEND-SWITCH.                                       CM1024.2
            063200     MOVE 5 TO MSG-NO.                                            CM1024.2
            063300     MOVE 24 TO MSG-LENGTH.                                       CM1024.2
            063400     MOVE MSG-E TO MSG-OUT.                                       CM1024.2
            063500     PERFORM SEND-AND-LOG.                                        CM1024.2
            063600 AFTER-PAGE-MSGS.                                                 CM1024.2
            063700     MOVE 6 TO MSG-NUM.                                           CM1024.2
            063800     MOVE 3 TO SEND-SWITCH.                                       CM1024.2
            063900     MOVE 63 TO MSG-LENGTH.                                       CM1024.2
            064000     PERFORM AFTER-PAGE-MSGS-01 5 TIMES.                          CM1024.2
            064100     GO TO AFTER-THREE-MSGS.                                      CM1024.2
            064200 AFTER-PAGE-MSGS-01.                                              CM1024.2
            064300     MOVE MSG-NUM TO MSG-F-NO.                                    CM1024.2
            064400     ADD 1 TO MSG-NUM.                                            CM1024.2
            064500     MOVE MSG-F TO MSG-OUT.                                       CM1024.2
            064600     PERFORM SEND-AND-LOG.                                        CM1024.2
            064700 AFTER-THREE-MSGS.                                                CM1024.2
            064800     MOVE 64 TO MSG-LENGTH.                                       CM1024.2
            064900     PERFORM AFTER-THREE-MSGS-01 5 TIMES.                         CM1024.2
            065000     GO TO EGI-ONLY.                                              CM1024.2
            065100 AFTER-THREE-MSGS-01.                                             CM1024.2
            065200     MOVE MSG-NUM TO MSG-G-NO.                                    CM1024.2
            065300     ADD 1 TO MSG-NUM SEND-SWITCH.                                CM1024.2
            065400     MOVE MSG-G TO MSG-OUT.                                       CM1024.2
            065500     PERFORM SEND-AND-LOG.                                        CM1024.2
            065600 EGI-ONLY.                                                        CM1024.2
            065700     MOVE "ONLY EGI WAS SENT.  NO MESSAGE ACCOMPANYING" TO MSG-OUTCM1024.2
            065800     ADD 1 TO SEND-SWITCH.                                        CM1024.2
            065900     MOVE 0 TO MSG-LENGTH.                                        CM1024.2
            066000     PERFORM SEND-AND-LOG.                                        CM1024.2
            066100 BEFORE-ADV-INIT.                                                 CM1024.2
            066200     MOVE "0LTH" TO TEST-IND.                                     CM1024.2
            066300     ADD 1 TO SEND-SWITCH.                                        CM1024.2
            066400     MOVE ERR-MSG TO MSG-OUT.                                     CM1024.2
            066500     PERFORM SEND-AND-LOG.                                        CM1024.2
            066600 BEFORE-PAGE-MSGS.                                                CM1024.2
            066700     MOVE 63 TO MSG-LENGTH.                                       CM1024.2
            066800     PERFORM AFTER-PAGE-MSGS-01 5 TIMES.                          CM1024.2
            066900 BEFORE-TWO-MSGS.                                                 CM1024.2
            067000     MOVE 64 TO MSG-LENGTH.                                       CM1024.2
            067100     PERFORM BEFORE-TWO-MSGS-01 5 TIMES.                          CM1024.2
            067200     GO TO ZERO-LINES-MSGS.                                       CM1024.2
            067300 BEFORE-TWO-MSGS-01.                                              CM1024.2
            067400     MOVE MSG-NUM TO MSG-H-NO.                                    CM1024.2
            067500     ADD 1 TO MSG-NUM.                                            CM1024.2
            067600     ADD 1 TO SEND-SWITCH.                                        CM1024.2
            067700     MOVE MSG-H TO MSG-OUT.                                       CM1024.2
            067800     PERFORM SEND-AND-LOG.                                        CM1024.2
            067900 ZERO-LINES-MSGS.                                                 CM1024.2
            068000     ADD 1 TO SEND-SWITCH.                                        CM1024.2
            068100     MOVE 59 TO MSG-LENGTH.                                       CM1024.2
            068200     MOVE "ALL" TO SUPERIMPOSITION.                               CM1024.2
            068300     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            068400     MOVE "WORDS" TO S-WORDS.                                     CM1024.2
            068500     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            068600     MOVE "IN" TO S-IN.                                           CM1024.2
            068700     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            068800     MOVE "THIS" TO S-THIS.                                       CM1024.2
            068900     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            069000     MOVE "MESSAGE" TO S-MESSAGE.                                 CM1024.2
            069100     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            069200     MOVE "SHOULD" TO S-SHOULD.                                   CM1024.2
            069300     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            069400     ADD 1 TO SEND-SWITCH.                                        CM1024.2
            069500     MOVE "COME" TO S-COME.                                       CM1024.2
            069600     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            069700     MOVE "OUT" TO S-OUT.                                         CM1024.2
            069800     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            069900     MOVE "ON" TO S-ON.                                           CM1024.2
            070000     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            070100     MOVE "THE" TO S-THE.                                         CM1024.2
            070200     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            070300     MOVE "SAME" TO S-SAME.                                       CM1024.2
            070400     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            070500     MOVE "LINE." TO S-LINE.                                      CM1024.2
            070600     PERFORM ZERO-LINES-MSGS-01.                                  CM1024.2
            070700     GO TO 433-CHARACTER-MSG.                                     CM1024.2
            070800 ZERO-LINES-MSGS-01.                                              CM1024.2
            070900     MOVE SUPERIMPOSITION TO MSG-OUT.                             CM1024.2
            071000     PERFORM SEND-AND-LOG.                                        CM1024.2
            071100     MOVE SPACES TO SUPERIMPOSITION.                              CM1024.2
            071200 433-CHARACTER-MSG.                                               CM1024.2
            071300     ADD 1 TO SEND-SWITCH.                                        CM1024.2
            071400     MOVE 433 TO MSG-LENGTH.                                      CM1024.2
            071500     MOVE LONG-MSG-S1 TO MSG-OUT.                                 CM1024.2
            071600     PERFORM SEND-AND-LOG.                                        CM1024.2
            071700     MOVE LONG-MSG-S2 TO MSG-FLD.                                 CM1024.2
            071800     WRITE PRINT-REC FROM LOG-LINE-1.                             CM1024.2
            071900     MOVE LONG-MSG-S3 TO MSG-FLD.                                 CM1024.2
            072000     WRITE PRINT-REC FROM LOG-LINE-1.                             CM1024.2
            072100     MOVE LONG-MSG-S4 TO MSG-FLD.                                 CM1024.2
            072200     WRITE PRINT-REC FROM LOG-LINE-1.                             CM1024.2
            072300     MOVE LONG-MSG-S5 TO MSG-FLD.                                 CM1024.2
            072400     WRITE PRINT-REC FROM LOG-LINE-1.                             CM1024.2
            072500     MOVE LONG-MSG-S6 TO MSG-FLD.                                 CM1024.2
            072600     WRITE PRINT-REC FROM LOG-LINE-1.                             CM1024.2
            072700     MOVE LONG-MSG-S7 TO MSG-FLD.                                 CM1024.2
            072800     WRITE PRINT-REC FROM LOG-LINE-1.                             CM1024.2
            072900 MSG-BEFORE-DELAY-AND-DISABLE.                                    CM1024.2
            073000     MOVE "EXPECT A PAUSE OF UP TO 30 SECONDS BEFORE TRANSMISSION CM1024.2
            073100-        "OF NEXT MESSAGE." TO MSG-OUT.                           CM1024.2
            073200     MOVE 72 TO MSG-LENGTH.                                       CM1024.2
            073300     MOVE 4 TO SEND-SWITCH.                                       CM1024.2
            073400     PERFORM SEND-AND-LOG.                                        CM1024.2
            073500 DELAY-FOR-30-SECS.                                               CM1024.2
            073600     ACCEPT SYSTEM-TIME FROM TIME.                                CM1024.2
            073700     IF (HOURS OF SYSTEM-TIME * 3600 + MINUTES OF SYSTEM-TIME * 60CM1024.2
            073800         + SECONDS OF SYSTEM-TIME) - (COMP-HRS * 3600 + COMP-MINS CM1024.2
            073900         * 60 + COMP-SECS) IS LESS THAN 30                        CM1024.2
            074000         GO TO DELAY-FOR-30-SECS.                                 CM1024.2
            074100 DISABLE-DEVICE.                                                  CM1024.2
            074200     MOVE "****  DEVICE DISABLED  ****" TO MSG-OUT.               CM1024.2
            074300     MOVE 0 TO MSG-LENGTH.                                        CM1024.2
            074400     MOVE 19 TO SEND-SWITCH.                                      CM1024.2
            074500     PERFORM SEND-AND-LOG.                                        CM1024.2
            074600 10-WHILE-DISABLED.                                               CM1024.2
            074700     MOVE "TRANSMISSION NOW RESUMED." TO MSG-OUT.                 CM1024.2
            074800     MOVE 25 TO MSG-LENGTH.                                       CM1024.2
            074900     MOVE 1 TO SEND-SWITCH.                                       CM1024.2
            075000     PERFORM SEND-AND-LOG.                                        CM1024.2
            075100     MOVE 24 TO MSG-LENGTH.                                       CM1024.2
            075200     PERFORM 10-WHILE-DISABLED-01 8 TIMES.                        CM1024.2
            075300     GO TO 10-WHILE-DISABLED-02.                                  CM1024.2
            075400 10-WHILE-DISABLED-01.                                            CM1024.2
            075500     MOVE MSG-NUM TO MSG-NO.                                      CM1024.2
            075600     ADD 1 TO MSG-NUM.                                            CM1024.2
            075700     MOVE MSG-E TO MSG-OUT.                                       CM1024.2
            075800     PERFORM SEND-AND-LOG.                                        CM1024.2
            075900 10-WHILE-DISABLED-02.                                            CM1024.2
            076000     MOVE "THERE SHOULD BE NO ABNORMAL DELAY IN RECEIVING THE NEXTCM1024.2
            076100-        " MESSAGE." TO MSG-OUT.                                  CM1024.2
            076200     MOVE 63 TO MSG-LENGTH.                                       CM1024.2
            076300     PERFORM SEND-AND-LOG.                                        CM1024.2
            076400 RE-ENABLE-OUTQUE.                                                CM1024.2
            076500     MOVE "****  DEVICE NOW RE-ENABLED  ****" TO MSG-OUT.         CM1024.2
            076600     MOVE 0 TO MSG-LENGTH.                                        CM1024.2
            076700     MOVE 20 TO SEND-SWITCH.                                      CM1024.2
            076800     PERFORM SEND-AND-LOG.                                        CM1024.2
            076900 ENQUEUE-500-MORE.                                                CM1024.2
            077000     MOVE "THIS IS THAT NEXT MESSAGE." TO MSG-OUT.                CM1024.2
            077100     MOVE 26 TO MSG-LENGTH.                                       CM1024.2
            077200     MOVE 2 TO SEND-SWITCH.                                       CM1024.2
            077300     PERFORM SEND-AND-LOG.                                        CM1024.2
            077400     MOVE 24 TO MSG-LENGTH.                                       CM1024.2
            077500     PERFORM 10-WHILE-DISABLED-01 500 TIMES.                      CM1024.2
            077600 DELAY-DISABLE-DELAY-AND-STOP.                                    CM1024.2
            077700     PERFORM DELAY-FOR-30-SECS.                                   CM1024.2
            077800     PERFORM DISABLE-DEVICE.                                      CM1024.2
            077900     PERFORM DELAY-FOR-30-SECS.                                   CM1024.2
            078000     PERFORM END-ROUTINE THRU PARA-Z.                             CM1024.2
            078100     PERFORM END-ROUTINE-4.                                       CM1024.2
            078200     CLOSE PRINT-FILE.                                            CM1024.2
            078300     STOP RUN.                                                    CM1024.2
            078400 SEND-AND-LOG.                                                    CM1024.2
            078500     ACCEPT SYSTEM-TIME FROM TIME.                                CM1024.2
            078600     PERFORM UNIFORM-SEND.                                        CM1024.2
            078700     ACCEPT COMP-TIME FROM TIME.                                  CM1024.2
            078800     MOVE CORR SYSTEM-TIME TO START-TIME.                         CM1024.2
            078900     COMPUTE ELAPSED =                                            CM1024.2
            079000         (COMP-HRS * 3600 + COMP-MINS * 60 + COMP-SECS) -         CM1024.2
            079100         (HOURS OF SYSTEM-TIME * 3600 + MINUTES OF SYSTEM-TIME *  CM1024.2
            079200         60 + SECONDS OF SYSTEM-TIME).                            CM1024.2
            079300     MOVE STATUS-KEY TO STAT.                                     CM1024.2
            079400     MOVE ERR-KEY TO ERR.                                         CM1024.2
            079500     MOVE MSG-LENGTH TO LNTH.                                     CM1024.2
            079600     MOVE LOG-LINE TO PRINT-REC.                                  CM1024.2
            079700     PERFORM WRITE-LINE.                                          CM1024.2
            079800 UNIFORM-SEND SECTION.                                            CM1024.2
            079900 UNIFORM-SEND-SWITCH.                                             CM1024.2
            080000     GO TO                                                        CM1024.2
            080100         SEND-EMI-A1                                              CM1024.2
            080200         SEND-EGI-A1                                              CM1024.2
            080300         SEND-EMI-AP                                              CM1024.2
            080400         SEND-EMI-A3-01                                           CM1024.2
            080500         SEND-EMI-A3-02                                           CM1024.2
            080600         SEND-EMI-A3-03                                           CM1024.2
            080700         SEND-EMI-A3-04                                           CM1024.2
            080800         SEND-EMI-A3-05                                           CM1024.2
            080900         SEND-EGI-ONLY                                            CM1024.2
            081000         SEND-EMI-BP                                              CM1024.2
            081100         SEND-EMI-B2-01                                           CM1024.2
            081200         SEND-EMI-B2-02                                           CM1024.2
            081300         SEND-EMI-B2-03                                           CM1024.2
            081400         SEND-EMI-B2-04                                           CM1024.2
            081500         SEND-EMI-B2-05                                           CM1024.2
            081600         SEND-EMI-A0                                              CM1024.2
            081700         SEND-EMI-B0                                              CM1024.2
            081800         SEND-LONG-MSG                                            CM1024.2
            081900         DISABLE-OUTQUE                                           CM1024.2
            082000         ENABLE-OUTQUE                                            CM1024.2
            082100             DEPENDING ON SEND-SWITCH.                            CM1024.2
            082200 SEND-EMI-A1.                                                     CM1024.2
            082300     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI.                      CM1024.2
            082400     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            082500 SEND-EGI-A1.                                                     CM1024.2
            082600     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EGI.                      CM1024.2
            082700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            082800 SEND-EMI-AP.                                                     CM1024.2
            082900     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI AFTER PAGE.           CM1024.2
            083000     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            083100 SEND-EMI-A3-01.                                                  CM1024.2
            083200     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI AFTER ADVANCING 3     CM1024.2
            083300         LINES.                                                   CM1024.2
            083400     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            083500 SEND-EMI-A3-02.                                                  CM1024.2
            083600     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            083700         AFTER ADVANCING THREE LINES.                             CM1024.2
            083800     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            083900 SEND-EMI-A3-03.                                                  CM1024.2
            084000     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            084100         AFTER 3 LINE.                                            CM1024.2
            084200     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            084300 SEND-EMI-A3-04.                                                  CM1024.2
            084400     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            084500         AFTER COMP-THREE.                                        CM1024.2
            084600     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            084700 SEND-EMI-A3-05.                                                  CM1024.2
            084800     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            084900         AFTER 3.                                                 CM1024.2
            085000     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            085100 SEND-EGI-ONLY.                                                   CM1024.2
            085200     SEND CM-OUTQUE-1 WITH EGI.                                   CM1024.2
            085300     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            085400 SEND-EMI-BP.                                                     CM1024.2
            085500     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            085600         BEFORE ADVANCING PAGE.                                   CM1024.2
            085700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            085800 SEND-EMI-B2-01.                                                  CM1024.2
            085900     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            086000         BEFORE ADVANCING 2 LINES.                                CM1024.2
            086100     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            086200 SEND-EMI-B2-02.                                                  CM1024.2
            086300     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            086400         BEFORE ADVANCING TWO LINES.                              CM1024.2
            086500     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            086600 SEND-EMI-B2-03.                                                  CM1024.2
            086700     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            086800         BEFORE 2 LINE.                                           CM1024.2
            086900     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            087000 SEND-EMI-B2-04.                                                  CM1024.2
            087100     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            087200         BEFORE COMP-TWO.                                         CM1024.2
            087300     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            087400 SEND-EMI-B2-05.                                                  CM1024.2
            087500     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            087600         BEFORE 2.                                                CM1024.2
            087700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            087800 SEND-EMI-A0.                                                     CM1024.2
            087900     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            088000         AFTER 0 LINES.                                           CM1024.2
            088100     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            088200 SEND-EMI-B0.                                                     CM1024.2
            088300     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
            088400         BEFORE ZERO LINES.                                       CM1024.2
            088500     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            088600 SEND-LONG-MSG.                                                   CM1024.2
            088700     SEND CM-OUTQUE-1 FROM LONG-MSG WITH EMI AFTER PAGE.          CM1024.2
            088800     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            088900 DISABLE-OUTQUE.                                                  CM1024.2
            089000     DISABLE OUTPUT CM-OUTQUE-1 KEY                               CM1024.2
            089100     PASSWORD1.                                                   CM1024.2
            089200     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
            089300 ENABLE-OUTQUE.                                                   CM1024.2
            089400     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
            089500     XXXXX033.                                                    CM1024.2
            089600 UNIFORM-SEND-EXIT.                                               CM1024.2
            089700     EXIT.                                                        CM1024.2
            089800 COMMON-SUBROUTINES SECTION.                                      CM1024.2
            089900 PASS.                                                            CM1024.2
            090000     MOVE "PASS" TO P-OR-F.                                       CM1024.2
            090100 FAIL.                                                            CM1024.2
            090200     ADD      1 TO ERROR-COUNTER.                                 CM1024.2
            090300     MOVE "FAIL*" TO P-OR-F.                                      CM1024.2
            090400 DE-LETE.                                                         CM1024.2
            090500     MOVE     SPACE TO P-OR-F.                                    CM1024.2
            090600     MOVE     "    ************    " TO COMPUTED-A.               CM1024.2
            090700     MOVE     "    ************    " TO CORRECT-A.                CM1024.2
            090800     MOVE "****TEST DELETED****" TO RE-MARK.                      CM1024.2
            090900     ADD 1 TO DELETE-CNT.                                         CM1024.2
            091000 PRINT-DETAIL.                                                    CM1024.2
            091100     MOVE     TEST-RESULTS TO PRINT-REC.                          CM1024.2
            091200     PERFORM WRITE-LINE.                                          CM1024.2
            091300     MOVE     SPACE TO P-OR-F.                                    CM1024.2
            091400     MOVE     SPACE TO COMPUTED-A.                                CM1024.2
            091500     MOVE SPACE TO CORRECT-A.                                     CM1024.2
            091600 COLUMN-NAMES-ROUTINE.                                            CM1024.2
            091700     MOVE     COLUMNS-LINE-1 TO DUMMY-RECORD.                     CM1024.2
            091800     PERFORM WRITE-LINE.                                          CM1024.2
            091900     MOVE     COLUMNS-LINE-2 TO DUMMY-RECORD.                     CM1024.2
            092000     PERFORM WRITE-LINE.                                          CM1024.2
            092100     PERFORM  BLANK-LINE-PRINT.                                   CM1024.2
            092200 END-ROUTINE.                                                     CM1024.2
            092300     MOVE     HYPHEN-LINE TO DUMMY-RECORD.                        CM1024.2
            092400     PERFORM WRITE-LINE.                                          CM1024.2
            092500 PARA-Z.                                                          CM1024.2
            092600     PERFORM  BLANK-LINE-PRINT 4 TIMES.                           CM1024.2
            092700     MOVE     CCVS-E-1 TO DUMMY-RECORD.                           CM1024.2
            092800     PERFORM WRITE-LINE.                                          CM1024.2
            092900 END-ROUTINE-1.                                                   CM1024.2
            093000     PERFORM  BLANK-LINE-PRINT.                                   CM1024.2
            093100     IF       ERROR-COUNTER IS EQUAL TO ZERO                      CM1024.2
            093200              GO TO END-ROUTINE-2.                                CM1024.2
            093300     MOVE     ERROR-COUNTER TO ERROR-TOTAL.                       CM1024.2
            093400     GO TO    END-ROUTINE-3.                                      CM1024.2
            093500 END-ROUTINE-2.                                                   CM1024.2
            093600     MOVE " NO" TO ERROR-TOTAL.                                   CM1024.2
            093700 END-ROUTINE-3.                                                   CM1024.2
            093800     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           CM1024.2
            093900     PERFORM WRITE-LINE.                                          CM1024.2
            094000     IF DELETE-CNT IS EQUAL TO ZERO                               CM1024.2
            094100         MOVE " NO" TO ERROR-TOTAL  ELSE                          CM1024.2
            094200     MOVE DELETE-CNT TO ERROR-TOTAL.                              CM1024.2
            094300     MOVE "TESTS DELETED     " TO ENDER-DESC.                     CM1024.2
            094400     MOVE CCVS-E-2 TO DUMMY-RECORD.                               CM1024.2
            094500     PERFORM WRITE-LINE.                                          CM1024.2
            094600 END-ROUTINE-4.                                                   CM1024.2
            094700     MOVE CCVS-E-3 TO DUMMY-RECORD.                               CM1024.2
            094800     PERFORM WRITE-LINE.                                          CM1024.2
            094900 BLANK-LINE-PRINT.                                                CM1024.2
            095000     MOVE     SPACE TO DUMMY-RECORD.                              CM1024.2
            095100     PERFORM WRITE-LINE.                                          CM1024.2
            095200 WRITE-LINE.                                                      CM1024.2
            095300     WRITE DUMMY-RECORD AFTER ADVANCING 1 LINE.                   CM1024.2
            095400 HEAD-ROUTINE.                                                    CM1024.2
            095500     MOVE CCVS-H-1 TO PRINT-REC                                   CM1024.2
            095600     WRITE PRINT-REC                                              CM1024.2
            095700         AFTER ADVANCING PAGE.                                    CM1024.2
            095800     MOVE CCVS-H-2 TO PRINT-REC.                                  CM1024.2
            095900     WRITE PRINT-REC                                              CM1024.2
            096000         AFTER 2 LINES.                                           CM1024.2
            096100     MOVE CCVS-H-3 TO PRINT-REC.                                  CM1024.2
            096200     WRITE PRINT-REC                                              CM1024.2
            096300         AFTER 5 LINES.                                           CM1024.2
            096400     MOVE HYPHEN-LINE TO PRINT-REC.                               CM1024.2
            096500     PERFORM WRITE-LINE.                                          CM1024.2
                  *END-OF,CM102M                                                            
        """)
    )

    @Test
    fun cm1034_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM103M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM1034.2
            000200 PROGRAM-ID.                                                      CM1034.2
            000300     CM103M.                                                      CM1034.2
            000400 AUTHOR.                                                          CM1034.2
            000500     FEDERAL COMPILER TESTING CENTER.                             CM1034.2
            000600 INSTALLATION.                                                    CM1034.2
            000700     GENERAL SERVICES ADMINISTRATION                              CM1034.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM1034.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM1034.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               CM1034.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 CM1034.2
            001200                                                                  CM1034.2
            001300     PHONE   (703) 756-6153                                       CM1034.2
            001400                                                                  CM1034.2
            001500     " HIGH       ".                                              CM1034.2
            001600 DATE-WRITTEN.                                                    CM1034.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           CM1034.2
            001800     CREATION DATE     /    VALIDATION DATE                       CM1034.2
            001900     "4.2 ".                                                      CM1034.2
            002000 SECURITY.                                                        CM1034.2
            002100     NONE.                                                        CM1034.2
            002200 ENVIRONMENT DIVISION.                                            CM1034.2
            002300 CONFIGURATION SECTION.                                           CM1034.2
            002400 SOURCE-COMPUTER.                                                 CM1034.2
            002500     XXXXX082.                                                    CM1034.2
            002600 OBJECT-COMPUTER.                                                 CM1034.2
            002700     XXXXX083.                                                    CM1034.2
            002800 INPUT-OUTPUT SECTION.                                            CM1034.2
            002900 FILE-CONTROL.                                                    CM1034.2
            003000     SELECT PRINT-FILE ASSIGN TO                                  CM1034.2
            003100     XXXXX055.                                                    CM1034.2
            003200 DATA DIVISION.                                                   CM1034.2
            003300 FILE SECTION.                                                    CM1034.2
            003400 FD  PRINT-FILE                                                   CM1034.2
            003500     LABEL RECORDS                                                CM1034.2
            003600     XXXXX084                                                     CM1034.2
            003700     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       CM1034.2
            003800 01  PRINT-REC PICTURE X(120).                                    CM1034.2
            003900 01  DUMMY-RECORD PICTURE X(120).                                 CM1034.2
            004000 WORKING-STORAGE SECTION.                                         CM1034.2
            004100 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         CM1034.2
            004200 01  REC-CT PICTURE 99 VALUE ZERO.                                CM1034.2
            004300 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        CM1034.2
            004400 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  CM1034.2
            004500 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          CM1034.2
            004600 01  PASS-COUNTER PIC 999 VALUE ZERO.                             CM1034.2
            004700 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              CM1034.2
            004800 01  ERROR-HOLD PIC 999 VALUE ZERO.                               CM1034.2
            004900 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           CM1034.2
            005000 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            CM1034.2
            005100 01  CCVS-H-1.                                                    CM1034.2
            005200     02  FILLER   PICTURE X(27)  VALUE SPACE.                     CM1034.2
            005300     02 FILLER PICTURE X(67) VALUE                                CM1034.2
            005400     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  CM1034.2
            005500-    " SYSTEM".                                                   CM1034.2
            005600     02  FILLER     PICTURE X(26)  VALUE SPACE.                   CM1034.2
            005700 01  CCVS-H-2.                                                    CM1034.2
            005800     02 FILLER PICTURE X(52) VALUE IS                             CM1034.2
            005900     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   CM1034.2
            006000     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   CM1034.2
            006100     02 TEST-ID PICTURE IS X(9).                                  CM1034.2
            006200     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   CM1034.2
            006300 01  CCVS-H-3.                                                    CM1034.2
            006400     02  FILLER PICTURE X(34) VALUE                               CM1034.2
            006500     " FOR OFFICIAL USE ONLY    ".                                CM1034.2
            006600     02  FILLER PICTURE X(58) VALUE                               CM1034.2
            006700     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".CM1034.2
            006800     02  FILLER PICTURE X(28) VALUE                               CM1034.2
            006900     "  COPYRIGHT   1974 ".                                       CM1034.2
            007000 01  CCVS-E-1.                                                    CM1034.2
            007100     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   CM1034.2
            007200     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        CM1034.2
            007300     02 ID-AGAIN PICTURE IS X(9).                                 CM1034.2
            007400     02 FILLER PICTURE X(45) VALUE IS                             CM1034.2
            007500     " NTIS DISTRIBUTION COBOL 74".                               CM1034.2
            007600 01  CCVS-E-2.                                                    CM1034.2
            007700     02  FILLER                   PICTURE X(31)  VALUE            CM1034.2
            007800     SPACE.                                                       CM1034.2
            007900     02  FILLER                   PICTURE X(21)  VALUE SPACE.     CM1034.2
            008000     02 CCVS-E-2-2.                                               CM1034.2
            008100         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            CM1034.2
            008200         03 FILLER PICTURE IS X VALUE IS SPACE.                   CM1034.2
            008300         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      CM1034.2
            008400 01  CCVS-E-3.                                                    CM1034.2
            008500     02  FILLER PICTURE X(22) VALUE                               CM1034.2
            008600     " FOR OFFICIAL USE ONLY".                                    CM1034.2
            008700     02  FILLER PICTURE X(12) VALUE SPACE.                        CM1034.2
            008800     02  FILLER PICTURE X(58) VALUE                               CM1034.2
            008900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".CM1034.2
            009000     02  FILLER PICTURE X(13) VALUE SPACE.                        CM1034.2
            009100     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 CM1034.2
            009200 01  CCVS-E-4.                                                    CM1034.2
            009300     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           CM1034.2
            009400     02 FILLER PIC XXXX VALUE " OF ".                             CM1034.2
            009500     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           CM1034.2
            009600     02 FILLER PIC X(40) VALUE                                    CM1034.2
            009700      "  TESTS WERE EXECUTED SUCCESSFULLY".                       CM1034.2
            009800 01  XXINFO.                                                      CM1034.2
            009900     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    CM1034.2
            010000     02 INFO-TEXT.                                                CM1034.2
            010100     04 FILLER PIC X(20) VALUE SPACE.                             CM1034.2
            010200     04 XXCOMPUTED PIC X(20).                                     CM1034.2
            010300     04 FILLER PIC X(5) VALUE SPACE.                              CM1034.2
            010400     04 XXCORRECT PIC X(20).                                      CM1034.2
            010500 01  HYPHEN-LINE.                                                 CM1034.2
            010600     02 FILLER PICTURE IS X VALUE IS SPACE.                       CM1034.2
            010700     02 FILLER PICTURE IS X(65) VALUE IS "************************CM1034.2
            010800-    "*****************************************".                 CM1034.2
            010900     02 FILLER PICTURE IS X(54) VALUE IS "************************CM1034.2
            011000-    "******************************".                            CM1034.2
            011100 01  CCVS-PGM-ID PIC X(6) VALUE                                   CM1034.2
            011200     "CM103M".                                                    CM1034.2
            011300 01  MCS-TIME.                                                    CM1034.2
            011400     02  HRS PIC 99.                                              CM1034.2
            011500     02  MINS PIC 99.                                             CM1034.2
            011600     02  SECS PIC 99V99.                                          CM1034.2
            011700 01  IN-TIME.                                                     CM1034.2
            011800     02  IN-HRS PIC 99.                                           CM1034.2
            011900     02  IN-MINS PIC 99.                                          CM1034.2
            012000     02  IN-SECS PIC 99V99.                                       CM1034.2
            012100 01  OUT-TIME.                                                    CM1034.2
            012200     02  OUT-HRS PIC 99.                                          CM1034.2
            012300     02  OUT-MINS PIC 99.                                         CM1034.2
            012400     02  OUT-SECS PIC 99V99.                                      CM1034.2
            012500 01  LOG-HDR-1.                                                   CM1034.2
            012600     02  FILLER PIC X(54) VALUE SPACES.                           CM1034.2
            012700     02  FILLER PIC X(11) VALUE "MESSAGE LOG".                    CM1034.2
            012800 01  LOG-HDR-2.                                                   CM1034.2
            012900     02  FILLER PIC X VALUE SPACE.                                CM1034.2
            013000     02  FILLER PIC X(12) VALUE "MCS RECEIPT".                    CM1034.2
            013100     02  FILLER PIC X(8) VALUE "PROGRAM".                         CM1034.2
            013200     02  FILLER PIC X(9) VALUE "MCS REC".                         CM1034.2
            013300     02  FILLER PIC X(12) VALUE "RECV SEND".                      CM1034.2
            013400     02  FILLER PIC X(38) VALUE "MSG".                            CM1034.2
            013500     02  FILLER PIC X(7) VALUE "MESSAGE".                         CM1034.2
            013600 01  LOG-HDR-3.                                                   CM1034.2
            013700     02  FILLER PIC XXX VALUE SPACE.                              CM1034.2
            013800     02  FILLER PIC X(10) VALUE "INBOUND".                        CM1034.2
            013900     02  FILLER PIC X(8) VALUE "RECEIPT".                         CM1034.2
            014000     02  FILLER PIC X(9) VALUE "OUTB""ND".                        CM1034.2
            014100     02  FILLER PIC X(11) VALUE "STAT STAT".                      CM1034.2
            014200     02  FILLER PIC X(39) VALUE "LENGTH".                         CM1034.2
            014300     02  FILLER PIC X(7) VALUE "CONTENT".                         CM1034.2
            014400 01  LOG-HDR-4.                                                   CM1034.2
            014500     02  FILLER PIC X VALUE SPACE.                                CM1034.2
            014600     02  FILLER PIC X(11) VALUE ALL "-".                          CM1034.2
            014700     02  FILLER PIC X VALUE SPACE.                                CM1034.2
            014800     02  FILLER PIC X(7) VALUE ALL "-".                           CM1034.2
            014900     02  FILLER PIC X VALUE SPACE.                                CM1034.2
            015000     02  FILLER PIC X(7) VALUE ALL "-".                           CM1034.2
            015100     02  FILLER PIC XX VALUE SPACES.                              CM1034.2
            015200     02  FILLER PIC X(11) VALUE "---- ----".                      CM1034.2
            015300     02  FILLER PIC X(5) VALUE ALL "-".                           CM1034.2
            015400     02  FILLER PIC XX VALUE SPACES.                              CM1034.2
            015500     02  FILLER PIC X(72) VALUE ALL "-".                          CM1034.2
            015600 01  LOG-LINE.                                                    CM1034.2
            015700     02  FILLER PIC X VALUE SPACE.                                CM1034.2
            015800     02  TIME-REC.                                                CM1034.2
            015900         03  HRS PIC 99.                                          CM1034.2
            016000         03  FILLER PIC X VALUE ":".                              CM1034.2
            016100         03  MINS PIC 99.                                         CM1034.2
            016200         03  FILLER PIC X VALUE ":".                              CM1034.2
            016300         03  SECS PIC 99.99.                                      CM1034.2
            016400     02  FILLER PIC X VALUE SPACE.                                CM1034.2
            016500     02  PROG-TIME PIC ---.99.                                    CM1034.2
            016600     02  FILLER PIC XX VALUE SPACES.                              CM1034.2
            016700     02  TIME-SENT PIC ---.99.                                    CM1034.2
            016800     02  FILLER PIC XXXX VALUE SPACES.                            CM1034.2
            016900     02  RECV-STATUS PIC XX.                                      CM1034.2
            017000     02  FILLER PIC XX VALUE SPACES.                              CM1034.2
            017100     02  SEND-STATUS PIC XX.                                      CM1034.2
            017200     02  FILLER PIC X VALUE "/".                                  CM1034.2
            017300     02  SEND-ERR PIC X.                                          CM1034.2
            017400     02  FILLER PIC XXX VALUE SPACES.                             CM1034.2
            017500     02  MSG-LNGTH PIC ZZ9.                                       CM1034.2
            017600     02  FILLER PIC XXX VALUE SPACES.                             CM1034.2
            017700     02  MSG.                                                     CM1034.2
            017800         03  KILL-FIELD PIC X(4).                                 CM1034.2
            017900         03  FILLER PIC X(68).                                    CM1034.2
            018000 COMMUNICATION SECTION.                                           CM1034.2
            018100 CD  CM-INQUE-1 FOR INPUT                                         CM1034.2
            018200     MAIN-QUEUE NO-SPEC-1 NO-SPEC-2 NO-SPEC-3 FILLER TIME-RECEIVEDCM1034.2
            018300     FILLER IN-LENGTH END-KEY IN-STATUS FILLER.                   CM1034.2
            018400 CD  CM-OUTQUE-1 FOR OUTPUT.                                      CM1034.2
            018500 01  OUTQUE-SPECIFICATIONS.                                       CM1034.2
            018600     02  ONE PIC 9999 VALUE IS 1.                                 CM1034.2
            018700     02  OUT-LENGTH PIC 9999.                                     CM1034.2
            018800     02  OUT-STATUS PIC XX.                                       CM1034.2
            018900     02  ERR-KEY PIC X.                                           CM1034.2
            019000     02  SYM-DEST PIC X(12) VALUE IS                              CM1034.2
            019100     XXXXX032.                                                    CM1034.2
            019200 PROCEDURE    DIVISION.                                           CM1034.2
            019300 SECT-CM103M-0001 SECTION.                                        CM1034.2
            019400 CM103M-INIT.                                                     CM1034.2
            019500     OPEN     OUTPUT PRINT-FILE.                                  CM1034.2
            019600     MOVE "CM103M     " TO TEST-ID.                               CM1034.2
            019700     MOVE     TEST-ID TO ID-AGAIN.                                CM1034.2
            019800     MOVE SPACES TO NO-SPEC-1 NO-SPEC-2 NO-SPEC-3.                CM1034.2
            019900     MOVE                                                         CM1034.2
            020000     XXXXX030                                                     CM1034.2
            020100         TO MAIN-QUEUE.                                           CM1034.2
            020200     ENABLE INPUT CM-INQUE-1 WITH KEY                             CM1034.2
            020300     XXXXX031.                                                    CM1034.2
            020400     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1034.2
            020500     XXXXX033.                                                    CM1034.2
            020600     PERFORM HEAD-ROUTINE.                                        CM1034.2
            020700     PERFORM LOG-HEADER.                                          CM1034.2
            020800 RECEIVE-ECHO-AND-LOG.                                            CM1034.2
            020900     MOVE SPACES TO MSG.                                          CM1034.2
            021000     RECEIVE CM-INQUE-1 MESSAGE INTO MSG.                         CM1034.2
            021100     ACCEPT IN-TIME FROM TIME.                                    CM1034.2
            021200     IF IN-LENGTH IS GREATER THAN 72                              CM1034.2
            021300         MOVE 72 TO OUT-LENGTH                                    CM1034.2
            021400         ELSE MOVE IN-LENGTH TO OUT-LENGTH.                       CM1034.2
            021500     SEND CM-OUTQUE-1 FROM MSG WITH EMI.                          CM1034.2
            021600     ACCEPT OUT-TIME FROM TIME.                                   CM1034.2
            021700     MOVE TIME-RECEIVED TO MCS-TIME.                              CM1034.2
            021800     MOVE CORR MCS-TIME TO TIME-REC.                              CM1034.2
            021900     COMPUTE PROG-TIME =                                          CM1034.2
            022000         (IN-HRS * 3600 + IN-MINS * 60 + IN-SECS)  -              CM1034.2
            022100         (HRS OF MCS-TIME * 3600 + MINS OF MCS-TIME * 60 +        CM1034.2
            022200          SECS OF MCS-TIME).                                      CM1034.2
            022300     COMPUTE TIME-SENT =                                          CM1034.2
            022400         (OUT-HRS * 3600 + OUT-MINS * 60 + OUT-SECS)  -           CM1034.2
            022500         (HRS OF MCS-TIME * 3600 + MINS OF MCS-TIME * 60 +        CM1034.2
            022600          SECS OF MCS-TIME).                                      CM1034.2
            022700     MOVE IN-STATUS TO RECV-STATUS.                               CM1034.2
            022800     MOVE OUT-STATUS TO SEND-STATUS.                              CM1034.2
            022900     MOVE ERR-KEY TO SEND-ERR.                                    CM1034.2
            023000     MOVE IN-LENGTH TO MSG-LNGTH.                                 CM1034.2
            023100     MOVE LOG-LINE TO PRINT-REC.                                  CM1034.2
            023200     WRITE PRINT-REC.                                             CM1034.2
            023300     IF KILL-FIELD IS NOT EQUAL TO "KILL"                         CM1034.2
            023400         GO TO RECEIVE-ECHO-AND-LOG.                              CM1034.2
            023500     PERFORM END-ROUTINE THRU END-ROUTINE-3.                      CM1034.2
            023600     CLOSE    PRINT-FILE.                                         CM1034.2
            023700     STOP     RUN.                                                CM1034.2
            023800 END-ROUTINE.                                                     CM1034.2
            023900     MOVE     HYPHEN-LINE TO DUMMY-RECORD.                        CM1034.2
            024000     PERFORM WRITE-LINE.                                          CM1034.2
            024100 PARA-Z.                                                          CM1034.2
            024200     PERFORM  BLANK-LINE-PRINT 4 TIMES.                           CM1034.2
            024300     MOVE     CCVS-E-1 TO DUMMY-RECORD.                           CM1034.2
            024400     PERFORM WRITE-LINE.                                          CM1034.2
            024500 END-ROUTINE-3.                                                   CM1034.2
            024600     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           CM1034.2
            024700     PERFORM WRITE-LINE.                                          CM1034.2
            024800     MOVE CCVS-E-3 TO DUMMY-RECORD.                               CM1034.2
            024900     PERFORM WRITE-LINE.                                          CM1034.2
            025000 BLANK-LINE-PRINT.                                                CM1034.2
            025100     MOVE     SPACE TO DUMMY-RECORD.                              CM1034.2
            025200     PERFORM WRITE-LINE.                                          CM1034.2
            025300 WRITE-LINE.                                                      CM1034.2
            025400     WRITE DUMMY-RECORD AFTER ADVANCING 1 LINE.                   CM1034.2
            025500 LOG-HEADER.                                                      CM1034.2
            025600     MOVE LOG-HDR-1 TO PRINT-REC                                  CM1034.2
            025700     WRITE PRINT-REC                                              CM1034.2
            025800         AFTER 3 LINES.                                           CM1034.2
            025900     MOVE LOG-HDR-2 TO PRINT-REC.                                 CM1034.2
            026000     WRITE PRINT-REC                                              CM1034.2
            026100         AFTER 3 LINES.                                           CM1034.2
            026200     MOVE LOG-HDR-3 TO PRINT-REC.                                 CM1034.2
            026300     WRITE PRINT-REC                                              CM1034.2
            026400     MOVE LOG-HDR-4 TO PRINT-REC.                                 CM1034.2
            026500     PERFORM WRITE-LINE.                                          CM1034.2
            026600     MOVE SPACES TO PRINT-REC.                                    CM1034.2
            026700     PERFORM WRITE-LINE.                                          CM1034.2
            026800 HEAD-ROUTINE.                                                    CM1034.2
            026900     MOVE CCVS-H-1 TO PRINT-REC                                   CM1034.2
            027000     WRITE PRINT-REC                                              CM1034.2
            027100         AFTER ADVANCING PAGE.                                    CM1034.2
            027200     MOVE CCVS-H-2 TO PRINT-REC.                                  CM1034.2
            027300     WRITE PRINT-REC                                              CM1034.2
            027400         AFTER 2 LINES.                                           CM1034.2
            027500     MOVE CCVS-H-3 TO PRINT-REC.                                  CM1034.2
            027600     WRITE PRINT-REC                                              CM1034.2
            027700         AFTER 5 LINES.                                           CM1034.2
            027800     MOVE HYPHEN-LINE TO PRINT-REC.                               CM1034.2
            027900     PERFORM WRITE-LINE.                                          CM1034.2
                  *END-OF,CM103M                                                            
        """)
    )

    @Test
    fun cm1044_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM104M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM1044.2
            000200 PROGRAM-ID.                                                      CM1044.2
            000300     CM104M.                                                      CM1044.2
            000400 AUTHOR.                                                          CM1044.2
            000500     FEDERAL COMPILER TESTING CENTER.                             CM1044.2
            000600 INSTALLATION.                                                    CM1044.2
            000700     GENERAL SERVICES ADMINISTRATION                              CM1044.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM1044.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM1044.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               CM1044.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 CM1044.2
            001200                                                                  CM1044.2
            001300     PHONE   (703) 756-6153                                       CM1044.2
            001400                                                                  CM1044.2
            001500     " HIGH       ".                                              CM1044.2
            001600 DATE-WRITTEN.                                                    CM1044.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           CM1044.2
            001800     CREATION DATE     /    VALIDATION DATE                       CM1044.2
            001900     "4.2 ".                                                      CM1044.2
            002000 SECURITY.                                                        CM1044.2
            002100     NONE.                                                        CM1044.2
            002200 ENVIRONMENT DIVISION.                                            CM1044.2
            002300 CONFIGURATION SECTION.                                           CM1044.2
            002400 SOURCE-COMPUTER.                                                 CM1044.2
            002500     XXXXX082.                                                    CM1044.2
            002600 OBJECT-COMPUTER.                                                 CM1044.2
            002700     XXXXX083.                                                    CM1044.2
            002800 INPUT-OUTPUT SECTION.                                            CM1044.2
            002900 FILE-CONTROL.                                                    CM1044.2
            003000     SELECT PRINT-FILE ASSIGN TO                                  CM1044.2
            003100     XXXXX055.                                                    CM1044.2
            003200 DATA DIVISION.                                                   CM1044.2
            003300 FILE SECTION.                                                    CM1044.2
            003400 FD  PRINT-FILE                                                   CM1044.2
            003500     LABEL RECORDS                                                CM1044.2
            003600     XXXXX084                                                     CM1044.2
            003700     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       CM1044.2
            003800 01  PRINT-REC PICTURE X(120).                                    CM1044.2
            003900 01  DUMMY-RECORD PICTURE X(120).                                 CM1044.2
            004000 WORKING-STORAGE SECTION.                                         CM1044.2
            004100 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         CM1044.2
            004200 01  REC-CT PICTURE 99 VALUE ZERO.                                CM1044.2
            004300 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        CM1044.2
            004400 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  CM1044.2
            004500 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          CM1044.2
            004600 01  PASS-COUNTER PIC 999 VALUE ZERO.                             CM1044.2
            004700 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              CM1044.2
            004800 01  ERROR-HOLD PIC 999 VALUE ZERO.                               CM1044.2
            004900 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           CM1044.2
            005000 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            CM1044.2
            005100 01  CCVS-H-1.                                                    CM1044.2
            005200     02  FILLER   PICTURE X(27)  VALUE SPACE.                     CM1044.2
            005300     02 FILLER PICTURE X(67) VALUE                                CM1044.2
            005400     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  CM1044.2
            005500-    " SYSTEM".                                                   CM1044.2
            005600     02  FILLER     PICTURE X(26)  VALUE SPACE.                   CM1044.2
            005700 01  CCVS-H-2.                                                    CM1044.2
            005800     02 FILLER PICTURE X(52) VALUE IS                             CM1044.2
            005900     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   CM1044.2
            006000     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   CM1044.2
            006100     02 TEST-ID PICTURE IS X(9).                                  CM1044.2
            006200     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   CM1044.2
            006300 01  CCVS-H-3.                                                    CM1044.2
            006400     02  FILLER PICTURE X(34) VALUE                               CM1044.2
            006500     " FOR OFFICIAL USE ONLY    ".                                CM1044.2
            006600     02  FILLER PICTURE X(58) VALUE                               CM1044.2
            006700     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".CM1044.2
            006800     02  FILLER PICTURE X(28) VALUE                               CM1044.2
            006900     "  COPYRIGHT   1974 ".                                       CM1044.2
            007000 01  CCVS-E-1.                                                    CM1044.2
            007100     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   CM1044.2
            007200     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        CM1044.2
            007300     02 ID-AGAIN PICTURE IS X(9).                                 CM1044.2
            007400     02 FILLER PICTURE X(45) VALUE IS                             CM1044.2
            007500     " NTIS DISTRIBUTION COBOL 74".                               CM1044.2
            007600 01  CCVS-E-2.                                                    CM1044.2
            007700     02  FILLER                   PICTURE X(31)  VALUE            CM1044.2
            007800     SPACE.                                                       CM1044.2
            007900     02  FILLER                   PICTURE X(21)  VALUE SPACE.     CM1044.2
            008000     02 CCVS-E-2-2.                                               CM1044.2
            008100         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            CM1044.2
            008200         03 FILLER PICTURE IS X VALUE IS SPACE.                   CM1044.2
            008300         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      CM1044.2
            008400 01  CCVS-E-3.                                                    CM1044.2
            008500     02  FILLER PICTURE X(22) VALUE                               CM1044.2
            008600     " FOR OFFICIAL USE ONLY".                                    CM1044.2
            008700     02  FILLER PICTURE X(12) VALUE SPACE.                        CM1044.2
            008800     02  FILLER PICTURE X(58) VALUE                               CM1044.2
            008900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".CM1044.2
            009000     02  FILLER PICTURE X(13) VALUE SPACE.                        CM1044.2
            009100     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 CM1044.2
            009200 01  CCVS-E-4.                                                    CM1044.2
            009300     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           CM1044.2
            009400     02 FILLER PIC XXXX VALUE " OF ".                             CM1044.2
            009500     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           CM1044.2
            009600     02 FILLER PIC X(40) VALUE                                    CM1044.2
            009700      "  TESTS WERE EXECUTED SUCCESSFULLY".                       CM1044.2
            009800 01  XXINFO.                                                      CM1044.2
            009900     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    CM1044.2
            010000     02 INFO-TEXT.                                                CM1044.2
            010100     04 FILLER PIC X(20) VALUE SPACE.                             CM1044.2
            010200     04 XXCOMPUTED PIC X(20).                                     CM1044.2
            010300     04 FILLER PIC X(5) VALUE SPACE.                              CM1044.2
            010400     04 XXCORRECT PIC X(20).                                      CM1044.2
            010500 01  HYPHEN-LINE.                                                 CM1044.2
            010600     02 FILLER PICTURE IS X VALUE IS SPACE.                       CM1044.2
            010700     02 FILLER PICTURE IS X(65) VALUE IS "************************CM1044.2
            010800-    "*****************************************".                 CM1044.2
            010900     02 FILLER PICTURE IS X(54) VALUE IS "************************CM1044.2
            011000-    "******************************".                            CM1044.2
            011100 01  CCVS-PGM-ID PIC X(6) VALUE                                   CM1044.2
            011200     "CM104M".                                                    CM1044.2
            011300 01  LOG-HDR-1.                                                   CM1044.2
            011400     02  FILLER PIC X(54) VALUE SPACES.                           CM1044.2
            011500     02  FILLER PIC X(11) VALUE "MESSAGE LOG".                    CM1044.2
            011600 01  LOG-HDR-2.                                                   CM1044.2
            011700     02  FILLER PIC XXX VALUE SPACES.                             CM1044.2
            011800     02  FILLER PIC X(12) VALUE "SYMBOLIC".                       CM1044.2
            011900     02  FILLER PIC X(15) VALUE "TIME MCS".                       CM1044.2
            012000     02  FILLER PIC X(6) VALUE "SEND".                            CM1044.2
            012100     02  FILLER PIC X(4) VALUE "MSG".                             CM1044.2
            012200     02  FILLER PIC XXX VALUE "IN".                               CM1044.2
            012300     02  FILLER PIC X(3) VALUE "OUT".                             CM1044.2
            012400 01  LOG-HDR-3.                                                   CM1044.2
            012500     02  FILLER PIC X(4) VALUE SPACES.                            CM1044.2
            012600     02  FILLER PIC X(11) VALUE "SOURCE".                         CM1044.2
            012700     02  FILLER PIC X(11) VALUE "RECEIVED".                       CM1044.2
            012800     02  FILLER PIC XXX VALUE "QD".                               CM1044.2
            012900     02  FILLER PIC X(7) VALUE "COMPLT".                          CM1044.2
            013000     02  FILLER PIC X(4) VALUE "LTH".                             CM1044.2
            013100     02  FILLER PIC XXX VALUE "ST".                               CM1044.2
            013200     02  FILLER PIC X(33) VALUE "STAT".                           CM1044.2
            013300     02  FILLER PIC X(16) VALUE "MESSAGE CONTENTS".               CM1044.2
            013400 01  LOG-HDR-4.                                                   CM1044.2
            013500     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            013600     02  FILLER PIC X(12) VALUE ALL "-".                          CM1044.2
            013700     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            013800     02  FILLER PIC X(11) VALUE ALL "-".                          CM1044.2
            013900     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            014000     02  FILLER PIC XXX VALUE "--".                               CM1044.2
            014100     02  FILLER PIC X(6) VALUE ALL "-".                           CM1044.2
            014200     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            014300     02  FILLER PIC X(4) VALUE "---".                             CM1044.2
            014400     02  FILLER PIC XXX VALUE "--".                               CM1044.2
            014500     02  FILLER PIC X(5) VALUE "----".                            CM1044.2
            014600     02  FILLER PIC X(72) VALUE ALL "-".                          CM1044.2
            014700 01  LOG-LINE.                                                    CM1044.2
            014800     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            014900     02  SYM-SOURCE PIC X(12).                                    CM1044.2
            015000     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            015100     02  LOG-TIME.                                                CM1044.2
            015200         03  HRS PIC 99.                                          CM1044.2
            015300         03  FILLER PIC X VALUE ":".                              CM1044.2
            015400         03  MINS PIC 99.                                         CM1044.2
            015500         03  FILLER PIC X VALUE ":".                              CM1044.2
            015600         03  SECS PIC 99.99.                                      CM1044.2
            015700     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            015800     02  QUEUE-DEPTH PIC Z9.                                      CM1044.2
            015900     02  OUT-TIME PIC -(4).99.                                    CM1044.2
            016000     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            016100     02  MSG-LENGTH PIC ZZ9.                                      CM1044.2
            016200     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            016300     02  IN-STATUS PIC XX.                                        CM1044.2
            016400     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            016500     02  OUT-STATUS PIC XX.                                       CM1044.2
            016600     02  FILLER PIC X VALUE "/".                                  CM1044.2
            016700     02  OUT-ERR-KEY PIC X.                                       CM1044.2
            016800     02  FILLER PIC X VALUE SPACE.                                CM1044.2
            016900     02  MSG.                                                     CM1044.2
            017000         03  KILL-FIELD PIC X(4).                                 CM1044.2
            017100         03  FILLER PIC X(68).                                    CM1044.2
            017200 01  SEND-TIME.                                                   CM1044.2
            017300     02  S-HRS PIC 99.                                            CM1044.2
            017400     02  S-MINS PIC 99.                                           CM1044.2
            017500     02  S-SECS PIC 99V99.                                        CM1044.2
            017600 COMMUNICATION SECTION.                                           CM1044.2
            017700 CD  CM-INQUE-1 FOR INPUT.                                        CM1044.2
            017800 01  INQUE-1-SPECIFICATIONS.                                      CM1044.2
            017900     02  QUEUE-1 PIC X(24) VALUE                                  CM1044.2
            018000     XXXXX030.                                                    CM1044.2
            018100     02  FILLER PIC X(30) VALUE SPACES.                           CM1044.2
            018200     02  TIME-RECEIVED-1.                                         CM1044.2
            018300         03  HRS PIC 99.                                          CM1044.2
            018400         03  MINS PIC 99.                                         CM1044.2
            018500         03  SECS PIC 99V99.                                      CM1044.2
            018600     02  SOURCE-1 PIC X(12).                                      CM1044.2
            018700     02  IN-LENGTH-1 PIC 9(4).                                    CM1044.2
            018800     02  END-KEY-1 PIC X.                                         CM1044.2
            018900     02  IN-STATUS-1 PIC XX.                                      CM1044.2
            019000     02  MSG-COUNT-1 PIC 9(6).                                    CM1044.2
            019100 01  INQUE-1-DUMMY-RECORD PIC X(87).                              CM1044.2
            019200 01  INQUE-1-DUMMY-TABLE.                                         CM1044.2
            019300     02  DUMMY-NAME PIC 9 OCCURS 87 TIMES INDEXED BY I1.          CM1044.2
            019400 CD  CM-OUTQUE-1 FOR OUTPUT.                                      CM1044.2
            019500 01  OUTQUE-1-SPECIFIACTIONS.                                     CM1044.2
            019600     02  DEST-COUNT-1 PIC 9(4) VALUE IS 1.                        CM1044.2
            019700     02  OUT-LENGTH-1 PIC 9(4).                                   CM1044.2
            019800     02  OUT-STATUS-1 PIC XX.                                     CM1044.2
            019900     02  ERR-KEY-1 PIC X.                                         CM1044.2
            020000     02  SYM-DEST-1 PIC X(12) VALUE                               CM1044.2
            020100     XXXXX032.                                                    CM1044.2
            020200 01  OUTQUE-1-DUMMY-RECORD PIC X(23).                             CM1044.2
            020300 01  OUTQUE-1-DUMMY-TABLE.                                        CM1044.2
            020400     02  DUMMY-NAME OCCURS 23 TIMES PIC X.                        CM1044.2
            020500 CD  CM-INQUE-2 FOR INPUT                                         CM1044.2
            020600     FILLER FILLER FILLER FILLER FILLER FILLER SOURCE-2           CM1044.2
            020700     IN-LENGTH-2 END-KEY-2 IN-STATUS-2 MSG-COUNT-2.               CM1044.2
            020800 01  INQUE-2-RECORD.                                              CM1044.2
            020900     02  FILLER PIC X(54) VALUE                                   CM1044.2
            021000     XXXXX034.                                                    CM1044.2
            021100     02  TIME-RECEIVED-2.                                         CM1044.2
            021200         03  HRS PIC 99.                                          CM1044.2
            021300         03  MINS PIC 99.                                         CM1044.2
            021400         03  SECS PIC 99V99.                                      CM1044.2
            021500     02  FILLER PIC X(25).                                        CM1044.2
            021600 CD  CM-OUTQUE-2 FOR OUTPUT                                       CM1044.2
            021700     TEXT LENGTH OUT-LENGTH-2                                     CM1044.2
            021800     STATUS KEY OUT-STATUS-2                                      CM1044.2
            021900     ERROR KEY ERR-KEY-2.                                         CM1044.2
            022000 01  OUTQUE-2-RECORD.                                             CM1044.2
            022100     02  FILLER PIC 9(4) VALUE 1.                                 CM1044.2
            022200     02  FILLER PIC X(7) VALUE SPACES.                            CM1044.2
            022300     02  FILLER PIC X(12) VALUE                                   CM1044.2
            022400     XXXXX035.                                                    CM1044.2
            022500 PROCEDURE    DIVISION.                                           CM1044.2
            022600 SECT-CM104M-0001 SECTION.                                        CM1044.2
            022700 CM104M-INIT.                                                     CM1044.2
            022800     OPEN     OUTPUT PRINT-FILE.                                  CM1044.2
            022900     MOVE "CM104M     " TO TEST-ID.                               CM1044.2
            023000     MOVE     TEST-ID TO ID-AGAIN.                                CM1044.2
            023100     PERFORM HEAD-ROUTINE.                                        CM1044.2
            023200     PERFORM LOG-HEADER.                                          CM1044.2
            023300     ENABLE INPUT CM-INQUE-1 WITH KEY                             CM1044.2
            023400     XXXXX031.                                                    CM1044.2
            023500     ENABLE INPUT CM-INQUE-2 WITH KEY                             CM1044.2
            023600     XXXXX036.                                                    CM1044.2
            023700     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1044.2
            023800     XXXXX033.                                                    CM1044.2
            023900     ENABLE OUTPUT CM-OUTQUE-2 WITH KEY                           CM1044.2
            024000     XXXXX037.                                                    CM1044.2
            024100 CM104M-POLL-1.                                                   CM1044.2
            024200     MOVE SPACES TO MSG.                                          CM1044.2
            024300     RECEIVE CM-INQUE-1 MESSAGE INTO MSG                          CM1044.2
            024400         NO DATA GO TO CM104M-POLL-2.                             CM1044.2
            024500     ACCEPT CM-INQUE-1 COUNT.                                     CM1044.2
            024600     IF IN-LENGTH-1 IS GREATER THAN 72                            CM1044.2
            024700         MOVE 72 TO OUT-LENGTH-2                                  CM1044.2
            024800             ELSE MOVE IN-LENGTH-1 TO OUT-LENGTH-2.               CM1044.2
            024900     SEND CM-OUTQUE-2 FROM MSG WITH EMI.                          CM1044.2
            025000     ACCEPT SEND-TIME FROM TIME.                                  CM1044.2
            025100     MOVE SOURCE-1 TO SYM-SOURCE.                                 CM1044.2
            025200     MOVE CORR TIME-RECEIVED-1 TO LOG-TIME.                       CM1044.2
            025300     COMPUTE OUT-TIME =                                           CM1044.2
            025400         (S-HRS * 3600 + S-MINS * 60 + S-SECS) -                  CM1044.2
            025500         (HRS OF TIME-RECEIVED-1 * 3600 + MINS OF TIME-RECEIVED-1 CM1044.2
            025600          * 60 + SECS OF TIME-RECEIVED-1).                        CM1044.2
            025700     MOVE MSG-COUNT-1 TO QUEUE-DEPTH.                             CM1044.2
            025800     MOVE IN-LENGTH-1 TO MSG-LENGTH.                              CM1044.2
            025900     MOVE IN-STATUS-1 TO IN-STATUS.                               CM1044.2
            026000     MOVE OUT-STATUS-2 TO OUT-STATUS.                             CM1044.2
            026100     MOVE ERR-KEY-2 TO OUT-ERR-KEY.                               CM1044.2
            026200     MOVE LOG-LINE TO PRINT-REC.                                  CM1044.2
            026300     WRITE PRINT-REC.                                             CM1044.2
            026400     IF KILL-FIELD IS EQUAL TO "KILL" GO TO CM104M-FINI.          CM1044.2
            026500 CM104M-POLL-2.                                                   CM1044.2
            026600     MOVE SPACES TO MSG.                                          CM1044.2
            026700     RECEIVE CM-INQUE-2 MESSAGE INTO MSG                          CM1044.2
            026800         NO DATA GO TO CM104M-POLL-1.                             CM1044.2
            026900     ACCEPT CM-INQUE-2 COUNT.                                     CM1044.2
            027000     IF IN-LENGTH-2 IS GREATER THAN 72                            CM1044.2
            027100         MOVE 72 TO OUT-LENGTH-1                                  CM1044.2
            027200             ELSE MOVE IN-LENGTH-2 TO OUT-LENGTH-1.               CM1044.2
            027300     SEND CM-OUTQUE-1 FROM MSG WITH EMI.                          CM1044.2
            027400     ACCEPT SEND-TIME FROM TIME.                                  CM1044.2
            027500     MOVE SOURCE-2 TO SYM-SOURCE.                                 CM1044.2
            027600     MOVE CORR TIME-RECEIVED-2 TO LOG-TIME.                       CM1044.2
            027700     COMPUTE OUT-TIME =                                           CM1044.2
            027800         (S-HRS * 3600 + S-MINS * 60 + S-SECS) -                  CM1044.2
            027900         (HRS OF TIME-RECEIVED-2 * 3600 + MINS OF TIME-RECEIVED-2 CM1044.2
            028000          * 60 + SECS OF TIME-RECEIVED-2).                        CM1044.2
            028100     MOVE MSG-COUNT-2 TO QUEUE-DEPTH.                             CM1044.2
            028200     MOVE IN-LENGTH-2 TO MSG-LENGTH.                              CM1044.2
            028300     MOVE IN-STATUS-2 TO IN-STATUS.                               CM1044.2
            028400     MOVE OUT-STATUS-1 TO OUT-STATUS.                             CM1044.2
            028500     MOVE ERR-KEY-1 TO OUT-ERR-KEY.                               CM1044.2
            028600     MOVE LOG-LINE TO PRINT-REC.                                  CM1044.2
            028700     WRITE PRINT-REC.                                             CM1044.2
            028800     IF KILL-FIELD IS EQUAL TO "KILL" GO TO CM104M-FINI.          CM1044.2
            028900     GO TO CM104M-POLL-1.                                         CM1044.2
            029000 CM104M-FINI.                                                     CM1044.2
            029100     PERFORM END-ROUTINE THRU END-ROUTINE-3.                      CM1044.2
            029200     CLOSE    PRINT-FILE.                                         CM1044.2
            029300     STOP     RUN.                                                CM1044.2
            029400 END-ROUTINE.                                                     CM1044.2
            029500     MOVE     HYPHEN-LINE TO DUMMY-RECORD.                        CM1044.2
            029600     PERFORM WRITE-LINE.                                          CM1044.2
            029700 PARA-Z.                                                          CM1044.2
            029800     PERFORM  BLANK-LINE-PRINT 4 TIMES.                           CM1044.2
            029900     MOVE     CCVS-E-1 TO DUMMY-RECORD.                           CM1044.2
            030000     PERFORM WRITE-LINE.                                          CM1044.2
            030100 END-ROUTINE-3.                                                   CM1044.2
            030200     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           CM1044.2
            030300     PERFORM WRITE-LINE.                                          CM1044.2
            030400     MOVE CCVS-E-3 TO DUMMY-RECORD.                               CM1044.2
            030500     PERFORM WRITE-LINE.                                          CM1044.2
            030600 BLANK-LINE-PRINT.                                                CM1044.2
            030700     MOVE     SPACE TO DUMMY-RECORD.                              CM1044.2
            030800     PERFORM WRITE-LINE.                                          CM1044.2
            030900 WRITE-LINE.                                                      CM1044.2
            031000     WRITE DUMMY-RECORD AFTER ADVANCING 1 LINE.                   CM1044.2
            031100 LOG-HEADER.                                                      CM1044.2
            031200     MOVE LOG-HDR-1 TO PRINT-REC                                  CM1044.2
            031300     WRITE PRINT-REC                                              CM1044.2
            031400         AFTER 3 LINES.                                           CM1044.2
            031500     MOVE LOG-HDR-2 TO PRINT-REC.                                 CM1044.2
            031600     WRITE PRINT-REC                                              CM1044.2
            031700         AFTER 3 LINES.                                           CM1044.2
            031800     MOVE LOG-HDR-3 TO PRINT-REC.                                 CM1044.2
            031900     WRITE PRINT-REC                                              CM1044.2
            032000     MOVE LOG-HDR-4 TO PRINT-REC.                                 CM1044.2
            032100     PERFORM WRITE-LINE.                                          CM1044.2
            032200     MOVE SPACES TO PRINT-REC.                                    CM1044.2
            032300     PERFORM WRITE-LINE.                                          CM1044.2
            032400 HEAD-ROUTINE.                                                    CM1044.2
            032500     MOVE CCVS-H-1 TO PRINT-REC                                   CM1044.2
            032600     WRITE PRINT-REC                                              CM1044.2
            032700         AFTER ADVANCING PAGE.                                    CM1044.2
            032800     MOVE CCVS-H-2 TO PRINT-REC.                                  CM1044.2
            032900     WRITE PRINT-REC                                              CM1044.2
            033000         AFTER 2 LINES.                                           CM1044.2
            033100     MOVE CCVS-H-3 TO PRINT-REC.                                  CM1044.2
            033200     WRITE PRINT-REC                                              CM1044.2
            033300         AFTER 5 LINES.                                           CM1044.2
            033400     MOVE HYPHEN-LINE TO PRINT-REC.                               CM1044.2
            033500     PERFORM WRITE-LINE.                                          CM1044.2
                  *END-OF,CM104M                                                            
        """)
    )

    @Test
    fun cm1054_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM105M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM1054.2
            000200 PROGRAM-ID.                                                      CM1054.2
            000300     CM105M.                                                      CM1054.2
            000400 AUTHOR.                                                          CM1054.2
            000500     FEDERAL COMPILER TESTING CENTER.                             CM1054.2
            000600 INSTALLATION.                                                    CM1054.2
            000700     GENERAL SERVICES ADMINISTRATION                              CM1054.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM1054.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM1054.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               CM1054.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 CM1054.2
            001200                                                                  CM1054.2
            001300     PHONE   (703) 756-6153                                       CM1054.2
            001400                                                                  CM1054.2
            001500     " HIGH       ".                                              CM1054.2
            001600 DATE-WRITTEN.                                                    CM1054.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           CM1054.2
            001800     CREATION DATE     /    VALIDATION DATE                       CM1054.2
            001900     "4.2 ".                                                      CM1054.2
            002000 SECURITY.                                                        CM1054.2
            002100     NONE.                                                        CM1054.2
            002200 ENVIRONMENT DIVISION.                                            CM1054.2
            002300 CONFIGURATION SECTION.                                           CM1054.2
            002400 SOURCE-COMPUTER.                                                 CM1054.2
            002500     XXXXX082.                                                    CM1054.2
            002600 OBJECT-COMPUTER.                                                 CM1054.2
            002700     XXXXX083.                                                    CM1054.2
            002800 INPUT-OUTPUT SECTION.                                            CM1054.2
            002900 FILE-CONTROL.                                                    CM1054.2
            003000     SELECT PRINT-FILE ASSIGN TO                                  CM1054.2
            003100     XXXXX055.                                                    CM1054.2
            003200 DATA DIVISION.                                                   CM1054.2
            003300 FILE SECTION.                                                    CM1054.2
            003400 FD  PRINT-FILE                                                   CM1054.2
            003500     LABEL RECORDS                                                CM1054.2
            003600     XXXXX084                                                     CM1054.2
            003700     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       CM1054.2
            003800 01  PRINT-REC PICTURE X(120).                                    CM1054.2
            003900 01  DUMMY-RECORD PICTURE X(120).                                 CM1054.2
            004000 WORKING-STORAGE SECTION.                                         CM1054.2
            004100 77  P    PIC X(12).                                              CM1054.2
            004200 77  PP   PIC X(24).                                              CM1054.2
            004300 77  PPP  PIC X(36).                                              CM1054.2
            004400 77  PS   PIC X(24).                                              CM1054.2
            004500 77  PSP  PIC X(36).                                              CM1054.2
            004600 77  PPS  PIC X(36).                                              CM1054.2
            004700 01  QUEUE-NAMES.                                                 CM1054.2
            004800     02  PPPP PIC X(48) VALUE                                     CM1054.2
            004900     XXXXX038.                                                    CM1054.2
            005000     02  PPPS PIC X(48) VALUE                                     CM1054.2
            005100     XXXXX039.                                                    CM1054.2
            005200     02  PPSP PIC X(48) VALUE                                     CM1054.2
            005300     XXXXX040.                                                    CM1054.2
            005400     02  PSPP PIC X(48) VALUE                                     CM1054.2
            005500     XXXXX041.                                                    CM1054.2
            005600 01  QUEUE-NAMES-TABLE REDEFINES QUEUE-NAMES.                     CM1054.2
            005700     02  NAME-SET PIC X(48) OCCURS 4 TIMES INDEXED BY I1.         CM1054.2
            005800 01  TEST-RESULTS.                                                CM1054.2
            005900     02 FILLER                    PICTURE X VALUE SPACE.          CM1054.2
            006000     02 FEATURE                   PICTURE X(18).                  CM1054.2
            006100     02 FILLER                    PICTURE X VALUE SPACE.          CM1054.2
            006200     02 P-OR-F                    PICTURE X(5).                   CM1054.2
            006300     02 FILLER                    PICTURE X  VALUE SPACE.         CM1054.2
            006400     02  PAR-NAME PIC X(20).                                      CM1054.2
            006500     02 FILLER                    PICTURE X VALUE SPACE.          CM1054.2
            006600     02  COMPUTED-A.                                              CM1054.2
            006700         03  FILLER PIC X(9) VALUE SPACE.                         CM1054.2
            006800         03  STAT PIC XX.                                         CM1054.2
            006900         03  FILLER PIC X(9) VALUE SPACE.                         CM1054.2
            007000     02 FILLER                    PICTURE X VALUE SPACE.          CM1054.2
            007100     02  CORRECT-A.                                               CM1054.2
            007200         03  FILLER PIC X(8).                                     CM1054.2
            007300         03  CORRECT-QUEUE PIC X(4).                              CM1054.2
            007400         03  FILLER PIC X(8).                                     CM1054.2
            007500     02 FILLER                    PICTURE X VALUE SPACE.          CM1054.2
            007600     02  RE-MARK.                                                 CM1054.2
            007700         03  QUEUE-KEY PIC X(4).                                  CM1054.2
            007800         03  FILLER PIC X(26).                                    CM1054.2
            007900 01  COLUMNS-LINE-1.                                              CM1054.2
            008000     02  FILLER PIC X(3) VALUE SPACES.                            CM1054.2
            008100     02  FILLER PIC X(17) VALUE "FEATURE TESTED".                 CM1054.2
            008200     02  FILLER PIC X(9) VALUE "RESLT".                           CM1054.2
            008300     02  FILLER PIC X(21) VALUE "PARAGRAPH NAME".                 CM1054.2
            008400     02  FILLER PIC X(22) VALUE "COMPUTED DATA".                  CM1054.2
            008500     02  FILLER PIC X(29) VALUE "CORRECT DATA".                   CM1054.2
            008600     02  FILLER PIC X(7) VALUE "REMARKS".                         CM1054.2
            008700 01  COLUMNS-LINE-2.                                              CM1054.2
            008800     02  FILLER PIC X VALUE SPACE.                                CM1054.2
            008900     02  FILLER PIC X(18) VALUE ALL "-".                          CM1054.2
            009000     02  FILLER PIC X VALUE SPACE.                                CM1054.2
            009100     02  FILLER PIC X(5) VALUE ALL "-".                           CM1054.2
            009200     02  FILLER PIC X VALUE SPACE.                                CM1054.2
            009300     02  FILLER PIC X(20) VALUE ALL "-".                          CM1054.2
            009400     02  FILLER PIC X VALUE SPACE.                                CM1054.2
            009500     02  FILLER PIC X(20) VALUE ALL "-".                          CM1054.2
            009600     02  FILLER PIC X VALUE SPACE.                                CM1054.2
            009700     02  FILLER PIC X(20) VALUE ALL "-".                          CM1054.2
            009800     02  FILLER PIC X VALUE SPACE.                                CM1054.2
            009900     02  FILLER PIC X(31) VALUE ALL "-".                          CM1054.2
            010000 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         CM1054.2
            010100 01  REC-CT PICTURE 99 VALUE ZERO.                                CM1054.2
            010200 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        CM1054.2
            010300 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  CM1054.2
            010400 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          CM1054.2
            010500 01  PASS-COUNTER PIC 999 VALUE ZERO.                             CM1054.2
            010600 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              CM1054.2
            010700 01  ERROR-HOLD PIC 999 VALUE ZERO.                               CM1054.2
            010800 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           CM1054.2
            010900 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            CM1054.2
            011000 01  CCVS-H-1.                                                    CM1054.2
            011100     02  FILLER   PICTURE X(27)  VALUE SPACE.                     CM1054.2
            011200     02 FILLER PICTURE X(67) VALUE                                CM1054.2
            011300     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  CM1054.2
            011400-    " SYSTEM".                                                   CM1054.2
            011500     02  FILLER     PICTURE X(26)  VALUE SPACE.                   CM1054.2
            011600 01  CCVS-H-2.                                                    CM1054.2
            011700     02 FILLER PICTURE X(52) VALUE IS                             CM1054.2
            011800     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   CM1054.2
            011900     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   CM1054.2
            012000     02 TEST-ID PICTURE IS X(9).                                  CM1054.2
            012100     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   CM1054.2
            012200 01  CCVS-H-3.                                                    CM1054.2
            012300     02  FILLER PICTURE X(34) VALUE                               CM1054.2
            012400     " FOR OFFICIAL USE ONLY    ".                                CM1054.2
            012500     02  FILLER PICTURE X(58) VALUE                               CM1054.2
            012600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".CM1054.2
            012700     02  FILLER PICTURE X(28) VALUE                               CM1054.2
            012800     "  COPYRIGHT   1974 ".                                       CM1054.2
            012900 01  CCVS-E-1.                                                    CM1054.2
            013000     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   CM1054.2
            013100     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        CM1054.2
            013200     02 ID-AGAIN PICTURE IS X(9).                                 CM1054.2
            013300     02 FILLER PICTURE X(45) VALUE IS                             CM1054.2
            013400     " NTIS DISTRIBUTION COBOL 74".                               CM1054.2
            013500 01  CCVS-E-2.                                                    CM1054.2
            013600     02  FILLER                   PICTURE X(31)  VALUE            CM1054.2
            013700     SPACE.                                                       CM1054.2
            013800     02  FILLER                   PICTURE X(21)  VALUE SPACE.     CM1054.2
            013900     02 CCVS-E-2-2.                                               CM1054.2
            014000         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            CM1054.2
            014100         03 FILLER PICTURE IS X VALUE IS SPACE.                   CM1054.2
            014200         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      CM1054.2
            014300 01  CCVS-E-3.                                                    CM1054.2
            014400     02  FILLER PICTURE X(22) VALUE                               CM1054.2
            014500     " FOR OFFICIAL USE ONLY".                                    CM1054.2
            014600     02  FILLER PICTURE X(12) VALUE SPACE.                        CM1054.2
            014700     02  FILLER PICTURE X(58) VALUE                               CM1054.2
            014800     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".CM1054.2
            014900     02  FILLER PICTURE X(13) VALUE SPACE.                        CM1054.2
            015000     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 CM1054.2
            015100 01  CCVS-E-4.                                                    CM1054.2
            015200     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           CM1054.2
            015300     02 FILLER PIC XXXX VALUE " OF ".                             CM1054.2
            015400     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           CM1054.2
            015500     02 FILLER PIC X(40) VALUE                                    CM1054.2
            015600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       CM1054.2
            015700 01  XXINFO.                                                      CM1054.2
            015800     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    CM1054.2
            015900     02 INFO-TEXT.                                                CM1054.2
            016000     04 FILLER PIC X(20) VALUE SPACE.                             CM1054.2
            016100     04 XXCOMPUTED PIC X(20).                                     CM1054.2
            016200     04 FILLER PIC X(5) VALUE SPACE.                              CM1054.2
            016300     04 XXCORRECT PIC X(20).                                      CM1054.2
            016400 01  HYPHEN-LINE.                                                 CM1054.2
            016500     02 FILLER PICTURE IS X VALUE IS SPACE.                       CM1054.2
            016600     02 FILLER PICTURE IS X(65) VALUE IS "************************CM1054.2
            016700-    "*****************************************".                 CM1054.2
            016800     02 FILLER PICTURE IS X(54) VALUE IS "************************CM1054.2
            016900-    "******************************".                            CM1054.2
            017000 01  CCVS-PGM-ID PIC X(6) VALUE                                   CM1054.2
            017100     "CM105M".                                                    CM1054.2
            017200 01  MAIN-QUEUE-NAME.                                             CM1054.2
            017300     02 MAIN-QUEUE   PIC X(12).                                   CM1054.2
            017400     02 FILLER       PIC X(36) VALUE SPACE.                       CM1054.2
            017500 COMMUNICATION SECTION.                                           CM1054.2
            017600 CD  CM-INQUE-1 INPUT STATUS KEY IS IN-STAT SUB-QUEUE-3           CM1054.2
            017700     IS-OF-NO-INTEREST COUNT NAMED-BELOW SOURCE NOT-USED.         CM1054.2
            017800 01  INQUE-RECORD.                                                CM1054.2
            017900     02  QUEUE-SET PIC X(48).                                     CM1054.2
            018000     02  FILLER PIC X(33).                                        CM1054.2
            018100     02  MSG-COUNT-N PIC 9(6).                                    CM1054.2
            018200     02  MSG-CNT REDEFINES MSG-COUNT-N.                           CM1054.2
            018300         03  FILLER PIC X(4).                                     CM1054.2
            018400         03  MSG-COUNT PIC XX.                                    CM1054.2
            018500 PROCEDURE    DIVISION.                                           CM1054.2
            018600 SECT-CM105M-0001 SECTION.                                        CM1054.2
            018700 CM105M-INIT.                                                     CM1054.2
            018800     OPEN     OUTPUT PRINT-FILE.                                  CM1054.2
            018900     MOVE "CM105M     " TO TEST-ID.                               CM1054.2
            019000     MOVE     TEST-ID TO ID-AGAIN.                                CM1054.2
            019100     MOVE    SPACE TO TEST-RESULTS.                               CM1054.2
            019200     PERFORM HEAD-ROUTINE.                                        CM1054.2
            019300     PERFORM COLUMN-NAMES-ROUTINE.                                CM1054.2
            019400     MOVE                                                         CM1054.2
            019500     XXXXX030                                                     CM1054.2
            019600     TO MAIN-QUEUE.                                               CM1054.2
            019700     MOVE MAIN-QUEUE-NAME TO QUEUE-SET.                           CM1054.2
            019800     ENABLE INPUT CM-INQUE-1 KEY                                  CM1054.2
            019900     XXXXX031.                                                    CM1054.2
            020000     PERFORM BUILD-UP-QUEUES VARYING I1 FROM 1 BY 1               CM1054.2
            020100         UNTIL I1 IS GREATER THAN 4.                              CM1054.2
            020200     GO TO BEGIN-TESTS.                                           CM1054.2
            020300 BUILD-UP-QUEUES.                                                 CM1054.2
            020400     MOVE NAME-SET (I1) TO QUEUE-SET.                             CM1054.2
            020500     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            020600     IF MSG-COUNT IS LESS THAN 10 GO TO BUILD-UP-QUEUES.          CM1054.2
            020700 BEGIN-TESTS.                                                     CM1054.2
            020800     DISABLE INPUT CM-INQUE-1 KEY                                 CM1054.2
            020900     XXXXX031.                                                    CM1054.2
            021000     MOVE PPPP TO  P  PP  PPP.                                    CM1054.2
            021100     MOVE PPSP TO  PPS.                                           CM1054.2
            021200     MOVE PSPP TO  PS  PSP.                                       CM1054.2
            021300 QUEUE-TEST-01.                                                   CM1054.2
            021400     MOVE "QUEUE SERIES PPPP" TO FEATURE.                         CM1054.2
            021500     MOVE PPPP TO QUEUE-SET.                                      CM1054.2
            021600     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            021700     IF QUEUE-KEY IS EQUAL TO "PPPP"                              CM1054.2
            021800         PERFORM PASS GO TO QUEUE-TEST-WRITE-01.                  CM1054.2
            021900     MOVE "PPPP" TO CORRECT-QUEUE.                                CM1054.2
            022000     PERFORM FAIL.                                                CM1054.2
            022100 QUEUE-TEST-WRITE-01.                                             CM1054.2
            022200     MOVE "QUEUE-TEST-01" TO PAR-NAME.                            CM1054.2
            022300     PERFORM PRINT-DETAIL.                                        CM1054.2
            022400 QUEUE-TEST-02.                                                   CM1054.2
            022500     MOVE "QUEUE SERIES PPPS" TO FEATURE.                         CM1054.2
            022600     MOVE PPPS TO QUEUE-SET.                                      CM1054.2
            022700     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            022800     IF QUEUE-KEY IS EQUAL TO "PPPS"                              CM1054.2
            022900         PERFORM PASS   GO TO QUEUE-TEST-WRITE-02.                CM1054.2
            023000     MOVE "PPPS" TO CORRECT-QUEUE.                                CM1054.2
            023100     PERFORM FAIL.                                                CM1054.2
            023200 QUEUE-TEST-WRITE-02.                                             CM1054.2
            023300     MOVE "QUEUE-TEST-02" TO PAR-NAME.                            CM1054.2
            023400     PERFORM PRINT-DETAIL.                                        CM1054.2
            023500 QUEUE-TEST-03.                                                   CM1054.2
            023600     MOVE "QUEUE SERIES PPSP" TO FEATURE.                         CM1054.2
            023700     MOVE PPSP TO QUEUE-SET.                                      CM1054.2
            023800     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            023900     IF QUEUE-KEY IS EQUAL TO "PPSP"                              CM1054.2
            024000         PERFORM PASS   GO TO QUEUE-TEST-WRITE-03.                CM1054.2
            024100     MOVE "PPSP" TO CORRECT-QUEUE.                                CM1054.2
            024200     PERFORM FAIL.                                                CM1054.2
            024300 QUEUE-TEST-WRITE-03.                                             CM1054.2
            024400     MOVE "QUEUE-TEST-03" TO PAR-NAME.                            CM1054.2
            024500     PERFORM PRINT-DETAIL.                                        CM1054.2
            024600 QUEUE-TEST-04.                                                   CM1054.2
            024700     MOVE "QUEUE SERIES PSPP" TO FEATURE.                         CM1054.2
            024800     MOVE PSPP TO QUEUE-SET.                                      CM1054.2
            024900     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            025000     IF QUEUE-KEY IS EQUAL TO "PSPP"                              CM1054.2
            025100         PERFORM PASS   GO TO QUEUE-TEST-WRITE-04.                CM1054.2
            025200     MOVE "PSPP" TO CORRECT-QUEUE.                                CM1054.2
            025300     PERFORM FAIL.                                                CM1054.2
            025400 QUEUE-TEST-WRITE-04.                                             CM1054.2
            025500     MOVE "QUEUE-TEST-04" TO PAR-NAME.                            CM1054.2
            025600     PERFORM PRINT-DETAIL.                                        CM1054.2
            025700 QUEUE-TEST-05.                                                   CM1054.2
            025800     MOVE "QUEUE SERIES P" TO FEATURE.                            CM1054.2
            025900     MOVE P TO QUEUE-SET.                                         CM1054.2
            026000     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            026100     IF QUEUE-KEY IS EQUAL TO "PPPP"                              CM1054.2
            026200         PERFORM PASS   GO TO QUEUE-TEST-WRITE-05.                CM1054.2
            026300     MOVE "PPPP" TO CORRECT-QUEUE.                                CM1054.2
            026400     PERFORM FAIL.                                                CM1054.2
            026500 QUEUE-TEST-WRITE-05.                                             CM1054.2
            026600     MOVE "QUEUE-TEST-05" TO PAR-NAME.                            CM1054.2
            026700     PERFORM PRINT-DETAIL.                                        CM1054.2
            026800 QUEUE-TEST-06.                                                   CM1054.2
            026900     MOVE "QUEUE SERIES PP" TO FEATURE.                           CM1054.2
            027000     MOVE PP TO QUEUE-SET.                                        CM1054.2
            027100     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            027200     IF QUEUE-KEY IS EQUAL TO "PPPP"                              CM1054.2
            027300         PERFORM PASS   GO TO QUEUE-TEST-WRITE-06.                CM1054.2
            027400     MOVE "PPPP" TO CORRECT-QUEUE.                                CM1054.2
            027500     PERFORM FAIL.                                                CM1054.2
            027600 QUEUE-TEST-WRITE-06.                                             CM1054.2
            027700     MOVE "QUEUE-TEST-06" TO PAR-NAME.                            CM1054.2
            027800     PERFORM PRINT-DETAIL.                                        CM1054.2
            027900 QUEUE-TEST-07.                                                   CM1054.2
            028000     MOVE "QUEUE SERIES PPP" TO FEATURE.                          CM1054.2
            028100     MOVE PPP TO QUEUE-SET.                                       CM1054.2
            028200     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            028300     IF QUEUE-KEY IS EQUAL TO "PPPP"                              CM1054.2
            028400         PERFORM PASS   GO TO QUEUE-TEST-WRITE-07.                CM1054.2
            028500     MOVE "PPPP" TO CORRECT-QUEUE.                                CM1054.2
            028600     PERFORM FAIL.                                                CM1054.2
            028700 QUEUE-TEST-WRITE-07.                                             CM1054.2
            028800     MOVE "QUEUE-TEST-07" TO PAR-NAME.                            CM1054.2
            028900     PERFORM PRINT-DETAIL.                                        CM1054.2
            029000 QUEUE-TEST-08.                                                   CM1054.2
            029100     MOVE "QUEUE SERIES PS" TO FEATURE.                           CM1054.2
            029200     MOVE PS TO QUEUE-SET.                                        CM1054.2
            029300     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            029400     IF QUEUE-KEY IS EQUAL TO "PSPP"                              CM1054.2
            029500         PERFORM PASS   GO TO QUEUE-TEST-WRITE-08.                CM1054.2
            029600     MOVE "PSPP" TO CORRECT-QUEUE.                                CM1054.2
            029700     PERFORM FAIL.                                                CM1054.2
            029800 QUEUE-TEST-WRITE-08.                                             CM1054.2
            029900     MOVE "QUEUE-TEST-08" TO PAR-NAME.                            CM1054.2
            030000     PERFORM PRINT-DETAIL.                                        CM1054.2
            030100 QUEUE-TEST-09.                                                   CM1054.2
            030200     MOVE "QUEUE SERIES PSP" TO FEATURE.                          CM1054.2
            030300     MOVE PSP TO QUEUE-SET.                                       CM1054.2
            030400     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            030500     IF QUEUE-KEY IS EQUAL TO "PSPP"                              CM1054.2
            030600         PERFORM PASS   GO TO QUEUE-TEST-WRITE-09.                CM1054.2
            030700     MOVE "PSPP" TO CORRECT-QUEUE.                                CM1054.2
            030800     PERFORM FAIL.                                                CM1054.2
            030900 QUEUE-TEST-WRITE-09.                                             CM1054.2
            031000     MOVE "QUEUE-TEST-09" TO PAR-NAME.                            CM1054.2
            031100     PERFORM PRINT-DETAIL.                                        CM1054.2
            031200 QUEUE-TEST-10.                                                   CM1054.2
            031300     MOVE "QUEUE SERIES PPS" TO FEATURE.                          CM1054.2
            031400     MOVE PPS TO QUEUE-SET.                                       CM1054.2
            031500     PERFORM RECEIVE-A-MSG.                                       CM1054.2
            031600     IF QUEUE-KEY IS EQUAL TO "PPSP"                              CM1054.2
            031700         PERFORM PASS  GO TO QUEUE-TEST-WRITE-10.                 CM1054.2
            031800     MOVE "PPSP" TO CORRECT-QUEUE.                                CM1054.2
            031900     PERFORM FAIL.                                                CM1054.2
            032000 QUEUE-TEST-WRITE-10.                                             CM1054.2
            032100     MOVE "QUEUE-TEST-10" TO PAR-NAME.                            CM1054.2
            032200     PERFORM PRINT-DETAIL.                                        CM1054.2
            032300 ACCEPT-TEST-01.                                                  CM1054.2
            032400     MOVE "ACCEPT GROUP QUEUE" TO FEATURE.                        CM1054.2
            032500     MOVE PPPP TO QUEUE-SET.                                      CM1054.2
            032600     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            032700     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            032800         MOVE IN-STAT TO STAT                                     CM1054.2
            032900         MOVE "BAD STATUS FOR PPPP" TO RE-MARK                    CM1054.2
            033000         ELSE                                                     CM1054.2
            033100         MOVE MSG-COUNT TO STAT                                   CM1054.2
            033200         MOVE "COUNT FOR PPPP" TO RE-MARK.                        CM1054.2
            033300     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            033400     MOVE PPPS TO QUEUE-SET.                                      CM1054.2
            033500     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            033600     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            033700         MOVE IN-STAT TO STAT                                     CM1054.2
            033800         MOVE "BAD STATUS FOR PPPS" TO RE-MARK                    CM1054.2
            033900         ELSE                                                     CM1054.2
            034000         MOVE MSG-COUNT TO STAT                                   CM1054.2
            034100         MOVE "COUNT FOR PPPS" TO RE-MARK.                        CM1054.2
            034200     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            034300     MOVE PPSP TO QUEUE-SET.                                      CM1054.2
            034400     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            034500     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            034600         MOVE IN-STAT TO STAT                                     CM1054.2
            034700         MOVE "BAD STATUS FOR PPSP"  TO RE-MARK                   CM1054.2
            034800         ELSE                                                     CM1054.2
            034900         MOVE MSG-COUNT TO STAT                                   CM1054.2
            035000         MOVE "COUNT FOR PPSP" TO RE-MARK.                        CM1054.2
            035100     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            035200     MOVE PSPP TO QUEUE-SET.                                      CM1054.2
            035300     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            035400     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            035500         MOVE IN-STAT TO STAT                                     CM1054.2
            035600         MOVE "BAD STATUS FOR PSPP" TO RE-MARK                    CM1054.2
            035700         ELSE                                                     CM1054.2
            035800         MOVE MSG-COUNT TO STAT                                   CM1054.2
            035900         MOVE "COUNT FOR PSPP" TO RE-MARK.                        CM1054.2
            036000     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            036100     MOVE P TO QUEUE-SET.                                         CM1054.2
            036200     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            036300     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            036400         MOVE IN-STAT TO STAT                                     CM1054.2
            036500         MOVE "BAD STATUS FOR P" TO RE-MARK                       CM1054.2
            036600         ELSE                                                     CM1054.2
            036700         MOVE MSG-COUNT TO STAT                                   CM1054.2
            036800         MOVE "COUNT FOR P" TO RE-MARK.                           CM1054.2
            036900     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            037000     MOVE PP TO QUEUE-SET.                                        CM1054.2
            037100     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            037200     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            037300         MOVE IN-STAT TO STAT                                     CM1054.2
            037400         MOVE "BAD STATUS FOR PP" TO STAT                         CM1054.2
            037500         ELSE                                                     CM1054.2
            037600         MOVE MSG-COUNT TO STAT                                   CM1054.2
            037700         MOVE "COUNT FOR PP" TO RE-MARK.                          CM1054.2
            037800     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            037900     MOVE PPP TO QUEUE-SET.                                       CM1054.2
            038000     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            038100     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            038200         MOVE IN-STAT TO STAT                                     CM1054.2
            038300         MOVE "BAD STATUS FOR PPP" TO STAT                        CM1054.2
            038400         ELSE                                                     CM1054.2
            038500         MOVE MSG-COUNT TO STAT                                   CM1054.2
            038600         MOVE "COUNT FOR PPP" TO RE-MARK.                         CM1054.2
            038700     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            038800     MOVE PS TO QUEUE-SET.                                        CM1054.2
            038900     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            039000     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            039100         MOVE IN-STAT TO STAT                                     CM1054.2
            039200         MOVE "BAD STATUS FOR PS" TO STAT                         CM1054.2
            039300         ELSE                                                     CM1054.2
            039400         MOVE MSG-COUNT TO STAT                                   CM1054.2
            039500         MOVE "COUNT FOR PS" TO RE-MARK.                          CM1054.2
            039600     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            039700     MOVE PSP TO QUEUE-SET.                                       CM1054.2
            039800     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            039900     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            040000         MOVE IN-STAT TO STAT                                     CM1054.2
            040100         MOVE "BAD STATUS FOR PSP" TO STAT                        CM1054.2
            040200         ELSE                                                     CM1054.2
            040300         MOVE MSG-COUNT TO STAT                                   CM1054.2
            040400         MOVE "COUNT FOR PSP" TO RE-MARK.                         CM1054.2
            040500     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            040600     MOVE PPS TO QUEUE-SET.                                       CM1054.2
            040700     ACCEPT CM-INQUE-1 COUNT.                                     CM1054.2
            040800     IF IN-STAT IS NOT EQUAL TO "00"                              CM1054.2
            040900         MOVE IN-STAT TO STAT                                     CM1054.2
            041000         MOVE "BAD STATUS FOR PPS" TO STAT                        CM1054.2
            041100         ELSE                                                     CM1054.2
            041200         MOVE MSG-COUNT TO STAT                                   CM1054.2
            041300         MOVE "COUNT FOR PPS" TO RE-MARK.                         CM1054.2
            041400     PERFORM ACCEPT-WRITE-01.                                     CM1054.2
            041500     GO TO CM105-FINI.                                            CM1054.2
            041600 ACCEPT-WRITE-01.                                                 CM1054.2
            041700     MOVE "ACCEPT-TEST-01" TO PAR-NAME.                           CM1054.2
            041800     MOVE "INFO" TO P-OR-F.                                       CM1054.2
            041900     PERFORM PRINT-DETAIL.                                        CM1054.2
            042000 CM105-FINI.                                                      CM1054.2
            042100     PERFORM END-ROUTINE THRU END-ROUTINE-4.                      CM1054.2
            042200     CLOSE PRINT-FILE.                                            CM1054.2
            042300     STOP RUN.                                                    CM1054.2
            042400 RECEIVE-A-MSG.                                                   CM1054.2
            042500     MOVE SPACE TO RE-MARK.                                       CM1054.2
            042600     RECEIVE CM-INQUE-1 MESSAGE INTO RE-MARK                      CM1054.2
            042700         NO DATA MOVE "NOTHING RECEIVED FROM MCS" TO RE-MARK.     CM1054.2
            042800 COMMON-SUBROUTINES SECTION.                                      CM1054.2
            042900 PASS.                                                            CM1054.2
            043000     MOVE "PASS" TO P-OR-F.                                       CM1054.2
            043100 FAIL.                                                            CM1054.2
            043200     MOVE " SEE REMARKS COLUMN " TO COMPUTED-A.                   CM1054.2
            043300     ADD      1 TO ERROR-COUNTER.                                 CM1054.2
            043400     MOVE "FAIL*" TO P-OR-F.                                      CM1054.2
            043500 PRINT-DETAIL.                                                    CM1054.2
            043600     MOVE     TEST-RESULTS TO PRINT-REC.                          CM1054.2
            043700     PERFORM WRITE-LINE.                                          CM1054.2
            043800     MOVE     SPACE TO P-OR-F.                                    CM1054.2
            043900     MOVE     SPACE TO COMPUTED-A.                                CM1054.2
            044000     MOVE SPACE TO CORRECT-A.                                     CM1054.2
            044100     MOVE SPACE TO RE-MARK.                                       CM1054.2
            044200 COLUMN-NAMES-ROUTINE.                                            CM1054.2
            044300     MOVE     COLUMNS-LINE-1 TO DUMMY-RECORD.                     CM1054.2
            044400     PERFORM WRITE-LINE.                                          CM1054.2
            044500     MOVE     COLUMNS-LINE-2 TO DUMMY-RECORD.                     CM1054.2
            044600     PERFORM WRITE-LINE.                                          CM1054.2
            044700     PERFORM  BLANK-LINE-PRINT.                                   CM1054.2
            044800 END-ROUTINE.                                                     CM1054.2
            044900     MOVE     HYPHEN-LINE TO DUMMY-RECORD.                        CM1054.2
            045000     PERFORM WRITE-LINE.                                          CM1054.2
            045100 PARA-Z.                                                          CM1054.2
            045200     PERFORM  BLANK-LINE-PRINT 4 TIMES.                           CM1054.2
            045300     MOVE     CCVS-E-1 TO DUMMY-RECORD.                           CM1054.2
            045400     PERFORM WRITE-LINE.                                          CM1054.2
            045500 END-ROUTINE-1.                                                   CM1054.2
            045600     PERFORM  BLANK-LINE-PRINT.                                   CM1054.2
            045700     IF       ERROR-COUNTER IS EQUAL TO ZERO                      CM1054.2
            045800              GO TO END-ROUTINE-2.                                CM1054.2
            045900     MOVE     ERROR-COUNTER TO ERROR-TOTAL.                       CM1054.2
            046000     GO TO    END-ROUTINE-3.                                      CM1054.2
            046100 END-ROUTINE-2.                                                   CM1054.2
            046200     MOVE " NO" TO ERROR-TOTAL.                                   CM1054.2
            046300 END-ROUTINE-3.                                                   CM1054.2
            046400     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           CM1054.2
            046500     PERFORM WRITE-LINE.                                          CM1054.2
            046600     IF DELETE-CNT IS EQUAL TO ZERO                               CM1054.2
            046700         MOVE " NO" TO ERROR-TOTAL  ELSE                          CM1054.2
            046800     MOVE DELETE-CNT TO ERROR-TOTAL.                              CM1054.2
            046900     MOVE "TESTS DELETED     " TO ENDER-DESC.                     CM1054.2
            047000     MOVE CCVS-E-2 TO DUMMY-RECORD.                               CM1054.2
            047100     PERFORM WRITE-LINE.                                          CM1054.2
            047200 END-ROUTINE-4.                                                   CM1054.2
            047300     MOVE CCVS-E-3 TO DUMMY-RECORD.                               CM1054.2
            047400     PERFORM WRITE-LINE.                                          CM1054.2
            047500 BLANK-LINE-PRINT.                                                CM1054.2
            047600     MOVE     SPACE TO DUMMY-RECORD.                              CM1054.2
            047700     PERFORM WRITE-LINE.                                          CM1054.2
            047800 WRITE-LINE.                                                      CM1054.2
            047900     WRITE DUMMY-RECORD AFTER ADVANCING 1 LINE.                   CM1054.2
            048000 HEAD-ROUTINE.                                                    CM1054.2
            048100     MOVE CCVS-H-1 TO PRINT-REC                                   CM1054.2
            048200     WRITE PRINT-REC                                              CM1054.2
            048300         AFTER ADVANCING PAGE.                                    CM1054.2
            048400     MOVE CCVS-H-2 TO PRINT-REC.                                  CM1054.2
            048500     WRITE PRINT-REC                                              CM1054.2
            048600         AFTER 2 LINES.                                           CM1054.2
            048700     MOVE CCVS-H-3 TO PRINT-REC.                                  CM1054.2
            048800     WRITE PRINT-REC                                              CM1054.2
            048900         AFTER 5 LINES.                                           CM1054.2
            049000     MOVE HYPHEN-LINE TO PRINT-REC.                               CM1054.2
            049100     PERFORM WRITE-LINE.                                          CM1054.2
                  *END-OF,CM105M                                                            
        """)
    )

    @Test
    fun cm2014_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM201M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM2014.2
            000200 PROGRAM-ID.                                                      CM2014.2
            000300     CM201M.                                                      CM2014.2
            000400 AUTHOR.                                                          CM2014.2
            000500     FEDERAL COMPILER TESTING CENTER.                             CM2014.2
            000600 INSTALLATION.                                                    CM2014.2
            000700     GENERAL SERVICES ADMINISTRATION                              CM2014.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM2014.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM2014.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               CM2014.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 CM2014.2
            001200                                                                  CM2014.2
            001300     PHONE   (703) 756-6153                                       CM2014.2
            001400                                                                  CM2014.2
            001500     " HIGH       ".                                              CM2014.2
            001600 DATE-WRITTEN.                                                    CM2014.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           CM2014.2
            001800     CREATION DATE     /    VALIDATION DATE                       CM2014.2
            001900     "4.2 ".                                                      CM2014.2
            002000 SECURITY.                                                        CM2014.2
            002100     NONE.                                                        CM2014.2
            002200 ENVIRONMENT DIVISION.                                            CM2014.2
            002300 CONFIGURATION SECTION.                                           CM2014.2
            002400 SOURCE-COMPUTER.                                                 CM2014.2
            002500     XXXXX082.                                                    CM2014.2
            002600 OBJECT-COMPUTER.                                                 CM2014.2
            002700     XXXXX083.                                                    CM2014.2
            002800 DATA DIVISION.                                                   CM2014.2
            002900 WORKING-STORAGE SECTION.                                         CM2014.2
            003000 77  MSG-72 PIC X(72).                                            CM2014.2
            003100 01  RECOGNITION-MSG-1.                                           CM2014.2
            003200     02  FILLER PIC X(32) VALUE                                   CM2014.2
            003300         "RECEIPT OF MESSAGE FROM DEVICE ""${'"'}.                     CM2014.2
            003400     02 DEVICE-NAME PIC X(12).                                    CM2014.2
            003500     02  FILLER PIC X(18) VALUE                                   CM2014.2
            003600         ""${'"'} IS ACKNOWLEDGED.".                                   CM2014.2
            003700 01  RECOGNITION-MSG-2.                                           CM2014.2
            003800     02  FILLER PIC X(20) VALUE                                   CM2014.2
            003900         "QUEUE INVOLVED WAS ""${'"'}.                                 CM2014.2
            004000     02  QUEUE-INVOLVED PIC X(48).                                CM2014.2
            004100     02  FILLER PIC XX VALUE ""${'"'}.".                               CM2014.2
            004200 01  RECOGNITION-MSG-3 PIC X(41) VALUE                            CM2014.2
            004300     "CM201M INVOKED BUT NO DATA WAS AVAILABLE.".                 CM2014.2
            004400 COMMUNICATION SECTION.                                           CM2014.2
            004500 CD  CM-INQUE-1 FOR INITIAL INPUT.                                CM2014.2
            004600 01  INQUE-1-RECORD.                                              CM2014.2
            004700     02  QUEUE-SET PIC X(48).                                     CM2014.2
            004800     02  FILLER PIC X(14).                                        CM2014.2
            004900     02  SYM-SOURCE PIC X(12).                                    CM2014.2
            005000     02  IN-LENGTH PIC 9(4).                                      CM2014.2
            005100     02  FILLER PIC XXX.                                          CM2014.2
            005200     02  MSG-COUNT PIC 9(6).                                      CM2014.2
            005300 CD  CM-OUTQUE-1 FOR OUTPUT.                                      CM2014.2
            005400 01  OUTQUE-1-RECORD.                                             CM2014.2
            005500     02  FILLER PIC 9999 VALUE 1.                                 CM2014.2
            005600     02  OUT-LENGTH PIC 9999.                                     CM2014.2
            005700     02  FILLER PIC XXX.                                          CM2014.2
            005800     02  SYM-DEST PIC X(12) VALUE                                 CM2014.2
            005900     XXXXX032.                                                    CM2014.2
            006000 PROCEDURE    DIVISION.                                           CM2014.2
            006100 SECT-CM201M-0001 SECTION.                                        CM2014.2
            006200 CM201M-INIT.                                                     CM2014.2
            006300     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM2014.2
            006400     XXXXX033.                                                    CM2014.2
            006500 TAKE-NEXT-MSG.                                                   CM2014.2
            006600     RECEIVE CM-INQUE-1 MESSAGE INTO MSG-72                       CM2014.2
            006700         NO DATA GO TO SOMETHING-IS-WRONG-HERE.                   CM2014.2
            006800     MOVE QUEUE-SET TO QUEUE-INVOLVED.                            CM2014.2
            006900     MOVE SYM-SOURCE TO DEVICE-NAME.                              CM2014.2
            007000     MOVE 62 TO OUT-LENGTH.                                       CM2014.2
            007100     SEND CM-OUTQUE-1 FROM RECOGNITION-MSG-1 WITH EMI.            CM2014.2
            007200     MOVE 70 TO OUT-LENGTH.                                       CM2014.2
            007300     SEND CM-OUTQUE-1 FROM RECOGNITION-MSG-2 WITH EMI.            CM2014.2
            007400     MOVE IN-LENGTH TO OUT-LENGTH.                                CM2014.2
            007500     SEND CM-OUTQUE-1 FROM MSG-72 WITH EGI.                       CM2014.2
            007600     ACCEPT CM-INQUE-1 MESSAGE COUNT.                             CM2014.2
            007700     IF MSG-COUNT IS EQUAL TO 0 STOP RUN                          CM2014.2
            007800         ELSE GO TO TAKE-NEXT-MSG.                                CM2014.2
            007900 SOMETHING-IS-WRONG-HERE.                                         CM2014.2
            008000     MOVE 40 TO OUT-LENGTH.                                       CM2014.2
            008100     SEND CM-OUTQUE-1 FROM RECOGNITION-MSG-3 WITH EMI.            CM2014.2
            008200     MOVE QUEUE-SET TO QUEUE-INVOLVED.                            CM2014.2
            008300     MOVE 70 TO OUT-LENGTH.                                       CM2014.2
            008400     SEND CM-OUTQUE-1 FROM RECOGNITION-MSG-2 WITH EGI.            CM2014.2
            008500     STOP RUN.                                                    CM2014.2
                  *END-OF,CM201M                                                            
        """)
    )

    @Test
    fun cm2024_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM202M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM2024.2
            000200 PROGRAM-ID.                                                      CM2024.2
            000300     CM202M.                                                      CM2024.2
            000400 AUTHOR.                                                          CM2024.2
            000500     FEDERAL COMPILER TESTING CENTER.                             CM2024.2
            000600 INSTALLATION.                                                    CM2024.2
            000700     GENERAL SERVICES ADMINISTRATION                              CM2024.2
            000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM2024.2
            000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM2024.2
            001000     5203 LEESBURG PIKE  SUITE 1100                               CM2024.2
            001100     FALLS CHURCH VIRGINIA 22041.                                 CM2024.2
            001200                                                                  CM2024.2
            001300     PHONE   (703) 756-6153                                       CM2024.2
            001400                                                                  CM2024.2
            001500     " HIGH       ".                                              CM2024.2
            001600 DATE-WRITTEN.                                                    CM2024.2
            001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.                           CM2024.2
            001800     CREATION DATE     /    VALIDATION DATE                       CM2024.2
            001900     "4.2 ".                                                      CM2024.2
            002000 SECURITY.                                                        CM2024.2
            002100     NONE.                                                        CM2024.2
            002200 ENVIRONMENT DIVISION.                                            CM2024.2
            002300 CONFIGURATION SECTION.                                           CM2024.2
            002400 SOURCE-COMPUTER.                                                 CM2024.2
            002500     XXXXX082.                                                    CM2024.2
            002600 OBJECT-COMPUTER.                                                 CM2024.2
            002700     XXXXX083.                                                    CM2024.2
            002800 INPUT-OUTPUT SECTION.                                            CM2024.2
            002900 FILE-CONTROL.                                                    CM2024.2
            003000     SELECT PRINT-FILE ASSIGN TO                                  CM2024.2
            003100     XXXXX055.                                                    CM2024.2
            003200 DATA DIVISION.                                                   CM2024.2
            003300 FILE SECTION.                                                    CM2024.2
            003400 FD  PRINT-FILE                                                   CM2024.2
            003500     LABEL RECORDS                                                CM2024.2
            003600     XXXXX084                                                     CM2024.2
            003700     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       CM2024.2
            003800 01  PRINT-REC PICTURE X(120).                                    CM2024.2
            003900 01  DUMMY-RECORD PICTURE X(120).                                 CM2024.2
            004000 WORKING-STORAGE SECTION.                                         CM2024.2
            004100 77  END-FLAG PIC 9.                                              CM2024.2
            004200 77  FAIL-SAFE PIC 999 COMP.                                      CM2024.2
            004300 77  ELAPSED-TIME PIC 999.                                        CM2024.2
            004400 77  ENABLE-ALL-MSG PIC X(57) VALUE                               CM2024.2
            004500     "ATTEMPT TO ENTER MESSAGES.  BOTH SOURCES ARE NOW ENABLED.". CM2024.2
            004600 01  INIT-TIME.                                                   CM2024.2
            004700     02  I-HRS PIC 99.                                            CM2024.2
            004800     02  I-MINS PIC 99.                                           CM2024.2
            004900     02  I-SECS PIC 99V99.                                        CM2024.2
            005000 01  TEST-TIME.                                                   CM2024.2
            005100     02  T-HRS PIC 99.                                            CM2024.2
            005200     02  T-MINS PIC 99.                                           CM2024.2
            005300     02  T-SECS PIC 99V99.                                        CM2024.2
            005400 01  SKIP-MSG-1 PIC X(71) VALUE "SEGMENTED-MSG-TEST-05: THERE SHOUCM2024.2
            005500-    "LD BE NO BLANK LINES BETWEEN THIS LINE".                    CM2024.2
            005600 01  SKIP-MSG-2 PIC X(17) VALUE "AND THIS ONE.".                  CM2024.2
            005700 01  INCOMP-MSG.                                                  CM2024.2
            005800     02  FILLER PIC X(20) VALUE "INCOMPLETE-MSG-TEST-".           CM2024.2
            005900     02  INC-MSG-NO PIC 99.                                       CM2024.2
            006000     02  FILLER PIC X(33) VALUE " FAILURE IF THIS SEGMENT APPEARS.CM2024.2
            006100-    "".                                                          CM2024.2
            006200 01  ENABLE-MSG.                                                  CM2024.2
            006300     02  FILLER PIC X(34) VALUE "ATTEMPT TO ENTER MESSAGES.  ONLY CM2024.2
            006400-    ""${'"'}${'"'}.                                                        CM2024.2
            006500     02  SELECTED-SOURCE PIC X(12).                               CM2024.2
            006600     02  FILLER PIC X(13) VALUE ""${'"'} IS ENABLED.".                 CM2024.2
            006700 01  DISABLE-MSG.                                                 CM2024.2
            006800     02  FILLER PIC X(36) VALUE                                   CM2024.2
            006900     "TERMINALS DEACTIVATED FOR INPUT:    ".                      CM2024.2
            007000     02  TERMINAL-1 PIC X(14) VALUE                               CM2024.2
            007100     XXXXX042.                                                    CM2024.2
            007200     02  TERMINAL-2 PIC X(12) VALUE                               CM2024.2
            007300     XXXXX043.                                                    CM2024.2
            007400 01  SEND-MSG.                                                    CM2024.2
            007500     02  FILLER PIC X(17) VALUE "SEND-STATUS-TEST-".              CM2024.2
            007600     02  TEST-NUMB PIC 99 VALUE 1.                                CM2024.2
            007700     02  FILLER PIC X(42) VALUE                                   CM2024.2
            007800     " FAILURE.  THIS MESSAGE SHOULD NOT APPEAR.".                CM2024.2
            007900 01  ONE-TERMINAL-MSG.                                            CM2024.2
            008000     02  FILLER PIC X(15) VALUE "ONLY TERMINAL ""${'"'}.               CM2024.2
            008100     02  SELECTED-DEST PIC X(12) VALUE                            CM2024.2
            008200     XXXXX032.                                                    CM2024.2
            008300     02  FILLER PIC X(30) VALUE                                   CM2024.2
            008400     ""${'"'} SHOULD RECEIVE THIS MESSAGE.".                           CM2024.2
            008500 01  SEG-INIT.                                                    CM2024.2
            008600     02  FILLER PIC X(19) VALUE "SEGMENTED-MSG-TEST-".            CM2024.2
            008700     02  SEG-TEST-NO PIC 99.                                      CM2024.2
            008800     02  FILLER PIC X(19) VALUE " SEGMENT INITIATED ".            CM2024.2
            008900     02  FILLER PIC X(16) VALUE "-LENGTH FAILURE ".               CM2024.2
            009000 01  SEG-CONT PIC X(10) VALUE "-CONTINUED".                       CM2024.2
            009100 01  MSG-COMP PIC X(14) VALUE "-MSG COMPLETE.".                   CM2024.2
            009200 01  GROUP-COMP PIC X(16) VALUE "-GROUP COMPLETE.".               CM2024.2
            009300 01  TEST-RESULTS.                                                CM2024.2
            009400     02 FILLER                    PICTURE X VALUE SPACE.          CM2024.2
            009500     02 FEATURE                   PICTURE X(18).                  CM2024.2
            009600     02 FILLER                    PICTURE X VALUE SPACE.          CM2024.2
            009700     02 P-OR-F                    PICTURE X(5).                   CM2024.2
            009800     02 FILLER                    PICTURE X  VALUE SPACE.         CM2024.2
            009900     02  PAR-NAME PIC X(20).                                      CM2024.2
            010000     02 FILLER                    PICTURE X VALUE SPACE.          CM2024.2
            010100     02  COMPUTED-A.                                              CM2024.2
            010200         03  FILLER PIC X(9).                                     CM2024.2
            010300         03  COMPUTED-STATUS PIC XX.                              CM2024.2
            010400         03  FILLER PIC X(9).                                     CM2024.2
            010500     02 FILLER                    PICTURE X VALUE SPACE.          CM2024.2
            010600     02  CORRECT-A.                                               CM2024.2
            010700         03  FILLER PIC X(9).                                     CM2024.2
            010800         03  CORRECT-STATUS PIC XX.                               CM2024.2
            010900         03  FILLER PIC X(9).                                     CM2024.2
            011000     02 FILLER                    PICTURE X VALUE SPACE.          CM2024.2
            011100     02  RE-MARK.                                                 CM2024.2
            011200         03  MSG-1 PIC X.                                         CM2024.2
            011300         03  FILLER PIC X(29).                                    CM2024.2
            011400 01  COLUMNS-LINE-1.                                              CM2024.2
            011500     02  FILLER PIC X(3) VALUE SPACES.                            CM2024.2
            011600     02  FILLER PIC X(17) VALUE "FEATURE TESTED".                 CM2024.2
            011700     02  FILLER PIC X(9) VALUE "RESLT".                           CM2024.2
            011800     02  FILLER PIC X(21) VALUE "PARAGRAPH NAME".                 CM2024.2
            011900     02  FILLER PIC X(22) VALUE "COMPUTED DATA".                  CM2024.2
            012000     02  FILLER PIC X(29) VALUE "CORRECT DATA".                   CM2024.2
            012100     02  FILLER PIC X(7) VALUE "REMARKS".                         CM2024.2
            012200 01  COLUMNS-LINE-2.                                              CM2024.2
            012300     02  FILLER PIC X VALUE SPACE.                                CM2024.2
            012400     02  FILLER PIC X(18) VALUE ALL "-".                          CM2024.2
            012500     02  FILLER PIC X VALUE SPACE.                                CM2024.2
            012600     02  FILLER PIC X(5) VALUE ALL "-".                           CM2024.2
            012700     02  FILLER PIC X VALUE SPACE.                                CM2024.2
            012800     02  FILLER PIC X(20) VALUE ALL "-".                          CM2024.2
            012900     02  FILLER PIC X VALUE SPACE.                                CM2024.2
            013000     02  FILLER PIC X(20) VALUE ALL "-".                          CM2024.2
            013100     02  FILLER PIC X VALUE SPACE.                                CM2024.2
            013200     02  FILLER PIC X(20) VALUE ALL "-".                          CM2024.2
            013300     02  FILLER PIC X VALUE SPACE.                                CM2024.2
            013400     02  FILLER PIC X(31) VALUE ALL "-".                          CM2024.2
            013500 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         CM2024.2
            013600 01  REC-CT PICTURE 99 VALUE ZERO.                                CM2024.2
            013700 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        CM2024.2
            013800 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  CM2024.2
            013900 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          CM2024.2
            014000 01  PASS-COUNTER PIC 999 VALUE ZERO.                             CM2024.2
            014100 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              CM2024.2
            014200 01  ERROR-HOLD PIC 999 VALUE ZERO.                               CM2024.2
            014300 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           CM2024.2
            014400 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            CM2024.2
            014500 01  CCVS-H-1.                                                    CM2024.2
            014600     02  FILLER   PICTURE X(27)  VALUE SPACE.                     CM2024.2
            014700     02 FILLER PICTURE X(67) VALUE                                CM2024.2
            014800     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  CM2024.2
            014900-    " SYSTEM".                                                   CM2024.2
            015000     02  FILLER     PICTURE X(26)  VALUE SPACE.                   CM2024.2
            015100 01  CCVS-H-2.                                                    CM2024.2
            015200     02 FILLER PICTURE X(52) VALUE IS                             CM2024.2
            015300     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   CM2024.2
            015400     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   CM2024.2
            015500     02 TEST-ID PICTURE IS X(9).                                  CM2024.2
            015600     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   CM2024.2
            015700 01  CCVS-H-3.                                                    CM2024.2
            015800     02  FILLER PICTURE X(34) VALUE                               CM2024.2
            015900     " FOR OFFICIAL USE ONLY    ".                                CM2024.2
            016000     02  FILLER PICTURE X(58) VALUE                               CM2024.2
            016100     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".CM2024.2
            016200     02  FILLER PICTURE X(28) VALUE                               CM2024.2
            016300     "  COPYRIGHT   1974 ".                                       CM2024.2
            016400 01  CCVS-E-1.                                                    CM2024.2
            016500     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   CM2024.2
            016600     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        CM2024.2
            016700     02 ID-AGAIN PICTURE IS X(9).                                 CM2024.2
            016800     02 FILLER PICTURE X(45) VALUE IS                             CM2024.2
            016900     " NTIS DISTRIBUTION COBOL 74".                               CM2024.2
            017000 01  CCVS-E-2.                                                    CM2024.2
            017100     02  FILLER                   PICTURE X(31)  VALUE            CM2024.2
            017200     SPACE.                                                       CM2024.2
            017300     02  FILLER                   PICTURE X(21)  VALUE SPACE.     CM2024.2
            017400     02 CCVS-E-2-2.                                               CM2024.2
            017500         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            CM2024.2
            017600         03 FILLER PICTURE IS X VALUE IS SPACE.                   CM2024.2
            017700         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      CM2024.2
            017800 01  CCVS-E-3.                                                    CM2024.2
            017900     02  FILLER PICTURE X(22) VALUE                               CM2024.2
            018000     " FOR OFFICIAL USE ONLY".                                    CM2024.2
            018100     02  FILLER PICTURE X(12) VALUE SPACE.                        CM2024.2
            018200     02  FILLER PICTURE X(58) VALUE                               CM2024.2
            018300     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".CM2024.2
            018400     02  FILLER PICTURE X(13) VALUE SPACE.                        CM2024.2
            018500     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 CM2024.2
            018600 01  CCVS-E-4.                                                    CM2024.2
            018700     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           CM2024.2
            018800     02 FILLER PIC XXXX VALUE " OF ".                             CM2024.2
            018900     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           CM2024.2
            019000     02 FILLER PIC X(40) VALUE                                    CM2024.2
            019100      "  TESTS WERE EXECUTED SUCCESSFULLY".                       CM2024.2
            019200 01  XXINFO.                                                      CM2024.2
            019300     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    CM2024.2
            019400     02 INFO-TEXT.                                                CM2024.2
            019500     04 FILLER PIC X(20) VALUE SPACE.                             CM2024.2
            019600     04 XXCOMPUTED PIC X(20).                                     CM2024.2
            019700     04 FILLER PIC X(5) VALUE SPACE.                              CM2024.2
            019800     04 XXCORRECT PIC X(20).                                      CM2024.2
            019900 01  HYPHEN-LINE.                                                 CM2024.2
            020000     02 FILLER PICTURE IS X VALUE IS SPACE.                       CM2024.2
            020100     02 FILLER PICTURE IS X(65) VALUE IS "************************CM2024.2
            020200-    "*****************************************".                 CM2024.2
            020300     02 FILLER PICTURE IS X(54) VALUE IS "************************CM2024.2
            020400-    "******************************".                            CM2024.2
            020500 01  CCVS-PGM-ID PIC X(6) VALUE                                   CM2024.2
            020600     "CM202M".                                                    CM2024.2
            020700 COMMUNICATION SECTION.                                           CM2024.2
            020800 CD  CM-INQUE-1 INPUT.                                            CM2024.2
            020900 01  INQUE-1-RECORD.                                              CM2024.2
            021000     02  QUEUE-SET PIC X(12) VALUE                                CM2024.2
            021100     XXXXX030.                                                    CM2024.2
            021200     02  FILLER PIC X(36) VALUE SPACES.                           CM2024.2
            021300     02  FILLER PIC X(14).                                        CM2024.2
            021400     02  SYM-SOURCE PIC X(12).                                    CM2024.2
            021500     02  IN-LENGTH PIC 9999.                                      CM2024.2
            021600     02  END-KEY PIC X.                                           CM2024.2
            021700     02  IN-STATUS PIC XX.                                        CM2024.2
            021800     02  MSG-COUNT PIC 9(6).                                      CM2024.2
            021900 CD  CM-OUTQUE-1 OUTPUT                                           CM2024.2
            022000     DESTINATION COUNT DEST-COUNT                                 CM2024.2
            022100     TEXT LENGTH OUT-LENGTH                                       CM2024.2
            022200     STATUS KEY OUT-STATUS                                        CM2024.2
            022300     DESTINATION TABLE OCCURS 2 TIMES INDEXED BY I1               CM2024.2
            022400     ERROR KEY ERR-KEY                                            CM2024.2
            022500     DESTINATION SYM-DEST.                                        CM2024.2
            022600 PROCEDURE    DIVISION.                                           CM2024.2
            022700 SECT-CM202M-0001 SECTION.                                        CM2024.2
            022800 CM202M-INIT.                                                     CM2024.2
            022900     OPEN     OUTPUT PRINT-FILE.                                  CM2024.2
            023000     MOVE "CM202M     " TO TEST-ID.                               CM2024.2
            023100     MOVE     TEST-ID TO ID-AGAIN.                                CM2024.2
            023200     MOVE    SPACE TO TEST-RESULTS.                               CM2024.2
            023300     PERFORM HEAD-ROUTINE.                                        CM2024.2
            023400     MOVE 2 TO DEST-COUNT                                         CM2024.2
            023500     MOVE                                                         CM2024.2
            023600     XXXXX032                                                     CM2024.2
            023700         TO SYM-DEST (1).                                         CM2024.2
            023800     MOVE                                                         CM2024.2
            023900     XXXXX035                                                     CM2024.2
            024000         TO SYM-DEST (2).                                         CM2024.2
            024100     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM2024.2
            024200     XXXXX033.                                                    CM2024.2
            024300     MOVE                                                         CM2024.2
            024400     XXXXX042                                                     CM2024.2
            024500         TO SELECTED-SOURCE  SYM-SOURCE.                          CM2024.2
            024600     MOVE 59 TO OUT-LENGTH.                                       CM2024.2
            024700     SEND CM-OUTQUE-1 FROM ENABLE-MSG WITH EMI.                   CM2024.2
            024800     ENABLE INPUT TERMINAL CM-INQUE-1 WITH KEY                    CM2024.2
            024900     XXXXX031.                                                    CM2024.2
            025000     PERFORM DELAY-FOR-30.                                        CM2024.2
            025100     DISABLE INPUT TERMINAL CM-INQUE-1 WITH KEY                   CM2024.2
            025200     XXXXX031.                                                    CM2024.2
            025300     MOVE                                                         CM2024.2
            025400     XXXXX043                                                     CM2024.2
            025500         TO SELECTED-SOURCE  SYM-SOURCE.                          CM2024.2
            025600     SEND CM-OUTQUE-1 FROM ENABLE-MSG WITH EMI.                   CM2024.2
            025700     ENABLE INPUT TERMINAL CM-INQUE-1 WITH KEY                    CM2024.2
            025800     XXXXX031.                                                    CM2024.2
            025900     PERFORM DELAY-FOR-30.                                        CM2024.2
            026000     MOVE 57 TO OUT-LENGTH.                                       CM2024.2
            026100     SEND CM-OUTQUE-1 FROM ENABLE-ALL-MSG WITH EMI.               CM2024.2
            026200     MOVE                                                         CM2024.2
            026300     XXXXX042                                                     CM2024.2
            026400         TO SYM-SOURCE.                                           CM2024.2
            026500     ENABLE INPUT TERMINAL CM-INQUE-1 WITH KEY                    CM2024.2
            026600     XXXXX031.                                                    CM2024.2
            026700     PERFORM DELAY-FOR-30.                                        CM2024.2
            026800     DISABLE INPUT TERMINAL CM-INQUE-1 WITH KEY                   CM2024.2
            026900     XXXXX031.                                                    CM2024.2
            027000     MOVE                                                         CM2024.2
            027100     XXXXX043                                                     CM2024.2
            027200         TO SYM-SOURCE.                                           CM2024.2
            027300     DISABLE INPUT TERMINAL CM-INQUE-1 WITH KEY                   CM2024.2
            027400     XXXXX031.                                                    CM2024.2
            027500     MOVE 62 TO OUT-LENGTH.                                       CM2024.2
            027600     SEND CM-OUTQUE-1 FROM DISABLE-MSG WITH EMI.                  CM2024.2
            027700     PERFORM DELAY-FOR-30.                                        CM2024.2
            027800 BEGIN-CM202M-STATUS-TESTS.                                       CM2024.2
            027900     MOVE 200 TO FAIL-SAFE.                                       CM2024.2
            028000     MOVE "RCV MSG/SHORT AREA" TO FEATURE.                        CM2024.2
            028100     MOVE "RECEIVE-TEST-01" TO PAR-NAME.                          CM2024.2
            028200     MOVE " SEE REMARKS COLUMN " TO COMPUTED-A.                   CM2024.2
            028300     MOVE "  INSPECT RESULTS" TO CORRECT-A.                       CM2024.2
            028400 RECEIVE-TEST-01.                                                 CM2024.2
            028500     RECEIVE CM-INQUE-1 MESSAGE INTO MSG-1  NO DATA               CM2024.2
            028600         MOVE "QUEUE TESTED EMPTY" TO COMPUTED-A                  CM2024.2
            028700         MOVE SPACES TO CORRECT-A RE-MARK                         CM2024.2
            028800         PERFORM FAIL  PERFORM PRINT-DETAIL                       CM2024.2
            028900         GO TO RECEIVE-TEST-02-INIT.                              CM2024.2
            029000     IF END-KEY IS EQUAL TO "1"                                   CM2024.2
            029100         MOVE SPACE TO CORRECT-A                                  CM2024.2
            029200         MOVE "   ESI WAS SENSED   " TO COMPUTED-A                CM2024.2
            029300         PERFORM FAIL                                             CM2024.2
            029400         PERFORM PRINT-DETAIL                                     CM2024.2
            029500         GO TO RECEIVE-TEST-02-INIT.                              CM2024.2
            029600     PERFORM PRINT-DETAIL.                                        CM2024.2
            029700     IF END-KEY IS NOT EQUAL TO "0" GO TO RECEIVE-TEST-02-INIT.   CM2024.2
            029800     MOVE "RECEIVE-TEST-01-CONT" TO PAR-NAME.                     CM2024.2
            029900     SUBTRACT 1 FROM FAIL-SAFE.                                   CM2024.2
            030000     IF FAIL-SAFE IS EQUAL TO 0                                   CM2024.2
            030100         MOVE "FAIL-SAFE ACTIVATED" TO CORRECT-A                  CM2024.2
            030200         MOVE "****** WARNING *****" TO COMPUTED-A                CM2024.2
            030300         PERFORM PRINT-DETAIL                                     CM2024.2
            030400         GO TO RECEIVE-TEST-02-INIT.                              CM2024.2
            030500     PERFORM PRINT-DETAIL.                                        CM2024.2
            030600     GO TO RECEIVE-TEST-01.                                       CM2024.2
            030700 RECEIVE-DELETE-01.                                               CM2024.2
            030800     PERFORM DE-LETE.                                             CM2024.2
            030900     PERFORM PRINT-DETAIL.                                        CM2024.2
            031000 RECEIVE-TEST-02-INIT.                                            CM2024.2
            031100     MOVE 200 TO FAIL-SAFE.                                       CM2024.2
            031200     MOVE "RCV SEG/SHORT AREA" TO FEATURE.                        CM2024.2
            031300     MOVE "RECEIVE-TEST-02" TO PAR-NAME.                          CM2024.2
            031400     MOVE " SEE REMARKS COLUMN " TO COMPUTED-A.                   CM2024.2
            031500     MOVE "  INSPECT RESULTS" TO CORRECT-A.                       CM2024.2
            031600 RECEIVE-TEST-02.                                                 CM2024.2
            031700     RECEIVE CM-INQUE-1 SEGMENT INTO MSG-1  NO DATA               CM2024.2
            031800         MOVE "QUEUE TESTED EMPTY" TO COMPUTED-A                  CM2024.2
            031900         MOVE SPACES TO CORRECT-A  RE-MARK                        CM2024.2
            032000         PERFORM FAIL  PERFORM PRINT-DETAIL                       CM2024.2
            032100         GO TO RECEIVE-TEST-03-INIT.                              CM2024.2
            032200     PERFORM PRINT-DETAIL.                                        CM2024.2
            032300     IF END-KEY IS NOT EQUAL TO "0" GO TO RECEIVE-TEST-03-INIT.   CM2024.2
            032400     MOVE "RECEIVE-TEST-02-CONT" TO PAR-NAME.                     CM2024.2
            032500     SUBTRACT 1 FROM FAIL-SAFE.                                   CM2024.2
            032600     IF FAIL-SAFE IS EQUAL TO 0                                   CM2024.2
            032700         MOVE "FAIL-SAFE ACTIVATED" TO CORRECT-A                  CM2024.2
            032800         MOVE "****** WARNING *****" TO COMPUTED-A                CM2024.2
            032900         PERFORM PRINT-DETAIL                                     CM2024.2
            033000         GO TO RECEIVE-TEST-03-INIT.                              CM2024.2
            033100     PERFORM PRINT-DETAIL.                                        CM2024.2
            033200     GO TO RECEIVE-TEST-02.                                       CM2024.2
            033300 RECEIVE-DELETE-02.                                               CM2024.2
            033400     PERFORM DE-LETE.                                             CM2024.2
            033500     PERFORM PRINT-DETAIL.                                        CM2024.2
            033600 RECEIVE-TEST-03-INIT.                                            CM2024.2
            033700     MOVE "RCV SEG REPEATEDLY" TO FEATURE.                        CM2024.2
            033800     MOVE "RECEIVE-TEST-03" TO PAR-NAME.                          CM2024.2
            033900 RECEIVE-TEST-03.                                                 CM2024.2
            034000     MOVE "COMPTD SHOWS END KEY" TO CORRECT-A.                    CM2024.2
            034100     MOVE ALL "*" TO RE-MARK.                                     CM2024.2
            034200     RECEIVE CM-INQUE-1 SEGMENT INTO RE-MARK                      CM2024.2
            034300         NO DATA GO TO ENABL-STATUS-TEST-01.                      CM2024.2
            034400     MOVE END-KEY TO COMPUTED-STATUS.                             CM2024.2
            034500     PERFORM PRINT-DETAIL.                                        CM2024.2
            034600     MOVE "RECEIVE-TEST-03-CONT" TO PAR-NAME.                     CM2024.2
            034700     GO TO RECEIVE-TEST-03.                                       CM2024.2
            034800 RECEIVE-DELETE-03.                                               CM2024.2
            034900     PERFORM DE-LETE.                                             CM2024.2
            035000     PERFORM PRINT-DETAIL.                                        CM2024.2
            035100 ENABL-STATUS-TEST-01.                                            CM2024.2
            035200     MOVE "ENABLE TERMINAL" TO FEATURE.                           CM2024.2
            035300     MOVE "BAD PASSWORD SUPPLIED" TO RE-MARK.                     CM2024.2
            035400     ENABLE INPUT TERMINAL CM-INQUE-1 WITH KEY "GARBAGE".         CM2024.2
            035500     IF IN-STATUS IS EQUAL TO "40"                                CM2024.2
            035600         PERFORM PASS GO TO ENABL-STATUS-WRITE-01.                CM2024.2
            035700     MOVE IN-STATUS TO COMPUTED-STATUS.                           CM2024.2
            035800     MOVE "40" TO CORRECT-STATUS.                                 CM2024.2
            035900     PERFORM FAIL.                                                CM2024.2
            036000     GO TO ENABL-STATUS-WRITE-01.                                 CM2024.2
            036100 ENABL-STATUS-DELETE-01.                                          CM2024.2
            036200     PERFORM DE-LETE.                                             CM2024.2
            036300 ENABL-STATUS-WRITE-01.                                           CM2024.2
            036400     MOVE "ENABL-STATUS-TEST-01" TO PAR-NAME.                     CM2024.2
            036500     PERFORM PRINT-DETAIL.                                        CM2024.2
            036600 ENABL-STATUS-TEST-02.                                            CM2024.2
            036700     MOVE "BAD SOURCE NAME USED" TO RE-MARK.                      CM2024.2
            036800     MOVE "GARBAGE" TO SYM-SOURCE.                                CM2024.2
            036900     ENABLE INPUT TERMINAL CM-INQUE-1 WITH KEY                    CM2024.2
            037000     XXXXX031.                                                    CM2024.2
            037100     IF IN-STATUS IS EQUAL TO "21"                                CM2024.2
            037200         PERFORM PASS  GO TO ENABL-STATUS-WRITE-02.               CM2024.2
            037300     MOVE IN-STATUS TO COMPUTED-STATUS.                           CM2024.2
            037400     MOVE "21" TO CORRECT-STATUS.                                 CM2024.2
            037500     PERFORM FAIL.                                                CM2024.2
            037600     GO TO ENABL-STATUS-WRITE-02.                                 CM2024.2
            037700 ENABL-STATUS-DELETE-02.                                          CM2024.2
            037800     PERFORM DE-LETE.                                             CM2024.2
            037900 ENABL-STATUS-WRITE-02.                                           CM2024.2
            038000     MOVE "ENABL-STATUS-TEST-02" TO PAR-NAME.                     CM2024.2
            038100     PERFORM PRINT-DETAIL.                                        CM2024.2
            038200 DISAB-STATUS-TEST-01.                                            CM2024.2
            038300     MOVE "DISABLE TERMINAL" TO FEATURE.                          CM2024.2
            038400     MOVE "BAD PASSWORD SUPPLIED" TO RE-MARK.                     CM2024.2
            038500     MOVE                                                         CM2024.2
            038600     XXXXX042                                                     CM2024.2
            038700         TO SYM-SOURCE.                                           CM2024.2
            038800     DISABLE INPUT TERMINAL CM-INQUE-1 WITH KEY "GARBAGE".        CM2024.2
            038900     IF IN-STATUS IS EQUAL TO "40"                                CM2024.2
            039000         PERFORM PASS  GO TO DISAB-STATUS-WRITE-01.               CM2024.2
            039100     MOVE IN-STATUS TO COMPUTED-STATUS.                           CM2024.2
            039200     MOVE "40" TO CORRECT-STATUS.                                 CM2024.2
            039300     PERFORM FAIL.                                                CM2024.2
            039400     GO TO DISAB-STATUS-WRITE-01.                                 CM2024.2
            039500 DISAB-STATUS-DELETE-01.                                          CM2024.2
            039600     PERFORM DE-LETE.                                             CM2024.2
            039700 DISAB-STATUS-WRITE-01.                                           CM2024.2
            039800     MOVE "DISAB-STATUS-TEST-01" TO PAR-NAME.                     CM2024.2
            039900     PERFORM PRINT-DETAIL.                                        CM2024.2
            040000 DISAB-STATUS-TEST-02.                                            CM2024.2
            040100     MOVE "BAD SOURCE NAME USED" TO RE-MARK.                      CM2024.2
            040200     MOVE "GARBAGE" TO SYM-SOURCE.                                CM2024.2
            040300     DISABLE INPUT TERMINAL CM-INQUE-1 WITH KEY                   CM2024.2
            040400     XXXXX031.                                                    CM2024.2
            040500     IF IN-STATUS IS EQUAL TO "21"                                CM2024.2
            040600         PERFORM PASS  GO TO DISAB-STATUS-WRITE-02.               CM2024.2
            040700     MOVE IN-STATUS TO COMPUTED-STATUS.                           CM2024.2
            040800     MOVE "21" TO CORRECT-STATUS.                                 CM2024.2
            040900     PERFORM FAIL.                                                CM2024.2
            041000     GO TO DISAB-STATUS-WRITE-02.                                 CM2024.2
            041100 DISAB-STATUS-DELETE-02.                                          CM2024.2
            041200     PERFORM DE-LETE.                                             CM2024.2
            041300 DISAB-STATUS-WRITE-02.                                           CM2024.2
            041400     MOVE "DISAB-STATUS-TEST-02" TO PAR-NAME.                     CM2024.2
            041500     PERFORM PRINT-DETAIL.                                        CM2024.2
            041600 SEND-STATUS-TEST-01.                                             CM2024.2
            041700     MOVE "COUNT EXCEEDS LIMIT" TO RE-MARK.                       CM2024.2
            041800     MOVE 3 TO DEST-COUNT.                                        CM2024.2
            041900     MOVE 61 TO OUT-LENGTH.                                       CM2024.2
            042000     SEND CM-OUTQUE-1 FROM SEND-MSG WITH EMI.                     CM2024.2
            042100     IF OUT-STATUS IS EQUAL TO "30"                               CM2024.2
            042200         PERFORM PASS  GO TO SEND-STATUS-WRITE-01.                CM2024.2
            042300     MOVE OUT-STATUS TO COMPUTED-STATUS.                          CM2024.2
            042400     MOVE "30" TO CORRECT-STATUS.                                 CM2024.2
            042500     PERFORM FAIL.                                                CM2024.2
            042600     GO TO SEND-STATUS-WRITE-01.                                  CM2024.2
            042700 SEND-STATUS-DELETE-01.                                           CM2024.2
            042800     PERFORM DE-LETE.                                             CM2024.2
            042900 SEND-STATUS-WRITE-01.                                            CM2024.2
            043000     MOVE "DESTINATION COUNT" TO FEATURE.                         CM2024.2
            043100     MOVE "SEND-STATUS-TEST-01" TO PAR-NAME.                      CM2024.2
            043200     PERFORM PRINT-DETAIL.                                        CM2024.2
            043300     MOVE 2 TO DEST-COUNT.                                        CM2024.2
            043400 SEND-STATUS-TEST-02.                                             CM2024.2
            043500     MOVE 2 TO TEST-NUMB.                                         CM2024.2
            043600     MOVE 0 TO OUT-LENGTH.                                        CM2024.2
            043700     SEND CM-OUTQUE-1 FROM SEND-MSG.                              CM2024.2
            043800     IF OUT-STATUS IS EQUAL TO "60"                               CM2024.2
            043900         PERFORM PASS  GO TO SEND-STATUS-WRITE-02.                CM2024.2
            044000     MOVE OUT-STATUS TO COMPUTED-STATUS.                          CM2024.2
            044100     MOVE "60" TO CORRECT-STATUS.                                 CM2024.2
            044200     PERFORM FAIL.                                                CM2024.2
            044300     GO TO SEND-STATUS-WRITE-02.                                  CM2024.2
            044400 SEND-STATUS-DELETE-02.                                           CM2024.2
            044500     PERFORM DE-LETE.                                             CM2024.2
            044600 SEND-STATUS-WRITE-02.                                            CM2024.2
            044700     MOVE "PARTIAL SEGMENT" TO FEATURE.                           CM2024.2
            044800     MOVE "ZERO CHARACTER COUNT USED" TO RE-MARK.                 CM2024.2
            044900     MOVE "SEND-STATUS-TEST-02" TO PAR-NAME.                      CM2024.2
            045000     PERFORM PRINT-DETAIL.                                        CM2024.2
            045100 SEND-STATUS-TEST-03.                                             CM2024.2
            045200     MOVE 0 TO END-FLAG.                                          CM2024.2
            045300     MOVE 3 TO TEST-NUMB.                                         CM2024.2
            045400     MOVE 61 TO OUT-LENGTH.                                       CM2024.2
            045500     SEND CM-OUTQUE-1 WITH END-FLAG.                              CM2024.2
            045600     IF OUT-STATUS IS EQUAL TO "60"                               CM2024.2
            045700         PERFORM PASS  GO TO SEND-STATUS-WRITE-03.                CM2024.2
            045800     MOVE OUT-STATUS TO COMPUTED-STATUS.                          CM2024.2
            045900     MOVE "60" TO CORRECT-STATUS.                                 CM2024.2
            046000     PERFORM FAIL.                                                CM2024.2
            046100     GO TO SEND-STATUS-WRITE-03.                                  CM2024.2
            046200 SEND-STATUS-DELETE-03.                                           CM2024.2
            046300     PERFORM DE-LETE.                                             CM2024.2
            046400 SEND-STATUS-WRITE-03.                                            CM2024.2
            046500     MOVE "NO SENDING AREA SPECIFIED" TO RE-MARK.                 CM2024.2
            046600     MOVE "SEND-STATUS-TEST-03" TO PAR-NAME.                      CM2024.2
            046700     PERFORM PRINT-DETAIL.                                        CM2024.2
            046800 SEND-STATUS-TEST-04.                                             CM2024.2
            046900     MOVE 57 TO OUT-LENGTH.                                       CM2024.2
            047000     MOVE "GARBAGE" TO SYM-DEST (2).                              CM2024.2
            047100     SEND CM-OUTQUE-1 FROM ONE-TERMINAL-MSG WITH EMI.             CM2024.2
            047200     IF OUT-STATUS IS NOT EQUAL TO "20"                           CM2024.2
            047300         MOVE OUT-STATUS TO COMPUTED-STATUS                       CM2024.2
            047400         MOVE "STATUS SHOULD BE 20" TO CORRECT-A                  CM2024.2
            047500         PERFORM FAIL                                             CM2024.2
            047600     ELSE IF ERR-KEY (2) IS NOT EQUAL TO "1"                      CM2024.2
            047700         MOVE ERR-KEY (2) TO COMPUTED-STATUS                      CM2024.2
            047800         MOVE " ERROR KEY (2) = 1" TO CORRECT-A                   CM2024.2
            047900         PERFORM FAIL                                             CM2024.2
            048000     ELSE IF ERR-KEY (1) IS NOT EQUAL TO "0"                      CM2024.2
            048100         MOVE ERR-KEY (1) TO COMPUTED-STATUS                      CM2024.2
            048200         MOVE " ERROR KEY (1) = 0" TO CORRECT-A                   CM2024.2
            048300         PERFORM FAIL                                             CM2024.2
            048400     ELSE PERFORM PASS.                                           CM2024.2
            048500     GO TO SEND-STATUS-WRITE-04.                                  CM2024.2
            048600 SEND-STATUS-DELETE-04.                                           CM2024.2
            048700     PERFORM DE-LETE.                                             CM2024.2
            048800 SEND-STATUS-WRITE-04.                                            CM2024.2
            048900     MOVE "SYMBOLIC DESTINAT""N (2) IS BAD" TO RE-MARK.           CM2024.2
            049000     MOVE "SEND-STATUS-TEST-04" TO PAR-NAME.                      CM2024.2
            049100     PERFORM PRINT-DETAIL.                                        CM2024.2
            049200     MOVE                                                         CM2024.2
            049300     XXXXX035                                                     CM2024.2
            049400         TO SYM-DEST (2).                                         CM2024.2
            049500 SEGMENTED-MSG-TEST-01.                                           CM2024.2
            049600     MOVE 1 TO SEG-TEST-NO.                                       CM2024.2
            049700     MOVE 39 TO OUT-LENGTH.                                       CM2024.2
            049800     SEND CM-OUTQUE-1 FROM SEG-INIT WITH ESI.                     CM2024.2
            049900     MOVE 14 TO OUT-LENGTH.                                       CM2024.2
            050000     SEND CM-OUTQUE-1 FROM MSG-COMP WITH EMI.                     CM2024.2
            050100 SEGMENTED-MSG-TEST-02.                                           CM2024.2
            050200     MOVE 2 TO SEG-TEST-NO.                                       CM2024.2
            050300     MOVE 39 TO OUT-LENGTH.                                       CM2024.2
            050400     SEND CM-OUTQUE-1 FROM SEG-INIT WITH ESI.                     CM2024.2
            050500     MOVE 16 TO OUT-LENGTH.                                       CM2024.2
            050600     SEND CM-OUTQUE-1 FROM GROUP-COMP WITH EGI.                   CM2024.2
            050700 SEGMENTED-MSG-TEST-03.                                           CM2024.2
            050800     MOVE 3 TO SEG-TEST-NO.                                       CM2024.2
            050900     MOVE 39 TO OUT-LENGTH.                                       CM2024.2
            051000     SEND CM-OUTQUE-1 FROM SEG-INIT.                              CM2024.2
            051100     MOVE 10 TO OUT-LENGTH.                                       CM2024.2
            051200     SEND CM-OUTQUE-1 FROM SEG-CONT WITH ESI.                     CM2024.2
            051300     MOVE 14 TO OUT-LENGTH.                                       CM2024.2
            051400     SEND CM-OUTQUE-1 FROM MSG-COMP WITH EMI.                     CM2024.2
            051500 SEGMENTED-MSG-TEST-04.                                           CM2024.2
            051600     MOVE 4 TO SEG-TEST-NO.                                       CM2024.2
            051700     MOVE 0 TO END-FLAG.                                          CM2024.2
            051800     MOVE 39 TO OUT-LENGTH.                                       CM2024.2
            051900     SEND CM-OUTQUE-1 FROM SEG-INIT WITH END-FLAG.                CM2024.2
            052000     MOVE 1 TO END-FLAG.                                          CM2024.2
            052100     MOVE 10 TO OUT-LENGTH.                                       CM2024.2
            052200     SEND CM-OUTQUE-1 FROM SEG-CONT WITH END-FLAG.                CM2024.2
            052300     MOVE 2 TO END-FLAG.                                          CM2024.2
            052400     MOVE 14 TO OUT-LENGTH.                                       CM2024.2
            052500     SEND CM-OUTQUE-1 FROM MSG-COMP WITH END-FLAG.                CM2024.2
            052600     MOVE 3 TO END-FLAG.                                          CM2024.2
            052700     MOVE 16 TO OUT-LENGTH.                                       CM2024.2
            052800     SEND CM-OUTQUE-1 FROM GROUP-COMP WITH END-FLAG.              CM2024.2
            052900 SEGMENTED-MSG-TEST-05.                                           CM2024.2
            053000     MOVE 71 TO OUT-LENGTH.                                       CM2024.2
            053100     MOVE 0 TO END-FLAG.                                          CM2024.2
            053200     SEND CM-OUTQUE-1 FROM SKIP-MSG-1 WITH END-FLAG               CM2024.2
            053300         BEFORE ADVANCING 4 LINES.                                CM2024.2
            053400     MOVE 17 TO OUT-LENGTH.                                       CM2024.2
            053500     MOVE 3 TO END-FLAG.                                          CM2024.2
            053600     SEND CM-OUTQUE-1 FROM SKIP-MSG-2 WITH END-FLAG.              CM2024.2
            053700 SINGLE-TERMINAL-TEST-01.                                         CM2024.2
            053800     MOVE 1 TO DEST-COUNT.                                        CM2024.2
            053900     MOVE 57 TO OUT-LENGTH.                                       CM2024.2
            054000     SEND CM-OUTQUE-1 FROM ONE-TERMINAL-MSG WITH EGI.             CM2024.2
            054100     MOVE 2 TO DEST-COUNT.                                        CM2024.2
            054200 INCOMPLETE-MSG-TEST-01.                                          CM2024.2
            054300     MOVE 55 TO OUT-LENGTH.                                       CM2024.2
            054400     MOVE 1 TO INC-MSG-NO.                                        CM2024.2
            054500     SEND CM-OUTQUE-1 FROM INCOMP-MSG.                            CM2024.2
            054600 INCOMPLETE-MSG-TEST-02.                                          CM2024.2
            054700     MOVE 55 TO OUT-LENGTH.                                       CM2024.2
            054800     MOVE 2 TO INC-MSG-NO.                                        CM2024.2
            054900     SEND CM-OUTQUE-1 FROM INCOMP-MSG WITH ESI.                   CM2024.2
            055000 INCOMPLETE-MSG-TEST-03.                                          CM2024.2
            055100     MOVE 0 TO END-FLAG.                                          CM2024.2
            055200     MOVE 55 TO OUT-LENGTH.                                       CM2024.2
            055300     MOVE 3 TO INC-MSG-NO.                                        CM2024.2
            055400     SEND CM-OUTQUE-1 FROM INCOMP-MSG WITH END-FLAG.              CM2024.2
            055500 INCOMPLETE-MSG-TEST-04.                                          CM2024.2
            055600     MOVE 1 TO END-FLAG.                                          CM2024.2
            055700     MOVE 55 TO OUT-LENGTH.                                       CM2024.2
            055800     MOVE 4 TO INC-MSG-NO.                                        CM2024.2
            055900     SEND CM-OUTQUE-1 FROM INCOMP-MSG WITH END-FLAG.              CM2024.2
            056000 STOP-WITHOUT-COMPLETING-MSG.                                     CM2024.2
            056100     PERFORM END-ROUTINE THRU END-ROUTINE-4.                      CM2024.2
            056200     CLOSE PRINT-FILE.                                            CM2024.2
            056300     STOP RUN.                                                    CM2024.2
            056400 DELAY-FOR-30 SECTION.                                            CM2024.2
            056500 TAKE-INIT-TIME.                                                  CM2024.2
            056600     ACCEPT INIT-TIME FROM TIME.                                  CM2024.2
            056700 TEST-ELAPSED-TIME.                                               CM2024.2
            056800     ACCEPT TEST-TIME FROM TIME.                                  CM2024.2
            056900     COMPUTE ELAPSED-TIME =                                       CM2024.2
            057000         (T-HRS * 3600 + T-MINS * 60 + T-SECS) -                  CM2024.2
            057100         (I-HRS * 3600 + I-MINS * 60 + I-SECS).                   CM2024.2
            057200     IF ELAPSED-TIME IS LESS THAN 30 GO TO TEST-ELAPSED-TIME.     CM2024.2
            057300 COMMON-SUBROUTINES SECTION.                                      CM2024.2
            057400 PASS.                                                            CM2024.2
            057500     MOVE "PASS" TO P-OR-F.                                       CM2024.2
            057600 FAIL.                                                            CM2024.2
            057700     ADD      1 TO ERROR-COUNTER.                                 CM2024.2
            057800     MOVE "FAIL*" TO P-OR-F.                                      CM2024.2
            057900 DE-LETE.                                                         CM2024.2
            058000     MOVE     SPACE TO P-OR-F.                                    CM2024.2
            058100     MOVE     "    ************    " TO COMPUTED-A.               CM2024.2
            058200     MOVE     "    ************    " TO CORRECT-A.                CM2024.2
            058300     MOVE "****TEST DELETED****" TO RE-MARK.                      CM2024.2
            058400     ADD 1 TO DELETE-CNT.                                         CM2024.2
            058500 PRINT-DETAIL.                                                    CM2024.2
            058600     MOVE     TEST-RESULTS TO PRINT-REC.                          CM2024.2
            058700     PERFORM WRITE-LINE.                                          CM2024.2
            058800     MOVE     SPACE TO P-OR-F.                                    CM2024.2
            058900     MOVE     SPACE TO COMPUTED-A.                                CM2024.2
            059000     MOVE SPACE TO CORRECT-A.                                     CM2024.2
            059100     MOVE SPACE TO RE-MARK.                                       CM2024.2
            059200     MOVE SPACE TO FEATURE.                                       CM2024.2
            059300 COLUMN-NAMES-ROUTINE.                                            CM2024.2
            059400     MOVE     COLUMNS-LINE-1 TO DUMMY-RECORD.                     CM2024.2
            059500     PERFORM WRITE-LINE.                                          CM2024.2
            059600     MOVE     COLUMNS-LINE-2 TO DUMMY-RECORD.                     CM2024.2
            059700     PERFORM WRITE-LINE.                                          CM2024.2
            059800     PERFORM  BLANK-LINE-PRINT.                                   CM2024.2
            059900 END-ROUTINE.                                                     CM2024.2
            060000     MOVE     HYPHEN-LINE TO DUMMY-RECORD.                        CM2024.2
            060100     PERFORM WRITE-LINE.                                          CM2024.2
            060200 PARA-Z.                                                          CM2024.2
            060300     PERFORM  BLANK-LINE-PRINT 4 TIMES.                           CM2024.2
            060400     MOVE     CCVS-E-1 TO DUMMY-RECORD.                           CM2024.2
            060500     PERFORM WRITE-LINE.                                          CM2024.2
            060600 END-ROUTINE-1.                                                   CM2024.2
            060700     PERFORM  BLANK-LINE-PRINT.                                   CM2024.2
            060800     IF       ERROR-COUNTER IS EQUAL TO ZERO                      CM2024.2
            060900              GO TO END-ROUTINE-2.                                CM2024.2
            061000     MOVE     ERROR-COUNTER TO ERROR-TOTAL.                       CM2024.2
            061100     GO TO    END-ROUTINE-3.                                      CM2024.2
            061200 END-ROUTINE-2.                                                   CM2024.2
            061300     MOVE " NO" TO ERROR-TOTAL.                                   CM2024.2
            061400 END-ROUTINE-3.                                                   CM2024.2
            061500     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           CM2024.2
            061600     PERFORM WRITE-LINE.                                          CM2024.2
            061700     IF DELETE-CNT IS EQUAL TO ZERO                               CM2024.2
            061800         MOVE " NO" TO ERROR-TOTAL  ELSE                          CM2024.2
            061900     MOVE DELETE-CNT TO ERROR-TOTAL.                              CM2024.2
            062000     MOVE "TESTS DELETED     " TO ENDER-DESC.                     CM2024.2
            062100     MOVE CCVS-E-2 TO DUMMY-RECORD.                               CM2024.2
            062200     PERFORM WRITE-LINE.                                          CM2024.2
            062300 END-ROUTINE-4.                                                   CM2024.2
            062400     MOVE CCVS-E-3 TO DUMMY-RECORD.                               CM2024.2
            062500     PERFORM WRITE-LINE.                                          CM2024.2
            062600 BLANK-LINE-PRINT.                                                CM2024.2
            062700     MOVE     SPACE TO DUMMY-RECORD.                              CM2024.2
            062800     PERFORM WRITE-LINE.                                          CM2024.2
            062900 WRITE-LINE.                                                      CM2024.2
            063000     WRITE DUMMY-RECORD AFTER ADVANCING 1 LINE.                   CM2024.2
            063100 HEAD-ROUTINE.                                                    CM2024.2
            063200     MOVE CCVS-H-1 TO PRINT-REC                                   CM2024.2
            063300     WRITE PRINT-REC                                              CM2024.2
            063400         AFTER ADVANCING PAGE.                                    CM2024.2
            063500     MOVE CCVS-H-2 TO PRINT-REC.                                  CM2024.2
            063600     WRITE PRINT-REC                                              CM2024.2
            063700         AFTER 2 LINES.                                           CM2024.2
            063800     MOVE CCVS-H-3 TO PRINT-REC.                                  CM2024.2
            063900     WRITE PRINT-REC                                              CM2024.2
            064000         AFTER 5 LINES.                                           CM2024.2
            064100     MOVE HYPHEN-LINE TO PRINT-REC.                               CM2024.2
            064200     PERFORM WRITE-LINE.                                          CM2024.2
                  *END-OF,CM202M                                                            
        """)
    )

    @Test
    fun cm3034_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM303M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM3034.2
            000200 PROGRAM-ID.                                                      CM3034.2
            000300     CM303M.                                                      CM3034.2
            000400*THE FOLLOWING PROGRAM TESTS THE FLAGGING OF                      CM3034.2
            000500*OBSOLETE FEATURES THAT ARE USED IN COMMUNICATIONS.               CM3034.2
            000600 ENVIRONMENT DIVISION.                                            CM3034.2
            000700 CONFIGURATION SECTION.                                           CM3034.2
            000800 SOURCE-COMPUTER.                                                 CM3034.2
            000900     XXXXX082.                                                    CM3034.2
            001000 OBJECT-COMPUTER.                                                 CM3034.2
            001100     XXXXX083.                                                    CM3034.2
            001200                                                                  CM3034.2
            001300                                                                  CM3034.2
            001400 DATA DIVISION.                                                   CM3034.2
            001500 FILE SECTION.                                                    CM3034.2
            001600 COMMUNICATION SECTION.                                           CM3034.2
            001700 CD COMMNAME FOR INITIAL INPUT.                                   CM3034.2
            001800 01 CREC.                                                         CM3034.2
            001900     03 CNAME1 PIC X(87).                                         CM3034.2
            002000                                                                  CM3034.2
            002100 PROCEDURE DIVISION.                                              CM3034.2
            002200                                                                  CM3034.2
            002300 CM303M-CONTROL.                                                  CM3034.2
            002400     PERFORM CM303M-DISABLE THRU CM303M-ENABLE.                   CM3034.2
            002500     STOP RUN.                                                    CM3034.2
            002600                                                                  CM3034.2
            002700 CM303M-DISABLE.                                                  CM3034.2
            002800     DISABLE INPUT COMMNAME WITH KEY CNAME1.                      CM3034.2
            002900*Message expected for above statement: OBSOLETE                   CM3034.2
            003000                                                                  CM3034.2
            003100 CM303M-ENABLE.                                                   CM3034.2
            003200     ENABLE INPUT COMMNAME WITH KEY CNAME1.                       CM3034.2
            003300*Message expected for above statement: OBSOLETE                   CM3034.2
            003400                                                                  CM3034.2
            003500*TOTAL NUMBER OF FLAGS EXPECTED = 2.                              CM3034.2
                  *END-OF,CM303M                                                            
        """)
    )

    @Test
    fun cm4014_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,CM401M                                                      
            000100 IDENTIFICATION DIVISION.                                         CM4014.2
            000200 PROGRAM-ID.                                                      CM4014.2
            000300      CM401M.                                                     CM4014.2
            000400*The following program tests the flagging of level 2              CM4014.2
            000500*features of the communication module.                            CM4014.2
            000600 ENVIRONMENT DIVISION.                                            CM4014.2
            000700 CONFIGURATION SECTION.                                           CM4014.2
            000800 SOURCE-COMPUTER.                                                 CM4014.2
            000900     XXXXX082.                                                    CM4014.2
            001000 OBJECT-COMPUTER.                                                 CM4014.2
            001100     XXXXX083.                                                    CM4014.2
            001200 DATA DIVISION.                                                   CM4014.2
            001300 FILE SECTION.                                                    CM4014.2
            001400 COMMUNICATION SECTION.                                           CM4014.2
            001500 CD COMMNAME FOR INITIAL INPUT                                    CM4014.2
            001600*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            001700     SYMBOLIC SUB-QUEUE-1 IS CQ.                                  CM4014.2
            001800*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            001900 01 CREC.                                                         CM4014.2
            002000     03 CNAME1   PIC X(8).                                        CM4014.2
            002100     03 CQ       PIC 9(8).                                        CM4014.2
            002200     03 FILLER   PIC X(62).                                       CM4014.2
            002300     03 CINT     PIC 9.                                           CM4014.2
            002400     03 FILLER   PIC X(8).                                        CM4014.2
            002500                                                                  CM4014.2
            002600 CD COMM2 FOR OUTPUT                                              CM4014.2
            002700     DESTINATION TABLE OCCURS 7 TIMES.                            CM4014.2
            002800*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            002900                                                                  CM4014.2
            003000 PROCEDURE DIVISION.                                              CM4014.2
            003100                                                                  CM4014.2
            003200 CM401M-CONTROL.                                                  CM4014.2
            003300     PERFORM CM401M-DISABLE THRU CM401M-SENDREP.                  CM4014.2
            003400     STOP RUN.                                                    CM4014.2
            003500                                                                  CM4014.2
            003600 CM401M-DISABLE.                                                  CM4014.2
            003700     DISABLE INPUT COMMNAME WITH KEY CNAME1.                      CM4014.2
            003800*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            003900                                                                  CM4014.2
            004000 CM401M-ENABLE.                                                   CM4014.2
            004100     ENABLE INPUT COMMNAME WITH KEY CNAME1.                       CM4014.2
            004200*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            004300                                                                  CM4014.2
            004400                                                                  CM4014.2
            004500 CM401M-PURGE.                                                    CM4014.2
            004600     PURGE COMM2.                                                 CM4014.2
            004700*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            004800                                                                  CM4014.2
            004900 CM401M-SEND.                                                     CM4014.2
            005000     SEND COMM2 FROM CNAME1.                                      CM4014.2
            005100*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            005200                                                                  CM4014.2
            005300 CM401M-SENDID.                                                   CM4014.2
            005400     SEND COMM2 FROM CNAME1 WITH CINT.                            CM4014.2
            005500*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            005600                                                                  CM4014.2
            005700 CM401M-SENDESI.                                                  CM4014.2
            005800     SEND COMM2 FROM CNAME1 WITH ESI.                             CM4014.2
            005900*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            006000                                                                  CM4014.2
            006100 CM401M-SENDREP.                                                  CM4014.2
            006200     SEND COMM2 WITH EMI REPLACING LINE.                          CM4014.2
            006300*Message expected for above statement: NON-CONFORMING STANDARD    CM4014.2
            006400                                                                  CM4014.2
            006500*TOTAL NUMBER OF FLAGS EXPECTED = 10.                             CM4014.2
                  *END-OF,CM401M                                                            
        """)
    )
}
